-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import "base-unicode-symbols" Prelude.Unicode
import "base" GHC.Generics (Generic)

import "data-default" Data.Default (Default, def)
import "base"         Data.Bool (bool)

import "aeson"        Data.Aeson ( ToJSON (toJSON)
                                 , encode
                                 , defaultOptions
                                 , genericToJSON
                                 )

import "aeson"        Data.Aeson.Types (Options (fieldLabelModifier), camelTo2)
import "bytestring"   Data.ByteString.Lazy.Char8 (ByteString, hPutStrLn, append)

import "base" Control.Monad (when)
import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import "base" System.IO (hFlush, stdout)
import "base" System.Exit (die, exitSuccess)

import "unix" System.Posix.Signals ( installHandler
                                   , Handler (Catch)
                                   , sigHUP
                                   , sigINT
                                   , sigTERM
                                   , sigPIPE
                                   )

import "dbus" DBus ( objectPath_
                   , busName_
                   , ObjectPath
                   , InterfaceName
                   , Signal (signalBody, signalSender, signalDestination)
                   , IsVariant (fromVariant)
                   , signal
                   )

import "dbus" DBus.Client ( connectSession
                          , disconnect
                          , requestName
                          , releaseName
                          , RequestNameReply (NamePrimaryOwner)
                          , addMatch
                          , removeMatch
                          , matchAny
                          , emit

                          , MatchRule ( matchPath
                                      , matchSender
                                      , matchDestination
                                      , matchInterface
                                      , matchMember
                                      )
                          )

import "X11" Graphics.X11.Xlib ( Display
                               , openDisplay
                               , closeDisplay
                               , displayString
                               )

-- local imports

import ParentProc (dieWithParent)


data State
  = State
  { numLock     ∷ Bool
  , capsLock    ∷ Bool
  , alternative ∷ Bool
  }

  deriving (Show, Eq)

instance Default State where
  def
    = State
    { numLock     = False
    , capsLock    = False
    , alternative = False
    }


data ProtocolInitialization
  = ProtocolInitialization
  { version     ∷ Int
  , stopSignal  ∷ Maybe Int
  , contSignal  ∷ Maybe Int
  , clickEvents ∷ Bool
  }

  deriving (Show, Eq, Generic)

instance Default ProtocolInitialization where
  def
    = ProtocolInitialization
    { version     = 1
    , stopSignal  = Nothing
    , contSignal  = Nothing
    , clickEvents = False
    }

instance ToJSON ProtocolInitialization where
  toJSON = genericToJSON $ withFieldNamer id


data Unit
  = Unit
  { fullText            ∷ String
  , shortText           ∷ Maybe String
  , color               ∷ Maybe String
  , background          ∷ Maybe String
  , border              ∷ Maybe String
  , minWidth            ∷ Maybe Int
  , align               ∷ Maybe String
  , name                ∷ Maybe String
  , _instance           ∷ Maybe String
  , urgent              ∷ Maybe Bool
  , separator           ∷ Maybe Bool
  , separatorBlockWidth ∷ Maybe Int
  , markup              ∷ Maybe String
  }

  deriving (Show, Eq, Generic)

instance Default Unit where
  def
    = Unit
    { fullText            = undefined
    , shortText           = Nothing
    , color               = Just "#999999"
    , background          = Nothing
    , border              = Nothing
    , minWidth            = Nothing
    , align               = Nothing
    , name                = Nothing
    , _instance           = Nothing
    , urgent              = Nothing
    , separator           = Just False
    , separatorBlockWidth = Nothing
    , markup              = Just "none"
    }

instance ToJSON Unit where
  toJSON = genericToJSON $ withFieldNamer f
    where f ('_':xs) = xs; f x = x


objPath ∷ ObjectPath
objPath = "/"

flushObjPathPfx ∷ String
flushObjPathPfx = "/com/github/unclechu/xmonadrc/"

-- To add current Display suffix
busNamePfx ∷ String
busNamePfx = "com.github.unclechu.xmonadrc."

interfaceName ∷ InterfaceName
interfaceName = "com.github.unclechu.xmonadrc"


view ∷ State → ByteString
view s = encode $ map (\f → f s) [numLockView, capsLockView, alternativeView]

  where numLockView, capsLockView, alternativeView ∷ State → Unit

        numLockView (numLock → isOn) =
          def { fullText = "num"
              , color    = Just $ bool "#999999" "#eeeeee" isOn
              }

        capsLockView (capsLock → isOn) =
          def { fullText = bool "caps" "CAPS" isOn
              , color    = Just $ bool "#999999" "#ff9900" isOn
              }

        alternativeView (alternative → isOn) =
          def { fullText = bool "hax" "HAX" isOn
              , color    = Just $ bool "#999999" "#ffff00" isOn
              }


main ∷ IO ()
main = do
  -- Connecting to DBus
  client  ← connectSession

  -- Getting bus name for our service that depends on Display name
  dpyView ← do dpy   ← openDisplay ""
               let x = getDisplayName dpy
               x <$ (x `seq` closeDisplay dpy)

  let busName = busName_ $ busNamePfx ⧺ dpyView
      flushObjPath = objectPath_ $ flushObjPathPfx ⧺ dpyView

  -- Grab the bus name for our service
  requestName client busName [] >>= \reply →
    when (reply ≢ NamePrimaryOwner) $
      die $ "Requesting name '" ⧺ show busName ⧺ "' error: " ⧺ show reply

  mVar ← newEmptyMVar

  let put = putMVar mVar

      basicMatchRule = matchAny { matchPath        = Just objPath
                                , matchInterface   = Just interfaceName
                                , matchDestination = Just busName
                                , matchSender      = Nothing
                                }

  -- If `xlib-keys-hack` started before ask it to reflush indicators
  emit client (signal flushObjPath interfaceName "request_flush_all")
                { signalSender      = Just busName
                , signalDestination = Nothing
                , signalBody        = []
                }

  sigHandlers ←
    let listen (member, lens) = addMatch client (matchRule member) $ handle lens
        matchRule member = basicMatchRule { matchMember = Just member }

        handle lens (signalBody → map fromVariant → [Just (v ∷ Bool)]) =
          put $ Just (lens, v)

        handle _ _ = return () -- Incorrect arguments, just ignoring it

     in mapM listen [ ("numlock",     \s v → s { numLock     = v })
                    , ("capslock",    \s v → s { capsLock    = v })
                    , ("alternative", \s v → s { alternative = v })
                    ]

  let terminate = do mapM_ (removeMatch client) sigHandlers
                     _ ← releaseName client busName
                     disconnect client
                     put Nothing

      catch sig = installHandler sig (Catch terminate) Nothing

   in mapM_ catch [sigHUP, sigINT, sigTERM, sigPIPE]

  dieWithParent

  echo $ encode (def ∷ ProtocolInitialization)
  echo "["
  echo "[]"

  let handle ∷ State → Maybe ((State → Bool → State), Bool) → IO State
      handle prevState Nothing = prevState <$ exitSuccess

      handle prevState (Just (lens, v)) =
        let newState = lens prevState v
         in if newState == prevState
               then return prevState
               else newState <$ echo ("," `append` view newState)

      next s = takeMVar mVar >>= handle s >>= next

   in () <$ echo ("," `append` view def) >> next def


echo ∷ ByteString → IO ()
echo s = hPutStrLn stdout s >> hFlush stdout

getDisplayName ∷ Display → String
getDisplayName dpy = map f $ displayString dpy
  where f ':' = '_'
        f '.' = '_'
        f  x  =  x

withFieldNamer ∷ (String → String) → Options
withFieldNamer f = defaultOptions { fieldLabelModifier = f ∘ camelTo2 '_' }
