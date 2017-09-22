-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import "base-unicode-symbols" Prelude.Unicode

import "data-default" Data.Default (def)
import "base"         Data.Bool (bool)
import "aeson"        Data.Aeson (encode)
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
import Types (State (..), ProtocolInitialization (..), Unit (..))


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
view s = encode [ numLockView
                , capsLockView
                , separateAfter alternativeView
                , kbdLayoutView
                ]

  where numLockView, capsLockView, alternativeView, kbdLayoutView ∷ Unit

        numLockView = let isOn = numLock s
          in def { fullText = "num"
                 , color    = Just $ bool "#999999" "#eeeeee" isOn
                 }

        capsLockView = let isOn = capsLock s
          in def { fullText = bool "caps" "CAPS" isOn
                 , color    = Just $ bool "#999999" "#ff9900" isOn
                 }

        alternativeView = let isOn = alternative s
          in def { fullText  = bool "hax" "HAX" isOn
                 , color     = Just $ bool "#999999" "#ffff00" isOn
                 }

        kbdLayoutView

          | kbdLayout s ∈ [0, 1] = let isRU = kbdLayout s ≢ 0
            in def { fullText = bool "US" "RU" isRU
                   , color    = Just $ bool "#ff0000" "#00ff00" isRU
                   }

          | otherwise = def { fullText = "%ERROR%", color = Just "#ff0000" }

        separateAfter unit = unit { separator           = Just True
                                  , separatorBlockWidth = Just 20
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

  -- Bind IPC events handlers
  -- TODO Handle keyboard layout
  sigHandlers ←
    let listen (member, lens) = addMatch client (matchRule member) $ handle lens
        matchRule member = basicMatchRule { matchMember = Just member }

        handle lens (signalBody → map fromVariant → [Just (v ∷ Bool)]) =
          put $ Just (lens, v)

        handle _ _ = return () -- Incorrect arguments, just ignoring it

                    -- Pairs of IPC method and lens for the state
     in mapM listen [ ("numlock",     \s v → s { numLock     = v })
                    , ("capslock",    \s v → s { capsLock    = v })
                    , ("alternative", \s v → s { alternative = v })
                    ]

  -- Handle POSIX signals to terminate application
  let terminate = do mapM_ (removeMatch client) sigHandlers
                     _ ← releaseName client busName
                     disconnect client
                     put Nothing

      catch sig = installHandler sig (Catch terminate) Nothing

   in mapM_ catch [sigHUP, sigINT, sigTERM, sigPIPE]

  dieWithParent -- make this app die if parent die

  echo $ encode (def ∷ ProtocolInitialization)
  echo "[" -- open lazy list

  -- Main thread is reactive loop that gets lens and value from another thread
  -- to update the state and render it (if it's Just) or terminate the
  -- application (it it's Nothing).
  let handle ∷ State → Maybe ((State → Bool → State), Bool) → IO State
      handle prevState Nothing = prevState <$ echo "]" >> exitSuccess

      handle prevState (Just (lens, v)) =
        let newState = lens prevState v
         in if newState == prevState
               then return prevState
               else newState <$ echo ("," `append` view newState)

      next s = takeMVar mVar >>= handle s >>= next

   in () <$ echo (view def) >> next def


echo ∷ ByteString → IO ()
echo s = hPutStrLn stdout s >> hFlush stdout

getDisplayName ∷ Display → String
getDisplayName dpy = map f $ displayString dpy
  where f ':' = '_'
        f '.' = '_'
        f  x  =  x
