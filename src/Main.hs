-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import "base-unicode-symbols" Prelude.Unicode

import "data-default" Data.Default (def)
import "base"         Data.Bool (bool)
import "base"         Data.Tuple (swap)
import "base"         Data.Fixed (Pico)
import "base"         Data.Maybe (fromMaybe)
import "aeson"        Data.Aeson (encode, decodeStrict)
import "time"         Data.Time.Clock (UTCTime)
import "bytestring"   Data.ByteString.Char8 (hGetLine, uncons)

import "bytestring"   Data.ByteString.Lazy.Char8 ( ByteString
                                                 , hPutStrLn
                                                 , append
                                                 )

import "time"         Data.Time.Format ( FormatTime
                                       , formatTime
                                       , defaultTimeLocale
                                       )

import "time"         Data.Time.LocalTime
                        ( TimeZone
                        , TimeOfDay (todSec)
                        , LocalTime (localTimeOfDay)
                        , ZonedTime ( zonedTimeToLocalTime
                                    , zonedTimeZone
                                    )

                        , getZonedTime
                        , zonedTimeToUTC
                        , utcToZonedTime
                        )


import "base" Control.Monad (when, forever)
import "base" Control.Concurrent (forkIO, threadDelay)
import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import "base"    System.IO (stdout, stdin, hFlush)
import "base"    System.Exit (die, exitSuccess)

import "process" System.Process ( CreateProcess ( std_in
                                                , std_out
                                                , std_err
                                                , new_session
                                                )

                                , StdStream (NoStream)
                                , proc
                                , createProcess
                                )

import "unix"    System.Posix.Signals ( installHandler
                                      , Handler (Catch)
                                      , sigHUP
                                      , sigINT
                                      , sigTERM
                                      , sigPIPE
                                      )

import "X11"  Graphics.X11.Types ( xK_Num_Lock
                                 , xK_Caps_Lock
                                 , xK_Shift_L
                                 , xK_Shift_R
                                 )

import "X11"  Graphics.X11.Xlib ( Display
                                , openDisplay
                                , closeDisplay
                                , displayString
                                )

import "dbus" DBus ( ObjectPath
                   , InterfaceName
                   , Signal (signalBody, signalSender, signalDestination)
                   , IsVariant (fromVariant)
                   , Type (TypeBoolean, TypeWord8)
                   , variantType
                   , objectPath_
                   , busName_
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

-- local imports

import X (initThreads, fakeKeyEvent)
import ParentProc (dieWithParent)
import Types ( State (..)
             , ProtocolInitialization (..)
             , Unit (..)
             , ClickEvent (..)
             )


objPath ∷ ObjectPath
objPath = "/"

flushObjPathPfx ∷ String
flushObjPathPfx = "/com/github/unclechu/xmonadrc/"

-- To add current Display suffix
busNamePfx ∷ String
busNamePfx = "com.github.unclechu.xmonadrc."

interfaceName ∷ InterfaceName
interfaceName = "com.github.unclechu.xmonadrc"

dateFormat ∷ String
dateFormat = "%A %-d %B %H:%M"


view ∷ State → ByteString
view s = encode [ numLockView
                , capsLockView
                , alternativeView
                , _separate
                , kbdLayoutView
                , _separate
                , dateAndTimeView
                ]

  where numLockView, capsLockView, alternativeView, kbdLayoutView ∷ Unit
        dateAndTimeView, _separate ∷ Unit

        numLockView = let isOn = numLock s
          in def { fullText = "num"
                 , color    = Just $ bool "#999999" "#eeeeee" isOn
                 , name     = Just "numlock"
                 }

        capsLockView = let isOn = capsLock s
          in def { fullText = bool "caps" "CAPS" isOn
                 , color    = Just $ bool "#999999" "#ff9900" isOn
                 , name     = Just "capslock"
                 }

        alternativeView = let isOn = alternative s
          in def { fullText = bool "hax" "HAX" isOn
                 , color    = Just $ bool "#999999" "#ffff00" isOn
                 , name     = Just "alternative"
                 }

        kbdLayoutView

          | kbdLayout s ∈ [0, 1] = let isRU = kbdLayout s ≢ 0
            in def { fullText = bool "US" "RU" isRU
                   , color    = Just $ bool "#ff0000" "#00ff00" isRU
                   , name     = Just "kbdlayout"
                   }

          | otherwise = def { fullText = "%ERROR%", color = Just "#ff0000" }

        dateAndTimeView =
          fromMaybe def { fullText = "…" } $ set ∘ render <$> lastTime s
          where render = renderDate ∘ uncurry utcToZonedTime ∘ swap
                set x  = def { fullText = x, name = Just "datentime" }

        _separate = def { fullText = "/", color = Just "#666666" }
        -- separateAfter x = x { separator           = Just True
        --                     , separatorBlockWidth = Just 20
        --                     }


fetchDateAndTime ∷ IO (Pico, UTCTime, TimeZone)
fetchDateAndTime = getZonedTime <&> \zt →

  let utc      = zonedTimeToUTC zt
      timeZone = zonedTimeZone  zt
      seconds  = todSec $ localTimeOfDay $ zonedTimeToLocalTime $ zt
      secsLeft = 60 - seconds -- left to next minute

   in (secsLeft, utc, timeZone)


handleClickEvent ∷ IO () → ClickEvent → IO ()
handleClickEvent tglAlt ((\x → name (x ∷ ClickEvent)) → Just x) = case x of

  "numlock"     → fakeKeyEvent $ map (xK_Num_Lock,)  [False, True, False]
  "capslock"    → fakeKeyEvent $ map (xK_Caps_Lock,) [False, True, False]
  "datentime"   → spawnProc "gnome-calendar" []
  "alternative" → tglAlt

  "kbdlayout"   → fakeKeyEvent $
                    let reducer s acc = (xK_Shift_L, s) : (xK_Shift_R, s) : acc
                     in foldr reducer [] [False, True, False]

  _             → return ()

handleClickEvent _ _ = return ()


main ∷ IO ()
main = do
  initThreads

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
  sigHandlers ←
    let listen (member, stateModifier) =
          addMatch client (matchRule member) $ handle stateModifier

        matchRule member = basicMatchRule { matchMember = Just member }
        flag = fromMaybe False . fromVariant
        num  = fromMaybe 0     . fromVariant

        handle stateModifier (signalBody → [x]) = case variantType x of
          TypeBoolean → put $ Just $ stateModifier x
          TypeWord8   → put $ Just $ stateModifier x
          _           → return () -- Incorrect arguments, just ignoring it

        -- Incorrect arguments, just ignoring it
        handle _ _ = return ()

     in mapM listen

             -- Pairs of IPC method and state modifir
             [ ("numlock",     \x s → s { numLock     = flag x })
             , ("capslock",    \x s → s { capsLock    = flag x })
             , ("alternative", \x s → s { alternative = flag x })
             , ("xkblayout",   \x s → s { kbdLayout   = num  x })
             ]

  -- Fetcing date and time thread
  _ ← forkIO $ forever $ do
    (secondsLeftToNextMinute, utc, timeZone) ← fetchDateAndTime
    put $ Just $ \s → s { lastTime = Just (utc, timeZone) }
    threadDelay $ ceiling $ secondsLeftToNextMinute * 1000 * 1000

  let _busName = busName_ $ "com.github.unclechu.xlib_keys_hack." ++ dpyView
      _iface   = "com.github.unclechu.xlib_keys_hack"

      handleEv = handleClickEvent $
        emit client (signal "/" _iface "toggle_alternative_mode")
                      { signalSender      = Just busName
                      , signalDestination = Just _busName
                      , signalBody        = []
                      }

  -- Reading click events from i3-bar
  _ ← forkIO $ do
    !"[" ← hGetLine stdin -- Opening of lazy list
    do -- First one (without comma)
      Just ev ← decodeStrict <$> hGetLine stdin
      handleEv ev
    forever $ do
      Just ev ← (\(uncons → Just (',', x)) → decodeStrict x) <$> hGetLine stdin
      handleEv ev

  -- Handle POSIX signals to terminate application
  let terminate = do mapM_ (removeMatch client) sigHandlers
                     _ ← releaseName client busName
                     disconnect client
                     put Nothing

      catch sig = installHandler sig (Catch terminate) Nothing

   in mapM_ catch [sigHUP, sigINT, sigTERM, sigPIPE]

  dieWithParent -- make this app die if parent die

  echo $ encode (def ∷ ProtocolInitialization) { clickEvents = True }
  echo "[" -- Opening of lazy list

  -- Main thread is reactive loop that gets state modifier from another thread
  -- to update the state and re-render it (if it's Just) or terminate the
  -- application (it it's Nothing).
  let handle ∷ State → Maybe (State → State) → IO State
      handle prevState Nothing = prevState <$ echo "]" >> exitSuccess

      handle prevState (Just stateModifier) =
        let newState = stateModifier prevState
         in if newState ≡ prevState
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

renderDate ∷ FormatTime t ⇒ t → String
renderDate = formatTime defaultTimeLocale dateFormat

(<&>) ∷ Functor f ⇒ f a → (a → b) → f b
(<&>) = flip (<$>)

spawnProc ∷ FilePath → [String] → IO ()
spawnProc cmd args = () <$ createProcess (proc cmd args)
  { std_in      = NoStream
  , std_out     = NoStream
  , std_err     = NoStream
  , new_session = True
  }
