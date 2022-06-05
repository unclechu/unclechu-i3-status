-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import "base" GHC.Generics (Generic)
import "base-unicode-symbols" Prelude.Unicode

import "aeson" Data.Aeson (ToJSON (..), genericToJSON, encode)
import "base" Data.Foldable (foldl')
import "base" Data.IORef (newIORef, readIORef, writeIORef)
import "data-default" Data.Default (Default (def))

import "base" Control.Arrow ((&&&))
import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import "base" Control.Exception (finally)
import "base" Control.Monad (join)
import qualified "async" Control.Concurrent.Async as Async

import "unix" System.Posix.Signals
  ( Handler (Catch)
  , installHandler
  , sigHUP
  , sigINT
  , sigTERM
  , sigPIPE
  )

import "X11" Graphics.X11.Xlib (openDisplay, closeDisplay)

-- Local imports

import UnclechuI3Status.Dzen (dzen)
import UnclechuI3Status.EventSubscriber.Battery (subscribeToBatteryChargeUpdates)
import UnclechuI3Status.EventSubscriber.DateTime (subscribeToDateTimeUpdates)
import UnclechuI3Status.EventSubscriber.IPC (IPCEvent (..), subscribeToIPCEvents)
import UnclechuI3Status.EventSubscriber.InputEvents (subscribeToClickEvents)
import UnclechuI3Status.Handler.AppState (State (..), appStateHandler)
import UnclechuI3Status.IPC (ipcSwitchAlternativeModeSignal)
import UnclechuI3Status.ParentProc (dieWithParent)
import UnclechuI3Status.Utils
import UnclechuI3Status.Utils.Aeson (withFieldNamer)
import UnclechuI3Status.X (initThreads)

import UnclechuI3Status.EventSubscriber.WindowTitle
  ( WindowTitle (..)
  , subscribeToFocusedWindowTitleUpdates
  )

import UnclechuI3Status.Handler.InputEvents
  ( HandleClickEventInterface (..)
  , handleClickEvent
  )


main ∷ IO ()
main = do
  initThreads

  withDisplayMarker ← do
    dpy ← openDisplay ""
    let !x = getDisplayName dpy
    closeDisplay dpy $> WithDisplayMarker ($ x)

  (putStateModification, getNextStateModification) ←
    (putMVar &&& takeMVar) <$> newEmptyMVar

  (ipcEmitSignal, ipcEventsThreadHandle) ←
    subscribeToIPCEvents (unWithDisplayMarker withDisplayMarker) $ \case
      NumLock x → putStateModification $ \s → s { numLock = x }
      CapsLock x → putStateModification $ \s → s { capsLock = x }
      KbdLayout x → putStateModification $ \s → s { kbdLayout = Just x }
      Alternative x → putStateModification $ \s → s { alternative = x }

  (initialDateAndTime, dateAndTimeThreadHandle) ←
    subscribeToDateTimeUpdates $ \(utc, timeZone) →
      putStateModification $ \s → s { lastTime = Just (utc, timeZone) }

  !batteryChargeUpdatesSubscription ←
    subscribeToBatteryChargeUpdates $ \case
      (Nothing, Nothing) → pure ()

      (Just chargeLeft, Just chargeState) →
        putStateModification $ \s → s
          { battery = Just (chargeLeft, chargeState) }

      (Just chargeLeft, Nothing) →
        putStateModification $ \s → s
          { battery = battery s >>= Just ∘ (chargeLeft,) ∘ snd }

      (Nothing, Just chargeState) →
        putStateModification $ \s → s
          { battery = battery s >>= Just ∘ (,chargeState) ∘ fst }

  (initialFocusedWindowTitle, focusedWindowTitleThreadHandle) ←
    subscribeToFocusedWindowTitleUpdates $
      \x → putStateModification $ \s → s { windowTitle = unWindowTitle <$> x }

  let
    defState ∷ State
    defState
      = def
      { battery = fst <$> batteryChargeUpdatesSubscription
      , windowTitle = unWindowTitle <$> initialFocusedWindowTitle
      , lastTime = Just initialDateAndTime
      }

  stateRef ← newIORef defState

  clickEventsThreadHandle ←
    subscribeToClickEvents . handleClickEvent $ HandleClickEventInterface
      { alternativeModeClickHandler =
          let
            newState =
              readIORef stateRef <&> alternative <&> \case
                Nothing     → 1
                Just (1, _) → 2
                _           → 0
            signal =
              ipcSwitchAlternativeModeSignal
                (unWithDisplayMarker withDisplayMarker)
          in
            ipcEmitSignal ∘ signal =<< newState

      , getCurrentKbdLayout
          = readIORef stateRef <&> kbdLayout
          • fmap (either (const Nothing) Just) • join
      }

  dieWithParent -- Make this app die if the parent process (i3 bar) dies

  dzenNotification
    ← newIORef Nothing <&>
    \ ref text color → fireAndForget $ dzen ref text color

  echo $ encode (def ∷ ProtocolInitialization) { clickEvents = True }

  appStateThreadHandle ←
    appStateHandler
      dzenNotification
        getNextStateModification
        (writeIORef stateRef)
        defState

  -- Handle POSIX signals to terminate application
  let
    terminateApplication
      = foldl' finally (pure ())
      [ maybe (pure ()) snd batteryChargeUpdatesSubscription
      , Async.uninterruptibleCancel focusedWindowTitleThreadHandle
      , Async.uninterruptibleCancel ipcEventsThreadHandle
      , Async.uninterruptibleCancel dateAndTimeThreadHandle
      , Async.uninterruptibleCancel clickEventsThreadHandle
      , Async.uninterruptibleCancel appStateThreadHandle
      ]

  mapM_
    (\sig → installHandler sig (Catch terminateApplication) Nothing)
    [sigHUP, sigINT, sigTERM, sigPIPE]

  Async.wait appStateThreadHandle


-- * Types

data ProtocolInitialization
  = ProtocolInitialization
  { version ∷ Word
  , stopSignal ∷ Maybe Int
  , contSignal ∷ Maybe Int
  , clickEvents ∷ Bool
  }
  deriving (Show, Eq, Generic)

instance Default ProtocolInitialization where
  def
    = ProtocolInitialization
    { version = 1
    , stopSignal = Nothing
    , contSignal = Nothing
    , clickEvents = False
    }

instance ToJSON ProtocolInitialization where
  toJSON = genericToJSON $ withFieldNamer id


-- | A wrapper to avoid impredicative polymorphism limitation
newtype WithDisplayMarker
  = WithDisplayMarker
  { unWithDisplayMarker ∷ ∀a. (String → a) → a
  }
