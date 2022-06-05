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
import "base" Data.Bifunctor (first)
import "base" Data.IORef (newIORef, readIORef, writeIORef)
import "base" Data.Word (Word32)
import "data-default" Data.Default (Default (def))

import "base" Control.Monad (void, join)
import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
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

import qualified "dbus" DBus

-- local imports

import UnclechuI3Status.EventSubscriber.Battery (subscribeToBatteryChargeUpdates)
import UnclechuI3Status.EventSubscriber.DateTime (subscribeToDateTimeUpdates)
import UnclechuI3Status.EventSubscriber.InputEvents (subscribeToClickEvents)
import UnclechuI3Status.Handler.AppState (State (..), appStateHandler)
import UnclechuI3Status.ParentProc (dieWithParent)
import UnclechuI3Status.Utils
import UnclechuI3Status.Utils.Aeson (withFieldNamer)
import UnclechuI3Status.X (initThreads)

import UnclechuI3Status.EventSubscriber.WindowTitle
  ( WindowTitle (..)
  , subscribeToFocusedWindowTitleUpdates
  )

import UnclechuI3Status.EventSubscriber.IPC
  ( IPCEvent (..)
  , XmonadrcIfaceParams (..)
  , XlibKeysHackIfaceParams (..)
  , subscribeToIPCEvents
  )

import UnclechuI3Status.Handler.InputEvents
  ( HandleClickEventInterface (..)
  , handleClickEvent
  )


-- | A wrapper to avoid impredicative polymorphism limitation
newtype WithDisplayMarker
  = WithDisplayMarker
  { unWithDisplayMarker ∷ ∀a. (String → a) → a
  }


main ∷ IO ()
main = do
  initThreads

  withDisplayMarker ← do
    dpy ← openDisplay ""
    let !x = getDisplayName dpy
    closeDisplay dpy $> WithDisplayMarker ($ x)

  stateChangeMVar ← newEmptyMVar

  let put = putMVar stateChangeMVar

  (ipcEmitSignal, ipcEventsThreadHandle) ←
    subscribeToIPCEvents (unWithDisplayMarker withDisplayMarker) $ \case
      NumLock x → put ∘ Just $ \s → s { numLock = x }
      CapsLock x → put ∘ Just $ \s → s { capsLock = x }
      KbdLayout x → put ∘ Just $ \s → s { kbdLayout = Just ∘ first Just $ x }
      Alternative x → put ∘ Just $ \s → s { alternative = x }

  (initialDateAndTime, _dateAndTimeThreadHandle) ←
    subscribeToDateTimeUpdates $ \(utc, timeZone) →
      put ∘ Just $ \s → s { lastTime = Just (utc, timeZone) }

  !batteryChargeUpdatesSubscription ←
    subscribeToBatteryChargeUpdates $ \case
      (Nothing, Nothing) → pure ()

      (Just chargeLeft, Just chargeState) →
        put ∘ Just $ \s → s { battery = Just (chargeLeft, chargeState) }

      (Just chargeLeft, Nothing) →
        put ∘ Just $ \s → s
          { battery = battery s >>= Just ∘ (chargeLeft,) ∘ snd }

      (Nothing, Just chargeState) →
        put ∘ Just $ \s → s
          { battery = battery s >>= Just ∘ (,chargeState) ∘ fst }

  (initialFocusedWindowTitle, focusedWindowTitleThreadHandle) ←
    subscribeToFocusedWindowTitleUpdates $
      \x → put ∘ Just $ \s → s { windowTitle = unWindowTitle <$> x }

  let
    defState ∷ State
    defState
      = def
      { battery = fst <$> batteryChargeUpdatesSubscription
      , windowTitle = unWindowTitle <$> initialFocusedWindowTitle
      , lastTime = Just initialDateAndTime
      }

  stateRef ← newIORef defState

  _clickEventsThreadHandle ←
    subscribeToClickEvents . handleClickEvent $
      let
        toggleAlternativeMode' = do
          (newAlternativeState ∷ Word32) ←
            readIORef stateRef <&> alternative <&> \case
              Nothing     → 1
              Just (1, _) → 2
              _           → 0

          ipcEmitSignal
            ( DBus.signal
                (objPath (def ∷ XlibKeysHackIfaceParams))
                (interfaceName (def ∷ XlibKeysHackIfaceParams))
                "switch_alternative_mode"
            )
            { DBus.signalSender
                = Just ∘ unWithDisplayMarker withDisplayMarker
                $ busName (def ∷ XmonadrcIfaceParams)
            , DBus.signalDestination
                = Just ∘ unWithDisplayMarker withDisplayMarker
                $ busName (def ∷ XlibKeysHackIfaceParams)
            , DBus.signalBody = [DBus.toVariant newAlternativeState]
            }
      in
        HandleClickEventInterface
          { toggleAlternativeMode = toggleAlternativeMode'
          , getCurrentKbdLayout
              = readIORef stateRef <&> kbdLayout
              • fmap (either (const Nothing) Just) • join
          }

  -- Handle POSIX signals to terminate application
  let
    terminate = do
      maybe (pure ()) snd batteryChargeUpdatesSubscription
      Async.uninterruptibleCancel focusedWindowTitleThreadHandle
      Async.uninterruptibleCancel ipcEventsThreadHandle
      put Nothing

    catch sig = installHandler sig (Catch terminate) Nothing

    in mapM_ catch [sigHUP, sigINT, sigTERM, sigPIPE]

  dieWithParent -- Make this app die if parent die

  dzen'
    ← newIORef Nothing <&>
    \ ref text color → void ∘ Async.async $ dzen ref text color

  echo $ encode (def ∷ ProtocolInitialization) { clickEvents = True }
  appStateHandler dzen' (takeMVar stateChangeMVar) (writeIORef stateRef) defState


-- * Types

data ProtocolInitialization
   = ProtocolInitialization
   { version ∷ Word
   , stopSignal ∷ Maybe Int
   , contSignal ∷ Maybe Int
   , clickEvents ∷ Bool
   } deriving (Show, Eq, Generic)

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
