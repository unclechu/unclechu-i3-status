-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module UnclechuI3Status.EventSubscriber.IPC
  ( subscribeToIPCEvents
  ) where

import "base" Data.Word (Word8)
import "data-default" Data.Default (Default (def))
import "qm-interpolated-string" Text.InterpolatedString.QM (qm)

import "base" Control.Arrow ((&&&))
import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import "base" Control.Exception (finally)
import "base" Control.Monad (when, void, forever)
import qualified "async" Control.Concurrent.Async as Async

import qualified "dbus" DBus
import qualified "dbus" DBus.Client

-- Local imports

import UnclechuI3Status.EventSubscriber.IPC.Types.IPCEvent (IPCEvent (..))
import qualified UnclechuI3Status.EventSubscriber.IPC.Types.XmonadrcIfaceParams as XmonadrcIfaceParams
import UnclechuI3Status.Layout (numToLayout)
import UnclechuI3Status.Utils


subscribeToIPCEvents
  ∷ (∀a. (String → a) → a)
  -- ^ Function that provides current display marker
  → (IPCEvent → IO ())
  -- ^ Value update callback
  → IO (DBus.Signal → IO (), Async.Async ())
  -- ^ Emit function and the thread handle
subscribeToIPCEvents withDisplayMarker eventCallback = do
  client ← DBus.Client.connectSession

  do -- Grab the bus name for the application
    let name = withDisplayMarker $ XmonadrcIfaceParams.busName def
    reply ← DBus.Client.requestName client name []
    when (reply ≢ DBus.Client.NamePrimaryOwner) $
      fail [qm| Requesting name '{name}' error: {reply} |]

  -- Listen to the events
  signalHandlers ←
    let
      listen member handler
        = (DBus.signalBody • handler)
        & DBus.Client.addMatch
            client
            basicMatchRule { DBus.Client.matchMember = Just member }
    in
      sequence
        [ listen "numlock" $ \case
            [DBus.fromVariant → Just (x ∷ Bool)] → eventCallback ∘ NumLock $ x
            _ → pure () -- Ignore incorrect arguments

        , listen "capslock" $ \case
            [DBus.fromVariant → Just (x ∷ Bool)] → eventCallback ∘ CapsLock $ x
            _ → pure () -- Ignore incorrect arguments

        , listen "xkblayout" $ \case
            [DBus.fromVariant → Just (x ∷ Word8)] →
              eventCallback ∘ KbdLayout ∘ maybe (Left x) Right ∘ numToLayout $ x
            _ → pure () -- Ignore incorrect arguments

        , listen "alternative_level" $ \case
            [ DBus.fromVariant → Just (level ∷ Word8),
              DBus.fromVariant → Just (isPermanent ∷ Bool) ] →
              eventCallback ∘ Alternative $
                if level > minBound
                then Just (level, isPermanent)
                else Nothing
            _ → pure () -- Ignore incorrect arguments
        ]

  -- If “xlib-keys-hack” started before ask it to reflush indicators.
  -- It’s important to put if *after* the listeners attachment.
  DBus.Client.emit client
    ( DBus.signal
        (withDisplayMarker $ XmonadrcIfaceParams.flushObjPath def)
        (XmonadrcIfaceParams.interfaceName def)
        "request_flush_all"
    )
    { DBus.signalSender =
        Just ∘ withDisplayMarker $ XmonadrcIfaceParams.busName def
    , DBus.signalDestination = Nothing
    , DBus.signalBody = []
    }

  (emitSignal, getNextEmitterTask) ← newEmptyMVar <&> putMVar &&& takeMVar

  threadHandle ← Async.async $
    let
      finalizer = do
        mapM_ (DBus.Client.removeMatch client) signalHandlers

        void -- Ignoring the reply, just trying to release the name
          ∘ DBus.Client.releaseName client
          ∘ withDisplayMarker
          $ XmonadrcIfaceParams.busName def

        DBus.Client.disconnect client

      handleTask = getNextEmitterTask >>= DBus.Client.emit client
    in
      forever @IO @() @() handleTask `finally` finalizer

  pure (emitSignal, threadHandle)

  where
    basicMatchRule = DBus.Client.matchAny
      { DBus.Client.matchPath =
          Just $ XmonadrcIfaceParams.objPath def
      , DBus.Client.matchInterface =
          Just $ XmonadrcIfaceParams.interfaceName def
      , DBus.Client.matchDestination =
          Just ∘ withDisplayMarker $ XmonadrcIfaceParams.busName def
      }
