-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | A collection of IPC signals
module UnclechuI3Status.IPC
     ( ipcSwitchAlternativeModeSignal
     ) where

import "base" Data.Word (Word32)
import "data-default" Data.Default (def)

import qualified "dbus" DBus

-- Local imports

import UnclechuI3Status.Utils

import UnclechuI3Status.EventSubscriber.IPC
  ( XmonadrcIfaceParams (..)
  , XlibKeysHackIfaceParams (..)
  )


-- | Switch alternative mode level to a given value
ipcSwitchAlternativeModeSignal
  ∷ (∀a. (String → a) → a)
  -- ^ Function that provides current display marker
  → Word32
  -- ^ Alternative mode level value (@0@ turns it off)
  → DBus.Signal
ipcSwitchAlternativeModeSignal withDisplayMarker newAlternativeState
  = (DBus.signal objPath' ifaceName memberName)
  { DBus.signalSender =
      Just ∘ withDisplayMarker $ busName (def ∷ XmonadrcIfaceParams)
  , DBus.signalDestination =
      Just ∘ withDisplayMarker $ busName (def ∷ XlibKeysHackIfaceParams)
  , DBus.signalBody = [DBus.toVariant newAlternativeState]
  }
  where
    objPath' = objPath (def ∷ XlibKeysHackIfaceParams)
    ifaceName = interfaceName (def ∷ XlibKeysHackIfaceParams)
    memberName = "switch_alternative_mode"
