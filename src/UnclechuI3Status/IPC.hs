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

import qualified UnclechuI3Status.EventSubscriber.IPC.Types.XmonadrcIfaceParams as XlibKeysHackIfaceParams
import qualified UnclechuI3Status.EventSubscriber.IPC.Types.XmonadrcIfaceParams as XmonadrcIfaceParams
import UnclechuI3Status.Utils


-- | Switch alternative mode level to a given value
ipcSwitchAlternativeModeSignal
  ∷ (∀a. (String → a) → a)
  -- ^ Function that provides current display marker
  → Word32
  -- ^ Alternative mode level value (@0@ turns it off)
  → DBus.Signal
ipcSwitchAlternativeModeSignal withDisplayMarker newAlternativeState = signal where
  objPath' = XlibKeysHackIfaceParams.objPath def
  ifaceName = XlibKeysHackIfaceParams.interfaceName def
  memberName = "switch_alternative_mode"

  signal
    = (DBus.signal objPath' ifaceName memberName)
    { DBus.signalSender =
        Just ∘ withDisplayMarker $ XmonadrcIfaceParams.busName def
    , DBus.signalDestination =
        Just ∘ withDisplayMarker $ XlibKeysHackIfaceParams.busName def
    , DBus.signalBody = [DBus.toVariant newAlternativeState]
    }
