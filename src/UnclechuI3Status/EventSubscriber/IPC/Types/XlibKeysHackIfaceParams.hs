-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.IPC.Types.XlibKeysHackIfaceParams
  ( XlibKeysHackIfaceParams (..)
  ) where

import "data-default" Data.Default (Default (def))
import qualified "dbus" DBus

import UnclechuI3Status.Utils

data XlibKeysHackIfaceParams
  = XlibKeysHackIfaceParams
  { objPath ∷ DBus.ObjectPath
  , busName ∷ String → DBus.BusName
  , interfaceName ∷ DBus.InterfaceName
  }

instance Default XlibKeysHackIfaceParams where
  def
    = XlibKeysHackIfaceParams
    { objPath = "/"
    , busName = DBus.busName_ ∘ ("com.github.unclechu.xlib_keys_hack." ⋄)
    , interfaceName = "com.github.unclechu.xlib_keys_hack"
    }
