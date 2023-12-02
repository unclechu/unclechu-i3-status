-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.IPC.Types.XmonadrcIfaceParams
  ( XmonadrcIfaceParams (..)
  ) where

import "data-default" Data.Default (Default (def))
import qualified "dbus" DBus

import UnclechuI3Status.Utils

data XmonadrcIfaceParams
  = XmonadrcIfaceParams
  { objPath ∷ DBus.ObjectPath
  , flushObjPath ∷ String → DBus.ObjectPath
  , busName ∷ String → DBus.BusName
  , interfaceName ∷ DBus.InterfaceName
  }

instance Default XmonadrcIfaceParams where
  def
    = XmonadrcIfaceParams
    { objPath = "/"
    , flushObjPath  = DBus.objectPath_ ∘ ("/com/github/unclechu/xmonadrc/" ⋄)
    , busName = DBus.busName_ ∘ ("com.github.unclechu.xmonadrc." ⋄)
    , interfaceName = "com.github.unclechu.xmonadrc"
    }
