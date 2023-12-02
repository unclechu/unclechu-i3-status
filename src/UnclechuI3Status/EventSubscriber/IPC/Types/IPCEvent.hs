-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.IPC.Types.IPCEvent
  ( IPCEvent (..)
  ) where

import "base" Data.Word (Word8)

import UnclechuI3Status.Layout (Layout (..))

data IPCEvent
  = NumLock Bool
  | CapsLock Bool
  | KbdLayout (Either Word8 Layout)
  -- ^ @Left@ when failed to parse @Layout@, providing the raw value instead
  | Alternative (Maybe (Word8, Bool))
  -- ^ @Nothing@ means alternative mode is turned off
  deriving (Show, Eq)
