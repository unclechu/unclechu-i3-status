-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventResolve
  ( EventResolve (..)
  ) where

import UnclechuI3Status.EventSubscriber.WindowTitle.Types.WindowTitle (WindowTitle)

data EventResolve
  = Ignore
  | FocusedWindowNotFound
  | FocusedWindowClosed
  | FocusedWindowTitle (Maybe WindowTitle)
  deriving (Show, Eq)
