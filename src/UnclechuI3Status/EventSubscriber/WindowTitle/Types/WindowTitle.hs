-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.WindowTitle.Types.WindowTitle
  ( WindowTitle (..)
  , mkWindowTitle
  ) where

import qualified UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventContainerWindowProperties as WindowProperties
import UnclechuI3Status.Utils

newtype WindowTitle = WindowTitle { unWindowTitle ∷ String }
  deriving (Show, Eq)

-- | Make "WindowTitle" out of window properties
mkWindowTitle ∷ WindowProperties.HasWindowProperties a ⇒ a → Maybe WindowTitle
mkWindowTitle
  = WindowProperties.getWindowProperties
  • (>>= WindowProperties.title)
  • fmap WindowTitle
