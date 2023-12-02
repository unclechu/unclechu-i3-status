-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.WindowTitle.Types.WindowTree
  ( WindowTree (..)
  ) where

import qualified "aeson" Data.Aeson as J
import "base" Data.Int (Int64)
import "base" GHC.Generics (Generic)

import qualified UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventContainerWindowProperties as WindowProperties
import UnclechuI3Status.Utils.Aeson (withFieldNamer)

-- | i3 windows tree
data WindowTree
  = WindowTree
  { id ∷ Int64
  , focused ∷ Bool
  , urgent ∷ Bool
  , layout ∷ String
  , output ∷ Maybe String
  , windowProperties ∷ Maybe WindowProperties.EventContainerWindowProperties
  , nodes ∷ [WindowTree]
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON WindowTree where
  parseJSON = J.genericParseJSON $ withFieldNamer Prelude.id

instance WindowProperties.HasWindowProperties WindowTree where
  getWindowProperties = windowProperties
