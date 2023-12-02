-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventContainer
  ( EventContainer (..)
  ) where

import qualified "aeson" Data.Aeson as J
import "base" Data.Int (Int64)
import "base" GHC.Generics (Generic)

import qualified UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventContainerWindowProperties as WindowProperties
import UnclechuI3Status.Utils.Aeson (withFieldNamer)

data EventContainer
  = EventContainer
  { id ∷ Int64
  , _type ∷ String
  , focused ∷ Bool
  , urgent ∷ Bool
  , output ∷ String
  , layout ∷ String
  , name ∷ Maybe String
  , window ∷ Int64
  , windowProperties ∷ Maybe WindowProperties.EventContainerWindowProperties
  , sticky ∷ Bool
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON EventContainer where
  parseJSON = J.genericParseJSON $ withFieldNamer f where
    f ('_' : xs) = xs; f x = x

instance WindowProperties.HasWindowProperties EventContainer where
  getWindowProperties = windowProperties
