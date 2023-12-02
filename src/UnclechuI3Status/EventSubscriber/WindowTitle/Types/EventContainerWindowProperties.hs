-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventContainerWindowProperties
  ( EventContainerWindowProperties (..)
  , HasWindowProperties (..)
  ) where

import qualified "aeson" Data.Aeson as J
import "base" GHC.Generics (Generic)

import UnclechuI3Status.Utils.Aeson (withFieldNamer)

data EventContainerWindowProperties
  = EventContainerWindowProperties
  { _class ∷ Maybe String
  , _instance ∷ Maybe String
  , title ∷ Maybe String
  , windowRole ∷ Maybe String
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON EventContainerWindowProperties where
  parseJSON = J.genericParseJSON $ withFieldNamer f where
    f ('_' : xs) = xs; f x = x

class HasWindowProperties a where
  getWindowProperties ∷ a → Maybe EventContainerWindowProperties
