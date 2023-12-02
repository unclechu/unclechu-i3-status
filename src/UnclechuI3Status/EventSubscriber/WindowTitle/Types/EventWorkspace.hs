-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventWorkspace
  ( EventWorkspace (..)
  ) where

import qualified "aeson" Data.Aeson as J
import "base" Data.Int (Int64)
import "base" GHC.Generics (Generic)

import UnclechuI3Status.Utils.Aeson (withFieldNamer)
import UnclechuI3Status.EventSubscriber.WindowTitle.Types.WindowTree (WindowTree)

data EventWorkspace
  = EventWorkspace
  { id ∷ Int64
  , _type ∷ String
  , focused ∷ Bool
  , urgent ∷ Bool
  , output ∷ String
  , layout ∷ String
  , name ∷ String
  , nodes ∷ [WindowTree]
  , sticky ∷ Bool
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON EventWorkspace where
  parseJSON = J.genericParseJSON $ withFieldNamer f where
    f ('_' : xs) = xs; f x = x
