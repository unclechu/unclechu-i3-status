-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- | Listening to input events
module UnclechuI3Status.EventSubscriber.InputEvents
     ( ClickEvent (..)
     , subscribeToClickEvents
     ) where

import "base" GHC.Generics (Generic)
import Prelude hiding (getLine)

import "aeson" Data.Aeson (FromJSON (..), decodeStrict, genericParseJSON)
import "base" Data.Word (Word8)
import "bytestring" Data.ByteString.Char8 (getLine, uncons)

import "base" Control.Monad (forever)
import qualified "async" Control.Concurrent.Async as Async

-- Local imports

import UnclechuI3Status.Utils
import UnclechuI3Status.Utils.Aeson (withFieldNamer)


-- | Reading click events from i3-bar
subscribeToClickEvents ∷ (ClickEvent → IO ()) → IO (Async.Async ())
subscribeToClickEvents eventCallback = Async.async $ do
  "[" ← getLine -- Opening of lazy list

  do -- First one (without comma)
    Just ev ← decodeStrict <$> getLine
    eventCallback ev

  forever $ do
    Just ev ← getLine <&> \(uncons → Just (',', x)) → decodeStrict x
    eventCallback ev


-- * Types

data ClickEvent
  = ClickEvent
  { name ∷ Maybe String
  , _instance ∷ Maybe String
  , button ∷ Word8
  , _x ∷ Int
  , _y ∷ Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ClickEvent where
  parseJSON = genericParseJSON $ withFieldNamer f
    where f ('_' : xs) = xs; f x = x
