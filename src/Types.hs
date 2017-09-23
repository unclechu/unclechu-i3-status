-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types
  ( State (..)
  , ProtocolInitialization (..)
  , Unit (..)
  , ClickEvent (..)
  ) where

import "base-unicode-symbols" Prelude.Unicode
import "base" GHC.Generics (Generic)

import "base"         Data.Word (Word, Word8)
import "data-default" Data.Default (Default (def))
import "aeson"        Data.Aeson.Types (Options (fieldLabelModifier), camelTo2)
import "time"         Data.Time.Clock (UTCTime)
import "time"         Data.Time.LocalTime (TimeZone)
import "aeson"        Data.Aeson ( ToJSON (toJSON)
                                 , FromJSON (parseJSON)
                                 , defaultOptions
                                 , genericToJSON
                                 , genericParseJSON
                                 )


data State
  = State
  { numLock     ∷ Bool
  , capsLock    ∷ Bool
  , alternative ∷ Bool
  , kbdLayout   ∷ Word8
  , lastTime    ∷ Maybe (UTCTime, TimeZone)
                      -- ^ The reason why don't just store ZonedTime here
                      --   is that it doesn't have Eq instance.
                      --   See https://github.com/haskell/time/issues/50
  }

  deriving (Show, Eq)

instance Default State where
  def
    = State
    { numLock     = False
    , capsLock    = False
    , alternative = False
    , kbdLayout   = 0
    , lastTime    = Nothing
    }


data ProtocolInitialization
  = ProtocolInitialization
  { version     ∷ Word
  , stopSignal  ∷ Maybe Int
  , contSignal  ∷ Maybe Int
  , clickEvents ∷ Bool
  }

  deriving (Show, Eq, Generic)

instance Default ProtocolInitialization where
  def
    = ProtocolInitialization
    { version     = 1
    , stopSignal  = Nothing
    , contSignal  = Nothing
    , clickEvents = False
    }

instance ToJSON ProtocolInitialization where
  toJSON = genericToJSON $ withFieldNamer id


data Unit
  = Unit
  { fullText            ∷ String
  , shortText           ∷ Maybe String
  , color               ∷ Maybe String
  , background          ∷ Maybe String
  , border              ∷ Maybe String
  , minWidth            ∷ Maybe Word
  , align               ∷ Maybe String
  , name                ∷ Maybe String
  , _instance           ∷ Maybe String
  , urgent              ∷ Maybe Bool
  , separator           ∷ Maybe Bool
  , separatorBlockWidth ∷ Maybe Word
  , markup              ∷ Maybe String
  }

  deriving (Show, Eq, Generic)

instance Default Unit where
  def
    = Unit
    { fullText            = ""
    , shortText           = Nothing
    , color               = Just "#999999"
    , background          = Nothing
    , border              = Nothing
    , minWidth            = Nothing
    , align               = Nothing
    , name                = Nothing
    , _instance           = Nothing
    , urgent              = Nothing
    , separator           = Just False
    , separatorBlockWidth = Nothing
    , markup              = Just "none"
    }

instance ToJSON Unit where
  toJSON = genericToJSON $ withFieldNamer f
    where f ('_':xs) = xs; f x = x


data ClickEvent
  = ClickEvent
  { name      ∷ Maybe String
  , _instance ∷ Maybe String
  , button    ∷ Word8
  , _x        ∷ Int
  , _y        ∷ Int
  }

  deriving (Show, Eq, Generic)

instance FromJSON ClickEvent where
  parseJSON = genericParseJSON $ withFieldNamer f
    where f ('_':xs) = xs; f x = x


withFieldNamer ∷ (String → String) → Options
withFieldNamer f = defaultOptions { fieldLabelModifier = f ∘ camelTo2 '_' }
