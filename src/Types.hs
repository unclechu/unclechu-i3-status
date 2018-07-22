-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Types
     ( State (..)
     , ProtocolInitialization (..)
     , Unit (..)
     , ClickEvent (..)
     , XmonadrcIfaceParams (..)
     , XlibKeysHackIfaceParams (..)
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

import "qm-interpolated-string" Text.InterpolatedString.QM (qm)

import "dbus" DBus (ObjectPath, BusName, InterfaceName)
import qualified "dbus" DBus as DBus


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
   } deriving (Show, Eq)

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
   } deriving (Show, Eq, Generic)

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
   } deriving (Show, Eq, Generic)

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
   } deriving (Show, Eq, Generic)

instance FromJSON ClickEvent where
  parseJSON = genericParseJSON $ withFieldNamer f
    where f ('_':xs) = xs; f x = x


data XmonadrcIfaceParams
   = XmonadrcIfaceParams
   { objPath       ∷ ObjectPath
   , flushObjPath  ∷ String → ObjectPath
   , busName       ∷ String → BusName
   , interfaceName ∷ InterfaceName
   }

instance Default XmonadrcIfaceParams where
  def
    = XmonadrcIfaceParams
    { objPath       = "/"

    , flushObjPath  = \d → DBus.objectPath_
                             [qm| /com/github/unclechu/xmonadrc/{d} |]

    , busName       = \d → DBus.busName_
                             [qm| com.github.unclechu.xmonadrc.{d} |]

    , interfaceName = "com.github.unclechu.xmonadrc"
    }


data XlibKeysHackIfaceParams
   = XlibKeysHackIfaceParams
   { objPath       ∷ ObjectPath
   , busName       ∷ String → BusName
   , interfaceName ∷ InterfaceName
   }

instance Default XlibKeysHackIfaceParams where
  def
    = XlibKeysHackIfaceParams
    { objPath       = "/"

    , busName       = \d → DBus.busName_
                             [qm| com.github.unclechu.xlib_keys_hack.{d} |]

    , interfaceName = "com.github.unclechu.xlib_keys_hack"
    }


withFieldNamer ∷ (String → String) → Options
withFieldNamer f = defaultOptions { fieldLabelModifier = f ∘ camelTo2 '_' }
