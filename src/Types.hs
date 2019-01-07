-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Types
     ( State (..)
     , ProtocolInitialization (..)
     , Unit (..)
     , ClickEvent (..)
     , XmonadrcIfaceParams (..)
     , XlibKeysHackIfaceParams (..)
     , UPowerBatteryState (..)
     ) where

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

import qualified "dbus" DBus
import qualified "dbus" DBus.Internal.Types as DBusInternal

import Utils


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
   , battery     ∷ Maybe (Double, UPowerBatteryState)
   } deriving (Show, Eq)

instance Default State where
  def
    = State
    { numLock     = False
    , capsLock    = False
    , alternative = False
    , kbdLayout   = 0
    , lastTime    = Nothing
    , battery     = Nothing
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
   { objPath       ∷ DBus.ObjectPath
   , flushObjPath  ∷ String → DBus.ObjectPath
   , busName       ∷ String → DBus.BusName
   , interfaceName ∷ DBus.InterfaceName
   }

instance Default XmonadrcIfaceParams where
  def
    = XmonadrcIfaceParams
    { objPath       = "/"
    , flushObjPath  = DBus.objectPath_ ∘ ("/com/github/unclechu/xmonadrc/" ◇)
    , busName       = DBus.busName_ ∘ ("com.github.unclechu.xmonadrc." ◇)
    , interfaceName = "com.github.unclechu.xmonadrc"
    }


data XlibKeysHackIfaceParams
   = XlibKeysHackIfaceParams
   { objPath       ∷ DBus.ObjectPath
   , busName       ∷ String → DBus.BusName
   , interfaceName ∷ DBus.InterfaceName
   }

instance Default XlibKeysHackIfaceParams where
  def
    = XlibKeysHackIfaceParams
    { objPath       = "/"
    , busName       = DBus.busName_ ∘ ("com.github.unclechu.xlib_keys_hack." ◇)
    , interfaceName = "com.github.unclechu.xlib_keys_hack"
    }


data UPowerBatteryState
   = Unknown
   | Charging
   | Discharging
   | Empty
   | FullyCharged
   | PendingCharge
   | PendingDischarge
     deriving (Eq, Show)

instance Enum UPowerBatteryState where
  toEnum = \case
    1 → Charging
    2 → Discharging
    3 → Empty
    4 → FullyCharged
    5 → PendingCharge
    6 → PendingDischarge
    _ → Unknown

  fromEnum = \case
    Unknown          → 0
    Charging         → 1
    Discharging      → 2
    Empty            → 3
    FullyCharged     → 4
    PendingCharge    → 5
    PendingDischarge → 6

instance DBus.IsVariant UPowerBatteryState where
  toVariant
    = DBusInternal.Variant
    ∘ DBusInternal.ValueAtom
    ∘ DBusInternal.AtomWord32
    ∘ fromIntegral
    ∘ fromEnum

  fromVariant ( DBusInternal.Variant
              ( DBusInternal.ValueAtom
              ( DBusInternal.AtomWord32 x ))) = Just $ toEnum $ fromIntegral x
  fromVariant _ = Nothing


withFieldNamer ∷ (String → String) → Options
withFieldNamer f = defaultOptions { fieldLabelModifier = f ∘ camelTo2 '_' }
