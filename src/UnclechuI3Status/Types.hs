-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax, PackageImports, LambdaCase, MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module UnclechuI3Status.Types
     ( ProtocolInitialization (..)
     , ClickEvent (..)
     , XmonadrcIfaceParams (..)
     , XlibKeysHackIfaceParams (..)
     , UPowerBatteryState (..)
     , ChangeEvent (..)
     , EventContainer (..)
     , EventContainerWindowProperties (..)
     , EventWorkspace (..)
     , WindowTree (..)
     ) where

import "base" GHC.Generics (Generic)

import "base"         Data.Int (Int64)
import "base"         Data.Word (Word8)
import "data-default" Data.Default (Default (def))
import "aeson"        Data.Aeson.Types (typeMismatch)
import "aeson"        Data.Aeson ( Value (Object)
                                 , ToJSON (toJSON)
                                 , FromJSON (parseJSON)
                                 , defaultOptions
                                 , genericToJSON
                                 , genericParseJSON
                                 )
import qualified "aeson" Data.Aeson.KeyMap as KM

import qualified "dbus" DBus
import qualified "dbus" DBus.Internal.Types as DBusInternal

import UnclechuI3Status.Utils
import UnclechuI3Status.Utils.Aeson (withFieldNamer)


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
  toJSON = genericToJSON $ withFieldNamer Prelude.id


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
    , flushObjPath  = DBus.objectPath_ ∘ ("/com/github/unclechu/xmonadrc/" ⋄)
    , busName       = DBus.busName_ ∘ ("com.github.unclechu.xmonadrc." ⋄)
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
    , busName       = DBus.busName_ ∘ ("com.github.unclechu.xlib_keys_hack." ⋄)
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


data ChangeEvent
   = WindowFocusEvent
   { container ∷ EventContainer
   }
   | WindowTitleEvent
   { container ∷ EventContainer
   }
   | WindowCloseEvent
   { container ∷ EventContainer
   }
   | WorkspaceFocusEvent
   { current ∷ EventWorkspace
   , old ∷ Maybe EventWorkspace
   }
   | OtherEvent Value
     deriving (Show, Eq, Generic)

instance FromJSON ChangeEvent where
  parseJSON json@(Object obj) =
    case KM.lookup "change" obj of
         Nothing → typeMismatch "ChangeEvent" json

         Just "focus" → do
           tag ←
             if | KM.member "container" obj → pure "WindowFocusEvent"
                | KM.member "current"   obj → pure "WorkspaceFocusEvent"
                | otherwise                 → typeMismatch "ChangeEvent" json

           genericParseJSON defaultOptions $
             Object $ KM.insert "tag" tag obj

         Just "title" →
           genericParseJSON defaultOptions $
             Object $ KM.insert "tag" "WindowTitleEvent" obj

         Just "close" →
           genericParseJSON defaultOptions $
             Object $ KM.insert "tag" "WindowCloseEvent" obj

         Just _ → pure $ OtherEvent json

  parseJSON json = typeMismatch "ChangeEvent" json


data EventContainer
   = EventContainer
   { id ∷ Int64
   , _type ∷ String
   , focused ∷ Bool
   , urgent ∷ Bool
   , output ∷ String
   , layout ∷ String
   , name ∷ String
   , window ∷ Int64
   , windowProperties ∷ EventContainerWindowProperties
   , sticky ∷ Bool
   } deriving (Show, Eq, Generic)

instance FromJSON EventContainer where
  parseJSON = genericParseJSON $ withFieldNamer f where
    f ('_':xs) = xs
    f x        = x

data EventContainerWindowProperties
   = EventContainerWindowProperties
   { _class ∷ String
   , _instance ∷ String
   , title ∷ String
   , windowRole ∷ Maybe String
   } deriving (Show, Eq, Generic)

instance FromJSON EventContainerWindowProperties where
  parseJSON = genericParseJSON $ withFieldNamer f where
    f ('_':xs) = xs
    f x        = x


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
   } deriving (Show, Eq, Generic)

instance FromJSON EventWorkspace where
  parseJSON = genericParseJSON $ withFieldNamer f where
    f ('_':xs) = xs
    f x        = x


-- | i3 windows tree
data WindowTree
   = WindowTree
   { id ∷ Int64
   , focused ∷ Bool
   , urgent ∷ Bool
   , layout ∷ String
   , output ∷ Maybe String
   , windowProperties ∷ Maybe EventContainerWindowProperties
   , nodes ∷ [WindowTree]
   } deriving (Show, Eq, Generic)

instance FromJSON WindowTree where
  parseJSON = genericParseJSON $ withFieldNamer Prelude.id
