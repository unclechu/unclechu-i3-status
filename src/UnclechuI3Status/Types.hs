-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax, PackageImports, LambdaCase, MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module UnclechuI3Status.Types
     ( State (..)
     , ProtocolInitialization (..)
     , Unit (..)
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
import "base"         Data.Word (Word, Word8)
import "data-default" Data.Default (Default (def))
import "aeson"        Data.Aeson.Types ( Options (fieldLabelModifier)
                                       , camelTo2
                                       , typeMismatch
                                       )
import "time"         Data.Time.Clock (UTCTime)
import "time"         Data.Time.LocalTime (TimeZone)
import "aeson"        Data.Aeson ( Value (Object)
                                 , ToJSON (toJSON)
                                 , FromJSON (parseJSON)
                                 , defaultOptions
                                 , genericToJSON
                                 , genericParseJSON
                                 )
import qualified "unordered-containers" Data.HashMap.Strict as HM

import qualified "dbus" DBus
import qualified "dbus" DBus.Internal.Types as DBusInternal

import UnclechuI3Status.Utils


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
   , windowTitle ∷ Maybe String
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
    , windowTitle = Nothing
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
  toJSON = genericToJSON $ withFieldNamer Prelude.id


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
    case HM.lookup "change" obj of
         Nothing → typeMismatch "ChangeEvent" json

         Just "focus" → do
           tag ←
             if | HM.member "container" obj → pure "WindowFocusEvent"
                | HM.member "current"   obj → pure "WorkspaceFocusEvent"
                | otherwise                 → typeMismatch "ChangeEvent" json

           genericParseJSON defaultOptions $
             Object $ HM.insert "tag" tag obj

         Just "title" →
           genericParseJSON defaultOptions $
             Object $ HM.insert "tag" "WindowTitleEvent" obj

         Just "close" →
           genericParseJSON defaultOptions $
             Object $ HM.insert "tag" "WindowCloseEvent" obj

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


withFieldNamer ∷ (String → String) → Options
withFieldNamer f = defaultOptions { fieldLabelModifier = f ∘ camelTo2 '_' }
