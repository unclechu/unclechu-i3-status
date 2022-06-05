-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.Types
     ( ChangeEvent (..)
     , EventContainer (..)
     , EventContainerWindowProperties (..)
     , EventWorkspace (..)
     , WindowTree (..)
     ) where

import "base" GHC.Generics (Generic)

import "base"         Data.Int (Int64)
import "aeson"        Data.Aeson.Types (typeMismatch)
import "aeson"        Data.Aeson ( Value (Object)
                                 , FromJSON (parseJSON)
                                 , defaultOptions
                                 , genericParseJSON
                                 )
import qualified "aeson" Data.Aeson.KeyMap as KM

import UnclechuI3Status.Utils.Aeson (withFieldNamer)


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
