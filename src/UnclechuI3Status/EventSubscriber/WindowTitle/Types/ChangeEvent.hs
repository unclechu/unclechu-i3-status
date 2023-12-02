-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.WindowTitle.Types.ChangeEvent
  ( ChangeEvent (..)
  ) where

import qualified "aeson" Data.Aeson as J
import qualified "aeson" Data.Aeson.KeyMap as KM
import "aeson" Data.Aeson.Types (typeMismatch)
import "base" GHC.Generics (Generic)
import "base" Control.Applicative ((<|>))

import UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventContainer (EventContainer)
import UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventWorkspace (EventWorkspace)
import UnclechuI3Status.Utils

data ChangeEvent
  = WindowFocusEvent EventContainer
  | WindowTitleEvent EventContainer
  | WindowCloseEvent EventContainer
  | WorkspaceFocusEvent EventWorkspace
  | OtherEvent J.Value
  deriving (Show, Eq, Generic)

instance J.FromJSON ChangeEvent where
  parseJSON json@(J.Object obj) =
    case KM.lookup "change" obj of
      Nothing → mismatch
      Just "focus" →
        maybe mismatch (fmap WindowFocusEvent ∘ J.parseJSON) (KM.lookup containerKey obj)
        <|> maybe mismatch (fmap WorkspaceFocusEvent ∘ J.parseJSON) (KM.lookup "current" obj)
        -- ↑ When there’s “current” there can be also optional “old” value
      Just "title" →
        maybe mismatch (fmap WindowTitleEvent ∘ J.parseJSON) (KM.lookup containerKey obj)
      Just "close" →
        maybe mismatch (fmap WindowCloseEvent ∘ J.parseJSON) (KM.lookup containerKey obj)
      Just _ →
        pure $ OtherEvent json
    where
      containerKey = "container"
      mismatch = typeMismatch "ChangeEvent" json
  parseJSON json = typeMismatch "ChangeEvent" json
