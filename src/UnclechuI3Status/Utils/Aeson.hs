{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.Utils.Aeson
     ( withFieldNamer
     ) where

import "base-unicode-symbols" Prelude.Unicode

import "aeson" Data.Aeson (defaultOptions)
import "aeson" Data.Aeson.Types (Options (fieldLabelModifier), camelTo2)


withFieldNamer ∷ (String → String) → Options
withFieldNamer f = defaultOptions { fieldLabelModifier = f ∘ camelTo2 '_' }
