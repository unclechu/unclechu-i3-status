-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | "Layout" data-type and related functions
module UnclechuI3Status.Layout
     ( Layout (..)
     , numToLayout
     , colorOfLayout
     ) where

import "base" Data.List (find)

-- Local imports

import UnclechuI3Status.Utils


-- | Layouts definition
--
-- The order defines here the mapping to the system layout number.
-- The name of the constructor is used for printing the layout name.
data Layout = US | RU | FI deriving (Eq, Show, Enum, Bounded)


numToLayout ∷ ∀ α. (Num α, Enum α, Bounded α, Eq α) ⇒ α → Maybe Layout
numToLayout n = zip [0..] [minBound..maxBound] & find (fst • (≡ n)) & fmap snd


colorOfLayout ∷ Layout → String
colorOfLayout = \case US → "#ff0000"; RU → "#00ff00"; FI → "#0000ff"
