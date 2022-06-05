-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax, PackageImports, LambdaCase, ScopedTypeVariables #-}

module UnclechuI3Status.Utils
     ( (•), (⋄), (<&!>)
     , module Prelude.Unicode
     , module Data.Function
     , module Data.Functor
     , echo
     , renderDate
     , getDisplayName
     , spawnProc

     , Layout (..)
     , numToLayout
     , colorOfLayout

     , showNumLock
     , colorOfNumLock

     , showCapsLock
     , colorOfCapsLock

     , showAlternativeState
     , colorOfAlternativeState
     ) where

import Prelude hiding (putStrLn)
import "base-unicode-symbols" Prelude.Unicode

import "base" Data.Word (Word8)
import "base" Data.Bool (bool)
import "base" Data.Function ((&))
import "base" Data.Functor ((<&>), ($>))
import "base" Data.List (find)
import "bytestring" Data.ByteString.Lazy.Char8 (ByteString, putStrLn)

import "time" Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)

import "base" Control.Monad ((<$!>), void)

import "base" System.IO (IOMode (ReadWriteMode), stdout, hFlush, openFile)

import "process" System.Process
  ( CreateProcess (std_in, std_out, std_err, new_session)
  , StdStream (UseHandle)
  , proc
  , createProcess
  )

import "X11" Graphics.X11.Xlib (Display, displayString)


(•) ∷ (α → β) → (β → γ) → α → γ; (•) = flip (∘); {-# INLINE (•) #-}; infixl 9 •
(⋄) ∷ Semigroup α ⇒ α → α → α;   (⋄) = (<>);     {-# INLINE (⋄) #-}; infixr 6 ⋄

(<&!>) ∷ Monad φ ⇒ φ α → (α → β) → φ β
(<&!>) = flip (<$!>)
{-# INLINE (<&!>) #-}
infixl 1 <&!>


echo ∷ ByteString → IO ()
echo s = putStrLn s >> hFlush stdout

renderDate ∷ FormatTime t ⇒ t → String
renderDate = formatTime defaultTimeLocale dateFormat
  where dateFormat ∷ String
        dateFormat = "%A %-d %B %H:%M"

getDisplayName ∷ Display → String
getDisplayName dpy = f <$> displayString dpy
  where f ':' = '_'
        f '.' = '_'
        f  x  =  x

spawnProc ∷ FilePath → [String] → IO ()
spawnProc cmd args = do
  devNull <- openFile "/dev/null" ReadWriteMode
  void $ createProcess (proc cmd args)
    { std_in      = UseHandle devNull
    , std_out     = UseHandle devNull
    , std_err     = UseHandle devNull
    , new_session = True
    }


data Layout = US | RU | FI deriving (Eq, Show, Enum, Bounded)

numToLayout ∷ ∀ α. (Num α, Enum α, Bounded α, Eq α) ⇒ α → Maybe Layout
numToLayout n = zip [0..] [minBound..maxBound] & find (fst • (≡ n)) & fmap snd

colorOfLayout ∷ Layout → String
colorOfLayout = \case US → "#ff0000"; RU → "#00ff00"; FI → "#0000ff"


showNumLock ∷ Bool → String
showNumLock = const "num"

colorOfNumLock ∷ Bool → String
colorOfNumLock = bool "#999999" "#eeeeee"


showCapsLock ∷ Bool → String
showCapsLock = bool "caps" "CAPS"

colorOfCapsLock ∷ Bool → String
colorOfCapsLock = bool "#999999" "#ff9900"


showAlternativeState ∷ Maybe (Word8, Bool) → Either Word8 String
showAlternativeState = go where
  text = bool "hax" "HAX"

  go = \case
    Nothing     → Right $ text False
    Just (1, p) → Right $ text p
    Just (2, p) → Right $ text p
    Just (n, _) → Left n

colorOfAlternativeState ∷ Maybe (Word8, Bool) → Either Word8 String
colorOfAlternativeState = \case
  Nothing     → Right "#999999"
  Just (1, _) → Right "#ffff00"
  Just (2, _) → Right "#00ffff"
  Just (n, _) → Left n
