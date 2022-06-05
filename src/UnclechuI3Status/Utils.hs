-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.Utils
     ( (•), (⋄), (<&!>)
     , module Prelude.Unicode
     , module Data.Function
     , module Data.Functor
     , echo
     , getDisplayName
     , spawnProc

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
import "bytestring" Data.ByteString.Lazy.Char8 (ByteString, putStrLn)

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
