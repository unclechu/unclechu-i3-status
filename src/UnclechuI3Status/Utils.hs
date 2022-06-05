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
     ) where

import Prelude hiding (putStrLn)
import "base-unicode-symbols" Prelude.Unicode

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


-- * Operators

(•) ∷ (α → β) → (β → γ) → α → γ; (•) = flip (∘); {-# INLINE (•) #-}; infixl 9 •
(⋄) ∷ Semigroup α ⇒ α → α → α;   (⋄) = (<>);     {-# INLINE (⋄) #-}; infixr 6 ⋄

(<&!>) ∷ Monad φ ⇒ φ α → (α → β) → φ β
(<&!>) = flip (<$!>)
{-# INLINE (<&!>) #-}
infixl 1 <&!>


-- * Helper functions

-- | Print a "ByteString" to stdout and flush it immediatelly
echo ∷ ByteString → IO ()
echo s = putStrLn s >> hFlush stdout


-- | Get a display name (printed number) with special symbols replaced with @_@
--   underscore so that you can use it as a part of some service names
--
-- Can be useful for defining DBus IPC scoped to a particular X11 session.
getDisplayName ∷ Display → String
getDisplayName dpy = go where
  go = f <$> displayString dpy

  f ':' = '_'
  f '.' = '_'
  f  x  =  x


-- | Spawn a process in fire-and-forget mode
spawnProc ∷ FilePath → [String] → IO ()
spawnProc cmd args = do
  devNull <- openFile "/dev/null" ReadWriteMode
  void $ createProcess (proc cmd args)
    { std_in = UseHandle devNull
    , std_out = UseHandle devNull
    , std_err = UseHandle devNull
    , new_session = True
    }
