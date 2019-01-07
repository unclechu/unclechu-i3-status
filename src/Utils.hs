{-# LANGUAGE UnicodeSyntax, PackageImports #-}

module Utils
     ( (×), (◇), (<&>), (<&!>)
     , module Prelude.Unicode
     , module Data.Function
     , echo
     , renderDate
     , getDisplayName
     , spawnProc
     ) where

import Prelude hiding (putStrLn)
import "base-unicode-symbols" Prelude.Unicode

import "base" Data.Function ((&))
import "base" Data.Monoid ((<>))
import "bytestring" Data.ByteString.Lazy.Char8 (ByteString, putStrLn)

import "time" Data.Time.Format ( FormatTime
                               , formatTime
                               , defaultTimeLocale
                               )

import "base" Control.Monad ((<$!>))

import "base" System.IO (stdout, hFlush)

import "process" System.Process ( CreateProcess ( std_in
                                                , std_out
                                                , std_err
                                                , new_session
                                                )

                                , StdStream (NoStream)
                                , proc
                                , createProcess
                                )

import "X11" Graphics.X11.Xlib (Display, displayString)


(×) ∷ Num α ⇒ α → α → α;    (×) = (*);  {-# INLINE (×) #-}; infixl 7 ×
(◇) ∷ Monoid α ⇒ α → α → α; (◇) = (<>); {-# INLINE (◇) #-}; infixr 6 ◇

(<&>) ∷ Functor φ ⇒ φ α → (α → β) → φ β
(<&>) = flip (<$>)
{-# INLINE (<&>) #-}
infixl 1 <&>

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
spawnProc cmd args = () <$ createProcess (proc cmd args)
  { std_in      = NoStream
  , std_out     = NoStream
  , std_err     = NoStream
  , new_session = True
  }
