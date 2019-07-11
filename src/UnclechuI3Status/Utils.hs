{-# LANGUAGE UnicodeSyntax, PackageImports, LambdaCase, ScopedTypeVariables #-}

module UnclechuI3Status.Utils
     ( (•), (◇), (<&!>)
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
     , dzenCurLayout
     ) where

import Prelude hiding (putStrLn)
import "base-unicode-symbols" Prelude.Unicode

import "base" Data.Function ((&))
import "base" Data.Functor ((<&>))
import "base" Data.List (find)
import "bytestring" Data.ByteString.Lazy.Char8 (ByteString, putStrLn)
import qualified "base" Data.IORef as IORef

import "time" Data.Time.Format ( FormatTime
                               , formatTime
                               , defaultTimeLocale
                               )

import "base" Control.Monad ((<$!>))

import "base" System.IO (stdout, hFlush, hClose, hPrint)

import "process" System.Process ( CreateProcess ( std_in
                                                , std_out
                                                , std_err
                                                , new_session
                                                )

                                , StdStream (NoStream, CreatePipe)
                                , ProcessHandle

                                , proc
                                , createProcess
                                , terminateProcess
                                )

import "X11" Graphics.X11.Xlib (Display, displayString)


(•) ∷ (α → β) → (β → γ) → α → γ; (•) = flip (∘); {-# INLINE (•) #-}; infixl 9 •
(◇) ∷ Semigroup α ⇒ α → α → α;   (◇) = (<>);     {-# INLINE (◇) #-}; infixr 6 ◇

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

data Layout = US | RU | FI deriving (Eq, Show, Enum, Bounded)

numToLayout ∷ ∀ α. (Num α, Enum α, Bounded α, Eq α) ⇒ α → Maybe Layout
numToLayout n = zip [0..] [minBound..maxBound] & find (fst • (≡ n)) & fmap snd

colorOfLayout ∷ Layout → String
colorOfLayout = \case US → "#ff0000"; RU → "#00ff00"; FI → "#0000ff"

dzenCurLayout ∷ IORef.IORef (Maybe ProcessHandle) → Layout → IO ()
dzenCurLayout procRef layout = do
  let wmTitle = "unclechu-i3-status--keyboard-layout"
  let timeoutSeconds = 1 ∷ Word
  let (w, h, x, y) = (120, 120, -100, 100); w, h ∷ Word; x, y ∷ Int
  let (bgColor, fgColor) = ("black", colorOfLayout layout)
  let (fontFamily, fontStyle, fontSize) = ("Hack", "bold", 70 ∷ Word)

  let args = [ "-ta", "c"
             , "-title-name", wmTitle
             , "-p", show timeoutSeconds
             , "-w", show w, "-h", show h
             , "-x", show (if x < 0 then x − fromIntegral w else x)
             , "-y", show (if y < 0 then y − fromIntegral h else y)
             , "-bg", bgColor, "-fg", fgColor

             , "-fn", "-*-" ◇ fontFamily    ◇ "-"
                            ◇ fontStyle     ◇ "-*-*-*-"
                            ◇ show fontSize ◇ "-*-*-*-*-*-*-*"
             ]

  IORef.readIORef procRef >>= pure () `maybe` terminateProcess

  (Just input, Nothing, Nothing, procHandler)
    ← createProcess (proc "dzen2" args)
    { std_in  = CreatePipe
    , std_out = NoStream
    , std_err = NoStream
    }

  hPrint input layout >> hFlush input >> hClose input
  IORef.writeIORef procRef $ Just procHandler
