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

     , showNumLock
     , colorOfNumLock

     , showCapsLock
     , colorOfCapsLock

     , showAlternativeState
     , colorOfAlternativeState

     , dzen
     ) where

import Prelude hiding (putStrLn)
import "base-unicode-symbols" Prelude.Unicode

import "base" Data.Word (Word8)
import "base" Data.Bool (bool)
import "base" Data.Function ((&))
import "base" Data.Functor ((<&>), ($>))
import "base" Data.List (find)
import "bytestring" Data.ByteString.Lazy.Char8 (ByteString, putStrLn)
import qualified "base" Data.IORef as IORef

import "time" Data.Time.Format ( FormatTime
                               , formatTime
                               , defaultTimeLocale
                               )

import "base" Control.Monad ((<$!>))
import "base" Control.Concurrent (ThreadId, forkIO, threadDelay, killThread)

import "base" System.IO (Handle, stdout, hFlush, hPutStrLn, hClose)

import "process" System.Process ( CreateProcess ( std_in
                                                , std_out
                                                , std_err
                                                , new_session
                                                )

                                , StdStream (NoStream, CreatePipe)
                                , ProcessHandle

                                , proc
                                , createProcess
                                , getProcessExitCode
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


dzen
  ∷ IORef.IORef (Maybe (ProcessHandle, Handle, ThreadId))
  → String
  → String
  → IO ()

dzen procRef text fgColor = do
  let wmTitle = "unclechu-i3-status--keyboard-layout"
  let timeoutSeconds = 1 ∷ Word
  let (w, h, x, y) = (120, 120, -100, 100); w, h ∷ Word; x, y ∷ Int
  let (bgColor, fgDefaultColor) = ("black", "white")
  let (fontFamily, fontStyle) = ("Hack", "bold")

  let fontSize ∷ Word
      fontSize | length text ≤ 2 = 70
               | length text ≡ 3 = 42
               | length text ≡ 4 = 32
               | otherwise       = 9

  let fontStr (size ∷ Word)
        = "-*-"
        ◇ fontFamily ◇ "-"
        ◇ fontStyle  ◇ "-*-*-*-"
        ◇ show size  ◇ "-*-*-*-*-*-*-*"

  let colorfulText = "^fn(" ◇ fontStr fontSize ◇ ")^fg(" ◇ fgColor ◇ ")" ◇ text

  let args = [ "-ta", "c"
             , "-title-name", wmTitle
             -- , "-p", show timeoutSeconds
             , "-w", show w, "-h", show h
             , "-x", show (if x < 0 then x − fromIntegral w else x)
             , "-y", show (if y < 0 then y − fromIntegral h else y)
             , "-bg", bgColor, "-fg", fgDefaultColor
             , "-fn", fontStr 9
             ]

  let runKiller input procHandler = forkIO $ do
        threadDelay $ fromIntegral timeoutSeconds × 1000 × 1000
        hClose input >> terminateProcess procHandler

  let getNewProc = do
        (Just input, Nothing, Nothing, procHandler)
          ← createProcess (proc "dzen2" args)
          { std_in  = CreatePipe
          , std_out = NoStream
          , std_err = NoStream
          }

        threadId ← runKiller input procHandler
        pure (procHandler, input, threadId)

  (procHandler, input, threadId) ←
    IORef.readIORef procRef >>= \case
      Nothing → getNewProc
      Just (procHandler, input, threadId) →
        getProcessExitCode procHandler >>= \case
          Just _ → killThread threadId >> getNewProc
          Nothing → do
            killThread threadId
            newThreadId ← runKiller input procHandler
            pure (procHandler, input, newThreadId)

  _ ← forkIO $ hPutStrLn input colorfulText >> hFlush input
  IORef.writeIORef procRef $ Just (procHandler, input, threadId)
