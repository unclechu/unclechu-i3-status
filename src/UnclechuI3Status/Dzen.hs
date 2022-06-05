-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Dzen2 notification window spawning functions
module UnclechuI3Status.Dzen
     ( dzen
     ) where

import qualified "base" Data.IORef as IORef

import "base" Control.Concurrent (threadDelay)
import qualified "async" Control.Concurrent.Async as Async

import "base" System.IO (Handle, hPutStrLn, hFlush, hClose)
import "base" System.Mem.Weak (addFinalizer)

import "process" System.Process
  ( CreateProcess (std_in, std_out, std_err)
  , StdStream (NoStream, CreatePipe)
  , ProcessHandle
  , proc
  , createProcess
  , getProcessExitCode
  , terminateProcess
  )

-- Local imports

import UnclechuI3Status.Utils


dzen
  ∷ IORef.IORef (Maybe (ProcessHandle, Handle, Async.Async ()))
  -- ^ Process handle, stdin of the process, and timeout timer thread handle
  → String
  → String
  → IO ()
dzen procRef text fgColor = do
  (procHandler, input, threadHandle) ←
    IORef.readIORef procRef >>= \case
      Nothing → getNewProc
      Just (procHandler, input, threadHandle) →
        getProcessExitCode procHandler >>= \case
          Just _ → Async.cancel threadHandle >> getNewProc
          Nothing → do
            Async.cancel threadHandle
            (procHandler, input,) <$> runKiller input procHandler

  fireAndForget $ hPutStrLn input colorfulText >> hFlush input
  IORef.writeIORef procRef $ Just (procHandler, input, threadHandle)

  where
    wmTitle = "unclechu-i3-status--keyboard-layout"
    timeoutSeconds = 1 ∷ Word
    (w, h, x, y) = (120, 120, -100, 100); w, h ∷ Word; x, y ∷ Int
    (bgColor, fgDefaultColor) = ("black", "white")
    (fontFamily, fontStyle) = ("Hack", "bold")

    fontSize ∷ Word
    fontSize
      | length text ≤ 2 = 70
      | length text ≡ 3 = 42
      | length text ≡ 4 = 32
      | otherwise = 9

    fontStr (size ∷ Word)
      = "-*-"
      ⋄ fontFamily ⋄ "-"
      ⋄ fontStyle  ⋄ "-*-*-*-"
      ⋄ show size  ⋄ "-*-*-*-*-*-*-*"

    colorfulText = "^fn(" ⋄ fontStr fontSize ⋄ ")^fg(" ⋄ fgColor ⋄ ")" ⋄ text

    args =
      [ "-ta", "c"
      , "-title-name", wmTitle
      -- , "-p", show timeoutSeconds
      , "-w", show w, "-h", show h
      , "-x", show (if x < 0 then x − fromIntegral w else x)
      , "-y", show (if y < 0 then y − fromIntegral h else y)
      , "-bg", bgColor, "-fg", fgDefaultColor
      , "-fn", fontStr 9
      ]

    runKiller input procHandler = Async.async $ do
      threadDelay $ fromIntegral timeoutSeconds × 1000 × 1000
      hClose input >> terminateProcess procHandler

    getNewProc = do
      (Just input, Nothing, Nothing, procHandler)
        ← createProcess (proc "dzen2" args)
        { std_in  = CreatePipe
        , std_out = NoStream
        , std_err = NoStream
        }

      addFinalizer procHandler $ terminateProcess procHandler
      (procHandler, input,) <$> runKiller input procHandler
