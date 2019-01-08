-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax, PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module UnclechuI3Status.WindowTitle
     ( setUpWindowTitle
     ) where

import "aeson" Data.Aeson (eitherDecodeStrict')
import "bytestring" Data.ByteString (hGetLine)
import "qm-interpolated-string" Text.InterpolatedString.QM (qn)

import "base" Control.Monad (void, forever)
import "base" Control.Concurrent (forkIO, killThread)

import "process" System.Process ( CreateProcess (std_in, std_out, std_err)
                                , StdStream (NoStream, Inherit, CreatePipe)
                                , proc
                                , createProcess
                                , terminateProcess
                                )

-- local imports

import UnclechuI3Status.Types


setUpWindowTitle
  ∷ (Either String ChangeEvent → IO ()) -- Update handler
  → IO (IO ()) -- Unsubscriber

setUpWindowTitle updateHandler = do
  (Nothing, Just hOut, _, procHandle) <-
    let args = ["-t", "subscribe", "-m", [qn| ["window"] |]]
     in createProcess (proc "i3-msg" args)
          { std_in  = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }

  cmdReaderThreadId <- forkIO $ forever $ do
    line <- hGetLine hOut
    void $ forkIO $ updateHandler $ eitherDecodeStrict' line

  pure $ terminateProcess procHandle >> killThread cmdReaderThreadId
