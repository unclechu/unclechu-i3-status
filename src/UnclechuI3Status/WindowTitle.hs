-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax, PackageImports #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module UnclechuI3Status.WindowTitle
     ( setUpWindowTitle
     ) where

import "aeson" Data.Aeson (eitherDecodeStrict')
import "bytestring" Data.ByteString (hGetLine, hGetContents)
import "qm-interpolated-string" Text.InterpolatedString.QM (qn, qm)
import "base" Data.Maybe (listToMaybe, catMaybes)

import "base" Control.Monad (void, forever)
import "base" Control.Concurrent (forkIO, killThread)

import "base" System.IO (stderr, hPutStrLn, hFlush)

import "process" System.Process ( CreateProcess (std_in, std_out, std_err)
                                , StdStream (NoStream, Inherit, CreatePipe)
                                , proc
                                , createProcess
                                , waitForProcess
                                , terminateProcess
                                )

-- local imports

import UnclechuI3Status.Utils
import UnclechuI3Status.Types


setUpWindowTitle
  ∷ (Either String ChangeEvent → IO ()) -- Update handler
  → IO (Maybe String, IO ()) -- Current focused window title and unsubscriber

setUpWindowTitle updateHandler = do
  (initialTree ∷ Either String WindowTree) ← do
    (Nothing, Just hOut, _, procHandle) ←
      let args = ["-t", "get_tree"]
       in createProcess (proc "i3-msg" args)
            { std_in  = NoStream
            , std_out = CreatePipe
            , std_err = Inherit
            }

    result ← eitherDecodeStrict' <$> hGetContents hOut
    result <$ waitForProcess procHandle

  (initTitle ∷ Maybe String) ←
    case initialTree of
         Left msg → Nothing <$ do
           hPutStrLn stderr [qm| Error while parsing i3 window tree: {msg} |]
           hFlush stderr
         Right tree → pure $ let
           f x@WindowTree { focused = True } = Just x
           f WindowTree { nodes } = listToMaybe $ catMaybes $ f <$> nodes
           in f tree >>= (\x → windowProperties (x ∷ WindowTree)) <&> title

  (Nothing, Just hOut, _, procHandle) ←
    let args = ["-t", "subscribe", "-m", [qn| ["window"] |]]
     in createProcess (proc "i3-msg" args)
          { std_in  = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }

  cmdReaderThreadId ← forkIO $ forever $ do
    line ← hGetLine hOut
    void $ updateHandler $ eitherDecodeStrict' line

  pure (initTitle, killThread cmdReaderThreadId >> terminateProcess procHandle)
