-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.EventSubscriber.WindowTitle
     ( WindowTitle (..)
     , subscribeToFocusedWindowTitleUpdates
     ) where

import "aeson" Data.Aeson (eitherDecodeStrict')
import "bytestring" Data.ByteString (hGetLine, hGetContents)
import "qm-interpolated-string" Text.InterpolatedString.QM (qn, qm)
import "base" Data.Maybe (listToMaybe, catMaybes)

import "base" Control.Monad (forever)
import qualified "async" Control.Concurrent.Async as Async

import "process" System.Process
  ( CreateProcess (std_in, std_out, std_err)
  , StdStream (NoStream, Inherit, CreatePipe)
  , proc
  , createProcess
  , waitForProcess
  )

-- local imports

import UnclechuI3Status.Utils
import UnclechuI3Status.Types


newtype WindowTitle = WindowTitle { unWindowTitle ∷ String }
  deriving (Show, Eq)


subscribeToFocusedWindowTitleUpdates
  ∷ (Maybe WindowTitle → IO ())
  -- ^ Focused winodw title update callback
  → IO (Maybe WindowTitle, Async.Async ())
  -- ^ Current/initial focused window title and thread handle
subscribeToFocusedWindowTitleUpdates updateCallback = do
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

  (initTitle ∷ Maybe WindowTitle) ←
    case initialTree of
      Left msg →
        Nothing <$ fail [qm| Error while parsing i3 window tree: {msg} |]
      Right tree → pure $ let
        f x@WindowTree { focused = True } = Just x
        f WindowTree { nodes } = listToMaybe $ catMaybes $ f <$> nodes

        windowProps = f tree >>= \x → windowProperties (x ∷ WindowTree)

        in WindowTitle ∘ title <$> windowProps

  (Nothing, Just hOut, _, _) ←
    let args = ["-t", "subscribe", "-m", [qn| ["window", "workspace"] |]]
     in createProcess (proc "i3-msg" args)
          { std_in  = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }

  threadHandle ← Async.async . forever $
    hGetLine hOut
      <&> eitherDecodeStrict'
      >>= either (fail ∘ ("Error while parsing window title event: " ⋄)) pure
      >>= getFocusedWindowTitle • \case
            Ignore → pure ()
            FocusedWindowNotFound → updateCallback Nothing
            FocusedWindowClosed → updateCallback Nothing
            FocusedWindowTitle title → updateCallback ∘ Just $ title

  pure (initTitle, threadHandle)


getFocusedWindowTitle ∷ ChangeEvent → EventResolve
getFocusedWindowTitle = \case
  WindowFocusEvent { container } →
    if not $ focused (container ∷ EventContainer)
    then Ignore
    else FocusedWindowTitle ∘ WindowTitle ∘ title $
           windowProperties (container ∷ EventContainer)
  WindowTitleEvent { container } →
    if not $ focused (container ∷ EventContainer)
    then Ignore
    else FocusedWindowTitle ∘ WindowTitle ∘ title $
           windowProperties (container ∷ EventContainer)
  WindowCloseEvent { container } →
    if focused (container ∷ EventContainer)
    then FocusedWindowClosed
    else Ignore

  WorkspaceFocusEvent { current } → let
    f x@WindowTree { focused = True } = Just x
    f WindowTree { nodes } = firstFocused nodes

    firstFocused = listToMaybe ∘ catMaybes ∘ map f
    top = firstFocused $ nodes (current ∷ EventWorkspace)
    t = top >>= \x → WindowTitle ∘ title <$> windowProperties (x ∷ WindowTree)

    in maybe FocusedWindowNotFound FocusedWindowTitle t

  OtherEvent _ → Ignore


data EventResolve
  = Ignore
  | FocusedWindowNotFound
  | FocusedWindowClosed
  | FocusedWindowTitle WindowTitle
  deriving (Show, Eq)
