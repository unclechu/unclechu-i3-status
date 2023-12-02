-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import "base" Data.Maybe (listToMaybe, catMaybes)
import "qm-interpolated-string" Text.InterpolatedString.QM (qn, qm)
import qualified "async" Control.Concurrent.Async as Async
import "base" Control.Monad (forever)

import "process" System.Process
  ( CreateProcess (std_in, std_out, std_err)
  , StdStream (NoStream, Inherit, CreatePipe)
  , proc
  , waitForProcess
  , withCreateProcess
  )

-- Local imports

import qualified UnclechuI3Status.EventSubscriber.WindowTitle.Types.ChangeEvent as ChangeEvent
import qualified UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventContainer as EventContainer
import qualified UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventResolve as EventResolve
import qualified UnclechuI3Status.EventSubscriber.WindowTitle.Types.EventWorkspace as EventWorkspace
import UnclechuI3Status.EventSubscriber.WindowTitle.Types.WindowTitle (WindowTitle (..))
import qualified UnclechuI3Status.EventSubscriber.WindowTitle.Types.WindowTitle as WindowTitle
import qualified UnclechuI3Status.EventSubscriber.WindowTitle.Types.WindowTree as WindowTree
import UnclechuI3Status.Utils


subscribeToFocusedWindowTitleUpdates
  ∷ (Maybe WindowTitle → IO ())
  -- ^ Focused winodw title update callback
  → IO (Maybe WindowTitle, Async.Async ())
  -- ^ Current/initial focused window title and thread handle
subscribeToFocusedWindowTitleUpdates updateCallback = do
  (initialTree ∷ Either String WindowTree.WindowTree) ← do
    let
      procSpec =
        (proc "i3-msg" ["-t", "get_tree"])
          { std_in  = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }
    withCreateProcess procSpec $ \Nothing (Just hOut) _ procHandle → do
      result ← eitherDecodeStrict' <$> hGetContents hOut
      result <$ waitForProcess procHandle

  (initTitle ∷ Maybe WindowTitle) ←
    case initialTree of
      Left msg →
        Nothing <$ fail [qm| Error while parsing i3 window tree: {msg} |]
      Right tree → pure $ let
        f x@WindowTree.WindowTree { focused = True } = Just x
        f WindowTree.WindowTree { nodes } = listToMaybe $ catMaybes $ f <$> nodes
        in f tree >>= WindowTitle.mkWindowTitle

  threadHandle ← Async.async $ do
    let
      procSpec =
        (proc "i3-msg" ["-t", "subscribe", "-m", [qn| ["window", "workspace"] |]])
          { std_in  = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }
    withCreateProcess procSpec $ \Nothing (Just hOut) _ _ →
      forever @IO @() @() $ hGetLine hOut
        <&> eitherDecodeStrict'
        >>= either (fail ∘ ("Error while parsing window title event: " ⋄)) pure
        >>= getFocusedWindowTitle • \case
              EventResolve.Ignore → pure ()
              EventResolve.FocusedWindowNotFound → updateCallback Nothing
              EventResolve.FocusedWindowClosed → updateCallback Nothing
              EventResolve.FocusedWindowTitle title → updateCallback title

  pure (initTitle, threadHandle)


getFocusedWindowTitle ∷ ChangeEvent.ChangeEvent → EventResolve.EventResolve
getFocusedWindowTitle = \case
  ChangeEvent.WindowFocusEvent container →
    if not $ EventContainer.focused container
    then EventResolve.Ignore
    else EventResolve.FocusedWindowTitle ∘ WindowTitle.mkWindowTitle $ container
  ChangeEvent.WindowTitleEvent container →
    if not $ EventContainer.focused container
    then EventResolve.Ignore
    else EventResolve.FocusedWindowTitle ∘ WindowTitle.mkWindowTitle $ container
  ChangeEvent.WindowCloseEvent container →
    if EventContainer.focused container
    then EventResolve.FocusedWindowClosed
    else EventResolve.Ignore

  ChangeEvent.WorkspaceFocusEvent current → let
    f x@WindowTree.WindowTree { focused = True } = Just x
    f WindowTree.WindowTree { nodes } = firstFocused nodes

    firstFocused = listToMaybe ∘ catMaybes ∘ map f
    top = firstFocused $ EventWorkspace.nodes current

    in
      maybe
        EventResolve.FocusedWindowNotFound
        (EventResolve.FocusedWindowTitle ∘ WindowTitle.mkWindowTitle)
        top

  ChangeEvent.OtherEvent _ → EventResolve.Ignore
