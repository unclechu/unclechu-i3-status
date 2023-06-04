-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.X
     ( initThreads
     , fakeKeyEvent
     ) where

import "qm-interpolated-string" Text.InterpolatedString.QM (qms)

import "base" Control.Concurrent (myThreadId)
import "base" Control.Exception (SomeException, catch, displayException)
import "base" Control.Monad (void, forM, unless)
import qualified "async" Control.Concurrent.Async as Async

import "base" Foreign.C.Types (CULong (CULong), CInt (CInt))
import "base" System.IO (hPutStrLn, stderr)

import "X11" Graphics.X11.Xlib.Misc (keysymToKeycode)

import "X11" Graphics.X11.Xlib
  ( Display (Display)
  , KeyCode
  , Status
  , KeySym
  , openDisplay
  , closeDisplay
  , sync
  )


foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
  xFakeKeyEvent ∷ Display → KeyCode → Bool → CULong → IO Status

foreign import ccall unsafe "X11/Xlib.h XInitThreads"
  xInitThreads ∷ IO Status


initThreads ∷ IO ()
initThreads =
  xInitThreads >>= \x → unless (x /= 0) (fail "XInitThreads call has failed")


-- | Trigger fake key event in fire-and-forget mode
fakeKeyEvent
  ∷ [(KeySym, Bool)]
  -- ^ A list of key state changes to evaluate sequentially
  --   ("Bool" means whether key is pressed or released)
  → IO ()
fakeKeyEvent keySyms = fireAndForget $ do
  dpy ← openDisplay ""
  keyCodes dpy >>= mapM_ (uncurry $ trig dpy)
  closeDisplay dpy

  where
    keyCodes dpy = forM keySyms $ \(k, s) → (, s) . test <$> keysymToKeycode dpy k
    test = (\(x, True) → x) . (\x → (x, x /= 0))
    trig dpy k s = xFakeKeyEvent dpy k s 0 >> sync dpy False


-- | Spawn fire-and-forget thread and report exception if it occurs inside it
fireAndForget ∷ IO () → IO ()
fireAndForget m =
  void . Async.async $ catch m $ \e → do
    threadId ← myThreadId
    hPutStrLn stderr [qms|
      Fire-and-forget thread ({threadId}) failed with exception:
      {displayException @SomeException e}
    |]
