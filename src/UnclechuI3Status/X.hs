-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnclechuI3Status.X
     ( initThreads
     , fakeKeyEvent
     ) where

import "base" Control.Monad (forM, void, unless)
import "base" Control.Concurrent (forkIO)

import "base" Foreign.C.Types (CULong (CULong), CInt (CInt))

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

 -- Local imports

import UnclechuI3Status.Utils


foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
  xFakeKeyEvent ∷ Display → KeyCode → Bool → CULong → IO Status

foreign import ccall unsafe "X11/Xlib.h XInitThreads"
  xInitThreads ∷ IO Status


initThreads ∷ IO ()
initThreads =
  xInitThreads >>= \x → unless (x ≠ 0) (fail "XInitThreads call has failed")


fakeKeyEvent ∷ [(KeySym, Bool)] → IO ()
fakeKeyEvent keySyms = void $ forkIO $ do
  dpy ← openDisplay ""
  keyCodes dpy >>= mapM_ (uncurry $ trig dpy)
  closeDisplay dpy

  where
    keyCodes dpy = forM keySyms $ \(k, s) → (, s) ∘ test <$> keysymToKeycode dpy k
    test = (\(x, True) → x) ∘ (\x → (x, x ≢ 0))
    trig dpy k s = xFakeKeyEvent dpy k s 0 >> sync dpy False
