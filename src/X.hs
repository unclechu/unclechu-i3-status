-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module X
     ( initThreads
     , fakeKeyEvent
     ) where

import "base-unicode-symbols" Prelude.Unicode

import "base" Control.Monad (forM, void)
import "base" Control.Concurrent (forkIO)

import "base" Foreign.C.Types (CULong (CULong), CInt (CInt))

import "X11"  Graphics.X11.Xlib.Misc (keysymToKeycode)
import "X11"  Graphics.X11.Xlib ( Display (Display)
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
initThreads = do !True ← (≢ 0) <$> xInitThreads; pure ()


fakeKeyEvent ∷ [(KeySym, Bool)] → IO ()
fakeKeyEvent keySyms = void $ forkIO $ do
  dpy ← openDisplay ""

  let keyCodes = forM keySyms $ \(k, s) → (, s) ∘ test <$> keysymToKeycode dpy k
      test     = (\(x, !True) → x) ∘ (\x → (x, x ≢ 0))
      trig k s = xFakeKeyEvent dpy k s 0 >> sync dpy False
   in keyCodes >>= mapM_ (uncurry trig)

  closeDisplay dpy
