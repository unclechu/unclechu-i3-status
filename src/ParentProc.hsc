-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax, PackageImports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module ParentProc (dieWithParent) where

import qualified "base" Foreign.C.Types as CTypes

#include <signal.h>
#include <linux/prctl.h>


foreign import ccall "sys/prctl.h prctl"
  prctl ∷ CTypes.CInt
        → CTypes.CULong
        → CTypes.CULong
        → CTypes.CULong
        → CTypes.CULong
        → IO CTypes.CInt


dieWithParent ∷ IO ()
dieWithParent = () <$ prctl (#const PR_SET_PDEATHSIG) (#const SIGHUP) 0 0 0
