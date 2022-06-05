{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- | Module responsible of handling events coming from stdin
module UnclechuI3Status.Handler.InputEvents
     ( HandleClickEventInterface (..)
     , handleClickEvent
     ) where

import "base" Data.Function (fix)

import "base" Control.Monad (join)

import "X11" Graphics.X11.Types
  ( xK_Num_Lock
  , xK_Caps_Lock
  , xK_Shift_L
  , xK_Shift_R
  )

-- Local imports

import UnclechuI3Status.EventSubscriber.InputEvents (ClickEvent (..))
import UnclechuI3Status.Utils
import UnclechuI3Status.X (fakeKeyEvent)


-- | Handle single click event
handleClickEvent ∷ HandleClickEventInterface → ClickEvent → IO ()
handleClickEvent iface (\x → name (x ∷ ClickEvent) → Just name') = case name' of

  "numlock"     → fakeKeyEvent $ map (xK_Num_Lock,)  [False, True, False]
  "capslock"    → fakeKeyEvent $ map (xK_Caps_Lock,) [False, True, False]
  "datentime"   → spawnProc "gnome-calendar" []
  "alternative" → toggleAlternativeMode iface

  ['k','b','d','l','a','y','o','u','t','-',a,b] →
    let
      enum = [minBound .. maxBound ∷ Layout]
      next = foldr reducer [] [False, True, False]
        where reducer s acc = (xK_Shift_L, s) : (xK_Shift_R, s) : acc
      switchTo curLayout toLayout = join $ replicate n next
        where
          n = fix (\f x@(l:ls) → if l ≡ curLayout then x else f ls) (cycle enum)
            & fix (\f i (l:ls) → if l ≡ toLayout then i else f (succ i) ls)
                  (0 ∷ Int)
      resolve = do
        layout ← getCurrentKbdLayout iface
        fakeKeyEvent ∘ maybe next (uncurry switchTo) $ (,)
          <$> layout
          <*> foldl (\acc l → if show l ≡ [a,b] then Just l else acc)
                Nothing enum
    in
      resolve

  _ → pure ()

handleClickEvent _ _ = pure ()


-- * Types

data HandleClickEventInterface
  = HandleClickEventInterface
  { toggleAlternativeMode ∷ IO ()
  , getCurrentKbdLayout ∷ IO (Maybe Layout)
  }
