{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}

module Utils
     ( (×), (◇), (<&>)
     , module Data.Function
     ) where

import "base" Data.Function ((&))
import "base" Data.Monoid ((<>))


(×) ∷ Num α ⇒ α → α → α;    (×) = (*);  {-# INLINE (×) #-}; infixl 7 ×
(◇) ∷ Monoid α ⇒ α → α → α; (◇) = (<>); {-# INLINE (◇) #-}; infixr 6 ◇

(<&>) ∷ Functor φ ⇒ φ α → (α → β) → φ β
(<&>) = flip (<$>)
{-# INLINE (<&>) #-}
infixl 1 <&>
