{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Snoc-lists.
--
-- This module is meant to be imported qualified.
module Panbench.Bwd
  ( Bwd(..)
  , last
  , scoped
  ) where

import Prelude
  ( Foldable(..)
  , Functor(..)
  , error
  )

import GHC.Stack

data Bwd a = Nil | Bwd a :> a
  deriving (Functor)

-- | The @'Foldable'@ instance for @'Bwd'@ follows the usual
-- Haskell convention where @'foldr'@ is structurally recursive.
--
-- This does mean that right folds actually start at the left-most
-- end of the list, and that @'toList'@ reverses the textual order
-- of elements in the list.
instance Foldable Bwd where
  foldr _ b Nil = b
  foldr f b (xs :> x) = f x (foldr f b xs)

-- | $O(1)$. Get the last element of a nonempty backwards list.
--
-- This function is partial: only use when you know the list is
-- nonempty.
last :: HasCallStack => Bwd a -> a
last Nil = error "Bwd.last: empty list"
last (_ :> x) = x

-- | $O(n)$. Fold over a list @xs@ and invoke a continuation when complete.
scoped :: (a -> c -> c) -> (a -> b) -> [a] -> (Bwd b -> c) -> c
scoped f g xs cont = go Nil xs
  where
    go bound [] = cont bound
    go bound (x:xs) = f x (go (bound :> g x) xs)
