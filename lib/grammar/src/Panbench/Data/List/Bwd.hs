{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Backwards lists.
--
-- This module is meant to be imported qualified.
module Panbench.Data.List.Bwd
  ( Bwd(..)
  , last
  , scoped
  , repeat
  ) where

import Prelude
  ( Foldable(..)
  , Functor(..)
  , Applicative(..)
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

-- | The @'Applicative'@ instance for backwards lists is the same as @'ZipList'@.
instance Applicative Bwd where
  pure = repeat
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (fs :> f) <*> (xs :> x) = (fs <*> xs) :> (f x)

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

repeat :: a -> Bwd a
repeat a = repeat a :> a
