{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
-- | Missing bits of the prelude.
module Panbench.Prelude
  ( asumMap1
  , type (~~>)
  -- * Re-exports
  , module Prelude
  , module IsString
  , module Natural
  ) where

import Prelude as Prelude hiding (pi)

import Data.String as IsString (IsString(..))


import Data.Functor.Alt
import Data.Semigroup.Foldable

import Numeric.Natural as Natural

newtype ASum f a = ASum { unASum :: f a }

instance (Alt f) => Semigroup (ASum f a) where
  (ASum x) <> (ASum y) = ASum (x <!> y)

asumMap1 :: (Foldable1 t, Alt f) => (a -> f b) -> t a -> f b
asumMap1 f = unASum . foldMap1 (ASum . f)

-- | Reifying fundep constraints as a class.
--
-- This class plays a similar role to constraints of the form @a ~ b@,
-- which allow us to write instance heads that look more general than they really are.
--
-- The main use for @a ~~> b@ constraints is when we write custom 'GHC.TypeLits.TypeError' instances
-- for classes that involve a fundep. GHC will (rightfully!) complain on instances like
--
-- @
-- class Foo a b c | b -> c where
--
-- instance (TypeError ('Text "Bad!")) => Foo String b c
-- @
--
-- as there is no way to determine @c@ from @b@. However, this constraint will never actually get
-- solved, so we can add a spurious dependency with a @b ~~> c@ constraint.
class (~~>) a b | a -> b where
