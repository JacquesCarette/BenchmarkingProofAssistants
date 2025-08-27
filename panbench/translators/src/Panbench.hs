{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
-- | Panbench utilities.
module Panbench
  ( -- $generators
    Gen(..)
  , module Panbench.Grammar
  , Natural
  ) where


import Data.Kind
import Data.Text (Text)

import Numeric.Natural

import Panbench.Grammar


-- * Generators
--
-- $generators

data Gen (size :: Type) (m :: Type) (rep :: Type) =
  Gen
  { genHeader :: Text -> m
  -- ^ Generate the module header.
  --
  -- This is used to establish baselines during benchmarking.
  , genBody :: size -> m
  -- ^ Generate the body of the module.
  }
