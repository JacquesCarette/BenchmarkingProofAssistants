-- | Ranges.
module Panbench.Shake.Range
  ( Range(..)
  , Scale(..)
  , sample
  ) where

import Development.Shake.Classes

import GHC.Generics

import Numeric.Natural

-- | A range of sizes to sample from.
data Range = Range
  { rangeScale :: !Scale
  -- ^ The scale of the range.
  , rangeStart :: !Natural
  -- ^ The start of the range, inclusive.
  , rangeSamples :: !Natural
  -- ^ The number of samples to use.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

data Scale
  = Linear !Natural
  -- ^ A linear scale with a given step size.
  | Log !Natural
  -- ^ A log scale of a given base.
  --
  -- Sampling from an 'Log' scale will produce an exponentially
  -- increasing sample.
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Sample from a given range.
sample :: Range -> [Natural]
sample (Range (Linear step) start n) =
  [ start + step*fromIntegral i | i <- [0..n-1] ]
sample (Range (Log base) start n) =
  [ start + base^i | i <- [0..n-1] ]
