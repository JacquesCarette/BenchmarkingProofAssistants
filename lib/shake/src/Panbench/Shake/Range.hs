-- | Ranges.
module Panbench.Shake.Range
  ( Range(..)
  , Scale(..)
  , interval
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

-- | Create a sampling range for an inclusive interval.
interval :: Scale -> Natural -> Natural -> Range
interval scale start end
  | end < start = Range scale start 0
  | otherwise =
    let nsamples =
          case scale of
            Linear step -> (end - start) `div` step
            Log base -> ilog base (end - start)
    in Range scale start (1 + nsamples)
  where
    ilog :: Natural -> Natural -> Natural
    ilog b n =
      let d = n `div` b
      in if d == 0 then 0 else 1 + ilog b d

-- | Sample from a given range.
sample :: Range -> [Natural]
sample (Range _ _ 0) = []
sample (Range (Linear step) start n) =
  [ start + step*fromIntegral i | i <- [0..n-1] ]
sample (Range (Log base) start n) =
  [ start + base^i | i <- [0..n-1] ]
