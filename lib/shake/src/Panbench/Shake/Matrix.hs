{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Panbench.Shake.Matrix
  (
  -- * Benchmarking matrices
    BenchmarkMatrixRow(..)
  , BenchmarkMatrix(..)
  , benchmarkMatrixName
  -- * Benchmark matrix statistics
  , BenchmarkMatrixStats(..)
  -- * Running benchmark matrices
  , needBenchmarkMatrix
  , needBenchmarkMatrices
  ) where

import Data.Aeson ((.:))
import Data.Aeson qualified as JSON
import Data.Foldable
import Data.Functor
import Data.Traversable
import Data.Word

import Debug.Trace (traceMarkerIO)

import Numeric.Natural

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Generator
import Panbench.Shake.Benchmark
import Panbench.Shake.Lang
import Panbench.Shake.Path

--------------------------------------------------------------------------------
-- Benchmarking matrices

-- | A benchmarking matrix row.
data BenchmarkMatrixRow where
  BenchmarkMatrixRow
    :: forall hdr defn
    . Lang hdr defn
    -> GenModule hdr defn Natural
    -> Word64
    -> BenchmarkMatrixRow

-- | A benchmarking matrix.
data BenchmarkMatrix where
  BenchmarkMatrix
    :: String
    -> [Natural]
    -> [BenchmarkMatrixRow]
    -> BenchmarkMatrix

benchmarkMatrixName :: BenchmarkMatrix -> String
benchmarkMatrixName (BenchmarkMatrix nm _ _) = nm

--------------------------------------------------------------------------------
-- Benchmarking matrix statistics

-- | Benchmarking statistics for a @'BenchmarkMatrix'@.
--
-- This is stored in a format that can easily be consumed by @vega-lite@.
newtype BenchmarkMatrixStats = BenchmarkMatrixStats [(String, JSON.Value, BenchmarkExecStats)]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)

-- | We need this somewhat annoying instance to make our data a bit more @vega-lite@
-- friendly.
instance JSON.ToJSON BenchmarkMatrixStats where
  toJSON (BenchmarkMatrixStats stats) =
    JSON.toJSON $ stats <&> \(lang, size, BenchmarkExecStats{..}) ->
      JSON.object
      [ ("lang", JSON.toJSON lang)
      , ("size", size)
      , ("user", JSON.toJSON benchUserTime)
      , ("system", JSON.toJSON benchSystemTime)
      , ("rss", JSON.toJSON benchMaxRss)
      , ("exit" , JSON.toJSON benchExitCode)
      ]

instance JSON.FromJSON BenchmarkMatrixStats where
  parseJSON =
    JSON.withArray "BenchmarkMatrixStats" \objs -> do
    entries <-
      for objs $ JSON.withObject "BenchmarkMatrixStat" \obj -> do
        lang <- obj .: "lang"
        size <- obj .: "size"
        benchUserTime <- obj .: "user"
        benchSystemTime <- obj .: "system"
        benchMaxRss <- obj .: "rss"
        benchExitCode <- obj .: "exit"
        pure (lang, size, BenchmarkExecStats {..})
    pure $ BenchmarkMatrixStats $ toList entries

--------------------------------------------------------------------------------
-- Running benchmark matrices

needBenchmarkMatrix
  :: BenchmarkMatrix
  -> Action BenchmarkMatrixStats
needBenchmarkMatrix (BenchmarkMatrix name sizes rows) = BenchmarkMatrixStats <$>
  for (liftA2 (,) rows sizes) \(BenchmarkMatrixRow lang gen limits, size) -> do
    liftIO $ traceMarkerIO name
    (dir, file) <- splitFileName <$> needModule lang gen size
    cleanBuildArtifacts lang dir
    stat <- benchmarkModule lang [Env [("HOME", decodeOS dir), ("LC_ALL", "C.UTF-8")], Cwd (decodeOS dir)] limits file
    pure (langName lang, JSON.toJSON size, stat)

needBenchmarkMatrices :: [BenchmarkMatrix] -> Action [BenchmarkMatrixStats]
needBenchmarkMatrices = traverse needBenchmarkMatrix
