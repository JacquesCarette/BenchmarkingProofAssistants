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
  , setupBenchmarkingMatrix
  , runBenchmarkingMatrix
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
    -> [Natural]
    -> Word64
    -> BenchmarkMatrixRow

-- | A benchmarking matrix.
data BenchmarkMatrix where
  BenchmarkMatrix
    :: String
    -> [BenchmarkMatrixRow]
    -> BenchmarkMatrix

benchmarkMatrixName :: BenchmarkMatrix -> String
benchmarkMatrixName (BenchmarkMatrix nm _) = nm

--------------------------------------------------------------------------------
-- Benchmarking matrix statistics

-- | Benchmarking statistics for a @'BenchmarkMatrix'@.
--
-- This is stored in a format that can easily be consumed by @vega-lite@.
newtype BenchmarkMatrixStats = BenchmarkMatrixStats [(String, Natural, BenchmarkExecStats)]
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | We need this somewhat annoying instance to make our data a bit more @vega-lite@
-- friendly.
instance JSON.ToJSON BenchmarkMatrixStats where
  toJSON (BenchmarkMatrixStats stats) =
    JSON.toJSON $ stats <&> \(lang, size, BenchmarkExecStats{..}) ->
      JSON.object
      [ ("lang", JSON.toJSON lang)
      , ("size", JSON.toJSON size)
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

-- | Generate all modules needed to run a benchmarking matrix.
--
-- Returns a list of actions that, when run, will run an individual benchmark.
setupBenchmarkingMatrix
  :: BenchmarkMatrix
  -> Action [Action (String, Natural, BenchmarkExecStats)]
setupBenchmarkingMatrix (BenchmarkMatrix name rows) =
  concat <$>
  for rows \(BenchmarkMatrixRow lang gen sizes limits) ->
  for sizes \size -> do
    liftIO $ traceMarkerIO $ "Generating module " <> langName lang <> "/" <> name <> "/" <> show size
    (dir, file) <- splitFileName <$> needModule lang gen size
    pure do
      liftIO $ traceMarkerIO $ "Running benchmark " <> langName lang <> "/" <> name <> "/" <> show size
      stat <- benchmarkModule lang [Env [("HOME", decodeOS dir), ("LC_ALL", "C.UTF-8")], Cwd (decodeOS dir)] limits file
      pure (langName lang, size, stat)

-- | Generate and run all benchmarks in a 'BenchmarkMatrix'.
runBenchmarkingMatrix
  :: BenchmarkMatrix
  -> Action BenchmarkMatrixStats
runBenchmarkingMatrix matrix = do
  benchmarks <- setupBenchmarkingMatrix matrix
  BenchmarkMatrixStats <$> sequence benchmarks
