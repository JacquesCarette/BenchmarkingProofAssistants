module Panbench.Shake.Matrix
  ( -- $shakeMatrix
    BenchmarkMatrix(..)
  , BenchmarkMatrixStats(..)
  , benchmarkMatrixRules
  , needBenchmarkMatrix
  , needBenchmarkMatrices
  ) where

import Data.Aeson ((.:))
import Data.Aeson qualified as JSON
import Data.Foldable
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Numeric.Natural

import System.FilePath

import Panbench.Shake.Benchmark
import Panbench.Lang (Lang)
import Panbench.Shake.Lang
import Panbench.Shake.File

-- * Benchmarking matrices.
--
-- $shakeMatrix

-- | Benchmarking matrix query.
data BenchmarkMatrix = BenchmarkMatrix
  { benchMatrixName :: String
  -- ^ Module to benchmark.
  , benchMatrixLangs :: Set Lang
  -- ^ What languages should we include in the matrix?
  , benchMatrixSizes :: [Natural]
  -- ^ Parameters to sample at.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Benchmarking statistics for a @'BenchmarkMatrix'@.
--
-- This is stored in a format that can easily be consumed by @vega-lite@.
newtype BenchmarkMatrixStats = BenchmarkMatrixStats [(Lang, Natural, BenchmarkExecStats)]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)

type instance RuleResult BenchmarkMatrix = BenchmarkMatrixStats

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
    pure $ BenchmarkMatrixStats $ toList $ entries

-- | Produce a list of @'GenerateModule'@ queries from a benchmarking matrix.
benchmarkMatrixModules :: BenchmarkMatrix -> [GenerateModule]
benchmarkMatrixModules BenchmarkMatrix{..} = do
  let generatorName = benchMatrixName
  generatorLang <-Set.toList benchMatrixLangs
  generatorSize <- benchMatrixSizes
  pure $ GenerateModule {..}

-- | Output path for the JSON results of a benchmarking matrix.
--
-- Currently, we use the convention
--
-- > _build/bench/{hash}.json
--
-- Where {hash} is the hash of the matrix. This is subject to
-- change, and user code should not rely on this.
benchmarkMatrixOutputPath :: BenchmarkMatrix -> FilePath
benchmarkMatrixOutputPath matrix =
  "_build" </> "bench" </> show (hash matrix) <.> "json"

-- | Rules for benchmarking matrices.
benchmarkMatrixRules :: Rules ()
benchmarkMatrixRules =
  addFileCacheOracle benchmarkMatrixOutputPath (either fail pure . JSON.eitherDecode) \matrix -> do
    let generators = benchmarkMatrixModules matrix
    modPaths <- needModules generators
    stats <-
      BenchmarkMatrixStats <$>
        for (zip generators modPaths) \(GenerateModule{..}, (dir, file)) -> do
          cleanBuildArtifacts generatorLang dir
          bin <- needLang generatorLang
          let args = langCheckDefaultArgs generatorLang file
          stat <- liftIO $ benchmark bin args [("HOME", dir)] dir
          pure (generatorLang, generatorSize, stat)
    pure (stats, JSON.encode stats)

-- | Get benchmarking statistics for a single benchmarking matrix.
--
-- This query is subject to caching.
needBenchmarkMatrix :: BenchmarkMatrix -> Action BenchmarkMatrixStats
needBenchmarkMatrix matrix =
  snd <$> askFileCacheOracle matrix

-- | Get benchmarking statistics for multiple benchmarking matrices.
--
-- This query lets us generate the output files in parallel, though
-- the actual benchmarks are still run sequentially.
--
-- This query is subject to caching.
needBenchmarkMatrices :: [BenchmarkMatrix] -> Action [BenchmarkMatrixStats]
needBenchmarkMatrices matrices =
  fmap snd <$> asksFileCacheOracle matrices
