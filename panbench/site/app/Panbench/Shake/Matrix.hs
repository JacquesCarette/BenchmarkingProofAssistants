{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Panbench.Shake.Matrix
  (
  -- * Benchmarking matrices
    BenchmarkMatrixRow(..)
  , benchmarkMatrixRow
  , BenchmarkMatrix(..)
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

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import System.FilePath

import Panbench.Generator
import Panbench.Shake.Benchmark
import Panbench.Shake.Lang

--------------------------------------------------------------------------------
-- Benchmarking matrices

-- | A benchmarking matrix row.
data BenchmarkMatrixRow size where
  -- | Pack up a 'GenModule' along with a 'ShakeLang' dictionary.
  --
  -- Users are encouraged to use 'benchmarkMatrixRow', which takes an explicit type argument.
  BenchmarkMatrixRow
    :: forall rep m hdr defn size. (ShakeLang m hdr defn rep)
    => GenModule size hdr defn
    -> Word64
    -> BenchmarkMatrixRow size


-- | Make a benchmarking matrix row.
benchmarkMatrixRow
  :: forall m hdr defn size. forall rep
  -> (ShakeLang m hdr defn rep)
  => GenModule size hdr defn
  -> Word64
  -> BenchmarkMatrixRow size
benchmarkMatrixRow _ = BenchmarkMatrixRow

-- | A benchmarking matrix.
data BenchmarkMatrix where
  BenchmarkMatrix
    :: forall size. (JSON.ToJSON size, Show size)
    => String
    -> [size]
    -> [BenchmarkMatrixRow size]
    -> BenchmarkMatrix

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
instance JSON.ToJSON (BenchmarkMatrixStats) where
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

instance JSON.FromJSON (BenchmarkMatrixStats) where
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

--------------------------------------------------------------------------------
-- Running benchmark matrices

needBenchmarkMatrix
  :: BenchmarkMatrix
  -> Action BenchmarkMatrixStats
needBenchmarkMatrix (BenchmarkMatrix _ sizes rows) = BenchmarkMatrixStats <$>
  for (liftA2 (,) sizes rows) \(size, BenchmarkMatrixRow @rep gen limits) -> do
    bin <- needLang rep
    (dir, file) <- splitFileName <$> needModule gen size
    cleanBuildArtifacts rep dir
    stat <- benchmarkModule rep [Env [("HOME", dir), ("LC_ALL", "C.UTF-8")], Cwd dir] limits bin file
    pure (langName rep, JSON.toJSON size, stat)

needBenchmarkMatrices :: [BenchmarkMatrix] -> Action [BenchmarkMatrixStats]
needBenchmarkMatrices = traverse needBenchmarkMatrix
