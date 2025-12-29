{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
-- | @shake@ build rules for @panbench@ modules.
module Panbench.Shake.Lang
  ( -- * Shake rules for languages
    Lang(..)
  , generatorOutputDir
  , SomeLangModule(..)
  , needModules
  ) where

import Data.Traversable
import Data.Word

import Development.Shake

import Numeric.Natural

import Panbench.Generator

import Panbench.Shake.Benchmark
import Panbench.Shake.Path

data Lang hdr defn = Lang
  { langName :: String
  -- ^ User-facing name of the language.
  , langExt :: String
  -- ^ File extension of a language
  , needModule :: GenModule hdr defn Natural -> Natural -> Action OsPath
  -- ^ Require that a module described by a 'Gen' get built.
  -- This should return an absolute filepath.
  , cleanBuildArtifacts :: OsPath -> Action ()
  -- ^ Clean all build artifacts for a language in the given directory.
  , benchmarkModule :: [CmdOption] -> Word64 -> OsPath -> Action BenchmarkExecStats
  -- ^ Create a benchmarking command for a language.
  }

-- | Existential for 'GenModule' that packs up evidence that we
-- actually know how generate and typecheck the module.
data SomeLangModule where
  -- | Pack a 'GenModule' alongside a 'ShakeLang' dictionary.
  SomeLangModule
    :: forall hdr defn
    . Lang hdr defn
    -> GenModule hdr defn Natural
    -> Natural
    -> SomeLangModule

-- | Get the output file for a module in a standardized format.
--
-- We will use the following convention for paths:
--
-- > _build/lang/gen/n/mod.ext
--
-- Where @lang@ is the language name, @gen@ is the name of the generator,
-- @n@ is the size parameter, and @mod.ext@ is the rendered module file.
generatorOutputDir
  :: String -- ^ Language name
  -> String -- ^ Module name
  -> String -- ^ Size
  -> String -- ^ Extension
  -> OsPath
generatorOutputDir lang nm size ext =
  [osp|_build/$lang/$nm/$size/$nm.$ext|]


-- | Request that a list of modules be generated.
--
-- This query is subject to caching.
needModules :: [SomeLangModule] -> Action [(OsPath, OsPath)]
needModules gens =
  -- Can't use a variant of 'asks' here for type reasons.
  for gens \(SomeLangModule lang gen size) -> do
    path <- needModule lang gen size
    pure (splitFileName path)

-- --------------------------------------------------------------------------------
-- -- Instances

-- instance ShakeLang AgdaMod AgdaHeader AgdaDefns Agda where
--   type Bin Agda = AgdaBin
--   langName _ = "agda"
--   langExt _ = ".agda"
--   needLang _ = do
--     opts <- needAgdaInstallOpts
--     needAgda opts
--   needModule gen size = do
--     let path = generatorOutputDir "agda" (T.unpack (genName gen)) (show size) ".agda"
--     putInfo $ "# generating " <> decodeOS path
--     writeBinaryHandleChanged path (genModuleVia (runAgdaM def) size gen)
--     pure path
--   benchmarkModule _ = agdaCheckBench
--   cleanBuildArtifacts _ dir = removeFilesAfter (decodeOS dir) ["*.agdai"]

-- instance ShakeLang IdrisMod IdrisHeader IdrisDefns Idris where
--   type Bin Idris = IdrisBin
--   langName _ = "idris"
--   langExt _ = ".idr"
--   needLang _ = do
--     opts <- needIdrisInstallOpts
--     needIdris opts
--   needModule gen size = do
--     let path = generatorOutputDir "idris" (T.unpack (genName gen)) (show size) ".idr"
--     putInfo $ "# generating " <> decodeOS path
--     writeBinaryHandleChanged path (genModuleVia (runIdrisM def) size gen)
--     pure path
--   benchmarkModule _ = idrisCheckBench
--   cleanBuildArtifacts _ dir = removeFilesAfter (decodeOS [osp|$dir/build|]) ["*"]

-- instance ShakeLang LeanMod LeanHeader LeanDefns Lean where
--   type Bin Lean = LeanBin
--   langName _ = "lean"
--   langExt _ = ".lean"
--   needLang _ = do
--     opts <- needLeanInstallOpts
--     needLean opts
--   needModule gen size = do
--     let path = generatorOutputDir "lean" (T.unpack (genName gen)) (show size) ".lean"
--     putInfo $ "# generating " <> decodeOS path
--     writeBinaryHandleChanged path (genModuleVia runLeanM size gen)
--     pure path
--   benchmarkModule _ = leanCheckBench
--   cleanBuildArtifacts _ _ = pure ()

-- instance ShakeLang RocqMod RocqHeader RocqDefns Rocq where
--   type Bin Rocq = RocqBin
--   langName _ = "rocq"
--   langExt _ = ".v"
--   needLang _ = do
--     opts <- needRocqInstallOpts
--     needRocq opts
--   needModule gen size = do
--     let path = generatorOutputDir "rocq" (T.unpack (genName gen)) (show size) ".v"
--     putInfo $ "# generating " <> decodeOS path
--     writeBinaryHandleChanged path (genModuleVia (runRocqM def) size gen)
--     pure path
--   benchmarkModule _ = rocqCheckBench
--   cleanBuildArtifacts _ dir = removeFilesAfter (decodeOS dir) ["*.vo", "*.vok", "*.vos", "*.glob"]
