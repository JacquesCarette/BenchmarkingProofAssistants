{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
-- | @shake@ build rules for @panbench@ modules.
module Panbench.Shake.Lang
  ( -- * Shake rules for languages
    ShakeLang(..)
  , generatorOutputDir
  , SomeLangModule(..)
  , needModules
  ) where


import Data.Aeson qualified as JSON
import Data.Text qualified as T
import Data.Traversable
import Data.Word

import Development.Shake

import Panbench.Generator

import Panbench.Grammar
import Panbench.Grammar.Agda
import Panbench.Grammar.Idris
import Panbench.Grammar.Lean
import Panbench.Grammar.Rocq

import Panbench.Shake.Benchmark
import Panbench.Shake.File
import Panbench.Shake.Lang.Agda
import Panbench.Shake.Lang.Idris
import Panbench.Shake.Lang.Lean
import Panbench.Shake.Lang.Rocq
import Panbench.Shake.Path

-- | Rules for installing, building, and cleaning files
-- for a given language
class (Module m hdr defn) => ShakeLang m hdr defn rep | m -> rep, rep -> m where
  -- Cannot be injective, as our binary types aren't indexed by an @ann@ type parameter.
  type Bin rep

  -- | User-facing name of the language.
  langName :: forall rep' -> (rep ~ rep') => String

  -- | File extension of a language
  langExt :: forall rep' -> (rep ~ rep') => String

  -- | Require that the language is installed.
  --
  -- We use @forall rep' -> (rep ~ rep') => Action FilePath@
  -- so that 'needLang' can be called with an explicit type
  -- argument like @needLang Agda@.
  needLang :: forall rep' -> (rep ~ rep') => Action (Bin rep)

  -- | Require that a module described by a 'Gen' get built.
  -- This should return an absolute filepath.
  --
  -- We require a 'JSON.ToJSON' constraint to be able to properly encode the sizes
  -- when we pass off the results to vega, and we need 'Show' to construct paths for sizes.
  needModule :: (JSON.ToJSON size, Show size) => GenModule size hdr defn -> size -> Action OsPath

  -- | Clean all build artifacts for a language in the given directory.
  cleanBuildArtifacts :: forall rep' -> (rep ~ rep') => OsPath -> Action ()

  -- | Create a benchmarking command for a language.
  benchmarkModule :: forall rep' -> (rep ~ rep') => [CmdOption] -> Word64 -> Bin rep -> OsPath -> Action BenchmarkExecStats

-- | Existential for 'GenModule' that packs up evidence that we
-- actually know how generate and typecheck the module.
data SomeLangModule where
  -- | Pack a 'GenModule' alongside a 'ShakeLang' dictionary.
  SomeLangModule
    :: forall m hdr defn rep size. (ShakeLang m hdr defn rep, Show size, JSON.ToJSON size)
    => GenModule size hdr defn
    -> size
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
  for gens \(SomeLangModule gen size) -> do
    path <- needModule gen size
    pure (splitFileName path)

--------------------------------------------------------------------------------
-- Instances

instance ShakeLang AgdaMod AgdaHeader AgdaDefn Agda where
  type Bin Agda = AgdaBin
  langName _ = "agda"
  langExt _ = ".agda"
  needLang _ = do
    opts <- needAgdaInstallOpts
    needAgda opts
  needModule gen size = do
    let path = generatorOutputDir "agda" (T.unpack (genName gen)) (show size) ".agda"
    putInfo $ "# generating " <> decodeOS path
    writeBinaryHandleChanged path (genModuleVia getAgdaMod size gen)
    pure path
  benchmarkModule _ = agdaCheckBench
  cleanBuildArtifacts _ dir = removeFilesAfter (decodeOS dir) ["*.agdai"]

instance ShakeLang IdrisMod IdrisHeader IdrisDefn Idris where
  type Bin Idris = IdrisBin
  langName _ = "idris"
  langExt _ = ".idr"
  needLang _ = do
    opts <- needIdrisInstallOpts
    needIdris opts
  needModule gen size = do
    let path = generatorOutputDir "idris" (T.unpack (genName gen)) (show size) ".idr"
    putInfo $ "# generating " <> decodeOS path
    writeBinaryHandleChanged path (genModuleVia getIdrisMod size gen)
    pure path
  benchmarkModule _ = idrisCheckBench
  cleanBuildArtifacts _ dir = removeFilesAfter (decodeOS [osp|$dir/build|]) ["*"]

instance ShakeLang LeanMod LeanHeader LeanDefn Lean where
  type Bin Lean = LeanBin
  langName _ = "lean"
  langExt _ = ".lean"
  needLang _ = do
    opts <- needLeanInstallOpts
    needLean opts
  needModule gen size = do
    let path = generatorOutputDir "lean" (T.unpack (genName gen)) (show size) ".lean"
    putInfo $ "# generating " <> decodeOS path
    writeBinaryHandleChanged path (genModuleVia getLeanMod size gen)
    pure path
  benchmarkModule _ = leanCheckBench
  cleanBuildArtifacts _ _ = pure ()

instance ShakeLang RocqMod RocqHeader RocqDefn Rocq where
  type Bin Rocq = RocqBin
  langName _ = "rocq"
  langExt _ = ".v"
  needLang _ = do
    opts <- needRocqInstallOpts
    needRocq opts
  needModule gen size = do
    let path = generatorOutputDir "rocq" (T.unpack (genName gen)) (show size) ".v"
    putInfo $ "# generating " <> decodeOS path
    writeBinaryHandleChanged path (genModuleVia getRocqMod size gen)
    pure path
  benchmarkModule _ = rocqCheckBench
  cleanBuildArtifacts _ dir = removeFilesAfter (decodeOS dir) ["*.vo", "*.vok", "*.vos", "*.glob"]
