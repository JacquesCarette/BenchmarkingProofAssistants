-- | @shake@ build rules for @panbench@ modules.
module Panbench.Shake.Lang
  ( -- $shakeLang
    needLang
  , cleanBuildArtifacts
  , langCheckDefaultArgs
    -- $shakeGenerate
  , GenerateModule(..)
  , needModule
  , needModules
  -- $shakeLangRules
  , langRules
  ) where

import Data.Char

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Numeric.Natural

import System.FilePath

import Panbench.Shake.File
import Panbench.Shake.Lang.Agda
import Panbench.Shake.Lang.Lean
import Panbench.Shake.Lang.Idris
import Panbench.Shake.Lang.Rocq
import Panbench

import Panbench.Lang qualified as Lang

-- * Shake rules for language-dependent things
--
-- $shakeLang

-- | Find the default executable for a given @'Lang'@.
--
-- Always returns an absolute path.
needLang :: Lang -> Action FilePath
needLang Agda = do
  opts <- needAgdaInstallOpts
  needAgda opts
needLang Idris = do
  opts <- needIdrisInstallOpts
  needIdris opts
needLang Lean = do
  opts <- needLeanInstallOpts
  needLean opts
needLang Rocq = do
  opts <- needRocqInstallOpts
  needRocq opts

-- | Remove all build artifacts for a @'Lang'@ in a directory.
cleanBuildArtifacts :: Lang -> FilePath -> Action ()
cleanBuildArtifacts lang dir =
  removeFilesAfter dir (Lang.buildArtifacts lang)

-- | Default arguments for a @'Lang'@ to typecheck a file.
langCheckDefaultArgs :: Lang -> FilePath -> [String]
langCheckDefaultArgs Agda = agdaCheckDefaultArgs
langCheckDefaultArgs Idris = idrisCheckDefaultArgs
langCheckDefaultArgs Lean = leanCheckDefaultArgs
langCheckDefaultArgs Rocq = rocqCheckDefaultArgs

-- * Shake rules for compiling generators

-- | Shake query for compiling a @panbench@ generator by name.
--
-- The generator name should be listed in @generators/panbench-generators.cabal@.
newtype GeneratorQ = GeneratorQ String
  deriving newtype (Eq, Ord, Show, Hashable, Binary, NFData)

type instance RuleResult GeneratorQ = FilePath

-- | Compile a generator, and return the absolute path of
-- the resulting binary.
compileGenerator :: GeneratorQ -> Action FilePath
compileGenerator (GeneratorQ gen) = do
  need ["generators" </> "app" </> gen <.> "hs"]
  command_ [] "cabal" ["build", gen]
  Stdout out <- command [] "cabal" ["list-bin", gen]
  pure (takeWhile (not . isSpace) out)

-- * Shake rules for generating per-language modules
--
-- $shakeGenerate
--
-- We will need to write out module files to disk to be able
-- to pass them off to our various proof assistants. Moreover,
-- we also need to write multiple versions of the same file with
-- different size parameters. We will use the following convention
-- for paths:
--
-- > _build/lang/gen/n/mod.ext
--
-- Where @lang@ is the language name, @gen@ is the name of the generator,
-- @n@ is the size parameter, and @mod.ext@ is the rendered module file.
--
-- WARNING: This convention is subject to change, and should not be relied
-- on by external programs.

-- | Query for rendering a module using a generator.
data GenerateModule = GenerateModule
  { generatorName :: String
  -- ^ The generator to use.
  , generatorLang :: Lang
  -- ^ The language to generate.
  , generatorSize :: Natural
  -- ^ Size of the module to generate.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult GenerateModule = ()

-- | Construct a relative @'FilePath'@ for a file
-- that will contain the result of a @'GenerateModule'@ query.
generatorOutputDir :: GenerateModule -> FilePath
generatorOutputDir GenerateModule{..} =
  "_build" </> Lang.name generatorLang </> generatorName </> show generatorSize
  </> generatorName <.> Lang.fileExt generatorLang

-- | Request that a single module be generated.
--
-- This query is subject to caching.
needModule :: GenerateModule -> Action (FilePath, FilePath)
needModule gen = do
  (path, _) <- askFileCacheOracle gen
  return (splitFileName path)

-- | Request that a list of modules be generated in parallel.
--
-- This query is subject to caching.
needModules :: [GenerateModule] -> Action [(FilePath, FilePath)]
needModules gens = do
  paths <- fmap fst <$> asksFileCacheOracle gens
  return (fmap splitFileName paths)

-- | Rules for module generation.
langRules :: Rules ()
langRules = do
  needGenerator <- newCache compileGenerator
  addFileCacheOracle generatorOutputDir (\_ -> pure ()) \GenerateModule{..} -> do
    generatorBin <- needGenerator (GeneratorQ generatorName)
    Stdout out <- command [] generatorBin ["generate", "--size", show generatorSize, "--language", Lang.name generatorLang]
    pure ((), out)
