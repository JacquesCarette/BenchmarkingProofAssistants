{-# LANGUAGE QuasiQuotes #-}

-- | Helpers for installing @lean@.
module Panbench.Shake.Lang.Lean
  ( -- * Installing Lean
    LeanQ(..)
  , defaultLeanCMakeFlags
  , defaultLeanMakeFlags
  -- * Shake rules
  , needLean
  , leanRules
  ) where

import Data.Text qualified as T

import Debug.Trace (traceMarkerIO)

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Grammar.Lean

import Panbench.Generator

import Panbench.Shake.AllCores
import Panbench.Shake.Benchmark
import Panbench.Shake.File
import Panbench.Shake.Lang
import Panbench.Shake.Git
import Panbench.Shake.Path
import Panbench.Shake.Store

import System.Directory.OsPath qualified as Dir

-- * Lean Installation
--
-- $shakeLeanInstall

-- | Query for installing a version of @lean@.
data LeanQ = LeanQ
  { leanInstallRev :: String
  -- ^ Revision of Lean to install.
  , leanCMakeFlags :: [String]
  -- ^ CMake flags used to build Lean.
  , leanMakeFlags :: [String]
  -- ^ Make flags used to build Lean.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LeanQ = FilePath

-- | Default flags to pass to @cmake@ when compiling lean.
defaultLeanCMakeFlags :: [String]
defaultLeanCMakeFlags = ["--preset=release"]

-- | Default flags to pass to @make@ when compiling lean.
defaultLeanMakeFlags :: [String]
defaultLeanMakeFlags = []

-- | Run a command with access to a Lean 4 git clone.
withLeanClone
  :: String -- ^ Revision of Lean 4 to check out.
  -> OsPath -- ^ Store directory.
  -> (OsPath -> Action a) -- ^ Action, parameterized by the clone directory.
  -> Action a
withLeanClone rev storeDir act =
  let workDir = replaceDirectory storeDir [osp|_build/repos|]
      clone = GitCloneQ
        { gitCloneUpstream = "https://github.com/leanprover/lean4.git"
        , gitCloneDir = workDir
        , gitCloneRevision = rev
        }
  in withGitClone clone (act workDir)

-- | Oracle for installing a version of Lean 4.
--
-- The oracle returns the absolute path to the produced @lean@ binary.
leanInstall :: LeanQ -> OsPath -> Action ()
leanInstall LeanQ{..} storeDir = do
    withLeanClone leanInstallRev storeDir \workDir -> do
      let stage = "stage2"
      withAllCores \nCores -> do
        command_ [Cwd (decodeOS workDir)] "cmake" leanCMakeFlags
        command_ [Cwd (decodeOS workDir)] "make" ([stage, "-C", "build/release", "-j" ++ show nCores] ++ leanMakeFlags)
      copyDirectoryRecursive [osp|$workDir/build/release/$stage|] storeDir
    -- This shaves ~750 MB off the size of the store.
    removePathForcibly [osp|$storeDir/lib/temp|]

-- | Require that a particular version of @lean@ is installed,
-- and return the absolute path pointing to the executable.
needLean :: String -> LeanOpts -> LeanQ -> Action (Lang LeanHeader LeanDefns)
needLean leanName opts q = do
  liftIO $ traceMarkerIO "Requiring Lean"
  store <- storeOraclePath <$> askStoreOracle q
  leanBin <- liftIO $ Dir.makeAbsolute [osp|$store/bin/lean|]
  pure $ Lang
    { langName = leanName
    , langExt = ".lean"
    , needModule = \gen size -> do
        let path = generatorOutputDir leanName (T.unpack (genName gen)) (show size) ".lean"
        putInfo $ "# generating " <> decodeOS path
        writeBinaryHandleChanged path (genModuleVia (runLeanM opts) size gen)
        pure path
    , cleanBuildArtifacts = \_dir ->
        pure ()
    , benchmarkModule = \opts timeout path ->
        benchmarkCommand opts timeout leanBin ["-D", "maxHeartbeats=0", decodeOS path]
    }

--------------------------------------------------------------------------------
-- Shake Rules

-- | Shake rules for installing @lean@.
leanRules :: Rules ()
leanRules = do
  addStoreOracle "lean" leanInstall

  phony "clean-lean" do
    removeFilesAfter "_build/repos" ["lean-*"]
    removeFilesAfter "_build/store" ["lean-*"]
