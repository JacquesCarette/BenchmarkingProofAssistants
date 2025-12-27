{-# LANGUAGE QuasiQuotes #-}

-- | Helpers for installing @lean@.
module Panbench.Shake.Lang.Lean
  ( -- * Installing Lean
    LeanQ(..)
  , defaultLeanCMakeFlags
  , defaultLeanMakeFlags
  -- * Shake rules
  , leanRules
  ) where

import Data.Text qualified as T

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

-- | Run a command with access to a Lean 4 git worktree.
withLeanWorktree
  :: String -- ^ Revision of Lean 4 to check out.
  -> OsPath -- ^ Store directory.
  -> (OsPath -> Action a) -- ^ Action, parameterized by the worktree directory.
  -> Action a
withLeanWorktree rev storeDir act =
  let repoDir = [osp|_build/repos/lean|]
      workDir = replaceDirectory storeDir [osp|_build/repos|]
      worktree = GitWorktreeQ
        { gitWorktreeUpstream = "https://github.com/leanprover/lean4.git"
        , gitWorktreeRepo = repoDir
        , gitWorktreeDir = workDir
        , gitWorktreeRev = rev
        }
  in withGitWorktree worktree (act workDir)

-- | Oracle for installing a version of Lean 4.
--
-- The oracle returns the absolute path to the produced @lean@ binary.
leanInstall :: LeanQ -> OsPath -> Action ()
leanInstall LeanQ{..} storeDir = do
    withLeanWorktree leanInstallRev storeDir \workDir -> do
      withAllCores \nCores -> do
        command_ [Cwd (decodeOS workDir)] "cmake" leanCMakeFlags
        command_ [Cwd (decodeOS workDir)] "make" (["stage3", "-C", "build/release", "-j" ++ show nCores] ++ leanMakeFlags)
      copyDirectoryRecursive [osp|$workDir/build/release/stage3|] storeDir

-- | Require that a particular version of @lean@ is installed,
-- and return the absolute path pointing to the executable.
needLean :: String -> LeanQ -> Action (Lang LeanHeader LeanDefns)
needLean leanName q = do
  (store, _) <- askStoreOracle q
  leanBin <- liftIO $ Dir.makeAbsolute [osp|$store/bin/lean|]
  pure $ Lang
    { langName = leanName
    , langExt = ".lean"
    , needModule = \gen size -> do
        let path = generatorOutputDir "lean" (T.unpack (genName gen)) (show size) ".lean"
        putInfo $ "# generating " <> decodeOS path
        writeBinaryHandleChanged path (genModuleVia runLeanM size gen)
        pure path
    , cleanBuildArtifacts = \_dir ->
        pure ()
    , benchmarkModule = \opts timeout path ->
        benchmarkCommand opts timeout leanBin ["-D", "maxHeartbeats=0", decodeOS path]
    }

--------------------------------------------------------------------------------
-- Shake Rules

-- | Shake rules for installing @lean@.
leanRules :: Rules (String -> LeanQ -> Action (Lang LeanHeader LeanDefns))
leanRules = do
  addStoreOracle "lean" leanInstall

  phony "clean-lean" do
    removeFilesAfter "_build/repos" ["lean-*"]
    removeFilesAfter "_build/store" ["lean-*"]
    pruneGitWorktrees [osp|_build/repos/lean|]

  pure needLean
