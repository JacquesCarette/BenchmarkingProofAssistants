{-# LANGUAGE QuasiQuotes #-}

-- | Helpers for installing @lean@.
module Panbench.Shake.Lang.Lean
  ( -- * Installing Lean
    LeanQ(..)
  , defaultLeanInstallRev
  , defaultLeanCMakeFlags
  , defaultLeanMakeFlags
  , needLeanInstallOpts
  , LeanBin(..)
  , needLean
    -- * Running Lean
  , leanCheck
  , leanCheckBench
  , leanDoctor
  -- * Shake rules
  , leanRules
  ) where

import Data.Word

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.Benchmark
import Panbench.Shake.Command
import Panbench.Shake.File
import Panbench.Shake.Git
import Panbench.Shake.Path
import Panbench.Shake.Store

import System.Directory.OsPath qualified as Dir
import System.File.OsPath qualified as File

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

-- | Default revision of @lean@ to install.
defaultLeanInstallRev :: String
defaultLeanInstallRev = "v4.21.0"

-- | Default flags to pass to @cmake@ when compiling lean.
defaultLeanCMakeFlags :: [String]
defaultLeanCMakeFlags = ["--preset=release"]

-- | Default flags to pass to @make@ when compiling lean.
defaultLeanMakeFlags :: [String]
defaultLeanMakeFlags = []

-- | Docs for the @install-lean@ rule.
leanInstallDocs :: String
leanInstallDocs = unlines
  [ "Install a version of lean."
  , "  Can be configured with the following environment variables:"
  , "  * $LEAN_VERSION: select the revision of lean to install."
  , "    Defaults to " <> defaultLeanInstallRev
  , "  * $LEAN_CMAKE_FLAGS: pass flags to cmake when building lean."
  , "    Arguments should be separated by spaces."
  , "    Defaults to " <> unwords defaultLeanCMakeFlags
  , "  * $LEAN_MAKE_FLAGS: pass flags to make when building lean."
  , "    Arguments should be separated by spaces."
  , "    Defaults to " <> unwords defaultLeanMakeFlags
  ]

-- | Get the version of @lean@ to install from the @$LEAN_VERSION@ environment variable.
needLeanInstallRev :: Action String
needLeanInstallRev = getEnvWithDefault defaultLeanInstallRev "LEAN_VERSION"

-- | Get cmake flags to build @lean@ from the @$LEAN_CMAKE_FLAGS@ environment variable.
needLeanCMakeFlags :: Action [String]
needLeanCMakeFlags = maybe defaultLeanCMakeFlags words <$> getEnv "LEAN_CMAKE_FLAGS"

-- | Get make flags to build @lean@ from the @$LEAN_MAKE_FLAGS@ environment variable.
needLeanMakeFlags :: Action [String]
needLeanMakeFlags = maybe defaultLeanMakeFlags words <$> getEnv "LEAN_MAKE_FLAGS"

-- | Get install options for @lean@ from environment variables.
needLeanInstallOpts :: Action LeanQ
needLeanInstallOpts = do
  leanInstallRev <- needLeanInstallRev
  leanCMakeFlags <- needLeanCMakeFlags
  leanMakeFlags <- needLeanMakeFlags
  pure LeanQ {..}

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

data LeanBin = LeanBin
  { leanBin :: OsPath
  }

-- | Require that a particular version of @lean@ is installed,
-- and return the absolute path pointing to the executable.
needLean :: LeanQ -> Action LeanBin
needLean q = do
  (store, _) <- askStoreOracle q
  path <- liftIO $ Dir.makeAbsolute [osp|$store/bin/lean|]
  pure $ LeanBin
    { leanBin = path
    }

--------------------------------------------------------------------------------
-- Running Lean

-- | Default arguments for @lean@ to check a file.
leanCheckDefaultArgs :: OsPath -> [String]
leanCheckDefaultArgs file = ["-D", "maxRecDepth=2000", "-D", "maxHeartbeats=0", decodeOS file]

-- | Check a file using a @lean@ installation.
leanCheck :: [CmdOption] -> LeanBin -> OsPath -> Action ()
leanCheck opts LeanBin{..} file =
  osCommand_ opts leanBin (leanCheckDefaultArgs file)

-- | Construct a benchmark for a given @lean@ binary.
leanCheckBench :: [CmdOption] -> Word64 -> LeanBin -> OsPath -> Action BenchmarkExecStats
leanCheckBench opts limits LeanBin{..} path =
  benchmarkCommand opts limits leanBin (leanCheckDefaultArgs path)


-- | Check that a @lean@ install is functioning by compiling an empty file.
leanDoctor :: LeanBin -> Action ()
leanDoctor lean = do
  withTempDir \dir -> do
    let testFile = encodeOS dir </> [osp|"Test.lean"|]
    liftIO $ File.writeFile' testFile ""
    leanCheck [Cwd dir] lean testFile


-- | Docs for the @doctor-lean@ rule.
leanDoctorDocs :: String
leanDoctorDocs = unlines
  [ "Check that an of lean is functional."
  , "  Can be configured with the following environment variables:"
  , "  * $LEAN_VERSION: select the revision of lean to install."
  , "    Defaults to " <> defaultLeanInstallRev
  , "  * $LEAN_CMAKE_FLAGS: pass flags to cmake when building lean."
  , "    Arguments should be separated by spaces."
  , "    Defaults to " <> unwords defaultLeanCMakeFlags
  , "  * $LEAN_MAKE_FLAGS: pass flags to make when building lean."
  , "    Arguments should be separated by spaces."
  , "    Defaults to " <> unwords defaultLeanMakeFlags
  ]


--------------------------------------------------------------------------------
-- Shake Rules

-- | Shake rules for installing @lean@.
leanRules :: Rules ()
leanRules = do
  addStoreOracle "lean" leanInstall

  withTargetDocs leanInstallDocs $ phony "install-lean" do
    opts <- needLeanInstallOpts
    _ <- needLean opts
    pure ()

  withTargetDocs leanDoctorDocs $ phony "doctor-lean" do
    opts <- needLeanInstallOpts
    lean <- needLean opts
    leanDoctor lean

  phony "clean-lean" do
    removeFilesAfter "_build/repos" ["lean-*"]
    removeFilesAfter "_build/store" ["lean-*"]
    pruneGitWorktrees [osp|"_build/repos/lean"|]
