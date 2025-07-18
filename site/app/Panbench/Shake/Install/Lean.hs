-- | Helpers for installing @lean@.
module Panbench.Shake.Install.Lean
  ( LeanInstallQ(..)
  , defaultLeanCMakeFlags
  , defaultLeanMakeFlags
  , needLeanInstall
  , leanInstallRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.Git

import System.Directory qualified as Dir
import System.FilePath

-- | Query for installing a version of @lean@.
data LeanInstallQ = LeanInstallQ
  { leanInstallRev :: String
  -- ^ Revision of Lean to install.
  , leanCMakeFlags :: [String]
  -- ^ CMake flags used to build Lean.
  , leanMakeFlags :: [String]
  -- ^ Make flags used to build Lean.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LeanInstallQ = FilePath

-- | Default flags to pass to @cmake@ when compiling lean.
defaultLeanCMakeFlags :: [String]
defaultLeanCMakeFlags = ["--preset", "release"]

-- | Default flags to pass to @make@ when compiling lean.
defaultLeanMakeFlags :: [String]
defaultLeanMakeFlags = []

-- | Oracle for installing a version of Lean 4.
--
-- The oracle returns the absolute path to the produced @lean@ binary.
leanInstallOracle :: Rules (LeanInstallQ -> Action FilePath)
leanInstallOracle =
  addOracle \LeanInstallQ{..} -> do
    let repoDir = "_build/repos/lean"
    let workDir = gitRevWorktreePath repoDir leanInstallRev
    needGitWorktree $ GitWorktreeQ
      { gitWorktreeUpstream = "https://github.com/leanprover/lean4.git"
      , gitWorktreeRepo = repoDir
      , gitWorktreeDir = workDir
      , gitWorktreeRev = leanInstallRev
      }
    withAllCores \nCores -> do
      command_ [Cwd workDir] "cmake" leanCMakeFlags
      command_ [Cwd workDir] "make" (["stage3", "-C", "build/release", "-j" ++ show nCores] ++ leanMakeFlags)
      cwd <- liftIO $ Dir.getCurrentDirectory
      pure (cwd </> workDir </> "build/release/stage3/bin/lean")

-- | Require that a particular version of @lean@ is installed,
-- and return the absolute path pointing to the executable.
needLeanInstall :: LeanInstallQ -> Action FilePath
needLeanInstall = askOracle

-- | Shake rules for installing @lean@.
leanInstallRules :: Rules ()
leanInstallRules = do
  _ <- leanInstallOracle
  pure ()
