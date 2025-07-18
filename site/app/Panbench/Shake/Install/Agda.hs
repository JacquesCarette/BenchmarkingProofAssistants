-- | Helpers for installing @agda@.
module Panbench.Shake.Install.Agda
  ( AgdaInstallQ(..)
  , defaultAgdaInstallFlags
  , needAgdaInstall
  , agdaInstallRules
  ) where

import Data.Char

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.Git

-- | Query for installing a version of @agda@.
data AgdaInstallQ = AgdaInstallQ
  { agdaInstallRev :: String
  -- ^ Revision of Agda to install.
  , agdaInstallFlags :: [String]
  -- ^ Compile flags used to build Agda.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult AgdaInstallQ = FilePath

-- | Default flags to use for Agda installation.
--
-- We use the following defaults:
-- * @-foptimise-heavily@ is the default for release builds of @agda@.
-- * @--disable-documentation@, as we don't need haddocks.
-- * @--disable-tests@, as we don't need to run the test suite.
-- * @--disable-profiling@, as we aren't creating profiled builds.
defaultAgdaInstallFlags :: [String]
defaultAgdaInstallFlags =
  [ "-foptimise-heavily"
  , "--disable-documentation"
  , "--disable-tests"
  , "--disable-profiling"
  ]

-- | Oracle for installing a version of Agda.
--
-- The oracle returns the absolute path to the produced @agda@ binary.
agdaInstallOracle :: Rules (AgdaInstallQ -> Action FilePath)
agdaInstallOracle =
  addOracle \AgdaInstallQ{..} -> do
    let repoDir = "_build/repos/agda"
    let workDir = gitRevWorktreePath repoDir agdaInstallRev
    needGitWorktree $ GitWorktreeQ
      { gitWorktreeUpstream = "https://github.com/agda/agda.git"
      , gitWorktreeRepo = repoDir
      , gitWorktreeDir = workDir
      , gitWorktreeRev = agdaInstallRev
      }
    -- [TODO: Reed M, 14/07/2025] We could be more reproducible by allowing the
    -- user to specify a cabal lockfile.
    --
    -- Note that this also uses the system GHC: we could make this more configurable by
    -- calling out to @ghcup@, but let's just get things working for now
    withAllCores \nCores -> do
      command_ [Cwd workDir] "cabal" (["build", "agda", "--project-dir=.", "--jobs=" ++ show nCores] ++ agdaInstallFlags)
      Stdout bin <- command [Cwd workDir] "cabal" (["list-bin", "agda", "--project-dir=.", "--jobs=" ++ show nCores] ++ agdaInstallFlags)
      pure (takeWhile (not . isSpace) bin)

-- | Require that a particular version of @agda@ is installed,
-- and return the absolute path pointing to the executable.
needAgdaInstall :: AgdaInstallQ -> Action FilePath
needAgdaInstall = askOracle

-- | Shake rules for installing @agda@.
agdaInstallRules :: Rules ()
agdaInstallRules = do
  _ <- agdaInstallOracle
  pure ()
