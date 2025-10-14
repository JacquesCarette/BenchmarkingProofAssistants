{-# LANGUAGE MultiParamTypeClasses #-}
-- | Helpers for installing @agda@.
module Panbench.Shake.Lang.Agda
  ( -- * Agda installation
    AgdaQ(..)
  , defaultAgdaInstallRev
  , defaultAgdaInstallFlags
  , needAgdaInstallOpts
  , AgdaBin
  , needAgda
  -- * Running Agda
  , agdaCheck
  , agdaCheckBench
  , agdaDoctor
  -- * Shake Rules
  , agdaRules
  ) where

import Data.Char

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.Benchmark
import Panbench.Shake.File
import Panbench.Shake.Git
import Panbench.Shake.Store

import System.Directory qualified as Dir
import System.FilePath

--------------------------------------------------------------------------------
-- Agda installation

-- | Query for installing a version of @agda@.
data AgdaQ = AgdaQ
  { agdaInstallRev :: String
  -- ^ Revision of Agda to install.
  , agdaInstallFlags :: [String]
  -- ^ Compile flags used to build Agda.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Default revision of @agda@ to install.
defaultAgdaInstallRev :: String
defaultAgdaInstallRev = "v2.8.0"

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

-- | Docs for the @install-agda@ rule.
agdaInstallDocs :: String
agdaInstallDocs = unlines
  [ "Install a version of agda."
  , "  Can be configured with the following environment variables:"
  , "  * $AGDA_VERSION: select the revision of agda to install."
  , "    Defaults to " <> defaultAgdaInstallRev
  , "  * $AGDA_CABAL_FLAGS: pass flags to cabal when building agda."
  , "    Arguments should be separated by spaces."
  , "    Defaults to " <> unwords defaultAgdaInstallFlags
  ]


-- | Get the version of @agda@ to install from the @$AGDA_VERSION@ environment variable.
needAgdaInstallRev :: Action String
needAgdaInstallRev = getEnvWithDefault defaultAgdaInstallRev "AGDA_VERSION"

-- | Get cabal flags to build @agda@ from the @$AGDA_CABAL_FLAGS@ environment variable.
needAgdaInstallFlags :: Action [String]
needAgdaInstallFlags = maybe defaultAgdaInstallFlags words <$> getEnv "AGDA_CABAL_FLAGS"

-- | Get install options for @agda@ from environment variables.
needAgdaInstallOpts :: Action AgdaQ
needAgdaInstallOpts = do
  agdaInstallRev <- needAgdaInstallRev
  agdaInstallFlags <- needAgdaInstallFlags
  pure AgdaQ {..}

-- | Run a command with access to a Agda git worktree.
withAgdaWorktree
  :: String -- ^ Revision of Agda to check out.
  -> FilePath -- ^ Store directory.
  -> (FilePath -> Action a) -- ^ Action, parameterized by the worktree directory.
  -> Action a
withAgdaWorktree rev storeDir act =
  let repoDir = "_build/repos/agda"
      workDir = replaceDirectory storeDir "_build/repos"
      worktree = GitWorktreeQ
        { gitWorktreeUpstream = "https://github.com/agda/agda.git"
        , gitWorktreeRepo = repoDir
        , gitWorktreeDir = workDir
        , gitWorktreeRev = rev
        }
  in withGitWorktree worktree (act workDir)

-- | Oracle for installing a version of Agda.
--
-- The oracle returns the absolute path to the produced @agda@ binary.
agdaInstall :: AgdaQ -> FilePath -> Action ()
agdaInstall AgdaQ{..} storeDir = do
  withAgdaWorktree agdaInstallRev storeDir \workDir -> do
    -- [TODO: Reed M, 14/07/2025] We could be more reproducible by allowing the
    -- user to specify a cabal lockfile.
    --
    -- Note that this also uses the system GHC: we could make this more configurable by
    -- calling out to @ghcup@, but let's just get things working for now
    withAllCores \nCores -> do
      command_ [Cwd workDir] "cabal" (["build", "agda", "--project-dir=.", "--jobs=" ++ show nCores] ++ agdaInstallFlags)
    Stdout listBinOut <- command [Cwd workDir] "cabal" (["list-bin", "agda", "--project-dir=."] ++ agdaInstallFlags)
    let outDir = takeDirectory $ takeWhile (not . isSpace) listBinOut
    copyDirectoryRecursive outDir storeDir

-- | A handle to an Agda binary.
data AgdaBin = AgdaBin
  { agdaBin :: FilePath
  }

-- | Require that a particular version of @agda@ is installed,
-- and return the absolute path pointing to the executable.
needAgda :: AgdaQ -> Action AgdaBin
needAgda q = do
  (store, _) <- askStoreOracle q
  path <- liftIO $ Dir.makeAbsolute (store </> "agda")
  pure $ AgdaBin
    { agdaBin = path
    }

--------------------------------------------------------------------------------
-- Running Agda

agdaCheckDefaultArgs :: FilePath -> [String]
agdaCheckDefaultArgs file = ["+RTS", "-M3.0G", "-RTS", file]

-- | Check an @agda@ file.
agdaCheck :: [CmdOption] -> AgdaBin -> FilePath -> Action ()
agdaCheck opts AgdaBin{..} file =
  command_ opts agdaBin (agdaCheckDefaultArgs file)

-- | Construct a benchmark for a given agda binary.
agdaCheckBench :: [CmdOption] -> [ResourceLimit] -> AgdaBin -> FilePath -> Action BenchmarkExecStats
agdaCheckBench opts limits AgdaBin{..} path =
  benchmarkCommand opts limits agdaBin (agdaCheckDefaultArgs path)

-- | Check that an @agda@ install is functioning by compiling an empty file.
agdaDoctor :: AgdaBin -> Action ()
agdaDoctor agda = do
  withTempDir \dir -> do
    let testFile = dir </> "Test.agda"
    liftIO $ writeFile testFile "module Test where"
    agdaCheck [Cwd dir] agda testFile

-- | Docs for the @doctor-agda@ rule.
agdaDoctorDocs :: String
agdaDoctorDocs = unlines
  [ "Check that an agda install is functional."
  , "  Can be configured with the following environment variables:"
  , "  * $AGDA_VERSION: select the revision of agda to install."
  , "    Defaults to " <> defaultAgdaInstallRev
  , "  * $AGDA_CABAL_FLAGS: pass flags to cabal when building agda."
  , "    Arguments should be separated by spaces."
  , "    Defaults to " <> unwords defaultAgdaInstallFlags
  ]

-- * Shake Rules for Agda
--
-- $shakeAgdaRules

-- | Shake rules for installing @agda@.
agdaRules :: Rules ()
agdaRules = do
  addStoreOracle "agda" agdaInstall

  withTargetDocs agdaInstallDocs $ phony "install-agda" do
    opts <- needAgdaInstallOpts
    _ <- needAgda opts
    pure ()

  withTargetDocs agdaDoctorDocs $ phony "doctor-agda" do
    opts <- needAgdaInstallOpts
    agda <- needAgda opts
    agdaDoctor agda

  phony "clean-agda" do
    removeFilesAfter "_build/repos" ["agda-*"]
    removeFilesAfter "_build/store" ["agda-*"]
    pruneGitWorktrees "_build/repos/agda"
