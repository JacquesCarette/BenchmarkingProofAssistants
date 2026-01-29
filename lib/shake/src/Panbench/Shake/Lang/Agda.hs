{-# LANGUAGE QuasiQuotes #-}

-- | Helpers for installing @agda@.
module Panbench.Shake.Lang.Agda
  ( -- * Agda installation
    AgdaQ(..)
  , defaultAgdaInstallFlags
  , needAgda
  , agdaRules
  ) where

import Data.Char
import Data.Text qualified as T

import Debug.Trace (traceMarkerIO)

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Grammar.Agda

import Panbench.Generator

import Panbench.Shake.AllCores
import Panbench.Shake.Benchmark
import Panbench.Shake.Cabal
import Panbench.Shake.File
import Panbench.Shake.Lang
import Panbench.Shake.Git
import Panbench.Shake.Path
import Panbench.Shake.Store

import System.Directory.OsPath qualified as Dir

--------------------------------------------------------------------------------
-- Agda installation

-- | Query for installing a version of @agda@.
data AgdaQ = AgdaQ
  { agdaInstallRev :: String
  -- ^ Revision of Agda to install.
  , agdaInstallFlags :: [String]
  -- ^ Compile flags used to build Agda.
  , agdaHackageIndex :: String
  -- ^ Hackage index to use when building Agda.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

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

-- | Run a command with access to a Agda git worktree.
withAgdaRepo
  :: String -- ^ Revision of Agda to check out.
  -> OsPath -- ^ Store directory.
  -> (OsPath -> Action a) -- ^ Action, parameterized by the worktree directory.
  -> Action a
withAgdaRepo rev storeDir act =
  let workDir = replaceDirectory storeDir [osp|_build/repos|]
      clone = GitCloneQ
        { gitCloneUpstream = "https://github.com/agda/agda.git"
        , gitCloneDir = workDir
        , gitCloneRevision = rev
        }
  in withGitClone clone (act workDir)

-- | Oracle for installing a version of Agda.
--
-- The oracle returns the absolute path to the produced @agda@ binary.
agdaInstallOracle :: AgdaQ -> OsPath -> Action ()
agdaInstallOracle AgdaQ{..} storeDir = do
  withAgdaRepo agdaInstallRev storeDir \workDir -> do
    cabal <- needCabal $ CabalQ
      { cabalHackageIndex = agdaHackageIndex
      }
    -- [TODO: Reed M, 27/12/2025]
    -- This uses the system GHC: we could make this more configurable by calling out to @ghcup@.
    withAllCores \nCores -> do
      cabalCommand_ [Cwd (decodeOS workDir)] cabal (["build", "agda", "--project-dir=.", "--jobs=" ++ show nCores] ++ agdaInstallFlags)
    Stdout listBinOut <- cabalCommand [Cwd (decodeOS workDir)] cabal (["list-bin", "agda", "--project-dir=."] ++ agdaInstallFlags)
    let outDir = takeDirectory $ encodeOS $ takeWhile (not . isSpace) listBinOut
    copyDirectoryRecursive outDir storeDir

-- | Require that a particular version of @agda@ is installed,
-- and return the absolute path pointing to the executable.
needAgda :: String -> AgdaOpts -> AgdaQ -> Action (Lang AgdaHeader AgdaDefns)
needAgda agdaName agdaOpts agdaInstall = do
  liftIO $ traceMarkerIO "Requiring Agda"
  store <- storeOraclePath <$> askStoreOracle agdaInstall
  agdaBin <- liftIO $ Dir.makeAbsolute [osp|$store/agda|]
  pure $ Lang
    { langName = agdaName
    , langExt = ".agda"
    , needModule = \gen size -> do
        let path = generatorOutputDir "agda" (T.unpack (genName gen)) (show size) ".agda"
        putInfo $ "# generating " <> decodeOS path
        writeBinaryHandleChanged path (genModuleVia (runAgdaM agdaOpts) size gen)
        pure path
    , cleanBuildArtifacts = \dir ->
        removeFilesAfter (decodeOS dir) ["*.agdai"]
    , benchmarkModule = \opts timeout path ->
        benchmarkCommand opts timeout agdaBin [decodeOS path]
    }

-- * Shake Rules for Agda
--
-- $shakeAgdaRules

-- | Shake rules for installing @agda@.
agdaRules :: Rules ()
agdaRules = do
  addStoreOracle "agda" agdaInstallOracle
  phony "clean-agda" do
    removeFilesAfter "_build/repos" ["agda-*"]
    removeFilesAfter "_build/store" ["agda-*"]
