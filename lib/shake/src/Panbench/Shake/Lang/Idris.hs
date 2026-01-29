{-# LANGUAGE QuasiQuotes #-}

-- | Shake rules for compiling a particular version of @idris2@.
module Panbench.Shake.Lang.Idris
  ( -- * Installing Idris
    IdrisQ(..)
  , SchemeCompiler(..)
    -- * Shake Rules
  , needIdris
  , idrisRules
  ) where

import Data.Default
import Data.Text qualified as T

import Debug.Trace (traceMarkerIO)

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Grammar.Idris

import Panbench.Generator

import Panbench.Shake.AllCores
import Panbench.Shake.Benchmark
import Panbench.Shake.Chez
import Panbench.Shake.File
import Panbench.Shake.Lang
import Panbench.Shake.Git
import Panbench.Shake.Make
import Panbench.Shake.Path
import Panbench.Shake.Store


-- * Idris 2 Installation
--
-- $shakeIdrisInstall

-- | Query for installing a version of @idris2@.
data IdrisQ = IdrisQ
  { idrisInstallRev :: String
  -- ^ Revision of @idris2@ to install.
  , idrisInstallScheme :: SchemeCompiler
  -- ^ Which scheme compiler to use to install @idris2@.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Scheme compiler to use to compile @idris2@.
data SchemeCompiler
  = Chez
  -- ^ Compile @idris2@ using Chez scheme.
  | Racket
  -- ^ Compile @idris2@ using Racket.
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult IdrisQ = FilePath

-- | Run a command with access to a Idris 2 git clone.
withIdrisClone
  :: String -- ^ Revision of Idris 2 to check out.
  -> OsPath -- ^ Store directory.
  -> (OsPath -> Action a) -- ^ Action, parameterized by the clone directory.
  -> Action a
withIdrisClone rev storeDir act =
  let workDir = replaceDirectory storeDir [osp|_build/repos|]
      clone = GitCloneQ
        { gitCloneUpstream = "https://github.com/idris-lang/Idris2.git"
        , gitCloneDir = workDir
        , gitCloneRevision = rev
        }
  in withGitClone clone (act workDir)

-- | Oracle for installing a version of Idris 2.
idrisInstall :: IdrisQ -> OsPath -> Action ()
idrisInstall IdrisQ{..} storeDir = do
  withIdrisClone idrisInstallRev storeDir \workDir -> do
    withAllCores \nCores -> do
      case idrisInstallScheme of
        Chez -> do
          chez <- needChez
          makeCommand_ [Cwd (decodeOS workDir), AddEnv "SCHEME" (decodeOS chez)] ["bootstrap", "-j" ++ show nCores]
          makeCommand_ [Cwd (decodeOS workDir), AddEnv "PREFIX" (decodeOS storeDir)] ["install", "-j" ++ show nCores]
          -- We need to also specify IDRIS2_PREFIX to get idris to install libraries in the correct location.
          makeCommand_ [Cwd (decodeOS workDir), AddEnv "PREFIX" (decodeOS storeDir), AddEnv "IDRIS2_PREFIX" (decodeOS storeDir)] ["install-libs", "-j" ++ show nCores]
        Racket -> do
          makeCommand_ [Cwd (decodeOS workDir)] ["bootstrap-racket", "-j" ++ show nCores]
          makeCommand_ [Cwd (decodeOS workDir), AddEnv "PREFIX" (decodeOS storeDir), AddEnv "IDRIS2_CG" "racket"] ["install", "-j" ++ show nCores]
            -- Same deal with IDRIS2_PREFIX as above.
          makeCommand_ [Cwd (decodeOS workDir), AddEnv "PREFIX" (decodeOS storeDir), AddEnv "IDRIS2_PREFIX" (decodeOS storeDir), AddEnv "IDRIS2_CG" "racket"] ["install-libs", "-j" ++ show nCores]

-- | Require that a particular version of @idris2@ is installed,
-- and return the absolute path pointing to the executable.
needIdris :: String -> IdrisQ -> Action (Lang IdrisHeader IdrisDefns)
needIdris idrisName q = do
  liftIO $ traceMarkerIO "Requiring Idris"
  store <- storeOraclePath <$> askStoreOracle q
  let idris2Bin = [osp|$store/bin/idris2|]
  pure $ Lang
    { langName = idrisName
    , langExt = ".idr"
    , needModule = \gen size -> do
        let path = generatorOutputDir idrisName (T.unpack (genName gen)) (show size) ".idr"
        putInfo $ "# generating " <> decodeOS path
        writeBinaryHandleChanged path (genModuleVia (runIdrisM def) size gen)
        pure path
    , cleanBuildArtifacts = \dir ->
        removeFilesAfter (decodeOS [osp|$dir/build|]) ["*"]
    , benchmarkModule = \opts timeout path ->
        benchmarkCommand (opts ++ [AddEnv "IDRIS2_PREFIX" (decodeOS store)]) timeout idris2Bin ["--check", decodeOS path]
    }


--------------------------------------------------------------------------------
-- Shake rules

-- | Shake rules for installing @idris2@.
idrisRules :: Rules ()
idrisRules = do
  addStoreOracle "idris2" idrisInstall

  phony "clean-idris" do
    removeFilesAfter "_build/repos" ["idris2-*"]
    removeFilesAfter "_build/store" ["idris2-*"]
