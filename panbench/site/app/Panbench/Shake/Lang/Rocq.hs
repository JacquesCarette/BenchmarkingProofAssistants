{-# LANGUAGE QuasiQuotes #-}

-- | Shake rules for @rocq@.
module Panbench.Shake.Lang.Rocq
  ( -- * Installing Rocq
    RocqQ(..)
  , defaultRocqOcamlCompiler
  -- $shakeRocqRules
  , rocqRules
  ) where

import Data.List
import Data.Text qualified as T

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Grammar.Rocq

import Panbench.Generator

import Panbench.Shake.AllCores
import Panbench.Shake.Benchmark
import Panbench.Shake.File
import Panbench.Shake.Lang
import Panbench.Shake.Git
import Panbench.Shake.Make
import Panbench.Shake.Opam
import Panbench.Shake.Path
import Panbench.Shake.Store

-- * Rocq Installation
--
-- $shakeRocqInstall

-- | Shake query for installing @rocq@.
data RocqQ = RocqQ
  { rocqInstallRev :: String
  -- ^ Revision of @rocq@ to use.
  , rocqOcamlCompiler :: String
  -- ^ The @ocaml@ compiler package to use, along with
  -- any associated @ocaml-option-*@ option packages.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Default @ocaml@ compiler to use for @rocq@.
defaultRocqOcamlCompiler :: String
defaultRocqOcamlCompiler = "ocaml-variants.4.14.2+options,ocaml-option-flambda"

-- | Run a command with access to a Rocq git clone.
withRocqClone
  :: String -- ^ Revision of Rocq to check out.
  -> OsPath -- ^ Store directory.
  -> (OsPath -> Action a) -- ^ Action, parameterized by the clone directory.
  -> Action a
withRocqClone rev storeDir act =
  let workDir = replaceDirectory storeDir [osp|_build/repos|]
      clone = GitCloneQ
        { gitCloneUpstream = "https://github.com/rocq-prover/rocq.git"
        , gitCloneDir = workDir
        , gitCloneRevision = rev
        }
  in withGitClone clone (act workDir)

-- | Oracle for installing a version of @rocq@.
rocqInstallOracle :: RocqQ -> OsPath -> Action ()
rocqInstallOracle RocqQ{..} storeDir = do
  withRocqClone rocqInstallRev storeDir \workDir -> do
    let rocqSwitchPkgs = intercalate "," [rocqOcamlCompiler, "dune", "ocamlfind", "zarith"]
    -- We set up the up the local switch inside of the store instead of the clone,
    -- as this ensures that we still can find our packages after we blow away the build.
    withOpamSwitch (LocalSwitch storeDir ["--packages=" ++ rocqSwitchPkgs, "--no-install"]) \opamEnv -> do
      command_ (Cwd (decodeOS workDir) : opamEnvOpts opamEnv) "./configure"
        ["-prefix", decodeOS storeDir
        ]
      makeCommand_ (Cwd (decodeOS workDir) : opamEnvOpts opamEnv) ["dunestrap"]
      -- We need to use @NJOBS@ over @-j@, see @dev/doc/build-system.dune.md@ for details.
      -- Moreover, note that -p implies --release!
      withAllCores \nCores ->
        duneCommand_ opamEnv [Cwd (decodeOS workDir), AddEnv "NJOBS" (show nCores)] ["build", "-p", "rocq-runtime,coq-core,rocq-core,coq"]
      duneCommand_ opamEnv [Cwd (decodeOS workDir)] ["install", "--prefix=" ++ decodeOS storeDir, "rocq-runtime", "coq-core", "rocq-core", "coq"]


-- | Require that a particular version of @rocq@ is installed,
-- and return the absolute path pointing to the executable.
needRocq :: String -> RocqOpts -> RocqQ -> Action (Lang RocqHeader RocqDefns)
needRocq rocqName rocqOpts q = do
  (store, _) <- askStoreOracle q
  let rocqBin = [osp|$store/bin/coqc|]
  pure $ Lang
    { langName = rocqName
    , langExt = ".v"
    , needModule = \gen size -> do
      let path = generatorOutputDir "rocq" (T.unpack (genName gen)) (show size) ".v"
      putInfo $ "# generating " <> decodeOS path
      writeBinaryHandleChanged path (genModuleVia (runRocqM rocqOpts) size gen)
      pure path
    , cleanBuildArtifacts = \dir ->
        removeFilesAfter (decodeOS dir) ["*.vo", "*.vok", "*.vos", "*.glob"]
    , benchmarkModule = \opts timeout path ->
        benchmarkCommand opts timeout rocqBin [decodeOS path]
    }

--------------------------------------------------------------------------------
-- Running Rocq


-- * Shake Rules for Rocq
--
-- $shakeRocqRules

-- | Shake rules for installing @rocq@.
rocqRules :: Rules (String -> RocqOpts -> RocqQ -> Action (Lang RocqHeader RocqDefns))
rocqRules = do
  addStoreOracle "rocq" rocqInstallOracle

  phony "clean-rocq" do
    removeFilesAfter "_build/repos" ["rocq-*"]
    removeFilesAfter "_build/store" ["rocq-*"]

  pure needRocq
