-- | Shake rules for compiling a particular version of @idris2@.
module Panbench.Shake.Lang.Idris
  ( -- * Installing Idris
    IdrisQ(..)
  , SchemeCompiler(..)
  , defaultIdrisInstallRev
  , defaultIdrisInstallScheme
  , needIdrisInstallOpts
  , IdrisBin(..)
  , needIdris
    -- * Running Idris
  , idrisCheck
  , idrisCheckBench
  , idrisDoctor
    -- * Shake Rules
  , idrisRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.Benchmark
import Panbench.Shake.Chez
import Panbench.Shake.Git
import Panbench.Shake.Make
import Panbench.Shake.Store

import System.FilePath

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

-- | Default revision of @idris@ to install.
defaultIdrisInstallRev :: String
defaultIdrisInstallRev = "v0.7.0"

-- | Default scheme flavor to use to install @idris@.
defaultIdrisInstallScheme :: SchemeCompiler
defaultIdrisInstallScheme = Chez

-- | Get the @idris@ install revision from the @$IDRIS_VERSION@ environment variable.
needIdrisInstallRev :: Action String
needIdrisInstallRev = getEnvWithDefault defaultIdrisInstallRev "IDRIS_VERSION"

-- | Get the @idris@ scheme flavor to use from the @$IDRIS_SCHEME@ environment variable.
needIdrisInstallScheme :: Action SchemeCompiler
needIdrisInstallScheme = do
  getEnv "IDRIS_SCHEME" >>= \case
    Nothing -> pure Chez
    Just "chez" -> pure Chez
    Just "racket" -> pure Racket
    Just scheme -> fail $ unlines
      [ "Unsupported scheme: " <> scheme
      , "Must be one of 'chez' or 'racket'."
      ]

-- | Docs for the @install-idris@ rule.
idrisInstallDocs :: String
idrisInstallDocs = unlines
  [ "Install a version of idris."
  , ""
  , "Can be configured with the following environment variables:"
  , "* $IDRIS_VERSION: select the revision of rocq to install "
  , "  Defaults to " <> defaultIdrisInstallRev
  , "* $IDRIS_SCHEME: select the scheme flavor used to build idris."
  , "  Must be one of 'chez' or 'racket'."
  , "  Defaults to 'chez'"
  ]

-- | Get install options for @idris@ from environment variables.
needIdrisInstallOpts :: Action IdrisQ
needIdrisInstallOpts = do
  idrisInstallRev <- needIdrisInstallRev
  idrisInstallScheme <- needIdrisInstallScheme
  pure IdrisQ {..}

-- | Run a command with access to a Idris 2 git worktree.
withIdrisWorktree
  :: String -- ^ Revision of Idris 2 to check out.
  -> FilePath -- ^ Store directory.
  -> (FilePath -> Action a) -- ^ Action, parameterized by the worktree directory.
  -> Action a
withIdrisWorktree rev storeDir act =
  let repoDir = "_build/repos/idris2"
      workDir = replaceDirectory storeDir "_build/repos"
      worktree = GitWorktreeQ
        { gitWorktreeUpstream = "https://github.com/idris-lang/IdrisBin.git"
        , gitWorktreeRepo = repoDir
        , gitWorktreeDir = workDir
        , gitWorktreeRev = rev
        }
  in withGitWorktree worktree (act workDir)

-- | Oracle for installing a version of Idris 2.
idrisInstall :: IdrisQ -> FilePath -> Action ()
idrisInstall IdrisQ{..} storeDir = do
  withIdrisWorktree idrisInstallRev storeDir \workDir -> do
    withAllCores \nCores -> do
      case idrisInstallScheme of
        Chez -> do
          chez <- needChez
          makeCommand_ [Cwd workDir, AddEnv "SCHEME" chez] ["bootstrap", "-j" ++ show nCores]
          makeCommand_ [Cwd workDir, AddEnv "PREFIX" storeDir] ["install", "-j" ++ show nCores]
          -- We need to also specify IDRIS2_PREFIX to get idris to install libraries in the correct location.
          makeCommand_ [Cwd workDir, AddEnv "PREFIX" storeDir, AddEnv "IDRIS2_PREFIX" storeDir] ["install-libs", "-j" ++ show nCores]
        Racket -> do
          makeCommand_ [Cwd workDir] ["bootstrap-racket", "-j" ++ show nCores]
          makeCommand_ [Cwd workDir, AddEnv "PREFIX" storeDir, AddEnv "IDRIS2_CG" "racket"] ["install", "-j" ++ show nCores]
            -- Same deal with IDRIS2_PREFIX as above.
          makeCommand_ [Cwd workDir, AddEnv "PREFIX" storeDir, AddEnv "IDRIS2_PREFIX" storeDir, AddEnv "IDRIS2_CG" "racket"] ["install-libs", "-j" ++ show nCores]

-- | All data required to run an idris2 binary.
data IdrisBin = IdrisBin
  { idris2Prefix :: FilePath
  -- ^ The value of @IDRIS2_PREFIX@ to use.
  --
  -- This controls where @idris2@ searches for library paths.
  -- This should be an absolute path.
  , idris2Bin :: FilePath
  -- ^ The location of the @idris2@ binary.
  -- This should be an absolute path.
  }

-- | Require that a particular version of @idris2@ is installed,
-- and return the absolute path pointing to the executable.
needIdris :: IdrisQ -> Action IdrisBin
needIdris q = do
  (store, _) <- askStoreOracle q
  pure $ IdrisBin
    { idris2Prefix = store
    , idris2Bin = store </> "bin" </> "idris2"
    }

--------------------------------------------------------------------------------
-- Running Idris

-- | Default arguments to pass to @idris2@ to check a file.
idrisCheckDefaultArgs :: FilePath -> [String]
idrisCheckDefaultArgs file = ["--check", file]

idrisCheckDefaultOpts :: IdrisBin -> [CmdOption]
idrisCheckDefaultOpts IdrisBin{..} = [AddEnv "IDRIS2_PREFIX" idris2Prefix]

-- | Check a file using @idris@.
idrisCheck :: [CmdOption] -> IdrisBin -> FilePath -> Action ()
idrisCheck opts idris@IdrisBin{..} file =
  command_ (idrisCheckDefaultOpts idris ++ opts) idris2Bin (idrisCheckDefaultArgs file)

-- | Check a file using @idris@.
idrisCheckBench :: [CmdOption] -> IdrisBin -> FilePath -> Action BenchmarkExecStats
idrisCheckBench opts idris@IdrisBin{..} file =
  benchmarkCommand (opts ++ idrisCheckDefaultOpts idris) idris2Bin (idrisCheckDefaultArgs file)

-- | Check that an @idris@ install is functioning by compiling an empty file.
idrisDoctor :: IdrisBin -> Action ()
idrisDoctor idris = do
  withTempDir \dir -> do
    let testFile = dir </> "Test.idr"
    liftIO $ writeFile testFile $ unlines
      [ "module Main"
      , ""
      , "main : IO ()"
      , "main = putStrLn \"\""
      ]
    idrisCheck [Cwd dir] idris testFile

-- | Docs for the @install-idris@ rule.
idrisDoctorDocs :: String
idrisDoctorDocs = unlines
  [ "Check that idris is installed, and can typecheck a file."
  , ""
  , "Can be configured with the following environment variables:"
  , "* $IDRIS_VERSION: select the revision of rocq to install "
  , "  Defaults to " <> defaultIdrisInstallRev
  , "* $IDRIS_SCHEME: select the scheme flavor used to build idris."
  , "  Must be one of 'chez' or 'racket'."
  , "  Defaults to 'chez'"
  ]

-- | Shake rules for installing @idris2@.
idrisRules :: Rules ()
idrisRules = do
  addStoreOracle "idris2" idrisInstall

  withTargetDocs idrisInstallDocs $ phony "install-idris" do
    opts <- needIdrisInstallOpts
    _ <- needIdris opts
    pure ()

  withTargetDocs idrisDoctorDocs $ phony "doctor-idris" do
    opts <- needIdrisInstallOpts
    idris <- needIdris opts
    idrisDoctor idris

  phony "clean-idris" do
    removeFilesAfter "_build/repos" ["idris2-*"]
    removeFilesAfter "_build/store" ["idris2-*"]
    pruneGitWorktrees "_build/repos/idris2"
