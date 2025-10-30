{-# LANGUAGE QuasiQuotes #-}
-- | Shake helpers for working with @opam@.
module Panbench.Shake.Opam
  (
    -- * Opam Commands
    OpamA(..)
  , needOpam
  , opamCommand
  , opamCommand_
    -- ** Opam Env
  , OpamEnvQ(..)
  , OpamEnvA(..)
  , opamEnvOpts
  , askOpamEnv
    -- ** Opam Switch
  , OpamSwitch(..)
  , needOpamSwitch
  , withOpamSwitch
    -- ** Opam Install
  , askOpamVersions
  , needOpamInstall
  , needsOpamInstall
  , needsOpamInstall_
    -- * Dune
  , needDune
  , askDuneVersion
  , duneCommand
  , duneCommand_
    -- * Shake Rules
  , opamRules
  ) where

import Control.Monad

import Data.ByteString qualified as BS
import Data.Char
import Data.Functor
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe

import Development.Shake
import Development.Shake.Classes

import GHC.Generics
import GHC.Stack

import Panbench.Shake.AllCores
import Panbench.Shake.Command
import Panbench.Shake.Digest
import Panbench.Shake.Env
import Panbench.Shake.Path

import System.Directory.OsPath qualified as Dir
import System.OsPath qualified as OsPath

import Text.ParserCombinators.ReadP

--------------------------------------------------------------------------------
-- Opam Commands

-- | Find a version of @opam@ on the system path.
data OpamQ = OpamQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Response of an @OpamQ@ query.
data OpamA = OpamA
  { opamBinPath :: OsPath
  -- ^ Absolute path of the @opam@ binary
  , opamVersion :: String
  -- ^ Version of opam, as reported by @opam --version@
  , opamDigest :: BS.ByteString
  -- ^ SHA 256 hash of the @opam@ binary.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult OpamQ = OpamA

-- | Require that @opam@ is installed.
needOpam :: Action OsPath
needOpam = opamBinPath <$> askOracle OpamQ

-- | Run an @opam@ command.
opamCommand :: (HasCallStack, CmdResult r) => [CmdOption] -> [String] -> Action r
opamCommand opts args = do
  opam <- needOpam
  putInfo $ "# opam " ++ unwords args
  quietly $ osCommand opts opam (args ++ ["--yes"])

-- | Run an @opam@ command, and ignore the results.
opamCommand_ :: (HasCallStack) => [CmdOption] -> [String] -> Action ()
opamCommand_ opts args = do
  opam <- needOpam
  putInfo $ "# opam " ++ unwords args
  quietly $ osCommand_ opts opam (args ++ ["--yes"])

-- | Shake oracle for finding the @opam@ binary.
findOpamCommandOracle :: OpamQ -> Action OpamA
findOpamCommandOracle OpamQ =
  (liftIO $ Dir.findExecutable [osp|opam|]) >>= \case
    Nothing ->
      fail $ unlines $
        [ "Could not find an opam executable in the path"
        , "Perhaps it is not installed?"
        ]
    Just opamBinPath -> do
      Stdout opamVersion <- osCommand [] opamBinPath ["--version"]
      opamDigest <- fileDigest opamBinPath
      pure OpamA {..}

--------------------------------------------------------------------------------
-- Opam Env

-- | Shake query for getting the @opam@ environment of a switch.
newtype OpamEnvQ = OpamEnvQ OpamSwitch
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

data OpamEnvA = OpamEnvA
  { opamEnvPathPrefix :: [OsString]
  , opamEnvPathSuffix :: [OsString]
  , opamEnvVars :: [(OsString, OsString)]
  , opamEnvSwitch :: OpamSwitch
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult OpamEnvQ = OpamEnvA

-- | Get the environment associated to a switch.
askOpamEnv :: OpamSwitch -> Action OpamEnvA
askOpamEnv = askOracle . OpamEnvQ

-- | Parse the output of @opam env --sexp@.
parseOpamEnv :: OpamSwitch -> String -> Action OpamEnvA
parseOpamEnv opamEnvSwitch envStr = do
  case parseSExpr envStr of
    Just envSexpr -> do
      opamEnvVars <- parseEnvVars envStr envSexpr
      path <- askPath
      let opamEnvPath = OsPath.splitSearchPath $ fromMaybe mempty $ lookup [osstr|PATH|] opamEnvVars
      let opamEnvPathPrefix = diffPathPrefix opamEnvPath path
      let opamEnvPathSuffix = diffPathSuffix opamEnvPath path
      pure OpamEnvA {..}
    Nothing ->
      fail $ unlines $
      [ "opam env --sexp returned a s-expression we could not parse:"
      , envStr
      ]
  where
    parseFail :: String -> Action a
    parseFail envStr =
      fail $ unlines
      [ "Could not parse output of 'opam env --sexp' as a list of environment variables."
      , envStr
      ]

    parseEnvVar :: String -> SExpr -> Action (OsString, OsString)
    parseEnvVar _ (List [String var, String val]) = pure (encodeOS var, encodeOS val)
    parseEnvVar envStr _ = parseFail envStr

    parseEnvVars :: String -> SExpr -> Action [(OsString, OsString)]
    parseEnvVars envStr (List xs) = traverse (parseEnvVar envStr) xs
    parseEnvVars envStr _ = parseFail envStr

-- | Print an opam environment.
putVerboseOpamEnv :: OpamEnvA -> Action ()
putVerboseOpamEnv OpamEnvA{..} =
  putVerbose $
  unlines
  [ "# opam environment for " <>  opamSwitchName opamEnvSwitch
  , "  path prefix: " <> intercalate ":" (decodeOS <$> opamEnvPathPrefix)
  , "  path suffix: " <> intercalate ":" (decodeOS <$> opamEnvPathSuffix)
  , "  env vars:"
  , unlines $ fmap (\(var, val) -> "    " <> decodeOS var <> "=" <> decodeOS val) opamEnvVars
  ]

-- | Shake oracle for @opam env@ queries.
opamEnvOracle :: OpamEnvQ -> Action OpamEnvA
opamEnvOracle (OpamEnvQ switch) = do
  Stdout envOut <- opamCommand [AddEnv "OPAMSWITCH" (opamSwitchName switch)] ["env", "--sexp"]
  parseOpamEnv switch envOut

--------------------------------------------------------------------------------
-- Opam Switches

-- | An opam switch.
data OpamSwitch
  = LocalSwitch OsPath
  -- ^ A local opam switch, along with its path.
  | NamedSwitch String
  -- ^ A named opam switch.
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Get the name of an @opam@ switch.
--
-- If the switch is a local switch, the name is the absolute path of the switch.
opamSwitchName :: OpamSwitch -> String
opamSwitchName (LocalSwitch dir) = decodeOS dir
opamSwitchName (NamedSwitch nm) = nm

-- | Require that an opam switch be created if it does not already exist.
needOpamSwitch
  :: OpamSwitch
  -- ^ The @opam@ switch.
  -> [String]
  -- ^ Options used to create the switch if it doesn't already exist.
  -> Action ()
needOpamSwitch (LocalSwitch switchDir) args = do
  -- [FIXME: Reed M, 21/07/2025] This doesn't trigger a rebuild when the options change.
  -- This can be fixed by using an oracle.
  absSwitchDir <- liftIO $ Dir.makeAbsolute switchDir
  Stdout switches <- opamCommand [] ["switch", "list", "--short"]
  when (decodeOS absSwitchDir `notElem` (lines switches)) do
    withAllCores \nCores ->
      opamCommand_ [] (["switch", "create", decodeOS switchDir, "--jobs=" <> show nCores] ++ args)
needOpamSwitch (NamedSwitch name) args = do
  Stdout switches <- opamCommand [] ["switch", "list", "--short"]
  when (name `notElem` (lines switches)) do
    withAllCores \nCores ->
      opamCommand_ [] (["switch", "create", name, "--jobs=" <> show nCores] ++ args)

-- | Run an @Action@ with access to the environment of an @opam@ switch.
--
-- If a local switch does not exist in the current directory, create it.
withOpamSwitch
  :: OpamSwitch
  -- ^ The @opam@ switch.
  -> [String]
  -- ^ Options used to create the switch if it doesn't already exist.
  -> (OpamEnvA -> Action a)
  -- ^ Action to run with the switch environment variables.
  -> Action a
withOpamSwitch switchDir args act = do
  needOpamSwitch switchDir args
  opamEnv <- askOpamEnv switchDir
  putVerboseOpamEnv opamEnv
  act opamEnv

-- | Build @CmdOption@s from an opam environment.
opamEnvOpts :: OpamEnvA -> [CmdOption]
opamEnvOpts OpamEnvA{..} = pathOpt <> envOpts <> switchOpt
  where
    pathOpt = [AddPath (fmap decodeOS opamEnvPathPrefix) (fmap decodeOS opamEnvPathSuffix)]
    envOpts = opamEnvVars <&> \(var, val) -> AddEnv (decodeOS var) (decodeOS val)
    -- Ensures that any further opam commands use this switch, even
    -- if we are outside of a local switches directory.
    switchOpt = [AddEnv "OPAMSWITCH" (opamSwitchName opamEnvSwitch)]


--------------------------------------------------------------------------------
-- Opam Install

-- | Shake query for getting installed versions of packages in an @opam@ switch.
data OpamVersionQ = OpamVersionQ
  { opamVersionEnv :: OpamEnvA
  , opamVersionPackages :: [String]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult OpamVersionQ = (Map String String, [String])

-- | Get a list of installed package versions in a @opam@ switch, along with a list
-- of packages that do not have versions.
--
-- Note that this will cache its results on a per-run basis.
-- For a version that does not cache, see @'askOpamVersionsNoCache'@
askOpamVersions :: OpamEnvA -> [String] -> Action (Map String String, [String])
askOpamVersions opamVersionEnv opamVersionPackages = askOracle OpamVersionQ {..}

-- | Get a list of installed package versions in a @opam@ switch, along with a list
-- of packages that do not have versions without caching the results.
askOpamVersionsNoCache :: OpamEnvA -> [String] -> Action (Map String String, [String])
askOpamVersionsNoCache opamVersionEnv opamVersionPackages =
  opamVersionOracle OpamVersionQ {..}

-- | Shake oracle for getting package versions.
opamVersionOracle :: OpamVersionQ -> Action (Map String String, [String])
opamVersionOracle OpamVersionQ{..} = do
  Stdout pkgVersions <- opamCommand (opamEnvOpts opamVersionEnv) (["info", "-f", "installed-version"] ++ opamVersionPackages)
  let (hasNoVersion, hasVersion) = partition (\(_, version) -> version == "--") $ zip opamVersionPackages (lines pkgVersions)
  pure (Map.fromList hasVersion, fmap fst hasNoVersion)

-- | Shake query for installing @opam@ packages in a switch.
data OpamInstallQ = OpamInstallQ
  { opamInstallEnv :: OpamEnvA
  , opamInstallPackages :: [String]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult OpamInstallQ = Map String String

-- | Ask that a single @opam@ package be installed.
needOpamInstall :: OpamEnvA -> String -> Action String
needOpamInstall opamInstallEnv opamInstallPackage = do
  versions <- needsOpamInstall opamInstallEnv [opamInstallPackage]
  pure $ versions Map.! opamInstallPackage

-- | Ask that a collection of packages be installed.
needsOpamInstall :: OpamEnvA -> [String] -> Action (Map String String)
needsOpamInstall opamInstallEnv opamInstallPackages = askOracle OpamInstallQ {..}

-- | Ask that a collection of packages be installed, and discard the
-- version information.
needsOpamInstall_ :: OpamEnvA -> [String] -> Action ()
needsOpamInstall_ opamInstallEnv opamInstallPackages =
  void $ needsOpamInstall opamInstallEnv opamInstallPackages

-- | Oracle for installing @opam@ packages.
opamInstallOracle :: OpamInstallQ -> Action (Map String String)
opamInstallOracle OpamInstallQ{..} = do
  (hasVersion, hasNoVersion) <- askOpamVersionsNoCache opamInstallEnv opamInstallPackages
  -- Install any missing deps.
  -- We need to make sure to skip the install/version check when all packages are installed,
  -- as @opam@ will yell about missing arguments when provided with an empty package list.
  justInstalledVersions <-
    case hasNoVersion of
      [] ->
        pure Map.empty
      _ -> do
        withAllCores \nCores ->
          opamCommand_ (opamEnvOpts opamInstallEnv) (["install"] ++ hasNoVersion ++ ["--jobs=" ++ show nCores])
        (justInstalledVersions, stillNoVersion) <- askOpamVersionsNoCache opamInstallEnv hasNoVersion
        when (not $ null stillNoVersion) do
          fail $ unlines $ ["The following packages were installed, yet still do not have a version:"] ++ stillNoVersion
        pure justInstalledVersions
  pure (Map.union hasVersion justInstalledVersions)

--------------------------------------------------------------------------------
-- Dune

-- | Shake query for installing @dune@.
newtype DuneQ = DuneQ OpamEnvA
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Answer to a @'DuneQ'@ query.
data DuneA = DuneA
  { duneBinPath :: FilePath
  -- ^ Absolute path of the @dune@ binary.
  , duneVersion :: String
  -- ^ Version of @dune@ that we have installed.
  , duneDigest :: BS.ByteString
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult DuneQ = DuneA

-- | Get the absolute path of @dune@ inside of an opam switch.
--
-- This will install @dune@ if it is not already present.
needDune :: OpamEnvA -> Action FilePath
needDune opamEnv = duneBinPath <$> askOracle (DuneQ opamEnv)

-- | Get the version of @dune@ installed in an @opam@ switch.
askDuneVersion :: OpamEnvA -> Action (Maybe String)
askDuneVersion opamEnv = do
  Stdout duneVersion <-
    opamCommand (opamEnvOpts opamEnv)
      ["info", "-f", "installed-version", "dune"]
  case duneVersion of
    "--" -> pure Nothing
    _ -> pure (Just duneVersion)

-- | Oracle for finding the @dune@ binary.
findDuneOracle :: DuneQ -> Action DuneA
findDuneOracle (DuneQ opamEnv) = do
  duneVersion <- needOpamInstall opamEnv "dune"
  Stdout duneOut <- opamCommand (opamEnvOpts opamEnv) ["exec", "which", "dune"]
  let duneBinPath = takeWhile (not . isSpace) duneOut
  duneDigest <- fileDigest (encodeOS duneBinPath)
  pure DuneA {..}

-- | Run a @dune@ command, and ignore the results.
duneCommand :: (HasCallStack, CmdResult r) => OpamEnvA -> [CmdOption] -> [String] -> Action r
duneCommand opamEnv opts args = do
  dune <- needDune opamEnv
  putInfo $ "# dune " <> unwords args
  quietly $ command (opamEnvOpts opamEnv ++ opts) dune args

-- | Run a @dune@ command, and ignore the results.
duneCommand_ :: (HasCallStack) => OpamEnvA -> [CmdOption] -> [String] -> Action ()
duneCommand_ opamEnv opts args = do
  dune <- needDune opamEnv
  putInfo $ "# dune " <> unwords args
  quietly $ command_ (opamEnvOpts opamEnv ++ opts) dune args

--------------------------------------------------------------------------------
-- S-Expressions
--
-- S-expressions are the lingua franca of ocaml build tools like
-- @opam@ and @dune@, so we need to be able to parse them.

data SExpr
  = String String
  | List [SExpr]
  deriving (Show, Eq, Ord)

-- | Parse an s-expression.
parseSExpr :: String -> Maybe SExpr
parseSExpr xs = do
  (a, rest) <- listToMaybe $ readP_to_S (parser <* skipSpaces) xs
  guard (rest == "")
  pure a
  where
    -- [TODO: Reed M, 21/07/2025]
    -- This doesn't handle escaped strings properly.
    -- However, I don't think this will be an issue in practice: we only
    -- use this for parsing opam/dune results, and those shouldn't contain
    -- escaped strings.
    parser :: ReadP SExpr
    parser =
      choice
      [ List <$> between (skipSpaces *> char '(') (skipSpaces *> char ')') (many parser)
      , String <$> between (skipSpaces *> char '\"') (skipSpaces *> char '\"') (munch ('\"' /=))
      ]

--------------------------------------------------------------------------------
-- Shake Rules

-- | Shake rules for @opam@.
opamRules :: Rules ()
opamRules = do
  _ <- addOracleCache findOpamCommandOracle
  _ <- addOracleCache findDuneOracle
  _ <- addOracleCache opamEnvOracle
  _ <- addOracleCache opamVersionOracle
  _ <- addOracleCache opamInstallOracle
  pure ()
