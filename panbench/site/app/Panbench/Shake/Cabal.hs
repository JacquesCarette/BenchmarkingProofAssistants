{-# LANGUAGE QuasiQuotes #-}
module Panbench.Shake.Cabal
  (
    -- * Cabal Commands
    CabalA(..)
  , cabalCommand
  , cabalCommand_
    -- * Shake Rules
  , cabalRules
  ) where

import Data.ByteString qualified as BS

import Development.Shake
import Development.Shake.Classes

import GHC.Generics
import GHC.Stack

import Panbench.Shake.Command
import Panbench.Shake.Digest
import Panbench.Shake.Path

import System.Directory.OsPath qualified as Dir

--------------------------------------------------------------------------------
-- Cabal Commands

-- | Find a version of @cabal@ on the system path.
data CabalQ = CabalQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Response of an @CabalQ@ query.
data CabalA = CabalA
  { cabalBinPath :: OsPath
  -- ^ Absolute path of the @cabal@ binary.
  , cabalVersion :: String
  -- ^ Version of cabal, as reported by @cabal --version@.
  , cabalDigest :: BS.ByteString
  -- ^ SHA 256 hash of the @cabal@ binary.
  , cabalStore :: OsPath
  -- ^ Absolute path of the @cabal@ store.
  , cabalConfig :: OsPath
  -- ^ Absolute path of the @cabal@ config.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult CabalQ = CabalA

cabalCommand :: (HasCallStack, CmdResult r) => [CmdOption] -> [String] -> Action r
cabalCommand opts args = do
  CabalA{..} <- askOracle CabalQ
  putInfo $ "# cabal " ++ unwords args
  quietly $ osCommand opts cabalBinPath (["--config-file=" ++ decodeOS cabalConfig, "--store-dir=" ++ decodeOS cabalStore] ++ args)

cabalCommand_ :: (HasCallStack) => [CmdOption] -> [String] -> Action ()
cabalCommand_ opts args = do
  CabalA{..} <- askOracle CabalQ
  putInfo $ "# cabal " ++ unwords args
  quietly $ osCommand_ opts cabalBinPath (["--config-file=" ++ decodeOS cabalConfig, "--store-dir=" ++ decodeOS cabalStore] ++ args)

findCabalOracle :: CabalQ -> Action CabalA
findCabalOracle CabalQ =
  liftIO (Dir.findExecutable [osp|cabal|]) >>= \case
    Nothing ->
      fail $ unlines
      [ "Could not find a cabal executable in the path"
      , "Perhaps it is not installed?"
      ]
    Just cabalBinPath -> do
      cwd <- liftIO Dir.getCurrentDirectory
      Stdout cabalVersion <- osCommand [] cabalBinPath ["--numeric-version"]
      cabalDigest <- fileDigest cabalBinPath
      let cabalConfig = [osp|$cwd/_build/cabal/config|]
      let cabalStore = [osp|$cwd/_build/cabal/store|]
      -- Set up a sandboxed cabal config and hackage index.
      osCommand_ [AddEnv "HOME" (decodeOS [osp|$cwd/_build/cabal|])] cabalBinPath ["--config-file=" ++ decodeOS cabalConfig , "user-config", "init", "--force"]
      osCommand_ [] cabalBinPath ["--config-file=" ++ decodeOS cabalConfig, "update"]
      pure CabalA {..}

--------------------------------------------------------------------------------
-- Shake Rules

-- | Shake rules for @cabal@.
cabalRules :: Rules ()
cabalRules = do
  _ <- addOracle findCabalOracle
  pure ()
