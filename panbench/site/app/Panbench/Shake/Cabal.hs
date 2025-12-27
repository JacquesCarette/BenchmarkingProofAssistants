{-# LANGUAGE QuasiQuotes #-}
module Panbench.Shake.Cabal
  (
    -- * Cabal Commands
    CabalQ(..)
  , CabalA(..)
  , needCabal
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
newtype CabalQ = CabalQ
  { cabalHackageIndex :: String
  }
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
  , cabalIndexState :: OsString
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult CabalQ = CabalA

cabalArgs :: CabalA -> [String]
cabalArgs CabalA{..} =
  [ "--config-file=" ++ decodeOS cabalConfig
  , "--store-dir=" ++ decodeOS cabalStore
  , "--index-state=" ++ decodeOS cabalIndexState
  ]

-- | Run a @cabal@ command with a particular @cabal@ configuration.
cabalCommand :: (HasCallStack, CmdResult r) => [CmdOption] -> CabalA -> [String] -> Action r
cabalCommand opts cabal args = do
  putInfo $ "# cabal " ++ unwords args
  quietly $ osCommand opts (cabalBinPath cabal) (cabalArgs cabal ++ args)

-- | Run a @cabal@ command with a particular @cabal@ configuration, and ignore the result.
cabalCommand_ :: (HasCallStack) => [CmdOption] -> CabalA -> [String] -> Action ()
cabalCommand_ opts cabal args = do
  putInfo $ "# cabal " ++ unwords args
  quietly $ osCommand_ opts (cabalBinPath cabal) (cabalArgs cabal ++ args)

-- | Shake oracle for getting @cabal@, and setting up a sandbox.
findCabalOracle :: CabalQ -> Action CabalA
findCabalOracle cabal@CabalQ {..} =
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
      -- Set up a sandboxed cabal config and hackage index.
      let cabalDir = "cabal" <> "-" <> showHex (binaryDigest cabal)
      let cabalConfig = [osp|$cwd/_build/$cabalDir/config|]
      let cabalStore = [osp|$cwd/_build/$cabalDir/store|]
      osCommand_ [AddEnv "HOME" (decodeOS [osp|$cwd/_build/$cabalDir|])] cabalBinPath ["--config-file=" ++ decodeOS cabalConfig , "user-config", "init", "--force"]
      osCommand_ [] cabalBinPath ["--config-file=" ++ decodeOS cabalConfig, "update", "hackage.haskell.org," ++ cabalHackageIndex]
      let cabalIndexState = encodeOS cabalHackageIndex
      pure CabalA {..}

needCabal :: CabalQ -> Action CabalA
needCabal = askOracle

--------------------------------------------------------------------------------
-- Shake Rules

-- | Shake rules for @cabal@.
cabalRules :: Rules ()
cabalRules = do
  _ <- addOracle findCabalOracle
  pure ()
