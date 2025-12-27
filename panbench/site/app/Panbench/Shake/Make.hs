{-# LANGUAGE QuasiQuotes #-}
-- | Make-specific rules for @shake@.
module Panbench.Shake.Make
  ( needMake
  , makeCommand_
  , makeCommand
  , makeRules
  ) where

import Data.ByteString qualified as BS
import Data.Char qualified as C
import Data.List qualified as L

import Development.Shake
import Development.Shake.Classes

import GHC.Generics
import GHC.Stack

import Panbench.Shake.Command
import Panbench.Shake.Digest
import Panbench.Shake.File
import Panbench.Shake.Path

-- | Shake query for finding a GNU @make@ binary.
data MakeQ = MakeQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

data MakeA = MakeA
  { makeBinPath :: OsPath
  -- ^ Absolute path of the @make@ binary.
  , makeVersion :: String
  -- ^ Version of @chez@, as reported by @make --version@
  , makeDigest :: BS.ByteString
  -- ^ SHA 256 hash of the @make@ binary.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult MakeQ = MakeA

-- | Shake query for getting a system-appropriate version of GNU @make@.
needMake :: Action OsPath
needMake = makeBinPath <$> askOracle MakeQ

-- | Oracle for finding GNU @make@.
findMakeOracle :: MakeQ -> Action MakeA
findMakeOracle MakeQ = do
  findExecutableAmong [[osp|gmake|], [osp|make|]] >>= \case
    Nothing ->
      fail $ unlines
      [ "Could not find GNU make executable."
      , "Perhaps it is not installed?"
      ]
    Just makeBinPath -> do
      makeVersion <- ensureGnuMake makeBinPath
      makeDigest <- fileDigest makeBinPath
      pure MakeA {..}

-- | Ensure that the binary at the provided 'OsPath' is a copy of GNU make
-- by running @<makeBin> --version@.
--
-- Returns the version of make if it is a copy of GNU make, otherwise calls 'fail'.
ensureGnuMake :: OsPath -> Action String
ensureGnuMake makeBinPath = do
  Stdout makeVersion <- osCommand [] makeBinPath ["--version"]
  case L.stripPrefix "GNU Make" makeVersion of
    Nothing -> fail $ unlines
      [ "Make executable '" ++ decodeOS makeBinPath ++ "' is not GNU make."
      , "'" ++ decodeOS makeBinPath ++ " --version' reported:"
      , makeVersion
      ]
    Just rest -> pure $ takeWhile (not . C.isSpace) $ dropWhile C.isSpace rest

-- | Run @'makeExecutable'@, and ignore the result.
--
-- See @'command_'@ for more documentation.
makeCommand_ :: (HasCallStack) => [CmdOption] -> [String] -> Action ()
makeCommand_ opts args = do
  make <- needMake
  osCommand_ opts make args

-- | Run @'makeExecutable'@, and capture the result.
--
-- See @'command'@ for more documentation.
makeCommand :: (HasCallStack, CmdResult r) => [CmdOption] -> [String] -> Action r
makeCommand opts args = do
  make <- needMake
  osCommand opts make args

-- | Shake rules for GNU @mak@.
makeRules :: Rules ()
makeRules = do
  _ <- addOracle findMakeOracle
  pure ()
