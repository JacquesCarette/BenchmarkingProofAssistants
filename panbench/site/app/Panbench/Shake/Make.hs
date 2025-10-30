{-# LANGUAGE QuasiQuotes #-}
-- | Make-specific rules for @shake@.
module Panbench.Shake.Make
  ( needMake
  , makeCommand_
  , makeCommand
  , makeRules
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
import System.Info qualified as Sys

-- | Shake query for finding a GNU @make@ binary.
data MakeQ = MakeQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

data MakeA = MakeA
  { makeBinPath :: OsPath
  , makeDigest :: BS.ByteString
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
  (liftIO $ Dir.findExecutable makeExecutable) >>= \case
    Just makeBinPath -> do
      makeDigest <- fileDigest makeBinPath
      pure MakeA {..}
    Nothing ->
      fail $ unlines
      [ "Could not find GNU make executable '" ++ show makeExecutable ++ "'."
      , "Perhaps it is not installed?"
      ]
  where
    makeExecutable :: OsPath
    makeExecutable
      | Sys.os `elem` ["darwin", "freebsd", "netbsd", "openbsd"] = [osp| "gmake" |]
      | otherwise = [osp| "make" |]

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
