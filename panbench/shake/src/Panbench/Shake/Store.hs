{-# LANGUAGE QuasiQuotes #-}
-- | Shake oracles that use content-addressable storage.
module Panbench.Shake.Store
  ( -- * Content-addressed oracles
    addStoreOracle
  , askStoreOracle
  ) where

import Control.Monad.IO.Class

import Data.Binary
import Data.ByteString qualified as BS

import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule

import GHC.Generics
import GHC.Stack

import Panbench.Shake.Digest
import Panbench.Shake.Path

import System.Directory.OsPath qualified as Dir

--------------------------------------------------------------------------------
-- Content-addressed oracles

newtype StoreOracleQ q = StoreOracleQ q
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult (StoreOracleQ q) = (OsPath, BS.ByteString)

-- | Add an oracle that stores its results in a content-addressed store.
--
-- The results of the oracle are content addressed via the hash of the
-- inputs. However, we also store the hash of the outputs inside the shake
-- database, which is used to invalidate builds. This allows us to get both
-- the benefits of input-addressing and output-addressing: input-addressing
-- for disk storage means that we can compute paths before running a build, and
-- output-addressing for the shake database means that we can avoid rebuilds
-- if an input change does not cause the final hash to change.
addStoreOracle
  :: forall q. (ShakeValue q, HasCallStack)
  => String
  -- ^ Name of the store entry.
  -> (q -> OsPath -> Action ())
  -- ^ Action to populate the store.
  -> Rules ()
addStoreOracle name act = do
  addBuiltinRule noLint identify run
  where
    identify :: StoreOracleQ q -> (OsPath, BS.ByteString) -> Maybe BS.ByteString
    identify _ (_, hash) = Just hash

    run :: StoreOracleQ q -> Maybe BS.ByteString -> RunMode -> Action (RunResult (OsPath, BS.ByteString))
    run (StoreOracleQ q) oldHash mode = do
      cwd <- liftIO Dir.getCurrentDirectory
      let storeKey = name <> "-" <> showHex (binaryDigest q)
      let storePath = [osp|$cwd/_build/store/$storeKey|]
      liftIO (Dir.doesDirectoryExist storePath) >>= \case
        True -> do
          !newHash <- directoryDigest storePath
          case (oldHash, mode) of
            (Just oldHash, RunDependenciesSame) | oldHash == newHash ->
              pure $ RunResult ChangedNothing oldHash (storePath, oldHash)
            _ ->
              pure $ RunResult ChangedRecomputeDiff newHash (storePath, newHash)
        False -> do
          act q storePath
          !newHash <- directoryDigest storePath
          case (oldHash, mode) of
            (Just oldHash, RunDependenciesSame) | oldHash == newHash ->
              pure $ RunResult ChangedRecomputeSame oldHash (storePath, oldHash)
            _ ->
              pure $ RunResult ChangedRecomputeDiff newHash (storePath, newHash)

-- | Query the store.
askStoreOracle
  :: forall q. (ShakeValue q, HasCallStack)
  => q
  -> Action (OsPath, BS.ByteString)
askStoreOracle = apply1 . StoreOracleQ
