-- | Shake oracles that use content-addressable storage.
module Panbench.Shake.Store
  ( -- $shakeStore
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

import System.Directory qualified as Dir
import System.FilePath

-- * Content-addressed oracles
--
-- $shakeStore

newtype StoreOracleQ q = StoreOracleQ q
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult (StoreOracleQ q) = (FilePath, BS.ByteString)

addStoreOracle
  :: forall q. (ShakeValue q, HasCallStack)
  => String
  -- ^ Name of the store entry.
  -> (q -> FilePath -> Action ())
  -- ^ Action to populate the store.
  -> Rules ()
addStoreOracle name act = do
  addBuiltinRule noLint identify run
  where
    identify :: StoreOracleQ q -> (FilePath, BS.ByteString) -> Maybe BS.ByteString
    identify _ (_, hash) = Just hash

    run :: StoreOracleQ q -> Maybe BS.ByteString -> RunMode -> Action (RunResult (FilePath, BS.ByteString))
    run (StoreOracleQ q) oldHash mode = do
      cwd <- liftIO $ Dir.getCurrentDirectory
      let storeKey = name <> "-" <> showHex (binaryDigest q)
      let storePath = cwd </> "_build" </> "store" </> storeKey
      (liftIO $ Dir.doesDirectoryExist storePath) >>= \case
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
  -> Action (FilePath, BS.ByteString)
askStoreOracle = apply1 . StoreOracleQ
