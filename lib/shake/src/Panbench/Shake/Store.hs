{-# LANGUAGE QuasiQuotes #-}
-- | Shake oracles that use content-addressable storage.
module Panbench.Shake.Store
  ( -- * Content-addressed oracles
    StoreOracleA(..)
  , addStoreOracle
  , askStoreOracle
  ) where

import Control.Monad.IO.Class

import Data.Binary
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule

import GHC.Generics
import GHC.Stack

import Panbench.Shake.Digest
import Panbench.Shake.File
import Panbench.Shake.Path

import System.Directory.OsPath qualified as Dir

--------------------------------------------------------------------------------
-- Content-addressed oracles

newtype StoreOracleQ q = StoreOracleQ q
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

data StoreOracleA = StoreOracleA
  { storeOraclePath :: !OsPath
  , storeOracleModTime :: !Word64
  , storeOracleHash :: !(Maybe BS.ByteString)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult (StoreOracleQ q) = StoreOracleA

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
addStoreOracle name act = versioned 2 do
  shakeOpts <- getShakeOptionsRules
  addBuiltinRule noLint (identify (shakeChange shakeOpts)) (run (shakeChange shakeOpts))
  where
    -- Used for shared shake caches.
    identify :: Change -> StoreOracleQ q -> StoreOracleA -> Maybe BS.ByteString
    identify ChangeModtime _ _ = Nothing -- If we are using modification times, don't use shared caches.
    identify _ _ (StoreOracleA _ _ hash) = hash

    changeModtime :: Maybe StoreOracleA -> RunMode -> OsPath -> Action (RunResult StoreOracleA)
    changeModtime oldAns mode storeOraclePath = do
      !newModTime <- getDirectoryModificationTime storeOraclePath
      case (oldAns, mode) of
        (Just oldAns@StoreOracleA { storeOracleModTime = oldModTime }, RunDependenciesSame)
          | oldModTime == newModTime ->
          pure $ RunResult ChangedNothing (LBS.toStrict $ encode oldAns) oldAns
        _ -> do
          let newAns = StoreOracleA storeOraclePath newModTime Nothing
          pure $ RunResult ChangedRecomputeDiff (LBS.toStrict $ encode newAns) newAns

    changeDigest :: Maybe StoreOracleA -> RunMode -> OsPath -> Action (RunResult StoreOracleA)
    changeDigest oldAns mode storeOraclePath = do
      -- Potential race condition here, but should be ok?
      !newModTime <- getDirectoryModificationTime storeOraclePath
      !newHash <- directoryDigest storeOraclePath
      case (oldAns, mode) of
        (Just oldAns@StoreOracleA { storeOracleHash = Just oldHash }, RunDependenciesSame)
          | oldHash == newHash ->
          pure $ RunResult ChangedNothing (LBS.toStrict $ encode oldAns) oldAns
        _ -> do
          let newAns = StoreOracleA storeOraclePath newModTime (Just newHash)
          pure $ RunResult ChangedRecomputeDiff (LBS.toStrict $ encode newAns) newAns

    changeModtimeOrDigest :: Maybe StoreOracleA -> RunMode -> OsPath -> Action (RunResult StoreOracleA)
    changeModtimeOrDigest oldAns mode storeOraclePath = do
      !newModTime <- getDirectoryModificationTime storeOraclePath
      !newHash <- directoryDigest storeOraclePath
      case (oldAns, mode) of
        (Just oldAns@StoreOracleA { storeOracleModTime = oldModTime, storeOracleHash = Just oldHash }, RunDependenciesSame)
          | oldModTime == newModTime && oldHash == newHash ->
          pure $ RunResult ChangedNothing (LBS.toStrict $ encode oldAns) oldAns
        _ -> do
          let newAns = StoreOracleA storeOraclePath newModTime (Just newHash)
          pure $ RunResult ChangedRecomputeDiff (LBS.toStrict $ encode newAns) newAns

    changeModtimeAndDigest :: Maybe StoreOracleA -> RunMode -> OsPath -> Action (RunResult StoreOracleA)
    changeModtimeAndDigest oldAns mode storeOraclePath = do
      !newModTime <- getDirectoryModificationTime storeOraclePath
      -- We could try and delay hashing here a bit, but it's kind pointless, as we'd need
      -- to insert hashes into the shake database anyways.
      !newHash <- directoryDigest storeOraclePath
      case (oldAns, mode) of
        (Just oldAns@StoreOracleA { storeOracleModTime = oldModTime, storeOracleHash = Just oldHash }, RunDependenciesSame)
          | oldModTime == newModTime || oldHash == newHash ->
          pure $ RunResult ChangedNothing (LBS.toStrict $ encode oldAns) oldAns
        _ -> do
          let newAns = StoreOracleA storeOraclePath newModTime (Just newHash)
          pure $ RunResult ChangedRecomputeDiff (LBS.toStrict $ encode newAns) newAns

    storeValue :: Change -> Maybe StoreOracleA -> RunMode -> OsPath -> Action (RunResult StoreOracleA)
    storeValue ChangeModtime = changeModtime
    storeValue ChangeDigest = changeDigest
    storeValue ChangeModtimeOrDigest = changeModtimeOrDigest
    storeValue ChangeModtimeAndDigest = changeModtimeAndDigest
    storeValue ChangeModtimeAndDigestInput = changeModtimeAndDigest

    run :: Change -> StoreOracleQ q -> Maybe BS.ByteString -> RunMode -> Action (RunResult StoreOracleA)
    run change (StoreOracleQ q) oldVal mode = do
      cwd <- liftIO Dir.getCurrentDirectory
      let oldAns = decode . LBS.fromStrict <$> oldVal
      let storeKey = name <> "-" <> showHex (binaryDigest q)
      let storeOraclePath = [osp|$cwd/_build/store/$storeKey|]
      liftIO (Dir.doesDirectoryExist storeOraclePath) >>= \case
        True -> do
          storeValue change oldAns mode storeOraclePath
        False -> do
          act q storeOraclePath
          -- We don't return ChangeRecomputeDiff here, as we could have deleted and rebuilt the
          -- store directory and gotten the same hash back, which could let us stop invalidating the build.
          storeValue change oldAns mode storeOraclePath

-- | Query the store.
askStoreOracle
  :: forall q. (ShakeValue q, HasCallStack)
  => q
  -> Action StoreOracleA
askStoreOracle = apply1 . StoreOracleQ
