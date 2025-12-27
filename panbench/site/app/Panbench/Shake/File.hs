{-# OPTIONS_GHC -Wno-orphans #-}
-- | Shake utilities for @panbench-site@.
module Panbench.Shake.File
  ( -- $shakefileutil
    createDirectoryRecursive
  , copyDirectoryRecursive
  , writeBinaryFileChanged
  , writeTextFileChanged
  , writeBinaryHandleChanged
  , findExecutableAmong
  , removePathForcibly
    -- $shakeFileOracle
  , addFileCacheOracle
  , askFileCacheOracle
  , asksFileCacheOracle
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Binary
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock.POSIX
import Data.Time.Clock

import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule

import Foreign.Storable

import GHC.ForeignPtr
import GHC.Generics
import GHC.Stack

import System.Directory.OsPath qualified as Dir
import System.File.OsPath qualified as File
import System.OsPath
import System.IO
  ( Handle, IOMode(..), SeekMode(..)
  , hSetFileSize, hFlush, hSeek
  )
import System.IO.Error
import System.IO.Unsafe (unsafeDupablePerformIO)

-- * Shake File Utilities
--
-- $shakefileutil
--
-- The following code was adapted from @writeFileChanged@ in @General.Extras@ in @shake-0.19.8@.
-- We need to be able to write binary files, which shake does not support OOTB.

-- | Recursively create a directory if it does not exist.
createDirectoryRecursive :: (MonadIO m) => OsPath -> m ()
createDirectoryRecursive dir = liftIO do
    x <- try @IOException $ Dir.doesDirectoryExist dir
    when (x /= Right True) $ Dir.createDirectoryIfMissing True dir

-- | @copyDirectoryRecursive srcDir tgtDir@ will recursively copy all files in @srcDir@
-- to @tgtDir@.
copyDirectoryRecursive :: (HasCallStack, MonadIO m) => OsPath -> OsPath -> m ()
copyDirectoryRecursive srcDir tgtDir = liftIO do
  srcPaths <- Dir.listDirectory srcDir
  for_ srcPaths \srcPath ->
    Dir.doesDirectoryExist (srcDir </> srcPath) >>= \case
      False -> do
        createDirectoryRecursive tgtDir
        Dir.copyFile (srcDir </> srcPath) (tgtDir </> srcPath)
      True ->
        copyDirectoryRecursive (srcDir </> srcPath) (tgtDir </> srcPath)

-- | Remove a file.
removeFile_ :: OsPath -> IO ()
removeFile_ x =
    Dir.removeFile x `catch` \e ->
        when (isPermissionError e) $ handle @IOException (\_ -> pure ()) $ do
            perms <- Dir.getPermissions x
            Dir.setPermissions x perms{Dir.readable = True, Dir.searchable = True, Dir.writable = True}
            Dir.removeFile x

-- | Remove a directory and all of its contents.
removePathForcibly :: (MonadIO m) => OsPath -> m ()
removePathForcibly path = liftIO $ Dir.removePathForcibly path

-- | Return the first executable found amongst a list of names.
findExecutableAmong :: (MonadIO m) => [OsPath] -> m (Maybe OsPath)
findExecutableAmong nms = do
  liftIO $ coerce $ foldMap (\nm -> coerce @(IO (Maybe OsPath)) @(Ap IO (First OsPath)) $ Dir.findExecutable nm) nms

-- | Write to a file if its contents would change, using
-- the provided reading/writing functions.
--
-- This function is not intended to be called directly: see
-- @'writeBinaryFileChanged'@ and related functions.
writeFileChangedWith
  :: (MonadIO m, Eq a)
  => (Handle -> IO a)
  -> (OsPath -> a -> IO ())
  -> OsPath
  -> a
  -> m ()
writeFileChangedWith readH writeF name x = liftIO $ do
  createDirectoryRecursive $ takeDirectory name
  exists <- Dir.doesFileExist name
  if not exists then
    writeF name x
  else do
    changed <- File.withFile name ReadMode $ \h -> do
        src <- readH h
        pure $! src /= x
    when changed $ do
        removeFile_ name -- symlink safety
        writeF name x
{-# INLINE writeFileChangedWith #-}

-- | Write to a file if its contents would change, using
-- the provided writing function.
--
-- This function first writes its result to a temporary file, and then
-- compares with an existing file and renames if needed.
-- This can often be more efficient than 'writeFileChangedWith',
-- as we can avoid having to materialize to a string entirely if the file
-- does not exist.
--
-- The handle will be opened in binary mode, which means that the text
-- encoding is set to 'char8' and all newline conversion is disabled.
-- All text written to this handle should be in the form of pre-encoded 'ByteString's.
-- Failure to do so will result in incorrectly encoded text.
writeBinaryHandleChanged
  :: (MonadIO m)
  => OsPath
  -> (Handle -> IO ())
  -> m ()
writeBinaryHandleChanged name writeF = liftIO $ do
  createDirectoryRecursive $ takeDirectory name
  exists <- Dir.doesFileExist name
  if not exists then
    File.withFile name WriteMode writeF
  else do
    tmpdir <- liftIO $ Dir.getTemporaryDirectory
    let tmpfile = tmpdir </> takeFileName name
    changed <- File.withBinaryFile tmpfile ReadWriteMode \tmpHdl -> do
      liftIO $ hSetFileSize tmpHdl 0
      writeF tmpHdl
      liftIO $ hFlush tmpHdl
      liftIO $ hSeek tmpHdl AbsoluteSeek 0
      File.withFile name ReadMode \oldHdl -> do
        old <- LBS.hGetContents oldHdl
        new <- LBS.hGetContents tmpHdl
        pure $! old /= new
    when changed do
      removeFile_ name -- symlink safety
      Dir.renameFile tmpfile name

-- | Write the contents of a lazy @ByteString@ to a file if the contents of
-- the file would change.
writeBinaryFileChanged :: (MonadIO m) => OsPath -> LBS.ByteString -> m ()
writeBinaryFileChanged = writeFileChangedWith LBS.hGetContents File.writeFile

-- | Write the contents of a strict @Text@ to a file if the contents of
-- the file would change.
writeTextFileChanged :: (MonadIO m) => OsPath -> T.Text -> m ()
writeTextFileChanged = writeFileChangedWith T.hGetContents (\path txt -> File.writeFile path (LBS.fromStrict $ T.encodeUtf8 txt))

-- * File-caching oracles
--
-- $shakeFileOracle
--
-- Oracles that write their results to a file.

newtype FileCacheOracleQ q = FileCacheOracleQ q
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

newtype FileCacheOracleA a = FileCacheOracleA (OsPath, a)
  deriving newtype (Eq, Ord, Show, NFData)

type instance RuleResult (FileCacheOracleQ q) = FileCacheOracleA (RuleResult q)

-- | Add a @shake@ oracle that caches its results to a file.
addFileCacheOracle
  :: forall q a. (RuleResult q ~ a, ShakeValue q, Typeable a, Show a, NFData a, Hashable a, HasCallStack)
  => (q -> OsPath)
  -- ^ The filepath to write our cached value.
  -> (LBS.ByteString -> Action a)
  -- ^ Read our answer back off of a bytestring.
  -> (q -> Action (a, LBS.ByteString))
  -- ^ Action to run to create a value, along with an encoded version to write to the cache file.
  -> Rules ()
addFileCacheOracle getPath decodeAnswer act = do
  addBuiltinRule noLint identify run
    where
      identify :: FileCacheOracleQ q -> FileCacheOracleA a -> Maybe BS.ByteString
      identify _ (FileCacheOracleA (_, ans)) = Just $ packStorable $ hash ans

      run :: FileCacheOracleQ q -> Maybe BS.ByteString -> RunMode -> Action (RunResult (FileCacheOracleA a))
      run (FileCacheOracleQ q) oldTime mode = do
        let path = getPath q
        newTime <- getModificationTime path
        case (newTime, oldTime, mode) of
          (Just newTime, Just oldTime, RunDependenciesSame) | newTime == oldTime -> do
            bytes <- liftIO $ File.readFile path
            ans <- decodeAnswer bytes
            pure $ RunResult ChangedNothing newTime (FileCacheOracleA (path, ans))
          _ -> do
            (ans, bytes) <- act q
            createDirectoryRecursive (takeDirectory path)
            liftIO $ File.writeFile path bytes
            -- [HACK: Race condition for file modification times]
            -- Ideally, we'd get the modification time atomically during creation.
            -- Unfortunately, there is little support for this on most systems, so
            -- we are just going to have to live with this race condition for now.
            writeTime <- fromMaybe (error "The file disappeared just after we wrote it.") <$> getModificationTime path
            pure $ RunResult ChangedRecomputeDiff writeTime (FileCacheOracleA (path, ans))

-- | Query a file cache oracle.
askFileCacheOracle
  :: (RuleResult q ~ a, ShakeValue q, Typeable a)
  => q
  -> Action (OsPath, a)
askFileCacheOracle =
  fmap coerce . apply1 . FileCacheOracleQ

-- | Perform multiple queries to a file cache oracle in parallel.
asksFileCacheOracle
  :: (RuleResult q ~ a, ShakeValue q, Typeable a)
  => [q]
  -> Action [(OsPath, a)]
asksFileCacheOracle =
  fmap coerce . apply . fmap FileCacheOracleQ

-- | Get the modification time of a file as a POSIX timestamp measured in
-- nanoseconds, and encode it as a strict bytestring.
-- If the file does not exist, return @'Nothing'@.
--
-- This function is intended to be used in concert with @'addBuiltinRule'@.
getModificationTime :: (MonadIO m) => OsPath -> m (Maybe BS.ByteString)
getModificationTime path = liftIO do
  (Just . packStorable . utcToNano <$> Dir.getModificationTime path) `catch` \e ->
    if isDoesNotExistError e then
      pure Nothing
    else
      throwIO e
  where
    -- [HACK: Potential inefficiency from @time@]
    -- As usual, @time@ is an extremely annoying library.
    -- Unfortunately, @directory@ reports modification times
    -- back using @UTCTime@, so avoiding @time@ would be even
    -- more of a yak-shave.
    --
    -- That being said, I'm going to complain anways. @time@
    -- stores its time as arbitrary precision integers with picosecond
    -- accuracy. This is absolutely ridiculous for things like file modification
    -- times. I've seen production code where this is a bottleneck, so this poor design
    -- choice isn't just hypothetical! For our use case, we should probably be dominated by
    -- IO, but it's something to keep an eye on.
    utcToNano :: UTCTime -> Word64
    utcToNano =
      floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

-- | Pack a @'Storable'@ type into a strict bytestring.
packStorable :: forall a. (Storable a) => a -> BS.ByteString
packStorable a =
  -- This call to @unsafeDupablePerformIO@ is safe for the following reasons:
  -- 1. We don't have our hands on the underlying pointer from outside @packStorable@,
  --    so we can't break referential transparency by modifying the pointer.
  -- 2. We are just allocating some buffers here, so it's fine if this
  --    gets run multiple times on different cores.
  unsafeDupablePerformIO do
    buffer <- mallocForeignPtr @a
    withForeignPtr buffer \ptr -> poke ptr a
    BS.mkDeferredByteString (castForeignPtr buffer) (sizeOf a)
