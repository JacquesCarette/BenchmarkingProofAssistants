-- | Utilities for computing SHA-256 digests.
module Panbench.Shake.Digest
  (
  -- $shakeDigest
    fileDigest
  , directoryDigest
  , binaryDigest
  -- $shakeDigestDisplay
  , showHex
  ) where

import Control.Monad
import Control.Monad.IO.Class

import Crypto.Hash.SHA256 qualified as SHA256

import Data.Binary
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Panbench.Shake.File

-- * SHA-256 digests
--
-- $shakeDigest

-- | Compute the SHA-256 hash of a file.
fileDigest :: (MonadIO m) => FilePath -> m BS.ByteString
fileDigest file =
  liftIO $ SHA256.hashlazy <$> LBS.readFile file

-- | Compute the SHA-256 hash of the contents of a directory.
--
-- Note that this only computes the digest of all of the *contents* of the
-- files, and does not take directory structure into account.
directoryDigest :: (MonadIO m) => FilePath -> m BS.ByteString
directoryDigest dir = liftIO do
  paths <- getDirectoryFilesRecursive dir
  SHA256.finalize <$!> foldM (\ctx path -> SHA256.update ctx <$!> BS.readFile path) SHA256.init paths

-- | Compute the SHA-256 hash of a haskell value with
-- a @'Binary'@ encoding.
binaryDigest :: (Binary a) => a -> BS.ByteString
binaryDigest = SHA256.hashlazy . encode

-- * Displaying digests
--
-- $shakeDigestDisplay

-- | Show a strict bytestring as hex.
showHex :: BS.ByteString -> String
showHex bytes = do
  b <- BS.unpack bytes
  let (hi, lo) = b `divMod` 16
  [word4Hex hi, word4Hex lo]
  where

-- | Convert a @'Word8'@ in the range @0..15@ to the corresponding
-- hex character.
--
-- If the byte is greater than 15, this will throw an error.
word4Hex :: Word8 -> Char
word4Hex 0 = '0'
word4Hex 1 = '1'
word4Hex 2 = '2'
word4Hex 3 = '3'
word4Hex 4 = '4'
word4Hex 5 = '5'
word4Hex 6 = '6'
word4Hex 7 = '7'
word4Hex 8 = '8'
word4Hex 9 = '9'
word4Hex 10 = 'a'
word4Hex 11 = 'b'
word4Hex 12 = 'c'
word4Hex 13 = 'd'
word4Hex 14 = 'e'
word4Hex 15 = 'f'
word4Hex _ = error "word4Hex: expected value in range 0..15"
