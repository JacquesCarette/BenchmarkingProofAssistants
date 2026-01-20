{-# LANGUAGE CPP #-}
-- | Cross-platform file utilities.
module Golden.Util.File
  ( createFile
  ) where

import Data.Bits

#if defined(mingw32_HOST_OS)
import System.Win32.File qualified as Win32
#else
import System.Posix.Files qualified as Unix
import System.Posix.IO qualified as Unix
#endif

-- | Create a file at a given path.
createFile :: FilePath -> IO ()
createFile path = do
#if defined(mingw32_HOST_OS)
  hdl <-
    Win32.createFile path
         Win32.gENERIC_WRITE
         Win32.fILE_SHARE_NONE
         Nothing
         Win32.oPEN_ALWAYS
         Win32.fILE_ATTRIBUTE_NORMAL
         Nothing
  Win32.closeHandle hdl
#else
  fd <-
    Unix.createFile path $
      Unix.ownerWriteMode
      .|. Unix.ownerReadMode
      .|. Unix.groupReadMode
      .|. Unix.otherReadMode
  Unix.closeFd fd
#endif
