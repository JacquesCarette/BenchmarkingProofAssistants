{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Path-based operations.
module Panbench.Shake.Path
  (
    -- * Encoding paths
    -- $pathEncoding
    EncodeOS(..)
  , DecodeOS(..)
    -- * Quasiquoters
  , osp
  -- * Re-exports
  , module OsPath
  , osstr
  ) where

import Control.Monad

import Development.Shake.Classes

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import System.OsString (osstr)
import System.OsPath as OsPath hiding (osp)
import System.OsString qualified as OsString

import System.IO (utf8, utf16le)

import System.FilePath.Posix qualified as Posix
import System.OsString.Internal.Types

--------------------------------------------------------------------------------
-- Encoding

-- $pathEncoding
--
-- We opt to use 'OsPath' for efficiency reasons, as benchmarking has shown that
-- most of our memory usage is path manipulation. This can directly impact our max RSS
-- measurements on linux, which does not clear the max RSS stats after a call to @execve@.
--
-- However, 'OsPath' also comes with zero guaruntees on encodings, as POSIX leaves the encoding
-- of filepaths unspecified, and Windows only vaguely requires UTF-16 LE. We never deal with
-- any strangely encoded filepaths, so this extra care for correctness ends up just being an
-- annoyance. To ease the burden a bit, we offer 'encodeOS' and 'decodeOS' to make translating
-- between 'String' and 'OsPath' a bit less painful. These functions are *technically* partial,
-- but only on filepaths that are not encoded in UTF-8 on linux or UTF-16 LE on Windows, which
-- is an edge case that we shouldn't ever encounter.

class EncodeOS a where
  encodeOS :: a -> OsPath

class DecodeOS a where
  decodeOS :: OsPath -> a

instance EncodeOS OsPath where
  encodeOS = id

instance DecodeOS OsPath where
  decodeOS = id

instance EncodeOS String where
  encodeOS path =
    case OsString.encodeWith utf8 utf16le path of
      Left err -> error $ "encodeOS: " <> show err
      Right str -> str

instance DecodeOS String where
  decodeOS path =
    case OsString.decodeWith utf8 utf16le path of
      Left err -> error $ "decodeOS: " <> show err
      Right str -> str

--------------------------------------------------------------------------------
-- Quasiquoters

joinExtensions :: [OsPath] -> OsPath
joinExtensions = foldr (<.>) mempty

-- | The 'osp' quasiquoter provides a small DSL for writing 'OsPath's.
--
-- It accepts inputs that are delimited by the @/@ character, followed by inputs delimited by @.@.
-- Moreover, it supports variable splicing, and also performs the appropriate encoding step.
--
-- As an example:
-- @
-- let x :: String = "foo"
--     y :: OsPath = [osp|bar/baz|] -- Becomes "bar/baz"
--     ext :: String = ".bux"
-- in [osp|$x/$y.$ext|] -- Becomes "foo/bar/baz.bux"
-- @
--
-- Note that spaces are *not* trimmed, so @[osp| foo/bar |]@ is really the string @" foo/bar "@.
-- Likewise, quotes are also *not* trimmed, so @[osp|"foo"|]@ is the string @"\"foo\""@.
osp :: QuasiQuoter
osp = QuasiQuoter {..}
  where
    quoteSegment :: String -> Q Exp
    quoteSegment ('$':(Posix.dropTrailingPathSeparator -> v)) =
      lookupValueName v >>= \case
        Just nm -> [| encodeOS $(pure $ VarE nm) |]
        Nothing -> fail $ "osp: variable " <> v <> "not in scope."
    quoteSegment seg =
      case OsString.encodeWith utf8 utf16le seg of
        Left err -> error $ "osp: could not encode.\n" <> show err
        Right str -> do
          when (not $ isValid str) $
            fail $ "osp: invalid filepath " <> seg
          lift str

    quotePath :: String -> Q [Exp]
    quotePath = traverse quoteSegment . Posix.splitPath

    quoteExtensions :: String -> Q [Exp]
    quoteExtensions exts =
      case Posix.splitExtension exts of
        (_, "") -> pure []
        (exts, _:ext) -> do
          qexts <- quoteExtensions exts
          qext <- quoteSegment ext
          pure (qexts ++ [qext])

    quoteExp :: String -> Q Exp
    quoteExp str = do
      let (path, exts) = Posix.splitExtensions str
      qpaths <- quotePath path
      qexts <- quoteExtensions exts
      [| joinPath $(pure $ ListE qpaths) <.> joinExtensions $(pure $ ListE qexts) |]

    quotePat _ = fail "path: pattern quotation not supported."
    quoteType _ = fail "path: type quotation not supported."
    quoteDec _ = fail "path: declaration quotation not supported."

--------------------------------------------------------------------------------
-- Orphans

-- | See https://github.com/haskell/filepath/issues/161
instance Binary OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  put (OsString (WindowsString str)) = put str
  get = (OsString . WindowsString) <$> get
#else
  put (OsString (PosixString str)) = put str
  get = (OsString . PosixString) <$> get
#endif
