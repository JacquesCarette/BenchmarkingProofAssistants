{-# LANGUAGE QuasiQuotes #-}
-- | Shake helpers for working with @chez@.
module Panbench.Shake.Chez
  ( -- * Locating @chez@
    ChezA(..)
  , needChez
    -- * Shake rules for @chez@
  , chezRules
  ) where

import Data.ByteString qualified as BS

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.Command
import Panbench.Shake.Digest
import Panbench.Shake.File
import Panbench.Shake.Path

-- | Find a version of @chez@ on the system path.
data ChezQ = ChezQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Response of a 'ChezQ' query.
data ChezA = ChezA
  { chezBinPath :: OsPath
  -- ^ Absolute path of the @chez@ binary.
  , chezVersion :: String
  -- ^ Version of @chez@, as reported by @chez --version@
  , chezDigest :: BS.ByteString
  -- ^ SHA 256 hash of the @opam@ binary.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult ChezQ = ChezA

-- | Require that @chez@ is installed.
needChez :: Action OsPath
needChez = chezBinPath <$> askOracle ChezQ

-- | Shake oracle for finding the @chez@ binary.
findChezCommandOracle :: ChezQ -> Action ChezA
findChezCommandOracle ChezQ = do
  findExecutableAmong [[osp|chez|], [osp|chezscheme|]] >>= \case
    Nothing ->
      fail $ unlines $
        [ "Could not find a chez executable in the path"
        , "Perhaps it is not installed?"
        ]
    Just chezBinPath -> do
      Stdout chezVersion <- osCommand [] chezBinPath ["--version"]
      chezDigest <- fileDigest chezBinPath
      pure ChezA {..}

-- | Shake rules for @chez@.
chezRules :: Rules ()
chezRules = do
    _ <- addOracleCache findChezCommandOracle
    pure ()
