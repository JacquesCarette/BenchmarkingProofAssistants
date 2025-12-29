-- | Shake utilities for interacting with @git@.
module Panbench.Shake.Git
  ( GitCloneQ(..)
  , withGitClone
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.Path

import System.Directory.OsPath qualified as Dir
import System.Process qualified as Proc


-- * Git Clone
--
-- $gitClone

-- | Shake query for cloning a git repository.
data GitCloneQ = GitCloneQ
  { gitCloneUpstream :: String
  -- ^ URL of the repository to clone.
  , gitCloneDir :: OsPath
  -- ^ Relative path to clone the repository to.
  , gitCloneRevision :: String
  -- ^ Revision to clone.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult GitCloneQ = ()

withGitClone :: GitCloneQ -> Action a -> Action a
withGitClone GitCloneQ{..} act = do
  actionBracket
    shallowClone
    (\_ -> Dir.removeDirectoryRecursive gitCloneDir)
    (\_ -> act)
  where
    shallowClone :: IO ()
    shallowClone =
      Proc.callProcess "git"
      [ "clone"
      , gitCloneUpstream
      , decodeOS gitCloneDir
      , "--depth=1"
      , "--revision=" <> gitCloneRevision
      ]
