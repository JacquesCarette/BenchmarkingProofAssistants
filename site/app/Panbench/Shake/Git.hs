-- | Shake utilities for interacting with @git@.
module Panbench.Shake.Git
  ( gitRepoExists
  , gitWorktreeExists
  -- $gitClone
  , GitCloneQ(..)
  , needGitClone
  -- $gitWorktree
  , GitWorktreeQ(..)
  , needGitWorktree
  , gitRevWorktreePath
  -- $gitRules
  , gitRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import System.FilePath
import System.Directory qualified as Dir

-- | Check if a directory exists and contains a git repository.
--
-- Unlike @'doesFileExist'@, this does not add the directory as a
-- @shake@ dependency.
gitRepoExists :: FilePath -> Action Bool
gitRepoExists dir =
  -- We use @Dir.doesDirectoryExist@ to avoid tracking the @.git@ folder as a dep.
  liftIO $ Dir.doesDirectoryExist (dir </> ".git")

-- | Check if a worktree exists and contains a git repository.
--
-- Unlike @'doesFileExist'@, this does not add the directory as a
-- @shake@ dependency.
gitWorktreeExists :: FilePath -> Action Bool
gitWorktreeExists dir =
  liftIO $ Dir.doesFileExist (dir </> ".git")


-- * Git Clone
--
-- $gitClone

-- | Shake query for cloning a git repository.
data GitCloneQ = GitCloneQ
  { gitCloneUpstream :: FilePath
  -- ^ URL of the repository to clone.
  , gitCloneDir :: FilePath
  -- ^ Relative path to clone the repository to.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult GitCloneQ = ()

-- | Oracle for cloning a git repository.
gitCloneOracle :: Rules (GitCloneQ -> Action ())
gitCloneOracle =
  addOracle \GitCloneQ{..} ->
    gitRepoExists gitCloneDir >>= \case
      True -> pure ()
      False -> command_ [] "git" ["clone", gitCloneUpstream, gitCloneDir]

-- | Require that a repository is cloned.
--
-- If a git repository already exists at provided
-- path, no clone will be performed.
needGitClone :: GitCloneQ -> Action ()
needGitClone = askOracle

-- * Git Worktrees
--
-- $gitWorktree

-- | Shake query for creating a @git@ worktree.
data GitWorktreeQ = GitWorktreeQ
  { gitWorktreeUpstream :: FilePath
  -- ^ Upstream of the main git repo.
  , gitWorktreeRepo :: FilePath
  -- ^ Relative path to the main repository of the worktree.
  , gitWorktreeDir :: FilePath
  -- ^ Relative path to create the worktree in.
  , gitWorktreeRev :: String
  -- ^ Revision to check out for the worktree.
  }

  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult GitWorktreeQ = ()

-- | Oracle for creating a git worktree.
gitWorktreeOracle :: Rules (GitWorktreeQ -> Action ())
gitWorktreeOracle =
  addOracle \GitWorktreeQ{..} -> do
    needGitClone (GitCloneQ gitWorktreeUpstream gitWorktreeRepo)
    -- Worktrees store their .git in a file, not a directory.
    gitWorktreeExists gitWorktreeDir >>= \case
      True -> pure ()
      False -> command [] "git" ["--git-dir", gitWorktreeRepo </> ".git", "worktree", "add", gitWorktreeDir, gitWorktreeRev]

-- | Require that a git worktree for a repository exists.
--
-- This will clone the repository if required.
needGitWorktree :: GitWorktreeQ -> Action ()
needGitWorktree = askOracle

-- | Construct a path to checkout a @git@ worktree to
-- based off of a revision.
gitRevWorktreePath :: FilePath -> String -> FilePath
gitRevWorktreePath repo rev =
  repo ++ "-" ++ map (\c -> if isPathSeparator c then '-' else c) rev

-- | Shake rules for git
--
-- $gitRules

gitRules :: Rules ()
gitRules = do
  _ <- gitCloneOracle
  _ <- gitWorktreeOracle
  pure ()
