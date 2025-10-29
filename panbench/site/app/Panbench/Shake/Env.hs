{-# LANGUAGE QuasiQuotes #-}
-- | Shake utilities for interacting with environment variables.
module Panbench.Shake.Env
  ( -- * Path-related queries
    askPath
  , diffPathPrefix
  , diffPathSuffix
    -- * Environment variables
  , askEnvironment
    -- * Shake Rules
  , envRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.Path

import System.Process.Environment.OsString qualified as Env

--------------------------------------------------------------------------------
-- Path-related queries

-- | Shake query for getting the path.
data PathQ = PathQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult PathQ = [OsString]

-- | Get the current value of $PATH.
askPath :: Action [OsString]
askPath = askOracle PathQ

-- | @diffPathPrefix new old@ returns a list of @$PATH@ entries
-- that were newly added to the front of @$PATH@.
diffPathPrefix :: [OsString] -> [OsString] -> [OsString]
diffPathPrefix [] _ = []
diffPathPrefix news [] = news
diffPathPrefix (new:news) (old:olds)
  | new == old = []
  | otherwise = new:(diffPathPrefix news (old:olds))

-- | @diffPathPrefix new old@ returns a list of @$PATH@ entries
-- that were newly added to the back of @$PATH@.
diffPathSuffix :: [OsString] -> [OsString] -> [OsString]
diffPathSuffix news olds = reverse (diffPathPrefix (reverse news) (reverse olds))

--------------------------------------------------------------------------------
-- Environment variables

-- | Shake query for getting environment variables.
data EnvQ = EnvQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult EnvQ = [(OsString, OsString)]

-- | Get all environment variables.
askEnvironment :: Action [(OsString, OsString)]
askEnvironment = askOracle EnvQ

--------------------------------------------------------------------------------
-- Shake rules

envRules :: Rules ()
envRules = do
  _ <- addOracleHash \PathQ -> liftIO do
    Just path <- Env.getEnv [osstr|PATH|]
    pure $ splitSearchPath path
  _ <- addOracleHash \EnvQ -> liftIO $ Env.getEnvironment
  pure ()
