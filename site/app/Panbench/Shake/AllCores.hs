{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
-- | Shake tools for commands that might use all cores on the
-- machine.
--
-- This resource is intended for use with external tools like
-- @make@ or @cabal@ that themselves take a @-j@ argument. If
-- we ran enough of these concurrently, we could very easily
-- bring the build process grinding to a halt, and also
-- exhuast the number of file descriptors of available while
-- we were at it.
module Panbench.Shake.AllCores
  ( withAllCores
  ) where

import Control.Concurrent.MVar

import Development.Shake

import GHC.Conc (getNumProcessors)

import System.IO.Unsafe

-- | An @MVar@ for ensuring exlusive access all cores.
--
-- Safety: @'allCores'@ is a purely internal implementation detail,
-- and should never be exposed to the outside. Moreover, we want to
-- ensure that this @'MVar'@ is globally unique across the entire
-- program. This makes @'unsafePerformIO'@ the right choice here:
-- any other way would require us to expose the ability to create
-- multiple handles, which is no good!
allCores :: MVar Int
allCores = unsafePerformIO do
  !nCores <- getNumProcessors
  newMVar nCores
{-# NOINLINE allCores #-}

-- | Run an @'Action'@ with exlusive access to all cores.
-- This is intended for use with commands like @cabal --jobs@ or
-- @make -j@, which provide their own external forms of parallelism.
--
-- Note that this will *not* prevent other haskell threads from running.
-- Instead, @'withAllCores'@ ensures that only one action wrapped with @'withAllCores'@
-- ever executes at a time.
withAllCores :: (Int -> Action a) -> Action a
withAllCores act = actionBracket (takeMVar allCores) (putMVar allCores) act
