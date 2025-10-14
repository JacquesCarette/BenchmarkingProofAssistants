{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
module Panbench.Shake.Benchmark
  ( -- * Benchmarking tools
    BenchmarkExecStats(..)
  , benchmark
  , benchmarkCommand
  ) where

import Data.Aeson
import Data.Foldable
import Data.Functor
import Data.Int
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Word

import Development.Shake
import Development.Shake.Classes (Hashable, Binary, NFData)

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import GHC.Generics

import System.Directory qualified as Dir

import Panbench.Shake.Env

--------------------------------------------------------------------------------
-- Benchmarking tools

-- | Benchmarking statistics gathered by @benchmark@.
data BenchmarkExecStats = BenchmarkExecStats
  { benchUserTime :: !Int64
  -- ^ The time spent in user code, measured in nanoseconds.
  , benchSystemTime :: !Int64
  -- ^ The time spent in kernel code, measured in nanoseconds.
  , benchMaxRss :: !Int64
  -- ^ Max resident set size, measured in bytes.
  , benchExitCode :: !Int64
  -- ^ The exit code of the benchmarked executable.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Hashable, Binary, NFData, FromJSON)

instance ToJSON BenchmarkExecStats where
    toEncoding = genericToEncoding defaultOptions

instance Storable BenchmarkExecStats where
  sizeOf _ = 4 * sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek sp = do
    let p = castPtr sp
    benchUserTime <- peek p
    benchSystemTime <- peekElemOff p 1
    benchMaxRss <- peekElemOff p 2
    benchExitCode <- peekElemOff p 3
    pure (BenchmarkExecStats {..})
  poke sp (BenchmarkExecStats{..}) = do
    let p = castPtr sp
    poke p benchUserTime
    pokeElemOff p 1 benchSystemTime
    pokeElemOff p 2 benchMaxRss
    pokeElemOff p 3 benchExitCode

foreign import capi "benchmark.h c_benchmark" c_benchmark
  :: CString
  -> Ptr CString
  -> Ptr CString
  -> CULong
  -> Ptr BenchmarkExecStats
  -> IO CInt

-- | Collect benchmarking stats for a single run of an executable.
--
-- @benchmark execPath args env@ will pause the GHC RTS system,
-- run the executable at @execPath@ with arguments @args@ with environment
-- variables @env@ set, and unpause the RTS.
--
-- If the executable exits with a non-zero exit code, then this
-- is reported in the returned @BenchmarkExecStats@. If there was some
-- other fatal error (executable not found, out of file descriptors, etc),
-- an @IOError@ is thrown.
--
-- For documentation on benchmarking statistics gathered, see @BenchmarkExecStats@.
benchmark :: FilePath -> [String] -> [(String, String)] -> Word64 -> FilePath -> IO BenchmarkExecStats
benchmark path args env timeout workingDir =
  Dir.withCurrentDirectory workingDir do
    p <- malloc
    r <-
      withCString path \cpath ->
      withMany withCString args \cargs ->
      withMany withCString (fmap (\(var, val) -> var <> "=" <> val) env) \cenv ->
      withArray0 nullPtr (cpath:cargs) \cargv ->
      withArray0 nullPtr cenv \cenvp ->
        c_benchmark cpath cargv cenvp (fromIntegral timeout) p
    if r == -1 then do
      throwErrnoPath "Panbench.Shake.Benchmark.benchmark" path
    else
      peek p
{-# NOINLINE benchmark #-}

-- | Benchmark command options.
--
-- Used in 'benchmarkCommand' when coalescing command options.
data BenchmarkCmdOpts = BenchmarkCmdOpts
  { benchEnvVars :: Map String String
  , benchCwd :: FilePath
  , benchPath :: [String]
  }

-- | A 'command_'-esque interface to 'benchmark'.
--
-- Options are processed left-to-right. Supported options are:
-- * 'Cwd', which sets the current working directory. Defaults to @_build@.
-- * 'Env', which sets the environment. This clobbers all existing environment variables.
-- * 'AddEnv', which adds an environment variable. This clobbers existing values for that variable.
-- * 'RemEnv', which removes an environment variable. If the variable is not present, this is a no-op.
-- * 'AddPath', which adds paths to the prefix and suffix of the path.
--
-- The defaults for the environment and path are taken from the shake process.
benchmarkCommand :: [CmdOption] -> Word64 -> FilePath -> [String] -> Action BenchmarkExecStats
benchmarkCommand opts timeout bin args = do
  path <- askPath
  envVars <- askEnvironment
  let initBench = BenchmarkCmdOpts
        { benchEnvVars = Map.fromList envVars
        , benchCwd = "_build"
        , benchPath = path
        }
  BenchmarkCmdOpts{..} <- foldlM handleOpt initBench opts
  liftIO (Dir.findFile benchPath bin) >>= \case
    Just absBin -> liftIO $ benchmark absBin args (Map.toList benchEnvVars) timeout benchCwd
    Nothing -> fail $ unlines $
        [ "benchmarkCommand: could not locate " <> bin <> " in PATH."
        , "The current PATH is:"
        ] ++ path
  where
    handleOpt :: BenchmarkCmdOpts -> CmdOption -> Action BenchmarkCmdOpts
    handleOpt bench (Cwd dir) = pure $ bench { benchCwd = dir }
    handleOpt bench (Env envVars) = pure $ bench { benchEnvVars = Map.fromList envVars }
    handleOpt bench (AddEnv var val) = pure $ bench { benchEnvVars = Map.insert var val (benchEnvVars bench) }
    handleOpt bench (RemEnv var) = pure $ bench { benchEnvVars = Map.delete var (benchEnvVars bench) }
    handleOpt bench (AddPath pfx sfx) = pure $ bench { benchPath = pfx ++ benchPath bench ++ sfx }
    handleOpt bench cmdOpt = putWarn ("Unsupported option " <> show cmdOpt <> " to benchmarkCommand, ignoring.") $> bench
