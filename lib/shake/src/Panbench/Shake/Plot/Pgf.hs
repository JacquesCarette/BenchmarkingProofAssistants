{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
-- | Plotting via @pgf-plots@.
module Panbench.Shake.Plot.Pgf
  ( PgfQ(..)
  , PgfA(..)
  , needPgf
  , needBenchmarkingPgfs
  , needPgfTeX
  -- * Shake rules
  , pgfRules
  ) where

import Control.Monad.IO.Class
import Control.Monad

import Data.ByteString qualified as BS
import Data.Foldable
import Data.Functor
import Data.Int
import Data.List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Numeric.Natural

import Panbench.Shake.Benchmark
import Panbench.Shake.File
import Panbench.Shake.Matrix
import Panbench.Shake.Path
import Panbench.Shake.Range
import Panbench.Shake.Store

import System.IO (Handle)
-- import System.FilePath qualified as FilePath

data PgfPoint = PgfPoint
  { pgfPointX :: !Double
  -- ^ The X coordinate of a PGF coordinate.
  , pgfPointY :: !Double
  -- ^ The Y coordinate of a PGF coordinate.
  , pgfPointMeta :: !Int64
  -- ^ Point metadata. This is used for adding exit-code markers.
  -- See <https://tikz.dev/pgfplots/reference-meta#pgfp./pgfplots/point:meta>
  -- for documentation.
  }

data PgfSubplot = PgfSubplot
  { pgfSubplotLegend :: !Text
  -- ^ Legend key to use for this subplot.
  , pgfSubplotPoints :: [PgfPoint]
  -- ^ Points on the subplot.
  }

data PgfPlot = PgfPlot
  { pgfTitle :: !Text
  -- ^ The title of the plot.
  , pgfXLabel :: !Text
  -- ^ Label on the X axis
  , pgfYLabel :: !Text
  -- ^ Label on the X axis
  , pgfXScale :: !Scale
  -- ^ Scale to use for X-axis.
  , pgfYScale :: !Scale
  -- ^ Scale to use for Y-axis.
  , pgfSubplots :: [PgfSubplot]
  -- ^ All subplots within the plot.
  }

data PgfQ = PgfQ
  { pgfQTitle :: !Text
  , pgfQXScale :: !Scale
  , pgfQYScale :: !Scale
  , pgfQStats :: !BenchmarkMatrixStats
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

data PgfA = PgfA
  { pgfATitle :: !Text
  , pgfAPlots :: [OsPath]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

pgfStorePaths :: OsPath -> [OsPath]
pgfStorePaths store = [[osp|$store/user.tex|], [osp|$store/system.tex|], [osp|$store/rss.tex|]]

-- | Generate a @pgf@ plot for some benchmarking statistics.
--
-- The result of the run will be content-addressed. This has some upsides:
-- we always keep previous reports around, which lets aggregate statistics over
-- time more easily. However, this can lead to bloat on disc, but this should be
-- relatively easy to manage by just running clean actions occasionally.
pgfStoreOracle :: PgfQ -> OsPath -> Action ()
pgfStoreOracle PgfQ{..} store = do
  let plots = hputPgf <$> generatePgfPlots pgfQTitle pgfQXScale pgfQYScale pgfQStats
  let paths = pgfStorePaths store
  zipWithM_ writeBinaryHandleChanged paths plots

needPgf :: PgfQ -> Action PgfA
needPgf pgf = do
  !store <- storeOraclePath <$> askStoreOracle pgf
  pure PgfA
    { pgfATitle = pgfQTitle pgf
    , pgfAPlots = pgfStorePaths store
    }

-- | Generate @pgf@ plots for a list of benchmarking matrices.
needBenchmarkingPgfs :: [BenchmarkMatrix] -> Action [PgfA]
needBenchmarkingPgfs matrices = do
  results <- for matrices \matrix -> do
    stats <- runBenchmarkingMatrix matrix
    -- If we have a benchmarking matrix, just default to using log-log or linear-linear.
    pure PgfQ
      { pgfQTitle = T.pack $ benchmarkMatrixName matrix
      , pgfQXScale = benchmarkMatrixScale matrix
      , pgfQYScale = benchmarkMatrixScale matrix
      , pgfQStats = stats
      }
  -- If we do this sequentially, then we can stream the results instead of
  -- having to gather them all in memory.
  traverse needPgf results

-- | Request a summary page that contains all pgfs.
needPgfTeX :: OsPath -> [PgfA] -> Action ()
needPgfTeX path pgfs =
  writeBinaryHandleChanged path (hputPgfTeX pgfs)

-- | Create a PGF plot from a benchmarking result.
generatePgfPlots
  :: Text
  -- ^ Benchmarking matrix name.
  -> Scale
  -- ^ Scale to use for the X-axis.
  -> Scale
  -- ^ Scale to use for the Y-axis.
  -> BenchmarkMatrixStats
  -- ^ Statistics for that matrix
  -> [PgfPlot]
  -- ^ PgfPlots corresponding to user time, system time, and max rss.
generatePgfPlots name xScale yScale (BenchmarkMatrixStats stats) =
  [ makePgfPlotViaYProjection name "User Time (seconds)" xScale yScale (nanoSecondsToSeconds . benchUserTime)
  , makePgfPlotViaYProjection name "System Time (seconds)" xScale yScale (nanoSecondsToSeconds . benchSystemTime)
  , makePgfPlotViaYProjection name "Max RSS (bytes)" xScale yScale (bytesToMegabytes . benchMaxRss)
  ]
  where
    -- Converting to @Double@ here is required, as PGFPlots does not make it easy to
    -- perform the scaling on the PGF side as far as I can tell.
    nanoSecondsToSeconds :: Int64 -> Double
    nanoSecondsToSeconds ns = fromIntegral ns / 1e9

    bytesToMegabytes :: Int64 -> Double
    bytesToMegabytes bytes = fromIntegral bytes / 1e6

    -- [HACK: Reed M, 10/02/2026] Our representation of benchmarking results is a bit
    -- annoying here. We chose to completely denormalize the benchmarking results,
    -- as @vega-lite@ really wants the data in this form. This means that we
    -- need to do a re-normalization step here, which is a bit silly.
    --
    -- However, it would be more work to produce data in normalized form and
    -- then convert it in the vega module, so we just do the suboptimal thing here.
    renormalizedStats :: [(String, [(Natural, BenchmarkExecStats)])]
    renormalizedStats =
      Map.toList
      $ Map.fromListWith (++)
      $ stats <&> \(lang, size, bench) -> (lang, [(size, bench)])

    makePgfPlotViaYProjection :: Text -> Text -> Scale -> Scale -> (BenchmarkExecStats -> Double) -> PgfPlot
    makePgfPlotViaYProjection pgfTitle pgfYLabel pgfXScale pgfYScale project = PgfPlot
      { pgfXLabel = "Size"
      , pgfSubplots =
        renormalizedStats <&> \(lang, langStats) -> PgfSubplot
        { pgfSubplotLegend = T.pack lang
        , pgfSubplotPoints = langStats <&> \(size, rowStats) -> PgfPoint
          { pgfPointX = fromIntegral size
          , pgfPointY = project rowStats
          , pgfPointMeta = benchExitCode rowStats
          }

        }
      , ..
      }

-- | Write a 'PgfPlot' to a handle.
hputPgf :: (MonadIO m) => PgfPlot -> Handle -> m ()
hputPgf PgfPlot{..} hdl =
  -- We don't bother with a pretty-printing library here,
  -- as we are on a tight memory budget and it is easy enough
  -- to just write the renderer by hand.
  liftIO $
  putTeXEnv "tikzpicture" do
      putTeXEnv "axis" do
        putDelimiter "[" "]" do
          putKeyValue "title" $ putUtf8Text pgfTitle
          putKeyValue "xlabel" $ putDelimiter "{" "}" $ putUtf8Text pgfXLabel
          putKeyValue "ylabel" $ putDelimiter "{" "}" $ putUtf8Text pgfYLabel
          putKeyValue "legend entries" $ putDelimiter "{" "}" $
            traverse_ putUtf8Text $ intersperse "," $ pgfSubplotLegend <$> pgfSubplots
          putKeyValue "legend pos" $ putUtf8Text "outer north east"
        for_ (pgfSubplotPoints <$> pgfSubplots) \points ->
          putUtf8Text "\\addplot coordinates " *> putDelimiter "{\n" "};\n" do
            for_ points \PgfPoint{..} -> do
              putDelimiter "(" ") " $ putUtf8Show pgfPointX *> putUtf8Text "," *> putUtf8Show pgfPointY
              putDelimiter "[" "]\n" $ putUtf8Show pgfPointMeta
   where
     putUtf8Text :: Text -> IO ()
     putUtf8Text =  BS.hPut hdl . T.encodeUtf8

     -- We don't want to use hPutStr here, as that consults the system locale
     -- to pick an encoding and does newline conversion.
     putUtf8Show :: (Show a) => a -> IO ()
     putUtf8Show = putUtf8Text . T.pack . show

     putDelimiter :: Text -> Text -> IO () -> IO ()
     putDelimiter l r m = putUtf8Text l *> m *> putUtf8Text r

     putTeXEnv :: Text -> IO () -> IO ()
     putTeXEnv env m = do
       -- [TODO: Reed M.] This avoids newline conversion on Windows; I don't know
       -- if this is the right thing to do? Would require testing on a windows machine.
       putUtf8Text "\\begin" *> putDelimiter "{" "}" (putUtf8Text env) *> putUtf8Text "\n"
       m
       putUtf8Text "\\end" *> putDelimiter "{" "}"  (putUtf8Text env) *> putUtf8Text "\n"

     putKeyValue :: Text -> IO () -> IO ()
     putKeyValue key m = putUtf8Text key *> putUtf8Text "=" *> m *> putUtf8Text ",\n"


-- | Create a TeX summary of a bunch of @pgf@ plots
hputPgfTeX :: (MonadIO m) => [PgfA] -> Handle -> m ()
hputPgfTeX pgfs hdl = liftIO $
  for_ pgfs \PgfA{..} -> do
    putSubSection pgfATitle
    sequence_ $ intersperse putLineBreak $ putInput <$> pgfAPlots
  where
    putUtf8Text :: Text -> IO ()
    putUtf8Text =  BS.hPut hdl . T.encodeUtf8

    putDelimiter :: Text -> Text -> IO () -> IO ()
    putDelimiter l r m = putUtf8Text l *> m *> putUtf8Text r

    putSubSection :: Text -> IO ()
    putSubSection sec = putUtf8Text "\\subsection" *> putDelimiter "{" "}\n" (putUtf8Text sec)

    putLineBreak :: IO ()
    putLineBreak = putUtf8Text "\\\\\n"

    putInput :: OsPath -> IO ()
    putInput path = putUtf8Text "\\input" *> putDelimiter "{" "}\n" (putUtf8Text $ decodeOS path)

pgfRules :: Rules ()
pgfRules = do
  addStoreOracle "pgf" pgfStoreOracle
