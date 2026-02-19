{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
-- | Plotting via @pgf-plots@.
module Panbench.Shake.Plot.Pgf
  ( PgfQ(..)
  , PgfA(..)
  , PgfLegendEntry(..)
  , PgfMarker(..)
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
import Data.Map (Map)
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
  { pgfSubplotLegend :: !PgfLegendEntry
  -- ^ Legend information for this subplot.
  , pgfSubplotPoints :: [PgfPoint]
  -- ^ Points on the subplot.
  }

-- | Marker styles.
data PgfMarker
  = Circle
  | Square
  | Diamond
  | Triangle
  | Cross
  -- ^ This marker should not be used for langauges,
  -- as it does not have a hollow form.
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

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

data PgfLegendEntry = PgfLegendEntry
  { pgfLegendEntryName :: !Text
  -- ^ User-facing name to use for the legend entry.
  , pgfLegendEntryColor :: !Text
  -- ^ The color to use for this language.
  --
  -- See <https://tikz.dev/pgfplots/reference-markers#sec-4.7.5> for
  -- documentation on color support.
  , pgfLegendEntryMarker :: !PgfMarker
  -- ^ The marker to use for this language.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

data PgfQ = PgfQ
  { pgfQTitle :: !Text
  , pgfQXScale :: !Scale
  , pgfQYScale :: !Scale
  , pgfQStats :: !BenchmarkMatrixStats
  , pgfQLegend :: !(Map Text PgfLegendEntry)
  -- ^ Per-language legend information.
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
  -- let colors nm = Map.findWithDefault "black" nm pgfQColors
  -- let markers nm = Map.findWithDefault "*" nm pgfQMarkers
  let plots = hputPgf <$> generatePgfPlots pgfQTitle pgfQXScale pgfQYScale pgfQLegend pgfQStats
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
needBenchmarkingPgfs :: Map Text PgfLegendEntry -> [BenchmarkMatrix] -> Action [PgfA]
needBenchmarkingPgfs legend matrices = do
  results <- for matrices \matrix -> do
    stats <- runBenchmarkingMatrix matrix
    -- If we have a benchmarking matrix, just default to using log-log or linear-linear.
    pure PgfQ
      { pgfQTitle = T.pack $ benchmarkMatrixName matrix
      , pgfQXScale = benchmarkMatrixScale matrix
      , pgfQYScale = benchmarkMatrixScale matrix
      , pgfQStats = stats
      , pgfQLegend = legend
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
  -> Map Text PgfLegendEntry
  -- ^ Legend information.
  -> BenchmarkMatrixStats
  -- ^ Statistics for that matrix
  -> [PgfPlot]
  -- ^ PgfPlots corresponding to user time, system time, and max rss.
generatePgfPlots name xScale yScale pgfQLegend (BenchmarkMatrixStats stats) =
  -- We make sure to use base-10 logarithms for time and base-2 logarithms for memory,
  -- as these tend to be *much* more readable.
  [ makePgfPlotViaYProjection name "User Time (seconds)" xScale (rebase 10 yScale) (nanoSecondsToSeconds . benchUserTime)
  , makePgfPlotViaYProjection name "System Time (seconds)" xScale (rebase 10 yScale) (nanoSecondsToSeconds . benchSystemTime)
  , makePgfPlotViaYProjection name "Max RSS (megabytes)" xScale (rebase 2 yScale) (bytesToMegabytes . benchMaxRss)
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
        renormalizedStats <&> \(T.pack -> lang, langStats) -> PgfSubplot
        { pgfSubplotLegend = Map.findWithDefault missingLegend lang pgfQLegend
        , pgfSubplotPoints = langStats <&> \(size, rowStats) -> PgfPoint
          { pgfPointX = fromIntegral size
          , pgfPointY = project rowStats
          , pgfPointMeta = benchExitCode rowStats
          }

        }
      , ..
      }

    missingLegend :: PgfLegendEntry
    missingLegend = PgfLegendEntry
      { pgfLegendEntryName = "Missing Name!"
      , pgfLegendEntryColor = "black"
      , pgfLegendEntryMarker = Cross
      }

-- | Write a 'PgfPlot' to a handle.
hputPgf :: (MonadIO m) => PgfPlot -> Handle -> m ()
hputPgf PgfPlot{..} hdl =
  -- We don't bother with a pretty-printing library here,
  -- as we are on a tight memory budget and it is easy enough
  -- to just write the renderer by hand.
  --
  -- FIXME: legend below, separate markers
  liftIO $
  putTeXEnv "tikzpicture" do
      putTeXEnv "axis" do
        putDelimiter "[" "]\n" $ putSeparated (putUtf8Text ",\n") $
          [ putKeyValue "title" $ putUtf8Text pgfTitle
          , putKeyValue "xlabel" $ putDelimiter "{" "}" $ putUtf8Text pgfXLabel
          , putKeyValue "ylabel" $ putDelimiter "{" "}" $ putUtf8Text pgfYLabel
          , putKeyValue "legend to name" $ putUtf8Text legendName
          , putKeyValue "legend columns" $ putUtf8Text "2"
          ]
          ++ putScale "x" pgfXScale
          ++ putScale "y" pgfYScale
        -- Generate scatter plots first.
        for_ pgfSubplots \PgfSubplot{..} -> do
          putAddPlot
            [ putUtf8Text (pgfLegendEntryColor pgfSubplotLegend)
            , putKeyValue "point meta" $ putUtf8Text "explicit"
            , putUtf8Text "scatter"
            , putKeyValue "mark" (putHollowMarker (pgfLegendEntryMarker pgfSubplotLegend))
            , putUtf8Text "only marks"
            , putUtf8Text "forget plot"
            ]
            (filter (\point -> pgfPointMeta point /= 0) pgfSubplotPoints)
        -- Do another pass to add lines, making sure to not connect tests that failed.
        for_ pgfSubplots \PgfSubplot{..} -> do
          putAddPlot
            [ putUtf8Text (pgfLegendEntryColor pgfSubplotLegend)
            , putKeyValue "mark" (putSolidMarker (pgfLegendEntryMarker pgfSubplotLegend))
            ]
            (filter (\point -> pgfPointMeta point == 0) pgfSubplotPoints)
          putAddLegendEntry (pgfLegendEntryName pgfSubplotLegend)
      putUtf8Text "\\node" *> putDelimiter "[" "] " (putKeyValue "anchor" (putUtf8Text "north"))
      putUtf8Text "at" *> putDelimiter " (" ") " (putUtf8Text "current axis.below south")
      putDelimiter "{" "};\n" (putUtf8Text "\\ref" *> putDelimiter "{" "}" (putUtf8Text legendName))
   where
     legendName :: Text
     legendName = "legend"

     putUtf8Text :: Text -> IO ()
     putUtf8Text =  BS.hPut hdl . T.encodeUtf8

     putScale :: Text -> Scale -> [IO ()]
     putScale axis (Linear _step) =
       [ putKeyValue (axis <> "mode") $ putUtf8Text "linear"
       ]
     putScale axis (Log base) =
       [ putKeyValue (axis <> "mode") $ putUtf8Text "log"
       , putKeyValue ("log basis " <> axis) $ putDelimiter "{" "}" (putUtf8Show base)
       ] ++
       -- When we are laying out a logarithmic Y axis, we don't want to write out tick
       -- labels as 10^tick.
       [ putKeyValue "yticklabel" $ putDelimiter "{\n" "\n}" $ putSeparated (putUtf8Text "\n")
         [ putUtf8Text "  \\pgfkeys" *> putDelimiter "{" "}" (putKeyValue "/pgf/fpu" (putUtf8Text "true"))
         , putUtf8Text "  \\pgfmathparse" *> putDelimiter "{" "}" do
             putUtf8Text "pow" *> putDelimiter "(" ")" do
               putSeparated (putUtf8Text ",") [putUtf8Show base, putUtf8Text "\\tick"]
         , putUtf8Text "  \\pgfmathprintnumber"
           *> putDelimiter "[" "]" (putSeparated (putUtf8Text ",") [putUtf8Text "fixed relative", putKeyValue "precision" (putUtf8Text "3")])
           *> putDelimiter "{" "}" (putUtf8Text "\\pgfmathresult")
         , putUtf8Text "  \\pgfkeys" *> putDelimiter "{" "}" (putKeyValue "/pgf/fpu" (putUtf8Text "false"))
         ]
       | axis == "y"
       ]

     -- We don't want to use hPutStr here, as that consults the system locale
     -- to pick an encoding and does newline conversion.
     putUtf8Show :: (Show a) => a -> IO ()
     putUtf8Show = putUtf8Text . T.pack . show

     putDelimiter :: Text -> Text -> IO () -> IO ()
     putDelimiter l r m = putUtf8Text l *> m *> putUtf8Text r

     putSeparated :: IO () -> [IO ()] -> IO ()
     putSeparated delim = sequence_ . intersperse delim

     putTeXEnv :: Text -> IO () -> IO ()
     putTeXEnv env m = do
       -- [TODO: Reed M.] This avoids newline conversion on Windows; I don't know
       -- if this is the right thing to do? Would require testing on a windows machine.
       putUtf8Text "\\begin" *> putDelimiter "{" "}" (putUtf8Text env) *> putUtf8Text "\n"
       m
       putUtf8Text "\\end" *> putDelimiter "{" "}"  (putUtf8Text env) *> putUtf8Text "\n"

     putKeyValue :: Text -> IO () -> IO ()
     putKeyValue key m = putUtf8Text key *> putUtf8Text "=" *> m

     putAddPlot :: [IO ()] -> [PgfPoint] -> IO ()
     putAddPlot options points = do
        putUtf8Text "\\addplot"
        putDelimiter " [\n" "\n] " $ putSeparated (putUtf8Text ",\n") options
        putUtf8Text "coordinates"
        putDelimiter " {\n" "\n};\n" $ putSeparated (putUtf8Text "\n") $
          points <&> \PgfPoint{..} -> do
              putDelimiter "(" ") " $ putUtf8Show pgfPointX *> putUtf8Text "," *> putUtf8Show pgfPointY

     putAddLegendEntry :: Text -> IO ()
     putAddLegendEntry txt = putUtf8Text "\\addlegendentry" *> putDelimiter "{" "}\n" (putUtf8Text txt)

     -- See https://tikz.dev/pgfplots/reference-markers
     putHollowMarker :: PgfMarker -> IO ()
     putHollowMarker Circle = putUtf8Text "o"
     putHollowMarker Square = putUtf8Text "square"
     putHollowMarker Diamond = putUtf8Text "diamond"
     putHollowMarker Triangle = putUtf8Text "triangle"
     putHollowMarker Cross = putUtf8Text "x"

     putSolidMarker :: PgfMarker -> IO ()
     putSolidMarker Circle = putUtf8Text "*"
     putSolidMarker Square = putUtf8Text "square*"
     putSolidMarker Diamond = putUtf8Text "diamond*"
     putSolidMarker Triangle = putUtf8Text "triangle*"
     putSolidMarker Cross = putUtf8Text "x"

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
