{-# LANGUAGE ViewPatterns #-}
-- | Plotting via @pgf-plots@.
module Panbench.Shake.Plot.Pgf
  ( addPgfMatrixRule
  ) where

import Control.Monad.IO.Class

import Data.ByteString qualified as BS
import Data.Foldable
import Data.Functor
import Data.Int
import Data.List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Development.Shake

import Numeric.Natural

import Panbench.Shake.Benchmark
import Panbench.Shake.File
import Panbench.Shake.Matrix
import Panbench.Shake.Path

import System.IO (Handle)
import System.FilePath qualified as FilePath

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
  , pgfSubplots :: [PgfSubplot]
  -- ^ All subplots within the plot.
  }

-- | Expand a filepath @_build/pgf/*/*.tex@ into a list
-- of PGF filepaths for each of the charts that we support.
expandPgfFilePaths :: FilePath -> Maybe [FilePath]
expandPgfFilePaths path
  | "_build/pgf/*/*.tex" ?== path =
    -- Technically a bit sketchy because of the deficiencies
    -- of @takeBaseName@ (See Panbench.Shake.Path)
    -- but you gotta do what you gotta do.
    let base = FilePath.takeBaseName path
    in if base `elem` ["user", "system", "rss"] then
      Just
      [ FilePath.replaceBaseName path "user"
      , FilePath.replaceBaseName path "system"
      , FilePath.replaceBaseName path "rss"
      ]
    else
      Nothing
  | otherwise = Nothing

-- | Add a rule for building pgf plots.
addPgfMatrixRule :: (String -> Action BenchmarkMatrix) -> Rules ()
addPgfMatrixRule needMatrix =
  expandPgfFilePaths &?> \case
    [userPath, systemPath, rssPath] -> do
      -- More sketchy path manipulation but there doesn't seem to be a
      -- nicer answer.
      let base = FilePath.takeBaseName $ FilePath.takeDirectory userPath
      matrix <- needMatrix base
      stats <- runBenchmarkingMatrix matrix
      let (userPlot, systemPlot, rssPlot) = benchmarkMatrixPgfPlots matrix stats
      writeBinaryHandleChanged (encodeOS userPath) (hputPgf userPlot)
      writeBinaryHandleChanged (encodeOS systemPath) (hputPgf systemPlot)
      writeBinaryHandleChanged (encodeOS rssPath) (hputPgf rssPlot)
    _ -> fail "addPgfMatrixRule: got more than the expected 3 pgf outputs."

-- | Create a PGF plot from a benchmarking result.
benchmarkMatrixPgfPlots
  :: BenchmarkMatrix
  -- ^ Benchmarking matrix.
  -> BenchmarkMatrixStats
  -- ^ Statistics for that matrix
  -> (PgfPlot, PgfPlot, PgfPlot)
  -- ^ PgfPlots corresponding to user time, system time, and max rss.
benchmarkMatrixPgfPlots (BenchmarkMatrix (T.pack -> name) _) (BenchmarkMatrixStats stats) =
  ( makePgfPlotViaYProjection name "User Time (seconds)" (nanoSecondsToSeconds . benchUserTime)
  , makePgfPlotViaYProjection name "System Time (seconds)" (nanoSecondsToSeconds . benchUserTime)
  , makePgfPlotViaYProjection name "Max RSS (bytes)" (bytesToMegabytes . benchMaxRss)
  )
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

    makePgfPlotViaYProjection :: Text -> Text -> (BenchmarkExecStats -> Double) -> PgfPlot
    makePgfPlotViaYProjection pgfTitle pgfYLabel project = PgfPlot
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
      putTeXEnv "loglogaxis" do
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
