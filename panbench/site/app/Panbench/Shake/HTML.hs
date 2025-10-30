-- | HTML reports for a @panbench@ run.
module Panbench.Shake.HTML
  ( reportHtml
  , siteRules
  ) where

import Data.Aeson (Value)
import Data.Aeson qualified as J
import Data.Foldable
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT

import Development.Shake

import Graphics.Vega.VegaLite (VegaLite)
import Graphics.Vega.VegaLite qualified as VL

import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Html.Renderer.Utf8 qualified as H

import Panbench.Shake.File
import Panbench.Shake.Matrix
import Panbench.Shake.Vega
import Panbench.Shake.Path

-- | Encode a JSON @'Value'@ as strict text.
encodeJsonUtf8 :: Value -> Text
encodeJsonUtf8 = LT.toStrict . LT.decodeUtf8 . J.encode

-- | Source code for our JS libraries.
data JsSources = JsSources
  { vegaJs :: Text
  , vegaLiteJs :: Text
  , vegaEmbedJs :: Text
  }

-- | Generate @<script>@ tags for all @'JSSources'@.
jsSourceHeader :: JsSources -> Html
jsSourceHeader JsSources{..} = do
  H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml vegaJs
  H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml vegaLiteJs
  H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml vegaEmbedJs

-- | Create @div@ for a single benchmarking matrix chart.
benchmarkMatrixHtml
  :: BenchmarkMatrix
  -> BenchmarkMatrixStats
  -> Html
benchmarkMatrixHtml (BenchmarkMatrix name _ _) stats = do
  H.div H.! A.id (H.toValue chartId) H.! A.class_ "chart" $ ""
  -- Create a containing <script> tag that embeds our data as a JSON blob,
  -- and call vegaEmbed on the above div.
  H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml $
    T.unlines
    [ "var spec" <> chartId <> " =" <> encodeJsonUtf8 (VL.fromVL chart) <> ";"
    , "vegaEmbed('#" <> chartId <> "', spec" <> chartId <> ")"
    , ".then((res) => {"
    , "});"
    ]
  where
    chartId :: Text
    chartId = T.pack name <> "Chart"

    chart :: VegaLite
    chart =
      VL.toVegaLite
      [ VL.datasets [("data", VL.dataFromJson (J.toJSON stats) [])]
      , VL.vConcat
        [ timeLayer "data" "user" "User Time (seconds)" 600 600
        , timeLayer "data" "system" "System Time (seconds)" 600 600
        , memoryLayer "data" "rss" "Max resident set size" 600 600
        ]
      ]

-- | Construct a benchmarking report from a JSON encoded data blob.
reportHtml
  :: JsSources
  -- ^ JS library source code.
  -> Text
  -- ^ CSS
  -> [(BenchmarkMatrix, BenchmarkMatrixStats)]
  -- ^ JSON data blob.
  -> Html
reportHtml jsSources css matrices =
  H.docTypeHtml do
    H.head do
      H.title "Panbench"
      H.meta H.! A.charset "UTF-8"
      jsSourceHeader jsSources
    H.body do
      H.style $ H.preEscapedToHtml css
      H.nav H.! A.id "tab-bar" $ do
        H.h4 $ H.a H.! A.href "#home" $ "Overview"
        H.h4 "Benchmarks"
        H.hr
        for_ matrices \(BenchmarkMatrix name _ _, _) -> do
          H.li $ H.a H.! A.href ("#" <> fromString name) $ fromString name
      H.main do
        H.div H.! A.id "home" H.! A.class_ "tab" $ do
          H.header $ H.h1 "Home"
        for_ matrices \(matrix@(BenchmarkMatrix name _ _), stats) ->
          H.div H.! A.id (fromString name) H.! A.class_ "tab" $ do
            H.header $ H.h1 $ H.preEscapedToHtml $ T.pack $ name
            benchmarkMatrixHtml matrix stats

-- | Rules for creating the site.
siteRules :: Rules (FilePath -> [BenchmarkMatrix] -> Action ())
siteRules = do
  needJsSources <- newCache \() -> do
    let vegaSrc = "web/js/vega@5.10.js"
    let vegaLiteSrc = "web/js/vega-lite@4.7.0.js"
    let vegaEmbedSrc = "web/js/vega-embed@6.3.2.js"
    need [vegaSrc, vegaLiteSrc, vegaEmbedSrc]
    vegaJs <- liftIO $ T.readFile vegaSrc
    vegaLiteJs <- liftIO $ T.readFile vegaLiteSrc
    vegaEmbedJs <- liftIO $ T.readFile vegaEmbedSrc
    pure JsSources{..}

  needCss <- newCache \() -> do
    need ["web/css/site.css"]
    liftIO $ T.readFile "web/css/site.css"

  pure \out matrices -> do
    jsSources <- needJsSources ()
    css <- needCss ()
    stats <- needBenchmarkMatrices matrices
    writeBinaryFileChanged (encodeOS out)
      $ H.renderHtml
      $ reportHtml jsSources css (zip matrices stats)
