-- | Benchmark plotting via @vega-lite@.
--
-- This module provides some @vega-lite@ layers
-- for user time, system time, and max
module Panbench.Shake.Vega
  ( userTimeLayer
  , systemTimeLayer
  , maxRssLayer
  ) where

import Data.Text (Text)

import Graphics.Vega.VegaLite qualified as VL

-- | Vega-lite spec for a user-time plot.
userTimeLayer
  :: Text
  -- ^ Data source name.
  -> Double
  -- ^ Chart width, in pixels.
  -> Double
  -- ^ Chart height, in pixels.
  -> VL.VLSpec
userTimeLayer dataSource width height =
  VL.asSpec
  [ VL.dataFromSource dataSource []
  , VL.title "User Time" []
  , VL.width width
  , VL.height height
  , VL.mark VL.Line [VL.MPoint $ VL.PMMarker []]
  , VL.encoding
      $ langColor
      $ sizeXPosition
      $ VL.position VL.Y
        [ VL.PName "user"
        , VL.PmType VL.Quantitative
        , VL.PAxis [VL.AxTitle "User time (seconds)"]
        ]
      $ VL.tooltips
        [ [VL.TName "lang", VL.TmType VL.Nominal]
        , [VL.TName "user", VL.TmType VL.Quantitative]
        ]
      $ []
  , VL.transform
    $ nanosecondTransform "user"
    $ langFilter
    $ []
  , VL.selection
    $ langSelection
    $ []
  ]

-- | Vega-lite spec for a system-time plot.
systemTimeLayer
  :: Text
  -- ^ Data source name.
  -> Double
  -- ^ Chart width, in pixels.
  -> Double
  -- ^ Chart height, in pixels.
  -> VL.VLSpec
systemTimeLayer dataSource width height =
  VL.asSpec
  [ VL.dataFromSource dataSource []
  , VL.title "System Time" []
  , VL.width width
  , VL.height height
  , VL.mark VL.Line [VL.MPoint $ VL.PMMarker []]
  , VL.encoding
      $ langColor
      $ sizeXPosition
      $ VL.position VL.Y
        [ VL.PName "system"
        , VL.PmType VL.Quantitative
        , VL.PAxis [VL.AxTitle "System time (seconds)"]
        ]
      $ VL.tooltips
        [ [VL.TName "lang", VL.TmType VL.Nominal]
        , [VL.TName "system", VL.TmType VL.Quantitative]
        ]
      $ []
  , VL.transform
    $ nanosecondTransform "system"
    $ langFilter
    $ []
  , VL.selection
    $ langSelection
    $ []
  ]

-- | Vega-lite spec for a max resident set size plot.
maxRssLayer
  :: Text
  -- ^ Data source name.
  -> Double
  -- ^ Chart width, in pixels.
  -> Double
  -- ^ Chart height, in pixels.
  -> VL.VLSpec
maxRssLayer dataSource width height =
  VL.asSpec
  [ VL.dataFromSource dataSource []
  , VL.title "Max Resident Set Size" []
  , VL.width width
  , VL.height height
  , VL.mark VL.Line [VL.MPoint $ VL.PMMarker []]
  , VL.encoding
      $ langColor
      $ sizeXPosition
      $ VL.position VL.Y
        [ VL.PName "rss"
        , VL.PmType VL.Quantitative
        , VL.PAxis [VL.AxTitle "Max resident set size", VL.AxFormat "s"]
        ]
      $ VL.tooltips
        [ [VL.TName "lang", VL.TmType VL.Nominal]
        , [VL.TName "rss", VL.TmType VL.Quantitative, VL.TFormat "s"]
        ]
      $ []
  , VL.transform
    $ langFilter
    $ []
  , VL.selection
    $ langSelection
    $ []
  ]

-- * Vega helpers

sizeXPosition :: [VL.EncodingSpec] -> [VL.EncodingSpec]
sizeXPosition =
  VL.position VL.X
  [ VL.PName "size"
  , VL.PmType VL.Ordinal
  , VL.PAxis [VL.AxTitle "Input size"]
  ]

langColor :: [VL.EncodingSpec] -> [VL.EncodingSpec]
langColor =
  VL.color
  [ VL.MName "lang"
  , VL.MmType VL.Nominal
  , VL.MLegend [VL.LTitle "Language"]
  ]

-- | Transform a data row from nanoseconds to seconds.
nanosecondTransform :: VL.FieldName -> [VL.TransformSpec] -> [VL.TransformSpec]
nanosecondTransform name =
  VL.calculateAs ("datum." <> name <> " / 1000000000") name

langFilter :: [VL.TransformSpec] -> [VL.TransformSpec]
langFilter = VL.filter (VL.FSelection "legend")

-- | Specification for the language selection side panel.
langSelection :: [VL.SelectSpec] -> [VL.SelectSpec]
langSelection =
  VL.select "legend" VL.Multi
  [ VL.BindLegend (VL.BLField "lang")
  ]
