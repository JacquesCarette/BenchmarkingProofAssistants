-- | Benchmark plotting via @vega-lite@.
--
-- This module provides some @vega-lite@ layers
-- for user time, system time, and max resident set size.
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
  , VL.mark VL.Line [VL.MPoint timeoutMarker]
  , VL.encoding
      $ langEncoding
      $ sizeXEncoding
      $ timeoutEncoding
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
    $ exitCodeTransform
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
  , VL.mark VL.Line [VL.MPoint timeoutMarker]
  , VL.encoding
      $ langEncoding
      $ sizeXEncoding
      $ timeoutEncoding
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
    $ exitCodeTransform
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
  , VL.mark VL.Line [VL.MPoint timeoutMarker]
  , VL.encoding
      $ langEncoding
      $ sizeXEncoding
      $ timeoutEncoding
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
    $ exitCodeTransform
    $ []
  , VL.selection
    $ langSelection
    $ []
  ]

-- * Vega helpers

-- | Timeout markers.
timeoutMarker :: VL.PointMarker
timeoutMarker =
  VL.PMMarker
  [ VL.MSize 100
  , VL.MAngle 45
  ]

-- | Place the benchmark size along the X axis.
sizeXEncoding :: [VL.EncodingSpec] -> [VL.EncodingSpec]
sizeXEncoding =
  VL.position VL.X
  [ VL.PName "size"
  , VL.PmType VL.Ordinal
  , VL.PAxis [VL.AxTitle "Input size"]
  ]

-- | Timeout styling.
timeoutEncoding :: [VL.EncodingSpec] -> [VL.EncodingSpec]
timeoutEncoding =
  VL.shape
  [ VL.MName "timeout"
  , VL.MmType VL.Ordinal
  , VL.MScale
    [ VL.SRange $ VL.RStrings ["circle", "cross"]
    ]
  , VL.MLegend
    [VL.LTitle "Failed"
    ]
  ]
  . VL.strokeDash
  [ VL.MName "timeout"
  , VL.MmType VL.Ordinal
  , VL.MScale
    [ VL.SRange $ VL.RNumberLists [[1,0], [0,1]]
    ]
  , VL.MLegend []
  ]

-- | Color the data by the language field.
langEncoding :: [VL.EncodingSpec] -> [VL.EncodingSpec]
langEncoding =
  VL.color
  [ VL.MName "lang"
  , VL.MmType VL.Nominal
  , VL.MLegend [VL.LTitle "Language"]
  ]
  . VL.opacity
  [ VL.MSelectionCondition (VL.SelectionName "legend") [VL.MNumber 1.0] [VL.MNumber 0.1]
  ]

-- | Transform a data row from nanoseconds to seconds.
nanosecondTransform :: VL.FieldName -> [VL.TransformSpec] -> [VL.TransformSpec]
nanosecondTransform name =
  VL.calculateAs ("datum." <> name <> " / 1000000000") name

-- | Clamp exit codes.
exitCodeTransform :: [VL.TransformSpec] -> [VL.TransformSpec]
exitCodeTransform =
  VL.calculateAs ("datum.exit != 0") "timeout"

-- | Specification for the language selection side panel.
langSelection :: [VL.SelectSpec] -> [VL.SelectSpec]
langSelection =
  VL.select "legend" VL.Multi
  [ VL.BindLegend (VL.BLField "lang")
  ]
