-- | Benchmark plotting via @vega-lite@.
--
-- This module provides some @vega-lite@ layers
-- for time and memory based metrics.
module Panbench.Shake.Plot.Vega
  ( timeLayer
  , memoryLayer
  ) where

import Data.Text (Text)

import Graphics.Vega.VegaLite qualified as VL

timeLayer
  :: Text
  -- ^ Data source name.
  -> VL.FieldName
  -- ^ Field name.
  -> Text
  -- ^ Title of field.
  -> Double
  -- ^ Chart width, in pixels.
  -> Double
  -- ^ Chart height, in pixels.
  -> VL.VLSpec
timeLayer dataSource field fieldTitle width height =
  VL.asSpec
  [ VL.dataFromSource dataSource []
  , VL.title fieldTitle []
  , VL.width width
  , VL.height height
  , VL.mark VL.Line [VL.MPoint timeoutMarker]
  , VL.encoding
      $ langEncoding
      $ sizeXEncoding
      $ timeoutEncoding
      $ timeYEncoding field fieldTitle
      $ []
  , VL.transform
    $ nanosecondTransform field
    $ exitCodeTransform
    $ []
  , VL.selection
    $ langSelection
    $ []
  ]

memoryLayer
  :: Text
  -- ^ Data source name.
  -> VL.FieldName
  -- ^ Field name.
  -> Text
  -- ^ Title of field.
  -> Double
  -- ^ Chart width, in pixels.
  -> Double
  -- ^ Chart height, in pixels.
  -> VL.VLSpec
memoryLayer dataSource field fieldTitle width height =
  VL.asSpec
  [ VL.dataFromSource dataSource []
  , VL.title fieldTitle []
  , VL.width width
  , VL.height height
  , VL.mark VL.Line [VL.MPoint timeoutMarker]
  , VL.encoding
      $ langEncoding
      $ sizeXEncoding
      $ timeoutEncoding
      $ memoryYEncoding field fieldTitle
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

-- | Encoding for a time-field on a Y axis.
timeYEncoding
  :: VL.FieldName
  -> Text
  -> [VL.EncodingSpec]
  -> [VL.EncodingSpec]
timeYEncoding field title =
  VL.position VL.Y
  [ VL.PName field
  , VL.PmType VL.Quantitative
  , VL.PScale
    [ VL.SType VL.ScLog
    , VL.SBase 2.0
    ]
  , VL.PAxis [VL.AxTitle title]
  ]
  . VL.tooltips
    [ [VL.TName "lang", VL.TmType VL.Nominal, VL.TTitle "Language"]
    , [VL.TName field, VL.TmType VL.Quantitative, VL.TTitle title]
    ]

-- | Encoding for a memory-field on a Y axis.
memoryYEncoding
  :: VL.FieldName
  -> Text
  -> [VL.EncodingSpec]
  -> [VL.EncodingSpec]
memoryYEncoding field title =
  VL.position VL.Y
  [ VL.PName field
  , VL.PmType VL.Quantitative
  , VL.PScale
    [ VL.SType VL.ScLog
    , VL.SBase 2.0
    ]
  , VL.PAxis [VL.AxTitle title, VL.AxFormat "s"]
  ]
  . VL.tooltips
    [ [VL.TName "lang", VL.TmType VL.Nominal, VL.TTitle "Language"]
    , [VL.TName field, VL.TmType VL.Quantitative, VL.TTitle title, VL.TFormat "s"]
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
