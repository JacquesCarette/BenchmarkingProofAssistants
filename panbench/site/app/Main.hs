{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NumDecimals #-}
module Main where

import Data.Foldable
import Data.Functor.Contravariant
import Data.Word

import Development.Shake

import Numeric.Natural

import System.Directory
import System.FilePath

import Panbench.Grammar.Agda
import Panbench.Grammar.Idris
import Panbench.Grammar.Lean
import Panbench.Grammar.Rocq

import Panbench.Shake.Dev
import Panbench.Shake.Chez
import Panbench.Shake.Env
import Panbench.Shake.Git
import Panbench.Shake.HTML
import Panbench.Shake.Lang.Agda
import Panbench.Shake.Lang.Idris
import Panbench.Shake.Lang.Lean
import Panbench.Shake.Lang.Rocq
import Panbench.Shake.Make
import Panbench.Shake.Matrix
import Panbench.Shake.Opam

import Panbench.Generator.Conversion.Addition qualified as ConversionAddition
import Panbench.Generator.DatatypeParameters qualified as DatatypeParameters
import Panbench.Generator.Empty qualified as Baseline
import Panbench.Generator.LargeDependentRecord qualified as LargeDependentRecord
import Panbench.Generator.LargeIndexedDatatype qualified as LargeIndexedDatatype
import Panbench.Generator.LargeIndexedParameterisedDatatype qualified as LargeIndexedParameterisedDatatype
import Panbench.Generator.LargeSimpleDatatype qualified as LargeSimpleDatatype
import Panbench.Generator.LargeSimpleRecord qualified as LargeSimpleRecord
import Panbench.Generator.NestedLet qualified as NestedLet
import Panbench.Generator.NestedLetAdditions qualified as NestedLetAdditions
import Panbench.Generator.NestedLetFunctions qualified as NestedLetFunctions
import Panbench.Generator.Newlines qualified as Newlines
import Panbench.Generator.Postulates qualified as Postulates
import Panbench.Generator.Parens qualified as Parens
import Panbench.Generator.RecordParameters qualified as RecordParameters
import Panbench.Generator.SequentialDefinitions qualified as SequentialDefinitions
import Panbench.Generator.SequentialDependentRecords qualified as SequentialDependentRecords
import Panbench.Generator.SequentialSimpleRecords qualified as SequentialSimpleRecords
import Panbench.Generator.SimpleDataDefinitions qualified as SimpleDataDefinitions

defaultTimeout :: Word64
defaultTimeout = 60

benchmarks :: [BenchmarkMatrix]
benchmarks =
  [
    BenchmarkMatrix "Baseline" [0..10]
    [ benchmarkMatrixRow Agda Baseline.generator defaultTimeout
    , benchmarkMatrixRow Idris Baseline.generator defaultTimeout
    , benchmarkMatrixRow Lean Baseline.generator defaultTimeout
    , benchmarkMatrixRow Rocq Baseline.generator defaultTimeout
    ]
  , BenchmarkMatrix "ConversionAddition" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow Agda (contramap (100,) ConversionAddition.generator) defaultTimeout
    , benchmarkMatrixRow Idris (contramap (100,) ConversionAddition.generator) defaultTimeout
    , benchmarkMatrixRow Lean (contramap (100,) ConversionAddition.generator) defaultTimeout
    , benchmarkMatrixRow Rocq (contramap (100,) ConversionAddition.generator) defaultTimeout
    ]
  , BenchmarkMatrix "DatatypeParameters" [2^n | (n :: Natural) <- [0..10]]
    [ benchmarkMatrixRow Agda DatatypeParameters.generator defaultTimeout
    , benchmarkMatrixRow Idris DatatypeParameters.generator defaultTimeout
    , benchmarkMatrixRow Lean DatatypeParameters.generator defaultTimeout
    , benchmarkMatrixRow Rocq DatatypeParameters.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeDependentRecord" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow Agda LargeDependentRecord.generator defaultTimeout
    , benchmarkMatrixRow Idris LargeDependentRecord.generator defaultTimeout
    , benchmarkMatrixRow Lean LargeDependentRecord.generator defaultTimeout
    , benchmarkMatrixRow Rocq LargeDependentRecord.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeIndexedDatatype" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow Agda LargeIndexedDatatype.generator defaultTimeout
    , benchmarkMatrixRow Idris LargeIndexedDatatype.generator defaultTimeout
    , benchmarkMatrixRow Lean LargeIndexedDatatype.generator defaultTimeout
    , benchmarkMatrixRow Rocq LargeIndexedDatatype.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeIndexedParameterisedDatatype" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow Agda LargeIndexedParameterisedDatatype.generator defaultTimeout
    , benchmarkMatrixRow Idris LargeIndexedParameterisedDatatype.generator defaultTimeout
    , benchmarkMatrixRow Lean LargeIndexedParameterisedDatatype.generator defaultTimeout
    , benchmarkMatrixRow Rocq LargeIndexedParameterisedDatatype.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeSimpleDatatype" [2^n | (n :: Natural) <- [0..11]]
    [ benchmarkMatrixRow Agda LargeSimpleDatatype.generator defaultTimeout
    , benchmarkMatrixRow Idris LargeSimpleDatatype.generator defaultTimeout
    , benchmarkMatrixRow Lean LargeSimpleDatatype.generator defaultTimeout
    , benchmarkMatrixRow Rocq LargeSimpleDatatype.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeSimpleRecord" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow Agda LargeSimpleRecord.generator defaultTimeout
    , benchmarkMatrixRow Idris LargeSimpleRecord.generator defaultTimeout
    , benchmarkMatrixRow Lean LargeSimpleRecord.generator defaultTimeout
    , benchmarkMatrixRow Rocq LargeSimpleRecord.generator defaultTimeout
    ]
  , BenchmarkMatrix "NestedLet" [2^n | (n :: Natural) <- [0..10]]
    [ benchmarkMatrixRow Agda NestedLet.generator defaultTimeout
    , benchmarkMatrixRow Idris NestedLet.generator defaultTimeout
    , benchmarkMatrixRow Lean NestedLet.generator defaultTimeout
    , benchmarkMatrixRow Rocq NestedLet.generator defaultTimeout
    ]
  , BenchmarkMatrix "NestedLetAdditions" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow Agda NestedLetAdditions.generator defaultTimeout
    , benchmarkMatrixRow Idris NestedLetAdditions.generator defaultTimeout
    , benchmarkMatrixRow Lean NestedLetAdditions.generator defaultTimeout
    , benchmarkMatrixRow Rocq NestedLetAdditions.generator defaultTimeout
    ]
  , BenchmarkMatrix "NestedLetFunctions" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow Agda NestedLetFunctions.generator defaultTimeout
    , benchmarkMatrixRow Idris NestedLetFunctions.generator defaultTimeout
    , benchmarkMatrixRow Lean NestedLetFunctions.generator defaultTimeout
    , benchmarkMatrixRow Rocq NestedLetFunctions.generator defaultTimeout
    ]
  , BenchmarkMatrix "Newlines" [10^n | (n :: Natural) <- [0..7]]
    [ benchmarkMatrixRow Agda Newlines.generator defaultTimeout
    , benchmarkMatrixRow Idris Newlines.generator defaultTimeout
    , benchmarkMatrixRow Lean Newlines.generator defaultTimeout
    , benchmarkMatrixRow Rocq Newlines.generator defaultTimeout
    ]
  , BenchmarkMatrix "Parens" [2^n | (n :: Natural) <- [0..16]]
    [ benchmarkMatrixRow Agda Parens.generator defaultTimeout
    , benchmarkMatrixRow Idris Parens.generator defaultTimeout
    , benchmarkMatrixRow Lean Parens.generator defaultTimeout
    , benchmarkMatrixRow Rocq Parens.generator defaultTimeout
    ]
  , BenchmarkMatrix "Postulates" [2^n | (n :: Natural) <- [0..16]]
    [ benchmarkMatrixRow Agda Postulates.generator defaultTimeout
    , benchmarkMatrixRow Idris Postulates.generator defaultTimeout
    , benchmarkMatrixRow Lean Postulates.generator defaultTimeout
    , benchmarkMatrixRow Rocq Postulates.generator defaultTimeout
    ]
  , BenchmarkMatrix "RecordParameters" [2^n | (n :: Natural) <- [0..10]]
    [ benchmarkMatrixRow Agda RecordParameters.generator defaultTimeout
    , benchmarkMatrixRow Idris RecordParameters.generator defaultTimeout
    , benchmarkMatrixRow Lean RecordParameters.generator defaultTimeout
    , benchmarkMatrixRow Rocq RecordParameters.generator defaultTimeout
    ]
  , BenchmarkMatrix "SequentialDefinitions" [2^n | (n :: Natural) <- [0..12]]
    [ benchmarkMatrixRow Agda SequentialDefinitions.generator defaultTimeout
    , benchmarkMatrixRow Idris SequentialDefinitions.generator defaultTimeout
    , benchmarkMatrixRow Lean SequentialDefinitions.generator defaultTimeout
    , benchmarkMatrixRow Rocq SequentialDefinitions.generator defaultTimeout
    ]
  , BenchmarkMatrix "SequentialDependentRecords" [2^n | (n :: Natural) <- [0..10]]
    [ benchmarkMatrixRow Agda SequentialDependentRecords.generator defaultTimeout
    , benchmarkMatrixRow Idris SequentialDependentRecords.generator defaultTimeout
    , benchmarkMatrixRow Lean SequentialDependentRecords.generator defaultTimeout
    , benchmarkMatrixRow Rocq SequentialDependentRecords.generator defaultTimeout
    ]
  , BenchmarkMatrix "SequentialSimpleRecords" [2^n | (n :: Natural) <- [0..11]]
    [ benchmarkMatrixRow Agda SequentialSimpleRecords.generator defaultTimeout
    , benchmarkMatrixRow Idris SequentialSimpleRecords.generator defaultTimeout
    , benchmarkMatrixRow Lean SequentialSimpleRecords.generator defaultTimeout
    , benchmarkMatrixRow Rocq SequentialSimpleRecords.generator defaultTimeout
    ]
  , BenchmarkMatrix "SimpleDataDefinitions" [2^n | (n :: Natural) <- [0..12]]
    [ benchmarkMatrixRow Agda SimpleDataDefinitions.generator defaultTimeout
    , benchmarkMatrixRow Idris SimpleDataDefinitions.generator defaultTimeout
    , benchmarkMatrixRow Lean SimpleDataDefinitions.generator defaultTimeout
    , benchmarkMatrixRow Rocq SimpleDataDefinitions.generator defaultTimeout
    ]
  ]

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  needSite <- siteRules
  alternatives $ do
    "_build/site/index.html" %> \out -> do
      needSite out benchmarks
    "_build/site/*.html" %> \out -> do
      let name = dropExtension $ takeFileName out
      case find (\b -> benchmarkMatrixName b == name) benchmarks of
        Just bench -> needSite out [bench]
        Nothing -> fail $ "No registered benchmark for " <> name <> "."

  chezRules
  envRules
  gitRules
  makeRules
  opamRules

  agdaRules
  idrisRules
  leanRules
  rocqRules

  withTargetDocs "Remove all generated html files." $
    phony "clean-site" do
      removeFilesAfter "_build" ["site/*"]

  withTargetDocs "Remove all generated outputs and html files." $
    phony "clean" do
      need ["clean-site"]
      removeFilesAfter "_build" ["agda/*", "lean/*", "idris2/*", "rocq/*", "*.html"]

  withTargetDocs "Delete the entire _build directory, including the shake database." $
    phony "clean-everything" do
      liftIO $ removeDirectoryRecursive "_build"

  withTargetDocs "Delete the build store." $
    phony "clean-store" do
      liftIO $ removeDirectoryRecursive "_build/store"

  -- Development rules
  generateCBitsClangd
