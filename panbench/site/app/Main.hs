{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NumDecimals #-}
module Main where

import Data.Word

import Development.Shake

import Numeric.Natural

import System.Directory

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
    [ benchmarkMatrixRow (Agda String) Baseline.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) Baseline.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) Baseline.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) Baseline.generator defaultTimeout
    ]
  , BenchmarkMatrix "DatatypeParameters" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) DatatypeParameters.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) DatatypeParameters.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) DatatypeParameters.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) DatatypeParameters.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeDependentRecord" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) LargeDependentRecord.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) LargeDependentRecord.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) LargeDependentRecord.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) LargeDependentRecord.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeIndexedDatatype" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) LargeIndexedDatatype.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) LargeIndexedDatatype.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) LargeIndexedDatatype.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) LargeIndexedDatatype.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeIndexedParameterisedDatatype" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) LargeIndexedParameterisedDatatype.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) LargeIndexedParameterisedDatatype.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) LargeIndexedParameterisedDatatype.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) LargeIndexedParameterisedDatatype.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeSimpleDatatype" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) LargeSimpleDatatype.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) LargeSimpleDatatype.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) LargeSimpleDatatype.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) LargeSimpleDatatype.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeSimpleRecord" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) LargeSimpleRecord.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) LargeSimpleRecord.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) LargeSimpleRecord.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) LargeSimpleRecord.generator defaultTimeout
    ]
  , BenchmarkMatrix "NestedLet" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) NestedLet.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) NestedLet.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) NestedLet.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) NestedLet.generator defaultTimeout
    ]
  , BenchmarkMatrix "NestedLetAdditions" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) NestedLetAdditions.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) NestedLetAdditions.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) NestedLetAdditions.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) NestedLetAdditions.generator defaultTimeout
    ]
  , BenchmarkMatrix "NestedLetFunctions" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) NestedLetFunctions.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) NestedLetFunctions.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) NestedLetFunctions.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) NestedLetFunctions.generator defaultTimeout
    ]
  , BenchmarkMatrix "Newlines" [10^n | (n :: Natural) <- [0..7]]
    [ benchmarkMatrixRow (Agda String) Newlines.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) Newlines.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) Newlines.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) Newlines.generator defaultTimeout
    ]
  , BenchmarkMatrix "RecordParameters" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) RecordParameters.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) RecordParameters.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) RecordParameters.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) RecordParameters.generator defaultTimeout
    ]
  , BenchmarkMatrix "SequentialDefinitions" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) SequentialDefinitions.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) SequentialDefinitions.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) SequentialDefinitions.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) SequentialDefinitions.generator defaultTimeout
    ]
  , BenchmarkMatrix "SequentialDependentRecords" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) SequentialDependentRecords.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) SequentialDependentRecords.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) SequentialDependentRecords.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) SequentialDependentRecords.generator defaultTimeout
    ]
  , BenchmarkMatrix "SequentialSimpleRecords" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) SequentialSimpleRecords.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) SequentialSimpleRecords.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) SequentialSimpleRecords.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) SequentialSimpleRecords.generator defaultTimeout
    ]
  , BenchmarkMatrix "SimpleDataDefinitions" [2^n | (n :: Natural) <- [0..8]]
    [ benchmarkMatrixRow (Agda String) SimpleDataDefinitions.generator defaultTimeout
    , benchmarkMatrixRow (Idris String) SimpleDataDefinitions.generator defaultTimeout
    , benchmarkMatrixRow (Lean String) SimpleDataDefinitions.generator defaultTimeout
    , benchmarkMatrixRow (Rocq String) SimpleDataDefinitions.generator defaultTimeout
    ]
  ]

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  needSite <- siteRules
  "_build/site/index.html" %> \out -> do
    needSite out benchmarks

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
