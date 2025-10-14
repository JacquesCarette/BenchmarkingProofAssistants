{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NumDecimals #-}
module Main where

import Development.Shake

import Numeric.Natural

import System.Directory

import Panbench.Grammar.Agda
import Panbench.Grammar.Idris
import Panbench.Grammar.Lean
import Panbench.Grammar.Rocq

import Panbench.Shake.Benchmark
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

defaultLimits :: [ResourceLimit]
defaultLimits =
  [ CPUTime (2 * 60) -- 2 Minutes
  , MaxRSS 3.0e9 -- 3 Gigabytes
  ]

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  needSite <- siteRules
  "_build/site/index.html" %> \out -> do
    needSite out
      [ BenchmarkMatrix "DatatypeParameters" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) DatatypeParameters.generator defaultLimits
        , benchmarkMatrixRow (Idris String) DatatypeParameters.generator defaultLimits
        , benchmarkMatrixRow (Lean String) DatatypeParameters.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) DatatypeParameters.generator defaultLimits
        ]
      , BenchmarkMatrix "LargeDependentRecord" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeDependentRecord.generator defaultLimits
        , benchmarkMatrixRow (Idris String) LargeDependentRecord.generator defaultLimits
        , benchmarkMatrixRow (Lean String) LargeDependentRecord.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) LargeDependentRecord.generator defaultLimits
        ]
      , BenchmarkMatrix "LargeIndexedDatatype" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeIndexedDatatype.generator defaultLimits
        , benchmarkMatrixRow (Idris String) LargeIndexedDatatype.generator defaultLimits
        , benchmarkMatrixRow (Lean String) LargeIndexedDatatype.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) LargeIndexedDatatype.generator defaultLimits
        ]
      , BenchmarkMatrix "LargeIndexedParameterisedDatatype" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeIndexedParameterisedDatatype.generator defaultLimits
        , benchmarkMatrixRow (Idris String) LargeIndexedParameterisedDatatype.generator defaultLimits
        , benchmarkMatrixRow (Lean String) LargeIndexedParameterisedDatatype.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) LargeIndexedParameterisedDatatype.generator defaultLimits
        ]
      , BenchmarkMatrix "LargeSimpleDatatype" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeSimpleDatatype.generator defaultLimits
        , benchmarkMatrixRow (Idris String) LargeSimpleDatatype.generator defaultLimits
        , benchmarkMatrixRow (Lean String) LargeSimpleDatatype.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) LargeSimpleDatatype.generator defaultLimits
        ]
      , BenchmarkMatrix "LargeSimpleRecord" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeSimpleRecord.generator defaultLimits
        , benchmarkMatrixRow (Idris String) LargeSimpleRecord.generator defaultLimits
        , benchmarkMatrixRow (Lean String) LargeSimpleRecord.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) LargeSimpleRecord.generator defaultLimits
        ]
      , BenchmarkMatrix "NestedLet" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) NestedLet.generator defaultLimits
        , benchmarkMatrixRow (Idris String) NestedLet.generator defaultLimits
        , benchmarkMatrixRow (Lean String) NestedLet.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) NestedLet.generator defaultLimits
        ]
      , BenchmarkMatrix "NestedLetAdditions" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) NestedLetAdditions.generator defaultLimits
        , benchmarkMatrixRow (Idris String) NestedLetAdditions.generator defaultLimits
        , benchmarkMatrixRow (Lean String) NestedLetAdditions.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) NestedLetAdditions.generator defaultLimits
        ]
      , BenchmarkMatrix "NestedLetFunctions" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) NestedLetFunctions.generator defaultLimits
        , benchmarkMatrixRow (Idris String) NestedLetFunctions.generator defaultLimits
        , benchmarkMatrixRow (Lean String) NestedLetFunctions.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) NestedLetFunctions.generator defaultLimits
        ]
      , BenchmarkMatrix "Newlines" [10^n | (n :: Natural) <- [0..7]]
        [ benchmarkMatrixRow (Agda String) Newlines.generator defaultLimits
        , benchmarkMatrixRow (Idris String) Newlines.generator defaultLimits
        , benchmarkMatrixRow (Lean String) Newlines.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) Newlines.generator defaultLimits
        ]
      , BenchmarkMatrix "RecordParameters" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) RecordParameters.generator defaultLimits
        , benchmarkMatrixRow (Idris String) RecordParameters.generator defaultLimits
        , benchmarkMatrixRow (Lean String) RecordParameters.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) RecordParameters.generator defaultLimits
        ]
      , BenchmarkMatrix "SequentialDefinitions" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) SequentialDefinitions.generator defaultLimits
        , benchmarkMatrixRow (Idris String) SequentialDefinitions.generator defaultLimits
        , benchmarkMatrixRow (Lean String) SequentialDefinitions.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) SequentialDefinitions.generator defaultLimits
        ]
      , BenchmarkMatrix "SequentialDependentRecords" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) SequentialDependentRecords.generator defaultLimits
        , benchmarkMatrixRow (Idris String) SequentialDependentRecords.generator defaultLimits
        , benchmarkMatrixRow (Lean String) SequentialDependentRecords.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) SequentialDependentRecords.generator defaultLimits
        ]
      , BenchmarkMatrix "SequentialSimpleRecords" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) SequentialSimpleRecords.generator defaultLimits
        , benchmarkMatrixRow (Idris String) SequentialSimpleRecords.generator defaultLimits
        , benchmarkMatrixRow (Lean String) SequentialSimpleRecords.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) SequentialSimpleRecords.generator defaultLimits
        ]
      , BenchmarkMatrix "SimpleDataDefinitions" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) SimpleDataDefinitions.generator defaultLimits
        , benchmarkMatrixRow (Idris String) SimpleDataDefinitions.generator defaultLimits
        , benchmarkMatrixRow (Lean String) SimpleDataDefinitions.generator defaultLimits
        , benchmarkMatrixRow (Rocq String) SimpleDataDefinitions.generator defaultLimits
        ]
      ]

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
