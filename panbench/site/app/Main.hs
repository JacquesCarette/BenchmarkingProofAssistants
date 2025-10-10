{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Main where

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

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  needSite <- siteRules
  "_build/site/index.html" %> \out -> do
    needSite out
      [ BenchmarkMatrix "DatatypeParameters" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) DatatypeParameters.generator
        , benchmarkMatrixRow (Idris String) DatatypeParameters.generator
        , benchmarkMatrixRow (Lean String) DatatypeParameters.generator
        , benchmarkMatrixRow (Rocq String) DatatypeParameters.generator
        ]
      , BenchmarkMatrix "LargeDependentRecord" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeDependentRecord.generator
        , benchmarkMatrixRow (Idris String) LargeDependentRecord.generator
        , benchmarkMatrixRow (Lean String) LargeDependentRecord.generator
        , benchmarkMatrixRow (Rocq String) LargeDependentRecord.generator
        ]
      , BenchmarkMatrix "LargeIndexedDatatype" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeIndexedDatatype.generator
        , benchmarkMatrixRow (Idris String) LargeIndexedDatatype.generator
        , benchmarkMatrixRow (Lean String) LargeIndexedDatatype.generator
        , benchmarkMatrixRow (Rocq String) LargeIndexedDatatype.generator
        ]
      , BenchmarkMatrix "LargeIndexedParameterisedDatatype" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeIndexedParameterisedDatatype.generator
        , benchmarkMatrixRow (Idris String) LargeIndexedParameterisedDatatype.generator
        , benchmarkMatrixRow (Lean String) LargeIndexedParameterisedDatatype.generator
        , benchmarkMatrixRow (Rocq String) LargeIndexedParameterisedDatatype.generator
        ]
      , BenchmarkMatrix "LargeSimpleDatatype" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeSimpleDatatype.generator
        , benchmarkMatrixRow (Idris String) LargeSimpleDatatype.generator
        , benchmarkMatrixRow (Lean String) LargeSimpleDatatype.generator
        , benchmarkMatrixRow (Rocq String) LargeSimpleDatatype.generator
        ]
      , BenchmarkMatrix "LargeSimpleRecord" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) LargeSimpleRecord.generator
        , benchmarkMatrixRow (Idris String) LargeSimpleRecord.generator
        , benchmarkMatrixRow (Lean String) LargeSimpleRecord.generator
        , benchmarkMatrixRow (Rocq String) LargeSimpleRecord.generator
        ]
      , BenchmarkMatrix "NestedLet" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) NestedLet.generator
        , benchmarkMatrixRow (Idris String) NestedLet.generator
        , benchmarkMatrixRow (Lean String) NestedLet.generator
        , benchmarkMatrixRow (Rocq String) NestedLet.generator
        ]
      , BenchmarkMatrix "NestedLetAdditions" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) NestedLetAdditions.generator
        , benchmarkMatrixRow (Idris String) NestedLetAdditions.generator
        , benchmarkMatrixRow (Lean String) NestedLetAdditions.generator
        , benchmarkMatrixRow (Rocq String) NestedLetAdditions.generator
        ]
      , BenchmarkMatrix "NestedLetFunctions" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) NestedLetFunctions.generator
        , benchmarkMatrixRow (Idris String) NestedLetFunctions.generator
        , benchmarkMatrixRow (Lean String) NestedLetFunctions.generator
        , benchmarkMatrixRow (Rocq String) NestedLetFunctions.generator
        ]
      , BenchmarkMatrix "Newlines" [10^n | (n :: Natural) <- [0..7]]
        [ benchmarkMatrixRow (Agda String) Newlines.generator
        , benchmarkMatrixRow (Idris String) Newlines.generator
        , benchmarkMatrixRow (Lean String) Newlines.generator
        , benchmarkMatrixRow (Rocq String) Newlines.generator
        ]
      , BenchmarkMatrix "RecordParameters" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) RecordParameters.generator
        , benchmarkMatrixRow (Idris String) RecordParameters.generator
        , benchmarkMatrixRow (Lean String) RecordParameters.generator
        , benchmarkMatrixRow (Rocq String) RecordParameters.generator
        ]
      , BenchmarkMatrix "SequentialDefinitions" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) SequentialDefinitions.generator
        , benchmarkMatrixRow (Idris String) SequentialDefinitions.generator
        , benchmarkMatrixRow (Lean String) SequentialDefinitions.generator
        , benchmarkMatrixRow (Rocq String) SequentialDefinitions.generator
        ]
      , BenchmarkMatrix "SequentialDependentRecords" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) SequentialDependentRecords.generator
        , benchmarkMatrixRow (Idris String) SequentialDependentRecords.generator
        , benchmarkMatrixRow (Lean String) SequentialDependentRecords.generator
        , benchmarkMatrixRow (Rocq String) SequentialDependentRecords.generator
        ]
      , BenchmarkMatrix "SequentialSimpleRecords" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) SequentialSimpleRecords.generator
        , benchmarkMatrixRow (Idris String) SequentialSimpleRecords.generator
        , benchmarkMatrixRow (Lean String) SequentialSimpleRecords.generator
        , benchmarkMatrixRow (Rocq String) SequentialSimpleRecords.generator
        ]
      , BenchmarkMatrix "SimpleDataDefinitions" [2^n | (n :: Natural) <- [0..8]]
        [ benchmarkMatrixRow (Agda String) SimpleDataDefinitions.generator
        , benchmarkMatrixRow (Idris String) SimpleDataDefinitions.generator
        , benchmarkMatrixRow (Lean String) SimpleDataDefinitions.generator
        , benchmarkMatrixRow (Rocq String) SimpleDataDefinitions.generator
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
