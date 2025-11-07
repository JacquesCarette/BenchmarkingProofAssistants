module Main where

import Data.Default
import Data.Functor.Contravariant
import Data.Foldable
import Data.Word

import Development.Shake

import Numeric.Natural

import System.Directory

import Panbench.Grammar.Agda
import Panbench.Grammar.Idris
import Panbench.Grammar.Lean
import Panbench.Grammar.Rocq

import Panbench.Shake.Cabal
import Panbench.Shake.Chez
import Panbench.Shake.Dev
import Panbench.Shake.Env
import Panbench.Shake.HTML
import Panbench.Shake.Lang
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
import Panbench.Generator.IdChain qualified as IdChain
import Panbench.Generator.LargeDependentRecord qualified as LargeDependentRecord
import Panbench.Generator.LargeIndexedDatatype qualified as LargeIndexedDatatype
import Panbench.Generator.LargeIndexedParameterisedDatatype qualified as LargeIndexedParameterisedDatatype
import Panbench.Generator.LargeLambda qualified as LargeLambda
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

allBenchmarks
  :: Lang AgdaHeader AgdaDefns
  -> Lang IdrisHeader IdrisDefns
  -> Lang LeanHeader LeanDefns
  -> Lang RocqHeader RocqDefns
  -> [BenchmarkMatrix]
allBenchmarks agda idris lean rocq =
  [
    BenchmarkMatrix "Baseline" [0..10]
    [ BenchmarkMatrixRow agda Baseline.generator defaultTimeout
    , BenchmarkMatrixRow idris Baseline.generator defaultTimeout
    , BenchmarkMatrixRow lean Baseline.generator defaultTimeout
    , BenchmarkMatrixRow rocq Baseline.generator defaultTimeout
    ]
  , BenchmarkMatrix "ConversionAddition" [2^n | (n :: Natural) <- [0..8]]
    [ BenchmarkMatrixRow agda (contramap (100,) ConversionAddition.generator) defaultTimeout
    , BenchmarkMatrixRow idris (contramap (100,) ConversionAddition.generator) defaultTimeout
    , BenchmarkMatrixRow lean (contramap (100,) ConversionAddition.generator) defaultTimeout
    , BenchmarkMatrixRow rocq (contramap (100,) ConversionAddition.generator) defaultTimeout
    ]
  , BenchmarkMatrix "DatatypeParameters" [2^n | (n :: Natural) <- [0..10]]
    [ BenchmarkMatrixRow agda DatatypeParameters.generator defaultTimeout
    , BenchmarkMatrixRow idris DatatypeParameters.generator defaultTimeout
    , BenchmarkMatrixRow lean DatatypeParameters.generator defaultTimeout
    , BenchmarkMatrixRow rocq DatatypeParameters.generator defaultTimeout
    ]
  , BenchmarkMatrix "IdChain" [2^n | (n :: Natural) <- [0..8]]
    [ BenchmarkMatrixRow agda IdChain.generator defaultTimeout
    , BenchmarkMatrixRow idris IdChain.generator defaultTimeout
    , BenchmarkMatrixRow lean IdChain.generator defaultTimeout
    , BenchmarkMatrixRow rocq IdChain.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeDependentRecord" [2^n | (n :: Natural) <- [0..8]]
    [ BenchmarkMatrixRow agda LargeDependentRecord.generator defaultTimeout
    , BenchmarkMatrixRow idris LargeDependentRecord.generator defaultTimeout
    , BenchmarkMatrixRow lean LargeDependentRecord.generator defaultTimeout
    , BenchmarkMatrixRow rocq LargeDependentRecord.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeIndexedDatatype" [2^n | (n :: Natural) <- [0..8]]
    [ BenchmarkMatrixRow agda LargeIndexedDatatype.generator defaultTimeout
    , BenchmarkMatrixRow idris LargeIndexedDatatype.generator defaultTimeout
    , BenchmarkMatrixRow lean LargeIndexedDatatype.generator defaultTimeout
    , BenchmarkMatrixRow rocq LargeIndexedDatatype.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeIndexedParameterisedDatatype" [2^n | (n :: Natural) <- [0..8]]
    [ BenchmarkMatrixRow agda LargeIndexedParameterisedDatatype.generator defaultTimeout
    , BenchmarkMatrixRow idris LargeIndexedParameterisedDatatype.generator defaultTimeout
    , BenchmarkMatrixRow lean LargeIndexedParameterisedDatatype.generator defaultTimeout
    , BenchmarkMatrixRow rocq LargeIndexedParameterisedDatatype.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeLambda" [2^n | (n :: Natural) <- [0..11]]
    [ BenchmarkMatrixRow agda LargeLambda.generator defaultTimeout
    , BenchmarkMatrixRow idris LargeLambda.generator defaultTimeout
    , BenchmarkMatrixRow lean LargeLambda.generator defaultTimeout
    , BenchmarkMatrixRow rocq LargeLambda.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeSimpleDatatype" [2^n | (n :: Natural) <- [0..11]]
    [ BenchmarkMatrixRow agda LargeSimpleDatatype.generator defaultTimeout
    , BenchmarkMatrixRow idris LargeSimpleDatatype.generator defaultTimeout
    , BenchmarkMatrixRow lean LargeSimpleDatatype.generator defaultTimeout
    , BenchmarkMatrixRow rocq LargeSimpleDatatype.generator defaultTimeout
    ]
  , BenchmarkMatrix "LargeSimpleRecord" [2^n | (n :: Natural) <- [0..8]]
    [ BenchmarkMatrixRow agda LargeSimpleRecord.generator defaultTimeout
    , BenchmarkMatrixRow idris LargeSimpleRecord.generator defaultTimeout
    , BenchmarkMatrixRow lean LargeSimpleRecord.generator defaultTimeout
    , BenchmarkMatrixRow rocq LargeSimpleRecord.generator defaultTimeout
    ]
  , BenchmarkMatrix "NestedLet" [2^n | (n :: Natural) <- [0..10]]
    [ BenchmarkMatrixRow agda NestedLet.generator defaultTimeout
    , BenchmarkMatrixRow idris NestedLet.generator defaultTimeout
    , BenchmarkMatrixRow lean NestedLet.generator defaultTimeout
    , BenchmarkMatrixRow rocq NestedLet.generator defaultTimeout
    ]
  , BenchmarkMatrix "NestedLetAdditions" [2^n | (n :: Natural) <- [0..8]]
    [ BenchmarkMatrixRow agda NestedLetAdditions.generator defaultTimeout
    , BenchmarkMatrixRow idris NestedLetAdditions.generator defaultTimeout
    , BenchmarkMatrixRow lean NestedLetAdditions.generator defaultTimeout
    , BenchmarkMatrixRow rocq NestedLetAdditions.generator defaultTimeout
    ]
  , BenchmarkMatrix "NestedLetFunctions" [2^n | (n :: Natural) <- [0..8]]
    [ BenchmarkMatrixRow agda NestedLetFunctions.generator defaultTimeout
    , BenchmarkMatrixRow idris NestedLetFunctions.generator defaultTimeout
    , BenchmarkMatrixRow lean NestedLetFunctions.generator defaultTimeout
    , BenchmarkMatrixRow rocq NestedLetFunctions.generator defaultTimeout
    ]
  , BenchmarkMatrix "Newlines" [10^n | (n :: Natural) <- [0..7]]
    [ BenchmarkMatrixRow agda Newlines.generator defaultTimeout
    , BenchmarkMatrixRow idris Newlines.generator defaultTimeout
    , BenchmarkMatrixRow lean Newlines.generator defaultTimeout
    , BenchmarkMatrixRow rocq Newlines.generator defaultTimeout
    ]
  , BenchmarkMatrix "Parens" [2^n | (n :: Natural) <- [0..16]]
    [ BenchmarkMatrixRow agda Parens.generator defaultTimeout
    , BenchmarkMatrixRow idris Parens.generator defaultTimeout
    , BenchmarkMatrixRow lean Parens.generator defaultTimeout
    , BenchmarkMatrixRow rocq Parens.generator defaultTimeout
    ]
  , BenchmarkMatrix "Postulates" [2^n | (n :: Natural) <- [0..16]]
    [ BenchmarkMatrixRow agda Postulates.generator defaultTimeout
    , BenchmarkMatrixRow idris Postulates.generator defaultTimeout
    , BenchmarkMatrixRow lean Postulates.generator defaultTimeout
    , BenchmarkMatrixRow rocq Postulates.generator defaultTimeout
    ]
  , BenchmarkMatrix "RecordParameters" [2^n | (n :: Natural) <- [0..10]]
    [ BenchmarkMatrixRow agda RecordParameters.generator defaultTimeout
    , BenchmarkMatrixRow idris RecordParameters.generator defaultTimeout
    , BenchmarkMatrixRow lean RecordParameters.generator defaultTimeout
    , BenchmarkMatrixRow rocq RecordParameters.generator defaultTimeout
    ]
  , BenchmarkMatrix "SequentialDefinitions" [2^n | (n :: Natural) <- [0..12]]
    [ BenchmarkMatrixRow agda SequentialDefinitions.generator defaultTimeout
    , BenchmarkMatrixRow idris SequentialDefinitions.generator defaultTimeout
    , BenchmarkMatrixRow lean SequentialDefinitions.generator defaultTimeout
    , BenchmarkMatrixRow rocq SequentialDefinitions.generator defaultTimeout
    ]
  , BenchmarkMatrix "SequentialDependentRecords" [2^n | (n :: Natural) <- [0..10]]
    [ BenchmarkMatrixRow agda SequentialDependentRecords.generator defaultTimeout
    , BenchmarkMatrixRow idris SequentialDependentRecords.generator defaultTimeout
    , BenchmarkMatrixRow lean SequentialDependentRecords.generator defaultTimeout
    , BenchmarkMatrixRow rocq SequentialDependentRecords.generator defaultTimeout
    ]
  , BenchmarkMatrix "SequentialSimpleRecords" [2^n | (n :: Natural) <- [0..11]]
    [ BenchmarkMatrixRow agda SequentialSimpleRecords.generator defaultTimeout
    , BenchmarkMatrixRow idris SequentialSimpleRecords.generator defaultTimeout
    , BenchmarkMatrixRow lean SequentialSimpleRecords.generator defaultTimeout
    , BenchmarkMatrixRow rocq SequentialSimpleRecords.generator defaultTimeout
    ]
  , BenchmarkMatrix "SimpleDataDefinitions" [2^n | (n :: Natural) <- [0..12]]
    [ BenchmarkMatrixRow agda SimpleDataDefinitions.generator defaultTimeout
    , BenchmarkMatrixRow idris SimpleDataDefinitions.generator defaultTimeout
    , BenchmarkMatrixRow lean SimpleDataDefinitions.generator defaultTimeout
    , BenchmarkMatrixRow rocq SimpleDataDefinitions.generator defaultTimeout
    ]
  ]

withProofAssistants
  :: (Lang AgdaHeader AgdaDefns
     -> Lang IdrisHeader IdrisDefns
     -> Lang LeanHeader LeanDefns
     -> Lang RocqHeader RocqDefns
     -> Action a)
  -> Action a
withProofAssistants k = do
  agda <- needAgda "agda" def $ AgdaQ
    { agdaInstallRev = "v2.8.0"
    , agdaInstallFlags = defaultAgdaInstallFlags
    , agdaHackageIndex = "2025-12-27T16:49:34Z"
    }
  idris <- needIdris "idris2" $ IdrisQ
    { idrisInstallRev = "v0.7.0"
    , idrisInstallScheme = Chez
    }
  lean <- needLean "lean" $ LeanQ
    { leanInstallRev = "v4.21.0"
    , leanCMakeFlags = defaultLeanCMakeFlags
    , leanMakeFlags = defaultLeanMakeFlags
    }
  rocq <- needRocq "rocq" def $ RocqQ
    { rocqInstallRev = "V9.0.0"
    , rocqOcamlCompiler = defaultRocqOcamlCompiler
    }
  k agda idris lean rocq

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  needSite <- siteRules

  cabalRules
  chezRules
  envRules
  makeRules
  opamRules

  agdaRules
  idrisRules
  leanRules
  rocqRules

  "_build/site/index.html" %> \out ->
    withProofAssistants \agda idris lean rocq ->
      needSite out (allBenchmarks agda idris lean rocq)

  withTargetDocs "Generate all benchmarking modules" $
    phony "generate-modules" do
      withProofAssistants \agda idris lean rocq ->
        traverse_ setupBenchmarkingMatrix (allBenchmarks agda idris lean rocq)

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
