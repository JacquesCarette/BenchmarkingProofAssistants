{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-type-defaults #-}
module Main where

import Data.Default
import Data.Foldable
import Data.Functor
import Data.Functor.Contravariant
import Data.Map.Lazy qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
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

import Panbench.Generator
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

import Panbench.Generator.LongName.Datatype qualified as LongNameDatatype
import Panbench.Generator.LongName.DatatypeConstructor qualified as LongNameDatatypeConstructor
import Panbench.Generator.LongName.Definition qualified as LongNameDefinition
import Panbench.Generator.LongName.DefinitionLhs qualified as LongNameDefinitionLhs
import Panbench.Generator.LongName.DefinitionRhs qualified as LongNameDefinitionRhs
import Panbench.Generator.LongName.Lambda qualified as LongNameLambda
import Panbench.Generator.LongName.Pi qualified as LongNamePi
import Panbench.Generator.LongName.Record qualified as LongNameRecord
import Panbench.Generator.LongName.RecordConstructor qualified as LongNameRecordConstructor
import Panbench.Generator.LongName.RecordField qualified as LongNameRecordField

import Panbench.Generator.ManyImplicits qualified as ManyImplicits
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

allGenerators :: _ => [(GenModule hdr defns Natural, [Natural])]
allGenerators =
  [ (Baseline.generator, [0..10])
  , (contramap (100,) ConversionAddition.generator, [2^n | n <- [0..8]])
  , (DatatypeParameters.generator, [2^n | n <- [0..8]])
  , (IdChain.generator, [2^n | n <- [0..5]])
  , (LargeDependentRecord.generator, [2^n | n <- [0..8]])
  , (LargeIndexedDatatype.generator, [2^n | n <- [0..8]])
  , (LargeIndexedParameterisedDatatype.generator, [2^n | n <- [0..8]])
  , (LargeLambda.generator, [2^n | n <- [0..11]])
  , (LargeSimpleDatatype.generator, [2^n | n <- [0..11]])
  , (LargeSimpleRecord.generator, [2^n | n <- [0..8]])
  , (ManyImplicits.generator, [2^n | n <- [0..10]])
  , (NestedLet.generator, [2^n | n <- [0..10]])
  , (NestedLetAdditions.generator, [2^n | n <- [0..8]])
  , (NestedLetFunctions.generator, [2^n | n <- [0..8]])
  , (Newlines.generator, [10^n | n <- [0..7]])
  , (Parens.generator, [2^n | n <- [0..16]])
  , (Postulates.generator, [2^n | n <- [0..16]])
  , (RecordParameters.generator, [2^n | n <- [0..10]])
  , (SequentialDefinitions.generator, [2^n | n <- [0..12]])
  , (SequentialDependentRecords.generator, [2^n | n <- [0..10]])
  , (SequentialSimpleRecords.generator, [2^n | n <- [0..11]])
  , (SimpleDataDefinitions.generator, [2^n | n <- [0..12]])
  ]

longNameGenerators :: _ => [(GenModule hdr defns Natural, [Natural])]
longNameGenerators =
  [ (LongNameDatatype.generator, [2^n | n <- [0..22]])
  , (LongNameDatatypeConstructor.generator, [2^n | n <- [0..22]])
  , (LongNameDefinition.generator, [2^n | n <- [0..22]])
  , (LongNameDefinitionLhs.generator, [2^n | n <- [0..22]])
  , (LongNameDefinitionRhs.generator, [2^n | n <- [0..22]])
  , (LongNameLambda.generator, [2^n | n <- [0..22]])
  , (LongNamePi.generator, [2^n | n <- [0..22]])
  , (LongNameRecord.generator, [2^n | n <- [0..22]])
  , (LongNameRecordConstructor.generator, [2^n | n <- [0..22]])
  , (LongNameRecordField.generator, [2^n | n <- [0..22]])
  ]

langBenchmark
  :: Lang hdr defns
  -> Word64
  -> [(GenModule hdr defns Natural, [Natural])]
  -> [(Text, BenchmarkMatrixRow)]
langBenchmark lang timeout generators =
  generators <&> \(gen, sizes) -> (genName gen, BenchmarkMatrixRow lang gen sizes timeout)

makeBenchmarkSuite
  :: [[(Text, BenchmarkMatrixRow)]]
  -> [BenchmarkMatrix]
makeBenchmarkSuite rows =
  let rowGroups = foldl' (\acc (nm, row) -> Map.insertWith (++) nm [row] acc) Map.empty $ concat rows
  in Map.toList rowGroups <&> \(name, rows) ->
    BenchmarkMatrix (T.unpack name) rows

withProofAssistants
  :: ((AgdaOpts -> Lang AgdaHeader AgdaDefns)
     -> Lang IdrisHeader IdrisDefns
     -> (LeanOpts -> Lang LeanHeader LeanDefns)
     -> (RocqOpts -> Lang RocqHeader RocqDefns)
     -> Action a)
  -> Action a
withProofAssistants k = do
  agda <- needAgda "agda" AgdaQ
    { agdaInstallRev = "v2.8.0"
    , agdaInstallFlags = defaultAgdaInstallFlags
    , agdaHackageIndex = "2025-12-27T16:49:34Z"
    }
  idris <- needIdris "idris2" IdrisQ
    { idrisInstallRev = "v0.7.0"
    , idrisInstallScheme = Chez
    }
  lean <- needLean "lean" LeanQ
    { leanInstallRev = "v4.21.0"
    , leanCMakeFlags = defaultLeanCMakeFlags
    , leanMakeFlags = defaultLeanMakeFlags
    }
  rocq <- needRocq "rocq" RocqQ
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
      let timeout = 60
      in needSite out $ makeBenchmarkSuite
        [ langBenchmark (agda def) timeout allGenerators
        , langBenchmark idris timeout allGenerators
        , langBenchmark (lean def) timeout allGenerators
        , langBenchmark (rocq def) timeout allGenerators
        ]

  "_build/site/agdas.html" %> \out -> do
      let timeout = 60
      let hackageIndex = "2025-12-27T16:49:34Z"
      agda28 <- needAgda "agda-2.8.0" AgdaQ
        { agdaInstallRev = "v2.8.0"
        , agdaInstallFlags = defaultAgdaInstallFlags
        , agdaHackageIndex = hackageIndex
        }
      agdaMaster <- needAgda "agda-master" AgdaQ
        { agdaInstallRev = "5891655fe8c2783a7951f649682e3e92a191df90"
        , agdaInstallFlags = defaultAgdaInstallFlags
        , agdaHackageIndex = hackageIndex
        }
      needSite out $ makeBenchmarkSuite
        [ langBenchmark (agda28 def) timeout allGenerators
        , langBenchmark (agdaMaster def) timeout allGenerators
        , langBenchmark (agdaMaster (def { agdaFlagsOpt = ["--cubical"] })) timeout allGenerators
        ]

  "_build/site/long-names.html" %> \out ->
    withProofAssistants \agda idris lean rocq ->
      let timeout = 60
      in needSite out $ makeBenchmarkSuite
        [ langBenchmark (agda def) timeout longNameGenerators
        , langBenchmark idris timeout longNameGenerators
        , langBenchmark (lean $ def { leanSetOpts = [("linter.unusedVariables", "false")] }) timeout longNameGenerators
        , langBenchmark (rocq def) timeout longNameGenerators
        ]
  withTargetDocs "Generate all benchmarking modules" $
    phony "generate-modules" do
      withProofAssistants \agda idris lean rocq ->
        -- [FIXME: Reed M, 29/01/2026] We are in a better place to not require tools installs
        -- here, but this is not a priority.
        let timeout = 60
        in traverse_ setupBenchmarkingMatrix $ makeBenchmarkSuite
        [ langBenchmark (agda def) timeout allGenerators
        , langBenchmark idris timeout allGenerators
        , langBenchmark (lean def) timeout allGenerators
        , langBenchmark (rocq def) timeout allGenerators
        ]

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
