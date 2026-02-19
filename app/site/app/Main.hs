{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
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

import Panbench.Grammar.Agda
import Panbench.Grammar.Idris
import Panbench.Grammar.Lean
import Panbench.Grammar.Rocq

import Panbench.Shake.Cabal
import Panbench.Shake.Chez
import Panbench.Shake.Dev
import Panbench.Shake.Env
import Panbench.Shake.File
import Panbench.Shake.HTML
import Panbench.Shake.Lang
import Panbench.Shake.Lang.Agda
import Panbench.Shake.Lang.Idris
import Panbench.Shake.Lang.Lean
import Panbench.Shake.Lang.Rocq
import Panbench.Shake.Make
import Panbench.Shake.Matrix
import Panbench.Shake.Opam
import Panbench.Shake.Path
import Panbench.Shake.Plot.Pgf
import Panbench.Shake.Range

import Panbench.Generator
import Panbench.Generator.Conversion.Addition qualified as ConversionAddition
import Panbench.Generator.DatatypeParameters qualified as DatatypeParameters
import Panbench.Generator.Empty qualified as Baseline
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

import Panbench.Generator.Record.Telescope qualified as RecordTelescope

import Panbench.Generator.Unification.IdChain qualified as IdChain
import Panbench.Generator.Unification.IdChainLambda qualified as IdChainLambda

allGenerators :: _ => [(GenModule hdr defns Natural, Range)]
allGenerators =
  [ (Baseline.generator, Range (Linear 1) 0 10)
  , (contramap (100,) ConversionAddition.generator, Range (Log 2) 0 13)
  , (DatatypeParameters.generator, Range (Log 2) 0 13)
  , (IdChain.generator, Range (Log 2) 0 6)
  , (IdChainLambda.generator, Range (Log 2) 0 6)
  , (LargeDependentRecord.generator, Range (Log 2) 0 9)
  , (LargeIndexedDatatype.generator, Range (Log 2) 0 9)
  , (LargeIndexedParameterisedDatatype.generator, Range (Log 2) 0 9)
  , (LargeLambda.generator, Range (Log 2) 0 12)
  , (LargeSimpleDatatype.generator, Range (Log 2) 0 12)
  , (LargeSimpleRecord.generator, Range (Log 2) 0 9)
  , (ManyImplicits.generator, Range (Log 2) 0 11)
  , (NestedLet.generator, Range (Log 2) 0 11)
  , (NestedLetAdditions.generator, Range (Log 2) 0 9)
  , (NestedLetFunctions.generator, Range (Log 2) 0 9)
  , (Newlines.generator, Range (Log 10) 0 7)
  , (Parens.generator, Range (Log 2) 0 17)
  , (Postulates.generator, Range (Log 2) 0 17)
  , (RecordParameters.generator, Range (Log 2) 0 11)
  , (SequentialDefinitions.generator, Range (Log 2) 0 13)
  , (SequentialDependentRecords.generator, Range (Log 2) 0 11)
  , (SequentialSimpleRecords.generator, Range (Log 2) 0 12)
  , (SimpleDataDefinitions.generator, Range (Log 2) 0 13)
  ]

getGenerator :: Text -> [(GenModule hdr defns Natural, Range)] -> Action (GenModule hdr defns Natural, Range)
getGenerator name gens =
  case find (\(gen, _) -> genName gen == name) gens of
    Just gen -> pure gen
    Nothing -> fail $ unlines $
      "Could not find the generator '" <> T.unpack name <> "' among the generators:"
      : fmap (T.unpack . genName . fst) gens

linearSuite :: _ => [(GenModule hdr defns Natural, Range)]
linearSuite =
  [ (IdChain.generator, interval (Linear 2) 8 32)
  , (IdChainLambda.generator, interval (Linear 2) 8 32)
  , (NestedLetAdditions.generator, interval (Linear 1) 16 32)
  ]

longNameGenerators :: _ => [(GenModule hdr defns Natural, Range)]
longNameGenerators =
  [ (LongNameDatatype.generator, Range (Log 2) 0 23)
  , (LongNameDatatypeConstructor.generator, Range (Log 2) 0 23)
  , (LongNameDefinition.generator, Range (Log 2) 0 23)
  , (LongNameDefinitionLhs.generator, Range (Log 2) 0 23)
  , (LongNameDefinitionRhs.generator, Range (Log 2) 0 23)
  , (LongNameLambda.generator, Range (Log 2) 0 23)
  , (LongNamePi.generator, Range (Log 2) 0 23)
  , (LongNameRecord.generator, Range (Log 2) 0 23)
  , (LongNameRecordConstructor.generator, Range (Log 2) 0 23)
  , (LongNameRecordField.generator, Range (Log 2) 0 23)
  ]

langBenchmark
  :: Lang hdr defns
  -> Word64
  -> [(GenModule hdr defns Natural, Range)]
  -> [(Text, Scale, BenchmarkMatrixRow)]
langBenchmark lang timeout generators =
  generators <&> \(gen, range) -> (genName gen, rangeScale range, BenchmarkMatrixRow lang gen (rangeStart range) (rangeSamples range) timeout)

makeBenchmarkSuite
  :: [[(Text, Scale, BenchmarkMatrixRow)]]
  -> [BenchmarkMatrix]
makeBenchmarkSuite rows =
  let rowGroups = foldl' (\acc (nm, scale, row) -> Map.insertWith (++) (nm, scale) [row] acc) Map.empty $ concat rows
  in Map.toList rowGroups <&> \((name, scale), rows) ->
    BenchmarkMatrix (T.unpack name) scale rows

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

  pgfRules

  "_build/site/index.html" %> \out ->
    withProofAssistants \agda idris lean rocq ->
      let timeout = 60
      in needSite out $ makeBenchmarkSuite
        [ langBenchmark (agda def) timeout allGenerators
        , langBenchmark idris timeout allGenerators
        , langBenchmark (lean def) timeout allGenerators
        , langBenchmark (rocq def) timeout allGenerators
        ]

  "_build/site/linear.html" %> \out ->
    withProofAssistants \agda idris lean rocq ->
      let timeout = 60
      in needSite out  $ makeBenchmarkSuite
        [ langBenchmark (agda def) timeout linearSuite
        , langBenchmark idris timeout linearSuite
        , langBenchmark (lean def) timeout linearSuite
        , langBenchmark (rocq def) timeout linearSuite
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

  phony "all-pgfs" do
    let timeout = 60
    let pgfDir = [osp|_build/pgfs|]
    -- Forcibly recreate the PGF directory.
    removePathForcibly pgfDir
    createDirectoryRecursive pgfDir
    withProofAssistants \agda idris lean rocq -> do
      -- [FIXME: Reed M, 17/02/2026] This suggests that the options API is messed up.
      namePgfs <- needBenchmarkingPgfs $ makeBenchmarkSuite
            [ langBenchmark (agda def) timeout longNameGenerators
            , langBenchmark idris timeout longNameGenerators
            , langBenchmark (lean $ def { leanSetOpts = [("linter.unusedVariables", "false")] }) timeout longNameGenerators
            , langBenchmark (rocq def) timeout longNameGenerators
            ]
      restPgfs <- needBenchmarkingPgfs $ makeBenchmarkSuite
          [ langBenchmark (agda def) timeout allGenerators
          , langBenchmark idris timeout allGenerators
          , langBenchmark (lean def) timeout allGenerators
          , langBenchmark (rocq def) timeout allGenerators
          ]
      for_ (namePgfs ++ restPgfs) \PgfA{..} -> do
        createDirectoryRecursive [osp|$pgfDir/$pgfATitle|]
        for_ pgfAPlots \path -> do
          let name = takeFileName path
          copyFile path [osp|$pgfDir/$pgfATitle/$name|]
      needPgfTeX [osp|$pgfDir/all-pgfs.tex|] (namePgfs ++ restPgfs)

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
      liftIO $ removePathForcibly [osp|_build|]

  withTargetDocs "Delete the build store." $
    phony "clean-store" do
      liftIO $ removePathForcibly [osp|_build/store|]

  -- Development rules
  generateCBitsClangd
