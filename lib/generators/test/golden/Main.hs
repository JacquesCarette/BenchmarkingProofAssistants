{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Golden tests.
module Main where

import Data.Text as T
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.Default
import Data.Functor

import Numeric.Natural

import Panbench.Grammar.Agda
import Panbench.Grammar.Idris
import Panbench.Grammar.Lean
import Panbench.Grammar.Rocq

import Panbench.Generator

import Panbench.Generator.DatatypeParameters qualified as DatatypeParameters
import Panbench.Generator.IdChain qualified as IdChain
import Panbench.Generator.LargeDependentRecord qualified as LargeDependentRecord
import Panbench.Generator.LargeIndexedDatatype qualified as LargeIndexedDatatype
import Panbench.Generator.LargeIndexedParameterisedDatatype qualified as LargeIndexedParameterisedDatatype
import Panbench.Generator.LargeLambda qualified as LargeLambda
import Panbench.Generator.LargeSimpleDatatype qualified as LargeSimpleDatatype
import Panbench.Generator.LargeSimpleRecord qualified as LargeSimpleRecord
import Panbench.Generator.LongDatatypeName qualified as LongDatatypeName
import Panbench.Generator.LongDefinitionName qualified as LongDefinitionName
import Panbench.Generator.LongRecordName qualified as LongRecordName
import Panbench.Generator.ManyImplicits qualified as ManyImplicits
import Panbench.Generator.NestedLet qualified as NestedLet
import Panbench.Generator.NestedLetAdditions qualified as NestedLetAdditions
import Panbench.Generator.NestedLetFunctions qualified as NestedLetFunctions
import Panbench.Generator.Newlines qualified as Newlines
import Panbench.Generator.Parens qualified as Parens
import Panbench.Generator.Postulates qualified as Postulates
import Panbench.Generator.RecordParameters qualified as RecordParameters
import Panbench.Generator.SequentialDefinitions qualified as SequentialDefinitions
import Panbench.Generator.SequentialDependentRecords qualified as SequentialDependentRecords
import Panbench.Generator.SequentialSimpleRecords qualified as SequentialSimpleRecords
import Panbench.Generator.SimpleDataDefinitions qualified as SimpleDataDefinitions

import System.Directory
import System.FilePath
import System.IO

import Test.Tasty.Golden
import Test.Tasty


-- * Testing utilities

-- | Convert a filename to a path to a snapshot file.
--
-- >>> snapshotPath "LetExample.agda"
-- "test/snapshots/LetExample.agda"
snapshotPath
  :: String
  -- ^ The base name to use for the snapshot file.
  -> FilePath
snapshotPath fname = "test" </> "snapshot" </> fname

-- | Convert a filename to a path to a staging file.
--
-- >>> stagingPath "LetExample.agda"
-- "test/staging/LetExample.agda"
stagingPath
  :: String
  -- ^ The name to use for the staging file.
  -> FilePath
stagingPath fname = "test" </> "staging" </> fname

-- | Create a golden test printing for a language.
--   The resulting snapshot files will always be encoded
--   using UTF-8. Moreover, no newline conversion will
--   be performed, and the users locale will be ignored.
printTestForLang
  :: String
  -- ^ The name of the language.
  -> (Handle -> IO ())
  -- ^ The printer to use for this thing.
  -> String
  -- ^ The extension to use for the saved snapshot and staging files.
  -> String
  -- ^ The base name to use for snapshot and staging files.
  -> TestTree
printTestForLang langName printer fileExt base =
  -- We have to use @goldenVsStringDiff@ ourselves to avoid bad unicode decoding...
  goldenVsStringDiff langName (\ref new -> ["diff", "--strip-trailing-cr" ,"-u", "--color=always", ref, new]) snapshotFile do
    createDirectoryIfMissing False ("test" </> "staging")
    withBinaryFile stagingFile ReadWriteMode \hdl -> do
      hSetFileSize hdl 0
      printer hdl
      hFlush hdl
      hSeek hdl AbsoluteSeek 0
      LBS.fromStrict <$> BS.hGetContents hdl
  where
    stagingFile = stagingPath (base <.> fileExt)
    snapshotFile = snapshotPath (base <.> fileExt)

agdaModuleTest
  :: GenModule AgdaHeader AgdaDefns size
  -> size
  -> TestTree
agdaModuleTest gen size =
  printTestForLang "agda" (genModuleVia (runAgdaM def) size gen) ".agda" (T.unpack (genName gen))

rocqModuleTest
  :: GenModule RocqHeader RocqDefns size
  -> size
  -> TestTree
rocqModuleTest gen size =
  printTestForLang "rocq" (genModuleVia (runRocqM def) size gen) ".v" (T.unpack (genName gen))

leanModuleTest
  :: GenModule LeanHeader LeanDefns size
  -> size
  -> TestTree
leanModuleTest gen size =
  printTestForLang "lean" (genModuleVia runLeanM size gen) ".lean" (T.unpack (genName gen))

idrisModuleTest
  :: GenModule IdrisHeader IdrisDefns size
  -> size
  -> TestTree
idrisModuleTest gen size =
  printTestForLang "idris" (genModuleVia (runIdrisM def) size gen) ".idr" (T.unpack (genName gen))

allGenerators :: _ => [GenModule hdr defns Natural]
allGenerators =
  [ DatatypeParameters.generator
  , IdChain.generator
  , LargeDependentRecord.generator
  , LargeIndexedDatatype.generator
  , LargeIndexedParameterisedDatatype.generator
  , LargeLambda.generator
  , LargeSimpleDatatype.generator
  , LargeSimpleRecord.generator
  , LongDatatypeName.generator
  , LongDefinitionName.generator
  , LongRecordName.generator
  , ManyImplicits.generator
  , NestedLet.generator
  , NestedLetAdditions.generator
  , NestedLetFunctions.generator
  , Newlines.generator
  , Parens.generator
  , Postulates.generator
  , RecordParameters.generator
  , SequentialDefinitions.generator
  , SequentialDependentRecords.generator
  , SequentialSimpleRecords.generator
  , SimpleDataDefinitions.generator
  ]

-- * Tests
--
-- These tests are all taken from the original @Tests.hs@ file,
-- and the size parameters were reverse engineered from @good_Output@.
-- All files in tests/snapshots originally also originated from @good_Output@,
-- so there is a throughline.

main :: IO ()
main = defaultMain $
  testGroup "Golden"
  [ testGroup "Agda" $ allGenerators <&> \gen -> agdaModuleTest gen 5
  , testGroup "Idris" $ allGenerators <&> \gen -> idrisModuleTest gen 5
  , testGroup "Lean" $ allGenerators <&> \gen -> leanModuleTest gen 5
  , testGroup "Rocq" $ allGenerators <&> \gen -> rocqModuleTest gen 5
  ]
