-- | Golden tests.
module Main where

import Data.Text as T
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.Default

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
import Panbench.Generator.LongDefinitionName qualified as LongDefinitionName
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

-- * Tests
--
-- These tests are all taken from the original @Tests.hs@ file,
-- and the size parameters were reverse engineered from @good_Output@.
-- All files in tests/snapshots originally also originated from @good_Output@,
-- so there is a throughline.

main :: IO ()
main = defaultMain $
  testGroup "Golden"
  [ testGroup "Agda"
    [ agdaModuleTest DatatypeParameters.generator 5
    , agdaModuleTest IdChain.generator 5
    , agdaModuleTest LargeDependentRecord.generator 5
    , agdaModuleTest LargeIndexedDatatype.generator 5
    , agdaModuleTest LargeIndexedParameterisedDatatype.generator 5
    , agdaModuleTest LargeLambda.generator 5
    , agdaModuleTest LargeSimpleDatatype.generator 5
    , agdaModuleTest LargeSimpleRecord.generator 5
    , agdaModuleTest LongDefinitionName.generator 5
    , agdaModuleTest ManyImplicits.generator 5
    , agdaModuleTest NestedLet.generator 5
    , agdaModuleTest NestedLetAdditions.generator 5
    , agdaModuleTest NestedLetFunctions.generator 5
    , agdaModuleTest Newlines.generator 5
    , agdaModuleTest Parens.generator 5
    , agdaModuleTest Postulates.generator 5
    , agdaModuleTest RecordParameters.generator 5
    , agdaModuleTest SequentialDefinitions.generator 5
    , agdaModuleTest SequentialDependentRecords.generator 5
    , agdaModuleTest SequentialSimpleRecords.generator 5
    , agdaModuleTest SimpleDataDefinitions.generator 5
    ]
  , testGroup "Idris"
    [ idrisModuleTest DatatypeParameters.generator 5
    , idrisModuleTest IdChain.generator 5
    , idrisModuleTest LargeDependentRecord.generator 5
    , idrisModuleTest LargeIndexedDatatype.generator 5
    , idrisModuleTest LargeIndexedParameterisedDatatype.generator 5
    , idrisModuleTest LargeLambda.generator 5
    , idrisModuleTest LargeSimpleDatatype.generator 5
    , idrisModuleTest LargeSimpleRecord.generator 5
    , idrisModuleTest LongDefinitionName.generator 5
    , idrisModuleTest ManyImplicits.generator 5
    , idrisModuleTest NestedLet.generator 5
    , idrisModuleTest NestedLetAdditions.generator 5
    , idrisModuleTest NestedLetFunctions.generator 5
    , idrisModuleTest Newlines.generator 5
    , idrisModuleTest Parens.generator 5
    , idrisModuleTest Postulates.generator 5
    , idrisModuleTest RecordParameters.generator 5
    , idrisModuleTest SequentialDefinitions.generator 5
    , idrisModuleTest SequentialDependentRecords.generator 5
    , idrisModuleTest SequentialSimpleRecords.generator 5
    , idrisModuleTest SimpleDataDefinitions.generator 5
    ]
  , testGroup "Lean"
    [ leanModuleTest DatatypeParameters.generator 5
    , leanModuleTest IdChain.generator 5
    , leanModuleTest LargeDependentRecord.generator 5
    , leanModuleTest LargeIndexedDatatype.generator 5
    , leanModuleTest LargeIndexedParameterisedDatatype.generator 5
    , leanModuleTest LargeLambda.generator 5
    , leanModuleTest LargeSimpleDatatype.generator 5
    , leanModuleTest LargeSimpleRecord.generator 5
    , leanModuleTest LongDefinitionName.generator 5
    , leanModuleTest ManyImplicits.generator 5
    , leanModuleTest NestedLet.generator 5
    , leanModuleTest NestedLetAdditions.generator 5
    , leanModuleTest NestedLetFunctions.generator 5
    , leanModuleTest Newlines.generator 5
    , leanModuleTest Parens.generator 5
    , leanModuleTest Postulates.generator 5
    , leanModuleTest RecordParameters.generator 5
    , leanModuleTest SequentialDefinitions.generator 5
    , leanModuleTest SequentialDependentRecords.generator 5
    , leanModuleTest SequentialSimpleRecords.generator 5
    , leanModuleTest SimpleDataDefinitions.generator 5
    ]
  , testGroup "Rocq"
    [ rocqModuleTest DatatypeParameters.generator 5
    , rocqModuleTest IdChain.generator 5
    , rocqModuleTest LargeDependentRecord.generator 5
    , rocqModuleTest LargeIndexedDatatype.generator 5
    , rocqModuleTest LargeIndexedParameterisedDatatype.generator 5
    , rocqModuleTest LargeLambda.generator 5
    , rocqModuleTest LargeSimpleDatatype.generator 5
    , rocqModuleTest LargeSimpleRecord.generator 5
    , rocqModuleTest LongDefinitionName.generator 5
    , rocqModuleTest ManyImplicits.generator 5
    , rocqModuleTest NestedLet.generator 5
    , rocqModuleTest NestedLetAdditions.generator 5
    , rocqModuleTest NestedLetFunctions.generator 5
    , rocqModuleTest Newlines.generator 5
    , rocqModuleTest Parens.generator 5
    , rocqModuleTest Postulates.generator 5
    , rocqModuleTest RecordParameters.generator 5
    , rocqModuleTest SequentialDefinitions.generator 5
    , rocqModuleTest SequentialDependentRecords.generator 5
    , rocqModuleTest SequentialSimpleRecords.generator 5
    , rocqModuleTest SimpleDataDefinitions.generator 5
    ]
  ]
