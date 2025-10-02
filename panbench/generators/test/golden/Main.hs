-- | Golden tests.
module Main where

import Data.Text.IO.Utf8 as Utf8
import Data.Text.Encoding as Utf8
import Data.Text as T
import Data.ByteString.Lazy as LBS

import Panbench.Grammar.Agda
import Panbench.Grammar.Idris
import Panbench.Grammar.Lean
import Panbench.Grammar.Rocq

import Panbench.Generator

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

import System.Directory
import System.FilePath

import Test.Tasty.Golden
import Test.Tasty

import Golden.Util.File

-- import Grammar
-- import Tests

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
  -> (a -> Text)
  -- ^ The printer to use for this thing.
  -> String
  -- ^ The extension to use for the saved snapshot and staging files.
  -> String
  -- ^ The base name to use for snapshot and staging files.
  -> a
  -- ^ The thing to print.
  -> TestTree
printTestForLang langName printer fileExt base syn =
  -- We have to use @goldenVsStringDiff@ ourselves to avoid bad unicode decoding...
  goldenVsStringDiff langName (\ref new -> ["diff", "--strip-trailing-cr" ,"-u", "--color=always", ref, new]) snapshotFile do
    createDirectoryIfMissing False ("test" </> "staging")
    createFile stagingFile
    -- Data.Text.IO.Utf8 always writes UTF-8, ignores the locale,
    -- and does not do line ending conversion.
    let result = printer syn
    Utf8.writeFile stagingFile result
    pure (LBS.fromStrict $ Utf8.encodeUtf8 result)
  where
    stagingFile = stagingPath (base <.> fileExt)
    snapshotFile = snapshotPath (base <.> fileExt)

agdaModuleTest
  :: GenModule size (AgdaHeader ()) (AgdaDefn ())
  -> size
  -> TestTree
agdaModuleTest gen size =
  printTestForLang "agda" (genModuleVia getAgdaMod size) ".agda" (T.unpack (genName gen)) gen


rocqModuleTest
  :: GenModule size (RocqHeader ()) (RocqDefn ())
  -> size
  -> TestTree
rocqModuleTest gen size =
  printTestForLang "rocq" (genModuleVia getRocqMod size) ".v" (T.unpack (genName gen)) gen

leanModuleTest
  :: GenModule size (LeanHeader ()) (LeanDefn ())
  -> size
  -> TestTree
leanModuleTest gen size =
  printTestForLang "lean" (genModuleVia getLeanMod size) ".lean" (T.unpack (genName gen)) gen

idrisModuleTest
  :: GenModule size (IdrisHeader ()) (IdrisDefn ())
  -> size
  -> TestTree
idrisModuleTest gen size =
  printTestForLang "idris" (genModuleVia getIdrisMod size) ".idr" (T.unpack (genName gen)) gen

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
    , agdaModuleTest LargeDependentRecord.generator 5
    , agdaModuleTest LargeIndexedDatatype.generator 5
    , agdaModuleTest LargeIndexedParameterisedDatatype.generator 5
    , agdaModuleTest LargeSimpleDatatype.generator 5
    , agdaModuleTest LargeSimpleRecord.generator 5
    , agdaModuleTest NestedLet.generator 5
    , agdaModuleTest NestedLetAdditions.generator 5
    , agdaModuleTest NestedLetFunctions.generator 5
    , agdaModuleTest Newlines.generator 5
    , agdaModuleTest RecordParameters.generator 5
    , agdaModuleTest SequentialDefinitions.generator 5
    , agdaModuleTest SequentialDependentRecords.generator 5
    , agdaModuleTest SequentialSimpleRecords.generator 5
    , agdaModuleTest SimpleDataDefinitions.generator 5
    ]
  , testGroup "Idris"
    [ idrisModuleTest DatatypeParameters.generator 5
    , idrisModuleTest LargeDependentRecord.generator 5
    , idrisModuleTest LargeIndexedDatatype.generator 5
    , idrisModuleTest LargeIndexedParameterisedDatatype.generator 5
    , idrisModuleTest LargeSimpleDatatype.generator 5
    , idrisModuleTest LargeSimpleRecord.generator 5
    , idrisModuleTest NestedLet.generator 5
    , idrisModuleTest NestedLetAdditions.generator 5
    , idrisModuleTest NestedLetFunctions.generator 5
    , idrisModuleTest Newlines.generator 5
    , idrisModuleTest RecordParameters.generator 5
    , idrisModuleTest SequentialDefinitions.generator 5
    , idrisModuleTest SequentialDependentRecords.generator 5
    , idrisModuleTest SequentialSimpleRecords.generator 5
    , idrisModuleTest SimpleDataDefinitions.generator 5
    ]
  , testGroup "Lean"
    [ leanModuleTest DatatypeParameters.generator 5
    , leanModuleTest LargeDependentRecord.generator 5
    , leanModuleTest LargeIndexedDatatype.generator 5
    , leanModuleTest LargeIndexedParameterisedDatatype.generator 5
    , leanModuleTest LargeSimpleDatatype.generator 5
    , leanModuleTest LargeSimpleRecord.generator 5
    , leanModuleTest NestedLet.generator 5
    , leanModuleTest NestedLetAdditions.generator 5
    , leanModuleTest NestedLetFunctions.generator 5
    , leanModuleTest Newlines.generator 5
    , leanModuleTest RecordParameters.generator 5
    , leanModuleTest SequentialDefinitions.generator 5
    , leanModuleTest SequentialDependentRecords.generator 5
    , leanModuleTest SequentialSimpleRecords.generator 5
    , leanModuleTest SimpleDataDefinitions.generator 5
    ]
  , testGroup "Rocq"
    [ rocqModuleTest DatatypeParameters.generator 5
    , rocqModuleTest LargeDependentRecord.generator 5
    , rocqModuleTest LargeIndexedDatatype.generator 5
    , rocqModuleTest LargeIndexedParameterisedDatatype.generator 5
    , rocqModuleTest LargeSimpleDatatype.generator 5
    , rocqModuleTest LargeSimpleRecord.generator 5
    , rocqModuleTest NestedLet.generator 5
    , rocqModuleTest NestedLetAdditions.generator 5
    , rocqModuleTest NestedLetFunctions.generator 5
    , rocqModuleTest Newlines.generator 5
    , rocqModuleTest RecordParameters.generator 5
    , rocqModuleTest SequentialDefinitions.generator 5
    , rocqModuleTest SequentialDependentRecords.generator 5
    , rocqModuleTest SequentialSimpleRecords.generator 5
    , rocqModuleTest SimpleDataDefinitions.generator 5
    ]
  ]
