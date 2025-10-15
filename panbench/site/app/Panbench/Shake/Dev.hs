-- | Development tools
module Panbench.Shake.Dev
  ( -- $shakeDev
    generateCBitsClangd
  ) where

import Data.List

import Development.Shake

-- * Shake rules for developing @panbench@
--
-- $shakeDev

-- | Shake rule for generating a @.clangd@ file for our @cbits@.
--
-- We need to make some FFI calls into the GHC RTS as a part of @benchmark.c@,
-- which requires us to include the RTS header files. If we want to use
-- the @clangd@ language server while working on @benchmark.c@, we need to
-- tell where these header files are located on disk. We can only do this
-- via absolute paths at the moment (see https://github.com/clangd/clangd/issues/1038),
-- which is not very portable!
--
-- This rule uses @ghc-pkg@ to find the relevant paths, and generates a @.clangd@ file
-- automatically.
generateCBitsClangd :: Rules ()
generateCBitsClangd =
  withTargetDocs "Generate a .clangd file for working with benchmark.c" do
    "panbench/site/cbits/.clangd" %> \out -> do
      Stdout dirs <- command [] "ghc-pkg" ["field", "rts", "include-dirs", "--simple-output"]
      let includes = intercalate ", " $ ("-I" <>) <$> words dirs
      writeFileChanged out $ unlines
        [ "CompileFlags:"
        , "      Add: [" <> includes <> "]"
        ]
