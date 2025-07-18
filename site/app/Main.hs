module Main where

import Development.Shake

import System.Directory

import Panbench.Shake.Dev
import Panbench.Shake.Git
import Panbench.Shake.HTML
import Panbench.Shake.Install.Agda
import Panbench.Shake.Install.Lean
import Panbench.Shake.Lang
import Panbench.Shake.Matrix

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  generatorRules
  benchmarkMatrixRules
  siteRules

  gitRules
  agdaInstallRules
  leanInstallRules

  withTargetDocs "Remove all generated html files." $
    phony "clean-site" do
      removeFilesAfter "_build" ["site/*"]

  withTargetDocs "Remove all generated outputs and html files." $
    phony "clean" do
      removeFilesAfter "_build" ["agda/*", "lean/*", "idris2/*", "rocq/*", "*.html"]

  withTargetDocs "Delete the entire _build directory, including the shake database." $
    phony "clean-everything" do
      liftIO $ removeDirectoryRecursive "_build"

  -- Development rules
  generateCBitsClangd
