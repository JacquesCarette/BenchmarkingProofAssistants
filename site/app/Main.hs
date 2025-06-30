module Main where

import Development.Shake

import System.Directory

import Panbench.Shake.HTML
import Panbench.Shake.Matrix
import Panbench.Shake.Lang

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  generatorRules
  benchmarkMatrixRules
  siteRules

  phony "clean-site" do
    removeFilesAfter "_build" ["site/*"]

  phony "clean" do
    removeFilesAfter "_build" ["agda/*", "lean/*", "idris2/*", "rocq/*", "*.html"]

  phony "clean-everything" do
    liftIO $ removeDirectoryRecursive "_build"
