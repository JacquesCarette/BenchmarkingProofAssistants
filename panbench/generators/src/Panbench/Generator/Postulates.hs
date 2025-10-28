module Panbench.Generator.Postulates where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defn
generator =
  GenModule "Postulates"
  [
  ] \size ->
  [ postulate ([] |- (nameN "P" i .: builtin "Type"))
  | i <- [0..size]
  ]
