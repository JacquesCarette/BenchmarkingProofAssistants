module Panbench.Generator.LargeSimpleDatatype where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule hdr defns Natural
generator =
  GenModule "LargeSimpleDatatype"
  [
  ] \size ->
  [ dataN_ ([] |- "D" .: builtin "Type") size \i ->
    nameN "C" i .: "D"
  ]
