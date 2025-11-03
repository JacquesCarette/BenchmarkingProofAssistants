module Panbench.Generator.Parens where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defn
generator =
  GenModule "Parens"
  [
  ] \size ->
  [ ([ "x" .: builtin "Type" ] |- ("P" .: builtin "Type")) .= parensN size "x"
  ]
