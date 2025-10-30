module Panbench.Generator.Empty where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defn
generator =
  GenModule "Empty"
  [
  ] \size ->
  [
  ]
