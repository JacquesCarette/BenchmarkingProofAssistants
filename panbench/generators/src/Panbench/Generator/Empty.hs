module Panbench.Generator.Empty where

import Numeric.Natural

import Panbench.Generator

generator :: _ => GenModule hdr defn Natural
generator =
  GenModule "Empty"
  [
  ] \_ ->
  [
  ]
