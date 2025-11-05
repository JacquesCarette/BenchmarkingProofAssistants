module Panbench.Generator.RecordParameters where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule hdr defns Natural
generator =
  GenModule "RecordParameters"
  [ import_ "Data.Nat"
  ] \size ->
  [ record_ ([ nameN "f" i .: builtin "Nat" | i <- [1..size]] |- "X" .: builtin "Type") "Const"
      [ "sums" .: builtin "Nat"
      ]
  -- [FIXME: Reed M, 26/09/2025] This is a bad benchmark, and we shouldn't be using addition here.
  , [] |- "test" .: app "X" [nat i | i <- [1..size]] .=
      app "Const" [parens $ foldl (\tm i -> op2 "+" tm (nat i)) (nat 1) [2..size]]
  ]
