module Panbench.Generator.SequentialDependentRecords where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defns
generator =
  GenModule "ChainDef_DependentRecordModule"
  [
  ] \size ->
  [ record_ ([] |- nameN "Dummy" i .: builtin "Type") (nameN "Const" i)
    [ if i == 1 then nameN "f" i .: builtin "Nat" else nameN "f" i .: nameN "Dummy" (i - 1)
    ]
  | i <- [1..size]
  ] ++
  [ [] |- "example" .: nameN "Dummy" size .=
      foldl (\tm i -> parens $ app (nameN "Const" i) [tm]) (nat 10) [1..size]
  ]
