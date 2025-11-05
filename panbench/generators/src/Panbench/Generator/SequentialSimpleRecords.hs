module Panbench.Generator.SequentialSimpleRecords where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule hdr defns Natural
generator =
  GenModule "SequentialSimpleRecords"
  [ import_ "Data.Nat"
  ] \size ->
  [ record_ ([] |- nameN "Dummy" i .: builtin "Type") (nameN "Const" i)
    [ nameN "f" i .: builtin "Nat"
    ]
  | i <- [1..size]
  ] ++
  [ [] |- "test" .: nameN "Dummy" size .=
    app (nameN "Const" size) [nat 1]
  ]
