module Panbench.Generator.LargeIndexedDatatype where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule hdr defns Natural
generator =
  GenModule "LargeIndexedDatatype"
  [ import_ "Data.Nat"
  ] \size ->
  [ dataN_ ([] |- "D" .: foldr (\_ tp -> anonChk (builtin "Nat") `arr` tp) (builtin "Type") [1..size]) size \i ->
    nameN "C" i .: pi [ implicit (nameN "x" j .: builtin "Nat") | j <- [1..i] ]
      (app "D" ([ if j <= i then nameN "x" j else nat 0 | j <- [1..size] ]))
  ]
