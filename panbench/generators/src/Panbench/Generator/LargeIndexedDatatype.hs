module Panbench.Generator.LargeIndexedDatatype where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Nat"
     , DataDefinition lhs ctor defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm cell, Binder None nm Single tm cell, Implicit cell
     , Binder Single nm Single tm hd
     , Binder Single nm Single tm ctor
     , Name nm
     , App tm, Arr cell tm, Pi cell tm, Name tm
     , Constant tm "Nat", Literal tm "Nat" Natural, Constant tm "Type"
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LargeIndexedDatatype"
  [ import_ "Data.Nat"
  ] \size ->
  [ dataN_ ([] |- "D" .: foldr (\_ tp -> (None .:* builtin "Nat") `arr` tp) (builtin "Type") [1..size]) size \i ->
    nameN "C" i .: pi [ implicit (nameN "x" j .: builtin "Nat") | j <- [1..i] ]
      (app "D" ([ if j <= i then nameN "x" j else nat 0 | j <- [1..size] ]))
  ]
