module Panbench.Generator.SequentialSimpleRecords where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Nat"
     , RecordDefinition lhs nm field defns, Definition lhs tm defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm cell
     , Binder Single nm Single tm hd
     , Binder Single nm Single tm field
     , Name nm
     , App tm, Name tm
     , Constant tm "Nat", Literal tm "Nat" Natural
     , Constant tm "Type"
     )
  => GenModule hdr defns Natural
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
