module Panbench.Generator.SequentialDependentRecords where

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
     , App tm, Parens tm, Name tm
     , Constant tm "Nat", Literal tm "Nat" Natural
     , Constant tm "Type"
     )
  => GenModule hdr defns Natural
generator =
  GenModule "SequentialDependentRecords"
  [ import_ "Data.Nat"
  ] \size ->
  [ record_ ([] |- nameN "Dummy" i .: builtin "Type") (nameN "Const" i)
    [ if i == 1 then nameN "f" i .: builtin "Nat" else nameN "f" i .: nameN "Dummy" (i - 1)
    ]
  | i <- [1..size]
  ] ++
  [ [] |- "test" .: nameN "Dummy" size .=
      foldl (\tm i -> parens $ app (nameN "Const" i) [tm]) (nat 10) [1..size]
  ]
