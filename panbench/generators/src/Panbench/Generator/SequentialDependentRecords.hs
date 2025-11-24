module Panbench.Generator.SequentialDependentRecords where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Nat"
     -- Records
     , RecordDefinition recordLhs nm recordField defns
     , TelescopeLhs recordCell recordHd recordLhs
     , Binder Single nm Single tm recordHd
     , Binder Single nm Single tm recordField
     -- Definitions
     , Definition defnLhs tm defns
     , TelescopeLhs defnCell defnHd defnLhs
     , Binder Single nm Single tm defnHd
     -- Terms
     , App tm, Parens tm, Constant tm "Type"
     -- Natural Numbers
     , Constant tm "Nat", Literal tm "Nat" Natural
     -- Names
     , Name nm, Name tm
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
