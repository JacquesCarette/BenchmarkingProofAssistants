module Panbench.Generator.RecordParameters where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Nat"
     -- Records
     , RecordDefinition recordLhs nm field defns
     , TelescopeLhs recordCell recordHd recordLhs
     , Binder Single nm Single tm recordCell
     , Binder Single nm Single tm recordHd
     , Binder Single nm Single tm field
     -- Definitions
     , Definition defnLhs tm defns
     , TelescopeLhs defnCell defnHd defnLhs
     , Binder Single nm Single tm defnHd
     -- Terms
     , App tm, Parens tm, Constant tm "Type"
     -- Natural Numbers
     , Constant tm "Nat", Literal tm "Nat" Natural, Op2 tm "+"
     -- Names
     , Name nm, Name tm
     )
  => GenModule hdr defns Natural
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
