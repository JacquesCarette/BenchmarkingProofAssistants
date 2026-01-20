module Panbench.Generator.LargeDependentRecord where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Nat"
     , DataDefinition dataLhs ctor defns, RecordDefinition recordLhs nm field defns, Definition defnLhs tm defns
     , TelescopeLhs dataCell dataHd dataLhs
     , TelescopeLhs recordCell recordHd recordLhs
     , TelescopeLhs defnCell defnHd defnLhs
     , Binder Single nm Single tm dataHd
     , Binder Single nm Single tm recordHd
     , Binder Single nm Single tm defnHd
     , Binder Single nm Single tm piCell, Implicit piCell
     , Binder Single nm Single tm ctor
     , Binder Single nm Single tm field
     , Name nm
     , App tm, Pi piCell tm, Parens tm, Name tm
     , Constant tm "Nat", Op1 tm "suc", Literal tm "Nat" Natural
     , Constant tm "Type"
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LargeDependentRecord"
  [ import_ "Data.Nat"
  ] \size ->
  [ data_ ([] |- ("P" .: pi ["n" .: builtin "Nat"] (builtin "Type")))
    [ "PZ" .: app "P" [nat 0]
    , "PS" .: pi [implicit ("n" .: builtin "Nat"), "xs" .: app "P" ["n"]] (app "P" [parens (op1 "suc" "n")])
    ]
  , record_ ([] |- "Cap_X" .: builtin "Type") "Const" $
      (nameN "f" 1 .: builtin "Nat") :
      [ nameN "f" i .: app "P" [parens $ sucN (i - 1) (nameN "f" 1)]
      | i <- [2..size]
      ]
  -- [TODO: Reed M, 26/09/2025] Should we use a record constructor literal here?
  , ([] |- "test" .: "Cap_X") .=
      app "Const" $
        nat 0 :
        [ foldr (\_ tm -> parens $ app "PS" [tm]) "PZ" [0..i-1]
        | i <- [1..size-1]
        ]

  ]
