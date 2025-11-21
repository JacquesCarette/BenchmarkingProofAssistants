module Panbench.Generator.NestedLetFunctions where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Nat"
     , Definition lhs tm defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm cell
     , Binder Single nm Single tm hd, Binder Single nm None tm hd
     , Name nm
     , App tm, Let letDefns tm, Name tm
     , Definition letLhs tm letDefns
     , TelescopeLhs cell hd letLhs
     , Constant tm "Nat", Literal tm "Nat" Natural, Op2 tm "+"
     )
  => GenModule hdr defns Natural
generator =
    GenModule "NestedLetFunctions"
      [ import_ "Data.Nat"
      ] \size ->
      [ ([] |- ("n" .: builtin "Nat")) .=
          let_ [
            [ nameN "x" j .: builtin "Nat" | j <- [1..i] ] |- (nameN "f" i .: builtin "Nat") .=
                foldl (op2 "+") (nat 1) [ nameN "x" j | j <- [1..i]]
            | i <- [1..size]
            ] $
          foldl (\tm i -> op2 "+" (app (nameN "f" i) [ nat j | j <- [2..i+1] ]) tm) (app (nameN "f" 1) [nat 2]) [2..size]
      ]
