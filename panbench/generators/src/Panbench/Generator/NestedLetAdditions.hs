module Panbench.Generator.NestedLetAdditions where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Nat"
     , Definition lhs tm defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm hd, Binder Single nm None tm hd
     , Name nm
     , Let letDefns tm, Name tm
     , Definition letLhs tm letDefns
     , TelescopeLhs cell hd letLhs
     , Constant tm "Nat", Literal tm "Nat" Natural, Op2 tm "+"
     )
  => GenModule hdr defns Natural
generator =
    GenModule "NestedLetAdditions"
      [ import_ "Data.Nat"
      ] \size ->
      [ ([] |- ("n" .: builtin "Nat")) .=
          let_ ( ([] |- syn (nameN "x" 1) .= (nat 1))
               : [[] |- syn (nameN "x" i) .= (op2 "+" (nameN "x" (i - 1)) (nameN "x" (i - 1))) | i <- [2..size]]
               ) $
          nameN "x" size
      ]
