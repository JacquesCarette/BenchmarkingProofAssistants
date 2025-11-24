module Panbench.Generator.NestedLetAdditions where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Nat"
     -- Definitions
     , Definition defnLhs tm defns
     , TelescopeLhs defnCell defnHd defnLhs
     , Binder Single nm Single tm defnHd
     -- Let Bindings
     , Let letDefns tm, Name tm
     , Definition letLhs tm letDefns
     , TelescopeLhs letCell letHd letLhs
     , Binder Single nm None tm letHd
     -- Natural Numbers
     , Constant tm "Nat", Literal tm "Nat" Natural, Op2 tm "+"
     -- Names
     , Name nm
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
