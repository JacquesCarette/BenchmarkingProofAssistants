module Panbench.Generator.NestedLetXors where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Bool.Xor"
     -- Definitions
     , Definition defnLhs tm defns
     , TelescopeLhs defnCell defnHd defnLhs
     , Binder Single nm Single tm defnHd
     -- Let Bindings
     , Let letDefns tm, Name tm
     , Definition letLhs tm letDefns
     , TelescopeLhs letCell letHd letLhs
     , Binder Single nm None tm letHd
     -- Booleans
     , Constant tm "Bool", Literal tm "Bool" Bool, Op2 tm "xor"
     -- Names
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
    GenModule "NestedLetXor"
      [ import_ "Data.Bool.Xor"
      ] \size ->
      [ ([] |- ("b" .: builtin "Bool")) .=
          let_ ( ([] |- syn (nameN "x" 1) .= bool True)
               : [[] |- syn (nameN "x" i) .= (op2 "xor" (nameN "x" (i - 1)) (nameN "x" (i - 1))) | i <- [2..size]]
               ) $
          nameN "x" size
      ]
