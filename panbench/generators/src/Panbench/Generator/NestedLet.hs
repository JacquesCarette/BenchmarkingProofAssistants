{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.NestedLet where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: (Import hdr "Data.Nat"
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
     , Constant tm "Nat", Literal tm "Nat" Natural
     -- Names
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
    GenModule "NestedLet"
      [ import_ "Data.Nat"
      ] \size ->
      [ ([] |- ("n" .: builtin "Nat")) .=
          let_ ( ([] |- syn (nameN "x" 0) .= (nat 1))
               : [[] |- syn (nameN "x" i) .= nameN "x" (i - 1) | i <- [1..size]]
               ) $
          nameN "x" size
      ]
