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
     , Definition lhs tm defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm cell
     , Binder Single nm Single tm hd, Binder Single nm None tm hd
     , Name nm
     , Let letDefns tm, Name tm
     , Definition letLhs tm letDefns
     , TelescopeLhs cell hd letLhs
     , Constant tm "Nat", Literal tm "Nat" Natural
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
