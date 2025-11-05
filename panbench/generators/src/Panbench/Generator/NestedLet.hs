{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.NestedLet where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule hdr defns Natural
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
