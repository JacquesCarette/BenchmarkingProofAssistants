{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.NestedLet where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defns
generator =
    GenModule "LetExample"
      [ import_ "Data.Nat"
      ] \size ->
      [ ([] |- (nameN "n" 0 .: builtin "Nat")) .=
          let_ [ [] |- syn (nameN "x" 0) .= (nat 1)] $
          let_ [ [] |- syn (nameN "x" i) .= nameN "x" (i - 1) | i <- [1..size]] $
          nameN "x" size
      ]
