{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Panbench.Generator.SequentialDefinitions where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defn
generator = GenModule "SequentialDefinitions"
  [ import_ "Data.Nat"
  ] \size ->
  [ [] |- nameN "x" i .: builtin "Nat" .= nat i
  | i <- [1..size]
  ] ++
  [ [] |- "result" .: builtin "Nat" .= op2 "+" (nameN "x" 1) (nameN "x" size)
  ]
