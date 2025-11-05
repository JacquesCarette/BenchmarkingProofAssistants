{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.LargeSimpleRecord where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule hdr defns Natural
generator =
  GenModule "LargeSimpleRecord"
  [ import_ "Data.Nat"
  ] \size ->
  [ recordN_ ([] |- "Cap_X" .: builtin "Type") "Const" size \i ->
      nameN "f" i .: builtin "Nat"
  -- [TODO: Reed M, 26/09/2025] Should we use a record constructor literal here?
  , ([] |- "test" .: "Cap_X") .=
      app "Const"
        [ nat 1
        | _ <- [1..size]
        ]
  ]
