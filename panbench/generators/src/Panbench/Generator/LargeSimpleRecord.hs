{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.LargeSimpleRecord where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Import hdr "Data.Nat"
     , RecordDefinition lhs nm field defns, Definition lhs tm defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm cell
     , Binder Single nm Single tm hd
     , Binder Single nm Single tm field
     , Name nm
     , App tm, Name tm
     , Constant tm "Nat", Literal tm "Nat" Natural, Constant tm "Type"
     )
  => GenModule hdr defns Natural
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
