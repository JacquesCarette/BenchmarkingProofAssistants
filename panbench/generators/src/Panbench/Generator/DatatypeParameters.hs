{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.DatatypeParameters where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( DataDefinition lhs ctor defns, TelescopeLhs cell hd lhs
     , Binder Single nm Single tm cell
     , Binder Single nm Single tm hd
     , Binder Single nm Single tm ctor
     , Name nm
     , App tm, Name tm
     , Constant tm "Type"
     )
  => GenModule hdr defns Natural
generator =
  GenModule "DatatypeParameters"
  [
  ] \size ->
  [ data_ ([ nameN "p" i .: builtin "Type" | i <- [1..size]] |- "D" .: builtin "Type")
    [ "C" .: app "D" [ nameN "p" i | i <- [1..size] ]
    ]
  ]
