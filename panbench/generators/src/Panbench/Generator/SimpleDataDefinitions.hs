{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.SimpleDataDefinitions where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( DataDefinition lhs ctor defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm hd
     , Binder Single nm Single tm ctor
     , Name nm
     , Name tm
     , Constant tm "Type"
     )
  => GenModule hdr defns Natural
generator =
  GenModule "SimpleDataDefinitions"
  [
  ] \size ->
  [ data_ ([] |- (nameN "X" i .: builtin "Type"))
    [ nameN "Y" i .: nameN "X" i
    ]
  | i <- [1..size]
  ]
