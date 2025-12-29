module Panbench.Generator.Postulates where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Postulate lhs defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm hd
     , Name nm
     , Constant tm "Type"
     )
  => GenModule hdr defns Natural
generator =
  GenModule "Postulates"
  [
  ] \size ->
  [ postulate
    [ [] |- (nameN "P" i .: builtin "Type")
    | i <- [0..size]
    ]
  ]
