module Panbench.Generator.LargeSimpleDatatype where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( DataDefinition lhs ctor defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm hd
     , Binder Single nm Single tm cell
     , Binder Single nm Single tm ctor
     , Name nm
     , Name tm
     , Constant tm "Type"
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LargeSimpleDatatype"
  [
  ] \size ->
  [ dataN_ ([] |- "D" .: builtin "Type") size \i ->
    nameN "C" i .: "D"
  ]
