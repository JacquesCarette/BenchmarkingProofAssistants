module Panbench.Generator.Parens where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Definition lhs tm defns
     , TelescopeLhs cell hd lhs
     , Binder Single nm Single tm cell
     , Binder Single nm Single tm hd
     , Name nm
     , Parens tm, Name tm
     , Constant tm "Type"
     )
  => GenModule hdr defns Natural
generator =
  GenModule "Parens"
  [
  ] \size ->
  [ ([ "x" .: builtin "Type" ] |- ("P" .: builtin "Type")) .= parensN size "x"
  ]
