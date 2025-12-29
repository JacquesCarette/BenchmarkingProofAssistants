module Panbench.Generator.IdChain where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: (
  Definition lhs tm defns,
  TelescopeLhs cell hd lhs,
  Binder Single nm Single tm cell,
  Implicit cell,
  Binder Single nm Single tm hd,
  Name nm,
  Name tm,
  App tm,
  Builtin tm "Type" tm) => GenModule hdr defns Natural
generator =
    GenModule "IdChain"
      [
      ] \size ->
      [ ([implicit ("A" .: builtin "Type"), ("x" .: "A")] |- ("id" .: "A") .= "x")
      ,
        [implicit ("A" .: builtin "Type"), ("x" .: "A") ] |- "test" .: "A" .=
           foldr (\_ tm -> app "id" [tm]) "x" [1..size]
      ]
