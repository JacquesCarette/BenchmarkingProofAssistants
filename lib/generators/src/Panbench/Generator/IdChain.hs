module Panbench.Generator.IdChain where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: (Constant tm "Type",
  Definition defns lhs tm, TelescopeLhs lhs hd cell,
  Chk nm tm cell, Chk nm tm hd,
  Name nm, Implicit cell, Name tm, App tm) => GenModule hdr defns Natural
generator =
    GenModule "IdChain"
      [
      ] \size ->
      [ ([implicit ("A" .: builtin "Type"), ("x" .: "A")] |- ("id" .: "A") .= "x")
      ,
        [implicit ("A" .: builtin "Type"), ("x" .: "A") ] |- "test" .: "A" .=
           foldr (\_ tm -> app "id" [tm]) "x" [1..size]
      ]
