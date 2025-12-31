module Panbench.Generator.IdChain where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: (
  Definition lhs tm defns,
  TelescopeLhs cell hd lhs,
  Binder Single nm Single tm cell,
  Binder Single nm Single tm arrCell,
  Binder Single nm Single tm lamCell,
  Implicit cell,
  Binder Single nm Single tm hd,
  Binder None nm Single tm arrCell,
  Name nm,
  Name tm,
  App tm,
  Arr arrCell tm,
  Lam lamCell tm,
  Builtin tm "Type" tm) => GenModule hdr defns Natural
generator =
    GenModule "IdChain"
      [
      ] \size ->
      [ ([implicit ("A" .: builtin "Type")] |- ("id" .: ((None .:* "A") `arr` "A")) .= lam ["x" .: "A"] "x")
      ,
        [implicit ("A" .: builtin "Type")] |- "test" .: ((None .:* "A") `arr` "A") .=
           foldr (\_ tm -> app "id" [tm]) "id" [1..size]
      ]
