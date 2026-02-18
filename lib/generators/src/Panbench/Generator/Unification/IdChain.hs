module Panbench.Generator.Unification.IdChain where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: (
  Definition defnLhs tm defns,
  TelescopeLhs defnCell defnHd defnLhs,
  Implicit defnCell, Unbound defnCell,
  Binder Single nm Single tm defnCell,
  Binder Single nm Single tm defnHd,
  Name nm,
  Name tm,
  App tm,
  Arr arrCell tm,
  Binder None nm Single tm arrCell,
  Lam lamCell tm,
  Binder Single nm Single tm lamCell,
  Builtin tm "Type" tm) => GenModule hdr defns Natural
generator =
    GenModule "IdChain"
      [
      ] \size ->
      [ [unbound $ implicit ("a" .: builtin "Type"), "x" .: "a"] |- ("f" .: "a") .= "x"
      ,
        [unbound $ implicit ("a" .: builtin "Type")] |- "test" .: (None .:* "a" `arr` "a") .=
           foldr (\_ tm -> app "f" [tm]) "f" [1..size]
      ]
