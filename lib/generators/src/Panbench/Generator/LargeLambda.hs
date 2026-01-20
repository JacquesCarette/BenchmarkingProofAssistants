module Panbench.Generator.LargeLambda where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Definition defnLhs tm defns
     , TelescopeLhs defnCell defnHd defnLhs
     , Binder Single nm Single tm defnHd
     , Binder Single nm Single tm defnCell, Binder [] nm Single tm defnCell, Implicit defnCell, Unbound defnCell
     , Arr arrCell tm
     , Binder None nm Single tm arrCell
     , Lam lamCell tm
     , Binder Single nm None tm lamCell, Binder [] nm None tm lamCell
     , Constant tm "Type"
     , Name nm, Name tm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LargeLambda"
  [
  ] \size ->
  [ [unbound $ implicit ("A" .: builtin "Type"), unbound $ implicit ([nameN "B" i | i <- [0..size]] .:* builtin "Type")]
    |- nameN "const" size .: ("A" .-> foldr (\i tp -> nameN "B" i .-> tp) "A" [0..size]) .=
      lam [syn "a", syns [nameN "b" i | i <- [0..size]]] "a"
  ]
