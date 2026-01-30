-- | Generator for very long names bound by lambdas.
--
-- We opt to not use the bound variable to avoid potentially
-- polluting our results with a long variable name that needs
-- to be looked up in the context.
module Panbench.Generator.LongName.Lambda where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Definition defnLhs tm defns
     , TelescopeLhs defnCell defnHd defnLhs
     , Binder Single nm Single tm defnCell
     , Implicit defnCell, Unbound defnCell
     , Binder Single nm Single tm defnHd
     , Arr arrCell tm
     , Binder None nm Single tm arrCell
     , Lam lamCell tm
     , Binder [] nm Single tm lamCell
     , Name tm
     , Constant tm "Type"
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LongNameLambda"
  [
  ] \size ->
  [ [unbound $ implicit $ "a" .: builtin "Type"] |- "f" .: arr (None .:* "a") (arr (None .:* "a") "a") .=
    lam [["x", replicateName "y" size] .:* "a"] "x"
  ]
