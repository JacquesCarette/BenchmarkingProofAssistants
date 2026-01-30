-- | Generator for very long names bound by pi types.
--
-- We opt to not use the bound variable to avoid potentially
-- polluting our results with a long variable name that needs
-- to be looked up in the context.
module Panbench.Generator.LongName.Pi where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Definition defnLhs tm defns
     , TelescopeLhs defnCell defnHd defnLhs
     , Binder Single nm Single tm defnCell
     , Implicit defnCell
     , Binder Single nm Single tm defnHd
     , Pi piCell tm
     , Binder [] nm Single tm piCell
     , Name tm
     , Constant tm "Type"
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LongNamePi"
  [
  ] \size ->
  [ [implicit $ "a" .: builtin "Type"] |- "f" .: builtin "Type" .=
    pi [["x", replicateName "y" size] .:* "a"] "a"
  ]
