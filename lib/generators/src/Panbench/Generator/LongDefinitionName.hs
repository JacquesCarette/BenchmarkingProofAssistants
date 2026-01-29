-- | Generator for very long definition names.
module Panbench.Generator.LongDefinitionName where

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
     , Name tm
     , Constant tm "Type"
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LongDefinitionName"
  [
  ] \size ->
  [ [unbound $ implicit $ "a" .: builtin "Type", "x" .: "a"] |- replicateName "f" size .: "a" .= "x"
  ]
