-- | Generator for very long names in the left-hand side
-- of a definition that are used in the right-hand side.
module Panbench.Generator.LongName.DefinitionRhs where

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
  GenModule "LongNameDefinitionRhs"
  [
  ] \size ->
  [ [unbound $ implicit $ "a" .: builtin "Type", replicateName "x" size .: "a", "y" .: "a"] |- "f" .: "a" .= replicateName "x" size
  ]
