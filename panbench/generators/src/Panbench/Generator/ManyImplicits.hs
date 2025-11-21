-- | Test for many implicit arguments.
module Panbench.Generator.ManyImplicits where

import Numeric.Natural

import Panbench.Grammar
import Panbench.Generator

generator
  :: (
    -- Definitions
       Definition defnLhs tm defns
     , TelescopeLhs defnCell defnHd defnLhs
     , Binder [] nm Single tm defnCell, Binder Single nm Single tm defnCell
     , Unbound defnCell, Implicit defnCell
     , Binder Single nm Single tm defnHd
     -- Types
     ,  Constant tm "Type"
     -- Names
     , Name nm, Name tm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "ManyImplicits"
  [
  ] \size ->
  [ [implicit ("A" .: builtin "Type"), unbound $ implicit ((nameN "x" <$> [0..size]) .:* "A"), "y" .: "A"] |- "f" .: "A" .= "y"
  ]
