-- | Generator for very long record names.
module Panbench.Generator.LongName.RecordField where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( RecordDefinition recordLhs nm recordField defns
     , TelescopeLhs recordCell recordHd recordLhs
     , Binder Single nm Single tm recordCell
     , Binder Single nm Single tm recordHd
     , Binder Single nm Single tm recordField
     , Name tm
     , Constant tm "Type"
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LongNameRecordConstructor"
  [
  ] \size ->
  [ record_ (["a" .: builtin "Type"] |- "R" .: builtin "Type") "C"
    [ replicateName "f" size .: "a"
    ]
  ]
