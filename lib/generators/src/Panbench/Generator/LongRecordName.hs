-- | Generator for very long record names.
module Panbench.Generator.LongRecordName where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( RecordDefinition recordLhs nm field defns
     , TelescopeLhs recordCell recordHd recordLhs
     , Binder Single nm Single tm recordHd
     , Constant tm "Type"
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LongRecordName"
  [
  ] \size ->
  [ record_ ([] |- replicateName "f" size .: builtin "Type") "Const" []
  ]
