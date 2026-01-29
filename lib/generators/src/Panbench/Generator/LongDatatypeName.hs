-- | Generator for very long datatype names.
module Panbench.Generator.LongDatatypeName where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( DataDefinition dataLhs ctor defns
     , TelescopeLhs dataCell dataHd dataLhs
     , Binder Single nm Single tm dataHd
     , Constant tm "Type"
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LongDatatypeName"
  [
  ] \size ->
  [ data_ ([] |- replicateName "R" size .: builtin "Type") []
  ]
