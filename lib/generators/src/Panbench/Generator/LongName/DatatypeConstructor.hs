-- | Generator for very long datatype constructor names.
module Panbench.Generator.LongName.DatatypeConstructor where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( DataDefinition dataLhs dataCtor defns
     , TelescopeLhs dataCell dataHd dataLhs
     , Binder Single nm Single tm dataHd
     , Binder Single nm Single tm dataCtor
     , Constant tm "Type"
     , Name tm
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "LongNameDatatypeConstructor"
  [
  ] \size ->
  [ data_ ([] |- "D" .: builtin "Type")
    [ replicateName "C" size .: "D"
    ]
  ]
