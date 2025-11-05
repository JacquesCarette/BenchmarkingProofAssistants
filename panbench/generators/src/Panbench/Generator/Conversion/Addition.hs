-- | A basic conversion tests for addition.
module Panbench.Generator.Conversion.Addition where

import Numeric.Natural

import Panbench
import Panbench.Generator

generator
  :: ( Import hdr "Data.Nat"
     , Import hdr "Data.Id"
     , Definition defns lhs tm, TelescopeLhs lhs cell hd, Chk nm tm cell, Name nm
     , Op2 tm "=", Constant tm "refl"
     , Op2 tm "+", Literal tm "Nat" Natural
     )
  => GenModule (Int, Natural) hdr defns
generator =
  GenModule "ConversionAddition"
  [ import_ "Data.Nat"
  , import_ "Data.Id"
  ] \(n, x) ->
  [ let tm = foldr (op2 "+") (nat 0) $ replicate n (nat x)
        nf = nat $ sum $ replicate n x
    in checkConvert "conv" tm nf
  ]
