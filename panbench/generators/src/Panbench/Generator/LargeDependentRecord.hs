module Panbench.Generator.LargeDependentRecord where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench


generator :: _ => GenModule Natural hdr defns
generator =
  GenModule "LargeDependentRecord"
  [ import_ "Data.Nat"
  ] \size ->
  [ data_ ([] |- ("P" .: pi ["n" .: builtin "Nat"] (builtin "Type")))
    [ "PZ" .: app "P" [nat 0]
    , "PS" .: pi [implicit ("n" .: builtin "Nat"), "xs" .: app "P" ["n"]] (app "P" [parens (op1 "suc" "n")])
    ]
  , record_ ([] |- "Cap_X" .: builtin "Type") "Const" $
      (nameN "f" 1 .: builtin "Nat") :
      [ nameN "f" i .: app "P" [parens $ sucN (i - 1) (nameN "f" 1)]
      | i <- [2..size]
      ]
  -- [TODO: Reed M, 26/09/2025] Should we use a record constructor literal here?
  , ([] |- "test" .: "Cap_X") .=
      app "Const" $
        nat 0 :
        [ foldr (\_ tm -> parens $ app "PS" [tm]) "PZ" [0..i-1]
        | i <- [1..size-1]
        ]

  ]
