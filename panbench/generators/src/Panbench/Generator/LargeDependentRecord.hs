module Panbench.Generator.LargeDependentRecord where

import Numeric.Natural

import Panbench.Generator
import Panbench


generator :: _ => GenModule Natural hdr defns
generator =
  GenModule "Fields_DependentRecordModule"
  [
  ] \size ->
  [ postulate (["n" .: builtin "Nat"] |- ("P" .: builtin "Type"))
  , postulate ([] |- "nil" .: app "P" [nat 0])
  , postulate (["n" .: builtin "Nat", "xs" .: app "P" ["n"]] |- ("cons" .: app "P" [op1 "suc" "n"]))
  , record_ ([] |- "Cap_X" .: builtin "Type") "Const" $
      (nameN "f" 1 .: builtin "Nat") :
      [ nameN "f" i .: app "P" [iter (op1 "suc") (nameN "f" 1) (i - 1)]
      | i <- [2..size]
      ]
  -- [TODO: Reed M, 26/09/2025] Should we use a record constructor literal here?
  , ([] |- "example" .: "Cap_X") .=
      app "Const"
        [ foldr (\j tm -> app "cons" [nat j, tm]) "nil" [0..i-1]
        | i <- [1..size]
        ]
  ]
