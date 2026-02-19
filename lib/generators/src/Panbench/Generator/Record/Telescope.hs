module Panbench.Generator.Record.Telescope where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( RecordDefinition recordLhs nm field defns
     , TelescopeLhs recordCell recordHd recordLhs
     , Binder Single nm Single tm recordCell
     , Binder Single nm Single tm recordHd
     , Binder Single nm Single tm field
     , Arr arrCell tm
     , Binder None nm Single tm arrCell
     , Pi piCell tm
     , Binder Single nm Single tm piCell
     , App tm
     , Parens tm
     , Constant tm "Type"
     , Name tm
     , Name nm
     )
  => GenModule hdr defns Natural
generator =
  GenModule "RecordTelescope"
  [
  ] \size ->
  [ record_ (["U" .: builtin "Type", "El" .: (None .:* "U" `arr` builtin "Type")] |- "Telescope" .: builtin "Type") "Tele"
    [ nameN "a" i .: pi [ nameN "x" (j-1) .: el (nameN "a" (j-1)) [nameN "x" (k-2) | k <- [2..j]] | j <- [1..i] ] "U"
    | i <- [0..size]
    ]
  ]
  where
    el x [] = app "El" [x]
    el x xs = app "El" [parens $ app x xs]
