{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Panbench.Generator.LargeIndexedParameterisedDatatype where

import Prelude hiding (pi)

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule hdr defns Natural
generator =
  GenModule "LargeIndexedParameterisedDatatype"
  [ import_ "Data.Nat"
  ] \size ->
  [ data_ ([ nameN "p" i .: builtin "Type" | i <- [1..size]] |- "D" .: foldr (\_ tp -> anonChk (builtin "Nat") `arr` tp) (builtin "Type") [1..size])
    [ "C" .: pi [implicit ([ nameN "x" i | i <- [1..size] ] .:* builtin "Nat")] (app "D" ([ nameN "p" i | i <- [1..size] ] ++ [ nameN "x" i | i <- [1..size] ]))
    ]
  ]
