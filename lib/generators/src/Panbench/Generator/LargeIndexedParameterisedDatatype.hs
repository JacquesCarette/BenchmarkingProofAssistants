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

generator
  :: ( Import hdr "Data.Nat"
     , DataDefinition lhs ctor defns, TelescopeLhs cell hd lhs
     , Binder Single nm Single tm ctor
     , Binder Single nm Single tm cell, Binder None nm Single tm cell, Binder [] nm Single tm cell, Implicit cell
     , Binder Single nm Single tm hd
     , Name nm
     , Pi piCell tm, Arr arrCell tm, App tm, Name tm
     , Binder [] nm Single tm piCell, Implicit piCell
     , Binder None nm Single tm arrCell
     , Constant tm "Type", Constant tm "Nat")
  => GenModule hdr defns Natural
generator =
  GenModule "LargeIndexedParameterisedDatatype"
  [ import_ "Data.Nat"
  ] \size ->
  [ data_ ([ nameN "p" i .: builtin "Type" | i <- [1..size]] |- "D" .: foldr (\_ tp -> (None .:* builtin "Nat") `arr` tp) (builtin "Type") [1..size])
    [ "C" .: pi [implicit ([ nameN "x" i | i <- [1..size] ] .:* builtin "Nat")] (app "D" ([ nameN "p" i | i <- [1..size] ] ++ [ nameN "x" i | i <- [1..size] ]))
    ]
  ]
