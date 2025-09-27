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

generator :: _ => GenModule Natural hdr defns
generator =
  GenModule "IndicesParameters_Datatypes"
  [ import_ "Data.Nat"
  ] \size ->
  [ data_ ([ nameN "p" i .: builtin "Type" | i <- [1..size]] |- "D" .: foldr (\_ tp -> anonChk (builtin "Nat") `arr` tp) (builtin "Type") [1..size]) []
  ]
