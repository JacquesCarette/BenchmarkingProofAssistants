{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.SimpleDataDefinitions where

import Numeric.Natural

import Panbench.Generator
import Panbench


generator :: _ => GenModule Natural hdr defns
generator =
  GenModule "SimpleDataDefinitions"
  [
  ] \size ->
  [ data_ ([] |- (nameN "X" i .: builtin "Type"))
    [ nameN "Y" i .: nameN "X" i
    ]
  | i <- [1..size]
  ]
