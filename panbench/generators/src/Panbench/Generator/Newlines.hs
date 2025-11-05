{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.Newlines where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Module mod hdr defn
     , Newline defn
     )
  => GenModule hdr defn Natural
generator =
  GenModule "Newlines"
  [
  ] \size ->
  [ newlines size
  ]
