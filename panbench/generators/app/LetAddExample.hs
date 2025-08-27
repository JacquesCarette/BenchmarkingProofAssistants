{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module LetAddExample where

import Panbench


letAddExample
  :: (Module rep m, Import m "Data.Nat", Constant rep "Nat", Literal rep "Nat" Natural, Op2 rep "+")
  => Gen Natural m rep
letAddExample = Gen header body
  where
    header nm = mconcat
        [ moduleHeader nm
        , import_ "Data.Nat"
        ]

    body size =
      defTm "n" (builtin "Nat") $
        let_ [SynLetDef (name "x" 0) [] (nat 1)] $
        let_ [SynLetDef (name "x" i) [] (op2 "+" (varN "x" (i - 1)) (varN  "x" (i - 1))) | i <- [1..size]] $
          var (name "x" size)
