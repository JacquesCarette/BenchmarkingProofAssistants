{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Print.Generic where

import Prettyprinter (Pretty(pretty), Doc, (<+>), hsep, emptyDoc, vcat)
import Numeric.Natural (Natural)

import Grammar (Visibility)

-- Utilities to be shared amongst all Print modules
prettyArgs :: Pretty b => b -> (a -> Doc ann) -> [a] -> Doc ann
prettyArgs var pr args = if null args then pretty var else pretty var <+> hsep (map pr args)

blanklines :: Natural -> Doc ann
blanklines n = vcat $ replicate (fromIntegral n) emptyDoc

-- Classes for Keywoards 
class Keywords rep where
  import_ :: rep
  assign  :: rep
  recrd   :: rep
  univ    :: rep
  data_   :: rep
  arr     :: rep
  lcons   :: rep
  vcons   :: rep
  typesep :: rep
  natT    :: rep
  strT    :: rep
  vectT   :: rep

-- Generic printing patterns
-- type annotations
class Keywords rep => TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: Visibility -> rep -> rep -> rep

