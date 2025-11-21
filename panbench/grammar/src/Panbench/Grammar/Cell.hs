{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- | Binders.
module Panbench.Grammar.Cell
  ( Cell(..)
  , MultiCell
  , SingleCell
  , pattern SingleCell
  , pattern RequiredCell
  , pattern UnAnnotatedCell
  , pattern UnAnnotatedCells
  , cellName
  -- * Telescopes
  , CellTelescope(..)
  ) where

import Data.Functor.Alt

import Control.Applicative

import Data.Semigroup.Foldable

import Panbench.Grammar
import Panbench.Prelude

-- | A generic binding cell type.
data Cell info arity name ann tm =
  Cell
  { cellInfo  :: info
  , cellNames :: arity name
  , cellTpAnn :: ann tm
  }

--------------------------------------------------------------------------------
-- Instances
--
-- [TODO: Reed M, 27/09/2025] Technically overkill to use Applicative and
-- Alternative here, could be Pointed and stripped down version of Alternative
-- that only provideds 'empty :: f a'.

instance (Default info, Applicative arity, Applicative ann) => Chk name tm (Cell info arity name ann tm) where
  chk nm tm = Cell def (pure nm) (pure tm)

instance (Default info, Applicative ann) => Chks name tm (Cell info [] name ann tm) where
  chks nms tm = Cell def nms (pure tm)

instance (Default info, Applicative arity, Alternative ann) => Syn name (Cell info arity name ann tm) where
  syn nm = Cell def (pure nm) empty

instance (Default info, Alternative ann) => Syns name (Cell info [] name ann tm) where
  syns nms = Cell def nms empty

instance (Default info, Alternative arity, Applicative ann) => AnonChk tm (Cell info arity name ann tm) where
  anonChk tm = Cell def empty (pure tm)

--------------------------------------------------------------------------------
-- Pattern Synonyms

type MultiCell info name tm = Cell info [] name Maybe tm
type SingleCell info name tm = Cell info Identity name Maybe tm
type RequiredCell info name tm = Cell info Identity name Identity tm

pattern SingleCell :: info -> name -> ann tm -> Cell info Identity name ann tm
pattern SingleCell info name tm = Cell info (Identity name) tm
{-# COMPLETE SingleCell #-}

pattern RequiredCell :: info -> name -> tm -> Cell info Identity name Identity tm
pattern RequiredCell info name tm = Cell info (Identity name) (Identity tm)
{-# COMPLETE RequiredCell #-}

-- | Get the bound name from a 'SingleCell'.
cellName :: SingleCell info name tm -> name
cellName = runIdentity . cellNames

pattern UnAnnotatedCell :: (Cell info arity name Maybe tm) -> (Cell info arity name Maybe tm)
pattern UnAnnotatedCell cell <- cell@(Cell _ _ Nothing)
  where
    UnAnnotatedCell cell = cell { cellTpAnn = Nothing }

pattern UnAnnotatedCells :: [Cell info arity name Maybe tm] -> [Cell info arity name Maybe tm]
pattern UnAnnotatedCells cells <- (traverse (\case (UnAnnotatedCell cell) -> Just cell; _ -> Nothing) -> Just cells)
  where
    UnAnnotatedCells cells = fmap (\cell -> cell { cellTpAnn = Nothing }) cells

--------------------------------------------------------------------------------
-- Telescopes

data CellTelescope argInfo argArity argName argAnn argTm hdInfo hdArity hdName hdAnn hdTm =
  [Cell argInfo argArity argName argAnn argTm] :- (Cell hdInfo hdArity hdName hdAnn hdTm)

instance TelescopeLhs
  (CellTelescope argInfo argArity argName argAnn argTm hdInfo hdArity hdName hdAnn hdTm)
  (Cell hdInfo hdArity hdName hdAnn hdTm)
  (Cell argInfo argArity argName argAnn argTm)
  where
    (|-) = (:-)
