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

import Panbench.Grammar
import Panbench.Prelude

-- | A generic binding cell type.
data Cell arity name ann tm = Cell
  { cellNames :: arity name
  , cellTpAnn :: ann tm
  }

--------------------------------------------------------------------------------
-- Instances
--
-- We could try and be cute here and use OVERLAPPING instances to try
-- and avoid writing explicit instances for all our different types of arity,
-- but this ends up giving horrible type errors if you do mess up.
--
-- [TODO: Reed M, 27/09/2025] Technically overkill to use Applicative and
-- Alternative here, could be Pointed and stripped down version of Alternative
-- that only provideds 'empty :: f a'.

instance (Alternative arity, Applicative ann) => Binder None name Single tm (Cell arity name ann tm) where
  binder _ (Single tp) = Cell empty (pure tp)

instance (Applicative arity, Alternative ann) => Binder Single name None tm (Cell arity name ann tm) where
  binder (Single nm) _ = Cell (pure nm) empty

instance (Applicative arity, Applicative ann) => Binder Single name Single tm (Cell arity name ann tm) where
  binder (Single nm) (Single tp) = Cell (pure nm) (pure tp)

instance (Alternative arity, Alt arity, Applicative arity, Applicative ann) => Binder [] name Single tm (Cell arity name ann tm) where
  binder nms (Single tp) = Cell (asumMap pure nms) (pure tp)

instance (Alternative arity, Alt arity, Applicative arity, Alternative ann) => Binder [] name None tm (Cell arity name ann tm) where
  binder nms _ = Cell (asumMap pure nms) empty

--------------------------------------------------------------------------------
-- Pattern Synonyms

type MultiCell name tm = Cell [] name Maybe tm
type SingleCell name tm = Cell Single name Maybe tm
type RequiredCell name tm = Cell Single name Single tm

pattern SingleCell :: name -> ann tm -> Cell Single name ann tm
pattern SingleCell name tm = Cell (Single name) tm
{-# COMPLETE SingleCell #-}

pattern RequiredCell :: name -> tm -> Cell Single name Single tm
pattern RequiredCell name tm = Cell (Single name) (Single tm)
{-# COMPLETE RequiredCell #-}

-- | Get the bound name from a 'SingleCell'.
cellName :: SingleCell name tm -> name
cellName = unSingle . cellNames

pattern UnAnnotatedCell :: (Cell arity name Maybe tm) -> (Cell arity name Maybe tm)
pattern UnAnnotatedCell cell <- cell@(Cell _ Nothing)
  where
    UnAnnotatedCell cell = cell { cellTpAnn = Nothing }

pattern UnAnnotatedCells :: [Cell arity name Maybe tm] -> [Cell arity name Maybe tm]
pattern UnAnnotatedCells cells <- (traverse (\case (UnAnnotatedCell cell) -> Just cell; _ -> Nothing) -> Just cells)
  where
    UnAnnotatedCells cells = fmap (\cell -> cell { cellTpAnn = Nothing }) cells

--------------------------------------------------------------------------------
-- Telescopes

data CellTelescope argArity argName argAnn argTm hdArity hdName hdAnn hdTm =
  [Cell argArity argName argAnn argTm] :- (Cell hdArity hdName hdAnn hdTm)

instance TelescopeLhs
  (Cell argArity argName argAnn argTm)
  (Cell hdArity hdName hdAnn hdTm)
  (CellTelescope argArity argName argAnn argTm hdArity hdName hdAnn hdTm)
  where
    (|-) = (:-)
