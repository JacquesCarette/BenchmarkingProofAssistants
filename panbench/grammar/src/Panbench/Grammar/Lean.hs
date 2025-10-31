{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Pretty printer for Lean 4.
module Panbench.Grammar.Lean
  ( Lean
  , LeanMod(..)
  , LeanHeader(..)
  , LeanDefn(..)
  ) where

import Data.Coerce
import Data.Default
import Data.Functor.Identity
import Data.Maybe
import Data.String (IsString(..))

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Pretty

-- | Type-level symbol for Lean.
data Lean

--------------------------------------------------------------------------------
-- Names

newtype LeanName = LeanName (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name LeanName where
  nameN x = subscript x

--------------------------------------------------------------------------------
-- Cells

data LeanVis
  = Visible
  -- ^ Visible arguments.
  | Implicit
  -- ^ Implicit arguments, which are written as @{x}@.
  | SemiImplicit
  -- ^ Semi-implicit arguments, which are written as @{{x}}@.
  deriving (Eq)

instance Default LeanVis where
  def = Visible

type LeanMultiCell info = MultiCell info LeanName LeanTm
type LeanSingleCell info = SingleCell info LeanName LeanTm
type LeanAnonCell info = Cell info Maybe LeanName Maybe LeanTm
type LeanRequiredCell info = Cell info Identity LeanName Identity LeanTm

type LeanTelescope hdInfo hdAnn = CellTelescope
   LeanVis [] LeanName Maybe LeanTm
   hdInfo Identity LeanName hdAnn LeanTm

instance Implicit (Cell LeanVis arity name ann tm) where
  implicit cell = cell { cellInfo = Implicit }

-- | Apply a lean 4 visibility modifier to a document.
leanVis :: (IsDoc doc, IsString doc) => LeanVis -> doc -> doc
leanVis Visible = enclose "(" ")"
leanVis Implicit = enclose "{" "}"
leanVis SemiImplicit = enclose "{{" "}}"

-- | Render a Lean binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
leanCell
  :: (Foldable arity, Foldable tpAnn , IsDoc doc, IsString doc)
  => Cell LeanVis arity LeanName tpAnn LeanTm
  -> doc
leanCell (Cell vis names tp)
  | null tp = leanVis vis (hsepMap coerce names)
  | otherwise = leanVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

leanCells
  :: (Foldable arity, Foldable tpAnn, IsDoc doc, Monoid doc, IsString doc)
  => [Cell LeanVis arity LeanName tpAnn LeanTm]
  -> doc
leanCells cells = hsepMap leanCell cells <> listAlt cells mempty space

--------------------------------------------------------------------------------
-- Top-level definitions

newtype LeanDefn = LeanDefn [Doc Ann]
  deriving newtype (Semigroup, Monoid)

leanDef :: Doc Ann -> LeanDefn
leanDef = LeanDefn . pure

type LeanTmDefnLhs = LeanTelescope () Maybe

instance Definition LeanDefn LeanTmDefnLhs LeanTm where
  (tele :- SingleCell _ nm tp) .= tm =
    leanDef $
    nest 2 $
    "def" <+> undoc nm <+> leanCells tele <> undoc (maybe mempty (":" <+>) tp) <+> ":=" <\?>
      undoc tm

type LeanPostulateDefnLhs = LeanTelescope () Identity

instance Postulate LeanDefn LeanPostulateDefnLhs where
  postulate (tele :- RequiredCell _ nm tp) =
    leanDef $
    nest 2 $
    "axiom" <+> undoc nm <+> leanCells tele <> ":" <+> undoc tp

type LeanDataDefnLhs = LeanTelescope () Identity

instance DataDefinition LeanDefn LeanDataDefnLhs (LeanRequiredCell ()) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    leanDef $ hardlines
    [ nest 2 $
      "inductive" <+> undoc nm <+> leanCells params <> ":" <+> undoc tp <+> "where" <\>
        hardlinesFor ctors \(RequiredCell _ ctorNm ctorTp) ->
          "|" <+> undoc ctorNm <+> ":" <+> nest 2 (undoc ctorTp)
    , mempty
    , "open" <+> undoc nm
    ]

type LeanRecordDefnLhs = LeanTelescope () Identity

instance RecordDefinition LeanDefn LeanRecordDefnLhs LeanName (LeanRequiredCell ()) where
  record_ (params :- RequiredCell _ nm tp) ctor fields =
    leanDef $
    hardlines
    [ nest 2 $
      hardlines
      [ "structure" <+> undoc nm <+> leanCells params <> ":" <+> undoc tp <+> "where"
      , undoc ctor <+> "::"
      , hardlinesFor fields \(RequiredCell _ fieldNm fieldTp) ->
          undoc fieldNm <+> ":" <+> undoc fieldTp
      ]
    , mempty
    , "open" <+> undoc nm
    ]

instance Newline LeanDefn where
  newlines n = leanDef $ hardlines (replicate (fromIntegral n) mempty)

--------------------------------------------------------------------------------
-- Let Bindings

newtype LeanLet = LeanLet (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

type LeanLetDefnLhs = LeanTelescope () Maybe

instance Definition LeanLet LeanLetDefnLhs LeanTm where
  (tele :- SingleCell _ nm tp) .= tm =
    doc $
    nest 4 $
    undoc nm <+> leanCells tele <> undoc (maybe mempty (\tp -> ":" <+> tp <> space) tp) <> ":=" <\?> undoc tm

instance Let LeanLet LeanTm where
  let_ defns tm =
    doc $
    undoc (hardlinesMap ("let" <+>) defns) <\>
    undoc tm

--------------------------------------------------------------------------------
-- Terms

newtype LeanTm = LeanTm (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name LeanTm where
  nameN = subscript

instance Pi LeanTm (LeanMultiCell LeanVis) where
  pi args body = group $ align $ foldr (\arg tp -> leanCell arg <+> "→" <> line <> tp) body args

instance Arr LeanTm (LeanAnonCell LeanVis) where
  arr (Cell _ _ ann) body = fromMaybe underscore ann <+> "→" <+> body

instance App LeanTm where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Underscore LeanTm where
  underscore = "_"

instance Parens LeanTm where
  parens = enclose "(" ")"

instance Literal LeanTm "Nat" Natural where
  mkLit = pretty

instance Builtin LeanTm "Nat" LeanTm where
  mkBuiltin = "Nat"

instance Builtin LeanTm "Type" LeanTm where
  mkBuiltin = "Type"

instance Builtin LeanTm "suc" (LeanTm -> LeanTm) where
  mkBuiltin x = "Nat.succ" <+> x

instance Builtin LeanTm "+" (LeanTm -> LeanTm -> LeanTm) where
  mkBuiltin x y = x <+> "+" <+> y

--------------------------------------------------------------------------------
-- Modules

newtype LeanHeader = LeanHeader [Doc Ann]
  deriving newtype (Semigroup, Monoid)

newtype LeanMod = LeanMod { getLeanMod :: Doc Ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Module LeanMod LeanHeader LeanDefn where
  module_ _ (LeanHeader header) (LeanDefn body) =
    doc $ hardlines
    [ hardlines header
    , hardlines (punctuate hardline body)
    ]

--------------------------------------------------------------------------------
-- Imports

-- | The equivalent of @Data.Nat@ is built-in for Lean.
instance Import LeanHeader "Data.Nat" where
  mkImport = mempty

-- | The equivalent of @Data.List@ is built-in for Lean.
instance Import LeanHeader "Data.List" where
  mkImport = mempty
