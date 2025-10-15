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
data Lean ann

--------------------------------------------------------------------------------
-- Names

newtype LeanName ann = LeanName (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (LeanName ann) where
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

type LeanMultiCell info ann = MultiCell info (LeanName ann) (LeanTm ann)
type LeanSingleCell info ann = SingleCell info (LeanName ann) (LeanTm ann)
type LeanAnonCell info ann = Cell info Maybe (LeanName ann) Maybe (LeanTm ann)
type LeanRequiredCell info ann = Cell info Identity (LeanName ann) Identity (LeanTm ann)

type LeanTelescope hdInfo hdAnn docAnn = CellTelescope
   LeanVis [] (LeanName docAnn) Maybe (LeanTm docAnn)
   hdInfo Identity (LeanName docAnn) hdAnn (LeanTm docAnn)

instance Implicit (Cell LeanVis arity name ann tm) where
  implicit cell = cell { cellInfo = Implicit }

-- | Apply a lean 4 visibility modifier to a document.
leanVis :: (IsDoc doc, IsString (doc ann)) => LeanVis -> doc ann -> doc ann
leanVis Visible = enclose "(" ")"
leanVis Implicit = enclose "{" "}"
leanVis SemiImplicit = enclose "{{" "}}"

-- | Render a Lean binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
leanCell
  :: (Foldable arity, Foldable tpAnn , IsDoc doc, IsString (doc ann))
  => Cell LeanVis arity (LeanName ann) tpAnn (LeanTm ann)
  -> doc ann
leanCell (Cell vis names tp)
  | null tp = leanVis vis (hsepMap coerce names)
  | otherwise = leanVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

leanCells
  :: (Foldable arity, Foldable tpAnn, IsDoc doc, Monoid (doc ann), IsString (doc ann))
  => [Cell LeanVis arity (LeanName ann) tpAnn (LeanTm ann)]
  -> doc ann
leanCells cells = hsepMap leanCell cells <> listAlt cells mempty space

--------------------------------------------------------------------------------
-- Top-level definitions

newtype LeanDefn ann = LeanDefn [Doc ann]
  deriving newtype (Semigroup, Monoid)

leanDef :: Doc ann -> LeanDefn ann
leanDef = LeanDefn . pure

type LeanTmDefnLhs ann = LeanTelescope () Maybe ann

instance Definition (LeanDefn ann) (LeanTmDefnLhs ann) (LeanTm ann) where
  (tele :- SingleCell _ nm tp) .= tm =
    leanDef $
    nest 2 $
    "def" <+> undoc nm <+> leanCells tele <> undoc (maybe mempty (":" <+>) tp) <+> ":=" <\?>
      undoc tm

type LeanPostulateDefnLhs ann = LeanTelescope () Identity ann

instance Postulate (LeanDefn ann) (LeanPostulateDefnLhs ann) where
  postulate (tele :- RequiredCell _ nm tp) =
    leanDef $
    nest 2 $
    "axiom" <+> undoc nm <+> leanCells tele <> ":" <+> undoc tp

type LeanDataDefnLhs ann = LeanTelescope () Identity ann

instance DataDefinition (LeanDefn ann) (LeanDataDefnLhs ann) (LeanRequiredCell () ann) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    leanDef $
    nest 2 $
    "inductive" <+> undoc nm <+> leanCells params <> ":" <+> undoc tp <+> "where" <\>
      hardlinesFor ctors \(RequiredCell _ ctorNm ctorTp) ->
        "|" <+> undoc ctorNm <+> ":" <+> nest 2 (undoc ctorTp)

type LeanRecordDefnLhs ann = LeanTelescope () Identity ann

instance RecordDefinition (LeanDefn ann) (LeanRecordDefnLhs ann) (LeanName ann) (LeanRequiredCell () ann) where
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
    , "open" <+> undoc nm
    ]

instance Newline (LeanDefn ann) where
  newlines n = leanDef $ hardlines (replicate (fromIntegral n) mempty)

--------------------------------------------------------------------------------
-- Let Bindings

newtype LeanLet ann = LeanLet (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

type LeanLetDefnLhs ann = LeanTelescope () Maybe ann

instance Definition (LeanLet ann) (LeanLetDefnLhs ann) (LeanTm ann) where
  (tele :- SingleCell _ nm tp) .= tm =
    doc $
    nest 4 $
    undoc nm <+> leanCells tele <> undoc (maybe mempty (\tp -> ":" <+> tp <> space) tp) <> ":=" <\?> undoc tm

instance Let (LeanLet ann) (LeanTm ann) where
  let_ defns tm =
    doc $
    undoc (hardlinesMap ("let" <+>) defns) <\>
    undoc tm

--------------------------------------------------------------------------------
-- Terms

newtype LeanTm ann = LeanTm (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (LeanTm ann) where
  nameN = subscript

instance Pi (LeanTm ann) (LeanMultiCell LeanVis ann) where
  pi arg body = leanCells arg <> "→" <\?> body

instance Arr (LeanTm ann) (LeanAnonCell LeanVis ann) where
  arr (Cell _ _ ann) body = fromMaybe underscore ann <+> "->" <+> body

instance App (LeanTm ann) where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Underscore (LeanTm ann) where
  underscore = "_"

instance Parens (LeanTm ann) where
  parens = enclose "(" ")"

instance Literal (LeanTm ann) "Nat" Natural where
  mkLit = pretty

instance Builtin (LeanTm ann) "Nat" (LeanTm ann) where
  mkBuiltin = "Nat"

instance Builtin (LeanTm ann) "Type" (LeanTm ann) where
  mkBuiltin = "Type"

instance Builtin (LeanTm ann) "suc" (LeanTm ann -> LeanTm ann) where
  mkBuiltin x = "Nat.succ" <+> x

instance Builtin (LeanTm ann) "+" (LeanTm ann -> LeanTm ann -> LeanTm ann) where
  mkBuiltin x y = x <+> "+" <+> y

--------------------------------------------------------------------------------
-- Modules

newtype LeanHeader ann = LeanHeader [Doc ann]
  deriving newtype (Semigroup, Monoid)

newtype LeanMod ann = LeanMod { getLeanMod :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Module (LeanMod ann) (LeanHeader ann) (LeanDefn ann) where
  module_ _ (LeanHeader header) (LeanDefn body) =
    doc $ hardlines
    [ hardlines header
    , hardlines (punctuate hardline body)
    ]

--------------------------------------------------------------------------------
-- Imports

-- | The equivalent of @Data.Nat@ is built-in for Lean.
instance Import (LeanHeader ann) "Data.Nat" where
  mkImport = mempty

-- | The equivalent of @Data.List@ is built-in for Lean.
instance Import (LeanHeader ann) "Data.List" where
  mkImport = mempty
