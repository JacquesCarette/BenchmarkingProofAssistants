{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Pretty printer for Agda.
module Panbench.Grammar.Agda
  ( Agda
  , AgdaMod(..)
  , AgdaHeader(..)
  , AgdaDefn(..)
  , AgdaTm(..)
  ) where

import Prelude hiding (pi)

import Data.Coerce
import Data.Default
import Data.Functor.Identity
import Data.Maybe
import Data.String (IsString(..))
import Data.Text (Text)

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Pretty

data Agda ann

newtype AgdaTm ann = AgdaTm (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

newtype AgdaName ann = AgdaName (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (AgdaName ann) where
  nameN = subscript

--------------------------------------------------------------------------------
-- Cells

data AgdaVis
  = Visible
  -- ^ Visible arguments like @(x : A)@.
  | Implicit
  -- ^ Implicit arguments like @(x : A)@.
  deriving (Eq)

instance Default AgdaVis where
  def = Visible

type AgdaMultiCell info ann = MultiCell info (AgdaName ann) (AgdaTm ann)
type AgdaSingleCell info ann = SingleCell info (AgdaName ann) (AgdaTm ann)
type AgdaAnonCell info ann = Cell info Maybe (AgdaName ann) Maybe (AgdaTm ann)
type AgdaRequiredCell info ann = Cell info Identity (AgdaName ann) Identity (AgdaTm ann)

type AgdaTelescope hdInfo hdAnn docAnn = CellTelescope
   AgdaVis [] (AgdaName docAnn) Maybe (AgdaTm docAnn)
   hdInfo Identity (AgdaName docAnn) hdAnn (AgdaTm docAnn)

instance Implicit (Cell AgdaVis arity nm ann tm) where
  implicit cell = cell { cellInfo = Implicit }

-- | Surround a document with the appropriate delimiters for a given 'Visibility'.
agdaVis :: (IsDoc doc, IsString (doc ann)) => AgdaVis -> doc ann -> doc ann
agdaVis Visible = enclose "(" ")"
agdaVis Implicit = enclose "{" "}"

-- | Render an Agda binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
agdaCell :: (Foldable arity, Foldable tpAnn, IsDoc doc, IsString (doc ann)) => Cell AgdaVis arity (AgdaName ann) tpAnn (AgdaTm ann) -> doc ann
agdaCell (Cell vis names tp) | null tp = agdaVis vis (hsepMap coerce names)
                             | otherwise = agdaVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

-- | Render a list of Agda binding cells, and add a final space if the list is non-empty
agdaCells :: (Foldable arity, Foldable tpAnn, IsDoc doc, Monoid (doc ann), IsString (doc ann)) => [Cell AgdaVis arity (AgdaName ann) tpAnn (AgdaTm ann)] -> doc ann
agdaCells [] = mempty
agdaCells cells = hsepMap agdaCell cells <> space

-- | Render the names of a list of Agda binding cells, and add a final space if the list is non-empty.
agdaCellNames :: (Foldable arity, Foldable tpAnn, IsDoc doc, Monoid (doc ann), IsString (doc ann)) => [Cell AgdaVis arity (AgdaName ann) tpAnn (AgdaTm ann)] -> doc ann
agdaCellNames [] = mempty
agdaCellNames cells = coerce (hsepMap (hsep . cellNames) cells <> space)

--------------------------------------------------------------------------------
-- Top-level definitions

newtype AgdaDefn ann = AgdaDefn [Doc ann]
  deriving newtype (Semigroup, Monoid)

agdaDefn :: Doc ann -> AgdaDefn ann
agdaDefn = AgdaDefn . pure

type AgdaTmDefnLhs ann = AgdaTelescope () Maybe ann

instance Definition (AgdaDefn ann) (AgdaTmDefnLhs ann) (AgdaTm ann) where
  (UnAnnotatedCells tele :- SingleCell _ nm Nothing) .= e =
    agdaDefn $
    undoc nm <+> agdaCells tele <> "=" <\?> undoc e

  (tele :- SingleCell _ nm ann) .= e =
    agdaDefn $
    hardlines
    [ nest 2 $ undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore ann))
    , nest 2 $ undoc nm <+> agdaCells tele <> "=" <\?> undoc e
    ]

type AgdaPostulateDefnLhs ann = AgdaTelescope () Identity ann

instance Postulate (AgdaDefn ann) (AgdaPostulateDefnLhs ann) where
  postulate (tele :- RequiredCell _ nm tp) =
    agdaDefn $
    nest 2 $
    hardlines
    [ "postulate"
    , nest 2 (undoc nm <+> ":" <+> undoc (pi tele tp))
    ]

type AgdaDataDefnLhs ann = AgdaTelescope () Identity ann

instance DataDefinition (AgdaDefn ann) (AgdaDataDefnLhs ann) (AgdaRequiredCell () ann) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    agdaDefn $
    nest 2 $ hardlines
    [ "data" <+> undoc nm <+> agdaCells params <> ":" <+> undoc tp <+> "where"
    , hardlinesFor ctors \(RequiredCell _ nm tp) ->
        nest 2 $ undoc nm <+> ":" <\?> undoc tp
    ]

type AgdaRecordDefnLhs ann = AgdaTelescope () Identity ann

instance RecordDefinition (AgdaDefn ann) (AgdaRecordDefnLhs ann) (AgdaName ann) (AgdaRequiredCell () ann) where
  record_ (params :- RequiredCell _ nm tp) ctor fields =
    agdaDefn $
    hardlines
    [ nest 2 $ hardlines
      [ "record" <+> undoc nm <+> agdaCells params <> ":" <+> undoc tp <+> "where"
      , "constructor" <+> undoc ctor
      , nest 2 $ hardlines
        [ "fields"
        , hardlinesFor fields \(RequiredCell _ nm tp) ->
          nest 2 $ undoc nm <+> ":" <\?> undoc tp
        ]
      ]
    , "open" <+> undoc nm
    ]

instance Newline (AgdaDefn ann) where
  newlines n = agdaDefn $ hardlines (replicate (fromIntegral n) mempty)

--------------------------------------------------------------------------------
-- Let Bindings
--
-- Right now, these are identical to top-level bindings, but in the future they
-- will include different left-hand sides.

newtype AgdaLet ann = AgdaLet (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

type AgdaLetDefnLhs ann = AgdaTelescope () Maybe ann


instance Definition (AgdaLet ann) (AgdaLetDefnLhs ann) (AgdaTm ann) where
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell _ nm _)) .= e =
    doc $
    undoc nm <+> agdaCellNames tele <> "=" <> group (line <> undoc e)
  (tele :- SingleCell _ nm ann) .= e =
    doc $
    hardlines
    [ undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore ann))
    , undoc nm <+> agdaCellNames tele <> "=" <> group (line <> undoc e)
    ]

instance Let (AgdaLet ann) (AgdaTm ann) where
  let_ [] e = e
  let_ defns e =
    doc $
    "let" <+> nest 4 (hardlines (undocs defns)) <> line <> "in" <+> nest 3 (undoc e)

--------------------------------------------------------------------------------
-- Terms

instance Name (AgdaTm ann) where
  nameN = subscript

instance Pi (AgdaTm ann) (AgdaMultiCell AgdaVis ann) where
  pi [] body = body
  pi args body = agdaCells args <> "→" <+> body

instance Arr (AgdaTm ann) (AgdaAnonCell AgdaVis ann) where
  arr (Cell _ _ tp) body = fromMaybe underscore tp <+> "→" <+> body

instance App (AgdaTm ann) where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Underscore (AgdaTm ann) where
  underscore = "_"

instance Parens (AgdaTm ann) where
  parens = enclose "(" ")"

instance Literal (AgdaTm ann) "Nat" Natural where
  mkLit = pretty

instance Builtin (AgdaTm ann) "Nat" (AgdaTm ann) where
  mkBuiltin = "Nat"

instance Builtin (AgdaTm ann) "suc" (AgdaTm ann -> AgdaTm ann) where
  mkBuiltin x = "suc" <+> x

instance Builtin (AgdaTm ann) "+" (AgdaTm ann -> AgdaTm ann -> AgdaTm ann) where
  mkBuiltin x y = x <+> "+" <+> y

instance Builtin (AgdaTm ann) "Type" (AgdaTm ann) where
  mkBuiltin = "Set"

--------------------------------------------------------------------------------
-- Modules

newtype AgdaMod ann = AgdaMod { getAgdaMod :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

newtype AgdaHeader ann = AgdaHeader [Doc ann]
  deriving newtype (Semigroup, Monoid)

instance Module (AgdaMod ann) (AgdaHeader ann) (AgdaDefn ann) where
  module_ nm (AgdaHeader header) (AgdaDefn body) =
    doc $ hardlines
    [ "module" <+> pretty nm <+> "where"
    , if null header then mempty else hardline <> hardlines header
    , hardlines (punctuate hardline body)
    , mempty
    ]

--------------------------------------------------------------------------------
-- Imports

openImport :: Text -> AgdaHeader ann
openImport m = AgdaHeader ["open" <+> "import" <+> pretty m <> hardline]

instance Import (AgdaHeader ann) "Data.Nat" where
  mkImport = openImport "Agda.Builtin.Nat"

instance Import (AgdaHeader ann) "Data.List" where
  mkImport = openImport "Agda.Builtin.List"

instance Import (AgdaHeader ann) "Data.String" where
  mkImport = openImport "Agda.Builtin.String"
