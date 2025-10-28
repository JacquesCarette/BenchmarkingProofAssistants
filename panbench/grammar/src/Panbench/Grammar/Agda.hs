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

data Agda

newtype AgdaTm = AgdaTm (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

newtype AgdaName = AgdaName (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name AgdaName where
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

type AgdaMultiCell info = MultiCell info AgdaName AgdaTm
type AgdaSingleCell info = SingleCell info AgdaName AgdaTm
type AgdaAnonCell info = Cell info Maybe AgdaName Maybe AgdaTm
type AgdaRequiredCell info = Cell info Identity AgdaName Identity AgdaTm

type AgdaTelescope hdInfo hdAnn = CellTelescope
   AgdaVis [] AgdaName Maybe AgdaTm
   hdInfo Identity AgdaName hdAnn (AgdaTm)

instance Implicit (Cell AgdaVis arity nm ann tm) where
  implicit cell = cell { cellInfo = Implicit }

-- | Surround a document with the appropriate delimiters for a given 'Visibility'.
agdaVis :: (IsDoc doc, IsString doc) => AgdaVis -> doc -> doc
agdaVis Visible = enclose "(" ")"
agdaVis Implicit = enclose "{" "}"

-- | Render an Agda binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
agdaCell :: (Foldable arity, Foldable tpAnn, IsDoc doc, IsString doc) => Cell AgdaVis arity AgdaName tpAnn AgdaTm -> doc
agdaCell (Cell vis names tp) | null tp = agdaVis vis (hsepMap coerce names)
                             | otherwise = agdaVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

-- | Render a list of Agda binding cells, and add a final space if the list is non-empty
agdaCells :: (Foldable arity, Foldable tpAnn, IsDoc doc, Monoid doc, IsString doc) => [Cell AgdaVis arity AgdaName tpAnn AgdaTm] -> doc
agdaCells [] = mempty
agdaCells cells = hsepMap agdaCell cells <> space

-- | Render the names of a list of Agda binding cells, and add a final space if the list is non-empty.
agdaCellNames :: (Foldable arity, Foldable tpAnn, IsDoc doc, Monoid doc, IsString doc) => [Cell AgdaVis arity AgdaName tpAnn AgdaTm] -> doc
agdaCellNames [] = mempty
agdaCellNames cells = coerce (hsepMap (hsep . cellNames) cells <> space)

--------------------------------------------------------------------------------
-- Top-level definitions

-- | Agda definition groups.
--
-- When rendered, @Doc ann@ inside of the list will be separated by a newline.
newtype AgdaDefn = AgdaDefn [Doc Ann]
  deriving newtype (Semigroup, Monoid)

defn :: Doc Ann -> AgdaDefn
defn = AgdaDefn . pure

sepDefns :: AgdaDefn -> Doc Ann
sepDefns (AgdaDefn defns) = hardlines $ punctuate hardline defns

sepDefnsFor :: (Foldable t) => t a -> (a -> AgdaDefn) -> Doc Ann
sepDefnsFor xs f = sepDefns $ foldMap f xs

type AgdaTmDefnLhs = AgdaTelescope () Maybe

instance Definition AgdaDefn AgdaTmDefnLhs AgdaTm where
  (UnAnnotatedCells tele :- SingleCell _ nm Nothing) .= e =
    defn $
    undoc nm <+> agdaCells tele <> "=" <\?> undoc e

  (tele :- SingleCell _ nm ann) .= e =
    defn $
    hardlines
    [ nest 2 $ undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore ann))
    , nest 2 $ undoc nm <+> agdaCells tele <> "=" <\?> undoc e
    ]

type AgdaPostulateDefnLhs = AgdaTelescope () Identity

instance Postulate AgdaDefn AgdaPostulateDefnLhs where
  postulate defns =
    defn $
    nest 2 $ hardlines
    [ "postulate"
    , sepDefnsFor defns \(tele :- RequiredCell _ nm tp) ->
        defn $ nest 2 (undoc nm <+> ":" <+> undoc (pi tele tp))
    ]

type AgdaDataDefnLhs = AgdaTelescope () Identity

instance DataDefinition AgdaDefn AgdaDataDefnLhs (AgdaRequiredCell ()) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    defn $
    nest 2 $ hardlines
    [ "data" <+> undoc nm <+> agdaCells params <> ":" <+> undoc tp <+> "where"
    , hardlinesFor ctors \(RequiredCell _ nm tp) ->
        nest 2 $ undoc nm <+> ":" <\?> undoc tp
    ]

type AgdaRecordDefnLhs = AgdaTelescope () Identity

instance RecordDefinition AgdaDefn AgdaRecordDefnLhs AgdaName (AgdaRequiredCell ()) where
  record_ (params :- RequiredCell _ nm tp) ctor fields =
    defn $ hardlines
    [ nest 2 $ hardlines
      [ "record" <+> undoc nm <+> agdaCells params <> ":" <+> undoc tp <+> "where"
      , "constructor" <+> undoc ctor
      , nest 2 $ hardlines
        [ "field"
        , hardlinesFor fields \(RequiredCell _ nm tp) ->
          nest 2 $ undoc nm <+> ":" <\?> undoc tp
        ]
      ]
    , mempty
    , "open" <+> undoc nm
    ]

instance Newline AgdaDefn where
  newlines n = defn $ duplicate (fromIntegral n) hardline

--------------------------------------------------------------------------------
-- Let Bindings
--
-- Right now, these are identical to top-level bindings, but in the future they
-- will include different left-hand sides.

newtype AgdaLet = AgdaLet (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

type AgdaLetDefnLhs = AgdaTelescope () Maybe

instance Definition AgdaLet AgdaLetDefnLhs AgdaTm where
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell _ nm _)) .= e =
    doc $
    undoc nm <+> agdaCellNames tele <> "=" <> nest 2 (group (line <> undoc e))
  (tele :- SingleCell _ nm ann) .= e =
    doc $
    hardlines
    [ undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore ann))
    , undoc nm <+> agdaCellNames tele <> "=" <> nest 2 (group (line <> undoc e))
    ]

instance Let (AgdaLet) AgdaTm where
  let_ [] e = e
  let_ defns e =
    doc $
    "let" <+> nest 4 (hardlines (undocs defns)) <> line <> "in" <+> nest 3 (undoc e)

--------------------------------------------------------------------------------
-- Terms

instance Name AgdaTm where
  nameN = subscript

instance Pi AgdaTm (AgdaMultiCell AgdaVis) where
  pi [] body = body
  pi args body = agdaCells args <> "→" <+> body

instance Arr AgdaTm (AgdaAnonCell AgdaVis) where
  arr (Cell _ _ tp) body = fromMaybe underscore tp <+> "→" <+> body

instance App AgdaTm where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Underscore AgdaTm where
  underscore = "_"

instance Parens AgdaTm where
  parens = enclose "(" ")"

instance Literal AgdaTm "Nat" Natural where
  mkLit = pretty

instance Builtin AgdaTm "Nat" AgdaTm where
  mkBuiltin = "Nat"

instance Builtin AgdaTm "suc" (AgdaTm -> AgdaTm) where
  mkBuiltin x = "suc" <+> x

instance Builtin AgdaTm "+" (AgdaTm -> AgdaTm -> AgdaTm) where
  mkBuiltin x y = x <+> "+" <+> y

instance Builtin AgdaTm "Type" AgdaTm where
  mkBuiltin = "Set"

--------------------------------------------------------------------------------
-- Modules

newtype AgdaMod = AgdaMod { getAgdaMod :: Doc Ann }
  deriving newtype (Semigroup, Monoid, IsString)

newtype AgdaHeader = AgdaHeader [Doc Ann]
  deriving newtype (Semigroup, Monoid)

instance Module AgdaMod AgdaHeader AgdaDefn where
  module_ nm (AgdaHeader header) defns =
    doc $ hardlines
    [ "module" <+> pretty nm <+> "where"
    , if null header then mempty else hardline <> hardlines header
    , sepDefns defns
    , mempty
    ]

--------------------------------------------------------------------------------
-- Imports

openImport :: Text -> AgdaHeader
openImport m = AgdaHeader ["open" <+> "import" <+> pretty m <> hardline]

instance Import AgdaHeader "Data.Nat" where
  mkImport = openImport "Agda.Builtin.Nat"

instance Import AgdaHeader "Data.List" where
  mkImport = openImport "Agda.Builtin.List"

instance Import AgdaHeader "Data.String" where
  mkImport = openImport "Agda.Builtin.String"
