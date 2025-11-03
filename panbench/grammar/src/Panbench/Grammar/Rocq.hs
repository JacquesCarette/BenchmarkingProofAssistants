{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Pretty printer for Rocq.
module Panbench.Grammar.Rocq
  ( Rocq
  , RocqMod(..)
  , RocqHeader(..)
  , RocqDefn(..)
  ) where

import Prelude hiding (pi)

import Data.Coerce
import Data.Default
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Data.String (IsString(..))
import Data.Text (Text)

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Pretty

-- | Type-level symbol for Rocq.
data Rocq

--------------------------------------------------------------------------------
-- Names

newtype RocqName = RocqnName (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name RocqName where
  nameN x i = x <> pretty i

--------------------------------------------------------------------------------
-- Cells

data RocqVis
  = Visible
  | MaximalImplicit
  -- ^ A maximal implicit, which are written as @{x}@.
  --
  -- These always get fully instantiated when a function with implicit
  -- arguments is partially applied.
  -- See https://rocq-prover.org/doc/V9.0.0/refman/language/extensions/implicit-arguments.html
  | NonMaximalImplicit
  -- ^ A non-maximal implicit, which are written as @[x]@.
  --
  -- These do not get automatically instantiated when a function with implicit
  -- arguments is partially applied.
  -- See https://rocq-prover.org/doc/V9.0.0/refman/language/extensions/implicit-arguments.html
  deriving (Eq)

instance Default RocqVis where
  def = Visible

type RocqMultiCell info = MultiCell info RocqName RocqTm
type RocqSingleCell info = SingleCell info RocqName RocqTm
type RocqAnonCell info = Cell info Maybe RocqName Maybe RocqTm
type RocqRequiredCell info = Cell info Identity RocqName Identity RocqTm

type RocqTelescope hdInfo hdAnn = CellTelescope
   RocqVis [] RocqName Maybe RocqTm
   hdInfo Identity RocqName hdAnn RocqTm

instance Implicit (Cell RocqVis arity nm ann tm) where
  implicit cell = cell { cellInfo = MaximalImplicit }

-- | Apply a Rocq visibility modifier to a document.
rocqVis :: (IsDoc doc, IsString doc) => RocqVis -> doc -> doc
rocqVis Visible = enclose "(" ")"
rocqVis MaximalImplicit = enclose "{" "}"
rocqVis NonMaximalImplicit = enclose "[" "]"

-- | Check if a cell is visible.
isVisible :: RocqVis -> Bool
isVisible Visible = True
isVisible _ = False

-- | Render a Rocq binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
rocqCell
  :: (Foldable arity, Foldable tpAnn , IsDoc doc, IsString doc)
  => Cell RocqVis arity RocqName tpAnn RocqTm
  -> doc
rocqCell (Cell vis names tp)
  | null tp = rocqVis vis (hsepMap coerce names)
  | otherwise = rocqVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

-- | Render a list of Rocq binding cells, and add a final space if the list is non-empty.
rocqCells
  :: (Foldable arity, Foldable tpAnn, IsDoc doc, Monoid doc, IsString doc)
  => [Cell RocqVis arity RocqName tpAnn RocqTm]
  -> doc
rocqCells cells = listAlt cells mempty (hsepMap rocqCell cells <> space)

--------------------------------------------------------------------------------
-- Top-level definitions

newtype RocqDefn = RocqDefn [Doc Ann]
  deriving newtype (Semigroup, Monoid)

defn :: Doc Ann -> RocqDefn
defn = RocqDefn . pure

catDefns :: [Doc Ann] -> RocqDefn
catDefns = RocqDefn

type RocqTmDefnLhs = RocqTelescope () Maybe

instance Definition RocqDefn RocqTmDefnLhs RocqTm where
  (tele :- SingleCell _ nm tp) .= tm =
    defn $
    nest 4 $
    "Definition" <+> undoc nm <+> rocqCells tele <> undoc (maybe mempty (":" <+>) tp) <+> ":=" <\?>
      undoc tm <> "."

type RocqPostulateDefnLhs = RocqTelescope () Identity

instance Postulate RocqDefn RocqPostulateDefnLhs where
  postulate defns =
    defn $ hardlines $
    defns <&> \(tele :- RequiredCell _ nm tp) ->
      nest 4 $
      "Axiom" <+> undoc nm <+> ":" <\?>
        undoc (pi tele tp) <> "."

type RocqDataDefnLhs = RocqTelescope () Identity

instance DataDefinition RocqDefn RocqDataDefnLhs (RocqRequiredCell ()) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    defn $
    "Inductive" <+> undoc nm <+> rocqCells params <> ":" <+> undoc tp <+> ":=" <\>
    hardlinesFor ctors (\(RequiredCell _ nm tp) -> nest 4 ("|" <+> undoc nm <+> ":" <\?> undoc tp)) <> "."

-- [TODO: Reed M, 29/09/2025] Technically rocq can omit type signatures on records.
type RocqRecordDefnLhs = RocqTelescope () Identity

instance RecordDefinition RocqDefn RocqRecordDefnLhs RocqName (RocqRequiredCell ()) where
  record_ (params :- (RequiredCell _ nm tp)) ctor fields
    | all (not . isVisible . cellInfo) params =
      defn $ hardlines $
      [ nest 2 $
        "Record" <+> undoc nm <+> rocqCells params <> ":" <+> undoc tp <+> ":=" <+> undoc ctor <>
        group (line <> "{ " <> hcat (punctuate (line' <> "; ") (fields <&> \(RequiredCell _ nm tp) -> undoc nm <+> ":" <+> undoc tp)) <> line <> "}.")
      ]
    | otherwise =
      defn $ hardlines $
      [ nest 2 $
        "Record" <+> undoc nm <+> rocqCells params <> ":" <+> undoc tp <+> ":=" <+> undoc ctor <>
        group (line <> "{ " <> hcat (punctuate (line' <> "; ") (fields <&> \(RequiredCell _ nm tp) -> undoc nm <+> ":" <+> undoc tp)) <> line <> "}.")
      , mempty
      , "Arguments" <+> undoc ctor <+> hsepMap (hsepMap (rocqVis MaximalImplicit . undoc) . cellNames) params <+> hsepMap (const "_") fields <> "."
      ]

instance Newline RocqDefn where
  newlines n = defn $ duplicate (fromIntegral n) hardline

--------------------------------------------------------------------------------
-- Let Bindings
--
-- Right now, these are identical to top-level bindings, but in the future they
-- will include different left-hand sides.

newtype RocqLet = RocqLet (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

type RocqLetDefnLhs = RocqTelescope () Maybe

instance Definition (RocqLet) RocqLetDefnLhs RocqTm where
  (tele :- (SingleCell _ nm tp)) .= tm =
    doc $
    nest 4 $
    undoc nm <+> rocqCells tele <> undoc (maybe mempty (\tp -> ":" <+> tp <> space) tp) <> ":=" <\?> undoc tm

instance Let RocqLet RocqTm where
  let_ defns tm =
    doc $ group $ foldr (\defn e -> "let" <+> undoc defn <> line <> "in" <+> e) (undoc tm) defns

--------------------------------------------------------------------------------
-- Terms

newtype RocqTm = RocqTm (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name RocqTm where
  nameN x i = x <> pretty i

instance Pi RocqTm (RocqMultiCell RocqVis) where
  pi [] body = body
  pi args tp = "forall" <+> hsepMap rocqCell args <> "," <\?> tp

instance Arr RocqTm (RocqAnonCell RocqVis) where
  arr (Cell _ _ arg) tp = fromMaybe underscore arg <+> "->" <+> tp

instance App RocqTm where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Underscore RocqTm where
  underscore = "_"

instance Parens RocqTm where
  parensN n = enclose (duplicate (fromIntegral n) "(") (duplicate (fromIntegral n) ")")

instance Literal RocqTm "Nat" Natural where
  mkLit = pretty

instance Builtin RocqTm "Nat" RocqTm where
  mkBuiltin = "nat"

instance Builtin RocqTm "Type" RocqTm where
  mkBuiltin = "Type"

instance Builtin RocqTm "suc" (RocqTm -> RocqTm) where
  mkBuiltin x = "S" <+> x

instance Builtin RocqTm "+" (RocqTm -> RocqTm -> RocqTm) where
  mkBuiltin x y = x <+> "+" <+> y

--------------------------------------------------------------------------------
-- Modules

newtype RocqHeader = RocqHeader [Doc Ann]
  deriving newtype (Semigroup, Monoid)

newtype RocqMod = RocqMod { getRocqMod :: Doc Ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Module RocqMod RocqHeader RocqDefn where
  module_ nm (RocqHeader header) (RocqDefn body) =
    doc $ hardlines
    [ if null header then mempty else hardline <> hardlines header
    , "Module" <+> pretty nm <> "."
    , mempty
    , hardlines (punctuate hardline body)
    , mempty
    , "End" <+> pretty nm <> "."
    ]

--------------------------------------------------------------------------------
-- Imports

requireImport :: Text -> RocqHeader
requireImport m = RocqHeader ["Require" <+> "Import" <+> pretty m <> "."]

justImport :: Text -> RocqHeader
justImport m = RocqHeader ["Import" <+> pretty m <> "."]

-- | The equivalent of @Data.Nat@ is built-in for Rocq.
instance Import (RocqHeader) "Data.Nat" where
  mkImport = mempty

-- | The equivalent of @Data.List@ is built-in for Rocq.
instance Import (RocqHeader) "Data.List" where
  mkImport = mempty
