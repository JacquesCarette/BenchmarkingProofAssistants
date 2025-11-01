{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Pretty printer for Idris.
module Panbench.Grammar.Idris
  ( Idris
  , IdrisMod(..)
  , IdrisHeader(..)
  , IdrisDefn(..)
  ) where

import Prelude hiding (pi)

import Data.Coerce
import Data.Default
import Data.String (IsString(..))
import Data.Functor.Identity
import Data.Maybe
import Data.Text (Text)

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Pretty

data Idris

--------------------------------------------------------------------------------
-- Names

newtype IdrisName = IdrisName (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name IdrisName where
  nameN x i = x <> pretty i

--------------------------------------------------------------------------------
-- Cells

data IdrisVis
  = Visible
  | Implicit
  deriving (Eq)

instance Default IdrisVis where
  def = Visible

type IdrisMultiCell info = MultiCell info IdrisName IdrisTm
type IdrisSingleCell info = SingleCell info IdrisName IdrisTm
type IdrisAnonCell info = Cell info Maybe IdrisName Maybe IdrisTm
type IdrisRequiredCell info = Cell info Identity IdrisName Identity IdrisTm

type IdrisTelescope hdInfo hdAnn = CellTelescope
   IdrisVis [] IdrisName Maybe IdrisTm
   hdInfo Identity IdrisName hdAnn IdrisTm

instance Implicit (Cell IdrisVis arity name ann tm) where
  implicit cell = cell { cellInfo = Implicit }

-- | Apply a Idris visibility modifier to a document.
idrisVis :: (IsDoc doc, IsString doc) => IdrisVis -> doc -> doc
idrisVis Visible = enclose "(" ")"
idrisVis Implicit = enclose "{" "}"

-- | Check if a cell is visible.
isVisible :: IdrisVis -> Bool
isVisible Visible = True
isVisible Implicit = False

-- | Render an Idris binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by folding
-- over the annotation with @foldrr const underscore@.
--
-- We also don't support a general @idrisCells@ function: Idris *really* does not like multi-cells,
-- so we should handle these on a case-by-case basis.
idrisCell
  :: (Foldable arity, Foldable tpAnn , IsDoc doc, IsString doc)
  => Cell IdrisVis arity IdrisName tpAnn IdrisTm
  -> doc
idrisCell (Cell vis names tp) = doc $ idrisVis vis (hsepMap coerce (punctuate "," names) <+> ":" <+> undoc (foldr const underscore tp))

-- | Render a list of idris binding cells as function arguments.
idrisArgs
  :: (Foldable arity, Foldable tpAnn , IsDoc doc, IsString doc)
  => [Cell IdrisVis arity IdrisName tpAnn IdrisTm]
  -> doc
idrisArgs =
  doc . hsepMap (hsepMap undoc . cellNames) . filter (isVisible . cellInfo)

--------------------------------------------------------------------------------
-- Top-level definitions

newtype IdrisDefn = IdrisDefn [Doc Ann]
  deriving newtype (Semigroup, Monoid)

idrisDefn :: Doc Ann -> IdrisDefn
idrisDefn = IdrisDefn . pure

type IdrisTmDefnLhs = IdrisTelescope () Maybe

instance Definition IdrisDefn IdrisTmDefnLhs IdrisTm where
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell _ nm _)) .= tm =
    -- Unclear if Idris supports unannotated top-level bindings?
    idrisDefn $
    nest 2 (undoc nm <+> ":" <+> "_") <\>
    nest 2 (undoc nm <+> idrisArgs tele <> listAlt tele mempty space <> "=" <\?> undoc tm)
  (tele :- SingleCell _ nm tp) .= tm =
    idrisDefn $
    nest 2 (undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore tp))) <\>
    nest 2 (undoc nm <+> idrisArgs tele <> listAlt tele mempty space <> "=" <\?> undoc tm)

type IdrisPostulateDefnLhs = IdrisTelescope () Identity

-- | Idris 2 does not support postulates OOTB, so we need to use the @believe_me : a -> b@
-- primitive to do an unsafe cast. Somewhat annoyingly, we need to actually pick *something*
-- to cast, and that thing really should vary based on the goal (EG: @believe_me Refl@ for equality, etc).
--
-- Pulling on this thread leads to a heap of issues with implicit resolution and requires our postulate
-- code to be type-aware, so we just opt to punt and always use @believe_me ()@. This is
-- unsafe and could lead to segfaults in compiled code, but the alternative is not worth the engineering effort.
instance Postulate IdrisDefn IdrisPostulateDefnLhs where
  postulate (tele :- RequiredCell _ nm tp) =
    (tele :- SingleCell () nm (Just tp)) .= "believe_me" <+> "()"

type IdrisDataDefnLhs = IdrisTelescope () Identity

instance DataDefinition IdrisDefn IdrisDataDefnLhs (IdrisRequiredCell ()) where
  -- It appears that Idris 2 does not support parameterised inductives?
  data_ (params :- RequiredCell _ nm tp) ctors =
    idrisDefn $
    nest 2 $
    "data" <+> undoc nm <+> ":" <+> group (undoc (pi params tp) <> line <> "where") <\>
      hardlinesFor ctors \(RequiredCell _ ctorNm ctorTp) ->
        -- We need to add the parameters as arguments, as Idris does not support parameterised inductives.
        undoc ctorNm <+> ":" <+> nest 2 (undoc (pi params ctorTp))

type IdrisRecordDefnLhs = IdrisTelescope () Identity

instance RecordDefinition IdrisDefn IdrisRecordDefnLhs IdrisName (IdrisRequiredCell ()) where
  -- Idris does not have universe levels so it does not allow for a sort annotation
  -- on a record definition.
  record_ (params :- (RequiredCell _ nm _)) ctor fields =
    idrisDefn $
    nest 2 $
    "record" <+> undoc nm <+> hsepMap idrisCell params <> listAlt params mempty space <> "where" <\>
      "constructor" <+> undoc ctor <\>
      hardlinesFor fields \(RequiredCell _ fieldNm fieldTp) ->
        undoc fieldNm <+> ":" <+> undoc fieldTp

instance Newline IdrisDefn where
  newlines n = idrisDefn $ duplicate (fromIntegral n) hardline

--------------------------------------------------------------------------------
-- Let Bindings

newtype IdrisLet = IdrisLet (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

type IdrisLetDefnLhs = IdrisTelescope () Maybe

-- | The grammar of Idris let bindings is a bit complicated, as it has
-- two separate tokens for definitions: @=@ and @:=@.
-- This is used to avoid the gramatical ambiguity caused by
--
-- @
-- let ty : Type = v = v in ty
-- @
--
-- which can parse as either
--
-- @
-- let ty : (Type = v) = v in ty
-- @
--
-- or
--
-- @
-- let ty : Type = (v = v) in ty
-- @
--
-- To further complicate matters, we can't use the @:=@ token when introducing a local definition.
-- This is used to resolve ambiguities like
--
-- @
-- let fo : m -> a -> m
--    fo ac el = ac <+> f el
--    initial := neutral
-- in foldl fo initial
-- @
--
-- We opt to use @:=@ whenever we can, and only fall back to @=@ when creating a parameterised definition.
-- This still leaves some space for generating ambigious code like
--
-- @
-- let ugh : a -> Type
--     ugh x = x = x
-- in ...
-- @
--
-- This is a fundamental flaw with the grammar. We could fix this by conditionally inserting parens,
-- but this is more effort than it's worth.
--
-- See https://idris2.readthedocs.io/en/latest/tutorial/typesfuns.html#let-bindings for more.
instance Definition IdrisLet IdrisLetDefnLhs IdrisTm where
  ([] :- SingleCell _ nm tp) .= tm =
    -- Unparameterised binding, use @:=@ with an inline type annotation.
    doc $
    undoc nm <> undoc (maybe mempty (":" <+>) tp) <+> ":=" <\?> undoc tm
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell _ nm _)) .= tm =
    -- Unannotated parameterised binding: omit the signature, and use @=@.
    doc $
    undoc nm <+> idrisArgs tele <+> "=" <\?> undoc tm
  (tele :- SingleCell _ nm tp) .= tm =
    -- Annotated parameterised binding, generate a signature, and use @=@.
    doc $
    hardlines
    [ undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore tp))
    , undoc nm <+> idrisArgs tele <+> "=" <\?> undoc tm
    ]

instance Let IdrisLet IdrisTm where
  let_ defns e =
    -- [FIXME: Reed M, 28/09/2025] Try to lay things out in a single line if we can.
    doc $
    "let" <+> undoc (nest 4 $ hardlines defns) <\> "in" <+> undoc e

--------------------------------------------------------------------------------
-- Terms

newtype IdrisTm = IdrisTm (Doc Ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name IdrisTm where
  nameN x i = x <> pretty i

instance Pi IdrisTm (IdrisMultiCell IdrisVis) where
  pi args body = group $ align (foldr (\arg tp -> idrisCell arg <+> "->" <> line <> tp) body args)

instance Arr IdrisTm (IdrisAnonCell IdrisVis) where
  arr (Cell _ _ tp) body = fromMaybe underscore tp <+> "->" <+> body

instance App IdrisTm where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Underscore IdrisTm where
  underscore = "_"

instance Parens IdrisTm where
  parens = enclose "(" ")"

--------------------------------------------------------------------------------
-- Builtins

instance Builtin IdrisTm "Nat" IdrisTm where
  mkBuiltin = "Nat"

instance Literal IdrisTm "Nat" Natural where
  mkLit n = pretty n

instance Builtin IdrisTm "suc" (IdrisTm -> IdrisTm) where
  mkBuiltin x = "S" <+> x

instance Builtin IdrisTm "+" (IdrisTm -> IdrisTm -> IdrisTm) where
  mkBuiltin x y = x <+> "+" <+> y

instance Builtin IdrisTm "Type" IdrisTm where
  mkBuiltin = "Type"

--------------------------------------------------------------------------------
-- Modules

newtype IdrisMod = IdrisMod { getIdrisMod :: Doc Ann }
  deriving newtype (Semigroup, Monoid, IsString)

newtype IdrisHeader = IdrisHeader [Doc Ann]
  deriving newtype (Semigroup, Monoid)

instance Module IdrisMod IdrisHeader IdrisDefn where
  -- [FIXME: Reed M, 30/09/2025] Adapted from existing code, why do we use @module Main@?
  module_ _ (IdrisHeader header) (IdrisDefn body) =
    doc $ hardlines
    [ "module Main"
    , if null header then mempty else hardline <> hardlines header
    , hardlines (punctuate hardline body)
    , mempty
    , "main : IO ()"
    , "main = putStrLn \"\""
    ]

--------------------------------------------------------------------------------
-- Imports

idrisImport :: Text -> IdrisHeader
idrisImport nm = IdrisHeader ["import" <+> pretty nm]

instance Import IdrisHeader "Data.Nat" where
  mkImport = mempty
