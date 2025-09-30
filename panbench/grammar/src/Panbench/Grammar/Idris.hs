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

data Idris ann

--------------------------------------------------------------------------------
-- Names

newtype IdrisName ann = IdrisName (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (IdrisName ann) where
  nameN x i = x <> pretty i

--------------------------------------------------------------------------------
-- Cells

data IdrisVis
  = Visible
  | Implicit
  deriving (Eq)

instance Default IdrisVis where
  def = Visible

type IdrisMultiCell info ann = MultiCell info (IdrisName ann) (IdrisTm ann)
type IdrisSingleCell info ann = SingleCell info (IdrisName ann) (IdrisTm ann)
type IdrisAnonCell info ann = Cell info Maybe (IdrisName ann) Maybe (IdrisTm ann)
type IdrisRequiredCell info ann = Cell info Identity (IdrisName ann) Identity (IdrisTm ann)

type IdrisTelescope hdInfo hdAnn docAnn = CellTelescope
   IdrisVis [] (IdrisName docAnn) Maybe (IdrisTm docAnn)
   hdInfo Identity (IdrisName docAnn) hdAnn (IdrisTm docAnn)

instance Implicit (Cell IdrisVis arity name ann tm) where
  implicit cell = cell { cellInfo = Implicit }

-- | Apply a Idris visibility modifier to a document.
idrisVis :: (IsDoc doc, IsString (doc ann)) => IdrisVis -> doc ann -> doc ann
idrisVis Visible = enclose "(" ")"
idrisVis Implicit = enclose "{" "}"

-- | Render a Rocq binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by folding
-- over the annotation with @foldrr const underscore@.
--
-- We also don't support a general @idrisCells@ function: Idris *really* does not like multi-cells,
-- so we should handle these on a case-by-case basis.
idrisCell
  :: (Foldable arity, Foldable tpAnn , IsDoc doc, IsString (doc ann))
  => Cell IdrisVis arity (IdrisName ann) tpAnn (IdrisTm ann)
  -> doc ann
idrisCell (Cell vis names tp) = doc $ idrisVis vis (hsepMap coerce (punctuate "," names) <+> ":" <+> undoc (foldr const underscore tp))

--------------------------------------------------------------------------------
-- Top-level definitions

newtype IdrisDefn ann = IdrisDefn [Doc ann]
  deriving newtype (Semigroup, Monoid)

idrisDefn :: Doc ann -> IdrisDefn ann
idrisDefn = IdrisDefn . pure

type IdrisTmDefnLhs ann = IdrisTelescope () Maybe ann

instance Definition (IdrisDefn ann) (IdrisTmDefnLhs ann) (IdrisTm ann) where
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell _ nm _)) .= tm =
    -- Unclear if Idris supports unannotated top-level bindings?
    idrisDefn $
    nest 2 (undoc nm <+> ":" <+> "_") <\>
    nest 2 (undoc nm <+> undoc (hsepMap (hsep . cellNames) tele) <> listAlt tele mempty space <> "=" <\?> undoc tm)
  (tele :- SingleCell _ nm tp) .= tm =
    idrisDefn $
    nest 2 (undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore tp))) <\>
    nest 2 (undoc nm <+> undoc (hsepMap (hsep . cellNames) tele) <> listAlt tele mempty space <> "=" <\?> undoc tm)

type IdrisPostulateDefnLhs ann = IdrisTelescope () Identity ann

-- | Idris 2 does not support postulates OOTB, so we need to use the @believe_me : a -> b@
-- primitive to do an unsafe cast. Somewhat annoyingly, we need to actually pick *something*
-- to cast, and that thing really should vary based on the goal (EG: @believe_me Refl@ for equality, etc).
--
-- Pulling on this thread leads to a heap of issues with implicit resolution and requires our postulate
-- code to be type-aware, so we just opt to punt and always use @believe_me ()@. This is
-- unsafe and could lead to segfaults in compiled code, but the alternative is not worth the engineering effort.
instance Postulate (IdrisDefn ann) (IdrisPostulateDefnLhs ann) where
  postulate (tele :- RequiredCell _ nm tp) =
    (tele :- SingleCell () nm (Just tp)) .= "believe_me" <+> "()"

type IdrisDataDefnLhs ann = IdrisTelescope () Identity ann

instance DataDefinition (IdrisDefn ann) (IdrisDataDefnLhs ann) (IdrisRequiredCell () ann) where
  -- It appears that Idris 2 does not support parameterised inductives?
  data_ (params :- RequiredCell _ nm tp) ctors =
    idrisDefn $
    nest 2 $
    "data" <+> undoc nm <+> ":" <+> group (undoc (pi params tp) <> line <> "where") <\>
      hardlinesFor ctors \(RequiredCell _ ctorNm ctorTp) ->
        -- We need to add the parameters as arguments, as Idris does not support parameterised inductives.
        undoc ctorNm <+> ":" <+> nest 2 (undoc (pi params ctorTp))

type IdrisRecordDefnLhs ann = IdrisTelescope () Identity ann

instance RecordDefinition (IdrisDefn ann) (IdrisRecordDefnLhs ann) (IdrisName ann) (IdrisRequiredCell () ann) where
  -- Idris does not have universe levels so it does not allow for a sort annotation
  -- on a record definition.
  record_ (params :- (RequiredCell _ nm _)) ctor fields =
    idrisDefn $
    nest 2 $
    "record" <+> undoc nm <+> hsepMap idrisCell params <+> "where" <\>
      "constructor" <+> undoc ctor <\>
      hardlinesFor fields \(RequiredCell _ fieldNm fieldTp) ->
        undoc fieldNm <+> ":" <+> undoc fieldTp

instance Newline (IdrisDefn ann) where
  newlines n = idrisDefn $ hardlines (replicate (fromIntegral n) mempty)

--------------------------------------------------------------------------------
-- Let Bindings

newtype IdrisLet ann = IdrisLet (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

type IdrisLetDefnLhs ann = IdrisTelescope () Maybe ann

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
instance Definition (IdrisLet ann) (IdrisLetDefnLhs ann) (IdrisTm ann) where
  ([] :- SingleCell _ nm tp) .= tm =
    -- Unparameterised binding, use @:=@ with an inline type annotation.
    doc $
    nest 4 $
    undoc nm <> undoc (maybe mempty (":" <+>) tp) <+> ":=" <\?> undoc tm
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell _ nm _)) .= tm =
    -- Unannotated parameterised binding: omit the signature, and use @=@.
    doc $
    nest 4 $
    undoc nm <+> undoc (hsepMap (hsep . cellNames) tele) <+> "=" <\?> undoc tm
  (tele :- SingleCell _ nm tp) .= tm =
    -- Annotated parameterised binding, generate a signature, and use @=@.
    doc $
    hardlines
    [ nest 4 $ undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore tp))
    , nest 4 $ undoc nm <+> undoc (hsepMap (hsep . cellNames) tele) <+> "=" <\?> undoc tm
    ]

instance Let (IdrisLet ann) (IdrisTm ann) where
  let_ defns e =
    -- [FIXME: Reed M, 28/09/2025] Try to lay things out in a single line if we can.
    doc $
    "let" <+> undoc (hardlines defns) <\> "in" <+> undoc e

--------------------------------------------------------------------------------
-- Terms

newtype IdrisTm ann = IdrisTm (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (IdrisTm ann) where
  nameN x i = x <> pretty i

instance Pi (IdrisTm ann) (IdrisMultiCell IdrisVis ann) where
  pi args body = group $ align (foldr (\arg tp -> idrisCell arg <+> "->" <> line <> tp) body args)

instance Arr (IdrisTm ann) (IdrisAnonCell IdrisVis ann) where
  arr (Cell _ _ tp) body = fromMaybe underscore tp <+> "->" <+> body

instance App (IdrisTm ann) where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Underscore (IdrisTm ann) where
  underscore = "_"

instance Parens (IdrisTm ann) where
  parens = enclose "(" ")"

--------------------------------------------------------------------------------
-- Builtins

instance Builtin (IdrisTm ann) "Nat" (IdrisTm ann) where
  mkBuiltin = "Nat"

instance Literal (IdrisTm ann) "Nat" Natural where
  mkLit n = pretty n

instance Builtin (IdrisTm ann) "suc" (IdrisTm ann -> IdrisTm ann) where
  mkBuiltin x = "S" <+> x

instance Builtin (IdrisTm ann) "+" (IdrisTm ann -> IdrisTm ann -> IdrisTm ann) where
  mkBuiltin x y = x <+> "+" <+> y

instance Builtin (IdrisTm ann) "Type" (IdrisTm ann) where
  mkBuiltin = "Type"

--------------------------------------------------------------------------------
-- Modules

newtype IdrisMod ann = IdrisMod { getIdrisMod :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

newtype IdrisHeader ann = IdrisHeader [Doc ann]
  deriving newtype (Semigroup, Monoid)

instance Module (IdrisMod ann) (IdrisHeader ann) (IdrisDefn ann) where
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

idrisImport :: Text -> IdrisHeader ann
idrisImport nm = IdrisHeader ["import" <+> pretty nm]

instance Import (IdrisHeader ann) "Data.Nat" where
  mkImport = mempty
