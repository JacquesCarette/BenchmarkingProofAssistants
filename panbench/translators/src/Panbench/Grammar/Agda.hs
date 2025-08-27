{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

-- | Pretty printer for Agda.
module Panbench.Grammar.Agda
  ( Agda(..)
  ) where

import Data.Text (Text)

import Data.String (IsString(..))

import Numeric.Natural

import Panbench.Grammar
import Panbench.Pretty

newtype Agda ann = Agda { getAgda :: Doc ann }
  deriving newtype (Semigroup, Monoid)

instance IsString (Agda ann) where
  fromString s = pretty s

visible :: Visibility -> Agda ann -> Agda ann
visible Explicit = enclose "(" ")"
visible Implicit = enclose "{" "}"

binder :: Arg [Name] (Agda ann) -> Agda ann
binder (Arg [] tp vis) = visible vis tp
binder (Arg nms tp vis) = visible vis (hsepMap pretty nms <+> ":" <+> tp)

binders :: (Foldable f) => f (Arg [Name] (Agda ann)) -> Agda ann
binders = hsepMap binder

pattern_ :: Pattern (Agda ann) -> Agda ann
pattern_ (VarPat Explicit nm) = pretty nm
pattern_ (VarPat Implicit nm) = "{" <> pretty nm <> "}"
pattern_ (ConPat vis nm pats) = visible vis $ pretty nm <+> hsepMap pattern_ pats

patterns :: [Pattern (Agda ann)] -> Agda ann
patterns = hsepMap pattern_

letDef :: LetDef (Agda ann) -> Agda ann
letDef (ChkLetDef nm tp pats body) =
  hardlines
  [ pretty nm <+> ":" <+> align tp
  , pretty nm <+> patterns pats <+> "=" <\?> align body
  ]
letDef (SynLetDef nm pats body) =
  pretty nm <+> patterns pats <+> "=" <\?> align body

letDefs :: [LetDef (Agda ann)] -> Agda ann
letDefs = align . hardlinesMap letDef

instance Term (Agda ann) where
  var x = pretty x
  arr x y = x <+> "->" <+> y
  pi xs y = binders xs <+> "->" <+> y
  app x ys = x <\?> sep ys
  let_ defs body = "let" <+> letDefs defs <+> "in" <+> body
  univ = "Set"
  parens = enclose "(" ")"

instance Builtin (Agda ann) "+" (Agda ann -> Agda ann -> Agda ann) where
  mkBuiltin x y = x <+> "+" <+> y

instance Builtin (Agda ann) "suc" (Agda ann -> Agda ann) where
  mkBuiltin x = "suc" <+> x

instance Literal (Agda ann) "Nat" Natural where
  mkLit = pretty

--------------------------------------------------------------------------------
-- Definitions

clauses :: Name -> [Clause (Agda ann)] -> Agda ann
clauses nm =
  hardlinesMap \(Clause pats body) ->
    pretty nm <+> patterns pats <+> "=" <\?> hang 2 body

constructors :: [Constr (Agda ann)] -> Agda ann
constructors =
  nest 2 . hardlinesMap \(Constr nm tp) ->
    pretty nm <+> ":" <+> tp


instance (Module (Agda ann) (Agda ann)) where
  moduleHeader modNm = "module" <+> pretty modNm <+> "where" <> hardline
  defTm nm tp body =
    hardlines
    [ pretty nm <+> ":" <+> tp
    , pretty nm <+> "=" <\?> nest 2 body
    ]

  defMatch nm tp cs =
    hardlines
    [ pretty nm <+> ":" <+> tp
    , clauses nm cs
    ]

  defData nm params cons =
    hardlines
    [ "data" <+> pretty nm <+> binders params <+> ":" <+> "where"
    , constructors cons
    ]

openImport :: Text -> Agda ann
openImport m = "open" <+> "import" <+> pretty m <> hardline

instance Import (Agda ann) "Data.Nat" where
  mkImport = openImport "Agda.Builtin.Nat"

instance Import (Agda ann) "Data.Vec" where
  mkImport = openImport "Data.Vec.Base"

instance Import (Agda ann) "Data.List" where
  mkImport = openImport "Agda.Builtin.List"

instance Import (Agda ann) "Data.String" where
  mkImport = openImport "Agda.Builtin.String"
