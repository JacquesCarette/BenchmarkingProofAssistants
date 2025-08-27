{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Tagless grammar for panbench.
module Panbench.Grammar
  (
  -- * Names
    Name
  , name
  , names
  -- * Binders
  , Visibility(..)
  , Arg(..)
  -- * Terms
  , LetDef(..)
  , Term(..)
  -- * Patterns
  , Pattern(..)
  , Clause(..)
  -- * Definitions
  , Constr(..)
  , Module(..)
  -- * Operators, Builtins, and literals
  , Builtin(..) , builtin
  -- ** Builtin shorthands
  , Constant , constant
  , Op1 , op1
  , Op2 , op2
  , Literal(..) , lit
  , nat, list, vec, string
  -- * Imports
  , Import(..) , import_
  -- * Helpers
  , vars
  , varN
  ) where

import Numeric.Natural

import Data.List.NonEmpty (NonEmpty)
import Data.Kind
import Data.Text (Text)
import Data.Text qualified as T

import GHC.TypeLits

--------------------------------------------------------------------------------
-- Names

type Name = Text

name :: Name -> Natural -> Name
name x n = x <> T.pack (show n)

names :: Name -> [Natural] -> [Name]
names x ns = name x <$> ns

--------------------------------------------------------------------------------
-- Binders

-- | Binder visibility
data Visibility
  = Explicit
  -- ^ An explicit binder like @(x : A) -> B x@
  | Implicit
  -- ^ An implicit binder like @{x : A} -> B x@

-- | Binder metadata.
data Arg nm arg = Arg { argNm :: nm, unArg :: arg , argVis :: Visibility}

--------------------------------------------------------------------------------
-- Terms

data LetDef rep
  = ChkLetDef Name rep [Pattern rep] rep
  | SynLetDef Name [Pattern rep] rep

-- | Terms.
class Term (rep :: Type) where
  -- | Variables.
  var :: Name -> rep

  -- | Non-dependent function types.
  --
  -- We differentiate between dependent and non-dependent function types
  -- to get nicer pretty-printing.
  arr :: rep -> rep -> rep

  -- | Dependent function types.
  pi :: NonEmpty (Arg [Name] rep) -> rep -> rep

  -- | N-ary application.
  --
  -- Using n-ary application allows for nicer display.
  app :: rep -> [rep] -> rep

  let_ :: [LetDef rep] -> rep -> rep

  -- | The universe.
  univ :: rep

  -- | Explicit parentheses.
  parens :: rep -> rep

--------------------------------------------------------------------------------
-- Operators and Builtins

-- | Builtins
--
-- We want to be able to be polymorphic over the builtin-constructs of a language,
-- *and* be able to separately dispatch on builtins when defining a language instance.
-- For example, one language might call the @List@ built-in @List A@, another @[A]@, and so on.
-- Moreover, we don't want to have to define a new class per built-in: this gets really cumbersome
-- really fast, and means that users might end up having to roll their own built-in classes, which
-- is not very good.
--
-- In light of this, we define a single 'Builtin' class that lets us dispatch on a statically known
-- 'Symbol'. We also allow the return type to vary, which lets us implement things like builtin infix operators
-- in a natural way via @Builtin rep "+" (rep -> rep -> rep)@.
class (KnownSymbol op) => Builtin (rep :: Type) (op :: Symbol) (tp :: Type) | rep op -> tp, op tp -> rep where
  -- | Make a builtin of a given type.
  --
  -- When writing generators, users are encouraged to use 'builtin' instead, as it
  -- has *much* better inference.
  mkBuiltin :: tp

-- | Construct a builtin term.
builtin :: forall rep tp. forall op -> (Builtin rep op tp) => tp
builtin o = mkBuiltin @rep @o @tp

type Constant rep op = Builtin rep op rep

-- | Shorthand for a builtin that doesn't have any arguments.
constant :: forall op -> Constant rep op => rep
constant = builtin

type Op1 rep op = Builtin rep op (rep -> rep)

-- | Shorthand for a unary operator.
op1 :: forall op -> Op1 rep op => rep -> rep
op1 = builtin

type Op2 rep op = Builtin rep op (rep -> rep -> rep)

-- | Shorthand for a binary operator.
op2 :: forall op -> Op2 rep op => rep -> rep -> rep
op2 = builtin

-- | Literals
--
-- Literals work like 'Builtin', but with a slight twist.
-- Instead of a single @mkBuiltin :: tp@ method, the 'Literal' class
-- has a single @mkLit :: tp -> rep@ method, which reflects that literals
-- must always be built out of *something*. This also leads to marginally better inference.
class (KnownSymbol sym) => Literal (rep :: Type) (sym :: Symbol) (tp :: Type) | rep sym -> tp where
  -- | Make a builtin of a given type.
  --
  -- When writing generators, users are encouraged to use 'lit' instead, as it
  -- has *much* better inference.
  mkLit :: tp -> rep

-- | Construct a literal term.
lit :: forall sym -> Literal rep sym tp => tp -> rep
lit sym x = mkLit @_ @sym x

-- | Construct a @Nat@ literal.
nat :: (Literal rep "Nat" Natural) => Natural -> rep
nat = lit "Nat"

-- | Construct a @List@ literal.
list :: (Literal rep "List" [rep]) => [rep] -> rep
list = lit "List"

-- | Construct a @Vec@ literal.
vec :: (Literal rep "Vec" [rep]) => [rep] -> rep
vec = lit "Vec"

-- | Construct a @String@ literal.
string :: (Literal rep "String" Text) => Text -> rep
string = lit "String"

--------------------------------------------------------------------------------
-- Patterns

data Pattern rep
  = ConPat Visibility Name [Pattern rep]
  | VarPat Visibility Name

data Clause rep = Clause { clausePats :: [Pattern rep], clauseBody :: rep }

--------------------------------------------------------------------------------
-- Definitions

data Constr rep = Constr { constrName :: Name, constrTp :: rep }

class (Term rep, Monoid m) => Module (rep :: Type) (m :: Type) | rep -> m, m -> rep where
  -- | Module header.
  moduleHeader :: Text -> m

  -- | Module with a single (possibly nullary) term.
  defTm
    :: Name -- ^ The name of the definition.
    -> rep  -- ^ Type of the term.
    -> rep  -- ^ Definition of the term.
    -> m

  -- | Module with a single definition created via pattern matching.
  defMatch
    :: Name -- ^ The name of the definition.
    -> rep -- ^ The type of the defininition
    -> [Clause rep] -- ^ Pattern matching clauses
    -> m

  -- | Define a datatype.
  defData
    :: Name -- ^ The name of the datatype.
    -> [Arg [Name] rep] -- ^ Datatype parameters.
    -> [Constr rep] -- ^ Datatype constructors.
    -> m

--------------------------------------------------------------------------------
-- Imports

class (KnownSymbol i) => Import (m :: Type) (i :: Symbol) where
  mkImport :: m

import_ :: forall i -> (Import m i) => m
import_ i = mkImport @_ @i

--------------------------------------------------------------------------------
-- Common constraints

--------------------------------------------------------------------------------
-- Helpers

-- | Construct a list of variables @x_i, x_j, x_k@ from a base name
-- and a list of numeric subscripts.
vars :: (Term rep) => Name -> [Natural] -> [rep]
vars x ns = var <$> names x ns

-- | Shorthand for @xn@ where @x@ is a name and @n@ is a natural number.
varN :: (Term rep) => Name -> Natural -> rep
varN x n = var (name x n)
