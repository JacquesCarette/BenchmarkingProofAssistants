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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Tagless grammar for panbench.
module Panbench.Grammar where

import Prelude hiding (pi)

import Numeric.Natural

import Data.Kind
import Data.Text (Text)
import Data.String (IsString(..))

import GHC.TypeLits

--------------------------------------------------------------------------------
-- Names

-- | A 'Name' is some type that supports idiomatic name operations
-- like subscripting, qualification, etc.
--
-- [TODO: Reed M, 27/09/2025] Qualified names.
class (IsString nm) => Name nm where
  nameN :: nm -> Natural -> nm

--------------------------------------------------------------------------------
-- Binding Cells

-- $binders
--
-- Our metalanguage needs to support lots of different binding constructs.
-- Moreover, all the languages tend to have different overlapping supported
-- sets of binding constructs. To handle this, we introduce a general langauge
-- of *binding cells* and *binding modifiers* that act on said cell.

-- | An annotated single binding cell.
class Chk nm tm cell | cell -> nm, cell -> tm where
  chk :: nm -> tm -> cell

-- | A annotated multi binding cell.
class Chks nm tm cell | cell -> nm, cell -> tm where
  chks :: [nm] -> tm -> cell

-- | An unannotated single binding cell.
class Syn nm cell | cell -> nm where
  syn :: nm -> cell

-- | An unannotated multi-binding cell.
class Syns nm cell | cell -> nm where
  syns :: [nm] -> cell

-- | Anonymous check cells.
--
-- These are used for binding forms like non-dependent function types,
-- which take a type but not a name.
class AnonChk tm cell | cell -> tm where
  anonChk :: tm -> cell

-- | Anonymous synthesis cells.
--
-- This somewhat odd construct is used when we put underscores in non-dependent function types, ala
-- @
-- foo : _ -> Type
-- @
--
-- These are a somewhat odd feature, but it does exist an is provided for symmetry with
-- 'AnonChk' cells. These are also subtly distinct from holes, as we can apply visibility
-- modifiers to them.
class AnonSyn tm cell | cell -> tm where
  anonSyn :: cell

--------------------------------------------------------------------------------
-- Deriving-via helpers

newtype SingletonCell cell = SingletonCell cell

instance Syns nm cell => Syn nm (SingletonCell cell) where
  syn nm = SingletonCell (syns [nm])

instance Chks nm tm cell => Chk nm tm (SingletonCell cell) where
  chk nm tm = SingletonCell (chks [nm] tm)

-- | Infix operator for 'chk'.
(.:) :: (Chk nm tm cell) => nm -> tm -> cell
(.:) = chk

-- | Infix operator for 'chks'.
(.:*) :: (Chks nm tm cell) => [nm] -> tm -> cell
(.:*) = chks

--------------------------------------------------------------------------------
-- Binder modifiers

-- $bindingModifiers
--
-- The other half of the binder API is *binding modifiers, which handle things
-- like implicit arguments, class arguments, erasure, etc. Every sort of modifier
-- has a class associated to it like 'Implicit', which provide an (possibly parameterized)
-- action @binder -> binder@.
--
-- On the user-facing side, writing a modified binder is as easy as
--
-- @
-- pi (implicit ("x" .: nat)) nat
-- @
--
-- Instances for @Binder@ and related classes will then have to delay the
-- decision on how to fully render the binder, which is why we provide the
-- extra degree of freedom in classes like 'Pi'.

class Implicit cell where
  -- | Mark a binder cell as implicit.
  implicit :: cell -> cell

class SemiImplicit cell where
  -- | Mark a binder cell as semi-implicit.
  semiImplicit :: cell -> cell

--------------------------------------------------------------------------------
-- Definitions

-- $definitions

-- | A term definition.
class Definition defn lhs tm | defn -> lhs, defn -> tm, tm lhs -> defn where
  (.=) :: lhs -> tm -> defn

infixr 0 .=

class Postulate defn lhs | defn -> lhs, lhs -> defn where
  postulate :: lhs -> defn

-- | Data definitions.
class DataDefinition defn lhs ctor | defn -> lhs, defn -> ctor, lhs ctor -> defn where
  data_
    :: lhs    -- ^ Left-hand side of the datatype.
    -> [ctor] -- ^ Constructors.
    -> defn

-- | Create a datatype with @n@ fields.
dataN_
  :: (DataDefinition defn lhs ctor)
  => lhs
  -> Natural
  -> (Natural -> ctor)
  -> defn
dataN_ lhs size ctor =
  data_ lhs [ctor i | i <- [1..size]]

-- | Record definitions.
class RecordDefinition defn lhs name field | defn -> lhs, defn -> name, defn -> field, lhs field -> defn where
  record_
    :: lhs     -- ^ Left-hand side of the record type.
    -> name    -- ^ Constructor name.
    -> [field] -- ^ Fields.
    -> defn

-- | Create a record with @n@ fields.
recordN_
  :: (RecordDefinition defn lhs name field)
  => lhs
  -> name
  -> Natural
  -> (Natural -> field)
  -> defn
recordN_ lhs nm size field =
  record_ lhs nm [field i | i <- [1..size]]

class Newline defn where
  -- | Generate @n@ newlines.
  newlines :: Natural -> defn

--------------------------------------------------------------------------------
-- Left-hand sides

-- $leftHandSides
--
-- We need to handle left-hand sides of definitions in two places:
--
-- 1. Local let bindings
-- 2. Top-level bindings
--
-- Both of these cases allow for named definitions, anonymous definitions,
-- annotated definitions, definitions that take arguments, and (sometimes) pattern
-- definitions.
--
-- To handle this zoo of features, we break down a LHS into the following components:
--
-- * A *LHS head* is the thing that we are actually defining. Typically, this will
--   consist of a name, an optional annotation, and some further metadata, though
--   we also consider the pattern portion of a destructuring let to be a head.
--
--   Users are intended to use the general 'Syn' and 'Chk' classes for
--   annotations of LHS heads.
--
-- * A *LHS cell* is a binding cell used for bindings like
--
--   @
--   let foo (A : Type) (x : Nat) (y : Nat) : Vec A (x + y) := ...
--   @
--
--   Typically, the we can re-use general binding cells for LHS cells, but we
--   distinguish the two conceptually to avoid confusion.

-- | Construct a LHS that is parameterised by a telescope of (possibly annotated)
-- arguments.
--
-- The intuition for the order of arguments is that we want to think
-- of a LHS like
--
-- @
-- let foo (A : Type) (x : Nat) (y : Nat) : Vec A (x + y) := ...
-- @
--
-- as definining a term @A : Type, x : Nat, y : Nat ⊢ foo A x y : Vec (x + y)@.
class ArgumentLhs hd cell lhs | lhs -> cell, lhs -> hd where
  (|-) :: [cell] -> hd -> lhs

infix 1 |-

-- | Shorthand for annotated telescope left-hand sides.
type TelescopeLhs name tm lhs hd cell = (ArgumentLhs hd cell lhs, Chk name tm hd, Chk name tm cell)

--------------------------------------------------------------------------------
-- Terms

class (Name nm) => Var nm tm | tm -> nm where
  var :: nm -> tm

varN :: (Var nm tm) => nm -> Natural -> tm
varN nm i = var (nameN nm i)

-- | Pi-types.
class Pi tm cell | tm -> cell where
  -- | Create a pi type over a list of @cell@.
  --
  -- See $binders for expected use.
  pi :: [cell] -> tm -> tm

class Arr tm cell | tm -> cell where
  -- | Create a pi type over a @cell@.
  --
  -- See $binders for expected use.
  arr :: cell -> tm -> tm

-- | Applications.
--
-- [FIXME: Reed M, 26/09/2025] Need to think about how visibility interacts with
-- application.
class App tm where
  app :: tm -> [tm] -> tm

-- | Sized application.
appN
  :: (App tm)
  => tm
  -> Natural
  -> (Natural -> tm)
  -> tm
appN fn size arg = app fn [ arg i | i <- [1..size] ]

-- | Let-bindings.
class Let defn tm | tm -> defn, defn -> tm where
  let_ :: [defn] -> tm -> tm


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
-- in a natural way via @Builtin tm "+" (tm -> tm -> tm)@.
class (KnownSymbol op) => Builtin (tm :: Type) (op :: Symbol) (tp :: Type) | tm op -> tp, op tp -> tm where
  -- | Make a builtin of a given type.
  --
  -- When writing generators, users are encouraged to use 'builtin' instead, as it
  -- has *much* better inference.
  mkBuiltin :: tp

-- | Construct a builtin term.
builtin :: forall tm tp. forall op -> (Builtin tm op tp) => tp
builtin o = mkBuiltin @tm @o @tp

type Constant tm op = Builtin tm op tm

-- | Shorthand for a builtin that doesn't have any arguments.
constant :: forall op -> Constant tm op => tm
constant = builtin

type Op1 tm op = Builtin tm op (tm -> tm)

-- | Shorthand for a unary operator.
op1 :: forall op -> Op1 tm op => tm -> tm
op1 = builtin

type Op2 tm op = Builtin tm op (tm -> tm -> tm)

-- | Shorthand for a binary operator.
op2 :: forall op -> Op2 tm op => tm -> tm -> tm
op2 = builtin

-- | Literals
--
-- Literals work like 'Builtin', but with a slight twist.
-- Instead of a single @mkBuiltin :: tp@ method, the 'Literal' class
-- has a single @mkLit :: tp -> tm@ method, which reflects that literals
-- must always be built out of *something*. This also leads to marginally better inference.
class (KnownSymbol sym) => Literal (tm :: Type) (sym :: Symbol) (tp :: Type) | tm sym -> tp where
  -- | Make a builtin of a given type.
  --
  -- When writing generators, users are encouraged to use 'lit' instead, as it
  -- has *much* better inference.
  mkLit :: tp -> tm

-- | Construct a literal term.
lit :: forall sym -> Literal tm sym tp => tp -> tm
lit sym x = mkLit @_ @sym x

-- | Construct a @Nat@ literal.
nat :: (Literal tm "Nat" Natural) => Natural -> tm
nat = lit "Nat"

-- | Construct a @List@ literal.
list :: (Literal tm "List" [tm]) => [tm] -> tm
list = lit "List"

-- | Construct a @String@ literal.
string :: (Literal tm "String" Text) => Text -> tm
string = lit "String"

--------------------------------------------------------------------------------
-- Top-level modules

-- $topLevel
--
-- Basic grammar is
--
-- @
-- module   := name hdr body
-- hdr      := (import | defn)*
-- body     := defn*
-- defn     := topLhs tm
-- topLhs   := topLhsHd cell*
-- topLhsHd := name tm?
-- @

class (Monoid hdr, Monoid defns) => Module mod hdr defns | mod -> hdr, mod -> defns, hdr defns -> mod where
  -- | Construct a top-level module.
  module_
    :: Text    -- ^ The name of the module
    -> hdr     -- ^ Module header.
    -> defns   -- ^ Module body.
    -> mod

--------------------------------------------------------------------------------
-- Imports

class (KnownSymbol i) => Import (hdr :: Type) (i :: Symbol) where
  mkImport :: hdr

import_ :: forall i -> (Import hdr i) => hdr
import_ i = mkImport @_ @i
