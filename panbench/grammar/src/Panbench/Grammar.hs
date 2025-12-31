{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
-- | Tagless grammar for panbench.
module Panbench.Grammar
  ( -- * Names
    Name(..)
    -- * Binding Cells
    -- $binders
    --
    -- $bindingModifiers
  , Binder(..)
  , None(..)
  , Single(..)
  , (.:)
  , (.:*)
  , syn
  , syns
  , Implicit(..)
  , SemiImplicit(..)
  , Unbound(..)
    -- * Definitions
    -- $definitions
  , Definition(..)
  , Postulate(..)
  , DataDefinition(..)
  , dataN_
  , RecordDefinition(..)
  , recordN_
  , CheckType(..)
  , checkConvert
  , CheckUsingAnonDefinition(..)
  , Newline(..)
    -- * Left-hand sides
    -- $leftHandSides
  , TelescopeLhs(..)
    -- * Terms
  , Pi(..)
  , Arr(..)
  , (.->)
  , App(..)
  , appN
  , Lam(..)
  , Let(..)
  , letN
  , Underscore(..)
  , Parens(..)
  , parens
    -- * Builtins
    -- $builtins
  , Builtin(..)
  , builtin
  , Constant
  , constant
  , Op1
  , op1
  , Op2
  , op2
    -- ** Literals
  , Literal(..)
  , lit
  , nat
  , sucN
  , list
  , string
    -- * Top-level modules
  , Module(..)
    -- * Imports
  , Import(..)
  , import_
  )
  where

import Control.Applicative

import Data.Kind
import Data.Functor.Alt
import Data.Foldable1
import Data.Text (Text)

import GHC.TypeLits

import Panbench.Prelude

--------------------------------------------------------------------------------
-- Names

-- | A 'Name' is some type that supports idiomatic name operations
-- like subscripting, qualification, etc.
--
-- [TODO: Reed M, 27/09/2025] Qualified names.
class (IsString nm) => Name nm where
  nameN :: nm -> Natural -> nm
  replicateName :: nm -> Natural -> nm

--------------------------------------------------------------------------------
-- Binding Cells

-- $binders
-- Our metalanguage needs to support lots of different binding constructs.
-- Moreover, all the languages tend to have different overlapping supported
-- sets of binding constructs. To handle this, we introduce a general langauge
-- of *binding cells* and *binding modifiers* that act on said cell.
--
-- A *binding cell* is a gramatical form like @(x : A)@, @{x y z : B}@, or just @x@ that binds
-- (potentially multiple) names. Binding cells clearly show up in things like telescopes for pi types,
-- but also show up in more unexpected places. For instance, consider a local let definition like
--
-- @
-- let x : Nat := ... in ...
-- @
--
-- Here, the @x : Nat@ is secretly a binding cell! We opt to classify binding cells along two axes:
--
-- 1. The "arity" of the binding cell, which describes the number of names bound.
--    This can be 0 in the case of "anonomyous" binders like @{Nat} → ...@, 1 as in @let@,
--    or multiple as in @(x y z : A) → @
-- 2. The "annotation" of the binding cell. This lets us distinguish between binding forms
--    like @∀ x y z → ...@ which may not require a type annotation, and the required annotation
--    on something like @data Foo : Set@.


class Binder arity nm ann tm cell | cell -> nm tm where
  binder :: arity nm -> ann tm -> cell

-- | Infix operator for an annotated binder with a single name.
(.:) :: (Binder Single nm Single tm cell) => nm -> tm -> cell
nm .: tp = binder (Single nm) (Single tp)

-- | Infix operator for an annotated binder.
(.:*) :: (Binder arity nm Single tm cell) => arity nm -> tm -> cell
nms .:* tp = binder nms (Single tp)

syn :: (Binder Single nm None tm cell) => nm -> cell
syn nm = binder (Single nm) None

syns :: (Binder arity nm None tm cell) => arity nm -> cell
syns nms = binder nms None

-- | No annotation or arity.
data None nm = None

instance Functor None where
  fmap _ _ = None

instance Applicative None where
  pure _ = None
  _ <*> _ = None

instance Alternative None where
  empty = None
  _ <|> _ = None

instance Foldable None where
  foldMap _ _ = mempty

-- | A single annotation or singular arity.
newtype Single a = Single { unSingle :: a }

instance Functor Single where
  fmap f (Single a) = Single (f a)

instance Applicative Single where
  pure = Single
  Single f <*> Single a = Single (f a)

instance Alt Single where
  x <!> _ = x

instance Foldable Single where
  foldMap f (Single x) = f x

instance Foldable1 Single where
  foldMap1 f (Single x) = f x

--------------------------------------------------------------------------------
-- Binder modifiers

-- $bindingModifiers
--
-- The other half of the binder API is *binding modifiers, which handle things
-- like implicit arguments, class arguments, erasure, etc. Every sort of modifier
-- has a class associated to it like 'Implicit', which provide an (possibly parameterized)
-- action @cell -> cell@.
--
-- On the user-facing side, writing a modified binder is as easy as
--
-- @
-- pi (implicit ("x" .: nat)) nat
-- @
--
-- Instances for @Chk@ and related classes will then have to delay the
-- decision on how to fully render the binder, which is why we provide the
-- extra degree of freedom in classes like 'Pi'.

class Implicit cell where
  -- | Mark a binder cell as implicit.
  implicit :: cell -> cell

class SemiImplicit cell where
  -- | Mark a binder cell as semi-implicit.
  semiImplicit :: cell -> cell

-- | The "unbinding" modifier.
--
-- To see why we need this modifier, consider the following contrived example:
--
-- @
-- prog : {A : Type} {n : Nat} → A → Vec A n
-- prog {n = n} x = replicate x n
-- @
--
-- The grammar of panbench does not separate top-level type annotations from terms,
-- so we would have to write this as something like
--
-- @
-- [implicit "A" .: builtin "Type", implicit "n" .: builtin "Nat", "x" .: "A"] |- builtin "Vec" "A" "n" .=
--   builtin "replicate" "x" "n"
-- @
--
-- This causes a bit of an awkward situation for languages that have separate type signatures, as
-- we don't have a good way of knowing between implicits which we actually bind to names
-- in a LHS.
--
-- Enter 'Unbound'. This binding modifer lets us explicitly mark the first implicit as unused within
-- the definition like so:
-- @
-- [unbound $ implicit "A" .: builtin "Type", implicit "n" .: builtin "Nat", "x" .: "A"] |- builtin "Vec" "A" "n" .=
--   builtin "replicate" "x" "n"
-- @
--
-- This lets us inform backends that they should omit the LHS binding if appropriate.
-- When applied to a visible cell, backends ought to use the appropriate notion of "unused" binding:
-- this is typically some variant of 'Underscore' ala
--
-- @
-- const : {A B : Type} → A → B → A
-- const a _ = a
-- @
class Unbound cell where
  -- | Mark a binder cell as "unbound".
  unbound :: cell -> cell

--------------------------------------------------------------------------------
-- Definitions

-- $definitions

-- | A term definition.
class Definition lhs tm defn | defn -> lhs tm where
  (.=) :: lhs -> tm -> defn

infixr 0 .=

class Postulate lhs defn | defn -> lhs where
  postulate :: [lhs] -> defn

-- | Data definitions.
class DataDefinition lhs ctor defn | defn -> lhs ctor where
  data_
    :: lhs    -- ^ Left-hand side of the datatype.
    -> [ctor] -- ^ Constructors.
    -> defn

-- | Create a datatype with @n@ fields.
dataN_
  :: (DataDefinition lhs ctor defn)
  => lhs
  -> Natural
  -> (Natural -> ctor)
  -> defn
dataN_ lhs size ctor =
  data_ lhs [ctor i | i <- [1..size]]

-- | Record definitions.
class RecordDefinition lhs name field defn | defn -> lhs name field where
  record_
    :: lhs     -- ^ Left-hand side of the record type.
    -> name    -- ^ Constructor name.
    -> [field] -- ^ Fields.
    -> defn

-- | Create a record with @n@ fields.
recordN_
  :: (RecordDefinition lhs name field defn)
  => lhs
  -> name
  -> Natural
  -> (Natural -> field)
  -> defn
recordN_ lhs nm size field =
  record_ lhs nm [field i | i <- [1..size]]

-- | Check that a term has a type.
class CheckType tm defn | defn -> tm where
  checkType :: tm -> tm -> defn

-- | Perform a conversion check on two terms.
--
-- This generates a definition like
-- @
-- nm : x = y
-- nm = refl
-- @
checkConvert
  :: ( Definition lhs tm defn, TelescopeLhs cell hd lhs, Binder Single nm Single tm hd
     , Op2 tm "=", Constant tm "refl"
     )
  => nm -> tm -> tm -> defn
checkConvert nm x y =
  [] |- (nm .: op2 "=" x y) .= builtin "refl"

-- | Deriving-via helper for implementing 'checkType' via an anonymous definition ala
-- @
-- _ : Nat
-- _ = 4
-- @
newtype CheckUsingAnonDefinition tm defn = CheckUsingAnonDefinition defn

instance (Definition lhs tm defn, TelescopeLhs cell hd lhs, Binder None nm Single tm hd) => CheckType tm (CheckUsingAnonDefinition tm defn) where
  checkType tm tp =
    CheckUsingAnonDefinition $
      [] |- None .:* tp .= tm

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
class TelescopeLhs cell hd lhs | lhs -> cell hd where
  (|-) :: [cell] -> hd -> lhs

infix 1 |-

--------------------------------------------------------------------------------
-- Terms

-- | Pi-types.
class Pi cell tm | tm -> cell where
  -- | Create a pi type over a list of @cell@.
  --
  -- See $binders for expected use.
  pi :: [cell] -> tm -> tm

class Arr cell tm | tm -> cell where
  -- | Create a non-dependent pi type over a @cell@.
  --
  -- See $binders for expected use.
  arr :: cell -> tm -> tm

(.->) :: (Arr cell tm, Binder None nm Single tm cell) => tm -> tm -> tm
a .-> b = binder None (Single a) `arr` b

infixr 5 .->

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

class Lam cell tm | tm -> cell where
  lam :: [cell] -> tm -> tm

-- | Let-bindings.
class Let defn tm | tm -> defn where
  let_ :: [defn] -> tm -> tm

-- | Sized let bindings.
letN
  :: (Let defn tm)
  => Natural
  -> (Natural -> defn)
  -> tm
  -> tm
letN size defn tm = let_ [ defn i | i <- [1..size] ] tm

class Underscore tm where
  underscore :: tm

-- | Wrap a term in @n@ parens.
class Parens tm where
  parensN :: Natural -> tm -> tm

-- | Wrap a term in a set of parens.
parens :: (Parens tm) => tm -> tm
parens = parensN 1

--------------------------------------------------------------------------------
-- Operators and Builtins

-- $builtins
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

-- | Literals work like 'Builtin', but with a slight twist.
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

sucN :: (Op1 tm "suc", Parens tm) => Natural -> tm -> tm
sucN 0 x = x
sucN 1 x = op1 "suc" x
sucN n x = op1 "suc" $ parens (sucN (n - 1) x)

-- | Construct a @List@ literal.
list :: (Literal tm "List" [tm]) => [tm] -> tm
list = lit "List"

-- | Construct a @String@ literal.
string :: (Literal tm "String" Text) => Text -> tm
string = lit "String"

--------------------------------------------------------------------------------
-- Top-level modules

class (Monoid hdr, Monoid defn) => Module mod hdr defn | mod -> hdr defn, hdr defn -> mod where
  -- | Construct a top-level module.
  module_
    :: Text    -- ^ The name of the module
    -> hdr     -- ^ Module header.
    -> defn    -- ^ Module body.
    -> mod

--------------------------------------------------------------------------------
-- Imports

class (KnownSymbol i) => Import (hdr :: Type) (i :: Symbol) where
  mkImport :: hdr

import_ :: forall i -> (Import hdr i) => hdr
import_ i = mkImport @_ @i
