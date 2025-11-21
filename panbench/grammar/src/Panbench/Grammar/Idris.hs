{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- | Pretty printer for Idris.
module Panbench.Grammar.Idris
  ( Idris
  , IdrisState(..)
  , IdrisM(..)
  , runIdrisM
  , IdrisMod
  , IdrisHeader
  , IdrisDefns
  ) where

import Control.Applicative
import Control.Monad.State

import Data.Default
import Data.Functor
import Data.Functor.Alt
import Data.Maybe
import Data.Monoid (Ap(..))
import Data.Text (Text)

import ListT (ListT)
import ListT qualified as ListT

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Prelude
import Panbench.Pretty

data Idris

data IdrisState = IdrisState
  { idrisTestFresh :: Natural
  -- ^ The number of @Check@ definitions we've produced.
  -- This is required to be able to generate fresh names
  -- for definitions whose sole purpose is to serve as a type
  -- checking command ala @Check@ in Rocq or @#check@ in Lean.
  }

instance Default IdrisState where
  def = IdrisState
    { idrisTestFresh = 0
    }

newtype IdrisM a = IdrisM (State IdrisState a)
  deriving newtype (Functor, Applicative, Monad, MonadState IdrisState)

deriving via (Ap IdrisM a) instance (Semigroup a) => Semigroup (IdrisM a)
deriving via (Ap IdrisM a) instance (Monoid a) => Monoid (IdrisM a)
deriving via (Ap IdrisM a) instance (IsString a) => IsString (IdrisM a)
deriving via (Ap IdrisM a) instance (Document a) => Document (IdrisM a)

runIdrisM :: IdrisState -> IdrisM a -> a
runIdrisM st (IdrisM m) = evalState m st

withFreshTestName :: (MonadState IdrisState m) => (IdrisName -> m a) -> m a
withFreshTestName k = do
  n <- gets idrisTestFresh
  modify' (\s -> s { idrisTestFresh = n + 1 })
  k (nameN "test" n)

--------------------------------------------------------------------------------
-- Names

type IdrisName = IdrisM (Doc Ann)

instance Name IdrisName where
  nameN x i = x <> pretty i
  replicateName x n = duplicate (fromIntegral n) <$> x

--------------------------------------------------------------------------------
-- Cells

data IdrisVis
  = Visible
  | Implicit

-- | Visibility forms a meet semilattice, where @Implicit <= Visible@.
instance Semigroup IdrisVis where
  Implicit <> _ = Implicit
  _ <> Implicit = Implicit
  Visible <> Visible = Visible

-- | 'Visible' is the top element of the visibility semilattice.
instance Monoid IdrisVis where
  mempty = Visible

data IdrisBound
  = Bound
  | Unbound

-- | Binding status forms a meet semilattice, where @Unbound <= Bound@.
instance Semigroup IdrisBound where
  Unbound <> _ = Unbound
  _ <> Unbound = Unbound
  Bound <> Bound = Bound

-- | 'Bound' is the top element of the binding semilattice.
instance Monoid IdrisBound where
  mempty = Bound

data IdrisArg arity nm = IdrisArg { argVis :: IdrisVis, argBound :: IdrisBound, argNames :: arity nm }

deriving instance Functor arity => Functor (IdrisArg arity)

-- | Applicative instance is basically @Writer@.
instance Applicative arity => Applicative (IdrisArg arity) where
  pure nm = IdrisArg
    { argVis = mempty
    , argBound = mempty
    , argNames = pure nm
    }
  f <*> a = IdrisArg
    { argVis = argVis f <> argVis a
    , argBound = argBound f <> argBound a
    , argNames = argNames f <*> argNames a
    }

instance Alternative arity => Alternative (IdrisArg arity) where
  empty = IdrisArg
    { argVis = mempty
    , argBound = mempty
    , argNames = empty
    }
  x <|> y = IdrisArg
    { argVis = argVis x <> argVis y
    , argBound = argBound x <> argBound y
    , argNames = argNames x <|> argNames y
    }

instance Alt arity => Alt (IdrisArg arity) where
  x <!> y = IdrisArg
    { argVis = argVis x <> argVis y
    , argBound = argBound x <> argBound y
    , argNames = argNames x <!> argNames y
    }

type IdrisCell arity ann = Cell arity IdrisName ann IdrisTm

type IdrisTelescope arity ann = CellTelescope (IdrisArg []) IdrisName Maybe IdrisTm arity IdrisName ann IdrisTm

instance Implicit (IdrisCell (IdrisArg arity) ann) where
  implicit (Cell arg tp) = Cell (arg { argVis = Implicit }) tp

instance Unbound (IdrisCell (IdrisArg arity) ann) where
  unbound (Cell arg tp) = Cell (arg { argBound = Unbound }) tp

-- | Surround a document with the appropriate delimiters for a given 'IdrisVis'.
withVis :: IdrisVis -> IdrisM (Doc Ann) -> IdrisM (Doc Ann)
withVis Visible = enclose "(" ")"
withVis Implicit = enclose "{" "}"

-- | Render an Idris binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by folding
-- over the annotation with @foldr const underscore@.
cell
  :: (Foldable arity, Foldable ann)
  => IdrisCell (IdrisArg arity) ann
  -> IdrisM (Doc Ann)
cell (Cell IdrisArg{..} tp) =
  withVis argVis (hsep (punctuate "," argNames) <+> ":" <+> foldr const underscore tp)

-- | Render a list of Agda binding cells, and add a final space if the list is non-empty
telescope :: (Foldable arity, Foldable ann) => [IdrisCell (IdrisArg arity) ann] -> IdrisM (Doc Ann)
telescope [] = mempty
telescope cells = hsepMap cell cells <> space

-- | Render the bound names of an 'IdrisCell'.
boundNames :: (Alternative arity) => IdrisCell (IdrisArg arity) ann -> arity IdrisName
boundNames (Cell { cellNames = IdrisArg Visible Bound nms }) = nms
boundNames (Cell { cellNames = IdrisArg Implicit Bound nms }) = nms <&> \nm -> withVis Implicit (nm <+> "=" <+> nm)
boundNames (Cell { cellNames = IdrisArg Implicit Unbound _}) = empty
boundNames (Cell { cellNames = IdrisArg Visible Unbound nms}) = underscore <$ nms

-- | Render a list of idris binding cells as function arguments.
arguments
  :: (Alternative arity, Foldable arity, Foldable ann)
  => [IdrisCell (IdrisArg arity) ann]
  -> IdrisM (Doc Ann)
arguments cells =
  let nms = asum $ boundNames <$> cells
  in if null nms then
    mempty
  else
    hsep nms <> space

--------------------------------------------------------------------------------
-- Top-level definitions

-- | Idris definitions require us to interleave effects and sequences.
type IdrisDefns = ListT IdrisM (Doc Ann)

defn :: IdrisM (Doc Ann) -> IdrisDefns
defn = lift

sepDefns :: IdrisDefns -> IdrisM (Doc Ann)
sepDefns ds = (hardlines . punctuate hardline) <$> ListT.toList ds

sepDefnsFor :: (Foldable t) => t a -> (a -> IdrisDefns) -> IdrisM (Doc Ann)
sepDefnsFor xs f = sepDefns $ foldMap f xs

data DefnVis = PublicExport | Export | Private

defnVis :: DefnVis -> IdrisM (Doc Ann)
defnVis PublicExport = "public" <+> "export"
defnVis Export = "export"
defnVis Private = "private"

-- | Create an idris namespace with an optional default visibility modifier.
namespace :: Text -> Maybe DefnVis -> IdrisDefns -> IdrisDefns
namespace nm Nothing defns =
  defn $
  nest 2 $ hardlines $
  [ "namespace" <+> pretty nm
  , sepDefns defns
  ]
namespace nm (Just vis) defns =
  defn $
  nest 2 $ hardlines $
  [ "namespace" <+> pretty nm
  , defnVis vis
  , mempty
  , sepDefns defns
  ]

instance Definition (IdrisTelescope Single Maybe) IdrisTm IdrisDefns where
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell nm _)) .= tm =
    -- Unclear if Idris supports unannotated top-level bindings?
    defn $
    nest 2 (nm <+> ":" <+> "_") <\>
    nest 2 (nm <+> arguments tele <> "=" <\?> tm)
  (tele :- SingleCell nm tp) .= tm =
    defn $
    nest 2 (nm <+> ":" <+> (pi tele (fromMaybe underscore tp))) <\>
    nest 2 (nm <+> arguments tele <> "=" <\?> tm)

-- | Idris 2 does not support postulates OOTB, so we need to use the @believe_me : a -> b@
-- primitive to do an unsafe cast. Somewhat annoyingly, we need to actually pick *something*
-- to cast, and that thing really should vary based on the goal (EG: @believe_me Refl@ for equality, etc).
--
-- Pulling on this thread leads to a heap of issues with implicit resolution and requires our postulate
-- code to be type-aware, so we just opt to punt and always use @believe_me ()@. This is
-- unsafe and could lead to segfaults in compiled code, but the alternative is not worth the engineering effort.
instance Postulate (IdrisTelescope Single Single) IdrisDefns where
  postulate lhss =
    namespace "Postulate" (Just Export) $
      foldFor lhss \(tele :- RequiredCell nm tp) ->
        tele :- SingleCell nm (Just tp) .= "believe_me" <+> "()"

type IdrisDataDefnLhs = IdrisTelescope Single Single

instance DataDefinition  IdrisDataDefnLhs (IdrisCell Single Single) IdrisDefns where
  -- It appears that Idris 2 does not support parameterised inductives?
  data_ (params :- RequiredCell nm tp) ctors =
    defn $
    nest 2 $
    "data" <+> nm <+> ":" <+> group ((pi params tp) <> line <> "where") <\>
      hardlinesFor ctors \(RequiredCell ctorNm ctorTp) ->
        -- We need to add the parameters as arguments, as Idris does not support parameterised inductives.
        ctorNm <+> ":" <+> nest 2 (pi params ctorTp)

instance RecordDefinition (IdrisTelescope Single Single) IdrisName (IdrisCell Single Single) IdrisDefns where
  -- Idris does not have universe levels so it does not allow for a sort annotation
  -- on a record definition.
  record_ (params :- (RequiredCell nm _)) ctor fields =
    defn $
    nest 2 $
    "record" <+> nm <+> telescope params <> "where" <\>
      "constructor" <+> ctor <\>
      hardlinesFor fields \(RequiredCell fieldNm fieldTp) ->
        fieldNm <+> ":" <+> fieldTp

-- | Idris doesn't let us create anonymous definitions, and also does not
-- come with a check command. This means that we need to create fresh definitions.
instance CheckType IdrisTm IdrisDefns where
  checkType tm tp =
    namespace "Test" (Just Private) do
      withFreshTestName \nm ->
        [] :- (nm .: tp) .= tm

instance Newline IdrisDefns where
  newlines n = defn $ duplicate (fromIntegral n) hardline

--------------------------------------------------------------------------------
-- Let Bindings

type IdrisLet = IdrisM (Doc Ann)

type IdrisLetDefnLhs = IdrisTelescope Single Maybe

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
instance Definition IdrisLetDefnLhs IdrisTm IdrisLet where
  ([] :- SingleCell nm tp) .= tm =
    -- Unparameterised binding, use @:=@ with an inline type annotation.
    nm <> (maybe mempty (":" <+>) tp) <+> ":=" <\?> tm
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell nm _)) .= tm =
    -- Unannotated parameterised binding: omit the signature, and use @=@.
    nm <+> arguments tele <+> "=" <\?> tm
  (tele :- SingleCell nm tp) .= tm =
    -- Annotated parameterised binding, generate a signature, and use @=@.
    hardlines
    [ nm <+> ":" <+> (pi tele (fromMaybe underscore tp))
    , nm <+> arguments tele <> "=" <\?> tm
    ]

instance Let IdrisLet IdrisTm where
  let_ defns e =
    -- [FIXME: Reed M, 28/09/2025] Try to lay things out in a single line if we can.
    "let" <+> (nest 4 $ hardlines defns) <\> "in" <+> e

--------------------------------------------------------------------------------
-- Terms

type IdrisTm = IdrisM (Doc Ann)

instance Pi (IdrisCell (IdrisArg []) Maybe) IdrisTm where
  pi args body = group $ align (foldr (\arg tp -> cell arg <+> "->" <> line <> tp) body args)

instance Arr (IdrisCell (IdrisArg Maybe) Maybe) IdrisTm where
  arr (Cell _ tp) body = fromMaybe underscore tp <+> "->" <+> body

instance App IdrisTm where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Underscore IdrisTm where
  underscore = "_"

instance Parens IdrisTm where
  parensN n = enclose (duplicate (fromIntegral n) "(") (duplicate (fromIntegral n) ")")

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

instance Builtin IdrisTm "=" (IdrisTm -> IdrisTm -> IdrisTm) where
  mkBuiltin x y = x <+> "=" <+> y

instance Builtin IdrisTm "refl" IdrisTm where
  mkBuiltin = "Refl"

instance Builtin IdrisTm "Type" IdrisTm where
  mkBuiltin = "Type"

--------------------------------------------------------------------------------
-- Modules

type IdrisMod = IdrisM (Doc Ann)
type IdrisHeader = [IdrisM (Doc Ann)]

instance Module IdrisMod IdrisHeader IdrisDefns where
  -- [FIXME: Reed M, 30/09/2025] Adapted from existing code, why do we use @module Main@?
  module_ _ header defns =
    hardlines
    [ "module Main"
    , if null header then mempty else hardline <> hardlines header
    , sepDefns defns
    , mempty
    , "main : IO ()"
    , "main = putStrLn \"\""
    ]

--------------------------------------------------------------------------------
-- Imports

idrisImport :: Text -> IdrisHeader
idrisImport nm = ["import" <+> pretty nm]

instance Import IdrisHeader "Data.Nat" where
  mkImport = mempty

instance Import IdrisHeader "Data.Id" where
  mkImport = mempty
