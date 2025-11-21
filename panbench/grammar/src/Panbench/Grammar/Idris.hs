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
withVis :: (Document doc) => IdrisVis -> doc -> doc
withVis Visible = enclose "(" ")"
withVis Implicit = enclose "{" "}"

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
cell
  :: (Foldable arity, Foldable tpAnn)
  => Cell IdrisVis arity IdrisName tpAnn IdrisTm
  -> IdrisM (Doc Ann)
cell (Cell vis names tp) = withVis vis (hsep (punctuate "," names) <+> ":" <+> (foldr const underscore tp))

-- | Render a list of idris binding cells as function arguments.
arguments
  :: (Foldable arity, Foldable tpAnn)
  => [Cell IdrisVis arity IdrisName tpAnn IdrisTm]
  -> IdrisM (Doc Ann)
arguments args = listAlt args mempty (hsepMap (hsep . cellNames) (filter (isVisible . cellInfo) args) <> space)

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


type IdrisTmDefnLhs = IdrisTelescope () Maybe

instance Definition IdrisDefns IdrisTmDefnLhs IdrisTm where
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell _ nm _)) .= tm =
    -- Unclear if Idris supports unannotated top-level bindings?
    defn $
    nest 2 (nm <+> ":" <+> "_") <\>
    nest 2 (nm <+> arguments tele <> "=" <\?> tm)
  (tele :- SingleCell _ nm tp) .= tm =
    defn $
    nest 2 (nm <+> ":" <+> (pi tele (fromMaybe underscore tp))) <\>
    nest 2 (nm <+> arguments tele <> "=" <\?> tm)

type IdrisPostulateDefnLhs = IdrisTelescope () Identity

-- | Idris 2 does not support postulates OOTB, so we need to use the @believe_me : a -> b@
-- primitive to do an unsafe cast. Somewhat annoyingly, we need to actually pick *something*
-- to cast, and that thing really should vary based on the goal (EG: @believe_me Refl@ for equality, etc).
--
-- Pulling on this thread leads to a heap of issues with implicit resolution and requires our postulate
-- code to be type-aware, so we just opt to punt and always use @believe_me ()@. This is
-- unsafe and could lead to segfaults in compiled code, but the alternative is not worth the engineering effort.
instance Postulate IdrisDefns IdrisPostulateDefnLhs where
  postulate lhss =
    namespace "Postulate" (Just Export) $
      foldFor lhss \(tele :- RequiredCell _ nm tp) ->
        tele :- SingleCell () nm (Just tp) .= "believe_me" <+> "()"

type IdrisDataDefnLhs = IdrisTelescope () Identity

instance DataDefinition IdrisDefns IdrisDataDefnLhs (IdrisRequiredCell ()) where
  -- It appears that Idris 2 does not support parameterised inductives?
  data_ (params :- RequiredCell _ nm tp) ctors =
    defn $
    nest 2 $
    "data" <+> nm <+> ":" <+> group ((pi params tp) <> line <> "where") <\>
      hardlinesFor ctors \(RequiredCell _ ctorNm ctorTp) ->
        -- We need to add the parameters as arguments, as Idris does not support parameterised inductives.
        ctorNm <+> ":" <+> nest 2 (pi params ctorTp)

type IdrisRecordDefnLhs = IdrisTelescope () Identity

instance RecordDefinition IdrisDefns IdrisRecordDefnLhs IdrisName (IdrisRequiredCell ()) where
  -- Idris does not have universe levels so it does not allow for a sort annotation
  -- on a record definition.
  record_ (params :- (RequiredCell _ nm _)) ctor fields =
    defn $
    nest 2 $
    "record" <+> nm <+> hsepMap cell params <> listAlt params mempty space <> "where" <\>
      "constructor" <+> ctor <\>
      hardlinesFor fields \(RequiredCell _ fieldNm fieldTp) ->
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
    nm <> (maybe mempty (":" <+>) tp) <+> ":=" <\?> tm
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell _ nm _)) .= tm =
    -- Unannotated parameterised binding: omit the signature, and use @=@.
    nm <+> arguments tele <+> "=" <\?> tm
  (tele :- SingleCell _ nm tp) .= tm =
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

instance Pi (IdrisMultiCell IdrisVis) IdrisTm where
  pi args body = group $ align (foldr (\arg tp -> cell arg <+> "->" <> line <> tp) body args)

instance Arr IdrisTm (IdrisAnonCell IdrisVis) where
  arr (Cell _ _ tp) body = fromMaybe underscore tp <+> "->" <+> body

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
