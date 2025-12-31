{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- | Pretty printer for Rocq.
module Panbench.Grammar.Rocq
  ( Rocq
  , RocqOpts
  , RocqEvalStrategy(..)
  , RocqM(..)
  , runRocqM
  , RocqMod
  , RocqHeader
  , RocqDefns
  )
  where

import Control.Applicative
import Control.Monad.Reader

import Data.Default
import Data.Functor
import Data.Functor.Alt
import Data.Maybe
import Data.Monoid (Ap(..))
import Data.Text (Text)

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Prelude
import Panbench.Pretty

-- | Type-level symbol for Rocq.
data Rocq

data RocqOpts = RocqOpts
  { rocqEvalStrategy :: RocqEvalStrategy
  }

instance Default RocqOpts where
  def = RocqOpts
    { rocqEvalStrategy = VmCompute
    }

data RocqEvalStrategy
  = VmCompute
  | NativeCompute

newtype RocqM a = RocqM (Reader RocqOpts a)
  deriving newtype (Functor, Applicative, Monad, MonadReader RocqOpts)

deriving via (Ap RocqM a) instance (Semigroup a) => Semigroup (RocqM a)
deriving via (Ap RocqM a) instance (Monoid a) => Monoid (RocqM a)
deriving via (Ap RocqM a) instance (IsString a) => IsString (RocqM a)
deriving via (Ap RocqM a) instance (Document a) => Document (RocqM a)

runRocqM :: RocqOpts -> RocqM a -> a
runRocqM opts (RocqM m) = runReader m opts

--------------------------------------------------------------------------------
-- Names

type RocqName = RocqM (Doc Ann)

instance Name RocqName where
  nameN x i = x <> pretty i
  replicateName x n = duplicate (fromIntegral n) <$> x

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

-- | Visibility forms a meet semilattice, where @MaximalImplicit <= NonMaximalImplicit <= Visible@.
instance Semigroup RocqVis where
  MaximalImplicit <> _ = MaximalImplicit
  _ <> MaximalImplicit = MaximalImplicit
  NonMaximalImplicit <> _ = NonMaximalImplicit
  _ <> NonMaximalImplicit = NonMaximalImplicit
  Visible <> Visible = Visible

-- | 'Visible' is the top element of the visibility semilattice.
instance Monoid RocqVis where
  mempty = Visible

data RocqArg arity nm = RocqArg { argVis :: RocqVis, argNames :: arity nm }

deriving instance Functor arity => Functor (RocqArg arity)

-- | Applicative instance is basically @Writer@.
instance Applicative arity => Applicative (RocqArg arity) where
  pure nm = RocqArg
    { argVis = mempty
    , argNames = pure nm
    }
  f <*> a = RocqArg
    { argVis = argVis f <> argVis a
    , argNames = argNames f <*> argNames a
    }

instance Alternative arity => Alternative (RocqArg arity) where
  empty = RocqArg
    { argVis = mempty
    , argNames = empty
    }
  x <|> y = RocqArg
    { argVis = argVis x <> argVis y
    , argNames = argNames x <|> argNames y
    }

instance Alt arity => Alt (RocqArg arity) where
  x <!> y = RocqArg
    { argVis = argVis x <> argVis y
    , argNames = argNames x <!> argNames y
    }


type RocqCell arity ann = Cell arity RocqName ann RocqTm
type RocqTelescope arity ann = CellTelescope (RocqArg []) RocqName Maybe RocqTm arity RocqName ann RocqTm

instance Implicit (RocqCell (RocqArg arity) ann) where
  implicit (Cell arg tp) = Cell (arg { argVis = MaximalImplicit }) tp

instance SemiImplicit (RocqCell (RocqArg arity) ann) where
  semiImplicit (Cell arg tp) = Cell (arg { argVis = NonMaximalImplicit }) tp

instance Unbound (RocqCell (RocqArg arity) ann) where
  unbound cell = cell

-- | Apply a Rocq visibility modifier to a document.
withVis :: RocqVis -> RocqM (Doc Ann) -> RocqM (Doc Ann)
withVis Visible = enclose "(" ")"
withVis MaximalImplicit = enclose "{" "}"
withVis NonMaximalImplicit = enclose "[" "]"

-- | Is a 'RocqArg' visible?
isVisible :: RocqArg arity nm -> Bool
isVisible RocqArg { argVis = Visible } = True
isVisible _ = False

-- | Render a Rocq binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
cell
  :: (Foldable arity, Foldable ann)
  => RocqCell (RocqArg arity) ann
  -> RocqM (Doc Ann)
cell (Cell RocqArg{..} tp)
  | null tp = withVis argVis (hsep argNames)
  | otherwise = withVis argVis (hsep argNames <+> ":" <+> hsep tp)

argument
  :: (Foldable arity, Foldable ann)
  => RocqCell (RocqArg arity) ann
  -> RocqM (Doc Ann)
argument (Cell RocqArg{..} _) = hsepMap (withVis argVis) argNames

-- | Render a list of Rocq binding cells, and add a final space if the list is non-empty.
telescope
  :: (Foldable arity, Foldable ann)
  => [RocqCell (RocqArg arity) ann]
  -> RocqM (Doc Ann)
telescope [] = mempty
telescope cells = hsepMap cell cells <> space

arguments
  :: (Foldable arity, Foldable ann)
  => [RocqCell (RocqArg arity) ann]
  -> RocqM (Doc Ann)
arguments [] = mempty
arguments cells = hsepMap argument cells <> space

--------------------------------------------------------------------------------
-- Top-level definitions

type RocqDefns = [RocqM (Doc Ann)]

defn :: RocqM (Doc Ann) -> RocqDefns
defn = pure

instance Definition (RocqTelescope Single Maybe) RocqTm RocqDefns where
  (tele :- SingleCell nm tp) .= tm =
    defn $
    nest 4 $
    "Definition" <+> nm <+> telescope tele <> (maybe mempty (":" <+>) tp) <+> ":=" <\?> tm <> "."

instance Postulate (RocqTelescope Single Single) RocqDefns where
  postulate defns =
    defn $ hardlines $
    defns <&> \(tele :- RequiredCell nm tp) ->
      nest 4 $
      "Axiom" <+> nm <+> ":" <\?>
        pi tele tp <> "."

instance DataDefinition (RocqTelescope Single Single) (RocqCell Single Single) RocqDefns where
  data_ (params :- RequiredCell nm tp) ctors =
    defn $
    "Inductive" <+> nm <+> telescope params <> ":" <+> tp <+> ":=" <\>
    hardlinesFor ctors (\(RequiredCell nm tp) -> nest 4 ("|" <+> nm <+> ":" <\?> tp)) <> "."

-- [TODO: Reed M, 29/09/2025] Technically rocq can omit type signatures on records.
instance RecordDefinition (RocqTelescope Single Single) RocqName (RocqCell Single Single) RocqDefns where
  record_ (params :- (RequiredCell nm tp)) ctor fields
    | all (not . isVisible . cellNames) params =
      defn $ hardlines $
      [ nest 2 $
        "Record" <+> nm <+> telescope params <> ":" <+> tp <+> ":=" <+> ctor <>
        group (line <> "{ " <> hcat (punctuate (line' <> "; ") (fields <&> \(RequiredCell nm tp) -> nm <+> ":" <+> tp)) <> line <> "}.")
      ]
    | otherwise =
      defn $ hardlines $
      [ nest 2 $
        "Record" <+> nm <+> telescope params <> ":" <+> tp <+> ":=" <+> ctor <>
        group (line <> "{ " <> hcat (punctuate (line' <> "; ") (fields <&> \(RequiredCell nm tp) -> nm <+> ":" <+> tp)) <> line <> "}.")
      , mempty
      , "Arguments" <+> ctor <+> arguments (implicit <$> params) <> hsepMap (const "_") fields <> "."
      ]

instance CheckType RocqTm RocqDefns where
  checkType tm tp =
    defn $
    "Check" <+> tm <+> ":" <+> tp <> "."

instance Newline RocqDefns where
  newlines n = defn $ duplicate (fromIntegral n) hardline

--------------------------------------------------------------------------------
-- Let Bindings
--
-- Right now, these are identical to top-level bindings, but in the future they
-- will include different left-hand sides.

type RocqLet = RocqM (Doc Ann)

instance Definition (RocqTelescope Single Maybe) RocqTm RocqLet where
  (tele :- (SingleCell nm tp)) .= tm =
    nest 4 $
    nm <+> telescope tele <> (maybe mempty (\tp -> ":" <+> tp <> space) tp) <> ":=" <\?> tm

instance Let RocqLet RocqTm where
  let_ defns tm =
    group $ foldr (\defn e -> "let" <+> defn <> line <> "in" <+> e) (tm) defns

--------------------------------------------------------------------------------
-- Terms

type RocqTm = RocqM (Doc Ann)

instance Pi (RocqCell (RocqArg []) Maybe) RocqTm where
  pi [] body = body
  pi args tp = "forall" <+> hsepMap cell args <> "," <\?> tp

instance Arr (RocqCell (RocqArg Maybe) Maybe) RocqTm where
  arr (Cell _ arg) tp = fromMaybe underscore arg <+> "->" <+> tp

instance Lam (RocqCell (RocqArg []) Maybe) RocqTm where
  lam [] body = body
  lam args tp = "forall" <+> hsepMap cell args <> "," <\?> tp

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

instance Builtin RocqTm "=" (RocqTm -> RocqTm -> RocqTm) where
  mkBuiltin x y = x <+> "=" <+> y

instance Builtin RocqTm "refl" RocqTm where
  mkBuiltin = "eq_refl"

--------------------------------------------------------------------------------
-- Modules

type RocqMod = RocqM (Doc Ann)
type RocqHeader = [RocqM (Doc Ann)]

instance Module RocqMod RocqHeader RocqDefns where
  module_ nm header body =
    hardlines
    [ if null header then mempty else hardline <> hardlines header
    , "Module" <+> pretty nm <> "."
    , mempty
    , hardlines (punctuate hardline body)
    , mempty
    , "End" <+> pretty nm <> "."
    , mempty
    ]

--------------------------------------------------------------------------------
-- Imports

requireImport :: Text -> RocqHeader
requireImport m = ["Require" <+> "Import" <+> pretty m <> "."]

justImport :: Text -> RocqHeader
justImport m = ["Import" <+> pretty m <> "."]

-- | The equivalent of @Data.Nat@ is built-in for Rocq.
instance Import (RocqHeader) "Data.Nat" where
  mkImport = mempty

-- | The equivalent of @Data.Nat@ is built-in for Rocq.
instance Import (RocqHeader) "Data.Id" where
  mkImport = mempty

-- | The equivalent of @Data.List@ is built-in for Rocq.
instance Import (RocqHeader) "Data.List" where
  mkImport = mempty
