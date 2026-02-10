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

-- | Pretty printer for Lean 4.
module Panbench.Grammar.Lean
  ( Lean
  , LeanOpts(..)
  , LeanM(..)
  , runLeanM
  , LeanMod
  , LeanHeader
  , LeanDefns
  ) where

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

-- | Type-level symbol for Lean.
data Lean

newtype LeanOpts = LeanOpts
  { leanSetOpts :: [(Text, Text)]
  }

instance Default LeanOpts where
  def = LeanOpts []

newtype LeanM a = LeanM (Reader LeanOpts a)
  deriving newtype (Functor, Applicative, Monad, MonadReader LeanOpts)

deriving via (Ap LeanM a) instance (Semigroup a) => Semigroup (LeanM a)
deriving via (Ap LeanM a) instance (Monoid a) => Monoid (LeanM a)
deriving via (Ap LeanM a) instance (IsString a) => IsString (LeanM a)
deriving via (Ap LeanM a) instance (Document a) => Document (LeanM a)

runLeanM :: LeanOpts -> LeanM a -> a
runLeanM opts (LeanM m) = runReader m opts

--------------------------------------------------------------------------------
-- Names

type LeanName = LeanM (Doc Ann)

instance Name LeanName where
  nameN x = subscript x
  replicateName x n = duplicate (fromIntegral n) <$> x

--------------------------------------------------------------------------------
-- Cells

data LeanVis
  = Visible
  -- ^ Visible arguments.
  | Implicit
  -- ^ Implicit arguments, which are written as @{x}@.
  | SemiImplicit
  -- ^ Semi-implicit arguments, which are written as @{{x}}@.

-- | Visibility forms a meet semilattice, where @Implicit <= SemiImplicit <= Visible@.
instance Semigroup LeanVis where
  Implicit <> _ = Implicit
  _ <> Implicit = Implicit
  SemiImplicit <> _ = SemiImplicit
  _ <> SemiImplicit = SemiImplicit
  Visible <> Visible = Visible

-- | 'Visible' is the top element of the visibility semilattice.
instance Monoid LeanVis where
  mempty = Visible

data LeanArg arity nm = LeanArg { argVis :: LeanVis, argNames :: arity nm }

deriving instance Functor arity => Functor (LeanArg arity)

-- | Applicative instance is basically @Writer@.
instance Applicative arity => Applicative (LeanArg arity) where
  pure nm = LeanArg
    { argVis = mempty
    , argNames = pure nm
    }
  f <*> a = LeanArg
    { argVis = argVis f <> argVis a
    , argNames = argNames f <*> argNames a
    }

instance Alternative arity => Alternative (LeanArg arity) where
  empty = LeanArg
    { argVis = mempty
    , argNames = empty
    }
  x <|> y = LeanArg
    { argVis = argVis x <> argVis y
    , argNames = argNames x <|> argNames y
    }

instance Alt arity => Alt (LeanArg arity) where
  x <!> y = LeanArg
    { argVis = argVis x <> argVis y
    , argNames = argNames x <!> argNames y
    }


type LeanCell arity ann = Cell arity LeanName ann LeanTm

type LeanTelescope arity ann = CellTelescope (LeanArg []) LeanName Maybe LeanTm arity LeanName ann LeanTm

instance Implicit (LeanCell (LeanArg arity) ann) where
  implicit (Cell arg tp) = Cell (arg { argVis = Implicit }) tp

instance SemiImplicit (LeanCell (LeanArg arity) ann) where
  semiImplicit (Cell arg tp) = Cell (arg { argVis = SemiImplicit }) tp

-- | Lean 4 combines type signatures and argument lists, so the 'Unbound'
-- modifier is a no-op.
instance Unbound (LeanCell (LeanArg arity) ann) where
  unbound cell = cell

-- | Surround a document with the appropriate delimiters for a given 'LeanVis'.
withVis :: LeanVis -> LeanM (Doc Ann) -> LeanM (Doc Ann)
withVis Visible = enclose "(" ")"
withVis Implicit = enclose "{" "}"
withVis SemiImplicit = enclose "{{" "}}"

-- | Render a Lean binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
cell
  :: (Foldable arity, Foldable ann)
  => LeanCell (LeanArg arity) ann
  -> LeanM (Doc Ann)
cell (Cell LeanArg{..} tp)
  | null tp = withVis argVis (hsep argNames)
  | otherwise = withVis argVis (hsep argNames <+> ":" <+> hsep tp)

telescope
  :: (Foldable arity, Foldable ann)
  => [LeanCell (LeanArg arity) ann]
  -> LeanM (Doc Ann)
telescope [] = mempty
telescope cells = hsepMap cell cells <> space

--------------------------------------------------------------------------------
-- Top-level definitions

sepDefns :: LeanDefns -> LeanM (Doc Ann)
sepDefns defns = hardlines $ punctuate hardline defns

type LeanDefns = [LeanM (Doc Ann)]

defn :: LeanM (Doc Ann) -> LeanDefns
defn = pure

type LeanTmDefnLhs = LeanTelescope Single Maybe

instance Definition LeanTmDefnLhs LeanTm LeanDefns where
  (tele :- SingleCell nm tp) .= tm =
    defn $
    nest 2 $
    "def" <+> nm <+> telescope tele <> (maybe mempty (":" <+>) tp) <+> ":=" <\?>
      tm

type LeanPostulateDefnLhs = LeanTelescope Single Single

instance Postulate LeanPostulateDefnLhs LeanDefns where
  postulate defns =
    defn $ hardlines $
    defns <&> \((tele :- RequiredCell nm tp)) ->
      nest 2 $
      "axiom" <+> nm <+> telescope tele <> ":" <+> tp

type LeanDataDefnLhs = LeanTelescope Single Single

instance DataDefinition LeanDataDefnLhs (LeanCell Single Single) LeanDefns where
  data_ (params :- RequiredCell nm tp) ctors =
    defn $ hardlines
    [ nest 2 $
      "inductive" <+> nm <+> telescope params <> ":" <+> tp <+> "where" <\>
        hardlinesFor ctors \(RequiredCell ctorNm ctorTp) ->
          "|" <+> ctorNm <+> ":" <+> nest 2 (ctorTp)
    , mempty
    , "open" <+> nm
    ]

type LeanRecordDefnLhs = LeanTelescope Single Single

instance RecordDefinition LeanRecordDefnLhs LeanName (LeanCell Single Single) LeanDefns where
  record_ (params :- RequiredCell nm tp) ctor fields =
    defn $
    hardlines
    [ nest 2 $
      hardlines
      [ "structure" <+> nm <+> telescope params <> ":" <+> tp <+> "where"
      , ctor <+> "::"
      , hardlinesFor fields \(RequiredCell fieldNm fieldTp) ->
          fieldNm <+> ":" <+> fieldTp
      ]
    , mempty
    , "open" <+> nm
    ]

instance CheckType LeanTm LeanDefns where
  checkType tm tp =
    defn $
    "#check" <+> "(" <> tm <+> ":" <+> tp <> ")"

instance Newline LeanDefns where
  newlines n = defn $ duplicate (fromIntegral n) hardline

--------------------------------------------------------------------------------
-- Let Bindings

type LeanLet = LeanM (Doc Ann)

type LeanLetDefnLhs = LeanTelescope Single Maybe

instance Definition LeanLetDefnLhs LeanTm LeanLet where
  (tele :- SingleCell nm tp) .= tm =
    nest 4 $
    nm <+> telescope tele <> (maybe mempty (\tp -> ":" <+> tp <> space) tp) <> ":=" <\?> tm

instance Let LeanLet LeanTm where
  let_ defns tm =
    (hardlinesMap ("let" <+>) defns) <\>
    tm

--------------------------------------------------------------------------------
-- Terms

type LeanTm = LeanM (Doc Ann)

instance Pi (LeanCell (LeanArg []) Maybe) LeanTm where
  pi args body = group $ align $ foldr (\arg tp -> cell arg <+> "→" <> line <> tp) body args

instance Arr (LeanCell (LeanArg None) Maybe) LeanTm where
  arr (Cell _ ann) body = fromMaybe underscore ann <+> "→" <+> body

instance App LeanTm where
  app fn args = nest 2 $ group (vsep (fn:args))

-- [TODO: Reed M, 31/12/2025] We render lambdas as @fun x ↦ e@, which is the
-- mathlib convention. However, w e can also render as @fun x => e@: this should
-- be configurable.
instance Lam (LeanCell (LeanArg []) Maybe) LeanTm where
  lam [] body = body
  lam args body = "fun" <+> hsepMap (hsep . argNames . cellNames) args <+> "↦" <\?> body

instance Underscore LeanTm where
  underscore = "_"

instance Parens LeanTm where
  parensN n = enclose (duplicate (fromIntegral n) "(") (duplicate (fromIntegral n) ")")

instance Literal LeanTm "Nat" Natural where
  mkLit = pretty

instance Builtin LeanTm "Nat" LeanTm where
  mkBuiltin = "Nat"

instance Builtin LeanTm "Type" LeanTm where
  mkBuiltin = "Type"

instance Builtin LeanTm "suc" (LeanTm -> LeanTm) where
  mkBuiltin x = "Nat.succ" <+> x

instance Builtin LeanTm "+" (LeanTm -> LeanTm -> LeanTm) where
  mkBuiltin x y = x <+> "+" <+> y

instance Builtin LeanTm "=" (LeanTm -> LeanTm -> LeanTm) where
  mkBuiltin x y = x <+> "=" <+> y

instance Builtin LeanTm "refl" LeanTm where
  mkBuiltin = "rfl"

--------------------------------------------------------------------------------
-- Modules

type LeanHeader = [LeanM (Doc Ann)]

type LeanMod = LeanM (Doc Ann)

instance Module LeanMod LeanHeader LeanDefns where
  module_ _  headers defns =
    hcat
    [ header
    , options
    , sepDefns defns
    , hardline
    ]
    where
      header :: LeanM (Doc Ann)
      header =
        if null headers then
          mempty
        else
          hardlines headers <> hardline

      options :: LeanM (Doc Ann)
      options = do
        options <- asks leanSetOpts
        if null options then
          mempty
        else
          hardlinesMap (\(opt, b) -> "set_option" <+> pretty opt <+> pretty b) options <> hardline <> hardline

--------------------------------------------------------------------------------
-- Imports

-- | The equivalent of @Data.Nat@ is built-in for Lean.
instance Import LeanHeader "Data.Nat" where
  mkImport = mempty

-- | The equivalent of @Data.Id@ is built-in for Lean.
instance Import LeanHeader "Data.Id" where
  mkImport = mempty

-- | The equivalent of @Data.List@ is built-in for Lean.
instance Import LeanHeader "Data.List" where
  mkImport = mempty
