{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- | Pretty printer for Agda.
module Panbench.Grammar.Agda
  ( Agda
  , AgdaOpts(..)
  , AgdaM(..)
  , runAgdaM
  , AgdaMod
  , AgdaHeader
  , AgdaDefns
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
import Panbench.Pretty
import Panbench.Prelude

data Agda

newtype AgdaOpts = AgdaOpts
  { agdaFlagsOpt :: [Text]
  }

newtype AgdaM a = AgdaM (Reader AgdaOpts a)
  deriving newtype (Functor, Applicative, Monad, MonadReader AgdaOpts)

deriving via (Ap AgdaM a) instance (Semigroup a) => Semigroup (AgdaM a)
deriving via (Ap AgdaM a) instance (Monoid a) => Monoid (AgdaM a)
deriving via (Ap AgdaM a) instance (IsString a) => IsString (AgdaM a)
deriving via (Ap AgdaM a) instance (Document a) => Document (AgdaM a)

runAgdaM :: AgdaOpts -> AgdaM a -> a
runAgdaM opts (AgdaM m) = runReader m opts

instance Default AgdaOpts where
  def = AgdaOpts
    { agdaFlagsOpt = []
    }

--------------------------------------------------------------------------------
-- Names

type AgdaName = AgdaM (Doc Ann)

instance Name AgdaName where
  nameN = subscript
  replicateName x n = duplicate (fromIntegral n) <$> x

--------------------------------------------------------------------------------
-- Cells

data AgdaVis
  = Visible
  -- ^ Visible arguments like @(x : A)@.
  | Implicit
  -- ^ Implicit arguments like @{x : A}@.

-- | Visibility forms a meet semilattice, where @Implicit <= Visible@.
instance Semigroup AgdaVis where
  Implicit <> _ = Implicit
  _ <> Implicit = Implicit
  Visible <> Visible = Visible

-- | 'Visible' is the top element of the visibility semilattice.
instance Monoid AgdaVis where
  mempty = Visible

data AgdaBound
  = Bound
  -- ^ Arguments that are actually bound as part of a LHS.
  | Unbound
  -- ^ Arguments that are not bound in a LHS.

-- | Binding status forms a meet semilattice, where @Unbound <= Bound@.
instance Semigroup AgdaBound where
  Unbound <> _ = Unbound
  _ <> Unbound = Unbound
  Bound <> Bound = Bound

-- | 'Bound' is the top element of the binding semilattice.
instance Monoid AgdaBound where
  mempty = Bound

data AgdaArg arity nm = AgdaArg { argVis :: AgdaVis, argBound :: AgdaBound, argNames :: arity nm }

deriving instance Functor arity => Functor (AgdaArg arity)

-- | Applicative instance is basically @Writer@.
instance Applicative arity => Applicative (AgdaArg arity) where
  pure nm = AgdaArg
    { argVis = mempty
    , argBound = mempty
    , argNames = pure nm
    }
  f <*> a = AgdaArg
    { argVis = argVis f <> argVis a
    , argBound = argBound f <> argBound a
    , argNames = argNames f <*> argNames a
    }

instance Alternative arity => Alternative (AgdaArg arity) where
  empty = AgdaArg
    { argVis = mempty
    , argBound = mempty
    , argNames = empty
    }
  x <|> y = AgdaArg
    { argVis = argVis x <> argVis y
    , argBound = argBound x <> argBound y
    , argNames = argNames x <|> argNames y
    }

instance Alt arity => Alt (AgdaArg arity) where
  x <!> y = AgdaArg
    { argVis = argVis x <> argVis y
    , argBound = argBound x <> argBound y
    , argNames = argNames x <!> argNames y
    }

type AgdaCell arity ann = Cell arity AgdaName ann AgdaTm

type AgdaTelescope arity ann = CellTelescope (AgdaArg []) AgdaName Maybe AgdaTm arity AgdaName ann AgdaTm

instance Implicit (AgdaCell (AgdaArg arity) ann) where
  implicit (Cell arg tp) = Cell (arg { argVis = Implicit }) tp

instance Unbound (AgdaCell (AgdaArg arity) ann) where
  unbound (Cell arg tp) = Cell (arg { argBound = Unbound }) tp

-- | Surround a document with the appropriate delimiters for a given 'AgdaVis'.
withVis :: AgdaVis -> AgdaM (Doc Ann) -> AgdaM (Doc Ann)
withVis Visible = enclose "(" ")"
withVis Implicit = enclose "{" "}"

-- | Render an Agda binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
cell :: (Foldable arity, Foldable ann) => AgdaCell (AgdaArg arity) ann -> AgdaM (Doc Ann)
cell (Cell AgdaArg{..} tp)
  | null tp = withVis argVis (hsep argNames)
  | otherwise = withVis argVis (hsep argNames <+> ":" <+> hsep tp)

-- | Render a list of Agda binding cells, and add a final space if the list is non-empty
telescope :: (Foldable arity, Foldable ann) => [AgdaCell (AgdaArg arity) ann] -> AgdaM (Doc Ann)
telescope [] = mempty
telescope cells = hsepMap cell cells <> space

-- | Render the bound names of an 'AgdaCell'.
boundNames :: (Alternative arity) => AgdaCell (AgdaArg arity) ann -> arity AgdaName
boundNames (Cell { cellNames = AgdaArg Visible Bound nms }) = nms
boundNames (Cell { cellNames = AgdaArg Implicit Bound nms }) = nms <&> \nm -> withVis Implicit (nm <+> "=" <+> nm)
boundNames (Cell { cellNames = AgdaArg Implicit Unbound _}) = empty
boundNames (Cell { cellNames = AgdaArg Visible Unbound nms}) = underscore <$ nms

-- | Render the names of a list of Agda binding cells, and add a final space if the list is non-empty.
arguments :: (Alternative arity, Foldable arity, Foldable ann) => [AgdaCell (AgdaArg arity) ann] -> AgdaM (Doc Ann)
arguments cells =
  let nms = asum $ boundNames <$> cells
  in if null nms then
    mempty
  else
    hsep nms <> space

--------------------------------------------------------------------------------
-- Top-level definitions

-- | Agda definition groups.
--
-- When rendered, @Doc ann@ inside of the list will be separated by a newline.
type AgdaDefns = [AgdaM (Doc Ann)]

defn :: AgdaM (Doc Ann) -> AgdaDefns
defn = pure

sepDefns :: AgdaDefns -> AgdaM (Doc Ann)
sepDefns defns = hardlines $ punctuate hardline defns

sepDefnsFor :: (Foldable t) => t a -> (a -> AgdaDefns) -> AgdaM (Doc Ann)
sepDefnsFor xs f = sepDefns $ foldMap f xs

type AgdaTmDefnLhs = AgdaTelescope Maybe Maybe

instance Definition AgdaTmDefnLhs AgdaTm AgdaDefns where
  (UnAnnotatedCells tele :- Cell (Just nm) Nothing) .= e =
    defn $
    nm <+> telescope tele <> "=" <\?> e
  (tele :- Cell (Just nm) ann) .= e =
    defn $
    hardlines
    [ nest 2 $ nm <+> ":" <+> (pi tele (fromMaybe underscore ann))
    , nest 2 $ nm <+> arguments tele <> "=" <\?> e
    ]
  (UnAnnotatedCells tele :- Cell Nothing Nothing) .= e =
    defn $
    "_" <+> "=" <\?> (lam tele e)
  (tele :- Cell Nothing ann) .= e =
    defn $
    hardlines
    [ nest 2 $ "_" <+> ":" <+> (pi tele (fromMaybe underscore ann))
    , nest 2 $ "_" <+> "=" <\?> (lam tele e)
    ]

type AgdaPostulateDefnLhs = AgdaTelescope Single Single

instance Postulate AgdaPostulateDefnLhs AgdaDefns where
  postulate defns =
    defn $
    nest 2 $ hardlines
    [ "postulate"
    , hardlines $ defns <&> \(tele :- RequiredCell nm tp) ->
        nest 2 (nm <+> ":" <+> (pi tele tp))
    ]

type AgdaDataDefnLhs = AgdaTelescope Single Single

instance DataDefinition AgdaDataDefnLhs (AgdaCell Single Single) AgdaDefns where
  data_ (params :- RequiredCell nm tp) ctors =
    defn $
    nest 2 $ hardlines
    [ "data" <+> nm <+> telescope params <> ":" <+> tp <+> "where"
    , hardlinesFor ctors \(RequiredCell nm tp) ->
        nest 2 $ nm <+> ":" <\?> tp
    ]

instance RecordDefinition (AgdaTelescope Single Single) AgdaName (AgdaCell Single Single) AgdaDefns where
  record_ (params :- RequiredCell nm tp) ctor fields =
    defn $ hardlines
    [ nest 2 $ hardlines $
      [ "record" <+> nm <+> telescope params <> ":" <+> tp <+> "where"
      , "constructor" <+> ctor
      ] ++
      [ nest 2 $ hardlines
        [ "field"
        , hardlinesFor fields \(RequiredCell nm tp) ->
            nest 2 $ nm <+> ":" <\?> tp
        ]
      | not (null fields)
      ]
    , mempty
    , "open" <+> nm
    ]

deriving via CheckUsingAnonDefinition AgdaTm AgdaDefns instance CheckType AgdaTm AgdaDefns

instance Newline AgdaDefns where
  newlines n = defn $ duplicate (fromIntegral n) hardline

--------------------------------------------------------------------------------
-- Let Bindings
--
-- Right now, these are identical to top-level bindings, but in the future they
-- will include different left-hand sides.

type AgdaLet = AgdaM (Doc Ann)

type AgdaLetDefnLhs = AgdaTelescope Single Maybe

instance Definition AgdaLetDefnLhs AgdaTm AgdaLet where
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell nm _)) .= e =
    nm <+> arguments tele <> "=" <> nest 2 (group (line <> e))
  (tele :- SingleCell nm ann) .= e =
    hardlines
    [ nm <+> ":" <+> (pi tele (fromMaybe underscore ann))
    , nm <+> arguments tele <> "=" <> nest 2 (group (line <> e))
    ]

instance Let (AgdaLet) AgdaTm where
  let_ [] e = e
  let_ defns e =
    "let" <+> nest 4 (hardlines defns) <> line <> "in" <+> nest 3 e

--------------------------------------------------------------------------------
-- Terms

type AgdaTm = AgdaM (Doc Ann)

instance Pi (AgdaCell (AgdaArg []) Maybe) AgdaTm where
  pi [] body = body
  pi args body = telescope args <> "→" <+> body

instance Arr (AgdaCell None Maybe) AgdaTm where
  arr (Cell _ tp) body = fromMaybe underscore tp <+> "→" <+> body

instance App AgdaTm where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Lam (AgdaCell (AgdaArg []) Maybe) AgdaTm where
  lam [] body = body
  lam args body = "λ" <+> arguments args <> "→" <\?> body

instance Underscore AgdaTm where
  underscore = "_"

instance Parens AgdaTm where
  parensN n = enclose (duplicate (fromIntegral n) "(") (duplicate (fromIntegral n) ")")

instance Literal AgdaTm "Nat" Natural where
  mkLit = pretty

instance Builtin AgdaTm "Nat" AgdaTm where
  mkBuiltin = "Nat"

instance Builtin AgdaTm "suc" (AgdaTm -> AgdaTm) where
  mkBuiltin x = "suc" <+> x

instance Builtin AgdaTm "+" (AgdaTm -> AgdaTm -> AgdaTm) where
  mkBuiltin x y = x <+> "+" <+> y

instance Builtin AgdaTm "=" (AgdaTm -> AgdaTm -> AgdaTm) where
  mkBuiltin x y = x <+> "≡" <+> y

instance Builtin AgdaTm "refl" AgdaTm where
  mkBuiltin = "refl"

instance Builtin AgdaTm "Type" AgdaTm where
  mkBuiltin = "Set"

--------------------------------------------------------------------------------
-- Modules

type AgdaHeader = [AgdaM (Doc Ann)]
type AgdaMod = AgdaM (Doc Ann)

instance Module AgdaMod AgdaHeader AgdaDefns where
  module_ nm headers defns =
    hcat
    [ options
    , "module" <+> pretty nm <+> "where" <> hardline
    , header
    , sepDefns defns
    , mempty
    ]
    where
      options :: AgdaM (Doc Ann)
      options = do
        options <- asks agdaFlagsOpt
        if null options then
          mempty
        else
          "{-#" <+> "OPTIONS" <+> hsepMap pretty options <+> "#-}" <> hardline

      header :: AgdaM (Doc Ann)
      header =
        if null headers then mempty else hardline <> hardlines headers

--------------------------------------------------------------------------------
-- Imports

openImport :: Text -> AgdaHeader
openImport m = ["open" <+> "import" <+> pretty m <> hardline]

instance Import AgdaHeader "Data.Nat" where
  mkImport = openImport "Agda.Builtin.Nat"

instance Import AgdaHeader "Data.Id" where
  mkImport = openImport "Agda.Builtin.Equality"

instance Import AgdaHeader "Data.List" where
  mkImport = openImport "Agda.Builtin.List"

instance Import AgdaHeader "Data.String" where
  mkImport = openImport "Agda.Builtin.String"
