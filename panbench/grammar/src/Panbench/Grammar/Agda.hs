{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Pretty printer for Agda.
module Panbench.Grammar.Agda
  ( Agda
  , AgdaOpts
  , AgdaM(..)
  , runAgdaM
  , AgdaMod
  , AgdaHeader
  , AgdaDefns
  ) where

import Prelude hiding (pi)

import Control.Monad.Reader

import Data.Coerce
import Data.Default
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.String (IsString(..))
import Data.Text (Text)

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Pretty

data Agda

data AgdaOpts = AgdaOpts
  { agdaFlags :: [Text]
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
    { agdaFlags = []
    }

--------------------------------------------------------------------------------
-- Names

type AgdaName = AgdaM (Doc Ann)

instance Name AgdaName where
  nameN = subscript

--------------------------------------------------------------------------------
-- Cells

data AgdaVis
  = Visible
  -- ^ Visible arguments like @(x : A)@.
  | Implicit
  -- ^ Implicit arguments like @(x : A)@.
  deriving (Eq)

instance Default AgdaVis where
  def = Visible

type AgdaMultiCell info = MultiCell info AgdaName AgdaTm
type AgdaSingleCell info = SingleCell info AgdaName AgdaTm
type AgdaAnonCell info = Cell info Maybe AgdaName Maybe AgdaTm
type AgdaRequiredCell info = Cell info Identity AgdaName Identity AgdaTm

type AgdaTelescope hdInfo hdArity hdAnn = CellTelescope
   AgdaVis [] AgdaName Maybe AgdaTm
   hdInfo hdArity AgdaName hdAnn (AgdaTm)

instance Implicit (Cell AgdaVis arity nm ann tm) where
  implicit cell = cell { cellInfo = Implicit }

-- | Surround a document with the appropriate delimiters for a given 'Visibility'.
agdaVis :: AgdaVis -> AgdaM (Doc Ann) -> AgdaM (Doc Ann)
agdaVis Visible = enclose "(" ")"
agdaVis Implicit = enclose "{" "}"

-- | Render an Agda binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
cell :: (Foldable arity, Foldable tpAnn) => Cell AgdaVis arity AgdaName tpAnn AgdaTm -> AgdaM (Doc Ann)
cell (Cell vis names tp) | null tp = agdaVis vis (hsepMap coerce names)
                         | otherwise = agdaVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

-- | Render a list of Agda binding cells, and add a final space if the list is non-empty
telescope :: (Foldable arity, Foldable tpAnn) => [Cell AgdaVis arity AgdaName tpAnn AgdaTm] -> AgdaM (Doc Ann)
telescope [] = mempty
telescope cells = hsepMap cell cells <> space

-- | Render the names of a list of Agda binding cells, and add a final space if the list is non-empty.
arguments :: (Foldable arity, Foldable tpAnn) => [Cell AgdaVis arity AgdaName tpAnn AgdaTm] -> AgdaM (Doc Ann)
arguments [] = mempty
arguments cells = coerce (hsepMap (hsep . cellNames) cells <> space)

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

type AgdaTmDefnLhs = AgdaTelescope () Maybe Maybe

instance Definition AgdaDefns AgdaTmDefnLhs AgdaTm where
  (UnAnnotatedCells tele :- Cell _ (Just nm) Nothing) .= e =
    defn $
    nm <+> telescope tele <> "=" <\?> e
  (tele :- Cell _ (Just nm) ann) .= e =
    defn $
    hardlines
    [ nest 2 $ nm <+> ":" <+> (pi tele (fromMaybe underscore ann))
    , nest 2 $ nm <+> arguments tele <> "=" <\?> e
    ]
  (UnAnnotatedCells tele :- Cell _ Nothing Nothing) .= e =
    defn $
    "_" <+> "=" <\?> (lam tele e)
  (tele :- Cell _ Nothing ann) .= e =
    defn $
    hardlines
    [ nest 2 $ "_" <+> ":" <+> (pi tele (fromMaybe underscore ann))
    , nest 2 $ "_" <+> "=" <\?> (lam tele e)
    ]

type AgdaPostulateDefnLhs = AgdaTelescope () Identity Identity

instance Postulate AgdaDefns AgdaPostulateDefnLhs where
  postulate defns =
    defn $
    nest 2 $ hardlines
    [ "postulate"
    , hardlines $ defns <&> \(tele :- RequiredCell _ nm tp) ->
        nest 2 (nm <+> ":" <+> (pi tele tp))
    ]

type AgdaDataDefnLhs = AgdaTelescope () Identity Identity

instance DataDefinition AgdaDefns AgdaDataDefnLhs (AgdaRequiredCell ()) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    defn $
    nest 2 $ hardlines
    [ "data" <+> nm <+> telescope params <> ":" <+> tp <+> "where"
    , hardlinesFor ctors \(RequiredCell _ nm tp) ->
        nest 2 $ nm <+> ":" <\?> tp
    ]

type AgdaRecordDefnLhs = AgdaTelescope () Identity Identity

instance RecordDefinition AgdaDefns AgdaRecordDefnLhs AgdaName (AgdaRequiredCell ()) where
  record_ (params :- RequiredCell _ nm tp) ctor fields =
    defn $ hardlines
    [ nest 2 $ hardlines
      [ "record" <+> nm <+> telescope params <> ":" <+> tp <+> "where"
      , "constructor" <+> ctor
      , nest 2 $ hardlines
        [ "field"
        , hardlinesFor fields \(RequiredCell _ nm tp) ->
          nest 2 $ nm <+> ":" <\?> tp
        ]
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

type AgdaLetDefnLhs = AgdaTelescope () Identity Maybe

instance Definition AgdaLet AgdaLetDefnLhs AgdaTm where
  (UnAnnotatedCells tele :- UnAnnotatedCell (SingleCell _ nm _)) .= e =
    nm <+> arguments tele <> "=" <> nest 2 (group (line <> e))
  (tele :- SingleCell _ nm ann) .= e =
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

instance Pi (AgdaMultiCell AgdaVis) AgdaTm where
  pi [] body = body
  pi args body = telescope args <> "→" <+> body

instance Arr AgdaTm (AgdaAnonCell AgdaVis) where
  arr (Cell _ _ tp) body = fromMaybe underscore tp <+> "→" <+> body

instance App AgdaTm where
  app fn args = nest 2 $ group (vsep (fn:args))

instance Lam (AgdaMultiCell AgdaVis) AgdaTm where
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
  module_ nm header defns =
    hardlines
    [ "module" <+> pretty nm <+> "where"
    , if null header then mempty else hardline <> hardlines header
    , sepDefns defns
    , mempty
    ]

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
