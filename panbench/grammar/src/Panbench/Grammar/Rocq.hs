{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  deriving (Eq)

-- [FIXME: Reed M, 04/11/2025] Should this be configurable?
instance Default RocqVis where
  def = Visible

type RocqMultiCell info = MultiCell info RocqName RocqTm
type RocqSingleCell info = SingleCell info RocqName RocqTm
type RocqAnonCell info = Cell info Maybe RocqName Maybe RocqTm
type RocqRequiredCell info = Cell info Identity RocqName Identity RocqTm

type RocqTelescope hdInfo hdAnn = CellTelescope
   RocqVis [] RocqName Maybe RocqTm
   hdInfo Identity RocqName hdAnn RocqTm

instance Implicit (Cell RocqVis arity nm ann tm) where
  implicit cell = cell { cellInfo = MaximalImplicit }

-- | Apply a Rocq visibility modifier to a document.
withVis :: RocqVis -> RocqM (Doc Ann) -> RocqM (Doc Ann)
withVis Visible = enclose "(" ")"
withVis MaximalImplicit = enclose "{" "}"
withVis NonMaximalImplicit = enclose "[" "]"

-- | Check if a cell is visible.
isVisible :: RocqVis -> Bool
isVisible Visible = True
isVisible _ = False

-- | Render a Rocq binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
cell
  :: (Foldable arity, Foldable tpAnn)
  => Cell RocqVis arity RocqName tpAnn RocqTm
  -> RocqM (Doc Ann)
cell (Cell vis names tp)
  | null tp = withVis vis (hsepMap coerce names)
  | otherwise = withVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

-- | Render a list of Rocq binding cells, and add a final space if the list is non-empty.
telescope
  :: (Foldable arity, Foldable tpAnn)
  => [Cell RocqVis arity RocqName tpAnn RocqTm]
  -> RocqM (Doc Ann)
telescope cells = listAlt cells mempty (hsepMap cell cells <> space)

--------------------------------------------------------------------------------
-- Top-level definitions

type RocqDefns = [RocqM (Doc Ann)]

defn :: RocqM (Doc Ann) -> RocqDefns
defn = pure

type RocqTmDefnLhs = RocqTelescope () Maybe

instance Definition RocqDefns RocqTmDefnLhs RocqTm where
  (tele :- SingleCell _ nm tp) .= tm =
    defn $
    nest 4 $
    "Definition" <+> nm <+> telescope tele <> (maybe mempty (":" <+>) tp) <+> ":=" <\?> tm <> "."

type RocqPostulateDefnLhs = RocqTelescope () Identity

instance Postulate RocqDefns RocqPostulateDefnLhs where
  postulate defns =
    defn $ hardlines $
    defns <&> \(tele :- RequiredCell _ nm tp) ->
      nest 4 $
      "Axiom" <+> nm <+> ":" <\?>
        pi tele tp <> "."

type RocqDataDefnLhs = RocqTelescope () Identity

instance DataDefinition RocqDefns RocqDataDefnLhs (RocqRequiredCell ()) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    defn $
    "Inductive" <+> nm <+> telescope params <> ":" <+> tp <+> ":=" <\>
    hardlinesFor ctors (\(RequiredCell _ nm tp) -> nest 4 ("|" <+> nm <+> ":" <\?> tp)) <> "."

-- [TODO: Reed M, 29/09/2025] Technically rocq can omit type signatures on records.
type RocqRecordDefnLhs = RocqTelescope () Identity

instance RecordDefinition RocqDefns RocqRecordDefnLhs RocqName (RocqRequiredCell ()) where
  record_ (params :- (RequiredCell _ nm tp)) ctor fields
    | all (not . isVisible . cellInfo) params =
      defn $ hardlines $
      [ nest 2 $
        "Record" <+> nm <+> telescope params <> ":" <+> tp <+> ":=" <+> ctor <>
        group (line <> "{ " <> hcat (punctuate (line' <> "; ") (fields <&> \(RequiredCell _ nm tp) -> nm <+> ":" <+> tp)) <> line <> "}.")
      ]
    | otherwise =
      defn $ hardlines $
      [ nest 2 $
        "Record" <+> nm <+> telescope params <> ":" <+> tp <+> ":=" <+> ctor <>
        group (line <> "{ " <> hcat (punctuate (line' <> "; ") (fields <&> \(RequiredCell _ nm tp) -> nm <+> ":" <+> tp)) <> line <> "}.")
      , mempty
      , "Arguments" <+> ctor <+> hsepMap (hsepMap (withVis MaximalImplicit) . cellNames) params <+> hsepMap (const "_") fields <> "."
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
-- newtype RocqLet = RocqLet (Doc Ann)
--   deriving newtype (Semigroup, Monoid, IsString)

type RocqLetDefnLhs = RocqTelescope () Maybe

instance Definition (RocqLet) RocqLetDefnLhs RocqTm where
  (tele :- (SingleCell _ nm tp)) .= tm =
    nest 4 $
    nm <+> telescope tele <> (maybe mempty (\tp -> ":" <+> tp <> space) tp) <> ":=" <\?> tm

instance Let RocqLet RocqTm where
  let_ defns tm =
    group $ foldr (\defn e -> "let" <+> defn <> line <> "in" <+> e) (tm) defns

--------------------------------------------------------------------------------
-- Terms

type RocqTm = RocqM (Doc Ann)

instance Pi (RocqMultiCell RocqVis) RocqTm where
  pi [] body = body
  pi args tp = "forall" <+> hsepMap cell args <> "," <\?> tp

instance Arr RocqTm (RocqAnonCell RocqVis) where
  arr (Cell _ _ arg) tp = fromMaybe underscore arg <+> "->" <+> tp

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
