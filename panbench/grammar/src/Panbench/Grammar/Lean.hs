{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Pretty printer for Lean 4.
module Panbench.Grammar.Lean
  ( Lean
  , LeanM(..)
  , runLeanM
  , LeanMod
  , LeanHeader
  , LeanDefns
  ) where

import Data.Coerce
import Data.Default
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.String (IsString(..))

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Pretty

-- | Type-level symbol for Lean.
data Lean

newtype LeanM a = LeanM (Identity a)
  deriving newtype (Functor, Applicative, Monad)

deriving via (Ap LeanM a) instance (Semigroup a) => Semigroup (LeanM a)
deriving via (Ap LeanM a) instance (Monoid a) => Monoid (LeanM a)
deriving via (Ap LeanM a) instance (IsString a) => IsString (LeanM a)
deriving via (Ap LeanM a) instance (Document a) => Document (LeanM a)

runLeanM :: LeanM a -> a
runLeanM (LeanM a) = runIdentity a

--------------------------------------------------------------------------------
-- Names

type LeanName = LeanM (Doc Ann)

instance Name LeanName where
  nameN x = subscript x

--------------------------------------------------------------------------------
-- Cells

data LeanVis
  = Visible
  -- ^ Visible arguments.
  | Implicit
  -- ^ Implicit arguments, which are written as @{x}@.
  | SemiImplicit
  -- ^ Semi-implicit arguments, which are written as @{{x}}@.
  deriving (Eq)

instance Default LeanVis where
  def = Visible

type LeanMultiCell info = MultiCell info LeanName LeanTm
type LeanSingleCell info = SingleCell info LeanName LeanTm
type LeanAnonCell info = Cell info Maybe LeanName Maybe LeanTm
type LeanRequiredCell info = Cell info Identity LeanName Identity LeanTm

type LeanTelescope hdInfo hdAnn = CellTelescope
   LeanVis [] LeanName Maybe LeanTm
   hdInfo Identity LeanName hdAnn LeanTm

instance Implicit (Cell LeanVis arity name ann tm) where
  implicit cell = cell { cellInfo = Implicit }

-- | Apply a lean 4 visibility modifier to a document.
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
  :: (Foldable arity, Foldable tpAnn)
  => Cell LeanVis arity LeanName tpAnn LeanTm
  -> LeanM (Doc Ann)
cell (Cell vis names tp)
  | null tp = withVis vis (hsepMap coerce names)
  | otherwise = withVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

telescope
  :: (Foldable arity, Foldable tpAnn)
  => [Cell LeanVis arity LeanName tpAnn LeanTm]
  -> LeanM (Doc Ann)
telescope cells = hsepMap cell cells <> listAlt cells mempty space

--------------------------------------------------------------------------------
-- Top-level definitions

type LeanDefns = [LeanM (Doc Ann)]

defn :: LeanM (Doc Ann) -> LeanDefns
defn = pure

type LeanTmDefnLhs = LeanTelescope () Maybe

instance Definition LeanDefns LeanTmDefnLhs LeanTm where
  (tele :- SingleCell _ nm tp) .= tm =
    defn $
    nest 2 $
    "def" <+> nm <+> telescope tele <> (maybe mempty (":" <+>) tp) <+> ":=" <\?>
      tm

type LeanPostulateDefnLhs = LeanTelescope () Identity

instance Postulate LeanDefns LeanPostulateDefnLhs where
  postulate defns =
    defn $ hardlines $
    defns <&> \((tele :- RequiredCell _ nm tp)) ->
      nest 2 $
      "axiom" <+> nm <+> telescope tele <> ":" <+> tp

type LeanDataDefnLhs = LeanTelescope () Identity

instance DataDefinition LeanDefns LeanDataDefnLhs (LeanRequiredCell ()) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    defn $ hardlines
    [ nest 2 $
      "inductive" <+> nm <+> telescope params <> ":" <+> tp <+> "where" <\>
        hardlinesFor ctors \(RequiredCell _ ctorNm ctorTp) ->
          "|" <+> ctorNm <+> ":" <+> nest 2 (ctorTp)
    , mempty
    , "open" <+> nm
    ]

type LeanRecordDefnLhs = LeanTelescope () Identity

instance RecordDefinition LeanDefns LeanRecordDefnLhs LeanName (LeanRequiredCell ()) where
  record_ (params :- RequiredCell _ nm tp) ctor fields =
    defn $
    hardlines
    [ nest 2 $
      hardlines
      [ "structure" <+> nm <+> telescope params <> ":" <+> tp <+> "where"
      , ctor <+> "::"
      , hardlinesFor fields \(RequiredCell _ fieldNm fieldTp) ->
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

type LeanLetDefnLhs = LeanTelescope () Maybe

instance Definition LeanLet LeanLetDefnLhs LeanTm where
  (tele :- SingleCell _ nm tp) .= tm =
    nest 4 $
    nm <+> telescope tele <> (maybe mempty (\tp -> ":" <+> tp <> space) tp) <> ":=" <\?> tm

instance Let LeanLet LeanTm where
  let_ defns tm =
    (hardlinesMap ("let" <+>) defns) <\>
    tm

--------------------------------------------------------------------------------
-- Terms

type LeanTm = LeanM (Doc Ann)

instance Pi (LeanMultiCell LeanVis) LeanTm where
  pi args body = group $ align $ foldr (\arg tp -> cell arg <+> "→" <> line <> tp) body args

instance Arr LeanTm (LeanAnonCell LeanVis) where
  arr (Cell _ _ ann) body = fromMaybe underscore ann <+> "→" <+> body

instance App LeanTm where
  app fn args = nest 2 $ group (vsep (fn:args))

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
  module_ _  header body =
    hardlines
    [ hardlines header
    , hardlines (punctuate hardline body)
    ]

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
