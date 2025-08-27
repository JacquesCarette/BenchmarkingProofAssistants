{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-- |
module Panbench.Pretty
  ( IsDoc
  , doc
  , pretty
  , renderVia
  -- * Constants
  , line
  , line'
  , softline
  , softline'
  , hardline
  -- * Combinators
  -- ** Unary Combinators
  , align
  , nest
  , hang
  , doubleQuote
  -- ** Binary Combinators
  , (<+>)
  , (<\?>)
  , (<\>)
  -- ** Ternary Combinators
  , enclose
  -- ** List Combinators
  , hsep
  , vsep
  , sep
  , vcat
  , hardlines
  , hsepMap
  , vcatMap
  , hardlinesMap
  , punctuate
  -- * Re-exports
  , P.Doc
  ) where

import Data.Kind
import Data.Coerce

import Data.Text (Text)

import Prettyprinter qualified as P
import Prettyprinter.Render.Text qualified as P

type IsDoc (doc :: Type -> Type) = forall ann. Coercible (P.Doc ann) (doc ann)

doc :: (IsDoc doc) => P.Doc ann -> doc ann
doc = coerce

undoc :: (IsDoc doc) => doc ann -> P.Doc ann
undoc = coerce

docs ::(IsDoc doc) => [P.Doc ann] -> [doc ann]
docs = coerce

undocs :: (IsDoc doc) => [doc ann] -> [P.Doc ann]
undocs = coerce

pretty :: forall a doc ann. (P.Pretty a, IsDoc doc) => a -> doc ann
pretty x = coerce @(P.Doc ann) @_ (P.pretty x)

liftDoc3 :: (IsDoc doc) => (P.Doc ann -> P.Doc ann -> P.Doc ann -> P.Doc ann) -> doc ann -> doc ann -> doc ann -> doc ann
liftDoc3 f x y z = coerce (f (coerce x) (coerce y) (coerce z))

renderVia :: (a -> P.Doc ann) -> a -> Text
renderVia toDoc = P.renderStrict . P.layoutPretty P.defaultLayoutOptions . toDoc

--------------------------------------------------------------------------------
-- Constants

line :: (IsDoc doc) => doc ann
line = doc P.line

line' :: (IsDoc doc) => doc ann
line' = doc P.line'

softline :: (IsDoc doc) => doc ann
softline = doc P.softline

softline' :: (IsDoc doc) => doc ann
softline' = doc P.softline'

hardline :: (IsDoc doc) => doc ann
hardline = doc P.hardline

--------------------------------------------------------------------------------
-- Unary Combinators

liftDoc1 :: (IsDoc doc) => (P.Doc ann -> P.Doc ann) -> doc ann -> doc ann
liftDoc1 f x = coerce (f (coerce x))

align :: (IsDoc doc) => doc ann -> doc ann
align = liftDoc1 P.align

nest :: (IsDoc doc) => Int -> doc ann -> doc ann
nest n = liftDoc1 (P.nest n)

hang :: (IsDoc doc) => Int -> doc ann -> doc ann
hang n = liftDoc1 (P.hang n)

doubleQuote :: (IsDoc doc) => doc ann -> doc ann
doubleQuote = enclose (doc "\"") (doc "\"")

--------------------------------------------------------------------------------
-- Binary Combinators

liftDoc2 :: (IsDoc doc) => (P.Doc ann -> P.Doc ann -> P.Doc ann) -> doc ann -> doc ann -> doc ann
liftDoc2 f x y = coerce (f (coerce x) (coerce y))

(<+>) :: (IsDoc doc) => doc ann -> doc ann -> doc ann
(<+>) = liftDoc2 (P.<+>)

-- | Only used to avoid redundant 'Semigroup' constraints.
cat :: (IsDoc doc) => doc ann -> doc ann -> doc ann
cat = liftDoc2 (<>)

(<\?>) :: (IsDoc doc) => doc ann -> doc ann -> doc ann
(<\?>) x y = x `cat` softline `cat` y

(<\>) :: (IsDoc doc) => doc ann -> doc ann -> doc ann
(<\>) x y = x `cat` hardline `cat` y

--------------------------------------------------------------------------------
-- Ternary Combinators

enclose :: (IsDoc doc) => doc ann -> doc ann -> doc ann -> doc ann
enclose = liftDoc3 P.enclose

--------------------------------------------------------------------------------
-- List Combinators

liftDocList :: (IsDoc doc) => ([P.Doc ann] -> P.Doc ann) -> [doc ann] -> doc ann
liftDocList f xs = coerce (f (coerce xs))

hsep :: (IsDoc doc) => [doc ann] -> doc ann
hsep = liftDocList P.hsep

vsep :: (IsDoc doc) => [doc ann] -> doc ann
vsep = liftDocList P.vsep

sep :: (IsDoc doc) => [doc ann] -> doc ann
sep = liftDocList P.sep

vcat :: (IsDoc doc) => [doc ann] -> doc ann
vcat = liftDocList P.vcat

hardlines :: (IsDoc doc) => [doc ann] -> doc ann
hardlines = foldr (<\>) (doc mempty)

vcatMap :: (IsDoc doc, Foldable t) => (a -> doc ann) -> t a -> doc ann
vcatMap f = foldr (\a doc -> f a `cat` line' `cat` doc) (doc mempty)

hsepMap :: (IsDoc doc, Foldable t) => (a -> doc ann) -> t a -> doc ann
hsepMap f = foldr (\a doc -> f a <+> doc) (doc mempty)

hardlinesMap :: (IsDoc doc, Foldable t) => (a -> doc ann) -> t a -> doc ann
hardlinesMap f = foldr (\a doc -> f a <\> doc) (doc mempty)

punctuate :: forall doc ann. (IsDoc doc) => doc ann -> [doc ann] -> [doc ann]
punctuate p xs = docs (P.punctuate (undoc p) (undocs xs))
