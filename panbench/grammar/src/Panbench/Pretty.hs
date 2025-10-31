{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- |
module Panbench.Pretty
  ( IsDoc
  , Ann
  , doc
  , undoc
  , undocs
  , pretty
  -- * Constants
  , line
  , line'
  , softline
  , softline'
  , hardline
  , space
  -- * Combinators
  -- ** Unary Combinators
  , align
  , nest
  , hang
  , group
  , doubleQuote
  , subscript
  -- ** Binary Combinators
  , (<+>)
  , (<\?>)
  , (<\>)
  , flatAlt
  -- ** Ternary Combinators
  , enclose
  -- ** List Combinators
  , hcat
  , hsep
  , vcat
  , vsep
  , sep
  , hardlines
  , hsepMap
  , hsepFor
  , vcatMap
  , vcatFor
  , hardlinesMap
  , hardlinesFor
  , punctuate
  , listAlt
  -- * Rendering
  , renderVia
  -- * Re-exports
  , P.Doc
  ) where

import Control.Monad.IO.Class

import Data.Kind
import Data.Coerce
import Data.Foldable

import Data.Char (chr)
import Data.String (IsString(..))
import Data.Text.IO qualified as T

import Numeric.Natural

import Prettyprinter qualified as P

import System.IO (Handle, hPutChar)

--------------------------------------------------------------------------------
-- Annotations

data Ann = Replicate !Int

type IsDoc :: Type -> Constraint
type IsDoc doc = (Coercible (P.Doc Ann) doc)

doc :: (IsDoc doc) => P.Doc Ann -> doc
doc = coerce

undoc :: (IsDoc doc) => doc -> P.Doc Ann
undoc = coerce

docs ::(IsDoc doc) => [P.Doc Ann] -> [doc]
docs = coerce

undocs :: (IsDoc doc) => [doc] -> [P.Doc Ann]
undocs = coerce

pretty :: forall a doc. (P.Pretty a, IsDoc doc) => a -> doc
pretty x = coerce @(P.Doc Ann) @_ (P.pretty x)

liftDoc3 :: (IsDoc doc) => (P.Doc Ann -> P.Doc Ann -> P.Doc Ann -> P.Doc Ann) -> doc -> doc -> doc -> doc
liftDoc3 f x y z = coerce (f (coerce x) (coerce y) (coerce z))

--------------------------------------------------------------------------------
-- Constants

line :: (IsDoc doc) => doc
line = doc P.line

line' :: (IsDoc doc) => doc
line' = doc P.line'

softline :: (IsDoc doc) => doc
softline = doc P.softline

softline' :: (IsDoc doc) => doc
softline' = doc P.softline'

hardline :: (IsDoc doc) => doc
hardline = doc P.hardline

space :: (IsDoc doc) => doc
space = doc " "

--------------------------------------------------------------------------------
-- Unary Combinators

liftDoc1 :: (IsDoc doc) => (P.Doc Ann -> P.Doc Ann) -> doc -> doc
liftDoc1 f x = coerce (f (coerce x))

align :: (IsDoc doc) => doc -> doc
align = liftDoc1 P.align

-- | Increase the current indentation level.
--
-- Note that the indentation level increase only has an effect if the
-- after a newline inside the 'nest' call. For example,
--
-- @
-- "x" <+> "=" <> group (line <> nest 2 ("let y = 1 in" <\> "y"))
-- @
--
-- will print as
--
-- @
-- x =
-- let y = 1 in
--   y
-- @
--
-- On the other hand,
--
-- @
-- "x" <+> "=" <> nest 2 (group (line <> nest 2 ("let y = 1 in" <\> "y")))
-- @
--
-- will print as
--
-- @
-- x =
--   let y = 1 in
--   y
-- @
--
-- See https://github.com/quchen/prettyprinter/issues/78
nest :: (IsDoc doc) => Int -> doc -> doc
nest n = liftDoc1 (P.nest n)

hang :: (IsDoc doc) => Int -> doc -> doc
hang n = liftDoc1 (P.hang n)

group :: (IsDoc doc) => doc -> doc
group = liftDoc1 P.group

doubleQuote :: (IsDoc doc) => doc -> doc
doubleQuote = enclose (doc "\"") (doc "\"")

-- | Add a unicode numeric subscript.
subscript :: (IsDoc doc) => doc -> Natural -> doc
subscript x n = x <-> doc (fromString (digits n []))
  where
    -- u2080..u2809 are the characters ₀..₉
    digit :: Natural -> Char
    digit n = chr (0x2080 + fromIntegral n)

    digits :: Natural -> String -> String
    digits n acc | n < 10 = digit n:acc
             | otherwise =
               let (d, r) = n `divMod` 10
               in digits d (digit r:acc)

--------------------------------------------------------------------------------
-- Binary Combinators

liftDoc2 :: (IsDoc doc) => (P.Doc Ann -> P.Doc Ann -> P.Doc Ann) -> doc -> doc -> doc
liftDoc2 f x y = coerce (f (coerce x) (coerce y))

(<+>) :: (IsDoc doc) => doc -> doc -> doc
(<+>) = liftDoc2 (P.<+>)

-- | Only used to avoid redundant 'Semigroup' constraints.
--
-- Mnemonic: If '<+>' adds a space, then '<->' does not.
(<->) :: (IsDoc doc) => doc -> doc -> doc
(<->) = liftDoc2 (<>)

-- | Concatenate two documents together with a 'line'.
(<\?>) :: (IsDoc doc) => doc -> doc -> doc
(<\?>) x y = x <-> group (line <-> y)

(<\>) :: (IsDoc doc) => doc -> doc -> doc
(<\>) x y = x <-> hardline <-> y

flatAlt :: (IsDoc doc) => doc -> doc -> doc
flatAlt = liftDoc2 P.flatAlt

--------------------------------------------------------------------------------
-- Ternary Combinators

enclose :: (IsDoc doc) => doc -> doc -> doc -> doc
enclose = liftDoc3 P.enclose

--------------------------------------------------------------------------------
-- List Combinators

liftDocList :: (IsDoc doc) => ([P.Doc Ann] -> P.Doc Ann) -> [doc] -> doc
liftDocList f xs = coerce (f (coerce xs))

hcat :: (IsDoc doc) => [doc] -> doc
hcat = liftDocList P.hcat

vcat :: (IsDoc doc) => [doc] -> doc
vcat = liftDocList P.vcat

hsep :: (IsDoc doc, Foldable t) => t (doc) -> doc
hsep = liftDocList P.hsep . toList

vsep :: (IsDoc doc) => [doc] -> doc
vsep = liftDocList P.vsep

sep :: (IsDoc doc) => [doc] -> doc
sep = liftDocList P.sep

hardlines :: (IsDoc doc) => [doc] -> doc
hardlines = hardlinesMap id

concatMapWith
  :: (IsDoc doc, Foldable t)
  => (doc -> doc -> doc)
  -> (a -> doc)
  -> t a -> doc
concatMapWith c f xs =
  case toList xs of
    [] -> doc mempty
    (x:xs) -> foldl' (\acc y -> c acc (f y)) (f x) xs

vcatMap :: (IsDoc doc, Foldable t) => (a -> doc) -> t a -> doc
vcatMap = concatMapWith (\x y -> x <-> line' <-> y)

vcatFor :: (IsDoc doc, Foldable t) => t a -> (a -> doc) -> doc
vcatFor = flip vcatMap

-- FIXME: All of these should use some variant of foldr1 or something??
hsepMap :: (IsDoc doc, Foldable t) => (a -> doc) -> t a -> doc
hsepMap = concatMapWith (<+>)

hsepFor :: (IsDoc doc, Foldable t) => t a -> (a -> doc) -> doc
hsepFor = flip hsepMap

hardlinesMap :: (IsDoc doc, Foldable t) => (a -> doc) -> t a -> doc
hardlinesMap = concatMapWith (<\>)

hardlinesFor :: (IsDoc doc, Foldable t) => t a -> (a -> doc) -> doc
hardlinesFor = flip hardlinesMap

punctuate :: forall t doc. (IsDoc doc, Foldable t) => doc -> t (doc) -> [doc]
punctuate p xs = docs (P.punctuate (undoc p) (undocs (toList xs)))

-- | Alternative layouts for when a list is empty.
listAlt
  :: (IsDoc doc, Foldable t)
  => t a
  -> doc
  -> doc
  -> doc
listAlt xs d1 d2 = if null xs then d1 else d2
