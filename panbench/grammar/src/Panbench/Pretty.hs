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
  , doc
  , undoc
  , undocs
  , pretty
  , renderVia
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
  -- * Re-exports
  , P.Doc
  ) where

import Control.Monad.IO.Class

import Data.Kind
import Data.Coerce
import Data.Foldable

import Data.Char (chr)
import Data.String (IsString(..))

import Numeric.Natural

import Prettyprinter qualified as P
import Prettyprinter.Render.Text qualified as P

import System.IO (Handle)

type IsDoc :: (Type -> Type) -> Constraint
type IsDoc doc = (forall ann. Coercible (P.Doc ann) (doc ann))

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

renderVia :: (MonadIO m) => (a -> P.Doc ann) -> a -> Handle -> m ()
renderVia toDoc a hdl = liftIO $ P.renderIO hdl $ P.layoutPretty P.defaultLayoutOptions (toDoc a)

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

space :: (IsDoc doc) => doc ann
space = doc " "

--------------------------------------------------------------------------------
-- Unary Combinators

liftDoc1 :: (IsDoc doc) => (P.Doc ann -> P.Doc ann) -> doc ann -> doc ann
liftDoc1 f x = coerce (f (coerce x))

align :: (IsDoc doc) => doc ann -> doc ann
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
nest :: (IsDoc doc) => Int -> doc ann -> doc ann
nest n = liftDoc1 (P.nest n)

hang :: (IsDoc doc) => Int -> doc ann -> doc ann
hang n = liftDoc1 (P.hang n)

group :: (IsDoc doc) => doc ann -> doc ann
group = liftDoc1 P.group

doubleQuote :: (IsDoc doc) => doc ann -> doc ann
doubleQuote = enclose (doc "\"") (doc "\"")

-- | Add a unicode numeric subscript.
--
--
-- @
-- @
subscript :: (IsDoc doc) => doc ann -> Natural -> doc ann
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

liftDoc2 :: (IsDoc doc) => (P.Doc ann -> P.Doc ann -> P.Doc ann) -> doc ann -> doc ann -> doc ann
liftDoc2 f x y = coerce (f (coerce x) (coerce y))

(<+>) :: (IsDoc doc) => doc ann -> doc ann -> doc ann
(<+>) = liftDoc2 (P.<+>)

-- | Only used to avoid redundant 'Semigroup' constraints.
--
-- Mnemonic: If '<+>' adds a space, then '<->' does not.
(<->) :: (IsDoc doc) => doc ann -> doc ann -> doc ann
(<->) = liftDoc2 (<>)

-- | Concatenate two documents together with a 'line'.
(<\?>) :: (IsDoc doc) => doc ann -> doc ann -> doc ann
(<\?>) x y = x <-> group (line <-> y)

(<\>) :: (IsDoc doc) => doc ann -> doc ann -> doc ann
(<\>) x y = x <-> hardline <-> y

flatAlt :: (IsDoc doc) => doc ann -> doc ann -> doc ann
flatAlt = liftDoc2 P.flatAlt

--------------------------------------------------------------------------------
-- Ternary Combinators

enclose :: (IsDoc doc) => doc ann -> doc ann -> doc ann -> doc ann
enclose = liftDoc3 P.enclose

--------------------------------------------------------------------------------
-- List Combinators

liftDocList :: (IsDoc doc) => ([P.Doc ann] -> P.Doc ann) -> [doc ann] -> doc ann
liftDocList f xs = coerce (f (coerce xs))

hcat :: (IsDoc doc) => [doc ann] -> doc ann
hcat = liftDocList P.hcat

vcat :: (IsDoc doc) => [doc ann] -> doc ann
vcat = liftDocList P.vcat

hsep :: (IsDoc doc, Foldable t) => t (doc ann) -> doc ann
hsep = liftDocList P.hsep . toList

vsep :: (IsDoc doc) => [doc ann] -> doc ann
vsep = liftDocList P.vsep

sep :: (IsDoc doc) => [doc ann] -> doc ann
sep = liftDocList P.sep

hardlines :: (IsDoc doc) => [doc ann] -> doc ann
hardlines = hardlinesMap id

concatMapWith
  :: (IsDoc doc, Foldable t)
  => (doc ann -> doc ann -> doc ann)
  -> (a -> doc ann)
  -> t a -> doc ann
concatMapWith c f xs =
  case toList xs of
    [] -> doc mempty
    (x:xs) -> foldl' (\acc y -> c acc (f y)) (f x) xs

vcatMap :: (IsDoc doc, Foldable t) => (a -> doc ann) -> t a -> doc ann
vcatMap = concatMapWith (\x y -> x <-> line' <-> y)

vcatFor :: (IsDoc doc, Foldable t) => t a -> (a -> doc ann) -> doc ann
vcatFor = flip vcatMap

-- FIXME: All of these should use some variant of foldr1 or something??
hsepMap :: (IsDoc doc, Foldable t) => (a -> doc ann) -> t a -> doc ann
hsepMap = concatMapWith (<+>)

hsepFor :: (IsDoc doc, Foldable t) => t a -> (a -> doc ann) -> doc ann
hsepFor = flip hsepMap

hardlinesMap :: (IsDoc doc, Foldable t) => (a -> doc ann) -> t a -> doc ann
hardlinesMap = concatMapWith (<\>)

hardlinesFor :: (IsDoc doc, Foldable t) => t a -> (a -> doc ann) -> doc ann
hardlinesFor = flip hardlinesMap

punctuate :: forall t doc ann. (IsDoc doc, Foldable t) => doc ann -> t (doc ann) -> [doc ann]
punctuate p xs = docs (P.punctuate (undoc p) (undocs (toList xs)))

-- | Alternative layouts for when a list is empty.
listAlt
  :: (IsDoc doc, Foldable t)
  => t a
  -> doc ann
  -> doc ann
  -> doc ann
listAlt xs d1 d2 = if null xs then d1 else d2
