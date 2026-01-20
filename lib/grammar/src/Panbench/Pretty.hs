{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module Panbench.Pretty
  ( Document(..)
  , P.Doc
  , Ann
  -- ** Constants
  , line
  , line'
  -- ** Unary Combinators
  , doubleQuote
  , subscript
  -- ** Binary Combinators
  , (<+>)
  , (<\?>)
  , (<\>)
  -- ** Ternary Combinators
  , enclose
  -- ** List Combinators
  , foldFor
  , hardlinesMap
  , hardlinesFor
  , hardlines
  , hsepMap
  , hsepFor
  , hsep
  , hcat
  , vsepMap
  , vsepFor
  , vsep
  , vcatMap
  , vcatFor
  , vcat
  , punctuate
  -- * Rendering
  , renderAnnotated
  , renderVia
  ) where

import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Foldable
import Data.Monoid
import Data.Text.Encoding qualified as T

import Data.Char (chr)

import Numeric.Natural

import Prettyprinter qualified as P

import Panbench.Prelude

import System.IO (Handle)

--------------------------------------------------------------------------------
-- Classy Pretty Printers

class (Monoid a, IsString a) => Document a where
  -- | A hard line break.
  hardline :: a

  -- | A space.
  space :: a

  -- | Layout with the first argument, but prefer the second when
  -- flattend by a 'group'.
  flatAlt :: a -> a -> a

  -- | Group a document.
  group :: a -> a

  -- | Align a document with the nesting level set to the current column.
  align :: a -> a

  -- | Indent by a number of columns.
  nest :: Int -> a -> a

  -- | Duplicate a document.
  duplicate :: Int -> a -> a

  pretty :: (P.Pretty p) => p -> a

data Ann = Duplicate !Int

instance Document (P.Doc Ann) where
  hardline = P.hardline
  space = " "
  flatAlt = P.flatAlt
  group = P.group
  align = P.align
  nest = P.nest
  duplicate n = P.annotate (Duplicate n)
  pretty = P.pretty

instance (Applicative f, IsString a) => IsString (Ap f a) where
  fromString = pure . fromString

instance (Applicative f, Document a) => (Document (Ap f a)) where
  hardline = pure hardline
  space = pure space
  flatAlt = liftA2 flatAlt
  group = fmap group
  nest n = fmap (nest n)
  align = fmap align
  duplicate n = fmap (duplicate n)
  pretty = pure . pretty

--------------------------------------------------------------------------------
-- Constants


line :: (Document a) => a
line = flatAlt hardline space

line' :: (Document a) => a
line' = flatAlt hardline mempty

--------------------------------------------------------------------------------
-- Unary Combinators

doubleQuote :: (Document a) => a -> a
doubleQuote = enclose "\"" "\""

-- | Add a unicode numeric subscript.
subscript :: (Document doc) => doc -> Natural -> doc
subscript x n = x <> fromString (digits n [])
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

(<+>) :: (Document a) => a -> a -> a
x <+> y = x <> space <> y

-- | Concatenate two documents together with a 'line'.
(<\?>) :: (Document a) => a -> a -> a
(<\?>) x y = x <> group (line <> y)

(<\>) :: (Document a) => a -> a -> a
(<\>) x y = x <> hardline <> y

--------------------------------------------------------------------------------
-- Ternary Combinators

enclose :: (Document a) => a -> a -> a -> a
enclose l r x = l <> x <> r

--------------------------------------------------------------------------------
-- List Combinators

foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
foldFor = flip foldMap

concatMapWith
  :: (Foldable t, Document doc)
  => (doc -> doc -> doc)
  -> (a -> doc)
  -> t a -> doc
concatMapWith c f xs =
  case toList xs of
    [] -> mempty
    (x:xs) -> foldl' (\acc y -> c acc (f y)) (f x) xs

hcat :: (Foldable t, Document doc) => t doc -> doc
hcat = foldMap id

vcatMap :: (Foldable t, Document doc) => (a -> doc) -> t a -> doc
vcatMap = concatMapWith (\x y -> x <> line' <> y)

vcatFor :: (Foldable t, Document doc) => t a -> (a -> doc) -> doc
vcatFor = flip vcatMap

vcat :: (Foldable t, Document doc) => t doc -> doc
vcat = vcatMap id

vsepMap :: (Foldable t, Document doc) => (a -> doc) -> t a -> doc
vsepMap = concatMapWith (\x y -> x <> line <> y)

vsepFor :: (Foldable t, Document doc) => t a -> (a -> doc) -> doc
vsepFor = flip vsepMap

vsep :: (Foldable t, Document doc) => t doc -> doc
vsep = vsepMap id

-- FIXME: All of these should use some variant of foldr1 or something??
hsepMap :: (Foldable t, Document doc) => (a -> doc) -> t a -> doc
hsepMap = concatMapWith (<+>)

hsepFor :: (Foldable t, Document doc) => t a -> (a -> doc) -> doc
hsepFor = flip hsepMap

hsep :: (Foldable t, Document doc) => t doc -> doc
hsep = hsepMap id

hardlinesMap :: (Foldable t, Document doc) => (a -> doc) -> t a -> doc
hardlinesMap = concatMapWith (<\>)

hardlinesFor :: (Foldable t, Document doc) => t a -> (a -> doc) -> doc
hardlinesFor = flip hardlinesMap

hardlines :: (Foldable t, Document doc) => t doc -> doc
hardlines = hardlinesMap id

punctuate :: (Foldable t, Document doc) => doc -> t doc -> [doc]
punctuate sep = loop . toList
  where
    loop [] = []
    loop [d] = [d]
    loop (d:ds) = (d <> sep) : loop ds

--------------------------------------------------------------------------------
-- Rendering

renderAnnotated :: (MonadIO m) => Handle -> P.SimpleDocStream Ann -> m ()
renderAnnotated hdl toks = liftIO $ loop [] toks (pure ())
  where
    -- Using a CPS-d renderer lets us re-run rendering actions multiple times,
    -- which lets us implement the 'Duplicate' annotation without any buffering.
    -- This should be a win for very large files, at the cost of building up
    -- a thunk that is the size of the 'SimpleDocStream'.
    --
    -- The idea here is that we keep a stack of replication counts and previous continuations,
    -- and every 'Duplicate' instruction causes us to push the action we are building
    -- to the stack, and start a fresh frame.
    --
    -- When we encoutner a pop, we then build an action that runs the previous action on the stack, followed
    -- by the current frame 'n' times.
    loop :: [(Int, IO ())] -> P.SimpleDocStream Ann -> IO () -> IO ()
    loop _ P.SFail _ =
      fail $ "renderAnnotated: uncaught SFail in SimpleDocStream. This is a bug in the layout algorithm!"
    loop _ P.SEmpty frame = frame
    loop stack (P.SChar c rest) frame =
      loop stack rest do
        frame
        BS.hPut hdl (UTF8.fromChar c)
    loop stack (P.SText _ t rest) frame =
      loop stack rest do
        frame
        BS.hPut hdl (T.encodeUtf8 t)
    loop stack (P.SLine n rest) frame =
      loop stack rest do
        frame
        -- This should be more memory efficient than allocating 'Text'.
        BS.hPut hdl (UTF8.fromChar '\n')
        replicateM_ n (BS.hPut hdl (UTF8.fromChar ' '))
    loop stack (P.SAnnPush (Duplicate n) rest) frame =
      loop ((n, frame):stack) rest (pure ())
    loop [] (P.SAnnPop _) _ =
      fail $ "renderAnnotated: unmatched SAnnPop in SimpleDocStream. This is a bug in the layout algorithm!"
    loop ((n, prev):stack) (P.SAnnPop rest) frame =
      loop stack rest do
        prev
        replicateM_ n frame

renderVia :: (MonadIO m) => (a -> P.Doc Ann) -> a -> Handle -> m ()
renderVia toDoc a hdl = liftIO $ renderAnnotated hdl $ P.layoutPretty P.defaultLayoutOptions (toDoc a)
