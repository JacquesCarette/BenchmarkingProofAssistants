{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Print.Generic where

import qualified Prettyprinter as P
import Numeric.Natural (Natural)

import Grammar (Visibility)

-- Classes for Keywoards
class Keywords rep where
  import_ :: rep
  assign  :: rep
  recrd   :: rep
  univ    :: rep
  data_   :: rep
  arr     :: rep
  lcons   :: rep
  vcons   :: rep
  typesep :: rep
  natT    :: rep
  strT    :: rep
  vectT   :: rep

-- Generic printing patterns
-- type annotations
class Keywords rep => TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: Visibility -> rep -> rep -> rep

-- Class for  'Doc containers'
class HasDoc c where
  doc :: c ann -> P.Doc ann
  build :: P.Doc ann -> c ann

-- Lifting Prettyprinter functions to HasDoc
-- These should not be exported.
l1 :: HasDoc c => (P.Doc ann -> P.Doc ann) -> c ann -> c ann
l1 f = build . f . doc

l1' :: HasDoc c => (a -> P.Doc ann -> P.Doc ann) -> a -> c ann -> c ann
l1' f a = l1 (f a)

l2 :: HasDoc c => (P.Doc ann -> P.Doc ann -> P.Doc ann) -> c ann -> c ann -> c ann
l2 f x y = build $ f (doc x) (doc y)

ll :: HasDoc c => ([P.Doc ann] -> P.Doc ann) -> [c ann] -> c ann
ll f = build . f . map doc

-- Export
-- 0-ary
hardline, line, softline, softline', emptyDoc, space, lbracket, rbracket, lbrace, rbrace, dquote, comma, pipe,
  dot, semi :: HasDoc c => c ann
hardline  = build P.hardline
line      = build P.line
softline  = build P.softline
softline' = build P.softline'
emptyDoc  = build P.emptyDoc
space     = build P.space
lbracket  = build P.lbracket
rbracket  = build P.rbracket
lbrace    = build P.lbrace
rbrace    = build P.rbrace
dquote    = build P.dquote
comma     = build P.comma
pipe      = build P.pipe
dot       = build P.dot
semi      = build P.semi

-- 1-ary
align, parens, dquotes, braces, group, brackets :: HasDoc c => c ann -> c ann
align    = l1 P.align
parens   = l1 P.parens
dquotes  = l1 P.dquotes
braces   = l1 P.braces
group    = l1 P.group
brackets = l1 P.brackets

-- special 1-ary case
pretty :: (HasDoc c , P.Pretty p) => p -> c ann
pretty = build . P.pretty

-- parametrized 1-ary
nest, indent :: HasDoc c => Int -> c ann -> c ann
nest   = l1' P.nest
indent = l1' P.indent

-- 2-ary
(<+>) :: HasDoc c => c ann -> c ann -> c ann
(<+>) = l2 (P.<+>)

-- list-ary
vcat, vsep, hcat, hsep, sep, fillSep :: HasDoc c => [c ann] -> c ann
vcat    = ll P.vcat
vsep    = ll P.vsep
hcat    = ll P.hcat
hsep    = ll P.hsep
sep     = ll P.sep
fillSep = ll P.fillSep

-- special
punctuate :: HasDoc c => c ann -> [c ann] -> [c ann]
punctuate c l = map build $ P.punctuate (doc c) (map doc l)

encloseSep :: HasDoc c => c ann -> c ann -> c ann -> [c ann] -> c ann
encloseSep a b c l = build $ P.encloseSep (doc a) (doc b) (doc c) (map doc l)

blanklines :: HasDoc c => Natural -> c ann
blanklines n = build $ P.vcat $ replicate (fromIntegral n) P.emptyDoc

concatWith :: HasDoc c => (c ann -> c ann -> c ann) -> [c ann] -> c ann
concatWith f l = build $ P.concatWith (\ x y -> doc $ f (build x) (build y)) (map doc l)

-- Utilities to be shared amongst all Print modules
prettyArgs :: (P.Pretty p, HasDoc b) => p -> (a -> b ann) -> [a] -> b ann
prettyArgs var pr args = if null args then pretty var else pretty var <+> hsep (map pr args)

