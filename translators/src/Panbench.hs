-- | Panbench utilities.
module Panbench
  ( -- $generators
    panbenchMain
  -- * Re-exports
  , Lang(..)
  , module Grammar
  ) where

import Data.Text.IO qualified as T

import Numeric.Natural

import Options.Applicative

import Panbench.Lang (Lang)
import Panbench.Lang qualified as Lang
import Grammar

-- * Generators
--
-- $generators

-- | Options to pass to a generator.
data PanbenchOpts = PanbenchOpts
  { optSize :: Natural
  -- ^ The size of the output to generate.
  , optLang :: Lang
  -- ^ The langauge to generate for.
  }
  deriving (Show)

-- | Option parser for @PanbenchOpts@.
panbenchOptParser :: Parser PanbenchOpts
panbenchOptParser = PanbenchOpts
  <$> option auto (long "size" <> metavar "SIZE" <> help "The size of the module to generate.")
  <*> option auto (long "language" <> metavar "LANG" <> help "The language to generate for.")

-- | Construct a @panbench@ generator binary.
--
-- This function is intended to be used as the @main@ function
-- of a @panbench@ generator, and will read in a size and language
-- as @--size@ and @--language@ arguments.
panbenchMain
  :: String
  -- ^ The name of the generator.
  -> (Natural -> Module)
  -- ^ Generate a module of a given size.
  -> IO ()
panbenchMain name gen = do
  opts <- execParser $
    info (panbenchOptParser <**> helper) $ mconcat
      [ fullDesc
      , header ("panbench generator for " <> name)
      ]
  let module_ = gen (optSize opts)
  T.putStr (Lang.render (optLang opts) module_)
