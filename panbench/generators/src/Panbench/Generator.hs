{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Panbench generators.
module Panbench.Generator
  ( GenModule(..)
  , genModuleVia
  ) where

import Control.Monad.IO.Class

import Data.Text (Text)

import GHC.Generics

import Panbench.Grammar
import Panbench.Pretty

import System.IO (Handle)

-- | A generator for a module.
data GenModule size hdr defns =
  GenModule
  { genName :: Text
  -- ^ The name of the module.
  , genHeader :: [hdr]
  -- ^ Generate the module header.
  --
  -- This is used to establish baselines during benchmarking.
  , genBody :: size -> [defns]
  -- ^ Generate the body of the module.
  }
  deriving (Generic)

-- | Generate a module, and render it as 'Text'.
genModuleVia
  :: (Module mod hdr body, MonadIO m)
  => (mod -> Doc ann) -- ^ How to print the module into a 'Doc'.
  -> size -- ^ The module size.
  -> GenModule size hdr body -- ^ The generator.
  -> Handle
  -> m ()
genModuleVia f size (GenModule nm header body) hdl =
  renderVia f (module_ nm (mconcat header) (mconcat (body size))) hdl
