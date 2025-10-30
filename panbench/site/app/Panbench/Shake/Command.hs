-- |
module Panbench.Shake.Command
  ( osCommand
  , osCommand_
  ) where

import GHC.Stack

import Development.Shake

import Panbench.Shake.Path

-- | A version of 'command' that uses 'OsString'.
osCommand :: (HasCallStack, CmdResult r) => [CmdOption] -> OsPath -> [String] -> Action r
osCommand opts bin args =
  command opts (decodeOS bin) args

-- | A version of 'command_' that uses 'OsString'.
osCommand_ :: (HasCallStack) => [CmdOption] -> OsPath -> [String] -> Action ()
osCommand_ opts bin args =
  command_ opts (decodeOS bin) args
