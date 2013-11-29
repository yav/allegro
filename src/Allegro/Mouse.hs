{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Mouse
  ( createMouse
  , FailedToInstallMouse(..)
  ) where

import           Allegro.Types
import           Allegro.C.Mouse

import           Foreign(newForeignPtr,nullPtr)
import           Data.Typeable(Typeable)
import qualified Control.Exception as X
import           Control.Monad(unless)

data FailedToInstallMouse     = FailedToInstallMouse
  deriving (Typeable,Show)

instance X.Exception FailedToInstallMouse

-- | Try to install the keyboard driver.
-- Throws 'FailedToInstallMouse' if something goes wrong.
createMouse :: IO Mouse
createMouse =
  do ok <- al_install_mouse
     unless ok $ X.throwIO FailedToInstallMouse
     Mouse `fmap` newForeignPtr shal_uninstall_mouse_addr nullPtr


