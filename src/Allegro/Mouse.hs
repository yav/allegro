{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Mouse
  ( install
  , FailedToInstallMouse(..)
  , uninstall
  , isInstalled
  ) where

import           Allegro.C.Mouse

import           Data.Typeable(Typeable)
import qualified Control.Exception as X
import           Control.Monad(unless)

data FailedToInstallMouse     = FailedToInstallMouse
  deriving (Typeable,Show)

instance X.Exception FailedToInstallMouse

-- | Try to install the keyboard driver.
-- Throws 'FailedToInstallMouse' if something goes wrong.
install :: IO ()
install =
  do ok <- al_install_mouse
     unless ok $ X.throwIO FailedToInstallMouse

uninstall :: IO ()
uninstall = al_uninstall_mouse

isInstalled :: IO Bool
isInstalled = al_is_mouse_installed


