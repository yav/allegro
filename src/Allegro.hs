{-# LANGUAGE DeriveDataTypeable #-}
module Allegro
  ( -- * Initialization
    allegro

  -- * Colors
  , Color(..)

  -- * Exceptions
  , FailedToInitialize(..)
  , FailedToInitializeFonts(..)
  ) where

import Allegro.Types (Color(..))
import Allegro.C
import Allegro.C.Font

import           Control.Concurrent ( runInBoundThread )
import qualified Control.Exception as X
import           Control.Monad ( unless )
import           Data.Typeable ( Typeable )

allegro :: IO () -> IO ()
allegro prog =
  runInBoundThread $
  do ok <- al_init
     unless ok $ X.throwIO FailedToInitialize
     initializeFonts
     prog `X.finally` shutdownFonts



initializeFonts :: IO ()
initializeFonts =
  do al_init_font_addon
     ok <- al_init_ttf_addon
     unless ok $ X.throwIO FailedToInitializeFonts

shutdownFonts :: IO ()
shutdownFonts = al_shutdown_ttf_addon



data FailedToInitialize = FailedToInitialize
  deriving (Typeable,Show)

instance X.Exception FailedToInitialize

data FailedToInitializeFonts = FailedToInitializeFonts
  deriving (Typeable,Show)

instance X.Exception FailedToInitializeFonts




