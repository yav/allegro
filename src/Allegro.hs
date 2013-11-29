{-# LANGUAGE DeriveDataTypeable #-}
module Allegro
  ( -- * Initialization
    allegro

  -- * Objects and Names
  , Object
  , Name
  , isNamed

  -- * Exceptions
  , FailedToInitialize(..)
  ) where

import Allegro.Types (Object, Name, isNamed)
import Allegro.C
import qualified Allegro.Font as Font

import           Control.Concurrent ( runInBoundThread )
import qualified Control.Exception as X
import           Control.Monad ( unless )
import           Data.Typeable ( Typeable )

allegro :: IO () -> IO ()
allegro prog =
  runInBoundThread $
  do ok <- al_init
     unless ok $ X.throwIO FailedToInitialize
     Font.initialize
     prog `X.finally` Font.shutdown



data FailedToInitialize = FailedToInitialize
  deriving (Typeable,Show)

instance X.Exception FailedToInitialize



