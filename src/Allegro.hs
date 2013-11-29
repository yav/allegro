{-# LANGUAGE DeriveDataTypeable #-}
module Allegro
  ( allegro
  , FailedToInitialize(..)
  ) where

import Allegro.C
import qualified Allegro.Font as Font

import qualified Control.Exception as X
import           Control.Monad ( unless )
import           Data.Typeable ( Typeable )

allegro :: IO () -> IO ()
allegro prog =
  do ok <- al_init
     unless ok $ X.throwIO FailedToInitialize
     Font.initialize
     prog `X.finally` Font.shutdown



data FailedToInitialize = FailedToInitialize
  deriving (Typeable,Show)

instance X.Exception FailedToInitialize



