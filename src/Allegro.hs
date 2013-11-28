{-# LANGUAGE DeriveDataTypeable #-}
module Allegro
  ( initialize
  , FailedToInitialize(..)
  ) where

import Allegro.C

import qualified Control.Exception as X
import           Control.Monad ( unless )
import           Data.Typeable ( Typeable )


initialize :: IO ()
initialize =
  do ok <- al_init
     unless ok $ X.throwIO FailedToInitialize

     

data FailedToInitialize = FailedToInitialize
  deriving (Typeable,Show)

instance X.Exception FailedToInitialize



