{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Types where

import Foreign
import Allegro.C.Types
import qualified Control.Exception as X
import Data.Typeable

data Exception
  = FailedToInitialize
  | FailedToCreateDisplay
  | FailedToCreateEventQueue
  deriving (Typeable,Show)

instance X.Exception Exception




newtype Display = Display (Ptr DISPLAY)
                  deriving Show


