module Allegro.Types where

import Foreign
import Allegro.C.Types

newtype Display = Display (Ptr DISPLAY)
                  deriving Show

data Keyboard   = Keyboard deriving Show
data Mouse      = Mouse deriving Show

data Color      = Color { cRed, cGreen, cBlue, cAlpha :: Float }
                  deriving (Show,Eq,Ord)


