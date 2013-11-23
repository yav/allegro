module Allegro.Types where

import Foreign
import Allegro.C.Types

newtype Display = Display (Ptr DISPLAY)
                  deriving Show

data Keyboard   = Keyboard
data Mouse      = Mouse


