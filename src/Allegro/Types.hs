{-# LANGUAGE TypeFamilies #-}
module Allegro.Types where

import Allegro.C.Types
import Foreign  ( Ptr, ForeignPtr, withForeignPtr )

data Color      = Color { cRed, cGreen, cBlue, cAlpha :: Float }
                  deriving (Eq,Show)


newtype Display = Display (Ptr DISPLAY)
                  deriving (Eq,Show)

class Foreign t where
  type CType t
  foreignPtr :: t -> ForeignPtr (CType t)

withPtr :: Foreign t => t -> (Ptr (CType t) -> IO a) -> IO a
withPtr = withForeignPtr . foreignPtr


newtype Keyboard  = Keyboard (ForeignPtr ())    deriving Eq

instance Foreign Keyboard where
  type CType Keyboard = ()
  foreignPtr (Keyboard t) = t


newtype Mouse     = Mouse    (ForeignPtr ())    deriving Eq

instance Foreign Mouse where
  type CType Mouse = ()
  foreignPtr (Mouse t) = t


newtype Timer     = Timer    (ForeignPtr TIMER) deriving Eq

instance Foreign Timer where
  type CType Timer = TIMER
  foreignPtr (Timer t) = t

newtype Bitmap = Bitmap (ForeignPtr BITMAP) deriving Eq

instance Foreign Bitmap where
  type CType Bitmap = BITMAP
  foreignPtr (Bitmap b) = b

