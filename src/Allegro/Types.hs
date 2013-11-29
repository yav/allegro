{-# LANGUAGE TypeFamilies #-}
module Allegro.Types where

import Allegro.C.Types
import Foreign  ( Ptr, ForeignPtr, withForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

data Color      = Color { cRed, cGreen, cBlue, cAlpha :: Float }
                  deriving (Eq,Show)


newtype Display = Display (Ptr DISPLAY)
                  deriving (Eq,Show)

class Object t where
  type CType t
  foreignPtr :: t -> ForeignPtr (CType t)

withPtr :: Object t => t -> (Ptr (CType t) -> IO a) -> IO a
withPtr = withForeignPtr . foreignPtr

newtype Name t    = Name (Ptr (CType t)) deriving Eq

isNamed :: Object t => t -> Name t -> Bool
isNamed t (Name x) = unsafeForeignPtrToPtr (foreignPtr t) == x


newtype Keyboard  = Keyboard (ForeignPtr ())    deriving Eq

instance Object Keyboard where
  type CType Keyboard = ()
  foreignPtr (Keyboard t) = t


newtype Mouse     = Mouse    (ForeignPtr ())    deriving Eq

instance Object Mouse where
  type CType Mouse = ()
  foreignPtr (Mouse t) = t


newtype Timer     = Timer    (ForeignPtr TIMER) deriving Eq

instance Object Timer where
  type CType Timer = TIMER
  foreignPtr (Timer t) = t

newtype Bitmap = Bitmap (ForeignPtr BITMAP) deriving Eq

instance Object Bitmap where
  type CType Bitmap = BITMAP
  foreignPtr (Bitmap b) = b











