{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Allegro.Graphics
  ( -- * Drawing
    drawBitmap
  , Flip(..)
  , clearToColor
  , Color(..)
    -- * Bitmaps
  , Bitmap
  , loadBitmap
  , bitmapWidth
  , bitmapHeight
    -- * Exceptions
  , FailedToLoadBitmap(..)
  ) where

import           Allegro.Types
import           Allegro.C.Graphics

import qualified Control.Exception as X
import           Control.Monad(when)
import           Foreign.C.String(withCString)
import           Foreign (nullPtr, newForeignPtr)
import           Data.Typeable(Typeable)
import           System.IO.Unsafe(unsafeDupablePerformIO)

loadBitmap :: FilePath -> IO Bitmap
loadBitmap path =
  do ptr <- withCString path al_load_bitmap
     when (ptr == nullPtr) $ X.throwIO $ FailedToLoadBitmap path
     Bitmap `fmap` newForeignPtr al_destroy_bitmap ptr

clearToColor :: Color -> IO ()
clearToColor Color { .. } = shal_clear_to_color (realToFrac cRed)
                                                (realToFrac cGreen)
                                                (realToFrac cBlue)
                                                (realToFrac cAlpha)

data Flip = FlipHorizontal | FlipVertical

drawBitmap :: Bitmap -> Float -> Float -> Maybe Flip -> IO ()
drawBitmap b x y fl = withBitmapPtr b $ \ptr ->
                      al_draw_bitmap ptr (realToFrac x) (realToFrac y) flags
  where
  flags = case fl of
            Nothing -> 0
            Just FlipHorizontal -> flip_horizontal
            Just FlipVertical   -> flip_vertical

-- This is pure because we do not provide any operations for
-- resizing a bitmap.
bitmapWidth :: Bitmap -> Int
bitmapWidth b = fromIntegral
              $ unsafeDupablePerformIO
              $ withBitmapPtr b al_get_bitmap_width

bitmapHeight :: Bitmap -> Int
bitmapHeight b = fromIntegral
               $ unsafeDupablePerformIO
               $ withBitmapPtr b al_get_bitmap_height

--------------------------------------------------------------------------------

data FailedToLoadBitmap = FailedToLoadBitmap FilePath
                          deriving (Show,Typeable)

instance X.Exception FailedToLoadBitmap
