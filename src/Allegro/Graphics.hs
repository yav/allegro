{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Allegro.Graphics
  ( -- * Drawing
    drawBitmap
  , drawModifiedBitmap
  , DrawOptions
  , normal
  , flipped
  , DrawFlipped(..)
  , rotated
  , scaled
  , tinted
  , region
  , clearToColor
  , Color(..)
  , Point
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
import           Foreign.C.Types(CFloat)
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

data DrawFlipped = FlipHorizontal | FlipVertical

data DrawOptions = DrawOptions
  { drawFlipped :: Maybe DrawFlipped
  , drawRotated :: Maybe (CFloat, CFloat, CFloat)
  , drawScaled  :: Maybe (CFloat, CFloat)
  , drawTinted  :: Maybe (CFloat, CFloat, CFloat, CFloat)
  , drawRegion  :: Maybe (CFloat, CFloat, CFloat, CFloat)
  }

normal :: DrawOptions
normal = DrawOptions { drawFlipped = Nothing
                     , drawRotated = Nothing
                     , drawScaled  = Nothing
                     , drawTinted  = Nothing
                     , drawRegion  = Nothing
                     }

flipped :: DrawFlipped -> DrawOptions -> DrawOptions
flipped x y = y { drawFlipped = Just x }

rotated :: Point    -- ^ Rotate around this point.  (0,0) is top-left.
        -> Float    -- ^ Rotate clock-wise, in radians.
        -> DrawOptions -> DrawOptions
rotated (x,y) r o = o { drawRotated = Just ( realToFrac x
                                           , realToFrac y
                                           , realToFrac r) }

scaled :: Float     -- ^ Horizontal scaling
       -> Float     -- ^ Vertical scaling
       -> DrawOptions -> DrawOptions
scaled x y o = o { drawScaled = Just (realToFrac x, realToFrac y) }

tinted :: Color -> DrawOptions -> DrawOptions
tinted Color { .. } o = o { drawTinted = Just ( realToFrac cRed
                                              , realToFrac cGreen
                                              , realToFrac cBlue
                                              , realToFrac cAlpha
                                              ) }

region :: Point   -- ^ Top-left corener of the region
       -> Float   -- ^ Region width
       -> Float   -- ^ Region height
       -> DrawOptions -> DrawOptions
region (x,y) w h o = o { drawRegion = Just ( realToFrac x
                                           , realToFrac y
                                           , realToFrac w
                                           , realToFrac h
                                           ) }

drawModifiedBitmap :: Bitmap -> Float -> Float -> DrawOptions -> IO ()
drawModifiedBitmap bm x y DrawOptions { .. } =
  withBitmapPtr bm $ \p ->
  case (drawTinted, drawRotated, drawScaled, drawRegion) of
    ( Nothing
      , Nothing
      , Nothing
      , Nothing
      ) -> al_draw_bitmap p dx dy flags

    ( Just (r,g,b,a)
      , Nothing
      , Nothing
      , Nothing
      ) -> shal_draw_tinted_bitmap p r g b a dx dy flags

    ( Nothing
      , Just (cx,cy,angle)
      , Nothing
      , Nothing
      ) -> al_draw_rotated_bitmap p cx cy dx dy angle flags

    ( Just (r,g,b,a)
      , Just (cx,cy,angle)
      , Nothing
      , Nothing
      ) -> shal_draw_tinted_rotated_bitmap p r g b a cx cy dx dy angle flags

    ( Nothing
      , Nothing
      , Just (sH, sV)
      , Nothing
      ) -> al_draw_scaled_bitmap p 0 0 w h dx dy (w * sH) (h * sV) flags

    ( Just (r,g,b,a)
      , Nothing
      , Just (sH, sV)
      , Nothing
      ) -> shal_draw_tinted_scaled_bitmap p r g b a
                                            0 0 w h
                                            dx dy (w * sH) (h * sV) flags
    ( Nothing
      , Just (cx,cy,angle)
      , Just (sH, sV)
      , Nothing
      ) -> al_draw_scaled_rotated_bitmap p cx cy dx dy sH sV angle flags

    ( Just (r,g,b,a)
      , Just (cx,cy,angle)
      , Just (sH, sV)
      , Nothing
      ) -> shal_draw_tinted_scaled_rotated_bitmap
             p r g b a cx cy dx dy sH sV angle flags

    ( Nothing
      , Nothing
      , Nothing
      , Just (sx,sy,sw,sh)
      ) -> al_draw_bitmap_region p sx sy sw sh dx dy flags

    ( Just (r,g,b,a)
      , Nothing
      , Nothing
      , Just (sx,sy,sw,sh)
      ) -> shal_draw_tinted_bitmap_region p r g b a sx sy sw sh dx dy flags

    ( Nothing
      , Just (cx,cy,angle)
      , Nothing
      , Just (sx,sy,sw,sh)
      ) -> shal_draw_tinted_scaled_rotated_bitmap_region p
              sx sy sw sh 1 1 1 1 cx cy dx dy 1 1 angle flags

    ( Just (r,g,b,a)
      , Just (cx,cy,angle)
      , Nothing
      , Just (sx,sy,sw,sh)
      ) -> shal_draw_tinted_scaled_rotated_bitmap_region p
              sx sy sw sh r g b a cx cy dx dy 1 1 angle flags

    ( Nothing
      , Nothing
      , Just (sH, sV)
      , Just (sx,sy,sw,sh)
      ) -> al_draw_scaled_bitmap p sx sy sw sh dx dy (sw * sH) (sh * sV) flags

    ( Just (r,g,b,a)
      , Nothing
      , Just (sH, sV)
      , Just (sx,sy,sw,sh)
      ) -> shal_draw_tinted_scaled_bitmap p r g b a
                      sx sy sw sh
                      dx dy (sw * sH) (sh * sV) flags

    ( Nothing
      , Just (cx,cy,angle)
      , Just (sH, sV)
      , Just (sx,sy,sw,sh)
      ) -> shal_draw_tinted_scaled_rotated_bitmap_region p
              sx sy sw sh 1 1 1 1 cx cy dx dy sH sV angle flags

    ( Just (r,g,b,a)
      , Just (cx,cy,angle)
      , Just (sH, sV)
      , Just (sx,sy,sw,sh)
      ) -> shal_draw_tinted_scaled_rotated_bitmap_region p
              sx sy sw sh r g b a cx cy dx dy sH sV angle flags

  where
  w = fromIntegral (bitmapWidth bm)
  h = fromIntegral (bitmapHeight bm)

  dx = realToFrac x
  dy = realToFrac y

  flags = case drawFlipped of
            Nothing -> 0
            Just FlipHorizontal -> flip_horizontal
            Just FlipVertical   -> flip_vertical




drawBitmap :: Bitmap -> Float -> Float -> IO ()
drawBitmap b x y = withBitmapPtr b $ \ptr ->
                      al_draw_bitmap ptr (realToFrac x) (realToFrac y) 0

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
