{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Graphics where

import Allegro.C.Types
import Foreign (Ptr, FunPtr)
import Foreign.C.Types(CFloat(..),CInt(..))
import Foreign.C.String(CString)

#include <allegro5/allegro.h>

foreign import ccall unsafe "al_load_bitmap"
  al_load_bitmap :: CString -> IO (Ptr BITMAP)

foreign import ccall unsafe "&al_destroy_bitmap"
  al_destroy_bitmap :: FunPtr (Ptr BITMAP -> IO ())

foreign import ccall unsafe "al_draw_bitmap"
  al_draw_bitmap :: Ptr BITMAP -> CFloat -> CFloat -> CInt -> IO ()

flip_horizontal :: CInt
flip_horizontal = #{const ALLEGRO_FLIP_HORIZONTAL}

flip_vertical :: CInt
flip_vertical = #{const ALLEGRO_FLIP_VERTICAL}

foreign import ccall "shal_clear_to_color"
  shal_clear_to_color :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "al_get_bitmap_width"
  al_get_bitmap_width :: Ptr BITMAP -> IO CInt

foreign import ccall unsafe "al_get_bitmap_height"
  al_get_bitmap_height :: Ptr BITMAP -> IO CInt

