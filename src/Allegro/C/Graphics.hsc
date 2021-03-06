{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Graphics where

import Allegro.C.Types
import Foreign (Ptr, FunPtr)
import Foreign.C.Types(CFloat(..),CInt(..))
import Foreign.C.String(CString)

#include <allegro5/allegro.h>

foreign import ccall unsafe "al_set_target_bitmap"
  al_set_target_bitmap :: Ptr BITMAP -> IO ()

foreign import ccall unsafe "al_set_target_backbuffer"
  al_set_target_backbuffer :: Ptr DISPLAY -> IO ()

foreign import ccall unsafe "al_get_current_display"
  al_get_current_display :: IO (Ptr DISPLAY)

foreign import ccall unsafe "al_create_bitmap"
  al_create_bitmap :: CInt -> CInt -> IO (Ptr BITMAP)

foreign import ccall unsafe "al_load_bitmap"
  al_load_bitmap :: CString -> IO (Ptr BITMAP)

foreign import ccall unsafe "&al_destroy_bitmap"
  al_destroy_bitmap :: FunPtr (Ptr BITMAP -> IO ())

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



foreign import ccall unsafe "al_draw_bitmap"
  al_draw_bitmap :: Ptr BITMAP -> CFloat -> CFloat -> CInt -> IO ()

foreign import ccall unsafe "al_draw_rotated_bitmap"
  al_draw_rotated_bitmap :: Ptr BITMAP ->
    CFloat -> CFloat -> CFloat -> CFloat -> CFloat
    -> CInt -> IO ()

foreign import ccall unsafe "al_draw_scaled_bitmap"
  al_draw_scaled_bitmap :: Ptr BITMAP ->
    CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat
    -> CInt -> IO ()

foreign import ccall unsafe "al_draw_scaled_rotated_bitmap"
  al_draw_scaled_rotated_bitmap :: Ptr BITMAP ->
    CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat
    -> CInt -> IO ()

foreign import ccall unsafe "al_draw_bitmap_region"
  al_draw_bitmap_region :: Ptr BITMAP ->
    CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat
    -> CInt -> IO ()

foreign import ccall unsafe "shal_draw_tinted_bitmap"
  shal_draw_tinted_bitmap :: Ptr BITMAP ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CFloat -> CFloat ->
  CInt -> IO ()

foreign import ccall unsafe "shal_draw_tinted_scaled_bitmap"
  shal_draw_tinted_scaled_bitmap :: Ptr BITMAP ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CInt -> IO ()

foreign import ccall unsafe "shal_draw_tinted_scaled_rotated_bitmap"
  shal_draw_tinted_scaled_rotated_bitmap :: Ptr BITMAP ->
   CFloat -> CFloat -> CFloat -> CFloat ->
   CFloat -> CFloat -> CFloat -> CFloat ->
   CFloat -> CFloat ->
   CFloat ->
   CInt -> IO ()

foreign import ccall unsafe "shal_draw_tinted_bitmap_region"
  shal_draw_tinted_bitmap_region :: Ptr BITMAP ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CFloat -> CFloat ->
  CInt -> IO ()

foreign import ccall unsafe "shal_draw_tinted_rotated_bitmap"
  shal_draw_tinted_rotated_bitmap :: Ptr BITMAP ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CFloat -> CFloat ->
  CFloat -> CFloat ->
  CFloat ->
  CInt -> IO ()

foreign import ccall unsafe "shal_draw_tinted_scaled_rotated_bitmap_region"
  shal_draw_tinted_scaled_rotated_bitmap_region :: Ptr BITMAP ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CFloat -> CFloat -> CFloat -> CFloat ->
  CFloat -> CFloat -> CFloat ->
  CInt -> IO ()




