{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.DisplayMode where

import Foreign
import Foreign.C.Types
import Allegro.C.Types

#include <allegro5/allegro.h>

foreign import ccall unsafe "al_get_num_display_modes"
  al_get_num_display_modes :: IO CInt

foreign import ccall unsafe "al_get_display_mode"
  al_get_display_mode :: CInt -> Ptr DISPLAY_MODE -> IO (Ptr DISPLAY_MODE)

display_mode_size_bytes :: Int
display_mode_size_bytes = #{size ALLEGRO_DISPLAY_MODE}

display_mode_width :: Ptr DISPLAY_MODE -> IO CInt
display_mode_width = #{peek ALLEGRO_DISPLAY_MODE,width}

display_mode_height :: Ptr DISPLAY_MODE -> IO CInt
display_mode_height = #{peek ALLEGRO_DISPLAY_MODE,height}

display_mode_format :: Ptr DISPLAY_MODE -> IO CInt
display_mode_format = #{peek ALLEGRO_DISPLAY_MODE,format}

display_mode_refresh_rate :: Ptr DISPLAY_MODE -> IO CInt
display_mode_refresh_rate = #{peek ALLEGRO_DISPLAY_MODE,refresh_rate}


foreign import ccall unsafe "al_get_pixel_size"
  al_get_pixel_size :: CInt -> CInt

foreign import ccall unsafe "al_get_pixel_format_bits"
  al_get_pixel_format_bits :: CInt -> CInt

