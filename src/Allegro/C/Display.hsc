{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Display where

import Foreign
import Foreign.C.Types
import Allegro.C.Types

#include <allegro5/allegro.h>

foreign import ccall unsafe "al_create_display"
  al_create_display :: CInt -> CInt -> IO (Ptr DISPLAY)

foreign import ccall unsafe "al_destroy_display"
  al_destroy_display :: Ptr DISPLAY -> IO ()

foreign import ccall unsafe "al_get_display_event_source"
  al_get_display_event_source :: Ptr DISPLAY -> IO (Ptr EVENT_SOURCE)

foreign import ccall unsafe "al_get_backbuffer"
  al_get_backbuffer :: Ptr DISPLAY -> IO (Ptr BITMAP)

foreign import ccall unsafe "al_flip_display"
  al_flip_display :: IO ()

foreign import ccall unsafe "al_update_display_region"
  al_update_display_region :: CInt -> CInt -> CInt -> CInt -> IO ()


foreign import ccall unsafe "al_set_new_display_flags"
  al_set_new_display_flags :: CInt -> IO ()


windowed
  , resizeable
  , fullscreen
  :: CInt

windowed = #{const ALLEGRO_WINDOWED}
resizeable = #{const ALLEGRO_RESIZABLE}
fullscreen = #{const ALLEGRO_FULLSCREEN}


