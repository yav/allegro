{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Display where

import Foreign
import Foreign.C.Types
import Allegro.C.Types

#include <allegro5/allegro.h>

foreign import ccall "al_create_display"
  al_create_display :: CInt -> CInt -> IO (Ptr DISPLAY)

foreign import ccall "al_destroy_display"
  al_destroy_display :: Ptr DISPLAY -> IO ()


foreign import ccall "al_get_display_event_source"
  al_get_display_event_source :: Ptr DISPLAY -> IO (Ptr EVENT_SOURCE)

foreign import ccall "al_get_backbuffer"
  al_get_backbuffer :: Ptr DISPLAY -> IO (Ptr BITMAP)

foreign import ccall "al_flip_display"
  al_flip_display :: IO ()

foreign import ccall "al_update_display_region"
  al_update_display_region :: CInt -> CInt -> CInt -> CInt -> IO ()



