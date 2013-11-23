{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Mouse where

import Allegro.C.Types

import Foreign
import Foreign.C.Types

#include <allegro5/allegro.h>

foreign import ccall unsafe "al_install_mouse"
  al_install_mouse :: IO Bool

foreign import ccall unsafe "al_uninstall_mouse"
  al_uninstall_mouse :: IO ()

foreign import ccall unsafe "al_is_mouse_installed"
  al_is_mouse_installed :: IO Bool

foreign import ccall unsafe "al_get_mouse_event_source"
  al_get_mouse_event_source :: IO (Ptr EVENT_SOURCE)

foreign import ccall unsafe "al_get_mouse_num_axes"
  al_get_mouse_num_axes :: IO CInt

foreign import ccall unsafe "al_get_mouse_num_buttons"
  al_get_mouse_num_buttons :: IO CInt

foreign import ccall unsafe "al_set_mouse_xy"
  al_set_mouse_xy :: Ptr DISPLAY -> CInt -> CInt -> IO Bool

foreign import ccall unsafe "al_set_mouse_z"
  al_set_mouse_z :: Ptr DISPLAY -> CInt -> CInt -> IO Bool

foreign import ccall unsafe "al_set_mouse_w"
  al_set_mouse_w :: Ptr DISPLAY -> CInt -> CInt -> IO Bool

foreign import ccall unsafe "al_get_mouse_state"
  al_get_mouse_state :: IO (Ptr MOUSE_STATE)

foreign import ccall unsafe "al_get_mouse_state_axis"
  al_get_mouse_state_axis :: Ptr MOUSE_STATE -> CInt -> IO CInt

foreign import ccall unsafe "al_mouse_button_down"
  al_mouse_button_down :: Ptr MOUSE_STATE -> CInt -> IO Bool

