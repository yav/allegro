module Allegro.C.Addon where

#include <allegro5/allegro_image.h>

foreign import ccall unsafe "al_init_image_addon"
  al_init_image_addon :: IO Bool

foreign import ccall unsafe "al_shutdown_image_addon"
  al_shutdown_image_addon :: IO ()


