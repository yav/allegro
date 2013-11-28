module Allegro.C.Font where

#include <allegro5/allegro_font.h>
#include <allegro5/allegro_ttf.h>

foreign import ccall unsafe "al_init_font_addon"
  al_init_font_addon :: IO ()

// No shutdown for font

foreign import ccall unsafe "al_init_ttf_addon"
  al_init_ttf_addon :: IO Bool

foreign import ccall unsafe "al_shutdown_ttf_addon"
  al_shutdown_ttf_addon :: IO ()




