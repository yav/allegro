{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Audio where

#include <allegro5/allegro.h>
#include <allegro5/allegro_audio.h>
#include <allegro5/allegro_acodec.h>

foreign import ccall unsafe "al_install_audio"
  al_install_audio :: IO Bool

foreign import ccall unsafe "al_uninstall_audio"
  al_uninstall_audio :: IO ()

foreign import ccall unsafe "al_init_acodec_addon"
  al_init_acodec_addon :: IO Bool

