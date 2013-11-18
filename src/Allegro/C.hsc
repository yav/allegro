{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C (al_init, al_install_system, al_clear_to_color) where

import Foreign
import Foreign.C.Types


#include <allegro5/allegro.h>

al_init :: IO Bool
al_init = al_install_system #{const ALLEGRO_VERSION_INT} fp_atexit

foreign import ccall "al_install_system"
  al_install_system :: CInt -> FunPtr (Ptr () -> IO CInt) -> IO Bool

foreign import ccall "&atexit"
  fp_atexit :: FunPtr (Ptr () -> IO CInt)

foreign import ccall "hsal_clear_to_color"
  al_clear_to_color :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()




