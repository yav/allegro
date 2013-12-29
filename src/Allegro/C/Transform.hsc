{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Transform where

import Allegro.C.Types (TRANSFORM)
import Foreign.Ptr ( Ptr )
import Foreign.C.Types ( CFloat(..) )

#include <allegro5/allegro.h>


transform_size_bytes :: Int
transform_size_bytes = #{size ALLEGRO_TRANSFORM}

foreign import ccall unsafe "al_identity_transform"
  al_identity_transform :: Ptr TRANSFORM -> IO ()

foreign import ccall unsafe "al_compose_transform"
  al_compose_transform :: Ptr TRANSFORM -> Ptr TRANSFORM -> IO ()

foreign import ccall unsafe "al_translate_transform"
  al_translate_transform :: Ptr TRANSFORM -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "al_rotate_transform"
  al_rotate_transform :: Ptr TRANSFORM -> CFloat -> IO ()

foreign import ccall unsafe "al_scale_transform"
  al_scale_transform :: Ptr TRANSFORM -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "al_build_transform"
  al_build_transform :: Ptr TRANSFORM ->
                        CFloat -> CFloat ->
                        CFloat -> CFloat ->
                        CFloat -> IO ()

foreign import ccall unsafe "al_use_transform"
  al_use_transform :: Ptr TRANSFORM -> IO ()

foreign import ccall unsafe "al_transform_coordinates"
  al_transform_coordinates :: Ptr TRANSFORM -> Ptr CFloat -> Ptr CFloat -> IO ()


