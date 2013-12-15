{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Primitives where

import Foreign (Ptr)
import Foreign.C (CFloat(..),CInt(..))

#include <allegro5/allegro5.h>
#include <allegro5/allegro_primitives.h>

foreign import ccall unsafe "shal_draw_line"
  shal_draw_line :: CFloat -> CFloat -> CFloat -> CFloat ->
                    CFloat -> CFloat -> CFloat -> CFloat ->
                    CFloat -> IO ()

foreign import ccall unsafe "shal_draw_triangle"
  shal_draw_triangle :: CFloat -> CFloat ->
                        CFloat -> CFloat ->
                        CFloat -> CFloat ->
                        CFloat -> CFloat -> CFloat -> CFloat ->
                        CFloat -> IO ()

foreign import ccall unsafe "shal_draw_filled_triangle"
  shal_draw_filled_triangle :: CFloat -> CFloat ->
                               CFloat -> CFloat ->
                               CFloat -> CFloat ->
                               CFloat -> CFloat -> CFloat -> CFloat ->
                               IO ()



foreign import ccall unsafe "shal_draw_rectangle"
  shal_draw_rectangle :: CFloat -> CFloat ->
                         CFloat -> CFloat ->
                         CFloat -> CFloat -> CFloat -> CFloat ->
                         CFloat -> IO ()

foreign import ccall unsafe "shal_draw_filled_rectangle"
  shal_draw_filled_rectangle :: CFloat -> CFloat ->
                                CFloat -> CFloat ->
                                CFloat -> CFloat -> CFloat -> CFloat ->
                                IO ()

foreign import ccall unsafe "shal_draw_rounded_rectangle"
  shal_draw_rounded_rectangle ::
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> IO ()

foreign import ccall unsafe "shal_draw_filled_rounded_rectangle"
  shal_draw_filled_rounded_rectangle ::
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    IO ()

foreign import ccall unsafe "al_calculate_spline"
  al_calculate_spline ::
    Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> CInt -> IO ()




