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

foreign import ccall unsafe "shal_draw_pieslice"
  shal_draw_pieslice ::
    CFloat -> CFloat ->
    CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> IO ()

foreign import ccall unsafe "shal_draw_filled_pieslice"
  shal_draw_filled_pieslice ::
    CFloat -> CFloat ->
    CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    IO ()


foreign import ccall unsafe "shal_draw_ellipse"
  shal_draw_ellipse ::
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> IO ()

foreign import ccall unsafe "shal_draw_filled_ellipse"
  shal_draw_filled_ellipse ::
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    IO ()

foreign import ccall unsafe "shal_draw_circle"
  shal_draw_circle ::
    CFloat -> CFloat ->
    CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> IO ()

foreign import ccall unsafe "shal_draw_filled_circle"
  shal_draw_filled_circle ::
    CFloat -> CFloat ->
    CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    IO ()




foreign import ccall unsafe "shal_draw_arc"
  shal_draw_arc ::
    CFloat -> CFloat ->
    CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> IO ()

foreign import ccall unsafe "shal_draw_elliptical_arc"
  shal_draw_elliptical_arc ::
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> IO ()

foreign import ccall unsafe "shal_draw_spline"
  shal_draw_spline ::
    Ptr CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> IO ()


foreign import ccall unsafe "shal_draw_ribbon"
  shal_draw_ribbon ::
    Ptr CFloat -> CInt ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> CInt -> IO ()


foreign import ccall unsafe "al_calculate_spline"
  al_calculate_spline ::
    Ptr CFloat -> CInt -> Ptr CFloat -> CFloat -> CInt -> IO ()

foreign import ccall unsafe "al_calculate_arc"
  al_calculate_arc ::
    Ptr CFloat -> CInt ->
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat -> CFloat ->
    CFloat ->
    CInt -> IO ()



