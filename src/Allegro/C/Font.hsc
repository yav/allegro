{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Font where

import Allegro.C.Types
import Foreign(Ptr,FunPtr)
import Foreign.C.Types
import Foreign.C.String

#include <allegro5/allegro.h>
#include <allegro5/allegro_font.h>
#include <allegro5/allegro_ttf.h>

foreign import ccall unsafe "al_init_font_addon"
  al_init_font_addon :: IO ()

-- No shutdown for font

foreign import ccall unsafe "al_init_ttf_addon"
  al_init_ttf_addon :: IO Bool

foreign import ccall unsafe "al_shutdown_ttf_addon"
  al_shutdown_ttf_addon :: IO ()

foreign import ccall unsafe "al_load_font"
  al_load_font :: CString -> CInt -> CInt -> IO (Ptr FONT)

foreign import ccall unsafe "al_destroy_font"
  al_destroy_font :: Ptr FONT -> IO ()

foreign import ccall unsafe "&al_destroy_font"
  al_destroy_font_addr :: FunPtr (Ptr FONT -> IO ())

foreign import ccall unsafe "al_get_font_line_height"
  al_get_font_line_height :: Ptr FONT -> IO CInt

foreign import ccall unsafe "al_get_font_ascent"
  al_get_font_ascent :: Ptr FONT -> IO CInt

foreign import ccall unsafe "al_get_font_descent"
  al_get_font_descent :: Ptr FONT -> IO CInt

foreign import ccall unsafe "al_get_text_width"
  al_get_text_width :: Ptr FONT -> CString -> IO CInt

foreign import ccall unsafe "al_get_text_dimensions"
  al_get_text_dimensions :: Ptr FONT -> CString
                       -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "shal_draw_text"
  shal_draw_text :: Ptr FONT ->
                    CFloat -> CFloat -> CFloat -> CFloat ->
                    CFloat -> CFloat ->
                    CInt -> CString -> IO ()

foreign import ccall unsafe "shal_draw_justified_text"
  shal_draw_justified_text :: Ptr FONT ->
                    CFloat -> CFloat -> CFloat -> CFloat ->
                    CFloat -> CFloat -> CFloat -> CFloat ->
                    CInt -> CString -> IO ()

ttf_no_kerning :: CInt
ttf_no_kerning   = #{const ALLEGRO_TTF_NO_KERNING}

ttf_monochrome :: CInt
ttf_monochrome   = #{const ALLEGRO_TTF_MONOCHROME}

ttf_no_auto_hint :: CInt
ttf_no_auto_hint = #{const ALLEGRO_TTF_NO_AUTOHINT}

align_left :: CInt
align_left    = #{const ALLEGRO_ALIGN_LEFT}

align_center :: CInt
align_center  = #{const ALLEGRO_ALIGN_CENTER}

align_right :: CInt
align_right   = #{const ALLEGRO_ALIGN_RIGHT}

