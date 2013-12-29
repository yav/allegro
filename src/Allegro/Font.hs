{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Allegro.Font
  ( loadFont
  , FontFlags (..)
  , defaultFontFlags

  -- * Fonts
  , Font
  , fontLineHeight
  , fontAscent
  , fontDescent

  -- * Dimensions of text using this font
  , textWidth
  , textDimensions

  -- * Drawing with the font
  , drawText
  , drawJustifiedText
  , Alignment(..)

  -- * Exceptions
  , FailedToLoadFont(..)
  ) where

import Allegro.Types
import Allegro.C.Types
import Allegro.C.Font

import qualified Control.Exception as X
import           Control.Monad ( when )
import           Data.Bits ( (.|.) )
import           Data.Typeable ( Typeable )
import           Foreign  ( Ptr, nullPtr, alloca, peek
                          , ForeignPtr, newForeignPtr, withForeignPtr
                          )
import           Foreign.C.Types(CInt)
import           Foreign.C.String(withCString)
import           System.IO.Unsafe(unsafeDupablePerformIO)


newtype Font = Font (ForeignPtr FONT)

withFontPtr :: Font -> (Ptr FONT -> IO a) -> IO a
withFontPtr (Font q) = withForeignPtr q

data FontFlags = FontFlags { noKerning, monochrome, noAutoHint :: Bool }

defaultFontFlags :: FontFlags
defaultFontFlags = FontFlags { noKerning = False
                             , monochrome = False
                             , noAutoHint = False }

loadFont :: FilePath
         -> Int     -- ^ Font size
         -> IO Font
loadFont file size = loadFontWithFlags file size defaultFontFlags

loadFontWithFlags
  :: FilePath
  -> Int     -- ^ Font size
  -> FontFlags
  -> IO Font
loadFontWithFlags file sz FontFlags { .. } =
  do ptr <- withCString file $ \f -> al_load_font f (fromIntegral sz) fs
     when (ptr == nullPtr) $ X.throwIO (FailedToLoadFont file)
     Font `fmap` newForeignPtr al_destroy_font_addr ptr
  where
  fs = (if noKerning  then ttf_no_kerning   else 0) .|.
       (if monochrome then ttf_monochrome   else 0) .|.
       (if noAutoHint then ttf_no_auto_hint else 0)

data FailedToLoadFont = FailedToLoadFont FilePath
  deriving (Typeable,Show)

instance X.Exception FailedToLoadFont

-- Get some information about a font.
-- This a ssumes that a font won't be changes, hence it is pure.
fontInfo :: (Ptr FONT -> IO CInt) -> Font -> Int
fontInfo info f = fromIntegral $ unsafeDupablePerformIO $ withFontPtr f info

fontLineHeight :: Font -> Int
fontLineHeight = fontInfo al_get_font_line_height

fontAscent :: Font -> Int
fontAscent = fontInfo al_get_font_ascent

fontDescent :: Font -> Int
fontDescent = fontInfo al_get_font_descent

textWidth :: Font -> String -> Int
textWidth f x = fontInfo (\p -> withCString x (al_get_text_width p)) f

textDimensions :: Font -> String -> (Int,Int,Int,Int)
textDimensions f t =
  unsafeDupablePerformIO $
  alloca $ \px ->
  alloca $ \py ->
  alloca $ \pw ->
  alloca $ \ph ->
  withCString t $ \ps -> do withFontPtr f $ \pf ->
                              al_get_text_dimensions pf ps px py pw ph
                            x <- peek px
                            y <- peek py
                            w <- peek pw
                            h <- peek ph
                            return ( fromIntegral x
                                   , fromIntegral y
                                   , fromIntegral w
                                   , fromIntegral h
                                   )

data Alignment = AlignLeft | AlignCenter | AlignRight
                 deriving (Eq,Show)

alignFlags :: Alignment -> CInt
alignFlags al = case al of
                  AlignLeft   -> align_left
                  AlignCenter -> align_center
                  AlignRight  -> align_right

drawText :: Font -> Color -> Point -> Alignment -> String -> IO ()
drawText font Color { .. } (x,y) al txt =
  withCString txt $ \sp ->
  withFontPtr font $ \fp -> shal_draw_text fp (realToFrac cRed)
                                         (realToFrac cGreen)
                                         (realToFrac cBlue)
                                         (realToFrac cAlpha)
                                         (realToFrac x)
                                         (realToFrac y)
                                         (alignFlags al)
                                         sp

drawJustifiedText :: Font       -- ^ use this font
                  -> Color      -- ^ text should have this color
                  -> Point      -- ^ position
                  -> Float      -- ^ right text boundary
                  -> Float      -- ^ maximum space between words
                  -> Alignment  -- ^ text alignement within boundary
                  -> String     -- ^ text to draw
                  -> IO ()
drawJustifiedText font Color { .. } (x1,y) x2 diff al txt =
  withCString txt $ \sp ->
  withFontPtr font $ \fp -> shal_draw_justified_text fp
                           (realToFrac cRed)
                           (realToFrac cGreen)
                           (realToFrac cBlue)
                           (realToFrac cAlpha)
                           (realToFrac x1)
                           (realToFrac x2)
                           (realToFrac y)
                           (realToFrac diff)
                           (alignFlags al)
                           sp



