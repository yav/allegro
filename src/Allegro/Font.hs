{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Allegro.Font
  ( initialize, shutdown
  , loadFont
  , Flags (..)
  , defaultFlags

  -- * Fonts
  , Font
  , lineHeight
  , ascent
  , descent

  -- * Dimensions of text using this font
  , textWidth
  , textDimensions

  -- * Drawing with the font
  , drawText
  , drawJustifiedText
  , Alignment(..)

  -- * Exceptions
  , FailedToInitializeFonts(..)
  , FailedToLoadFont(..)
  ) where

import Allegro.Types
import Allegro.C.Types
import Allegro.C.Font

import qualified Control.Exception as X
import           Control.Monad ( when, unless )
import           Data.Bits ( (.|.) )
import           Data.Typeable ( Typeable )
import           Foreign  ( Ptr, nullPtr, alloca, peek
                          , ForeignPtr, newForeignPtr, withForeignPtr
                          )
import           Foreign.C.Types(CInt)
import           Foreign.C.String(withCString)
import           System.IO.Unsafe(unsafeDupablePerformIO)


initialize :: IO ()
initialize =
  do al_init_font_addon
     ok <- al_init_ttf_addon
     unless ok $ X.throwIO FailedToInitializeFonts

shutdown :: IO ()
shutdown = al_shutdown_ttf_addon

data FailedToInitializeFonts = FailedToInitializeFonts
  deriving (Typeable,Show)

instance X.Exception FailedToInitializeFonts

newtype Font = Font (ForeignPtr FONT)

withFontPtr :: Font -> (Ptr FONT -> IO a) -> IO a
withFontPtr (Font q) = withForeignPtr q

data Flags = Flags { noKerning, monochrome, noAutoHint :: Bool }

defaultFlags :: Flags
defaultFlags = Flags { noKerning = False
                     , monochrome = False
                     , noAutoHint = False }

loadFont :: FilePath -> Int -> Flags -> IO Font
loadFont file sz Flags { .. } =
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

lineHeight :: Font -> Int
lineHeight = fontInfo al_get_font_line_height

ascent :: Font -> Int
ascent = fontInfo al_get_font_ascent

descent :: Font -> Int
descent = fontInfo al_get_font_descent

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

drawText :: Font -> Color -> Float -> Float -> Alignment -> String -> IO ()
drawText font Color { .. } x y al txt =
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
                  -> Float      -- ^ left text boundary
                  -> Float      -- ^ right text boundary
                  -> Float      -- ^ vertical text position
                  -> Float      -- ^ maximum space between words
                  -> Alignment  -- ^ text alignement within boundary
                  -> String     -- ^ text to draw
                  -> IO ()
drawJustifiedText font Color { .. } x1 x2 y diff al txt =
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



