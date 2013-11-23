{-# LANGUAGE RecordWildCards #-}
module Allegro.DisplayMode where

import Allegro.C.DisplayMode
import Foreign
import Foreign.C.Types

newtype PixelFormat = PixelFormat CInt
                      deriving Show

data DisplayMode = DisplayMode
  { dmWidth       :: Int
  , dmHeight      :: Int
  , dmPixelFormat :: PixelFormat
  , dmRefreshRate :: Maybe Int
  } deriving Show

displayModes :: IO [DisplayMode]
displayModes =
  do n <- al_get_num_display_modes
     allocaBytes display_mode_size_bytes (go (n-1) [])

  where
  go n res p
    | n >= 0 =
      do q <- al_get_display_mode n p
         if q == nullPtr
           then go (n-1) res p
           else do dmWidth       <- fromIntegral `fmap` display_mode_width q
                   dmHeight      <- fromIntegral `fmap` display_mode_height q
                   dmPixelFormat <- PixelFormat  `fmap` display_mode_format q
                   dmRefreshRate <-
                     do x <- display_mode_refresh_rate q
                        return (if x == 0 then Nothing
                                          else Just (fromIntegral x))
                   go (n-1) (DisplayMode { .. } : res) p
    | otherwise = return res

