{-# LANGUAGE RecordWildCards #-}
module Allegro.Primitives
  ( drawLine
  , DrawOptions(..)
  , drawTriangle
  , drawRectangle
  , drawRoundedRectangle
  , calculateSpline
  ) where

import Allegro.Types (Point, Color(..))
import Allegro.C.Primitives

import Foreign(allocaArray,pokeElemOff,peek,peekElemOff,sizeOf,advancePtr)
import Foreign.Ptr(Ptr)
import Foreign.C(CFloat,CInt)
import GHC.IOArray(unsafeWriteIOArray)
import Data.Array.Unsafe(unsafeFreeze)
import Data.Array(Array)
import Data.Array.MArray(newArray_)
import Data.Array.IO(IOArray)

drawLine :: Point -> Point
         -> Color
         -> Float   -- ^ Thikness
         -> IO ()
drawLine (x1,y1) (x2,y2) Color { .. } thick =
  shal_draw_line (f x1) (f y1) (f x2) (f y2)
                 (f cRed) (f cGreen) (f cBlue) (f cAlpha)
                 (f thick)

data DrawOptions = Outlined Float -- ^ Draw an outline. 0 for hairline outline.
                 | Filled         -- ^ Fill the shape

drawTriangle :: Point -> Point -> Point -> Color -> DrawOptions -> IO ()
drawTriangle (x1,y1) (x2,y2) (x3,y3) Color { .. } how =
  case how of
    Outlined thick -> shal_draw_triangle (f x1) (f y1)
                                         (f x2) (f y2)
                                         (f x3) (f y3)
                                         (f cRed) (f cGreen)
                                         (f cBlue) (f cAlpha)
                                         (f thick)
    Filled -> shal_draw_filled_triangle  (f x1) (f y1)
                                         (f x2) (f y2)
                                         (f x3) (f y3)
                                         (f cRed) (f cGreen)
                                         (f cBlue) (f cAlpha)

drawRectangle :: Point  -- ^ Top-left
              -> Point  -- ^ Bottom-right
              -> Color -> DrawOptions -> IO ()
drawRectangle (x1,y1) (x2,y2) Color { .. } how =
  case how of
    Outlined thick -> shal_draw_rectangle (f x1) (f y1)
                                          (f x2) (f y2)
                                          (f cRed) (f cGreen)
                                          (f cBlue) (f cAlpha)
                                          (f thick)
    Filled -> shal_draw_filled_rectangle  (f x1) (f y1)
                                          (f x2) (f y2)
                                          (f cRed) (f cGreen)
                                          (f cBlue) (f cAlpha)

drawRoundedRectangle :: Point           -- ^ Top-left
                     -> Point           -- ^ Bottom-right
                     -> (Float,Float)   -- ^ Radii for rounding
                     -> Color
                     -> DrawOptions -> IO ()
drawRoundedRectangle (x1,y1) (x2,y2) (rx,ry) Color { .. } how =
  case how of
    Outlined thick -> shal_draw_rounded_rectangle
                        (f x1) (f y1) (f x2) (f y2)
                        (f rx) (f ry)
                        (f cRed) (f cGreen) (f cBlue) (f cAlpha)
                        (f thick)
    Filled -> shal_draw_filled_rounded_rectangle
                        (f x1) (f y1) (f x2) (f y2)
                        (f rx) (f ry)
                        (f cRed) (f cGreen) (f cBlue) (f cAlpha)




calculateSpline :: Point -> Point -> Point -> Point
                -> Float -- ^ Thickness, 0 for hairline.
                -> Int   -- ^ Number of segments
                -> IO (Array Int Point)
calculateSpline (x1,y1) (x2,y2) (x3,y3) (x4,y4) thick n =
  allocaArray (2 * out_elems) $ \out ->
  allocaArray 8 $ \inp ->
    do pokeElemOff inp 0 (f x1)
       pokeElemOff inp 1 (f y1)
       pokeElemOff inp 2 (f x2)
       pokeElemOff inp 3 (f y2)
       pokeElemOff inp 4 (f x3)
       pokeElemOff inp 5 (f y3)
       pokeElemOff inp 6 (f x4)
       pokeElemOff inp 7 (f y4)
       let stride = i (2 * sizeOf (undefined :: CFloat))
       al_calculate_spline out stride inp cThick (i n)
       arr <- newArray_ (0,out_elems-1)
       mkPoints 0 out_elems out arr
  where
  cThick     = f thick
  out_elems  = if cThick <= 0 then n else 2 * n

mkPoints :: Int -> Int -> Ptr CFloat -> IOArray Int Point ->
            IO (Array Int Point)
mkPoints pos num ptr arr
  | pos < num = do x <- peek ptr
                   y <- peekElemOff ptr 1
                   unsafeWriteIOArray arr pos (realToFrac x, realToFrac y)
                   mkPoints (pos + 1) num (advancePtr ptr 2) arr
mkPoints _ _ _ arr = unsafeFreeze arr


f :: Float -> CFloat
f = realToFrac

i :: Int -> CInt
i = fromIntegral
