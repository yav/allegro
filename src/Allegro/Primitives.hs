{-# LANGUAGE RecordWildCards #-}
module Allegro.Primitives
  ( -- * Drawing
    drawLine
  , DrawOptions(..)
  , drawTriangle
  , drawRectangle
  , drawRoundedRectangle
  , drawRibbon

  -- * Calculation
  , calculateSpline
  , calculateArc
  ) where

import Allegro.Types (Point, Color(..))
import Allegro.C.Primitives

import Foreign(allocaArray,pokeElemOff,peek,peekElemOff,pokeElemOff,
               sizeOf,advancePtr)
import Foreign.Ptr(Ptr)
import Foreign.C(CFloat,CInt)
import GHC.IOArray(unsafeWriteIOArray)
import Data.Array.Unsafe(unsafeFreeze)
import Data.Array(Array,bounds,rangeSize,assocs)
import Data.Array.MArray(newArray_)
import Data.Array.IO(IOArray)
import System.IO.Unsafe(unsafeDupablePerformIO)

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

drawRibbon :: Array Int Point
           -> Color
           -> Float   -- ^ Thickness
           -> IO ()
drawRibbon pts Color { .. } thick =
  withPoints pts $ \inp ->
    shal_draw_ribbon inp pointStride
      (f cRed) (f cGreen) (f cBlue) (f cAlpha) (f thick) (i seg)

  where
  seg = rangeSize (bounds pts)



--------------------------------------------------------------------------------

calculation :: Int -> (Ptr CFloat -> IO ()) -> Array Int Point
calculation n calc =
  unsafeDupablePerformIO $
  allocaArray (2 * n) $ \out ->
  do calc out
     mkPoints 0 n out =<< newArray_ (0,n-1)



calculateSpline :: Point -> Point -> Point -> Point
                -> Int   -- ^ Number of segments
                -> Array Int Point
calculateSpline (x1,y1) (x2,y2) (x3,y3) (x4,y4) n =
  calculation n $ \outPtr ->
  allocaArray 8 $ \inp ->
    do pokeElemOff inp 0 (f x1)
       pokeElemOff inp 1 (f y1)
       pokeElemOff inp 2 (f x2)
       pokeElemOff inp 3 (f y2)
       pokeElemOff inp 4 (f x3)
       pokeElemOff inp 5 (f y3)
       pokeElemOff inp 6 (f x4)
       pokeElemOff inp 7 (f y4)
       al_calculate_spline outPtr pointStride inp 0 (i n)

calculateArc :: Point
             -> (Float,Float) -- ^ Arc radii
             -> Float         -- ^ Starting angle, in radians
             -> Float         -- ^ Angular span
             -> Int           -- ^ Number of segments
             -> Array Int Point
calculateArc (x,y) (rx,ry) theta delta segments =
  calculation segments $ \outPtr ->
    al_calculate_arc outPtr pointStride
        (f x) (f y)
        (f rx) (f ry)
        (f theta) (f delta)
        0 (i segments)

withPoints :: Array Int Point -> (Ptr CFloat -> IO ()) -> IO ()
withPoints pts k =
  allocaArray (2 * rangeSize bs) $ \buf ->
      do mapM_ (upd buf) (assocs pts)
         k buf
  where
  bs@(start,_) = bounds pts
  upd ptr (pos,(x,y)) = do let p = 2 * (pos - start)
                           pokeElemOff ptr p       (f x)
                           pokeElemOff ptr (p + 1) (f y)

mkPoints :: Int -> Int -> Ptr CFloat -> IOArray Int Point ->
            IO (Array Int Point)
mkPoints pos num ptr arr
  | pos < num = do x <- peek ptr
                   y <- peekElemOff ptr 1
                   unsafeWriteIOArray arr pos (realToFrac x, realToFrac y)
                   mkPoints (pos + 1) num (advancePtr ptr 2) arr
mkPoints _ _ _ arr = unsafeFreeze arr

pointStride :: CInt
pointStride = i (2 * sizeOf (undefined :: CFloat))

--------------------------------------------------------------------------------

f :: Float -> CFloat
f = realToFrac

i :: Int -> CInt
i = fromIntegral
