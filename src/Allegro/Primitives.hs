{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Allegro.Primitives
  ( -- * Drawing lines and curves
    DrawLine(..)
  , Color(..)
  , Point
  , Dim

  -- * Drawing shapes
  , DrawShape(..)
  , DrawOptions(..)

  -- * Shapes and lines
  , Line(..)
  , Arc(..)
  , Spline(..)
  , Triangle(..)
  , Rectangle(..)
  , rectangle
  , Circle(..)
  , circle

  -- * Calculation
  , Calculate(..)
  ) where

import Allegro.Types (Point, Dim, Color(..))
import Allegro.C.Primitives

import Foreign(allocaArray,pokeElemOff,peek,peekElemOff,pokeElemOff,
               sizeOf,advancePtr)
import Foreign.Ptr(Ptr)
import Foreign.C(CFloat,CInt)
import GHC.IOArray(unsafeWriteIOArray)
import Data.Array.Unsafe(unsafeFreeze)
import Data.Array(Array,bounds,rangeSize,assocs,listArray)
import Data.Array.MArray(newArray_)
import Data.Array.IO(IOArray)

import System.IO.Unsafe(unsafeDupablePerformIO)


data Line = Line Point Point

data Arc = Arc { arcCenter  :: Point
               , arcRadius  :: Float
               , arcRadiusY :: Maybe Float  -- ^ For elliptical arcs
               , arcTheta   :: Float        -- ^ Starting point, in radians;
                                            -- 0 points east.
               , arcDelta   :: Float        -- ^ Angular span, in radians
               }

data Spline = Spline Point Point Point Point

class DrawLine t where
  drawLine :: t
           -> Color
           -> Float   -- ^ Thikness, 0 for hairline.
           -> IO ()

-- | Straigh-line
instance DrawLine Line where
  drawLine (Line (x1,y1) (x2,y2)) Color { .. } thick =
    shal_draw_line (f x1) (f y1) (f x2) (f y2)
                   (f cRed) (f cGreen) (f cBlue) (f cAlpha)
                   (f thick)

-- | Multi-segment line
instance DrawLine [Point] where
  drawLine ps c t = drawLine (listArray (0,length ps - 1) ps) c t

-- | Multi-segment line
instance DrawLine (Array Int Point) where
  drawLine pts Color { .. } thick =
    withPoints pts $ \inp ->
      shal_draw_ribbon inp pointStride
        (f cRed) (f cGreen) (f cBlue) (f cAlpha) (f thick) (i seg)
    where
    seg = rangeSize (bounds pts)

instance DrawLine Arc where
  drawLine Arc { arcCenter = (x,y), .. } Color { .. } thick =
    case arcRadiusY of
      Nothing ->
        shal_draw_arc (f x) (f y)
                      (f arcRadius)
                      (f arcTheta) (f arcDelta)
                      (f cRed) (f cGreen) (f cBlue) (f cAlpha)
                      (f thick)
      Just ry ->
        shal_draw_elliptical_arc
          (f x) (f y)
          (f arcRadius) (f ry)
          (f arcTheta) (f arcDelta)
          (f cRed) (f cGreen) (f cBlue) (f cAlpha)
          (f thick)

instance DrawLine Spline where
  drawLine (Spline (x1,y1) (x2,y2) (x3,y3) (x4,y4)) Color { .. } thick =
      allocaArray 8 $ \inp ->
        do pokeElemOff inp 0 (f x1)
           pokeElemOff inp 1 (f y1)
           pokeElemOff inp 2 (f x2)
           pokeElemOff inp 3 (f y2)
           pokeElemOff inp 4 (f x3)
           pokeElemOff inp 5 (f y3)
           pokeElemOff inp 6 (f x4)
           pokeElemOff inp 7 (f y4)
           shal_draw_spline inp (f cRed) (f cGreen) (f cBlue) (f cAlpha)
                                                              (f thick)




data DrawOptions = Outlined Float -- ^ Draw an outline. 0 for hairline outline.
                 | Filled         -- ^ Fill the shape

class DrawShape t where
  drawShape :: t -> Color -> DrawOptions -> IO ()

data Triangle   = Triangle Point Point Point
data Rectangle  = Rectangle { rectTopLeft     :: Point
                            , rectBottomRight :: Point
                            , rectCurved      :: Maybe (Float, Float)
                              -- ^ Radii for curved corrners.
                            }

rectangle :: Point -> Dim -> Rectangle
rectangle (x,y) (w,h) = Rectangle { rectTopLeft = (x,y)
                                  , rectBottomRight = (x + w, y + h)
                                  , rectCurved = Nothing
                                  }

data Circle       = Circle { circCenter   :: Point
                           , circRadius   :: Float
                           , circRadiusY  :: Maybe Float -- ^ For ellipses
                           }

circle :: Point -> Float -> Circle
circle circCenter circRadius = Circle { circRadiusY = Nothing, .. }

instance DrawShape Triangle where
  drawShape (Triangle (x1,y1) (x2,y2) (x3,y3)) Color { .. } how =
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

instance DrawShape Rectangle where
  drawShape Rectangle { rectTopLeft     = (x1,y1)
                      , rectBottomRight = (x2,y2)
                      , .. }
                      Color { .. } how =

    case (rectCurved, how) of

      (Nothing, Outlined thick) ->
        shal_draw_rectangle (f x1) (f y1) (f x2) (f y2)
                            (f cRed) (f cGreen) (f cBlue) (f cAlpha) (f thick)

      (Nothing, Filled) ->
        shal_draw_filled_rectangle  (f x1) (f y1) (f x2) (f y2)
                                    (f cRed) (f cGreen) (f cBlue) (f cAlpha)

      (Just (rx,ry), Outlined thick) ->
        shal_draw_rounded_rectangle
                          (f x1) (f y1) (f x2) (f y2)
                          (f rx) (f ry)
                          (f cRed) (f cGreen) (f cBlue) (f cAlpha)
                          (f thick)

      (Just (rx,ry), Filled) ->
        shal_draw_filled_rounded_rectangle
                          (f x1) (f y1) (f x2) (f y2)
                          (f rx) (f ry)
                          (f cRed) (f cGreen) (f cBlue) (f cAlpha)


-- | Pie-slice; second radius is ignored.
instance DrawShape Arc where
  drawShape Arc { arcCenter = (x,y), .. } Color { .. } how =
    case how of

      Outlined thick ->
        shal_draw_pieslice
          (f x) (f y)
          (f arcRadius)
          (f arcTheta) (f arcDelta)
          (f cRed) (f cGreen) (f cBlue) (f cAlpha)
          (f thick)

      Filled ->
        shal_draw_filled_pieslice
          (f x) (f y)
          (f arcRadius)
          (f arcTheta) (f arcDelta)
          (f cRed) (f cGreen) (f cBlue) (f cAlpha)

instance DrawShape Circle where
  drawShape Circle { circCenter = (x,y), .. } Color { .. } how =
    case (circRadiusY, how) of

      (Nothing, Outlined thick) ->
        shal_draw_circle
          (f x) (f y)
          (f circRadius)
          (f cRed) (f cGreen) (f cBlue) (f cAlpha)
          (f thick)

      (Nothing, Filled) ->
        shal_draw_filled_circle
          (f x) (f y)
          (f circRadius)
          (f cRed) (f cGreen) (f cBlue) (f cAlpha)

      (Just ry, Outlined thick) ->
        shal_draw_ellipse
          (f x) (f y)
          (f circRadius) (f ry)
          (f cRed) (f cGreen) (f cBlue) (f cAlpha)
          (f thick)

      (Just ry, Filled) ->
        shal_draw_filled_ellipse
          (f x)  (f y)
          (f circRadius) (f ry)
          (f cRed) (f cGreen) (f cBlue) (f cAlpha)




--------------------------------------------------------------------------------

class Calculate t where
  calculate :: t -> Int -> Array Int Point


calculation :: Int -> (Ptr CFloat -> IO ()) -> Array Int Point
calculation n calc =
  unsafeDupablePerformIO $
  allocaArray (2 * n) $ \out ->
  do calc out
     mkPoints 0 n out =<< newArray_ (0,n-1)



instance Calculate Spline where
  calculate (Spline (x1,y1) (x2,y2) (x3,y3) (x4,y4)) n =
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

instance Calculate Arc where
  calculate Arc { arcCenter = (x,y), .. } segments =
    calculation segments $ \outPtr ->
      al_calculate_arc outPtr pointStride
          (f x) (f y)
          (f arcRadius) (case arcRadiusY of
                           Nothing -> f arcRadius
                           Just ry -> f ry)
          (f arcTheta) (f arcDelta)
          0 (i segments)

circleToArk :: Circle -> Arc
circleToArk Circle { .. } =
  Arc { arcCenter = circCenter
      , arcRadius = circRadius
      , arcRadiusY = circRadiusY
      , arcTheta  = 0
      , arcDelta  = 2 * pi
      }

instance Calculate Circle where
  calculate c n = calculate (circleToArk c) n




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
