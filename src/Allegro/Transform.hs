module Allegro.Transform
  ( withTransformSRT
  ) where

import Allegro.Types (Point)
import Allegro.C.Transform

import Foreign.C.Types(CFloat)
import Foreign.Marshal(allocaBytes)

-- | Scale, then rotate, then translate a drawing.
withTransformSRT :: (Float,Float) -- ^ Horizontal and vertical scale
                 -> Float         -- ^ Rotate, in radians
                 -> Point         -- ^ Translate to this point
                 -> IO a          -- ^ Scope of transformation
                 -> IO a
withTransformSRT (sx, sy) theta (x,y) m =
  allocaBytes transform_size_bytes $ \p ->
    do al_build_transform p (f x) (f y) (f sx) (f sy) (f theta)
       al_use_transform p
       a <- m
       al_identity_transform p
       al_use_transform p
       return a

f :: Float -> CFloat
f = realToFrac

