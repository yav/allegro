{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Timer
  ( Timer
  , createTimer
  , startTimer
  , stopTimer
  , timerStarted
  , timerCount
  , setTimerCount
  , addTimerCount
  , timerSpeed
  , setTimerSpeed

  -- * Exceptions
  , FailedToCreateTimer(..)
  ) where

import Allegro.Types
import Allegro.C.Timer

import qualified Control.Exception as X
import           Data.Int( Int64 )
import           Control.Monad ( when )
import           Data.Typeable ( Typeable )
import           Foreign ( newForeignPtr, nullPtr )

createTimer :: Double -> IO Timer
createTimer sp =
  do ptr <- al_create_timer (realToFrac sp)
     when (ptr == nullPtr) $ X.throwIO FailedToCreateTimer
     Timer `fmap` newForeignPtr al_destroy_timer_addr ptr

data FailedToCreateTimer = FailedToCreateTimer deriving (Typeable,Show)

instance X.Exception FailedToCreateTimer



startTimer :: Timer -> IO ()
startTimer t = withPtr t al_start_timer

stopTimer :: Timer -> IO ()
stopTimer t = withPtr t al_stop_timer

timerStarted :: Timer -> IO Bool
timerStarted t = withPtr t al_get_timer_started

timerCount :: Timer -> IO Int64
timerCount t = withPtr t al_get_timer_count

setTimerCount :: Timer -> Int64 -> IO ()
setTimerCount t x = withPtr t $ \p -> al_set_timer_count p x

addTimerCount :: Timer -> Int64 -> IO ()
addTimerCount t x = withPtr t $ \p -> al_add_timer_count p x

timerSpeed :: Timer -> IO Double
timerSpeed t = fmap realToFrac (withPtr t al_get_timer_speed)

setTimerSpeed :: Timer -> Double -> IO ()
setTimerSpeed t x = withPtr t $ \p -> al_set_timer_speed p (realToFrac x)



