{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Timer
  ( Timer
  , create
  , start
  , stop
  , isStarted
  , getCount
  , setCount
  , addCount
  , getSpeed
  , setSpeed

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

create :: Double -- ^ Timer speed
       -> IO Timer
create sp =
  do ptr <- al_create_timer (realToFrac sp)
     when (ptr == nullPtr) $ X.throwIO FailedToCreateTimer
     Timer `fmap` newForeignPtr al_destroy_timer_addr ptr

data FailedToCreateTimer = FailedToCreateTimer deriving (Typeable,Show)

instance X.Exception FailedToCreateTimer



start :: Timer -> IO ()
start t = withPtr t al_start_timer

stop :: Timer -> IO ()
stop t = withPtr t al_stop_timer

isStarted :: Timer -> IO Bool
isStarted t = withPtr t al_get_timer_started

getCount :: Timer -> IO Int64
getCount t = withPtr t al_get_timer_count

setCount :: Timer -> Int64 -> IO ()
setCount t x = withPtr t $ \p -> al_set_timer_count p x

addCount :: Timer -> Int64 -> IO ()
addCount t x = withPtr t $ \p -> al_add_timer_count p x

getSpeed :: Timer -> IO Double
getSpeed t = fmap realToFrac (withPtr t al_get_timer_speed)

setSpeed :: Timer -> Double -> IO ()
setSpeed t x = withPtr t $ \p -> al_set_timer_speed p (realToFrac x)



