{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Timer
  ( -- * Operations
    Timer
  , createTimer
  , startTimer
  , stopTimer
  , isTimerStarted
  , getTimerCount
  , setTimerCount
  , addTimerCount
  , getTimerSpeed
  , setTimerSpeed

  -- * Events
  , TimerEvent
  , evTimer
  , evCount

  -- * Exceptions
  , FailedToCreateTimer(..)
  ) where

import Allegro.Types

import Allegro.C.Types
import Allegro.C.Timer
import Allegro.C.Event

import qualified Control.Exception as X
import           Data.Int( Int64 )
import           Data.IORef ( readIORef )
import           Control.Applicative ( (<$>), (<*>) )
import           Control.Monad ( when )
import           Data.Typeable ( Typeable )
import           Foreign ( ForeignPtr, newForeignPtr, castForeignPtr
                         , withForeignPtr
                         , Ptr, nullPtr, castPtr
                         )
import           Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )


newtype Timer = Timer (ForeignPtr TIMER) deriving Eq

withTimer :: Timer -> (Ptr TIMER -> IO a) -> IO a
withTimer (Timer x) = withForeignPtr x

instance EventSource Timer where
  eventSource t           = withTimer t al_get_timer_event_source
  foreignClient (Timer t) = Just (castForeignPtr t)



createTimer :: Double -- ^ Timer speed
       -> IO Timer
createTimer sp =
  do ptr <- al_create_timer (realToFrac sp)
     when (ptr == nullPtr) $ X.throwIO FailedToCreateTimer
     Timer `fmap` newForeignPtr al_destroy_timer_addr ptr

data FailedToCreateTimer = FailedToCreateTimer deriving (Typeable,Show)

instance X.Exception FailedToCreateTimer



startTimer :: Timer -> IO ()
startTimer t = withTimer t al_start_timer

stopTimer :: Timer -> IO ()
stopTimer t = withTimer t al_stop_timer

isTimerStarted :: Timer -> IO Bool
isTimerStarted t = withTimer t al_get_timer_started

getTimerCount :: Timer -> IO Int64
getTimerCount t = withTimer t al_get_timer_count

setTimerCount :: Timer -> Int64 -> IO ()
setTimerCount t x = withTimer t $ \p -> al_set_timer_count p x

addTimerCount :: Timer -> Int64 -> IO ()
addTimerCount t x = withTimer t $ \p -> al_add_timer_count p x

getTimerSpeed :: Timer -> IO Double
getTimerSpeed t = fmap realToFrac (withTimer t al_get_timer_speed)

setTimerSpeed :: Timer -> Double -> IO ()
setTimerSpeed t x = withTimer t $ \p -> al_set_timer_speed p (realToFrac x)


--------------------------------------------------------------------------------
-- Events

data TimerEvent = EvTimer
  { tiSuper' :: {-# UNPACK #-} !SomeEvent
  , evTimer  :: !Timer    -- strict to avoid holding on to clients of
                          -- event queue.
  , evCount  :: !Int64
  }

instance ParseEvent TimerEvent where
  eventDetails q p = EvTimer <$> eventDetails q p
                             <*> (getTimer =<< event_timer_source p)
                             <*> event_timer_count p
    where
    getTimer t = findMe (castPtr t) <$> readIORef (eqReg q)

    findMe :: Ptr () -> [ForeignPtr ()] -> Timer
    findMe x (y:_) | x == unsafeForeignPtrToPtr y = Timer (castForeignPtr y)
    findMe x (_:ys) = findMe x ys
    findMe _ [] = error "Allegro bug: received event from unregistered timer."



instance HasType      TimerEvent where evType      = evType' . tiSuper'
instance HasTimestamp TimerEvent where evTimestamp = evTimestamp' . tiSuper'
