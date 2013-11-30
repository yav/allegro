{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Timer
  ( -- * Operations
    Timer
  , create
  , start
  , stop
  , isStarted
  , getCount
  , setCount
  , addCount
  , getSpeed
  , setSpeed

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



create :: Double -- ^ Timer speed
       -> IO Timer
create sp =
  do ptr <- al_create_timer (realToFrac sp)
     when (ptr == nullPtr) $ X.throwIO FailedToCreateTimer
     Timer `fmap` newForeignPtr al_destroy_timer_addr ptr

data FailedToCreateTimer = FailedToCreateTimer deriving (Typeable,Show)

instance X.Exception FailedToCreateTimer



start :: Timer -> IO ()
start t = withTimer t al_start_timer

stop :: Timer -> IO ()
stop t = withTimer t al_stop_timer

isStarted :: Timer -> IO Bool
isStarted t = withTimer t al_get_timer_started

getCount :: Timer -> IO Int64
getCount t = withTimer t al_get_timer_count

setCount :: Timer -> Int64 -> IO ()
setCount t x = withTimer t $ \p -> al_set_timer_count p x

addCount :: Timer -> Int64 -> IO ()
addCount t x = withTimer t $ \p -> al_add_timer_count p x

getSpeed :: Timer -> IO Double
getSpeed t = fmap realToFrac (withTimer t al_get_timer_speed)

setSpeed :: Timer -> Double -> IO ()
setSpeed t x = withTimer t $ \p -> al_set_timer_speed p (realToFrac x)


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
