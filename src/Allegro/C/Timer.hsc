{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Timer where

import Foreign
import Foreign.C.Types
import Allegro.C.Types

#include <allegro5/allegro.h>


foreign import ccall "al_create_timer"
  al_create_timer :: CDouble -> IO (Ptr TIMER)

foreign import ccall "al_destroy_timer"
  al_destroy_timer :: Ptr TIMER -> IO ()

foreign import ccall "&al_destroy_timer"
  al_destroy_timer_addr :: FunPtr (Ptr TIMER -> IO ())

foreign import ccall "al_start_timer"
  al_start_timer :: Ptr TIMER -> IO ()

foreign import ccall "al_stop_timer"
  al_stop_timer :: Ptr TIMER -> IO ()

foreign import ccall "al_get_timer_started"
  al_get_timer_started :: Ptr TIMER -> IO Bool

foreign import ccall "al_get_timer_count"
  al_get_timer_count :: Ptr TIMER -> IO Int64

foreign import ccall "al_set_timer_count"
  al_set_timer_count :: Ptr TIMER -> Int64 -> IO ()

foreign import ccall "al_add_timer_count"
  al_add_timer_count :: Ptr TIMER -> Int64 -> IO ()

foreign import ccall "al_get_timer_speed"
  al_get_timer_speed :: Ptr TIMER -> IO CDouble

foreign import ccall "al_set_timer_speed"
  al_set_timer_speed :: Ptr TIMER -> CDouble -> IO ()

foreign import ccall "al_get_timer_event_source"
  al_get_timer_event_source :: Ptr TIMER -> IO (Ptr EVENT_SOURCE)


