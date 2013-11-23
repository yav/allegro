{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.EventQueue where

import Foreign
import Foreign.C.Types

import Allegro.C.Types

#include <allegro5/allegro.h>


foreign import ccall unsafe "al_create_event_queue"
  al_create_event_queue :: IO (Ptr EVENT_QUEUE)

foreign import ccall unsafe "al_destroy_event_queue"
  al_destroy_event_queue :: Ptr EVENT_QUEUE -> IO ()

foreign import ccall unsafe "&al_destroy_event_queue"
  al_destroy_event_queue_addr :: FunPtr (Ptr EVENT_QUEUE -> IO ())

foreign import ccall unsafe "al_register_event_source"
  al_register_event_source :: Ptr EVENT_QUEUE -> Ptr EVENT_SOURCE -> IO ()

foreign import ccall unsafe "al_unregister_event_source"
  al_unregister_event_source :: Ptr EVENT_QUEUE -> Ptr EVENT_SOURCE -> IO ()

foreign import ccall unsafe "al_is_event_queue_empty"
  al_is_event_queue_empty :: Ptr EVENT_QUEUE -> Bool

foreign import ccall unsafe "al_get_next_event"
  al_get_next_event :: Ptr EVENT_QUEUE -> Ptr EVENT -> IO Bool

foreign import ccall unsafe "al_peek_next_event"
  al_peek_next_event :: Ptr EVENT_QUEUE -> Ptr EVENT -> IO Bool

foreign import ccall unsafe "al_drop_next_event"
  al_drop_next_event :: Ptr EVENT_QUEUE -> IO Bool

foreign import ccall unsafe "al_flush_event_queue"
  al_flush_event_queue :: Ptr EVENT_QUEUE -> IO ()

foreign import ccall unsafe "al_wait_for_event"
  al_wait_for_event :: Ptr EVENT_QUEUE -> Ptr EVENT -> IO ()

foreign import ccall unsafe "al_wait_for_event_timed"
  al_wait_for_event_timed :: Ptr EVENT_QUEUE -> Ptr EVENT -> CFloat -> IO Bool



