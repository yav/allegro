{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Event
  (
    -- * Event Queues
    EventQueue
  , createEventQueue
  , registerEventSource
  , waitForEvent
  , unregisterEventSource
  , EventSource

    -- * Events
  , Event(..)
  , SomeEvent
  , HasTimestamp(..)
  , HasType(..)

  -- * Exceptions
  , FailedToCreateEventQueue(..)
  ) where

import Allegro.Types
import Allegro.Timer(TimerEvent)
import Allegro.Keyboard(KeyEvent,KeyCharEvent)
import Allegro.Mouse(MouseEvent,MouseMoveEvent,MouseButtonEvent)
import Allegro.Display(DisplayEvent)

import Allegro.C.Types
import Allegro.C.Event
import Allegro.C.EventQueue

import qualified Control.Exception as X
import           Data.IORef
import           Control.Monad ( when )
import           Data.Typeable ( Typeable )
import           Foreign  ( Ptr, nullPtr, allocaBytes
                          , ForeignPtr, newForeignPtr, withForeignPtr
                          )


-- The event queue keeps track of twho's registered with it, so
-- that they don;t get destroyed prematurely.
data EventQueue     = EventQueue { eqPtr :: ForeignPtr EVENT_QUEUE
                                 , eqReg :: IORef [ ForeignPtr () ]
                                 }
                      deriving Eq

withQ :: EventQueue -> (Ptr EVENT_QUEUE -> IO a) -> IO a
withQ = withForeignPtr . eqPtr

data FailedToCreateEventQueue = FailedToCreateEventQueue
  deriving (Typeable,Show)

instance X.Exception FailedToCreateEventQueue

createEventQueue :: IO EventQueue
createEventQueue =
  do p <- al_create_event_queue
     when (p == nullPtr) $ X.throwIO FailedToCreateEventQueue
     fp <- newForeignPtr al_destroy_event_queue_addr p
     clients <- newIORef []
     return EventQueue { eqPtr = fp, eqReg = clients }

registerEventSource :: EventSource t => EventQueue -> t -> IO ()
registerEventSource q x =
  do s <- eventSource x
     case foreignClient x of
       Just fp -> atomicModifyIORef (eqReg q) $ \as -> (fp : as, ())
       Nothing -> return ()
     withQ q $ \qp -> al_register_event_source qp s

unregisterEventSource :: EventSource t => EventQueue -> t -> IO ()
unregisterEventSource q x =
  do s <- eventSource x
     withQ q $ \qp -> al_unregister_event_source qp s
     case foreignClient x of
       Just fp -> atomicModifyIORef (eqReg q) $ \as -> let as' = rm fp as id
                                                       in seq as' (as',())
       Nothing -> return ()
  where
  rm _ [] k                 = k []
  rm v (a : as) k | v == a  = k as
  rm v (a : as) k           = rm v as $ \bs -> k (a : bs)


waitForEvent :: EventQueue -> IO Event
waitForEvent q =
  allocaBytes event_size_bytes $ \evPtr ->
    do withQ q $ \qp -> al_wait_for_event qp evPtr
       parseEvent evPtr


parseEvent :: Ptr EVENT -> IO Event
parseEvent p =
  do t <- event_type p
     case () of

          -- Display
       _ | t == event_display_close ->
             DisplayClose `fmap` eventDetails p

         | t == event_display_switch_in ->
             DisplaySwitchIn `fmap` eventDetails p

         | t == event_display_switch_out ->
             DisplaySwitchOut `fmap` eventDetails p

         -- Keyboard
         | t == event_key_down ->
             KeyDown `fmap` eventDetails p

         | t == event_key_up ->
             KeyUp `fmap` eventDetails p

         | t == event_key_char ->
             KeyChar `fmap` eventDetails p

         -- Mouse
         | t == event_mouse_enter_display ->
             MouseEnter `fmap` eventDetails p

         | t == event_mouse_leave_display ->
             MouseLeave `fmap` eventDetails p

         | t == event_mouse_axes ->
             MouseMove `fmap` eventDetails p

         | t == event_mouse_warped ->
             MouseWarp `fmap` eventDetails p

         | t == event_mouse_button_down ->
             MouseButtonDown `fmap` eventDetails p

         | t == event_mouse_button_up ->
             MouseButtonUp `fmap` eventDetails p

         | t == event_timer ->
             Time `fmap` eventDetails p

         -- Fallback
         | otherwise ->
             Unknown `fmap` eventDetails p

data Event  = DisplayClose     {-# UNPACK #-} !DisplayEvent
            | DisplaySwitchIn  {-# UNPACK #-} !DisplayEvent
            | DisplaySwitchOut {-# UNPACK #-} !DisplayEvent

            | KeyUp            {-# UNPACK #-} !KeyEvent
            | KeyDown          {-# UNPACK #-} !KeyEvent
            | KeyChar          {-# UNPACK #-} !KeyCharEvent

            | MouseEnter       {-# UNPACK #-} !MouseEvent
            | MouseLeave       {-# UNPACK #-} !MouseEvent
            | MouseMove        {-# UNPACK #-} !MouseMoveEvent
            | MouseWarp        {-# UNPACK #-} !MouseMoveEvent
            | MouseButtonDown  {-# UNPACK #-} !MouseButtonEvent
            | MouseButtonUp    {-# UNPACK #-} !MouseButtonEvent

            | Time             {-# UNPACK #-} !TimerEvent

            | Unknown          {-# UNPACK #-} !SomeEvent






