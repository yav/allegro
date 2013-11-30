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
import           Foreign  ( Ptr, nullPtr, allocaBytes, newForeignPtr )


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
       parseEvent q evPtr


parseEvent :: EventQueue -> Ptr EVENT -> IO Event
parseEvent q p =
  do t <- event_type p
     case () of

          -- Display
       _ | t == event_display_close ->
             DisplayClose `fmap` eventDetails q p

         | t == event_display_switch_in ->
             DisplaySwitchIn `fmap` eventDetails q p

         | t == event_display_switch_out ->
             DisplaySwitchOut `fmap` eventDetails q p

         -- Keyboard
         | t == event_key_down ->
             KeyDown `fmap` eventDetails q p

         | t == event_key_up ->
             KeyUp `fmap` eventDetails q p

         | t == event_key_char ->
             KeyChar `fmap` eventDetails q p

         -- Mouse
         | t == event_mouse_enter_display ->
             MouseEnter `fmap` eventDetails q p

         | t == event_mouse_leave_display ->
             MouseLeave `fmap` eventDetails q p

         | t == event_mouse_axes ->
             MouseMove `fmap` eventDetails q p

         | t == event_mouse_warped ->
             MouseWarp `fmap` eventDetails q p

         | t == event_mouse_button_down ->
             MouseButtonDown `fmap` eventDetails q p

         | t == event_mouse_button_up ->
             MouseButtonUp `fmap` eventDetails q p

         | t == event_timer ->
             Time `fmap` eventDetails q p

         -- Fallback
         | otherwise ->
             Unknown `fmap` eventDetails q p

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


instance HasType Event where
  evType ev =
    case ev of
      DisplayClose     x -> evType x
      DisplaySwitchIn  x -> evType x
      DisplaySwitchOut x -> evType x

      KeyUp            x -> evType x
      KeyDown          x -> evType x
      KeyChar          x -> evType x

      MouseEnter       x -> evType x
      MouseLeave       x -> evType x
      MouseMove        x -> evType x
      MouseWarp        x -> evType x
      MouseButtonDown  x -> evType x
      MouseButtonUp    x -> evType x

      Time             x -> evType x

      Unknown          x -> evType x


instance HasTimestamp Event where
  evTimestamp ev =
    case ev of
      DisplayClose     x -> evTimestamp x
      DisplaySwitchIn  x -> evTimestamp x
      DisplaySwitchOut x -> evTimestamp x

      KeyUp            x -> evTimestamp x
      KeyDown          x -> evTimestamp x
      KeyChar          x -> evTimestamp x

      MouseEnter       x -> evTimestamp x
      MouseLeave       x -> evTimestamp x
      MouseMove        x -> evTimestamp x
      MouseWarp        x -> evTimestamp x
      MouseButtonDown  x -> evTimestamp x
      MouseButtonUp    x -> evTimestamp x

      Time             x -> evTimestamp x

      Unknown          x -> evTimestamp x



