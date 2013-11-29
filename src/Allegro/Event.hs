{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
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

    -- ** Common fields
  , HasTimestamp(..)
  , HasType(..)
  , SomeEvent

    -- ** Display Events
  , HasDisplay(..)
  , DisplayEvent

    -- ** Keyboard Events
  , HasKey(..)
  , KeyEvent

  , KeyCharEvent
  , evKeyChar
  , evKeyMod
  , evKeyRepeated

    -- ** Mouse Events
  , HasMouseAxis(..)
  , MouseEvent

  , MouseMoveEvent
  , evMouseDX, evMouseDY, evMouseDZ, evMouseDW

  , MouseButtonEvent
  , evMouseButton

  -- ** Timer Events
  , TimerEvent
  , evTimer
  , evCount

  -- * Exceptions
  , FailedToCreateEventQueue(..)
  ) where

import Allegro.Types
import Allegro.C.Types
import Allegro.C.Event
import Allegro.C.EventQueue
import Allegro.C.Timer
import Allegro.C.Display(al_get_display_event_source)
import Allegro.C.Keyboard
import Allegro.C.Mouse

import qualified Control.Exception as X
import           Data.IORef
import           Data.Int(Int64)
import           Control.Monad ( when, guard )
import           Control.Applicative ( (<$>), (<*>) )
import           Data.Typeable ( Typeable )
import           Foreign  ( Ptr, nullPtr, allocaBytes
                          , ForeignPtr, newForeignPtr
                          , castForeignPtr
                          )


-- The event queue keeps track of twho's registered with it, so
-- that they don;t get destroyed prematurely.
data EventQueue     = EventQueue { eqPtr :: ForeignPtr EVENT_QUEUE
                                 , eqReg :: IORef [ ForeignPtr () ]
                                 }
                      deriving Eq

instance Object EventQueue where
  type CType EventQueue = EVENT_QUEUE
  foreignPtr = eqPtr

class EventSource t where
  eventSource   :: t -> IO (Ptr EVENT_SOURCE)
  foreignClient :: t -> Maybe (ForeignPtr ())

instance EventSource Display where
  eventSource (Display p) = al_get_display_event_source p
  foreignClient _ = Nothing -- XXX?

instance EventSource Keyboard where
  eventSource _ = al_get_keyboard_event_source
  foreignClient = Just . foreignPtr

instance EventSource Mouse where
  eventSource _ = al_get_mouse_event_source
  foreignClient = Just . foreignPtr

instance EventSource Timer where
  eventSource t = withPtr t al_get_timer_event_source
  foreignClient (Timer t) = Just (castForeignPtr t)

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
     withPtr q $ \qp -> al_register_event_source qp s

unregisterEventSource :: EventSource t => EventQueue -> t -> IO ()
unregisterEventSource q x =
  do s <- eventSource x
     withPtr q $ \qp -> al_unregister_event_source qp s
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
    do withPtr q $ \qp -> al_wait_for_event qp evPtr
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

toAny :: Event -> SomeEvent
toAny ev =
  case ev of
    DisplayClose     x -> dSuper' x
    DisplaySwitchIn  x -> dSuper' x
    DisplaySwitchOut x -> dSuper' x

    KeyUp            x -> kSuper' x
    KeyDown          x -> kSuper' x
    KeyChar          x -> kSuper' $ kcSuper' x

    MouseEnter       x -> meSuper' x
    MouseLeave       x -> meSuper' x
    MouseMove        x -> meSuper' $ mmeSuper' x
    MouseWarp        x -> meSuper' $ mmeSuper' x
    MouseButtonDown  x -> meSuper' $ mbeSuper' x
    MouseButtonUp    x -> meSuper' $ mbeSuper' x

    Time             x -> tiSuper' x

    Unknown          x -> x

instance HasType      Event where evType      = evType      . toAny
instance HasTimestamp Event where evTimestamp = evTimestamp . toAny


class ParseEvent t where
  eventDetails :: Ptr EVENT -> IO t

displayFrom :: (Ptr EVENT -> IO (Ptr DISPLAY)) -> Ptr EVENT -> IO Display
displayFrom f p = Display `fmap` f p

--------------------------------------------------------------------------------
-- Event fields

class HasType t where
  evType      :: t -> Int

class HasTimestamp t where
  evTimestamp :: t -> Double

class HasDisplay t where
  evDisplay   :: t -> Display

class HasKey t where
  evKey       :: t -> Key

class HasMouseAxis t where
  evMouseX, evMouseY, evMouseZ, evMouseW :: t -> Int



--------------------------------------------------------------------------------
-- Catch all


data SomeEvent = EvAny { evType'      :: Int
                      , evTimestamp' :: Double
                      }

instance ParseEvent SomeEvent where
  eventDetails p = EvAny <$> (fromIntegral <$> event_type p)
                         <*> (realToFrac   <$> event_any_timestamp p)

instance HasType      SomeEvent where evType      = evType'
instance HasTimestamp SomeEvent where evTimestamp = evTimestamp'

--------------------------------------------------------------------------------
-- Timer events

data TimerEvent = EvTimer
  { tiSuper' :: {-# UNPACK #-} !SomeEvent
  , evTimer  :: Name Timer
  , evCount  :: Int64
  }

instance ParseEvent TimerEvent where
  eventDetails p = EvTimer <$> eventDetails p
                           <*> (Name <$> event_timer_source p)
                           <*> event_timer_count p


--------------------------------------------------------------------------------
-- Display events

data DisplayEvent = EvDisplay
  { dSuper'   :: {-# UNPACK #-} !SomeEvent
  , dDisplay' :: Display
  }

instance ParseEvent DisplayEvent where
  eventDetails p = EvDisplay <$> eventDetails p
                             <*> displayFrom event_display_source p

instance HasType      DisplayEvent where evType      = evType      . dSuper'
instance HasTimestamp DisplayEvent where evTimestamp = evTimestamp . dSuper'
instance HasDisplay   DisplayEvent where evDisplay   = dDisplay'


---------------------------------------------------------------------------------- Keyboard events

data KeyEvent = EvKey
  { kSuper'   :: {-# UNPACK #-} !SomeEvent
  , kDisplay' :: Display
  , kKey'     :: Key
  }

instance ParseEvent KeyEvent where
  eventDetails p = EvKey <$> eventDetails p
                         <*> displayFrom event_keyboard_display p
                         <*> (Key <$> event_keyboard_keycode p)

instance HasType      KeyEvent where evType      = evType      . kSuper'
instance HasTimestamp KeyEvent where evTimestamp = evTimestamp . kSuper'
instance HasDisplay   KeyEvent where evDisplay   = kDisplay'
instance HasKey       KeyEvent where evKey       = kKey'



data KeyCharEvent = EvKeyChar
  { kcSuper'      :: {-# UNPACK #-} !KeyEvent
  , evKeyChar     :: Maybe Char
  , evKeyMod      :: KeyMod
  , evKeyRepeated :: Bool
  }

instance ParseEvent KeyCharEvent where
  eventDetails p = EvKeyChar <$> eventDetails p
                             <*> parseChar
                             <*> (KM <$> event_keyboard_modifiers p)
                             <*> event_keyboard_repeat p

    where parseChar = do x <- event_keyboard_unichar p
                         return $ do guard (x > 0)
                                     return $ toEnum $ fromEnum x

instance HasType      KeyCharEvent where evType      = evType      . kcSuper'
instance HasTimestamp KeyCharEvent where evTimestamp = evTimestamp . kcSuper'
instance HasDisplay   KeyCharEvent where evDisplay   = evDisplay   . kcSuper'
instance HasKey       KeyCharEvent where evKey       = evKey       . kcSuper'





--------------------------------------------------------------------------------
-- Mouse events

data MouseEvent   = ME
  { meSuper'    :: {-# UNPACK #-} !SomeEvent
  , meDisplay'  :: Display
  , meX', meY', meZ', meW' :: Int
  }

instance ParseEvent MouseEvent where
  eventDetails p = ME <$> eventDetails p
                      <*> displayFrom event_mouse_display p
                      <*> (fromIntegral <$> event_mouse_x p)
                      <*> (fromIntegral <$> event_mouse_y p)
                      <*> (fromIntegral <$> event_mouse_z p)
                      <*> (fromIntegral <$> event_mouse_w p)

instance HasType      MouseEvent where evType      = evType      . meSuper'
instance HasTimestamp MouseEvent where evTimestamp = evTimestamp . meSuper'
instance HasDisplay   MouseEvent where evDisplay   = meDisplay'

instance HasMouseAxis MouseEvent where
  evMouseX    = meX'
  evMouseY    = meY'
  evMouseZ    = meZ'
  evMouseW    = meW'


data MouseMoveEvent    = MME
  { mmeSuper' :: {-# UNPACK #-} !MouseEvent
  , evMouseDX, evMouseDY, evMouseDZ, evMouseDW :: Int
  }

instance ParseEvent MouseMoveEvent where
  eventDetails p = MME <$> eventDetails p
                       <*> (fromIntegral <$> event_mouse_dx p)
                       <*> (fromIntegral <$> event_mouse_dy p)
                       <*> (fromIntegral <$> event_mouse_dz p)
                       <*> (fromIntegral <$> event_mouse_dw p)



instance HasType      MouseMoveEvent where evType      = evType      . mmeSuper'
instance HasTimestamp MouseMoveEvent where evTimestamp = evTimestamp . mmeSuper'
instance HasDisplay   MouseMoveEvent where evDisplay   = evDisplay   . mmeSuper'

instance HasMouseAxis MouseMoveEvent where
  evMouseX    = evMouseX . mmeSuper'
  evMouseY    = evMouseY . mmeSuper'
  evMouseZ    = evMouseZ . mmeSuper'
  evMouseW    = evMouseW . mmeSuper'





data MouseButtonEvent  = MBE
  { mbeSuper'     :: {-# UNPACK #-} !MouseEvent
  , evMouseButton :: Int
  }

instance ParseEvent MouseButtonEvent where
  eventDetails p = MBE <$> eventDetails p
                       <*> (fromIntegral <$> event_mouse_button p)

instance HasType      MouseButtonEvent where evType      = evType     .mbeSuper'
instance HasTimestamp MouseButtonEvent where evTimestamp = evTimestamp.mbeSuper'
instance HasDisplay   MouseButtonEvent where evDisplay   = evDisplay  .mbeSuper'

instance HasMouseAxis MouseButtonEvent where
  evMouseX    = evMouseX . mbeSuper'
  evMouseY    = evMouseY . mbeSuper'
  evMouseZ    = evMouseZ . mbeSuper'
  evMouseW    = evMouseW . mbeSuper'




