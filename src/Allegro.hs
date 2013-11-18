module Allegro where

import Allegro.C.Types
import Allegro.C.Display
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Control.Monad
import Control.Applicative
import Control.Concurrent

newtype Display     = Display (Ptr DISPLAY)
newtype Timer       = Timer (Ptr TIMER)
newtype Joystick    = Joystick (Ptr JOYSTICK)
data EventQueue  = EventQueue (Ptr EVENT_QUEUE)
newtype EventSource = EventSource (Ptr EVENT_SOURCE)
                      deriving (Eq,Ord)

withDisplay :: Int -> Int -> (Display -> IO ()) -> IO ()
withDisplay w h k =
  runInBoundThread $
  do p <- al_create_display (toEnum w) (toEnum h)
     if p == nullPtr then fail "Failed to create display"
                     else k (Display p)
     al_destroy_display p

-- XXX
getDisplayEventSource :: Display -> IO (Ptr EVENT_SOURCE) -- EventSource
getDisplayEventSource (Display fp) =
  id <$> al_get_display_event_source fp

{-
newtype Modifiers = Modifiers CUInt

getEvent :: EventQueue -> IO (Maybe Event)
getEvent (EventQueue q) =
  allocaBytes eventSizeBytes $ \p ->
  do yes <- al_get_next_event q p
     if yes then Just `fmap` parseEvent p
            else return Nothing

peekEvent :: EventQueue -> IO (Maybe Event)
peekEvent (EventQueue q) =
  allocaBytes eventSizeBytes $ \p ->
  do yes <- al_peek_next_event q p
     if yes then Just `fmap` parseEvent p
            else return Nothing

parseEvent :: Ptr EVENT -> IO Event
parseEvent p =
  do ty <- event_type p
     case () of
      _
     {-
       | ty == aLLEGRO_EVENT_JOYSTICK_AXIS          ->
          EventJoysticAxis <$> 

       | ty == aLLEGRO_EVENT_JOYSTICK_BUTTON_DOWN   ->
       | ty == aLLEGRO_EVENT_JOYSTICK_BUTTON_UP     ->
       | ty == aLLEGRO_EVENT_JOYSTICK_CONFIGURATION ->
       -}

       | ty == allegroEventKeyDown ->
                KeyDown <$> (Display    <$> event_keyboard_display p)
                        <*> (parseKey   <$> event_keyboard_keycode p)

       | ty == allegroEventKeyUp ->
                KeyUp   <$> (Display    <$> event_keyboard_display p)
                        <*> (parseKey   <$> event_keyboard_keycode p)

       | ty == allegroEventKeyChar ->
                KeyChar <$> (Display    <$> event_keyboard_display p)
                        <*> (parseKey   <$> event_keyboard_keycode p)
                        <*> (parseChar  <$> event_keyboard_unichar p)
                        <*> (Modifiers  <$> event_keyboard_modifiers p)
                        <*> event_keyboard_repeat p
{-

       | ty == aLLEGRO_EVENT_MOUSE_AXES             ->
       | ty == aLLEGRO_EVENT_MOUSE_BUTTON_DOWN      ->
       | ty == aLLEGRO_EVENT_MOUSE_BUTTON_UP        ->
       | ty == aLLEGRO_EVENT_MOUSE_ENTER_DISPLAY    ->
       | ty == aLLEGRO_EVENT_MOUSE_LEAVE_DISPLAY    ->
       | ty == aLLEGRO_EVENT_MOUSE_WARPED           ->

       | ty == aLLEGRO_EVENT_TIMER                  ->

       | ty == aLLEGRO_EVENT_DISPLAY_EXPOSE         ->
       | ty == aLLEGRO_EVENT_DISPLAY_RESIZE         ->
       | ty == aLLEGRO_EVENT_DISPLAY_CLOSE          ->
       | ty == aLLEGRO_EVENT_DISPLAY_LOST           ->
       | ty == aLLEGRO_EVENT_DISPLAY_FOUND          ->
       | ty == aLLEGRO_EVENT_DISPLAY_SWITCH_IN      ->
       | ty == aLLEGRO_EVENT_DISPLAY_SWITCH_OUT     ->
       | ty == aLLEGRO_EVENT_DISPLAY_ORIENTATION    -> 
-}
       | otherwise  -> return $ UnknownEvent $ fromEnum ty

  where
  parseChar :: CInt -> Maybe Char
  parseChar c = do guard (c >= 0)
                   return $ toEnum $ fromEnum c

  parseKey :: CInt -> Key
  parseKey _ = Key_S

data Event = KeyDown Display Key
           | KeyUp   Display Key
           | KeyChar Display Key (Maybe Char) Modifiers Bool
           | UnknownEvent Int

keycode_to_key :: CInt -> Key
keycode_to_key c | 0 <= c && c < #{const ALLEGRO_KEY_MAX} = keyMap ! fromEnum c
                 | otherwise = KEY_UNKNOWN c



{-
data JoystickAxisEvent = JOYSTICK_AXIS          Joystick Int Int Float
data Event = JOYSTICK_BUTTON_DOWN   Joystick Int
data Event = JOYSTICK_BUTTON_UP     Joystick Int
data Event = JOYSTICK_CONFIGURATION  -- XXX

data Event = MOUSE_AXES           Display Int Int Int Int Int Int Int Int
data Event = MOUSE_BUTTON_DOWN    Display Int Int Int Int Word
data Event = MOUSE_BUTTON_UP      Display Int Int Int Int Word
data Event = MOUSE_ENTER_DISPLAY  Display Int Int Int Int
data Event = MOUSE_LEAVE_DISPLAY  Display Int Int Int Int
data Event = MOUSE_WARPED

data Event = TIMER Timer Int64

data Event = DISPLAY_EXPOSE Display Int Int Int Int
data Event = DISPLAY_RESIZE Display Int Int Int Int
data Event = DISPLAY_CLOSE Display
data Event = DISPLAY_LOST Display
data Event = DISPLAY_FOUND Display
data Event = DISPLAY_SWITCH_IN Display
data Event = DISPLAY_SWITCH_OUT Display
data Event = DISPLAY_ORIENTATION Display Orientation
-}
-}
