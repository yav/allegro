{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Event where

import Allegro.C.Types

import Foreign
import Foreign.C.Types

#include <allegro5/allegro.h>

event_joystick_axis           = #{const ALLEGRO_EVENT_JOYSTICK_AXIS}
event_joystick_button_down    = #{const ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN}
event_joystick_button_up      = #{const ALLEGRO_EVENT_JOYSTICK_BUTTON_UP}
event_joystick_configuration  = #{const ALLEGRO_EVENT_JOYSTICK_CONFIGURATION}

event_key_down                = #{const ALLEGRO_EVENT_KEY_DOWN}
event_key_char                = #{const ALLEGRO_EVENT_KEY_CHAR}
event_key_up                  = #{const ALLEGRO_EVENT_KEY_UP}

event_mouse_axes              = #{const ALLEGRO_EVENT_MOUSE_AXES}
event_mouse_button_down       = #{const ALLEGRO_EVENT_MOUSE_BUTTON_DOWN}
event_mouse_button_up         = #{const ALLEGRO_EVENT_MOUSE_BUTTON_UP}
event_mouse_enter_display     = #{const ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY}
event_mouse_leave_display     = #{const ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY}
event_mouse_warped            = #{const ALLEGRO_EVENT_MOUSE_WARPED}

event_timer                   = #{const ALLEGRO_EVENT_TIMER}

event_display_expose          = #{const ALLEGRO_EVENT_DISPLAY_EXPOSE}
event_display_resize          = #{const ALLEGRO_EVENT_DISPLAY_RESIZE}
event_display_close           = #{const ALLEGRO_EVENT_DISPLAY_CLOSE}
event_display_lost            = #{const ALLEGRO_EVENT_DISPLAY_LOST}
event_display_found           = #{const ALLEGRO_EVENT_DISPLAY_FOUND}
event_display_switch_in       = #{const ALLEGRO_EVENT_DISPLAY_SWITCH_IN}
event_display_switch_out      = #{const ALLEGRO_EVENT_DISPLAY_SWITCH_OUT}
event_display_orientation     = #{const ALLEGRO_EVENT_DISPLAY_ORIENTATION}


event_size_bytes :: Int
event_size_bytes = #{size ALLEGRO_EVENT}

event_type :: Ptr EVENT -> IO CInt
event_type = #{peek ALLEGRO_EVENT,type}

event_any_source :: Ptr EVENT -> IO (Ptr EVENT_SOURCE)
event_any_source = #{peek ALLEGRO_EVENT,any.source}

event_any_timestamp :: Ptr EVENT -> IO CDouble
event_any_timestamp = #{peek ALLEGRO_EVENT,any.timestamp}



event_keyboard_keycode :: Ptr EVENT -> IO CInt
event_keyboard_keycode = #{peek ALLEGRO_EVENT,keyboard.keycode}

event_keyboard_unichar :: Ptr EVENT -> IO CInt
event_keyboard_unichar = #{peek ALLEGRO_EVENT,keyboard.unichar}

event_keyboard_modifiers :: Ptr EVENT -> IO CUInt
event_keyboard_modifiers = #{peek ALLEGRO_EVENT,keyboard.unichar}

event_keyboard_repeat :: Ptr EVENT -> IO Bool
event_keyboard_repeat = #{peek ALLEGRO_EVENT,keyboard.repeat}

event_keyboard_display :: Ptr EVENT -> IO (Ptr DISPLAY)
event_keyboard_display = #{peek ALLEGRO_EVENT,keyboard.display}




event_mouse_x :: Ptr EVENT -> IO CInt
event_mouse_x = #{peek ALLEGRO_EVENT,mouse.x}

event_mouse_y :: Ptr EVENT -> IO CInt
event_mouse_y = #{peek ALLEGRO_EVENT,mouse.y}

event_mouse_z :: Ptr EVENT -> IO CInt
event_mouse_z = #{peek ALLEGRO_EVENT,mouse.z}

event_mouse_w :: Ptr EVENT -> IO CInt
event_mouse_w = #{peek ALLEGRO_EVENT,mouse.w}

event_mouse_dx :: Ptr EVENT -> IO CInt
event_mouse_dx = #{peek ALLEGRO_EVENT,mouse.dx}

event_mouse_dy :: Ptr EVENT -> IO CInt
event_mouse_dy = #{peek ALLEGRO_EVENT,mouse.dy}

event_mouse_dz :: Ptr EVENT -> IO CInt
event_mouse_dz = #{peek ALLEGRO_EVENT,mouse.dz}

event_mouse_dw :: Ptr EVENT -> IO CInt
event_mouse_dw = #{peek ALLEGRO_EVENT,mouse.dw}

event_mouse_button :: Ptr EVENT -> IO CUInt
event_mouse_button = #{peek ALLEGRO_EVENT,mouse.button}

event_mouse_display :: Ptr EVENT -> IO (Ptr DISPLAY)
event_mouse_display = #{peek ALLEGRO_EVENT,mouse.display}


event_timer_source :: Ptr EVENT -> IO (Ptr TIMER)
event_timer_source = #{peek ALLEGRO_EVENT,timer.source}

event_timer_count :: Ptr EVENT -> IO Int64
event_timer_count = #{peek ALLEGRO_EVENT,timer.count}


event_display_source :: Ptr EVENT -> IO (Ptr DISPLAY)
event_display_source = #{peek ALLEGRO_EVENT,display.source}

event_display_x :: Ptr EVENT -> IO CInt
event_display_x = #{peek ALLEGRO_EVENT,display.x}

event_display_y :: Ptr EVENT -> IO CInt
event_display_y = #{peek ALLEGRO_EVENT,display.y}

event_display_width :: Ptr EVENT -> IO CInt
event_display_width = #{peek ALLEGRO_EVENT,display.width}

event_display_height :: Ptr EVENT -> IO CInt
event_display_height = #{peek ALLEGRO_EVENT,display.height}




