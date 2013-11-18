{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Keyboard where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Allegro.C.Types
import Data.Array

#include <allegro5/allegro.h>

foreign import ccall "al_install_keyboard"
  al_install_keyboard :: IO Bool

foreign import ccall "al_uninstall_keyboard"
  al_uninstall_keyboard :: IO ()

foreign import ccall "al_is_keyboard_installed"
  al_is_keyboard_installed :: IO Bool

foreign import ccall "al_get_keyboard_event_source"
  al_get_keyboard_event_source :: IO (Ptr EVENT_SOURCE)

foreign import ccall "al_get_keyboard_state"
  al_get_keyboard_state :: IO (Ptr KEYBOARD_STATE)

foreign import ccall "al_key_down"
  al_key_down :: Ptr KEYBOARD_STATE -> CInt -> IO Bool

foreign import ccall "al_keycode_to_name"
  al_keycode_to_name :: CInt -> CString

key_map :: Array Int Key
key_map = array (0,#{const ALLEGRO_KEY_MAX}-1)
  [(#{const ALLEGRO_KEY_A }, KEY_A )
  ,(#{const ALLEGRO_KEY_B }, KEY_B )
  ,(#{const ALLEGRO_KEY_C }, KEY_C )
  ,(#{const ALLEGRO_KEY_D }, KEY_D )
  ,(#{const ALLEGRO_KEY_E }, KEY_E )
  ,(#{const ALLEGRO_KEY_F }, KEY_F )
  ,(#{const ALLEGRO_KEY_G }, KEY_G )
  ,(#{const ALLEGRO_KEY_H}, KEY_H)
  ,(#{const ALLEGRO_KEY_I }, KEY_I )
  ,(#{const ALLEGRO_KEY_J }, KEY_J )
  ,(#{const ALLEGRO_KEY_K }, KEY_K )
  ,(#{const ALLEGRO_KEY_L }, KEY_L )
  ,(#{const ALLEGRO_KEY_M }, KEY_M )
  ,(#{const ALLEGRO_KEY_N }, KEY_N )
  ,(#{const ALLEGRO_KEY_O }, KEY_O )
  ,(#{const ALLEGRO_KEY_P}, KEY_P)
  ,(#{const ALLEGRO_KEY_Q }, KEY_Q )
  ,(#{const ALLEGRO_KEY_R }, KEY_R )
  ,(#{const ALLEGRO_KEY_S }, KEY_S )
  ,(#{const ALLEGRO_KEY_T }, KEY_T )
  ,(#{const ALLEGRO_KEY_U }, KEY_U )
  ,(#{const ALLEGRO_KEY_V }, KEY_V )
  ,(#{const ALLEGRO_KEY_W }, KEY_W )
  ,(#{const ALLEGRO_KEY_X}, KEY_X)
  ,(#{const ALLEGRO_KEY_Y }, KEY_Y )
  ,(#{const ALLEGRO_KEY_Z}, KEY_Z)
  ,(#{const ALLEGRO_KEY_0 }, KEY_0 )
  ,(#{const ALLEGRO_KEY_1 }, KEY_1 )
  ,(#{const ALLEGRO_KEY_2 }, KEY_2 )
  ,(#{const ALLEGRO_KEY_3 }, KEY_3 )
  ,(#{const ALLEGRO_KEY_4 }, KEY_4 )
  ,(#{const ALLEGRO_KEY_5 }, KEY_5 )
  ,(#{const ALLEGRO_KEY_6 }, KEY_6 )
  ,(#{const ALLEGRO_KEY_7}, KEY_7)
  ,(#{const ALLEGRO_KEY_8 }, KEY_8 )
  ,(#{const ALLEGRO_KEY_9}, KEY_9)
  ,(#{const ALLEGRO_KEY_PAD_0 }, KEY_PAD_0 )
  ,(#{const ALLEGRO_KEY_PAD_1 }, KEY_PAD_1 )
  ,(#{const ALLEGRO_KEY_PAD_2 }, KEY_PAD_2 )
  ,(#{const ALLEGRO_KEY_PAD_3 }, KEY_PAD_3 )
  ,(#{const ALLEGRO_KEY_PAD_4 }, KEY_PAD_4 )
  ,(#{const ALLEGRO_KEY_PAD_5}, KEY_PAD_5)
  ,(#{const ALLEGRO_KEY_PAD_6 }, KEY_PAD_6 )
  ,(#{const ALLEGRO_KEY_PAD_7 }, KEY_PAD_7 )
  ,(#{const ALLEGRO_KEY_PAD_8 }, KEY_PAD_8 )
  ,(#{const ALLEGRO_KEY_PAD_9}, KEY_PAD_9)
  ,(#{const ALLEGRO_KEY_ESCAPE }, KEY_ESCAPE )
  ,(#{const ALLEGRO_KEY_TILDE }, KEY_TILDE )
  ,(#{const ALLEGRO_KEY_MINUS }, KEY_MINUS )
  ,(#{const ALLEGRO_KEY_EQUALS }, KEY_EQUALS )
  ,(#{const ALLEGRO_KEY_BACKSPACE }, KEY_BACKSPACE )
  ,(#{const ALLEGRO_KEY_TAB}, KEY_TAB)
  ,(#{const ALLEGRO_KEY_OPENBRACE }, KEY_OPENBRACE )
  ,(#{const ALLEGRO_KEY_CLOSEBRACE }, KEY_CLOSEBRACE )
  ,(#{const ALLEGRO_KEY_ENTER }, KEY_ENTER )
  ,(#{const ALLEGRO_KEY_SEMICOLON }, KEY_SEMICOLON )
  ,(#{const ALLEGRO_KEY_QUOTE}, KEY_QUOTE)
  ,(#{const ALLEGRO_KEY_BACKSLASH }, KEY_BACKSLASH )
  ,(#{const ALLEGRO_KEY_BACKSLASH2 }, KEY_BACKSLASH2 )
  ,(#{const ALLEGRO_KEY_COMMA }, KEY_COMMA )
  ,(#{const ALLEGRO_KEY_FULLSTOP }, KEY_FULLSTOP )
  ,(#{const ALLEGRO_KEY_SLASH}, KEY_SLASH)
  ,(#{const ALLEGRO_KEY_SPACE}, KEY_SPACE)
  ,(#{const ALLEGRO_KEY_INSERT }, KEY_INSERT )
  ,(#{const ALLEGRO_KEY_DELETE }, KEY_DELETE )
  ,(#{const ALLEGRO_KEY_HOME }, KEY_HOME )
  ,(#{const ALLEGRO_KEY_END }, KEY_END )
  ,(#{const ALLEGRO_KEY_PGUP }, KEY_PGUP )
  ,(#{const ALLEGRO_KEY_PGDN}, KEY_PGDN)
  ,(#{const ALLEGRO_KEY_LEFT }, KEY_LEFT )
  ,(#{const ALLEGRO_KEY_RIGHT }, KEY_RIGHT )
  ,(#{const ALLEGRO_KEY_UP }, KEY_UP )
  ,(#{const ALLEGRO_KEY_DOWN}, KEY_DOWN)
  ,(#{const ALLEGRO_KEY_PAD_SLASH }, KEY_PAD_SLASH )
  ,(#{const ALLEGRO_KEY_PAD_ASTERISK }, KEY_PAD_ASTERISK )
  ,(#{const ALLEGRO_KEY_PAD_MINUS }, KEY_PAD_MINUS )
  ,(#{const ALLEGRO_KEY_PAD_PLUS}, KEY_PAD_PLUS)
  ,(#{const ALLEGRO_KEY_PAD_DELETE }, KEY_PAD_DELETE )
  ,(#{const ALLEGRO_KEY_PAD_ENTER }, KEY_PAD_ENTER )
  ,(#{const ALLEGRO_KEY_PRINTSCREEN }, KEY_PRINTSCREEN )
  ,(#{const ALLEGRO_KEY_PAUSE }, KEY_PAUSE )
  ,(#{const ALLEGRO_KEY_ABNT_C1}, KEY_ABNT_C1)
  ,(#{const ALLEGRO_KEY_YEN }, KEY_YEN )
  ,(#{const ALLEGRO_KEY_KANA }, KEY_KANA )
  ,(#{const ALLEGRO_KEY_CONVERT }, KEY_CONVERT )
  ,(#{const ALLEGRO_KEY_NOCONVERT }, KEY_NOCONVERT )
  ,(#{const ALLEGRO_KEY_AT }, KEY_AT )
  ,(#{const ALLEGRO_KEY_CIRCUMFLEX}, KEY_CIRCUMFLEX)
  ,(#{const ALLEGRO_KEY_COLON2 }, KEY_COLON2 )
  ,(#{const ALLEGRO_KEY_KANJI }, KEY_KANJI )
  ,(#{const ALLEGRO_KEY_LSHIFT }, KEY_LSHIFT )
  ,(#{const ALLEGRO_KEY_RSHIFT }, KEY_RSHIFT )
  ,(#{const ALLEGRO_KEY_LCTRL }, KEY_LCTRL )
  ,(#{const ALLEGRO_KEY_RCTRL}, KEY_RCTRL)
  ,(#{const ALLEGRO_KEY_ALT }, KEY_ALT )
  ,(#{const ALLEGRO_KEY_ALTGR }, KEY_ALTGR )
  ,(#{const ALLEGRO_KEY_LWIN }, KEY_LWIN )
  ,(#{const ALLEGRO_KEY_RWIN }, KEY_RWIN )
  ,(#{const ALLEGRO_KEY_MENU }, KEY_MENU )
  ,(#{const ALLEGRO_KEY_SCROLLLOCK}, KEY_SCROLLLOCK)
  ,(#{const ALLEGRO_KEY_NUMLOCK }, KEY_NUMLOCK )
  ,(#{const ALLEGRO_KEY_CAPSLOCK }, KEY_CAPSLOCK )
  ,(#{const ALLEGRO_KEY_PAD_EQUALS }, KEY_PAD_EQUALS )
  ,(#{const ALLEGRO_KEY_BACKQUOTE }, KEY_BACKQUOTE )
  ,(#{const ALLEGRO_KEY_SEMICOLON2}, KEY_SEMICOLON2)
  ,(#{const ALLEGRO_KEY_COMMAND}, KEY_COMMAND)
  ]



data Key
  = KEY_A | KEY_B | KEY_C | KEY_D | KEY_E | KEY_F | KEY_G | KEY_H
  | KEY_I | KEY_J | KEY_K | KEY_L | KEY_M | KEY_N | KEY_O | KEY_P
  | KEY_Q | KEY_R | KEY_S | KEY_T | KEY_U | KEY_V | KEY_W | KEY_X
  | KEY_Y | KEY_Z

  | KEY_0 | KEY_1 | KEY_2 | KEY_3 | KEY_4 | KEY_5 | KEY_6 | KEY_7
  | KEY_8 | KEY_9

  | KEY_PAD_0 | KEY_PAD_1 | KEY_PAD_2 | KEY_PAD_3 | KEY_PAD_4 | KEY_PAD_5
  | KEY_PAD_6 | KEY_PAD_7 | KEY_PAD_8 | KEY_PAD_9

  | KEY_ESCAPE | KEY_TILDE | KEY_MINUS | KEY_EQUALS | KEY_BACKSPACE | KEY_TAB
  | KEY_OPENBRACE | KEY_CLOSEBRACE | KEY_ENTER | KEY_SEMICOLON | KEY_QUOTE
  | KEY_BACKSLASH | KEY_BACKSLASH2 | KEY_COMMA | KEY_FULLSTOP | KEY_SLASH
  | KEY_SPACE
  | KEY_INSERT | KEY_DELETE | KEY_HOME | KEY_END | KEY_PGUP | KEY_PGDN
  | KEY_LEFT | KEY_RIGHT | KEY_UP | KEY_DOWN
  | KEY_PAD_SLASH | KEY_PAD_ASTERISK | KEY_PAD_MINUS | KEY_PAD_PLUS
  | KEY_PAD_DELETE | KEY_PAD_ENTER | KEY_PRINTSCREEN | KEY_PAUSE | KEY_ABNT_C1
  | KEY_YEN | KEY_KANA | KEY_CONVERT | KEY_NOCONVERT | KEY_AT | KEY_CIRCUMFLEX
  | KEY_COLON2 | KEY_KANJI | KEY_LSHIFT | KEY_RSHIFT | KEY_LCTRL | KEY_RCTRL
  | KEY_ALT | KEY_ALTGR | KEY_LWIN | KEY_RWIN | KEY_MENU | KEY_SCROLLLOCK
  | KEY_NUMLOCK | KEY_CAPSLOCK | KEY_PAD_EQUALS | KEY_BACKQUOTE | KEY_SEMICOLON2
  | KEY_COMMAND

  | KEY_UNKNOWN CInt


keymod_SHIFT :: CInt
keymod_SHIFT      = #{const ALLEGRO_KEYMOD_SHIFT}

keymod_CTRL :: CInt
keymod_CTRL       = #{const ALLEGRO_KEYMOD_CTRL}

keymod_ALT :: CInt
keymod_ALT        = #{const ALLEGRO_KEYMOD_ALT}

keymod_LWIN :: CInt
keymod_LWIN       = #{const ALLEGRO_KEYMOD_LWIN}

keymod_RWIN :: CInt
keymod_RWIN       = #{const ALLEGRO_KEYMOD_RWIN}

keymod_MENU :: CInt
keymod_MENU       = #{const ALLEGRO_KEYMOD_MENU}

keymod_ALTGR :: CInt
keymod_ALTGR      = #{const ALLEGRO_KEYMOD_ALTGR}

keymod_COMMAND :: CInt
keymod_COMMAND    = #{const ALLEGRO_KEYMOD_COMMAND}

keymod_SCROLLLOCK :: CInt
keymod_SCROLLLOCK = #{const ALLEGRO_KEYMOD_SCROLLLOCK}

keymod_NUMLOCK :: CInt
keymod_NUMLOCK    = #{const ALLEGRO_KEYMOD_NUMLOCK}

keymod_CAPSLOCK :: CInt
keymod_CAPSLOCK   = #{const ALLEGRO_KEYMOD_CAPSLOCK}

keymod_INALTSEQ :: CInt
keymod_INALTSEQ   = #{const ALLEGRO_KEYMOD_INALTSEQ}

keymod_ACCENT1 :: CInt
keymod_ACCENT1    = #{const ALLEGRO_KEYMOD_ACCENT1}

keymod_ACCENT2 :: CInt
keymod_ACCENT2    = #{const ALLEGRO_KEYMOD_ACCENT2}

keymod_ACCENT3 :: CInt
keymod_ACCENT3    = #{const ALLEGRO_KEYMOD_ACCENT3}

keymod_ACCENT4 :: CInt
keymod_ACCENT4    = #{const ALLEGRO_KEYMOD_ACCENT4}



