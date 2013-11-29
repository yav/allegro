{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Keyboard where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Allegro.C.Types

#include <allegro5/allegro.h>

foreign import ccall "al_install_keyboard"
  al_install_keyboard :: IO Bool

foreign import ccall "al_uninstall_keyboard"
  al_uninstall_keyboard :: IO ()

foreign import ccall "&shal_uninstall_keyboard"
  shal_uninstall_keyboard_addr :: FunPtr (Ptr () -> IO ())

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

newtype Key = Key CInt
              deriving (Eq,Ord,Show)

key_A
  , key_B
  , key_C
  , key_D
  , key_E
  , key_F
  , key_G
  , key_H
  , key_I
  , key_J
  , key_K
  , key_L
  , key_M
  , key_N
  , key_O
  , key_P
  , key_Q
  , key_R
  , key_S
  , key_T
  , key_U
  , key_V
  , key_W
  , key_X
  , key_Y
  , key_Z
  , key_0
  , key_1
  , key_2
  , key_3
  , key_4
  , key_5
  , key_6
  , key_7
  , key_8
  , key_9
  , key_PAD_0
  , key_PAD_1
  , key_PAD_2
  , key_PAD_3
  , key_PAD_4
  , key_PAD_5
  , key_PAD_6
  , key_PAD_7
  , key_PAD_8
  , key_PAD_9
  , key_ESCAPE
  , key_TILDE
  , key_MINUS
  , key_EQUALS
  , key_BACKSPACE
  , key_TAB
  , key_OPENBRACE
  , key_CLOSEBRACE
  , key_ENTER
  , key_SEMICOLON
  , key_QUOTE
  , key_BACKSLASH
  , key_BACKSLASH2
  , key_COMMA
  , key_FULLSTOP
  , key_SLASH
  , key_SPACE
  , key_INSERT
  , key_DELETE
  , key_HOME
  , key_END
  , key_PGUP
  , key_PGDN
  , key_LEFT
  , key_RIGHT
  , key_UP
  , key_DOWN
  , key_PAD_SLASH
  , key_PAD_ASTERISK
  , key_PAD_MINUS
  , key_PAD_PLUS
  , key_PAD_DELETE
  , key_PAD_ENTER
  , key_PRINTSCREEN
  , key_PAUSE
  , key_ABNT_C1
  , key_YEN
  , key_KANA
  , key_CONVERT
  , key_NOCONVERT
  , key_AT
  , key_CIRCUMFLEX
  , key_COLON2
  , key_KANJI
  , key_LSHIFT
  , key_RSHIFT
  , key_LCTRL
  , key_RCTRL
  , key_ALT
  , key_ALTGR
  , key_LWIN
  , key_RWIN
  , key_MENU
  , key_SCROLLLOCK
  , key_NUMLOCK
  , key_CAPSLOCK
  , key_PAD_EQUALS
  , key_BACKQUOTE
  , key_SEMICOLON2
  , key_COMMAND
  :: Key

key_A            = Key #{const ALLEGRO_KEY_A}
key_B            = Key #{const ALLEGRO_KEY_B}
key_C            = Key #{const ALLEGRO_KEY_C}
key_D            = Key #{const ALLEGRO_KEY_D}
key_E            = Key #{const ALLEGRO_KEY_E}
key_F            = Key #{const ALLEGRO_KEY_F}
key_G            = Key #{const ALLEGRO_KEY_G}
key_H            = Key #{const ALLEGRO_KEY_H}
key_I            = Key #{const ALLEGRO_KEY_I}
key_J            = Key #{const ALLEGRO_KEY_J}
key_K            = Key #{const ALLEGRO_KEY_K}
key_L            = Key #{const ALLEGRO_KEY_L}
key_M            = Key #{const ALLEGRO_KEY_M}
key_N            = Key #{const ALLEGRO_KEY_N}
key_O            = Key #{const ALLEGRO_KEY_O}
key_P            = Key #{const ALLEGRO_KEY_P}
key_Q            = Key #{const ALLEGRO_KEY_Q}
key_R            = Key #{const ALLEGRO_KEY_R}
key_S            = Key #{const ALLEGRO_KEY_S}
key_T            = Key #{const ALLEGRO_KEY_T}
key_U            = Key #{const ALLEGRO_KEY_U}
key_V            = Key #{const ALLEGRO_KEY_V}
key_W            = Key #{const ALLEGRO_KEY_W}
key_X            = Key #{const ALLEGRO_KEY_X}
key_Y            = Key #{const ALLEGRO_KEY_Y}
key_Z            = Key #{const ALLEGRO_KEY_Z}
key_0            = Key #{const ALLEGRO_KEY_0}
key_1            = Key #{const ALLEGRO_KEY_1}
key_2            = Key #{const ALLEGRO_KEY_2}
key_3            = Key #{const ALLEGRO_KEY_3}
key_4            = Key #{const ALLEGRO_KEY_4}
key_5            = Key #{const ALLEGRO_KEY_5}
key_6            = Key #{const ALLEGRO_KEY_6}
key_7            = Key #{const ALLEGRO_KEY_7}
key_8            = Key #{const ALLEGRO_KEY_8}
key_9            = Key #{const ALLEGRO_KEY_9}
key_PAD_0        = Key #{const ALLEGRO_KEY_PAD_0}
key_PAD_1        = Key #{const ALLEGRO_KEY_PAD_1}
key_PAD_2        = Key #{const ALLEGRO_KEY_PAD_2}
key_PAD_3        = Key #{const ALLEGRO_KEY_PAD_3}
key_PAD_4        = Key #{const ALLEGRO_KEY_PAD_4}
key_PAD_5        = Key #{const ALLEGRO_KEY_PAD_5}
key_PAD_6        = Key #{const ALLEGRO_KEY_PAD_6}
key_PAD_7        = Key #{const ALLEGRO_KEY_PAD_7}
key_PAD_8        = Key #{const ALLEGRO_KEY_PAD_8}
key_PAD_9        = Key #{const ALLEGRO_KEY_PAD_9}
key_ESCAPE       = Key #{const ALLEGRO_KEY_ESCAPE}
key_TILDE        = Key #{const ALLEGRO_KEY_TILDE}
key_MINUS        = Key #{const ALLEGRO_KEY_MINUS}
key_EQUALS       = Key #{const ALLEGRO_KEY_EQUALS}
key_BACKSPACE    = Key #{const ALLEGRO_KEY_BACKSPACE}
key_TAB          = Key #{const ALLEGRO_KEY_TAB}
key_OPENBRACE    = Key #{const ALLEGRO_KEY_OPENBRACE}
key_CLOSEBRACE   = Key #{const ALLEGRO_KEY_CLOSEBRACE}
key_ENTER        = Key #{const ALLEGRO_KEY_ENTER}
key_SEMICOLON    = Key #{const ALLEGRO_KEY_SEMICOLON}
key_QUOTE        = Key #{const ALLEGRO_KEY_QUOTE}
key_BACKSLASH    = Key #{const ALLEGRO_KEY_BACKSLASH}
key_BACKSLASH2   = Key #{const ALLEGRO_KEY_BACKSLASH2}
key_COMMA        = Key #{const ALLEGRO_KEY_COMMA}
key_FULLSTOP     = Key #{const ALLEGRO_KEY_FULLSTOP}
key_SLASH        = Key #{const ALLEGRO_KEY_SLASH}
key_SPACE        = Key #{const ALLEGRO_KEY_SPACE}
key_INSERT       = Key #{const ALLEGRO_KEY_INSERT}
key_DELETE       = Key #{const ALLEGRO_KEY_DELETE}
key_HOME         = Key #{const ALLEGRO_KEY_HOME}
key_END          = Key #{const ALLEGRO_KEY_END}
key_PGUP         = Key #{const ALLEGRO_KEY_PGUP}
key_PGDN         = Key #{const ALLEGRO_KEY_PGDN}
key_LEFT         = Key #{const ALLEGRO_KEY_LEFT}
key_RIGHT        = Key #{const ALLEGRO_KEY_RIGHT}
key_UP           = Key #{const ALLEGRO_KEY_UP}
key_DOWN         = Key #{const ALLEGRO_KEY_DOWN}
key_PAD_SLASH    = Key #{const ALLEGRO_KEY_PAD_SLASH}
key_PAD_ASTERISK = Key #{const ALLEGRO_KEY_PAD_ASTERISK}
key_PAD_MINUS    = Key #{const ALLEGRO_KEY_PAD_MINUS}
key_PAD_PLUS     = Key #{const ALLEGRO_KEY_PAD_PLUS}
key_PAD_DELETE   = Key #{const ALLEGRO_KEY_PAD_DELETE}
key_PAD_ENTER    = Key #{const ALLEGRO_KEY_PAD_ENTER}
key_PRINTSCREEN  = Key #{const ALLEGRO_KEY_PRINTSCREEN}
key_PAUSE        = Key #{const ALLEGRO_KEY_PAUSE}
key_ABNT_C1      = Key #{const ALLEGRO_KEY_ABNT_C1}
key_YEN          = Key #{const ALLEGRO_KEY_YEN}
key_KANA         = Key #{const ALLEGRO_KEY_KANA}
key_CONVERT      = Key #{const ALLEGRO_KEY_CONVERT}
key_NOCONVERT    = Key #{const ALLEGRO_KEY_NOCONVERT}
key_AT           = Key #{const ALLEGRO_KEY_AT}
key_CIRCUMFLEX   = Key #{const ALLEGRO_KEY_CIRCUMFLEX}
key_COLON2       = Key #{const ALLEGRO_KEY_COLON2}
key_KANJI        = Key #{const ALLEGRO_KEY_KANJI}
key_LSHIFT       = Key #{const ALLEGRO_KEY_LSHIFT}
key_RSHIFT       = Key #{const ALLEGRO_KEY_RSHIFT}
key_LCTRL        = Key #{const ALLEGRO_KEY_LCTRL}
key_RCTRL        = Key #{const ALLEGRO_KEY_RCTRL}
key_ALT          = Key #{const ALLEGRO_KEY_ALT}
key_ALTGR        = Key #{const ALLEGRO_KEY_ALTGR}
key_LWIN         = Key #{const ALLEGRO_KEY_LWIN}
key_RWIN         = Key #{const ALLEGRO_KEY_RWIN}
key_MENU         = Key #{const ALLEGRO_KEY_MENU}
key_SCROLLLOCK   = Key #{const ALLEGRO_KEY_SCROLLLOCK}
key_NUMLOCK      = Key #{const ALLEGRO_KEY_NUMLOCK}
key_CAPSLOCK     = Key #{const ALLEGRO_KEY_CAPSLOCK}
key_PAD_EQUALS   = Key #{const ALLEGRO_KEY_PAD_EQUALS}
key_BACKQUOTE    = Key #{const ALLEGRO_KEY_BACKQUOTE}
key_SEMICOLON2   = Key #{const ALLEGRO_KEY_SEMICOLON2}
key_COMMAND      = Key #{const ALLEGRO_KEY_COMMAND}


newtype KeyMod = KM CUInt
                  deriving (Eq,Show)

km_SHIFT
  , km_CTRL
  , km_ALT
  , km_LWIN
  , km_RWIN
  , km_MENU
  , km_ALTGR
  , km_COMMAND
  , km_SCROLLLOCK
  , km_NUMLOCK
  , km_CAPSLOCK
  , km_INALTSEQ
  , km_ACCENT1
  , km_ACCENT2
  , km_ACCENT3
  , km_ACCENT4
  :: KeyMod

km_SHIFT      = KM #{const ALLEGRO_KEYMOD_SHIFT}
km_CTRL       = KM #{const ALLEGRO_KEYMOD_CTRL}
km_ALT        = KM #{const ALLEGRO_KEYMOD_ALT}
km_LWIN       = KM #{const ALLEGRO_KEYMOD_LWIN}
km_RWIN       = KM #{const ALLEGRO_KEYMOD_RWIN}
km_MENU       = KM #{const ALLEGRO_KEYMOD_MENU}
km_ALTGR      = KM #{const ALLEGRO_KEYMOD_ALTGR}
km_COMMAND    = KM #{const ALLEGRO_KEYMOD_COMMAND}
km_SCROLLLOCK = KM #{const ALLEGRO_KEYMOD_SCROLLLOCK}
km_NUMLOCK    = KM #{const ALLEGRO_KEYMOD_NUMLOCK}
km_CAPSLOCK   = KM #{const ALLEGRO_KEYMOD_CAPSLOCK}
km_INALTSEQ   = KM #{const ALLEGRO_KEYMOD_INALTSEQ}
km_ACCENT1    = KM #{const ALLEGRO_KEYMOD_ACCENT1}
km_ACCENT2    = KM #{const ALLEGRO_KEYMOD_ACCENT2}
km_ACCENT3    = KM #{const ALLEGRO_KEYMOD_ACCENT3}
km_ACCENT4    = KM #{const ALLEGRO_KEYMOD_ACCENT4}



