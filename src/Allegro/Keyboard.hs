{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Keyboard
  ( -- * Operations
    Keyboard
  , create

    -- * Events
  , KeyEvent
  , HasKey(..)

  , KeyCharEvent
  , evKeyChar
  , evKeyMod
  , evKeyRepeated



  -- * Keys
  , Key
  , keyName

  , key_A
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

  -- * Key modifiers
  , KeyMod

  -- ** Modifier sets
  , kmNone
  , kmAdd
  , kmCommon
  , kmOthers
  , kmDiff
  , kmHas

  -- ** Single modifiers
  , km_SHIFT
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

  -- * Exceptions
  , FailedToInstallKeyboard(..)
  ) where

import Allegro.Types ( EventSource(..)
                     , ParseEvent(..)
                     , SomeEvent, HasTimestamp(..), HasType(..)
                     , Display(..), HasDisplay(..)
                     )
import Allegro.C.Keyboard
import Allegro.C.Event

import           Control.Applicative ( (<$>), (<*>) )
import           Control.Monad ( unless, guard )
import qualified Control.Exception as X
import           Data.Bits ( (.|.), (.&.), complement )
import           Data.Typeable(Typeable)
import           Foreign(ForeignPtr, newForeignPtr, nullPtr)
import           Foreign.C.String(peekCString)
import           System.IO.Unsafe(unsafeDupablePerformIO)

newtype Keyboard  = Keyboard (ForeignPtr ())    deriving Eq

instance EventSource Keyboard where
  eventSource _ = al_get_keyboard_event_source
  foreignClient (Keyboard t) = Just t

-- | Try to install the keyboard driver.
-- Throws '`FailedToInstallKeyboard' if we something goes wrong.
create :: IO Keyboard
create =
  do ok <- al_install_keyboard
     unless ok $ X.throwIO FailedToInstallKeyboard
     Keyboard `fmap` newForeignPtr shal_uninstall_keyboard_addr nullPtr

data FailedToInstallKeyboard  = FailedToInstallKeyboard
  deriving (Typeable,Show)
instance X.Exception FailedToInstallKeyboard


keyName :: Key -> String
keyName (Key x) = unsafeDupablePerformIO
                $ peekCString
                $ al_keycode_to_name x

-- | No modifiers
kmNone :: KeyMod
kmNone = KM 0

-- | Union: modifiers that are in either of parameters.
kmAdd :: KeyMod -> KeyMod -> KeyMod
kmAdd (KM x) (KM y) = KM (x .|. y)

-- | Intersection: modifiers that appear in both parameters.
kmCommon :: KeyMod -> KeyMod -> KeyMod
kmCommon (KM x) (KM y) = KM (x .&. y)

-- | Complement: modifiers that are not in the parameter.
kmOthers :: KeyMod -> KeyMod
kmOthers (KM x) = KM (complement x)

-- | Difference: modifiers that are in the first but not the second paramter.
kmDiff :: KeyMod -> KeyMod -> KeyMod
kmDiff x y = kmCommon x (kmOthers y)

-- | Subset: Does the first parameter contain all the members of the second?
kmHas :: KeyMod -> KeyMod -> Bool
kmHas x y = kmCommon x y == y


---------------------------------------------------------------------------------- Keyboard events

data KeyEvent = EvKey
  { kSuper'   :: {-# UNPACK #-} !SomeEvent
  , kDisplay' :: !Display
  , kKey'     :: !Key
  }


class HasKey t where
  evKey       :: t -> Key


instance ParseEvent KeyEvent where
  eventDetails q p = EvKey <$> eventDetails q p
                           <*> (Display <$> event_keyboard_display p)
                           <*> (Key <$> event_keyboard_keycode p)


instance HasType      KeyEvent where evType      = evType      . kSuper'
instance HasTimestamp KeyEvent where evTimestamp = evTimestamp . kSuper'
instance HasDisplay   KeyEvent where evDisplay   = kDisplay'
instance HasKey       KeyEvent where evKey       = kKey'



data KeyCharEvent = EvKeyChar
  { kcSuper'      :: {-# UNPACK #-} !KeyEvent
  , evKeyChar     :: !(Maybe Char)
  , evKeyMod      :: !KeyMod
  , evKeyRepeated :: !Bool
  }

instance ParseEvent KeyCharEvent where
  eventDetails q p = EvKeyChar <$> eventDetails q p
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






