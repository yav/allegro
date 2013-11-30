{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Mouse
  ( -- * Initialization
    installMouse
  , Mouse

    -- * Events
  , MouseEvent
  , HasMouseAxis(..)

  , MouseMoveEvent
  , evMouseDX, evMouseDY, evMouseDZ, evMouseDW

  , MouseButtonEvent
  , evMouseButton

  -- * Exceptions
  , FailedToInstallMouse(..)
  ) where

import           Allegro.Types( EventSource(..), ParseEvent(..)
                              , SomeEvent, HasTimestamp(..), HasType(..)
                              , Display(..), HasDisplay(..)
                              )
import           Allegro.C.Mouse
import           Allegro.C.Event

import           Foreign(ForeignPtr, newForeignPtr, nullPtr)
import           Data.Typeable(Typeable)
import qualified Control.Exception as X
import           Control.Monad(unless)
import           Control.Applicative( (<$>), (<*>) )

newtype Mouse     = Mouse    (ForeignPtr ())    deriving Eq

instance EventSource Mouse where
  eventSource _ = al_get_mouse_event_source
  foreignClient (Mouse t) = Just t


-- | Try to install the keyboard driver.
-- Throws 'FailedToInstallMouse' if something goes wrong.
installMouse :: IO Mouse
installMouse =
  do ok <- al_install_mouse
     unless ok $ X.throwIO FailedToInstallMouse
     Mouse `fmap` newForeignPtr shal_uninstall_mouse_addr nullPtr

--------------------------------------------------------------------------------
-- Mouse events

data MouseEvent   = ME
  { meSuper'    :: {-# UNPACK #-} !SomeEvent
  , meDisplay'  :: !Display
  , meX', meY', meZ', meW' :: !Int
  }

instance ParseEvent MouseEvent where
  eventDetails q p = ME <$> eventDetails q p
                        <*> (Display <$> event_mouse_display p)
                        <*> (fromIntegral <$> event_mouse_x p)
                        <*> (fromIntegral <$> event_mouse_y p)
                        <*> (fromIntegral <$> event_mouse_z p)
                        <*> (fromIntegral <$> event_mouse_w p)

instance HasType      MouseEvent where evType      = evType      . meSuper'
instance HasTimestamp MouseEvent where evTimestamp = evTimestamp . meSuper'
instance HasDisplay   MouseEvent where evDisplay   = meDisplay'

class HasMouseAxis t where
  evMouseX, evMouseY, evMouseZ, evMouseW :: t -> Int

instance HasMouseAxis MouseEvent where
  evMouseX    = meX'
  evMouseY    = meY'
  evMouseZ    = meZ'
  evMouseW    = meW'


data MouseMoveEvent    = MME
  { mmeSuper' :: {-# UNPACK #-} !MouseEvent
  , evMouseDX, evMouseDY, evMouseDZ, evMouseDW :: !Int
  }

instance ParseEvent MouseMoveEvent where
  eventDetails q p = MME <$> eventDetails q p
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
  , evMouseButton :: !Int
  }

instance ParseEvent MouseButtonEvent where
  eventDetails q p = MBE <$> eventDetails q p
                         <*> (fromIntegral <$> event_mouse_button p)

instance HasType      MouseButtonEvent where evType      = evType     .mbeSuper'
instance HasTimestamp MouseButtonEvent where evTimestamp = evTimestamp.mbeSuper'
instance HasDisplay   MouseButtonEvent where evDisplay   = evDisplay  .mbeSuper'

instance HasMouseAxis MouseButtonEvent where
  evMouseX    = evMouseX . mbeSuper'
  evMouseY    = evMouseY . mbeSuper'
  evMouseZ    = evMouseZ . mbeSuper'
  evMouseW    = evMouseW . mbeSuper'






--------------------------------------------------------------------------------
-- Exceptions

data FailedToInstallMouse     = FailedToInstallMouse
  deriving (Typeable,Show)

instance X.Exception FailedToInstallMouse


