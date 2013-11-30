{-# LANGUAGE TypeFamilies #-}
module Allegro.Types where

import Allegro.C.Types
import Allegro.C.Display
import Allegro.C.Event
import Control.Applicative ( (<$>), (<*>) )
import Foreign  ( Ptr, ForeignPtr, withForeignPtr )

newtype Bitmap  = Bitmap (ForeignPtr BITMAP) deriving Eq
newtype Display = Display (Ptr DISPLAY)
                  deriving (Eq,Show)

data Color      = Color { cRed, cGreen, cBlue, cAlpha :: Float }
                  deriving (Eq,Show)

--------------------------------------------------------------------------------
withBitmapPtr :: Bitmap -> (Ptr BITMAP -> IO a) -> IO a
withBitmapPtr (Bitmap x) = withForeignPtr x

--------------------------------------------------------------------------------
instance EventSource Display where
  eventSource (Display p) = al_get_display_event_source p
  foreignClient _ = Nothing -- XXX?


class HasName t where
  type CType t
  isNamed :: t -> Name t -> Bool


{- | The name of an object.
Object names are used to recognize an object (see 'isNamed').
The main use-case for 'Name' is to link events with the object that
issued them.  A name is valid only while its corresponding object is alive.
-}
newtype Name t    = Name (Ptr (CType t)) deriving Eq



class ParseEvent t where
  eventDetails :: Ptr EVENT -> IO t

class EventSource t where
  eventSource   :: t -> IO (Ptr EVENT_SOURCE)
  foreignClient :: t -> Maybe (ForeignPtr ())

--------------------------------------------------------------------------------
-- Common Events


data SomeEvent = EvAny { evType'      :: Int
                       , evTimestamp' :: Double
                       }

class HasType t where
  evType      :: t -> Int

class HasTimestamp t where
  evTimestamp :: t -> Double

class HasDisplay t where
  evDisplay   :: t -> Display



instance ParseEvent SomeEvent where
  eventDetails p = EvAny <$> (fromIntegral <$> event_type p)
                         <*> (realToFrac   <$> event_any_timestamp p)

instance HasType      SomeEvent where evType      = evType'
instance HasTimestamp SomeEvent where evTimestamp = evTimestamp'











