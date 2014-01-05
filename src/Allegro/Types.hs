{-# LANGUAGE Safe #-}
module Allegro.Types where

import Allegro.C.Types
import Allegro.C.Display
import Allegro.C.Event
import Control.Applicative ( (<$>), (<*>) )
import Data.IORef (IORef)
import Foreign.Safe  ( Ptr, ForeignPtr, withForeignPtr )

type Point = (Float,Float)    -- ^ A position
type Dim   = (Float,Float)    -- ^ width, height

--------------------------------------------------------------------------------

newtype Display = Display (Ptr DISPLAY)
                  deriving (Eq,Show)

data Color      = Color { cRed, cGreen, cBlue, cAlpha :: Float }
                  deriving (Eq,Show)

--------------------------------------------------------------------------------
newtype Bitmap  = Bitmap (ForeignPtr BITMAP) deriving Eq

withBitmapPtr :: Bitmap -> (Ptr BITMAP -> IO a) -> IO a
withBitmapPtr (Bitmap x) = withForeignPtr x


--------------------------------------------------------------------------------
-- The event queue keeps track of who's registered with it, so
-- that they don't get destroyed prematurely.
data EventQueue     = EventQueue { eqPtr :: ForeignPtr EVENT_QUEUE
                                 , eqReg :: IORef [ ForeignPtr () ]
                                 }
                      deriving Eq

withQ :: EventQueue -> (Ptr EVENT_QUEUE -> IO a) -> IO a
withQ = withForeignPtr . eqPtr



class ParseEvent t where
  eventDetails :: EventQueue -> Ptr EVENT -> IO t



class EventSource t where
  eventSource   :: t -> IO (Ptr EVENT_SOURCE)
  foreignClient :: t -> Maybe (ForeignPtr ())

instance EventSource Display where
  eventSource (Display p) = al_get_display_event_source p
  foreignClient _ = Nothing -- XXX?



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
  eventDetails _ p = EvAny <$> (fromIntegral <$> event_type p)
                           <*> (realToFrac   <$> event_any_timestamp p)

instance HasType      SomeEvent where evType      = evType'
instance HasTimestamp SomeEvent where evTimestamp = evTimestamp'











