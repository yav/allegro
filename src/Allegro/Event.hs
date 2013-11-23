module Allegro.Event where

import Allegro.Types
import Allegro.C.Types
import Allegro.C.Event
import Allegro.C.EventQueue
import Foreign
import Foreign.C.Types
import qualified Control.Exception as X
import Control.Monad


newtype EventSource = EventSource (Ptr EVENT_SOURCE)
                      deriving (Eq,Show)


newtype EventQueue  = EventQueue  (Ptr EVENT_QUEUE)
                      deriving Eq

class IsEventSource t where
  eventSource :: t -> IO EventSource

instance IsEventSource EventSource where
  eventSource = return

createEventQueue :: IO EventQueue
createEventQueue =
  do p <- al_create_event_queue
     when (p == nullPtr) $ X.throwIO FailedToCreateEventQueue
     return (EventQueue p)

destroyEventQueue :: EventQueue -> IO ()
destroyEventQueue (EventQueue q) = al_destroy_event_queue q

registerEventSource :: IsEventSource t => EventQueue -> t -> IO ()
registerEventSource (EventQueue q) x =
  do EventSource s <- eventSource x
     al_register_event_source q s

waitForEvent :: EventQueue -> IO Event
waitForEvent (EventQueue q) =
  allocaBytes event_size_bytes $ \evPtr ->
    do al_wait_for_event q evPtr
       parseEvent evPtr


parseEvent :: Ptr EVENT -> IO Event
parseEvent p =
  do src  <- event_any_source p
     stmp <- event_any_timestamp p
     info <- parseEventInfo p =<< event_type p
     return Event { evSource    = EventSource src
                  , evTimestamp = fromRational (toRational stmp)
                  , evInfo      = info
                  }

parseEventInfo :: Ptr EVENT -> CInt -> IO EventInfo
parseEventInfo p ty
  | ty == event_display_close       = DisplayClose `fmap` evt_display_source
  | ty == event_display_switch_in   = DisplaySwitchIn `fmap` evt_display_source
  | ty == event_display_switch_out  = DisplaySwitchOut `fmap` evt_display_source

  where
  evt_display_source = Display `fmap` event_display_source p

parseEventInfo _ ty = return (Unknown ty)




data Event = Event { evSource    :: EventSource
                   , evTimestamp :: Double
                   , evInfo      :: EventInfo
                   }
              deriving Show

data EventInfo  = DisplayClose     Display
                | DisplaySwitchIn  Display
                | DisplaySwitchOut Display
                | Unknown CInt
                deriving Show




