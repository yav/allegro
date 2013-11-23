import Allegro
import Allegro.Display
import Allegro.DisplayMode
import Allegro.Event
import Allegro.C
import Allegro.C.Display
import Allegro.C.Event
import Allegro.C.EventQueue

import System.IO(hPutStrLn,stderr)
import System.Exit(exitFailure)
import Control.Monad(when,unless,forever)
import Control.Concurrent(threadDelay)

import Foreign

main :: IO ()
main =
  do Allegro.initialize
     mapM_ print =<< displayModes

     display <- createDisplay (Windowed False) 800 600


     q <- createEventQueue
     registerEventSource q display

     print =<< waitForEvent q

     destroyDisplay display
     destroyEventQueue q



