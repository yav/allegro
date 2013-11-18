import Allegro
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
  do ok <- al_init
     unless ok $
       do hPutStrLn stderr "Failed to initialize Allegro"
          exitFailure

     withDisplay 640 480 $ \display -> do
     {-
     when (display == nullPtr) $
        do hPutStrLn stderr "Failed to create display"
           exitFailure -}

     al_clear_to_color 0.5 0 0 1
     al_flip_display

     q <- al_create_event_queue
     when (q == nullPtr) $
        do hPutStrLn stderr "Failed to create display"
           -- al_destroy_display display
           exitFailure

     al_register_event_source q =<< getDisplayEventSource display

     allocaBytes event_size_bytes $ \evPtr ->
       forever $ do al_wait_for_event q evPtr
                    print =<< event_type evPtr

--     al_destroy_display display



