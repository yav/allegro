{-# LANGUAGE ViewPatterns #-}
import Allegro
import Allegro.Types
import Allegro.Display
import Allegro.Event
import Allegro.Timer as Timer
import Allegro.Keyboard as Keyboard


import qualified Control.Exception as X

red = Color 1 0 0 0

main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 640 480 $ \d ->
  do q <- createEventQueue
     t1 <- Timer.create (1 / 25)
     t2 <- Timer.create (1 / 30)
     registerEventSource q =<< createKeyboard
     registerEventSource q t1
     registerEventSource q t2
     let go n =
          do ev <- waitForEvent q
             print $ evType ev
             case ev of
               KeyDown (evKey -> key_ESCAPE) -> return ()
cd 
               _ -> go (n + 1)
     go 0




