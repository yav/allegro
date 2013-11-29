{-# LANGUAGE ViewPatterns #-}
import Allegro
import Allegro.Display
import Allegro.Event
import Allegro.Keyboard as Keyboard


import qualified Control.Exception as X


main :: IO ()
main =
  allegro $
  withDisplay (Windowed False) 800 600 $ \d ->
  do keyboard <- createKeyboard

     q <- createEventQueue
     registerEventSource q keyboard
     let go = do ev <- waitForEvent q
                 print $ evType ev
                 case ev of
                   KeyChar (evKeyChar -> Just 'q') -> return ()
                   _ -> go
     go



