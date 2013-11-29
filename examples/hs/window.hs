{-# LANGUAGE ViewPatterns #-}
import Allegro
import Allegro.Display
import Allegro.Event
import Allegro.Keyboard as Keyboard


import qualified Control.Exception as X


main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 800 600 $ \d ->
  do q <- createEventQueue
     registerEventSource q =<< createKeyboard
     let go = do ev <- waitForEvent q
                 print $ evType ev
                 case ev of
                   KeyChar (evKeyChar -> Just 'q') -> return ()
                   _ -> go
     go



