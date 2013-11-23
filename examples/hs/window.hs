{-# LANGUAGE ViewPatterns #-}
import Allegro
import Allegro.Display
import Allegro.Event
import Allegro.Keyboard as Keyboard


import qualified Control.Exception as X


main :: IO ()
main =
  do Allegro.initialize
     d <- createDisplay (Windowed False) 800 600
     do Keyboard.install
        q <- createEventQueue

        registerEventSource q Keyboard
        let go = do ev <- waitForEvent q
                    print $ evType ev
                    case ev of
                      KeyChar (evKeyChar -> Just 'q') -> return ()
                      _ -> go
        go

      `X.finally` destroyDisplay d



