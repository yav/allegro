import Allegro
import Allegro.Display
import Allegro.EventQueue as EventQueue
import Allegro.Keyboard as Keyboard


import qualified Control.Exception as X


main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 800 600 $ \d ->
  do q <- EventQueue.create
     EventQueue.register q =<< Keyboard.create
     let go = do ev <- EventQueue.wait q
                 print $ evType ev
                 case ev of
                   KeyDown k
                     | evKey k == key_ESCAPE -> return ()
                   _ -> go
     go



