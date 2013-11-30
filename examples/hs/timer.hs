import Allegro
import Allegro.Display
import Allegro.EventQueue as EventQueue
import Allegro.Timer      as Timer
import Allegro.Keyboard   as Keyboard


import qualified Control.Exception as X

red = Color 1 0 0 0

main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 640 480 $ \d ->
  do q <-  EventQueue.create
     t1 <- Timer.create (4 / 4)
     t2 <- Timer.create (3 / 4)
     EventQueue.register q =<< Keyboard.create
     EventQueue.register q t1
     EventQueue.register q t2
     let go =
           do ev <- EventQueue.wait q
              case ev of
                KeyDown k
                  | evKey k == key_ESCAPE -> return ()
                  | evKey k == key_S      -> do Timer.start t1
                                                Timer.start t2
                                                go
                  | evKey k == key_D      -> do Timer.stop t1
                                                Timer.stop t2
                                                go
                Time t
                  | evTimer t == t1 -> print (1, evCount t) >> go
                  | evTimer t == t2 -> print (2, evCount t) >> go
                _ -> go
     go




