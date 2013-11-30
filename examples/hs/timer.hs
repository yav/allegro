import Allegro
import Allegro.Display
import Allegro.EventQueue
import Allegro.Timer
import Allegro.Keyboard

main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 640 480 $ \_ ->
  do q <-  createEventQueue
     t1 <- createTimer (4 / 4)
     t2 <- createTimer (3 / 4)
     registerEventSource q =<< installKeyboard
     registerEventSource q t1
     registerEventSource q t2
     let go =
           do ev <- waitForEvent q
              case ev of
                KeyDown k
                  | evKey k == key_ESCAPE -> return ()
                  | evKey k == key_S      -> do startTimer t1
                                                startTimer t2
                                                go
                  | evKey k == key_D      -> do stopTimer t1
                                                stopTimer t2
                                                go
                Time t
                  | evTimer t == t1 -> print (1::Int, evCount t) >> go
                  | evTimer t == t2 -> print (2::Int, evCount t) >> go
                _ -> go
     go




