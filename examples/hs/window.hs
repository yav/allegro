import Allegro
import Allegro.Display
import Allegro.EventQueue
import Allegro.Keyboard

main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 800 600 $ \_ ->
  do q <- createEventQueue
     registerEventSource q =<< installKeyboard
     let go = do ev <- waitForEvent q
                 print $ evType ev
                 case ev of
                   KeyDown k | evKey k == key_ESCAPE -> return ()
                   _ -> go
     go



