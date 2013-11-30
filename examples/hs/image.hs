import Allegro
import Allegro.Display
import Allegro.EventQueue
import Allegro.Keyboard
import Allegro.Graphics

main :: IO ()
main =
  allegro $
  do b <- loadBitmap "../resources/example.jpg"
     withDisplay FixedWindow (bitmapWidth b) (bitmapHeight b) $ \_ ->
       do drawBitmap b 0 0 Nothing
          flipDisplay
          q <- createEventQueue
          registerEventSource q =<< installKeyboard
          let go = do ev <- waitForEvent q
                      case ev of
                        KeyDown k | evKey k == key_ESCAPE -> return ()
                        _ -> go
          go


