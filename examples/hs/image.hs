import Allegro
import Allegro.Display
import Allegro.EventQueue as EventQueue
import Allegro.Keyboard as Keyboard
import Allegro.Graphics


import qualified Control.Exception as X


main :: IO ()
main =
  allegro $
  do b <- loadBitmap "../resources/example.jpg"
     withDisplay FixedWindow (bitmapWidth b) (bitmapHeight b) $ \d ->
       do drawBitmap b 0 0 Nothing
          flipDisplay
          q <- EventQueue.create
          EventQueue.register q =<< Keyboard.create
          let go = do ev <- EventQueue.wait q
                      case ev of
                        KeyDown k | evKey k == key_ESCAPE -> return ()
                        _ -> go
          go


