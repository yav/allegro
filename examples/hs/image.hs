import Allegro
import Allegro.Display
import Allegro.EventQueue
import Allegro.Keyboard
import Allegro.Mouse
import Allegro.Graphics
import Allegro.Timer

red = Color 1 1 1 1


drawIt b t x y = drawModifiedBitmap b dx dy (tinted (Color al al al 1) $
                                             region (dx,dy) 100 100 normal)
  where
  dx = realToFrac x
  dy = realToFrac y
  al = abs (sin (realToFrac t / 40))



main :: IO ()
main =
  allegro $
  do b <- loadBitmap "../resources/example.jpg"
     withDisplay FixedWindow (bitmapWidth b) (bitmapHeight b) $ \_ ->
       do q <- createEventQueue
          registerEventSource q =<< installKeyboard
          registerEventSource q =<< installMouse
          t <- createTimer (1/40)
          registerEventSource q t
          startTimer t
          let go x y = do ev <- waitForEvent q
                          case ev of
                            KeyDown k | evKey k == key_ESCAPE -> return ()
                            MouseMove m -> go (evMouseX m) (evMouseY m)
                            Time t ->
                              do clearToColor (Color 0 0 0 0)
                                 drawIt b (evCount t) x y
                                 flipDisplay
                                 go x y
                            _ -> go x y
          go (bitmapWidth b `div` 2) (bitmapHeight b `div` 2)


