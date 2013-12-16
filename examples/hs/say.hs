import Allegro
import Allegro.Graphics
import Allegro.Display
import Allegro.EventQueue
import Allegro.Keyboard
import Allegro.Primitives
import Allegro.Font
import System.Environment

black = Color 0 0 0   1
white = Color 1 1 1   1
green = Color 0 0.5 0 1


textBox :: Font -> String -> Point -> IO ()
textBox f msg (x,y) =
  do drawShape r white Filled
     drawShape r black (Outlined 3)
     drawText f black (x + 5) (y + 5) AlignLeft msg
  where
  w = 10 + fromIntegral (textWidth f msg)
  h = fromIntegral (fontLineHeight f) + 10
  r = Rectangle { rectTopLeft     = (x,y)
                , rectBottomRight = (x + w, y + h)
                , rectCurved      = Just (2,2)
                }

main :: IO ()
main =
  allegro $
  loadFont "../resources/font.ttf" 16 defaultFontFlags >>= \f ->
  getArgs >>= \as ->
  withDisplay FixedWindow 640 480 $ \_ ->
    do q <- createEventQueue
       registerEventSource q =<< installKeyboard

       clearToColor green
       textBox f (unwords as) (100,100)

       flipDisplay
       let go = do ev <- waitForEvent q
                   case ev of
                     KeyDown k | evKey k == key_ESCAPE -> return ()
                     _ -> go
       go


