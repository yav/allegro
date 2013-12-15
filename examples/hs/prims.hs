import Allegro
import Allegro.Graphics
import Allegro.Display
import Allegro.EventQueue
import Allegro.Keyboard
import Allegro.Primitives
import Data.Array

red = Color 1 0 0 1
green = Color 0 1 0 1


test (p1 : p2 : more) = drawLine p1 p2 green 0 >> test (p2 : more)
test _ = return ()



main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 640 480 $ \_ ->
    do q <- createEventQueue
       registerEventSource q =<< installKeyboard
       {-
       drawLine (0,0) (100,100) red 1
       drawTriangle (0,200) (100,200) (100,300) green Filled
       drawTriangle (0,200) (100,200) (100,300) red   (Outlined 2)

       drawRoundedRectangle (120,200) (220,300) (20,10) green Filled
       drawRoundedRectangle (120,200) (220,300) (20,10) green Filled
       -}

       ps <- calculateSpline (0,0) (50,100) (100,50) (150,100) 10 1000
       print (length (elems ps))
       test (elems ps)


       flipDisplay
       let go = do ev <- waitForEvent q
                   case ev of
                     KeyDown k | evKey k == key_ESCAPE -> return ()
                     _ -> go
       go


