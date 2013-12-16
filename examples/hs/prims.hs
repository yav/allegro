import Allegro
import Allegro.Graphics
import Allegro.Display
import Allegro.EventQueue
import Allegro.Keyboard
import Allegro.Primitives
import Data.Array

red = Color 1 0 0 1
green = Color 0 1 0 1

mark (x,y) = drawRectangle (x-2,y-2) (x+2,y+2) red Filled

test (p1 : p2 : more) = do mark p1
                           drawLine p1 p2 green 0
                           test (p2 : more)
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

       let ps = calculateSpline (0,0) (50,100) (100,50) (150,100)
       let ps = calculateArc (100,100) (20,50) (pi/2) (pi/2) 5
       -- drawRibbon (listArray (0,4) [ (0,0), (100,100), (150,50), (200,100), (0,0) ]) red 5
       test (elems ps)

       flipDisplay
       let go = do ev <- waitForEvent q
                   case ev of
                     KeyDown k | evKey k == key_ESCAPE -> return ()
                     _ -> go
       go


