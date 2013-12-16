import Allegro
import Allegro.Graphics
import Allegro.Display
import Allegro.EventQueue
import Allegro.Keyboard
import Allegro.Primitives
import Data.Array

red = Color 1 0 0 1
green = Color 0 1 0 1

mark (x,y) = drawShape Rectangle { rectTopLeft = (x-2,y-2)
                                 , rectBottomRight = (x+2,y+2)
                                 , rectCurved = Nothing
                                 } red Filled

test (p1 : p2 : more) = do mark p1
                           drawLine (Line p1 p2) green 0
                           test (p2 : more)
test _ = return ()



main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 640 480 $ \_ ->
    do q <- createEventQueue
       registerEventSource q =<< installKeyboard

       drawShape (Triangle (0,30) (30,30) (15,0)) green Filled

       drawShape Rectangle { rectTopLeft     = (50,0)
                           , rectBottomRight = (80,30)
                           , rectCurved      = Nothing
                           } green Filled

       drawShape Rectangle { rectTopLeft     = (100,0)
                           , rectBottomRight = (130,30)
                           , rectCurved      = Just (5,5)
                           } green Filled

       drawShape Arc { arcCenter = (165,15)
                     , arcRadius = 15
                     , arcRadiusY = Nothing
                     , arcTheta  = 0
                     , arcDelta  = (2 * pi) / 3
                     } green (Outlined 3)

       drawShape Circle { circCenter   = (215,30)
                        , circRadius   = 15
                        , circRadiusY  = Just 30
                        } green Filled


       drawLine (Line (0,50) (30,80)) green 3

       drawLine Arc { arcCenter = (165,65)
                    , arcRadius = 15
                    , arcRadiusY = Nothing
                    , arcTheta  = 0
                    , arcDelta  = (2 * pi) / 3
                    } green 3




       flipDisplay
       let go = do ev <- waitForEvent q
                   case ev of
                     KeyDown k | evKey k == key_ESCAPE -> return ()
                     _ -> go
       go


