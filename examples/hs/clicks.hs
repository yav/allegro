{-# LANGUAGE RecordWildCards #-}
import Allegro
import Allegro.Display
import Allegro.EventQueue
import Allegro.Keyboard
import Allegro.Mouse
import Allegro.Graphics
import Allegro.Timer
import Allegro.Primitives
import Control.Monad

red = Color 1 1 1 1


drawIt :: [Thing] -> IO ()
drawIt ps = do clearToColor (Color 0 0.5 0 1)
               forM_ ps $ \Thing { .. } ->
                   drawShape Circle { circCenter  = pos
                                    , circRadius  = 10
                                    , circRadiusY = Nothing
                                    } (Color (life/5) 0 0 1) Filled
               forM_ (splines $ map pos ps) $ \s -> drawLine s (Color 1 0 1 1) 2
               flipDisplay
  where
  splines (a : b : c : d : more) = Spline a b c d : splines (b : c : d : more)
  splines _ = []

data Thing = Thing { pos :: Point, life :: Float }

enQ :: (Int,Int) -> [Thing] -> [Thing]
enQ (x,y) xs = Thing { pos = (fromIntegral x, fromIntegral y), life = 5 } : xs

age :: [Thing] -> [Thing]
age xs = [ Thing { life = life - 1, .. } | Thing { .. } <- xs, life > 0.1 ]


main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 640 480 $ \_ ->
       do q <- createEventQueue
          registerEventSource q =<< installKeyboard
          registerEventSource q =<< installMouse
          t <- createTimer (1/4)
          registerEventSource q t
          startTimer t
          let go xs  = do ev <- waitForEvent q
                          case ev of
                            KeyDown k | evKey k == key_ESCAPE -> return ()
                            MouseButtonDown ev ->
                              go (enQ (evMouseX ev, evMouseY ev) xs)
                            Time t ->
                              do drawIt xs
                                 go (age xs)
                            _ -> go xs
          go []


