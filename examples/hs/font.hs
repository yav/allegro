{-# LANGUAGE ViewPatterns #-}
import Allegro
import Allegro.Types
import Allegro.Display
import Allegro.Event
import Allegro.Font as Font
import Allegro.Keyboard as Keyboard


import qualified Control.Exception as X

red = Color 1 0 0 0

main :: IO ()
main =
  allegro $
  withDisplay (Windowed False) 640 480 $ \d ->
  do f <- loadFont "../resources/font.ttf" 12 Font.defaultFlags
     putStr $ unlines [ "lineHeight = " ++ show (lineHeight f)
                      , "ascent = " ++ show (ascent f)
                      , "descent = " ++ show (descent f)
                      ]
     q <- createEventQueue
     registerEventSource q Keyboard
     let go n =
          do ev <- waitForEvent q
             drawText f red (fromIntegral (div n 20 * 2 * textWidth f "m"))
                            (fromIntegral $ mod n 20 * Font.lineHeight f)
                            AlignLeft $ show $ evType ev
             flipDisplay
             print $ evType ev
             case ev of
               KeyChar (evKeyChar -> Just 'q') -> return ()
               _ -> go (n + 1)
     go 0




