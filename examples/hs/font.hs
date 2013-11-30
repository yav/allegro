import Allegro
import Allegro.Display
import Allegro.EventQueue
import Allegro.Font
import Allegro.Keyboard
import Allegro.Graphics

red :: Color
red = Color 1 0 0 0

main :: IO ()
main =
  allegro $
  do f <- loadFont "../resources/font.ttf" 12 defaultFontFlags
     putStr $ unlines [ "lineHeight = " ++ show (fontLineHeight f)
                      , "ascent = "     ++ show (fontAscent f)
                      , "descent = "    ++ show (fontDescent f)
                      ]
     withDisplay FixedWindow 640 480 $ \d ->
       do setWindowTitle d "Hello"
          q <- createEventQueue
          registerEventSource q =<< installKeyboard
          let go n =
               do ev <- waitForEvent q
                  drawText f red (fromIntegral (div n 20 * 2 * textWidth f "m"))
                                 (fromIntegral $ mod n 20 * fontLineHeight f)
                                 AlignLeft $ show $ evType ev
                  flipDisplay
                  print $ evType ev
                  case ev of
                    KeyDown k | evKey k == key_ESCAPE -> return ()
                    _ -> go (n + 1)
          go 0




