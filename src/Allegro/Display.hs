{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Display
  ( withDisplay
  , createDisplay
  , Display
  , DisplayWindow(..)
  , destroyDisplay

  -- * Display properties
  , setWindowTitle
  , setDisplayIcon

  -- * Working with the current display
  , flipDisplay

  -- * Exceptions
  , FailedToCreateDisplay(..)
  ) where

import Allegro.Types
import Allegro.C.Display

import           Foreign (nullPtr)
import           Foreign.C.String ( withCString )
import           Data.Bits( (.|.) )
import           Control.Monad (when)
import qualified Control.Exception as X
import           Data.Typeable (Typeable)

data DisplayWindow  = FixedWindow
                    | ResizableWindow
                    | FullScreen

withDisplay :: DisplayWindow -> Int -> Int -> (Display -> IO ()) -> IO ()
withDisplay win w h k =
  do d <- createDisplay win w h
     k d `X.finally` destroyDisplay d

createDisplay :: DisplayWindow -> Int -> Int -> IO Display
createDisplay win w h =
  do al_set_new_display_flags $
      case win of
        FullScreen      -> fullscreen
        FixedWindow     -> windowed
        ResizableWindow -> windowed .|. resizeable

     p <- al_create_display (fromIntegral w) (fromIntegral h)
     when (p == nullPtr) $ X.throwIO FailedToCreateDisplay
     return (Display p)

data FailedToCreateDisplay    = FailedToCreateDisplay
  deriving (Typeable,Show)

instance X.Exception FailedToCreateDisplay

setWindowTitle :: Display -> String -> IO ()
setWindowTitle (Display d) x =
  withCString x $ \p -> al_set_window_title d p

setDisplayIcon :: Display -> Bitmap -> IO ()
setDisplayIcon (Display d) x =
  withPtr x $ \p -> al_set_display_icon d p

destroyDisplay :: Display -> IO ()
destroyDisplay (Display p) = al_destroy_display p

flipDisplay :: IO ()
flipDisplay = al_flip_display




