{-# LANGUAGE DeriveDataTypeable #-}
module Allegro.Display where

import Allegro.Types
import Allegro.C.Display

import           Foreign
import           Control.Monad
import qualified Control.Exception as X
import           Data.Typeable

data DisplayType = Windowed Bool    -- ^ Is it resizeable
                 | FullScreen       -- ^ Create a full-screen window


withDisplay :: DisplayType -> Int -> Int -> (Display -> IO ()) -> IO ()
withDisplay how w h k =
  do d <- createDisplay how w h
     k d `X.finally` destroyDisplay d

createDisplay :: DisplayType -> Int -> Int -> IO Display
createDisplay how w h =
  do al_set_new_display_flags $
      case how of
        FullScreen -> fullscreen
        Windowed res | res        -> windowed .|. resizeable
                     | otherwise  -> windowed

     p <- al_create_display (fromIntegral w) (fromIntegral h)
     when (p == nullPtr) $ X.throwIO FailedToCreateDisplay
     return (Display p)

destroyDisplay :: Display -> IO ()
destroyDisplay (Display p) = al_destroy_display p

data FailedToCreateDisplay    = FailedToCreateDisplay
  deriving (Typeable,Show)

instance X.Exception FailedToCreateDisplay

flipDisplay :: IO ()
flipDisplay = al_flip_display


