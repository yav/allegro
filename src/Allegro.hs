{-# LANGUAGE DeriveDataTypeable #-}
module Allegro
  ( -- * Initialization
    allegro

  -- * Colors
  , Color(..)

  -- * Exceptions
  , FailedToInitialize(..)
  , FailedToInitializeFonts(..)
  ) where

import Allegro.Types (Color(..))
import Allegro.C
import Allegro.C.Font
import Allegro.C.Image
import Allegro.C.Audio

import           Control.Concurrent ( runInBoundThread )
import qualified Control.Exception as X
import           Control.Monad ( unless )
import           Data.Typeable ( Typeable )

brack :: X.Exception x => IO Bool -> x -> IO () -> IO a -> IO a
brack mk err unmk k =
  do ok <- mk
     unless ok (X.throwIO err)
     k `X.finally` unmk


allegro :: IO () -> IO ()
allegro m =
  runInBoundThread $
  brack al_init FailedToInitialize (return ()) $
  withFonts $ withImages $ withACodecs $ withAudio m

withFonts :: IO a -> IO a
withFonts =
  brack (al_init_font_addon >> al_init_ttf_addon)
        FailedToInitializeFonts
        al_shutdown_ttf_addon

withImages :: IO a -> IO a
withImages =
  brack al_init_image_addon
        FailedToInitializeImages
        al_shutdown_image_addon

withAudio :: IO a -> IO a
withAudio =
  brack al_install_audio
        FailedToInitializeAudio
        al_uninstall_audio

withACodecs :: IO a -> IO a
withACodecs =
  brack al_init_acodec_addon
        FailedToInitializeCodecs
        (return ())



data FailedToInitialize       = FailedToInitialize
                                deriving (Typeable,Show)
data FailedToInitializeFonts  = FailedToInitializeFonts
                                deriving (Typeable,Show)
data FailedToInitializeImages = FailedToInitializeImages
                                deriving (Typeable,Show)
data FailedToInitializeAudio  = FailedToInitializeAudio
                                deriving (Typeable,Show)
data FailedToInitializeCodecs = FailedToInitializeCodecs
                                deriving (Typeable,Show)

instance X.Exception FailedToInitialize
instance X.Exception FailedToInitializeFonts
instance X.Exception FailedToInitializeImages
instance X.Exception FailedToInitializeAudio
instance X.Exception FailedToInitializeCodecs




