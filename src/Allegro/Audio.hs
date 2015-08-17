{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.Audio
  ( -- * Setup
    withAudioAndCodecs
  , withAudio
  , installAudio
  , uninstallAudio
  , FailedToInstallAudio
  , FailedToInitACodecAddon

  -- * Voices
  , createVoice
  , Voice
  , AudioDepth(..)
  , ChannelConf(..)
  , attachMixerToVoice
  , detachVoice
  , getVoiceFrequency
  , getVoiceChannels
  , getVoiceDepth
  , getVoicePlaying
  , setVoicePlaying
  , getVoicePosition
  , setVoicePosition

  , FailedToAttachMixerToVoice(..)
  , FailedToSetVoicePlaying(..)
  , FailedToSetVoicePosition(..)



  -- * Mixers
  , createMixer
  , Mixer
  , attachSampleInstanceToMixer
  , attachAudioStreamToMixer
  , attachMixerToMixer
  , getMixerAttached
  , detachMixer
  , FailedToDetachMixer(..)
  , getMixerFrequency
  , setMixerFrequency
  , FailedToSetMixerFrequncy(..)
  , getMixerChannels
  , getMixerGain
  , setMixerGain
  , FailedToSetMixerGain(..)
  , getMixerQuality
  , MixerQuality(..)
  , setMixerQuality
  , FailedToSetMixerQuality(..)
  , getMixerPlaying
  , setMixerPlaying
  , FailedToSetMixerPlaying(..)

  , FailedToAttachSampleInstanceToMixer(..)
  , FailedToAttachAudioStreamToMixer(..)
  , FailedToAttachMixerToMixer(..)



  -- * Sample Instances
  , createSampleInstance
  , SampleInstance
  , getSampleInstanceChannels
  , getSampleInstanceDepth
  , getSampleInstanceFrequency
  , getSampleInstanceLength
  , setSampleInstanceLength
  , FailedToSetSampleInstanceLength(..)
  , getSampleInstancePosition
  , setSampleInstancePosition
  , FailedToSetSampleInstancePosition(..)
  , getSampleInstanceSpeed
  , setSampleInstanceSpeed
  , FailedToSetSampleInstanceSpeed(..)
  , getSampleInstanceGain
  , setSampleInstanceGain
  , FailedToSetSampleInstanceGain(..)
  , getSampleInstancePan
  , setSampleInstancePan
  , FailedToSetSampleInstancePan(..)
  , PlayMode(..)
  , InvalidPlayMode(..)
  , getSampleInstancePlayMode
  , setSampleInstancePlayMode
  , FailedToSetPlayMode(..)
  , getSampleInstancePlaying
  , setSampleInstancePlaying
  , FailedToSetSampleInstancePlaying(..)
  , getSampleInstanceTime
  , getSampleInstanceAttached
  , detachSampleInstance
  , FailedToDetachSampleInstance(..)

  -- * Samples
  , loadSample
  , Sample
  , FailedToLoadSample(..)

  -- * Audio Streams
  , loadAudioStream
  , AudioStream
  , FailedToLoadAudioStream(..)

  -- * Excetions
  , FailedToCreateVoice(..)
  , FailedToCreateMixer(..)
  , FailedToCreateSampleInstance(..)


  , InvalidAudioDepth(..)
  , InvalidChannelConf(..)
  , InvalidMixerQuality(..)

 ) where

import Allegro.C.Types
import Allegro.C.Audio

import Data.IORef
import Data.Typeable(Typeable)
import Control.Exception(Exception, throwIO, finally)
import Control.Monad(when, unless)

import Foreign  ( Ptr, nullPtr
                , ForeignPtr, newForeignPtr, withForeignPtr, FunPtr
                )
import Foreign.C.String(withCString)


-- | Throws 'FailedToInstallAudio'
installAudio :: IO ()
installAudio = chk FailedToInstallAudio al_install_audio

data FailedToInstallAudio = FailedToInstallAudio
  deriving (Typeable,Show)

instance Exception FailedToInstallAudio

uninstallAudio :: IO ()
uninstallAudio = al_uninstall_audio

-- | Throws 'FailedToInitACodecAddon'
initACodecAddon :: IO ()
initACodecAddon = chk FailedToInitACodecAddon al_init_acodec_addon

data FailedToInitACodecAddon =
  FailedToInitACodecAddon
  deriving (Typeable,Show)

instance Exception FailedToInitACodecAddon


-- | Throws 'FailedToInstallAudio'
withAudio :: IO a -> IO a
withAudio m = do installAudio
                 m `finally` uninstallAudio

-- | Throws 'FailedToInstallAudio', 'FailedToInitACodecAddon'
withAudioAndCodecs :: IO a -> IO a
withAudioAndCodecs m = withAudio (initACodecAddon >> m)


--------------------------------------------------------------------------------


data AudioDepth = Int8
                | Int16
                | Int24
                | Float32
                | Word8
                | Word16
                | Word24
                  deriving (Eq,Ord,Show,Typeable)

inAudioDepth :: AUDIO_DEPTH -> IO AudioDepth
inAudioDepth x
  | x == audio_depth_int8     = return Int8
  | x == audio_depth_int16    = return Int16
  | x == audio_depth_int24    = return Int24
  | x == audio_depth_uint8    = return Word8
  | x == audio_depth_uint16   = return Word16
  | x == audio_depth_uint24   = return Word24
  | x == audio_depth_float32  = return Float32
  | otherwise                 = throwIO (InvalidAudioDepth x)

data InvalidAudioDepth = InvalidAudioDepth AUDIO_DEPTH
                              deriving (Typeable,Show)
instance Exception InvalidAudioDepth

outAudioDepth :: AudioDepth -> AUDIO_DEPTH
outAudioDepth x =
  case x of
    Int8    -> audio_depth_int8
    Int16   -> audio_depth_int16
    Int24   -> audio_depth_int24
    Word8   -> audio_depth_uint8
    Word16  -> audio_depth_uint16
    Word24  -> audio_depth_uint24
    Float32 -> audio_depth_float32

data ChannelConf = ChannelConf_1
                 | ChannelConf_2
                 | ChannelConf_3
                 | ChannelConf_4
                 | ChannelConf_5_1
                 | ChannelConf_6_1
                 | ChannelConf_7_1
                  deriving (Eq,Ord,Show,Typeable)

inChannelConf :: CHANNEL_CONF -> IO ChannelConf
inChannelConf x
  | x == channel_conf_1   = return ChannelConf_1
  | x == channel_conf_2   = return ChannelConf_2
  | x == channel_conf_3   = return ChannelConf_3
  | x == channel_conf_4   = return ChannelConf_4
  | x == channel_conf_5_1 = return ChannelConf_5_1
  | x == channel_conf_6_1 = return ChannelConf_6_1
  | x == channel_conf_7_1 = return ChannelConf_7_1
  | otherwise = throwIO (InvalidChannelConf x)

data InvalidChannelConf = InvalidChannelConf CHANNEL_CONF
                              deriving (Typeable,Show)

instance Exception InvalidChannelConf



outChannelConf :: ChannelConf -> CHANNEL_CONF
outChannelConf x =
  case x of
    ChannelConf_1     -> channel_conf_1
    ChannelConf_2     -> channel_conf_2
    ChannelConf_3     -> channel_conf_3
    ChannelConf_4     -> channel_conf_4
    ChannelConf_5_1   -> channel_conf_5_1
    ChannelConf_6_1   -> channel_conf_6_1
    ChannelConf_7_1   -> channel_conf_7_1


newtype Voice = Voice (ForeignPtr VOICE)
                      deriving (Eq,Ord,Show,Typeable)


mkFP :: Exception x => (ForeignPtr a -> b) ->
                      x -> IO (Ptr a) -> FunPtr (Ptr a -> IO ()) -> IO b
mkFP mk err creat destr =
  do ptr <- creat
     when (ptr == nullPtr) (throwIO err)
     v <- newForeignPtr destr ptr
     return (mk v)


-- | Throws 'FailedToCreateVoice'
createVoice :: Word {- ^ Frequency -} -> AudioDepth -> ChannelConf -> IO Voice
createVoice freq depth conf =
  mkFP Voice (FailedToCreateVoice freq depth conf)
             (al_create_voice (fromIntegral freq) (outAudioDepth depth)
                                                  (outChannelConf conf))
             al_destroy_voice_addr


data FailedToCreateVoice = FailedToCreateVoice Word AudioDepth ChannelConf
                              deriving (Typeable,Show)

instance Exception FailedToCreateVoice



chk :: Exception x => x -> IO Bool -> IO ()
chk err b =
  do x <- b
     unless x (throwIO err)


-- | Throws 'FailedToAttachMixerToVoice'
attachMixerToVoice :: Mixer -> Voice -> IO ()
attachMixerToVoice m@(Mixer mfp p) v@(Voice vfp) =
  withForeignPtr mfp $ \mp ->
  withForeignPtr vfp $ \vp ->
  chk (FailedToAttachMixerToVoice m v) $
    do b <- al_attach_mixer_to_voice mp vp
       when b $ writeIORef p (MixerVoice v)
       return b

data FailedToAttachMixerToVoice = FailedToAttachMixerToVoice Mixer Voice
                              deriving (Typeable,Show)

instance Exception FailedToAttachMixerToVoice



detachVoice :: Voice -> IO ()
detachVoice (Voice vfp) = withForeignPtr vfp al_detach_voice

getVoiceFrequency :: Voice -> IO Word
getVoiceFrequency (Voice vfp) =
  fmap fromIntegral $ withForeignPtr vfp $ al_get_voice_frequency

-- | Throws 'InvalidChannelConf'
getVoiceChannels :: Voice -> IO ChannelConf
getVoiceChannels (Voice vfp) =
  inChannelConf =<< withForeignPtr vfp al_get_voice_channels

-- | Throws 'InvalidAudioDepth'
getVoiceDepth :: Voice -> IO AudioDepth
getVoiceDepth (Voice vfp) =
  inAudioDepth =<< withForeignPtr vfp al_get_voice_depth

getVoicePlaying :: Voice -> IO Bool
getVoicePlaying (Voice vfp) = withForeignPtr vfp $ al_get_voice_playing

-- | Throws 'FailedToSetVoicePlaying'
setVoicePlaying :: Voice -> Bool -> IO ()
setVoicePlaying v@(Voice vfp) b =
  withForeignPtr vfp $ \p ->
    chk (FailedToSetVoicePlaying v b) $ al_set_voice_playing p b

data FailedToSetVoicePlaying = FailedToSetVoicePlaying Voice Bool
                              deriving (Typeable,Show)

instance Exception FailedToSetVoicePlaying


getVoicePosition :: Voice -> IO Word
getVoicePosition (Voice vfp) =
  fmap fromIntegral $ withForeignPtr vfp $ al_get_voice_position

-- | Throws 'FailedToSetVoicePosition'
setVoicePosition :: Voice -> Word -> IO ()
setVoicePosition v@(Voice vfp) pos =
  withForeignPtr vfp $ \p ->
    chk (FailedToSetVoicePosition v pos) $ al_set_voice_position p
                                                          (fromIntegral pos)
data FailedToSetVoicePosition = FailedToSetVoicePosition Voice Word
                              deriving (Typeable,Show)

instance Exception FailedToSetVoicePosition





--------------------------------------------------------------------------------

data MixerQuality = MixerQualityPoint
                  | MixerQualityLinear
                  | MixerQualityCubic
                    deriving (Eq,Show,Ord,Typeable)

inMixerQuality :: MIXER_QUALITY -> IO MixerQuality
inMixerQuality x
  | x == mixer_quality_point  = return MixerQualityPoint
  | x == mixer_quality_linear = return MixerQualityLinear
  | x == mixer_quality_cubic  = return MixerQualityCubic
  | otherwise                 = throwIO (InvalidMixerQuality x)

data InvalidMixerQuality = InvalidMixerQuality MIXER_QUALITY
                              deriving (Typeable,Show)

instance Exception InvalidMixerQuality


outMixerQuality :: MixerQuality -> MIXER_QUALITY
outMixerQuality x =
  case x of
    MixerQualityPoint  -> mixer_quality_point
    MixerQualityLinear -> mixer_quality_linear
    MixerQualityCubic  -> mixer_quality_cubic


data MixerParent = MixerNoParent | MixerVoice Voice | MixerMixer Mixer
                      deriving (Eq,Ord,Show,Typeable)

data Mixer = Mixer (ForeignPtr MIXER) (IORef MixerParent)
                      deriving (Eq,Typeable)

instance Show Mixer where
  showsPrec p (Mixer f _) = showsPrec p f

instance Ord Mixer where
  compare (Mixer f _) (Mixer g _) = compare f g

-- | Throws 'FailedToCreateMixer'
createMixer :: Word {- ^ Frequency -} -> AudioDepth -> ChannelConf -> IO Mixer
createMixer freq depth conf =
  do p <- newIORef MixerNoParent
     mkFP (`Mixer` p)
          (FailedToCreateMixer freq depth conf)
          (al_create_mixer (fromIntegral freq) (outAudioDepth depth)
                                               (outChannelConf conf))
          al_destroy_mixer_addr


data FailedToCreateMixer = FailedToCreateMixer Word AudioDepth ChannelConf
                           deriving (Typeable,Show)

instance Exception FailedToCreateMixer


-- | Throws 'FailedToAttachSampleInstanceToMixer'
attachSampleInstanceToMixer :: SampleInstance -> Mixer -> IO ()
attachSampleInstanceToMixer s@(SampleInstance x) m@(Mixer y _) =
  withForeignPtr x $ \px ->
  withForeignPtr y $ \py ->
    chk (FailedToAttachSampleInstanceToMixer s m)
    $ al_attach_sample_instance_to_mixer px py

data FailedToAttachSampleInstanceToMixer =
  FailedToAttachSampleInstanceToMixer SampleInstance Mixer
                           deriving (Typeable,Show)

instance Exception FailedToAttachSampleInstanceToMixer

-- | Throws 'FailedToAttachAudioStreamToMixer'
attachAudioStreamToMixer :: AudioStream -> Mixer -> IO ()
attachAudioStreamToMixer s@(AudioStream x) m@(Mixer y _) =
  withForeignPtr x $ \px ->
  withForeignPtr y $ \py ->
    chk (FailedToAttachAudioStreamToMixer s m)
    $ al_attach_audio_stream_to_mixer px py

data FailedToAttachAudioStreamToMixer =
  FailedToAttachAudioStreamToMixer AudioStream Mixer
  deriving (Typeable,Show)

instance Exception FailedToAttachAudioStreamToMixer


-- | Throws 'FailedToAttachMixerToMixer'
attachMixerToMixer :: Mixer -> Mixer -> IO ()
attachMixerToMixer s@(Mixer x p) m@(Mixer y _) =
  withForeignPtr x $ \px ->
  withForeignPtr y $ \py ->
    chk (FailedToAttachMixerToMixer s m)
    $ do b <- al_attach_mixer_to_mixer px py
         when b (writeIORef p (MixerMixer m))
         return b

data FailedToAttachMixerToMixer =
  FailedToAttachMixerToMixer Mixer Mixer
  deriving (Typeable,Show)

instance Exception FailedToAttachMixerToMixer


getMixerAttached :: Mixer -> IO Bool
getMixerAttached (Mixer x _) = withForeignPtr x $ al_get_mixer_attached


-- | Throws 'FailedToDetachMixer'
detachMixer :: Mixer -> IO ()
detachMixer m@(Mixer x _) =
  withForeignPtr x $ \px ->
    chk (FailedToDetachMixer m) $ al_detach_mixer px

data FailedToDetachMixer =
  FailedToDetachMixer Mixer
  deriving (Typeable,Show)

instance Exception FailedToDetachMixer

getMixerFrequency :: Mixer -> IO Word
getMixerFrequency (Mixer x _) =
  fmap fromIntegral (withForeignPtr x al_get_mixer_frequency)

-- | Throws 'FailedToSetMixerFrequncy'
setMixerFrequency :: Mixer -> Word -> IO ()
setMixerFrequency m@(Mixer x _) w =
  withForeignPtr x $ \px ->
    chk (FailedToSetMixerFrequncy m w)
    $ al_set_mixer_frequency px (fromIntegral w)

data FailedToSetMixerFrequncy =
  FailedToSetMixerFrequncy Mixer Word
  deriving (Typeable,Show)

instance Exception FailedToSetMixerFrequncy

-- | Throws 'InvalidChannelConf'
getMixerChannels :: Mixer -> IO ChannelConf
getMixerChannels (Mixer m _) =
  inChannelConf =<< withForeignPtr m al_get_mixer_channels

getMixerGain :: Mixer -> IO Float
getMixerGain (Mixer x _) =
  fmap f2f (withForeignPtr x al_get_mixer_gain)

-- | Throws 'FailedToSetMixerGain'
setMixerGain :: Mixer -> Float -> IO ()
setMixerGain m@(Mixer x _) f =
  withForeignPtr x $ \p ->
    chk (FailedToSetMixerGain m f) $ al_set_mixer_gain p (f2f f)

data FailedToSetMixerGain =
  FailedToSetMixerGain Mixer Float
  deriving (Typeable,Show)

instance Exception FailedToSetMixerGain


-- | Throws 'InvalidMixerQuality'
getMixerQuality :: Mixer -> IO MixerQuality
getMixerQuality (Mixer x _) =
  inMixerQuality =<< withForeignPtr x al_get_mixer_quality

-- | Throws 'FailedToSetMixerQuality'
setMixerQuality :: Mixer -> MixerQuality -> IO ()
setMixerQuality m@(Mixer x _) q =
  chk (FailedToSetMixerQuality m q)
  $ withForeignPtr x (\p -> al_set_mixer_quality p (outMixerQuality q))

data FailedToSetMixerQuality =
  FailedToSetMixerQuality Mixer MixerQuality
  deriving (Typeable, Show)

instance Exception FailedToSetMixerQuality

getMixerPlaying :: Mixer -> IO Bool
getMixerPlaying (Mixer m _) = withForeignPtr m al_get_mixer_playing

-- | Throws 'FailedToSetMixerPlaying'
setMixerPlaying :: Mixer -> Bool -> IO ()
setMixerPlaying m@(Mixer x _) b =
  chk (FailedToSetMixerPlaying m b)
  $ withForeignPtr x (\q -> al_set_mixer_playing q b)

data FailedToSetMixerPlaying =
  FailedToSetMixerPlaying Mixer Bool
  deriving (Typeable,Show)

instance Exception FailedToSetMixerPlaying


f2f :: (Real a, Fractional b) => a -> b
f2f x = fromRational (toRational x)

--------------------------------------------------------------------------------

newtype Sample = Sample (ForeignPtr SAMPLE)
                      deriving (Eq,Ord,Show,Typeable)

-- | Throws 'FailedToLoadSample'
loadSample :: FilePath -> IO Sample
loadSample path =
  withCString path $ \str ->
  mkFP Sample (FailedToLoadSample path)
              (al_load_sample str)
              al_destroy_sample_adr


data FailedToLoadSample = FailedToLoadSample FilePath
                              deriving (Typeable,Show)

instance Exception FailedToLoadSample

--------------------------------------------------------------------------------
-- Sample Instances

newtype SampleInstance = SampleInstance (ForeignPtr SAMPLE_INSTANCE)
                      deriving (Eq,Ord,Show,Typeable)

-- | Throws 'FailedToCreateSampleInstance'
createSampleInstance :: Sample -> IO SampleInstance
createSampleInstance s@(Sample fp) =
  withForeignPtr fp $ \sptr ->
    mkFP SampleInstance
         (FailedToCreateSampleInstance s)
         (al_create_sample_instance sptr)
         al_destroy_sample_instance_addr

data FailedToCreateSampleInstance = FailedToCreateSampleInstance Sample
                              deriving (Typeable,Show)

instance Exception FailedToCreateSampleInstance


-- | Throws 'InvalidChannelConf'
getSampleInstanceChannels :: SampleInstance -> IO ChannelConf
getSampleInstanceChannels (SampleInstance x) =
  inChannelConf =<< withForeignPtr x al_get_sample_instance_channels

-- | Throws 'InvalidAudioDepth'
getSampleInstanceDepth :: SampleInstance -> IO AudioDepth
getSampleInstanceDepth (SampleInstance x) =
  inAudioDepth =<< withForeignPtr x al_get_sample_instance_depth

getSampleInstanceFrequency :: SampleInstance -> IO Word
getSampleInstanceFrequency (SampleInstance x) =
  fmap fromIntegral (withForeignPtr x al_get_sample_instance_frequency)

getSampleInstanceLength :: SampleInstance -> IO Word
getSampleInstanceLength (SampleInstance x) =
  fmap fromIntegral (withForeignPtr x al_get_sample_instance_length)

-- | Throws 'FailedToSetSampleInstanceLength'
setSampleInstanceLength :: SampleInstance -> Word -> IO ()
setSampleInstanceLength s@(SampleInstance x) w =
  chk (FailedToSetSampleInstanceLength s w)
  $ withForeignPtr x $ \p -> al_set_sample_instance_length p (fromIntegral w)

data FailedToSetSampleInstanceLength =
  FailedToSetSampleInstanceLength SampleInstance Word
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstanceLength

getSampleInstancePosition :: SampleInstance -> IO Word
getSampleInstancePosition (SampleInstance x) =
  fmap fromIntegral $ withForeignPtr x al_get_sample_instance_position

-- | Throws 'FailedToSetSampleInstancePosition'
setSampleInstancePosition :: SampleInstance -> Word -> IO ()
setSampleInstancePosition s@(SampleInstance x) w =
  chk (FailedToSetSampleInstancePosition s w)
  $ withForeignPtr x $ \p -> al_set_sample_instance_position p (fromIntegral w)

data FailedToSetSampleInstancePosition =
  FailedToSetSampleInstancePosition SampleInstance Word
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstancePosition

getSampleInstanceSpeed :: SampleInstance -> IO Float
getSampleInstanceSpeed (SampleInstance x) =
  fmap f2f (withForeignPtr x al_get_sample_instance_speed)

-- | Throws 'FailedToSetSampleInstanceSpeed'
setSampleInstanceSpeed :: SampleInstance -> Float -> IO ()
setSampleInstanceSpeed s@(SampleInstance x) f =
  chk (FailedToSetSampleInstanceSpeed s f)
  $ withForeignPtr x $ \p -> al_set_sample_instance_speed p (f2f f)

data FailedToSetSampleInstanceSpeed =
  FailedToSetSampleInstanceSpeed SampleInstance Float
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstanceSpeed

getSampleInstanceGain :: SampleInstance -> IO Float
getSampleInstanceGain (SampleInstance x) =
  fmap f2f $ withForeignPtr x al_get_sample_instance_gain

-- | Throws 'FailedToSetSampleInstanceGain'
setSampleInstanceGain :: SampleInstance -> Float -> IO ()
setSampleInstanceGain s@(SampleInstance x) f =
  chk (FailedToSetSampleInstanceGain s f)
  $ withForeignPtr x $ \p -> al_set_sample_instance_gain p (f2f f)

data FailedToSetSampleInstanceGain =
  FailedToSetSampleInstanceGain SampleInstance Float
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstanceGain

getSampleInstancePan :: SampleInstance -> IO Float
getSampleInstancePan (SampleInstance x) =
  fmap f2f $ withForeignPtr x al_get_sample_instance_pan

-- | Throws 'FailedToSetSampleInstancePan'
setSampleInstancePan :: SampleInstance -> Maybe Float -> IO ()
setSampleInstancePan s@(SampleInstance x) mb =
  chk (FailedToSetSampleInstancePan s mb)
  $ withForeignPtr x
  $ \p -> al_set_sample_instance_pan p (maybe audio_pan_none f2f mb)

data FailedToSetSampleInstancePan =
  FailedToSetSampleInstancePan SampleInstance (Maybe Float)
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstancePan

data PlayMode = Once | Loop | BiDir
                deriving (Typeable,Eq,Show)

outPlayMode :: PlayMode -> PLAYMODE
outPlayMode x =
  case x of
    Once  -> playmode_once
    Loop  -> playmode_loop
    BiDir -> playmode_bidir

inPlayMode :: PLAYMODE -> IO PlayMode
inPlayMode x
  | x == playmode_once  = return Once
  | x == playmode_loop  = return Loop
  | x == playmode_bidir = return BiDir
  | otherwise           = throwIO (InvalidPlayMode x)

data InvalidPlayMode = InvalidPlayMode PLAYMODE
                        deriving (Typeable,Show)

instance Exception InvalidPlayMode

-- | Throws 'InvalidPlayMode'
getSampleInstancePlayMode :: SampleInstance -> IO PlayMode
getSampleInstancePlayMode (SampleInstance s) =
  inPlayMode =<< withForeignPtr s al_get_sample_instance_playmode

-- | Throws 'FailedToSetPlayMode'
setSampleInstancePlayMode :: SampleInstance -> PlayMode -> IO ()
setSampleInstancePlayMode s@(SampleInstance x) p =
  chk (FailedToSetPlayMode s p)
  $ withForeignPtr x
  $ \ptr -> al_set_sample_instance_playmode ptr (outPlayMode p)

data FailedToSetPlayMode =
  FailedToSetPlayMode SampleInstance PlayMode
  deriving (Typeable,Show)

instance Exception FailedToSetPlayMode

getSampleInstancePlaying :: SampleInstance -> IO Bool
getSampleInstancePlaying (SampleInstance x) =
  withForeignPtr x al_get_sample_instance_playing

-- | Throws 'FailedToSetSampleInstancePlaying'
setSampleInstancePlaying :: SampleInstance -> Bool -> IO ()
setSampleInstancePlaying s@(SampleInstance x) b =
  chk (FailedToSetSampleInstancePlaying s b)
  $ withForeignPtr x $ \p -> al_set_sample_instance_playing p b

data FailedToSetSampleInstancePlaying =
  FailedToSetSampleInstancePlaying SampleInstance Bool
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstancePlaying

getSampleInstanceTime :: SampleInstance -> IO Float
getSampleInstanceTime (SampleInstance x) =
  fmap f2f (withForeignPtr x al_get_sample_instance_time)

getSampleInstanceAttached :: SampleInstance -> IO Bool
getSampleInstanceAttached (SampleInstance x) =
  withForeignPtr x al_get_sample_instance_attached

-- | Throws 'FailedToDetachSampleInstance'
detachSampleInstance :: SampleInstance -> IO ()
detachSampleInstance s@(SampleInstance x) =
  chk (FailedToDetachSampleInstance s)
  (withForeignPtr x al_detach_sample_instance)

data FailedToDetachSampleInstance =
  FailedToDetachSampleInstance SampleInstance
  deriving (Typeable,Show)

instance Exception FailedToDetachSampleInstance



--------------------------------------------------------------------------------
-- Audio Streams





newtype AudioStream = AudioStream (ForeignPtr AUDIO_STREAM)
                      deriving (Eq,Ord,Show,Typeable)

instance Exception AudioStream

-- | Throws 'FailedToLoadAudioStream'
loadAudioStream :: FilePath -> Word {- ^ buffer count -} ->
                   Word {-^ samples -} -> IO AudioStream
loadAudioStream path buffers samples =
  withCString path $ \str ->
  mkFP AudioStream (FailedToLoadAudioStream path buffers samples)
      (al_load_audio_stream str (fromIntegral buffers) (fromIntegral samples))
      al_destroy_audio_stream_addr

data FailedToLoadAudioStream = FailedToLoadAudioStream FilePath Word Word
                              deriving (Typeable,Show)

instance Exception FailedToLoadAudioStream






