{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.Audio
  (
  -- * Voices
    createVoice
  , Voice
  , destroyVoice
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
  , destroyMixer
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
  , destroySampleInstance
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
  , destroySample
  , Sample
  , FailedToLoadSample(..)

  -- * Audio Streams
  , loadAudioStream
  , destroyAudioStream
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

import Data.Typeable(Typeable)
import Control.Exception(Exception, throwIO)
import Control.Monad(when, unless)

import Foreign  (Ptr, nullPtr)
import Foreign.C.String(withCString)




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



chk :: Exception x => x -> IO Bool -> IO ()
chk err b =
  do x <- b
     unless x (throwIO err)

chkPtr :: Exception x => x -> IO (Ptr a) -> IO (Ptr a)
chkPtr err b =
  do x <- b
     when (x == nullPtr) (throwIO err)
     return x

mkPtr :: Exception x => (Ptr a -> b) -> x -> IO (Ptr a) -> IO b
mkPtr mk err creat = fmap mk (chkPtr err creat)


newtype Voice = Voice (Ptr VOICE)
                      deriving (Eq,Ord,Show,Typeable)



-- | Throws 'FailedToCreateVoice'
createVoice :: Word {- ^ Frequency -} -> AudioDepth -> ChannelConf -> IO Voice
createVoice freq depth conf =
  mkPtr Voice (FailedToCreateVoice freq depth conf)
      $ al_create_voice (fromIntegral freq) (outAudioDepth depth)
                                            (outChannelConf conf)

data FailedToCreateVoice = FailedToCreateVoice Word AudioDepth ChannelConf
                              deriving (Typeable,Show)

instance Exception FailedToCreateVoice


destroyVoice :: Voice -> IO ()
destroyVoice (Voice v) = al_destroy_voice v



-- | Throws 'FailedToAttachMixerToVoice'
attachMixerToVoice :: Mixer -> Voice -> IO ()
attachMixerToVoice m@(Mixer mp) v@(Voice vp) =
  chk (FailedToAttachMixerToVoice m v) (al_attach_mixer_to_voice mp vp)

data FailedToAttachMixerToVoice = FailedToAttachMixerToVoice Mixer Voice
                              deriving (Typeable,Show)

instance Exception FailedToAttachMixerToVoice



detachVoice :: Voice -> IO ()
detachVoice (Voice vfp) = al_detach_voice vfp

getVoiceFrequency :: Voice -> IO Word
getVoiceFrequency (Voice vfp) = fmap fromIntegral (al_get_voice_frequency vfp)

-- | Throws 'InvalidChannelConf'
getVoiceChannels :: Voice -> IO ChannelConf
getVoiceChannels (Voice vfp) =
  inChannelConf =<< al_get_voice_channels vfp

-- | Throws 'InvalidAudioDepth'
getVoiceDepth :: Voice -> IO AudioDepth
getVoiceDepth (Voice vfp) =
  inAudioDepth =<< al_get_voice_depth vfp

getVoicePlaying :: Voice -> IO Bool
getVoicePlaying (Voice vfp) = al_get_voice_playing vfp

-- | Throws 'FailedToSetVoicePlaying'
setVoicePlaying :: Voice -> Bool -> IO ()
setVoicePlaying v@(Voice p) b =
  chk (FailedToSetVoicePlaying v b) $ al_set_voice_playing p b

data FailedToSetVoicePlaying = FailedToSetVoicePlaying Voice Bool
                              deriving (Typeable,Show)

instance Exception FailedToSetVoicePlaying


getVoicePosition :: Voice -> IO Word
getVoicePosition (Voice vfp) = fmap fromIntegral (al_get_voice_position vfp)

-- | Throws 'FailedToSetVoicePosition'
setVoicePosition :: Voice -> Word -> IO ()
setVoicePosition v@(Voice p) pos =
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

newtype Mixer = Mixer (Ptr MIXER) deriving (Eq,Ord,Typeable,Show)

-- | Throws 'FailedToCreateMixer'
createMixer :: Word {- ^ Frequency -} -> AudioDepth -> ChannelConf -> IO Mixer
createMixer freq depth conf =
  mkPtr Mixer (FailedToCreateMixer freq depth conf)
  $ (al_create_mixer (fromIntegral freq) (outAudioDepth depth)
                                         (outChannelConf conf))


data FailedToCreateMixer = FailedToCreateMixer Word AudioDepth ChannelConf
                           deriving (Typeable,Show)

instance Exception FailedToCreateMixer


destroyMixer :: Mixer -> IO ()
destroyMixer (Mixer m) = al_destroy_mixer m


-- | Throws 'FailedToAttachSampleInstanceToMixer'
attachSampleInstanceToMixer :: SampleInstance -> Mixer -> IO ()
attachSampleInstanceToMixer s@(SampleInstance px) m@(Mixer py) =
  chk (FailedToAttachSampleInstanceToMixer s m)
  $ al_attach_sample_instance_to_mixer px py

data FailedToAttachSampleInstanceToMixer =
  FailedToAttachSampleInstanceToMixer SampleInstance Mixer
                           deriving (Typeable,Show)

instance Exception FailedToAttachSampleInstanceToMixer

-- | Throws 'FailedToAttachAudioStreamToMixer'
attachAudioStreamToMixer :: AudioStream -> Mixer -> IO ()
attachAudioStreamToMixer s@(AudioStream px) m@(Mixer py) =
  chk (FailedToAttachAudioStreamToMixer s m)
  $ al_attach_audio_stream_to_mixer px py

data FailedToAttachAudioStreamToMixer =
  FailedToAttachAudioStreamToMixer AudioStream Mixer
  deriving (Typeable,Show)

instance Exception FailedToAttachAudioStreamToMixer


-- | Throws 'FailedToAttachMixerToMixer'
attachMixerToMixer :: Mixer -> Mixer -> IO ()
attachMixerToMixer s@(Mixer px) m@(Mixer py) =
  chk (FailedToAttachMixerToMixer s m)
  $ al_attach_mixer_to_mixer px py

data FailedToAttachMixerToMixer =
  FailedToAttachMixerToMixer Mixer Mixer
  deriving (Typeable,Show)

instance Exception FailedToAttachMixerToMixer


getMixerAttached :: Mixer -> IO Bool
getMixerAttached (Mixer x) = al_get_mixer_attached x


-- | Throws 'FailedToDetachMixer'
detachMixer :: Mixer -> IO ()
detachMixer m@(Mixer px) = chk (FailedToDetachMixer m) $ al_detach_mixer px

data FailedToDetachMixer =
  FailedToDetachMixer Mixer
  deriving (Typeable,Show)

instance Exception FailedToDetachMixer

getMixerFrequency :: Mixer -> IO Word
getMixerFrequency (Mixer x) = fmap fromIntegral (al_get_mixer_frequency x)

-- | Throws 'FailedToSetMixerFrequncy'
setMixerFrequency :: Mixer -> Word -> IO ()
setMixerFrequency m@(Mixer px) w =
  chk (FailedToSetMixerFrequncy m w)
  $ al_set_mixer_frequency px (fromIntegral w)

data FailedToSetMixerFrequncy =
  FailedToSetMixerFrequncy Mixer Word
  deriving (Typeable,Show)

instance Exception FailedToSetMixerFrequncy

-- | Throws 'InvalidChannelConf'
getMixerChannels :: Mixer -> IO ChannelConf
getMixerChannels (Mixer m) =
  inChannelConf =<< al_get_mixer_channels m

getMixerGain :: Mixer -> IO Float
getMixerGain (Mixer m) = fmap f2f (al_get_mixer_gain m)

-- | Throws 'FailedToSetMixerGain'
setMixerGain :: Mixer -> Float -> IO ()
setMixerGain m@(Mixer p) f =
  chk (FailedToSetMixerGain m f)
  $ al_set_mixer_gain p (f2f f)

data FailedToSetMixerGain =
  FailedToSetMixerGain Mixer Float
  deriving (Typeable,Show)

instance Exception FailedToSetMixerGain


-- | Throws 'InvalidMixerQuality'
getMixerQuality :: Mixer -> IO MixerQuality
getMixerQuality (Mixer x) = inMixerQuality =<< al_get_mixer_quality x

-- | Throws 'FailedToSetMixerQuality'
setMixerQuality :: Mixer -> MixerQuality -> IO ()
setMixerQuality m@(Mixer p) q =
  chk (FailedToSetMixerQuality m q)
  $ al_set_mixer_quality p (outMixerQuality q)

data FailedToSetMixerQuality =
  FailedToSetMixerQuality Mixer MixerQuality
  deriving (Typeable, Show)

instance Exception FailedToSetMixerQuality

getMixerPlaying :: Mixer -> IO Bool
getMixerPlaying (Mixer m) = al_get_mixer_playing m

-- | Throws 'FailedToSetMixerPlaying'
setMixerPlaying :: Mixer -> Bool -> IO ()
setMixerPlaying m@(Mixer q) b =
  chk (FailedToSetMixerPlaying m b)
  $ al_set_mixer_playing q b

data FailedToSetMixerPlaying =
  FailedToSetMixerPlaying Mixer Bool
  deriving (Typeable,Show)

instance Exception FailedToSetMixerPlaying


f2f :: (Real a, Fractional b) => a -> b
f2f x = fromRational (toRational x)

--------------------------------------------------------------------------------

newtype Sample = Sample (Ptr SAMPLE)
                      deriving (Eq,Ord,Show,Typeable)

-- | Throws 'FailedToLoadSample'
loadSample :: FilePath -> IO Sample
loadSample path =
  withCString path $ \str ->
  mkPtr Sample (FailedToLoadSample path)
               (al_load_sample str)

destroySample :: Sample -> IO ()
destroySample (Sample x) = al_destroy_sample x


data FailedToLoadSample = FailedToLoadSample FilePath
                              deriving (Typeable,Show)

instance Exception FailedToLoadSample

--------------------------------------------------------------------------------
-- Sample Instances

newtype SampleInstance = SampleInstance (Ptr SAMPLE_INSTANCE)
                      deriving (Eq,Ord,Show,Typeable)

-- | Throws 'FailedToCreateSampleInstance'
createSampleInstance :: Sample -> IO SampleInstance
createSampleInstance s@(Sample sptr) =
  mkPtr SampleInstance (FailedToCreateSampleInstance s)
                       (al_create_sample_instance sptr)

destroySampleInstance :: SampleInstance -> IO ()
destroySampleInstance (SampleInstance x) = al_destroy_sample_instance x

data FailedToCreateSampleInstance = FailedToCreateSampleInstance Sample
                              deriving (Typeable,Show)

instance Exception FailedToCreateSampleInstance


-- | Throws 'InvalidChannelConf'
getSampleInstanceChannels :: SampleInstance -> IO ChannelConf
getSampleInstanceChannels (SampleInstance x) =
  inChannelConf =<< al_get_sample_instance_channels x

-- | Throws 'InvalidAudioDepth'
getSampleInstanceDepth :: SampleInstance -> IO AudioDepth
getSampleInstanceDepth (SampleInstance x) =
  inAudioDepth =<< al_get_sample_instance_depth x

getSampleInstanceFrequency :: SampleInstance -> IO Word
getSampleInstanceFrequency (SampleInstance x) =
  fmap fromIntegral (al_get_sample_instance_frequency x)

getSampleInstanceLength :: SampleInstance -> IO Word
getSampleInstanceLength (SampleInstance x) =
  fmap fromIntegral (al_get_sample_instance_length x)

-- | Throws 'FailedToSetSampleInstanceLength'
setSampleInstanceLength :: SampleInstance -> Word -> IO ()
setSampleInstanceLength s@(SampleInstance p) w =
  chk (FailedToSetSampleInstanceLength s w)
  $ al_set_sample_instance_length p (fromIntegral w)

data FailedToSetSampleInstanceLength =
  FailedToSetSampleInstanceLength SampleInstance Word
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstanceLength

getSampleInstancePosition :: SampleInstance -> IO Word
getSampleInstancePosition (SampleInstance x) =
  fmap fromIntegral $ al_get_sample_instance_position x

-- | Throws 'FailedToSetSampleInstancePosition'
setSampleInstancePosition :: SampleInstance -> Word -> IO ()
setSampleInstancePosition s@(SampleInstance p) w =
  chk (FailedToSetSampleInstancePosition s w)
  $ al_set_sample_instance_position p (fromIntegral w)

data FailedToSetSampleInstancePosition =
  FailedToSetSampleInstancePosition SampleInstance Word
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstancePosition

getSampleInstanceSpeed :: SampleInstance -> IO Float
getSampleInstanceSpeed (SampleInstance x) =
  fmap f2f (al_get_sample_instance_speed x)

-- | Throws 'FailedToSetSampleInstanceSpeed'
setSampleInstanceSpeed :: SampleInstance -> Float -> IO ()
setSampleInstanceSpeed s@(SampleInstance p) f =
  chk (FailedToSetSampleInstanceSpeed s f)
  $ al_set_sample_instance_speed p (f2f f)

data FailedToSetSampleInstanceSpeed =
  FailedToSetSampleInstanceSpeed SampleInstance Float
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstanceSpeed

getSampleInstanceGain :: SampleInstance -> IO Float
getSampleInstanceGain (SampleInstance x) =
  fmap f2f $ al_get_sample_instance_gain x

-- | Throws 'FailedToSetSampleInstanceGain'
setSampleInstanceGain :: SampleInstance -> Float -> IO ()
setSampleInstanceGain s@(SampleInstance p) f =
  chk (FailedToSetSampleInstanceGain s f)
  $ al_set_sample_instance_gain p (f2f f)

data FailedToSetSampleInstanceGain =
  FailedToSetSampleInstanceGain SampleInstance Float
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstanceGain

getSampleInstancePan :: SampleInstance -> IO Float
getSampleInstancePan (SampleInstance x) =
  fmap f2f $ al_get_sample_instance_pan x

-- | Throws 'FailedToSetSampleInstancePan'
setSampleInstancePan :: SampleInstance -> Maybe Float -> IO ()
setSampleInstancePan s@(SampleInstance p) mb =
  chk (FailedToSetSampleInstancePan s mb)
  $ al_set_sample_instance_pan p (maybe audio_pan_none f2f mb)

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
  inPlayMode =<< al_get_sample_instance_playmode s

-- | Throws 'FailedToSetPlayMode'
setSampleInstancePlayMode :: SampleInstance -> PlayMode -> IO ()
setSampleInstancePlayMode s@(SampleInstance ptr) p =
  chk (FailedToSetPlayMode s p)
  $ al_set_sample_instance_playmode ptr (outPlayMode p)

data FailedToSetPlayMode =
  FailedToSetPlayMode SampleInstance PlayMode
  deriving (Typeable,Show)

instance Exception FailedToSetPlayMode

getSampleInstancePlaying :: SampleInstance -> IO Bool
getSampleInstancePlaying (SampleInstance x) =
  al_get_sample_instance_playing x

-- | Throws 'FailedToSetSampleInstancePlaying'
setSampleInstancePlaying :: SampleInstance -> Bool -> IO ()
setSampleInstancePlaying s@(SampleInstance p) b =
  chk (FailedToSetSampleInstancePlaying s b)
  $ al_set_sample_instance_playing p b

data FailedToSetSampleInstancePlaying =
  FailedToSetSampleInstancePlaying SampleInstance Bool
  deriving (Typeable,Show)

instance Exception FailedToSetSampleInstancePlaying

getSampleInstanceTime :: SampleInstance -> IO Float
getSampleInstanceTime (SampleInstance x) =
  fmap f2f (al_get_sample_instance_time x)

getSampleInstanceAttached :: SampleInstance -> IO Bool
getSampleInstanceAttached (SampleInstance x) = al_get_sample_instance_attached x

-- | Throws 'FailedToDetachSampleInstance'
detachSampleInstance :: SampleInstance -> IO ()
detachSampleInstance s@(SampleInstance x) =
  chk (FailedToDetachSampleInstance s)
  (al_detach_sample_instance x)

data FailedToDetachSampleInstance =
  FailedToDetachSampleInstance SampleInstance
  deriving (Typeable,Show)

instance Exception FailedToDetachSampleInstance



--------------------------------------------------------------------------------
-- Audio Streams





newtype AudioStream = AudioStream (Ptr AUDIO_STREAM)
                      deriving (Eq,Ord,Show,Typeable)

instance Exception AudioStream

-- | Throws 'FailedToLoadAudioStream'
loadAudioStream :: FilePath -> Word {- ^ buffer count -} ->
                   Word {-^ samples -} -> IO AudioStream
loadAudioStream path buffers samples =
  withCString path $ \str ->
  mkPtr AudioStream (FailedToLoadAudioStream path buffers samples)
      (al_load_audio_stream str (fromIntegral buffers) (fromIntegral samples))

destroyAudioStream :: AudioStream -> IO ()
destroyAudioStream (AudioStream p) = al_destroy_audio_stream p

data FailedToLoadAudioStream = FailedToLoadAudioStream FilePath Word Word
                              deriving (Typeable,Show)

instance Exception FailedToLoadAudioStream






