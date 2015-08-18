{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Allegro.C.Audio where

import Foreign(FunPtr,Ptr)
import Foreign.C.Types
import Foreign.C.String (CString)

import Allegro.C.Types

#include <allegro5/allegro.h>
#include <allegro5/allegro_audio.h>
#include <allegro5/allegro_acodec.h>

foreign import ccall unsafe "al_install_audio"
  al_install_audio :: IO Bool

foreign import ccall unsafe "al_is_audio_installed"
  al_is_audio_installed :: IO Bool

foreign import ccall unsafe "al_uninstall_audio"
  al_uninstall_audio :: IO ()

foreign import ccall unsafe "al_init_acodec_addon"
  al_init_acodec_addon :: IO Bool


--------------------------------------------------------------------------------
-- Voices

foreign import ccall unsafe "al_create_voice"
  al_create_voice :: CUInt -> AUDIO_DEPTH -> CHANNEL_CONF -> IO (Ptr VOICE)

foreign import ccall unsafe "al_destroy_voice"
  al_destroy_voice :: Ptr VOICE -> IO ()

foreign import ccall unsafe "&al_destroy_voice"
  al_destroy_voice_addr :: FunPtr (Ptr VOICE -> IO ())

foreign import ccall unsafe "al_detach_voice"
  al_detach_voice :: Ptr VOICE -> IO ()

foreign import ccall unsafe "al_attach_mixer_to_voice"
  al_attach_mixer_to_voice :: Ptr MIXER -> Ptr VOICE -> IO Bool

foreign import ccall unsafe "al_get_voice_frequency"
  al_get_voice_frequency :: Ptr VOICE -> IO CUInt

foreign import ccall unsafe "al_get_voice_channels"
  al_get_voice_channels :: Ptr VOICE -> IO CHANNEL_CONF

foreign import ccall unsafe "al_get_voice_depth"
  al_get_voice_depth :: Ptr VOICE -> IO AUDIO_DEPTH

foreign import ccall unsafe "al_get_voice_playing"
  al_get_voice_playing:: Ptr VOICE -> IO Bool

foreign import ccall unsafe "al_set_voice_playing"
  al_set_voice_playing:: Ptr VOICE -> Bool -> IO Bool

foreign import ccall unsafe "al_get_voice_position"
  al_get_voice_position :: Ptr VOICE -> IO CUInt

foreign import ccall unsafe "al_set_voice_position"
  al_set_voice_position :: Ptr VOICE -> CUInt -> IO Bool


--------------------------------------------------------------------------------
-- Samples

foreign import ccall unsafe "al_load_sample"
  al_load_sample :: CString -> IO (Ptr SAMPLE)

foreign import ccall unsafe "al_destroy_sample"
  al_destroy_sample :: Ptr SAMPLE -> IO ()

foreign import ccall unsafe "&al_destroy_sample"
  al_destroy_sample_adr :: FunPtr (Ptr SAMPLE -> IO ())

foreign import ccall unsafe "al_get_sample_channels"
  al_get_sample_channels :: Ptr SAMPLE -> IO CHANNEL_CONF

foreign import ccall unsafe "al_get_sample_depth"
  al_get_sample_depth :: Ptr SAMPLE -> IO AUDIO_DEPTH

foreign import ccall unsafe "al_get_sample_frequency"
  al_get_sample_frequency :: Ptr SAMPLE -> IO CUInt

foreign import ccall unsafe "al_get_sample_length"
  al_get_sample_length :: Ptr SAMPLE -> IO CUInt

--------------------------------------------------------------------------------
-- Sample Instances

foreign import ccall unsafe "al_create_sample_instance"
  al_create_sample_instance :: Ptr SAMPLE -> IO (Ptr SAMPLE_INSTANCE)

foreign import ccall unsafe "al_destroy_sample_instance"
  al_destroy_sample_instance :: Ptr SAMPLE_INSTANCE -> IO ()

foreign import ccall unsafe "&al_destroy_sample_instance"
  al_destroy_sample_instance_addr :: FunPtr (Ptr SAMPLE_INSTANCE -> IO ())

foreign import ccall unsafe "al_play_sample_instance"
  al_play_sample_instance :: Ptr SAMPLE_INSTANCE -> IO Bool

foreign import ccall unsafe "al_stop_sample_instance"
  al_stop_sample_instance :: Ptr SAMPLE_INSTANCE -> IO Bool

foreign import ccall unsafe "al_get_sample_instance_channels"
  al_get_sample_instance_channels :: Ptr SAMPLE_INSTANCE -> IO CHANNEL_CONF

foreign import ccall unsafe "al_get_sample_instance_depth"
  al_get_sample_instance_depth :: Ptr SAMPLE_INSTANCE -> IO AUDIO_DEPTH

foreign import ccall unsafe "al_get_sample_instance_frequency"
  al_get_sample_instance_frequency :: Ptr SAMPLE_INSTANCE -> IO CUInt

foreign import ccall unsafe "al_get_sample_instance_length"
  al_get_sample_instance_length :: Ptr SAMPLE_INSTANCE -> IO CUInt

foreign import ccall unsafe "al_set_sample_instance_length"
  al_set_sample_instance_length :: Ptr SAMPLE_INSTANCE -> CUInt -> IO Bool

foreign import ccall unsafe "al_get_sample_instance_position"
  al_get_sample_instance_position :: Ptr SAMPLE_INSTANCE -> IO CUInt

foreign import ccall unsafe "al_set_sample_instance_position"
  al_set_sample_instance_position :: Ptr SAMPLE_INSTANCE -> CUInt -> IO Bool

foreign import ccall unsafe "al_get_sample_instance_speed"
  al_get_sample_instance_speed :: Ptr SAMPLE_INSTANCE -> IO CFloat

foreign import ccall unsafe "al_set_sample_instance_speed"
  al_set_sample_instance_speed :: Ptr SAMPLE_INSTANCE -> CFloat -> IO Bool

foreign import ccall unsafe "al_get_sample_instance_gain"
  al_get_sample_instance_gain :: Ptr SAMPLE_INSTANCE -> IO CFloat

foreign import ccall unsafe "al_set_sample_instance_gain"
  al_set_sample_instance_gain :: Ptr SAMPLE_INSTANCE -> CFloat -> IO Bool

foreign import ccall unsafe "al_get_sample_instance_pan"
  al_get_sample_instance_pan :: Ptr SAMPLE_INSTANCE -> IO CFloat

foreign import ccall unsafe "al_set_sample_instance_pan"
  al_set_sample_instance_pan :: Ptr SAMPLE_INSTANCE -> CFloat -> IO Bool

foreign import ccall unsafe "al_get_sample_instance_time"
  al_get_sample_instance_time :: Ptr SAMPLE_INSTANCE -> IO CFloat

foreign import ccall unsafe "al_get_sample_instance_playmode"
  al_get_sample_instance_playmode :: Ptr SAMPLE_INSTANCE -> IO PLAYMODE

foreign import ccall unsafe "al_set_sample_instance_playmode"
  al_set_sample_instance_playmode :: Ptr SAMPLE_INSTANCE -> PLAYMODE -> IO Bool

foreign import ccall unsafe "al_get_sample_instance_playing"
  al_get_sample_instance_playing :: Ptr SAMPLE_INSTANCE -> IO Bool

foreign import ccall unsafe "al_set_sample_instance_playing"
  al_set_sample_instance_playing :: Ptr SAMPLE_INSTANCE -> Bool -> IO Bool

foreign import ccall unsafe "al_get_sample_instance_attached"
  al_get_sample_instance_attached :: Ptr SAMPLE_INSTANCE -> IO Bool

foreign import ccall unsafe "al_detach_sample_instance"
  al_detach_sample_instance :: Ptr SAMPLE_INSTANCE -> IO Bool



--------------------------------------------------------------------------------
-- Mixers

foreign import ccall unsafe "al_create_mixer"
  al_create_mixer :: CUInt -> AUDIO_DEPTH -> CHANNEL_CONF -> IO (Ptr MIXER)

foreign import ccall unsafe "al_destroy_mixer"
  al_destroy_mixer :: Ptr MIXER -> IO ()

foreign import ccall unsafe "&al_destroy_mixer"
  al_destroy_mixer_addr :: FunPtr (Ptr MIXER -> IO ())

foreign import ccall unsafe "al_attach_sample_instance_to_mixer"
  al_attach_sample_instance_to_mixer
    :: Ptr SAMPLE_INSTANCE -> Ptr MIXER -> IO Bool

foreign import ccall unsafe "al_attach_audio_stream_to_mixer"
  al_attach_audio_stream_to_mixer
    :: Ptr AUDIO_STREAM -> Ptr MIXER -> IO Bool

foreign import ccall unsafe "al_attach_mixer_to_mixer"
  al_attach_mixer_to_mixer
    :: Ptr MIXER -> Ptr MIXER -> IO Bool

foreign import ccall unsafe "al_get_mixer_attached"
  al_get_mixer_attached :: Ptr MIXER -> IO Bool

foreign import ccall unsafe "al_detach_mixer"
  al_detach_mixer :: Ptr MIXER -> IO Bool

foreign import ccall unsafe "al_get_mixer_frequency"
  al_get_mixer_frequency :: Ptr MIXER -> IO CUInt

foreign import ccall unsafe "al_set_mixer_frequency"
  al_set_mixer_frequency :: Ptr MIXER -> CUInt -> IO Bool

foreign import ccall unsafe "al_get_mixer_channels"
  al_get_mixer_channels :: Ptr MIXER -> IO CHANNEL_CONF

foreign import ccall unsafe "al_get_mixer_gain"
  al_get_mixer_gain :: Ptr MIXER -> IO CFloat

foreign import ccall unsafe "al_set_mixer_gain"
  al_set_mixer_gain :: Ptr MIXER -> CFloat -> IO Bool

foreign import ccall unsafe "al_get_mixer_quality"
  al_get_mixer_quality :: Ptr MIXER -> IO MIXER_QUALITY

foreign import ccall unsafe "al_set_mixer_quality"
  al_set_mixer_quality :: Ptr MIXER -> MIXER_QUALITY -> IO Bool

foreign import ccall unsafe "al_get_mixer_playing"
  al_get_mixer_playing :: Ptr MIXER -> IO Bool

foreign import ccall unsafe "al_set_mixer_playing"
  al_set_mixer_playing :: Ptr MIXER -> Bool -> IO Bool



--------------------------------------------------------------------------------
-- Audio streams

foreign import ccall unsafe "al_load_audio_stream"
  al_load_audio_stream :: CString -> CSize -> CUInt -> IO (Ptr AUDIO_STREAM)

foreign import ccall unsafe "al_destroy_audio_stream"
  al_destroy_audio_stream :: Ptr AUDIO_STREAM -> IO ()


--------------------------------------------------------------------------------

type AUDIO_DEPTH = CInt

audio_depth_int8
  , audio_depth_int16
  , audio_depth_int24
  , audio_depth_float32
  , audio_depth_uint8
  , audio_depth_uint16
  , audio_depth_uint24
  :: AUDIO_DEPTH

audio_depth_int8      = #{const ALLEGRO_AUDIO_DEPTH_INT8}
audio_depth_int16     = #{const ALLEGRO_AUDIO_DEPTH_INT16}
audio_depth_int24     = #{const ALLEGRO_AUDIO_DEPTH_INT24}
audio_depth_float32   = #{const ALLEGRO_AUDIO_DEPTH_FLOAT32}
audio_depth_uint8     = #{const ALLEGRO_AUDIO_DEPTH_UINT8}
audio_depth_uint16    = #{const ALLEGRO_AUDIO_DEPTH_UINT16}
audio_depth_uint24    = #{const ALLEGRO_AUDIO_DEPTH_UINT24}


audio_pan_none :: CFloat
audio_pan_none = #{const ALLEGRO_AUDIO_PAN_NONE}


type CHANNEL_CONF = CInt

channel_conf_1
  , channel_conf_2
  , channel_conf_3
  , channel_conf_4
  , channel_conf_5_1
  , channel_conf_6_1
  , channel_conf_7_1
  :: CHANNEL_CONF

channel_conf_1     = #{const ALLEGRO_CHANNEL_CONF_1}
channel_conf_2     = #{const ALLEGRO_CHANNEL_CONF_2}
channel_conf_3     = #{const ALLEGRO_CHANNEL_CONF_3}
channel_conf_4     = #{const ALLEGRO_CHANNEL_CONF_4}
channel_conf_5_1   = #{const ALLEGRO_CHANNEL_CONF_5_1}
channel_conf_6_1   = #{const ALLEGRO_CHANNEL_CONF_6_1}
channel_conf_7_1   = #{const ALLEGRO_CHANNEL_CONF_7_1}


type MIXER_QUALITY = CInt

mixer_quality_point
  , mixer_quality_linear
  , mixer_quality_cubic
  :: MIXER_QUALITY

mixer_quality_point  = #{const ALLEGRO_MIXER_QUALITY_POINT}
mixer_quality_linear = #{const ALLEGRO_MIXER_QUALITY_LINEAR}
mixer_quality_cubic  = #{const ALLEGRO_MIXER_QUALITY_CUBIC}

type PLAYMODE = CInt

playmode_once
  , playmode_loop
  , playmode_bidir
  :: PLAYMODE

playmode_once  = #{const ALLEGRO_PLAYMODE_ONCE}
playmode_loop  = #{const ALLEGRO_PLAYMODE_LOOP}
playmode_bidir = #{const ALLEGRO_PLAYMODE_BIDIR}

