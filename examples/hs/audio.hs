import Allegro
import Allegro.Audio
import Allegro.C.Audio

import Control.Concurrent

main :: IO ()
main =
  allegro $
  do v <- createVoice 44000 Int16 ChannelConf_2
     m <- createMixer 44000 Int16 ChannelConf_2
     s <- loadAudioStream "../resources/song.wav" 2 1000
     attachAudioStreamToMixer s m
     attachMixerToVoice m v
     threadDelay (3 * 1000 * 1000)


