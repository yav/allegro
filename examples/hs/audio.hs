import Allegro
import Allegro.Audio
import Allegro.C.Audio

import Control.Concurrent

main :: IO ()
main =
  allegro $ withAudioAndCodecs $
  do _ <- createVoice 44000 Int16 ChannelConf_2
     -- m <- createMixer 44000 Int16 ChannelConf_2
     return () {-
     s <- loadAudioStream "../resources/song.wav1" 2 1000
     print "here"
     -- attachAudioStreamToMixer s m
     attachMixerToVoice m v
     threadDelay (3 * 1000 * 1000) -}
