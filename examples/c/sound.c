#include <allegro5/allegro.h>
#include <allegro5/allegro_audio.h>
#include <allegro5/allegro_acodec.h>
#include <stdio.h>
#include <math.h>


int main(int argc, char *argv[]) {
  int res = 0;

  if (!al_init()) {
    fprintf(stderr, "Failed to initialize Allegro.\n");
    res = 1;
    goto X0;
  }

  if (!al_install_audio()) {
    fprintf(stderr, "Failed to install audio.\n");
    res = 2;
    goto X0;
  }

  if (!al_init_acodec_addon()) {
    fprintf(stderr, "Failed to install audio codecs addon.\n");
    res = 3;
    goto X1;
  }

  int sample_num = 2;
  if (!al_reserve_samples(sample_num)) {
    fprintf(stderr, "Failed to reserve %d samples.\n", sample_num);
    res = 4;
    goto X1;
  }

  const char* sample_name = "resources/sound.wav";
  ALLEGRO_SAMPLE *sample = al_load_sample(sample_name);
  if (sample == NULL) {
    fprintf(stderr, "Failed to load sample from %s.\n", sample_name);
    res = 5;
    goto X1;
  }

  unsigned int len = al_get_sample_length(sample);
  unsigned int freq = al_get_sample_frequency(sample);
  float time = (float)len / freq;
  printf ("length = %u, frequency = %u, time = %.2fs\n", len, freq, time);

  if (! al_play_sample(sample, 1, 0, 1, ALLEGRO_PLAYMODE_ONCE, NULL)) {
    fprintf(stderr, "Failed to play sample 1.\n");
    res = 6;
    goto X2;
  }

  al_rest(time/4);

  if (! al_play_sample(sample, 1, 0, 1, ALLEGRO_PLAYMODE_ONCE, NULL)) {
    fprintf(stderr, "Failed to play sample 2.\n");
    res = 7;
    goto X2;
  }

  al_rest(time);

X2: al_destroy_sample(sample);
X1: al_uninstall_audio();
X0: return res;
}

