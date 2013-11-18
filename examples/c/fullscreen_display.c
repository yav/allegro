#include <stdio.h>
#include <allegro5/allegro.h>

int main (int argc, char *argv[]) {

  if (!al_init()) {
    fprintf(stderr, "Failed to initialize Allegro\n");
    return 1;
  }

  int mode_num = al_get_num_display_modes();
  if (mode_num < 1) {
    fprintf(stderr, "Could not find any display modes.\n");
    return 2;
  }

  ALLEGRO_DISPLAY_MODE mode;
  if (al_get_display_mode(0, &mode) == NULL) {
    printf("Failed to get information about display mode 0\n");
    return 2;
  }

  al_set_new_display_flags(ALLEGRO_FULLSCREEN);

  ALLEGRO_DISPLAY *display = al_create_display(mode.width, mode.height);
  al_clear_to_color(al_map_rgb(255,0,0));
  al_flip_display();
  al_rest(3.0);
  al_destroy_display(display);

  return 0;
}

