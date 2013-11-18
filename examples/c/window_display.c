#include <stdio.h>
#include <allegro5/allegro.h>

int main (int argc, char *argv[]) {


  if (!al_init()) {
    fprintf(stderr, "Failed to initialize Allegro\n");
    return 1;
  }

  ALLEGRO_DISPLAY *display = al_create_display(640,480);
  if (display == NULL) {
    fprintf(stderr, "Failed to create a display\n");
    return 2;
  }

  al_clear_to_color(al_map_rgb(255,0,0));
  al_flip_display();
  al_rest(3.0);
  al_destroy_display(display);

  return 0;
}

