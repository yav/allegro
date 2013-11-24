#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>
#include <stdio.h>


int main(int argc, char *argv[]) {

  if (!al_init()) {
    fprintf(stderr, "Failed to initialize Allegro\n");
    return 1;
  }

  ALLEGRO_DISPLAY *display = al_create_display(640,480);
  if (display == NULL) {
    fprintf(stderr, "Failed to create a display\n");
    return 2;
  }

  if (!al_init_image_addon()) {
    fprintf(stderr, "Failed to initialize image add-on\n");
    al_destroy_display(display);
    return 3;
  }

  ALLEGRO_BITMAP *img = al_load_bitmap("example.jpg");
  if (img == NULL) {
    fprintf(stderr, "Failed to load image.\n");
    al_shutdown_image_addon();
    al_destroy_display(display);
    return 4;
  }

  al_draw_tinted_bitmap(img, al_map_rgba_f(0.5, 0.5, 0.5, 0.5), 0, 0, 0);
  al_flip_display();
  al_rest(3);

  al_shutdown_image_addon();
  al_destroy_display(display);
  return 0;
}
