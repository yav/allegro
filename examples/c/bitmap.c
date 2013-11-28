#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>
#include <stdio.h>

int main(int argc, char *argv[]) {

  if (!al_init()) {
    fprintf(stderr, "Failed to initialize Allegro\n");
    return 1;
  }

  if (!al_init_image_addon()) {
    fprintf(stderr, "Failed to initialize image add-on\n");
    return 3;
  }

  const char* img_name = "../resources/example.jpg";
  ALLEGRO_BITMAP *img = al_load_bitmap(img_name);
  if (img == NULL) {
    fprintf(stderr, "Failed to load image %s.\n", img_name);
    al_shutdown_image_addon();
    return 4;
  }

  int w = al_get_bitmap_width(img);
  int h = al_get_bitmap_height(img);
  printf("width = %d, height = %d\n", w, h);

  ALLEGRO_DISPLAY *display = al_create_display(w,h);
  if (display == NULL) {
    fprintf(stderr, "Failed to create a display\n");
    return 2;
  }

  al_draw_bitmap(img, 0,0,0);
  al_flip_display();
  al_rest(3);

  al_destroy_bitmap(img);
  al_destroy_display(display);
  al_shutdown_image_addon();
  return 0;
}
