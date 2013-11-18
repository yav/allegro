#include <allegro5/allegro.h>

void hsal_clear_to_color(float r, float g, float b, float a) {
  al_clear_to_color(al_map_rgba_f(r,g,b,a));
}
