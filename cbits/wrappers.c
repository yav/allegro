#include <allegro5/allegro.h>
#include <allegro5/allegro_font.h>

void hsal_clear_to_color(float r, float g, float b, float a) {
  al_clear_to_color(al_map_rgba_f(r,g,b,a));
}

void shal_draw_text( ALLEGRO_FONT *f
                 , float r, float g, float b, float a
                 , float x, float y
                 , int flags, const char *text ) {
  al_draw_text(f, al_map_rgba_f(r,g,b,a), x, y, flags, text);
}

void shal_draw_justified_text ( ALLEGRO_FONT *f
                 , float r, float g, float b, float a
                 , float x1, float x2, float y, float d
                 , int flags, const char *text ) {
  al_draw_justified_text(f, al_map_rgba_f(r,g,b,a), x1, x2, y, d,flags, text);
}
