#include <allegro5/allegro.h>
#include <allegro5/allegro_font.h>

void shal_uninstall_keyboard(void *p) {
  al_uninstall_keyboard();
}

void shal_uninstall_mouse(void *p) {
  al_uninstall_mouse();
}

void shal_clear_to_color(float r, float g, float b, float a) {
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



void shal_draw_tinted_bitmap
  (ALLEGRO_BITMAP *p,
  float r,  float g,  float b, float a,
  float dx, float dy,
  int flags)
  { al_draw_tinted_bitmap (p, al_map_rgba_f(r,   g,   b,  a), dx,  dy, flags); }



void shal_draw_tinted_scaled_bitmap
  (ALLEGRO_BITMAP *p,
  float r,  float g,  float b, float a,
  float sx, float sy, float sw, float sh,
  float dx, float dy, float dw, float dh,
  int flags)
  { al_draw_tinted_scaled_bitmap (p, al_map_rgba_f(r,   g,   b,  a),
      sx,  sy,  sw,  sh, dx,  dy,  dw,  dh, flags); }

void shal_draw_tinted_scaled_rotated_bitmap(ALLEGRO_BITMAP *bitmap,
   float r, float g, float b, float a,
   float cx, float cy, float dx, float dy, float xscale, float yscale,
   float angle, int flags)
   { al_draw_tinted_scaled_rotated_bitmap(bitmap,
    al_map_rgba_f(r,  g,  b,  a),
    cx,  cy,  dx,  dy,  xscale,  yscale,
    angle, flags);
   }


void shal_draw_tinted_bitmap_region
  (ALLEGRO_BITMAP *p,
  float r,  float g,  float b, float a,
  float sx, float sy, float sw, float sh,
  float dx, float dy,
  int flags)
  { al_draw_tinted_bitmap_region (p, al_map_rgba_f(r,   g,   b,  a),
       sx,  sy,  sw,  sh, dx,  dy, flags); }



void shal_draw_tinted_rotated_bitmap
  (ALLEGRO_BITMAP *p,
  float r,  float g,  float b, float a,
  float cx, float cy,
  float dx, float dy,
  float angle,
  int flags)
  {  al_draw_tinted_rotated_bitmap (p, al_map_rgba_f(r,   g,   b,  a),
      cx,  cy, dx,  dy, angle, flags); }

void shal_draw_tinted_scaled_rotated_bitmap_region
  (ALLEGRO_BITMAP *p,
  float sx, float sy, float sw, float sh,
  float r,  float g,  float b, float a,
  float cx, float cy,
  float dx, float dy,
  float xscale, float yscale,
  float angle,
  int flags)
  { al_draw_tinted_scaled_rotated_bitmap_region
      (p, sx,  sy,  sw,  sh, al_map_rgba_f(r,   g,   b,  a),
        cx,  cy, dx,  dy, xscale,  yscale, angle, flags); }





