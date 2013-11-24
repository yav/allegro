#include <allegro5/allegro.h>
#include <allegro5/allegro_font.h>
#include <allegro5/allegro_ttf.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  int res = 0;

  if (!al_init()) {
    fprintf(stderr, "Failed to initialize Allegro\n");
    res = 1;
    goto X0;
  }

  al_init_font_addon();

  if (!al_init_ttf_addon()) {
    fprintf(stderr, "Failed to initialize the TTF addong\n");
    res = 3;
    goto X1;
  }

  const char *font_name = "resources/font.ttf";
  ALLEGRO_FONT *font = al_load_ttf_font(font_name, 12, 0);
  if (font == NULL) {
    fprintf(stderr, "Failed to load font %s\n", font_name);
    res = 4;
    goto X2;
  }

  ALLEGRO_DISPLAY *display = al_create_display(640,480);
  if (display == NULL) {
    fprintf(stderr, "Failed to creat display\n");
    res = 5;
    goto X3;
  }

  al_draw_text(font, al_map_rgb(255,255,255), 0, 0, 0, "Hello Явор");
  al_flip_display();
  al_rest(3);

    al_destroy_display(display);
X3: al_destroy_font(font);
X2: al_shutdown_ttf_addon();
X1: al_shutdown_font_addon();
X0: return res;
}
