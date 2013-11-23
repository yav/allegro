#include <stdio.h>
#include <allegro5/allegro.h>

int main (int argc, char *argv[]) {

  int res = 0;

  if (!al_init()) {
    fprintf(stderr, "Failed to initialize Allegro\n");
    return 1;
  }

  ALLEGRO_EVENT_QUEUE *q = al_create_event_queue();
  if (q == NULL) {
    fprintf(stderr, "Failed to create an event queue\n");
    return 3;
  }

  if (!al_install_keyboard()) {
    fprintf(stderr, "Failed to install keyboard\n");
    res = 2;
    goto END_Q;
  }

  ALLEGRO_DISPLAY *d = al_create_display(640,480);
  if (d == NULL) {
    fprintf(stderr, "Failed to create a display\n");
    res = 3;
    goto END_KB;
  }

  al_register_event_source(q, al_get_keyboard_event_source());

  while (true) {
    ALLEGRO_EVENT ev;
    al_wait_for_event(q, &ev);
    printf ("Event type: %d\n", ev.type);
  }

  al_destroy_display(d);

END_KB:
  al_uninstall_keyboard();

END_Q:
  al_destroy_event_queue(q);

  return res;
}

