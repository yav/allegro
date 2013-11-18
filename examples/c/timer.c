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

  ALLEGRO_EVENT_QUEUE *event_queue = al_create_event_queue();
  if (event_queue == NULL) {
    fprintf(stderr, "Failed to create an event queue\n");
    al_destroy_display(display);
    return 3;
  }

  ALLEGRO_TIMER *timer;
  timer = al_create_timer(1);
  if (timer == NULL) {
    fprintf(stderr, "Failed to create an event queue\n");
    al_destroy_display(display);
    al_destroy_event_queue(event_queue);
    return 4;
  }

  al_register_event_source(event_queue, al_get_display_event_source(display));
  al_register_event_source(event_queue, al_get_timer_event_source(timer));

  al_clear_to_color(al_map_rgb(255,0,0));
  al_flip_display();
  al_start_timer(timer);

  while (true) {
    ALLEGRO_EVENT ev;
    al_wait_for_event(event_queue, &ev);
    printf ("Event type: %d\n", ev.type);
  }

  al_destroy_timer(timer);
  al_destroy_display(display);
  al_destroy_event_queue(event_queue);

  return 0;
}

