#include <allegro5/allegro5.h>
#include <allegro5/allegro_font.h>
#include <allegro5/allegro_ttf.h>
#include <allegro5/allegro_primitives.h>

void shal_draw_arc(float cx,
                   float cy,
                   float r,
                   float start_theta,
                   float delta_theta,
                   float color_r,
                   float color_g,
                   float color_b,
                   float color_a,
                   float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_arc(cx,
                       cy,
                       r,
                       start_theta,
                       delta_theta,
                       color,
                       thickness);
}
void shal_draw_filled_rounded_rectangle(float x1,
                                        float y1,
                                        float x2,
                                        float y2,
                                        float rx,
                                        float ry,
                                        float color_r,
                                        float color_g,
                                        float color_b,
                                        float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_filled_rounded_rectangle(x1,
                                            y1,
                                            x2,
                                            y2,
                                            rx,
                                            ry,
                                            color);
}
void shal_draw_ellipse(float cx,
                       float cy,
                       float rx,
                       float ry,
                       float color_r,
                       float color_g,
                       float color_b,
                       float color_a,
                       float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_ellipse(cx, cy, rx, ry, color, thickness);
}
void shal_draw_spline(float points[8],
                      float color_r,
                      float color_g,
                      float color_b,
                      float color_a,
                      float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_spline(points, color, thickness);
}
void shal_draw_line(float x1,
                    float y1,
                    float x2,
                    float y2,
                    float color_r,
                    float color_g,
                    float color_b,
                    float color_a,
                    float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_line(x1, y1, x2, y2, color, thickness);
}
void shal_draw_circle(float cx,
                      float cy,
                      float r,
                      float color_r,
                      float color_g,
                      float color_b,
                      float color_a,
                      float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_circle(cx, cy, r, color, thickness);
}
void shal_draw_ribbon(const float * points,
                      int points_stride,
                      float color_r,
                      float color_g,
                      float color_b,
                      float color_a,
                      float thickness,
                      int num_segments)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_ribbon(points,
                          points_stride,
                          color,
                          thickness,
                          num_segments);
}
void shal_draw_rectangle(float x1,
                         float y1,
                         float x2,
                         float y2,
                         float color_r,
                         float color_g,
                         float color_b,
                         float color_a,
                         float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_rectangle(x1, y1, x2, y2, color, thickness);
}
void shal_draw_pieslice(float cx,
                        float cy,
                        float r,
                        float start_theta,
                        float delta_theta,
                        float color_r,
                        float color_g,
                        float color_b,
                        float color_a,
                        float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_pieslice(cx,
                            cy,
                            r,
                            start_theta,
                            delta_theta,
                            color,
                            thickness);
}
void shal_draw_filled_triangle(float x1,
                               float y1,
                               float x2,
                               float y2,
                               float x3,
                               float y3,
                               float color_r,
                               float color_g,
                               float color_b,
                               float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_filled_triangle(x1, y1, x2, y2, x3, y3, color);
}
void shal_draw_filled_pieslice(float cx,
                               float cy,
                               float r,
                               float start_theta,
                               float delta_theta,
                               float color_r,
                               float color_g,
                               float color_b,
                               float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_filled_pieslice(cx,
                                   cy,
                                   r,
                                   start_theta,
                                   delta_theta,
                                   color);
}
void shal_draw_triangle(float x1,
                        float y1,
                        float x2,
                        float y2,
                        float x3,
                        float y3,
                        float color_r,
                        float color_g,
                        float color_b,
                        float color_a,
                        float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_triangle(x1, y1, x2, y2, x3, y3, color, thickness);
}
void shal_draw_filled_ellipse(float cx,
                              float cy,
                              float rx,
                              float ry,
                              float color_r,
                              float color_g,
                              float color_b,
                              float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_filled_ellipse(cx, cy, rx, ry, color);
}
void shal_draw_filled_circle(float cx,
                             float cy,
                             float r,
                             float color_r,
                             float color_g,
                             float color_b,
                             float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_filled_circle(cx, cy, r, color);
}
void shal_draw_rounded_rectangle(float x1,
                                 float y1,
                                 float x2,
                                 float y2,
                                 float rx,
                                 float ry,
                                 float color_r,
                                 float color_g,
                                 float color_b,
                                 float color_a,
                                 float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_rounded_rectangle(x1,
                                     y1,
                                     x2,
                                     y2,
                                     rx,
                                     ry,
                                     color,
                                     thickness);
}
void shal_draw_elliptical_arc(float cx,
                              float cy,
                              float rx,
                              float ry,
                              float start_theta,
                              float delta_theta,
                              float color_r,
                              float color_g,
                              float color_b,
                              float color_a,
                              float thickness)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_elliptical_arc(cx,
                                  cy,
                                  rx,
                                  ry,
                                  start_theta,
                                  delta_theta,
                                  color,
                                  thickness);
}
void shal_draw_filled_rectangle(float x1,
                                float y1,
                                float x2,
                                float y2,
                                float color_r,
                                float color_g,
                                float color_b,
                                float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_filled_rectangle(x1, y1, x2, y2, color);
}
void shal_draw_text(ALLEGRO_FONT const * font,
                    float color_r,
                    float color_g,
                    float color_b,
                    float color_a,
                    float x,
                    float y,
                    int flags,
                    const char * text)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_text(font, color, x, y, flags, text);
}
void shal_draw_ustr(ALLEGRO_FONT const * font,
                    float color_r,
                    float color_g,
                    float color_b,
                    float color_a,
                    float x,
                    float y,
                    int flags,
                    ALLEGRO_USTR const * ustr)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_ustr(font, color, x, y, flags, ustr);
}
void shal_draw_justified_ustr(ALLEGRO_FONT const * font,
                              float color_r,
                              float color_g,
                              float color_b,
                              float color_a,
                              float x1,
                              float x2,
                              float y,
                              float diff,
                              int flags,
                              ALLEGRO_USTR const * text)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_justified_ustr(font,
                                  color,
                                  x1,
                                  x2,
                                  y,
                                  diff,
                                  flags,
                                  text);
}
void shal_draw_justified_text(ALLEGRO_FONT const * font,
                              float color_r,
                              float color_g,
                              float color_b,
                              float color_a,
                              float x1,
                              float x2,
                              float y,
                              float diff,
                              int flags,
                              const char * text)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_justified_text(font,
                                  color,
                                  x1,
                                  x2,
                                  y,
                                  diff,
                                  flags,
                                  text);
}
void shal_map_rgba(unsigned char r,
                   unsigned char g,
                   unsigned char b,
                   unsigned char a,
                   float * out_r,
                   float * out_g,
                   float * out_b,
                   float * out_a)
{
    ALLEGRO_COLOR out;
    out = al_map_rgba(r, g, b, a);
    *out_r = out.r;
    *out_g = out.g;
    *out_b = out.b;
    *out_a = out.a;
}
void shal_map_rgb(unsigned char r,
                  unsigned char g,
                  unsigned char b,
                  float * out_r,
                  float * out_g,
                  float * out_b,
                  float * out_a)
{
    ALLEGRO_COLOR out;
    out = al_map_rgb(r, g, b);
    *out_r = out.r;
    *out_g = out.g;
    *out_b = out.b;
    *out_a = out.a;
}
void shal_convert_mask_to_alpha(ALLEGRO_BITMAP * bitmap,
                                float mask_color_r,
                                float mask_color_g,
                                float mask_color_b,
                                float mask_color_a)
{
    ALLEGRO_COLOR mask_color;
    mask_color.r = mask_color_r;
    mask_color.g = mask_color_g;
    mask_color.b = mask_color_b;
    mask_color.a = mask_color_a;
    return al_convert_mask_to_alpha(bitmap, mask_color);
}
void shal_draw_tinted_bitmap_region(ALLEGRO_BITMAP * bitmap,
                                    float tint_r,
                                    float tint_g,
                                    float tint_b,
                                    float tint_a,
                                    float sx,
                                    float sy,
                                    float sw,
                                    float sh,
                                    float dx,
                                    float dy,
                                    int flags)
{
    ALLEGRO_COLOR tint;
    tint.r = tint_r;
    tint.g = tint_g;
    tint.b = tint_b;
    tint.a = tint_a;
    return al_draw_tinted_bitmap_region(bitmap,
                                        tint,
                                        sx,
                                        sy,
                                        sw,
                                        sh,
                                        dx,
                                        dy,
                                        flags);
}
void shal_put_pixel(int x,
                    int y,
                    float color_r,
                    float color_g,
                    float color_b,
                    float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_put_pixel(x, y, color);
}
void shal_unmap_rgba_f(float color_r,
                       float color_g,
                       float color_b,
                       float color_a,
                       float * r,
                       float * g,
                       float * b,
                       float * a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_unmap_rgba_f(color, r, g, b, a);
}
void shal_unmap_rgb_f(float color_r,
                      float color_g,
                      float color_b,
                      float color_a,
                      float * r,
                      float * g,
                      float * b)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_unmap_rgb_f(color, r, g, b);
}
void shal_unmap_rgba(float color_r,
                     float color_g,
                     float color_b,
                     float color_a,
                     unsigned char * r,
                     unsigned char * g,
                     unsigned char * b,
                     unsigned char * a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_unmap_rgba(color, r, g, b, a);
}
void shal_unmap_rgb(float color_r,
                    float color_g,
                    float color_b,
                    float color_a,
                    unsigned char * r,
                    unsigned char * g,
                    unsigned char * b)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_unmap_rgb(color, r, g, b);
}
void shal_map_rgb_f(float r,
                    float g,
                    float b,
                    float * out_r,
                    float * out_g,
                    float * out_b,
                    float * out_a)
{
    ALLEGRO_COLOR out;
    out = al_map_rgb_f(r, g, b);
    *out_r = out.r;
    *out_g = out.g;
    *out_b = out.b;
    *out_a = out.a;
}
void shal_get_pixel(ALLEGRO_BITMAP * bitmap,
                    int x,
                    int y,
                    float * out_r,
                    float * out_g,
                    float * out_b,
                    float * out_a)
{
    ALLEGRO_COLOR out;
    out = al_get_pixel(bitmap, x, y);
    *out_r = out.r;
    *out_g = out.g;
    *out_b = out.b;
    *out_a = out.a;
}
void shal_draw_tinted_scaled_rotated_bitmap(ALLEGRO_BITMAP * bitmap,
                                            float tint_r,
                                            float tint_g,
                                            float tint_b,
                                            float tint_a,
                                            float cx,
                                            float cy,
                                            float dx,
                                            float dy,
                                            float xscale,
                                            float yscale,
                                            float angle,
                                            int flags)
{
    ALLEGRO_COLOR tint;
    tint.r = tint_r;
    tint.g = tint_g;
    tint.b = tint_b;
    tint.a = tint_a;
    return al_draw_tinted_scaled_rotated_bitmap(bitmap,
                                                tint,
                                                cx,
                                                cy,
                                                dx,
                                                dy,
                                                xscale,
                                                yscale,
                                                angle,
                                                flags);
}
void shal_map_rgba_f(float r,
                     float g,
                     float b,
                     float a,
                     float * out_r,
                     float * out_g,
                     float * out_b,
                     float * out_a)
{
    ALLEGRO_COLOR out;
    out = al_map_rgba_f(r, g, b, a);
    *out_r = out.r;
    *out_g = out.g;
    *out_b = out.b;
    *out_a = out.a;
}
void shal_draw_tinted_scaled_rotated_bitmap_region(ALLEGRO_BITMAP * bitmap,
                                                   float sx,
                                                   float sy,
                                                   float sw,
                                                   float sh,
                                                   float tint_r,
                                                   float tint_g,
                                                   float tint_b,
                                                   float tint_a,
                                                   float cx,
                                                   float cy,
                                                   float dx,
                                                   float dy,
                                                   float xscale,
                                                   float yscale,
                                                   float angle,
                                                   int flags)
{
    ALLEGRO_COLOR tint;
    tint.r = tint_r;
    tint.g = tint_g;
    tint.b = tint_b;
    tint.a = tint_a;
    return al_draw_tinted_scaled_rotated_bitmap_region(bitmap,
                                                       sx,
                                                       sy,
                                                       sw,
                                                       sh,
                                                       tint,
                                                       cx,
                                                       cy,
                                                       dx,
                                                       dy,
                                                       xscale,
                                                       yscale,
                                                       angle,
                                                       flags);
}
void shal_draw_pixel(float x,
                     float y,
                     float color_r,
                     float color_g,
                     float color_b,
                     float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_draw_pixel(x, y, color);
}
void shal_clear_to_color(float color_r,
                         float color_g,
                         float color_b,
                         float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_clear_to_color(color);
}

void shal_put_blended_pixel(int x,
                            int y,
                            float color_r,
                            float color_g,
                            float color_b,
                            float color_a)
{
    ALLEGRO_COLOR color;
    color.r = color_r;
    color.g = color_g;
    color.b = color_b;
    color.a = color_a;
    return al_put_blended_pixel(x, y, color);
}
void shal_draw_tinted_bitmap(ALLEGRO_BITMAP * bitmap,
                             float tint_r,
                             float tint_g,
                             float tint_b,
                             float tint_a,
                             float dx,
                             float dy,
                             int flags)
{
    ALLEGRO_COLOR tint;
    tint.r = tint_r;
    tint.g = tint_g;
    tint.b = tint_b;
    tint.a = tint_a;
    return al_draw_tinted_bitmap(bitmap, tint, dx, dy, flags);
}
void shal_draw_tinted_scaled_bitmap(ALLEGRO_BITMAP * bitmap,
                                    float tint_r,
                                    float tint_g,
                                    float tint_b,
                                    float tint_a,
                                    float sx,
                                    float sy,
                                    float sw,
                                    float sh,
                                    float dx,
                                    float dy,
                                    float dw,
                                    float dh,
                                    int flags)
{
    ALLEGRO_COLOR tint;
    tint.r = tint_r;
    tint.g = tint_g;
    tint.b = tint_b;
    tint.a = tint_a;
    return al_draw_tinted_scaled_bitmap(bitmap,
                                        tint,
                                        sx,
                                        sy,
                                        sw,
                                        sh,
                                        dx,
                                        dy,
                                        dw,
                                        dh,
                                        flags);
}
void shal_draw_tinted_rotated_bitmap(ALLEGRO_BITMAP * bitmap,
                                     float tint_r,
                                     float tint_g,
                                     float tint_b,
                                     float tint_a,
                                     float cx,
                                     float cy,
                                     float dx,
                                     float dy,
                                     float angle,
                                     int flags)
{
    ALLEGRO_COLOR tint;
    tint.r = tint_r;
    tint.g = tint_g;
    tint.b = tint_b;
    tint.a = tint_a;
    return al_draw_tinted_rotated_bitmap(bitmap,
                                         tint,
                                         cx,
                                         cy,
                                         dx,
                                         dy,
                                         angle,
                                         flags);
}
