#pragma once
#include "vec3.h"
#include <vector>

struct Pixel {
  uint8_t r, g, b;
};

// initial with purple
// gf2 window.cpp:867
// uint8_t pixel[3] = { (uint8_t) (display->bits[i] >> 16), (uint8_t)
// (display->bits[i] >> 8), (uint8_t) display->bits[i] };
// r => pixel >> 16
// g => pixel >> 8
// b => pixel
// so define by b, g, r order
struct alignas(uint32_t) Gf2Pixel {
  uint8_t b{50}, g{0}, r{50};
  uint8_t alpha{255};
};

using color = vec3;
inline void write_color(std::vector<Gf2Pixel> &visual_buffer,
                        std::vector<Pixel> &data, size_t idx,
                        const color &pixel_color) {
  auto r = pixel_color.x();
  auto g = pixel_color.y();
  auto b = pixel_color.z();
  auto rbyte = static_cast<uint8_t>(255.999 * r);
  auto gbyte = static_cast<uint8_t>(255.999 * g);
  auto bbyte = static_cast<uint8_t>(255.999 * b);
  auto &vb = visual_buffer[idx];
  auto &d = data[idx];

  //             [0-7][8-15][16-23][24-32]
  // PPM         R    G     B      A
  // Gf2 bitmap  B    G     R      A:not use  little enden

  // gf2 and ppm data
  d.r = vb.r = rbyte;
  d.g = vb.g = gbyte;
  d.b = vb.b = bbyte;
}
