#include "color.h"
#include <fstream>
#include <print>

constexpr int image_width = 256;
constexpr int image_height = 256;

int main() {
  // watch buffer._M_impl._M_start
  std::vector<Gf2Pixel> buffer(image_width * image_height);
  std::vector<Pixel> pixels(image_width * image_height);

  // render
  for (int h = 0; h < image_height; ++h) {
    for (int w = 0; w < image_width; ++w) {
      auto idx = h * image_width + w;
      auto pixel_color = color((double(w) / (image_width - 1)),
                               double(h) / (image_height - 1), 0);
      write_color(buffer, pixels, idx, pixel_color);
    }
  }

  std::string output_path("main.ppm");
  std::ofstream ofs(output_path, std::ios::binary);
  if (!ofs) {
    std::println("can't write file");
    return 1;
  }

  std::print(ofs, "P6\n{} {}\n255\n", image_width, image_height);
  // write(const char* s, streamsize count)
  ofs.write(reinterpret_cast<const char *>(pixels.data()),
            image_width * image_height * sizeof(Pixel));
}
