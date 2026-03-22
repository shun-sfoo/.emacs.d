#include <array>
#include <fstream>
#include <iostream>

struct Pixel {
  unsigned char r, g, b;
};

constexpr int image_width = 256;
constexpr int image_height = 256;

int main() {
  std::string output_path("main.ppm");
  std::ofstream ofs(output_path, std::ios::binary);
  if (!ofs) {
    std::cerr << "can't write file" << std::endl;
    return 1;
  }

  ofs << "P6\n" << image_width << " " << image_height << "\n255\n";
  std::array<Pixel, image_width * image_height> pixels{0};

  auto *ptr = pixels.data();
  for (int h = 0; h < image_height; ++h) {
    for (int w = 0; w < image_width; ++w) {
      ptr->r = static_cast<unsigned char>(255.999 * w / (image_width - 1));
      ptr->g = static_cast<unsigned char>(255.999 * h / (image_height - 1));
      ptr->b = 0;
      ++ptr;
    }
  }

  ofs.write(reinterpret_cast<const char *>(pixels.data()),
            pixels.size() * sizeof(Pixel));
}
