#include <stdio.h>
#include <stdlib.h>

int main() {

  char buf[256];
  for (int i = 0; i < 60; ++i) {
    snprintf(buf, sizeof(buf), "output-%02d.ppm", i);
    const char *output_path = buf;
    FILE *f = fopen(output_path, "wb");
    int w = 16 * 60;
    int h = 9 * 60;
    fprintf(f, "P6\n");
    fprintf(f, "%d %d\n", w, h);
    fprintf(f, "255\n");
    for (int y = 0; y < h; ++y) {
      for (int x = 0; x < w; ++x) {
        if (((x + i) / 60 + (y + i) / 60) % 2) {
          fputc(0xFF, f);
          fputc(0x00, f);
          fputc(0x00, f);
        } else {
          fputc(0x00, f);
          fputc(0x00, f);
          fputc(0x00, f);
        }
      }
    }

    fclose(f);
    printf("Generated %s\n", output_path);
  }
  system("ffmpeg -i output-%02d.ppm -r 60 output.mp4");
  system("rm *.ppm");
  return 0;
}
