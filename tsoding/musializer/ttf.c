#include <complex.h>
#include <math.h>
#include <stdio.h>

#define N 8

float pi;
int main() {

  float in[N];
  float out[N];
  pi = atan2(1, 1) * 4;

  for (size_t i = 0; i < N; ++i) {
    float t = (float)i / N;
    in[i] = sinf(2 * pi * t) + sinf(2 * pi * t * 3);
  }

  for (size_t f = 0; f < N; ++f) {
    out[f] = 0;
    for (size_t i = 0; i < N; ++i) {
      float t = (float)i / N;
      out[f] += in[i] * sinf(2 * pi * t * f);
    }
  }

  for (size_t i = 0; i < N; ++i) {
    printf("%zu %5.2f\n", i, out[i]);
  }
}
