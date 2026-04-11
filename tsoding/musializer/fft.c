#include <assert.h>
#include <complex.h>
#include <math.h>
#include <stdio.h>

#define N 4096 * 2 * 2

float pi;

void dft(float in[], float complex out[], size_t n) {
  for (size_t f = 0; f < n; ++f) {
    out[f] = 0;
    for (size_t i = 0; i < n; ++i) {
      float t = (float)i / n;
      out[f] += in[i] * cexp(2 * I * pi * f * t);
    }
  }
}

void fft(float in[], size_t stride, float complex out[], size_t n) {
  assert(n > 0);

  if (n == 1) {
    out[0] = in[0];
    return;
  }
  
  fft(in, stride * 2, out, n / 2);
  fft(in + stride, stride * 2, out + n / 2, n / 2);

  for (size_t k = 0; k < n / 2; ++k) {
    float t = (float)k / n;
    float complex v = cexp(-2 * I * pi * t) * out[k + n / 2];
    float complex e = out[k];
    out[k] = e + v;
    out[k + n / 2] = e - v;
  }
}

int main() {

  float in[N];
  float complex out[N];
  pi = atan2(1, 1) * 4;

  for (size_t i = 0; i < N; ++i) {
    float t = (float)i / N;
    in[i] = cosf(2 * pi * t * 1) + sinf(2 * pi * t * 2);
  }

  //dft(in, out, N);
  fft(in, 1, out, N);
}
