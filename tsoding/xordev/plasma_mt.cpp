// $ c++ -O3 -std=c++23 -pthread -o plasma_mt plasma_mt.cpp -lm
// $ ./plasma_mt
// $ ffmpeg -i output-%03d.ppm -r 60 output.mp4

#include <math.h> // can't  use cmath instead of , because MATH.PI define
#include <cstdio>
#include <print>
#include <thread>
#include <vector>

struct vec4 {
    float x, y, z, w;
    vec4(float x = 0, float y = 0, float z = 0, float w = 0)
        : x(x), y(y), z(z), w(w) {}
};

struct vec2 {
    float x, y;
    vec2(float x = 0, float y = 0) : x(x), y(y) {}
    vec2 yx() const { return vec2(y, x); }
    vec4 xyyx() const { return vec4(x, y, y, x); }
};

vec2 operator*(const vec2 &a, float s) { return vec2(a.x * s, a.y * s); }
vec2 operator+(const vec2 &a, float s) { return vec2(a.x + s, a.y + s); }
vec2 operator*(float s, const vec2 &a) { return a * s; }
vec2 operator-(const vec2 &a, const vec2 &b) { return vec2(a.x - b.x, a.y - b.y); }
vec2 operator+(const vec2 &a, const vec2 &b) { return vec2(a.x + b.x, a.y + b.y); }
vec2 operator*(const vec2 &a, const vec2 &b) { return vec2(a.x * b.x, a.y * b.y); }
vec2 operator/(const vec2 &a, float s) { return vec2(a.x / s, a.y / s); }
float dot(const vec2 &a, const vec2 &b) { return a.x * b.x + a.y * b.y; }
vec2 abs(const vec2 &a) { return vec2(fabsf(a.x), fabsf(a.y)); }
vec2 &operator+=(vec2 &a, const vec2 &b) { a = a + b; return a; }
vec2 &operator+=(vec2 &a, float s) { a = a + s; return a; }
vec2 cos(const vec2 &a) { return vec2(cosf(a.x), cosf(a.y)); }
vec4 sin(const vec4 &a) { return vec4(sinf(a.x), sinf(a.y), sinf(a.z), sinf(a.w)); }
vec4 exp(const vec4 &a) { return vec4(expf(a.x), expf(a.y), expf(a.z), expf(a.w)); }
vec4 tanh(const vec4 &a) { return vec4(tanhf(a.x), tanhf(a.y), tanhf(a.z), tanhf(a.w)); }
vec4 operator+(const vec4 &a, float s) { return vec4(a.x + s, a.y + s, a.z + s, a.w + s); }
vec4 operator*(const vec4 &a, float s) { return vec4(a.x * s, a.y * s, a.z * s, a.w * s); }
vec4 operator*(float s, const vec4 &a) { return a * s; }
vec4 operator+(const vec4 &a, const vec4 &b) { return vec4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w); }
vec4 &operator+=(vec4 &a, const vec4 &b) { a = a + b; return a; }
vec4 operator-(float s, const vec4 &a) { return vec4(s - a.x, s - a.y, s - a.z, s - a.w); }
vec4 operator/(const vec4 &a, const vec4 &b) { return vec4(a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w); }

void render_frame(int frame, int total_frames) {
    const int w = 16 * 60;
    const int h = 9 * 60;
    vec2 r = {(float)w, (float)h};

    char buf[256];
    snprintf(buf, sizeof(buf), "output-%03d.ppm", frame);
    FILE* f = fopen(buf, "wb");
    fprintf(f, "P6\n%d %d\n255\n", w, h);

    std::vector<unsigned char> pixels(w * h * 3);
    unsigned char* out = pixels.data();

    float t = ((float)frame / 240) * 2 * M_PI;
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            vec4 o;
            vec2 FC = {(float)x, (float)y};
            vec2 pos = (FC * 2. - r) / r.y, l, i,
                 v = pos * (l += 4. - 4. * abs(.7 - dot(pos, pos)));
            for (; i.y++ < 8.; o += (sin(v.xyyx()) + 1.) * abs(v.x - v.y))
                v += cos(v.yx() * i.y + i + t) / i.y + .7;
            o = tanh(5. * exp(l.x - 4. - pos.y * vec4(-1, 1, 2, 0)) / o);
            out[0] = o.x * 255;
            out[1] = o.y * 255;
            out[2] = o.z * 255;
            out += 3;
        }
    }
    fwrite(pixels.data(), 1, pixels.size(), f);
    fclose(f);
    std::print("Generated {} ({:3}/{:3})\n", buf, frame + 1, total_frames);
}

int main() {
    const int total_frames = 240;
    unsigned int num_threads = std::thread::hardware_concurrency();
    if (num_threads == 0) num_threads = 4;
    std::print("Using {} threads\n", num_threads);

    std::vector<std::jthread> threads;
    int frames_per_thread = (total_frames + num_threads - 1) / num_threads;

    for (unsigned int i = 0; i < num_threads; ++i) {
        int start = i * frames_per_thread;
        int end = std::min(start + frames_per_thread, total_frames);
        if (start >= total_frames) break;
        threads.emplace_back([start, end, total_frames](auto) {
            for (int frame = start; frame < end; ++frame) {
                render_frame(frame, total_frames);
            }
        });
    }
}
