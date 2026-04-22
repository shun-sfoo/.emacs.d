#pragma once
#include <cmath>
#include <format>
#include <string>

class vec3 {
public:
  double e[3];
  vec3() : e{0, 0, 0} {}
  vec3(double e1, double e2, double e3) : e{e1, e2, e3} {}
  double x() const { return e[0]; }
  double y() const { return e[1]; }
  double z() const { return e[2]; }

  vec3 operator-() { return vec3(-e[0], -e[1], -e[2]); }
  double operator[](int i) const { return e[i]; }
  double &operator[](int i) { return e[i]; }
  vec3 &operator+=(const vec3 &v) {
    e[0] += v.e[0];
    e[1] += v.e[1];
    e[2] += v.e[2];
    return *this;
  }

  vec3 &operator*=(double t) {
    e[0] *= t;
    e[1] *= t;
    e[2] *= t;
    return *this;
  }

  vec3 &operator/=(double t) { return *this *= (1 / t); }

  double length() const { return std::sqrt(length_squared()); }

  double length_squared() const {
    return e[0] * e[0] + e[1] * e[1] + e[2] * e[2];
  }
};

using point3 = vec3;

inline std::string to_string(const vec3 &v) {
  return std::format("{}, {} , {}", v.e[0], v.e[1], v.e[2]);
}

inline vec3 operator*(double t, const vec3 &v) {
  return vec3(t * v.e[0], t * v.e[1], t * v.e[2]);
}

inline vec3 operator*(const vec3 &v, double t) { return t * v; }

inline vec3 operator+(const vec3 &u, const vec3 &v) {
  return vec3(u.e[0] + v.e[0], u.e[1] + v.e[1], u.e[2] + v.e[2]);
}
