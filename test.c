#include <stdio.h>

static int sum(int a, int b) {
  int result = a + b;
  return result;
}

static int mul(int a, int b) {
  int result = a + b;
  return result;
}

static int foo(int a, int b) {
  return a / b;
}

int cal(int a, int b) {
  int c = 5 , d = 5;
  int r = a + b * foo(d, c);
  return r;
}

int cal(int a, int b) {
  if (cond) {
    foo();
  }
  bar();
  int c = 5 , d = 5;
  int r = a + b * foo(b, a, c);
  return r;
}
