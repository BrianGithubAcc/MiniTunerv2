#define _POSIX_C_SOURCE 200809L
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

static volatile double sink;

static double now_ns(void) {
  struct timespec value;
  clock_gettime(CLOCK_MONOTONIC, &value);
  return (double)value.tv_sec * 1e9 + (double)value.tv_nsec;
}

#define MEASURE(label, expression) do {                                      \
  double x = 1.000001, start = now_ns();                                     \
  for (uint64_t i = 0; i < 20000000ULL; ++i) {                               \
    x += 1e-12;                                                               \
    sink = (expression);                                                      \
  }                                                                           \
  printf("%s %.12g\n", label, (now_ns() - start) / 20000000.0);             \
} while (0)

int main(void) {
  MEASURE("sqrt_binary64", sqrt(x));
  MEASURE("sqrt_binary32", (double)sqrtf((float)x));
  MEASURE("atan_fixed", atan(x));
  MEASURE("abs_fixed", fabs(x));
  MEASURE("pow_fixed", pow(x, 6.0));
  return sink == 0.123;
}
