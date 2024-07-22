// math_utils.h
#ifndef MATH_UTILS_H
#define MATH_UTILS_H
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

double em_nan();
_Complex double em_cnan();

double em_inf();
_Complex double em_cinf();

int em_iseven(int64_t n);

double em_factorial(double _value);

int em_isinteger(double _value);

double em_zeta(int64_t);
double em_harmonic(int64_t);
double em_neg_eta(int64_t);

#ifdef __cplusplus
}
#endif

#endif // MATH_UTILS_H
