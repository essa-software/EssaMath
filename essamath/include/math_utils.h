// math_utils.h
#ifndef MATH_UTILS_H
#define MATH_UTILS_H
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

double em_numeric_nan();
_Complex double em_numeric_cnan();

double em_numeric_inf();
_Complex double em_numeric_cinf();

int em_numeric_iseven(int64_t n);

double em_numeric_factorial(double _value);

int em_numeric_isinteger(double _value);

double em_numeric_zeta(int64_t);
double em_numeric_harmonic(int64_t);
double em_numeric_neg_eta(int64_t);

#ifdef __cplusplus
}
#endif

#endif // MATH_UTILS_H
