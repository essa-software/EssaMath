// math_utils.h
#ifndef MATH_UTILS_H
#define MATH_UTILS_H
#include <stdint.h>
#include "expression.h"

#ifdef __cplusplus
extern "C" {
#endif

em_val em_numeric_nan();
em_val em_numeric_inf();
em_val em_numeric_minf();
em_val em_numeric_i();
em_val em_numeric_e();
em_val em_numeric_pi();
em_val em_numeric_phi();

int em_numeric_equal(int* _result, em_val _a, em_val _b);
int em_numeric_nequal(int* _result, em_val _a, em_val _b);
int em_numeric_gth(int* _result, em_val _a, em_val _b);
int em_numeric_geq(int* _result, em_val _a, em_val _b);
int em_numeric_lth(int* _result, em_val _a, em_val _b);
int em_numeric_leq(int* _result, em_val _a, em_val _b);

int em_numeric_add(em_val* _result, em_val _a, em_val _b);
int em_numeric_sub(em_val* _result, em_val _a, em_val _b);
int em_numeric_mul(em_val* _result, em_val _a, em_val _b);
int em_numeric_div(em_val* _result, em_val _a, em_val _b);
int em_numeric_pow(em_val* _result, em_val _a, em_val _b);
int em_numeric_neg(em_val* _result, em_val _a);
int em_numeric_mod(em_val* _result, em_val _a, em_val _b);

int em_numeric_abs(em_val* _result, em_val _a);
int em_numeric_exp(em_val* _result, em_val _a);
int em_numeric_log(em_val* _result, em_val _a);
int em_numeric_floor(em_val* _result, em_val _a);
int em_numeric_ceil(em_val* _result, em_val _a);
int em_numeric_round(em_val* _result, em_val _a);
int em_numeric_sin(em_val* _result, em_val _a);
int em_numeric_cos(em_val* _result, em_val _a);
int em_numeric_tan(em_val* _result, em_val _a);
int em_numeric_cot(em_val* _result, em_val _a);
int em_numeric_sec(em_val* _result, em_val _a);
int em_numeric_csc(em_val* _result, em_val _a);
int em_numeric_asin(em_val* _result, em_val _a);
int em_numeric_acos(em_val* _result, em_val _a);
int em_numeric_atan(em_val* _result, em_val _a);
int em_numeric_atan2(em_val* _result, em_val _a, em_val _b);
int em_numeric_acot(em_val* _result, em_val _a);
int em_numeric_asec(em_val* _result, em_val _a);
int em_numeric_acsc(em_val* _result, em_val _a);
int em_numeric_sinh(em_val* _result, em_val _a);
int em_numeric_cosh(em_val* _result, em_val _a);
int em_numeric_tanh(em_val* _result, em_val _a);
int em_numeric_coth(em_val* _result, em_val _a);
int em_numeric_sech(em_val* _result, em_val _a);
int em_numeric_csch(em_val* _result, em_val _a);
int em_numeric_asinh(em_val* _result, em_val _a);
int em_numeric_acosh(em_val* _result, em_val _a);
int em_numeric_atanh(em_val* _result, em_val _a);
int em_numeric_acoth(em_val* _result, em_val _a);
int em_numeric_asech(em_val* _result, em_val _a);
int em_numeric_acsch(em_val* _result, em_val _a);

int em_numeric_iseven(int64_t n);

int em_numeric_factorial(em_val* _result, em_val _a);
int em_numeric_isinteger(em_val _value);

double em_numeric_zeta(int64_t);
double em_numeric_harmonic(int64_t);
double em_numeric_neg_eta(int64_t);

#ifdef __cplusplus
}
#endif

#endif // MATH_UTILS_H
