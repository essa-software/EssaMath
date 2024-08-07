// 10_elementary_functions.h
#include "expression.h"
#ifndef ELEMENTARY_FUNCTIONS_H
#define ELEMENTARY_FUNCTIONS_H

#ifdef __cplusplus
extern "C" {
#endif

// 10.1 Functions for Numbers
int em_abs(em_object _z);
int em_ceiling(em_object _x);
int em_entier(em_object _x);
int em_floor(em_object _x);
int em_fix(em_object _x);
int em_hstep(em_object _x);
int em_lmax(em_object _l);
int em_lmin(em_object _l);
int em_max(size_t n, ...);
int em_min(size_t n, ...);
int em_round(em_object _x);
int em_signum(em_object _x);
int em_truncate(em_object _x);

// 10.2 Functions for Complex Numbers
int em_cabs(em_object _expr);
int em_carg(em_object _z);
int em_conjugate(em_object _x);
int em_imagpart(em_object _expr);
int em_polarform(em_object _expr);
int em_realpart(em_object _expr);
int em_rectform(em_object _expr);

// 10.3 Combinatorial Functions
int em_binomial(em_object _x, em_object _y);
int em_factcomb(em_object _expr);
int em_genfact(em_object _x, em_object _y, em_object _z);
int em_minfactorial(em_object _expr);

// 10.4 Root, Exponential and Logarithmic Functions
int em_exp(em_object _x);
int em_li(em_object _s, em_object _z);
int em_log(em_object _x);
int em_logarc(em_object _expr);
int em_logcontract(em_object _expr);
int em_plog(em_object _x);
int em_sqrt(em_object _x);

// 10.5 Trigonometric Functions
int em_acos(em_object _x);
int em_acosh(em_object _x);
int em_acot(em_object _x);
int em_acoth(em_object _x);
int em_acsc(em_object _x);
int em_acsch(em_object _x);
int em_asec(em_object _x);
int em_asech(em_object _x);
int em_asin(em_object _x);
int em_asinh(em_object _x);
int em_atan(em_object _x);
int em_atan2(em_object _y, em_object _x);
int em_atanh(em_object _x);
int em_cos(em_object _x);
int em_cosh(em_object _x);
int em_cot(em_object _x);
int em_coth(em_object _x);
int em_csc(em_object _x);
int em_csch(em_object _x);
int em_sec(em_object _x);
int em_sech(em_object _x);
int em_sin(em_object _x);
int em_sinh(em_object _x);
int em_tan(em_object _x);
int em_tanh(em_object _x);
int em_trigexpand(em_object _expr);
int em_trigreduce(em_object _expr);
int em_trigreduce_2(em_object _expr, em_object _x);
int em_trigsimp(em_object _expr);
int em_trigrat(em_object _expr);

// 10.6 Random Numbers
int em_make_random_state(em_object _n);
int em_set_random_state(em_object _s);
int em_random(em_object _x);

#ifdef __cplusplus
}
#endif

#endif // ELEMENTARY_FUNCTIONS_H
