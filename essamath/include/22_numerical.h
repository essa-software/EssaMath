// 22_numerical.h
#include "expression.h"
#ifndef NUMERICAL_H
#define NUMERICAL_H

#ifdef __cplusplus
extern "C" {
#endif

// 22.2 Functions and Variables for fft
int em_polartorect(em_object _r, em_object _t);
int em_recttopolar(em_object _a, em_object _b);
int em_inverse_fft(em_object _y);
int em_fft(em_object _x);
int em_real_fft(em_object _x);
int em_inverse_real_fft(em_object _y);
int em_bf_inverse_fft(em_object _y);
int em_bf_fft(em_object _y);
int em_bf_real_fft(em_object _y);
int em_bf_inverse_real_fft(em_object _y);

// 22.3 Functions and Variables for FFTPACK5
int em_fftpack5_fft(em_object _x);
int em_fftpack5_inverse_fft(em_object _y);
int em_fftpack5_real_fft(em_object _x);
int em_fftpack5_inverse_real_fft(em_object _y, em_object _n);

// 22.4 Functions for numerical solution of equations ¶
int em_horner(em_object _expr);
int em_horner_2(em_object _expr, em_object _x);
int em_find_root(em_object _expr, em_object _x, em_object _a, em_object _b, em_object _options);
int em_find_root_2(em_object _f, em_object _a, em_object _b, em_object _options);
int em_bf_find_root(em_object _expr, em_object _x, em_object _a, em_object _b, em_object _options);
int em_bf_find_root_2(em_object _f, em_object _a, em_object _b, em_object _options);
int em_newton(em_object _expr, em_object _x, em_object _x0, em_object _eps);
int em_rk(em_object _ODE, em_object _var, em_object _initial, em_object _domain);

#ifdef __cplusplus
}
#endif

#endif // NUMERICAL_H
