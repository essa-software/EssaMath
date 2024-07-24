// 15_special_functions.h
#include "expression.h"
#ifndef SPECIAL_FUNCTIONS_H
#define SPECIAL_FUNCTIONS_H

#ifdef __cplusplus
extern "C" {
#endif

// 15.2 Bessel Functions
int em_bessel_j(em_object _v, em_object _z);
int em_bessel_y(em_object _v, em_object _z);
int em_bessel_i(em_object _v, em_object _z);
int em_bessel_k(em_object _v, em_object _z);
int em_hankel_1(em_object _v, em_object _z);
int em_hankel_2(em_object _v, em_object _z);
int em_scaled_bessel_i(em_object _v, em_object _z);
int em_scaled_bessel_i0(em_object _z);
int em_scaled_bessel_i1(em_object _z);
int em_s(em_object _u, em_object _v, em_object _z);
int em_slommel(em_object _u, em_object _v, em_object _z);

// 15.3 Airy Functions
int em_airy_ai(em_object _x);
int em_airy_dai(em_object _x);
int em_airy_bi(em_object _x);
int em_airy_dbi(em_object _x);

// 15.4 Gamma and Factorial Functions
int em_bffac(em_object _expr, em_object _n);
int em_bfpsi(em_object _n, em_object _z, em_object _fpprec);
int em_bfpsi0(em_object _z, em_object _fpprec);
int em_cbffac(em_object _z, em_object _fpprec);
int em_gamma(em_object _z);
int em_log_gamma(em_object _z);
int em_gamma_incomplete_lower(em_object _a, em_object _z);
int em_gamma_incomplete(em_object _a, em_object _z);
int em_gamma_incomplete_regularized(em_object _a, em_object _z);
int em_gamma_incomplete_generalized(em_object _a, em_object _z1, em_object _z2);
int em_makegamma(em_object _expr);
int em_beta(em_object _a, em_object _b);
int em_beta_incomplete(em_object _a, em_object _b, em_object _z);
int em_beta_incomplete_regularized(em_object _a, em_object _b, em_object _z);
int em_beta_incomplete_generalized(em_object _a, em_object _b, em_object _z1, em_object _z2);
int em_makefact(em_object _expr);
int em_numfactor(em_object _expr);

// 15.5 Exponential Integrals
int em_expintegral_e1(em_object _z);
int em_expintegral_ei(em_object _x);
int em_expintegral_li(em_object _x);
int em_expintegral_e(em_object _n, em_object _z);
int em_expintegral_si(em_object _z);
int em_expintegral_ci(em_object _z);
int em_expintegral_shi(em_object _z);
int em_expintegral_chi(em_object _z);

// 15.6 Error Function
int em_erf(em_object _z);
int em_erfc(em_object _z);
int em_erfi(em_object _z);
int em_erf_generalized(em_object _z1, em_object _z2);
int em_fresnel_c(em_object _z);
int em_fresnel_s(em_object _z);

// 15.7 Struve Functions
int em_struve_h(em_object _v, em_object _z);
int em_struve_l(em_object _v, em_object _z);

// 15.8 Hypergeometric Functions
int em_m(em_object _k, em_object _u, em_object _z);
int em_w(em_object _k, em_object _u, em_object _z);
int em_f(em_object _p, em_object _q, em_object _a, em_object _b, em_object _z);
int em_hypergeometric(em_object _a, em_object _b, em_object _x);
int em_hypergeometric_simp(em_object _e);
int em_hgfred(em_object _a, em_object _b, em_object _t);

// 15.9 Parabolic Cylinder Functions
int em_parabolic_cylinder_d(em_object _v, em_object _z);

// 15.10 Functions and Variables for Special Functions
int em_lambert_w(em_object _z);
int em_generalized_lambert_w(em_object _k, em_object _z);
int em_kbateman(em_object _v, em_object _x);
int em_nzeta(em_object _z);
int em_nzetar(em_object _z);
int em_nzetai(em_object _z);

#ifdef __cplusplus
}
#endif

#endif // SPECIAL_FUNCTIONS_H
