#include "15_special_functions.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_bessel_j(em_object _v, em_object _z){
    return em_invoke("bessel_j", 2, _v, _z);
}

int em_bessel_y(em_object _v, em_object _z){
    return em_invoke("bessel_y", 2, _v, _z);
}

int em_bessel_i(em_object _v, em_object _z){
    return em_invoke("bessel_i", 2, _v, _z);
}

int em_bessel_k(em_object _v, em_object _z){
    return em_invoke("bessel_k", 2, _v, _z);
}

int em_hankel_1(em_object _v, em_object _z){
    return em_invoke("hankel_1", 2, _v, _z);
}

int em_hankel_2(em_object _v, em_object _z){
    return em_invoke("hankel_2", 2, _v, _z);
}

int em_scaled_bessel_i(em_object _v, em_object _z){
    return em_invoke("scaled_bessel_i", 2, _v, _z);
}

int em_scaled_bessel_i0(em_object _z){
    return em_invoke("scaled_bessel_i0", 1, _z);
}

int em_scaled_bessel_i1(em_object _z){
    return em_invoke("scaled_bessel_i1", 1, _z);
}

int em_s(em_object _u, em_object _v, em_object _z){
    return em_invoke("%s", 3, _u, _v, _z);
}

int em_slommel(em_object _u, em_object _v, em_object _z){
    return em_invoke("slommel", 3, _u, _v, _z);
}

int em_airy_ai(em_object _x){
    return em_invoke("airy_ai", 1, _x);
}

int em_airy_dai(em_object _x){
    return em_invoke("airy_dai", 1, _x);
}

int em_airy_bi(em_object _x){
    return em_invoke("airy_bi", 1, _x);
}

int em_airy_dbi(em_object _x){
    return em_invoke("airy_dbi", 1, _x);
}

int em_bffac(em_object _expr, em_object _n){
    return em_invoke("bffac", 2, _expr, _n);
}

int em_bfpsi(em_object _n, em_object _z, em_object _fpprec){
    return em_invoke("bfpsi", 3, _n, _z, _fpprec);
}

int em_bfpsi0(em_object _z, em_object _fpprec){
    return em_invoke("bfpsi0", 2, _z, _fpprec);
}

int em_cbffac(em_object _z, em_object _fpprec){
    return em_invoke("cbffac", 2, _z, _fpprec);
}

int em_gamma(em_object _z){
    return em_invoke("gamma", 1, _z);
}

int em_log_gamma(em_object _z){
    return em_invoke("log_gamma", 1, _z);
}

int em_gamma_incomplete_lower(em_object _a, em_object _z){
    return em_invoke("gamma_incomplete_lower", 2, _a, _z);
}

int em_gamma_incomplete(em_object _a, em_object _z){
    return em_invoke("gamma_incomplete", 2, _a, _z);
}

int em_gamma_incomplete_regularized(em_object _a, em_object _z){
    return em_invoke("gamma_incomplete_regularized", 2, _a, _z);
}

int em_gamma_incomplete_generalized(em_object _a, em_object _z1, em_object _z2){
    return em_invoke("gamma_incomplete_generalized", 3, _a, _z1, _z2);
}

int em_makegamma(em_object _expr){
    return em_invoke("makegamma", 1, _expr);
}

int em_beta(em_object _a, em_object _b){
    return em_invoke("beta", 2, _a, _b);
}

int em_beta_incomplete(em_object _a, em_object _b, em_object _z){
    return em_invoke("beta_incomplete", 3, _a, _b, _z);
}

int em_beta_incomplete_regularized(em_object _a, em_object _b, em_object _z){
    return em_invoke("beta_incomplete_regularized", 3, _a, _b, _z);
}

int em_beta_incomplete_generalized(em_object _a, em_object _b, em_object _z1, em_object _z2){
    return em_invoke("beta_incomplete_generalized", 4, _a, _b, _z1, _z2);
}

int em_makefact(em_object _expr){
    return em_invoke("makefact", 1, _expr);
}

int em_numfactor(em_object _expr){
    return em_invoke("numfactor", 1, _expr);
}

int em_expintegral_e1(em_object _z){
    return em_invoke("expintegral_e1", 1, _z);
}

int em_expintegral_ei(em_object _x){
    return em_invoke("expintegral_ei", 1, _x);
}

int em_expintegral_li(em_object _x){
    return em_invoke("expintegral_li", 1, _x);
}

int em_expintegral_e(em_object _n, em_object _z){
    return em_invoke("expintegral_e", 2, _n, _z);
}

int em_expintegral_si(em_object _z){
    return em_invoke("expintegral_si", 1, _z);
}

int em_expintegral_ci(em_object _z){
    return em_invoke("expintegral_ci", 1, _z);
}

int em_expintegral_shi(em_object _z){
    return em_invoke("expintegral_shi", 1, _z);
}

int em_expintegral_chi(em_object _z){
    return em_invoke("expintegral_chi", 1, _z);
}

int em_erf(em_object _z){
    return em_invoke("erf", 1, _z);
}

int em_erfc(em_object _z){
    return em_invoke("erfc", 1, _z);
}

int em_erfi(em_object _z){
    return em_invoke("erfi", 1, _z);
}

int em_erf_generalized(em_object _z1, em_object _z2){
    return em_invoke("erf_generalized", 2, _z1, _z2);
}

int em_fresnel_c(em_object _z){
    return em_invoke("fresnel_c", 1, _z);
}

int em_fresnel_s(em_object _z){
    return em_invoke("fresnel_s", 1, _z);
}

int em_struve_h(em_object _v, em_object _z){
    return em_invoke("struve_h", 2, _v, _z);
}

int em_struve_l(em_object _v, em_object _z){
    return em_invoke("struve_l", 2, _v, _z);
}

int em_m(em_object _k, em_object _u, em_object _z){
    return em_invoke("%m", 3, _k, _u, _z);
}

int em_w(em_object _k, em_object _u, em_object _z){
    return em_invoke("%w", 3, _k, _u, _z);
}

int em_f(em_object _p, em_object _q, em_object _a, em_object _b, em_object _z){
    return em_invoke("%f", 5, _p, _q, _a, _b, _z);
}

int em_hypergeometric(em_object _a, em_object _b, em_object _x){
    return em_invoke("hypergeometric", 3, _a, _b, _x);
}

int em_hypergeometric_simp(em_object _e){
    return em_invoke("hypergeometric_simp", 1, _e);
}

int em_hgfred(em_object _a, em_object _b, em_object _t){
    return em_invoke("hgfred", 3, _a, _b, _t);
}

int em_parabolic_cylinder_d(em_object _v, em_object _z){
    return em_invoke("parabolic_cylinder_d", 2, _v, _z);
}

int em_lambert_w(em_object _z){
    return em_invoke("lambert_w", 1, _z);
}

int em_generalized_lambert_w(em_object _k, em_object _z){
    return em_invoke("generalized_lambert_w", 2, _k, _z);
}

int em_kbateman(em_object _v, em_object _x){
    return em_invoke("kbateman", 2, _v, _x);
}

int em_nzeta(em_object _z){
    return em_invoke("nzeta", 1, _z);
}

int em_nzetar(em_object _z){
    return em_invoke("nzetar", 1, _z);
}

int em_nzetai(em_object _z){
    return em_invoke("nzetai", 1, _z);
}
