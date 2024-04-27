#pragma once
#include "Expression.hpp"
#include <vector>

namespace Essa::Math{

template<typename T>
expression<T> bessel_j(int _index, expression<T>& _expr);

template<typename T>
expression<T> bessel_y(int _index, expression<T>& _expr);

template<typename T>
expression<T> bessel_i(int _index, expression<T>& _expr);

template<typename T>
expression<T> bessel_k(int _index, expression<T>& _expr);

template<typename T>
expression<T> hankel_1(int _index, expression<T>& _expr);

template<typename T>
expression<T> hankel_2(int _index, expression<T>& _expr);

template<typename T>
expression<T> scaled_bessel_i(int _index, expression<T>& _expr);

template<typename T>
expression<T> scaled_bessel_i0(expression<T>& _expr);

template<typename T>
expression<T> scaled_bessel_i1(expression<T>& _expr);

template<typename T>
expression<T> airy_ai(expression<T>& _expr);

template<typename T>
expression<T> airy_dai(expression<T>& _expr);

template<typename T>
expression<T> airy_bi(expression<T>& _expr);

template<typename T>
expression<T> airy_dbi(expression<T>& _expr);

template<typename T>
expression<T> gamma(expression<T>& _expr);

template<typename T>
expression<T> log_gamma(expression<T>& _expr);

template<typename T>
expression<T> gamma_incomplete(expression<T>& _a, expression<T>& _z);

template<typename T>
expression<T> gamma_incomplete_regularized(expression<T>& _a, expression<T>& _z);

template<typename T>
expression<T> gamma_incomplete_generalized(expression<T>& _a, expression<T>& _z1, expression<T>& _z2);

template<typename T>
expression<T> makegamma(expression<T>& _expr);

template<typename T>
expression<T> beta(expression<T>& _a, expression<T>& _z);

template<typename T>
expression<T> beta_incomplete(expression<T>& _a, expression<T>& _b, expression<T>& _z);

template<typename T>
expression<T> beta_incomplete_regularized(expression<T>& _a, expression<T>& _b, expression<T>& _z);

template<typename T>
expression<T> beta_incomplete_regularized(expression<T>& _a, expression<T>& _b, expression<T>& _z1, expression<T>& _z2);

template<typename T>
expression<T> psi(int _n, expression<T>& _expr);

template<typename T>
expression<T> makefact(expression<T>& _expr);

template<typename T>
expression<T> numfactor(expression<T>& _expr);

template<typename T>
expression<T> numfactor(expression<T>& _expr);

template<typename T>
expression<T> erf(expression<T>& _expr);

template<typename T>
expression<T> erfc(expression<T>& _expr);

template<typename T>
expression<T> erfi(expression<T>& _expr);

template<typename T>
expression<T> erf_generalized(expression<T>& _z1, expression<T>& _z2);

template<typename T>
expression<T> fresnel_c(expression<T>& _expr);

template<typename T>
expression<T> fresnel_s(expression<T>& _expr);

template<typename T>
expression<T> struve_h(int _n, expression<T>& _expr);

template<typename T>
expression<T> struve_l(int _n, expression<T>& _expr);

template<typename T>
expression<T> hypergeometric(std::vector<expression<T>>& _a, std::vector<expression<T>>& _b, expression<T>& _expr);

template<typename T>
expression<T> hgfred(std::vector<expression<T>>& _a, std::vector<expression<T>>& _b, expression<T>& _expr);

template<typename T>
expression<T> specint(expression<T>& _expr);

template<typename T>
expression<T> lambert_w(expression<T>& _expr);

template<typename T>
expression<T> nzeta(expression<T>& _expr);

template<typename T>
expression<T> nzetar(expression<T>& _expr);

template<typename T>
expression<T> nzetai(expression<T>& _expr);

template<typename T>
expression<T> jacobi_sn(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_cn(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_dn(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_ns(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_sc(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_sd(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_nc(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_sc(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_cd(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_nd(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_ds(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> jacobi_dc(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_sn(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_cn(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_dn(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_ns(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_sc(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_sd(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_nc(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_cs(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_cd(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_nd(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_ds(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> inverse_jacobi_dc(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> elliptic_f(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> elliptic_e(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> elliptic_eu(expression<T>& _u, expression<T>& _m);

template<typename T>
expression<T> elliptic_pi(expression<T>& _n, expression<T>& _phi, expression<T>& _m);

template<typename T>
expression<T> elliptic_kc(expression<T>& _m);

template<typename T>
expression<T> elliptic_ec(expression<T>& _m);

}
