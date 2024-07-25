// 29_number_theory.h
#include "expression.h"
#ifndef NUMBER_THEORY_H
#define NUMBER_THEORY_H

#ifdef __cplusplus
extern "C" {
#endif

int em_bern(em_object _n);
int em_bernpoly(em_object _x, em_object _n);
int em_bfzeta(em_object _s, em_object _n);
int em_bfhzeta(em_object _s, em_object _h, em_object _n);
int em_burn(em_object _n);
int em_chinese(em_object _r, em_object _m);
int em_cf(em_object _expr);
int em_cfdisrep(em_object _list);
int em_cfexpand(em_object _x);
int em_divsum(em_object _n);
int em_divsum_2(em_object _n, em_object _k);
int em_euler(em_object _n);
int em_fib(em_object _n);
int em_fibtophi(em_object _expr);
int em_ifactors(em_object _n);
int em_igcdex(em_object _n, em_object _k);
int em_inrt(em_object _x, em_object _n);
int em_inv_mod(em_object _n, em_object _m);
int em_isqrt(em_object _x);
int em_jacobi(em_object _p, em_object _q);
int em_lcm(size_t n, ...);
int em_lucas(em_object _n);
int em_mod(em_object _x, em_object _y);
int em_next_prime(em_object _n);
int em_partfrac(em_object _expr, em_object _var);
int em_power_mod(em_object _a, em_object _n, em_object _m);
int em_primep(em_object _n);
int em_primes(em_object _start, em_object _end);
int em_prev_prime(em_object _n);
int em_qunit(em_object _n);
int em_totient(em_object _n);
int em_zeta(em_object _n);
int em_zn_add_table(em_object _n);
int em_zn_characteristic_factors(em_object _n);
int em_zn_carmichael_lambda(em_object _n);
int em_zn_determinant(em_object _matrix, em_object _p);
int em_zn_factor_generators(em_object _n);
int em_zn_invert_by_lu(em_object _matrix, em_object _p);
int em_zn_log(em_object _a, em_object _g, em_object _n);
int em_zn_mult_table(em_object _n);
int em_zn_mult_table_2(em_object _n, em_object _gcd);
int em_zn_nth_root(em_object _x, em_object _n, em_object _m);
int em_zn_nth_root_2(em_object _x, em_object _n, em_object _m, em_object _p_e);
int em_zn_order(em_object _x, em_object _n);
int em_zn_order_2(em_object _x, em_object _n, em_object _p_e);
int em_zn_power_table(em_object _n);
int em_zn_power_table_2(em_object _n, em_object _gcd);
int em_zn_power_table_3(em_object _n, em_object _gcd, em_object _max_exp);
int em_zn_primroot(em_object _n);
int em_zn_primroot_2(em_object _n, em_object _p_e);
int em_zn_primroot_p(em_object _x, em_object _n);
int em_zn_primroot_p_2(em_object _x, em_object _n, em_object _p_e);

#ifdef __cplusplus
}
#endif

#endif // NUMBER_THEORY_H
