// 14_polynomials.h
#include "expression.h"
#ifndef POLYLONIALS_H
#define POLYLONIALS_H

#ifdef __cplusplus
extern "C" {
#endif

// 14.2 Functions and Variables for Polynomials
int em_bezout(em_object _p1, em_object _p2, em_object _x);
int em_bothcoef(em_object _expr, em_object _x);
int em_coeff(em_object _expr, em_object _x);
int em_coeff_2(em_object _expr, em_object _x, em_object _n);
int em_content(size_t n, ...);
int em_denom(em_object _expr);
int em_divide(size_t n, ...);
int em_eliminate(em_object _eqn, em_object _x);
int em_ezgcd(size_t n, ...);
int em_factor(em_object _expr);
int em_factor_2(em_object _expr, em_object _p);
int em_factorout(em_object _expr, size_t n, ...);
int em_factorsum(em_object _expr);
int em_fasttimes(em_object _p1, em_object _p2);
int em_fullratsimp(em_object _expr);
int em_fullratsubst(em_object _new, em_object _old, em_object _expr);
int em_gcd(size_t n, ...);
int em_gcdex(em_object _f, em_object _g);
int em_gcdex_2(em_object _f, em_object _g, em_object _x);
int em_gcfactor(em_object _n);
int em_gfactor(em_object _expr);
int em_gfactorsum(em_object _expr);
int em_hipow(em_object _expr, em_object _x);
int em_lopow(em_object _expr, em_object _x);
int em_lratsubst(em_object _new, em_object _old, em_object _expr);
int em_lratsubst_2(em_object _subst, em_object _expr);
int em_num(em_object _expr);
int em_polymod(em_object _p);
int em_polymod_2(em_object _p, em_object _m);
int em_polynomialp(em_object _p, em_object _L, em_object _coeffp, em_object _exponp);
int em_polynomialp_2(em_object _p, em_object _L, em_object _coeffp);
int em_polynomialp_3(em_object _p, em_object _L);
int em_quotient(em_object _p1, em_object _p2, size_t n, ...);
int em_rat(em_object _expr);
int em_rat_2(em_object _expr, size_t n, ...);
int em_ratcoef(em_object _expr, em_object _x, em_object _n);
int em_ratcoef_2(em_object _expr, em_object _x);
int em_ratdenom(em_object _expr);
int em_ratdiff(em_object _expr, em_object _x);
int em_ratdisrep(em_object _expr);
int em_ratexpand(em_object _expr);
int em_ratnumer(em_object _expr);
int em_ratp(em_object _expr);
int em_ratsimp(em_object _expr);
int em_ratsimp_2(em_object _expr, size_t n, ...);
int em_ratsubst(em_object _a, em_object _b, em_object _c);
int em_ratvars();
int em_ratvars_2(size_t n, ...);
int em_ratweight();
int em_ratweight_2(size_t n, ...);
int em_remainder(em_object _p1, em_object _p2);
int em_remainder_2(em_object _p1, em_object _p2, size_t n, ...);
int em_resultant(em_object _p1, em_object _p2, em_object _x);
int em_showratvars(em_object _expr);
int em_sqfr(em_object _expr);
int em_tellrat();
int em_tellrat_2(size_t n, ...);
int em_totaldisrep(em_object _expr);
int em_untellrat(size_t n, ...);

// 14.4 Functions and Variables for algebraic extensions
int em_algfac(em_object _f, em_object _p);
int em_algnorm(em_object _f, em_object _p, em_object _a);
int em_algtrace(em_object _f, em_object _p, em_object _a);
int em_bdiscr(em_object _args);
int em_primelmt(em_object _f_b, em_object _p_a, em_object _c);
int em_splitfield(em_object _p, em_object _x);

#ifdef __cplusplus
}
#endif

#endif // POLYLONIALS_H
