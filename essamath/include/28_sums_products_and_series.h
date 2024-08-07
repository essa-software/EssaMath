// 28_sums_products_and_series.h
#include "expression.h"
#ifndef SUMS_PRODUCTS_AND_SERIES_H
#define SUMS_PRODUCTS_AND_SERIES_H

#ifdef __cplusplus
extern "C" {
#endif

int em_bashindices(em_object _expr);
int em_lsum(em_object _expr, em_object _x, em_object _l);
int em_intosum(em_object _expr);
int em_product(em_object _expr, em_object _i, em_object _i0, em_object _i1);
int em_sum(em_object _expr, em_object _i, em_object _i0, em_object _i1);
int em_sumcontract(em_object _expr);
int em_deftaylor(size_t n, ...);
int em_niceindices(em_object _expr);
int em_nusum(em_object _expr, em_object _i, em_object _i0, em_object _i1);
int em_pade(em_object _taylor_series, em_object _numer_deg_bound, em_object _denom_deg_bound);
int em_powerseries(em_object _expr, em_object _x, em_object _a);
int em_revert(em_object _expr, em_object _x);
int em_revert2(em_object _expr, em_object _x, em_object _n);
int em_taylor(em_object _expr, em_object _x, em_object _a, em_object _n);
int em_taylor_2(em_object _expr, em_object _args);
int em_taylor_3(size_t n, ...);
int em_taylorinfo(em_object _expr);
int em_taylorp(em_object _expr);
int em_taylor_simplifier(em_object _expr);
int em_taytorat(em_object _expr);
int em_trunc(em_object _expr);
int em_unsum(em_object _f, em_object _n);
int em_equalp(em_object _x, em_object _y);
int em_remfun(em_object _f, em_object _expr);
int em_remfun_2(em_object _f, em_object _expr, em_object _x);
int em_funp(em_object _f, em_object _expr);
int em_funp_2(em_object _f, em_object _expr, em_object _x);
int em_absint(em_object _f, em_object _x);
int em_absint_2(em_object _f, em_object _x, em_object _halfplane);
int em_absint_3(em_object _f, em_object _x, em_object _a, em_object _b);
int em_fourier(em_object _f, em_object _x, em_object _p);
int em_foursimp(em_object _l);
int em_fourexpand(em_object _l, em_object _x, em_object _p, em_object _limit);
int em_fourcos(em_object _f, em_object _x, em_object _p);
int em_foursin(em_object _f, em_object _x, em_object _p);
int em_totalfourier(em_object _f, em_object _x, em_object _p);
int em_fourint(em_object _f, em_object _x);
int em_fourintcos(em_object _f, em_object _x);
int em_fourintsin(em_object _f, em_object _x);
int em_intopois(em_object _a);
int em_outofpois(em_object _a);
int em_poisdiff(em_object _a, em_object _b);
int em_poisexpt(em_object _a, em_object _b);
int em_poisint(em_object _a, em_object _b);
int em_poismap(em_object _series, em_object _sinfn, em_object _cosfn);
int em_poisplus(em_object _a, em_object _b);
int em_poissimp(em_object _a);
int em_poissubst(em_object _a, em_object _b, em_object _c);
int em_poistimes(em_object _a, em_object _b);
int em_poistrim(void);
int em_printpois(em_object _a);

#ifdef __cplusplus
}
#endif

#endif // SUMS_PRODUCTS_AND_SERIES_H
