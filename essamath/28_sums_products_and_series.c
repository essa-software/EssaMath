#include "28_sums_products_and_series.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>

int em_bashindices(em_object _expr){
    return em_invoke("bashindices", 1, _expr);
}

int em_lsum(em_object _expr, em_object _x, em_object _l){
    return em_invoke("lsum", 3, _expr, _x, _l);
}

int em_intosum(em_object _expr){
    return em_invoke("intosum", 1, _expr);
}

int em_product(em_object _expr, em_object _i, em_object _i0, em_object _i1){
    return em_invoke("product", 4, _expr, _i, _i0, _i1);
}

int em_sum(em_object _expr, em_object _i, em_object _i0, em_object _i1){
    return em_invoke("sum", 4, _expr, _i, _i0, _i1);
}

int em_sumcontract(em_object _expr){
    return em_invoke("sumcontract", 1, _expr);
}

int em_deftaylor(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("deftaylor", n, ptr);
    va_end(ptr);

    return result;
}

int em_niceindices(em_object _expr){
    return em_invoke("niceindices", 1, _expr);
}

int em_nusum(em_object _expr, em_object _i, em_object _i0, em_object _i1){
    return em_invoke("nusum", 4, _expr, _i, _i0, _i1);
}

int em_pade(em_object _taylor_series, em_object _numer_deg_bound, em_object _denom_deg_bound){
    return em_invoke("pade", 3, _taylor_series, _numer_deg_bound, _denom_deg_bound);
}

int em_powerseries(em_object _expr, em_object _x, em_object _a){
    return em_invoke("powerseries", 3, _expr, _x, _a);
}

int em_revert(em_object _expr, em_object _x){
    return em_invoke("revert", 2, _expr, _x);
}

int em_revert2(em_object _expr, em_object _x, em_object _n){
    return em_invoke("revert2", 3, _expr, _x, _n);
}

int em_taylor(em_object _expr, em_object _x, em_object _a, em_object _n){
    return em_invoke("taylor", 4, _expr, _x, _a, _n);
}

int em_taylor_2(em_object _expr, em_object _args){
    return em_invoke("taylor", 2, _expr, _args);
}

int em_taylor_3(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("taylor", n, ptr);
    va_end(ptr);

    return result;
}

int em_taylorinfo(em_object _expr){
    return em_invoke("taylorinfo", 1, _expr);
}

int em_taylorp(em_object _expr){
    return em_invoke("taylorp", 1, _expr);
}

int em_taylor_simplifier(em_object _expr){
    return em_invoke("taylor_simplifier", 1, _expr);
}

int em_taytorat(em_object _expr){
    return em_invoke("taytorat", 1, _expr);
}

int em_trunc(em_object _expr){
    return em_invoke("trunc", 1, _expr);
}

int em_unsum(em_object _f, em_object _n){
    return em_invoke("unsum", 2, _f, _n);
}

int em_equalp(em_object _x, em_object _y){
    return em_invoke("equalp", 2, _x, _y);
}

int em_remfun(em_object _f, em_object _expr){
    return em_invoke("remfun", 2, _f, _expr);
}

int em_remfun_2(em_object _f, em_object _expr, em_object _x){
    return em_invoke("remfun", 3, _f, _expr, _x);
}

int em_funp(em_object _f, em_object _expr){
    return em_invoke("funp", 2, _f, _expr);
}

int em_funp_2(em_object _f, em_object _expr, em_object _x){
    return em_invoke("funp", 3, _f, _expr, _x);
}

int em_absint(em_object _f, em_object _x){
    return em_invoke("absint", 2, _f, _x);
}

int em_absint_2(em_object _f, em_object _x, em_object _halfplane){
    return em_invoke("absint", 3, _f, _x, _halfplane);
}

int em_absint_3(em_object _f, em_object _x, em_object _a, em_object _b){
    return em_invoke("absint", 4, _f, _x, _a, _b);
}

int em_fourier(em_object _f, em_object _x, em_object _p){
    return em_invoke("fourier", 3, _f, _x, _p);
}

int em_foursimp(em_object _l){
    return em_invoke("foursimp", 1, _l);
}

int em_fourexpand(em_object _l, em_object _x, em_object _p, em_object _limit){
    return em_invoke("fourexpand", 4, _l, _x, _p, _limit);
}

int em_fourcos(em_object _f, em_object _x, em_object _p){
    return em_invoke("fourcos", 3, _f, _x, _p);
}

int em_foursin(em_object _f, em_object _x, em_object _p){
    return em_invoke("foursin", 3, _f, _x, _p);
}

int em_totalfourier(em_object _f, em_object _x, em_object _p){
    return em_invoke("totalfourier", 3, _f, _x, _p);
}

int em_fourint(em_object _f, em_object _x){
    return em_invoke("fourint", 2, _f, _x);
}

int em_fourintcos(em_object _f, em_object _x){
    return em_invoke("fourintcos", 2, _f, _x);
}

int em_fourintsin(em_object _f, em_object _x){
    return em_invoke("fourintsin", 2, _f, _x);
}

int em_intopois(em_object _a){
    return em_invoke("intopois", 1, _a);
}

int em_outofpois(em_object _a){
    return em_invoke("outofpois", 1, _a);
}

int em_poisdiff(em_object _a, em_object _b){
    return em_invoke("poisdiff", 2, _a, _b);
}

int em_poisexpt(em_object _a, em_object _b){
    return em_invoke("poisexpt", 2, _a, _b);
}

int em_poisint(em_object _a, em_object _b){
    return em_invoke("poisint", 2, _a, _b);
}

int em_poismap(em_object _series, em_object _sinfn, em_object _cosfn){
    return em_invoke("poismap", 3, _series, _sinfn, _cosfn);
}

int em_poisplus(em_object _a, em_object _b){
    return em_invoke("poisplus", 2, _a, _b);
}

int em_poissimp(em_object _a){
    return em_invoke("poissimp", 1, _a);
}

int em_poissubst(em_object _a, em_object _b, em_object _c){
    return em_invoke("poissubst", 3, _a, _b, _c);
}

int em_poistimes(em_object _a, em_object _b){
    return em_invoke("poistimes", 2, _a, _b);
}

int em_poistrim(void){
    return em_invoke("poistrim", 0);
}

int em_printpois(em_object _a){
    return em_invoke("printpois", 1, _a);
}
