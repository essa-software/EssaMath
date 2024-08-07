#include "10_elementary_functions.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>

int em_abs(em_object _z){
    return em_invoke("abs", 1, _z);
}

int em_ceiling(em_object _x){
    return em_invoke("ceiling", 1, _x);
}

int em_entier(em_object _x){
    return em_invoke("entier", 1, _x);
}

int em_floor(em_object _x){
    return em_invoke("floor", 1, _x);
}

int em_fix(em_object _x){
    return em_invoke("fix", 1, _x);
}

int em_hstep(em_object _x){
    return em_invoke("hstep", 1, _x);
}

int em_lmax(em_object _l){
    return em_invoke("lmax", 1, _l);
}

int em_lmin(em_object _l){
    return em_invoke("lmin", 1, _l);
}

int em_max(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("max", n, ptr);
    va_end(ptr);

    return result;
}

int em_min(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("min", n, ptr);
    va_end(ptr);

    return result;
}

int em_round(em_object _x){
    return em_invoke("round", 1, _x);
}

int em_signum(em_object _x){
    return em_invoke("signum", 1, _x);
}

int em_truncate(em_object _x){
    return em_invoke("truncate", 1, _x);
}

int em_cabs(em_object _expr){
    return em_invoke("cabs", 1, _expr);
}

int em_carg(em_object _z){
    return em_invoke("carg", 1, _z);
}

int em_conjugate(em_object _x){
    return em_invoke("conjugate", 1, _x);
}

int em_imagpart(em_object _expr){
    return em_invoke("imagpart", 1, _expr);
}

int em_polarform(em_object _expr){
    return em_invoke("polarform", 1, _expr);
}

int em_realpart(em_object _expr){
    return em_invoke("realpart", 1, _expr);
}

int em_rectform(em_object _expr){
    return em_invoke("rectform", 1, _expr);
}

int em_binomial(em_object _x, em_object _y){
    return em_invoke("binomial", 2, _x, _y);
}

int em_factcomb(em_object _expr){
    return em_invoke("factcomb", 1, _expr);
}

int em_genfact(em_object _x, em_object _y, em_object _z){
    return em_invoke("genfact", 3, _x, _y, _z);
}

int em_minfactorial(em_object _expr){
    return em_invoke("minfactorial", 1, _expr);
}

int em_exp(em_object _x){
    return em_invoke("exp", 1, _x);
}

int em_li(em_object _s, em_object _z){
    return em_invoke("li", 2, _s, _z);
}

int em_log(em_object _x){
    return em_invoke("log", 1, _x);
}

int em_logarc(em_object _expr){
    return em_invoke("logarc", 1, _expr);
}

int em_logcontract(em_object _expr){
    return em_invoke("logcontract", 1, _expr);
}

int em_plog(em_object _x){
    return em_invoke("plog", 1, _x);
}

int em_sqrt(em_object _x){
    return em_invoke("sqrt", 1, _x);
}

int em_acos(em_object _x){
    return em_invoke("acos", 1, _x);
}

int em_acosh(em_object _x){
    return em_invoke("acosh", 1, _x);
}

int em_acot(em_object _x){
    return em_invoke("acot", 1, _x);
}

int em_acoth(em_object _x){
    return em_invoke("acoth", 1, _x);
}

int em_acsc(em_object _x){
    return em_invoke("acsc", 1, _x);
}

int em_acsch(em_object _x){
    return em_invoke("acsch", 1, _x);
}

int em_asec(em_object _x){
    return em_invoke("asec", 1, _x);
}

int em_asech(em_object _x){
    return em_invoke("asech", 1, _x);
}

int em_asin(em_object _x){
    return em_invoke("asin", 1, _x);
}

int em_asinh(em_object _x){
    return em_invoke("asinh", 1, _x);
}

int em_atan(em_object _x){
    return em_invoke("atan", 1, _x);
}

int em_atan2(em_object _y, em_object _x){
    return em_invoke("atan2", 2, _y, _x);
}

int em_atanh(em_object _x){
    return em_invoke("atanh", 1, _x);
}

int em_cos(em_object _x){
    return em_invoke("cos", 1, _x);
}

int em_cosh(em_object _x){
    return em_invoke("cosh", 1, _x);
}

int em_cot(em_object _x){
    return em_invoke("cot", 1, _x);
}

int em_coth(em_object _x){
    return em_invoke("coth", 1, _x);
}

int em_csc(em_object _x){
    return em_invoke("csc", 1, _x);
}

int em_csch(em_object _x){
    return em_invoke("csch", 1, _x);
}

int em_sec(em_object _x){
    return em_invoke("sec", 1, _x);
}

int em_sech(em_object _x){
    return em_invoke("sech", 1, _x);
}

int em_sin(em_object _x){
    return em_invoke("sin", 1, _x);
}

int em_sinh(em_object _x){
    return em_invoke("sinh", 1, _x);
}

int em_tan(em_object _x){
    return em_invoke("tan", 1, _x);
}

int em_tanh(em_object _x){
    return em_invoke("tanh", 1, _x);
}

int em_trigexpand(em_object _expr){
    return em_invoke("trigexpand", 1, _expr);
}

int em_trigreduce(em_object _expr){
    return em_invoke("trigreduce", 1, _expr);
}

int em_trigreduce_2(em_object _expr, em_object _x){
    return em_invoke("trigreduce", 2, _expr, _x);
}

int em_trigsimp(em_object _expr){
    return em_invoke("trigsimp", 1, _expr);
}

int em_trigrat(em_object _expr){
    return em_invoke("trigrat", 1, _expr);
}

int em_make_random_state(em_object _n){
    return em_invoke("make_random_state", 1, _n);
}

int em_set_random_state(em_object _s){
    return em_invoke("set_random_state", 1, _s);
}

int em_random(em_object _x){
    return em_invoke("random", 1, _x);
}
