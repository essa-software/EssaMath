#include "09_simplification.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>

int em_combine(em_object _expr){
    return em_invoke("combine", 1, _expr);
}

int em_demoivre(em_object _expr){
    return em_invoke("demoivre", 1, _expr);
}

int em_distrib(em_object _expr){
    return em_invoke("distrib", 1, _expr);
}

int em_expand(em_object _expr){
    return em_invoke("expand", 1, _expr);
}

int em_expand_2(em_object _expr, em_object _p, em_object _n){
    return em_invoke("expand", 3, _expr, _p, _n);
}

int em_expandwrt(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("expandwrt", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_exponentialize(em_object _expr){
    return em_invoke("exponentialize", 1, _expr);
}

int em_multthru(em_object _expr){
    return em_invoke("multthru", 1, _expr);
}

int em_multthru_2(em_object _expr1, em_object _expr2){
    return em_invoke("multthru", 2, _expr1, _expr2);
}

int em_define_opproperty(em_object _property_name, em_object _simplifier_fn){
    return em_invoke("define_opproperty", 2, _property_name, _simplifier_fn);
}

int em_radcan(em_object _expr){
    return em_invoke("radcan", 1, _expr);
}

int em_scsimp(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("scsimp", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_xthru(em_object _expr){
    return em_invoke("xthru", 1, _expr);
}
