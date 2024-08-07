#include "24_affine.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>

int em_fast_linsolve(em_object _expr, em_object _x){
    return em_invoke("fast_linsolve", 2, _expr, _x);
}

int em_grobner_basis(em_object _expr){
    return em_invoke("grobner_basis", 1, _expr);
}

int em_set_up_dot_simplifications(em_object _eqns){
    return em_invoke("set_up_dot_simplifications", 1, _eqns);
}

int em_set_up_dot_simplifications_2(em_object _eqns, em_object _check_through_degree){
    return em_invoke("set_up_dot_simplifications", 2, _eqns, _check_through_degree);
}

int em_declare_weights(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("declare_weights", n, ptr);
    va_end(ptr);

    return result;
}

int em_nc_degree(em_object _p){
    return em_invoke("nc_degree", 1, _p);
}

int em_dotsimp(em_object _f){
    return em_invoke("dotsimp", 1, _f);
}

int em_fast_central_elements(em_object _x, em_object _n){
    return em_invoke("fast_central_elements", 2, _x, _n);
}

int em_check_overlaps(em_object _n, em_object _add_to_simps){
    return em_invoke("check_overlaps", 2, _n, _add_to_simps);
}

int em_mono(em_object _x, em_object _n){
    return em_invoke("mono", 2, _x, _n);
}

int em_monomial_dimensions(em_object _n){
    return em_invoke("monomial_dimensions", 1, _n);
}

int em_extract_linear_equations(em_object _p, em_object _m){
    return em_invoke("extract_linear_equations", 2, _p, _m);
}

int em_list_nc_monomials(em_object _p){
    return em_invoke("list_nc_monomials", 1, _p);
}
