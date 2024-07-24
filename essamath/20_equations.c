#include "20_equations.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_algsys(em_object _expr, em_object _x){
    return em_invoke("algsys", 2, _expr, _x);
}

int em_allroots(em_object _expr){
    return em_invoke("allroots", 1, _expr);
}

int em_bfallroots(em_object _expr){
    return em_invoke("bfallroots", 1, _expr);
}

int em_dimension(em_object _eqn){
    return em_invoke("dimension", 1, _eqn);
}

int em_dimension_2(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "dimension");

    size_t index = strlen(command);
    command[index++] = '(';
 
    va_start(ptr, n);
    for (size_t i = 0; i < n; i++){
        em_object obj = va_arg(ptr, em_object);
        em_tostring(obj, command + index, size - index);
        index = strlen(command);
        command[index++] = ',';
    }
    command[index - 1] = ')';
 
    // Ending argument list traversal
    va_end(ptr);

    int result =  em_eval(command);
    free(command);

    return result;
}

int em_funcsolve(em_object _eqn, em_object _g){
    return em_invoke("funcsolve", 2, _eqn, _g);
}

int em_ieqn(em_object _ie, em_object _unk, em_object _tech, em_object _n, em_object _guess){
    return em_invoke("ieqn", 5, _ie, _unk, _tech, _n, _guess);
}

int em_lhs(em_object _expr){
    return em_invoke("lhs", 1, _expr);
}

int em_linsolve(em_object _expr, em_object _x){
    return em_invoke("linsolve", 2, _expr, _x);
}

int em_nroots(em_object _p, em_object _low, em_object _high){
    return em_invoke("nroots", 3, _p, _low, _high);
}

int em_nthroot(em_object _p, em_object _n){
    return em_invoke("nthroot", 2, _p, _n);
}

int em_realroots(em_object _expr){
    return em_invoke("realroots", 1, _expr);
}

int em_realroots_2(em_object _expr, em_object _bound){
    return em_invoke("realroots", 2, _expr, _bound);
}

int em_rhs(em_object _expr){
    return em_invoke("rhs", 1, _expr);
}

int em_rootscontract(em_object _expr){
    return em_invoke("rootscontract", 1, _expr);
}

int em_solve(em_object _expr){
    return em_invoke("solve", 1, _expr);
}

int em_solve_2(em_object _expr, em_object _x){
    return em_invoke("solve", 2, _expr, _x);
}
