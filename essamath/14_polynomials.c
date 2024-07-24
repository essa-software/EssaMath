#include "14_polynomials.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_bezout(em_object _p1, em_object _p2, em_object _x){
    return em_invoke("bezout", 3, _p1, _p2, _x);
}

int em_bothcoef(em_object _expr, em_object _x){
    return em_invoke("bothcoef", 2, _expr, _x);
}

int em_coeff(em_object _expr, em_object _x){
    return em_invoke("coeff", 2, _expr, _x);
}

int em_coeff_2(em_object _expr, em_object _x, em_object _n){
    return em_invoke("coeff", 3, _expr, _x, _n);
}

int em_content(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "content");

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

int em_denom(em_object _expr){
    return em_invoke("denom", 1, _expr);
}

int em_divide(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "divide");

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

int em_eliminate(em_object _eqn, em_object _x){
    return em_invoke("eliminate", 2, _eqn, _x);
}

int em_ezgcd(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "ezgcd");

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

int em_factor(em_object _expr){
    return em_invoke("factor", 1, _expr);
}

int em_factor_2(em_object _expr, em_object _p){
    return em_invoke("factor", 2, _expr, _p);
}

int em_factorout(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "factorout");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_expr, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
 
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

int em_factorsum(em_object _expr){
    return em_invoke("factorsum", 1, _expr);
}

int em_fasttimes(em_object _p1, em_object _p2){
    return em_invoke("fasttimes", 2, _p1, _p2);
}

int em_fullratsimp(em_object _expr){
    return em_invoke("fullratsimp", 1, _expr);
}

int em_fullratsubst(em_object _new, em_object _old, em_object _expr){
    return em_invoke("fullratsubst", 3, _new, _old, _expr);
}

int em_gcd(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "gcd");

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

int em_gcdex(em_object _f, em_object _g){
    return em_invoke("gcdex", 2, _f, _g);
}

int em_gcdex_2(em_object _f, em_object _g, em_object _x){
    return em_invoke("gcdex", 3, _f, _g, _x);
}

int em_gcfactor(em_object _n){
    return em_invoke("gcfactor", 1, _n);
}

int em_gfactor(em_object _expr){
    return em_invoke("gfactor", 1, _expr);
}

int em_gfactorsum(em_object _expr){
    return em_invoke("gfactorsum", 1, _expr);
}

int em_hipow(em_object _expr, em_object _x){
    return em_invoke("hipow", 2, _expr, _x);
}

int em_lopow(em_object _expr, em_object _x){
    return em_invoke("lopow", 2, _expr, _x);
}

int em_lratsubst(em_object _new, em_object _old, em_object _expr){
    return em_invoke("lratsubst", 3, _new, _old, _expr);
}

int em_lratsubst_2(em_object _subst, em_object _expr){
    return em_invoke("lratsubst", 2, _subst, _expr);
}

int em_num(em_object _expr){
    return em_invoke("num", 1, _expr);
}

int em_polymod(em_object _p){
    return em_invoke("polymod", 1, _p);
}

int em_polymod_2(em_object _p, em_object _m){
    return em_invoke("polymod", 2, _p, _m);
}

int em_polynomialp(em_object _p, em_object _L, em_object _coeffp, em_object _exponp){
    return em_invoke("polynomialp", 4, _p, _L, _coeffp, _exponp);
}

int em_polynomialp_2(em_object _p, em_object _L, em_object _coeffp){
    return em_invoke("polynomialp", 3, _p, _L, _coeffp);
}

int em_polynomialp_3(em_object _p, em_object _L){
    return em_invoke("polynomialp", 2, _p, _L);
}

int em_quotient(em_object _p1, em_object _p2, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "quotient");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_p1, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
    em_tostring(_p2, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
 
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

int em_rat(em_object _expr){
    return em_invoke("rat", 1, _expr);
}

int em_rat_2(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "rat");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_expr, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
 
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

int em_ratcoef(em_object _expr, em_object _x, em_object _n){
    return em_invoke("ratcoef", 3, _expr, _x, _n);
}

int em_ratcoef_2(em_object _expr, em_object _x){
    return em_invoke("ratcoef", 2, _expr, _x);
}

int em_ratdenom(em_object _expr){
    return em_invoke("ratdenom", 1, _expr);
}

int em_ratdiff(em_object _expr, em_object _x){
    return em_invoke("ratdiff", 2, _expr, _x);
}

int em_ratdisrep(em_object _expr){
    return em_invoke("ratdisrep", 1, _expr);
}

int em_ratexpand(em_object _expr){
    return em_invoke("ratexpand", 1, _expr);
}

int em_ratnumer(em_object _expr){
    return em_invoke("ratnumer", 1, _expr);
}

int em_ratp(em_object _expr){
    return em_invoke("ratp", 1, _expr);
}

int em_ratsimp(em_object _expr){
    return em_invoke("ratsimp", 1, _expr);
}

int em_ratsimp_2(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "ratsimp");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_expr, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
 
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

int em_ratsubst(em_object _a, em_object _b, em_object _c){
    return em_invoke("ratsubst", 3, _a, _b, _c);
}

int em_ratvars(){
    return em_invoke("ratvars", 0);
}

int em_ratvars_2(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "ratvars");

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

int em_ratweight(){
    return em_invoke("ratweight", 0);
}

int em_ratweight_2(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "ratweight");

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

int em_remainder(em_object _p1, em_object _p2){
    return em_invoke("remainder", 2, _p1, _p2);
}

int em_remainder_2(em_object _p1, em_object _p2, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "remainder");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_p1, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
    em_tostring(_p2, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
 
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

int em_resultant(em_object _p1, em_object _p2, em_object _x){
    return em_invoke("resultant", 3, _p1, _p2, _x);
}

int em_showratvars(em_object _expr){
    return em_invoke("showratvars", 1, _expr);
}

int em_sqfr(em_object _expr){
    return em_invoke("sqfr", 1, _expr);
}

int em_tellrat(){
    return em_invoke("tellrat", 0);
}

int em_tellrat_2(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "tellrat");

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

int em_totaldisrep(em_object _expr){
    return em_invoke("totaldisrep", 1, _expr);
}

int em_untellrat(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "untellrat");

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

int em_algfac(em_object _f, em_object _p){
    return em_invoke("algfac", 2, _f, _p);
}

int em_algnorm(em_object _f, em_object _p, em_object _a){
    return em_invoke("algnorm", 3, _f, _p, _a);
}

int em_algtrace(em_object _f, em_object _p, em_object _a){
    return em_invoke("algtrace", 3, _f, _p, _a);
}

int em_bdiscr(em_object _args){
    return em_invoke("bdiscr", 1, _args);
}

int em_primelmt(em_object _f_b, em_object _p_a, em_object _c){
    return em_invoke("primelmt", 3, _f_b, _p_a, _c);
}

int em_splitfield(em_object _p, em_object _x){
    return em_invoke("splitfield", 2, _p, _x);
}
