#include "29_number_theory.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_bern(em_object _n){
    return em_invoke("bern", 1, _n);
}

int em_bernpoly(em_object _x, em_object _n){
    return em_invoke("bernpoly", 2, _x, _n);
}

int em_bfzeta(em_object _s, em_object _n){
    return em_invoke("bfzeta", 2, _s, _n);
}

int em_bfhzeta(em_object _s, em_object _h, em_object _n){
    return em_invoke("bfhzeta", 3, _s, _h, _n);
}

int em_burn(em_object _n){
    return em_invoke("burn", 1, _n);
}

int em_chinese(em_object _r, em_object _m){
    return em_invoke("chinese", 2, _r, _m);
}

int em_cf(em_object _expr){
    return em_invoke("cf", 1, _expr);
}

int em_cfdisrep(em_object _list){
    return em_invoke("cfdisrep", 1, _list);
}

int em_cfexpand(em_object _x){
    return em_invoke("cfexpand", 1, _x);
}

int em_divsum(em_object _n){
    return em_invoke("divsum", 1, _n);
}

int em_divsum_2(em_object _n, em_object _k){
    return em_invoke("divsum", 2, _n, _k);
}

int em_euler(em_object _n){
    return em_invoke("euler", 1, _n);
}

int em_fib(em_object _n){
    return em_invoke("fib", 1, _n);
}

int em_fibtophi(em_object _expr){
    return em_invoke("fibtophi", 1, _expr);
}

int em_ifactors(em_object _n){
    return em_invoke("ifactors", 1, _n);
}

int em_igcdex(em_object _n, em_object _k){
    return em_invoke("igcdex", 2, _n, _k);
}

int em_inrt(em_object _x, em_object _n){
    return em_invoke("inrt", 2, _x, _n);
}

int em_inv_mod(em_object _n, em_object _m){
    return em_invoke("inv_mod", 2, _n, _m);
}

int em_isqrt(em_object _x){
    return em_invoke("isqrt", 1, _x);
}

int em_jacobi(em_object _p, em_object _q){
    return em_invoke("jacobi", 2, _p, _q);
}

int em_lcm(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "lcm");

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

int em_lucas(em_object _n){
    return em_invoke("lucas", 1, _n);
}

int em_mod(em_object _x, em_object _y){
    return em_invoke("mod", 2, _x, _y);
}

int em_next_prime(em_object _n){
    return em_invoke("next_prime", 1, _n);
}

int em_partfrac(em_object _expr, em_object _var){
    return em_invoke("partfrac", 1, _expr);
}

int em_power_mod(em_object _a, em_object _n, em_object _m){
    return em_invoke("power_mod", 3, _a, _n, _m);
}

int em_primep(em_object _n){
    return em_invoke("primep", 1, _n);
}

int em_primes(em_object _start, em_object _end){
    return em_invoke("primes", 2, _start, _end);
}

int em_prev_prime(em_object _n){
    return em_invoke("prev_prime", 1, _n);
}

int em_qunit(em_object _n){
    return em_invoke("qunit", 1, _n);
}

int em_totient(em_object _n){
    return em_invoke("totient", 1, _n);
}

int em_zeta(em_object _n){
    return em_invoke("zeta", 1, _n);
}

int em_zn_add_table(em_object _n){
    return em_invoke("zn_add_table", 1, _n);
}

int em_zn_characteristic_factors(em_object _n){
    return em_invoke("zn_characteristic_factors", 1, _n);
}

int em_zn_carmichael_lambda(em_object _n){
    return em_invoke("zn_carmichael_lambda", 1, _n);
}

int em_zn_determinant(em_object _matrix, em_object _p){
    return em_invoke("zn_determinant", 2, _matrix, _p);
}

int em_zn_factor_generators(em_object _n){
    return em_invoke("zn_factor_generators", 1, _n);
}

int em_zn_invert_by_lu(em_object _matrix, em_object _p){
    return em_invoke("zn_invert_by_lu", 2, _matrix, _p);
}

int em_zn_log(em_object _a, em_object _g, em_object _n){
    return em_invoke("zn_log", 3, _a, _g, _n);
}

int em_zn_mult_table(em_object _n){
    return em_invoke("zn_mult_table", 1, _n);
}

int em_zn_mult_table_2(em_object _n, em_object _gcd){
    return em_invoke("zn_mult_table_2", 2, _n, _gcd);
}

int em_zn_nth_root(em_object _x, em_object _n, em_object _m){
    return em_invoke("zn_nth_root", 3, _x, _n, _m);
}

int em_zn_nth_root_2(em_object _x, em_object _n, em_object _m, em_object _p_e){
    return em_invoke("zn_nth_root_2", 4, _x, _n, _m, _p_e);
}

int em_zn_order(em_object _x, em_object _n){
    return em_invoke("zn_order", 2, _x, _n);
}

int em_zn_order_2(em_object _x, em_object _n, em_object _p_e){
    return em_invoke("zn_order_2", 3, _x, _n, _p_e);
}

int em_zn_power_table(em_object _n){
    return em_invoke("zn_power_table", 1, _n);
}

int em_zn_power_table_2(em_object _n, em_object _gcd){
    return em_invoke("zn_power_table_2", 2, _n, _gcd);
}

int em_zn_power_table_3(em_object _n, em_object _gcd, em_object _max_exp){
    return em_invoke("zn_power_table_3", 3, _n, _gcd, _max_exp);
}

int em_zn_primroot(em_object _n){
    return em_invoke("zn_primroot", 1, _n);
}

int em_zn_primroot_2(em_object _n, em_object _p_e){
    return em_invoke("zn_primroot_2", 2, _n, _p_e);
}

int em_zn_primroot_p(em_object _x, em_object _n){
    return em_invoke("zn_primroot_p", 2, _x, _n);
}

int em_zn_primroot_p_2(em_object _x, em_object _n, em_object _p_e){
    return em_invoke("zn_primroot_p_2", 3, _x, _n, _p_e);
}
