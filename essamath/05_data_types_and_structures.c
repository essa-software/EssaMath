#include "05_data_types_and_structures.h"
#include "expression.h"
#include <stdarg.h>

int em_bfloat(em_object _expr){
    return em_invoke("bfloat", 1, _expr);
}

int em_bfloatp(em_object _expr){
    return em_invoke("bfloatp", 1, _expr);
}

int em_bigfloat_bits(void){
    return em_invoke("bigfloat_bits", 0);
}

int em_bigfloat_eps(void){
    return em_invoke("bigfloat_eps", 0);
}

int em_decode_float(em_object _f){
    return em_invoke("decode_float", 1, _f);
}

int em_evenp(em_object _expr){
    return em_invoke("evenp", 1, _expr);
}

int em_float(em_object _expr){
    return em_invoke("float", 1, _expr);
}

int em_float_bits(void){
    return em_invoke("float_bits", 0);
}

int em_float_eps(void){
    return em_invoke("float_eps", 0);
}

int em_float_precision(em_object _f){
    return em_invoke("float_precision", 1, _f);
}

int em_floatnump(em_object _expr){
    return em_invoke("floatnump", 1, _expr);
}

int em_integerp(em_object _expr){
    return em_invoke("integerp", 1, _expr);
}

int em_integer_decode_float(em_object _f){
    return em_invoke("integer_decode_float", 1, _f);
}

int em_is_power_of_two(em_object _n){
    return em_invoke("is_power_of_two", 1, _n);
}

int em_nonnegintegerp(em_object _n){
    return em_invoke("nonnegintegerp", 1, _n);
}

int em_numberp(em_object _expr){
    return em_invoke("numberp", 1, _expr);
}

int em_numerval(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("numerval", n, ptr);
    va_end(ptr);

    return result;
}

int em_oddp(em_object _expr){
    return em_invoke("numberp", 1, _expr);
}

int em_rationalize(em_object _expr){
    return em_invoke("rationalize", 1, _expr);
}

int em_ratnump(em_object _expr){
    return em_invoke("ratnump", 1, _expr);
}

int em_scale_float(em_object _f, em_object _n){
    return em_invoke("scale_float", 2, _f, _n);
}

int em_unit_in_last_plase(em_object _n){
    return em_invoke("unit_in_last_plase", 1, _n);
}

int em_concat(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("concat", n, ptr);
    va_end(ptr);

    return result;
}

int em_sconcat(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("sconcat", n, ptr);
    va_end(ptr);

    return result;
}

int em_string(em_object _expr){
    return em_invoke("string", 1, _expr);
}

int em_append(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("append", n, ptr);
    va_end(ptr);

    return result;
}

int em_assoc(em_object _key, em_object _e, em_object _default){
    return em_invoke("assoc", 3, _key, _e, _default);
}

int em_assoc_2(em_object _key, em_object _e){
    return em_invoke("assoc", 2, _key, _e);
}

int em_cons(em_object _expr1, em_object _expr2){
    return em_invoke("cons", 2, _expr1, _expr2);
}

int em_copylist(em_object _list){
    return em_invoke("copylist", 1, _list);
}

int em_create_list(em_object _form, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("create_list", n + 1, _form, ptr);
    va_end(ptr);

    return result;
}

int em_delete(em_object _expr1, em_object _expr2){
    return em_invoke("delete", 2, _expr1, _expr2);
}

int em_delete_2(em_object _expr1, em_object _expr2, em_object _n){
    return em_invoke("delete", 3, _expr1, _expr2, _n);
}

int em_eighth(em_object _expr){
    return em_invoke("eighth", 1, _expr);
}

int em_endcons(em_object _expr1, em_object _expr2){
    return em_invoke("endcons", 2, _expr1, _expr2);
}

int em_fifth(em_object _expr){
    return em_invoke("fifth", 1, _expr);
}

int em_first(em_object _expr){
    return em_invoke("first", 1, _expr);
}

int em_firstn(em_object _expr, em_object _n){
    return em_invoke("firstn", 2, _expr, _n);
}

int em_fourth(em_object _expr){
    return em_invoke("fourth", 1, _expr);
}

int em_join(em_object _l, em_object _m){
    return em_invoke("join", 2, _l, _m);
}

int em_last(em_object _expr){
    return em_invoke("last", 1, _expr);
}

int em_lastn(em_object _expr, em_object _n){
    return em_invoke("lastn", 2, _expr, _n);
}

int em_length(em_object _expr){
    return em_invoke("length", 1, _expr);
}

int em_listp(em_object _expr){
    return em_invoke("listp", 1, _expr);
}
int em_lreduce(em_object _f, em_object _s){
    return em_invoke("lreduce", 2, _f, _s);
}

int em_lreduce_2(em_object _f, em_object _s, em_object _s_0){
    return em_invoke("lreduce", 2, _f, _s, _s_0);
}

int em_makelist(void){
    return em_invoke("makelist", 0);
}

int em_makelist_2(em_object _expr, em_object _n){
    return em_invoke("makelist", 2, _expr, _n);
}

int em_makelist_3(em_object _expr, em_object _i, em_object _i_max){
    return em_invoke("makelist", 3, _expr, _i, _i_max);
}

int em_makelist_4(em_object _expr, em_object _i, em_object _i_0, em_object _i_max){
    return em_invoke("makelist", 4, _expr, _i, _i_0, _i_max);
}

int em_makelist_5(em_object _expr, em_object _i, em_object _i_0, em_object _i_max, em_object _step){
    return em_invoke("makelist", 5, _expr, _i, _i_0, _i_max, _step);
}

int em_member(em_object _expr1, em_object _expr2){
    return em_invoke("member", 2, _expr1, _expr2);
}

int em_ninth(em_object _expr){
    return em_invoke("ninth", 1, _expr);
}

int em_pop(em_object _expr){
    return em_invoke("pop", 1, _expr);
}

int em_push(em_object _item, em_object _list){
    return em_invoke("push", 2, _item, _list);
}

int em_rest(em_object _expr, em_object _n){
    return em_invoke("rest", 2, _expr, _n);
}

int em_rest_2(em_object _expr){
    return em_invoke("rest", 1, _expr);
}

int em_reverse(em_object _list){
    return em_invoke("reverse", 1, _list);
}

int em_rreduce(em_object _f, em_object _s){
    return em_invoke("rreduce", 2, _f, _s);
}

int em_rreduce_2(em_object _f, em_object _s, em_object _s_np1){
    return em_invoke("rreduce", 3, _f, _s, _s_np1);
}

int em_second(em_object _expr){
    return em_invoke("second", 1, _expr);
}

int em_seventh(em_object _expr){
    return em_invoke("seventh", 1, _expr);
}

int em_sixth(em_object _expr){
    return em_invoke("sixth", 1, _expr);
}

int em_sort(em_object _l, em_object _p){
    return em_invoke("sort", 2, _l, _p);
}

int em_sort_2(em_object _l){
    return em_invoke("sort", 1, _l);
}

int em_sublist(em_object _list, em_object _p){
    return em_invoke("sublist", 2, _list, _p);
}

int em_sublist_indices(em_object _l, em_object _p){
    return em_invoke("sublist_indices", 2, _l, _p);
}

int em_tenth(em_object _expr){
    return em_invoke("tenth", 1, _expr);
}

int em_tree_reduce(em_object _f, em_object _s){
    return em_invoke("tree_reduce", 2, _f, _s);
}

int em_tree_reduce_2(em_object _f, em_object _s, em_object _s_0){
    return em_invoke("tree_reduce", 3, _f, _s, _s_0);
}

int em_unique(em_object _l){
    return em_invoke("unique", 1, _l);
}

int em_xreduce(em_object _f, em_object _s){
    return em_invoke("xreduce", 2, _f, _s);
}

int em_xreduce_2(em_object _f, em_object _s, em_object _s_0){
    return em_invoke("xreduce", 3, _f, _s, _s_0);
}

int em_array(em_object _names, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("array", n + 1, _names, ptr);
    va_end(ptr);

    return result;
}

int em_array_2(em_object _name, em_object _type, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("array", n + 2, _name, _type, ptr);
    va_end(ptr);

    return result;
}

int em_arrayapply(em_object _a, em_object _i){
    return em_invoke("arrayapply", 2, _a, _i);
}

int em_arrayinfo(em_object _a){
    return em_invoke("arrayinfo", 1, _a);
}

int em_arraymake(em_object _a, em_object _i){
    return em_invoke("arraymake", 2, _a, _i);
}

int em_arraysetapply(em_object _a, em_object _i, em_object _x){
    return em_invoke("arraysetapply", 3, _a, _i, _x);
}

int em_fillarray(em_object _a, em_object _b){
    return em_invoke("fillarray", 2, _a, _b);
}

int em_listarray(em_object _a){
    return em_invoke("listarray", 1, _a);
}

int em_make_array(em_object _type, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("make_array", n + 1, _type, ptr);
    va_end(ptr);

    return result;
}

int em_rearray(em_object _a, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("rearray", n + 1, _a, ptr);
    va_end(ptr);

    return result;
}

int em_remarray(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("remarray", n, ptr);
    va_end(ptr);

    return result;
}

int em_remarrayall(void){
    return em_eval("remarray(all)");
}

int em_subvar(em_object _x, em_object _i){
    return em_invoke("subvar", 2, _x, _i);
}

int em_subvarp(em_object _expr){
    return em_invoke("subvarp", 1, _expr);
}

int em_defstruct(em_object _s){
    return em_invoke("defstruct", 1, _s);
}

int em_new(em_object _s){
    return em_invoke("new", 1, _s);
}
