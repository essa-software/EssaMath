#include "11_maximas_database.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>

int em_constantp(em_object _expr){
    return em_invoke("constantp", 1, _expr);
}

int em_declare(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("declare", n, ptr);
    va_end(ptr);

    return result;
}

int em_featurep(em_object _a, em_object _f){
    return em_invoke("featurep", 2, _a, _f);
}

int em_get(em_object _a, em_object _i){
    return em_invoke("get", 2, _a, _i);
}

int em_nonscalarp(em_object _expr){
    return em_invoke("nonscalarp", 1, _expr);
}

int em_printprops(em_object _a, em_object _i){
    return em_invoke("printprops", 2, _a, _i);
}

int em_properties(em_object _a){
    return em_invoke("properties", 1, _a);
}

int em_propvars(em_object _prop){
    return em_invoke("propvars", 1, _prop);
}

int em_put(em_object _atom, em_object _value, em_object _indicator){
    return em_invoke("put", 3, _atom, _value, _indicator);
}

int em_qput(em_object _atom, em_object _value, em_object _indicator){
    return em_invoke("qput", 3, _atom, _value, _indicator);
}

int em_rem(em_object _atom, em_object _indicator){
    return em_invoke("rem", 2, _atom, _indicator);
}

int em_remove(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("remove", n, ptr);
    va_end(ptr);

    return result;
}

int em_scalarp(em_object _expr){
    return em_invoke("scalarp", 1, _expr);
}

int em_activate(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("activate", n, ptr);
    va_end(ptr);

    return result;
}

int em_askequal(em_object _expr1, em_object _expr2){
    return em_invoke("askequal", 2, _expr1, _expr2);
}

int em_askinteger(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("askinteger", n, ptr);
    va_end(ptr);

    return result;
}

int em_asksign(em_object _expr){
    return em_invoke("asksign", 1, _expr);
}

int em_assume(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("assume", n, ptr);
    va_end(ptr);

    return result;
}

int em_deactivate(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("deactivate", n, ptr);
    va_end(ptr);

    return result;
}

int em_facts(void){
    return em_invoke("facts", 0);
}

int em_facts_2(em_object _item){
    return em_invoke("facts", 1, _item);
}

int em_forget(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("forget", n, ptr);
    va_end(ptr);

    return result;
}

int em_forget_2(em_object _l){
    return em_invoke("forget", 1, _l);
}

int em_is(em_object _expr){
    return em_invoke("is", 1, _expr);
}

int em_killcontext(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("killcontext", n, ptr);
    va_end(ptr);

    return result;
}

int em_maybe(em_object _expr){
    return em_invoke("maybe", 1, _expr);
}

int em_newcontext(em_object _name){
    return em_invoke("newcontext", 1, _name);
}

int em_newcontext_2(void){
    return em_invoke("newcontext", 0);
}

int em_sign(em_object _expr){
    return em_invoke("sign", 1, _expr);
}

int em_supcontext(em_object _name, em_object _context){
    return em_invoke("supcontext", 2, _name, _context);
}

int em_supcontext_2(em_object _name){
    return em_invoke("supcontext", 1, _name);
}

int em_supcontext_3(void){
    return em_invoke("supcontext", 0);
}

int em_charfun(em_object _p){
    return em_invoke("charfun", 1, _p);
}

int em_compare(em_object _x, em_object _y){
    return em_invoke("compare", 2, _x, _y);
}

int em_equal(em_object _a, em_object _b){
    return em_invoke("equal", 2, _a, _b);
}

int em_notequal(em_object _a, em_object _b){
    return em_invoke("notequal", 2, _a, _b);
}

int em_unknown(em_object _expr){
    return em_invoke("unknown", 1, _expr);
}

int em_zeroequiv(em_object _expr, em_object _v){
    return em_invoke("zeroequiv", 2, _expr, _v);
}
