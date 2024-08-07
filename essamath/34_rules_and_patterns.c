#include "34_rules_and_patterns.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>

int em_apply1(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("apply1", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_apply2(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("apply2", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_applyb1(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("applyb1", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_defmatch(em_object _progname, em_object _pattern){
    return em_invoke("defmatch", 2, _progname, _pattern);
}

int em_defmatch_2(em_object _progname, em_object _pattern, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("defmatch", n + 2, _progname, _pattern, ptr);
    va_end(ptr);

    return result;
}

int em_defrule(em_object _rulename, em_object _pattern, em_object _replacement){
    return em_invoke("defrule", 3, _rulename, _pattern, _replacement);
}

int em_disprule(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("disprule", n, ptr);
    va_end(ptr);

    return result;
}

int em_dispruleall(void){
    return em_invoke("disprule", 1, em_createstring("all"));
}

int em_let(em_object _prod, em_object _repl, em_object _prodname, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("let", n + 3, _prod, _repl, _prodname, ptr);
    va_end(ptr);

    return result;
}

int em_let_2(em_object _args, em_object _package_name){
    return em_invoke("let", 2, _args, _package_name);
}

int em_letrules(void){
    return em_invoke("letrules", 0);
}

int em_letrules_2(em_object _package_name){
    return em_invoke("letrules", 1, _package_name);
}

int em_letsimp(em_object _expr){
    return em_invoke("letsimp", 1, _expr);
}

int em_letsimp_2(em_object _expr, em_object _package_name){
    return em_invoke("letsimp", 2, _expr, _package_name);
}

int em_letsimp_3(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("letsimp", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_matchdeclare(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("concat", n, ptr);
    va_end(ptr);

    return result;
}

int em_remlet(void){
    return em_invoke("remlet", 0);
}

int em_remlet_2(em_object _prod, em_object _name){
    return em_invoke("remlet", 2, _prod, _name);
}

int em_remletall(void){
    return em_invoke("remlet", 1, em_createstring("all"));
}

int em_remletall_2(em_object _name){
    return em_invoke("remlet", 2, em_createstring("all"), _name);
}

int em_remrule(em_object _op, em_object _rulename){
    return em_invoke("remrule", 2, _op, _rulename);
}

int em_remruleall(em_object _op){
    return em_invoke("remrule", 2, _op, em_createstring("all"));
}

int em_tellsimp(em_object _pattern, em_object _replacement){
    return em_invoke("tellsimp", 2, _pattern, _replacement);
}

int em_tellsimpafter(em_object _pattern, em_object _replacement){
    return em_invoke("tellsimpafter", 2, _pattern, _replacement);
}

int em_clear_rules(void){
    return em_invoke("clear_rules", 0);
}
