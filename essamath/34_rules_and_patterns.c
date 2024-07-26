#include "34_rules_and_patterns.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_apply1(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "apply1");

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

int em_apply2(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "apply2");

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

int em_applyb1(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "applyb1");

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

int em_defmatch(em_object _progname, em_object _pattern){
    return em_invoke("defmatch", 2, _progname, _pattern);
}

int em_defmatch_2(em_object _progname, em_object _pattern, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "applyb1");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_progname, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
    em_tostring(_pattern, command + index, size - index);
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

int em_defrule(em_object _rulename, em_object _pattern, em_object _replacement){
    return em_invoke("defrule", 3, _rulename, _pattern, _replacement);
}

int em_disprule(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "disprule");

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

int em_dispruleall(){
    return em_invoke("disprule", 1, em_create_string("all"));
}

int em_let(em_object _prod, em_object _repl, em_object _prodname, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "let");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_prod, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
    em_tostring(_repl, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
    em_tostring(_prodname, command + index, size - index);
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

int em_let_2(em_object _args, em_object _package_name){
    return em_invoke("let", 2, _args, _package_name);
}

int em_letrules(){
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
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "letsimp");

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

int em_matchdeclare(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "matchdeclare");

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

int em_remlet(){
    return em_invoke("remlet", 0);
}

int em_remlet_2(em_object _prod, em_object _name){
    return em_invoke("remlet", 2, _prod, _name);
}

int em_remletall(){
    return em_invoke("remlet", 1, em_create_string("all"));
}

int em_remletall_2(em_object _name){
    return em_invoke("remlet", 2, em_create_string("all"), _name);
}

int em_remrule(em_object _op, em_object _rulename){
    return em_invoke("remrule", 2, _op, _rulename);
}

int em_remruleall(em_object _op){
    return em_invoke("remrule", 2, _op, em_create_string("all"));
}

int em_tellsimp(em_object _pattern, em_object _replacement){
    return em_invoke("tellsimp", 2, _pattern, _replacement);
}

int em_tellsimpafter(em_object _pattern, em_object _replacement){
    return em_invoke("tellsimpafter", 2, _pattern, _replacement);
}

int em_clear_rules(){
    return em_invoke("clear_rules", 0);
}
