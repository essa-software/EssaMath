#include "18_differentiation.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_antid(em_object _expr, em_object _x, em_object _u){
    return em_invoke("antid", 3, _expr, _x, _u);
}

int em_antidiff(em_object _expr, em_object _x, em_object _u){
    return em_invoke("antidiff", 3, _expr, _x, _u);
}

int em_at(em_object _expr, em_object _eqn){
    return em_invoke("at", 2, _expr, _eqn);
}

int em_atvalue(em_object _expr, em_object _x, em_object _c){
    return em_invoke("atvalue", 3, _expr, _x, _c);
}

int em_init_cartan(em_object _x){
    return em_invoke("init_cartan", 1, _x);
}

int em_del(em_object _x){
    return em_invoke("del", 1, _x);
}

int em_delta(em_object _t){
    return em_invoke("delta", 1, _t);
}

int em_dependencies(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "dependencies");

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

int em_depends(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "depends");

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

int em_derivdegree(em_object _expr, em_object _y, em_object _x){
    return em_invoke("derivdegree", 3, _expr, _y, _x);
}

int em_derivlist(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "derivlist");

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

int em_diff(em_object _expr){
    return em_invoke("diff", 1, _expr);
}

int em_diff_2(em_object _expr, em_object _x){
    return em_invoke("diff", 2, _expr, _x);
}

int em_diff_3(em_object _expr, em_object _x, em_object _n){
    return em_invoke("diff", 3, _expr, _x, _n);
}

int em_diff_4(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "diff");

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

int em_express(em_object _expr){
    return em_invoke("express", 1, _expr);
}

int em_gradef(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "gradef");

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

int em_gradef_2(em_object _a, em_object _x, em_object _expr){
    return em_invoke("gradef", 3, _a, _x, _expr);
}

