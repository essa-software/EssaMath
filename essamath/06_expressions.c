#include "06_expressions.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_alias(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "alias");

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

int em_args(em_object _expr){
    return em_invoke("args", 1, _expr);
}

int em_atom(em_object _expr){
    return em_invoke("atom", 1, _expr);
}

int em_box(em_object _expr){
    return em_invoke("box", 1, _expr);
}

int em_box_2(em_object _expr, em_object _a){
    return em_invoke("box", 2, _expr, _a);

}

int em_collapse(em_object _expr){
    return em_invoke("collapse", 1, _expr);
}

int em_copy(em_object _e){
    return em_invoke("copy", 1, _e);
}

int em_disolate(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "disolate");

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

int em_dispform(em_object _expr){
    return em_invoke("dispform", 1, _expr);
}

int em_dispformall(em_object _expr){
    return em_invoke("dispform", 2, _expr, em_createstring("all"));
}

int em_dpart(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "dpart");

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

int em_ev(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "ev");

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

int em_freeof(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "freeof");

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

int em_inpart(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "inpart");

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

int em_isolate(em_object _expr, em_object _x){
    return em_invoke("isolate", 2, _expr, _x);
}

int em_listofvars(em_object _expr){
    return em_invoke("listofvars", 1, _expr);
}

int em_lfreeof(em_object _list, em_object _expr){
    return em_invoke("lfreeof", 2, _list, _expr);
}

int em_lpart(em_object _label, em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "ipart");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_label, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
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

int em_nounify(em_object _f){
    return em_invoke("nounify", 1, _f);
}

int em_nterms(em_object _expr){
    return em_invoke("nterms", 1, _expr);
}

int em_op(em_object _expr){
    return em_invoke("op", 1, _expr);
}

int em_operatorp(em_object _expr, em_object _op){
    return em_invoke("operatorp", 2, _expr, _op);
}

int em_optimize(em_object _expr){
    return em_invoke("optimize", 1, _expr);
}

int em_ordergreat(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "ordergreat");

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

int em_orderless(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "orderless");

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

int em_ordergreatp(em_object _expr1, em_object _expr2){
    return em_invoke("ordergreatp", 2, _expr1, _expr2);
}

int em_orderlessp(em_object _expr1, em_object _expr2){
    return em_invoke("orderlessp", 2, _expr1, _expr2);
}

int em_part(em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "part");

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

int em_partition(em_object _expr, em_object _x){
    return em_invoke("partition", 2, _expr, _x);
}

int em_pickapart(em_object _expr, em_object _n){
    return em_invoke("pickapart", 2, _expr, _n);
}

int em_psubst(em_object _list, em_object _expr){
    return em_invoke("psubst", 2, _list, _expr);
}

int em_psubst_2(em_object _a, em_object _b, em_object _expr){
    return em_invoke("psubst", 3, _a, _b, _expr);
}

int em_rembox_unlabelled(em_object _expr){
    return em_invoke("rembox_unlabelled", 1, _expr);
}

int em_rembox_labelled(em_object _expr, em_object _label){
    return em_invoke("rembox_labelled", 2, _expr, _label);
}

int em_rembox(em_object _expr){
    return em_invoke("rembox", 1, _expr);
}

int em_reveal(em_object _expr, em_object _depth){
    return em_invoke("reveal", 2, _expr, _depth);
}

int em_sqrtdenest(em_object _expr){
    return em_invoke("sqrtdenest", 1, _expr);
}

int em_sublis(em_object _list, em_object _expr){
    return em_invoke("sublis", 2, _list, _expr);
}

int em_subst(em_object _a, em_object _b, em_object _c){
    return em_invoke("subst", 3, _a, _b, _c);
}

int em_substinpart(em_object _x, em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "substinpart");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_x, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
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

int em_substpart(em_object _x, em_object _expr, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "substpart");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_x, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
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

int em_symbolp(em_object _expr){
    return em_invoke("symbolp", 1, _expr);
}

int em_unorder(){
    return em_invoke("symbolp", 0);
}

int em_verbify(em_object _f){
    return em_invoke("verbify", 1, _f);
}
