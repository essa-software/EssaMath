#include "35_sets.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_adjoin(em_object _x, em_object _a){
    return em_invoke("adjoin", 2, _x, _a);
}

int em_belln(em_object _n){
    return em_invoke("belln", 1, _n);
}

int em_cardinality(em_object _a){
    return em_invoke("cardinality", 1, _a);
}

int em_cartesian_product(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "cartesian_product");

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

int em_cartesian_product_list(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "cartesian_product_list");

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

int em_disjoin(em_object _x, em_object _a){
    return em_invoke("disjoin", 2, _x, _a);
}

int em_disjointp(em_object _x, em_object _a){
    return em_invoke("disjointp", 2, _x, _a);
}

int em_divisors(em_object _n){
    return em_invoke("divisors", 1, _n);
}

int em_elementp(em_object _x, em_object _a){
    return em_invoke("elementp", 2, _x, _a);
}

int em_emptyp(em_object _a){
    return em_invoke("emptyp", 1, _a);
}

int em_equiv_classes(em_object _s, em_object _F){
    return em_invoke("equiv_classes", 2, _s, _F);
}

int em_every(em_object _f, em_object _s){
    return em_invoke("every", 2, _f, _s);
}

int em_every_2(em_object _f, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "every");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_f, command + index, size - index);
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

int em_equiv_classesmax(em_object _s, em_object _f){
    return em_invoke("equiv_classesmax", 2, _s, _f);
}

int em_equiv_classesmin(em_object _s, em_object _f){
    return em_invoke("equiv_classesmin", 2, _s, _f);
}

int em_flatten(em_object _expr){
    return em_invoke("flatten", 1, _expr);
}

int em_full_listify(em_object _a){
    return em_invoke("full_listify", 1, _a);
}

int em_fullsetify(em_object _a){
    return em_invoke("fullsetify", 1, _a);
}

int em_identity(em_object _x){
    return em_invoke("identity", 1, _x);
}

int em_integer_partitions(em_object _n){
    return em_invoke("integer_partitions", 1, _n);
}

int em_integer_partitions_2(em_object _n, em_object _len){
    return em_invoke("integer_partitions", 2, _n, _len);
}

int em_intersect(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "intersect");

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

int em_intersection(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "intersection");

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

int em_kron_delta(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "kron_delta");

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

int em_listify(em_object _a){
    return em_invoke("listify", 1, _a);
}

int em_makeset(em_object _expr, em_object _x, em_object _s){
    return em_invoke("makeset", 3, _expr, _x, _s);
}

int em_moebius(em_object _n){
    return em_invoke("moebius", 1, _n);
}

int em_multinomial_coeff(){
    return em_invoke("multinomial_coeff", 0);
}

int em_multinomial_coeff_2(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "multinomial_coeff");

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

int em_num_distinct_partitions(em_object _n){
    return em_invoke("num_distinct_partitions", 1, _n);
}

int em_num_distinct_partitions_2(em_object _n, em_object _list){
    return em_invoke("num_distinct_partitions", 2, _n, _list);
}

int em_num_partitions(em_object _n){
    return em_invoke("num_partitions", 1, _n);
}

int em_num_partitions_2(em_object _n, em_object _list){
    return em_invoke("num_partitions", 2, _n, _list);
}

int em_partition_set(em_object _a, em_object _f){
    return em_invoke("partition_set", 2, _a, _f);
}

int em_permutations(em_object _a){
    return em_invoke("permutations", 1, _a);
}

int em_powerset(em_object _a){
    return em_invoke("powerset", 1, _a);
}

int em_powerset_2(em_object _a, em_object _n){
    return em_invoke("powerset", 2, _a, _n);
}

int em_random_permutation(em_object _a){
    return em_invoke("random_permutation", 1, _a);
}

int em_setdifference(em_object _a, em_object _b){
    return em_invoke("setdifference", 2, _a, _b);
}

int em_setequalp(em_object _a, em_object _b){
    return em_invoke("setequalp", 2, _a, _b);
}

int em_setify(em_object _a){
    return em_invoke("setify", 1, _a);
}

int em_setp(em_object _a){
    return em_invoke("setp", 1, _a);
}

int em_set_partitions(em_object _a){
    return em_invoke("set_partitions", 1, _a);
}

int em_set_partitions_2(em_object _a, em_object _n){
    return em_invoke("set_partitions", 2, _a, _n);
}

int em_some(em_object _f, em_object _a){
    return em_invoke("some", 2, _f, _a);
}

int em_some_2(em_object _f, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "some");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_f, command + index, size - index);
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

int em_stirling1(em_object _n, em_object _m){
    return em_invoke("stirling1", 2, _n, _m);
}

int em_stirling2(em_object _n, em_object _m){
    return em_invoke("stirling2", 2, _n, _m);
}

int em_subset(em_object _a, em_object _f){
    return em_invoke("subset", 2, _a, _f);
}

int em_subsetp(em_object _a, em_object _b){
    return em_invoke("subsetp", 2, _a, _b);
}

int em_symmdifference(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "symmdifference");

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

int em_union(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "union");

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
