#include "23_matrices_and_linear_algebra.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_addcol(em_object _M, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "addcol");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_M, command + index, size - index);
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

int em_addrow(em_object _M, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "addrow");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_M, command + index, size - index);
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

int em_addjoint(em_object _M){
    return em_invoke("addjoint", 1, _M);
}

int em_augcoefmatrix(em_object _eqn, em_object _x){
    return em_invoke("augcoefmatrix", 2, _eqn, _x);
}

int em_cauchy_matrix(em_object _x){
    return em_invoke("cauchy_matrix", 1, _x);
}

int em_cauchy_matrix_2(em_object _x, em_object _y){
    return em_invoke("cauchy_matrix", 2, _x, _y);
}

int em_charpoly(em_object _M, em_object _x){
    return em_invoke("charpoly", 2, _M, _x);
}

int em_coefmatrix(em_object _eqn, em_object _x){
    return em_invoke("coefmatrix", 2, _eqn, _x);
}

int em_col(em_object _M, em_object _i){
    return em_invoke("col", 2, _M, _i);
}

int em_columnvector(em_object _L){
    return em_invoke("columnvector", 1, _L);
}

int em_covect(em_object _L){
    return em_invoke("covect", 1, _L);
}

int em_copymatrix(em_object _M){
    return em_invoke("copymatrix", 1, _M);
}

int em_determinant(em_object _M){
    return em_invoke("determinant", 1, _M);
}

int em_diagmatrix(em_object _n, em_object _x){
    return em_invoke("diagmatrix", 2, _n, _x);
}

int em_echelon(em_object _M){
    return em_invoke("echelon", 1, _M);
}

int em_eigenvalues(em_object _M){
    return em_invoke("eigenvalues", 1, _M);
}

int em_eivals(em_object _M){
    return em_invoke("eivals", 1, _M);
}

int em_eigenvectors(em_object _M){
    return em_invoke("eigenvectors", 1, _M);
}

int em_eivects(em_object _M){
    return em_invoke("eivects", 1, _M);
}

int em_ematrix(em_object _m, em_object _n, em_object _x, em_object _i, em_object _j){
    return em_invoke("ematrix", 5, _m, _n, _x, _i, _j);
}

int em_entermatrix(em_object _m, em_object _n){
    return em_invoke("entermatrix", 2, _m, _n);
}

int em_genmatrix(em_object _a, em_object _i2, em_object _j2){
    return em_invoke("genmatrix", 3, _a, _i2, _j2);
}

int em_genmatrix_2(em_object _a, em_object _i2, em_object _j2, em_object _i1){
    return em_invoke("genmatrix", 4, _a, _i2, _j2, _i1);
}

int em_genmatrix_3(em_object _a, em_object _i2, em_object _j2, em_object _i1, em_object _j1){
    return em_invoke("genmatrix", 5, _a, _i2, _j2, _i1, _j1);
}

int em_gramschmidt(em_object _x){
    return em_invoke("gramschmidt", 1, _x);
}

int em_gramschmidt_2(em_object _x, em_object _F){
    return em_invoke("gramschmidt", 2, _x, _F);
}

int em_ident(em_object _n){
    return em_invoke("ident", 1, _n);
}

int em_innerproduct(em_object _x, em_object _y){
    return em_invoke("innerproduct", 2, _x, _y);
}

int em_inprod(em_object _x, em_object _y){
    return em_invoke("inprod", 2, _x, _y);
}

int em_invert_by_adjoint(em_object _M){
    return em_invoke("invert_by_adjoint", 1, _M);
}

int em_invert(em_object _M){
    return em_invoke("invert", 1, _M);
}

int em_list_matrix_entries(em_object _M){
    return em_invoke("list_matrix_entries", 1, _M);
}

int em_matrix(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "matrix");

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

int em_matrixexp(em_object _M){
    return em_invoke("matrixexp", 1, _M);
}

int em_matrixexp_2(em_object _M, em_object _V){
    return em_invoke("matrixexp", 2, _M, _V);
}

int em_matrixmap(em_object _f, em_object _M){
    return em_invoke("matrixmap", 2, _f, _M);
}

int em_matrixp(em_object _expr){
    return em_invoke("matrixp", 1, _expr);
}

int em_mattrace(em_object _M){
    return em_invoke("mattrace", 1, _M);
}

int em_minor(em_object _M, em_object _i, em_object _j){
    return em_invoke("minor", 3, _M, _i, _j);
}

int em_ncharpoly(em_object _M, em_object _x){
    return em_invoke("ncharpoly", 2, _M, _x);
}

int em_newdet(em_object _M){
    return em_invoke("newdet", 1, _M);
}

int em_permanent(em_object _M){
    return em_invoke("permanent", 1, _M);
}

int em_rank(em_object _M){
    return em_invoke("rank", 1, _M);
}

int em_row(em_object _M, em_object _i){
    return em_invoke("row", 2, _M, _i);
}

int em_setelmx(em_object _x, em_object _i, em_object _j, em_object _M){
    return em_invoke("setelmx", 4, _x, _i, _j, _M);
}

int em_similaritytransform(em_object _M){
    return em_invoke("similaritytransform", 1, _M);
}

int em_simtran(em_object _M){
    return em_invoke("simtran", 1, _M);
}

int em_submatrix(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "submatrix");

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

int em_transpose(em_object _M){
    return em_invoke("transpose", 1, _M);
}

int em_triangularize(em_object _M){
    return em_invoke("triangularize", 1, _M);
}

int em_uniteigenvectors(em_object _M){
    return em_invoke("uniteigenvectors", 1, _M);
}

int em_ueivects(em_object _M){
    return em_invoke("ueivects", 1, _M);
}

int em_unitvector(em_object _x){
    return em_invoke("unitvector", 1, _x);
}

int em_uvect(em_object _x){
    return em_invoke("uvect", 1, _x);
}

int em_vectorpotential(em_object _givencurl){
    return em_invoke("vectorpotential", 1, _givencurl);
}

int em_vectorsimp(em_object _expr){
    return em_invoke("vectorsimp", 1, _expr);
}

int em_zeromatrix(em_object _m, em_object _n){
    return em_invoke("zeromatrix", 1, _m, _n);
}
