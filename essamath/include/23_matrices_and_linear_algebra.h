// 23_matrices_and_linear_algebra.h
#include "expression.h"
#ifndef MATRICES_AND_LINEAR_ALGEBRA_H
#define MATRICES_AND_LINEAR_ALGEBRA_H

#ifdef __cplusplus
extern "C" {
#endif

// 23.2 Functions and Variables for Matrices and Linear Algebra ¶
int em_addcol(em_object _m, size_t n, ...);
int em_addrow(em_object _m, size_t n, ...);
int em_addjoint(em_object _m);
int em_augcoefmatrix(em_object _eqn, em_object _x);
int em_cauchy_matrix(em_object _x);
int em_cauchy_matrix_2(em_object _x, em_object _y);
int em_charpoly(em_object _m, em_object _x);
int em_coefmatrix(em_object _eqn, em_object _x);
int em_col(em_object _m, em_object _i);
int em_columnvector(em_object _l);
int em_covect(em_object _l);
int em_copymatrix(em_object _m);
int em_determinant(em_object _m);
int em_diagmatrix(em_object _n, em_object _x);
int em_echelon(em_object _m);
int em_eigenvalues(em_object _m);
int em_eivals(em_object _m);
int em_eigenvectors(em_object _m);
int em_eivects(em_object _m);
int em_ematrix(em_object _m, em_object _n, em_object _x, em_object _i, em_object _j);
int em_entermatrix(em_object _m, em_object _n);
int em_genmatrix(em_object _a, em_object _i2, em_object _j2);
int em_genmatrix_2(em_object _a, em_object _i2, em_object _j2, em_object _i1);
int em_genmatrix_3(em_object _a, em_object _i2, em_object _j2, em_object _i1, em_object _j1);
int em_gramschmidt(em_object _x);
int em_gramschmidt_2(em_object _x, em_object _f);
int em_ident(em_object _n);
int em_innerproduct(em_object _x, em_object _y);
int em_inprod(em_object _x, em_object _y);
int em_invert_by_adjoint(em_object _m);
int em_invert(em_object _m);
int em_list_matrix_entries(em_object _m);
int em_matrix(size_t n, ...);
int em_matrixexp(em_object _m);
int em_matrixexp_2(em_object _m, em_object _v);
int em_matrixmap(em_object _f, em_object _m);
int em_matrixp(em_object _expr);
int em_mattrace(em_object _m);
int em_minor(em_object _m, em_object _i, em_object _j);
int em_ncharpoly(em_object _m, em_object _x);
int em_newdet(em_object _m);
int em_permanent(em_object _m);
int em_rank(em_object _m);
int em_row(em_object _m, em_object _i);
int em_setelmx(em_object _x, em_object _i, em_object _j, em_object _m);
int em_similaritytransform(em_object _m);
int em_simtran(em_object _m);
int em_submatrix(size_t n, ...);
int em_transpose(em_object _m);
int em_triangularize(em_object _m);
int em_uniteigenvectors(em_object _m);
int em_ueivects(em_object _m);
int em_unitvector(em_object _x);
int em_uvect(em_object _x);
int em_vectorpotential(em_object _givencurl);
int em_vectorsimp(em_object _expr);
int em_zeromatrix(em_object _m, em_object _n);

#ifdef __cplusplus
}
#endif

#endif // MATRICES_AND_LINEAR_ALGEBRA_H
