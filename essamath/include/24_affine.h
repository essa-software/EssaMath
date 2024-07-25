// 24_affine.h
#include "expression.h"
#ifndef AFFINE_H
#define AFFINE_H

#ifdef __cplusplus
extern "C" {
#endif

int em_fast_linsolve(em_object _expr, em_object _x);
int em_grobner_basis(em_object _expr);
int em_set_up_dot_simplifications(em_object _eqns);
int em_set_up_dot_simplifications_2(em_object _eqns, em_object _check_through_degree);
int em_declare_weights(size_t n, ...);
int em_nc_degree(em_object _p);
int em_dotsimp(em_object _f);
int em_fast_central_elements(em_object _x, em_object _n);
int em_check_overlaps(em_object _n, em_object _add_to_simps);
int em_mono(em_object _x, em_object _n);
int em_monomial_dimensions(em_object _n);
int em_extract_linear_equations(em_object _p, em_object _m);
int em_list_nc_monomials(em_object _p);

#ifdef __cplusplus
}
#endif

#endif // AFFINE_H
