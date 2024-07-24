// 18_differentiation.h
#include "expression.h"
#ifndef DIFFERENTIATION_H
#define DIFFERENTIATION_H

#ifdef __cplusplus
extern "C" {
#endif

// 18.1 Functions and Variables for Differentiation
int em_antid(em_object _expr, em_object _x, em_object _u);
int em_antidiff(em_object _expr, em_object _x, em_object _u);
int em_at(em_object _expr, em_object _eqn);
int em_atvalue(em_object _expr, em_object _x, em_object _c);
int em_init_cartan(em_object _x);
int em_del(em_object _x);
int em_delta(em_object _t);
int em_dependencies(size_t n, ...);
int em_depends(size_t n, ...);
int em_derivdegree(em_object _expr, em_object _y, em_object _x);
int em_derivlist(size_t n, ...);
int em_diff(em_object _expr);
int em_diff_2(em_object _expr, em_object _x);
int em_diff_3(em_object _expr, em_object _x, em_object _n);
int em_diff_4(em_object _expr, size_t n, ...);
int em_express(em_object _expr);
int em_gradef(em_object _expr, size_t n, ...);
int em_gradef_2(em_object _a, em_object _x, em_object _expr);

#ifdef __cplusplus
}
#endif

#endif // DIFFERENTIATION_H
