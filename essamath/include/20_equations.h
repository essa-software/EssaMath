// 20_equations.h
#include "expression.h"
#ifndef EQUATIONS_H
#define EQUATIONS_H

#ifdef __cplusplus
extern "C" {
#endif

// 20.1 Functions and Variables for Equations
int em_algsys(em_object _expr, em_object _x);
int em_allroots(em_object _expr);
int em_bfallroots(em_object _expr);
int em_dimension(em_object _eqn);
int em_dimension_2(size_t n, ...);
int em_funcsolve(em_object _eqn, em_object _g);
int em_ieqn(em_object _ie, em_object _unk, em_object _tech, em_object _n, em_object _guess);
int em_lhs(em_object _expr);
int em_linsolve(em_object _expr, em_object _x);
int em_nroots(em_object _p, em_object _low, em_object _high);
int em_nthroot(em_object _p, em_object _n);
int em_realroots(em_object _expr);
int em_realroots_2(em_object _expr, em_object _bound);
int em_rhs(em_object _expr);
int em_rootscontract(em_object _expr);
int em_solve(em_object _expr);
int em_solve_2(em_object _expr, em_object _x);

#ifdef __cplusplus
}
#endif

#endif // EQUATIONS_H
