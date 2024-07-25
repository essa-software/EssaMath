// 21_differential_equations.h
#include "expression.h"
#ifndef DIFFERENTIAL_EQUATIONS_H
#define DIFFERENTIAL_EQUATIONS_H

#ifdef __cplusplus
extern "C" {
#endif

int em_bc2(em_object _solution, em_object _xval1, em_object _yval1, em_object _xval2, em_object _yval2);
int em_desolve(em_object _eqn, em_object _y);
int em_ic1(em_object _solution, em_object _xval, em_object _yval);
int em_ic2(em_object _solution, em_object _xval, em_object _yval, em_object _dval);
int em_ode2(em_object _eqn, em_object _dvar, em_object _ivar);

#ifdef __cplusplus
}
#endif

#endif // DIFFERENTIAL_EQUATIONS_H
