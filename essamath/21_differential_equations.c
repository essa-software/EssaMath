#include "21_differential_equations.h"
#include "expression.h"

int em_bc2(em_object _solution, em_object _xval1, em_object _yval1, em_object _xval2, em_object _yval2){
    return em_invoke("bc2", 5, _solution, _xval1, _yval1, _xval2, _yval2);
}

int em_desolve(em_object _eqn, em_object _y){
    return em_invoke("desolve", 2, _eqn, _y);
}

int em_ic1(em_object _solution, em_object _xval, em_object _yval){
    return em_invoke("ic1", 3, _solution, _xval, _yval);
}

int em_ic2(em_object _solution, em_object _xval, em_object _yval, em_object _dval){
    return em_invoke("ic2", 4, _solution, _xval, _yval, _dval);
}

int em_ode2(em_object _eqn, em_object _dvar, em_object _ivar){
    return em_invoke("ode2", 3, _eqn, _dvar, _ivar);
}
