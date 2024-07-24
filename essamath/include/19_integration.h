// 19_integration.h
#include "expression.h"
#ifndef INTEGRATION_H
#define INTEGRATION_H

#ifdef __cplusplus
extern "C" {
#endif

// 19.2 Functions and Variables for integration
int em_antid(em_object _expr, em_object _f, em_object _y, em_object _x);
int em_dblint(em_object _f, em_object _r, em_object _s, em_object _a, em_object _b);
int em_defint(em_object _expr, em_object _x, em_object _a, em_object _b);
int em_ilt(em_object _expr, em_object _s, em_object _t);
int em_integrate(em_object _expr, em_object _x);
int em_integrate_2(em_object _expr, em_object _x, em_object _a, em_object _b);
int em_laplace(em_object _expr, em_object _t, em_object _s);
int em_ldefint(em_object _expr, em_object _x, em_object _a, em_object _b);
int em_pwilt(em_object _expr, em_object _s, em_object _t);
int em_potential(em_object _givengradient);
int em_residue(em_object _expr, em_object _z, em_object _z0);
int em_risch(em_object _expr, em_object _x);
int em_specint(em_object _expr, em_object _t);
int em_tldefint(em_object _expr, em_object _x, em_object _a, em_object _b);

// 19.4 Functions and Variables for QUADPACK
int em_quad_qag(em_object _f, em_object _x, em_object _a, em_object _b, em_object _key, em_object _eps);
int em_quad_qags(em_object _f, em_object _x, em_object _a, em_object _b, em_object _key, em_object _eps);
int em_quad_qagi(em_object _f, em_object _x, em_object _a, em_object _b, em_object _key, em_object _eps);
int em_quad_qawc(em_object _f, em_object _x, em_object _c, em_object _a, em_object _b, em_object _key, em_object _eps);
int em_quad_qawf(em_object _f, em_object _x, em_object _a, em_object _omega, em_object _trig, em_object _eps);
int em_quad_qawo(em_object _f, em_object _x, em_object _a, em_object _b, em_object _omega, em_object _trig, em_object _key, em_object _eps);
int em_quad_qaws(em_object _f, em_object _x, em_object _a, em_object _b, em_object _alpha, em_object _beta, em_object _wfun, em_object _eps);
int em_quad_qagp(em_object _f, em_object _x, em_object _a, em_object _b, em_object _points, em_object _eps);
int em_quad_quad_control(em_object _parameter, em_object _value);

#ifdef __cplusplus
}
#endif

#endif // INTEGRATION_H
