#include "19_integration.h"
#include "expression.h"

int em_antid_2(em_object _expr, em_object _f, em_object _y, em_object _x){
    return em_invoke("antid", 4, _expr, _f, _y, _x);
}

int em_dblint(em_object _f, em_object _r, em_object _s, em_object _a, em_object _b){
    return em_invoke("dblint", 5, _f, _r, _s, _a, _b);
}

int em_defint(em_object _expr, em_object _x, em_object _a, em_object _b){
    return em_invoke("defint", 4, _expr, _x, _a, _b);
}

int em_ilt(em_object _expr, em_object _s, em_object _t){
    return em_invoke("ilt", 3, _expr, _s, _t);
}

int em_integrate(em_object _expr, em_object _x){
    return em_invoke("integrate", 2, _expr, _x);
}

int em_integrate_2(em_object _expr, em_object _x, em_object _a, em_object _b){
    return em_invoke("integrate_", 4, _expr, _x, _a, _b);
}

int em_laplace(em_object _expr, em_object _t, em_object _s){
    return em_invoke("laplace", 3, _expr, _t, _s);
}

int em_ldefint(em_object _expr, em_object _x, em_object _a, em_object _b){
    return em_invoke("ldefint", 4, _expr, _x, _a, _b);
}

int em_pwilt(em_object _expr, em_object _s, em_object _t){
    return em_invoke("pwilt", 3, _expr, _s, _t);
}

int em_potential(em_object _givengradient){
    return em_invoke("potential", 1, _givengradient);
}

int em_residue(em_object _expr, em_object _z, em_object _z0){
    return em_invoke("residue", 3, _expr, _z, _z0);
}

int em_risch(em_object _expr, em_object _x){
    return em_invoke("risch", 2, _expr, _x);
}

int em_specint(em_object _expr, em_object _t){
    return em_invoke("specint", 2, _expr, _t);
}

int em_tldefint(em_object _expr, em_object _x, em_object _a, em_object _b){
    return em_invoke("tldefint", 4, _expr, _x, _a, _b);
}

int em_quad_qag(em_object _f, em_object _x, em_object _a, em_object _b, em_object _key, em_object _eps){
    return em_invoke("quad_qag", 6, _f, _x, _a, _b, _key, _eps);
}

int em_quad_qags(em_object _f, em_object _x, em_object _a, em_object _b, em_object _key, em_object _eps){
    return em_invoke("quad_qags", 6, _f, _x, _a, _b, _key, _eps);
}

int em_quad_qagi(em_object _f, em_object _x, em_object _a, em_object _b, em_object _key, em_object _eps){
    return em_invoke("quad_qagi", 6, _f, _x, _a, _b, _key, _eps);
}

int em_quad_qawc(em_object _f, em_object _x, em_object _c, em_object _a, em_object _b, em_object _key, em_object _eps){
    return em_invoke("quad_qawc", 7, _f, _x, _c, _a, _b, _key, _eps);
}

int em_quad_qawf(em_object _f, em_object _x, em_object _a, em_object _omega, em_object _trig, em_object _eps){
    return em_invoke("quad_qawf", 6, _f, _x, _a, _omega, _trig, _eps);
}

int em_quad_qawo(em_object _f, em_object _x, em_object _a, em_object _b, em_object _omega, em_object _trig, em_object _key, em_object _eps){
    return em_invoke("quad_qawo", 8, _f, _x, _a, _b, _omega, _trig, _key, _eps);
}

int em_quad_qaws(em_object _f, em_object _x, em_object _a, em_object _b, em_object _alpha, em_object _beta, em_object _wfun, em_object _eps){
    return em_invoke("quad_qaws", 8, _f, _x, _a, _b, _alpha, _beta, _wfun, _eps);
}

int em_quad_qagp(em_object _f, em_object _x, em_object _a, em_object _b, em_object _points, em_object _eps){
    return em_invoke("quad_qagp", 6, _f, _x, _a, _b, _points, _eps);
}

int em_quad_quad_control(em_object _parameter, em_object _value){
    return em_invoke("quad_quad_control", 2, _parameter, _value);
}
