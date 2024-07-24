// 16_elliptic_functions.h
#include "expression.h"
#ifndef ELLIPTIC_FUNCTIONS_H
#define ELLIPTIC_FUNCTIONS_H

#ifdef __cplusplus
extern "C" {
#endif

// 16.2 Functions and Variables for Elliptic Functions
int em_jacobi_sn(em_object _u, em_object _m);
int em_jacobi_cn(em_object _u, em_object _m);
int em_jacobi_dn(em_object _u, em_object _m);
int em_jacobi_ns(em_object _u, em_object _m);
int em_jacobi_sc(em_object _u, em_object _m);
int em_jacobi_sd(em_object _u, em_object _m);
int em_jacobi_nc(em_object _u, em_object _m);
int em_jacobi_cs(em_object _u, em_object _m);
int em_jacobi_cd(em_object _u, em_object _m);
int em_jacobi_nd(em_object _u, em_object _m);
int em_jacobi_ds(em_object _u, em_object _m);
int em_jacobi_dc(em_object _u, em_object _m);
int em_inverse_jacobi_sn(em_object _u, em_object _m);
int em_inverse_jacobi_cn(em_object _u, em_object _m);
int em_inverse_jacobi_dn(em_object _u, em_object _m);
int em_inverse_jacobi_ns(em_object _u, em_object _m);
int em_inverse_jacobi_sc(em_object _u, em_object _m);
int em_inverse_jacobi_sd(em_object _u, em_object _m);
int em_inverse_jacobi_nc(em_object _u, em_object _m);
int em_inverse_jacobi_cs(em_object _u, em_object _m);
int em_inverse_jacobi_cd(em_object _u, em_object _m);
int em_inverse_jacobi_nd(em_object _u, em_object _m);
int em_inverse_jacobi_ds(em_object _u, em_object _m);
int em_inverse_jacobi_dc(em_object _u, em_object _m);

// 16.3 Functions and Variables for Elliptic Integrals
int em_elliptic_f(em_object _phi, em_object _m);
int em_elliptic_e(em_object _phi, em_object _m);
int em_elliptic_eu(em_object _u, em_object _m);
int em_elliptic_pi(em_object _n, em_object _phi, em_object _m);
int em_elliptic_kc(em_object _m);
int em_elliptic_ec(em_object _m);
int em_elliptic_rc(em_object _x, em_object _y);
int em_elliptic_rd(em_object _x, em_object _y, em_object _z);
int em_carlson_rf(em_object _x, em_object _y, em_object _z);
int em_carlson_rj(em_object _x, em_object _y, em_object _z, em_object _p);

#ifdef __cplusplus
}
#endif

#endif // SPECIAL_FUNCTIONS_H
