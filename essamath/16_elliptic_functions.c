#include "16_elliptic_functions.h"

int em_jacobi_sn(em_object _u, em_object _m){
    return em_invoke("jacobi_sn", 2, _u, _m);
}

int em_jacobi_cn(em_object _u, em_object _m){
    return em_invoke("jacobi_cn", 2, _u, _m);
}

int em_jacobi_dn(em_object _u, em_object _m){
    return em_invoke("jacobi_dn", 2, _u, _m);
}

int em_jacobi_ns(em_object _u, em_object _m){
    return em_invoke("jacobi_ns", 2, _u, _m);
}

int em_jacobi_sc(em_object _u, em_object _m){
    return em_invoke("jacobi_sc", 2, _u, _m);
}

int em_jacobi_sd(em_object _u, em_object _m){
    return em_invoke("jacobi_sd", 2, _u, _m);
}

int em_jacobi_nc(em_object _u, em_object _m){
    return em_invoke("jacobi_nc", 2, _u, _m);
}

int em_jacobi_cs(em_object _u, em_object _m){
    return em_invoke("jacobi_cs", 2, _u, _m);
}

int em_jacobi_cd(em_object _u, em_object _m){
    return em_invoke("jacobi_cd", 2, _u, _m);
}

int em_jacobi_nd(em_object _u, em_object _m){
    return em_invoke("jacobi_nd", 2, _u, _m);
}

int em_jacobi_ds(em_object _u, em_object _m){
    return em_invoke("jacobi_ds", 2, _u, _m);
}

int em_jacobi_dc(em_object _u, em_object _m){
    return em_invoke("jacobi_dc", 2, _u, _m);
}

int em_inverse_jacobi_sn(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_sn", 2, _u, _m);
}

int em_inverse_jacobi_cn(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_cn", 2, _u, _m);
}

int em_inverse_jacobi_dn(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_dn", 2, _u, _m);
}

int em_inverse_jacobi_ns(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_ns", 2, _u, _m);
}

int em_inverse_jacobi_sc(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_sc", 2, _u, _m);
}

int em_inverse_jacobi_sd(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_sd", 2, _u, _m);
}

int em_inverse_jacobi_nc(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_nc", 2, _u, _m);
}

int em_inverse_jacobi_cs(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_cs", 2, _u, _m);
}

int em_inverse_jacobi_cd(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_cd", 2, _u, _m);
}

int em_inverse_jacobi_nd(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_nd", 2, _u, _m);
}

int em_inverse_jacobi_ds(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_ds", 2, _u, _m);
}

int em_inverse_jacobi_dc(em_object _u, em_object _m){
    return em_invoke("inverse_jacobi_dc", 2, _u, _m);
}

int em_elliptic_f(em_object _phi, em_object _m){
    return em_invoke("elliptic_f", 2, _phi, _m);
}

int em_elliptic_e(em_object _phi, em_object _m){
    return em_invoke("elliptic_e", 2, _phi, _m);
}

int em_elliptic_eu(em_object _u, em_object _m){
    return em_invoke("elliptic_eu", 2, _u, _m);
}

int em_elliptic_pi(em_object _n, em_object _phi, em_object _m){
    return em_invoke("elliptic_pi", 3, _n, _phi, _m);
}

int em_elliptic_kc(em_object _m){
    return em_invoke("elliptic_kc", 1, _m);
}

int em_elliptic_ec(em_object _m){
    return em_invoke("elliptic_ec", 1, _m);
}

int em_elliptic_rc(em_object _x, em_object _y){
    return em_invoke("elliptic_rc", 2, _x, _y);
}

int em_elliptic_rd(em_object _x, em_object _y, em_object _z){
    return em_invoke("elliptic_rd", 3, _x, _y, _z);
}

int em_carlson_rf(em_object _x, em_object _y, em_object _z){
    return em_invoke("carlson_rf", 3, _x, _y, _z);
}

int em_carlson_rj(em_object _x, em_object _y, em_object _z, em_object _p){
    return em_invoke("carlson_rj", 4, _x, _y, _z, _p);
}
