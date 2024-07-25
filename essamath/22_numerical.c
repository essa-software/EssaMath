#include "22_numerical.h"
#include "expression.h"

int em_polartorect(em_object _r, em_object _t){
    return em_invoke("polartorect", 2, _r, _t);
}

int em_recttopolar(em_object _a, em_object _b){
    return em_invoke("recttopolar", 2, _a, _b);
}

int em_inverse_fft(em_object _y){
    return em_invoke("inverse_fft", 1, _y);
}

int em_fft(em_object _x){
    return em_invoke("fft", 1, _x);
}

int em_real_fft(em_object _x){
    return em_invoke("real_fft", 1, _x);
}

int em_inverse_real_fft(em_object _y){
    return em_invoke("inverse_real_fft", 1, _y);
}

int em_bf_inverse_fft(em_object _y){
    return em_invoke("bf_inverse_fft", 1, _y);
}

int em_bf_fft(em_object _y){
    return em_invoke("bf_fft", 1, _y);
}

int em_bf_real_fft(em_object _y){
    return em_invoke("bf_real_fft", 1, _y);
}

int em_bf_inverse_real_fft(em_object _y){
    return em_invoke("bf_inverse_real_fft", 1, _y);
}

int em_fftpack5_fft(em_object _x){
    return em_invoke("fftpack5_fft", 1, _x);
}

int em_fftpack5_inverse_fft(em_object _y){
    return em_invoke("fftpack5_inverse_fft", 1, _y);
}

int em_fftpack5_real_fft(em_object _x){
    return em_invoke("fftpack5_real_fft", 1, _x);
}

int em_fftpack5_inverse_real_fft(em_object _y, em_object _n){
    return em_invoke("fftpack5_inverse_real_fft", 2, _y, _n);
}

int em_horner(em_object _expr){
    return em_invoke("horner", 1, _expr);
}

int em_horner_2(em_object _expr, em_object _x){
    return em_invoke("horner", 2, _expr, _x);
}

int em_find_root(em_object _expr, em_object _x, em_object _a, em_object _b, em_object _options){
    return em_invoke("find_root", 5, _expr, _x, _a, _b, _options);
}

int em_find_root_2(em_object _f, em_object _a, em_object _b, em_object _options){
    return em_invoke("find_root", 4, _f, _a, _b, _options);
}

int em_bf_find_root(em_object _expr, em_object _x, em_object _a, em_object _b, em_object _options){
    return em_invoke("bf_find_root", 5, _expr, _x, _a, _b, _options);
}

int em_bf_find_root_2(em_object _f, em_object _a, em_object _b, em_object _options){
    return em_invoke("bf_find_root", 4, _f, _a, _b, _options);
}

int em_newton(em_object _expr, em_object _x, em_object _x0, em_object _eps){
    return em_invoke("newton", 4, _expr, _x, _x0, _eps);
}

int em_rk(em_object _ODE, em_object _var, em_object _initial, em_object _domain){
    return em_invoke("rk", 4, _ODE, _var, _initial, _domain);
}
