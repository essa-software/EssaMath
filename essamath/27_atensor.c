#include "27_atensor.h"
#include "expression.h"

int em_init_atensor(em_object _alg_type){
    return em_invoke("init_atensor", 1, _alg_type);
}

int em_init_atensor_2(em_object _alg_type, em_object _opt_dims){
    return em_invoke("init_atensor", 2, _alg_type, _opt_dims);
}

int em_atensimp(em_object _expr){
    return em_invoke("atensimp", 1, _expr);
}

int em_sf(em_object _u, em_object _v){
    return em_invoke("sf", 2, _u, _v);
}

int em_af(em_object _u, em_object _v){
    return em_invoke("af", 2, _u, _v);
}

int em_av(em_object _u, em_object _v){
    return em_invoke("av", 2, _u, _v);
}

int em_abasep(em_object _v){
    return em_invoke("abasep", 1, _v);
}

