#include "17_limits.h"
#include "expression.h"

int em_limit(em_object _expr){
    return em_invoke("limit", 1, _expr);
}

int em_limit_2(em_object _expr, em_object _x, em_object _val){
    return em_invoke("limit", 3, _expr, _x, _val);
}

int em_limit_3(em_object _expr, em_object _x, em_object _val, em_object _dir){
    return em_invoke("limit", 4, _expr, _x, _val, _dir);
}

int em_tlimit(em_object _expr){
    return em_invoke("tlimit", 1, _expr);
}

int em_tlimit_2(em_object _expr, em_object _x, em_object _val){
    return em_invoke("tlimit", 3, _expr, _x, _val);
}

int em_tlimit_3(em_object _expr, em_object _x, em_object _val, em_object _dir){
    return em_invoke("tlimit", 4, _expr, _x, _val, _dir);
}

int em_gruntz(em_object _expr, em_object _var, em_object _vlue){
    return em_invoke("gruntz", 3, _expr, _var, _vlue);
}

int em_gruntz_2(em_object _expr, em_object _var, em_object _vlue, em_object _direction){
    return em_invoke("gruntz", 4, _expr, _var, _vlue, _direction);
}
