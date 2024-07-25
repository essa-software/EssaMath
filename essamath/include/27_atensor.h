// 27_atensor.h
#include "expression.h"
#ifndef ATENSOR_H
#define ATENSOR_H

#ifdef __cplusplus
extern "C" {
#endif

int em_init_atensor(em_object _alg_type);
int em_init_atensor_2(em_object _alg_type, em_object _opt_dims);
int em_atensimp(em_object _expr);
int em_sf(em_object _u, em_object _v);
int em_af(em_object _u, em_object _v);
int em_av(em_object _u, em_object _v);
int em_abasep(em_object _v);

#ifdef __cplusplus
}
#endif

#endif // ATENSOR_H
