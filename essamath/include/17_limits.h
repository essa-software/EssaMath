// 17_limits.h
#include "expression.h"
#ifndef LIMITS_H
#define LIMITS_H

#ifdef __cplusplus
extern "C" {
#endif

// 17.1 Functions and Variables for Limits
int em_limit(em_object _expr);
int em_limit_2(em_object _expr, em_object _x, em_object _val);
int em_limit_3(em_object _expr, em_object _x, em_object _val, em_object _dir);
int em_tlimit(em_object _expr);
int em_tlimit_2(em_object _expr, em_object _x, em_object _val);
int em_tlimit_3(em_object _expr, em_object _x, em_object _val, em_object _dir);
int em_gruntz(em_object _expr, em_object _var, em_object _vlue);
int em_gruntz_2(em_object _expr, em_object _var, em_object _vlue, em_object _direction);

#ifdef __cplusplus
}
#endif

#endif // LIMITS_H
