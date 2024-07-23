// 09_simplification.h
#include "expression.h"
#ifndef SIMPLIFICATION_H
#define SIMPLIFICATION_H

#ifdef __cplusplus
extern "C" {
#endif

int em_combine(em_object _expr);
int em_demoivre(em_object _expr);
int em_distrib(em_object _expr);
int em_expand(em_object _expr);
int em_expand_2(em_object _expr, em_object _p, em_object _n);
int em_expandwrt(em_object _expr, size_t n, ...);
int em_exponentialize(em_object _expr);
int em_multthru(em_object _expr);
int em_multthru_2(em_object _expr1, em_object _expr2);
int em_define_opproperty(em_object _property_name, em_object _simplifier_fn);
int em_radcan(em_object _expr);
int em_scsimp(em_object _expr, size_t n, ...);
int em_xthru(em_object _expr);

#ifdef __cplusplus
}
#endif

#endif // SIMPLIFICATION_H
