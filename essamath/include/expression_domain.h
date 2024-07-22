#include "expression.h"
// expression_domain.h
#ifndef EXPRESSION_DOMAIN_H
#define EXPRESSION_DOMAIN_H

#ifdef __cplusplus
extern "C" {
#endif

void em_inithashmapdouble_domain(void);
void em_inithashmapcomplex_domain(void);

int (*em_getfunctionptr_domain(const char* _funcname))(struct EmValueNode**, size_t);
int (*em_getcomplexfunctionptr_domain(const char* _funcname))(struct EmComplexValueNode**, size_t);

void em_freehashmapdouble_domain(void);
void em_freehashmapcomplex_domain(void);

#ifdef __cplusplus
}
#endif

#endif // EXPRESSION_DOMAIN_H
