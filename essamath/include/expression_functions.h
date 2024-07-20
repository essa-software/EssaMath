#include "expression.h"
// expression_functions.h
#ifndef EXPRESSION_FUNCTIONS_H
#define EXPRESSION_FUNCTIONS_H

#ifdef __cplusplus
extern "C" {
#endif

void em_inithashmapdouble(void);
void em_inithashmapcomplex(void);

double (*em_getfunctionptr(const char* _funcname))(struct EmValueNode**, size_t);
_Complex double (*em_getcomplexfunctionptr(const char* _funcname))(struct EmComplexValueNode**, size_t);

void em_freehashmapdouble(void);
void em_freehashmapcomplex(void);

#ifdef __cplusplus
}
#endif

#endif // EXPRESSION_FUNCTIONS_H
