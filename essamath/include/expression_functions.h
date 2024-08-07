#include "expression.h"
// expression_functions.h
#ifndef EXPRESSION_FUNCTIONS_H
#define EXPRESSION_FUNCTIONS_H

#ifdef __cplusplus
extern "C" {
#endif

#define EM_FUNC (em_val (*)(em_object, struct EmValueNode**, size_t))
#define EM_FUNCDEF(name) (em_val (*(name))(em_object, struct EmValueNode**, size_t))

void em_inithashmapdouble(void);
em_val (*em_getfunctionptr(const char* _funcname))(em_object, struct EmValueNode**, size_t);
void em_freehashmapdouble(void);

#ifdef __cplusplus
}
#endif

#endif // EXPRESSION_FUNCTIONS_H
