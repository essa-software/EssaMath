// essamath.h
#include "expression.h"
#ifndef ESSAMATH_H
#define ESSAMATH_H

#ifdef __cplusplus
extern "C" {
#endif

void em_init_math(void);
void em_free_math(void);
const char* em_getlasterror(void);

#ifdef __cplusplus
}
#endif

#endif // ESSAMATH_H
