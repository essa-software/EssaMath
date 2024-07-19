// essamath.h
#include "expression.h"
#ifndef ESSAMATH_H
#define ESSAMATH_H

#ifdef __cplusplus
extern "C" {
#endif

void em_initmath(void);
void em_freemath(void);

const char* em_getlasterror(void);
const char* em_getlastoutput(void);
const char* em_getlastintermediate(void);

em_object em_getvar(const char* _varname);

#ifdef __cplusplus
}
#endif

#endif // ESSAMATH_H
