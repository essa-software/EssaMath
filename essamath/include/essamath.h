// essamath.h
#include "expression.h"
#ifndef ESSAMATH_H
#define ESSAMATH_H

#ifdef __cplusplus
extern "C" {
#endif

void em_initmath(void);
void em_freemath(void);

em_object em_getlastoutput(void);
em_object em_getlastintermediate(void);

em_object em_getvar(const char* _varname);
void em_setvar(const char* _varname, em_object _value);

#ifdef __cplusplus
}
#endif

#endif // ESSAMATH_H
