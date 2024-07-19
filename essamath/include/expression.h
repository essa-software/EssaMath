// expression.h
#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <stddef.h>
#ifdef __cplusplus
extern "C" {
#endif
struct EmList;

#define EM_UNDEF    1000
#define EM_NUMBER   1001
#define EM_STRING   1002
#define EM_LIST     1003

#define EM_RTUND    2000
#define EM_RTNORM   2001
#define EM_RTERROR  2002

union EmValue{
    double emNumber;
    char* emString;
    struct EmList* emList;
};

struct EmList{
    union EmValue emVal;
    int emType;
    struct EmList* emNext;
};

#define em_object struct EmList*

int em_eval(const char* _expr);
void em_printf(em_object _toprint);
void em_tostring(em_object _toprint, char* _buf, size_t _size);
void em_rellist(em_object _tofree);
em_object em_getexpr(em_object _identifier);

#ifdef __cplusplus
}
#endif

#endif // EXPRESSION_H
