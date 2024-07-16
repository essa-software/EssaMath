// expression.h
#ifndef EXPRESSION_H
#define EXPRESSION_H

#ifdef __cplusplus
extern "C" {
#endif
struct EmList;

#define EM_INTEGER  1001
#define EM_DOUBLE   1001
#define EM_STRING   1001
#define EM_LIST     1001

#define EM_RTNORM   2001
#define EM_RTERROR  2002

union EmValue{
    int emInt;
    double emDouble;
    char* emString;
    struct EmList* emList;
};

struct EmList{
    union EmValue emVal;
    int emType;
    struct EmList* emNext;
};

int em_eval(const char* _expr);
void em_printf(struct EmList* _value);
int em_toint(struct EmList* _value);
double em_todbl(struct EmList* _value);
const char* em_tostring(struct EmList* _value);
const char* em_tolist(struct EmList* _value);

#ifdef __cplusplus
}
#endif

#endif // EXPRESSION_H
