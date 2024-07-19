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

#define EM_EXPRUND    3000
#define EM_EXPRNUM    3001
#define EM_EXPRVAR    3002
#define EM_EXPREXP    3003

union EmExprValue{
    double emNumber;
    double* emVariable;
    struct EmExpression* emExpr;
};

struct EmValueNode{
    union EmExprValue emVal;
    int emType;
};

struct EmExpression{
    struct EmValueNode** EmArgs;
    size_t EmCount;
    double (*EmFunc)(struct EmValueNode**, size_t);
};
union EmExprComplexValue{
    double emNumber;
    double* emVariable;
    struct EmComplexExpression* emExpr;
};

struct EmComplexValueNode{
    union EmExprComplexValue emVal;
    int emType;
};

struct EmComplexExpression{
    struct EmComplexValueNode** EmArgs;
    size_t EmCount;
    _Complex double (*EmFunc)(struct EmComplexValueNode**, size_t);
};

#define em_expr struct EmExpression*
#define em_complexexpr struct EmComplexExpression*

em_expr em_createexpressiondouble(em_object _object, const char** _varlist, double** _vardata);
em_complexexpr em_createexpressioncomplex(em_object _object, const char** _varlist, double** _vardata);

double em_calculateexpr(em_expr _expr);
_Complex double em_calculatecomplexexpr(em_complexexpr _expr);

double em_calculateexprnode(struct EmValueNode* _expr);
_Complex double em_calculatecomplexexprnode(struct EmComplexValueNode* _expr);

#define EM_NEAREQUAL(_lhs, _rhs, _eps) \
(fabs((_lhs) - (_rhs)) <= (_eps))

#ifdef __cplusplus
}
#endif

#endif // EXPRESSION_H
