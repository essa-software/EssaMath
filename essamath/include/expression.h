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
int em_invoke(const char* _funcname, size_t n, ...);
void em_printf(em_object _toprint);
void em_tostring(em_object _toprint, char* _buf, size_t _size);
void em_rellist(em_object _tofree);
em_object em_getexpr(em_object _identifier);
em_object em_clonelist(em_object _other);

em_object em_createstring(const char* _str);
em_object em_createnumber(double _number);

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
    em_object EmHead;
    struct EmValueNode** EmArgs;
    size_t EmCount;
    double (*EmFunc)(em_object, struct EmValueNode**, size_t);
    _Complex int (*EmDomain)(struct EmValueNode**, size_t);
};
union EmExprComplexValue{
    _Complex double emNumber;
    _Complex double* emVariable;
    struct EmComplexExpression* emExpr;
};

struct EmComplexValueNode{
    union EmExprComplexValue emVal;
    int emType;
};

struct EmComplexExpression{
    em_object EmHead;
    struct EmComplexValueNode** EmArgs;
    size_t EmCount;
    _Complex double (*EmFunc)(em_object, struct EmComplexValueNode**, size_t);
    _Complex int (*EmDomain)(struct EmComplexValueNode**, size_t);
};

#define em_expr struct EmExpression*
#define em_complexexpr struct EmComplexExpression*

em_expr em_createexpression(em_object _object, size_t _varcount, const char** _varlist, double** _vardata);
em_complexexpr em_createcomplexexpression(em_object _object, size_t _varcount, const char** _varlist, _Complex double** _vardata);

double em_calculateexpr(em_expr _expr);
_Complex double em_calculatecomplexexpr(em_complexexpr _expr);

double em_calculateexprnode(struct EmValueNode* _expr);
_Complex double em_calculatecomplexexprnode(struct EmComplexValueNode* _expr);

void em_relexpr(em_expr _tofree);
void em_relcomplexexpr(em_complexexpr _tofree);
void em_relexprnode(struct EmValueNode* _tofree);
void em_relcomplexexprnode(struct EmComplexValueNode* _tofree);

#define EM_NEAREQUAL(_lhs, _rhs, _eps) \
(fabs((_lhs) - (_rhs)) <= (_eps))

#ifdef __cplusplus
}
#endif

#endif // EXPRESSION_H
