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

union EmListValue{
    double emNumber;
    char* emString;
    struct EmList* emList;
};

struct EmList{
    union EmListValue emVal;
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

#define EM_VALUND       3000
#define EM_VALREAL      3001
#define EM_VALCOMPLEX   3002
#define EM_VALVECTOR    3003
// #define EM_VALMATRIX    3004

struct EmNumericValue;
#define em_val struct EmNumericValue
struct EmVector{
    size_t emSize;
    em_val* emData;
};

// struct EmMatrix{
//     size_t EmRows, EmCols;
//     em_val** emData;
// };

union EmValue{
    double emReal;
    _Complex double emComplex;
    struct EmVector emVector;
    // struct EmMatrix emMatrix;
};

struct EmNumericValue{
    union EmValue emValue;
    int emType;
};

_Complex double em_complex(double _real, double _imag);

em_val em_createreal(double _number);
em_val em_createcomplex(_Complex double _number);
em_val em_createvector(em_val* _number, size_t _size);
// em_val em_creatematrix(em_val** _number, size_t _rows, size_t _cols);

double em_getdouble(em_val _value);
_Complex double em_getcomplex(em_val _value);
size_t em_getvectorsize(em_val _value);
em_val* em_getvectordata(em_val _value);

union EmExprValue{
    em_val emNumber;
    em_val* emVariable;
    struct EmExpression* emExpr;
};

struct EmValueNode{
    union EmExprValue emVal;
    int emType;
};

struct EmExpression{
    em_object emHead;
    struct EmValueNode** emArgs;
    size_t emCount;
    em_val (*emFunc)(em_object, struct EmValueNode**, size_t);
};

#define em_expr struct EmExpression*
em_expr em_createexpression(em_object _object, size_t _varcount, const char** _varlist, em_val** _vardata);

em_val em_calculateexpr(em_expr _expr);
em_val em_calculateexprnode(struct EmValueNode* _expr);

void em_relexpr(em_expr _tofree);
void em_relexprnode(struct EmValueNode* _tofree);

#define EM_NEAREQUAL(_lhs, _rhs, _eps) \
(fabs((_lhs) - (_rhs)) <= (_eps))

#ifdef __cplusplus
}
#endif

#endif // EXPRESSION_H
