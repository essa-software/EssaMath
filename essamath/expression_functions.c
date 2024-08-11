#include "expression_functions.h"
#include "math_utils.h"
#include "expression.h"
#include "hashmap.h"
#include <complex.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

em_val dummy_expr(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return em_calculateexprnode(_args[0]);
}

em_val add_expr(struct EmValueNode** _args, size_t _count){
    em_val result = em_createreal(0);

    for(size_t i = 0; i < _count; i++){
        em_val other = em_calculateexprnode(_args[i]);
        if(!em_numeric_add(&result, &result, &other)){
            return em_numeric_nan();
        }
    }

    return result;
}

em_val sub_expr(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    em_val result = em_createreal(0);
    em_val other = em_calculateexprnode(_args[0]);
    if(!em_numeric_neg(&result, &other)){
        return em_numeric_nan();
    }

    return result;
}

em_val mul_expr(struct EmValueNode** _args, size_t _count){
    em_val result = em_createreal(1);

    for(size_t i = 0; i < _count; i++){
        em_val other = em_calculateexprnode(_args[i]);
        if(!em_numeric_mul(&result, &result, &other)){
            return em_numeric_nan();
        }
    }

    return result;
}

em_val div_expr(struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    em_val result = em_createreal(0);
    em_val lhs = em_calculateexprnode(_args[0]);
    em_val rhs = em_calculateexprnode(_args[1]);
    if(!em_numeric_div(&result, &lhs, &rhs)){
        return em_numeric_nan();
    }

    return result;
}

em_val pow_expr(struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    em_val result = em_createreal(0);
    em_val lhs = em_calculateexprnode(_args[0]);
    em_val rhs = em_calculateexprnode(_args[1]);
    if(!em_numeric_pow(&result, &lhs, &rhs)){
        return em_numeric_nan();
    }

    return result;
}

em_val fact_expr(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    em_val result = em_createreal(0);
    em_val other = em_calculateexprnode(_args[0]);
    if(!em_numeric_factorial(&result, &other)){
        return em_numeric_nan();
    }

    return result;
}

em_val mod_expr(struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    em_val result = em_createreal(0);
    em_val lhs = em_calculateexprnode(_args[0]);
    em_val rhs = em_calculateexprnode(_args[1]);
    if(!em_numeric_mod(&result, &lhs, &rhs)){
        return em_numeric_nan();
    }

    return result;
}

#define EM_EXPR_FUNC(name)                                                                          \
em_val name##_expr(struct EmValueNode** _args, size_t _count){                                      \
    if(_count != 1){                                                                                \
        return em_numeric_nan();                                                                    \
    }                                                                                               \
    em_val result = em_createreal(0);                                                               \
    em_val other = em_calculateexprnode(_args[0]);                                                  \
    if(!em_numeric_##name(&result, &other)){                                                        \
        return em_numeric_nan();                                                                    \
    }                                                                                               \
    return result;                                                                                  \
}

EM_EXPR_FUNC(abs)
EM_EXPR_FUNC(exp)
EM_EXPR_FUNC(log)
EM_EXPR_FUNC(sin)
EM_EXPR_FUNC(cos)
EM_EXPR_FUNC(tan)
EM_EXPR_FUNC(cot)
EM_EXPR_FUNC(sec)
EM_EXPR_FUNC(csc)
EM_EXPR_FUNC(asin)
EM_EXPR_FUNC(acos)
EM_EXPR_FUNC(atan)
EM_EXPR_FUNC(acot)
EM_EXPR_FUNC(asec)
EM_EXPR_FUNC(acsc)
EM_EXPR_FUNC(sinh)
EM_EXPR_FUNC(cosh)
EM_EXPR_FUNC(tanh)
EM_EXPR_FUNC(coth)
EM_EXPR_FUNC(sech)
EM_EXPR_FUNC(csch)
EM_EXPR_FUNC(asinh)
EM_EXPR_FUNC(acosh)
EM_EXPR_FUNC(atanh)
EM_EXPR_FUNC(acoth)
EM_EXPR_FUNC(asech)
EM_EXPR_FUNC(acsch)
EM_EXPR_FUNC(floor)
EM_EXPR_FUNC(ceil)
EM_EXPR_FUNC(round)

em_val atan2_expr(struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    em_val result = em_createreal(0);
    em_val lhs = em_calculateexprnode(_args[0]);
    em_val rhs = em_calculateexprnode(_args[1]);
    if(!em_numeric_atan2(&result, &lhs, &rhs)){
        return em_numeric_nan();
    }
    return result;
}

em_val list_expr(struct EmValueNode** _args, size_t _count){
    em_val result = em_createvector((em_val*)malloc(_count*sizeof(em_val)), _count);

    for(size_t i = 0; i < _count; i++){
        result.emValue.emVector.emData[i] = em_calculateexprnode(_args[i]);
    }

    return result;
}

em_val bfloat_expr(struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    double p = 56;
    em_val m = em_calculateexprnode(_args[0]);
    em_val e = em_calculateexprnode(_args[1]);

    return em_createreal(em_getdouble(&m) * pow(2, em_getdouble(&e)-p));
}

static struct EmHashmap* hashmapdouble = NULL;

void em_inithashmapdouble(void){
    hashmapdouble = (struct EmHashmap*)malloc(sizeof(struct EmHashmap));
    em_initializehashmap(hashmapdouble);

    em_hashmapinsert(hashmapdouble, "dummy", (void*)&dummy_expr);
    em_hashmapinsert(hashmapdouble, "plus", (void*)&add_expr);
    em_hashmapinsert(hashmapdouble, "minus", (void*)&sub_expr);
    em_hashmapinsert(hashmapdouble, "times", (void*)&mul_expr);
    em_hashmapinsert(hashmapdouble, "rat", (void*)&div_expr);
    em_hashmapinsert(hashmapdouble, "quotient", (void*)&div_expr);
    em_hashmapinsert(hashmapdouble, "expt", (void*)&pow_expr);
    em_hashmapinsert(hashmapdouble, "factorial", (void*)&fact_expr);
    em_hashmapinsert(hashmapdouble, "mod", (void*)&mod_expr);

    em_hashmapinsert(hashmapdouble, "abs", (void*)&abs_expr);
    em_hashmapinsert(hashmapdouble, "exp", (void*)&exp_expr);
    em_hashmapinsert(hashmapdouble, "log", (void*)&log_expr);

    em_hashmapinsert(hashmapdouble, "floor", (void*)&floor_expr);
    em_hashmapinsert(hashmapdouble, "ceil", (void*)&ceil_expr);
    em_hashmapinsert(hashmapdouble, "round", (void*)&round_expr);

    em_hashmapinsert(hashmapdouble, "sin", (void*)&sin_expr);
    em_hashmapinsert(hashmapdouble, "cos", (void*)&cos_expr);
    em_hashmapinsert(hashmapdouble, "tan", (void*)&tan_expr);
    em_hashmapinsert(hashmapdouble, "cot", (void*)&cot_expr);
    em_hashmapinsert(hashmapdouble, "sec", (void*)&sec_expr);
    em_hashmapinsert(hashmapdouble, "csc", (void*)&csc_expr);

    em_hashmapinsert(hashmapdouble, "asin", (void*)&asin_expr);
    em_hashmapinsert(hashmapdouble, "acos", (void*)&acos_expr);
    em_hashmapinsert(hashmapdouble, "atan", (void*)&atan_expr);
    em_hashmapinsert(hashmapdouble, "atan2", (void*)&atan2_expr);
    em_hashmapinsert(hashmapdouble, "acot", (void*)&acot_expr);
    em_hashmapinsert(hashmapdouble, "asec", (void*)&asec_expr);
    em_hashmapinsert(hashmapdouble, "acsc", (void*)&acsc_expr);

    em_hashmapinsert(hashmapdouble, "sinh", (void*)&sinh_expr);
    em_hashmapinsert(hashmapdouble, "cosh", (void*)&cosh_expr);
    em_hashmapinsert(hashmapdouble, "tanh", (void*)&tanh_expr);
    em_hashmapinsert(hashmapdouble, "coth", (void*)&coth_expr);
    em_hashmapinsert(hashmapdouble, "sech", (void*)&sech_expr);
    em_hashmapinsert(hashmapdouble, "csch", (void*)&csch_expr);

    em_hashmapinsert(hashmapdouble, "asinh", (void*)&asinh_expr);
    em_hashmapinsert(hashmapdouble, "acosh", (void*)&acosh_expr);
    em_hashmapinsert(hashmapdouble, "atanh", (void*)&atanh_expr);
    em_hashmapinsert(hashmapdouble, "acoth", (void*)&acoth_expr);
    em_hashmapinsert(hashmapdouble, "asech", (void*)&asech_expr);
    em_hashmapinsert(hashmapdouble, "acsch", (void*)&acsch_expr);

    em_hashmapinsert(hashmapdouble, "list", (void*)&list_expr);
    em_hashmapinsert(hashmapdouble, "bigfloat", (void*)&bfloat_expr);
}

em_val (*em_getfunctionptr(const char* _funcname))(struct EmValueNode**, size_t){
    void* result = em_hashmapsearch(hashmapdouble, _funcname);

    return (em_val (*)(struct EmValueNode**, size_t))result;
}

void em_freehashmapdouble(void){
    em_freehashmap(hashmapdouble);
    free(hashmapdouble);

    hashmapdouble = NULL;
}
