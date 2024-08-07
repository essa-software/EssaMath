#include "expression_functions.h"
#include "math_utils.h"
#include "expression.h"
#include "hashmap.h"
#include <complex.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

em_val dummy_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return em_calculateexprnode(_args[0]);
}

em_val add_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    em_val result = em_createreal(0);

    for(size_t i = 0; i < _count; i++){
        if(!em_numeric_add(&result, result, em_calculateexprnode(_args[i]))){
            return em_numeric_nan();
        }
    }

    return result;
}

em_val sub_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    em_val result;
    if(!em_numeric_neg(&result, em_calculateexprnode(_args[0]))){
        return em_numeric_nan();
    }

    return result;
}

em_val mul_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    em_val result = em_createreal(1);

    for(size_t i = 0; i < _count; i++){
        if(!em_numeric_mul(&result, result, em_calculateexprnode(_args[i]))){
            return em_numeric_nan();
        }
    }

    return result;
}

em_val div_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    em_val result;
    if(!em_numeric_div(&result, em_calculateexprnode(_args[0]), em_calculateexprnode(_args[1]))){
        return em_numeric_nan();
    }

    return result;
}

em_val pow_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    em_val result;
    if(!em_numeric_pow(&result, em_calculateexprnode(_args[0]), em_calculateexprnode(_args[1]))){
        return em_numeric_nan();
    }

    return result;
}

em_val fact_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    em_val result;
    if(!em_numeric_factorial(&result, em_calculateexprnode(_args[0]))){
        return em_numeric_nan();
    }

    return result;
}

em_val mod_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    em_val result;
    if(!em_numeric_mod(&result, em_calculateexprnode(_args[0]), em_calculateexprnode(_args[1]))){
        return em_numeric_nan();
    }

    return result;
}

#define EM_EXPR_FUNC(name)                                                                          \
em_val name##_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){    \
    if(_count != 1){                                                                                \
        return em_numeric_nan();                                                                    \
    }                                                                                               \
    em_val result;                                                                                  \
    if(!em_numeric_##name(&result, em_calculateexprnode(_args[0]))){                                \
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

em_val floor_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }
    em_val result;
    if(!em_numeric_floor(&result, em_calculateexprnode(_args[0]))){
        return em_numeric_nan();
    }
    return result;
}

em_val ceil_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }
    em_val result;
    if(!em_numeric_ceil(&result, em_calculateexprnode(_args[0]))){
        return em_numeric_nan();
    }
    return result;
}

em_val round_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }
    em_val result;
    if(!em_numeric_round(&result, em_calculateexprnode(_args[0]))){
        return em_numeric_nan();
    }
    return result;
}

em_val atan2_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    em_val result;
    if(!em_numeric_atan2(&result, em_calculateexprnode(_args[0]), em_calculateexprnode(_args[1]))){
        return em_numeric_nan();
    }
    return result;
}

em_val list_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    em_val result = em_createvector((em_val*)malloc(_count*sizeof(em_val)), _count);

    for(size_t i = 0; i < _count; i++){
        result.emValue.emVector.emData[i] = em_calculateexprnode(_args[i]);
    }

    return result;
}

em_val bfloat_expr([[maybe_unused]] em_object _head, struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    double p = 0;
    em_object current = _head;
    while(current){
        if(current->emType == EM_NUMBER){
            p = current->emVal.emNumber;
        }

        current = current->emNext;
    }

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

em_val (*em_getfunctionptr(const char* _funcname))(em_object, struct EmValueNode**, size_t){
    void* result = em_hashmapsearch(hashmapdouble, _funcname);

    return (em_val (*)(em_object, struct EmValueNode**, size_t))result;
}

void em_freehashmapdouble(void){
    em_freehashmap(hashmapdouble);
    free(hashmapdouble);

    hashmapdouble = NULL;
}
