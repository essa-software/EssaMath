#include "expression_functions.h"
#include "math_utils.h"
#include "expression.h"
#include "hashmap.h"
#include <complex.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

double dummy_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return em_calculateexprnode(_args[0]);
}

_Complex double dummy_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return em_calculatecomplexexprnode(_args[0]);
}

double add_double(struct EmValueNode** _args, size_t _count){
    double result = 0;

    for(size_t i = 0; i < _count; i++){
        result += em_calculateexprnode(_args[i]);
    }

    return result;
}

_Complex double add_complex(struct EmComplexValueNode** _args, size_t _count){
    _Complex double result = 0;

    for(size_t i = 0; i < _count; i++){
        result += em_calculatecomplexexprnode(_args[i]);
    }

    return result;
}

double sub_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return -em_calculateexprnode(_args[0]);
}

_Complex double sub_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return -em_calculatecomplexexprnode(_args[0]);
}

double mul_double(struct EmValueNode** _args, size_t _count){
    double result = 1;

    for(size_t i = 0; i < _count; i++){
        result *= em_calculateexprnode(_args[i]);
    }

    return result;
}

_Complex double mul_complex(struct EmComplexValueNode** _args, size_t _count){
    _Complex double result = 1;

    for(size_t i = 0; i < _count; i++){
        result *= em_calculatecomplexexprnode(_args[i]);
    }

    return result;
}

double div_double(struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    double nom = em_calculateexprnode(_args[0]);
    double denom = em_calculateexprnode(_args[1]);

    if(denom == 0){
        return em_numeric_nan();
    }

    return nom / denom;
}

_Complex double div_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    _Complex double nom = em_calculatecomplexexprnode(_args[0]);
    _Complex double denom = em_calculatecomplexexprnode(_args[1]);

    if(denom == 0){
        return em_numeric_nan();
    }

    return nom / denom;
}

double pow_double(struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    _Complex double result = cpow(em_calculateexprnode(_args[0]),em_calculateexprnode(_args[1]));
    if(cimag(result) != 0){
        return em_numeric_nan();
    }

    return creal(result);
}

_Complex double pow_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    return cpow(em_calculatecomplexexprnode(_args[0]),em_calculatecomplexexprnode(_args[1]));
}

double fact_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }
    
    double dbl = em_calculateexprnode(_args[0]);
    int res = (int)dbl;

    if(dbl != res){
        return em_numeric_nan();
    }

    int result = 1;
    for(int i = 1; i <= res; i++){
        result *= i;
    }

    return (double)result;
}

double abs_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return fabs(em_calculateexprnode(_args[0]));
}

_Complex double abs_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return cabs(em_calculatecomplexexprnode(_args[0]));
}

double exp_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return exp(em_calculateexprnode(_args[0]));
}

_Complex double exp_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return cexp(em_calculatecomplexexprnode(_args[0]));
}

double log_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return log(em_calculateexprnode(_args[0]));
}

_Complex double log_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return clog(em_calculatecomplexexprnode(_args[0]));
}

double erf_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return erf(em_calculateexprnode(_args[0]));
}

double floor_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return floor(em_calculateexprnode(_args[0]));
}

double ceil_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return ceil(em_calculateexprnode(_args[0]));
}

double round_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return round(em_calculateexprnode(_args[0]));
}

double sin_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return sin(em_calculateexprnode(_args[0]));
}

_Complex double sin_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return csin(em_calculatecomplexexprnode(_args[0]));
}

double cos_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return cos(em_calculateexprnode(_args[0]));
}

_Complex double cos_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return ccos(em_calculatecomplexexprnode(_args[0]));
}

double tan_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return tan(em_calculateexprnode(_args[0]));
}

_Complex double tan_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return ctan(em_calculatecomplexexprnode(_args[0]));
}

double cot_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/tan(em_calculateexprnode(_args[0]));
}

_Complex double cot_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/ctan(em_calculatecomplexexprnode(_args[0]));
}

double sec_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/cos(em_calculateexprnode(_args[0]));
}

_Complex double sec_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/ccos(em_calculatecomplexexprnode(_args[0]));
}

double csc_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/sin(em_calculateexprnode(_args[0]));
}

_Complex double csc_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/csin(em_calculatecomplexexprnode(_args[0]));
}

double asin_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return asin(em_calculateexprnode(_args[0]));
}

_Complex double asin_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return casin(em_calculatecomplexexprnode(_args[0]));
}

double acos_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return acos(em_calculateexprnode(_args[0]));
}

_Complex double acos_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return cacos(em_calculatecomplexexprnode(_args[0]));
}

double atan_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return atan(em_calculateexprnode(_args[0]));
}

_Complex double atan_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return catan(em_calculatecomplexexprnode(_args[0]));
}

double atan2_double(struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return em_numeric_nan();
    }

    return atan2(em_calculateexprnode(_args[0]), em_calculateexprnode(_args[1]));
}

_Complex double atan2_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return catan(em_calculatecomplexexprnode(_args[0]) / em_calculatecomplexexprnode(_args[1]));
}

double acot_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/atan(em_calculateexprnode(_args[0]));
}

_Complex double acot_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/catan(em_calculatecomplexexprnode(_args[0]));
}

double asec_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/acos(em_calculateexprnode(_args[0]));
}

_Complex double asec_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/cacos(em_calculatecomplexexprnode(_args[0]));
}

double acsc_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/asin(em_calculateexprnode(_args[0]));
}

_Complex double acsc_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/casin(em_calculatecomplexexprnode(_args[0]));
}

double sinh_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return sinh(em_calculateexprnode(_args[0]));
}

_Complex double sinh_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return csinh(em_calculatecomplexexprnode(_args[0]));
}

double cosh_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return cosh(em_calculateexprnode(_args[0]));
}

_Complex double cosh_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return ccosh(em_calculatecomplexexprnode(_args[0]));
}

double tanh_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return tanh(em_calculateexprnode(_args[0]));
}

_Complex double tanh_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return ctanh(em_calculatecomplexexprnode(_args[0]));
}

double coth_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/tanh(em_calculateexprnode(_args[0]));
}

_Complex double coth_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/ctanh(em_calculatecomplexexprnode(_args[0]));
}

double sech_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/cosh(em_calculateexprnode(_args[0]));
}

_Complex double sech_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/ccosh(em_calculatecomplexexprnode(_args[0]));
}

double csch_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/sinh(em_calculateexprnode(_args[0]));
}

_Complex double csch_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/csinh(em_calculatecomplexexprnode(_args[0]));
}

double asinh_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return asinh(em_calculateexprnode(_args[0]));
}

_Complex double asinh_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return casinh(em_calculatecomplexexprnode(_args[0]));
}

double acosh_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return acosh(em_calculateexprnode(_args[0]));
}

_Complex double acosh_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return cacosh(em_calculatecomplexexprnode(_args[0]));
}

double atanh_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return atanh(em_calculateexprnode(_args[0]));
}

_Complex double atanh_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return catanh(em_calculatecomplexexprnode(_args[0]));
}

double acoth_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/atanh(em_calculateexprnode(_args[0]));
}

_Complex double acoth_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/catanh(em_calculatecomplexexprnode(_args[0]));
}

double asech_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/acosh(em_calculateexprnode(_args[0]));
}

_Complex double asech_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/cacosh(em_calculatecomplexexprnode(_args[0]));
}

double acsch_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/asinh(em_calculateexprnode(_args[0]));
}

_Complex double acsch_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return em_numeric_nan();
    }

    return 1.0/casinh(em_calculatecomplexexprnode(_args[0]));
}

static struct EmHashmap* hashmapdouble = NULL;
static struct EmHashmap* hashmapcomplex = NULL;

void em_inithashmapdouble(void){
    hashmapdouble = (struct EmHashmap*)malloc(sizeof(struct EmHashmap));
    em_initializehashmap(hashmapdouble);

    em_hashmapinsert(hashmapdouble, "dummy", (void*)&dummy_double);
    em_hashmapinsert(hashmapdouble, "plus", (void*)&add_double);
    em_hashmapinsert(hashmapdouble, "minus", (void*)&sub_double);
    em_hashmapinsert(hashmapdouble, "times", (void*)&mul_double);
    em_hashmapinsert(hashmapdouble, "rat", (void*)&div_double);
    em_hashmapinsert(hashmapdouble, "quotient", (void*)&div_double);
    em_hashmapinsert(hashmapdouble, "expt", (void*)&pow_double);
    em_hashmapinsert(hashmapdouble, "factorial", (void*)&fact_double);

    em_hashmapinsert(hashmapdouble, "abs", (void*)&abs_double);
    em_hashmapinsert(hashmapdouble, "exp", (void*)&exp_double);
    em_hashmapinsert(hashmapdouble, "log", (void*)&log_double);
    em_hashmapinsert(hashmapdouble, "erf", (void*)&erf_double);

    em_hashmapinsert(hashmapdouble, "floor", (void*)&floor_double);
    em_hashmapinsert(hashmapdouble, "ceil", (void*)&ceil_double);
    em_hashmapinsert(hashmapdouble, "round", (void*)&round_double);

    em_hashmapinsert(hashmapdouble, "sin", (void*)&sin_double);
    em_hashmapinsert(hashmapdouble, "cos", (void*)&cos_double);
    em_hashmapinsert(hashmapdouble, "tan", (void*)&tan_double);
    em_hashmapinsert(hashmapdouble, "cot", (void*)&cot_double);
    em_hashmapinsert(hashmapdouble, "sec", (void*)&sec_double);
    em_hashmapinsert(hashmapdouble, "csc", (void*)&csc_double);

    em_hashmapinsert(hashmapdouble, "asin", (void*)&asin_double);
    em_hashmapinsert(hashmapdouble, "acos", (void*)&acos_double);
    em_hashmapinsert(hashmapdouble, "atan", (void*)&atan_double);
    em_hashmapinsert(hashmapdouble, "atan2", (void*)&atan2_double);
    em_hashmapinsert(hashmapdouble, "acot", (void*)&acot_double);
    em_hashmapinsert(hashmapdouble, "asec", (void*)&asec_double);
    em_hashmapinsert(hashmapdouble, "acsc", (void*)&acsc_double);

    em_hashmapinsert(hashmapdouble, "sinh", (void*)&sinh_double);
    em_hashmapinsert(hashmapdouble, "cosh", (void*)&cosh_double);
    em_hashmapinsert(hashmapdouble, "tanh", (void*)&tanh_double);
    em_hashmapinsert(hashmapdouble, "coth", (void*)&coth_double);
    em_hashmapinsert(hashmapdouble, "sech", (void*)&sech_double);
    em_hashmapinsert(hashmapdouble, "csch", (void*)&csch_double);

    em_hashmapinsert(hashmapdouble, "asinh", (void*)&asinh_double);
    em_hashmapinsert(hashmapdouble, "acosh", (void*)&acosh_double);
    em_hashmapinsert(hashmapdouble, "atanh", (void*)&atanh_double);
    em_hashmapinsert(hashmapdouble, "acoth", (void*)&acoth_double);
    em_hashmapinsert(hashmapdouble, "asech", (void*)&asech_double);
    em_hashmapinsert(hashmapdouble, "acsch", (void*)&acsch_double);
}

void em_inithashmapcomplex(void){
    hashmapcomplex = (struct EmHashmap*)malloc(sizeof(struct EmHashmap));
    em_initializehashmap(hashmapcomplex);
    
    em_hashmapinsert(hashmapcomplex, "dummy", (void*)&dummy_complex);
    em_hashmapinsert(hashmapcomplex, "plus", (void*)&add_complex);
    em_hashmapinsert(hashmapcomplex, "minus", (void*)&sub_complex);
    em_hashmapinsert(hashmapcomplex, "times", (void*)&mul_complex);
    em_hashmapinsert(hashmapcomplex, "rat", (void*)&div_complex);
    em_hashmapinsert(hashmapcomplex, "quotient", (void*)&div_complex);
    em_hashmapinsert(hashmapcomplex, "expt", (void*)&pow_complex);

    em_hashmapinsert(hashmapcomplex, "abs", (void*)&abs_complex);
    em_hashmapinsert(hashmapcomplex, "exp", (void*)&exp_complex);
    em_hashmapinsert(hashmapcomplex, "log", (void*)&log_complex);

    em_hashmapinsert(hashmapcomplex, "sin", (void*)&sin_complex);
    em_hashmapinsert(hashmapcomplex, "cos", (void*)&cos_complex);
    em_hashmapinsert(hashmapcomplex, "tan", (void*)&tan_complex);
    em_hashmapinsert(hashmapcomplex, "cot", (void*)&cot_complex);
    em_hashmapinsert(hashmapcomplex, "sec", (void*)&sec_complex);
    em_hashmapinsert(hashmapcomplex, "csc", (void*)&csc_complex);

    em_hashmapinsert(hashmapcomplex, "asin", (void*)&asin_complex);
    em_hashmapinsert(hashmapcomplex, "acos", (void*)&acos_complex);
    em_hashmapinsert(hashmapcomplex, "atan", (void*)&atan_complex);
    em_hashmapinsert(hashmapcomplex, "atan2", (void*)&atan2_complex);
    em_hashmapinsert(hashmapcomplex, "acot", (void*)&acot_complex);
    em_hashmapinsert(hashmapcomplex, "asec", (void*)&asec_complex);
    em_hashmapinsert(hashmapcomplex, "acsc", (void*)&acsc_complex);

    em_hashmapinsert(hashmapcomplex, "sinh", (void*)&sinh_complex);
    em_hashmapinsert(hashmapcomplex, "cosh", (void*)&cosh_complex);
    em_hashmapinsert(hashmapcomplex, "tanh", (void*)&tanh_complex);
    em_hashmapinsert(hashmapcomplex, "coth", (void*)&coth_complex);
    em_hashmapinsert(hashmapcomplex, "sech", (void*)&sech_complex);
    em_hashmapinsert(hashmapcomplex, "csch", (void*)&csch_complex);

    em_hashmapinsert(hashmapcomplex, "asinh", (void*)&asinh_complex);
    em_hashmapinsert(hashmapcomplex, "acosh", (void*)&acosh_complex);
    em_hashmapinsert(hashmapcomplex, "atanh", (void*)&atanh_complex);
    em_hashmapinsert(hashmapcomplex, "acoth", (void*)&acoth_complex);
    em_hashmapinsert(hashmapcomplex, "asech", (void*)&asech_complex);
    em_hashmapinsert(hashmapcomplex, "acsch", (void*)&acsch_complex);
}

double (*em_getfunctionptr(const char* _funcname))(struct EmValueNode**, size_t){
    void* result = em_hashmapsearch(hashmapdouble, _funcname);

    return (double (*)(struct EmValueNode**, size_t))result;
}

_Complex double (*em_getcomplexfunctionptr(const char* _funcname))(struct EmComplexValueNode**, size_t){
    void* result = em_hashmapsearch(hashmapcomplex, _funcname);

    return (_Complex double (*)(struct EmComplexValueNode**, size_t))result;
}

void em_freehashmapdouble(void){
    em_freehashmap(hashmapdouble);
    free(hashmapdouble);

    hashmapdouble = NULL;
}

void em_freehashmapcomplex(void){
    em_freehashmap(hashmapcomplex);
    free(hashmapcomplex);

    hashmapcomplex = NULL;
}
