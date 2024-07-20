#include "expression_functions.h"
#include "expression.h"
#include "hashmap.h"
#include <complex.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

double dummy_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return (double)NAN;
    }

    return em_calculateexprnode(_args[0]);
}

_Complex double dummy_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return (double)NAN;
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
    double result = 0;

    for(size_t i = 0; i < _count; i++){
        result -= em_calculateexprnode(_args[i]);
    }

    return result;
}

_Complex double sub_complex(struct EmComplexValueNode** _args, size_t _count){
    _Complex double result = 0;

    for(size_t i = 0; i < _count; i++){
        result -= em_calculatecomplexexprnode(_args[i]);
    }

    return result;
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
        return (double)NAN;
    }

    return em_calculateexprnode(_args[0]) / em_calculateexprnode(_args[1]);
}

_Complex double div_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 2){
        return (double)NAN;
    }

    return em_calculatecomplexexprnode(_args[0]) / em_calculatecomplexexprnode(_args[1]);
}

double pow_double(struct EmValueNode** _args, size_t _count){
    if(_count != 2){
        return (double)NAN;
    }

    return pow(em_calculateexprnode(_args[0]),em_calculateexprnode(_args[1]));
}

_Complex double pow_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 2){
        return (double)NAN;
    }

    return cpow(em_calculatecomplexexprnode(_args[0]),em_calculatecomplexexprnode(_args[1]));
}

double fact_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return (double)NAN;
    }
    
    double dbl = em_calculateexprnode(_args[0]);
    int res = (int)dbl;

    if(!EM_NEAREQUAL(dbl, res, 1e-3)){
        return (double)NAN;
    }

    int result = 1;
    for(int i = 1; i <= res; i++){
        result *= i;
    }

    return (double)result;
}

double abs_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return (double)NAN;
    }

    return fabs(em_calculateexprnode(_args[0]));
}

_Complex double abs_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return (double)NAN;
    }

    return cabs(em_calculatecomplexexprnode(_args[0]));
}

double sin_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return (double)NAN;
    }

    return sin(em_calculateexprnode(_args[0]));
}

_Complex double sin_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return (double)NAN;
    }

    return csin(em_calculatecomplexexprnode(_args[0]));
}

double cos_double(struct EmValueNode** _args, size_t _count){
    if(_count != 1){
        return (double)NAN;
    }

    return cos(em_calculateexprnode(_args[0]));
}

_Complex double cos_complex(struct EmComplexValueNode** _args, size_t _count){
    if(_count != 1){
        return (double)NAN;
    }

    return ccos(em_calculatecomplexexprnode(_args[0]));
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
    em_hashmapinsert(hashmapdouble, "cabs", (void*)&abs_double);
    em_hashmapinsert(hashmapdouble, "sin", (void*)&sin_double);
    em_hashmapinsert(hashmapdouble, "cos", (void*)&cos_double);
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
    em_hashmapinsert(hashmapcomplex, "cabs", (void*)&abs_complex);
    em_hashmapinsert(hashmapcomplex, "sin", (void*)&sin_complex);
    em_hashmapinsert(hashmapcomplex, "cos", (void*)&cos_complex);
}

double (*em_getfunctiondouble(const char* _funcname))(struct EmValueNode**, size_t){
    void* result = em_hashmapsearch(hashmapdouble, _funcname);

    return (double (*)(struct EmValueNode**, size_t))result;
}

_Complex double (*em_getfunctioncomplex(const char* _funcname))(struct EmComplexValueNode**, size_t){
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
