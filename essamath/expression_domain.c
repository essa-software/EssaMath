#include "expression_domain.h"
#include "hashmap.h"
#include <stdlib.h>


static struct EmHashmap* hashmapdouble_domain = NULL;
static struct EmHashmap* hashmapcomplex_domain = NULL;

void em_inithashmapdouble_domain(void){
    hashmapdouble_domain = (struct EmHashmap*)malloc(sizeof(struct EmHashmap));
    em_initializehashmap(hashmapdouble_domain);

}

void em_inithashmapcomplex_domain(void){
    hashmapcomplex_domain = (struct EmHashmap*)malloc(sizeof(struct EmHashmap));
    em_initializehashmap(hashmapcomplex_domain);
    
}

int (*em_getfunctionptr_domain(const char* _funcname))(struct EmValueNode**, size_t){
    void* result = em_hashmapsearch(hashmapdouble_domain, _funcname);

    return (int (*)(struct EmValueNode**, size_t))result;
}

int (*em_getcomplexfunctionptr_domain(const char* _funcname))(struct EmComplexValueNode**, size_t){
    void* result = em_hashmapsearch(hashmapcomplex_domain, _funcname);

    return (int (*)(struct EmComplexValueNode**, size_t))result;
}

void em_freehashmapdouble_domain(void){
    em_freehashmap(hashmapdouble_domain);
    free(hashmapdouble_domain);

    hashmapdouble_domain = NULL;
}

void em_freehashmapcomplex_domain(void){
    em_freehashmap(hashmapcomplex_domain);
    free(hashmapcomplex_domain);

    hashmapcomplex_domain = NULL;
}
