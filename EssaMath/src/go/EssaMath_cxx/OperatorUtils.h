#pragma once

#ifdef __cplusplus
extern "C" {
#endif
    
void* abs_f(void*);
void* abs_d(void*);
void* abs_cf(void*);
void* abs_cd(void*);
    
void* cabs_f(void*);
void* cabs_d(void*);
void* cabs_cf(void*);
void* cabs_cd(void*);
    
const char* compare_f(void*, void*);
const char* compare_d(void*, void*);
const char* compare_cf(void*, void*);
const char* compare_cd(void*, void*);
    
void* polymod_f(void*, int);
void* polymod_d(void*, int);
void* polymod_cf(void*, int);
void* polymod_cd(void*, int);
    
void* psubst_f(void**, int, void*);
void* psubst_d(void**, int, void*);
void* psubst_cf(void**, int, void*);
void* psubst_cd(void**, int, void*);
    
void* rationalize_f(void*);
void* rationalize_d(void*);
void* rationalize_cf(void*);
void* rationalize_cd(void*);
    
const char* sign_f(void*);
const char* sign_d(void*);
const char* sign_cf(void*);
const char* sign_cd(void*);
    
void* subst_f(void**, int, void*);
void* subst_d(void**, int, void*);
void* subst_cf(void**, int, void*);
void* subst_cd(void**, int, void*);
    
void** sort_f(void**, int);
void** sort_d(void**, int);
void** sort_cf(void**, int);
void** sort_cd(void**, int);

void* sqrt_f(void*);
void* sqrt_d(void*);
void* sqrt_cf(void*);
void* sqrt_cd(void*);

void* xthru_f(void*);
void* xthru_d(void*);
void* xthru_cf(void*);
void* xthru_cd(void*);
    
const char* zeroequiv_f(void*, const char*);
const char* zeroequiv_d(void*, const char*);
const char* zeroequiv_cf(void*, const char*);
const char* zeroequiv_cd(void*, const char*);

#ifdef __cplusplus
}  // extern "C" 
#endif
