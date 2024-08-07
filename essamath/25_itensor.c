#include "25_itensor.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>

int em_dispcon(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("dispcon", n, ptr);
    va_end(ptr);

    return result;
}

int em_dispconall(void){
    return em_invoke("dispcon", 1, em_createstring("all"));
}

int em_entertensor(em_object _name){
    return em_invoke("entertensor", 1, _name);
}

int em_changename(em_object _old, em_object _new, em_object _expr){
    return em_invoke("changename", 3, _old, _new, _expr);
}

int em_listoftens(em_object _expr){
    return em_invoke("listoftens", 1, _expr);
}

int em_indices(em_object _expr){
    return em_invoke("indices", 1, _expr);
}

int em_rename(em_object _expr){
    return em_invoke("rename", 1, _expr);
}

int em_rename_2(em_object _expr, em_object _count){
    return em_invoke("rename", 2, _expr, _count);
}

int em_defcon(em_object _tensor1){
    return em_invoke("defcon", 1, _tensor1);
}

int em_defcon_2(em_object _tensor1, em_object _tensor2, em_object _tensor3){
    return em_invoke("defcon", 3, _tensor1, _tensor2, _tensor3);
}

int em_remcon(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("remcon", n, ptr);
    va_end(ptr);

    return result;
}

int em_remconall(void){
    return em_invoke("remcon", 1, em_createstring("all"));
}

int em_contract(em_object _expr){
    return em_invoke("contract", 1, _expr);
}

int em_indexed_tensor(em_object _tensor){
    return em_invoke("indexed_tensor", 1, _tensor);
}

int em_components(em_object _tensor, em_object _expr){
    return em_invoke("components", 2, _tensor, _expr);
}

int em_remcomps(em_object _tensor){
    return em_invoke("remcomps", 1, _tensor);
}

int em_showcomps(em_object _tensor){
    return em_invoke("showcomps", 1, _tensor);
}

int em_idummy(void){
    return em_invoke("idummy", 0);
}

int em_kdelta(em_object _l1, em_object _l2){
    return em_invoke("kdelta", 2, _l1, _l2);
}

int em_kdels(em_object _l1, em_object _l2){
    return em_invoke("kdels", 2, _l1, _l2);
}

int em_levi_civita(em_object _l){
    return em_invoke("levi_civita", 1, _l);
}

int em_lc2kdt(em_object _expr){
    return em_invoke("lc2kdt", 1, _expr);
}

int em_lc_u(em_object _expr){
    return em_invoke("lc_u", 1, _expr);
}

int em_canten(em_object _expr){
    return em_invoke("canten", 1, _expr);
}

int em_concan(em_object _expr){
    return em_invoke("concan", 1, _expr);
}

int em_decsym(em_object _tensor, em_object _m, em_object _n, em_object _cov, em_object _contr){
    return em_invoke("decsym", 5, _tensor, _m, _n, _cov, _contr);
}

int em_remsym(em_object _tensor, em_object _m, em_object _n){
    return em_invoke("remsym", 3, _tensor, _m, _n);
}

int em_dispsym(em_object _tensor, em_object _m, em_object _n){
    return em_invoke("dispsym", 3, _tensor, _m, _n);
}

int em_canform(em_object _expr){
    return em_invoke("canform", 1, _expr);
}

int em_canform_2(em_object _expr, em_object _rename){
    return em_invoke("canform", 2, _expr, _rename);
}

int em_idiff(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("idiff", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_liediff(em_object _v ,em_object _ten){
    return em_invoke("liediff", 2, _v ,_ten);
}

int em_rediff(em_object _ten){
    return em_invoke("rediff", 1, _ten);
}

int em_undiff(em_object _expr){
    return em_invoke("undiff", 1, _expr);
}

int em_evundiff(em_object _expr){
    return em_invoke("evundiff", 1, _expr);
}

int em_flush(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("flush", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_flushd(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("flushd", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_flushnd(em_object _expr, em_object _tensor, em_object _n){
    return em_invoke("flushnd", 3, _expr, _tensor, _n);
}

int em_coord(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("coord", n, ptr);
    va_end(ptr);

    return result;
}

int em_remcoord(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("remcoord", n, ptr);
    va_end(ptr);

    return result;
}

int em_remcoordall(void){
    return em_invoke("remcoordall", 1, em_createstring("all"));
}

int em_makebox(em_object _expr, em_object _g){
    return em_invoke("makebox", 2, _expr, _g);
}

int em_conmetderiv(em_object _expr, em_object _tensor){
    return em_invoke("conmetderiv", 2, _expr, _tensor);
}

int em_simpmetderiv(em_object _expr){
    return em_invoke("simpmetderiv", 1, _expr);
}

int em_simpmetderiv_2(em_object _expr, em_object _options){
    return em_invoke("simpmetderiv", 2, _expr, _options);
}

int em_flush1deriv(em_object _expr, em_object _tensor){
    return em_invoke("flush1deriv", 2, _expr, _tensor);
}

int em_imetric(em_object _g){
    return em_invoke("imetric", 1, _g);
}

int em_idim(em_object _n){
    return em_invoke("idim", 1, _n);
}

int em_ichr1(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("ichr1", n, ptr);
    va_end(ptr);

    return result;
}

int em_ichr2(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("ichr2", n, ptr);
    va_end(ptr);

    return result;
}

int em_icurvature(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("icurvature", n, ptr);
    va_end(ptr);

    return result;
}

int em_covdiff(em_object _expr, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("covdiff", n + 1, _expr, ptr);
    va_end(ptr);

    return result;
}

int em_lorentz_gauge(em_object _expr){
    return em_invoke("lorentz_gauge", 1, _expr);
}

int em_igeodesic_coords(em_object _expr, em_object _name){
    return em_invoke("igeodesic_coords", 2, _expr, _name);
}

int em_iframes(void){
    return em_invoke("iframes", 0);
}

int em_extdiff(em_object _expr, em_object _i){
    return em_invoke("extdiff", 2, _expr, _i);
}

int em_hodge(em_object _expr){
    return em_invoke("hodge", 1, _expr);
}

int em_tentex(em_object _expr){
    return em_invoke("tentex", 1, _expr);
}

int em_ic_convert(em_object _eqn){
    return em_invoke("ic_convert", 1, _eqn);
}
