#include "31_runtime_environment.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>

int em_sstatus(em_object _keyword, em_object _item){
    return em_invoke("sstatus", 2, _keyword, _item);
}

int em_status(em_object _feature){
    return em_invoke("status", 1, _feature);
}

int em_status_2(em_object _feature, em_object _item){
    return em_invoke("status", 2, _feature, _item);
}

int em_system(em_object _command){
    return em_invoke("system", 1, _command);
}

int em_time(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("time", n, ptr);
    va_end(ptr);

    return result;
}

int em_timedate(void){
    return em_invoke("timedate", 0);
}

int em_timedate_2(em_object _t){
    return em_invoke("timedate", 1, _t);
}

int em_timedate_3(em_object _t, em_object _tz_offset){
    return em_invoke("timedate", 2, _t, _tz_offset);
}

int em_parse_timedate(em_object _s){
    return em_invoke("parse_timedate", 1, _s);
}

int em_encode_time(em_object _year, em_object _month, em_object _day, em_object _hours, em_object _minutes, em_object _seconds){
    return em_invoke("encode_time", 6, _year, _month, _day, _hours, _minutes, _seconds);
}

int em_encode_time_2(em_object _year, em_object _month, em_object _day, em_object _hours, em_object _minutes, em_object _seconds, em_object _tz_offset){
    return em_invoke("encode_time", 7, _year, _month, _day, _hours, _minutes, _seconds, _tz_offset);
}

int em_decode_time(em_object _t){
    return em_invoke("decode_time", 1, _t);
}

int em_decode_time_2(em_object _t, em_object _tz_offset){
    return em_invoke("decode_time", 2, _t, _tz_offset);
}

int em_absolute_real_time(void){
    return em_invoke("absolute_real_time", 0);
}

int em_elapsed_real_time(void){
    return em_invoke("elapsed_real_time", 0);
}

int em_elapsed_run_time(void){
    return em_invoke("elapsed_run_time", 0);
}

int em_gensym(void){
    return em_invoke("gensym", 0);
}

int em_gensym_2(em_object _x){
    return em_invoke("gensym", 1, _x);
}

int em_remvalue(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("remvalue", n, ptr);
    va_end(ptr);

    return result;
}

int em_remvalueall(void){
    return em_invoke("remvalueall", 1, em_createstring("all"));
}

int em_rncombine(em_object _expr){
    return em_invoke("rncombine", 1, _expr);
}

int em_setup_autoload(em_object _filename, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("remarray", n + 1, _filename, ptr);
    va_end(ptr);

    return result;
}
