#include "31_runtime_environment.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "time");

    size_t index = strlen(command);
    command[index++] = '(';
 
    va_start(ptr, n);
    for (size_t i = 0; i < n; i++){
        em_object obj = va_arg(ptr, em_object);
        em_tostring(obj, command + index, size - index);
        index = strlen(command);
        command[index++] = ',';
    }
    command[index - 1] = ')';
 
    // Ending argument list traversal
    va_end(ptr);

    int result =  em_eval(command);
    free(command);

    return result;
}

int em_timedate(){
    return em_invoke("timedate", 0);
}

int em_timedate_2(em_object _T){
    return em_invoke("timedate", 1, _T);
}

int em_timedate_3(em_object _T, em_object _tz_offset){
    return em_invoke("timedate", 2, _T, _tz_offset);
}

int em_parse_timedate(em_object _S){
    return em_invoke("parse_timedate", 1, _S);
}

int em_encode_time(em_object _year, em_object _month, em_object _day, em_object _hours, em_object _minutes, em_object _seconds){
    return em_invoke("encode_time", 6, _year, _month, _day, _hours, _minutes, _seconds);
}

int em_encode_time_2(em_object _year, em_object _month, em_object _day, em_object _hours, em_object _minutes, em_object _seconds, em_object _tz_offset){
    return em_invoke("encode_time", 7, _year, _month, _day, _hours, _minutes, _seconds, _tz_offset);
}

int em_decode_time(em_object _T){
    return em_invoke("decode_time", 1, _T);
}

int em_decode_time_2(em_object _T, em_object _tz_offset){
    return em_invoke("decode_time", 2, _T, _tz_offset);
}

int em_absolute_real_time(){
    return em_invoke("absolute_real_time", 0);
}

int em_elapsed_real_time(){
    return em_invoke("elapsed_real_time", 0);
}

int em_elapsed_run_time(){
    return em_invoke("elapsed_run_time", 0);
}

int em_gensym(){
    return em_invoke("gensym", 0);
}

int em_gensym_2(em_object _x){
    return em_invoke("gensym", 1, _x);
}

int em_remvalue(size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "remvalue");

    size_t index = strlen(command);
    command[index++] = '(';
 
    va_start(ptr, n);
    for (size_t i = 0; i < n; i++){
        em_object obj = va_arg(ptr, em_object);
        em_tostring(obj, command + index, size - index);
        index = strlen(command);
        command[index++] = ',';
    }
    command[index - 1] = ')';
 
    // Ending argument list traversal
    va_end(ptr);

    int result =  em_eval(command);
    free(command);

    return result;
}

int em_remvalueall(){
    return em_invoke("remvalueall", 1, em_createstring("all"));
}

int em_rncombine(em_object _expr){
    return em_invoke("rncombine", 1, _expr);
}

int em_setup_autoload(em_object _filename, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, "setup_autoload");

    size_t index = strlen(command);
    command[index++] = '(';
    em_tostring(_filename, command + index, size - index);
    index = strlen(command);
    command[index++] = ',';
 
    va_start(ptr, n);
    for (size_t i = 0; i < n; i++){
        em_object obj = va_arg(ptr, em_object);
        em_tostring(obj, command + index, size - index);
        index = strlen(command);
        command[index++] = ',';
    }
    command[index - 1] = ')';
 
    // Ending argument list traversal
    va_end(ptr);

    int result =  em_eval(command);
    free(command);

    return result;
}
