#include "36_function_definition.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>

int em_buildq(em_object _l, em_object _expr){
    return em_invoke("buildq", 2, _l, _expr);
}

int em_macroexpand(em_object _expr){
    return em_invoke("macroexpand", 1, _expr);
}

int em_macroexpand1(em_object _expr){
    return em_invoke("macroexpand1", 1, _expr);
}

int em_splice(em_object _a){
    return em_invoke("splice", 1, _a);
}

int em_apply(em_object _f, em_object _x){
    return em_invoke("apply", 2, _f, _x);
}

int em_block(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("block", n, ptr);
    va_end(ptr);

    return result;
}

int em_block_2(em_object _v, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("block_2", n + 1, _v, ptr);
    va_end(ptr);

    return result;
}

int em_break(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("break", n, ptr);
    va_end(ptr);

    return result;
}

int em_catch(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("catch", n, ptr);
    va_end(ptr);

    return result;
}

int em_compfile(em_object _filename, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("compfile", n + 1, _filename, ptr);
    va_end(ptr);

    return result;
}

int em_compfilefunctions(em_object _filename){
    return em_invoke("compfile", 2, _filename, em_createstring("functions"));
}

int em_compfileall(em_object _filename){
    return em_invoke("compfileall", 2, _filename, em_createstring("functions"));
}

int em_compile(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("compile", n, ptr);
    va_end(ptr);

    return result;
}

int em_compilefunctions(void){
    return em_invoke("compile", 1, em_createstring("functions"));
}

int em_compileall(void){
    return em_invoke("compileall", 1, em_createstring("all"));
}

int em_define(em_object _f, em_object _expr){
    return em_invoke("define", 2, _f, _expr);
}

int em_define_variable(em_object _name, em_object _default_value, em_object _mode){
    return em_invoke("define_variable", 3, _name, _default_value, _mode);
}

int em_dispfun(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("dispfun", n, ptr);
    va_end(ptr);

    return result;
}

int em_dispfunall(void){
    return em_invoke("dispfun", 1, em_createstring("all"));
}

int em_fullmap(em_object _f, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("fullmap", n + 1, _f, ptr);
    va_end(ptr);

    return result;
}

int em_fullmapl(em_object _f, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("fullmapl", n + 1, _f, ptr);
    va_end(ptr);

    return result;
}

int em_fundef(em_object _f){
    return em_invoke("fundef", 1, _f);
}

int em_funmake(em_object _f, em_object _arg){
    return em_invoke("funmake", 2, _f, _arg);
}

int em_lambda(em_object _x, size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("lambda", n + 1, _x, ptr);
    va_end(ptr);

    return result;
}

int em_local(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("local", n, ptr);
    va_end(ptr);

    return result;
}

int em_mode_declare(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("mode_declare", n, ptr);
    va_end(ptr);

    return result;
}

int em_modedeclare(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("modedeclare", n, ptr);
    va_end(ptr);

    return result;
}

int em_mode_identity(em_object _arg1, em_object _arg2){
    return em_invoke("mode_identity", 2, _arg1, _arg2);
}

int em_remfunction(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("remfunction", n, ptr);
    va_end(ptr);

    return result;
}

int em_remfunctionall(void){
    return em_invoke("remfunction", 1, em_createstring("all"));
}

int em_translate(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("translate", n, ptr);
    va_end(ptr);

    return result;
}

int em_translatefunctions(void){
    return em_invoke("translate", 1, em_createstring("functions"));
}

int em_translateall(void){
    return em_invoke("translate", 1, em_createstring("all"));
}

int em_translate_file(em_object _maxima_filename, em_object _lisp_filename){
    return em_invoke("translate_file", 2, _maxima_filename, _lisp_filename);
}

int em_tr_warnings_get(void){
    return em_invoke("tr_warnings_get", 0);
}

int em_compile_file(em_object _filename){
    return em_invoke("compile_file", 1, _filename);
}

int em_compile_file_2(em_object _filename, em_object _compiled_filename){
    return em_invoke("compile_file", 2, _filename, _compiled_filename);
}

int em_compile_file_3(em_object _filename, em_object _compiled_filename, em_object _lisp_filename){
    return em_invoke("compile_file", 3, _filename, _compiled_filename, _lisp_filename);
}

int em_declare_translated(size_t n, ...){
    va_list ptr;
    int result = 0;
    
    va_start(ptr, n);
    result = em_invoke("declare_translated", n, ptr);
    va_end(ptr);

    return result;
}
