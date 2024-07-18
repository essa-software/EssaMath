#include "essamath.h"
#include "expression.h"
#include <ecl/ecl.h>
#include <ecl/external.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void init_lib_MAXIMA(cl_object);

void em_initmath(void){
    int argc = 1;
    char buf[256];
    ssize_t count = readlink("/proc/self/exe", buf, 256);
    if(count == -1) {
        return;
    }
    
    char* argv[1];
    *argv = (char*)malloc((size_t)count);
    strncpy(*argv, buf, (size_t)count);
    cl_boot(argc, argv);
    
    ecl_init_module(NULL, init_lib_MAXIMA);

    cl_eval(c_string_to_object("(initialize-runtime-globals)"));
    cl_eval(c_string_to_object("(setf *debugger-hook* nil)"));
    cl_eval(c_string_to_object("(setf $ttyoff 1)"));
    cl_eval(c_string_to_object("(setf *merror-signals-$error-p* T)"));
    cl_eval(c_string_to_object("(setf *maxima-quiet* 1)"));
    cl_eval(c_string_to_object("(setf $errormsg 0)"));
}

void em_freemath(void){
    cl_shutdown();
}

const char* em_getlasterror(void){
    cl_object error = cl_eval(c_string_to_object("($labels)"));

    printf("count: %lu\n", wcslen(error->string.self));

    // if(error->base_string.dim == 0) {
    //     return "No error.";
    // }

    // char* result = (char*)malloc(error->base_string.dim);
    // for(size_t i = 0; i < error->base_string.dim; i++){
    //     result[i] = (char)error->base_string.self[i];
    // }

    // return result;

    return "";
}

extern em_object em_parse(cl_object _list);

em_object em_getvar(const char* _varname){
    char buf[2048] = {0};
    sprintf(buf, "(api-eval \"%s$\")", _varname);
    cl_object obj = cl_eval(c_string_to_object(buf));

    return em_parse(obj);
}
