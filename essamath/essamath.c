#include "essamath.h"
#include <ecl/ecl.h>
#include <ecl/external.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void init_lib_MAXIMA(cl_object);

void em_init_math(void){
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

void em_free_math(void){
    cl_shutdown();
}

const char* em_getlasterror(void){

    return "";
}
