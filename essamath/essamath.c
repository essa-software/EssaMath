#include "essamath.h"
#include "expression.h"
#include <ctype.h>
#include <stddef.h>
#include <ecl/ecl.h>
#include <ecl/external.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "expression_functions.h"
#include "expression_domain.h"

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

    printf("%s\n", argv[0]);

    ecl_set_option(ECL_OPT_TRAP_SIGSEGV, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGFPE, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGINT, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGILL, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGBUS, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGPIPE, FALSE);

    cl_boot(argc, argv);
    
    ecl_init_module(NULL, init_lib_MAXIMA);


    cl_eval(c_string_to_object("(initialize-runtime-globals)"));
    cl_eval(c_string_to_object("(setf *debugger-hook* nil)"));
    cl_eval(c_string_to_object("(setf $ttyoff 1)"));
    cl_eval(c_string_to_object("(setf *merror-signals-$error-p* T)"));
    cl_eval(c_string_to_object("(setf *maxima-quiet* 1)"));
    cl_eval(c_string_to_object("(setf $errormsg 0)"));

    em_inithashmapdouble();
    em_inithashmapdouble_domain();
    em_inithashmapcomplex();
    em_inithashmapcomplex_domain();
}

void em_freemath(void){
    em_freehashmapdouble();
    em_freehashmapdouble_domain();
    em_freehashmapcomplex();
    em_freehashmapcomplex_domain();
    
    cl_shutdown();
}

em_object em_getlast(const char* _varname){
    em_object object = em_getvar("labels");
    char* result = (char*)malloc(64);
    memset(result, 0, 64);

    object = object->emVal.emList->emNext;
    while(object){
        if(object->emType == EM_STRING && strncmp(_varname, object->emVal.emString, strlen(_varname)) == 0){
            strcpy(result, object->emVal.emString + 1);
            break;
        }
        object = object->emNext;
    }
    em_rellist(object);

    if(result == NULL){
        return NULL;
    }

    object = (em_object)malloc(sizeof(struct EmList));
    object->emVal.emString = result;
    object->emType = EM_STRING;
    object->emNext = NULL;

    return object;
}

em_object em_getlastoutput(void){
    return em_getlast("$%o");
}

em_object em_getlastintermediate(void){
    return em_getlast("$%t");
}

extern em_object em_parse(cl_object _list);

em_object em_getvar(const char* _varname){
    size_t size = strlen(_varname);
    for(size_t i = 0; i < size; i++){
        if(!isalpha(_varname[i])){
            return NULL;
        }
    }

    char buf[2048] = {0};
    sprintf(buf, "(api-eval \"%s$\")", _varname);
    cl_object obj = cl_eval(c_string_to_object(buf));

    return em_parse(obj);
}

void em_setvar(const char* _varname, em_object _value){
    size_t size = strlen(_varname);
    for(size_t i = 0; i < size; i++){
        if(!isalpha(_varname[i])){
            return;
        }
    }

    char value[256] = {0};
    em_tostring(_value, value, 256);
    char processedvalue[1024] = {0};
    if(_value->emType == EM_STRING){
        sprintf(processedvalue, "\"%s\"", value);
    }else{
        sprintf(processedvalue, "%s", value);
    }
    char buf[2048] = {0};
    sprintf(buf, "(setq %s %s)", _varname, processedvalue);
    cl_eval(c_string_to_object(buf));
}
