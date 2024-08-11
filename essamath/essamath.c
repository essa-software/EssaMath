#include "essamath.h"
#include "expression.h"
#include <ctype.h>
#include <printf.h>
#include <stddef.h>
#include <ecl/ecl.h>
#include <ecl/external.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "expression_functions.h"

void init_lib_MAXIMA(cl_object);

// NOLINTBEGIN(readability-non-const-parameter)

static int em_printf_list_handler(
	FILE *stream, /*  stream output */
	const struct printf_info *info, /* information about the various options */
         const void *const *args /* arguments */
	)
	{
    (void)info;

	int ret=0;
	const em_object obj=*((const em_object const*)(args[0]));
	
    char buf[1024] = {0};

    em_printlist(buf, 1024, obj);

	ret+=fputs(buf,stream);
	
	return ret;
}

static int em_printf_list_arginfo(
	const struct printf_info *info,
	size_t n,
	int *argtypes,
	int *size)
	{
    (void)info;
    (void)size;

	 if (n > 0) { argtypes[0] = PA_POINTER;}
	return 1;
	}

static int em_printf_expr_handler(
	FILE *stream, /*  stream output */
	const struct printf_info *info, /* information about the various options */
         const void *const *args /* arguments */
	)
	{
    (void)info;

	int ret=0;
	const em_object obj=*((const em_object const*)(args[0]));
	
    char buf[1024] = {0};

    em_printexpr(obj,buf, 1024);

	ret+=fputs(buf,stream);
	
	return ret;
}

static int em_printf_expr_arginfo(
	const struct printf_info *info,
	size_t n,
	int *argtypes,
	int *size)
	{
    (void)info;
    (void)size;

	 if (n > 0) { argtypes[0] = PA_POINTER;}
	return 1;
	}

static int em_printf_val_handler(
	FILE *stream, /*  stream output */
	const struct printf_info *info, /* information about the various options */
         const void *const *args /* arguments */
	)
	{
    (void)info;

	int ret=0;
	const em_val* obj=*((const em_val**)(args[0]));
	
    char buf[1024] = {0};

    em_printval(obj, buf, 1024);

	ret+=fputs(buf,stream);
	
	return ret;
}

static int em_printf_val_arginfo(
	const struct printf_info *info,
	size_t n,
	int *argtypes,
	int *size)
	{
    (void)info;
    (void)size;

	 if (n > 0) { argtypes[0] = PA_POINTER;}
	return 1;
	}

// NOLINTEND(readability-non-const-parameter)

void em_initmath(void){
    int argc = 1;
    char buf[256];
    char* argv[1];

    ssize_t count = readlink("/proc/self/exe", buf, 256);
    if(count == -1) {
        return;
    }
    
    *argv = (char*)malloc((size_t)count);
    strncpy(*argv, buf, (size_t)count);

    // printf("%s\n", argv[0]);

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

	if(register_printf_specifier('C',
		/* handler function */
		em_printf_list_handler,
		/* arginfo function */
		em_printf_list_arginfo 
		)!=0)
	{
		fputs("Cannot register print function",stderr);
	}

	if(register_printf_specifier('R',
		/* handler function */
		em_printf_expr_handler,
		/* arginfo function */
		em_printf_expr_arginfo 
		)!=0)
	{
		fputs("Cannot register print function",stderr);
	}

	if(register_printf_specifier('V',
		/* handler function */
		em_printf_val_handler,
		/* arginfo function */
		em_printf_val_arginfo 
		)!=0)
	{
		fputs("Cannot register print function",stderr);
	}
}

void em_freemath(void){
    em_freehashmapdouble();
    
    cl_shutdown();
}

extern em_object em_parse(cl_object _list);
em_object em_getlastoutput(void){
	cl_object obj = cl_eval(c_string_to_object("(api-eval \"second(labels)$\")"));

    return em_parse(obj);
}

em_object em_getlastintermediate(void){
	cl_object obj = cl_eval(c_string_to_object("(api-eval \"third(labels)$\")"));

    return em_parse(obj);
}


em_object em_getvar(const char* _varname){
    size_t size = strlen(_varname);
    char buf[2048] = {0};
	cl_object obj;

    for(size_t i = 0; i < size; i++){
        if(!isalpha(_varname[i])){
            return NULL;
        }
    }

    sprintf(buf, "(api-eval \"%s$\")", _varname);
    obj = cl_eval(c_string_to_object(buf));

    return em_parse(obj);
}

void em_setvar(const char* _varname, em_object _value){
    size_t size = strlen(_varname);
    char value[256] = {0};
    char processedvalue[1024] = {0};
    char buf[2048] = {0};

    for(size_t i = 0; i < size; i++){
        if(!isalpha(_varname[i])){
            return;
        }
    }

    em_printexpr(_value, value, 256);
    if(_value->emType == EM_STRING){
        sprintf(processedvalue, "\"%s\"", value);
    }else{
        sprintf(processedvalue, "%s", value);
    }
    sprintf(buf, "(setq %s %s)", _varname, processedvalue);
    cl_eval(c_string_to_object(buf));
}
