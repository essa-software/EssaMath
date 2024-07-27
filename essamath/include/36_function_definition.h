// 36_function_definition.h
#include "expression.h"
#ifndef FUNCTION_DEFINITION_H
#define FUNCTION_DEFINITION_H

#ifdef __cplusplus
extern "C" {
#endif

int em_buildq(em_object _L, em_object _expr);
int em_macroexpand(em_object _expr);
int em_macroexpand1(em_object _expr);
int em_splice(em_object _a);
int em_apply(em_object _F, em_object _x);
int em_block(size_t n, ...);
int em_block_2(em_object _v, size_t n, ...);
int em_break(size_t n, ...);
int em_catch(size_t n, ...);
int em_compfile(em_object _filename, size_t n, ...);
int em_compfilefunctions(em_object _filename);
int em_compfileall(em_object _filename);
int em_compile(size_t n, ...);
int em_compilefunctions();
int em_compileall();
int em_define(em_object _f, em_object _expr);
int em_define_variable(em_object _name, em_object _default_value, em_object _mode);
int em_dispfun(size_t n, ...);
int em_dispfunall();
int em_fullmap(em_object _f, size_t n, ...);
int em_fullmapl(em_object _f, size_t n, ...);
int em_fundef(em_object _f);
int em_funmake(em_object _F, em_object _arg);
int em_lambda(em_object _x, size_t n, ...);
int em_local(size_t n, ...);
int em_mode_declare(size_t n, ...);
int em_modedeclare(size_t n, ...);
int em_mode_identity(em_object _arg1, em_object _arg2);
int em_remfunction(size_t n, ...);
int em_remfunctionall();
int em_translate(size_t n, ...);
int em_translatefunctions();
int em_translateall();
int em_translate_file(em_object _maxima_filename, em_object _lisp_filename);
int em_tr_warnings_get();
int em_compile_file(em_object _filename);
int em_compile_file_2(em_object _filename, em_object _compiled_filename);
int em_compile_file_3(em_object _filename, em_object _compiled_filename, em_object _lisp_filename);
int em_declare_translated(size_t n, ...);

#ifdef __cplusplus
}
#endif

#endif // FUNCTION_DEFINITION_H
