#pragma once

#ifdef __cplusplus
extern "C" {
#endif
    
void* create_parser_f();
void* create_parser_d();
void* create_parser_cf();
void* create_parser_cd();

void destroy_parser_f(void*);
void destroy_parser_d(void*);
void destroy_parser_cf(void*);
void destroy_parser_cd(void*);

int compile_parser_f(void*, const char*, void*);
int compile_parser_d(void*, const char*, void*);
int compile_parser_cf(void*, const char*, void*);
int compile_parser_cd(void*, const char*, void*);

void* compile_and_create_expression_parser_f(void*, const char*, void*);
void* compile_and_create_expression_parser_d(void*, const char*, void*);
void* compile_and_create_expression_parser_cf(void*, const char*, void*);
void* compile_and_create_expression_parser_cd(void*, const char*, void*);

const char* error_parser_f(void*);
const char* error_parser_d(void*);
const char* error_parser_cf(void*);
const char* error_parser_cd(void*);

int error_count_parser_f(void*);
int error_count_parser_d(void*);
int error_count_parser_cf(void*);
int error_count_parser_cd(void*);

#ifdef __cplusplus
}  // extern "C" 
#endif
