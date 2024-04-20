#pragma once

#ifdef __cplusplus
extern "C" {
#endif
    
void* create_expression_f();
void* create_expression_d();
void* create_expression_cf();
void* create_expression_cd();

void* clone_expression_f(void*);
void* clone_expression_d(void*);
void* clone_expression_cf(void*);
void* clone_expression_cd(void*);

void destroy_expression_f(void*);
void destroy_expression_d(void*);
void destroy_expression_cf(void*);
void destroy_expression_cd(void*);

void assign_expression_f(void*, void*);
void assign_expression_d(void*, void*);
void assign_expression_cf(void*, void*);
void assign_expression_cd(void*, void*);

int compare_expression_f(void*, void*);
int compare_expression_d(void*, void*);
int compare_expression_cf(void*, void*);
int compare_expression_cd(void*, void*);

int negate_expression_f(void*);
int negate_expression_d(void*);
int negate_expression_cf(void*);
int negate_expression_cd(void*);

void* value_expression_f(void*);
void* value_expression_d(void*);
void* value_expression_cf(void*);
void* value_expression_cd(void*);

void free_value_expression_f(void*);
void free_value_expression_d(void*);
void free_value_expression_cf(void*);
void free_value_expression_cd(void*);

int is_true_expression_f(void*);
int is_true_expression_d(void*);
int is_true_expression_cf(void*);
int is_true_expression_cd(void*);

void register_symbol_table_expression_f(void*, void*);
void register_symbol_table_expression_d(void*, void*);
void register_symbol_table_expression_cf(void*, void*);
void register_symbol_table_expression_cd(void*, void*);

void* get_symbol_table_expression_f(void*, int);
void* get_symbol_table_expression_d(void*, int);
void* get_symbol_table_expression_cf(void*, int);
void* get_symbol_table_expression_cd(void*, int);

const char* to_string_expression_f(void*);
const char* to_string_expression_d(void*);
const char* to_string_expression_cf(void*);
const char* to_string_expression_cd(void*);

int strlen_api(const char*);

void free_string_api(const char*);

#ifdef __cplusplus
}  // extern "C" 
#endif
