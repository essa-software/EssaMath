#pragma once

#ifdef __cplusplus
extern "C" {
#endif
    
void* create_symbol_table_f();
void* create_symbol_table_d();
void* create_symbol_table_cf();
void* create_symbol_table_cd();

void* clone_symbol_table_f(void*);
void* clone_symbol_table_d(void*);
void* clone_symbol_table_cf(void*);
void* clone_symbol_table_cd(void*);

void destroy_symbol_table_f(void*);
void destroy_symbol_table_d(void*);
void destroy_symbol_table_cf(void*);
void destroy_symbol_table_cd(void*);

void assign_symbol_table_f(void*, void*);
void assign_symbol_table_d(void*, void*);
void assign_symbol_table_cf(void*, void*);
void assign_symbol_table_cd(void*, void*);

int compare_symbol_table_f(void*, void*);
int compare_symbol_table_d(void*, void*);
int compare_symbol_table_cf(void*, void*);
int compare_symbol_table_cd(void*, void*);

int mutability_symbol_table_f(void*);
int mutability_symbol_table_d(void*);
int mutability_symbol_table_cf(void*);
int mutability_symbol_table_cd(void*);

void clear_variables_symbol_table_f(void*, int);
void clear_variables_symbol_table_d(void*, int);
void clear_variables_symbol_table_cf(void*, int);
void clear_variables_symbol_table_cd(void*, int);

void clear_vectors_symbol_table_f(void*);
void clear_vectors_symbol_table_d(void*);
void clear_vectors_symbol_table_cf(void*);
void clear_vectors_symbol_table_cd(void*);

void clear_local_constants_symbol_table_f(void*);
void clear_local_constants_symbol_table_d(void*);
void clear_local_constants_symbol_table_cf(void*);
void clear_local_constants_symbol_table_cd(void*);

void clear_symbol_table_f(void*);
void clear_symbol_table_d(void*);
void clear_symbol_table_cf(void*);
void clear_symbol_table_cd(void*);

int variable_count_symbol_table_f(void*);
int variable_count_symbol_table_d(void*);
int variable_count_symbol_table_cf(void*);
int variable_count_symbol_table_cd(void*);

int vector_count_symbol_table_f(void*);
int vector_count_symbol_table_d(void*);
int vector_count_symbol_table_cf(void*);
int vector_count_symbol_table_cd(void*);

int is_constant_node_symbol_table_f(void*, const char*);
int is_constant_node_symbol_table_d(void*, const char*);
int is_constant_node_symbol_table_cf(void*, const char*);
int is_constant_node_symbol_table_cd(void*, const char*);

int create_variable_symbol_table_f(void*, const char*, void*);
int create_variable_symbol_table_d(void*, const char*, void*);
int create_variable_symbol_table_cf(void*, const char*, void*);
int create_variable_symbol_table_cd(void*, const char*, void*);

int add_variable_symbol_table_f(void*, const char*, void*, int);
int add_variable_symbol_table_d(void*, const char*, void*, int);
int add_variable_symbol_table_cf(void*, const char*, void*, int);
int add_variable_symbol_table_cd(void*, const char*, void*, int);

int add_constant_symbol_table_f(void*, const char*, void*);
int add_constant_symbol_table_d(void*, const char*, void*);
int add_constant_symbol_table_cf(void*, const char*, void*);
int add_constant_symbol_table_cd(void*, const char*, void*);

int remove_variable_symbol_table_f(void*, const char*, int);
int remove_variable_symbol_table_d(void*, const char*, int);
int remove_variable_symbol_table_cf(void*, const char*, int);
int remove_variable_symbol_table_cd(void*, const char*, int);

int remove_vector_symbol_table_f(void*, const char*);
int remove_vector_symbol_table_d(void*, const char*);
int remove_vector_symbol_table_cf(void*, const char*);
int remove_vector_symbol_table_cd(void*, const char*);

int add_constants_symbol_table_f(void*);
int add_constants_symbol_table_d(void*);
int add_constants_symbol_table_cf(void*);
int add_constants_symbol_table_cd(void*);

int add_pi_symbol_table_f(void*);
int add_pi_symbol_table_d(void*);
int add_pi_symbol_table_cf(void*);
int add_pi_symbol_table_cd(void*);

int add_e_symbol_table_f(void*);
int add_e_symbol_table_d(void*);
int add_e_symbol_table_cf(void*);
int add_e_symbol_table_cd(void*);

int add_i_symbol_table_f(void*);
int add_i_symbol_table_d(void*);
int add_i_symbol_table_cf(void*);
int add_i_symbol_table_cd(void*);

int add_epsilon_symbol_table_f(void*);
int add_epsilon_symbol_table_d(void*);
int add_epsilon_symbol_table_cf(void*);
int add_epsilon_symbol_table_cd(void*);

int add_infinity_symbol_table_f(void*);
int add_infinity_symbol_table_d(void*);
int add_infinity_symbol_table_cf(void*);
int add_infinity_symbol_table_cd(void*);

int symbol_exists_symbol_table_f(void*, const char*, int);
int symbol_exists_symbol_table_d(void*, const char*, int);
int symbol_exists_symbol_table_cf(void*, const char*, int);
int symbol_exists_symbol_table_cd(void*, const char*, int);

int is_variable_symbol_table_f(void*, const char*);
int is_variable_symbol_table_d(void*, const char*);
int is_variable_symbol_table_cf(void*, const char*);
int is_variable_symbol_table_cd(void*, const char*);

int is_vector_symbol_table_f(void*, const char*);
int is_vector_symbol_table_d(void*, const char*);
int is_vector_symbol_table_cf(void*, const char*);
int is_vector_symbol_table_cd(void*, const char*);

int valid_symbol_table_f(void*);
int valid_symbol_table_d(void*);
int valid_symbol_table_cf(void*);
int valid_symbol_table_cd(void*);

#ifdef __cplusplus
}  // extern "C" 
#endif
