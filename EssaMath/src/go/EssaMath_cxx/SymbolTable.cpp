#include "SymbolTable.h"
#include <EssaMath/EssaMath.hpp>
#include <EssaMath/SymbolTable.hpp>
#include <unistd.h>

template<typename T>
void* create_symbol_table(){
    return new Essa::Math::symbol_table<T>();
}

void* create_symbol_table_f(){
    return create_symbol_table<float>();
}

void* create_symbol_table_d(){
    return create_symbol_table<double>();
}

void* create_symbol_table_cf(){
    return create_symbol_table<std::complex<float>>();
}

void* create_symbol_table_cd(){
    return create_symbol_table<std::complex<double>>();
}

template<typename T>
void* clone_symbol_table(void* _other){
    return new Essa::Math::symbol_table<T>(*static_cast<Essa::Math::symbol_table<T>*>(_other));
}

void* clone_symbol_table_f(void* _other){
    return clone_symbol_table<float>(_other);
}
void* clone_symbol_table_d(void* _other){
    return clone_symbol_table<double>(_other);
}
void* clone_symbol_table_cf(void* _other){
    return clone_symbol_table<std::complex<float>>(_other);
}
void* clone_symbol_table_cd(void* _other){
    return clone_symbol_table<std::complex<double>>(_other);
}

template<typename T>
void destroy_symbol_table(void* _ptr){
    delete static_cast<Essa::Math::symbol_table<T>*>(_ptr);
}

void destroy_symbol_table_f(void* _ptr){
    destroy_symbol_table<float>(_ptr);
}

void destroy_symbol_table_d(void* _ptr){
    destroy_symbol_table<double>(_ptr);
}

void destroy_symbol_table_cf(void* _ptr){
    destroy_symbol_table<std::complex<float>>(_ptr);
}

void destroy_symbol_table_cd(void* _ptr){
    destroy_symbol_table<std::complex<double>>(_ptr);
}

template<typename T>
void assign_symbol_table(void* _self, void* _other){
    *static_cast<Essa::Math::symbol_table<T>*>(_self) = *static_cast<Essa::Math::symbol_table<T>*>(_other);
}

void assign_symbol_table_f(void* _self, void* _other){
    assign_symbol_table<float>(_self, _other);
}

void assign_symbol_table_d(void* _self, void* _other){
    assign_symbol_table<double>(_self, _other);
}

void assign_symbol_table_cf(void* _self, void* _other){
    assign_symbol_table<std::complex<float>>(_self, _other);
}

void assign_symbol_table_cd(void* _self, void* _other){
    assign_symbol_table<std::complex<double>>(_self, _other);
}

template<typename T>
int compare_symbol_table(void* _self, void* _other){
    return *static_cast<Essa::Math::symbol_table<T>*>(_self) == *static_cast<Essa::Math::symbol_table<T>*>(_other);
}

int compare_symbol_table_f(void* _self, void* _other){
    return compare_symbol_table<float>(_self, _other);
}

int compare_symbol_table_d(void* _self, void* _other){
    return compare_symbol_table<double>(_self, _other);
}

int compare_symbol_table_cf(void* _self, void* _other){
    return compare_symbol_table<std::complex<float>>(_self, _other);
}

int compare_symbol_table_cd(void* _self, void* _other){
    return compare_symbol_table<std::complex<double>>(_self, _other);
}

template<typename T>
int mutability_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->mutability();
}

int mutability_symbol_table_f(void* _self){
    return mutability_symbol_table<float>(_self);
}

int mutability_symbol_table_d(void* _self){
    return mutability_symbol_table<double>(_self);
}

int mutability_symbol_table_cf(void* _self){
    return mutability_symbol_table<std::complex<float>>(_self);
}

int mutability_symbol_table_cd(void* _self){
    return mutability_symbol_table<std::complex<double>>(_self);
}

template<typename T>
void clear_variables_symbol_table(void* _self, bool delete_node){
    static_cast<Essa::Math::symbol_table<T>*>(_self)->clear_variables(delete_node);
}

void clear_variables_symbol_table_f(void* self, int delete_node){
    clear_variables_symbol_table<float>(self, delete_node);
}

void clear_variables_symbol_table_d(void* self, int delete_node){
    clear_variables_symbol_table<double>(self, delete_node);
}

void clear_variables_symbol_table_cf(void* self, int delete_node){
    clear_variables_symbol_table<std::complex<float>>(self, delete_node);
}

void clear_variables_symbol_table_cd(void* self, int delete_node){
    clear_variables_symbol_table<std::complex<double>>(self, delete_node);
}

template<typename T>
void clear_vectors_symbol_table(void* _self){
    static_cast<Essa::Math::symbol_table<T>*>(_self)->clear_vectors();
}

void clear_vectors_symbol_table_f(void* _self){
    clear_vectors_symbol_table<float>(_self);
}

void clear_vectors_symbol_table_d(void* _self){
    clear_vectors_symbol_table<double>(_self);
}

void clear_vectors_symbol_table_cf(void* _self){
    clear_vectors_symbol_table<std::complex<float>>(_self);
}

void clear_vectors_symbol_table_cd(void* _self){
    clear_vectors_symbol_table<std::complex<double>>(_self);
}

template<typename T>
void clear_local_constants_symbol_table(void* _self){
    static_cast<Essa::Math::symbol_table<T>*>(_self)->clear_local_constants();
}

void clear_local_constants_symbol_table_f(void* _self){
    clear_local_constants_symbol_table<float>(_self);
}
void clear_local_constants_symbol_table_d(void* _self){
    clear_local_constants_symbol_table<double>(_self);
}
void clear_local_constants_symbol_table_cf(void* _self){
    clear_local_constants_symbol_table<std::complex<float>>(_self);
}
void clear_local_constants_symbol_table_cd(void* _self){
    clear_local_constants_symbol_table<std::complex<double>>(_self);
}

template<typename T>
void clear_symbol_table(void* _self){
    static_cast<Essa::Math::symbol_table<T>*>(_self)->clear();
}

void clear_symbol_table_f(void* _self){
    clear_symbol_table<float>(_self);
}

void clear_symbol_table_d(void* _self){
    clear_symbol_table<double>(_self);
}

void clear_symbol_table_cf(void* _self){
    clear_symbol_table<std::complex<float>>(_self);
}

void clear_symbol_table_cd(void* _self){
    clear_symbol_table<std::complex<double>>(_self);
}

template<typename T>
int variable_count_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->vector_count();
}

int variable_count_symbol_table_f(void* _self){
    return variable_count_symbol_table<float>(_self);
}
int variable_count_symbol_table_d(void* _self){
    return variable_count_symbol_table<double>(_self);
}
int variable_count_symbol_table_cf(void* _self){
    return variable_count_symbol_table<std::complex<float>>(_self);
}
int variable_count_symbol_table_cd(void* _self){
    return variable_count_symbol_table<std::complex<double>>(_self);
}

template<typename T>
int vector_count_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->vector_count();
}

int vector_count_symbol_table_f(void* _self){
    return vector_count_symbol_table<float>(_self);
}
int vector_count_symbol_table_d(void* _self){
    return vector_count_symbol_table<double>(_self);
}
int vector_count_symbol_table_cf(void* _self){
    return vector_count_symbol_table<std::complex<float>>(_self);
}
int vector_count_symbol_table_cd(void* _self){
    return vector_count_symbol_table<std::complex<double>>(_self);
}

template<typename T>
int is_constant_node_symbol_table(void* _self, const char* _name){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->is_constant_node(_name);
}

int is_constant_node_symbol_table_f(void* _self, const char* _name){
    return is_constant_node_symbol_table<float               >(_self,_name);
}

int is_constant_node_symbol_table_d(void* _self, const char* _name){
    return is_constant_node_symbol_table<double              >(_self,_name);
}

int is_constant_node_symbol_table_cf(void* _self, const char* _name){
    return is_constant_node_symbol_table<std::complex<float> >(_self,_name);
}

int is_constant_node_symbol_table_cd(void* _self, const char* _name){
    return is_constant_node_symbol_table<std::complex<double>>(_self,_name);
}

template<typename T>
int create_variable_symbol_table(void* _self, const char* _name, void* _ptr){
    T _value = T(*static_cast<T*>(_ptr)); 
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->create_variable(_name, _value);
}

int create_variable_symbol_table_f(void* _self, const char* _name, void* _ptr){
    return create_variable_symbol_table<float>(_self, _name, _ptr);
}
int create_variable_symbol_table_d(void* _self, const char* _name, void* _ptr){
    return create_variable_symbol_table<double>(_self, _name, _ptr);
}
int create_variable_symbol_table_cf(void* _self, const char* _name, void* _ptr){
    return create_variable_symbol_table<std::complex<float>>(_self, _name, _ptr);
}
int create_variable_symbol_table_cd(void* _self, const char* _name, void* _ptr){
    return create_variable_symbol_table<std::complex<double>>(_self, _name, _ptr);
}

template<typename T>
int add_variable_symbol_table(void* _self, const char* _name, void* _ptr, int _constant){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->add_variable(_name, *static_cast<T*>(_ptr), _constant);
}

int add_variable_symbol_table_f(void* _self, const char* _name, void* _ptr, int _constant){
    return add_variable_symbol_table<float>(_self, _name, _ptr, _constant);
}
int add_variable_symbol_table_d(void* _self, const char* _name, void* _ptr, int _constant){
    return add_variable_symbol_table<double>(_self, _name, _ptr, _constant);
}
int add_variable_symbol_table_cf(void* _self, const char* _name, void* _ptr, int _constant){
    return add_variable_symbol_table<std::complex<float>>(_self, _name, _ptr, _constant);
}
int add_variable_symbol_table_cd(void* _self, const char* _name, void* _ptr, int _constant){
    return add_variable_symbol_table<std::complex<double>>(_self, _name, _ptr, _constant);
}

template<typename T>
int add_constant_symbol_table(void* _self, const char* _name, void* _ptr){
    T _value = T(*static_cast<T*>(_ptr)); 
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->add_constant(_name, _value);
}

int add_constant_symbol_table_f(void* _self, const char* _name, void* _ptr){
    return add_constant_symbol_table<float>(_self, _name, _ptr);
}

int add_constant_symbol_table_d(void* _self, const char* _name, void* _ptr){
    return add_constant_symbol_table<double>(_self, _name, _ptr);
}

int add_constant_symbol_table_cf(void* _self, const char* _name, void* _ptr){
    return add_constant_symbol_table<std::complex<float>>(_self, _name, _ptr);
}

int add_constant_symbol_table_cd(void* _self, const char* _name, void* _ptr){
    return add_constant_symbol_table<std::complex<double>>(_self, _name, _ptr);
}

template<typename T>
int remove_variable_symbol_table(void* _self, const char* _name, int delete_node){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->remove_variable(_name, delete_node);
}

int remove_variable_symbol_table_f(void* _self, const char* _name, int delete_node){
    return remove_variable_symbol_table<float>(_self, _name, delete_node);
}

int remove_variable_symbol_table_d(void* _self, const char* _name, int delete_node){
    return remove_variable_symbol_table<double>(_self, _name, delete_node);
}

int remove_variable_symbol_table_cf(void* _self, const char* _name, int delete_node){
    return remove_variable_symbol_table<std::complex<float>>(_self, _name, delete_node);
}

int remove_variable_symbol_table_cd(void* _self, const char* _name, int delete_node){
    return remove_variable_symbol_table<std::complex<double>>(_self, _name, delete_node);
}

template<typename T>
int remove_vector_symbol_table(void* _self, const char* _name){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->remove_vector(_name);
}

int remove_vector_symbol_table_f(void* _self, const char* _name){
    return remove_vector_symbol_table<float>(_self, _name);
}

int remove_vector_symbol_table_d(void* _self, const char* _name){
    return remove_vector_symbol_table<double>(_self, _name);
}

int remove_vector_symbol_table_cf(void* _self, const char* _name){
    return remove_vector_symbol_table<std::complex<float>>(_self, _name);
}

int remove_vector_symbol_table_cd(void* _self, const char* _name){
    return remove_vector_symbol_table<std::complex<double>>(_self, _name);
}

template<typename T>
int add_constants_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->add_constants();
}

int add_constants_symbol_table_f(void* _self){
    return add_constants_symbol_table<float>(_self);
}

int add_constants_symbol_table_d(void* _self){
    return add_constants_symbol_table<double>(_self);
}

int add_constants_symbol_table_cf(void* _self){
    return add_constants_symbol_table<std::complex<float>>(_self);
}

int add_constants_symbol_table_cd(void* _self){
    return add_constants_symbol_table<std::complex<double>>(_self);
}

template<typename T>
int add_pi_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->add_pi();
}

int add_pi_symbol_table_f(void* _self){
    return add_pi_symbol_table<float>(_self);
}

int add_pi_symbol_table_d(void* _self){
    return add_pi_symbol_table<double>(_self);
}

int add_pi_symbol_table_cf(void* _self){
    return add_pi_symbol_table<std::complex<float>>(_self);
}

int add_pi_symbol_table_cd(void* _self){
    return add_pi_symbol_table<std::complex<double>>(_self);
}

template<typename T>
int add_e_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->add_e();
}

int add_e_symbol_table_f(void* _self){
    return add_e_symbol_table<float>(_self);
}

int add_e_symbol_table_d(void* _self){
    return add_e_symbol_table<double>(_self);
}

int add_e_symbol_table_cf(void* _self){
    return add_e_symbol_table<std::complex<float>>(_self);
}

int add_e_symbol_table_cd(void* _self){
    return add_e_symbol_table<std::complex<double>>(_self);
}

template<typename T>
int add_i_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->add_i();
}

int add_i_symbol_table_f(void* _self){
    return add_i_symbol_table<float>(_self);
}

int add_i_symbol_table_d(void* _self){
    return add_i_symbol_table<double>(_self);
}

int add_i_symbol_table_cf(void* _self){
    return add_i_symbol_table<std::complex<float>>(_self);
}

int add_i_symbol_table_cd(void* _self){
    return add_i_symbol_table<std::complex<double>>(_self);
}

template<typename T>
int add_epsilon_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->add_i();
}

int add_epsilon_symbol_table_f(void* _self){
    return add_epsilon_symbol_table<float>(_self);
}

int add_epsilon_symbol_table_d(void* _self){
    return add_epsilon_symbol_table<double>(_self);
}

int add_epsilon_symbol_table_cf(void* _self){
    return add_epsilon_symbol_table<std::complex<float>>(_self);
}

int add_epsilon_symbol_table_cd(void* _self){
    return add_epsilon_symbol_table<std::complex<double>>(_self);
}

template<typename T>
int add_infinity_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->add_i();
}

int add_infinity_symbol_table_f(void* _self){
    return add_infinity_symbol_table<float>(_self);
}

int add_infinity_symbol_table_d(void* _self){
    return add_infinity_symbol_table<double>(_self);
}

int add_infinity_symbol_table_cf(void* _self){
    return add_infinity_symbol_table<std::complex<float>>(_self);
}

int add_infinity_symbol_table_cd(void* _self){
    return add_infinity_symbol_table<std::complex<double>>(_self);
}

template<typename T>
int symbol_exists_symbol_table(void* _self, const char* _name, int check_reserved_symb){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->symbol_exists(_name, check_reserved_symb);
}

int symbol_exists_symbol_table_f(void* _self, const char* _name, int check_reserved_symb){
    return symbol_exists_symbol_table<float>(_self, _name, check_reserved_symb);
}

int symbol_exists_symbol_table_d(void* _self, const char* _name, int check_reserved_symb){
    return symbol_exists_symbol_table<double>(_self, _name, check_reserved_symb);
}

int symbol_exists_symbol_table_cf(void* _self, const char* _name, int check_reserved_symb){
    return symbol_exists_symbol_table<std::complex<float>>(_self, _name, check_reserved_symb);
}

int symbol_exists_symbol_table_cd(void* _self, const char* _name, int check_reserved_symb){
    return symbol_exists_symbol_table<std::complex<double>>(_self, _name, check_reserved_symb);
}

template<typename T>
int is_variable_symbol_table(void* _self, const char* _name){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->is_variable(_name);
}

int is_variable_symbol_table_f(void* _self, const char* _name){
    return is_variable_symbol_table<float>(_self, _name);
}

int is_variable_symbol_table_d(void* _self, const char* _name){
    return is_variable_symbol_table<double>(_self, _name);
}

int is_variable_symbol_table_cf(void* _self, const char* _name){
    return is_variable_symbol_table<std::complex<float>>(_self, _name);
}

int is_variable_symbol_table_cd(void* _self, const char* _name){
    return is_variable_symbol_table<std::complex<double>>(_self, _name);
}

template<typename T>
int is_vector_symbol_table(void* _self, const char* _name){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->is_vector(_name);
}

int is_vector_symbol_table_f(void* _self, const char* _name){
    return is_vector_symbol_table<float               >(_self, _name);
}

int is_vector_symbol_table_d(void* _self, const char* _name){
    return is_vector_symbol_table<double              >(_self, _name);
}

int is_vector_symbol_table_cf(void* _self, const char* _name){
    return is_vector_symbol_table<std::complex<float> >(_self, _name);
}

int is_vector_symbol_table_cd(void* _self, const char* _name){
    return is_vector_symbol_table<std::complex<double>>(_self, _name);
}

template<typename T>
int valid_symbol_table(void* _self){
    return static_cast<Essa::Math::symbol_table<T>*>(_self)->valid();
}

int valid_symbol_table_f(void* _self){
    return valid_symbol_table<float>(_self);
}

int valid_symbol_table_d(void* _self){
    return valid_symbol_table<double>(_self);
}

int valid_symbol_table_cf(void* _self){
    return valid_symbol_table<std::complex<float>>(_self);
}

int valid_symbol_table_cd(void* _self){
    return valid_symbol_table<std::complex<double>>(_self);
}
