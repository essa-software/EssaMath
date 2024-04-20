#include "Expression.h"
#include <EssaMath/EssaMath.hpp>
#include <EssaMath/SymbolTable.hpp>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <unistd.h>

template<typename T>
void* create_expression(){
    return new Essa::Math::expression<T>();
}

void* create_expression_f(){
    return create_expression<float>();
}

void* create_expression_d(){
    return create_expression<double>();
}

void* create_expression_cf(){
    return create_expression<std::complex<float>>();
}

void* create_expression_cd(){
    return create_expression<std::complex<double>>();
}

template<typename T>
void* clone_expression(void* _other){
    return new Essa::Math::expression<T>(*static_cast<Essa::Math::expression<T>*>(_other));
}

void* clone_expression_f(void* _other){
    return clone_expression<float>(_other);
}
void* clone_expression_d(void* _other){
    return clone_expression<double>(_other);
}
void* clone_expression_cf(void* _other){
    return clone_expression<std::complex<float>>(_other);
}
void* clone_expression_cd(void* _other){
    return clone_expression<std::complex<double>>(_other);
}

template<typename T>
void destroy_expression(void* _ptr){
    delete static_cast<Essa::Math::expression<T>*>(_ptr);
}

void destroy_expression_f(void* _ptr){
    destroy_expression<float>(_ptr);
}

void destroy_expression_d(void* _ptr){
    destroy_expression<double>(_ptr);
}

void destroy_expression_cf(void* _ptr){
    destroy_expression<std::complex<float>>(_ptr);
}

void destroy_expression_cd(void* _ptr){
    destroy_expression<std::complex<double>>(_ptr);
}

template<typename T>
void assign_expression(void* _self, void* _other){
    *static_cast<Essa::Math::expression<T>*>(_self) = *static_cast<Essa::Math::expression<T>*>(_other);
}

void assign_expression_f(void* _self, void* _other){
    assign_expression<float>(_self, _other);
}

void assign_expression_d(void* _self, void* _other){
    assign_expression<double>(_self, _other);
}

void assign_expression_cf(void* _self, void* _other){
    assign_expression<std::complex<float>>(_self, _other);
}

void assign_expression_cd(void* _self, void* _other){
    assign_expression<std::complex<double>>(_self, _other);
}

template<typename T>
int compare_expression(void* _self, void* _other){
    return *static_cast<Essa::Math::expression<T>*>(_self) == *static_cast<Essa::Math::expression<T>*>(_other);
}

int compare_expression_f(void* _self, void* _other){
    return compare_expression<float>(_self, _other);
}

int compare_expression_d(void* _self, void* _other){
    return compare_expression<double>(_self, _other);
}

int compare_expression_cf(void* _self, void* _other){
    return compare_expression<std::complex<float>>(_self, _other);
}

int compare_expression_cd(void* _self, void* _other){
    return compare_expression<std::complex<double>>(_self, _other);
}

template<typename T>
int negate_expression(void* _self){
    return !*static_cast<Essa::Math::expression<T>*>(_self);
}

int negate_expression_f(void* _self){
    return negate_expression<float>(_self);
}

int negate_expression_d(void* _self){
    return negate_expression<double>(_self);
}

int negate_expression_cf(void* _self){
    return negate_expression<std::complex<float>>(_self);
}

int negate_expression_cd(void* _self){
    return negate_expression<std::complex<double>>(_self);
}

template<typename T>
void* value_expression(void* _self){
    return new T(static_cast<Essa::Math::expression<T>*>(_self)->value());
}

void* value_expression_f(void* _self){
    return value_expression<float>(_self);
}

void* value_expression_d(void* _self){
    return value_expression<double>(_self);
}

void* value_expression_cf(void* _self){
    return value_expression<std::complex<float>>(_self);
}

void* value_expression_cd(void* _self){
    return value_expression<std::complex<double>>(_self);
}

void free_value_expression_f(void* _ptr){
    delete static_cast<float*>(_ptr);
}

void free_value_expression_d(void* _ptr){
    delete static_cast<double*>(_ptr);
}

void free_value_expression_cf(void* _ptr){
    delete static_cast<std::complex<float>*>(_ptr);
}

void free_value_expression_cd(void* _ptr){
    delete static_cast<std::complex<double>*>(_ptr);
}

template<typename T>
int is_true_expression(void* _self){
    return bool(static_cast<Essa::Math::expression<T>*>(_self));
}

int is_true_expression_f(void* _self){
    return is_true_expression<float               >(_self);
}

int is_true_expression_d(void* _self){
    return is_true_expression<double              >(_self);
}

int is_true_expression_cf(void* _self){
    return is_true_expression<std::complex<float> >(_self);
}

int is_true_expression_cd(void* _self){
    return is_true_expression<std::complex<double>>(_self);
}

template<typename T>
void register_symbol_table_expression(void* _self, void* _table){
    static_cast<Essa::Math::expression<T>*>(_self)->register_symbol_table(*static_cast<Essa::Math::symbol_table<T>*>(_table));
}

void register_symbol_table_expression_f(void* _self, void* _table){
    register_symbol_table_expression<float>(_self, _table);
}

void register_symbol_table_expression_d(void* _self, void* _table){
    register_symbol_table_expression<double>(_self, _table);
}

void register_symbol_table_expression_cf(void* _self, void* _table){
    register_symbol_table_expression<std::complex<float>>(_self, _table);
}

void register_symbol_table_expression_cd(void* _self, void* _table){
    register_symbol_table_expression<std::complex<double>>(_self, _table);
}

template<typename T>
void* get_symbol_table_expression(void* _self, int _index){
    return &static_cast<Essa::Math::expression<T>*>(_self)->get_symbol_table(_index);
}

void* get_symbol_table_expression_f(void* _self, int _index){
    return get_symbol_table_expression<float>(_self, _index);
}

void* get_symbol_table_expression_d(void* _self, int _index){
    return get_symbol_table_expression<double>(_self, _index);
}

void* get_symbol_table_expression_cf(void* _self, int _index){
    return get_symbol_table_expression<std::complex<float>>(_self, _index);
}

void* get_symbol_table_expression_cd(void* _self, int _index){
    return get_symbol_table_expression<std::complex<double>>(_self, _index);
}

template<typename T>
const char* to_string_expression(void* _self){
    std::string _copy = static_cast<Essa::Math::expression<T>*>(_self)->to_string();
    if(_copy.empty())
        return "";
    char* _buf = new char[_copy.size()];
    strcpy(_buf, _copy.c_str());
    return _buf;
}

const char* to_string_expression_f(void* _self){
    return to_string_expression<float>(_self);
}

const char* to_string_expression_d(void* _self){
    return to_string_expression<double>(_self);
}

const char* to_string_expression_cf(void* _self){
    return to_string_expression<std::complex<float>>(_self);
}

const char* to_string_expression_cd(void* _self){
    return to_string_expression<std::complex<double>>(_self);
}

int strlen_api(const char* _str){
    return strlen(_str);
}

void free_string_api(const char* _str){
    delete[] _str;
}
