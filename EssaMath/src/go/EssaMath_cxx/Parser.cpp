#include "Parser.h"
#include <EssaMath/EssaMath.hpp>
#include <EssaMath/Expression.hpp>
#include <EssaMath/SymbolTable.hpp>

template<typename T>
void* create_parser(){
    return new Essa::Math::parser<T>();
}

void* create_parser_f(){
    return create_parser<float>();
}

void* create_parser_d(){
    return create_parser<double>();
}

void* create_parser_cf(){
    return create_parser<std::complex<float>>();
}

void* create_parser_cd(){
    return create_parser<std::complex<double>>();
}

template<typename T>
void destroy_parser(void* _self){
    delete static_cast<Essa::Math::parser<T>*>(_self);
}

void destroy_parser_f(void* _self){
    destroy_parser<float>(_self);
}

void destroy_parser_d(void* _self){
    destroy_parser<double>(_self);
}

void destroy_parser_cf(void* _self){
    destroy_parser<std::complex<float>>(_self);
}

void destroy_parser_cd(void* _self){
    destroy_parser<std::complex<double>>(_self);
}

template<typename T>
int compile_parser(void* _self, const char* expression_string, void* expr){
    return static_cast<Essa::Math::parser<T>*>(_self)->compile(expression_string, *static_cast<Essa::Math::expression<T>*>(expr));
}

int compile_parser_f(void* _self, const char* expression_string, void* expr){
    return compile_parser<float>(_self, expression_string, expr);
}

int compile_parser_d(void* _self, const char* expression_string, void* expr){
    return compile_parser<double>(_self, expression_string, expr);
}

int compile_parser_cf(void* _self, const char* expression_string, void* expr){
    return compile_parser<std::complex<float>>(_self, expression_string, expr);
}

int compile_parser_cd(void* _self, const char* expression_string, void* expr){
    return compile_parser<std::complex<double>>(_self, expression_string, expr);
}

template<typename T>
void* compile_and_create_expression_parser(void* _self, const char* expression_string, void* symtab){
    return new Essa::Math::expression<T>(static_cast<Essa::Math::parser<T>*>(_self)->compile(expression_string, *static_cast<Essa::Math::symbol_table<T>*>(symtab)));
}

void* compile_and_create_expression_parser_f(void* _self, const char* expression_string, void* symtab){
    return compile_and_create_expression_parser<float>(_self, expression_string, symtab);
}
void* compile_and_create_expression_parser_d(void* _self, const char* expression_string, void* symtab){
    return compile_and_create_expression_parser<double>(_self, expression_string, symtab);
}
void* compile_and_create_expression_parser_cf(void* _self, const char* expression_string, void* symtab){
    return compile_and_create_expression_parser<std::complex<float>>(_self, expression_string, symtab);
}
void* compile_and_create_expression_parser_cd(void* _self, const char* expression_string, void* symtab){
    return compile_and_create_expression_parser<std::complex<double>>(_self, expression_string, symtab);
}

template<typename T>
const char* error_parser(void* _self){
    std::string _copy = static_cast<Essa::Math::parser<T>*>(_self)->error();
    if(_copy.empty())
        return "";
    char* _buf = new char[_copy.size()];
    strcpy(_buf, _copy.c_str());
    return _buf;
}

const char* error_parser_f(void* _self){
    return error_parser<float>(_self);
}

const char* error_parser_d(void* _self){
    return error_parser<double>(_self);
}

const char* error_parser_cf(void* _self){
    return error_parser<std::complex<float>>(_self);
}

const char* error_parser_cd(void* _self){
    return error_parser<std::complex<double>>(_self);
}

template<typename T>
int error_count_parser(void* _self){
    return static_cast<Essa::Math::parser<T>*>(_self)->error_count();
}

int error_count_parser_f(void* _self){
    return error_count_parser<float>(_self);
}

int error_count_parser_d(void* _self){
    return error_count_parser<double>(_self);
}

int error_count_parser_cf(void* _self){
    return error_count_parser<std::complex<float>>(_self);
}

int error_count_parser_cd(void* _self){
    return error_count_parser<std::complex<double>>(_self);
}
