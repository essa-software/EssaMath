#include <cctype>
#include <codecvt>
#include <cstddef>
#include <iostream>
#include <locale>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <stdio.h>
#include <ecl/ecl.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include "include/LispApi.hpp"
#include <deque>
#include <vector>

std::string to_lower(std::string const& _str){
    std::string _result;
    for(auto& c : _str){
        _result += std::tolower(c);
    }

    return _result;
}

extern "C"{
    void init_lib_MAXIMA(cl_object);
} 

namespace Essa::Math {

class LispObject{
public: 
    struct Token{
        enum class Type{
            LEFT, RIGHT, STRING
        };

        Type _type;
        std::string _str;
    };
    LispObject(std::deque<Token>& _tokens){}

    static std::shared_ptr<LispObject> Parse(std::istream& _ss);

    virtual std::string to_string() const = 0;
};

class LispList : public LispObject{
public:
    LispList(std::deque<Token>& _tokens);
    std::string to_string() const override;
private:
    std::string _name;
    std::vector<std::shared_ptr<LispObject>> _objectVec;
};

class LispValue : public LispObject{
public:
    LispValue(std::deque<Token>& _tokens);
    std::string to_string() const override;
private:
    std::string _value;
};

std::shared_ptr<LispObject> LispObject::Parse(std::istream& _ss){
    std::deque<Token> _tokens;
    char c;
    std::string _res;
    while(!_ss.eof()){
        _ss.read(&c, 1);

        if(!_res.empty() && (std::isspace(c) || c == '(' || c == ')')){
            _tokens.push_back(Token{._type = Token::Type::STRING, ._str = _res});
            _res = "";
        }

        if(c == '('){
            _tokens.push_back(Token{._type = Token::Type::LEFT, ._str = "("});
        }else if(c == ')'){
            _tokens.push_back(Token{._type = Token::Type::RIGHT, ._str = ")"});
        }else if(!std::isspace(c)){
            _res += c;
        }
    }

    if(!_res.empty()){
        if(_tokens.size() == 0)
            _res = _res.substr(0, _res.size() - 1);
        _tokens.push_back(Token{._type = Token::Type::STRING, ._str = _res});
    }

    if(_tokens.front()._type == Token::Type::LEFT){
        return std::make_shared<LispList>(_tokens);
    }else{
        return std::make_shared<LispValue>(_tokens);
    }
}

LispList::LispList(std::deque<Token>& _tokens) : LispObject(_tokens){
    _tokens.pop_front();
    _tokens.pop_front();

    _name += _tokens.front()._str;
    _tokens.pop_front();
    _tokens.pop_front();

    while(_tokens.front()._type != Token::Type::RIGHT){
        if(_tokens.front()._type == Token::Type::LEFT){
            _objectVec.push_back(std::make_shared<LispList>(_tokens));
        }else{
            _objectVec.push_back(std::make_shared<LispValue>(_tokens));
        }
    }

    _tokens.pop_front();
}

std::string LispList::to_string() const{
    std::string _op;
    bool _braces = false;
    bool _initial = false;

    if(_name == "MPLUS"){
        _op = "+";
        _braces = true;
        _initial = false;
    }else if(_name == "MMINUS"){
        _op = "-";
        _braces = true;
        _initial = true;
    }else if(_name == "MTIMES"){
        _op = "*";
        _braces = true;
        _initial = false;
    }else if(_name == "MQUOTIENT" || _name == "RAT"){
        _op = "/";
        _braces = true;
        _initial = false;
    }else if(_name == "MEXPT"){
        _op = "^";
        _braces = false;
        _initial = false;
    }else if(_name == "MLESSP"){
        _op = "<";
        _braces = false;
        _initial = false;
    }else if(_name == "MLEQP"){
        _op = "<=";
        _braces = false;
        _initial = false;
    }else if(_name == "MEQUAL"){
        _op = "=";
        _braces = false;
        _initial = false;
    }else if(_name == "MGEQP"){
        _op = ">=";
        _braces = false;
        _initial = false;
    }else if(_name == "MGREATERP"){
        _op = ">";
        _braces = false;
        _initial = false;
    }else if(_name == "MLIST"){
        std::string _result = "[";

        for(size_t i = 0; i < _objectVec.size(); i++){
            if(i != 0){
                _result += ",";
            }

            _result += _objectVec[i]->to_string();
        }

        _result += "]";
        return _result;
    }else if(_name.front() == '%' || _name.front() == 'M' || _name.front() == '$'){
        std::string _result = to_lower(_name.substr(1)) + "(";

        for(size_t i = 0; i < _objectVec.size(); i++){
            if(i != 0){
                _result += ",";
            }

            _result += _objectVec[i]->to_string();
        }

        _result += ")";
        return _result;
    }else{
        return "";
    }
    std::string _result;

    if(_braces) _result += "(";

    for(size_t i = 0; i < _objectVec.size(); i++){
        if(i == 0 && _initial || i != 0){
            _result += _op;
        }

        _result += _objectVec[i]->to_string();
    }

    if(_braces) _result += ")";

    return _result;
}

LispValue::LispValue(std::deque<Token>& _tokens) : LispObject(_tokens){
    _value = to_lower(_tokens.front()._str);
    if(_value.front() == '$'){
        _value = _value.substr(1);
    }
    _tokens.pop_front();
}

std::string LispValue::to_string() const{
    return _value;
}

std::string func1(const std::string& _func, const std::string& _arg1) {
    try{
        cl_object arg1 = c_string_to_object(_arg1.c_str());
        cl_object name = ecl_make_symbol(_func.c_str(), "MAXIMA");
        cl_object output = cl_funcall(2, name, arg1);

        static_assert(sizeof(ecl_character)==sizeof(wchar_t),"sizes must be the same");
            
        std::wstring _rawstr = reinterpret_cast<wchar_t*>(output->string.self);
        std::string _res;
        for(size_t i = 0; i < _rawstr.size(); i++){
            if(_rawstr[i] > 255)
                break;
            _res += (char)_rawstr[i];
        }

        auto _pos = _res.find(" SIMP");
        while(_pos != std::string::npos){
            _res = _res.substr(0, _pos) + _res.substr(_pos + 5);
            _pos = _res.find(" SIMP");
        }

        _pos = _res.find(" RATSIMP");
        while(_pos != std::string::npos){
            _res = _res.substr(0, _pos) + _res.substr(_pos + 8);
            _pos = _res.find(" RATSIMP");
        }

        return _res;
    }catch(...){
        return func1(_func, _arg1);
    }
}

std::string func2(const std::string& _func, const std::string& _arg1, const std::string& _arg2) {
    try{
        cl_object arg1 = c_string_to_object(_arg1.c_str());
        cl_object arg2 = c_string_to_object(_arg2.c_str());
        cl_object name = ecl_make_symbol(_func.c_str(), "MAXIMA");
        cl_object output = cl_funcall(3, name, arg1, arg2);

        static_assert(sizeof(ecl_character)==sizeof(wchar_t),"sizes must be the same");
            
        std::wstring _rawstr = reinterpret_cast<wchar_t*>(output->string.self);
        std::string _res;
        for(size_t i = 0; i < _rawstr.size(); i++){
            if(_rawstr[i] > 255)
                break;
            _res += (char)_rawstr[i];
        }

        auto _pos = _res.find(" SIMP");
        while(_pos != std::string::npos){
            _res = _res.substr(0, _pos) + _res.substr(_pos + 5);
            _pos = _res.find(" SIMP");
        }

        _pos = _res.find(" RATSIMP");
        while(_pos != std::string::npos){
            _res = _res.substr(0, _pos) + _res.substr(_pos + 8);
            _pos = _res.find(" RATSIMP");
        }

        return _res;
    }catch(...){
        return func2(_func, _arg1, _arg2);
    }
}

std::string evaluate(const std::string& _expr) {
    std::stringstream ss;
    ss << func1("api-eval", "\"" + _expr + "$\"");
    auto _obj = LispObject::Parse(ss);
    return _obj->to_string();
}

void load(const std::string& path){
    std::string exp = "\"" + path + "\"";
        
    cl_object arg1 = c_string_to_object(exp.c_str());
    cl_object name = ecl_make_symbol("api-load", "MAXIMA");
    cl_object output = cl_funcall(2, name, arg1);
}

void init_math(int argc, char **argv){
    cl_boot(argc, argv);
    ecl_init_module(NULL, init_lib_MAXIMA);

    cl_eval(c_string_to_object("(initialize-runtime-globals)"));
    // cl_eval(c_string_to_object("(print-directories)"));
    cl_eval(c_string_to_object("(setq $fpprec 10)"));
    cl_eval(c_string_to_object("(setq *debugger-hook* nil)"));
    cl_eval(c_string_to_object("(setq $dispflag nil)"));
}

void free_math(){
    cl_shutdown();
}

extern std::string evaluate(const std::string& _expr);
extern void load(const std::string& path);

}
