#include "OperatorUtils.h"
#include <EssaMath/Expression.hpp>
#include <EssaMath/OperatorUtils.hpp>
#include <vector>

template<typename T>
void* abs_func(void* _expr){
    return new Essa::Math::expression<T>(Essa::Math::abs(*static_cast<Essa::Math::expression<T>*>(_expr)));
}

void* abs_f(void* _expr){
    return abs_func<float>(_expr);
}
void* abs_d(void* _expr){
    return abs_func<double>(_expr);
}
void* abs_cf(void* _expr){
    return abs_func<std::complex<float>>(_expr);
}
void* abs_cd(void* _expr){
    return abs_func<std::complex<double>>(_expr);
}

template<typename T>
void* cabs_func(void* _expr){
    return new Essa::Math::expression<T>(Essa::Math::abs(*static_cast<Essa::Math::expression<T>*>(_expr)));
}
    
void* cabs_f(void* _expr){
    return cabs_func<float>(_expr);
}

void* cabs_d(void* _expr){
    return cabs_func<double>(_expr);
}

void* cabs_cf(void* _expr){
    return cabs_func<std::complex<float>>(_expr);
}

void* cabs_cd(void* _expr){
    return cabs_func<std::complex<double>>(_expr);
}

template<typename T>
const char* compare(void* _lhs, void* _rhs){
    auto _copy = Essa::Math::compare(*static_cast<Essa::Math::expression<T>*>(_lhs), *static_cast<Essa::Math::expression<T>*>(_rhs));
    if(_copy.empty())
        return "";
    char* _buf = new char[_copy.size()];
    strcpy(_buf, _copy.c_str());
    return _buf;
}
    
const char* compare_f(void* _lhs, void* _rhs){
    return compare<float>(_lhs, _rhs);
}

const char* compare_d(void* _lhs, void* _rhs){
    return compare<double>(_lhs, _rhs);
}

const char* compare_cf(void* _lhs, void* _rhs){
    return compare<std::complex<float>>(_lhs, _rhs);
}

const char* compare_cd(void* _lhs, void* _rhs){
    return compare<std::complex<double>>(_lhs, _rhs);
}
    
template<typename T>
void* polymod_func(void* _expr, int _val){
    return new Essa::Math::expression<T>(Essa::Math::polymod(*static_cast<Essa::Math::expression<T>*>(_expr), _val));
}

void* polymod_f(void* _expr, int _val){
    return polymod_func<float>(_expr, _val);
}
void* polymod_d(void* _expr, int _val){
    return polymod_func<double>(_expr, _val);
}
void* polymod_cf(void* _expr, int _val){
    return polymod_func<std::complex<float>>(_expr, _val);
}
void* polymod_cd(void* _expr, int _val){
    return polymod_func<std::complex<double>>(_expr, _val);
}

template<typename T>
void* psubst_func(void** _list, int _len, void* _expr){
    Essa::Math::expression<T>** _exprlist = (Essa::Math::expression<T>**)_list;
    std::vector<Essa::Math::expression<T>> _vec;
    for(size_t i = 0; i < _len; i++){
        _vec.push_back(*_exprlist[i]);
    }

    return new Essa::Math::expression<T>(Essa::Math::psubst(_vec, *static_cast<Essa::Math::expression<T>*>(_expr)));
}
    
void* psubst_f(void** _list, int _len, void* _expr){
    return psubst_func<float>(_list, _len, _expr);
}

void* psubst_d(void** _list, int _len, void* _expr){
    return psubst_func<double>(_list, _len, _expr);
}

void* psubst_cf(void** _list, int _len, void* _expr){
    return psubst_func<std::complex<float>>(_list, _len, _expr);
}

void* psubst_cd(void** _list, int _len, void* _expr){
    return psubst_func<std::complex<double>>(_list, _len, _expr);
}

template<typename T>
void* rationalize_func(void* _expr){
    return new Essa::Math::expression<T>(Essa::Math::rationalize(*static_cast<Essa::Math::expression<T>*>(_expr)));
}

void* rationalize_f(void* _expr){
    return rationalize_func<float>(_expr);
}

void* rationalize_d(void* _expr){
    return rationalize_func<double>(_expr);
}

void* rationalize_cf(void* _expr){
    return rationalize_func<std::complex<float>>(_expr);
}

void* rationalize_cd(void* _expr){
    return rationalize_func<std::complex<double>>(_expr);
}
    
template<typename T>
const char* sign_func(void* _expr){
    auto _copy = Essa::Math::sign(*static_cast<Essa::Math::expression<T>*>(_expr));
    if(_copy.empty())
        return "";
    char* _buf = new char[_copy.size()];
    strcpy(_buf, _copy.c_str());
    return _buf;
}

const char* sign_f(void* _expr){
    return sign_func<float>(_expr);
}

const char* sign_d(void* _expr){
    return sign_func<double>(_expr);
}

const char* sign_cf(void* _expr){
    return sign_func<std::complex<float>>(_expr);
}

const char* sign_cd(void* _expr){
    return sign_func<std::complex<double>>(_expr);
}

template<typename T>
void* subst_func(void** _list, int _len, void* _expr){
    Essa::Math::expression<T>** _exprlist = (Essa::Math::expression<T>**)_list;
    std::vector<Essa::Math::expression<T>> _vec;
    for(size_t i = 0; i < _len; i++){
        _vec.push_back(*_exprlist[i]);
    }

    return new Essa::Math::expression<T>(Essa::Math::subst(_vec, *static_cast<Essa::Math::expression<T>*>(_expr)));
}

void* subst_f(void** _list, int _len, void* _expr){
    return subst_func<float>(_list, _len, _expr);
}

void* subst_d(void** _list, int _len, void* _expr){
    return subst_func<double>(_list, _len, _expr);
}

void* subst_cf(void** _list, int _len, void* _expr){
    return subst_func<std::complex<float>>(_list, _len, _expr);
}

void* subst_cd(void** _list, int _len, void* _expr){
    return subst_func<std::complex<double>>(_list, _len, _expr);
}

template<typename T>
void** sort_func(void** _list, int _len){
    Essa::Math::expression<T>** _exprlist = (Essa::Math::expression<T>**)_list;
    std::vector<Essa::Math::expression<T>> _vec;
    for(size_t i = 0; i < _len; i++){
        _vec.push_back(*_exprlist[i]);
    }

    auto _result = Essa::Math::sort(_vec);
    Essa::Math::expression<T>** _resultptr;
    _resultptr = new Essa::Math::expression<T>*[_result.size()];
    for(size_t i = 0; i < _result.size(); i++){
        _resultptr[i] = new Essa::Math::expression<T>(_result[i]);
    }

    return (void**)_resultptr;
}

void** sort_f(void** _list, int _len){
    return sort_func<float               >(_list, _len);
}

void** sort_d(void** _list, int _len){
    return sort_func<double              >(_list, _len);
}

void** sort_cf(void** _list, int _len){
    return sort_func<std::complex<float> >(_list, _len);
}

void** sort_cd(void** _list, int _len){
    return sort_func<std::complex<double>>(_list, _len);
}

template<typename T>
void* sqrt_func(void* _expr){
    return new Essa::Math::expression<T>(Essa::Math::sqrt(*static_cast<Essa::Math::expression<T>*>(_expr)));
}

void* sqrt_f(void* _expr){
    return sqrt_func<float>(_expr);
}

void* sqrt_d(void* _expr){
    return sqrt_func<double>(_expr);
}

void* sqrt_cf(void* _expr){
    return sqrt_func<std::complex<float>>(_expr);
}

void* sqrt_cd(void* _expr){
    return sqrt_func<std::complex<double>>(_expr);
}

template<typename T>
void* xthru_func(void* _expr){
    return new Essa::Math::expression<T>(Essa::Math::xthru(*static_cast<Essa::Math::expression<T>*>(_expr)));
}

void* xthru_f(void* _expr){
    return xthru_func<float>(_expr);
}

void* xthru_d(void* _expr){
    return xthru_func<double>(_expr);
}

void* xthru_cf(void* _expr){
    return xthru_func<std::complex<float>>(_expr);
}

void* xthru_cd(void* _expr){
    return xthru_func<std::complex<double>>(_expr);
}
    
template<typename T>
const char* zeroequiv_func(void* _expr, const char* _var){
    auto _copy = Essa::Math::zeroequiv(*static_cast<Essa::Math::expression<T>*>(_expr), _var);
    if(_copy.empty())
        return "";
    char* _buf = new char[_copy.size()];
    strcpy(_buf, _copy.c_str());
    return _buf;
}

const char* zeroequiv_f(void* _expr, const char* _var){
    return zeroequiv_func<float>(_expr, _var);
}

const char* zeroequiv_d(void* _expr, const char* _var){
    return zeroequiv_func<double>(_expr, _var);
}

const char* zeroequiv_cf(void* _expr, const char* _var){
    return zeroequiv_func<std::complex<float>>(_expr, _var);
}

const char* zeroequiv_cd(void* _expr, const char* _var){
    return zeroequiv_func<std::complex<double>>(_expr, _var);
}

// float               
// double              
// std::complex<float> 
// std::complex<double>