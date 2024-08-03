#include "test_utils.hpp"
#include "essamath.h"
#include "expression.h"
#include "math_utils.h"
#include <complex.h>
#include <complex>
#include <functional>
#include <iostream>
#include <vector>

void traverse_expr(std::vector<TestOptionsReal> const& _arr, size_t _index, std::vector<em_val>& _vars, std::function<void()> const& _foo) {
    if (_index == _arr.size()) {
        _foo();
        return;
    }

    // Recursive case: create the current loop
    for (_vars[_index] = em_createreal(_arr[_index]._lbound); em_getdouble(_vars[_index]) < _arr[_index]._rbound; em_numeric_add(&_vars[_index], _vars[_index], em_createreal(_arr[_index]._step))) {
        traverse_expr(_arr, _index + 1, _vars, _foo);
    }
}

bool test_real(std::string const& _exprstring, std::function<double(std::vector<em_val> const&)> const& _exprtest, std::vector<TestOptionsReal> const& _options, bool _verbose){
    if(em_eval(_exprstring.c_str()) == EM_RTNORM){
        auto expr = em_getexpr(em_getlastoutput());
        char buf[256] = {0};
        em_tostring(expr, buf, 256);
        if(_verbose){
            em_printf(expr);
            std::cout << "\n" << buf << "\n";
        }

        std::vector<const char*> varlist;
        varlist.resize(_options.size(), nullptr);
        std::vector<em_val*> varsptr;
        varsptr.resize(_options.size(), nullptr);
        std::vector<em_val> vars;
        vars.resize(_options.size());

        for(size_t i = 0; i < _options.size(); i++){
            varlist[i] = _options[i]._varname.c_str();
            varsptr[i] = &vars[i];
        }

        auto e = em_createexpression(expr, _options.size(), varlist.data(), varsptr.data());
        bool result = true;

        if(_verbose){
            for(size_t i = 0; i < _options.size(); i++){
                std::cout << _options[i]._varname << "\t\t";
            }
            std::cout << "Maxima:\t\tMath:\n";
        }
        
        traverse_expr(_options, 0, vars, [&](){
            double lhs = em_getdouble(em_calculateexpr(e));
            double rhs = _exprtest(vars);
            if(_verbose){
                for(size_t i = 0; i < _options.size(); i++){
                    std::cout << em_getdouble(vars[i]) << "\t\t";
                }
                std::cout << lhs << "\t\t" << rhs << "\n";
                std::cout << "\n";
            }

            if(!std::isnan(lhs) || !std::isnan(rhs)) {
                result &= EM_NEAREQUAL(lhs, rhs, EPS);
            }
        });

        // em_relexpr(e);
        // em_rellist(expr);

        return result;
    }
    return false;
}

void traverse_complex_expr(std::vector<TestOptionsComplex> const& _arr, size_t _index, std::vector<em_val>& _vars, std::function<void()> const& _foo) {
    if (_index == _arr.size()) {
        _foo();
        return;
    }

    _Complex double step = _arr[_index]._step.real() + _arr[_index]._step.imag();

    // Recursive case: create the current loop
    for (size_t i = 0; i < _arr[_index]._count; i++, em_numeric_add(&_vars[_index], _vars[_index], em_createcomplex(em_complex(_arr[_index]._step.real(), _arr[_index]._step.imag())))) {
        traverse_complex_expr(_arr, _index + 1, _vars, _foo);
    }
}

bool test_complex(std::string const& _exprstring, std::function<std::complex<double>(std::vector<em_val> const&)> const& _exprtest, std::vector<TestOptionsComplex> const& _options, bool _verbose){
    if(em_eval(_exprstring.c_str()) == EM_RTNORM){
        auto expr = em_getexpr(em_getlastoutput());
        char buf[256] = {0};
        em_tostring(expr, buf, 256);
        if(_verbose){
            em_printf(expr);
            std::cout << "\n" << buf << "\n";
        }

        std::vector<const char*> varlist;
        varlist.resize(_options.size(), nullptr);
        std::vector<em_val*> varsptr;
        varsptr.resize(_options.size(), nullptr);
        std::vector<em_val> vars;
        vars.resize(_options.size());

        for(size_t i = 0; i < _options.size(); i++){
            varlist[i] = _options[i]._varname.c_str();
            varsptr[i] = &vars[i];
        }

        auto e = em_createexpression(expr, _options.size(), varlist.data(), varsptr.data());
        bool result = true;

        if(_verbose){
            for(size_t i = 0; i < _options.size(); i++){
                std::cout << _options[i]._varname << "\t\t";
            }
            std::cout << "Maxima:\t\tMath:\n";
        }
        
        traverse_complex_expr(_options, 0, vars, [&](){
            std::complex<double> lhs = em_getcomplex(em_calculateexpr(e));
            std::complex<double> rhs = _exprtest(vars);
            if(_verbose){
                for(size_t i = 0; i < _options.size(); i++){
                    std::cout << std::complex<double>(em_getcomplex(vars[i])) << "\t\t";
                }
                std::cout << lhs << "\t\t" << rhs << "\n";
                std::cout << "\n";
            }

            if(!std::isnan(lhs.real()) || !std::isnan(rhs.real())) {
                result &= EM_NEAREQUAL(lhs.real(), rhs.real(), EPS) && EM_NEAREQUAL(lhs.imag(), rhs.imag(), EPS);
            }
        });

        // em_relcomplexexpr(e);
        // em_rellist(expr);

        return result;
    }
    return false;
}
