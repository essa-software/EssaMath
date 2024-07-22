#include "test_utils.hpp"
#include "essamath.h"
#include "expression.h"
#include "math_utils.h"
#include <complex>
#include <functional>
#include <iostream>
#include <vector>

void traverse_expr(std::vector<TestOptionsReal> const& _arr, size_t _index, std::vector<double>& _vars, std::function<void()> const& _foo) {
    if (_index == _arr.size()) {
        _foo();
        return;
    }

    // Recursive case: create the current loop
    for (_vars[_index] = _arr[_index]._lbound; _vars[_index] < _arr[_index]._rbound; _vars[_index] += _arr[_index]._step) {
        traverse_expr(_arr, _index + 1, _vars, _foo);
    }
}

bool test_real(std::string const& _exprstring, std::function<double(std::vector<double> const&)> const& _exprtest, std::vector<TestOptionsReal> const& _options, bool _verbose){
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
        std::vector<double*> varsptr;
        varsptr.resize(_options.size(), nullptr);
        std::vector<double> vars;
        vars.resize(_options.size(), 0);

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
            double lhs = em_calculateexpr(e);
            double rhs = _exprtest(vars);
            if(_verbose){
                for(size_t i = 0; i < _options.size(); i++){
                    std::cout << vars[i] << "\t\t";
                }
                std::cout << lhs << "\t\t" << rhs << "\n";
                std::cout << "\n";
            }

            if(!std::isnan(lhs) || !std::isnan(rhs)) {
                result &= EM_NEAREQUAL(lhs, rhs, 1e-3);
            }
        });

        // em_relexpr(e);
        // em_rellist(expr);

        return result;
    }
    return false;
}

void traverse_complex_expr(std::vector<TestOptionsComplex> const& _arr, size_t _index, std::vector<std::complex<double>>& _vars, std::function<void()> const& _foo) {
    if (_index == _arr.size()) {
        _foo();
        return;
    }

    // Recursive case: create the current loop
    for (size_t i = 0; i < _arr[_index]._count; i++, _vars[_index] += _arr[_index]._step) {
        traverse_complex_expr(_arr, _index + 1, _vars, _foo);
    }
}

bool test_complex(std::string const& _exprstring, std::function<std::complex<double>(std::vector<std::complex<double>> const&)> const& _exprtest, std::vector<TestOptionsComplex> const& _options, bool _verbose){
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
        std::vector<_Complex double*> varsptr;
        varsptr.resize(_options.size(), nullptr);
        std::vector<std::complex<double>> vars;
        vars.resize(_options.size(), 0);

        for(size_t i = 0; i < _options.size(); i++){
            varlist[i] = _options[i]._varname.c_str();
            varsptr[i] = reinterpret_cast<_Complex double*>(&vars[i]);
        }

        auto e = em_createcomplexexpression(expr, _options.size(), varlist.data(), varsptr.data());
        bool result = true;

        if(_verbose){
            for(size_t i = 0; i < _options.size(); i++){
                std::cout << _options[i]._varname << "\t\t";
            }
            std::cout << "Maxima:\t\tMath:\n";
        }
        
        traverse_complex_expr(_options, 0, vars, [&](){
            std::complex<double> lhs = em_calculatecomplexexpr(e);
            std::complex<double> rhs = _exprtest(vars);
            if(_verbose){
                for(size_t i = 0; i < _options.size(); i++){
                    std::cout << vars[i] << "\t\t";
                }
                std::cout << lhs << "\t\t" << rhs << "\n";
                std::cout << "\n";
            }

            if(!std::isnan(lhs.real()) || !std::isnan(rhs.real())) {
                result &= EM_NEAREQUAL(lhs.real(), rhs.real(), 1e-3) && EM_NEAREQUAL(lhs.imag(), rhs.imag(), 1e-3);
            }
        });

        // em_relcomplexexpr(e);
        // em_rellist(expr);

        return result;
    }
    return false;
}
