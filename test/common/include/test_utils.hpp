#pragma once

#include "expression.h"
#include "essamath.h"
#include <complex>
#include <functional>
#include <string>
#include <vector>

#define EPS 1e-6

struct TestOptionsReal{
    std::string _varname;
    double _lbound = 0.0, _rbound = 0.0, _step = 0.0;
};
bool test_real(std::string const& _exprstring, std::function<double(std::vector<double> const&)> const& _exprtest, std::vector<TestOptionsReal> const& _options, bool _verbose = false);

struct TestOptionsComplex{
    std::string _varname;
    std::complex<double> _value = 0.0, _step = 0.0;
    size_t _count = 0;
};

bool test_complex(std::string const& _exprstring, std::function<std::complex<double>(std::vector<std::complex<double>> const&)> const& _exprtest, std::vector<TestOptionsComplex> const& _options, bool _verbose = false);

template<typename... Args>
bool test_numeric_function(int (*_foo)(Args...), double _expected, Args... _args){
    if(_foo && _foo(_args...) == EM_RTNORM){
        auto expr = em_getexpr(em_getlastoutput());
        auto e = em_createexpression(expr, 0, nullptr, nullptr);
        return EM_NEAREQUAL(em_calculateexpr(e), _expected, EPS);
    }

    return false;
}
