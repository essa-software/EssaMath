#pragma once

#include "expression.h"
#include "essamath.h"
#include "math_utils.h"
#include <functional>
#include <math.h>
#include <string>
#include <vector>

struct TestOptions{
    std::string _varname;
    em_val _value, _step;
    size_t _count = 0;
};
bool test_loop(std::string const& _exprstring, std::function<em_val(std::vector<em_val> const&)> const& _exprtest, std::vector<TestOptions> const& _options, bool _verbose = false);

template<typename... Args>
bool test_numeric_function(int (*_foo)(Args...), em_val _expected, Args... _args){
    if(_foo && _foo(_args...) == EM_RTNORM){
        auto expr = em_getexpr(em_getlastoutput());
        auto e = em_createexpression(expr, 0, nullptr, nullptr);
        auto res = em_calculateexpr(e);

        int _result = 0;
        if(em_numeric_equal(&_result, &res, &_expected)){
            return _result;
        }
    }

    return false;
}
