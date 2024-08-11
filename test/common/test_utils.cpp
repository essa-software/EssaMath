#include "test_utils.hpp"
#include "essamath.h"
#include "expression.h"
#include "math_utils.h"
#include <functional>
#include <iostream>
#include <vector>

// NOLINTBEGIN(misc-no-recursion)
void traverse_expr(std::vector<TestOptions> const& _arr, size_t _index, std::vector<em_val>& _vars, std::function<void()> const& _foo) {
    if (_index == _arr.size()) {
        _foo();
        return;
    }

    // Recursive case: create the current loop
    _vars[_index] = _arr[_index]._value;
    for (size_t i = 0; i < _arr[_index]._count; i++, em_numeric_add(&_vars[_index], &_vars[_index], &_arr[_index]._step)) {
        traverse_expr(_arr, _index + 1, _vars, _foo);
    }
}
// NOLINTEND(misc-no-recursion)

bool test_loop(std::string const& _exprstring, std::function<em_val(std::vector<em_val> const&)> const& _exprtest, std::vector<TestOptions> const& _options, bool _verbose){
    if(em_eval(_exprstring.c_str()) == EM_RTNORM){
        auto expr = em_getexpr(em_getlastoutput());
        if(_verbose){
            printf("%C\n%R\n", expr, expr);
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
            for(const auto & _option : _options){
                std::cout << _option._varname << "\t\t";
            }
            std::cout << "Maxima:\t\tMath:\n";
        }
        
        traverse_expr(_options, 0, vars, [&](){
            auto lhs = em_calculateexpr(e);
            auto rhs = _exprtest(vars);
            if(_verbose){
                for(size_t i = 0; i < _options.size(); i++){
                    printf("%V\t\t", &vars[i]);
                }
                printf("%V\t\t%V\t\t", &lhs, &rhs);
            }

            int res = 0;
            em_numeric_equal(&res, &lhs, &rhs);
            
            if(res){
                printf("SUCCESS!\n");
            }else{
                printf("FAILURE!\n");
            }

            result &= res;
        });

        em_relexpr(e);
        em_rellist(expr);

        return result;
    }
    return false;
}
