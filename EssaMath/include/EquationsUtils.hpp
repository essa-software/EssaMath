#pragma once
#include "Expression.hpp"

namespace Essa::Math{

template<typename T>
expression<T> algsys(std::vector<expression<T>>& _expr, std::vector<std::string> _x);

template<typename T>
expression<T> allroots(expression<T>& _expr);

template<typename T>
expression<T> bfallroots(expression<T>& _expr);

template<typename T>
expression<T> dimension(expression<T>& _expr);

template<typename T>
expression<T> dimension(std::vector<expression<T>>& _expr);

template<typename T>
expression<T> lhs(expression<T>& _expr);

template<typename T>
expression<T> linsolve(std::vector<expression<T>>& _expr, std::vector<std::string> _x);

template<typename T>
expression<T> nroots(expression<T>& _p, expression<T>& _low, expression<T>& _high);

template<typename T>
expression<T> nthroot(expression<T>& _p, int _n);

template<typename T>
expression<T> realroots(expression<T>& _expr, double _bound);

template<typename T>
expression<T> realroots(expression<T>& _expr);

template<typename T>
expression<T> rhs(expression<T>& _expr);

template<typename T>
expression<T> rootscontract(expression<T>& _expr);

template<typename T>
expression<T> solve(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> solve(expression<T>& _expr);

template<typename T>
expression<T> solve(std::vector<expression<T>>& _expr, std::vector<std::string> _x);

template<typename T>
expression<T> bc2(expression<T>& _solution, expression<T>& _xval1, expression<T>& _yval1, expression<T>& _xval2, expression<T>& _yval2);

template<typename T>
expression<T> desolve(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> desolve(std::vector<expression<T>>& _expr, std::vector<std::string> _x);

template<typename T>
expression<T> ic1(expression<T>& _solution, expression<T>& _xval, expression<T>& _yval);

template<typename T>
expression<T> ic2(expression<T>& _solution, expression<T>& _xval, expression<T>& _yval, expression<T>& _zval);

template<typename T>
expression<T> ode2(expression<T>& _eqn, std::string _dvar, std::string _ivar);

}
