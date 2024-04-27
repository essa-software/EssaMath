#pragma once
#include "Expression.hpp"

namespace Essa::Math{

template<typename T>
expression<T> limit(expression<T>& _expr, std::string _x, expression<T>& _val, std::string _dir);

template<typename T>
expression<T> limit(expression<T>& _expr, std::string _x, expression<T>& _val);

template<typename T>
expression<T> limit(expression<T>& _expr);

template<typename T>
expression<T> tlimit(expression<T>& _expr, std::string _x, expression<T>& _val, std::string _dir);

template<typename T>
expression<T> tlimit(expression<T>& _expr, std::string _x, expression<T>& _val);

template<typename T>
expression<T> tlimit(expression<T>& _expr);

template<typename T>
expression<T> diff(expression<T>& _expr, std::string _x, int _n);

template<typename T>
expression<T> diff(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> diff(expression<T>& _expr);

template<typename T>
expression<T> dscalar(expression<T>& _expr);

template<typename T>
expression<T> express(expression<T>& _expr);

template<typename T>
expression<T> laplace(expression<T>& _expr, expression<T>& _t, expression<T>& _s);

template<typename T>
expression<T> changevar(expression<T>& _expr, expression<T>& _func, std::string _x,  std::string _y);

template<typename T>
expression<T> defint(expression<T>& _expr, std::string const& _x, expression<T>& _a, expression<T>& _b);

template<typename T>
expression<T> ilt(expression<T>& _expr, std::string const& _s, expression<T>& _t);

template<typename T>
expression<T> integrate(expression<T>& _expr, std::string const& _x);

template<typename T>
expression<T> integrate(expression<T>& _expr, std::string const& _x, expression<T>& _a, expression<T>& _b);

template<typename T>
expression<T> ldefint(expression<T>& _expr, std::string const& _x, expression<T>& _a, expression<T>& _b);

template<typename T>
expression<T> residue(expression<T>& _expr, std::string const& _z, expression<T>& _z_0);

template<typename T>
expression<T> risch(expression<T>& _expr, std::string const& _x);

template<typename T>
expression<T> tldefint(expression<T>& _expr, std::string const& _x, expression<T>& _a, expression<T>& _b);

}
