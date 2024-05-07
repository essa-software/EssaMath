#pragma once
#include "Expression.hpp"
#include <string>
#include <vector>

namespace Essa::Math{

template<typename T>
expression<T> bern(int _n);

template<typename T>
expression<T> bernpoly(std::string _x, int _n);

template<typename T>
expression<T> bfzeta(expression<T>& _s, int _n);

template<typename T>
expression<T> bfhzeta(expression<T>& _s, expression<T>& _h, int _n);

template<typename T>
expression<T> binomial(expression<T>& _x, int _y);

template<typename T>
expression<T> burn(int _n);

template<typename T>
expression<T> cf(expression<T>& _expr);

template<typename T>
expression<T> cfdisrep(expression<T>& _expr);

template<typename T>
expression<T> cfexpand(expression<T>& _x);

template<typename T>
expression<T> cfexpand(expression<T>& _x);

template<typename T>
expression<T> divsum(int _n, int _k);

template<typename T>
expression<T> divsum(int _n);

template<typename T>
expression<T> euler(int _n);

template<typename T>
expression<T> factorial(int _x);

template<typename T>
expression<T> fib(int _n);

template<typename T>
expression<T> fibtophi(expression<T>& _expr);

template<typename T>
expression<T> ifactors(int _n);

template<typename T>
expression<T> jacobi(int _p, int _q);

template<typename T>
expression<T> lcm(std::vector<expression<T>>& _expr);

template<typename T>
expression<T> minfactorial(expression<T>& _expr);

template<typename T>
expression<T> next_prime(int _n);

template<typename T>
expression<T> partfrac(expression<T>& _expr, std::string _var);

template<typename T>
bool primep(int _n);

template<typename T>
expression<T> prev_prime(int _n);

template<typename T>
expression<T> qunit(expression<T>& _n);

template<typename T>
expression<T> zeta(expression<T>& _n);

}
