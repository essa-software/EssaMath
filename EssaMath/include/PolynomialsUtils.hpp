#pragma once
#include "Expression.hpp"
#include <initializer_list>

namespace Essa::Math{

template<typename T>
expression<T> bezout(expression<T>& _p1, expression<T>& _p2, std::string _x);

template<typename T>
expression<T> bothcoef(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> coeff(expression<T>& _expr, std::string _x, int _n);

template<typename T>
expression<T> coeff(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> combine(expression<T>& _expr);

template<typename T>
expression<T> denom(expression<T>& _expr);

template<typename T>
expression<T> divide(std::vector<expression<T>>& _list);

template<typename T>
expression<T> eliminate(std::vector<expression<T>>& _list, std::vector<std::string> _args);

template<typename T>
expression<T> eliminate(std::vector<expression<T>>& _list, std::initializer_list<std::string> _args);

template<typename T>
expression<T> ezgcd(std::vector<expression<T>>& _list);

template<typename T>
expression<T> factcomb(expression<T>& _expr);

template<typename T>
expression<T> factor(expression<T>& _expr);

template<typename T>
expression<T> factor(expression<T>& _expr, expression<T>& _p);

template<typename T>
expression<T> factorout(expression<T>& _expr, std::vector<std::string> _args);

template<typename T>
expression<T> factorout(expression<T>& _expr, std::initializer_list<std::string> _args);

template<typename T>
expression<T> factorsum(expression<T>& _expr);

template<typename T>
expression<T> fasttimes(expression<T>& _p1, expression<T>& _p2);

template<typename T>
expression<T> fullratsimp(expression<T>& _expr);

template<typename T>
expression<T> fullratsubst(expression<T>& _a, expression<T>& _b, expression<T>& _c);

template<typename T>
expression<T> gcdex(expression<T>& _f, expression<T>& _g);

template<typename T>
expression<T> gcdex(expression<T>& _f, expression<T>& _g, std::string _x);

template<typename T>
expression<T> gcfactor(int _n);

template<typename T>
expression<T> gfactor(expression<T>& _expr);

template<typename T>
expression<T> gfactorsum(expression<T>& _expr);

template<typename T>
expression<T> hipow(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> lratsubst(std::vector<expression<T>>& _list, expression<T>& _expr);

template<typename T>
expression<T> num(expression<T>& _expr);

template<typename T>
expression<T> polydecomp(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> quotient(expression<T>& _p1, expression<T>& _p2);

template<typename T>
expression<T> quotient(expression<T>& _p1, expression<T>& _p2, std::vector<std::string> _args);

template<typename T>
expression<T> quotient(expression<T>& _p1, expression<T>& _p2, std::initializer_list<std::string> _args);

template<typename T>
expression<T> rat(expression<T>& _expr);

template<typename T>
expression<T> rat(expression<T>& _expr, std::vector<std::string> _args);

template<typename T>
expression<T> rat(expression<T>& _expr, std::initializer_list<std::string> _args);

template<typename T>
expression<T> ratcoef(expression<T>& _expr, std::string _x, int _n);

template<typename T>
expression<T> ratcoef(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> ratdenom(expression<T>& _expr);

template<typename T>
expression<T> ratdiff(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> ratdisrep(expression<T>& _expr);

template<typename T>
expression<T> ratexpand(expression<T>& _expr);

template<typename T>
expression<T> ratnumer(expression<T>& _expr);

template<typename T>
expression<T> ratnump(expression<T>& _expr);

template<typename T>
expression<T> ratp(expression<T>& _expr);

template<typename T>
expression<T> ratsimp(expression<T>& _expr);

template<typename T>
expression<T> ratsimp(expression<T>& _expr, std::vector<std::string> _args);

template<typename T>
expression<T> ratsimp(expression<T>& _expr, std::initializer_list<std::string> _args);

template<typename T>
expression<T> ratsubst(expression<T>& _a, expression<T>& _b, expression<T>& _c);

template<typename T>
expression<T> remainder(expression<T>& _p1, expression<T>& _p2);

template<typename T>
expression<T> remainder(expression<T>& _p1, expression<T>& _p2, std::vector<std::string> _args);

template<typename T>
expression<T> remainder(expression<T>& _p1, expression<T>& _p2, std::initializer_list<std::string> _args);

template<typename T>
expression<T> resultant(expression<T>& _p1, expression<T>& _p2, std::string _x);

template<typename T>
expression<T> sqfr(expression<T>& _expr);

template<typename T>
expression<T> totaldisrep(expression<T>& _expr);

template<typename T>
expression<T> fibtophi(int _n);

}
