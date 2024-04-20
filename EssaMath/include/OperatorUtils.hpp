#pragma once
#include "SymbolTable.hpp"
#include "Parser.hpp"
#include "Expression.hpp"

namespace Essa::Math{

template<typename T>
expression<T> abs(expression<T>& _expr);

template<typename T>
expression<T> cabs(expression<T>& _expr);

template<typename T>
std::string compare(expression<T>& _lhs, expression<T>& _rhs);

template<typename T>
expression<T> polymod(expression<T>& _expr, const int _val);

template<typename T>
expression<T> psubst(std::vector<expression<T>>& _list, expression<T>& _expr);

template<typename T>
expression<T> rationalize(expression<T>& _expr);

template<typename T>
std::string sign(expression<T>& _expr);

template<typename T>
expression<T> subst(std::vector<expression<T>>& _list, expression<T>& _expr);

template<typename T>
std::vector<expression<T>> sort(std::vector<expression<T>>& _list);

template<typename T>
expression<T> sqrt(expression<T>& _expr);

template<typename T>
expression<T> xthru(expression<T>& _expr);

template<typename T>
std::string zeroequiv(expression<T>& _expr, std::string const& _var);

}
