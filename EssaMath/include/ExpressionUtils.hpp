#pragma once
#include "SymbolTable.hpp"
#include "Parser.hpp"
#include "Expression.hpp"
#include <initializer_list>
#include <optional>
#include <vector>

namespace Essa::Math{

template<typename T>
expression<T> carg(expression<T>& _expr);

template<typename T>
std::string constantp(expression<T>& _expr);

template<typename T>
expression<T> carg(expression<T>& _expr);

template<typename T>
expression<T> distrib(expression<T>& _expr);

template<typename T>
expression<T> multthru(expression<T>& _expr);

template<typename T>
expression<T> expand(expression<T>& _expr);

template<typename T>
expression<T> exp(expression<T>& _expr);

template<typename T>
std::string freeof(std::vector<expression<T>>& _list, expression<T>& _expr);

template<typename T>
expression<T> realpart(expression<T>& _expr);

template<typename T>
expression<T> imagpart(expression<T>& _expr);

template<typename T>
expression<T> isolate(expression<T>& _expr, std::string const& _var);

template<typename T>
expression<T> integrate(expression<T>& _expr, std::string const& _var);

template<typename T>
expression<T> integrate(expression<T>& _expr, expression<T>& _lhs, expression<T>& _rhs, std::string const& _var);

template<typename T>
expression<T> differentiate(expression<T>& _expr, std::string const& _var);

template<typename T>
int nterms(expression<T>& _expr);

template<typename T>
expression<T> optimize(expression<T>& _expr);

template<typename T>
expression<T> part(expression<T>& _expr, const std::vector<int> _part, const std::vector<int> _noun = std::vector<int>());

template<typename T>
expression<T> part(expression<T>& _expr, const std::initializer_list<int> _part, const std::initializer_list<int> _noun = std::initializer_list<int>());

template<typename T>
std::pair<expression<T>,expression<T>> partition(expression<T>& _expr, std::string const& _var);

template<typename T>
expression<T> polarform(expression<T>& _expr);

template<typename T>
expression<T> rectform(expression<T>& _expr);

}
