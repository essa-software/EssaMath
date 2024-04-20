#pragma once
#include "SymbolTable.hpp"
#include "Parser.hpp"
#include "Expression.hpp"
#include <initializer_list>
#include <optional>
#include <vector>

namespace Essa::Math{

template<typename T>
expression<T> demoivre(expression<T>& _expr);

template<typename T>
expression<T> expandwrt(expression<T>& _expr, const std::vector<std::string>& _list);

template<typename T>
expression<T> expandwrt(expression<T>& _expr, const std::initializer_list<std::string>& _list);

template<typename T>
expression<T> expandwrt_factored(expression<T>& _expr, const std::vector<std::string>& _list);

template<typename T>
expression<T> expandwrt_factored(expression<T>& _expr, const std::initializer_list<std::string>& _list);

template<typename T>
expression<T> exponentialize(expression<T>& _expr);

template<typename T>
expression<T> radcan(expression<T>& _expr);

template<typename T>
expression<T> scsimp(expression<T>& _expr, std::vector<expression<T>>& _eq_list);

}
