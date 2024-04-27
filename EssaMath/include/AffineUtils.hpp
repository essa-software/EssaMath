#pragma once
#include "Expression.hpp"
#include <string>

namespace Essa::Math{

template<typename T>
expression<T> fast_linsolve(std::vector<expression<T>>& _expr, std::vector<std::string>& _x);

template<typename T>
expression<T> grobner_basis(std::vector<expression<T>>& _expr);

template<typename T>
expression<T> nc_degree(expression<T>& _p);

template<typename T>
expression<T> dotsimp(expression<T>& _f);

}
