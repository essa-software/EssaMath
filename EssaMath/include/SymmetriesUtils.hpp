#pragma once
#include "Expression.hpp"
#include <string>
#include <vector>

namespace Essa::Math{

template<typename T>
expression<T> comp2pui(int _n, std::vector<expression<T>>& _L);

template<typename T>
expression<T> ele2pui(int _m, std::vector<expression<T>>& _L);

template<typename T>
expression<T> ele2comp(int _m, std::vector<expression<T>>& _L);

template<typename T>
expression<T> elem(std::vector<expression<T>>& _ele, expression<T>& _sym, std::vector<std::string> _lvar);

template<typename T>
expression<T> mon2schur(std::vector<expression<T>>& _L);

}
