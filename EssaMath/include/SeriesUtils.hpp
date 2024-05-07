#pragma once
#include "Expression.hpp"
#include <string>
#include <vector>

namespace Essa::Math{

template<typename T>
expression<T> nusum(expression<T>& _expr, std::string _x, expression<T>& _i_0, expression<T>& _i_1);

template<typename T>
expression<T> pade(expression<T>& _taylor_series, int numer_deg_bound, int denom_deg_bound);

template<typename T>
expression<T> powerseries(expression<T>& _expr, std::string _x, expression<T>& _a);

template<typename T>
expression<T> revert(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> revert2(expression<T>& _expr, std::string _x, int _n);

template<typename T>
expression<T> taylor(expression<T>& _expr, std::string _x, expression<T>& _a, int _n);

template<typename T>
expression<T> taylor(expression<T>& _expr, std::vector<std::string> _x, expression<T>& _a, int _n);

template<typename T>
expression<T> taylor(expression<T>& _expr, std::vector<std::string> _x, std::vector<expression<T>>& _a, std::vector<int> _n);

template<typename T>
expression<T> taylorinfo(expression<T>& _expr);

template<typename T>
bool taylorp(expression<T>& _expr);

template<typename T>
expression<T> taylor_simplifier(expression<T>& _expr);

template<typename T>
expression<T> taytorat(expression<T>& _expr);

template<typename T>
expression<T> trunc(expression<T>& _expr);

template<typename T>
expression<T> intopois(expression<T>& _a);

template<typename T>
expression<T> outofpois(expression<T>& _a);

template<typename T>
expression<T> poisdiff(expression<T>& _a, std::string _x);

template<typename T>
expression<T> poisexpt(expression<T>& _a, std::string _x);

template<typename T>
expression<T> poisint(expression<T>& _a, std::string _x);

template<typename T>
expression<T> poismap(expression<T>& _series, expression<T>& _sinfn, expression<T>& _cosfn);

template<typename T>
expression<T> poisplus(expression<T>& _a, expression<T>& _b);

template<typename T>
expression<T> poissimp(expression<T>& _a);

template<typename T>
expression<T> poissubst(expression<T>& _a, expression<T>& _b, expression<T>& _c);

template<typename T>
expression<T> poistimes(expression<T>& _a, expression<T>& _b);

}
