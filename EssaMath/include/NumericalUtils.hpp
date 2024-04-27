#pragma once
#include "Expression.hpp"

namespace Essa::Math{

template<typename T>
expression<T> polartorect(expression<T>& _r, expression<T>& _t);

template<typename T>
expression<T> recttopolar(expression<T>& _a, expression<T>& _b);

template<typename T>
expression<T> inverse_fft(std::vector<expression<T>>& _y);

template<typename T>
expression<T> fft(std::vector<expression<T>>& _x);

template<typename T>
expression<T> horner(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> find_root(expression<T>& _expr, std::string _x, expression<T>& _a, expression<T>& _b);

template<typename T>
expression<T> bf_find_root(expression<T>& _expr, std::string _x, expression<T>& _a, expression<T>& _b);

template<typename T>
expression<T> newton(expression<T>& _expr, std::string _x, expression<T>& _x_0);

template<typename T>
expression<T> absint(expression<T>& _f, std::string _x, std::string _halfplane);

template<typename T>
expression<T> absint(expression<T>& _f, std::string _x);

template<typename T>
expression<T> absint(expression<T>& _f, std::string _x, expression<T>& _a, expression<T>& _b);

template<typename T>
expression<T> fourier(expression<T>& _f, std::string _x, expression<T>& _p);

template<typename T>
expression<T> foursimp(expression<T>& _l);

template<typename T>
expression<T> fourexpand(expression<T>& _l, std::string _x, expression<T>& _p, expression<T>& _limit);

template<typename T>
expression<T> fourcos(expression<T>& _f, std::string _x, expression<T>& _p);

template<typename T>
expression<T> foursin(expression<T>& _f, std::string _x, expression<T>& _p);

template<typename T>
expression<T> totalfourier(expression<T>& _f, std::string _x, expression<T>& _p);

template<typename T>
expression<T> fourint(expression<T>& _f, std::string _x);

template<typename T>
expression<T> fourintcos(expression<T>& _f, std::string _x);

template<typename T>
expression<T> fourintsin(expression<T>& _f, std::string _x);

}
