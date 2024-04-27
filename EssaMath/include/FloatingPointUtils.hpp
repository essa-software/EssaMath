#pragma once
#include "Expression.hpp"

namespace Essa::Math{

template<typename T>
expression<T> bffac(expression<T>& _expr, int _n);

template<typename T>
expression<T> bfloat(expression<T>& _expr);

template<typename T>
std::string bfloatp(expression<T>& _expr);

template<typename T>
T bfpsi(int _n, double _z, int _fpprec);

template<typename T>
T bfpsi0(double _z, int _fpprec);

}
