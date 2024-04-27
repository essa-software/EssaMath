#pragma once
#include "Expression.hpp"

namespace Essa::Math{

template<typename T>
expression<T> acos(expression<T>& _expr);

template<typename T>
expression<T> acosh(expression<T>& _expr);

template<typename T>
expression<T> acot(expression<T>& _expr);

template<typename T>
expression<T> acoth(expression<T>& _expr);

template<typename T>
expression<T> acsc(expression<T>& _expr);

template<typename T>
expression<T> acsch(expression<T>& _expr);

template<typename T>
expression<T> asec(expression<T>& _expr);

template<typename T>
expression<T> asech(expression<T>& _expr);

template<typename T>
expression<T> asin(expression<T>& _expr);

template<typename T>
expression<T> asinh(expression<T>& _expr);

template<typename T>
expression<T> atan(expression<T>& _expr);

template<typename T>
expression<T> atanh(expression<T>& _expr);

template<typename T>
expression<T> cos(expression<T>& _expr);

template<typename T>
expression<T> cosh(expression<T>& _expr);

template<typename T>
expression<T> cot(expression<T>& _expr);

template<typename T>
expression<T> coth(expression<T>& _expr);

template<typename T>
expression<T> csc(expression<T>& _expr);

template<typename T>
expression<T> csch(expression<T>& _expr);

template<typename T>
expression<T> sec(expression<T>& _expr);

template<typename T>
expression<T> sech(expression<T>& _expr);

template<typename T>
expression<T> sin(expression<T>& _expr);

template<typename T>
expression<T> sinh(expression<T>& _expr);

template<typename T>
expression<T> tan(expression<T>& _expr);

template<typename T>
expression<T> tanh(expression<T>& _expr);

template<typename T>
expression<T> trigexpand(expression<T>& _expr);

template<typename T>
expression<T> trigreduce(expression<T>& _expr);

template<typename T>
expression<T> trigreduce(expression<T>& _expr, std::string _x);

template<typename T>
expression<T> trigsimp(expression<T>& _expr);

template<typename T>
expression<T> trigrat(expression<T>& _expr);

}
