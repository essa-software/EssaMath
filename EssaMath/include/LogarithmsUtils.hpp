#pragma once
#include "Expression.hpp"

namespace Essa::Math{

template<typename T>
expression<T> li(expression<T>& _s, expression<T>& _z);

template<typename T>
expression<T> log(expression<T>& _expr);

template<typename T>
expression<T> logarc(expression<T>& _expr);

template<typename T>
expression<T> logcontract(expression<T>& _expr);

}
