#include "include/OperatorUtils.hpp"
#include <cstddef>
#include <string>

namespace Essa::Math{

extern std::string evaluate(const std::string& _exp);

template<typename T>
expression<T> construct_expression(expression<T>& _expr, std::string const& _eval){
    parser<T> _parser;
    expression<T> _result;
    _result.register_symbol_table(_expr.get_symbol_table());
    _parser.compile(_eval, _result);
    return _result;
}

template<typename T>
expression<T> abs(expression<T>& _expr){
    return construct_expression(_expr, evaluate("abs(" + _expr.to_string() + ")"));
}
template expression<float>                      abs(expression<float>& _expr);
template expression<double>                     abs(expression<double>& _expr);
template expression<long double>                abs(expression<long double>& _expr);
template expression<std::complex<float>>        abs(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       abs(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  abs(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> cabs(expression<T>& _expr){
    return construct_expression(_expr, evaluate("cabs(" + _expr.to_string() + ")"));
}
template expression<float>                      cabs(expression<float>& _expr);
template expression<double>                     cabs(expression<double>& _expr);
template expression<long double>                cabs(expression<long double>& _expr);
template expression<std::complex<float>>        cabs(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       cabs(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  cabs(expression<std::complex<long double>>& _expr);

template<typename T>
std::string compare(expression<T>& _lhs, expression<T>& _rhs){
    return evaluate("compare(" + _lhs.to_string() + "," + _rhs.to_string() + ")");
}
template std::string compare(expression<float>& _list,                        expression<float>& _expr);
template std::string compare(expression<double>& _list,                       expression<double>& _expr);
template std::string compare(expression<long double>& _list,                  expression<long double>& _expr);
template std::string compare(expression<std::complex<float>>& _list,          expression<std::complex<float>>& _expr);
template std::string compare(expression<std::complex<double>>& _list,         expression<std::complex<double>>& _expr);
template std::string compare(expression<std::complex<long double>>& _list,    expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> polymod(expression<T>& _expr, const int _val){
    return construct_expression(_expr, evaluate("polymod(" + _expr.to_string() + ", " + std::to_string(_val) + ")"));
}
template expression<float>                      polymod(expression<float>& _expr,                     const int _val);
template expression<double>                     polymod(expression<double>& _expr,                    const int _val);
template expression<long double>                polymod(expression<long double>& _expr,               const int _val);
template expression<std::complex<float>>        polymod(expression<std::complex<float>>& _expr,       const int _val);
template expression<std::complex<double>>       polymod(expression<std::complex<double>>& _expr,      const int _val);
template expression<std::complex<long double>>  polymod(expression<std::complex<long double>>& _expr, const int _val);

template<typename T>
expression<T> psubst(std::vector<expression<T>>& _list, expression<T>& _expr){
    std::string _liststr = "[";
    for(size_t i = 0; i < _list.size(); i++){
        if(i != 0){
            _liststr += ",";
        }
        _liststr += _list[i].to_string();
    }
    _liststr += "]";

    return construct_expression(_expr, evaluate("psubst(" + _liststr + ", " + _expr.to_string() + ")"));
}
template expression<float>                      psubst(std::vector<expression<float>>& _list,                        expression<float>& _expr);
template expression<double>                     psubst(std::vector<expression<double>>& _list,                       expression<double>& _expr);
template expression<long double>                psubst(std::vector<expression<long double>>& _list,                  expression<long double>& _expr);
template expression<std::complex<float>>        psubst(std::vector<expression<std::complex<float>>>& _list,          expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       psubst(std::vector<expression<std::complex<double>>>& _list,         expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  psubst(std::vector<expression<std::complex<long double>>>& _list,    expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> subst(std::vector<expression<T>>& _list, expression<T>& _expr){
    std::string _liststr = "[";
    for(size_t i = 0; i < _list.size(); i++){
        if(i != 0){
            _liststr += ",";
        }
        _liststr += _list[i].to_string();
    }
    _liststr += "]";

    return construct_expression(_expr, evaluate("psubst(" + _liststr + ", " + _expr.to_string() + ")"));
}
template expression<float>                      subst(std::vector<expression<float>>& _list,                        expression<float>& _expr);
template expression<double>                     subst(std::vector<expression<double>>& _list,                       expression<double>& _expr);
template expression<long double>                subst(std::vector<expression<long double>>& _list,                  expression<long double>& _expr);
template expression<std::complex<float>>        subst(std::vector<expression<std::complex<float>>>& _list,          expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       subst(std::vector<expression<std::complex<double>>>& _list,         expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  subst(std::vector<expression<std::complex<long double>>>& _list,    expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> rationalize(expression<T>& _expr){
    return construct_expression(_expr, evaluate("rationalize(" + _expr.to_string() + ")"));
}
template expression<float>                      rationalize(expression<float>& _expr);
template expression<double>                     rationalize(expression<double>& _expr);
template expression<long double>                rationalize(expression<long double>& _expr);
template expression<std::complex<float>>        rationalize(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       rationalize(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  rationalize(expression<std::complex<long double>>& _expr);

template<typename T>
std::string sign(expression<T>& _expr){
    return evaluate("sign(" + _expr.to_string() + ")");
}
template std::string sign(expression<float>& _expr);
template std::string sign(expression<double>& _expr);
template std::string sign(expression<long double>& _expr);
template std::string sign(expression<std::complex<float>>& _expr);
template std::string sign(expression<std::complex<double>>& _expr);
template std::string sign(expression<std::complex<long double>>& _expr);

template<typename T>
std::vector<expression<T>> sort(std::vector<expression<T>>& _list){
    std::string _liststr = "[";
    for(size_t i = 0; i < _list.size(); i++){
        if(i != 0){
            _liststr += ",";
        }
        _liststr += _list[i].to_string();
    }
    _liststr += "]";
    std::vector<expression<T>> _res;
    size_t i = 0;
    std::string _result = evaluate("sort(" + _liststr + ")").substr(1);
    std::string _buf;
    for(const auto& c : _result){
        if((c == ',' || c == ']') && !_buf.empty()){
            _res.push_back(construct_expression(_list[i], _buf));
        }else{
            _buf += c;
        }
    }
    return _res;
}
template std::vector<expression<float>>                      sort(std::vector<expression<float>>& _expr);
template std::vector<expression<double>>                     sort(std::vector<expression<double>>& _expr);
template std::vector<expression<long double>>                sort(std::vector<expression<long double>>& _expr);
template std::vector<expression<std::complex<float>>>        sort(std::vector<expression<std::complex<float>>>& _expr);
template std::vector<expression<std::complex<double>>>       sort(std::vector<expression<std::complex<double>>>& _expr);
template std::vector<expression<std::complex<long double>>>  sort(std::vector<expression<std::complex<long double>>>& _expr);

template<typename T>
expression<T> sqrt(expression<T>& _expr){
    return construct_expression(_expr, evaluate("sqrt(" + _expr.to_string() + ")"));
}
template expression<float>                      sqrt(expression<float>& _expr);
template expression<double>                     sqrt(expression<double>& _expr);
template expression<long double>                sqrt(expression<long double>& _expr);
template expression<std::complex<float>>        sqrt(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       sqrt(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  sqrt(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> xthru(expression<T>& _expr){
    return construct_expression(_expr, evaluate("xthru(" + _expr.to_string() + ")"));
}
template expression<float>                      xthru(expression<float>& _expr);
template expression<double>                     xthru(expression<double>& _expr);
template expression<long double>                xthru(expression<long double>& _expr);
template expression<std::complex<float>>        xthru(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       xthru(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  xthru(expression<std::complex<long double>>& _expr);

template<typename T>
std::string zeroequiv(expression<T>& _expr, std::string const& _var){
    return evaluate("zeroequiv(" + _expr.to_string() + "," + _var + ")");
}
template std::string zeroequiv(expression<float>& _list,                        std::string const& _var);
template std::string zeroequiv(expression<double>& _list,                       std::string const& _var);
template std::string zeroequiv(expression<long double>& _list,                  std::string const& _var);
template std::string zeroequiv(expression<std::complex<float>>& _list,          std::string const& _var);
template std::string zeroequiv(expression<std::complex<double>>& _list,         std::string const& _var);
template std::string zeroequiv(expression<std::complex<long double>>& _list,    std::string const& _var);

}
