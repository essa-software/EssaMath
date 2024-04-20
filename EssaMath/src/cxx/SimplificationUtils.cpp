#include "include/SimplificationUtils.hpp"
#include <initializer_list>
#include <string>

namespace Essa::Math{

extern std::string evaluate(const std::string& _expr);

template<typename T>
expression<T> construct_expression(expression<T>& _expr, std::string const& _eval){
    parser<T> _parser;
    expression<T> _result;
    _result.register_symbol_table(_expr.get_symbol_table());
    _parser.compile(_eval, _result);
    return _result;
}

template<typename T>
expression<T> demoivre(expression<T>& _expr){
    return construct_expression(_expr, evaluate("demoivre(" + _expr.to_string() + ")"));
}
template expression<float>                      demoivre(expression<float>& _expr);
template expression<double>                     demoivre(expression<double>& _expr);
template expression<long double>                demoivre(expression<long double>& _expr);
template expression<std::complex<float>>        demoivre(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       demoivre(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  demoivre(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> expandwrt(expression<T>& _expr, const std::vector<std::string>& _list){
    std::string _command = "expandwrt(" + _expr.to_string();
    for(const auto& p : _list){
        _command += "," + p;
    }
    _command += ")";
    return construct_expression(_expr, evaluate(_command));
}
template expression<float>                      expandwrt(expression<float>& _expr,                     const std::vector<std::string>& _list);
template expression<double>                     expandwrt(expression<double>& _expr,                    const std::vector<std::string>& _list);
template expression<long double>                expandwrt(expression<long double>& _expr,               const std::vector<std::string>& _list);
template expression<std::complex<float>>        expandwrt(expression<std::complex<float>>& _expr,       const std::vector<std::string>& _list);
template expression<std::complex<double>>       expandwrt(expression<std::complex<double>>& _expr,      const std::vector<std::string>& _list);
template expression<std::complex<long double>>  expandwrt(expression<std::complex<long double>>& _expr, const std::vector<std::string>& _list);

template<typename T>
expression<T> expandwrt(expression<T>& _expr, const std::initializer_list<std::string>& _list){
    std::string _command = "expandwrt(" + _expr.to_string();
    for(const auto& p : _list){
        _command += "," + p;
    }
    _command += ")";
    return construct_expression(_expr, evaluate(_command));
}
template expression<float>                      expandwrt(expression<float>& _expr,                     const std::initializer_list<std::string>& _list);
template expression<double>                     expandwrt(expression<double>& _expr,                    const std::initializer_list<std::string>& _list);
template expression<long double>                expandwrt(expression<long double>& _expr,               const std::initializer_list<std::string>& _list);
template expression<std::complex<float>>        expandwrt(expression<std::complex<float>>& _expr,       const std::initializer_list<std::string>& _list);
template expression<std::complex<double>>       expandwrt(expression<std::complex<double>>& _expr,      const std::initializer_list<std::string>& _list);
template expression<std::complex<long double>>  expandwrt(expression<std::complex<long double>>& _expr, const std::initializer_list<std::string>& _list);

template<typename T>
expression<T> expandwrt_factored(expression<T>& _expr, const std::vector<std::string>& _list){
    std::string _command = "expandwrt_factored(" + _expr.to_string();
    for(const auto& p : _list){
        _command += "," + p;
    }
    _command += ")";
    return construct_expression(_expr, evaluate(_command));
}
template expression<float>                      expandwrt_factored(expression<float>& _expr,                     const std::vector<std::string>& _list);
template expression<double>                     expandwrt_factored(expression<double>& _expr,                    const std::vector<std::string>& _list);
template expression<long double>                expandwrt_factored(expression<long double>& _expr,               const std::vector<std::string>& _list);
template expression<std::complex<float>>        expandwrt_factored(expression<std::complex<float>>& _expr,       const std::vector<std::string>& _list);
template expression<std::complex<double>>       expandwrt_factored(expression<std::complex<double>>& _expr,      const std::vector<std::string>& _list);
template expression<std::complex<long double>>  expandwrt_factored(expression<std::complex<long double>>& _expr, const std::vector<std::string>& _list);

template<typename T>
expression<T> expandwrt_factored(expression<T>& _expr, const std::initializer_list<std::string>& _list){
    std::string _command = "expandwrt_factored(" + _expr.to_string();
    for(const auto& p : _list){
        _command += "," + p;
    }
    _command += ")";
    return construct_expression(_expr, evaluate(_command));
}
template expression<float>                      expandwrt_factored(expression<float>& _expr,                     const std::initializer_list<std::string>& _list);
template expression<double>                     expandwrt_factored(expression<double>& _expr,                    const std::initializer_list<std::string>& _list);
template expression<long double>                expandwrt_factored(expression<long double>& _expr,               const std::initializer_list<std::string>& _list);
template expression<std::complex<float>>        expandwrt_factored(expression<std::complex<float>>& _expr,       const std::initializer_list<std::string>& _list);
template expression<std::complex<double>>       expandwrt_factored(expression<std::complex<double>>& _expr,      const std::initializer_list<std::string>& _list);
template expression<std::complex<long double>>  expandwrt_factored(expression<std::complex<long double>>& _expr, const std::initializer_list<std::string>& _list);

template<typename T>
expression<T> exponentialize(expression<T>& _expr){
    return construct_expression(_expr, evaluate("exponentialize(" + _expr.to_string() + ")"));
}
template expression<float>                      exponentialize(expression<float>& _expr);
template expression<double>                     exponentialize(expression<double>& _expr);
template expression<long double>                exponentialize(expression<long double>& _expr);
template expression<std::complex<float>>        exponentialize(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       exponentialize(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  exponentialize(expression<std::complex<long double>>& _expr);
template<typename T>
expression<T> radcan(expression<T>& _expr){
    return construct_expression(_expr, evaluate("radcan(" + _expr.to_string() + ")"));
}
template expression<float>                      radcan(expression<float>& _expr);
template expression<double>                     radcan(expression<double>& _expr);
template expression<long double>                radcan(expression<long double>& _expr);
template expression<std::complex<float>>        radcan(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       radcan(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  radcan(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> scsimp(expression<T>& _expr, std::vector<expression<T>>& _eq_list){
    std::string _command = "scsimp(" + _expr.to_string();
    for(const auto& p : _eq_list){
        _command += "," + p.to_string();
    }
    _command += ")";
    return construct_expression(_expr, evaluate(_command));
}
template expression<float>                      scsimp(expression<float>& _expr,                        std::vector<expression<float>>& _eq_list);
template expression<double>                     scsimp(expression<double>& _expr,                       std::vector<expression<double>>& _eq_list);
template expression<long double>                scsimp(expression<long double>& _expr,                  std::vector<expression<long double>>& _eq_list);
template expression<std::complex<float>>        scsimp(expression<std::complex<float>>& _expr,          std::vector<expression<std::complex<float>>>& _eq_list);
template expression<std::complex<double>>       scsimp(expression<std::complex<double>>& _expr,         std::vector<expression<std::complex<double>>>& _eq_list);
template expression<std::complex<long double>>  scsimp(expression<std::complex<long double>>& _expr,    std::vector<expression<std::complex<long double>>>& _eq_list);

}
