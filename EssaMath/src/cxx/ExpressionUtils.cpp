#include "include/ExpressionUtils.hpp"
#include "include/Parser.hpp"
#include <cstddef>
#include <string>
#include <utility>

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
expression<T> carg(expression<T>& _expr){
    return construct_expression(_expr, evaluate("carg(" + _expr.to_string() + ")"));
}
template expression<float>                      carg(expression<float>& _expr);
template expression<double>                     carg(expression<double>& _expr);
template expression<long double>                carg(expression<long double>& _expr);
template expression<std::complex<float>>        carg(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       carg(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  carg(expression<std::complex<long double>>& _expr);

template<typename T>
std::string constantp(expression<T>& _expr){
    return evaluate("constantp(" + _expr.to_string() + ")");
}
template std::string constantp(expression<float>& _expr);
template std::string constantp(expression<double>& _expr);
template std::string constantp(expression<long double>& _expr);
template std::string constantp(expression<std::complex<float>>& _expr);
template std::string constantp(expression<std::complex<double>>& _expr);
template std::string constantp(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> distrib(expression<T>& _expr){
    return construct_expression(_expr, evaluate("distrib(" + _expr.to_string() + ")"));
}
template expression<float>                      distrib(expression<float>& _expr);
template expression<double>                     distrib(expression<double>& _expr);
template expression<long double>                distrib(expression<long double>& _expr);
template expression<std::complex<float>>        distrib(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       distrib(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  distrib(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> multthru(expression<T>& _expr){
    return construct_expression(_expr, evaluate("multthru(" + _expr.to_string() + ")"));
}
template expression<float>                      multthru(expression<float>& _expr);
template expression<double>                     multthru(expression<double>& _expr);
template expression<long double>                multthru(expression<long double>& _expr);
template expression<std::complex<float>>        multthru(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       multthru(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  multthru(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> expand(expression<T>& _expr){
    return construct_expression(_expr, evaluate("expand(" + _expr.to_string() + ")"));
}
template expression<float>                      expand(expression<float>& _expr);
template expression<double>                     expand(expression<double>& _expr);
template expression<long double>                expand(expression<long double>& _expr);
template expression<std::complex<float>>        expand(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       expand(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  expand(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> exp(expression<T>& _expr){
    return construct_expression(_expr, evaluate("exp(" + _expr.to_string() + ")"));
}
template expression<float>                      exp(expression<float>& _expr);
template expression<double>                     exp(expression<double>& _expr);
template expression<long double>                exp(expression<long double>& _expr);
template expression<std::complex<float>>        exp(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       exp(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  exp(expression<std::complex<long double>>& _expr);

template<typename T>
std::string freeof(std::vector<expression<T>>& _list, expression<T>& _expr){
    std::string _liststr;
    for(size_t i = 0; i < _list.size(); i++){
        if(i != 0){
            _liststr += ",";
        }
        _liststr += _list[i].to_string();
    }

    return evaluate("freeof(" + _liststr + "," + _expr.to_string() + ")");
}
template std::string freeof(std::vector<expression<float>>& _list,                        expression<float>& _expr);
template std::string freeof(std::vector<expression<double>>& _list,                       expression<double>& _expr);
template std::string freeof(std::vector<expression<long double>>& _list,                  expression<long double>& _expr);
template std::string freeof(std::vector<expression<std::complex<float>>>& _list,          expression<std::complex<float>>& _expr);
template std::string freeof(std::vector<expression<std::complex<double>>>& _list,         expression<std::complex<double>>& _expr);
template std::string freeof(std::vector<expression<std::complex<long double>>>& _list,    expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> realpart(expression<T>& _expr){
    return construct_expression(_expr, evaluate("realpart(" + _expr.to_string() + ")"));
}
template expression<float>                      realpart(expression<float>& _expr);
template expression<double>                     realpart(expression<double>& _expr);
template expression<long double>                realpart(expression<long double>& _expr);
template expression<std::complex<float>>        realpart(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       realpart(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  realpart(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> imagpart(expression<T>& _expr){
    return construct_expression(_expr, evaluate("imagpart(" + _expr.to_string() + ")"));
}
template expression<float>                      imagpart(expression<float>& _expr);
template expression<double>                     imagpart(expression<double>& _expr);
template expression<long double>                imagpart(expression<long double>& _expr);
template expression<std::complex<float>>        imagpart(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       imagpart(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  imagpart(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> isolate(expression<T>& _expr, std::string const& _var){

    std::string ev = evaluate("isolate(" + _expr.to_string() + "," + _var + ")");

    std::string expression_string;
    if(ev.find_last_of("%t") != std::string::npos)
        expression_string = ev.substr(0, ev.find_last_of("%t") - 2) + ")";
    else
        expression_string = ev;
    return construct_expression(_expr, expression_string);
}
template expression<float>                      isolate(expression<float>& _expr, std::string const& _var);
template expression<double>                     isolate(expression<double>& _expr, std::string const& _var);
template expression<long double>                isolate(expression<long double>& _expr, std::string const& _var);
template expression<std::complex<float>>        isolate(expression<std::complex<float>>& _expr, std::string const& _var);
template expression<std::complex<double>>       isolate(expression<std::complex<double>>& _expr, std::string const& _var);
template expression<std::complex<long double>>  isolate(expression<std::complex<long double>>& _expr, std::string const& _var);

template<typename T>
expression<T> integrate(expression<T>& _expr, std::string const& _var){
    return construct_expression(_expr, evaluate("integrate(" + _expr.to_string() + "," + _var + ")"));
}
template expression<float>                      integrate(expression<float>& _expr, std::string const& _var);
template expression<double>                     integrate(expression<double>& _expr, std::string const& _var);
template expression<long double>                integrate(expression<long double>& _expr, std::string const& _var);
template expression<std::complex<float>>        integrate(expression<std::complex<float>>& _expr, std::string const& _var);
template expression<std::complex<double>>       integrate(expression<std::complex<double>>& _expr, std::string const& _var);
template expression<std::complex<long double>>  integrate(expression<std::complex<long double>>& _expr, std::string const& _var);

template<typename T>
expression<T> integrate(expression<T>& _expr, expression<T>& _lhs, expression<T>& _rhs, std::string const& _var){
    return construct_expression(_expr, evaluate("integrate(" + _expr.to_string() + "," + _var + ", " + _lhs.to_string() + ", " + _rhs.to_string() + ")"));
}
template expression<float>                      integrate(expression<float>& _expr,                     expression<float>& _lhs,                      expression<float>& _rhs, std::string const& _var);
template expression<double>                     integrate(expression<double>& _expr,                    expression<double>& _lhs,                     expression<double>& _rhs, std::string const& _var);
template expression<long double>                integrate(expression<long double>& _expr,               expression<long double>& _lhs,                expression<long double>& _rhs, std::string const& _var);
template expression<std::complex<float>>        integrate(expression<std::complex<float>>& _expr,       expression<std::complex<float>>& _lhs,        expression<std::complex<float>>& _rhs, std::string const& _var);
template expression<std::complex<double>>       integrate(expression<std::complex<double>>& _expr,      expression<std::complex<double>>& _lhs,       expression<std::complex<double>>& _rhs, std::string const& _var);
template expression<std::complex<long double>>  integrate(expression<std::complex<long double>>& _expr, expression<std::complex<long double>>& _lhs,  expression<std::complex<long double>>& _rhs, std::string const& _var);

template<typename T>
expression<T> differentiate(expression<T>& _expr, std::string const& _var){
    return construct_expression(_expr, evaluate("diff(" + _expr.to_string() + "," + _var + ")"));
}
template expression<float>                      differentiate(expression<float>& _expr, std::string const& _var);
template expression<double>                     differentiate(expression<double>& _expr, std::string const& _var);
template expression<long double>                differentiate(expression<long double>& _expr, std::string const& _var);
template expression<std::complex<float>>        differentiate(expression<std::complex<float>>& _expr, std::string const& _var);
template expression<std::complex<double>>       differentiate(expression<std::complex<double>>& _expr, std::string const& _var);
template expression<std::complex<long double>>  differentiate(expression<std::complex<long double>>& _expr, std::string const& _var);

template<typename T>
int nterms(expression<T>& _expr){
    return std::stoi(evaluate("nterms(" + _expr.to_string() + ")"));
}
template int nterms(expression<float>& _expr);
template int nterms(expression<double>& _expr);
template int nterms(expression<long double>& _expr);
template int nterms(expression<std::complex<float>>& _expr);
template int nterms(expression<std::complex<double>>& _expr);
template int nterms(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> optimize(expression<T>& _expr){
    return construct_expression(_expr, evaluate("optimize(" + _expr.to_string() + ")"));
}
template expression<float>                      optimize(expression<float>& _expr);
template expression<double>                     optimize(expression<double>& _expr);
template expression<long double>                optimize(expression<long double>& _expr);
template expression<std::complex<float>>        optimize(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       optimize(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  optimize(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> part(expression<T>& _expr, const std::vector<int> _part, const std::vector<int> _noun){
    std::string _command = "part(" + _expr.to_string();
    for(const auto& p : _part){
        _command += "," + std::to_string(p);
    }
    if(!_noun.empty()){
        _command += ",[";
        for(size_t i = 0; i < _noun.size(); i++){
            if(i != 0){
                _command += ",";
            }

            _command += std::to_string(_noun[i]);
        }
        _command += "]";
    }
    _command += ")";
    
    return construct_expression(_expr, evaluate(_command));
}
template expression<float>                      part(expression<float>& _expr,                     const std::vector<int> _part, const std::vector<int> _noun);
template expression<double>                     part(expression<double>& _expr,                    const std::vector<int> _part, const std::vector<int> _noun);
template expression<long double>                part(expression<long double>& _expr,               const std::vector<int> _part, const std::vector<int> _noun);
template expression<std::complex<float>>        part(expression<std::complex<float>>& _expr,       const std::vector<int> _part, const std::vector<int> _noun);
template expression<std::complex<double>>       part(expression<std::complex<double>>& _expr,      const std::vector<int> _part, const std::vector<int> _noun);
template expression<std::complex<long double>>  part(expression<std::complex<long double>>& _expr, const std::vector<int> _part, const std::vector<int> _noun);
template<typename T>

expression<T> part(expression<T>& _expr, const std::initializer_list<int> _part, const std::initializer_list<int> _noun){
    std::string _command = "part(" + _expr.to_string();
    for(const auto& p : _part){
        _command += "," + std::to_string(p);
    }
    if(_noun.size() != 0){
        _command += ",[";
        for(size_t i = 0; i < _noun.size(); i++){
            if(i != 0){
                _command += ",";
            }

            _command += std::to_string(*(_noun.begin() + i));
        }
        _command += "]";
    }
    _command += ")";

    return construct_expression(_expr, evaluate(_command));
}
template expression<float>                      part(expression<float>& _expr,                     const std::initializer_list<int> _part, const std::initializer_list<int> _noun);
template expression<double>                     part(expression<double>& _expr,                    const std::initializer_list<int> _part, const std::initializer_list<int> _noun);
template expression<long double>                part(expression<long double>& _expr,               const std::initializer_list<int> _part, const std::initializer_list<int> _noun);
template expression<std::complex<float>>        part(expression<std::complex<float>>& _expr,       const std::initializer_list<int> _part, const std::initializer_list<int> _noun);
template expression<std::complex<double>>       part(expression<std::complex<double>>& _expr,      const std::initializer_list<int> _part, const std::initializer_list<int> _noun);
template expression<std::complex<long double>>  part(expression<std::complex<long double>>& _expr, const std::initializer_list<int> _part, const std::initializer_list<int> _noun);

template<typename T>
std::pair<expression<T>,expression<T>> partition(expression<T>& _expr, std::string const& _var){
    std::vector<expression<T>> _res;
    std::string _result = evaluate("partition(" + _expr.to_string() + "," + _var+ ")").substr(1);
    std::string _buf;
    for(const auto& c : _result){
        if((c == ',' || c == ']') && !_buf.empty()){
            expression<T> _expr;
            _res.push_back(construct_expression(_expr, _buf));
            _buf = "";
        }else{
            _buf += c;
        }
    }

    return std::make_pair(_res.front(), _res.back());
}
template std::pair<expression<float>                    , expression<float>                    > partition(expression<float>& _expr, std::string const& _var);
template std::pair<expression<double>                   , expression<double>                   > partition(expression<double>& _expr, std::string const& _var);
template std::pair<expression<long double>              , expression<long double>              > partition(expression<long double>& _expr, std::string const& _var);
template std::pair<expression<std::complex<float>>      , expression<std::complex<float>>      > partition(expression<std::complex<float>>& _expr, std::string const& _var);
template std::pair<expression<std::complex<double>>     , expression<std::complex<double>>     > partition(expression<std::complex<double>>& _expr, std::string const& _var);
template std::pair<expression<std::complex<long double>>, expression<std::complex<long double>>> partition(expression<std::complex<long double>>& _expr, std::string const& _var);

template<typename T>
expression<T> polarform(expression<T>& _expr){
    return construct_expression(_expr, evaluate("polarform(" + _expr.to_string() + ")"));
}
template expression<float>                      polarform(expression<float>& _expr);
template expression<double>                     polarform(expression<double>& _expr);
template expression<long double>                polarform(expression<long double>& _expr);
template expression<std::complex<float>>        polarform(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       polarform(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  polarform(expression<std::complex<long double>>& _expr);

template<typename T>
expression<T> rectform(expression<T>& _expr){
    return construct_expression(_expr, evaluate("rectform(" + _expr.to_string() + ")"));
}
template expression<float>                      rectform(expression<float>& _expr);
template expression<double>                     rectform(expression<double>& _expr);
template expression<long double>                rectform(expression<long double>& _expr);
template expression<std::complex<float>>        rectform(expression<std::complex<float>>& _expr);
template expression<std::complex<double>>       rectform(expression<std::complex<double>>& _expr);
template expression<std::complex<long double>>  rectform(expression<std::complex<long double>>& _expr);

}
