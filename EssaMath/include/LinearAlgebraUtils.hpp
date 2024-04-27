#pragma once
#include "Expression.hpp"
#include <string>

namespace Essa::Math{

template<typename T>
expression<T> addcol(expression<T>& _M, std::vector<expression<T>>& _list);

template<typename T>
expression<T> addrow(expression<T>& _M, std::vector<expression<T>>& _list);

template<typename T>
expression<T> adjoint(expression<T>& _M);

template<typename T>
expression<T> augcoefmatrix(std::vector<expression<T>>& _eqn, std::vector<std::string> _x);

template<typename T>
expression<T> cauchy_matrix(std::vector<std::string> _x, std::vector<std::string> _y);

template<typename T>
expression<T> cauchy_matrix(std::vector<std::string> _x);

template<typename T>
expression<T> charpoly(expression<T>& _M, std::vector<std::string>& _x);

template<typename T>
expression<T> coefmatrix(std::vector<expression<T>>& _eqn, std::vector<std::string> _x);

template<typename T>
expression<T> col(expression<T>& _M, int _i);

template<typename T>
expression<T> columnvector(std::vector<expression<T>>& _L);

template<typename T>
expression<T> covect(std::vector<expression<T>>& _L);

template<typename T>
expression<T> conjugate(expression<T>& _x);

template<typename T>
expression<T> determinant(expression<T>& _M);

template<typename T>
expression<T> diagmatrix(int _n, expression<T>& _x);

template<typename T>
expression<T> echelon(expression<T>& _M);

template<typename T>
expression<T> eigenvalues(expression<T>& _M);

template<typename T>
expression<T> eivals(expression<T>& _M);

template<typename T>
expression<T> ematrix(int _m, int _n, expression<T>& _x, int _i, int _j);

template<typename T>
expression<T> genmatrix(expression<T>& _a, int _i_2, int _j_2, int _i_1, int _j_1);

template<typename T>
expression<T> genmatrix(expression<T>& _a, int _i_2, int _j_2, int _i_1);

template<typename T>
expression<T> genmatrix(expression<T>& _a, int _i_2, int _j_2);

template<typename T>
expression<T> gramschmidt(expression<T>& _x);

template<typename T>
expression<T> gramschmidt(expression<T>& _x, expression<T>& _F);

template<typename T>
expression<T> ident(int _n);

template<typename T>
expression<T> innerproduct(expression<T>& _x, expression<T>& _y);

template<typename T>
expression<T> inprod(expression<T>& _x, expression<T>& _y);

template<typename T>
expression<T> invert(expression<T>& _M);

template<typename T>
expression<T> list_matrix_entries(expression<T>& _M);

template<typename T>
expression<T> matrix(std::vector<expression<T>>& _rows);

template<typename T>
expression<T> mattrace(expression<T>& _M);

template<typename T>
expression<T> minor(expression<T>& _M, int _i, int _j);

template<typename T>
expression<T> ncharpoly(expression<T>& _M, std::string _x);

template<typename T>
expression<T> newdet(expression<T>& _M);

template<typename T>
expression<T> permanent(expression<T>& _M);

template<typename T>
expression<T> rank(expression<T>& _M);

template<typename T>
expression<T> row(expression<T>& _M, int _i);

template<typename T>
expression<T> scalefactors(std::vector<expression<T>>& _expr, std::vector<expression<T>>& _det);

template<typename T>
expression<T> setelmx(expression<T>& _x, int _i, int _j, expression<T>& _M);

template<typename T>
expression<T> similaritytransform(expression<T>& _M);

template<typename T>
expression<T> simtran(expression<T>& _M);

template<typename T>
expression<T> simtran(std::vector<int> _i, expression<T>& _M, std::vector<int> _j);

template<typename T>
expression<T> simtran(std::vector<int> _i, expression<T>& _M);

template<typename T>
expression<T> simtran(expression<T>& _M, std::vector<int> _j);

template<typename T>
expression<T> transpose(expression<T>& _M);

template<typename T>
expression<T> triangularize(expression<T>& _M);

template<typename T>
expression<T> uniteigenvectors(expression<T>& _M);

template<typename T>
expression<T> ueivects(expression<T>& _M);

template<typename T>
expression<T> ueivects(expression<T>& _x);

template<typename T>
expression<T> uvect(expression<T>& _x);

template<typename T>
expression<T> vectorsimp(expression<T>& _expr);

template<typename T>
expression<T> zeromatrix(int _m, int _n);

}
