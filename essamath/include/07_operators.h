// 07_operators.h
#include "expression.h"
#ifndef OPERATORS_H
#define OPERATORS_H

#ifdef __cplusplus
extern "C" {
#endif

int em_infix(em_object _op);
int em_infix_2(em_object _op, em_object _lbp, em_object _rbp);
int em_infix_3(em_object _op, em_object _lbp, em_object _rbp, em_object _lpos, em_object _rpos, em_object _pos);
int em_matchfix(em_object _ldelimiter, em_object _rdelimiter);
int em_matchfix_2(em_object _ldelimiter, em_object _rdelimiter, em_object _arg_pos, em_object _pos);
int em_nary(em_object _op);
int em_nary_2(em_object _op, em_object _bp, em_object _arg_pos, em_object _pos);
int em_nofix(em_object _op);
int em_nofix_2(em_object _op, em_object _pos);
int em_postfix(em_object _op);
int em_postfix_2(em_object _op, em_object _lbp, em_object _lpos, em_object _pos);
int em_prefix(em_object _op);
int em_prefix_2(em_object _op, em_object _rbp, em_object _rpos, em_object _pos);

#ifdef __cplusplus
}
#endif

#endif // OPERATORS_H
