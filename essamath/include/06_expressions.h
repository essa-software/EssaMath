// 06_expressions.h
#include "expression.h"
#ifndef EXPRESSIONS_H
#define EXPRESSIONS_H

#ifdef __cplusplus
extern "C" {
#endif

int em_alias(size_t n, ...);
int em_args(em_object _expr);
int em_atom(em_object _expr);
int em_box(em_object _expr);
int em_box_2(em_object _expr, em_object _a);
int em_collapse(em_object _expr);
int em_copy(em_object _e);
int em_disolate(em_object _expr, size_t n, ...);
int em_dispform(em_object _expr);
int em_dispformall(em_object _expr);
int em_dpart(em_object _expr, size_t n, ...);
int em_ev(em_object _expr, size_t n, ...);
int em_freeof(size_t n, ...);
int em_inpart(em_object _expr, size_t n, ...);
int em_isolate(em_object _expr, em_object _x);
int em_listofvars(em_object _expr);
int em_lfreeof(em_object _list, em_object _expr);
int em_lpart(em_object _label, em_object _expr, size_t n, ...);
int em_nounify(em_object _f);
int em_nterms(em_object _expr);
int em_op(em_object _expr);
int em_operatorp(em_object _expr, em_object _op);
int em_optimize(em_object _expr);
int em_ordergreat(size_t n, ...);
int em_orderless(size_t n, ...);
int em_ordergreatp(em_object _expr1, em_object _expr2);
int em_orderlessp(em_object _expr1, em_object _expr2);
int em_part(em_object _expr, size_t n, ...);
int em_partition(em_object _expr, em_object _x);
int em_pickapart(em_object _expr, em_object _n);
int em_psubst(em_object _list, em_object _expr);
int em_psubst_2(em_object _a, em_object _b, em_object _expr);
int em_rembox_unlabelled(em_object _expr);
int em_rembox_labelled(em_object _expr, em_object _label);
int em_rembox(em_object _expr);
int em_reveal(em_object _expr, em_object _depth);
int em_sqrtdenest(em_object _expr);
int em_sublis(em_object _list, em_object _expr);
int em_subst(em_object _a, em_object _b, em_object _c);
int em_substinpart(em_object _x, em_object _expr, size_t n, ...);
int em_substpart(em_object _x, em_object _expr, size_t n, ...);
int em_symbolp(em_object _expr);
int em_unorder(void);
int em_verbify(em_object _f);

#ifdef __cplusplus
}
#endif

#endif // EXPRESSIONS_H
