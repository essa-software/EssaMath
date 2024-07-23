// 11_maximas_database.h
#include "expression.h"
#ifndef MAXIMAS_DATABASE_H
#define MAXIMAS_DATABASE_H

#ifdef __cplusplus
extern "C" {
#endif

int em_constantp(em_object _expr);
int em_declare(size_t n, ...);
int em_featurep(em_object _a, em_object _f);
int em_get(em_object _a, em_object _i);
int em_nonscalarp(em_object _expr);
int em_printprops(em_object _a, em_object _i);
int em_properties(em_object _a);
int em_propvars(em_object _prop);
int em_put(em_object _atom, em_object _value, em_object _indicator);
int em_qput(em_object _atom, em_object _value, em_object _indicator);
int em_rem(em_object _atom, em_object _indicator);
int em_remove(size_t n, ...);
int em_scalarp(em_object _expr);
int em_activate(size_t n, ...);
int em_askequal(em_object _expr1, em_object _expr2);
int em_askinteger(size_t n, ...);
int em_asksign(em_object _expr);
int em_assume(size_t n, ...);
int em_deactivate(size_t n, ...);
int em_facts();
int em_facts_2(em_object _item);
int em_forget(size_t n, ...);
int em_forget_2(em_object _L);
int em_is(em_object _expr);
int em_killcontext(size_t n, ...);
int em_maybe(em_object _expr);
int em_newcontext(em_object _name);
int em_newcontext_2();
int em_sign(em_object _expr);
int em_supcontext(em_object _name, em_object _context);
int em_supcontext_2(em_object _name);
int em_supcontext_3();
int em_charfun(em_object _p);
int em_compare(em_object _x, em_object _y);
int em_equal(em_object _a, em_object _b);
int em_notequal(em_object _a, em_object _b);
int em_unknown(em_object _expr);
int em_zeroequiv(em_object _expr, em_object _v);

#ifdef __cplusplus
}
#endif

#endif // MAXIMAS_DATABASE_H
