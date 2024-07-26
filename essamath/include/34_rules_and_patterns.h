// 34_rules_and_patterns.h
#include "expression.h"
#ifndef RULES_AND_PATTERNS_H
#define RULES_AND_PATTERNS_H

#ifdef __cplusplus
extern "C" {
#endif

int em_apply1(em_object _expr, size_t n, ...);
int em_apply2(em_object _expr, size_t n, ...);
int em_applyb1(em_object _expr, size_t n, ...);
int em_defmatch(em_object _progname, em_object _pattern);
int em_defmatch_2(em_object _progname, em_object _pattern, size_t n, ...);
int em_defrule(em_object _rulename, em_object _pattern, em_object _replacement);
int em_disprule(size_t n, ...);
int em_dispruleall();
int em_let(em_object _prod, em_object _repl, em_object _prodname, size_t n, ...);
int em_let_2(em_object _args, em_object _package_name);
int em_letrules();
int em_letrules_2(em_object _package_name);
int em_letsimp(em_object _expr);
int em_letsimp_2(em_object _expr, em_object _package_name);
int em_letsimp_3(em_object _expr, size_t n, ...);
int em_matchdeclare(size_t n, ...);
int em_remlet();
int em_remlet_2(em_object _prod, em_object _name);
int em_remletall();
int em_remletall_2(em_object _name);
int em_remrule(em_object _op, em_object _rulename);
int em_remruleall(em_object _op);
int em_tellsimp(em_object _pattern, em_object _replacement);
int em_tellsimpafter(em_object _pattern, em_object _replacement);
int em_clear_rules();

#ifdef __cplusplus
}
#endif

#endif // RULES_AND_PATTERNS_H
