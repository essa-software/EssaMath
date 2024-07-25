// 30_symmetries.h
#include "expression.h"
#ifndef SYMMETRIES_H
#define SYMMETRIES_H

#ifdef __cplusplus
extern "C" {
#endif

int em_comp2pui(em_object _n, em_object _L);
int em_ele2pui(em_object _m, em_object _L);
int em_ele2comp(em_object _m, em_object _L);
int em_elem(em_object _ele, em_object _sym, em_object _lvar);
int em_mon2schur(em_object _L);
int em_multi_elem(em_object _l_elem, em_object _multi_pc, em_object _l_var);
int em_multi_pui(em_object _l_elem, em_object _multi_pc, em_object _l_var);
int em_pui(em_object _L, em_object _sym, em_object _lvar);
int em_pui2comp(em_object _n, em_object _lpui);
int em_pui2ele(em_object _n, em_object _lpui);
int em_puireduc(em_object _n, em_object _lpui);
int em_schur2comp(em_object _P, em_object _l_var);
int em_cont2part(em_object _pc, em_object _lvar);
int em_contract_2(em_object _psym, em_object _lvar);
int em_explose(em_object _pc, em_object _lvar);
int em_part2cont(em_object _ppart, em_object _lvar);
int em_partpol(em_object _psym, em_object _lvar);
int em_tcontract(em_object _psol, em_object _lvar);
int em_tpartpol(em_object _psol, em_object _lvar);
int em_direct(em_object _p, em_object _y, em_object _f, em_object _lvar);
int em_multi_orbit(em_object _P, em_object _lvar);
int em_multsym(em_object _ppart_1, em_object _ppart_2, em_object _n);
int em_orbit(em_object _P, em_object _lvar);
int em_pui_direct(em_object _orbite, em_object _lvar, em_object _d);
int em_kostka(em_object _part_1, em_object _part_2);
int em_lgtreillis(em_object _n, em_object _m);
int em_ltreillis(em_object _n, em_object _m);
int em_treillis(em_object _n);
int em_treinat(em_object _part);
int em_ele2polynome(em_object _L, em_object _z);
int em_polynome2ele(em_object _P, em_object _x);
int em_prodrac(em_object _L, em_object _k);
int em_pui2polynome(em_object _x, em_object _lpui);
int em_somrac(em_object _L, em_object _k);
int em_resolvante(em_object _P, em_object _x, em_object _f, em_object _xd);
int em_resolvante_alternee1(em_object _P, em_object _x);
int em_resolvante_bipartite(em_object _P, em_object _x);
int em_resolvante_diedrale(em_object _P, em_object _x);
int em_resolvante_klein(em_object _P, em_object _x);
int em_resolvante_klein3(em_object _P, em_object _x);
int em_resolvante_produit_sym(em_object _P, em_object _x);
int em_resolvante_unitaire(em_object _P, em_object _Q, em_object _x);
int em_resolvante_vierer(em_object _P, em_object _x);
int em_multinomial(em_object _r, em_object _part);
int em_permut(em_object _L);

#ifdef __cplusplus
}
#endif

#endif // SYMMETRIES_H
