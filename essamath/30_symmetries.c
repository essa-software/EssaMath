#include "30_symmetries.h"
#include "expression.h"

int em_comp2pui(em_object _n, em_object _l){
    return em_invoke("comp2pui", 2, _n, _l);
}

int em_ele2pui(em_object _m, em_object _l){
    return em_invoke("ele2pui", 2, _m, _l);
}

int em_ele2comp(em_object _m, em_object _l){
    return em_invoke("ele2comp", 2, _m, _l);
}

int em_elem(em_object _ele, em_object _sym, em_object _lvar){
    return em_invoke("elem", 3, _ele, _sym, _lvar);
}

int em_mon2schur(em_object _l){
    return em_invoke("mon2schur", 1, _l);
}

int em_multi_elem(em_object _l_elem, em_object _multi_pc, em_object _l_var){
    return em_invoke("multi_elem", 3, _l_elem, _multi_pc, _l_var);
}

int em_multi_pui(em_object _l_elem, em_object _multi_pc, em_object _l_var){
    return em_invoke("multi_pui", 3, _l_elem, _multi_pc, _l_var);
}

int em_pui(em_object _l, em_object _sym, em_object _lvar){
    return em_invoke("pui", 3, _l, _sym, _lvar);
}

int em_pui2comp(em_object _n, em_object _lpui){
    return em_invoke("pui2comp", 2, _n, _lpui);
}

int em_pui2ele(em_object _n, em_object _lpui){
    return em_invoke("pui2ele", 2, _n, _lpui);
}

int em_puireduc(em_object _n, em_object _lpui){
    return em_invoke("puireduc", 2, _n, _lpui);
}

int em_schur2comp(em_object _p, em_object _l_var){
    return em_invoke("schur2comp", 2, _p, _l_var);
}

int em_cont2part(em_object _pc, em_object _lvar){
    return em_invoke("cont2part", 2, _pc, _lvar);
}

int em_contract_2(em_object _psym, em_object _lvar){
    return em_invoke("contract", 2, _psym, _lvar);
}

int em_explose(em_object _pc, em_object _lvar){
    return em_invoke("explose", 2, _pc, _lvar);
}

int em_part2cont(em_object _ppart, em_object _lvar){
    return em_invoke("part2cont", 2, _ppart, _lvar);
}

int em_partpol(em_object _psym, em_object _lvar){
    return em_invoke("partpol", 2, _psym, _lvar);
}

int em_tcontract(em_object _psol, em_object _lvar){
    return em_invoke("tcontract", 2, _psol, _lvar);
}

int em_tpartpol(em_object _psol, em_object _lvar){
    return em_invoke("tpartpol", 2, _psol, _lvar);
}

int em_direct(em_object _p, em_object _y, em_object _f, em_object _lvar){
    return em_invoke("direct", 4, _p, _y, _f, _lvar);
}

int em_multi_orbit(em_object _p, em_object _lvar){
    return em_invoke("multi_orbit", 2, _p, _lvar);
}

int em_multsym(em_object _ppart_1, em_object _ppart_2, em_object _n){
    return em_invoke("multsym", 3, _ppart_1, _ppart_2, _n);
}

int em_orbit(em_object _p, em_object _lvar){
    return em_invoke("orbit", 2, _p, _lvar);
}

int em_pui_direct(em_object _orbite, em_object _lvar, em_object _d){
    return em_invoke("pui_direct", 3, _orbite, _lvar, _d);
}

int em_kostka(em_object _part_1, em_object _part_2){
    return em_invoke("kostka", 2, _part_1, _part_2);
}

int em_lgtreillis(em_object _n, em_object _m){
    return em_invoke("lgtreillis", 2, _n, _m);
}

int em_ltreillis(em_object _n, em_object _m){
    return em_invoke("ltreillis", 2, _n, _m);
}

int em_treillis(em_object _n){
    return em_invoke("treillis", 1, _n);
}

int em_treinat(em_object _part){
    return em_invoke("treinat", 1, _part);
}

int em_ele2polynome(em_object _l, em_object _z){
    return em_invoke("ele2polynome", 2, _l, _z);
}

int em_polynome2ele(em_object _p, em_object _x){
    return em_invoke("polynome2ele", 2, _p, _x);
}

int em_prodrac(em_object _l, em_object _k){
    return em_invoke("prodrac", 2, _l, _k);
}

int em_pui2polynome(em_object _x, em_object _lpui){
    return em_invoke("pui2polynome", 2, _x, _lpui);
}

int em_somrac(em_object _l, em_object _k){
    return em_invoke("somrac", 2, _l, _k);
}

int em_resolvante(em_object _p, em_object _x, em_object _f, em_object _xd){
    return em_invoke("resolvante", 4, _p, _x, _f, _xd);
}

int em_resolvante_alternee1(em_object _p, em_object _x){
    return em_invoke("resolvante_alternee1", 2, _p, _x);
}

int em_resolvante_bipartite(em_object _p, em_object _x){
    return em_invoke("resolvante_bipartite", 2, _p, _x);
}

int em_resolvante_diedrale(em_object _p, em_object _x){
    return em_invoke("resolvante_diedrale", 2, _p, _x);
}

int em_resolvante_klein(em_object _p, em_object _x){
    return em_invoke("resolvante_klein", 2, _p, _x);
}

int em_resolvante_klein3(em_object _p, em_object _x){
    return em_invoke("resolvante_klein3", 2, _p, _x);
}

int em_resolvante_produit_sym(em_object _p, em_object _x){
    return em_invoke("resolvante_produit_sym", 2, _p, _x);
}

int em_resolvante_unitaire(em_object _p, em_object _q, em_object _x){
    return em_invoke("resolvante_unitaire", 3, _p, _q, _x);
}

int em_resolvante_vierer(em_object _p, em_object _x){
    return em_invoke("resolvante_vierer", 2, _p, _x);
}

int em_multinomial(em_object _r, em_object _part){
    return em_invoke("multinomial", 2, _r, _part);
}

int em_permut(em_object _l){
    return em_invoke("permut", 1, _l);
}

int em_todd_coxeter(em_object _relations){
    return em_invoke("todd_coxeter", 1, _relations);
}

int em_todd_coxeter_2(em_object _relations, em_object subgroup){
    return em_invoke("todd_coxeter", 2, _relations, subgroup);
}

