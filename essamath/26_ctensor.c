#include "26_ctensor.h"
#include "expression.h"

int em_csetup(){
    return em_invoke("csetup", 0);
}

int em_cmetric(){
    return em_invoke("cmetric", 0);
}

int em_cmetricdis(){
    return em_invoke("cmetricdis", 0);
}

int em_ct_coordsys(em_object _coordinate_system){
    return em_invoke("ct_coordsys", 1, _coordinate_system);
}

int em_ct_coordsys_2(em_object _coordinate_system, em_object _extra_arg){
    return em_invoke("ct_coordsys", 2, _coordinate_system, _extra_arg);
}

int em_init_ctensor(){
    return em_invoke("init_ctensor", 0);
}

int em_christof(em_object _dis){
    return em_invoke("christof", 1, _dis);
}

int em_ricci(em_object _dis){
    return em_invoke("ricci", 1, _dis);
}

int em_uricci(em_object _dis){
    return em_invoke("uricci", 1, _dis);
}

int em_scurvature(){
    return em_invoke("scurvature", 0);
}

int em_einstein(em_object _dis){
    return em_invoke("einstein", 1, _dis);
}

int em_leinstein(em_object _dis){
    return em_invoke("leinstein", 1, _dis);
}

int em_riemann(em_object _dis){
    return em_invoke("riemann", 1, _dis);
}

int em_lriemann(em_object _dis){
    return em_invoke("lriemann", 1, _dis);
}

int em_uriemann(em_object _dis){
    return em_invoke("uriemann", 1, _dis);
}

int em_rinvariant(){
    return em_invoke("rinvariant", 0);
}

int em_weyl(em_object _dis){
    return em_invoke("weyl", 1, _dis);
}

int em_ctaylor(){
    return em_invoke("ctaylor", 0);
}

int em_frame_bracket(em_object _fr, em_object _fri, em_object _diagframe){
    return em_invoke("frame_bracket", 3, _fr, _fri, _diagframe);
}

int em_nptetrad(){
    return em_invoke("nptetrad", 0);
}

int em_psi(em_object _dis){
    return em_invoke("psi", 1, _dis);
}

int em_petrov(){
    return em_invoke("petrov", 0);
}

int em_contortion(em_object _tr){
    return em_invoke("contortion", 1, _tr);
}

int em_nonmetricity(em_object _nm){
    return em_invoke("nonmetricity", 1, _nm);
}

int em_ctransform(em_object _M){
    return em_invoke("ctransform", 1, _M);
}

int em_findde(em_object _A, em_object _n){
    return em_invoke("findde", 2, _A, _n);
}

int em_cograd(){
    return em_invoke("cograd", 0);
}

int em_contragrad(){
    return em_invoke("contragrad", 0);
}

int em_dscalar(){
    return em_invoke("dscalar", 0);
}

int em_checkdiv(){
    return em_invoke("checkdiv", 0);
}

int em_cgeodesic(){
    return em_invoke("cgeodesic", 0);
}

int em_bdvac(){
    return em_invoke("bdvac", 0);
}

int em_invariant1(){
    return em_invoke("invariant1", 0);
}

int em_invariant2(){
    return em_invoke("invariant2", 0);
}

int em_bimetric(){
    return em_invoke("bimetric", 0);
}

int em_diagmatrixp(em_object _M, em_object _n){
    return em_invoke("diagmatrixp", 2, _M, _n);
}

int em_symmetricp(em_object _M, em_object _n){
    return em_invoke("symmetricp", 2, _M, _n);
}

int em_ntermst(em_object _f){
    return em_invoke("ntermst", 1, _f);
}

int em_cdisplay(){
    return em_invoke("cdisplay", 0);
}

int em_deleten(em_object _L, em_object _n){
    return em_invoke("deleten", 2, _L, _n);
}
