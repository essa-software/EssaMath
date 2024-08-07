// 26_ctensor.h
#include "expression.h"
#ifndef CTENSOR_H
#define CTENSOR_H

#ifdef __cplusplus
extern "C" {
#endif

int em_csetup(void);
int em_cmetric(void);
int em_cmetricdis(void);
int em_ct_coordsys(em_object _coordinate_system);
int em_ct_coordsys_2(em_object _coordinate_system, em_object _extra_arg);
int em_init_ctensor(void);
int em_christof(em_object _dis);
int em_ricci(em_object _dis);
int em_uricci(em_object _dis);
int em_scurvature(void);
int em_einstein(em_object _dis);
int em_leinstein(em_object _dis);
int em_riemann(em_object _dis);
int em_lriemann(em_object _dis);
int em_uriemann(em_object _dis);
int em_rinvariant(void);
int em_weyl(em_object _dis);
int em_ctaylor(void);
int em_frame_bracket(em_object _fr, em_object _fri, em_object _diagframe);
int em_nptetrad(void);
int em_psi(em_object _dis);
int em_petrov(void);
int em_contortion(em_object _tr);
int em_nonmetricity(em_object _nm);
int em_ctransform(em_object _m);
int em_findde(em_object _a, em_object _n);
int em_cograd(void);
int em_contragrad(void);
int em_dscalar(void);
int em_checkdiv(void);
int em_cgeodesic(void);
int em_bdvac(void);
int em_invariant1(void);
int em_invariant2(void);
int em_bimetric(void);
int em_diagmatrixp(em_object _m, em_object _n);
int em_symmetricp(em_object _m, em_object _n);
int em_ntermst(em_object _f);
int em_cdisplay(void);
int em_deleten(em_object _l, em_object _n);

#ifdef __cplusplus
}
#endif

#endif // CTENSOR_H
