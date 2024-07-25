// 26_ctensor.h
#include "expression.h"
#ifndef CTENSOR_H
#define CTENSOR_H

#ifdef __cplusplus
extern "C" {
#endif

int em_csetup();
int em_cmetric();
int em_cmetricdis();
int em_ct_coordsys(em_object _coordinate_system);
int em_ct_coordsys_2(em_object _coordinate_system, em_object _extra_arg);
int em_init_ctensor();
int em_christof(em_object _dis);
int em_ricci(em_object _dis);
int em_uricci(em_object _dis);
int em_scurvature();
int em_einstein(em_object _dis);
int em_leinstein(em_object _dis);
int em_riemann(em_object _dis);
int em_lriemann(em_object _dis);
int em_uriemann(em_object _dis);
int em_rinvariant();
int em_weyl(em_object _dis);
int em_ctaylor();
int em_frame_bracket(em_object _fr, em_object _fri, em_object _diagframe);
int em_nptetrad();
int em_psi(em_object _dis);
int em_petrov();
int em_contortion(em_object _tr);
int em_nonmetricity(em_object _nm);
int em_ctransform(em_object _M);
int em_findde(em_object _A, em_object _n);
int em_cograd();
int em_contragrad();
int em_dscalar();
int em_checkdiv();
int em_cgeodesic();
int em_bdvac();
int em_invariant1();
int em_invariant2();
int em_bimetric();
int em_diagmatrixp(em_object _M, em_object _n);
int em_symmetricp(em_object _M, em_object _n);
int em_ntermst(em_object _f);
int em_cdisplay();
int em_deleten(em_object _L, em_object _n);

#ifdef __cplusplus
}
#endif

#endif // CTENSOR_H
