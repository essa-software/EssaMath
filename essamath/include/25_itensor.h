// 25_itensor.h
#include "expression.h"
#ifndef ITENSOR_H
#define ITENSOR_H

#ifdef __cplusplus
extern "C" {
#endif

int em_dispcon(size_t n, ...);
int em_dispconall(void);
int em_entertensor(em_object _name);
int em_changename(em_object _old, em_object _new, em_object _expr);
int em_listoftens(em_object _expr);
int em_indices(em_object _expr);
int em_rename(em_object _expr);
int em_rename_2(em_object _expr, em_object _count);
int em_defcon(em_object _tensor1);
int em_defcon_2(em_object _tensor1, em_object _tensor2, em_object _tensor3);
int em_remcon(size_t n, ...);
int em_remconall(void);
int em_contract(em_object _expr);
int em_indexed_tensor(em_object _tensor);
int em_components(em_object _tensor, em_object _expr);
int em_remcomps(em_object _tensor);
int em_showcomps(em_object _tensor);
int em_idummy(void);
int em_kdelta(em_object _l1, em_object _l2);
int em_kdels(em_object _l1, em_object _l2);
int em_levi_civita(em_object _l);
int em_lc2kdt(em_object _expr);
int em_lc_u(em_object _expr);
int em_canten(em_object _expr);
int em_concan(em_object _expr);
int em_decsym(em_object _tensor, em_object _m, em_object _n, em_object _cov, em_object _contr);
int em_remsym(em_object _tensor, em_object _m, em_object _n);
int em_dispsym(em_object _tensor, em_object _m, em_object _n);
int em_canform(em_object _expr);
int em_canform_2(em_object _expr, em_object _rename);
int em_idiff(em_object _expr, size_t n, ...);
int em_liediff(em_object _v ,em_object _ten);
int em_rediff(em_object _ten);
int em_undiff(em_object _expr);
int em_evundiff(em_object _expr);
int em_flush(em_object _expr, size_t n, ...);
int em_flushd(em_object _expr, size_t n, ...);
int em_flushnd(em_object _expr, em_object _tensor, em_object _n);
int em_coord(size_t n, ...);
int em_remcoord(size_t n, ...);
int em_remcoordall(void);
int em_makebox(em_object _expr, em_object _g);
int em_conmetderiv(em_object _expr, em_object _tensor);
int em_simpmetderiv(em_object _expr);
int em_simpmetderiv_2(em_object _expr, em_object _options);
int em_flush1deriv(em_object _expr, em_object _tensor);
int em_imetric(em_object _g);
int em_idim(em_object _n);
int em_ichr1(size_t n, ...);
int em_ichr2(size_t n, ...);
int em_icurvature(size_t n, ...);
int em_covdiff(em_object _expr, size_t n, ...);
int em_lorentz_gauge(em_object _expr);
int em_igeodesic_coords(em_object _expr, em_object _name);
int em_iframes(void);
int em_extdiff(em_object _expr, em_object _i);
int em_hodge(em_object _expr);
int em_tentex(em_object _expr);
int em_ic_convert(em_object _eqn);

#ifdef __cplusplus
}
#endif

#endif // ITENSOR_H
