// 05_data_types_and_structures.h
#include "expression.h"
#ifndef DATA_TYPES_AND_STRUCTURES_H
#define DATA_TYPES_AND_STRUCTURES_H

#ifdef __cplusplus
extern "C" {
#endif

// 5.1 Numbers
int em_bfloat(em_object _expr);
int em_bfloatp(em_object _expr);
int em_bigfloat_bits();
int em_bigfloat_eps();
int em_decode_float(em_object _f);
int em_evenp(em_object _expr);
int em_float(em_object _expr);
int em_float_bits();
int em_float_eps();
int em_float_precision(em_object _f);
int em_floatnump(em_object _expr);
int em_integerp(em_object _expr);
int em_integer_decode_float(em_object _f);
int em_is_power_of_two(em_object _n);
int em_nonnegintegerp(em_object _n);
int em_numberp(em_object _expr);
int em_numerval(size_t n, ...);
int em_oddp(em_object _expr);
int em_rationalize(em_object _expr);
int em_ratnump(em_object _expr);
int em_scale_float(em_object _f, em_object _n);
int em_unit_in_last_plase(em_object _n);

// 5.2 Strings
int em_concat(size_t n, ...);
int em_sconcat(size_t n, ...);
int em_string(em_object _expr);

// 5.4 Lists
int em_append(size_t n, ...);
int em_assoc(em_object _key, em_object _e, em_object _default);
int em_assoc_2(em_object _key, em_object _e);
int em_cons(em_object _expr1, em_object _expr2);
int em_copylist(em_object _list);
int em_create_list(em_object _form, size_t n, ...);
int em_delete(em_object _expr1, em_object _expr2);
int em_delete_2(em_object _expr1, em_object _expr2, em_object _n);
int em_eighth(em_object _expr);
int em_endcons(em_object _expr1, em_object _expr2);
int em_fifth(em_object _expr);
int em_first(em_object _expr);
int em_firstn(em_object _expr, em_object _n);
int em_fourth(em_object _expr);
int em_join(em_object _l, em_object _m);
int em_last(em_object _expr);
int em_lastn(em_object _expr, em_object _n);
int em_length(em_object _expr);
int em_listp(em_object _expr);
int em_lreduce(em_object _F, em_object _s);
int em_lreduce_2(em_object _F, em_object _s, em_object _s_0);
int em_makelist();
int em_makelist_2(em_object _expr, em_object _n);
int em_makelist_3(em_object _expr, em_object _i, em_object _i_max);
int em_makelist_4(em_object _expr, em_object _i, em_object _i_0, em_object _i_max);
int em_makelist_5(em_object _expr, em_object _i, em_object _i_0, em_object _i_max, em_object _step);
int em_member(em_object _expr1, em_object _expr2);
int em_ninth(em_object _expr);
int em_pop(em_object _expr);
int em_push(em_object _item, em_object _list);
int em_rest(em_object _expr, em_object _n);
int em_rest_2(em_object _expr);
int em_rreduce(em_object _F, em_object _s);
int em_rreduce_2(em_object _F, em_object _s, em_object _s_np1);
int em_second(em_object _expr);
int em_seventh(em_object _expr);
int em_sixth(em_object _expr);
int em_sort(em_object _L, em_object _P);
int em_sort_2(em_object _L);
int em_sublist(em_object _list, em_object _p);
int em_sublist_indices(em_object _L, em_object _P);
int em_tenth(em_object _expr);
int em_tree_reduce(em_object _F, em_object _s);
int em_tree_reduce_2(em_object _F, em_object _s, em_object _s_0);
int em_unique(em_object _L);
int em_xreduce(em_object _F, em_object _s);
int em_xreduce_2(em_object _F, em_object _s, em_object _s_0);

// 5.5 Arrays
int em_array(em_object _names, size_t n, ...);
int em_array_2(em_object _name, em_object _type, size_t n, ...);
int em_arrayapply(em_object _A, em_object _i);
int em_arrayinfo(em_object _A);
int em_arraymake(em_object _A, em_object _i);
int em_arraysetapply(em_object _A, em_object _i, em_object _x);
int em_fillarray(em_object _A, em_object _B);
int em_listarray(em_object _A);
int em_make_array(em_object _type, size_t n, ...);
int em_rearray(em_object _A, size_t n, ...);
int em_remarray(size_t n, ...);
int em_remarray_2();
int em_subvar(em_object _x, em_object _i);
int em_subvarp(em_object _expr);

// 5.6 Structures
int em_defstruct(em_object _S);
int em_new(em_object _S);

#ifdef __cplusplus
}
#endif

#endif // DATA_TYPES_AND_STRUCTURES_H
