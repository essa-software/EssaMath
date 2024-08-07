// 35_sets.h
#include "expression.h"
#ifndef SETS_H
#define SETS_H

#ifdef __cplusplus
extern "C" {
#endif

int em_adjoin(em_object _x, em_object _a);
int em_belln(em_object _n);
int em_cardinality(em_object _a);
int em_cartesian_product(size_t n, ...);
int em_cartesian_product_list(size_t n, ...);
int em_disjoin(em_object _x, em_object _a);
int em_disjointp(em_object _x, em_object _a);
int em_divisors(em_object _n);
int em_elementp(em_object _x, em_object _a);
int em_emptyp(em_object _a);
int em_equiv_classes(em_object _s, em_object _f);
int em_every(em_object _f, em_object _s);
int em_every_2(em_object _f, size_t n, ...);
int em_equiv_classesmax(em_object _s, em_object _f);
int em_equiv_classesmin(em_object _s, em_object _f);
int em_flatten(em_object _expr);
int em_full_listify(em_object _a);
int em_fullsetify(em_object _a);
int em_identity(em_object _x);
int em_integer_partitions(em_object _n);
int em_integer_partitions_2(em_object _n, em_object _len);
int em_intersect(size_t n, ...);
int em_intersection(size_t n, ...);
int em_kron_delta(size_t n, ...);
int em_listify(em_object _a);
int em_makeset(em_object _expr, em_object _x, em_object _s);
int em_moebius(em_object _n);
int em_multinomial_coeff(void);
int em_multinomial_coeff_2(size_t n, ...);
int em_num_distinct_partitions(em_object _n);
int em_num_distinct_partitions_2(em_object _n, em_object _list);
int em_num_partitions(em_object _n);
int em_num_partitions_2(em_object _n, em_object _list);
int em_partition_set(em_object _a, em_object _f);
int em_permutations(em_object _a);
int em_powerset(em_object _a);
int em_powerset_2(em_object _a, em_object _n);
int em_random_permutation(em_object _a);
int em_setdifference(em_object _a, em_object _b);
int em_setequalp(em_object _a, em_object _b);
int em_setify(em_object _a);
int em_setp(em_object _a);
int em_set_partitions(em_object _a);
int em_set_partitions_2(em_object _a, em_object _n);
int em_some(em_object _f, em_object _a);
int em_some_2(em_object _f, size_t n, ...);
int em_stirling1(em_object _n, em_object _m);
int em_stirling2(em_object _n, em_object _m);
int em_subset(em_object _a, em_object _f);
int em_subsetp(em_object _a, em_object _b);
int em_symmdifference(size_t n, ...);
int em_union(size_t n, ...);

#ifdef __cplusplus
}
#endif

#endif // SETS_H
