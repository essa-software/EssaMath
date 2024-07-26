// 31_runtime_environment.h
#include "expression.h"
#ifndef RUNTIME_ENVIRONMENT_H
#define RUNTIME_ENVIRONMENT_H

#ifdef __cplusplus
extern "C" {
#endif

int em_sstatus(em_object _keyword, em_object _item);
int em_status(em_object _feature);
int em_status_2(em_object _feature, em_object _item);
int em_system(em_object _command);
int em_time(size_t n, ...);
int em_timedate();
int em_timedate_2(em_object _T);
int em_timedate_3(em_object _T, em_object _tz_offset);
int em_parse_timedate(em_object _S);
int em_encode_time(em_object _year, em_object _month, em_object _day, em_object _hours, em_object _minutes, em_object _seconds);
int em_encode_time_2(em_object _year, em_object _month, em_object _day, em_object _hours, em_object _minutes, em_object _seconds, em_object _tz_offset);
int em_decode_time(em_object _T);
int em_decode_time_2(em_object _T, em_object _tz_offset);
int em_absolute_real_time();
int em_elapsed_real_time();
int em_elapsed_run_time();
int em_gensym();
int em_gensym_2(em_object _x);
int em_remvalue(size_t n, ...);
int em_remvalueall();
int em_rncombine(em_object _expr);
int em_setup_autoload(em_object _filename, size_t n, ...);

#ifdef __cplusplus
}
#endif

#endif // RUNTIME_ENVIRONMENT_H
