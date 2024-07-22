// polylogarithm.h
#ifndef POLYLOGARITHM_H
#define POLYLOGARITHM_H
#ifdef __cplusplus
extern "C" {
#endif
#include <stdint.h>

/** returns clog(1 + z) with double precision */
double _Complex em_clog1p(double _Complex z);

double em_li2(double);
double _Complex em_cli2(double _Complex);

double em_li3(double);
double _Complex em_cli3(double _Complex);

double em_li4(double);
double _Complex em_cli4(double _Complex);

double _Complex em_li(int64_t n, double _Complex);

#ifdef __cplusplus
}
#endif
#endif