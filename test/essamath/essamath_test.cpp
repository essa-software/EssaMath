
#include <functional>
#include <gtest/gtest.h>
#include <math.h>

#include "essamath.h"
#include "expression.h"
#include <complex.h>
#undef complex

bool test(const char* _expr, std::function<double(double)> const& _foo, double _lbound, double _rbound, double _step){
    if(em_eval(_expr) == EM_RTNORM){
        auto expr = em_getexpr(em_getlastoutput());
        // em_printf(expr);
        char buf[256] = {0};
        em_tostring(expr, buf, 256);
        std::cout << buf << "\n";
        const char* varlist[] = {"x"};

        double x = 0;
        double* vars[1];
        vars[0] = &x;
        auto e = em_createexpression(expr, 1, varlist, vars);
        bool result = true;

        std::cout << "Maxima:\t\tMath:\n";
        for(x = _lbound; x <= _rbound; x += _step){
            double lhs = em_calculateexpr(e);
            double rhs = _foo(x);
            std::cout << lhs << "\t\t" << rhs << "\n";
            result &= EM_NEAREQUAL(lhs, rhs, 1e-3);
        }


        em_relexpr(e);
        em_rellist(expr);
        
        return result;
    }
    return false;
}

bool testcomplex(const char* _expr, std::function<std::complex<double>(std::complex<double>)> const& _foo, std::complex<double> x){
    if(em_eval(_expr) == EM_RTNORM){
        auto expr = em_getexpr(em_getlastoutput());
        // em_printf(expr);
        char buf[256] = {0};
        em_tostring(expr, buf, 256);
        std::cout << buf << "\n";
        const char* varlist[] = {"x"};

        _Complex double* vars[1];
        vars[0] = reinterpret_cast<_Complex double*>(&x);
        auto e = em_createcomplexexpression(expr, 1, varlist, vars);
        bool result = true;
        
        std::complex<double> lhs = em_calculatecomplexexpr(e);
        std::complex<double> rhs = _foo(x);
        result &= (lhs == rhs);

        std::cout << "Complex: " << lhs << "\n";

        em_relcomplexexpr(e);
        em_rellist(expr);
        
        return result;
    }
    return false;
}

TEST(BasicEssaMathTests, InitEssaMath) {
    em_initmath();

    bool result = true;
    result &= test("integrate(cos(x)^2, x)", [](double x) -> double{return (sin(2*x)/2+x)/2;}, M_PI / 2.0, 3 * M_PI / 2.0, M_PI / 16.0);
    result &= test("diff(cos(x)^2, x)", [](double x) -> double{return -2*cos(x)*sin(x);}, M_PI / 2.0, 3 * M_PI / 2.0, M_PI / 16.0);
    result &= testcomplex("integrate(cos(x)^2, x)", [](std::complex<double> x) -> std::complex<double>{return (std::sin(2.0*x)/2.0+x)/2.0;}, std::complex<double>(M_PI / 2.0, -M_PI));
    result &= testcomplex("diff(cos(x)^2, x)", [](std::complex<double> x) -> std::complex<double>{return -2.0*std::cos(x)*std::sin(x);}, M_PI / 2.0);
    result &= test("asinh(x)^2", [](double x) -> double{return pow(asinh(x), 2);}, -1.0, 0.0, 0.1);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}
