#include <functional>
#include <gtest/gtest.h>
#include <math.h>

#include "essamath.h"
#include "expression.h"

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
        auto e = em_createexpressiondouble(expr, 1, varlist, vars);
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

TEST(BasicEssaMathTests, InitEssaMath) {
    em_initmath();

    bool result = true;
    result &= test("integrate(cos(x)^2, x)", [](double x) -> double{return (sin(2*x)/2+x)/2;}, M_PI / 2.0, 3 * M_PI / 2.0, M_PI / 16.0);
    result &= test("diff(cos(x)^2, x)", [](double x) -> double{return -2*cos(x)*sin(x);}, M_PI / 2.0, 3 * M_PI / 2.0, M_PI / 16.0);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}
