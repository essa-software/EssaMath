#include <gtest/gtest.h>
#include <math.h>

#include "essamath.h"
#include "expression.h"

void test(const char* _expr){
    if(em_eval(_expr) == EM_RTNORM){
        auto expr = em_getexpr(em_getlastoutput());
        // em_printf(expr);
        char buf[256] = {0};
        em_tostring(expr, buf, 256);
        std::cout << buf << "\n";
        const char* varlist[] = {"x"};

        double x = M_PI / 2.0;
        double* vars[1];
        vars[0] = &x;
        auto e = em_createexpressiondouble(expr, 1, varlist, vars);

        // std::cout << em_calculateexpr(e) << "\n";
        em_rellist(expr);
    }
}

TEST(BasicEssaMathTests, InitEssaMath) {
    em_initmath();

    test("integrate(cos(x)^2, x)");
    test("diff(cos(x)^2, x)");

    em_freemath();
    // Assert
    EXPECT_TRUE(true);
}
