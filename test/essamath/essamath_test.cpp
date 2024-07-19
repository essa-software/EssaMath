#include <gtest/gtest.h>

#include "essamath.h"
#include "expression.h"

void test(const char* _expr){
    if(em_eval(_expr) == EM_RTNORM){
        auto expr = em_getexpr(em_getlastoutput());
        // em_printf(expr);
        char buf[256] = {0};
        em_tostring(expr, buf, 256);
        std::cout << buf << "\n";
        em_rellist(expr);
    }
}

TEST(BasicEssaMathTests, InitEssaMath) {
    em_initmath();

    test("integrate(cos(x)^2, x)");
    test("diff(cos(x)^2, x)");
    test("laplace(cos(x)^2, x)");

    em_freemath();
    // Assert
    EXPECT_TRUE(true);
}
