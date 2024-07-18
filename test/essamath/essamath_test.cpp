#include <gtest/gtest.h>

#include "essamath.h"
#include "expression.h"

TEST(BasicEssaMathTests, InitEssaMath) {
    em_initmath();

    if(em_eval("integrate(1/(x+1), x)") == EM_RTNORM){
        std::cout << "SUCCESS!\n";
    }else{
        std::cout << "FAILURE!\n";
    }

    auto var =  em_getvar("labels");
    em_printf(var);
    char buf[256] = {0};
    em_tostring(var, buf, 256);
    std::cout << "\n" << buf << "\n";
    em_rellist(var);

    em_freemath();
    // Assert
    EXPECT_TRUE(true);
}
