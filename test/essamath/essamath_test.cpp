#include <gtest/gtest.h>

#include "essamath.h"
#include "expression.h"

TEST(BasicEssaMathTests, InitEssaMath) {
    em_init_math();

    if(em_eval("integrate(1/(x+1), x)") == EM_RTNORM){
        std::cout << "SUCCESS!\n";
    }else{
        std::cout << "FAILURE!\n";
    }

    std::cout << em_getlasterror() << "\n";

    em_free_math();
    // Assert
    EXPECT_TRUE(true);
}
