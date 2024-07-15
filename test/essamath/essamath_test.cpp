#include <gtest/gtest.h>

#include "essamath.h"

TEST(BasicEssaMathTests, InitEssaMath) {
    em_init_math();
    em_free_math();

    // Assert
    EXPECT_TRUE(true);
}
