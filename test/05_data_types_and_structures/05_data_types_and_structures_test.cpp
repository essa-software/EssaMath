#include <complex>
#include <gtest/gtest.h>

#include "essamath.h"
#include "expression.h"
#include "05_data_types_and_structures.h"
#include "test_utils.hpp"

TEST(EssaMathTests, bfloatFunc) {
    em_initmath();

    bool result = true;
    // result &= test_numeric_function(em_bfloat, 123, em_createstring("123"));
    // result &= test_numeric_function(em_bfloat, 5.862068965517241e-1, em_createstring("17/29"));
    // result &= test_numeric_function(em_bfloat, 1.75, em_createstring("1.75"));
    // result &= test_numeric_function(em_bfloat, 2.718281828459045, em_createstring("%e"));
    // result &= test_numeric_function(em_bfloat, 5.772156649015329e-1, em_createstring("%gamma"));
    // result &= test_numeric_function(em_bfloat, 1.618033988749895, em_createstring("%phi"));
    // result &= test_numeric_function(em_bfloat, 3.141592653589793, em_createstring("%pi"));

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}
