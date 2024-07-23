#include <complex>
#include <gtest/gtest.h>

#include "essamath.h"
#include "math_utils.h"
#include "test_utils.hpp"

TEST(EssaMathTestsDataTypesAndStructures, AdditionAndSubtraction) {
    em_initmath();

    bool result = true;
    result &= test_real(
        "x+y+%pi", 
        [](std::vector<double> const& _vars) 
            -> double{
                auto _x = _vars[0];
                auto _y = _vars[1];

                return _x+_y + M_PI;
            }, 
        std::vector<TestOptionsReal>{
        TestOptionsReal{._varname = "x", ._lbound = -2*M_PI, ._rbound = 2*M_PI, ._step = M_PI / 8},
        TestOptionsReal{._varname = "y", ._lbound = -2.0, ._rbound = 5.0, ._step = 1}
    });

    result &= test_real(
            "a+b-c+2.5-%e", 
            [](std::vector<double> const& _vars) 
                -> double{
                    auto _a = _vars[0];
                    auto _b = _vars[1];
                    auto _c = _vars[2];

                    return _a+_b-_c + 2.5 - M_E;
                }, 
        std::vector<TestOptionsReal>{
        TestOptionsReal{._varname = "a", ._lbound = -10.0, ._rbound = 10.0, ._step = 5.0},
        TestOptionsReal{._varname = "b", ._lbound = -7.0, ._rbound = 5.0, ._step = 5.0},
        TestOptionsReal{._varname = "c", ._lbound = 5.0, ._rbound = 15.0, ._step = 5.0}
    });

    result &= test_complex(
        "d+%i*e+%i-2*%pi", 
        [&](std::vector<std::complex<double>> const& _vars) 
            -> std::complex<double>{
                auto _i = std::complex<double>(0, 1);
                auto _d = _vars[0];
                auto _e = _vars[1];
                
                return _d+_i*_e+_i-2*M_PI;
            }, 
        std::vector<TestOptionsComplex>{
        TestOptionsComplex{._varname = "d", ._value = std::complex<double>(1, 2), ._step = std::complex<double>(M_PI / 8, -2 * M_E), ._count = 10},
        TestOptionsComplex{._varname = "e", ._value = std::complex<double>(-3, -4), ._step = std::complex<double>(1, -M_E), ._count = 10},
    });

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}
