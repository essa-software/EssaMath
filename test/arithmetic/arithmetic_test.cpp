#include <complex>
#include <gtest/gtest.h>

#include "essamath.h"
#include "expression.h"
#include "math_utils.h"
#include "test_utils.hpp"

TEST(EssaMathTestsArithmetic, AdditionAndSubtraction1) {
    em_initmath();

    bool result = true;
    result &= test_real(
        "x+y+%pi", 
        [](std::vector<em_val> const& _vars) 
            -> double{
                auto _x = em_getdouble(_vars[0]);
                auto _y = em_getdouble(_vars[1]);

                return _x+_y + M_PI;
            }, 
        std::vector<TestOptionsReal>{
        TestOptionsReal{._varname = "x", ._lbound = -2*M_PI, ._rbound = 2*M_PI, ._step = M_PI / 8},
        TestOptionsReal{._varname = "y", ._lbound = -2.0, ._rbound = 5.0, ._step = 1}
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmetic, AdditionAndSubtraction2) {
    em_initmath();

    bool result = true;
    result &= test_real(
            "a+b-c+2.5-%e", 
            [](std::vector<em_val> const& _vars) 
                -> double{
                    auto _a = em_getdouble(_vars[0]);
                    auto _b = em_getdouble(_vars[1]);
                    auto _c = em_getdouble(_vars[2]);

                    return _a+_b-_c + 2.5 - M_E;
                }, 
        std::vector<TestOptionsReal>{
        TestOptionsReal{._varname = "a", ._lbound = -10.0, ._rbound = 10.0, ._step = 5.0},
        TestOptionsReal{._varname = "b", ._lbound = -7.0, ._rbound = 5.0, ._step = 5.0},
        TestOptionsReal{._varname = "c", ._lbound = 5.0, ._rbound = 15.0, ._step = 5.0}
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmeticComplex, AdditionAndSubtraction1) {
    em_initmath();

    bool result = true;
    result &= test_complex(
        "d+%i*e+%i-2*%pi", 
        [&](std::vector<em_val> const& _vars) 
            -> std::complex<double>{
                std::complex<double> _i = std::complex<double>(0, 1);
                std::complex<double> _d = em_getcomplex(_vars[0]);
                std::complex<double> _e = em_getcomplex(_vars[1]);
                
                return _d+_i*_e+_i-2*M_PI;
            }, 
        std::vector<TestOptionsComplex>{
        TestOptionsComplex{._varname = "d", ._value = std::complex<double>(1, 2), ._step = std::complex<double>(M_PI / 8, -2 * M_E), ._count = 10},
        TestOptionsComplex{._varname = "e", ._value = std::complex<double>(-3, -4), ._step = std::complex<double>(1, -M_E), ._count = 10},
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmeticComplex, AdditionAndSubtraction2) {
    em_initmath();

    bool result = true;
    result &= test_complex(
        "f+g*%i-%pi-2-f*%i+g", 
        [&](std::vector<em_val> const& _vars) 
            -> std::complex<double>{
                std::complex<double> _i = std::complex<double>(0, 1);
                std::complex<double> _f = em_getcomplex(_vars[0]);
                std::complex<double> _g = em_getcomplex(_vars[1]);
                
                return _f+_g*_i-M_PI-2.0-_f*_i+_g;
            }, 
        std::vector<TestOptionsComplex>{
        TestOptionsComplex{._varname = "f", ._value = std::complex<double>(M_PI, 3), ._step = std::complex<double>(M_PI / 8, -2), ._count = 5},
        TestOptionsComplex{._varname = "g", ._value = std::complex<double>(-2, -M_E), ._step = std::complex<double>(1, -M_E / 2.0), ._count = 4},
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmetic, MultiplicationAndDivision1) {
    em_initmath();

    bool result = true;
    result &= test_real(
        "x*y+x*%pi-y*%e/x", 
        [](std::vector<em_val> const& _vars) 
            -> double{
                auto _x = em_getdouble(_vars[0]);
                auto _y = em_getdouble(_vars[1]);

                return _x*_y+_x*M_PI-_y*M_E/_x;
            }, 
        std::vector<TestOptionsReal>{
        TestOptionsReal{._varname = "x", ._lbound = -2*M_PI, ._rbound = 2*M_PI, ._step = M_PI / 8},
        TestOptionsReal{._varname = "y", ._lbound = -2.0, ._rbound = 5.0, ._step = 1}
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmetic, MultiplicationAndDivision2) {
    em_initmath();

    bool result = true;
    result &= test_real(
            "(a+b-2)*c+3*a*b-4*c+%phi*b", 
            [](std::vector<em_val> const& _vars) 
                -> double{
                    auto _a = em_getdouble(_vars[0]);
                    auto _b = em_getdouble(_vars[1]);
                    auto _c = em_getdouble(_vars[2]);

                    return (_a+_b-2)*_c+3*_a*_b-4*_c+em_getdouble(em_numeric_phi())*_b;
                }, 
        std::vector<TestOptionsReal>{
        TestOptionsReal{._varname = "a", ._lbound = -10.0, ._rbound = 10.0, ._step = 5.0},
        TestOptionsReal{._varname = "b", ._lbound = -7.0, ._rbound = 5.0, ._step = 5.0},
        TestOptionsReal{._varname = "c", ._lbound = 5.0, ._rbound = 15.0, ._step = 5.0}
    });

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmeticComplex, MultiplicationAndDivision1) {
    em_initmath();

    bool result = true;
    result &= test_complex(
        "(d+e*%i-1/2)*2*d*%i-%e*e + %pi*%i", 
        [&](std::vector<em_val> const& _vars) 
            -> std::complex<double>{
                std::complex<double> _i = std::complex<double>(0, 1);
                std::complex<double> _d = em_getcomplex(_vars[0]);
                std::complex<double> _e = em_getcomplex(_vars[1]);
                
                return (_d+_e*_i-1.0/2.0)*2.0*_d*_i-M_E*_e + M_PI*_i;
            }, 
        std::vector<TestOptionsComplex>{
        TestOptionsComplex{._varname = "d", ._value = std::complex<double>(1, 2), ._step = std::complex<double>(M_PI / 8, -2 * M_E), ._count = 10},
        TestOptionsComplex{._varname = "e", ._value = std::complex<double>(-3, -4), ._step = std::complex<double>(1, -M_E), ._count = 10},
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmeticComplex, MultiplicationAndDivision2) {
    em_initmath();

    bool result = true;
    result &= test_complex(
        "f/g-%e*f+%pi/(%i*g)+f*g*%e", 
        [&](std::vector<em_val> const& _vars) 
            -> std::complex<double>{
                std::complex<double> _i = std::complex<double>(0, 1);
                std::complex<double> _f = em_getcomplex(_vars[0]);
                std::complex<double> _g = em_getcomplex(_vars[1]);

                if(_g == 0.0 || _i*_g == 0.0){
                    return __builtin_nan("");
                }
                
                return _f/_g-M_E*_f+M_PI/(_i*_g)+_f*_g*M_E;
            }, 
        std::vector<TestOptionsComplex>{
        TestOptionsComplex{._varname = "f", ._value = std::complex<double>(M_PI, 3), ._step = std::complex<double>(M_PI / 8, -2), ._count = 5},
        TestOptionsComplex{._varname = "g", ._value = std::complex<double>(-2, -M_E), ._step = std::complex<double>(1, -M_E / 2.0), ._count = 4},
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmetic, ExponentationAndFactorial1) {
    em_initmath();

    bool result = true;
    result &= test_real(
        "x^%e*y+(x*%pi-y*%e/x)^(x/%pi)", 
        [](std::vector<em_val> const& _vars) 
            -> double{
                auto _x = em_getdouble(_vars[0]);
                auto _y = em_getdouble(_vars[1]);
                if(_x == 0){
                    return __builtin_nan("");
                }

                std::complex<double> test = std::pow(std::complex<double>(_x*M_PI-_y*M_E/_x, 0), _x/M_PI);
                if(test.imag() != 0){
                    return __builtin_nan("");
                }

                return std::pow(_x,M_E)*_y+std::pow(_x*M_PI-_y*M_E/_x, _x/M_PI);
            }, 
        std::vector<TestOptionsReal>{
        TestOptionsReal{._varname = "x", ._lbound = 0, ._rbound = 2*M_PI, ._step = M_PI / 8},
        TestOptionsReal{._varname = "y", ._lbound = 2.0, ._rbound = 5.0, ._step = 1}
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmetic, ExponentationAndFactorial2) {
    em_initmath();

    bool result = true;
    result &= test_real(
            "(a!+b!)/(c!)", 
            [](std::vector<em_val> const& _vars) 
                -> double{
                    auto _a = _vars[0];
                    auto _b = _vars[1];
                    auto _c = _vars[2];

                    em_val a, b, c;
                    int valid = 1;
                    valid &= em_numeric_factorial(&a, _a);
                    valid &= em_numeric_factorial(&b, _b);
                    valid &= em_numeric_factorial(&c, _c);

                    if(!valid){
                        return __builtin_nan("");
                    }

                    return (em_getdouble(a)+em_getdouble(b))/(em_getdouble(c));
                }, 
        std::vector<TestOptionsReal>{
        TestOptionsReal{._varname = "a", ._lbound = 1.0, ._rbound = 7.0, ._step = 2.0},
        TestOptionsReal{._varname = "b", ._lbound = 2.0, ._rbound = 7.0, ._step = 2.0},
        TestOptionsReal{._varname = "c", ._lbound = 3.0, ._rbound = 7.0, ._step = 2.0}
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmeticComplex, ExponentationAndFactorial1) {
    em_initmath();

    bool result = true;
    result &= test_complex(
        "(d+e*%i-1/(2*d*e))^(2*d*%i-%e*e) + %pi*%i/(e^d)", 
        [&](std::vector<em_val> const& _vars) 
            -> std::complex<double>{
                std::complex<double> _i = std::complex<double>(0, 1);
                std::complex<double> _d = em_getcomplex(_vars[0]);
                std::complex<double> _e = em_getcomplex(_vars[1]);
                if(_e == 0.0){
                    return __builtin_nan("");
                }
                
                return std::pow(_d+_e*_i-1./(2.*_d*_e),2.*_d*_i-M_E*_e) + M_PI*_i/std::pow(_e,_d);
            }, 
        std::vector<TestOptionsComplex>{
        TestOptionsComplex{._varname = "d", ._value = std::complex<double>(1, 2), ._step = std::complex<double>(M_PI / 8, -2 * M_E), ._count = 10},
        TestOptionsComplex{._varname = "e", ._value = std::complex<double>(-3, -4), ._step = std::complex<double>(1, -M_E), ._count = 10},
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}

TEST(EssaMathTestsArithmeticComplex, ExponentationAndFactorial2) {
    em_initmath();

    bool result = true;
    result &= test_complex(
        "f^(g*%i)+2.5*f*%i+g^%e/%phi", 
        [&](std::vector<em_val> const& _vars) 
            -> std::complex<double>{
                std::complex<double> _i = std::complex<double>(0, 1);
                std::complex<double> _f = em_getcomplex(_vars[0]);
                std::complex<double> _g = em_getcomplex(_vars[1]);
                
                return std::pow(_f,_g*_i)+2.5*_f*_i+std::pow(_g,M_E)/em_getdouble(em_numeric_phi());
            }, 
        std::vector<TestOptionsComplex>{
        TestOptionsComplex{._varname = "f", ._value = std::complex<double>(M_PI, 3), ._step = std::complex<double>(M_PI / 8, -2), ._count = 5},
        TestOptionsComplex{._varname = "g", ._value = std::complex<double>(-2, -M_E), ._step = std::complex<double>(1, -M_E / 2.0), ._count = 4},
    }, true);

    em_freemath();
    // Assert
    EXPECT_TRUE(result);
}
