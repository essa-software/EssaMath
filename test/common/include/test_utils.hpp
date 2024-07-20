#pragma once

#include <complex>
#include <functional>
#include <string>
#include <vector>

struct TestOptionsReal{
    std::string _varname;
    double _lbound = 0.0, _rbound = 0.0, _step = 0.0;
};
bool test_real(std::string const& _exprstring, std::function<double(std::vector<double> const&)> const& _exprtest, std::vector<TestOptionsReal> const& _options, bool _verbose = false);

struct TestOptionsComplex{
    std::string _varname;
    std::complex<double> _value = 0.0, _step = 0.0;
    size_t _count = 0;
};

bool test_complex(std::string const& _exprstring, std::function<std::complex<double>(std::vector<std::complex<double>> const&)> const& _exprtest, std::vector<TestOptionsComplex> const& _options, bool _verbose = false);
