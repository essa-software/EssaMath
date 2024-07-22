#include "math_utils.h"

double em_nan(){
    return (__builtin_nan (""));
}

double em_factorial(double _value){
    int res = (int)_value;

    if(_value != res || _value < 0){
        return em_nan();
    }

    int result = 1;
    for(int i = 1; i <= res; i++){
        result *= i;
    }

    return (double)result;
}

int em_isinteger(double _value){
    return (int)_value == _value;
}
