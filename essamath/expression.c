#include "expression.h"
#include "math_utils.h"
#include <complex.h>
#include "expression_functions.h"
#include <ctype.h>
#include <ecl/ecl.h>
#include <ecl/external.h>
#include <math.h>
#include <printf.h>
#include <stdlib.h>
#include <string.h>

// NOLINTBEGIN(misc-no-recursion)

int em_eval(const char* _expr){
    cl_env_ptr l_env = ecl_process_env();

	ECL_HANDLER_CASE_BEGIN(l_env, ecl_list1(ECL_T)) {
        char buf[1024] = {0};
        cl_eval(c_string_to_object("(setq *maxima-started* nil)"));
        sprintf(buf, "(handler-case (catch 'macsyma-quit (macsyma-top-level (make-string-input-stream \"%s$\") :batch) ) )", _expr);
        cl_eval(c_string_to_object(buf));
	} } else if (__the_env->values[0] == ecl_make_fixnum(1)) {
        [[maybe_unused]] const cl_object args = __the_env->values[1]; {
        return EM_RTERROR;
	} ECL_HANDLER_CASE_END;

    return EM_RTNORM;
}

int em_invoke(const char* _funcname, size_t n, ...){

    va_list ptr;
    size_t index = 0;
    char command[1024] = {0};
    int result = 0;

    strcpy(command, _funcname);

    index = strlen(command);
    command[index++] = '(';
 
    va_start(ptr, n);
    for (size_t i = 0; i < n; i++){
        em_object obj = va_arg(ptr, em_object);
        printf("%zx\n", (size_t)obj);
        em_printexpr(obj, command + index, 1024 - index);
        index = strlen(command);
        command[index++] = ',';
    }
    command[index - 1] = ')';
    command[index] = '\0';
 
    // Ending argument list traversal
    va_end(ptr);

    result =  em_eval(command);

    return result;
}

static em_object em_parse_from_string(const char* _buf, size_t _begin, size_t _end){
    em_object result = NULL;
    em_object current = NULL;
    em_object obj = NULL;
    size_t depthBrackets = 0, depthQuotes = 0, begin = _begin;
    char* str = NULL;

    for(; _begin < _end; _begin++){
        if(!isspace(_buf[_begin])) {
            break;
        }
    }
    for(; _begin > _end; _end--){
        if(!isspace(_buf[_end - 1])) {
            break;
        }
    }
    result = (em_object)malloc(sizeof(struct EmList));

    if(_buf[_begin] == '(' && _buf[_end - 1] == ')'){
        _begin++; _end--;
        current = result;
        current->emType = EM_LIST;
        current->emVal.emList = NULL;
        begin = _begin;
        for(size_t i = _begin; i < _end; i++){
            if(_buf[i] == '('){
                depthBrackets++;
            }else if(_buf[i] == ')'){
                depthBrackets--;
            }

            if(_buf[i] == '\"' || _buf[i] == '\''){
                if(depthQuotes){
                    depthQuotes--;
                }else{
                    depthQuotes++;
                }
            }

            if(depthBrackets || depthQuotes){
                continue;
            }

            if(isspace(_buf[i])){
                obj = em_parse_from_string(_buf, begin, i);
                if(!current->emVal.emList){
                    current->emVal.emList = obj;
                    current = obj;
                }else{
                    current->emNext = obj;
                    current = obj;

                }
                begin = i;
            }
        }

        obj = em_parse_from_string(_buf, begin, _end);
        if(!current->emVal.emList){
            current->emVal.emList = obj;
            current = obj;
        }else{
            current->emNext = obj;
            current = obj;
        }
        current->emNext = NULL;
    }else{
        int isnum = 1, point = 0;
        for(size_t i = _begin; i < _end; i++){
            if(i == _begin && _buf[i] == '-'){
                continue;
            }

            if(!point && _buf[i] == '.'){
                point = 1;
            }else if(!isdigit(_buf[i])){
                isnum = 0;
                break;
            }
        }

        str = (char*)malloc(_end - _begin + 1);
        strncpy(str, _buf + _begin, _end - _begin);
        str[_end - _begin] = 0;
        if(isnum){
            result->emType = EM_NUMBER;
            result->emVal.emNumber = atof(str);
            free(str);
        }else{
            result->emType = EM_STRING;
            result->emVal.emString = str;
        }
    }

    return result;
}

em_object em_parse(cl_object _list){
    char* buf = (char*)malloc(_list->string.dim);
    char* test = (char*)malloc(_list->string.dim);
    em_object result = NULL;
    
    size_t index = 0;
    int ommitwhitespace = 0;
    for(size_t i = 0; i < _list->string.dim; i++){
        char c = (char)tolower(_list->string.self[i]);
        test[i] = c;
        
        if(isgraph(c)){
            ommitwhitespace = 0;
            buf[index] = c;
            index++;
        }else if(!ommitwhitespace){
            ommitwhitespace = 1;
            buf[index] = ' ';
            index++;
        }
    }
    // printf("%s\n", test);
    buf[index] = 0;
    result =  em_parse_from_string(buf, 0, index);
    
    free(buf);
    free(test);
    return result;
}

void em_printlist(char* _buf, size_t _buf_size, const em_object _toprint) {
    const em_object current = _toprint;
    size_t index = strlen(_buf);
    
    while (current && index < _buf_size) {
        switch (current->emType) {
            case EM_NUMBER: {
                int written = snprintf(_buf + index, _buf_size - index, "%g", current->emVal.emNumber);
                if (written < 0 || (size_t)written >= _buf_size - index) {
                    // Handle error or truncation
                    return;
                }
                index += written;
                break;
            }
            case EM_STRING: {
                int written = snprintf(_buf + index, _buf_size - index, "%s", current->emVal.emString);
                if (written < 0 || (size_t)written >= _buf_size - index) {
                    // Handle error or truncation
                    return;
                }
                index += written;
                break;
            }
            case EM_LIST: {
                if (index < _buf_size - 1) {
                    _buf[index++] = '(';
                    em_printlist(_buf + index, _buf_size - index, current->emVal.emList);
                    index = strlen(_buf);
                    if (index < _buf_size - 1) {
                        _buf[index++] = ')';
                    } else {
                        // Handle truncation
                        return;
                    }
                } else {
                    // Handle truncation
                    return;
                }
                break;
            }
            default:
                break;
        }
        current = current->emNext;
        if (current && index < _buf_size - 1) {
            _buf[index++] = ' ';
        }
    }
    if (index < _buf_size) {
        _buf[index] = '\0';
    } else {
        _buf[_buf_size - 1] = '\0';
    }
}

em_object em_clonelist(em_object _other){
    em_object current = NULL;

    if(_other == NULL){
        return NULL;
    }
    current = (em_object)malloc(sizeof(struct EmList));

    switch(_other->emType){
        case EM_NUMBER:
            current->emType = EM_NUMBER;
            current->emVal.emNumber = _other->emVal.emNumber;
        break;
        case EM_STRING:
            current->emType = EM_STRING;
            current->emVal.emString = (char*)malloc(strlen(_other->emVal.emString));
            strcpy(current->emVal.emString, _other->emVal.emString);
        break;
        case EM_LIST:
            current->emVal.emList = em_clonelist(_other->emVal.emList);
        break;
        default:
        break;
    } 

    current->emNext = em_clonelist(_other->emNext);
    return current;
}

static void append_to_buffer(char* buf, size_t* buf_pos, size_t buf_size, const char* str) {
    size_t len = strlen(str);
    if (*buf_pos + len < buf_size) {
        strcpy(&buf[*buf_pos], str);
        *buf_pos += len;
    } else {
        // Handle buffer overflow error if needed
    }
}

static void em_tostring_helper(const em_object _current, char* _buf, size_t _size, size_t* _buf_pos, int _significance);
static void em_append_operator(const em_object _list, char* _buf, size_t _size, size_t* _buf_pos, int _significance, const char* _lbracket, const char* _rbracket, const char* _separator, int _operatormode){
    append_to_buffer(_buf, _buf_pos, _size, _lbracket);
    while (_list != NULL) {
        if (_operatormode == 0) { 
            if(*_buf_pos > 0 && _buf[*_buf_pos - 1] == '+'){ (*_buf_pos)--;}
            append_to_buffer(_buf, _buf_pos, _size, _separator);
        }
        em_tostring_helper(_list, _buf, _size, _buf_pos, _significance);
        if (_list->emNext != NULL && _operatormode == 1) { append_to_buffer(_buf, _buf_pos, _size, _separator);}
        if (_operatormode == 2) { append_to_buffer(_buf, _buf_pos, _size, _separator);}
        _list = _list->emNext;
    }
    append_to_buffer(_buf, _buf_pos, _size, _rbracket);
}

static void em_tostring_helper(const em_object _current, char* _buf, size_t _size, size_t* _buf_pos, int _significance) {
    if (_current == NULL) { return; }

    switch (_current->emType) {
        case EM_NUMBER: {
            char number_buf[32];
            if(_current->emVal.emNumber < 0) {
                snprintf(number_buf, sizeof(number_buf), "(%g)", _current->emVal.emNumber);
            } else {
                snprintf(number_buf, sizeof(number_buf), "%g", _current->emVal.emNumber);
            }
            append_to_buffer(_buf, _buf_pos, _size, number_buf);
            break;
        }
        case EM_STRING: {
            if (_current->emVal.emString[0] == '$') {
                append_to_buffer(_buf, _buf_pos, _size, &_current->emVal.emString[1]);
            } else {
                append_to_buffer(_buf, _buf_pos, _size, _current->emVal.emString);
            }
            break;
        }
        case EM_LIST: {
            em_object list = _current->emVal.emList;
            if(list != NULL){
                em_object name = list->emVal.emList;
                list = list->emNext;
                if (name != NULL && name->emType == EM_STRING) {
                    if (strcmp(name->emVal.emString, "mequal") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "(", ")", "=", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "", "", "=", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mnotequal") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "(", ")", "#", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "", "", "#", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mgreaterp") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "(", ")", ">", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "", "", ">", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mgeqp") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "(", ")", ">=", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "", "", ">=", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mlessp") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "(", ")", "<", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "", "", "<", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mleqp") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "(", ")", "<=", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 0, "", "", "<=", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mplus") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 1, "(", ")", "+", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 1, "", "", "+", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mminus") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 1, "(", ")", "-", 0);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 1, "", "", "-", 0);
                        }
                    } else if (strcmp(name->emVal.emString, "mtimes") == 0) {
                        if(_significance > 2){
                            em_append_operator(list, _buf, _size, _buf_pos, 2, "(", ")", "*", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 2, "", "", "*", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "rat") == 0 || strcmp(name->emVal.emString, "mquotient") == 0) {
                        if(_significance > 2){
                            em_append_operator(list, _buf, _size, _buf_pos, 2, "(", ")", "/", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 2, "", "", "/", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mexpt") == 0) {
                        if(_significance > 3){
                            em_append_operator(list, _buf, _size, _buf_pos, 3, "(", ")", "^", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 3, "", "", "^", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mfactorial") == 0) {
                        if(_significance > 4){
                            em_append_operator(list, _buf, _size, _buf_pos, 4, "(", ")", "!", 1);
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 4, "", "", "!", 1);
                        }
                    } else if (strcmp(name->emVal.emString, "mlist") == 0) {
                       em_append_operator(list, _buf, _size, _buf_pos, 0, "[", "]", ",", 1);
                    } else {
                        if (name->emVal.emString[0] == '%') {
                            append_to_buffer(_buf, _buf_pos, _size, &name->emVal.emString[1]);
                        } else {
                            append_to_buffer(_buf, _buf_pos, _size, name->emVal.emString);
                        }
                       em_append_operator(list, _buf, _size, _buf_pos, 0, "(", ")", ",", 1);
                    }
                }
                break;
            }
        }
        break; 
        default:
        break;
    }
}

em_val em_real(double _real){
    return em_createreal(_real);
}

em_val em_complex(double _real, double _imag){
    return em_createcomplex(_real + I * _imag);
}

em_val em_vector(size_t n, ...){
    em_val* arr = (em_val*)malloc(n * sizeof(em_val));

    va_list ptr;
    va_start(ptr, n);
    for (size_t i = 0; i < n; i++){
        arr[i] = va_arg(ptr, em_val);
    }
    va_end(ptr);

    return em_createvector(arr, n);
}

struct EmNumericValue em_createreal(double _number){
    struct EmNumericValue result;
    result.emType = EM_VALREAL;
    result.emValue.emReal = _number;

    return result;
}

struct EmNumericValue em_createcomplex(_Complex double _number){
    struct EmNumericValue result;
    result.emType = EM_VALCOMPLEX;
    result.emValue.emComplex = _number;

    return result;
}

struct EmNumericValue em_createvector(em_val* _number, size_t _size){
    struct EmNumericValue result;
    result.emType = EM_VALVECTOR;
    result.emValue.emVector.emData = _number;
    result.emValue.emVector.emSize = _size;

    return result;
}

double em_getdouble(const em_val* _value){
    switch (_value->emType) {
        case EM_VALREAL:
            return _value->emValue.emReal;
        case EM_VALCOMPLEX:{
            if(cimag(_value->emValue.emComplex) != 0){
                return __builtin_nan("");
            }

            return creal(_value->emValue.emComplex);
        }
        default:
            return __builtin_nan("");
    }
}

_Complex double em_getcomplex(const em_val* _value){
    switch (_value->emType) {
        case EM_VALREAL:
            return _value->emValue.emReal;
        case EM_VALCOMPLEX:
            return _value->emValue.emComplex;
        default:
            return __builtin_nan("");
    }
}

size_t em_getvectorsize(const em_val* _value){
    switch (_value->emType) {
        case EM_VALVECTOR:
            return _value->emValue.emVector.emSize;
        default:
            return 0;
    }
}

const em_val* em_getvectordata(const em_val* _value){
    switch (_value->emType) {
        case EM_VALVECTOR:
            return _value->emValue.emVector.emData;
        default:
            return NULL;
    }
}


void em_printexpr(const em_object _current, char* _buf, size_t _size) {
    size_t buf_pos = 0;
    em_tostring_helper(_current, _buf, _size, &buf_pos, 0);
    _buf[buf_pos] = '\0';  // Null-terminate the string
}

void em_printval(const em_val* _toprint, char* _buf, size_t _buf_size) {
    size_t index = strlen(_buf);
    switch (_toprint->emType) {
        case EM_VALREAL: {
            int written = snprintf(_buf + index, _buf_size - index, "%g", _toprint->emValue.emReal);
            if (written < 0 || (size_t)written >= _buf_size - index) {
                // Handle error or truncation
                return;
            }
            index += written;
            break;
        }
        case EM_VALCOMPLEX: {
            int written = 0;
            if (cimag(_toprint->emValue.emComplex) > 0) {
                written = snprintf(_buf + index, _buf_size - index, "%g+%g*%%i", creal(_toprint->emValue.emComplex), cimag(_toprint->emValue.emComplex));
            } else if (cimag(_toprint->emValue.emComplex) < 0) {
                written = snprintf(_buf + index, _buf_size - index, "%g-%g*%%i", creal(_toprint->emValue.emComplex), fabs(cimag(_toprint->emValue.emComplex)));
            } else {
                written = snprintf(_buf + index, _buf_size - index, "%g", creal(_toprint->emValue.emComplex));
            }
            if (written < 0 || (size_t)written >= _buf_size - index) {
                // Handle error or truncation
                return;
            }
            index += written;
            break;
        }
        case EM_VALVECTOR: {
            if (index < _buf_size - 1) {
                _buf[index++] = '[';
                for (size_t i = 0; i < _toprint->emValue.emVector.emSize; i++) {
                    em_printval(_toprint->emValue.emVector.emData + i, _buf + index, _buf_size - index);
                    index = strlen(_buf);
                    if (i < _toprint->emValue.emVector.emSize - 1 && index < _buf_size - 1) {
                        _buf[index++] = ',';
                    }
                }
                if (index < _buf_size - 1) {
                    _buf[index++] = ']';
                }
            }
            break;
        }
        default:
            break;
    }
    if (index < _buf_size) {
        _buf[index] = '\0';
    } else {
        _buf[_buf_size - 1] = '\0';
    }
}

void em_rellist(em_object _tofree){
    em_object current = _tofree;
    while(current){
        switch(current->emType){
            case EM_STRING:
                free(current->emVal.emString);
            break;
            case EM_LIST:
                em_rellist(current->emVal.emList);
                free(current->emVal.emList);
            break;
            default:
            break;
        } 
        current = current->emNext;
    }
}

em_object em_getexpr(em_object _identifier){
    char buf[2048] = {0};
    char id[256] = {0};
    cl_object obj;

    if(!_identifier){
        return NULL;
    }

    if(_identifier->emType != EM_STRING || _identifier->emNext){
        return NULL;
    }

    em_printexpr(_identifier, id, 256);
    sprintf(buf, "(api-eval \"%s$\")", id);
    obj = cl_eval(c_string_to_object(buf));

    return em_parse(obj);
}

int em_nearequal(double _lhs, double _rhs, double _eps){
    double diff = fabs(_lhs - _rhs);

    double largest = fmax(fabs(_lhs), fabs(_rhs));

    double dynamic_epsilon = _eps * largest;

    return diff <= dynamic_epsilon;
}

static struct EmValueNode* em_createexpression_helper(em_object _current, size_t _varcount, const char** _varlist, em_val** _vardata) {
    struct EmValueNode* result = NULL;
    if (_current == NULL) { return NULL;}

    result = (struct EmValueNode*)malloc(sizeof(struct EmValueNode));

    switch (_current->emType) {
        case EM_NUMBER: {
            result->emType = EM_EXPRNUM;
            result->emVal.emNumber = em_createreal(_current->emVal.emNumber);
            // printf("Number: %g\n", _current->emVal.emNumber);
            break;
        }
        case EM_STRING: {
            if (_current->emVal.emString[0] == '$') {
                if(_current->emVal.emString[1] == '%'){
                    // printf("Constant: %s\n", _current->emVal.emString);
                    if(strcmp(_current->emVal.emString, "$%pi") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = em_numeric_pi();
                    }else if(strcmp(_current->emVal.emString, "$%e") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = em_numeric_e();
                    }else if(strcmp(_current->emVal.emString, "$%i") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = em_numeric_i();
                    }else if(strcmp(_current->emVal.emString, "$%inf") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = em_numeric_inf();
                    }else if(strcmp(_current->emVal.emString, "$%minf") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = em_numeric_minf();
                    }else if(strcmp(_current->emVal.emString, "$%phi") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = em_numeric_phi();
                    }else{
                        result->emType = EM_EXPREXP;
                    }
                }else{
                    result->emType = EM_EXPRVAR;
                    for(size_t i = 0; i < _varcount; i++){
                        if(strcmp(&_current->emVal.emString[1], _varlist[i]) == 0){
                            result->emVal.emVariable = _vardata[i];
                            // printf("Variable: %s\t%zx\n", &_current->emVal.emString[1], (size_t)_vardata[i]);
                        }
                    }
                }
            }
            break;
        }
        case EM_LIST: {
            result->emType = EM_EXPREXP;
            result->emVal.emExpr = em_createexpression(_current, _varcount, _varlist, _vardata);
            break;
        }
        default:
        break;
    }

    return result;
}

em_expr em_createexpression(em_object _current, size_t _varcount, const char** _varlist, em_val** _vardata){
    struct EmExpression* result = NULL;
    size_t count = 0;
    em_object current;
    
    if (_current == NULL) { return NULL;}

    result = (em_expr)malloc(sizeof(struct EmExpression));

    switch (_current->emType) {
        case EM_NUMBER: {
        __attribute__((fallthrough)); case EM_STRING:
            result->emCount = 1;
            result->emHead = NULL;
            result->emFunc = em_getfunctionptr("dummy");
            result->emArgs = (struct EmValueNode**)malloc(sizeof(struct EmValueNode*));
            result->emArgs[0] = em_createexpression_helper(_current, _varcount, _varlist, _vardata);
            break;
        }
        case EM_LIST: {
            em_object list = _current->emVal.emList;

            if(list != NULL){
                em_object name = list->emVal.emList;
                list = list->emNext;
                count = 0;

                current = list;
                while(current){
                    count++;
                    current = current->emNext;
                }
                if (name != NULL && name->emType == EM_STRING) {
                    result->emCount = count;
                    result->emHead = name;
                    result->emArgs = (struct EmValueNode**)malloc(count * sizeof(struct EmValueNode*));
                    if(name->emVal.emString[0] == 'm' || name->emVal.emString[0] == '%'){
                        result->emFunc = em_getfunctionptr(&name->emVal.emString[1]);
                    }else{
                        result->emFunc = em_getfunctionptr(name->emVal.emString);
                    }
                    // printf("Name: %s\t%zx\n", name->emVal.emString, (size_t)result->emFunc);
                    for(size_t i = 0; i < count; i++){
                        result->emArgs[i] = em_createexpression_helper(list, _varcount, _varlist, _vardata);
                        list = list->emNext;
                    }
                }
                break;
            }
        }
        break; 
        default:
        break;
    }

    return result;
}

em_val em_calculateexpr(em_expr _expr){
    if(!_expr->emFunc) {
        return em_numeric_nan();
    }
    return (*_expr->emFunc)(_expr->emHead, _expr->emArgs, _expr->emCount); 
}

em_val em_calculateexprnode(struct EmValueNode* _expr){
    switch (_expr->emType) {
        case EM_EXPRNUM:
            return _expr->emVal.emNumber;
        case EM_EXPRVAR:
            return *_expr->emVal.emVariable;
        case EM_EXPREXP:
            return em_calculateexpr(_expr->emVal.emExpr);
        default:
        break;
    }

    return em_createreal(0.0);
}

void em_relexpr(em_expr _tofree){
    for(size_t i = 0; i < _tofree->emCount; i++){
        em_relexprnode(_tofree->emArgs[i]);
    }
    free((void*)_tofree->emArgs);
}

void em_relexprnode(struct EmValueNode* _tofree){
    switch (_tofree->emType) {
        case EM_EXPREXP:
            em_relexpr(_tofree->emVal.emExpr);
            break;
        default:
        break;
    }
}

em_object em_createstring(const char* _str){
    em_object result = (em_object)malloc(sizeof(struct EmList));
    size_t len = strlen(_str);
    result->emType = EM_STRING;
    result->emNext = NULL;
    if(len > 0){
        result->emVal.emString = (char*)malloc(len + 1);
        strcpy(result->emVal.emString, _str);
        result->emVal.emString[len] = '\0';
    }

    return result;
}

em_object em_createnumber(double _number){
    em_object result = (em_object)malloc(sizeof(struct EmList));
    result->emType = EM_NUMBER;
    result->emNext = NULL;
    result->emVal.emNumber = _number;

    return result;
}
// NOLINTEND(misc-no-recursion)
