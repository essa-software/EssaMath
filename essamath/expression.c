#include "expression.h"
#include "math_utils.h"
#include <complex.h>
#include "expression_functions.h"
#include <ctype.h>
#include <ecl/ecl.h>
#include <ecl/external.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

int em_eval(const char* _expr){
    cl_env_ptr l_env = ecl_process_env();

	ECL_HANDLER_CASE_BEGIN(l_env, ecl_list1(ECL_T)) {
        char buf[1024] = {0};
        cl_eval(c_string_to_object("(setq *maxima-started* nil)"));
        sprintf(buf, "(handler-case (catch 'macsyma-quit (macsyma-top-level (make-string-input-stream \"%s$\") :batch) ) )", _expr);
        cl_eval(c_string_to_object(buf));
	} ECL_HANDLER_CASE(1, condition) {
        return EM_RTERROR;
	} ECL_HANDLER_CASE_END;

    return EM_RTNORM;
}

int em_invoke(const char* _funcname, size_t n, ...){
    va_list ptr;
    size_t size = 256 * n;
    char* command = (char*)malloc(size);
    memset(command, 0, size);
    strcpy(command, _funcname);

    size_t index = strlen(command);
    command[index++] = '(';
 
    va_start(ptr, n);
    for (size_t i = 0; i < n; i++){
        em_object obj = va_arg(ptr, em_object);
        em_tostring(obj, command + index, size - index);
        index = strlen(command);
        command[index++] = ',';
    }
    command[index - 1] = ')';
 
    // Ending argument list traversal
    va_end(ptr);

    int result =  em_eval(command);
    free(command);

    return result;
}

em_object em_parse_from_string(const char* _buf, size_t _begin, size_t _end){
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
    em_object result = (em_object)malloc(sizeof(struct EmList));

    if(_buf[_begin] == '(' && _buf[_end - 1] == ')'){
        _begin++; _end--;
        em_object current = result;
        current->emType = EM_LIST;
        current->emVal.emList = NULL;
        size_t depthBrackets = 0, depthQuotes = 0;
        size_t begin = _begin;
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
                em_object obj = em_parse_from_string(_buf, begin, i);
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

        em_object obj = em_parse_from_string(_buf, begin, _end);
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

        char* str = (char*)malloc(_end - _begin + 1);
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
    em_object result =  em_parse_from_string(buf, 0, index);
    
    free(buf);
    free(test);
    return result;
}

void em_printf(em_object _toprint){
    em_object current = _toprint;
    while(current){
        switch(current->emType){
            case EM_NUMBER:
                printf("%g", current->emVal.emNumber);
            break;
            case EM_STRING:
                printf("%s", current->emVal.emString);
            break;
            case EM_LIST:
                printf("(");
                em_printf(current->emVal.emList);
                printf(")");
            break;
            default:
            break;
        } 
        current = current->emNext;
        if(current){
            printf(" ");
        }
    }
}

void append_to_buffer(char* buf, size_t* buf_pos, size_t buf_size, const char* str) {
    size_t len = strlen(str);
    if (*buf_pos + len < buf_size) {
        strcpy(&buf[*buf_pos], str);
        *buf_pos += len;
    } else {
        // Handle buffer overflow error if needed
    }
}

void em_tostring_helper(em_object _current, char* _buf, size_t _size, size_t* _buf_pos, int _significance);
void em_append_operator(em_object _list, char* _buf, size_t _size, size_t* _buf_pos, int _significance, int _operatormode, const char* _lbracket, const char* _rbracket, const char* _separator){
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

void em_tostring_helper(em_object _current, char* _buf, size_t _size, size_t* _buf_pos, int _significance) {
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
                    if (strcmp(name->emVal.emString, "mplus") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 1, 1, "(", ")", "+");
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 1, 1, "", "", "+");
                        }
                    } else if (strcmp(name->emVal.emString, "mminus") == 0) {
                        if(_significance > 1){
                            em_append_operator(list, _buf, _size, _buf_pos, 1, 0, "(", ")", "-");
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 1, 0, "", "", "-");
                        }
                    } else if (strcmp(name->emVal.emString, "mtimes") == 0) {
                        if(_significance > 2){
                            em_append_operator(list, _buf, _size, _buf_pos, 2, 1, "(", ")", "*");
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 2, 1, "", "", "*");
                        }
                    } else if (strcmp(name->emVal.emString, "mrat") == 0 || strcmp(name->emVal.emString, "mquotient") == 0) {
                        if(_significance > 2){
                            em_append_operator(list, _buf, _size, _buf_pos, 2, 1, "(", ")", "/");
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 2, 1, "", "", "/");
                        }
                    } else if (strcmp(name->emVal.emString, "mexpt") == 0) {
                        if(_significance > 3){
                            em_append_operator(list, _buf, _size, _buf_pos, 3, 1, "(", ")", "^");
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 3, 1, "", "", "^");
                        }
                    } else if (strcmp(name->emVal.emString, "mfactorial") == 0) {
                        if(_significance > 4){
                            em_append_operator(list, _buf, _size, _buf_pos, 4, 2, "(", ")", "!");
                        }else{
                            em_append_operator(list, _buf, _size, _buf_pos, 4, 2, "", "", "!");
                        }
                    } else if (strcmp(name->emVal.emString, "mlist") == 0) {
                       em_append_operator(list, _buf, _size, _buf_pos, 0, 1, "[", "]", ",");
                    } else {
                        if (name->emVal.emString[0] == '%') {
                            append_to_buffer(_buf, _buf_pos, _size, &name->emVal.emString[1]);
                        } else {
                            append_to_buffer(_buf, _buf_pos, _size, name->emVal.emString);
                        }
                       em_append_operator(list, _buf, _size, _buf_pos, 0, 1, "(", ")", ",");
                    }
                }
                break;
            }
        }
        default:
        break;
    }
}

void em_tostring(em_object _current, char* _buf, size_t _size) {
    size_t buf_pos = 0;
    em_tostring_helper(_current, _buf, _size, &buf_pos, 0);
    _buf[buf_pos] = '\0';  // Null-terminate the string
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
    if(!_identifier){
        return NULL;
    }

    if(_identifier->emType != EM_STRING || _identifier->emNext){
        return NULL;
    }

    char buf[2048] = {0};
    char id[256] = {0};
    em_tostring(_identifier, id, 256);
    sprintf(buf, "(api-eval \"%s$\")", id);
    cl_object obj = cl_eval(c_string_to_object(buf));

    return em_parse(obj);
}


struct EmValueNode* em_createexpressiondouble_helper(em_object _current, size_t _varcount, const char** _varlist, double** _vardata) {
    if (_current == NULL) { return NULL;}

    struct EmValueNode* result = (struct EmValueNode*)malloc(sizeof(struct EmValueNode));

    switch (_current->emType) {
        case EM_NUMBER: {
            result->emType = EM_EXPRNUM;
            result->emVal.emNumber = _current->emVal.emNumber;
            // printf("Number: %g\n", _current->emVal.emNumber);
            break;
        }
        case EM_STRING: {
            if (_current->emVal.emString[0] == '$') {
                if(_current->emVal.emString[1] == '%'){
                    // printf("Constant: %s\n", _current->emVal.emString);
                    if(strcmp(_current->emVal.emString, "$%pi") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = M_PI;
                    }else if(strcmp(_current->emVal.emString, "$%e") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = M_E;
                    }else if(strcmp(_current->emVal.emString, "$%i") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = (double)I;
                    }else if(strcmp(_current->emVal.emString, "$%inf") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = (double)INFINITY;
                    }else if(strcmp(_current->emVal.emString, "$%minf") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = -(double)INFINITY;
                    }else if(strcmp(_current->emVal.emString, "$%phi") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = (1.0+sqrt(5))/2.0;
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

em_expr em_createexpression(em_object _current, size_t _varcount, const char** _varlist, double** _vardata){
    if (_current == NULL) { return NULL;}

    em_expr result = (em_expr)malloc(sizeof(struct EmExpression));

    switch (_current->emType) {
        case EM_NUMBER: {
        case EM_STRING:
            result->EmCount = 1;
            result->EmFunc = em_getfunctionptr("dummy");
            result->EmArgs = (struct EmValueNode**)malloc(sizeof(struct EmValueNode*));
            result->EmArgs[0] = em_createexpressiondouble_helper(_current, _varcount, _varlist, _vardata);
            break;
        }
        case EM_LIST: {
            em_object list = _current->emVal.emList;

            if(list != NULL){
                em_object name = list->emVal.emList;
                list = list->emNext;
                size_t count = 0;

                em_object current = list;
                while(current){
                    count++;
                    current = current->emNext;
                }
                if (name != NULL && name->emType == EM_STRING) {
                    result->EmCount = count;
                    result->EmArgs = (struct EmValueNode**)malloc(count * sizeof(struct EmValueNode*));
                    if(name->emVal.emString[0] == 'm' || name->emVal.emString[0] == '%'){
                        result->EmFunc = em_getfunctionptr(&name->emVal.emString[1]);
                    }else{
                        result->EmFunc = em_getfunctionptr(name->emVal.emString);
                    }
                    // printf("Name: %s\t%zx\n", name->emVal.emString, (size_t)result->EmFunc);
                    for(size_t i = 0; i < count; i++){
                        result->EmArgs[i] = em_createexpressiondouble_helper(list, _varcount, _varlist, _vardata);
                        list = list->emNext;
                    }
                }
                break;
            }
        }
        default:
        break;
    }

    return result;
}


struct EmComplexValueNode* em_createcomplexexpression_helper(em_object _current, size_t _varcount, const char** _varlist, _Complex double** _vardata) {
    if (_current == NULL) { return NULL;}

    struct EmComplexValueNode* result = (struct EmComplexValueNode*)malloc(sizeof(struct EmComplexValueNode));

    switch (_current->emType) {
        case EM_NUMBER: {
            result->emType = EM_EXPRNUM;
            result->emVal.emNumber = _current->emVal.emNumber;
            break;
        }
        case EM_STRING: {
            if (_current->emVal.emString[0] == '$') {
                if(_current->emVal.emString[1] == '%'){
                    // printf("Constant: %s\n", _current->emVal.emString);
                    if(strcmp(_current->emVal.emString, "$%pi") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = M_PI;
                    }else if(strcmp(_current->emVal.emString, "$%e") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = M_E;
                    }else if(strcmp(_current->emVal.emString, "$%i") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = (__extension__ 1.0i);
                    }else if(strcmp(_current->emVal.emString, "$%inf") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = (double)INFINITY;
                    }else if(strcmp(_current->emVal.emString, "$%minf") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = -(double)INFINITY;
                    }else if(strcmp(_current->emVal.emString, "$%phi") == 0){
                        result->emType = EM_EXPRNUM;
                        result->emVal.emNumber = (1.0+sqrt(5))/2.0;
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
            result->emVal.emExpr = em_createcomplexexpression(_current, _varcount, _varlist, _vardata);
            break;
        }
        default:
        break;
    }

    return result;
}

em_complexexpr em_createcomplexexpression(em_object _current, size_t _varcount, const char** _varlist, _Complex double** _vardata){
    if (_current == NULL) { return NULL;}

    em_complexexpr result = (em_complexexpr)malloc(sizeof(struct EmComplexExpression));

    switch (_current->emType) {
        case EM_NUMBER: {
        case EM_STRING:
            result->EmCount = 1;
            result->EmFunc = em_getcomplexfunctionptr("dummy");
            result->EmArgs = (struct EmComplexValueNode**)malloc(sizeof(struct EmComplexValueNode*));
            result->EmArgs[0] = em_createcomplexexpression_helper(_current, _varcount, _varlist, _vardata);
            break;
        }
        case EM_LIST: {
            em_object list = _current->emVal.emList;

            if(list != NULL){
                em_object name = list->emVal.emList;
                list = list->emNext;
                size_t count = 0;

                em_object current = list;
                while(current){
                    count++;
                    current = current->emNext;
                }
                if (name != NULL && name->emType == EM_STRING) {
                    result->EmCount = count;
                    result->EmArgs = (struct EmComplexValueNode**)malloc(count * sizeof(struct EmComplexValueNode*));
                    if(name->emVal.emString[0] == 'm' || name->emVal.emString[0] == '%'){
                        result->EmFunc = em_getcomplexfunctionptr(&name->emVal.emString[1]);
                    }else{
                        result->EmFunc = em_getcomplexfunctionptr(name->emVal.emString);
                    }
                    for(size_t i = 0; i < count; i++){
                        result->EmArgs[i] = em_createcomplexexpression_helper(list, _varcount, _varlist, _vardata);
                        list = list->emNext;
                    }
                }
                break;
            }
        }
        default:
        break;
    }

    return result;
}

double em_calculateexpr(em_expr _expr){
    if(!_expr->EmFunc) {
        return em_nan();
    }
    return (*_expr->EmFunc)(_expr->EmArgs, _expr->EmCount); 
}

_Complex double em_calculatecomplexexpr(em_complexexpr _expr){
    if(!_expr->EmFunc) {
        return 0.0;
    }
    return (*_expr->EmFunc)(_expr->EmArgs, _expr->EmCount);
}

double em_calculateexprnode(struct EmValueNode* _expr){
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

    return 0.0;
}

_Complex double em_calculatecomplexexprnode(struct EmComplexValueNode* _expr){
    switch (_expr->emType) {
        case EM_EXPRNUM:
            return _expr->emVal.emNumber;
        case EM_EXPRVAR:
            return *_expr->emVal.emVariable;
        case EM_EXPREXP:
            return em_calculatecomplexexpr(_expr->emVal.emExpr);
        default:
        break;
    }

    return 0.0;
}

void em_relexpr(em_expr _tofree){
    for(size_t i = 0; i < _tofree->EmCount; i++){
        em_relexprnode(_tofree->EmArgs[i]);
    }
    free(_tofree->EmArgs);
}

void em_relcomplexexpr(em_complexexpr _tofree){
    for(size_t i = 0; i < _tofree->EmCount; i++){
        em_relcomplexexprnode(_tofree->EmArgs[i]);
    }
    free(_tofree->EmArgs);
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

void em_relcomplexexprnode(struct EmComplexValueNode* _tofree){
    switch (_tofree->emType) {
        case EM_EXPREXP:
            em_relcomplexexpr(_tofree->emVal.emExpr);
            break;
        default:
        break;
    }
}

em_object em_create_string(const char* _str){
    em_object result = (em_object)malloc(sizeof(struct EmList));
    result->emType = EM_STRING;
    result->emNext = NULL;
    size_t len = strlen(result->emVal.emString);
    if(len > 0){
        result->emVal.emString = (char*)malloc(len);
        strcpy(result->emVal.emString, _str);
    }

    return result;
}

em_object em_create_number(double _number){
    em_object result = (em_object)malloc(sizeof(struct EmList));
    result->emType = EM_NUMBER;
    result->emNext = NULL;
    result->emVal.emNumber = _number;

    return result;
}
