#include "expression.h"
#include <complex.h>
#include <ctype.h>
#include <ecl/ecl.h>
#include <ecl/external.h>
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
            if(!point && _buf[i] == '.'){
                point = 1;
            }else if(!isdigit(_buf[i])){
                isnum = 0;
                break;
            }
        }

        char* str = (char*)malloc(_end - _begin);
        memset(str, 0, _end - _begin);
        strncpy(str, _buf + _begin, _end - _begin);
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
    memset(buf, 0, _list->string.dim);
    
    for(size_t i = 0; i < _list->string.dim; i++){
        buf[i] = (char)tolower(_list->string.self[i]);
    }

    em_object result =  em_parse_from_string(buf, 0, strlen(buf));
    
    free(buf);
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
void em_append_operator(em_object _list, char* _buf, size_t _size, size_t* _buf_pos, int _significance, int _ommitfirst, const char* _lbracket, const char* _rbracket, const char* _separator){
    append_to_buffer(_buf, _buf_pos, _size, _lbracket);
    while (_list != NULL) {
        if (!_ommitfirst) append_to_buffer(_buf, _buf_pos, _size, _separator);
        em_tostring_helper(_list, _buf, _size, _buf_pos, _significance);
        if (_list->emNext != NULL && _ommitfirst) append_to_buffer(_buf, _buf_pos, _size, _separator);
        _list = _list->emNext;
    }
    append_to_buffer(_buf, _buf_pos, _size, _rbracket);
}

void em_tostring_helper(em_object _current, char* _buf, size_t _size, size_t* _buf_pos, int _significance) {
    if (_current == NULL) return;

    switch (_current->emType) {
        case EM_NUMBER: {
            char number_buf[32];
            snprintf(number_buf, sizeof(number_buf), "%g", _current->emVal.emNumber);
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
