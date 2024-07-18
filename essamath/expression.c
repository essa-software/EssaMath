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
        sprintf(buf, "(handler-case (catch 'macsyma-quit (continue :stream (make-string-input-stream \"%s$\") :batch-or-demo-flag :batch) ) )", _expr);
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
                i++;
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

        current->emNext = em_parse_from_string(_buf, begin, _end);
        current->emNext->emNext = NULL;
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
    
    for(size_t i = 0; i < _list->string.dim; i++){
        buf[i] = (char)tolower(_list->string.self[i]);
    }

    em_object result =  em_parse_from_string(buf, 0, _list->string.dim);
    
    free(buf);
    return result;
}

void em_printf(em_object _toprint){
    em_object current = _toprint;
    while(current){
        switch(current->emType){
            case EM_NUMBER:
                printf("%f", current->emVal.emNumber);
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
void em_getstring_recursive(em_object _current, char* _buf, size_t _index, size_t _size){
    if(_index >= _size){
        return;
    }

    em_object current = _current;
    const char* name = NULL;
    if(current->emType == EM_LIST){
        if(current->emVal.emList->emType == EM_STRING){
            name = current->emVal.emList->emVal.emString;
        }
    }
    current = current->emNext;
    if(!name){
        return;
    }

    if(strcmp(name, "mlist") == 0){
        _buf[_index] = '[';
    }else{
        if(name[0] == '%'){
            sprintf(_buf + _index, "%s", name + 1);
            _index = strlen(_buf);
        }
        _buf[_index] = '(';
    }
    _index++;
    size_t count = 0;

    while(current){
        if(count != 0){
            if(strcmp(name, "mplus") == 0){
                _buf[_index] = '+';
            }else if(strcmp(name, "mminus") == 0){
                _buf[_index] = '-';
            }else if(strcmp(name, "mtimes") == 0){
                _buf[_index] = '*';
            }else if(strcmp(name, "mquotient") == 0 || strcmp(name, "mrat") == 0){
                _buf[_index] = '/';
            }else if(strcmp(name, "mexpt") == 0){
                _buf[_index] = '^';
            }else{
                _buf[_index] = ',';
            }
            _index++;
        }

        switch(current->emType){
            case EM_NUMBER:
                sprintf(_buf + _index, "%f", current->emVal.emNumber);
            break;
            case EM_STRING:
                if(current->emVal.emString[0] == '$'){
                    sprintf(_buf + _index, "%s", current->emVal.emString + 1);
                }else{
                    sprintf(_buf + _index, "%s", current->emVal.emString);
                }
            break;
            case EM_LIST:
                em_getstring_recursive(_current, _buf, _index, _size);
            break;
            default:
            break;
        } 
        current = current->emNext;
        _index = strlen(_buf);

        count++;
    }

    if(strcmp(name, "mlist") == 0){
        _buf[_index] = ']';
    }else{
        _buf[_index] = ')';
    }
    _index++;
}

void em_tostring(em_object _toprint, char* _buf, size_t _size){
    memset(_buf, 0, _size);
    if(_toprint->emType == EM_LIST){
        em_getstring_recursive(_toprint->emVal.emList, _buf, 0, _size);
    }else{
        em_getstring_recursive(_toprint, _buf, 0, _size);
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
