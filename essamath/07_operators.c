#include "07_operators.h"
#include "expression.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int em_infix(em_object _op){
    return em_invoke("infix", 1, _op);
}

int em_infix_2(em_object _op, em_object _lbp, em_object _rbp){
    return em_invoke("infix", 3, _op, _lbp, _rbp);
}

int em_infix_3(em_object _op, em_object _lbp, em_object _rbp, em_object _lpos, em_object _rpos, em_object _pos){
    return em_invoke("infix", 6, _op, _lbp, _rbp, _lpos, _rpos, _pos);
}

int em_matchfix(em_object _ldelimiter, em_object _rdelimiter){
    return em_invoke("matchfix", 2, _ldelimiter, _rdelimiter);
}

int em_matchfix_2(em_object _ldelimiter, em_object _rdelimiter, em_object _arg_pos, em_object _pos){
    return em_invoke("matchfix", 4, _ldelimiter, _rdelimiter, _arg_pos, _pos);
}

int em_nary(em_object _op){
    return em_invoke("nary", 1, _op);
}

int em_nary_2(em_object _op, em_object _bp, em_object _arg_pos, em_object _pos){
    return em_invoke("nary", 4, _op, _bp, _arg_pos, _pos);
}

int em_nofix(em_object _op){
    return em_invoke("nofix", 1, _op);
}

int em_nofix_2(em_object _op, em_object _pos){
    return em_invoke("nofix", 2, _op, _pos);
}

int em_postfix(em_object _op){
    return em_invoke("postfix", 1, _op);
}

int em_postfix_2(em_object _op, em_object _lbp, em_object _lpos, em_object _pos){
    return em_invoke("postfix", 4, _op, _lbp, _lpos, _pos);
}

int em_prefix(em_object _op){
    return em_invoke("prefix", 1, _op);
}

int em_prefix_2(em_object _op, em_object _rbp, em_object _rpos, em_object _pos){
    return em_invoke("prefix", 4, _op, _rbp, _rpos, _pos);
}
