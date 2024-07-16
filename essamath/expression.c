#include "expression.h"
#include <ecl/ecl.h>
#include <ecl/external.h>

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
