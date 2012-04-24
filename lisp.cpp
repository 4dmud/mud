//c++ <-> lisp interaction
#if ECL
#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"

#include <ecl/ecl.h>


ACMD(do_lisp) {
  cl_object form = ecl_cstring_to_base_string_or_nil(argument);
  cl_object result = cl_funcall(2, c_string_to_object("4D::EVAL-STRING"), form);
  *ch << (char *) (si_coerce_to_base_string(result)->string.self) << "\r\n";

}
#endif
