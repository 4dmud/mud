//c++ <-> lisp interaction
#if ECL
#include <ecl/ecl.h>

#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "interpreter.h"
#include "comm.h"



ACMD(do_lisp) {
  cl_object form = ecl_cstring_to_base_string_or_nil(argument);
  cl_funcall(3, c_string_to_object("4D::EVAL-STRING"), ecl_cstring_to_base_string_or_nil(GET_NAME(ch)), form);
}

void lisp_game_loop_fn(struct game_loop_data* data) {
  cl_funcall(2, c_string_to_object("4d-event:game-loop-fn"), ecl_make_pointer(data));
}

void player_login_event(Character* player) {
  cl_funcall(2, c_string_to_object("SIGNAL"), c_string_to_object("4D-EVENT:PLAYER-LOGIN"), player);
}

void player_logout_event(Character* player) {
  cl_funcall(2, c_string_to_object("SIGNAL"), c_string_to_object("4D-EVENT:PLAYER-LOGOUT"), player);
}

#endif
