
#include <ecl/ecl.h>
#include "comm.h"

extern "C" {
  void init_4d_lisp(cl_object);
}

void player_login_event(Character* player);
void player_logout_event(Character* player);
void lisp_game_loop_fn(struct game_loop_data* data);
