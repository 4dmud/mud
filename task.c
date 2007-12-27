#include "conf.h"
#include "sysdep.h"

#include <sys/stat.h>

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "constants.h"
#include "dg_scripts.h"
#include "dg_event.h"


int stop_task(struct char_data *ch) {
if (GET_TASK(ch) != NULL) {
toggle_sub_status(ch, GET_TASK(ch)->sub, STATUS_OFF);
free(GET_TASK(ch));
}

GET_TASK_NUM(ch) = SUB_UNDEFINED;
return 1;
}

int set_task(struct char_data *ch, int task) {
if (GET_TASK_NUM(ch) == SUB_UNDEFINED) {
GET_TASK_NUM(ch) = task;
return 1;
} else {
return 0;
}
}

void create_task(struct char_data *ch, int sub, char * argument) {

struct sub_task_obj *task;

stop_task(ch);

CREATE(task, struct sub_task_obj, 1);
task->sub = sub;
strcpy(task->arg, argument);

}

void run_task(struct char_data *ch) {
ACMD(do_subskill);
if (DEAD(ch))
return;
if (GET_TASK_NUM(ch) == SUB_UNDEFINED)
return;
  //GET_TASK_NUM(ch) = SUB_UNDEFINED;
if (GET_TASK(ch) == NULL)
return;
if (!(GET_TASK(ch)->arg != NULL && *GET_TASK(ch)->arg)) {
stop_task(ch);
return;
}

  switch (GET_TASK(ch)->sub) {
  default:
     /* this needs to work like an alias, where you can seperate commands via a colon
     or preprogram the logic for it here in the switch blocks - mordecai */
     do_subskill(ch, GET_TASK(ch)->arg, 0, GET_TASK(ch)->sub);
    break;
  }

}

ACMD(do_task) {
char buf[MAX_STRING_LENGTH];
char buf2[MAX_STRING_LENGTH];
int sub_num;
if (argument == NULL || !*argument) {
new_send_to_char(ch, "TASK <subskill> <arguments>\r\nNot all subskills will have a continuous task.\r\n");
return;
}

half_chop(argument, buf, buf2);

if ((sub_num = sub_number(buf)) <= 0) {
new_send_to_char(ch, "Sorry that isnt a valid subskill.\r\n");
return;
}

if (GET_SUB(ch, sub_num) == 0) {
new_send_to_char(ch, "Sorry but you don't even know that subskill.\r\n");
return;
}

if (GET_TASK(ch) ) {

new_send_to_char(ch, "Cancelling current task %s.\r\n", sub_name(GET_TASK(ch)->sub));
stop_task(ch);
}

if (get_sub_status(ch, sub_num)) {
new_send_to_char(ch, "That subskill already IS your current task.\r\n");
return;
}
if (set_task(ch, sub_num)) {
create_task(ch, sub_num, buf2);
run_task(ch);

new_send_to_char(ch, "Task created.\r\n");
} else 

new_send_to_char(ch, "Task not created. Try again soon\r\n");

}



