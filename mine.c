/***************************************************************************
                          mine.c  -  description
                             -------------------
    begin                : Tue Mar 16 2004
    copyright            : (C) 2004 by molly
    email                : Mordecai@xtra.co.nz
 ***************************************************************************/
#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "dg_scripts.h"
#include "constants.h"
#include "dg_event.h"


extern struct room_data *world_vnum[];
extern struct corpse_list_data *corpse_list;
extern struct char_data *character_list;
extern struct descriptor_data *descriptor_list;
extern struct index_data *mob_index;
extern struct index_data *obj_index;
extern struct time_info_data time_info;
extern struct spell_info_type spell_info[];
EVENTFUNC(message_event);
void mine_damage(struct char_data *vict, int dam);
void send_char_pos(struct char_data *ch, int dam);
void die(struct char_data *ch, struct char_data *killer);



ASUB(sub_tunneling)
{
  room_rnum rm = IN_ROOM(ch);
  room_vnum vrm = GET_ROOM_VNUM(rm);
  struct message_event_obj *msg = NULL;
  char direction[MAX_INPUT_LENGTH];
  int dir, pick = TRUE, hard = FALSE, soft = FALSE, density;
  struct obj_data *tool;

  if (GET_SUB(ch, SUB_TUNNELING) <= 0)
  {
    new_send_to_char(ch, "You have no idea how to use that command!\r\n");
    return SUB_UNDEFINED;
  }

  if (get_sub_status(ch, SUB_TUNNELING) == STATUS_ON)
  {
    new_send_to_char(ch, "You stop tunneling.\r\n");
    act("$n stops tunneling.", FALSE, ch, 0, 0, TO_ROOM);
    toggle_sub_status(ch, SUB_TUNNELING, STATUS_OFF);
    if (GET_TASK(ch) && GET_TASK(ch)->sub == SUB_TUNNELING)
      stop_task(ch);
    return SUB_UNDEFINED;
  }

  if ((tool = GET_EQ(ch, WEAR_WIELD)) == NULL)
  {
    new_send_to_char(ch, "You can't mine with out a tool!\r\n");
    return SUB_UNDEFINED;
  }
  if (GET_OBJ_TYPE(tool)==ITEM_SHOVEL)
    pick = FALSE;
  else if (GET_OBJ_TYPE(tool)== ITEM_PICKAXE)
    pick = TRUE;
  else
  {
    new_send_to_char(ch, "You need to wield a pickaxe or a shovel.\r\n");
    return SUB_UNDEFINED;
  }

  if ((hard = (str_str(rm->name, "rock") || str_str(rm->name, "hard") ||
               str_str(rm->name, "mine") || str_str(rm->name, "cave"  )))
      && pick == FALSE)
  {
    new_send_to_char(ch, "Sorry but you need a pickaxe to tunnel there.\r\n");
    return SUB_UNDEFINED;
  }

  if ((soft = (str_str(rm->name, "sand") || str_str(rm->name, "earth") ||
               str_str(rm->name, "soft")) )
      && pick == TRUE)
  {
    new_send_to_char(ch, "Sorry but you need a shovel to tunnel there.\r\n");
    return SUB_UNDEFINED;
  }

  if (!hard && !soft && (vrm > 55000 || vrm < 54000))
  {
    new_send_to_char(ch, "Sorry, this place cannot be tunneled.\r\n");
    return SUB_UNDEFINED;
  }
  if (str_str(rm->name, "bridge") || str_str(rm->name, "moria") ||
      str_str(rm->name, "abyss" ) || str_str(rm->name, "bat"  ) ||
      str_str(rm->name, "heart" ) || str_str(rm->name, "stalactite") ||
      str_str(rm->name, "cart"  ) || str_str(rm->name, "lake") ||
      (vrm >= 54831 && vrm <= 54899))
  {
    new_send_to_char(ch, "Sorry, this place cannot be tunneled.\r\n");
    return SUB_UNDEFINED;
  }

  one_argument(argument, direction);

  if ((!direction || !*direction))
  {
    new_send_to_char(ch, "Usage: tunnel <direction>\r\n");
    return SUB_UNDEFINED;
  }

  switch (LOWER(*direction))
  {
  case 'n':
    dir = NORTH;
    break;
  case 's':
    dir = SOUTH;
    break;
  case 'e':
    dir = EAST;
    break;
  case 'w':
    dir = WEST;
    break;
  case 'u':
    dir = UP;
    break;
  case 'd':
    dir = DOWN;
    break;
  default:
    new_send_to_char(ch, "Invalid direction! Directions are: north south east west up down.\r\n");
    return SUB_UNDEFINED;
  }
  if ((dir != DOWN && (vrm > 55000 || vrm < 54000)) || W_EXIT(rm, dir))
  {
    new_send_to_char(ch, "There is no good mining area that direction.\r\n");
    return SUB_UNDEFINED;
  }

  if (GET_MESSAGE_EVENT(ch)!=NULL)
  {
    new_send_to_char(ch, "You are in the middle of something else!\r\n");
    return  SUB_UNDEFINED;
  }
  if (GET_MSG_RUN(ch))
  {
    new_send_to_char(ch, "You are already working on something else!\r\n");
    return  SUB_UNDEFINED;
  }

  if (rm == NULL)
  {
    new_send_to_char(ch, "Error! Invalid room\r\n");
    return SUB_UNDEFINED;
  }

  density = 1;
  density += (hard ? 3 + number(1, 3) : 0);
  density += (soft ? 1 + number(1, 2) : 0);

  new_send_to_char(ch, "You begin to tunnel %s.\r\n", dirs[dir]);

  GET_MSG_RUN(ch) = TRUE;
  MINE_DIR(ch) = dir;
  toggle_sub_status(ch, SUB_TUNNELING, STATUS_ON);

  CREATE(msg, struct message_event_obj, 1);
  msg->ch_id = GET_ID(ch);
  msg->skill = SUB_TUNNELING;
  msg->type = THING_SUB;
  msg->msg_num = density;
  msg->id = (long) (rm->number + ROOM_ID_BASE);

  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, (1 RL_SEC));
  return SUB_TUNNELING;
}

void check_mine_traps(struct char_data *ch)
{
  int hit = GET_HIT(ch), mhit = GET_MAX_HIT(ch);
  int dam = 0;
  if (number(0, 100))
    return;

  act("You dig into and inspect a cunning trap.", FALSE, ch, 0, 0, TO_CHAR);
  act("$n digs into and inspects a cunning trap.", FALSE, ch, 0, 0, TO_ROOM);
  if (number(0, 110) < MINE_STEALTH(ch))
  {
    act("You are relieved with a twanging sound as you disarm the trap.", FALSE, ch, 0, 0, TO_CHAR);
    return;
  }

  switch (number(1, 9))
  {
  case 1:
    act("You are surrounded by a strange white mist!", FALSE, ch, 0, 0, TO_CHAR);
    dam = ((mhit)/3);
    break;
  case 2:
    act("You are surrounded by a pungent green gas!", FALSE, ch, 0, 0, TO_CHAR);
    dam = ((mhit)/3)*2;
    break;
  case 3:
    act("You are surrounded by a gas of scintillating colors!", FALSE, ch, 0, 0, TO_CHAR);
    dam = ((mhit)/2);
    break;
  case 4:
    act("You are surrounded by a sparking black gas!", FALSE, ch, 0, 0, TO_CHAR);
    dam = ((hit)/5)*4;
    break;
  case 5:
    act("You are splashed with acid!", FALSE, ch, 0, 0, TO_CHAR);
    dam = ((hit)-40);
    break;
  case 6:
    act("You are enveloped in a column of fire!!", FALSE, ch, 0, 0, TO_CHAR);
    dam = ((hit)-10);
    break;
  case 7:
    act("You are coated with frost!", FALSE, ch, 0, 0, TO_CHAR);
    dam = ((mhit)/4)*3;
    break;
  case 8:
    act("You are slammed with a blizzard!", FALSE, ch, 0, 0, TO_CHAR);
    dam = ((mhit)-(5));
    break;
  case 9:
    act("You are enveloped in a cloud of smoke!", FALSE, ch, 0, 0, TO_CHAR);
    dam = ((mhit)/2);
    break;
  }
  if (dam)
    dam *= ((float)(100.0 - MINE_DAMAGE(ch))/100.0);

  mine_damage(ch, dam);
}
/*
  newroom = 54000+number(1,180);
      if (number(0, 110) < MINE_BONUS(ch))
       newroom += 20;
 
       if ((%newroom.name%/=rock)|| (%newroom.name%/=sand)|| (%newroom.name%/=earth))
         set yes_go_up 1
        elseif ((%newroom.name%/=stone)||(%newroom.name%/=mine)|| (%newroom.name%/=soft)|| (%newroom.name%/=hard))
         set yes_go_up 1
        end
        if %yes_go_up%
         %send% %actor% %tu% You dig up into new unexplored territory.
         %echoaround% %actor% %tu% %actor.name% creates a new mine exit up to %newroom.name%.
        else
         %send% %actor% %tu% You fail to dig through into new territory.
         %echoaround% %actor% %tu% %actor.name% fails to dig through into new territory.
         halt
        end
*/

void make_tunnel(struct char_data *ch)
{
  room_vnum vrm = GET_ROOM_VNUM(IN_ROOM(ch));
  room_vnum newroom = NOWHERE;
  room_rnum rnew = NULL;
  int size = 0, pass = TRUE, level = 0;
  char description[MAX_INPUT_LENGTH];
  struct room_direction_data *exit;
  struct mine_info
  {
    int size;
    room_vnum start;
  }
  mine_shaft[] = {
                   {0, 0},
                   {200, 54000},
                   {200, 54200},
                   {200, 54400},
                   {200, 54600},
                   {100, 54800},
                   {100, 54900}
                 };

  if (vrm > 55000 || vrm < 54000)
  {
    level = 0;
  }
  else if (vrm>=54000 && vrm<=54199)
  {
    level = 1;
  }
  else if (vrm>=54200 && vrm<=54399)
  {
    level = 2;
  }
  else if (vrm>=54400 && vrm<=54599)
  {
    level = 3;
  }
  else if (vrm>=54600 && vrm<=54799)
  {
    level = 4;
  }
  else if (vrm>=54800 && vrm<=54899)
  {
    level = 5;
  }
  else if (vrm>=54900 && vrm<=54999)
  {
    level = 6;
  }

  if (level == 0 && MINE_DIR(ch) != DOWN)
    return;
  else if (level == 1 && MINE_DIR(ch) == UP)
  {
    new_send_to_char(ch, "You can't seem to break through.\r\n");
    return;
  }

  switch (MINE_DIR(ch))
  {
  case DOWN:
    newroom = mine_shaft[level + (level == 6 ? 0 : 1)].start;
    size = mine_shaft[level + (level == 6 ? 0 : 1)].size;
    break;
  case UP:
    newroom = mine_shaft[level - 1].start;
    size =  mine_shaft[level - 1].size;
    break;
  default:
    newroom = mine_shaft[level].start;
    size = mine_shaft[level].size;
    break;
  }
  newroom += number(0, size - (size/10));
  if (number(0, 110) < MINE_BONUS(ch))
    newroom += size/10;

  if ((rnew = world_vnum[newroom]) == NULL)
    return;

  if (W_EXIT(rnew, rev_dir[MINE_DIR(ch)]))
    pass = FALSE;
  else if (newroom >= 54831 && newroom <= 54899)
    pass = FALSE;
  else if (str_str(rnew->name, "Bridge") || str_str(rnew->name, "Moria") ||
           str_str(rnew->name, "Abyss" ) || str_str(rnew->name, "Bat"  ) ||
           str_str(rnew->name, "Stalactite") || str_str(rnew->name, "Heart") ||
           str_str(rnew->name, "Lake") || str_str(rnew->name, "Cart"))
    pass = FALSE;

  CREATE(exit, struct room_direction_data, 1);
  (IN_ROOM(ch))->dir_option[MINE_DIR(ch)] = exit;
  if (!pass)
  {
    exit->to_room = NULL;
    exit->key = NOTHING;
    exit->exit_info = 0;
    exit->keyword = NULL;
    snprintf(description, sizeof(description), "A cave-in made by %s.\r\n", GET_NAME(ch));
    exit->general_description = str_dup(description);
    new_send_to_char(ch, "As your last strike hits the %s wall it collapses in a cave-in.\r\n", dirs[MINE_DIR(ch)]);
    act("$n's tunneling causes a cave-in!", FALSE, ch, 0, 0, TO_ROOM);
  }
  else
  {
    exit->to_room = rnew;
    exit->key = NOTHING;
    exit->exit_info = 0;
    exit->keyword = NULL;
    snprintf(description, sizeof(description), "A tunnel entrance made by %s.\r\n", GET_NAME(ch));
    exit->general_description = str_dup(description);
    CREATE(exit, struct room_direction_data, 1);
    (rnew)->dir_option[rev_dir[MINE_DIR(ch)]] = exit;
    exit->to_room = IN_ROOM(ch);
    exit->key = NOTHING;
    exit->exit_info = 0;
    exit->keyword = NULL;
    snprintf(description, sizeof(description), "A tunnel entrance made by %s.\r\n", GET_NAME(ch));
    exit->general_description = str_dup(description);
    new_send_to_char(ch, "As your last strike hits the %s wall it opens up a new tunnel.\r\n", dirs[MINE_DIR(ch)]);
    act("$n tunnels a new passage!", FALSE, ch, 0, 0, TO_ROOM);
    send_to_room(rnew, "A passage opens up in the %s wall.\r\n", dirs[rev_dir[MINE_DIR(ch)]]);
  }
  toggle_sub_status(ch, SUB_TUNNELING, STATUS_OFF);
  if (GET_TASK(ch) && GET_TASK(ch)->sub == SUB_TUNNELING)
    stop_task(ch);

}
void mine_damage(struct char_data *vict, int dam)
{
  if (!dam)
    return;
  if (GET_LEVEL(vict)>=LVL_IMMORT && (dam > 0))
  {
    new_send_to_char(vict, "Being the cool immortal you are, you sidestep a trap,\r\n"
                     "obviously placed to kill you.\r\n");
    return;
  }

  alter_hit(vict, dam);

  update_pos(vict);
  send_char_pos(vict, dam);

  if (GET_POS(vict) == POS_DEAD)
  {
    if (!IS_NPC(vict))
      new_mudlog( BRF, 0, TRUE, "%s killed by mining trap at %s [%d]",
                  GET_NAME(vict), IN_ROOM(vict)->name, GET_ROOM_VNUM(IN_ROOM(vict)));
    die(vict, NULL);
  }
}


