/***************************************************************************
                          mine.c  -  description
                             -------------------
    begin                : Tue Mar 16 2004
    copyright            : (C) 2004 by molly
    email                : Mordecai@xtra.co.nz
 ***************************************************************************/
#include "config.h"
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

extern struct time_info_data time_info;
EVENTFUNC(message_event);
int mine_damage(Character *vict, int dam);
void die(Character *ch, Character *killer);
struct mine_list *mine_shafts = NULL;


ASUB(sub_tunneling)
{
  room_rnum rm = IN_ROOM(ch);
  struct message_event_obj *msg = NULL;
  char direction[MAX_INPUT_LENGTH];
  int dir, hard = FALSE, soft = FALSE, density;
  struct obj_data *pri = GET_EQ(ch, WEAR_WIELD), *sec = GET_EQ(ch, WEAR_WIELD_2);

  if (GET_SUB(ch, SUB_TUNNELING) <= 0)
  {
    ch->Send( "You have no idea how to use that command!\r\n");
    return SUB_UNDEFINED;
  }

  if (get_sub_status(ch, SUB_TUNNELING) == STATUS_ON)
  {
    ch->Send( "You stop tunneling.\r\n");
    act("$n stops tunneling.", FALSE, ch, 0, 0, TO_ROOM);
    toggle_sub_status(ch, SUB_TUNNELING, STATUS_OFF);
    if (GET_TASK(ch) && GET_TASK(ch)->sub == SUB_TUNNELING)
      stop_task(ch);
    return SUB_UNDEFINED;
  }



  if ((hard = (rm->mine.tool == TOOL_PICKAXE)))
  {
    if (pri == NULL)
    {
      ch->Send( "You can't mine with out a tool!\r\n");
      return SUB_UNDEFINED;
    }


    if (!(GET_OBJ_TYPE(pri) == ITEM_PICKAXE || (sec && GET_OBJ_TYPE(sec) == ITEM_PICKAXE)))
    {
      ch->Send( "Sorry but you need a pickaxe to tunnel there.\r\n");
      return SUB_UNDEFINED;
    }
  }
  else if ((soft =  (rm->mine.tool == TOOL_SHOVEL) ))
  {

    if (pri == NULL)
    {
      ch->Send( "You can't mine with out a tool!\r\n");
      return SUB_UNDEFINED;
    }


    if (!(GET_OBJ_TYPE(pri) == ITEM_SHOVEL || (sec && GET_OBJ_TYPE(sec) == ITEM_SHOVEL)))
    {
      ch->Send( "Sorry but you need a shovel to tunnel there.\r\n");
      return SUB_UNDEFINED;
    }
  }

  if (!hard && !soft && rm->mine.num == -1)
  {
    ch->Send( "Sorry, this place cannot be tunneled.\r\n");
    return SUB_UNDEFINED;
  }


  one_argument(argument, direction);

  if ((!*direction))
  {
    ch->Send( "Usage: tunnel <direction>\r\n");
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
    ch->Send( "Invalid direction! Directions are: north south east west up down.\r\n");
    return SUB_UNDEFINED;
  }
  if ((dir != DOWN && (rm->mine.num == -1)) || W_EXIT(rm, dir))
  {
    ch->Send( "There is no good mining area that direction.\r\n");
    return SUB_UNDEFINED;
  }

  if (GET_MESSAGE_EVENT(ch)!=NULL)
  {
    ch->Send( "You are in the middle of something else!\r\n");
    return  SUB_UNDEFINED;
  }
  if (GET_MSG_RUN(ch))
  {
    ch->Send( "You are already working on something else!\r\n");
    return  SUB_UNDEFINED;
  }

  if (rm == NULL)
  {
    ch->Send( "Error! Invalid room\r\n");
    return SUB_UNDEFINED;
  }

  density = 2;
  density += (hard ? 3 + number(1, 3) : 0);
  density += (soft ? 1 + number(1, 2) : 0);

  ch->Send( "You begin to tunnel %s.\r\n", dirs[dir]);

  GET_MSG_RUN(ch) = TRUE;
  MINE_DIR(ch) = dir;
  toggle_sub_status(ch, SUB_TUNNELING, STATUS_ON);

  msg = new message_event_obj(ch, SUB_TUNNELING, THING_SUB, density, (long) (rm->number + ROOM_ID_BASE), (char *)"");
  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, (1 RL_SEC), EVENT_TYPE_MESSAGE);
  return SUB_TUNNELING;
}

int check_mine_traps(Character *ch)
{
  int hit = GET_HIT(ch), mhit = GET_MAX_HIT(ch);
  int dam = 0;
  if (number(0, 100))
    return 0;

  act("You dig into and inspect a cunning trap.", FALSE, ch, 0, 0, TO_CHAR);
  act("$n digs into and inspects a cunning trap.", FALSE, ch, 0, 0, TO_ROOM);
  if (number(0, 110) < MINE_STEALTH(ch))
  {
    act("You are relieved with a twanging sound as you disarm the trap.", FALSE, ch, 0, 0, TO_CHAR);
    return 0;
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
    act("You are surrounded by a gas of scintillating colours!", FALSE, ch, 0, 0, TO_CHAR);
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
    dam = (FTOI( dam * ((float)(100.0 - MAX(0, MINE_DAMAGE(ch)))/100.0)));

  return mine_damage(ch, dam);
  
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

void add_room_to_mine(room_rnum room)
{
  room_vnum vrm;
  struct mine_list *shaft;
  struct mine_rooms *mr;
  if (!room)
    return;
  vrm = GET_ROOM_VNUM(room);
  if (room->mine.num == -1 && (vrm < 54000 || vrm > 54999))
    return;
  if ((vrm >= 54000 || vrm <= 54999) && room->mine.num == -1)
  {
    if (str_str(room->name, "bridge") || str_str(room->name, "moria") ||
        str_str(room->name, "abyss" ) || str_str(room->name, "bat"  ) ||
        str_str(room->name, "heart" ) || str_str(room->name, "stalactite") ||
        str_str(room->name, "cart"  ) || str_str(room->name, "lake") ||
        (vrm >= 54831 && vrm <= 54899))
    {
      return ;
    }
    room->mine.num = 0;
    if  (str_str(room->name, "sand") || str_str(room->name, "earth") || str_str(room->name, "soft"))
      room->mine.tool = TOOL_SHOVEL;
    else if (str_str(room->name, "rock") || str_str(room->name, "hard") || str_str(room->name, "water") ||
             str_str(room->name, "mine") || str_str(room->name, "cave") || str_str(room->name, "dwarven"))
      room->mine.tool = TOOL_PICKAXE;
    else
      room->mine.tool = TOOL_SHOVEL;
  }
  if (vrm>=54000 && vrm<=54199)
  {
    room->mine.dif = 0;
  }
  else if (vrm>=54200 && vrm<=54399)
  {
    room->mine.dif = 1;
  }
  else if (vrm>=54400 && vrm<=54599)
  {
    room->mine.dif = 2;
  }
  else if (vrm>=54600 && vrm<=54799)
  {
    room->mine.dif = 3;
  }
  else if (vrm>=54800 && vrm<=54899)
  {
    room->mine.dif = 4;
  }
  else if (vrm>=54900 && vrm<=54999)
  {
    room->mine.dif = 5;
  }




  /** add room to mine list along with all others in the same shaft **/


  for (shaft = mine_shafts; shaft; shaft = shaft->next)
  {
    if (shaft->number == room->mine.num)
    {
      shaft->size++;
      CREATE(mr, struct mine_rooms, 1);
      mr->room = vrm;
      mr->next = shaft->rooms;
      shaft->rooms = mr;
      return;
    }
  }

  CREATE(shaft, struct mine_list, 1);
  shaft->size=1;
  shaft->number = room->mine.num;
  shaft->rooms = NULL;
  shaft->next = mine_shafts;
  mine_shafts = shaft;
  CREATE(mr, struct mine_rooms, 1);
  mr->room = vrm;
  mr->next = shaft->rooms;
  shaft->rooms = mr;
  return;



}
void free_shaft_list(struct mine_rooms *mine)
{
  if (!mine)
    return;
  if (mine->next)
    free_shaft_list(mine->next);
  free(mine);
}
void free_mine_shafts(void)
{
  struct mine_list *shaft, *next;
  for (shaft = mine_shafts; shaft; shaft = next)
  {
    next = shaft->next;
    free_shaft_list(shaft->rooms);
    free(shaft);
  }
}

room_vnum find_mine_room(int num, int dif)
{
  struct mine_list *shaft;
  struct mine_rooms *mr;
  for (shaft = mine_shafts; shaft; shaft = shaft->next)
  {
    if (shaft->number == num)
    {
      int rnd = 0;
      //log("Shaft Number: %d", num);
      for (mr = shaft->rooms;mr;mr = mr->next)
        if (dif == world_vnum[mr->room]->mine.dif)
          rnd++;
      //log("On level %d, %d rooms were found.", dif, rnd);
      if (rnd)
        rnd = number(1, rnd);
      //log("Room %d was selected.", rnd);
      for (mr = shaft->rooms;mr && rnd;mr = mr->next)
        if (dif == world_vnum[mr->room]->mine.dif)
          rnd--;

      if (mr)
        return mr->room;
      else
        log("find_mine_room couldn't find any rooms of difficulty %d in mine %d", dif, num);
    }
  }
  return -1;
}

void make_tunnel(Character *ch)
{
  room_vnum newroom = NOWHERE;
  room_rnum rnew = NULL;
  int pass = TRUE, level = 0;
  char description[MAX_INPUT_LENGTH];
  struct room_direction_data *exit;


  if (IN_ROOM(ch)->mine.dif == -1)
    level = 1;
  else
    level = IN_ROOM(ch)->mine.dif+1;

  if (level == 0 && MINE_DIR(ch) != DOWN)
  {
    ch->Send( "You can't mine there.\r\n");
    return;
  }
  else if (level == 1 && MINE_DIR(ch) == UP)
  {
    ch->Send( "You can't seem to break through.\r\n");
    return;
  }
  else if (level == 6 && MINE_DIR(ch) == DOWN)
  {
    ch->Send( "You can't mine there.\r\n");
    return;
  }

  switch (MINE_DIR(ch))
  {
  case DOWN:
    newroom = find_mine_room(IN_ROOM(ch)->mine.num, IN_ROOM(ch)->mine.dif + 1);
    break;
  case UP:
    newroom = find_mine_room(IN_ROOM(ch)->mine.num, IN_ROOM(ch)->mine.dif - 1);
    break;
  default:
    newroom = find_mine_room(IN_ROOM(ch)->mine.num, IN_ROOM(ch)->mine.dif);
    break;
  }

  if (newroom == -1 || (rnew = world_vnum[newroom]) == NULL)
    return;

  if (W_EXIT(rnew, rev_dir[MINE_DIR(ch)]))
    pass = FALSE;
  /*
  else if (newroom >= 54831 && newroom <= 54899)
    pass = FALSE;
  else if (str_str(rnew->name, "Bridge") || str_str(rnew->name, "Moria") ||
           str_str(rnew->name, "Abyss" ) || str_str(rnew->name, "Bat"  ) ||
           str_str(rnew->name, "Stalactite") || str_str(rnew->name, "Heart") ||
           str_str(rnew->name, "Lake") || str_str(rnew->name, "Cart"))
    pass = FALSE;
    */

  CREATE(exit, struct room_direction_data, 1);
  (IN_ROOM(ch))->dir_option[MINE_DIR(ch)] = exit;
  if (!pass)
  {
    exit->to_room = NULL;
    exit->key = NOTHING;
    exit->exit_info = 0;
    exit->keyword = NULL;
    exit->nosave = 1;
    snprintf(description, sizeof(description), "A cave-in made by %s.\r\n", GET_NAME(ch));
    exit->general_description = str_dup(description);
    ch->Send( "As your last strike hits the %s wall it collapses in a cave-in.\r\n", dirs[MINE_DIR(ch)]);
    act("$n's tunneling causes a cave-in!", FALSE, ch, 0, 0, TO_ROOM);
  }
  else
  {
    exit->to_room = rnew;
    exit->key = NOTHING;
    exit->exit_info = 0;
    exit->keyword = NULL;
    exit->nosave = 1;
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
    ch->Send( "As your last strike hits the %s wall it opens up a new tunnel.\r\n", dirs[MINE_DIR(ch)]);
    act("$n tunnels a new passage!", FALSE, ch, 0, 0, TO_ROOM);
    send_to_room(rnew, "A passage opens up in the %s wall.\r\n", dirs[rev_dir[MINE_DIR(ch)]]);
    log("%s creates a tunnel.", GET_NAME(ch));
  }
  toggle_sub_status(ch, SUB_TUNNELING, STATUS_OFF);
  if (GET_TASK(ch) && GET_TASK(ch)->sub == SUB_TUNNELING)
    stop_task(ch);

}

