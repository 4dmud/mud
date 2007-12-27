/**************************************************************************
*  File: wldcmd.c                                                         *
*  Usage: contains the command_interpreter for rooms,                     *
*         room commands.                                                  *
*                                                                         *
*                                                                         *
*  $Author: w4dimenscor $
*  $Date: 2005/02/25 07:33:47 $
*  $Revision: 1.5 $
**************************************************************************/

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "screen.h"
#include "dg_scripts.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "constants.h"


void die(struct char_data *ch, struct char_data *killer);
zone_rnum real_zone_by_thing(room_vnum vznum);
bitvector_t asciiflag_conv(char *flag);

#define WCMD(name)  \
    void (name)(room_data *room, char *argument, int cmd, int subcmd)
void wld_log(room_data *room, const char *format, ...);
void act_to_room(char *str, room_data *room);
WCMD(do_wasound);
WCMD(do_wecho);
WCMD(do_wsend);
WCMD(do_wzoneecho);
WCMD(do_wrecho);
WCMD(do_wdoor);
WCMD(do_wteleport);
WCMD(do_wforce);
WCMD(do_wpurge);
WCMD(do_wload);
WCMD(do_wdamage);
WCMD(do_wat);
void wld_command_interpreter(room_data *room, char *argument);

struct wld_command_info
{
  char *command;
  void (*command_pointer)
  (room_data * room, char *argument, int cmd, int subcmd);
  int subcmd;
};


/* do_wsend */
#define SCMD_WSEND        0
#define SCMD_WECHOAROUND  1



/* attaches room vnum to msg and sends it to script_log */
void wld_log(room_data *room, const char *format, ...)
{
  va_list args;
  char output[MAX_STRING_LENGTH];

  snprintf(output, sizeof(output), "Room %d :: %s", room->number, format);

  va_start(args, format);
  script_vlog(output, args);
  va_end(args);
}

/* sends str to room */
void act_to_room(char *str, room_data *room)
{
  /* no one is in the room */
  if (!room->people)
    return;

  /*
   * since you can't use act(..., TO_ROOM) for an room, send it
   * TO_ROOM and TO_CHAR for some char in the room.
   * (just dont use $n or you might get strange results)
   */
  act(str, FALSE, room->people, 0, 0, TO_ROOM);
  act(str, FALSE, room->people, 0, 0, TO_CHAR);
}




/* World commands */

/* prints the argument to all the rooms aroud the room */
WCMD(do_wasound)
{
  int door;

  skip_spaces(&argument);

  if (!*argument)
  {
    wld_log(room, "wasound called with no argument");
    return;
  }

  for (door = 0; door < NUM_OF_DIRS; door++)
  {
    struct room_direction_data *exit;

    if ((exit = room->dir_option[door]) && (exit->to_room != NULL)
        && room != exit->to_room)
      act_to_room(argument, exit->to_room);
  }
}
WCMD(do_wlag)
{
  char_data *victim;
  int wait = 0;
  //int room;
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

  two_arguments(argument, arg1, arg2);

  if (!*arg1 || !*arg2)
  {
    wld_log(room, "wlag called with too few args");
    return;
  }



  if (*arg1 == UID_CHAR)
  {
    if (!(victim = get_char(arg1)))
    {
      wld_log(room, "wlag: victim (%s) not found", arg1);
      return;
    }
  }
  else if (!(victim = get_char_by_room(room, arg1)))
  {
    wld_log(room, "wlag: victim (%s) not found", arg1);
    return;
  }



  if (!IS_NPC(victim) && PRF_FLAGGED(victim, PRF_NOHASSLE))
  {
    wld_log(room, "wlag: target has nohassle on");
    return;
  }

  if ((wait = atoi(arg2)) < 1)
    return;

  if (wait > 300)
  {
    wld_log(room,"wlag: duration longer then 30 seconds outside range.");
    return;
  }



  wait = (wait RL_SEC)*0.1;

  WAIT_STATE(victim, wait);
  return;
}

WCMD(do_wecho)
{
  skip_spaces(&argument);

  if (!*argument)
    wld_log(room, "wecho called with no args");

  else
    act_to_room(argument, room);
}


WCMD(do_wsend)
{
  char buf[MAX_INPUT_LENGTH], *msg;
  char_data *ch;

  msg = any_one_arg(argument, buf);

  if (!*buf)
  {
    wld_log(room, "wsend called with no args");
    return;
  }

  skip_spaces(&msg);

  if (!*msg)
  {
    wld_log(room, "wsend called without a message");
    return;
  }

  if ((ch = get_char_by_room(room, buf)))
  {
    if (subcmd == SCMD_WSEND)
      sub_write(msg, ch, TRUE, TO_CHAR);
    else if (subcmd == SCMD_WECHOAROUND)
    {
      if (IN_ROOM(ch) != NULL)
      {
        sub_write(msg, ch, TRUE, TO_ROOM);
      }
      else
      {
        wld_log(room, "calling wechoaround when %s is in nowhere", GET_NAME(ch));
      }
    }

  }
  else
  {
    wld_log(room, "no target found for wsend");
    wld_log(room, msg);
  }
}

/*int real_zone(int number)
{
  int counter;
      
  for (counter = 0; counter <= top_of_zone_table; counter++)
    if ((number >= (zone_table[counter].number * 100)) &&
	(number <= (zone_table[counter].top)))
      return counter;
 
  return -1;
}*/

WCMD(do_wzecho)
{
  int zone;
  char zone_name[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;

  msg = any_one_arg(argument, zone_name);
  skip_spaces(&msg);

  log("zone_name: %s, msg: %s.", zone_name, msg);

  if (!*zone_name || !*msg)
    wld_log(room, "wzoneecho called with too few args");

  else if ((zone = real_zone(atoi(zone_name))) < 0)
    wld_log(room, "wzoneecho called for nonexistant zone");

  else
  {
    log("zone: %d.", zone);
    snprintf(buf, sizeof(buf), "%s\r\n", msg);
    send_to_zone(buf, zone);
  }
}

WCMD(do_wzrecho)
{
  int zone, lower_vnum, upper_vnum;
  char zone_name[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;
  char lower[MAX_INPUT_LENGTH], upper[MAX_INPUT_LENGTH];

  msg = any_one_arg(argument, zone_name);
  msg = two_arguments(msg, lower, upper);

  skip_spaces(&msg);

  if (!*zone_name || !*msg || !*lower || !*upper)
    wld_log(room, "wzrecho called with too few args");


  else if ((zone = real_zone(atoi(zone_name))) < 0)
    wld_log(room, "wzrecho called for nonexistant zone");

  else
  {
    lower_vnum = atoi(lower);
    upper_vnum = atoi(upper);

    sprintf(buf, "%s\r\n", msg);
    send_to_zone_range(buf, zone, lower_vnum, upper_vnum);
  }
}

/* prints the message to everyone in the range of numbers */
WCMD(do_wrecho)
{
  char start[MAX_INPUT_LENGTH], finish[MAX_INPUT_LENGTH], *msg;

  msg = two_arguments(argument, start, finish);

  skip_spaces(&msg);

  if (!*msg || !*start || !*finish)
    wld_log(room, "wrecho called with too few args");
  else
    send_to_range(atoi(start), atoi(finish), "%s\r\n", msg);

}

WCMD(do_wdoor)
{
  char target[MAX_INPUT_LENGTH], direction[MAX_INPUT_LENGTH];
  char field[MAX_INPUT_LENGTH], *value;
  room_data *rm;
  struct room_direction_data *exit;
  int dir, fd;
  room_rnum to_room;

  const char *door_field[] =
    {
      "purge",
      "description",
      "flags",
      "key",
      "name",
      "room",
      "\n"
    };


  argument = two_arguments(argument, target, direction);
  value = one_argument(argument, field);
  skip_spaces(&value);

  if (!*target || !*direction || !*field)
  {
    wld_log(room, "wdoor called with too few args");
    return;
  }

  if ((rm = get_room(target)) == NULL)
  {
    wld_log(room, "wdoor: invalid target");
    return;
  }

  if ((dir = search_block(direction, dirs, FALSE)) == -1)
  {
    wld_log(room, "wdoor: invalid direction");
    return;
  }

  if ((fd = search_block(field, door_field, FALSE)) == -1)
  {
    wld_log(room, "wdoor: invalid field");
    return;
  }

  exit = rm->dir_option[dir];

  /* purge exit */
  if (fd == 0)
  {
    if (exit)
    {
      if (exit->general_description)
        free(exit->general_description);
      if (exit->keyword)
        free(exit->keyword);
      free(exit);
      rm->dir_option[dir] = NULL;
    }
  }

  else
  {
    if (!exit)
    {
      CREATE(exit, struct room_direction_data, 1);
      rm->dir_option[dir] = exit;
    }

    switch (fd)
    {
    case 1:		/* description */
      if (exit->general_description)
        free(exit->general_description);
      CREATE(exit->general_description, char, strlen(value) + 3);
      strcpy(exit->general_description, value);
      strcat(exit->general_description, "\r\n");
      break;
    case 2:		/* flags       */
      exit->exit_info = (sh_int) asciiflag_conv(value);
      break;
    case 3:		/* key         */
      exit->key = atoi(value);
      break;
    case 4:		/* name        */
      if (exit->keyword)
        free(exit->keyword);
      CREATE(exit->keyword, char, strlen(value) + 1);
      strcpy(exit->keyword, value);
      break;
    case 5:		/* room        */
      if ((to_room = real_room(atoi(value))) != NULL)
        exit->to_room = to_room;
      else
        wld_log(room, "wdoor: invalid door target");
      break;
    }
  }
}


WCMD(do_wteleport)
{
  char_data *ch, *next_ch;
  room_rnum target;
  room_vnum nr;
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

  two_arguments(argument, arg1, arg2);

  if (!*arg1 || !*arg2)
  {
    wld_log(room, "wteleport called with too few args");
    return;
  }

  nr = atoi(arg2);
  target = real_room(nr);

  if (target == NULL)
    wld_log(room, "wteleport target is an invalid room (vnum: %s)", arg2);

  else if (!str_cmp(arg1, "all"))
  {
    if (nr == room->number)
    {
      wld_log(room, "wteleport all target is itself");
      return;
    }

    for (ch = room->people; ch; ch = next_ch)
    {
      next_ch = ch->next_in_room;
      if (!valid_dg_target(ch, TRUE))
        continue;
      char_from_room(ch);
      char_to_room(ch, target);
      enter_wtrigger(IN_ROOM(ch), ch, -1);
    }
  }

  else
  {
    if ((ch = get_char_by_room(room, arg1)))
    {
      if (valid_dg_target(ch, TRUE))
      {
        char_from_room(ch);
        char_to_room(ch, target);
        enter_wtrigger(IN_ROOM(ch), ch, -1);
      }
    }
    else
      wld_log(room, "wteleport: no target found");
  }
}


WCMD(do_wforce)
{
  char_data *ch, *next_ch;
  char arg1[MAX_INPUT_LENGTH], *line;

  line = one_argument(argument, arg1);

  if (!*arg1 || !*line)
  {
    wld_log(room, "wforce called with too few args");
    return;
  }

  if (!str_cmp(arg1, "all"))
  {
    for (ch = room->people; ch; ch = next_ch)
    {
      next_ch = ch->next_in_room;

      if (valid_dg_target(ch, FALSE))
        command_interpreter(ch, line);
    }
  }
  else
  {
    if ((ch = get_char_by_room(room, arg1)))
    {
      if (valid_dg_target(ch, FALSE))
      {
        command_interpreter(ch, line);
      }
    }
    else
      wld_log(room, "wforce: no target found");
  }
}



/* purge all objects an npcs in room, or specified object or mob */
WCMD(do_wpurge)
{
  char arg[MAX_INPUT_LENGTH];
  char_data *ch, *next_ch;
  obj_data *obj, *next_obj;

  one_argument(argument, arg);

  if (!*arg)
  {
    /* purge all */
    for (ch = room->people; ch; ch = next_ch )
    {
      next_ch = ch->next_in_room;
      if (IS_NPC(ch))
        extract_char(ch);
    }

    for (obj = room->contents; obj; obj = next_obj )
    {
      next_obj = obj->next_content;
      extract_obj(obj);
    }

    return;
  }

  if (*arg == UID_CHAR)
    ch = get_char(arg);
  else
    ch = get_char_in_room(room, arg);
  if (!ch)
  {
    if (*arg == UID_CHAR)
      obj = get_obj(arg);
    else
      obj = get_obj_in_room(room, arg);
    if (obj)
    {
      extract_obj(obj);
    }
    else
      wld_log(room, "wpurge: bad argument");

    return;
  }

  if (!IS_NPC(ch))
  {
    wld_log(room, "wpurge: purging a PC");
    return;
  }

  extract_char(ch);
}


/* loads a mobile or object into the room */
WCMD(do_wload)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
  int number = 0;
  char_data *mob;
  obj_data *object;
  char *target;
  char_data *tch;
  obj_data *cnt;
  int pos;


  target = two_arguments(argument, arg1, arg2);
  skip_spaces(&target);

  if (!*arg1 || !*arg2 || !is_number(arg2)
      || ((number = atoi(arg2)) < 0))
  {
    wld_log(room, "wload: bad syntax");
    return;
  }
  /* load mob to target room - Jamie Nelson, April 13 2004 */
  if (is_abbrev(arg1, "mob"))
  {
    room_rnum rnum;
    if (!target || !*target)
      rnum = room;
    else
    {
      if (!isdigit(*target) || (rnum = real_room(atoi(target))) == NULL)
      {
        wld_log(room, "wload: room target vnum doesn't exist (loading mob vnum %d to room %s)", number, target);
        return;
      }
    }
    if ((mob = read_mobile(number, VIRTUAL)) == NULL)
    {
      wld_log(room, "mload: bad mob vnum");
      return;

    }
    char_to_room(mob, rnum);
    load_mtrigger(mob);
    if (SCRIPT(room))
    { // it _should_ have, but it might be detached.
      char buf[MAX_INPUT_LENGTH];
      sprintf(buf, "%c%ld", UID_CHAR, GET_ID(mob));
      add_var(&(SCRIPT(room)->global_vars), "lastloaded", buf, 0);
    }
  }


  else if (is_abbrev(arg1, "obj"))
  {
    object = read_object(number, VIRTUAL);
    if (object == NULL)
    {
      wld_log(room, "wload: bad object vnum");
      return;
    }
    if (SCRIPT(room))
    { // it _should_ have, but it might be detached.
      char buf[MAX_INPUT_LENGTH];
      sprintf(buf, "%c%ld", UID_CHAR, GET_ID(object));
      add_var(&(SCRIPT(room)->global_vars), "loaded", buf, 0);
    }
    /* special handling to make objects able to load on a person/in a container/worn etc. */
    if (!target || !*target)
    {
      obj_to_room(object, real_room(room->number));
      load_otrigger(object);
      return;
    }

    two_arguments(target, arg1, arg2); /* recycling ... */
    tch = get_char_in_room(room, arg1);
    if (tch)
    {
      if (arg2 && *arg2 &&
          (pos = find_eq_pos_script(arg2)) >= 0 &&
          !GET_EQ(tch, pos) &&
          can_wear_on_pos(object, pos))
      {
        equip_char(tch, object, pos);
        load_otrigger(object);
        return;
      }
      obj_to_char(object, tch);
      load_otrigger(object);
      return;
    }
    cnt = get_obj_in_room(room, arg1);
    if (cnt && GET_OBJ_TYPE(cnt) == ITEM_CONTAINER)
    {
      obj_to_obj(object, cnt);
      load_otrigger(object);
      return;
    }
    /* neither char nor container found - just dump it in room */
    obj_to_room(object, real_room(room->number));
    load_otrigger(object);
    return;
  }

  else
    wld_log(room, "wload: bad type");
}

WCMD(do_wdamage)
{
  char name[MAX_INPUT_LENGTH], amount[MAX_INPUT_LENGTH];
  int dam = 0;
  char_data *ch;

  two_arguments(argument, name, amount);

  /* who cares if it's a number ? if not it'll just be 0 */
  if (!*name || !*amount)
  {
    wld_log(room, "wdamage: bad syntax");
    return;
  }

  dam = atoi(amount);
  ch = get_char_by_room(room, name);

  if (!ch)
  {
    wld_log(room, "wdamage: target not found");
    return;
  }

  script_damage(ch, dam);
}

WCMD(do_wat)
{
  char location[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
  int vnum = 0;

  half_chop(argument, location, arg2);

  if (!*location || !*arg2 || !isdigit(*location))
  {
    wld_log(room, "wat: bad syntax");
    return;
  }
  vnum = atoi(location);
  if (NULL == real_room(vnum))
  {
    wld_log(room, "wat: location not found");
    return;
  }

  wld_command_interpreter(world_vnum[vnum], arg2);
}

const struct wld_command_info wld_cmd_info[] =
  {
    {"RESERVED", 0, 0}
    ,		/* this must be first -- for specprocs */

    {"wasound ", do_wasound, 0},
    {"wdoor ", do_wdoor, 0},
    {"wecho ", do_wecho, 0},
    {"wechoaround ", do_wsend, SCMD_WECHOAROUND},
    {"wforce ", do_wforce, 0},
    {"wload ", do_wload, 0},
    {"wpurge ", do_wpurge, 0},
    {"wsend ", do_wsend, SCMD_WSEND},
    {"wteleport ", do_wteleport, 0},
    {"wzecho ", do_wzecho, 0},
    { "wrecho "     , do_wrecho    , 0 },
    {"wzrecho ", do_wzrecho, 0},
    {"wdamage ", do_wdamage, 0},
    {"wat ", do_wat, 0},
    {"wlag", do_wlag, 0},
    {"\n", 0, 0}		/* this must be last */
  };


/*
 *  This is the command interpreter used by rooms, called by script_driver.
 */
void wld_command_interpreter(room_data * room, char *argument)
{
  int cmd, length;
  char *line, arg[MAX_INPUT_LENGTH];

  skip_spaces(&argument);

  /* just drop to next line for hitting CR */
  if (!*argument)
    return;

  line = any_one_arg(argument, arg);


  /* find the command */
  for (length = strlen(arg), cmd = 0;
       *wld_cmd_info[cmd].command != '\n'; cmd++)
    if (!strncmp(wld_cmd_info[cmd].command, arg, length))
      break;

  if (*wld_cmd_info[cmd].command == '\n')
  {
    wld_log(room, "Unknown world cmd: '%s'", argument);
  }
  else
    ((*wld_cmd_info[cmd].command_pointer)
        (room, line, cmd, wld_cmd_info[cmd].subcmd));
}
