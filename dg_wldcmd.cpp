/**************************************************************************
*  File: wldcmd.c                                                         *
*  Usage: contains the command_interpreter for rooms,                     *
*         room commands.                                                  *
*                                                                         *
*                                                                         *
*  $Author: w4dimenscor $
*  $Date: 2007/08/19 01:06:10 $
*  $Revision: 1.15 $
**************************************************************************/

#include "config.h"
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


void die(Character *ch, Character *killer);
zone_rnum real_zone_by_thing(room_vnum vznum);

#define WCMD(name)  \
void (name)(Room *room, char *argument, int cmd, int subcmd)

int followers_to_master(Character *ch, room_rnum was_in);
void wld_log(Room *room, const char *format, ...);
void act_to_room(char *str, Room *room);
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
void wld_command_interpreter(Room *room, char *argument);

struct wld_command_info
{
  const char *command;
  void (*command_pointer)
  (Room * room, char *argument, int cmd, int subcmd);
  int subcmd;
};


/* do_wsend */
#define SCMD_WSEND        0
#define SCMD_WECHOAROUND  1



/* attaches room vnum to msg and sends it to script_log */
void wld_log(Room *room, const char *format, ...)
{
  va_list args;
  char buf[MAX_INPUT_LENGTH];

  snprintf(buf, sizeof(buf), "Room %d :: %s", room->number, format);

  va_start(args, format);
  script_vlog(buf, args);
  va_end(args);
}

/* sends str to room */
void act_to_room(char *str, Room *room)
{
  /* no one is in the room */
  if (!room->people)
    return;

  /*
   * since you can't use act(..., TO_ROOM) for an room, send it
   * TO_ROOM and TO_CHAR for some char in the room.
   * (just don't use $n or you might get strange results)
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
    struct room_direction_data *ex;

    if ((ex = room->dir_option[door]) && (ex->to_room != NULL)
        && room != ex->to_room)
      act_to_room(argument, ex->to_room);
  }
}
WCMD(do_wlag)
{
  Character *victim;
  int w = 0;
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

  if ((w = atoi(arg2)) < 1)
    return;

  if (w > 300)
  {
    wld_log(room,"wlag: duration longer than 30 seconds outside range.");
    return;
  }



  w = FTOI((w RL_SEC)*0.1);

  WAIT_STATE(victim, w);
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
  Character *ch;

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
  char strlower[MAX_INPUT_LENGTH], strupper[MAX_INPUT_LENGTH];

  msg = any_one_arg(argument, zone_name);
  msg = two_arguments(msg, strlower, strupper);

  skip_spaces(&msg);

  if (!*zone_name || !*msg || !*strlower || !*strupper)
    wld_log(room, "wzrecho called with too few args");


  else if ((zone = real_zone(atoi(zone_name))) < 0)
    wld_log(room, "wzrecho called for nonexistant zone");

  else
  {
    lower_vnum = atoi(strlower);
    upper_vnum = atoi(strupper);

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
  Room *rm;
  struct room_direction_data *ex;
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

  ex = rm->dir_option[dir];

  /* purge exit */
  if (fd == 0)
  {
    if (ex)
    {
      if (ex->general_description)
        free(ex->general_description);
      if (ex->keyword)
        free(ex->keyword);
      free(ex);
      rm->dir_option[dir] = NULL;
    }
  }

  else
  {
    if (!ex)
    {
      CREATE(ex, struct room_direction_data, 1);
      rm->dir_option[dir] = ex;
    }

    switch (fd)
    {
    case 1:		/* description */
      if (ex->general_description)
        free(ex->general_description);
      CREATE(ex->general_description, char, strlen(value) + 3);
      strcpy(ex->general_description, value);
      strcat(ex->general_description, "\r\n");
      break;
    case 2:		/* flags       */
      ex->exit_info = (sh_int) asciiflag_conv(value);
      break;
    case 3:		/* key         */
      ex->key = atoi(value);
      break;
    case 4:		/* name        */
      if (ex->keyword)
        free(ex->keyword);
      CREATE(ex->keyword, char, strlen(value) + 1);
      strcpy(ex->keyword, value);
      break;
    case 5:		/* room        */
      if ((to_room = real_room(atoi(value))) != NULL)
        ex->to_room = to_room;
      else
        wld_log(room, "wdoor: invalid door target");
      break;
    }
  }
}


WCMD(do_wteleport)
{
  Character *ch, *next_ch, *target_char = nullptr;
  obj_data *obj, *target_container = nullptr;
  room_rnum target_room = nullptr, was_in;
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

  argument = two_arguments(argument, arg1, arg2);
  skip_spaces(&argument);

  if (!*arg1 || !*arg2)
  {
    wld_log(room, "wteleport called with too few args: %s, %s", arg1, arg2);
    return;
  }

  target_room = get_room (arg2);
  if (target_room == nullptr)
  {
    target_char = get_char_by_room (room, arg2);
    if (target_char == nullptr)
      target_container = get_obj_by_room (room, arg2);
  }

  if (!str_cmp(arg1, "all"))
  {
    if (target_room == nullptr)
    {
      wld_log(room, "wteleport target is an invalid room (vnum: %s)", arg2);
      return;
    }

    for (ch = room->people; ch; ch = next_ch)
    {
      next_ch = ch->next_in_room;
      if (!valid_dg_target(ch, TRUE))
        continue;
      char_from_room(ch);
      char_to_room(ch, target_room);
      entry_memory_mtrigger(ch);
      greet_mtrigger(ch, -1);
      greet_memory_mtrigger(ch);
      enter_wtrigger(IN_ROOM(ch), ch, -1);
    }
  }
  else if ((ch = get_char_by_room(room, arg1)))
  {
    if (target_room == nullptr)
    {
      wld_log(room, "wteleport target is an invalid room (vnum: %s)", arg2);
      return;
    }

    if (valid_dg_target(ch, TRUE))
    {
      was_in = IN_ROOM(ch);
      char_from_room(ch);
      char_to_room(ch, target_room);
      entry_memory_mtrigger(ch);
      greet_mtrigger(ch, -1);
      greet_memory_mtrigger(ch);
      enter_wtrigger(IN_ROOM(ch), ch, -1);
      if (isname(argument, "followers"))
        followers_to_master(ch, was_in);
    }
  }
  else if ((obj = get_obj_by_room (room, arg1)))
  {
    if (target_room || target_char || target_container)
    {
      if (IN_ROOM (obj))
        obj_from_room (obj);
      else if (obj->in_obj)
        obj_from_obj (obj);
      else if (obj->carried_by)
        obj_from_char (obj);
      else if (obj->worn_by)
        unequip_char (obj->worn_by, obj->worn_on);
    }

    if (target_room)
      obj_to_room (obj, target_room);
    else if (target_char)
      obj_to_char (obj, target_char);
    else if (target_container)
      obj_to_obj (obj, target_container);
    else
      wld_log(room, "wteleport: no target found (%s, %s)", arg1, arg2);
  }
  else
    wld_log(room, "wteleport: no target found (%s, %s)", arg1, arg2);
}


WCMD(do_wforce)
{
  Character *ch, *next_ch;
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



/* purge all objects and npcs in room, or specified object or mob */
/* don't purge objects in a house, crashproof objects in a room, or player corpses */
WCMD(do_wpurge)
{
  char arg[MAX_INPUT_LENGTH];
  Character *ch, *next_ch;
  obj_data *obj, *next_obj;

  one_argument(argument, arg);

  if (!*arg)
  {
    /* purge all */
    for (ch = room->people; ch; ch = next_ch )
    {
      next_ch = ch->next_in_room;
      if (IS_NPC(ch) && !DEAD(ch))
        extract_char(ch);
    }

    if ( ROOM_FLAGGED ( room, ROOM_HOUSE ) )
      return;

    for (obj = room->contents; obj; obj = next_obj )
    {
      next_obj = obj->next_content;
      if ( !OBJ_FLAGGED ( obj, ITEM_CRASHPROOF ) && !OBJ_FLAGGED ( obj, ITEM_PC_CORPSE ) )
        extract_obj(obj);
    }

    return;
  }

  if (*arg == UID_CHAR)
    ch = get_char(arg);
  else if ( !strcmp ( arg, "target_not_found" ) )
    return;
  else
    ch = get_char_in_room(room, arg);
  if (!ch)
  {
    if (*arg == UID_CHAR)
      obj = get_obj(arg);
    else
      obj = get_obj_in_room(room, arg);

    if ( obj && !( IN_ROOM ( obj ) && ( OBJ_FLAGGED ( obj, ITEM_CRASHPROOF ) || ROOM_FLAGGED ( IN_ROOM ( obj ), ROOM_HOUSE ) ) ) && !OBJ_FLAGGED ( obj, ITEM_PC_CORPSE ) )
      extract_obj(obj);
    else
      wld_log(room, "wpurge: bad argument");

    return;
  }

  if (!IS_NPC(ch))
  {
    wld_log(room, "wpurge: purging a PC");
    return;
  }

  if (!DEAD(ch))
    extract_char(ch);
}


/* loads a mobile or object into the room */
WCMD(do_wload)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
  int num = 0;
  Character *mob;
  obj_data *object;
  char *target;
  Character *tch;
  obj_data *cnt;
  int pos;


  target = two_arguments(argument, arg1, arg2);
  skip_spaces(&target);

  if (!*arg1 || !*arg2 || !is_number(arg2)
      || ((num = atoi(arg2)) < 0))
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
        wld_log(room, "wload: room target vnum doesn't exist (loading mob vnum %d to room %s)", num, target);
        return;
      }
    }
    if ((mob = read_mobile(num)) == NULL)
    {
      wld_log(room, "wload: bad mob vnum");
      return;

    }
    char_to_room(mob, rnum);
    load_mtrigger(mob);
    if (SCRIPT(room))
    { // it _should_ have, but it might be detached.
      char buf[MAX_INPUT_LENGTH];
      sprintf(buf, "%c%ld", UID_CHAR, GET_ID(mob));
      add_var(&(SCRIPT(room)->global_vars), "loaded", buf, 0);
    }
  }


  else if (is_abbrev(arg1, "obj"))
  {
    object = read_object(num, VIRTUAL);
    if (object == NULL)
    {
      wld_log ( room, "wload: bad object vnum %d", num );
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
      if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
          wld_log ( room, "[TOKEN] loads %s", object->short_description );
      obj_to_room(object, room);
      load_otrigger(object);
      return;
    }

    two_arguments(target, arg1, arg2); /* recycling ... */

    /* load to char */
    tch = ( *arg1 == UID_CHAR ) ? get_char ( arg1 ) : get_char_room ( arg1, NULL, room );
    if (tch)
    {
      if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
          wld_log ( room, "[TOKEN] loads %s to %s", object->short_description, GET_NAME ( tch ) );

      if (*arg2 && (pos = find_eq_pos_script(arg2)) >= 0 && !GET_EQ(tch, pos) && can_wear_on_pos(object, pos))
        equip_char(tch, object, pos);
      else
        obj_to_char(object, tch);
      load_otrigger(object);
      return;
    }

    /* load to container */
    cnt = ( *arg1 == UID_CHAR ) ? get_obj ( arg1 ) : get_obj_in_list ( arg1, room->contents );
    if (cnt && GET_OBJ_TYPE(cnt) == ITEM_CONTAINER)
    {
      if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
          wld_log ( room, "[TOKEN] loads %s to %s", object->short_description, cnt->short_description );
      obj_to_obj(object, cnt);
      load_otrigger(object);
      return;
    }

    /* load to room */
    Room *r = NULL;
    if ( ( r = get_room ( arg1 ) ) != NULL )
    {
        if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
            wld_log ( room, "[TOKEN] loads %s to room %d", object->short_description, GET_ROOM_VNUM ( r ) );
        obj_to_room ( object, r );
        load_otrigger(object);
    }
    else
    {
        if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
            wld_log ( room, "[TOKEN] loads %s, but target %s couldn't be found, purging.", object->short_description, arg1 );
        else
            wld_log ( room, "loads %s, but target %s couldn't be found, purging.", object->short_description, arg1 );
        extract_obj ( object );
    }
    return;
  }

  else
    wld_log ( room, "wload: bad type %s", argument );
}

WCMD(do_wdamage)
{
  char name[MAX_INPUT_LENGTH], amount[MAX_INPUT_LENGTH];
  int dam = 0;
  Character *ch;

  two_arguments(argument, name, amount);

  /* who cares if it's a number ? if not it'll just be 0 */
  if (!*name || !*amount)
  {
    wld_log(room, "wdamage: bad syntax");
    return;
  }

  dam = atoi(amount);

  if (!str_cmp("all", name))
  {
    Character *tvict, *vict;
    if ((room) == NULL)
      return;
    /**TODO: I hate this loop, because it is possable that on the extraction
             of a mob or player after damage, it could wipe the next char.
      **/
    for (vict = room->people;vict;vict = tvict)
    {
        if (!DEAD(vict))
            tvict = vict->next_in_room;
        else
            tvict = NULL;

        if (!IS_NPC(vict))
            script_damage(vict, dam);
    }
    return;
  }

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
void wld_command_interpreter(Room * room, char *argument)
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
