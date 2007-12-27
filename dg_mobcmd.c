/***************************************************************************
 *  Original Diku Mud copyright (C) 1990, 1991 by Sebastian Hammer,        *
 *  Michael Seifert, Hans Henrik St{rfeldt, Tom Madsen, and Katja Nyboe.   *
 *                                                                         *
 *  Merc Diku Mud improvments copyright (C) 1992, 1993 by Michael          *
 *  Chastain, Michael Quan, and Mitchell Tse.                              *
 *                                                                         *
 *  In order to use any part of this Merc Diku Mud, you must comply with   *
 *  both the original Diku license in 'license.doc' as well the Merc       *
 *  license in 'license.txt'.  In particular, you may not remove either of *
 *  these copyright notices.                                               *
 *                                                                         *
 *  Much time and thought has gone into this software and you are          *
 *  benefitting.  We hope that you share your changes too.  What goes      *
 *  around, comes around.                                                  *
 ***************************************************************************/

/***************************************************************************
 *  The MOBprograms have been contributed by N'Atas-ha.  Any support for   *
 *  these routines should not be expected from Merc Industries.  However,  *
 *  under no circumstances should the blame for bugs, etc be placed on     *
 *  Merc Industries.  They are not guaranteed to work on all systems due   *
 *  to their frequent use of strxxx functions.  They are also not the most *
 *  efficient way to perform their tasks, but hopefully should be in the   *
 *  easiest possible way to install and begin using. Documentation for     *
 *  such installation can be found in INSTALL.  Enjoy........    N'Atas-Ha *
 ***************************************************************************/
/*
 * $Log: dg_mobcmd.c,v $
 * Revision 1.1  2004/11/12 02:16:48  w4dimenscor
 * Initial revision
 *
 * Revision 1.23  2004/08/15 01:12:26  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */
#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "screen.h"
#include "dg_scripts.h"
#include "db.h"
#include "utils.h"
#include "handler.h"
#include "interpreter.h"
#include "comm.h"
#include "spells.h"
#include "constants.h"

//extern struct str_app_type str_app[];

extern struct descriptor_data *descriptor_list;
extern struct index_data *mob_index;
extern struct room_data *world_vnum[];
extern struct zone_data *zone_table;
extern int dg_owner_purged;
extern room_rnum find_target_room(char_data * ch, char *rawroomstr);
extern int top_of_zone_table;
extern const char *dirs[];

void raw_kill(struct char_data *ch, struct char_data *killer);
void sub_write(char *arg, char_data * ch, byte find_invis, int targets);
void send_to_zone(char *messg, int zone_rnum);
void send_to_zone_range(char *messg, int zone_rnum, int lower_vnum,
                        int upper_vnum);
void reset_zone(zone_rnum zone);
bitvector_t asciiflag_conv(char *flag);
int real_zone(int number);
void send_to_zone(char *messg, zone_rnum zone);
void die(struct char_data *ch, struct char_data *killer);
int valid_dg_target(struct char_data *ch, int allow_gods);
void send_char_pos(struct char_data *ch, int dam);

room_data *get_room(char *name);

/*
 * Local functions.
 */

/* attaches mob's name and vnum to msg and sends it to script_log */
void mob_log(char_data *mob, const char *format, ...)
{
  va_list args;
  char output[MAX_STRING_LENGTH];

  snprintf(output, sizeof(output), "Mob (%s, VNum %d):: %s",
           GET_SHORT(mob), GET_MOB_VNUM(mob), format);

  va_start(args, format);
  script_vlog(output, args);
  va_end(args);
}


/*
** macro to determine if a mob is permitted to use these commands
*/
#define MOB_OR_IMPL(ch) \
  (IS_NPC(ch) && (!(ch)->desc || GET_LEVEL((ch)->desc->original)>=LVL_IMPL))



/* mob commands */

ACMD(do_msteal)
{
  struct char_data *vict;
  struct obj_data *obj;
  char vict_name[MAX_INPUT_LENGTH], obj_name[MAX_INPUT_LENGTH];

  vict = get_char_room_vis(ch, vict_name, NULL);
  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  two_arguments(argument, obj_name, vict_name);

  send_to_char("In Function\r\n", vict);

  if (!(vict))
  {
    mob_log(ch, "msteal: victim not found");
    return;
  }
  else if (vict == ch)
  {
    mob_log(ch, "msteal: victim is self");
    return;
  }

  if (!(obj = get_obj_in_list_vis(vict, obj_name, NULL, vict->carrying)))
  {
    mob_log(ch, "msteal called with no object argument");
    return;
  }
  else
  {
    send_to_char("Object found..about to steal\r\n", vict);
    if ((IS_CARRYING_N(ch) + 1 < CAN_CARRY_N(ch)))
    {
      if ((IS_CARRYING_W(ch) + GET_OBJ_WEIGHT(obj)) <
          CAN_CARRY_W(ch))
      {
        obj_from_char(obj);
        obj_to_char(obj, ch);
        mob_log(ch, "msteal: Successful steal");
        return;
      }
    }
    else
    {
      mob_log(ch, "msteal: Cannot carry weight");
      return;
    }
  }
}

/* prints the argument to all the rooms aroud the mobile */
ACMD(do_masound)
{
  room_rnum was_in_room;
  int door;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  if (!*argument)
  {
    mob_log(ch, "masound called with no argument");
    return;
  }

  skip_spaces(&argument);

  was_in_room = IN_ROOM(ch);
  for (door = 0; door < NUM_OF_DIRS; door++)
  {
    struct room_direction_data *newexit;

    if (((newexit = was_in_room->dir_option[door]) != NULL) &&
        newexit->to_room != NULL && newexit->to_room != was_in_room)
    {
      IN_ROOM(ch) = newexit->to_room;
      sub_write(argument, ch, TRUE, TO_ROOM);
    }
  }

  IN_ROOM(ch) = was_in_room;
}


/* lets the mobile kill any player or mobile without murder*/
ACMD(do_mkill)
{
  char arg[MAX_INPUT_LENGTH];
  char_data *victim;

  if (!MOB_OR_IMPL(ch))
  {
    new_send_to_char(ch, "Huh?!?\r\n");
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    mob_log(ch, "mkill called with no argument");
    return;
  }

  if (*arg == UID_CHAR)
  {
    if (!(victim = get_char(arg)))
    {
      mob_log(ch, "mkill: victim (%s) not found",arg);
      return;
    }
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    mob_log(ch, "mkill: victim (%s) not found",arg);
    return;
  }

  if (SELF(victim, ch))
  {
    mob_log(ch, "mkill: victim is self");
    return;
  }

  if (!IS_NPC(victim) && PRF_FLAGGED(victim, PRF_NOHASSLE))
  {
    mob_log(ch, "mkill: target has nohassle on");
    return;
  }

  if (FIGHTING(ch))
  {
    mob_log(ch, "mkill: already fighting");
    return;
  }

  start_fighting(ch, victim);
  return;
}


ACMD(do_mlag)
{
  char arg[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];
  char_data *victim;
  int wait = 0;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  two_arguments(argument, arg, arg2);

  if (!*arg)
  {
    mob_log(ch, "mkill called with no argument");
    return;
  }

  if (*arg == UID_CHAR)
  {
    if (!(victim = get_char(arg)))
    {
      mob_log(ch, "mlag: victim (%s) not found", arg);
      return;
    }
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    mob_log(ch, "mlag: victim (%s) not found", arg);
    return;
  }

  if (victim == ch)
  {
    mob_log(ch, "mlag: victim is self");
    return;
  }

  if (!IS_NPC(victim) && PRF_FLAGGED(victim, PRF_NOHASSLE))
  {
    mob_log(ch, "mlag: target has nohassle on");
    return;
  }

  if ((wait = atoi(arg2)) < 1)
    return;

  if (wait > 300)
  {
    mob_log(ch, "mlag: duration longer then 30 seconds outside range.");
    return;
  }
  if (wait <= 0)
    return;


  wait = (wait RL_SEC)/10;

  WAIT_STATE(ch, wait);
  return;
}

/*
 * lets the mobile destroy an object in its inventory
 * it can also destroy a worn object and it can destroy 
 * items using all.xxxxx or just plain all of them
 */
ACMD(do_mjunk)
{
  char arg[MAX_INPUT_LENGTH];
  int pos, junk_all = 0;
  obj_data *obj = NULL;
  obj_data *obj_next = NULL;
  int find_eq_pos(struct char_data *ch, struct obj_data *obj, char *arg);


  if (!MOB_OR_IMPL(ch))
  {
    new_send_to_char(ch, "Huh?!?\r\n");
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    mob_log(ch, "mjunk called with no argument");
    return;
  }

  if (!str_cmp(arg, "all")) junk_all = 1;

  if (*arg == UID_CHAR)
    obj = get_obj(arg);

  if (obj != NULL)
  {
    if (obj->worn_by!= NULL)
      extract_obj(unequip_char(ch, find_eq_pos(ch, obj, 0)));
    else
      extract_obj(obj);
    return;
  }


  if ((find_all_dots(arg) == FIND_INDIV) && !junk_all)
  {
    /* Thanks to Carlos Myers for fixing the line below */
    if ((pos = get_obj_pos_in_equip_vis(ch, arg, NULL, ch->equipment)) >= 0)
    {
      extract_obj(unequip_char(ch, pos));
      return;
    }
    if ((obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)) != NULL )
      extract_obj(obj);
    return;
  }
  else
  {
    for (obj = ch->carrying; obj != NULL; obj = obj_next)
    {
      obj_next = obj->next_content;
      if (arg[3] == '\0' || isname(arg+4, obj->name))
      {
        extract_obj(obj);
      }
    }
    /* Thanks to Carlos Myers for fixing the line below */
    while ((pos = get_obj_pos_in_equip_vis(ch, arg, NULL, ch->equipment)) >= 0)
      extract_obj(unequip_char(ch, pos));
  }
  return;
}



/* prints the message to everyone in the room other than the mob and victim */
ACMD(do_mechoaround)
{
  char arg[MAX_INPUT_LENGTH];
  char_data *victim;
  char *p;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  p = one_argument(argument, arg);
  skip_spaces(&p);

  if (!*arg)
  {
    mob_log(ch, "mechoaround called with no argument");
    return;
  }

  if (*arg == UID_CHAR)
  {
    if (!(victim = get_char(arg)))
    {
      // sprintf(buf, "mechoaround: victim (%s) does not exist",arg);
      // mob_log(ch, buf);
      return;
    }
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    // sprintf(buf, "mechoaround: victim (%s) does not exist",arg);
    // mob_log(ch, buf);
    return;
  }
  if (IN_ROOM(victim) != NULL)
  {
    sub_write(p, victim, TRUE, TO_ROOM);
  }
  else
  {
    mob_log(ch, "calling mechoaround when %s is in nowhere", GET_NAME(victim));
  }
}


/* sends the message to only the victim */
ACMD(do_msend)
{
  char arg[MAX_INPUT_LENGTH];
  char_data *victim;
  char *p;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  p = one_argument(argument, arg);
  skip_spaces(&p);

  if (!*arg)
  {
    mob_log(ch, "msend called with no argument");
    return;
  }

  if (*arg == UID_CHAR)
  {
    if (!(victim = get_char(arg)))
    {
      // sprintf(buf, "msend: victim (%s) does not exist",arg);
      // mob_log(ch, buf);
      return;
    }
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    // sprintf(buf, "msend: victim (%s) does not exist",arg);
    // mob_log(ch, buf);
    return;
  }

  sub_write(p, victim, TRUE, TO_CHAR);
}

ACMD(do_mzoneecho)
{
  int zone;
  char room_number[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;

  msg = any_one_arg(argument, room_number);
  skip_spaces(&msg);

  if (!*room_number || !*msg)
    mob_log(ch, "mzoneecho called with too few args");

  else if ((zone = real_zone(atoi(room_number))) == NOWHERE)
    mob_log(ch, "mzoneecho called for nonexistant zone");

  else
  {
    sprintf(buf, "%s\r\n", msg);
    send_to_zone(buf, zone);
  }
}


/* prints the message to the room at large */
ACMD(do_mecho)
{
  char *p;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  if (!*argument)
  {
    mob_log(ch, "mecho called with no arguments");
    return;
  }
  p = argument;
  skip_spaces(&p);

  sub_write(p, ch, TRUE, TO_ROOM);
}

/* prints the message to everyone in the zone */
ACMD(do_mzecho)
{
  int zone;
  char zone_name[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  msg = any_one_arg(argument, zone_name);
  skip_spaces(&msg);

  if (!*zone_name || !*msg)
    mob_log(ch, "mzoneecho called with too few args");

  else if ((zone = real_zone(atoi(zone_name))) < 0)
    mob_log(ch, "mzoneecho called for nonexistant zone");

  else
  {
    log("zone: %d.", zone);
    sprintf(buf, "%s\r\n", msg);
    send_to_zone(buf, zone);
  }
}

/* prints the message to everyone in the zone within a range of numbers */
ACMD(do_mzrecho)
{
  int zone, lower_vnum, upper_vnum;
  char zone_name[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;
  char lower[MAX_INPUT_LENGTH], upper[MAX_INPUT_LENGTH];

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  msg = any_one_arg(argument, zone_name);
  msg = two_arguments(msg, lower, upper);

  skip_spaces(&msg);

  if (!*zone_name || !*msg || !*lower || !*upper)
    mob_log(ch, "mzrecho called with too few args");


  else if ((zone = real_zone(atoi(zone_name))) < 0)
    mob_log(ch, "mzrecho called for nonexistant zone");

  else
  {
    lower_vnum = atoi(lower);
    upper_vnum = atoi(upper);

    sprintf(buf, "%s\r\n", msg);
    send_to_zone_range(buf, zone, lower_vnum, upper_vnum);
  }
}

/* prints the message to everyone in the range of numbers */
ACMD(do_mrecho)
{
  char start[MAX_INPUT_LENGTH], finish[MAX_INPUT_LENGTH], *msg;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }
  msg = two_arguments(argument, start, finish);

  skip_spaces(&msg);

  if (!*msg || !*start || !*finish)
    mob_log(ch, "mrecho called with too few args");
  else
    send_to_range(atoi(start), atoi(finish), "%s\r\n", msg);

}

ACMD(do_mdamage)
{
  char name[MAX_INPUT_LENGTH], amount[MAX_INPUT_LENGTH];
  int dam = 0;
  char_data *vict;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  two_arguments(argument, name, amount);

  if (!*name || !*amount)
  {
    mob_log(ch, "mdamage: bad syntax");
    return;
  }

  dam = atoi(amount);
  if (*name == UID_CHAR)
  {
    if (!(vict = get_char(name)))
    {
      mob_log(ch, "mdamage: victim (%s) does not exist", name);
      return;
    }
  }
  else if (!(vict = get_char_room_vis(ch, name, NULL)))
  {
    mob_log(ch, "mdamage: victim (%s) does not exist (in this room)", name);
    return;
  }

  script_damage(vict, dam);
}


/*
 * lets the mobile load an item or mobile.  All items
 * are loaded into inventory, unless it is NO-TAKE. 
 */
ACMD(do_mload)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
  int number = 0;
  char_data *mob;
  obj_data *object;
  char *target;
  char_data *tch;
  obj_data *cnt;
  int pos;

  if (!MOB_OR_IMPL(ch))
  {
    new_send_to_char(ch, "Huh?!?\r\n");
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  if( ch->desc && GET_LEVEL(ch->desc->original) < LVL_IMPL)
    return;

  target = two_arguments(argument, arg1, arg2);

  skip_spaces(&target);

  if (!*arg1 || !*arg2 || !is_number(arg2) || ((number = atoi(arg2)) < 0))
  {
    mob_log(ch, "mload: bad syntax");
    return;
  }

  /* load mob to target room - Jamie Nelson, April 13 2004 */
  if (is_abbrev(arg1, "mob"))
  {
    room_rnum rnum;
    if (!target || !*target)
      rnum = IN_ROOM(ch);
    else
    {
      if (!isdigit(*target) || (rnum = real_room(atoi(target))) == NULL)
      {
        mob_log(ch, "mload: room target vnum doesn't exist (loading mob vnum %d to room %s)", number, target);
        return;
      }
    }
    if ((mob = read_mobile(number, VIRTUAL)) == NULL)
    {
      mob_log(ch, "mload: bad mob vnum");
      return;

    }

    char_to_room(mob, rnum);
    if (load_mtrigger(mob)!= -1)
    {
      if (SCRIPT(ch))
      { // it _should_ have, but it might be detached.
        char buf[MAX_INPUT_LENGTH];
        sprintf(buf, "%c%ld", UID_CHAR, GET_ID(mob));
        add_var(&(SCRIPT(ch)->global_vars), "loaded", buf, 0);
      }
    }
  }

  else if (is_abbrev(arg1, "obj"))
  {
    if ((object = read_object(number, VIRTUAL)) == NULL)
    {
      mob_log(ch, "mload: bad object vnum");
      return;
    }

    if (SCRIPT(ch))
    { // it _should_ have, but it might be detached.
      char buf[MAX_INPUT_LENGTH];
      sprintf(buf, "%c%ld", UID_CHAR, GET_ID(object));
      add_var(&(SCRIPT(ch)->global_vars), "loaded", buf, 0);
    }

    /* special handling to make objects able to load on a person/in a container/worn etc. */
    if (!target || !*target)
    {
      if (CAN_WEAR(object, ITEM_WEAR_TAKE))
      {
        obj_to_char(object, ch);
      }
      else
      {
        obj_to_room(object, IN_ROOM(ch));
      }
      load_otrigger(object);
      return;
    }
    two_arguments(target, arg1, arg2); /* recycling ... */
    tch = (arg1 && *arg1 == UID_CHAR) ? get_char(arg1) : get_char_room_vis(ch, arg1, NULL);
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
    cnt = (arg1 && *arg1 == UID_CHAR) ? get_obj(arg1) : get_obj_vis(ch, arg1, NULL);
    if (cnt && GET_OBJ_TYPE(cnt) == ITEM_CONTAINER)
    {
      obj_to_obj(object, cnt);
      load_otrigger(object);
      return;
    }
    /* neither char nor container found - just dump it in room */
    obj_to_room(object, IN_ROOM(ch));
    load_otrigger(object);
    return;
  }

  else
    mob_log(ch, "mload: bad type");
}
/*
 * lets the mobile purge all objects and other npcs in the room,
 * or purge a specified object or mob in the room.  It can purge
 *  itself, but this will be the last command it does.
 */
ACMD(do_mpurge)
{
  char arg[MAX_INPUT_LENGTH];
  char_data *victim;
  obj_data  *obj;

  if (!MOB_OR_IMPL(ch))
  {
    new_send_to_char(ch, "Huh?!?\r\n");
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    /* 'purge' */
    char_data *vnext;
    obj_data  *obj_next;

    for (victim = IN_ROOM(ch)->people; victim; victim = vnext)
    {
      vnext = victim->next_in_room;
      if (IS_NPC(victim) && victim != ch)
        extract_char(victim);
    }

    for (obj = IN_ROOM(ch)->contents; obj; obj = obj_next)
    {
      obj_next = obj->next_content;
      extract_obj(obj);
    }

    return;
  }

  if (*arg == UID_CHAR)
    victim = get_char(arg);
  else victim = get_char_room_vis(ch, arg, NULL);

  if (victim == NULL)
  {
    if (*arg == UID_CHAR)
      obj = get_obj(arg);
    else
      obj = get_obj_vis(ch, arg, NULL);

    if (obj)
    {
      extract_obj(obj);
      obj = NULL;
    }
    else
      mob_log(ch, "mpurge: bad argument");

    return;
  }

  if (!IS_NPC(victim))
  {
    mob_log(ch, "mpurge: purging a PC (%s)", GET_NAME(victim));
    return;
  }

  if (victim==ch) dg_owner_purged = 1;

  extract_char(victim);
}


/* lets the mobile goto any location it wishes that is not private */
ACMD(do_mgoto)
{
  char arg[MAX_INPUT_LENGTH];
  room_rnum location;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    mob_log(ch, "mgoto called with no argument");
    return;
  }

  if ((location = find_target_room(ch, arg)) == NULL)
  {
    mob_log(ch, "mgoto: invalid location");
    return;
  }

  if (FIGHTING(ch))
    stop_fighting(ch);

  char_from_room(ch);
  char_to_room(ch, location);
  enter_wtrigger(IN_ROOM(ch), ch, -1);
}


/* lets the mobile do a command at another location. Very useful */
ACMD(do_mat)
{
  char arg[MAX_INPUT_LENGTH];
  room_rnum location, original;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  argument = one_argument(argument, arg);

  if (!*arg || !*argument)
  {
    mob_log(ch, "mat: bad argument");
    return;
  }

  if ((location = find_target_room(ch, arg)) == NULL)
  {
    mob_log(ch, "mat: invalid location");
    return;
  }

  original = IN_ROOM(ch);
  char_from_room(ch);
  char_to_room(ch, location);
  command_interpreter(ch, argument);

  /*
   * See if 'ch' still exists before continuing!
   * Handles 'at XXXX quit' case.
   */
  if (IN_ROOM(ch) == location)
  {
    char_from_room(ch);
    char_to_room(ch, original);
  }
}


/*
 * lets the mobile transfer people.  the all argument transfers
 * everyone in the current room to the specified location
 */
ACMD(do_mteleport)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
  room_rnum target;
  char_data *vict, *next_ch;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  argument = two_arguments(argument, arg1, arg2);

  if (!*arg1 || !*arg2)
  {
    mob_log(ch, "mteleport: bad syntax");
    return;
  }

  target = find_target_room(ch, arg2);

  if (target == NULL)
  {
    mob_log(ch, "mteleport target is an invalid room");
    return;
  }

  if (!str_cmp(arg1, "all"))
  {
    if (target == IN_ROOM(ch))
    {
      //mob_log(ch, "mteleport all is teleporting people to the same room as the mob teleporting them is in");
      return;
    }

    for (vict = IN_ROOM(ch)->people; vict; vict = next_ch)
    {
      next_ch = vict->next_in_room;
      if (vict == ch)
        continue;
      if (valid_dg_target(vict, TRUE))
      {
        char_from_room(vict);
        char_to_room(vict, target);
        enter_wtrigger(IN_ROOM(ch), ch, -1);
      }
    }
  }
  else
  {
    if (*arg1 == UID_CHAR)
    {
      if (!(vict = get_char(arg1)))
      {
        // sprintf(buf, "mteleport: victim (%s) does not exist",arg1);
        // mob_log(ch, buf);
        return;
      }
    }
    else if (!(vict = get_char_vis(ch, arg1, NULL, FIND_CHAR_ROOM)))
    {
      // sprintf(buf, "mteleport: victim (%s) does not exist",arg1);
      // mob_log(ch, buf);
      return;
    }

    if (valid_dg_target(ch, TRUE))
    {
      char_from_room(vict);
      char_to_room(vict, target);
      enter_wtrigger(IN_ROOM(ch), ch, -1);
    }
  }
}


/*
 * lets the mobile force someone to do something.  must be mortal level
 * and the all argument only affects those in the room with the mobile
 */
ACMD(do_mforce)
{
  char arg[MAX_INPUT_LENGTH];

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
    return;

  argument = one_argument(argument, arg);

  if (!*arg || !*argument)
  {
    mob_log(ch, "mforce: bad syntax");
    return;
  }

  if (!str_cmp(arg, "all"))
  {
    struct descriptor_data *i;
    char_data *vch;

    for (i = descriptor_list; i; i = i->next)
    {
      if ((i->character != ch) && !i->connected &&
          (IN_ROOM(i->character) == IN_ROOM(ch)))
      {
        vch = i->character;
        if (GET_LEVEL(vch) < GET_LEVEL(ch) && CAN_SEE(ch, vch) &&
            valid_dg_target(vch, FALSE))
        {
          command_interpreter(vch, argument);
        }
      }
    }
  }
  else
  {
    char_data *victim;

    if (*arg == UID_CHAR)
    {
      if (!(victim = get_char(arg)))
      {
        // sprintf(buf, "mforce: victim (%s) does not exist",arg);
        // mob_log(ch, buf);
        return;
      }
    }
    else if ((victim = get_char_room_vis(ch, arg, NULL)) == NULL)
    {
      // mob_log(ch, "mforce: no such victim");
      return;
    }

    if (victim == ch)
    {
      mob_log(ch, "mforce: forcing self");
      return;
    }

    if (valid_dg_target(victim, FALSE))
    {
      command_interpreter(victim, argument);
    }
  }
}


/* hunt for someone */
ACMD(do_mhunt)
{
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    mob_log(ch, "mhunt called with no argument");
    return;
  }


  if (FIGHTING(ch))
    return;

  if (*arg == UID_CHAR)
  {
    if (!(victim = get_char(arg)))
    {
      // sprintf(buf, "mhunt: victim (%s) does not exist", arg);
      // mob_log(ch, buf);
      return;
    }
  }
  else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)))
  {
    // sprintf(buf, "mhunt: victim (%s) does not exist", arg);
    // mob_log(ch, buf);
    return;
  }
  HUNTING(ch) = victim;
  remove_hunter(ch);
  add_hunter(ch);



}


/* place someone into the mob's memory list */
ACMD(do_mremember)
{
  char_data *victim;
  struct script_memory *mem;
  char arg[MAX_INPUT_LENGTH];

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
    return;

  argument = one_argument(argument, arg);

  if (!*arg)
  {
    mob_log(ch, "mremember: bad syntax");
    return;
  }

  if (*arg == UID_CHAR)
  {
    if (!(victim = get_char(arg)))
    {
      // sprintf(buf, "mremember: victim (%s) does not exist", arg);
      // mob_log(ch, buf);
      return;
    }
  }
  else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)))
  {
    // sprintf(buf, "mremember: victim (%s) does not exist", arg);
    // mob_log(ch, buf);
    return;
  }

  /* create a structure and add it to the list */
  CREATE(mem, struct script_memory, 1);
  if (!SCRIPT_MEM(ch))
    SCRIPT_MEM(ch) = mem;
  else
  {
    struct script_memory *tmpmem = SCRIPT_MEM(ch);
    while (tmpmem->next)
      tmpmem = tmpmem->next;
    tmpmem->next = mem;
  }

  /* fill in the structure */
  mem->id = GET_ID(victim);
  if (argument && *argument)
  {
    log("strdup from mremember");
    mem->cmd = strdup(argument);
  }
}


/* remove someone from the list */
ACMD(do_mforget)
{
  char_data *victim;
  struct script_memory *mem, *prev;
  char arg[MAX_INPUT_LENGTH];

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    mob_log(ch, "mforget: bad syntax");
    return;
  }

  if (*arg == UID_CHAR)
  {
    if (!(victim = get_char(arg)))
    {
      // sprintf(buf, "mforget: victim (%s) does not exist", arg);
      // mob_log(ch, buf);
      return;
    }
  }
  else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)))
  {
    // sprintf(buf, "mforget: victim (%s) does not exist", arg);
    // mob_log(ch, buf);
    return;
  }

  mem = SCRIPT_MEM(ch);
  prev = NULL;
  while (mem)
  {
    if (mem->id == GET_ID(victim))
    {
      if (mem->cmd)
        free(mem->cmd);
      if (prev == NULL)
      {
        SCRIPT_MEM(ch) = mem->next;
        free(mem);
        mem = SCRIPT_MEM(ch);
      }
      else
      {
        prev->next = mem->next;
        free(mem);
        mem = prev->next;
      }
    }
    else
    {
      prev = mem;
      mem = mem->next;
    }
  }
}


/* transform into a different mobile */
ACMD(do_mtransform)
{
  char arg[MAX_INPUT_LENGTH];
  char_data *m, tmpmob;
  obj_data *obj[NUM_WEARS];
  mob_rnum this_rnum = GET_MOB_RNUM(ch);
  int keep_hp = 1;		/* new mob keeps the old mob's hp/max hp/exp */
  int pos;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  if (ch->desc)
  {
    send_to_char
    ("You've got no VNUM to return to, dummy! try 'switch'\r\n",
     ch);
    return;
  }

  one_argument(argument, arg);

  if (!*arg)
    mob_log(ch, "mtransform: missing argument");
  else if (!isdigit(*arg) && *arg != '-')
    mob_log(ch, "mtransform: bad argument");
  else
  {
    if (isdigit(*arg))
      m = read_mobile(atoi(arg), VIRTUAL);
    else
    {
      keep_hp = 0;
      m = read_mobile(atoi(arg + 1), VIRTUAL);
    }
    if (m == NULL)
    {
      mob_log(ch, "mtransform: bad mobile vnum");
      return;
    }

    /* move new obj info over to old object and delete new obj */

    for (pos = 0; pos < NUM_WEARS; pos++)
    {
      if (GET_EQ(ch, pos))
        obj[pos] = unequip_char(ch, pos);
      else
        obj[pos] = NULL;
    }

    if (GET_MOB_RNUM(ch) != NOBODY)
      mob_index[GET_MOB_RNUM(ch)].number--;

    /* put the mob in the same room as ch so extract will work */
    char_to_room(m, IN_ROOM(ch));

    memcpy(&tmpmob, m, sizeof(*m));
    //rryan: we need to copy the strings so we don't end up free'ing the prototypes later
    if(m->player.name)
      tmpmob.player.name = strdup(m->player.name);
    if(m->player.title)
      tmpmob.player.title = strdup(m->player.title);
    if(m->player.short_descr)
      tmpmob.player.short_descr = strdup(m->player.short_descr);
    if(m->player.long_descr)
      tmpmob.player.long_descr = strdup(m->player.long_descr);
    if(m->player.description)
      tmpmob.player.description = strdup(m->player.description);
    tmpmob.id = ch->id;
    tmpmob.affected = ch->affected;
    tmpmob.carrying = ch->carrying;
    tmpmob.proto_script = ch->proto_script;
    tmpmob.script = ch->script;
    tmpmob.memory = ch->memory;
    tmpmob.next_in_room = ch->next_in_room;
    tmpmob.next = ch->next;
    tmpmob.next_fighting = ch->next_fighting;
    tmpmob.followers = ch->followers;
    tmpmob.master = ch->master;

    GET_WAS_IN(&tmpmob) = GET_WAS_IN(ch);
    if (keep_hp)
    {
      GET_HIT(&tmpmob) = GET_HIT(ch);
      GET_MAX_HIT(&tmpmob) = GET_MAX_HIT(ch);
      GET_EXP(&tmpmob) = GET_EXP(ch);
    }
    GET_GOLD(&tmpmob) = GET_GOLD(ch);
    GET_POS(&tmpmob) = GET_POS(ch);
    IS_CARRYING_W(&tmpmob) = IS_CARRYING_W(ch);
    IS_CARRYING_N(&tmpmob) = IS_CARRYING_N(ch);
    FIGHTING(&tmpmob) = FIGHTING(ch);
    remove_hunter(ch);
    if ((HUNTING(&tmpmob) = HUNTING(ch)) != NULL)
      add_hunter(&tmpmob);
    RIDING(&tmpmob) = RIDING(ch);
    RIDDEN_BY(&tmpmob) = RIDDEN_BY(ch);
    MOB_TIER(&tmpmob) = MOB_TIER(ch);
    memcpy(ch, &tmpmob, sizeof(*ch));

    for (pos = 0; pos < NUM_WEARS; pos++)
    {
      if (obj[pos]) equip_char(ch, obj[pos], pos);
    }
    ch->nr = this_rnum;
    extract_char(m);
  }
}


ACMD(do_mdoor)
{
  char target[MAX_INPUT_LENGTH], direction[MAX_INPUT_LENGTH];
  char field[MAX_INPUT_LENGTH], *value;
  struct room_data *rm;
  struct room_direction_data *newexit;
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


  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
    return;

  argument = two_arguments(argument, target, direction);
  value = one_argument(argument, field);
  skip_spaces(&value);

  if (!*target || !*direction || !*field)
  {
    mob_log(ch, "mdoor called with too few args");
    return;
  }

  if ((rm = get_room(target)) == NULL)
  {
    mob_log(ch, "mdoor: invalid target");
    return;
  }

  if ((dir = search_block(direction, dirs, FALSE)) == -1)
  {
    mob_log(ch, "mdoor: invalid direction");
    return;
  }

  if ((fd = search_block(field, door_field, FALSE)) == -1)
  {
    mob_log(ch, "odoor: invalid field");
    return;
  }

  newexit = rm->dir_option[dir];

  /* purge exit */
  if (fd == 0)
  {
    if (newexit)
    {
      if (newexit->general_description)
        free(newexit->general_description);
      if (newexit->keyword)
        free(newexit->keyword);
      free(newexit);
      rm->dir_option[dir] = NULL;
    }
  }

  else
  {
    if (!newexit)
    {
      CREATE(newexit, struct room_direction_data, 1);
      rm->dir_option[dir] = newexit;
    }

    switch (fd)
    {
    case 1:		/* description */
      if (newexit->general_description)
        free(newexit->general_description);
      CREATE(newexit->general_description, char, strlen(value) + 3);
      strcpy(newexit->general_description, value);
      strcat(newexit->general_description, "\r\n");
      break;
    case 2:		/* flags       */
      newexit->exit_info = (sh_int) asciiflag_conv(value);
      break;
    case 3:		/* key         */
      newexit->key = atoi(value);
      break;
    case 4:		/* name        */
      if (newexit->keyword)
        free(newexit->keyword);
      CREATE(newexit->keyword, char, strlen(value) + 1);
      strcpy(newexit->keyword, value);
      break;
    case 5:		/* room        */
      if ((to_room = real_room(atoi(value))) != NULL)
        newexit->to_room = to_room;
      else
        mob_log(ch, "mdoor: invalid door target");
      break;
    }
  }
}

ACMD(do_mslay)
{
  struct char_data *vict;
  char arg[MAX_INPUT_LENGTH];

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("Kill who?\r\n", ch);
    return;
  }

  if (!(vict = get_char_room_vis(ch, arg, NULL)))
  {
    send_to_char("They aren't here.\r\n", ch);
    return;
  }

  if (ch == vict)
  {
    send_to_char("Your mother would be so sad.. :(\r\n", ch);
    return;
  }

  act("You chop $M to pieces!  Ah!  The blood!", FALSE, ch, 0, vict,
      TO_CHAR);
  act("$N chops you to pieces!", FALSE, vict, 0, ch, TO_CHAR);
  act("$n brutally slays $N!", FALSE, ch, 0, vict, TO_NOTVICT);

  if (vict)
    raw_kill(vict, ch);

}

ACMD(do_mcollision)
{
  struct obj_data *obj = NULL, *obj_next = NULL;
  int damage = 0;
  room_rnum was_in;
  char arg[MAX_INPUT_LENGTH];

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  one_argument(argument, arg);

  if (!arg)
  {
    mob_log(ch, "mcollision: called without amount of damage.");
    return;
  }

  damage = atoi(arg);

  if (damage < 0 || damage > 3)
  {
    mob_log(ch, "mcollision: damage value is out of range, %d.",
            damage);
    return;
  }

  was_in = IN_ROOM(ch);

  for (obj = IN_ROOM(ch)->contents; obj; obj = obj_next)
  {
    obj_next = obj->next_content;
    if (GET_OBJ_TYPE(obj) == ITEM_VEHICLE)
    {
      IN_ROOM(ch) = real_room(GET_OBJ_VAL(obj, 0));
      GET_OBJ_VAL(obj, 2) -= damage;
      if (GET_OBJ_VAL(obj, 2) <= 0)
      {	// blow it up
        //act("As the $p collides with $n, it explodes.",
        //    FALSE, ch, obj, NULL, TO_ROOM);
        IN_ROOM(ch) = was_in;
        act("As $p collides with $n, it explodes.",
            FALSE, ch, obj, NULL, TO_ROOM);

        extract_obj(obj);
        return;
      }
      else
      {
        act("$n collides with the ship, causing damage to the hull.", FALSE, ch, obj, NULL, TO_ROOM);
        IN_ROOM(ch) = was_in;
        act("$n collides with $p, causing some damage to $p.",
            FALSE, ch, obj, NULL, TO_ROOM);
        return;
      }
    }
  }
}

ACMD(do_mzreset)
{
  zone_rnum i;

  if (!MOB_OR_IMPL(ch))
  {
    send_to_char("Huh?!?\r\n", ch);
    return;
  }

  i = ch->in_room->zone;

  if (i >= 0 && i <= top_of_zone_table)
  {
    reset_zone(i);
    new_mudlog(NRM, MAX(LVL_GRGOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s reset zone %d (%s)", GET_NAME(ch), i,
               zone_table[i].name);
  }
  else
    send_to_char("Invalid zone number.\r\n", ch);
}
