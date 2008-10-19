/* locker code by mordecai (jamie nelson <mordecai@xtra.co.nz>) */
/* 18 jan 2004 */
#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "screen.h"
#include "spells.h"
#include "handler.h"
#include "interpreter.h"
#include "db.h"
#include "constants.h"

/*external functions */
OBJ_DATA * load_objects_to_list(FILE *fl); /*objsave.c*/
void convert_tokens(Character *ch); /*cali.c*/
int save_one_item( OBJ_DATA *obj, FILE *fl, int locate); /*objsave.c*/

#define ITEMS_PER_BRONZE 15
/*local functions */
int add_item_to_locker(Character *ch, OBJ_DATA *obj);
OBJ_DATA *item_from_locker(Character *ch, OBJ_DATA *obj);
int remove_item_from_locker(Character *ch, int num);
void save_locker(Character *ch);
void list_locker_to_char(Character *ch);
void load_locker(Character *ch);
void send_policy(Character *ch);
int locker_in_room(room_rnum room);
void extract_all_in_list(OBJ_DATA *obj);
void rent_locker(Character *ch, int size,int months);
/*
Adds an item to a locker to be saved.
   returns:
   0 is fail
   1 is success
*/

int count_locker(Character *ch)
{
  OBJ_DATA *obj;
  int count = 0;
  for (obj = LOCKER(ch); obj; obj = obj->next_content)
    count++;
  return count;
}

int add_item_to_locker(Character *ch, OBJ_DATA *obj)
{
  if (!obj)
    return 0;
  if (!LOCKER_EXPIRE(ch))
  {
    send_policy(ch);
    return 0;
  }
  if (count_locker(ch) >= LOCKER_LIMIT(ch))
  {
    ch->Send( "Your locker is full.\r\n");
    return 0;
  }
  if (GET_OBJ_TYPE(obj) == ITEM_CONTAINER)
  {
    ch->Send( "Containers cannot be stored.\r\n");
    return 0;
  }

  obj_from_char(obj);
  obj->next_content = LOCKER(ch);
  obj->in_locker = ch;
  LOCKER(ch) = obj;
  save_locker(ch);
  Crash_crashsave(ch);
  return 1;
}
OBJ_DATA *item_from_locker(Character *ch, OBJ_DATA *obj)
{
  OBJ_DATA *temp = NULL;
  REMOVE_FROM_LIST(obj, LOCKER(ch), next_content);
  obj->next_content = NULL;
  obj->in_locker = NULL;
  return obj;
}

int remove_item_from_locker(Character *ch, int num)
{
  OBJ_DATA *obj;
  int count = 1, found = FALSE;
  if (!LOCKER_EXPIRE(ch))
  {
    ch->Send( "{cRYour locker has expired, you have %d items in it.\r\n"
                     "To regain access to these items, please purchase a new locker.\r\n\r\n", count_locker(ch));
    send_policy(ch);
    return 0;
  }
  for (obj = LOCKER(ch); obj; obj = obj->next_content)
  {
    if (count == num)
    {
      if (item_from_locker(ch, obj) != NULL)
      {
        found = TRUE;
        break;
      }
      else
      {
        return 0;
      }
    }
    count++;
  }
  if (!found)
  {
    ch->Send( "There is no item at that number.\r\n");
    return 0;
  }
  obj_to_char(obj, ch);
  ch->Send( "You get %s from your locker.\r\n", obj->short_description);
  save_locker(ch);
  Crash_crashsave(ch);
  return 1;
}

void send_policy(Character *ch)
{
  ch->Send( "You aren't currently renting a locker.\r\n"
                   "To rent a locker type: rent <number of items> <number of months>\r\n"
                   "Where <number of items is the space for items in your locker.\r\n"
                   "It's 1 bronze per 10 items per month.\r\n"
                   "You can't rerent a locker until your current lease has expired.\r\n"
                   "Any items left in the locker when the lease expires will be incinerated.\r\n");
}

void save_locker(Character *ch)
{
  char fname[MAX_INPUT_LENGTH];
  char tempname[MAX_INPUT_LENGTH + 4];
  FILE *fp;
  OBJ_DATA *obj;
  int retval = 1;
  if (!get_filename(GET_NAME(ch), fname, LOCKER_FILES))
    return;

  snprintf(tempname, sizeof(tempname), "%s%s", fname, ".tmp");

  if (!(fp = fopen(tempname, "wb")))
  {
    log("ERR: Can't save %s's locker file.", GET_NAME(ch));
    return;
  }

  for (obj = LOCKER(ch); obj && retval >= 0; obj = obj->next_content)
    if ((retval = save_one_item(obj, fp, 0)) < 0)
    {

      fclose(fp);
      if (remove(tempname) == -1)
      {
        new_mudlog(NRM, LVL_GOD, TRUE, "unable to remove temp file: %s", tempname);
        log("unable to remove temp file: %s", tempname);
      }
      return;
    }
  fclose(fp);
  if (rename(tempname, fname) == -1)
  {
    new_mudlog(NRM, LVL_GOD, TRUE, "Major error (no disk space) can't save file: %s", tempname);
    core_dump();
  }
}

void load_locker(Character *ch)
{
  char fname[MAX_INPUT_LENGTH];
  FILE *fp;
  if (!get_filename(GET_NAME(ch), fname, LOCKER_FILES))
    return;

  if (!(fp = fopen(fname, "rb")))
  {
    //log("ERR: Can't load %s's locker file.", GET_NAME(ch));
    return;
  }

  LOCKER(ch) = load_objects_to_list(fp);

  fclose(fp);
}

void list_locker_to_char(Character *ch)
{
  OBJ_DATA *obj;
  int  count = 0;
  int days = 0, hours = 0, mins = 0;
  char buf[MAX_INPUT_LENGTH];
  DYN_DEFINE;

  if (LOCKER_EXPIRE(ch) < time(0))
  {
    ch->Send( "{cRSorry but your locker has expired. \r\n"
                     "To access these items, rent a new locker of any size.{c0\r\n");
  }
  else
  {
    days = (LOCKER_EXPIRE(ch) - time(0))/SECS_PER_REAL_DAY;
    ch->Send( "You have %d days left on this locker.\r\n", days);
    hours = (LOCKER_EXPIRE(ch) - time(0))/SECS_PER_REAL_HOUR;
    ch->Send( "You have %d hours left on this locker.\r\n", hours);
    mins = (LOCKER_EXPIRE(ch) - time(0))/SECS_PER_REAL_MIN;
    ch->Send( "You have %d mins left on this locker.\r\n", mins);
  }
  if (LOCKER(ch))
  {
    DYN_CREATE;
    *dynbuf = 0;

    for (obj = LOCKER(ch); obj; obj = obj->next_content)
    {
      snprintf(buf, sizeof(buf), "{cy%-3d{cg) {cc[%-15s]{cg %s{c0  \r\n", ++count, item_types[(int)GET_OBJ_TYPE(obj)], obj->short_description);
      DYN_RESIZE(buf);
    }


    ch->Send( "You have %d items in your locker, with room for %d more.\r\n",count, LOCKER_LIMIT(ch) - count);



    ch->Send( "No.     Type     Name\r\n"
                     "----------------------------------------------\r\n");

    page_string(ch->desc, dynbuf, DYN_BUFFER);
  }
  else
    ch->Send( "\r\nYour locker is empty.\r\n");
}

ACMD(do_locker)
{
  char arg[MAX_INPUT_LENGTH], arg1[MAX_INPUT_LENGTH];
  int size, months;
  OBJ_DATA *obj = NULL;

  argument = two_arguments(argument, arg, arg1);

  if (!*arg)
  {
    ch->Send(
                     "{cCLocker commands: - cost: %d items per bronze per 4 weeks{cy\r\n"
                     "locker list                 - lists the content of your locker\r\n"
                     "locker put <item>           - puts an item from your inventory into the locker\r\n"
                     "locker get <list number>    - takes an item from a locker\r\n"
                     "locker rent <size> <months> - rents a locker with room for <size> items for <months>\r\n"
                     "locker cancel               - cancels your locker subscription early (purges all in it)\r\n"
                     "{c0", ITEMS_PER_BRONZE
                    );
    return;
  }

  switch (LOWER(*arg))
  {
  default:
    ch->Send(
                     "{cCLocker commands: - cost: %d items per bronze per 4 weeks{cy\r\n"
                     "locker list                 - lists the content of your locker\r\n"
                     "locker put <item>           - puts an item from your inventory into the locker\r\n"
                     "locker get <list number>    - takes an item from a locker\r\n"
                     "locker rent <size> <months> - rents a locker with room for <size> items for <months>\r\n"
                     "locker cancel               - cancels your locker subscription early (purges all in it)\r\n"
                     "{c0", ITEMS_PER_BRONZE
                    );
    return;
    break;
  case 'l':
    list_locker_to_char(ch);
    break;
  case 'p':
    if (!locker_in_room(IN_ROOM(ch)))
    {
      ch->Send( "You need to be at a locker to do that.\r\n");
      return;
    }
    if ( !*arg1)
    {
      ch->Send( "locker put what?\r\n");
      return;
    }
    if (!(obj = get_obj_in_list_vis(ch, arg1, NULL, ch->carrying)))
    {
      ch->Send( "You aren't carrying %s %s.\r\n",  AN(arg1), arg1);
      return;
    }
    if (add_item_to_locker(ch, obj))
    {
      ch->Send( "You put %s into your locker.\r\n", obj->short_description);
      return;
    }
    break;
  case 'g':
    if (!locker_in_room(IN_ROOM(ch)))
    {
      ch->Send( "You need to be at a locker to do that.\r\n");
      return;
    }
    if ( !*arg1)
    {
      ch->Send( "locker get, what num?\r\n");
      return;
    }
    if (!atoi(arg1))
    {
      ch->Send( "That is not a valid number.\r\n");
      return;
    }
    remove_item_from_locker(ch, atoi(arg1));
    return;
    break;
  case 'r':
    if (!locker_in_room(IN_ROOM(ch)))
    {
      ch->Send( "You need to be at a locker to do that.\r\n");
      return;
    }
    if ( !*arg1 || !argument || !*argument)
    {
      ch->Send( "locker rent how big, how long?\r\n");
      return;
    }
    if ((size = atoi(arg1)) == 0)
    {
      ch->Send( "That is not a valid size.\r\n");
      return;
    }
    if ((months = atoi(argument)) == 0)
    {
      ch->Send( "That is not a valid number of months.\r\n");
      return;
    }
    rent_locker(ch, size, months);
    break;
  case 'c':
    if (!locker_in_room(IN_ROOM(ch)))
    {
      ch->Send( "You need to be at a locker to do that.\r\n");
      return;
    }
    if (strcmp(arg, "cancel"))
    {
      ch->Send( "You need to type the whole word 'cancel' for it to work.\r\n");
      return;
    }
    LOCKER_EXPIRE(ch) = 0;
    LOCKER_LIMIT(ch) = 0;
    extract_all_in_list(LOCKER(ch));
    LOCKER(ch) = NULL;
    save_locker(ch);
    ch->save();
    ch->Send( "Locker canceled and purged.\r\n");
    break;
  }

}

int locker_in_room(room_rnum room)
{
  OBJ_DATA *obj;
  if (!VALID_ROOM_RNUM(room))
    return 0;
  for (obj = room->contents;obj;obj = obj->next_content)
  {
    if (GET_OBJ_TYPE(obj) == ITEM_LOCKER)
      return 1;
  }
  return 0;
}

void extract_all_in_list(OBJ_DATA *obj)
{
  if (!obj) return;

  if (obj->next_content)
    extract_all_in_list(obj->next_content);

  extract_obj(obj);
}

void rent_locker(Character *ch, int size,int months)
{
  int cost = (size * months)/(ITEMS_PER_BRONZE);
  int payment = 0;
  time_t adding = 0;

  if (size%ITEMS_PER_BRONZE)
  {
    ch->Send( "The size of the locker needs to be a multiple of %d.\r\n", ITEMS_PER_BRONZE);
    return;
  }
  payment += (100 * GET_GOLD_TOKEN_COUNT(ch));
  payment += (10 * GET_SILVER_TOKEN_COUNT(ch));
  payment += (1 * GET_BRONZE_TOKEN_COUNT(ch));

  if (payment < cost)
  {
    ch->Send( "You can't afford something of that size and length\r\n"
                     "It's cost would be %d bronze. You have %d bronze.\r\n", cost, payment);
    return;

  }
  if (LOCKER_EXPIRE(ch) != 0)
  {
    if (LOCKER_LIMIT(ch) != size)
    {
      ch->Send( "You can't change an existing lockers size.\r\n");
      return;
    }
    else
    {
      ch->Send( "You add %d more months onto your locker rental.\r\n", months);
      adding = LOCKER_EXPIRE(ch) - time(0);
    }
  }
  else
    ch->Send( "You rent a %d item locker for %d month%s.\r\n", size, months, months > 1 ? "s" : "");

  LOCKER_LIMIT(ch) = size;
  LOCKER_EXPIRE(ch) = time(0) + (SECS_PER_REAL_DAY * 28 * months) + adding;
  payment -= cost;

  GET_GOLD_TOKEN_COUNT(ch) = 0;
  GET_SILVER_TOKEN_COUNT(ch) = 0;
  GET_BRONZE_TOKEN_COUNT(ch) = payment;

  convert_tokens(ch);
  ch->save();


}
