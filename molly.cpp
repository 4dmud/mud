#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "screen.h"
#include "constants.h"
#include "dg_scripts.h"
#include "dg_event.h"
#include "damage.h"
#include "descriptor.h"


/* external functions */
char *find_exdesc(char *word, struct extra_descr_data *list);

ACMD(do_smell)
{
  int bits, found = FALSE, j,fnum;
  Character *found_char = NULL;
  struct obj_data *obj = NULL, *found_obj = NULL;
  char arg[MAX_STRING_LENGTH],arg2[MAX_STRING_LENGTH];
  char *arg1;
  arg1=arg;
  if (!ch->desc)
    return;

  one_argument(argument, arg);
  strncpy(arg2,arg,MAX_STRING_LENGTH);
  if (!*arg)
  {
    ch->Send( "%s", IN_ROOM(ch)->smell);
    return;
  }
  if (!(fnum = get_number(&arg1)))
  {
    send_to_char("Smell what?\r\n", ch);
    return;
  }
  bits =
    generic_find(arg2,
                 FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP |
                 FIND_CHAR_ROOM, ch, &found_char, &found_obj);

  /* characters */
//  if (found_char != NULL)
//  
//    if (ch != found_char)
//      if (CAN_SEE(found_char, ch)) {
 //          act("You lean in close to $N and take a deep breath. Mmmmm ...", FALSE, ch, 0, found_char, TO_CHAR);
//           act("$n leans in close and takes a deep breath..", FALSE,
//               ch, 0, found_char, TO_VICT);
//           act("$n smells $N.", FALSE, ch, 0, found_char, TO_NOTVICT);
//           return;
//      }

    for(found_char=IN_ROOM(ch)->people;found_char;found_char=found_char->next_in_room)
       if(CAN_SEE(found_char, ch) && isname(arg, GET_NAME(found_char))) 
          if(--fnum==0){
             act("You lean in close to $N and take a deep breath. Mmmmm ...", FALSE, ch, 0, found_char, TO_CHAR);
           act("$n leans in close and takes a deep breath..", FALSE,
               ch, 0, found_char, TO_VICT);
           act("$n smells $N.", FALSE, ch, 0, found_char, TO_NOTVICT);
           found = TRUE;
          }
  /* worn equipment */
  for (j = 0; j < NUM_WEARS && !found; j++)
    if (GET_EQ(ch, j) && CAN_SEE_OBJ(ch, GET_EQ(ch, j)))
      if (isname(arg, GET_EQ(ch, j)->name))
        if(--fnum==0){
           if (GET_EQ(ch, j)->smell != NULL)
               send_to_char(GET_EQ(ch, j)->smell, ch);
           else
               send_to_char("You smell nothing special.\n",ch);
           found = TRUE;
        }
  

  /* carried equipment */
  for (obj = ch->carrying; obj && !found; obj = obj->next_content)
    if (CAN_SEE_OBJ(ch, obj))
      if (isname(arg, (obj)->name))
        if(--fnum==0){
           if (obj->smell != NULL)
             send_to_char((obj)->smell, ch);
           else
             send_to_char("You smell nothing special.\n",ch);
           found = TRUE;
          
        }
  

  /* equipment in room */
  for (obj = IN_ROOM(ch)->contents; obj && !found;
       obj = obj->next_content)
    if (CAN_SEE_OBJ(ch, obj))
      if (isname(arg, obj->name))
        if(--fnum==0){
           if (obj->smell != NULL)
             send_to_char(obj->smell, ch);
           else
             send_to_char("You smell nothing special.\n",ch);
           found = TRUE;
          
        }
  

  if (!found)
    send_to_char("You do not see that here.\r\n", ch);
}

ACMD(do_taste)
{
  int bits, found = FALSE, j,fnum;
  Character *found_char = NULL;
  struct obj_data *obj = NULL, *found_obj = NULL;
  char arg[MAX_STRING_LENGTH];
  char *arg1;

  arg1=arg;
  if (!ch->desc)
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("What do you want to taste?\r\n", ch);
    return;
  }
  if (!(fnum = get_number(&arg1)))
  {
    send_to_char("Taste what?\r\n", ch);
    return;
  }
  bits =
    generic_find(arg,
                 FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP |
                 FIND_CHAR_ROOM, ch, &found_char, &found_obj);

  /* characters
     if (found_char != NULL) {
     if (ch != found_char) {
     if (CAN_SEE(found_char, ch)) {
     act("You lean in close to $N and take a deep breath. Mmmmm ...", FALSE,
     ch, 0, found_char, TO_CHAR);
     act("$n leans in close and takes a deep breath..", FALSE, ch, 0,
     found_char, TO_VICT);
     act("$n smells $N.", FALSE, ch, 0, found_char, TO_NOTVICT);
     }
     }
     return;
     }
   */

  /* worn equipment */
  for (j = 0; j < NUM_WEARS && !found; j++)
    if (GET_EQ(ch, j) && CAN_SEE_OBJ(ch, GET_EQ(ch, j)))
      if (isname(arg, GET_EQ(ch, j)->name))
        if(--fnum==0){
          if (GET_EQ(ch, j)->taste != NULL)
             send_to_char(GET_EQ(ch, j)->taste, ch);
          else
             send_to_char("You taste nothing special.",ch);
          found = TRUE;
        }
  

  /* carried equipment */
  for (obj = ch->carrying; obj && !found; obj = obj->next_content)
    if (CAN_SEE_OBJ(ch, obj))
      if (isname(arg, (obj)->name))
        if(--fnum==0){
           if (obj->taste != NULL)
             send_to_char((obj)->taste, ch);
           else
             send_to_char("You taste nothing special.",ch);
           found = TRUE;
        }
  

  /* equipment in room */
  for (obj = IN_ROOM(ch)->contents; obj && !found;
       obj = obj->next_content)
    if (CAN_SEE_OBJ(ch, obj))
      if (isname(arg, obj->name))
        if(--fnum==0){
           if (obj->taste != NULL)
             send_to_char(obj->taste, ch);
           else
             send_to_char("You taste nothing special.",ch);
           found = TRUE;
        }
  

  if (!found)
    send_to_char("You do not see that here.\r\n", ch);
}

ACMD(do_feel)
{
  int bits, found = FALSE, j, fnum;
  Character *found_char = NULL;
  struct obj_data *obj = NULL, *found_obj = NULL;
  char arg[MAX_STRING_LENGTH];
  char *arg1;
  arg1=arg;
  if (!ch->desc)
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("What do you want to feel?\r\n", ch);
    return;
  }

  if (!(fnum = get_number(&arg1)))
  {
    send_to_char("Feel what?\r\n", ch);
    return;
  }

  bits =
    generic_find(arg,
                 FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP |
                 FIND_CHAR_ROOM, ch, &found_char, &found_obj);

  /* worn equipment */
  for (j = 0; j < NUM_WEARS && !found; j++)
    if (GET_EQ(ch, j) && CAN_SEE_OBJ(ch, GET_EQ(ch, j)))
      if (isname(arg, GET_EQ(ch, j)->name))
        if(--fnum==0){
           if (GET_EQ(ch, j)->feel != NULL)
             send_to_char(GET_EQ(ch, j)->feel, ch);
           else
             send_to_char("You feel nothing special.",ch);
           found = TRUE;
        }

  /* carried equipment */
  for (obj = ch->carrying; obj && !found; obj = obj->next_content)
    if (CAN_SEE_OBJ(ch, obj))
      if (isname(arg, (obj)->name))
        if(--fnum==0){
          if (obj->feel != NULL)
             send_to_char((obj)->feel, ch);
          else
             send_to_char("You feel nothing special.",ch);
          found = TRUE;
        }

  /* equipment in room */
  for (obj = IN_ROOM(ch)->contents; obj && !found;
       obj = obj->next_content)
    if (CAN_SEE_OBJ(ch, obj))
      if (isname(arg, obj->name))
        if(--fnum==0){
           if (obj->feel != NULL)
             send_to_char(obj->feel, ch);
           else
             send_to_char("You feel nothing special.",ch);
          found = TRUE;
        }

  if (!found)
    send_to_char("You do not see that here.\r\n", ch);
}

ACMD(do_listen)
{
  if (!ch->desc)
    return;
  if (IN_ROOM(ch)->listen)
    ch->Send( "%s", IN_ROOM(ch)->listen);
  return;
}

void look_above_target(Character *ch, char *arg)
{
  char *desc;

  if (!ch->desc)		// Am I a mob?
    return;

  if (!*arg)
  {
    send_to_char("Look above what?\r\n", ch);
    return;
  }

  if ((desc =
         find_exdesc(arg,
                     IN_ROOM(ch)->look_above_description)) != NULL)
  {
    page_string(ch->desc, desc, FALSE);
    return;
  }
  else
  {
    send_to_char("You see nothing special above there.\r\n", ch);
    return;
  }
}

void look_behind_target(Character *ch, char *arg)
{
  char *desc;

  if (!ch->desc)		// Am I a mob?
    return;

  if (!*arg)
  {
    send_to_char("Look behind what?\r\n", ch);
    return;
  }

  if ((desc =
         find_exdesc(arg,
                     IN_ROOM(ch)->look_behind_description)) !=
      NULL)
  {
    page_string(ch->desc, desc, FALSE);
    return;
  }
  else
  {
    send_to_char("You see nothing special behind that.\r\n", ch);
    return;
  }
}

void look_under_target(Character *ch, char *arg)
{
  char *desc;

  if (!ch->desc)		// Am I a mob?
    return;

  if (!*arg)
  {
    send_to_char("Look under what?\r\n", ch);
    return;
  }

  if ((desc =
         find_exdesc(arg,
                     IN_ROOM(ch)->look_under_description)) != NULL)
  {
    page_string(ch->desc, desc, FALSE);
    return;
  }
  else
  {
    send_to_char("You see nothing special under that.\r\n", ch);
    return;
  }
}

ACMD(do_climb)
{
  struct obj_data *obj = NULL;
  int percent = 0;
  int chance = 0;
  char buf[MAX_STRING_LENGTH];

  percent = number(1, 101);
  chance = 70;

  if (GET_CLASS(ch) == CLASS_THIEF)
    chance = 75;
  
  chance += GET_DEX(ch);

  one_argument(argument, buf);

  if (*buf)
  {			/* an argument was supplied, search keyword */
    if ((obj =
           get_obj_in_list_vis(ch, buf, NULL,IN_ROOM(ch)->contents)))
    {
      if (CAN_SEE_OBJ(ch, obj))
      {
        if (GET_OBJ_TYPE(obj) == ITEM_CLIMBABLE)
        {
          if (GET_OBJ_VAL(obj, 0) != NOWHERE && VALID_ROOM_RNUM(world_vnum[GET_OBJ_VAL(obj, 0)]))
          {
            if (percent <= chance)
            {
              char_from_room(ch);
              act("$n climbs $p.", FALSE, ch, obj, 0,
                  TO_ROOM);
              char_to_room(ch, world_vnum[GET_OBJ_VAL(obj, 0)]);
              LOOK(ch);
              act("$n has arrived.", FALSE, ch, obj, 0,TO_ROOM);

              entry_memory_mtrigger(ch);
              greet_mtrigger(ch, -1);
              greet_memory_mtrigger(ch);
              enter_wtrigger(IN_ROOM(ch), ch, -1);
            }
            else
            {
              act("As you start climbing $p, you slip and fall.", FALSE, ch, obj, 0, TO_CHAR);
              act("$n starts climbing $p but falls to the ground. That's got to hurt.", FALSE, ch, obj, 0, TO_ROOM);
              damage(ch, ch, (GET_HIT(ch) / 4), TYPE_UNDEFINED);
            }
          }
          else if (real_room(GET_OBJ_VAL(obj, 1)) != NULL)
          {
            if (percent <= chance)
            {
              char_from_room(ch);
              act("$n climbs $p.", FALSE, ch, obj, 0,
                  TO_ROOM);
              char_to_room(ch,
                           real_room(GET_OBJ_VAL(obj, 1)));
              act("$n has arrived.", FALSE, ch, obj, 0,
                  TO_ROOM);
              LOOK(ch);
              entry_memory_mtrigger(ch);
              greet_mtrigger(ch, -1);
              greet_memory_mtrigger(ch);
              enter_wtrigger(IN_ROOM(ch), ch, -1);
            }
            else
            {
              act("As you start climbing $p, you slip and fall.", FALSE, ch, obj, 0, TO_CHAR);
              act("$n starts climbing $p but falls to the ground. That's got to hurt.", FALSE, ch, obj, 0, TO_ROOM);
              damage(ch, ch, (GET_HIT(ch) / 4), TYPE_UNDEFINED);
            }
          }
          return;
        }
        else if(GET_OBJ_TYPE(obj) == ITEM_DESCENDABLE){
           act("There is no $p here that you can climb.", FALSE, ch, obj, 0, TO_CHAR);
           return;
        }
      }
    }
    ch->Send( "There is no %s here.\r\n", buf);
  }
  else
  {
    send_to_char("What do you want to climb?\r\n", ch);
  }
}

ACMD(do_descend)
{
  struct obj_data *obj = NULL;
  int percent = 0, chance = 0;
  char buf[MAX_STRING_LENGTH];

  percent = number(1, 101);
  chance = 70;

  if (GET_CLASS(ch) == CLASS_THIEF)
    chance = 75;
  
  chance += GET_DEX(ch);

  one_argument(argument, buf);

  if (*buf)
  {			/* an argument was supplied, search keyword */
    if ((obj =
           get_obj_in_list_vis(ch, buf, NULL,
                               IN_ROOM(ch)->contents)))
    {
      if (CAN_SEE_OBJ(ch, obj))
      {
        if (GET_OBJ_TYPE(obj) == ITEM_DESCENDABLE)
        {
          if (GET_OBJ_VAL(obj, 0) != NOWHERE && VALID_ROOM_RNUM(world_vnum[GET_OBJ_VAL(obj, 0)]))
          {
            if (percent <= chance )
            {
              char_from_room(ch);
              act("$n descends $p.", FALSE, ch, obj, 0, TO_ROOM);
              char_to_room(ch, world_vnum[GET_OBJ_VAL(obj, 0)]);
              LOOK(ch);
              act("$n has arrived.", FALSE, ch, obj, 0,TO_ROOM);
            }
            else
            {
              act("As you start descending $p, you slip and tumble down.", FALSE, ch, obj, 0, TO_CHAR);
              char_from_room(ch);
              char_to_room(ch, world_vnum[GET_OBJ_VAL(obj, 0)]);
              act("$n starts descending $p but slips and tumbles down, head over heals.\r\nThat's got to hurt.", FALSE, ch, obj, 0, TO_ROOM);
              damage(ch, ch, (GET_HIT(ch) / 4), TYPE_UNDEFINED);
              LOOK(ch);
            }

            entry_memory_mtrigger(ch);
            greet_mtrigger(ch, -1);
            greet_memory_mtrigger(ch);
            enter_wtrigger(IN_ROOM(ch), ch, -1);
          }
          else if (real_room(GET_OBJ_VAL(obj, 1)) != NULL)
          {
            if (percent <= chance)
            {
              char_from_room(ch);
              act("$n descends $p.", FALSE, ch, obj, 0,TO_ROOM);
              char_to_room(ch, real_room(GET_OBJ_VAL(obj, 1)));
              act("$n has arrived.", FALSE, ch, obj, 0, TO_ROOM);
              LOOK(ch);
            }
            else
            {
              act("As you start descending $p, you slip and tumble down.", FALSE, ch, obj, 0, TO_CHAR);
              char_from_room(ch);
              char_to_room(ch, real_room(GET_OBJ_VAL(obj, 1)));
              act("$n starts descending $p but slips and tumbles down,head over heals.\r\nThat's got to hurt.", FALSE, ch, obj, 0, TO_ROOM);
              damage(ch, ch, (GET_HIT(ch) / 4), TYPE_UNDEFINED);
              LOOK(ch);
            }

            entry_memory_mtrigger(ch);
            greet_mtrigger(ch, -1);
            greet_memory_mtrigger(ch);
            enter_wtrigger(IN_ROOM(ch), ch, -1);
          }
          return;
        }
        else if (GET_OBJ_TYPE(obj) == ITEM_CLIMBABLE){
           act("There is no $p here that you can descend.", FALSE, ch, obj, 0, TO_CHAR);
          return;
       }
      }
    }
    ch->Send( "There is no %s here.\r\n", buf);
  }
  else
  {
    send_to_char("What do you want to descend?\r\n", ch);
  }
}
