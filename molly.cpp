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
  Character *found_char = NULL;
  struct obj_data *found_obj = NULL;
  char arg[MAX_STRING_LENGTH];

  if (!ch->desc)
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    if ( IN_ROOM ( ch ) != NULL )
      ch->Send( "%s", IN_ROOM(ch)->smell);
    return;
  }

  generic_find ( arg, FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP |
                      FIND_CHAR_ROOM, ch, &found_char, &found_obj );

  if ( found_obj != NULL )
  {
    if ( found_obj->smell != NULL )
      ch->Send ( "%s\r\n", found_obj->smell );
        else
      ch->Send ( "You smell nothing special.\r\n" );
  }

  else if ( found_char != NULL )
  {
    act ( "You lean in close to $N and take a deep breath. Mmmmm ...", FALSE, ch, 0, found_char, TO_CHAR );
    act ( "$n leans in close and takes a deep breath..", FALSE, ch, 0, found_char, TO_VICT );
    act ( "$n smells $N.", FALSE, ch, 0, found_char, TO_NOTVICT );
  }
  
  else ch->Send ( "You do not see that here.\r\n" );

}

ACMD(do_taste)
{
  Character *found_char = NULL;
  struct obj_data *found_obj = NULL;
  char arg[MAX_STRING_LENGTH];

  if (!ch->desc)
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("What do you want to taste?\r\n", ch);
    return;
  }

  generic_find(arg, FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP |
                    FIND_CHAR_ROOM, ch, &found_char, &found_obj );

  if ( found_obj != NULL )
  {
    if ( found_obj->taste != NULL )
      ch->Send ( "%s\r\n", found_obj->taste );
        else
      ch->Send ( "You taste nothing special.\r\n" );
  }

  else ch->Send ( "You do not see that here.\r\n" );

}

ACMD(do_feel)
{
  Character *found_char = NULL;
  struct obj_data *found_obj = NULL;
  char arg[MAX_STRING_LENGTH];

  if (!ch->desc)
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("What do you want to feel?\r\n", ch);
    return;
  }

  generic_find(arg, FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP |
                    FIND_CHAR_ROOM, ch, &found_char, &found_obj );

  if ( found_obj != NULL )
  {
    if ( found_obj->feel != NULL )
      ch->Send ( "%s\r\n", found_obj->feel );
        else
      ch->Send ( "You feel nothing special.\r\n" );
  }

  else ch->Send ( "You do not see that here.\r\n" );

}

ACMD(do_listen)
{
  if (!ch->desc)
    return;
  if ( IN_ROOM ( ch ) != NULL && IN_ROOM ( ch )->listen )
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

  if ( IN_ROOM ( ch ) != NULL && (desc = find_exdesc(arg,
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

  if ( IN_ROOM ( ch ) != NULL && (desc = find_exdesc(arg,
                     IN_ROOM(ch)->look_behind_description)) != NULL)
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

  if ( IN_ROOM ( ch ) != NULL && (desc = find_exdesc(arg,
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
  Character *dummy = NULL;

  percent = number(1, 101);
  chance = 70;

  if (GET_CLASS(ch) == CLASS_THIEF)
    chance = 75;
  
  chance += GET_DEX(ch);

  one_argument(argument, buf);

  if (*buf)
  {			/* an argument was supplied, search keyword */
    generic_find ( buf, FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP | FIND_CHAR_ROOM, ch, &dummy, &obj );

    if ( obj && CAN_SEE_OBJ ( ch, obj ) )
    {
      if (GET_OBJ_TYPE(obj) == ITEM_CLIMBABLE)
      {
        if (real_room(GET_OBJ_VAL(obj, 1)) != NULL)
        {
          if (percent <= chance)
          {
            act("$n climbs $p.", FALSE, ch, obj, 0, TO_ROOM);
            char_from_room ( ch );
            char_to_room(ch, real_room(GET_OBJ_VAL(obj, 1)));
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
          return;
        }
        log ( "SYSERR: obj vnum %d has a non-existing room to climb to", GET_OBJ_VNUM ( obj ) );
        return;
      }
      else {
        ch->Send ( "You can't climb %s.\r\n", obj->short_description );
        return;
      }
    }
    ch->Send( "There is no %s here.\r\n", buf);
  }
  else
    send_to_char("What do you want to climb?\r\n", ch);
}

ACMD(do_descend)
{
  struct obj_data *obj = NULL;
  int percent = 0, chance = 0;
  char buf[MAX_STRING_LENGTH];
  Character *dummy = NULL;

  percent = number(1, 101);
  chance = 70;

  if (GET_CLASS(ch) == CLASS_THIEF)
    chance = 75;
  
  chance += GET_DEX(ch);

  one_argument(argument, buf);

  if (*buf)
  {			/* an argument was supplied, search keyword */
      generic_find ( buf, FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP | FIND_CHAR_ROOM, ch, &dummy, &obj );

      if ( obj && CAN_SEE_OBJ ( ch, obj ) )
      {
        if (GET_OBJ_TYPE(obj) == ITEM_DESCENDABLE)
        {
          if (real_room(GET_OBJ_VAL(obj, 1)) != NULL)
          {
            if (percent <= chance)
            {
              act("$n descends $p.", FALSE, ch, obj, 0,TO_ROOM);
              char_from_room ( ch );
              char_to_room(ch, real_room(GET_OBJ_VAL(obj, 1)));
              act("$n has arrived.", FALSE, ch, obj, 0, TO_ROOM);
              LOOK(ch);
              entry_memory_mtrigger(ch);
              greet_mtrigger(ch, -1);
              greet_memory_mtrigger(ch);
              enter_wtrigger(IN_ROOM(ch), ch, -1);
            }
            else
            {
              act("As you start descending $p, you slip and tumble down.", FALSE, ch, obj, 0, TO_CHAR);
              act("$n starts descending $p but slips and tumbles down, head over heals.\r\nThat's got to hurt.", FALSE, ch, obj, 0, TO_ROOM);
              damage(ch, ch, (GET_HIT(ch) / 4), TYPE_UNDEFINED);
            }
            return;
          }
          log ( "SYSERR: obj vnum %d has a non-existing room to descend to", GET_OBJ_VNUM ( obj ) );
          return;
        }
        else {
          ch->Send ( "You can't descend %s.\r\n", obj->short_description );
          return;
        }
      }
      ch->Send( "There is no %s here.\r\n", buf);
    }
    else
      send_to_char("What do you want to descend?\r\n", ch);
}
