/* ************************************************************************
*   File: act.offensive.c                               Part of CircleMUD *
*  Usage: player-level commands of an offensive nature                    *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "dg_event.h"
#include "dg_scripts.h"
#include "constants.h"
#include "fight.h"
#include "descriptor.h"

/* extern variables */
int can_fight(Character *ch, Character *vict, int silent);


/* extern functions */
void raw_kill(Character *ch, Character *killer);
int perform_move(Character *ch, int dir, int specials_check);
struct ignore *find_ignore(struct ignore *ignore_list, char *str);
void write_ignorelist(Character *ch);
int compute_armor_class(Character *ch);
void print_ignorelist(Character *ch, Character *vict);
int fe_after_damage(Character* ch, Character* vict, int damage, int w_type);
//
/* Daniel Houghton's revised external functions */
int skill_roll(Character *ch, int skill_num);
void strike_missile(Character *ch, Character *tch,
                    struct obj_data *missile, int dir, int attacktype);
void miss_missile(Character *ch, Character *tch,
                  struct obj_data *missile, int dir, int attacktype);
void mob_reaction(Character *ch, Character *vict, int dir);
void fire_missile(Character *ch, char arg1[MAX_INPUT_LENGTH],
                  struct obj_data *missile, int pos, int range, int dir);
void check_killer(Character *ch, Character *vict);
int computer_armor_class(Character *ch);
int arena_ok(Character *ch, Character *victim);
void improve_skill(Character *ch, int skill);




/* local functions */
ACMD(do_assist);
ACMD(do_hit);
ACMD(do_backstab);
ACMD(do_order);
ACMD(do_flee);
ACMD(do_bash);
ACMD(do_rescue);
ACMD(do_kick);
ACMD(do_slay);
ACMD(do_trample);
void send_not_to_spam(char *buf, Character *ch,
                      Character *victim, struct obj_data *weap,
                      int spam);
void perform_assist(Character *ch, Character *helpee);


ACMD(do_assist)
{
  Character *helpee;
  char arg[MAX_INPUT_LENGTH];

  if (FIGHTING(ch))
  {
    send_to_char
    ("You're already fighting!  How can you assist someone else?\r\n",
     ch);
    return;
  }
  one_argument(argument, arg);

  if (!*arg)
    send_to_char("Whom do you wish to assist?\r\n", ch);
  else if (!(helpee = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)))
    ch->Send( "%s", CONFIG_NOPERSON);
  else if (helpee == ch)
    send_to_char("You can't help yourself any more than this!\r\n",
                 ch);
  else
    perform_assist(ch, helpee);
}

void perform_assist(Character *ch, Character *helpee)
{
  Character  *opponent;


  /*
   * Hit the same enemy the person you're helping is.
   */
  if (FIGHTING(helpee))
    opponent = FIGHTING(helpee);
  else
    for (opponent = ch->in_room->people;
         opponent && (FIGHTING(opponent) != helpee);
         opponent = opponent->next_in_room);

  if (!opponent)
    act("But nobody is fighting $M!", FALSE, ch, 0, helpee,
        TO_CHAR);
  else if (!CAN_SEE(ch, opponent))
    act("You can't see who is fighting $M!", FALSE, ch, 0, helpee,
        TO_CHAR);
  else if (!CONFIG_PK_ALLOWED && !IS_NPC(opponent)
           && !arena_ok(ch, opponent))
    /* prevent accidental pkill */
    act("Use 'murder' if you really want to attack $N.", FALSE,
        ch, 0, opponent, TO_CHAR);
  else
  {
    if (ch->nr != real_mobile(DG_CASTER_PROXY) &&
        ch != opponent &&
        ROOM_FLAGGED(IN_ROOM(ch), ROOM_PEACEFUL))
    {
      send_to_char("This room just has such a peaceful, easy feeling...\r\n", ch);
      return;
    }
    if (!can_fight(ch, opponent, FALSE))
      return;
    send_to_char("You join the fight!\r\n", ch);
    act("$N assists you!", 0, helpee, 0, ch, TO_CHAR);
    act("{cg$n engages in combat with $N.{c0",FALSE, ch, NULL, opponent, TO_ROOM);
    start_fighting(ch, opponent);
  }

}

ACMD(do_hit)
{
  Character *vict;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("Hit who?\r\n", ch);
    return;
  }
  if (!(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)))
  {
#if KILL_ALL_ENABLED
    if ( !strcmp(arg, "all") )
    {
      if ( ROOM_FLAGGED(IN_ROOM(ch), ROOM_PEACEFUL) )
      {
        send_to_char("This room just has such a peaceful, easy feeling...\r\n", ch);
        return;
      }
      for (vict = IN_ROOM(ch)->people; vict ; vict = vict->next_in_room)
      {
        if (SELF(ch, vict))
          continue;
        if (AFF_FLAGGED(ch, AFF_CHARM))
        {
          if (!IS_NPC(ch->master) && !IS_NPC(vict))
            continue;
          if (ch->master == vict)
            continue;
        }
        //continue;
        if ((vict->master == ch) && AFF_FLAGGED(vict, AFF_CHARM))
          continue;
        if (!CAN_SEE(ch, vict))
          continue;
        if (!can_fight(ch, vict, TRUE))
          continue;
        if (!CONFIG_PK_ALLOWED && !arena_ok(ch, vict))
        {
          if (!IS_NPC(vict) && !IS_NPC(ch))
            continue;
          if (FIGHTING(vict))
            continue;

          act("{cg$n engages in combat with $N.{c0",FALSE, ch, NULL, vict, TO_ROOM);
          ch->Send( "You engage in combat with %s!\r\n", GET_NAME(vict));
          if (!FIGHTING(ch))
          {
            start_fighting(ch, vict);
          }
          else
          {
            fe_after_damage(ch, vict, 0, 0);
          }
        }
      }
      /*
      if (FIGHTING(ch))
      send_to_char("They don't seem to be here.\r\n", ch);
      */
      return;
    }
#endif
    send_to_char("They don't seem to be here.\r\n", ch);
    return;
  }
  if (vict == ch)
  {
    send_to_char("You hit yourself...OUCH!.\r\n", ch);
    act("$n hits $mself, and says OUCH!", FALSE, ch, 0, vict, TO_ROOM);
    return;
  }
  if (AFF_FLAGGED(ch, AFF_CHARM) && (ch->master == vict))
  {
    act("$N is just such a good friend, you simply can't hit $M.", FALSE, ch, 0, vict, TO_CHAR);
    return;
  }
  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_PEACEFUL))
  {
    send_to_char("This room just has such a peaceful, easy feeling...\r\n", ch);
    return;
  }
  if (!can_fight(ch, vict, FALSE))
  {
    return;
  }

  if (!CONFIG_PK_ALLOWED && !arena_ok(ch, vict))
  {
    if (!IS_NPC(vict) && !IS_NPC(ch))
    {
      if (subcmd != SCMD_MURDER)
      {
        send_to_char("Use 'murder' to hit another player.\r\n", ch);
        return;
      }
      else
      {
        check_killer(ch, vict);
      }
    }
    if (AFF_FLAGGED(ch, AFF_CHARM) && !IS_NPC(ch->master)
        && !IS_NPC(vict))
      return;       /* you can't order a charmed pet to attack a player */
  }

  //if ((GET_POS(ch) == POS_STANDING) && (vict != FIGHTING(ch))) {
  if (!FIGHTING(ch))
  {
    send_not_to_spam( "{cg$n engages in combat with $N.{c0", ch, vict, NULL, 1);
    ch->Send( "You engage in combat with %s!\r\n", GET_NAME(vict));
    start_fighting(ch, vict);
  }
  else
    send_to_char("You do the best you can!\r\n", ch);

  return;
}

ACMD(do_slay)
{
  Character *vict;
  char arg2[MAX_INPUT_LENGTH];
  char arg[MAX_INPUT_LENGTH];

  two_arguments(argument, arg, arg2);

  if (!*arg)
  {
    send_to_char("Kill who?\r\n", ch);
  }
  else
  {
    if (!(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)))
      send_to_char("They aren't here.\r\n", ch);
    else if (ch == vict)
      send_to_char("Your mother would be so sad.. :(\r\n", ch);
    else if (GET_LEVEL(ch)<GET_LEVEL(vict) && !IS_NPC(vict))
    {
      ch->Send( "You chop %s to pieces, but %s reforms immediately and slices your throat!\r\n", GET_NAME(vict), HSSH(vict));
      vict = ch;
    }
    else if (!str_cmp(arg2, "skin"))
    {
      act("You rip the flesh from $N and send $S soul to the fiery depths\r\n" "of hell.", FALSE, ch, 0, vict, TO_CHAR);
      act("Your flesh has been torn from your bones and your bodiless soul\r\n" "now watches your bones incinerate in the fires of hell.", FALSE, ch, 0, vict, TO_VICT);
      act("$n rips the flesh off of $N, releasing $S soul into the fiery\r\n" "depths of hell.", FALSE, ch, 0, vict, TO_NOTVICT);
    }
    else if (!str_cmp(arg2, "xwife") && GET_LEVEL(ch) == LVL_IMPL)
    {
      act("You whip out the rotting leg of Colin's x-wife and beat $N\r\n" "to death with it!.", FALSE, ch, NULL, vict, TO_CHAR);
      act("$n pulls out the rotting leg of Colin's x-wife and beats $N\r\n" "to death with it!", FALSE, ch, NULL, vict, TO_VICT);
      act("$n pulls out the rotting leg of Colin's x-wife and beats $N\r\n" "to death with it!", FALSE, ch, NULL, vict, TO_NOTVICT);
    }
    else if (!str_cmp(arg2, "deheart"))
    {
      act("You rip through $N's chest and pull out $S beating heart in\r\n" "your hand.", FALSE, ch, NULL, vict, TO_CHAR);
      act("You feel a sharp pain as $n rips into your chest and pulls\r\n" "out your still beating heart in $S hand.", FALSE, ch, NULL, vict, TO_VICT);
      act("Specks of blood hit your face as $n rips through $N's chest\r\n" "pulling out $S still beating heart.", FALSE, ch, NULL, vict, TO_NOTVICT);
    }
    else if (!str_cmp(arg2, "immolate"))
    {
      act("Your fireball turns $N into a blazing inferno.", FALSE, ch, NULL, vict, TO_CHAR);
      act("$n releases a searing fireball in your direction.", FALSE, ch, NULL, vict, TO_VICT);
      act("$n points at $N, who bursts into a flaming inferno.", FALSE, ch, NULL, vict, TO_NOTVICT);
    }
    else if (!str_cmp(arg2, "shatter"))
    {
      act("You freeze $N with a glance and shatter the frozen corpse\r\n" "into tiny shards.", FALSE, ch, NULL, vict, TO_CHAR);
      act("$n freezes you with a glance and shatters your frozen body\r\n" "into tiny shards.", FALSE, ch, NULL, vict, TO_VICT);
      act("$n freezes $N with a glance and shatters the frozen\r\n body into tiny shards.", FALSE, ch, NULL, vict, TO_NOTVICT);
    }
    else if (!str_cmp(arg2, "demon"))
    {
      act(
        "You gesture and a slavering demon appears.  With a horrible grin,\r\n"
        "the foul creature turns on $N, who screams in panic before being\r\n"
        "eaten alive.", FALSE, ch, NULL, vict, TO_CHAR);
      act("$n gestures and a slavering demon appears.  The foul creature\r\n"
          "turns on you with a horrible grin.  You scream in panic before\r\n"
          "being eaten alive.", FALSE, ch, NULL, vict, TO_VICT);
      act("$n gestures and a slavering demon appears.  With a horrible grin,\r\n"
          "the foul creature turn on $N, who screams in panic before being\r\n"
          "eaten alive.", FALSE, ch, NULL, vict, TO_NOTVICT);
    }
    else if (!str_cmp(arg2, "pounce"))
    {
      act("Leaping upon $N with bared fangs, you tear open $S throat\r\n"
          "and toss the corpse to the ground.", FALSE, ch, NULL, vict, TO_CHAR);
      act("In a heartbead, $n rips $s fangs through your throat!\r\n"
          "Your blood sprays and pours to the ground as your life ends...",
          FALSE, ch, NULL, vict, TO_VICT);
      act("Leaping suddenly, $n sinks $s fangs into $N's throat.  As blood\r\n"
          "sprays and gushes, $n tosses $N's dying body away", FALSE, ch, NULL, vict, TO_NOTVICT);
    }
    else if (!str_cmp(arg2, "slit"))
    {
      act("You calmly slit $N's throat.", FALSE, ch, NULL, vict, TO_CHAR);
      act("$n reaches out with a clawed finger and calmly slits your throat.", FALSE, ch, NULL, vict, TO_VICT);
      act("A claw extends from $n's hand as $M clamly slits $N's throat.", FALSE, ch, NULL, vict, TO_NOTVICT);
    }
    else if (!str_cmp(arg2, "wax"))
    {
      act("You hold down $N and give them a bikini wax to die for.", FALSE, ch, NULL, vict, TO_CHAR);
      act("$n holds you down and gives you a bikini wax to die for.", FALSE, ch, NULL, vict, TO_VICT);
      act("$n holds $N down and gives $M a bikini wax to die for.", FALSE, ch, NULL, vict, TO_NOTVICT);
    }
    else
    {
      act("You chop $M to pieces!  Ah!  The blood!", FALSE, ch, 0,
          vict, TO_CHAR);
      act("$N chops you to pieces!", FALSE, vict, 0, ch, TO_CHAR);
      act("$n brutally slays $N!", FALSE, ch, 0, vict, TO_NOTVICT);
    }
    if (vict)
      raw_kill(vict, ch);

  }
}


ACMD(do_order)
{
  char name[MAX_INPUT_LENGTH], message[MAX_INPUT_LENGTH];
  bool found = FALSE;
  room_rnum org_room;
  Character *vict;
  struct follow_type *k;
  char buf[MAX_INPUT_LENGTH];

  half_chop(argument, name, message);

  if (!*name || !*message)
    send_to_char("Order who to do what?\r\n", ch);
  else if (!(vict = get_char_vis(ch, name, NULL, FIND_CHAR_ROOM))
           && !is_abbrev(name, "followers"))
    send_to_char("That person isn't here.\r\n", ch);
  else if (ch == vict)
    send_to_char("You obviously suffer from skitzofrenia.\r\n", ch);

  else
  {
    if (AFF_FLAGGED(ch, AFF_CHARM))
    {
      send_to_char
      ("Your superior would not aprove of you giving orders.\r\n",
       ch);
      return;
    }
    if (vict)
    {
      snprintf(buf, sizeof(buf), "$N orders you to '%s'", message);
      act(buf, FALSE, vict, 0, ch, TO_CHAR);
      act("$n gives $N an order.", FALSE, ch, 0, vict, TO_ROOM);

      if ((vict->master != ch) || !AFF_FLAGGED(vict, AFF_CHARM))
        act("$n has an indifferent look.", FALSE, vict, 0, 0,
            TO_ROOM);
      else
      {
        ch->Send( "%s", CONFIG_OK);
        command_interpreter(vict, message);
      }
    }
    else
    {          /* This is order "followers" */
      snprintf(buf, sizeof(buf), "$n issues the order '%s'.", message);
      act(buf, FALSE, ch, 0, vict, TO_ROOM);

      org_room = ch->in_room;

      for (k = ch->followers; k; k = k->next)
      {
        if (org_room == k->follower->in_room)
          if (AFF_FLAGGED(k->follower, AFF_CHARM))
          {
            found = TRUE;
            command_interpreter(k->follower, message);
          }
      }
      if (found)
        ch->Send( "%s", CONFIG_OK);
      else
        send_to_char
        ("Nobody here is a loyal subject of yours!\r\n", ch);
    }
  }
}



ACMD(do_flee)
{
  int i, attempt, loss;
  Character *was_fighting;
  void halt_fighting(Character *ch);

  if (AFF_FLAGGED(ch, AFF_HOLD))
  {
    send_to_char("You have been snared and can't flee.\r\n", ch);
    return;
  }

  if (GET_POS(ch) < POS_FIGHTING)
  {
    send_to_char("You are in pretty bad shape, unable to flee!\r\n",
                 ch);
    return;
  }
  if (!number(0, 4) && IS_NPC(ch))
    return;

  for (i = 0; i < 6; i++)
  {
    attempt = number(0, NUM_OF_DIRS - 1);    /* Select a random direction */
    if (CAN_GO(ch, attempt) &&
        !ROOM_FLAGGED(EXIT(ch, attempt)->to_room, ROOM_DEATH))
    {
      act("$n panics, and attempts to flee!", TRUE, ch, 0, 0,  TO_ROOM);
      was_fighting = FIGHTING(ch);

      if ( do_simple_move(ch, attempt, TRUE))
      {
        if (was_fighting && !DEAD(was_fighting))
        {
          stop_fighting(was_fighting);
        }
        stop_fighting(ch);


        send_to_char("You flee head over heels.\r\n", ch);
        if (was_fighting && !DEAD(was_fighting) && !IS_NPC(ch) && IS_NPC(was_fighting) &&
            !(GET_LEVEL(ch) <= 20 && REMORTS(ch) == 0))
        {
          loss = FTOI(IRANGE(0, GET_EXP(was_fighting) * 0.3, CONFIG_MAX_EXP_LOSS * 5));
          ch->Send( "[You lose %d exp]\r\n", loss);
          gain_exp(ch, -loss);
        }
      }
      else
      {
        act("$n tries to flee, but can't!", TRUE, ch, 0, 0,
            TO_ROOM);
      }
      return;
    }
  }
  send_to_char("PANIC!  You couldn't escape!\r\n", ch);
}





ACMD(do_shoot)
{
  struct obj_data *wielding;
  struct obj_data *holding;

  char arg2[MAX_INPUT_LENGTH];
  char arg1[MAX_INPUT_LENGTH];
  int dir, range;

  wielding = GET_EQ(ch, WEAR_WIELD);
  holding = GET_EQ(ch, WEAR_HOLD);

  //broken
  //return;

  if (!wielding)
  {
    send_to_char("You aren't wielding a shooting weapon!\r\n", ch);
    return;
  }

  if (!holding && GET_OBJ_TYPE(wielding) != ITEM_GUN)
  {
    send_to_char("You need to be holding a projectile!\r\n", ch);
    return;
  }

  if (GET_OBJ_TYPE(wielding) == ITEM_GUN)
  {

    if (wielding->obj_flags.value[3]>0)
    {
      holding = read_object(wielding->obj_flags.value[3], VIRTUAL);
      if (holding)
      {
        if (GET_OBJ_TYPE(holding) != ITEM_AMMO)
          holding = NULL;
      }
      else
      {
        holding = NULL;
      }
    }

  }
  else
    holding = GET_EQ(ch, WEAR_HOLD);

  if (!holding || holding < 0)
  {
    ch->Send( "Snap Its Broken! It probably has the wrong kinda ammo in it.\r\n");
    return;
  }

  if ((GET_OBJ_TYPE(wielding) == ITEM_SLING) &&
      (GET_OBJ_TYPE(holding) == ITEM_ROCK))
    range = wielding->obj_flags.value[0];
  else if ((GET_OBJ_TYPE(holding) == ITEM_ARROW) &&
           (GET_OBJ_TYPE(wielding) == ITEM_BOW))
    range = wielding->obj_flags.value[0];
  else if ((GET_OBJ_TYPE(holding) == ITEM_BOLT) &&
           (GET_OBJ_TYPE(wielding) == ITEM_CROSSBOW))
    range = wielding->obj_flags.value[0];
  else if (GET_OBJ_TYPE(wielding) == ITEM_GUN)
    range = wielding->obj_flags.value[0];
  else
  {
    send_to_char
    ("You should wield a ranged weapon and hold a projectile!\r\n",
     ch);
    return;
  }

  two_arguments(argument, arg1, arg2);

  if (!*arg1 || !*arg2)
  {
    send_to_char("You should try: shoot <someone> <direction>\r\n",
                 ch);
    return;
  }

  if ((GET_OBJ_TYPE(wielding) == ITEM_GUN) &&     //gonna add auto reload to this.- mord
      wielding->obj_flags.value[2] <= 0)
  {
    send_to_char("Your weapon is out of ammo.\r\n", ch);
    return;
  }

  if (IS_DARK(ch->in_room))
  {
    send_to_char("You can't see that far.\r\n", ch);
    return;
  }

  if ((dir = search_block(arg2, dirs, FALSE)) < 0)
  {
    send_to_char("What direction?\r\n", ch);
    return;
  }

  if (!CAN_GO(ch, dir))
  {
    send_to_char("Something blocks the way!\r\n", ch);
    return;
  }

  if (range > 3)
    range = 3;
  if (range < 1)
    range = 1;

  wielding->obj_flags.value[2] = wielding->obj_flags.value[2] - 1;

  fire_missile(ch, arg1, holding, WEAR_HOLD, range, dir);

}


ACMD(do_throw)
{

  /* sample format: throw monkey east
     this would throw a throwable or grenade object wielded
     into the room 1 east of the pc's current room. The chance
     to hit the monkey would be calculated based on the pc's skill.
     if the wielded object is a grenade then it does not 'hit' for
     damage, it is merely dropped into the room. (the timer is set
     with the 'pull pin' command.) */

  struct obj_data *missile;
  int dir, range = 1;
  char arg2[MAX_INPUT_LENGTH];
  char arg1[MAX_INPUT_LENGTH];
  two_arguments(argument, arg1, arg2);

  /* only two types of throwing objects:
     ITEM_THROW - knives, stones, etc
     ITEM_GRENADE - calls tick_grenades.c . */

  if (!(GET_EQ(ch, WEAR_WIELD)))
  {
    send_to_char("You should wield something first!\r\n", ch);
    return;
  }

  missile = GET_EQ(ch, WEAR_WIELD);

  if (!((GET_OBJ_TYPE(missile) == ITEM_THROW) ||
        (GET_OBJ_TYPE(missile) == ITEM_GRENADE)))
  {
    send_to_char("You should wield a throwing weapon first!\r\n", ch);
    return;
  }

  if (GET_OBJ_TYPE(missile) == ITEM_GRENADE)
  {
    if (!*arg1)
    {
      send_to_char("You should try: throw <direction>\r\n", ch);
      return;
    }
    if ((dir = search_block(arg1, dirs, FALSE)) < 0)
    {
      send_to_char("What direction?\r\n", ch);
      return;
    }
  }
  else
  {

    two_arguments(argument, arg1, arg2);

    if (!*arg1 || !*arg2)
    {
      send_to_char("You should try: throw <someone> <direction>\r\n",
                   ch);
      return;
    }

    /* arg2 must be a direction */

    if ((dir = search_block(arg2, dirs, FALSE)) < 0)
    {
      send_to_char("What direction?\r\n", ch);
      return;
    }
  }

  /* make sure we can go in the direction throwing. */
  if (!CAN_GO(ch, dir))
  {
    send_to_char("Something blocks the way!\r\n", ch);
    return;
  }
  improve_skill(ch, SKILL_THROW);
  fire_missile(ch, arg1, missile, WEAR_WIELD, range, dir);
}

ACMD(do_ignore)
{
  Character *victim;
  struct ignore *a, *temp;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!*arg)
  {
    ch->Send( "SYNTAX: ignore <victim>\r\n");
    ch->Send( "Currently on your list:\r\n");
    print_ignorelist(ch, ch);
  }
  else if ((a = find_ignore(GET_IGNORELIST(ch), arg)) != NULL)
  {
    REMOVE_FROM_LIST(a, GET_IGNORELIST(ch), next);
    ch->Send( "You no longer ignore them.\r\n");
    free(a->ignore);
    free(a);
    write_ignorelist(ch);
  }
  else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
    ch->Send( "%s", CONFIG_NOPERSON);
  else if (victim == ch)
    ch->Send( "Ignore yourself?  Go seek help!\r\n");
  else if (IS_NPC(victim))
    ch->Send( "No ignoring monsters.  That isn't nice.\r\n");

  else
  {
    CREATE(a, struct ignore, 1);
    a->ignore = str_dup(arg);
    a->next = GET_IGNORELIST(ch);
    GET_IGNORELIST(ch) = a;
    ch->Send( "%s", CONFIG_OK);
    write_ignorelist(ch);
  }
}

/*
         gain = 0;
 
              if (AFF_FLAGGED(ch, AFF_BURNING) &&
          SECT(IN_ROOM(ch)) == SECT_UNDERWATER) {
          REMOVE_BIT_AR(AFF_FLAGS(ch), AFF_BURNING);
         }
         if (AFF_FLAGGED(ch, AFF_ACIDED) &&
          SECT(IN_ROOM(ch)) == SECT_UNDERWATER) {
          REMOVE_BIT_AR(AFF_FLAGS(ch), AFF_ACIDED);
         }
         if (AFF_FLAGGED(ch, AFF_FREEZING) &&
          SECT(IN_ROOM(ch)) == SECT_DESERT) {
          REMOVE_BIT_AR(AFF_FLAGS(ch), AFF_FREEZING);
         }
         if (AFF_FLAGGED(ch, AFF_BURNING)
          && !(AFF_FLAGGED(ch, AFF_PROT_FIRE))) {
          gain += BURN_DAMAGE*3;
          if (!IS_NPC(ch))
          ch->Send( "{crYour skin burns!{c0\r\n");
          }
          
         if (AFF_FLAGGED(ch, AFF_FREEZING)
          && !(AFF_FLAGGED(ch, AFF_PROT_COLD))) {
          gain += BURN_DAMAGE*3;
          if (!IS_NPC(ch))
          ch->Send( "{cCYou shiver with cold!{c0\r\n");
          }
         if (AFF_FLAGGED(ch, AFF_ACIDED)
          && !(AFF_FLAGGED(ch, AFF_STONESKIN))) {
          gain += BURN_DAMAGE*3;
          if (!IS_NPC(ch))
          ch->Send( "{cgYou skin bubbles with concentrated acid!{c0\r\n");
          }
         
    if (AFF_FLAGGED(ch, AFF_POISON_1))
     gain += (GET_MANA(ch) / 7);
 
    if (AFF_FLAGGED(ch, AFF_POISON_2))
     gain += (GET_MANA(ch) / 5);
 
    if (AFF_FLAGGED(ch, AFF_POISON_3))
     gain += (GET_MANA(ch) / 3);
 
    if (AFF_FLAGGED(ch, AFF_POISON_4))
     gain += (GET_MANA(ch) / 2);
   }
*/

