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

/* extern variables */
int can_fight(struct char_data *ch, struct char_data *vict);
struct aff_dam_event_obj
{
  struct char_data* ch;
  int damage;
  int interval;
  int recurse;
  int (*event_fun)(int, struct char_data*);
  int id;
};

struct aff_dam_event_list
{
  struct char_data* ch;
  struct event* event;
  struct aff_dam_event_list* next;
  int id;
};

/* extern functions */
void raw_kill(struct char_data *ch, struct char_data *killer);
int perform_move(struct char_data *ch, int dir, int specials_check);
struct ignore *find_ignore(struct ignore *ignore_list, char *str);
int compute_armor_class(struct char_data *ch);
void print_ignorelist(struct char_data *ch, struct char_data *vict);
int fe_after_damage(struct char_data* ch, struct char_data* vict, int damage, int w_type);
//
/* Daniel Houghton's revised external functions */
int skill_roll(struct char_data *ch, int skill_num);
void strike_missile(struct char_data *ch, struct char_data *tch,
                    struct obj_data *missile, int dir, int attacktype);
void miss_missile(struct char_data *ch, struct char_data *tch,
                  struct obj_data *missile, int dir, int attacktype);
void mob_reaction(struct char_data *ch, struct char_data *vict, int dir);
void fire_missile(struct char_data *ch, char arg1[MAX_INPUT_LENGTH],
                  struct obj_data *missile, int pos, int range, int dir);
void check_killer(struct char_data *ch, struct char_data *vict);
int computer_armor_class(struct char_data *ch);
int arena_ok(struct char_data *ch, struct char_data *victim);
void improve_skill(struct char_data *ch, int skill);

void aff_damage(struct char_data *ch, int dam, long interval, int recurse,
                int (*event_fun)(int, struct char_data*));
EVENTFUNC(affdam_event);
void add_ade(struct aff_dam_event_obj* new_ade_obj);
void cancel_affdam_events(struct char_data* ch);
void remove_affdam_event(int idnum);

struct aff_dam_event_list* ade_list = NULL;
struct aff_dam_event_list* ade_list_end = NULL;
int ade_counter = 0;

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
void send_not_to_spam(char *buf, struct char_data *ch,
                      struct char_data *victim, struct obj_data *weap,
                      int spam);

void aff_damage(struct char_data *ch, int dam, long interval, int recurse,
                int (*event_fun)(int, struct char_data*))
{
  struct aff_dam_event_obj* new_event;
  CREATE(new_event, struct aff_dam_event_obj, 1);
  new_event->ch = ch;
  new_event->damage = dam;
  new_event->interval = interval;
  new_event->recurse = recurse;
  new_event->event_fun = event_fun;
  new_event->id = ade_counter++;
  add_ade(new_event);
}

EVENTFUNC(affdam_event)
{
  struct aff_dam_event_obj* ade_obj = (struct aff_dam_event_obj*) event_obj;
  struct aff_dam_event_obj* new_event;

  remove_affdam_event(ade_obj->id);
  if(ade_obj->recurse < 0)
  {
    free(ade_obj);
    return 0;
  }

  if(ade_obj->event_fun(ade_obj->damage, ade_obj->ch))
  {
    CREATE(new_event, struct aff_dam_event_obj, 1);
    new_event->event_fun = ade_obj->event_fun;
    new_event->damage = ade_obj->damage;
    new_event->interval = ade_obj->interval;
    new_event->recurse = (ade_obj->recurse > 0 ? ade_obj->recurse - 1 : 0);
    new_event->ch = ade_obj->ch;
    new_event->id = ade_counter++;
    add_ade(new_event);
  }

  free(ade_obj);
  return 1;
}

void add_ade(struct aff_dam_event_obj* new_ade_obj)
{
  struct aff_dam_event_list* new_elem;
  long time = 0;
  CREATE(new_elem, struct aff_dam_event_list, 1);

  if(ade_list_end)
  {
    ade_list_end->next = new_elem;
    ade_list_end = new_elem;
  }
  else
  {
    ade_list_end = ade_list = new_elem;
  }

  new_elem->next = NULL;
  new_elem->ch = new_ade_obj->ch;
  new_elem->id = new_ade_obj->id;
  time = new_ade_obj->interval RL_SEC;
  new_elem->event = event_create(affdam_event, new_ade_obj, time);
}

void cancel_affdam_events(struct char_data* ch)
{
  struct aff_dam_event_list* current;
  struct aff_dam_event_list* oneback;

  current = ade_list;
  while (current && current->ch == ch)
  {
    ade_list = current->next;
    event_cancel(current->event);
    free(current);
    current = ade_list;
  }

  if(current)
  {
    oneback = current;
    current = current->next;
  }

  while(current)
  {
    if(current->ch == ch)
    {
      oneback->next = current->next;
      event_cancel(current->event);
      free(current);
      current = oneback->next;
    }
    else
    {
      oneback = current;
      current = current->next;
    }
  }
}

void remove_affdam_event(int idnum)
{
  struct aff_dam_event_list* current;
  struct aff_dam_event_list* oneback;

  current = ade_list;
  if(current && current->id == idnum)
  {
    ade_list = current->next;
    free(current);
    return;
  }

  oneback = current;
  if(current) current = current->next;
  while(current)
  {
    if(current->id == idnum)
    {
      oneback->next = current->next;
      free(current);
      break;
    }
    else
    {
      oneback = current;
      current = current->next;
    }
  }
}

ACMD(do_assist)
{
  struct char_data *helpee, *opponent;
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
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
  else if (helpee == ch)
    send_to_char("You can't help yourself any more than this!\r\n",
                 ch);
  else
  {
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
      if (!can_fight(ch, opponent))
        return;
      send_to_char("You join the fight!\r\n", ch);
      act("$N assists you!", 0, helpee, 0, ch, TO_CHAR);
      send_not_to_spam("{cg$n engages in combat with $N.{c0", ch, opponent, NULL, 0);
      start_fighting(ch, opponent);
    }
  }
}


ACMD(do_hit)
{
  struct char_data *vict;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("Hit who?\r\n", ch);
    return;
  }
  if (!(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)))
  {
  /**Disabled for the moment**/
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
        continue;
        if ((vict->master == ch) && AFF_FLAGGED(vict, AFF_CHARM))
          continue;
        if (!CAN_SEE(ch, vict))
          continue;
        if (!can_fight(ch, vict))
          continue;
        if (!CONFIG_PK_ALLOWED && !arena_ok(ch, vict))
        {
          if (!IS_NPC(vict) && !IS_NPC(ch))
            continue;
          
          send_not_to_spam( "{cg$n engages in combat with $N.{c0", ch, vict, NULL, 0);
          new_send_to_char(ch, "You engage in combat with %s!\r\n", GET_NAME(vict));
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
      if (!FIGHTING(ch))      
    send_to_char("They don't seem to be here.\r\n", ch);
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
  if (!can_fight(ch, vict))
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
      return;		/* you can't order a charmed pet to attack a player */
  }

  //if ((GET_POS(ch) == POS_STANDING) && (vict != FIGHTING(ch))) {
  if (!FIGHTING(ch))
  {
    send_not_to_spam( "{cg$n engages in combat with $N.{c0", ch, vict, NULL, 0);
    new_send_to_char(ch, "You engage in combat with %s!\r\n", GET_NAME(vict));
    start_fighting(ch, vict);
  }
  else
    send_to_char("You do the best you can!\r\n", ch);

  return;
}

ACMD(do_slay)
{
  struct char_data *vict;
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
      new_send_to_char(ch, "You chop %s to pieces, but %s reforms immediately and slices your throat!\r\n", GET_NAME(vict), HSSH(vict));
      vict = ch;
    }
    else if (!str_cmp(arg2, "skin"))
    {
      act("You rip the flesh from $N and send $S soul to the fiery depths\r\n" "of hell.", FALSE, ch, 0, vict, TO_CHAR);
      act("Your flesh has been torn from your bones and your bodiless soul\r\n" "now watches your bones incinerate in the fires of hell.", FALSE, ch, 0, vict, TO_VICT);
      act("$n rips the fless off of $N, releasing $S soul into the fiery\r\n" "depths of hell.", FALSE, ch, 0, vict, TO_NOTVICT);
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
      act("Specks of blood hit your face as $n rips through $N's chest\r\n" "pulling out $S's still beating heart.", FALSE, ch, NULL, vict, TO_NOTVICT);
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
      "the foul creatre turns on $N, who screams in panic before being\r\n" 
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
      act("Leaping upon $N with bared fangs, you tear open $S's throat\r\n" 
      "and toss $S corpse to the ground.", FALSE, ch, NULL, vict, TO_CHAR);
      act("In a heartbead, $n rips $s fangs through your throat!\r\n"
          "Your blood sprays and pours to the ground as your life ends...",
          FALSE, ch, NULL, vict, TO_VICT);
      act("Leaping suddenly, $n sinks $s fangs into $N's throat.  As blood\r\n" 
      "sprays and gushes, $n tosses $N's dying body away", FALSE, ch, NULL, vict, TO_NOTVICT);
    }
    else if (!str_cmp(arg2, "slit"))
    {
      act("You calmly slit $N's throat.", FALSE, ch, NULL, vict,
          TO_CHAR);
      act("$n reaches out with a clawed finger and calmly slits your throat.", FALSE, ch, NULL, vict, TO_VICT);
      act("A claw extends from $n's hand as $M clamly slits $N's throat.", FALSE, ch, NULL, vict, TO_NOTVICT);
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
  struct char_data *vict;
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
        new_send_to_char(ch, "%s", CONFIG_OK);
        command_interpreter(vict, message);
      }
    }
    else
    {		/* This is order "followers" */
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
        new_send_to_char(ch, "%s", CONFIG_OK);
      else
        send_to_char
        ("Nobody here is a loyal subject of yours!\r\n", ch);
    }
  }
}



ACMD(do_flee)
{
  int i, attempt, loss;
  struct char_data *was_fighting;
  void halt_fighting(struct char_data *ch);

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
    attempt = number(0, NUM_OF_DIRS - 1);	/* Select a random direction */
    if (CAN_GO(ch, attempt) &&
        !ROOM_FLAGGED(EXIT(ch, attempt)->to_room, ROOM_DEATH))
    {
      act("$n panics, and attempts to flee!", TRUE, ch, 0, 0,
          TO_ROOM);
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
          loss = IRANGE(0, GET_EXP(was_fighting) * 0.3, CONFIG_MAX_EXP_LOSS * 5);
          new_send_to_char(ch, "[You lose %d exp]\r\n", loss);
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
    new_send_to_char(ch, "Snap Its Broken! It probably has the wrong kinda ammo in it.\r\n");
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

  if ((GET_OBJ_TYPE(wielding) == ITEM_GUN) &&	//gonna add auto reload to this.- mord
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
  struct char_data *victim;
  struct ignore *a, *temp;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("SYNTAX: ignore <victim>\r\n", ch);
    send_to_char("Currently on your list:\r\n", ch);
    print_ignorelist(ch, ch);
  }
  else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
  else if (victim == ch)
    send_to_char("Ignore yourself?  Go seek help!\r\n", ch);
  else if (IS_NPC(victim))
    send_to_char("No ignoring monsters.  That isn't nice.\r\n", ch);
  else if ((a = find_ignore(GET_IGNORELIST(ch), arg)) != NULL)
  {
    REMOVE_FROM_LIST(a, GET_IGNORELIST(ch), next);
    send_to_char("You no longer ignore them.\r\n", ch);
    free(a->ignore);
    free(a);
  }
  else
  {
    CREATE(a, struct ignore, 1);
    a->ignore = str_dup(arg);
    a->next = GET_IGNORELIST(ch);
    GET_IGNORELIST(ch) = a;
    new_send_to_char(ch, "%s", CONFIG_OK);
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
		new_send_to_char(ch, "{crYour skin burns!{c0\r\n");
		}
		
	    if (AFF_FLAGGED(ch, AFF_FREEZING)
		&& !(AFF_FLAGGED(ch, AFF_PROT_COLD))) {
		gain += BURN_DAMAGE*3;
		if (!IS_NPC(ch))
		new_send_to_char(ch, "{cCYou shiver with cold!{c0\r\n");
		}
	    if (AFF_FLAGGED(ch, AFF_ACIDED)
		&& !(AFF_FLAGGED(ch, AFF_STONESKIN))) {
		gain += BURN_DAMAGE*3;
		if (!IS_NPC(ch))
		new_send_to_char(ch, "{cgYou skin bubbles with concentrated acid!{c0\r\n");
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

