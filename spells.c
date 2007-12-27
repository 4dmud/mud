/* ************************************************************************
*   File: spells.c                                      Part of CircleMUD *
*  Usage: Implementation of "manual spells".  Circle 2.2 spell compat.    *
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
#include "spells.h"
#include "handler.h"
#include "db.h"
#include "constants.h"
#include "interpreter.h"
#include "dg_scripts.h"
#include "fight.h"
#include "dg_event.h"

extern int mini_mud;
ACMD(do_flee);
EVENTFUNC(message_event);

int wep_hands(OBJ_DATA *wep);
void dismount_char(struct char_data *ch);
char *balance_display(int balance);
int weapon_type_mod(int w_type, int area);
void clearMemory(struct char_data *ch);
void weight_change_object(struct obj_data *obj, int weight);
void add_follower(struct char_data *ch, struct char_data *leader);
int mag_savingthrow(struct char_data *ch, int type, int modifier);
void name_to_drinkcon(struct obj_data *obj, int type);
void name_from_drinkcon(struct obj_data *obj);
void set_race(struct char_data *ch, int race);
int compute_armor_class(struct char_data *ch);
struct char_data *FindNext(struct char_data *ch, struct char_data *v[]);
int perf_balance(int weapon_type);
int curr_balance(OBJ_DATA *wep);
int fuzzy_balance(OBJ_DATA *wep);
const char *weapon_type_name(OBJ_DATA *wep);
const char *material_name(int type);
void add_identifier(struct obj_data *obj, long id);
void free_identifier(struct obj_data *obj);
int has_identifier(struct obj_data *obj, long id);
/*
 * Special spells appear below.
 */

ASPELL(spell_create_water)
{
  int water;

  if (ch == NULL || obj == NULL)
    return;
  /* level = MAX(MIN(level, LVL_IMPL), 1);       - not used */

  if (!obj)
  {
    new_send_to_char(ch,"What are you trying to do? Flood the play. Specify a container.\r\n");
    return;
  }

  if (GET_OBJ_TYPE(obj) == ITEM_DRINKCON || GET_OBJ_TYPE(obj) == ITEM_FOUNTAIN)
  {
    if ((GET_OBJ_VAL(obj, 2) != LIQ_WATER)
        && (GET_OBJ_VAL(obj, 1) != 0))
    {
      name_from_drinkcon(obj);
      GET_OBJ_VAL(obj, 2) = LIQ_SLIME;
      name_to_drinkcon(obj, LIQ_SLIME);
    }
    else
    {
      water = MAX(GET_OBJ_VAL(obj, 0) - GET_OBJ_VAL(obj, 1), 0);
      if (water > 0)
      {
        if (GET_OBJ_VAL(obj, 1) >= 0)
          GET_OBJ_VAL(obj, 2) = LIQ_WATER;
        GET_OBJ_VAL(obj, 1) += water;
        weight_change_object(obj, water);
        act("$p is filled.", FALSE, ch, obj, 0, TO_CHAR);
        act("$p is filled.", FALSE, ch, obj, 0, TO_ROOM);
      }
    }
  }
}

ASPELL(spell_water_to_wine)
{
  if (ch == NULL || obj == NULL)
    return;
  /* level = MAX(MIN(level, LVL_IMPL), 1);       - not used */

  if (!obj)
  {
    new_send_to_char(ch,"What are you trying to do? Flood the play. Specify a container.\r\n");
    return;
  }

  if (GET_OBJ_TYPE(obj) == ITEM_DRINKCON || GET_OBJ_TYPE(obj) == ITEM_FOUNTAIN)
  {
    if ((GET_OBJ_VAL(obj, 2) == LIQ_WATER) && (GET_OBJ_VAL(obj, 1) != 0))
    {
      GET_OBJ_VAL(obj, 2) = LIQ_WINE;
      act("$p gets hazy for a moment then turns a dark red.", FALSE, ch, obj, 0, TO_CHAR);
      act("$p gets hazy for a moment then turns a dark red.", FALSE, ch, obj, 0, TO_ROOM);

    }
    else
      new_send_to_char(ch, "You can't cast it on that it is not water!\r\n");

  }
  else
    new_send_to_char(ch, "You can't cast it on that!\r\n");
}

ASPELL(spell_midas_touch)
{
  struct affected_type af;
  WAIT_STATE(ch, 1 RL_SEC);
  if (use_stamina(ch, 5) < 0)
  {
    new_send_to_char(ch, "You are too exausted!");
    return;
  }

  /* charmed mobs not allowed to do this */
  if ((!IS_NPC(ch) && (number(1, 201) < GET_SKILL(ch, SPELL_MIDAS_TOUCH))))
  {
    af.type = SPELL_MIDAS_TOUCH;
    af.expire = HOURS_TO_EXPIRE( 1 + ((GET_LEVEL(ch)) / 8));
    af.modifier = -200;
    af.location = APPLY_SPEED;
    af.bitvector = AFF_HOLD;

    affect_join(victim, &af, FALSE, FALSE, FALSE, FALSE);

    act("You touch $N and a golden sheen spreads out and engulfs $M!", FALSE, ch, NULL, victim, TO_CHAR);
    act("$n touches you and a golden sheen spreads out and engulfs you!", FALSE, ch, NULL, victim, TO_VICT);
    act("$n touch $N and a golden sheen spreads out and engulfs $M.", FALSE, ch, NULL, victim, TO_NOTVICT);
    start_fighting(ch, victim);
    return;
  }
  else
  {
    WAIT_STATE(ch, 2 RL_SEC);
    act("You miss $N with your touch of gold!", FALSE, ch, NULL, victim, TO_CHAR);
    act("$n misses you with $s touch of gold!", FALSE, ch, NULL, victim, TO_VICT);
    start_fighting(victim, ch);
  }

  return;
}



ASPELL(spell_recall)
{
  if (victim == NULL)
    return;
  if (IS_NPC(victim) && victim->master != ch)
    return;

  if (IS_IMM(victim))
  {
    new_send_to_char(ch,"That isn't such a good idea.\r\n");
    return;
  }

  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_NORECALL))
  {
    new_send_to_char(ch,"You cannot recall from this room.\r\n");
    return;
  }

  if (ZONE_FLAGGED(IN_ROOM(ch)->zone, ZONE_NORECALL))
  {
    new_send_to_char(ch,"You cannot recall from this zone.\r\n");
    return;
  }

  act("$n disappears.", TRUE, victim, 0, 0, TO_ROOM);
  dismount_char(victim);
  move_char_to(victim, CONFIG_MORTAL_START);
  act("$n appears in the middle of the room.", TRUE, victim, 0, 0, TO_ROOM);
  look_at_room(victim, 0);
  entry_memory_mtrigger(victim);
  greet_mtrigger(victim, -1);
  greet_memory_mtrigger(victim);
  enter_wtrigger(IN_ROOM(victim), victim, -1);
}


ASPELL(spell_teleport)
{
  room_rnum to_room;

  if (victim == NULL)
    return;

  if (IS_IMM(victim) || IS_NPC(victim))
  {
    new_send_to_char(ch,"That's not such a good idea.\r\n");
    return;
  }

  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_NOTELEPORT_OUT) ||
      ZONE_FLAGGED(IN_ROOM(ch)->zone, ZONE_NOTELEPORT_OUT))
  {
    new_send_to_char(ch,"A magical barrier prevents you from leaving.\r\n");
    return;
  }
  /*possible infinite loop here*/
  int cnt = 0;
  do
  {
    cnt ++;
    to_room = world_vnum[number(0, top_of_world)];
    if (cnt > top_of_world)
    {
      log("Error with teleport, can't find destination room");
      return;
    }
  }
  while (!to_room || ROOM_FLAGGED(to_room, ROOM_PRIVATE) ||
         ROOM_FLAGGED(to_room, ROOM_GODROOM) ||
         ROOM_FLAGGED(to_room, ROOM_ROLEPLAY) ||
         ROOM_FLAGGED(to_room, ROOM_HOUSE) ||
         ROOM_FLAGGED(to_room, ROOM_DEATH) ||
         ROOM_FLAGGED(to_room, ROOM_NOTELEPORT_IN) ||
         ZONE_FLAGGED(to_room->zone, ZONE_NOTELEPORT_IN));
  act("$n slowly fades out of existence and is gone.",  FALSE, victim, 0, 0, TO_ROOM);
  dismount_char(victim);
  move_char_to(victim, to_room);
  act("$n slowly fades into existence.", FALSE, victim, 0, 0, TO_ROOM);
  look_at_room(victim, 0);
  entry_memory_mtrigger(victim);
  greet_mtrigger(victim, -1);
  greet_memory_mtrigger(victim);
  enter_wtrigger(IN_ROOM(victim), victim, -1);

}

#define SUMMON_FAIL "You failed.\r\n"

ASPELL(spell_summon)
{
  if (ch == NULL || victim == NULL)
    return;

  if (GET_LEVEL(victim) > MIN(LVL_GOD - 1, level + 3))
  {
    new_send_to_char(ch, "%s", SUMMON_FAIL);
    return;
  }

  if (ZONE_FLAGGED(victim->in_room->zone, ZONE_NOSUMMON_OUT))
  {
    new_send_to_char(ch,
                     "A magical barrier prevents you from summoning %s from that zone.\r\n",
                     GET_NAME(victim));
    return;
  }

  if (ROOM_FLAGGED(victim->in_room, ROOM_NOSUMMON_OUT))
  {
    new_send_to_char(ch,
                     "A magical barrier prevents you from summoning %s from that room.\r\n",
                     GET_NAME(victim));
    return;
  }

  if (ZONE_FLAGGED(IN_ROOM(ch)->zone, ZONE_NOSUMMON_IN))
  {
    new_send_to_char(ch,
                     "A magical barrier prevents you from summoning %s to this zone.\r\n",
                     GET_NAME(victim));
    return;
  }

  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_NOSUMMON_IN))
  {
    new_send_to_char(ch,
                     "A magical barrier prevents you from summoning %s to this room.\r\n",
                     GET_NAME(victim));
    return;
  }

  if (!CONFIG_PK_ALLOWED)
  {
    if (MOB_FLAGGED(victim, MOB_AGGRESSIVE))
    {
      act("As the words escape your lips and $N travels\r\n"
          "through time and space towards you, you realize that $E is\r\n"
          "aggressive and might harm you, so you wisely send $M back.",
          FALSE, ch, 0, victim, TO_CHAR);
      return;
    }
    if (!IS_NPC(victim) && !PRF_FLAGGED(victim, PRF_SUMMONABLE) &&
        !PLR_FLAGGED(victim, PLR_KILLER))
    {
      new_send_to_char(ch, "%s just tried to summon you to: %s.\r\n"
                       "%s failed because you have summon protection on.\r\n"
                       "Type NOSUMMON to allow other players to summon you.\r\n",
                       GET_NAME(ch), IN_ROOM(ch)->name,
                       (ch->player.sex == SEX_MALE) ? "He" : "She");

      new_send_to_char(ch,
                       "You failed because %s has summon protection on.\r\n",
                       GET_NAME(victim));


      new_mudlog(BRF, LVL_GOD, TRUE, "%s failed summoning %s to %s.",
                 GET_NAME(ch), GET_NAME(victim),
                 IN_ROOM(ch)->name);
      return;
    }
  }

  if (MOB_FLAGGED(victim, MOB_NOSUMMON) ||
      (IS_NPC(victim) && mag_savingthrow(victim, SAVING_SPELL, 0)))
  {
    new_send_to_char(ch, "%s", SUMMON_FAIL);
    return;
  }

  act("$n disappears suddenly.", TRUE, victim, 0, 0, TO_ROOM);
  dismount_char(victim);
  move_char_to(victim, IN_ROOM(ch));

  act("$n arrives suddenly.", TRUE, victim, 0, 0, TO_ROOM);
  act("$n has summoned you!", FALSE, ch, 0, victim, TO_VICT);
  look_at_room(victim, 0);
  entry_memory_mtrigger(victim);
  greet_mtrigger(victim, -1);
  greet_memory_mtrigger(victim);
  enter_wtrigger(IN_ROOM(victim), victim, -1);
}

ASPELL(spell_gate)
{
  if (ch == NULL || victim == NULL)
    return;

  if (GET_LEVEL(victim) > MIN(LVL_GOD - 1, level + 3))
  {
    new_send_to_char(ch, "%s", SUMMON_FAIL);
    return;
  }

  if (ZONE_FLAGGED(IN_ROOM(ch)->zone, ZONE_NOSUMMON_OUT))
  {
    new_send_to_char(ch,"A magical barrier prevents you from gating to %s from this zone.\r\n",
                     GET_NAME(victim));
    return;
  }

  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_NOSUMMON_OUT))
  {
    new_send_to_char(ch, "A magical barrier prevents you from gating to %s from this room.\r\n",
                     GET_NAME(victim));
    return;
  }

  if (ZONE_FLAGGED(IN_ROOM(victim)->zone, ZONE_NOSUMMON_IN))
  {
    new_send_to_char(ch, "A magical barrier prevents you from gating to %s to that zone.\r\n",
                     GET_NAME(victim));
    return;
  }

  if (ROOM_FLAGGED(IN_ROOM(victim), ROOM_NOSUMMON_IN))
  {
    new_send_to_char(ch, "A magical barrier prevents you from summoning %s to this room.\r\n",
                     GET_NAME(victim));
    return;
  }
  if (IS_NPC(victim))
  {
    new_send_to_char(ch, "You cant seem to get a good focus on your target.");
    return;
  }

  if (!PRF_FLAGGED(victim, PRF_GATEABLE) &&
      !PLR_FLAGGED(victim, PLR_KILLER))
  {
    new_send_to_char(victim, "%s just tried to gate to you: %s.\r\n"
                     "%s failed because you have gate protection on.\r\n"
                     "Type NOGATE to allow other players to gate to you.\r\n",
                     GET_NAME(ch), IN_ROOM(victim)->name,
                     HSSH(ch));

    new_send_to_char(ch, "You failed the gate because %s has summon protection on.\r\n",
                     GET_NAME(victim));


    new_mudlog( BRF, LVL_GOD, TRUE, "%s failed gating to %s at %s.",
                GET_NAME(ch), GET_NAME(victim),
                IN_ROOM(victim)->name);
    return;
  }


  act("$n disappears suddenly.", TRUE, ch, 0, 0, TO_ROOM);

  dismount_char(ch);
  move_char_to(ch, IN_ROOM(victim));

  act("$n arrives suddenly.", TRUE, ch, 0, 0, TO_ROOM);
  act("$n has gated to you!", FALSE, ch, 0, victim, TO_VICT);
  look_at_room(ch, 0);
  entry_memory_mtrigger(victim);
  greet_mtrigger(victim, -1);
  greet_memory_mtrigger(victim);
  enter_wtrigger(IN_ROOM(victim), victim, -1);
}



ASPELL(spell_locate_object)
{
  struct obj_data *i;
  int j;
  char buf[MAX_STRING_LENGTH];
  DYN_DEFINE;
  *buf = 0;

  /*
   * FIXME: This is broken.  The spell parser routines took the argument
   * the player gave to the spell and located an object with that keyword.
   * Since we're passed the object and not the keyword we can only guess
   * at what the player originally meant to search for. -gg
   */
  // strcpy(name, fname(obj->name));
  /* use the temporary object that do_cast created */
  if (!strarg)
  {
    new_send_to_char(ch,"What object would you like to find?\r\n");
    return;
  }

  j = (level / 2) * (TIERNUM+1);

  DYN_CREATE;
  *dynbuf = 0;

  for (i = object_list; i && (j > 0); i = i->next)
  {
    if (!isname_full(strarg, i->name))
      continue;
    if (GET_LEVEL(ch) < LVL_IMMORT && number(0, 1))
      continue;

    if ((i->carried_by && !IS_NPC(i->carried_by) && GET_LEVEL(i->carried_by) >= LVL_IMMORT))
      continue;
    if (i->worn_by && !IS_NPC(i->worn_by) && GET_LEVEL(i->worn_by) >= LVL_IMMORT)
      continue;
    else if (IS_OBJ_STAT(i, ITEM_NO_LOCATE))
      sprintf(buf, "You can not devine the location of %s.\r\n",
              i->short_description);
    else if (i->carried_by)
      sprintf(buf, "%s is being carried by %s.\r\n",
              i->short_description, PERS(i->carried_by, ch));
    else if (i->in_room != NULL)
      sprintf(buf, "%s is in %s.\r\n", i->short_description,
              i->in_room->name);
    else if (i->in_obj)
      sprintf(buf, "%s is in %s.\r\n", i->short_description,
              i->in_obj->short_description);
    else if (i->worn_by)
      sprintf(buf, "%s is being worn by %s.\r\n",
              i->short_description, PERS(i->worn_by, ch));
    else if (i->in_locker)
      sprintf(buf, "%s is in someones locker.\r\n",i->short_description);
    else
      sprintf(buf, "%s's location is uncertain.\r\n",
              i->short_description);

    CAP(buf);
    DYN_RESIZE(buf);
    j--;
  }

  if (j == ((level / 2) * (TIERNUM+1)))
  {
    free_string(&dynbuf);
    new_send_to_char(ch, "You sense nothing.\r\n");
  }
  else
    page_string(ch->desc, dynbuf, DYN_BUFFER);
}



ASPELL(spell_charm)
{
  struct affected_type af;

  if (victim == NULL || ch == NULL)
    return;

  if (victim == ch)
    new_send_to_char(ch,"You like yourself even better!\r\n");
  else if (!IS_NPC(victim) && !PRF_FLAGGED(victim, PRF_SUMMONABLE))
    new_send_to_char(ch,"You fail because SUMMON protection is on!\r\n");
  else if (AFF_FLAGGED(victim, AFF_SANCTUARY))
    new_send_to_char(ch,"Your victim is protected by sanctuary!\r\n");
  else if (MOB_FLAGGED(victim, MOB_NOCHARM))
    new_send_to_char(ch,"Your victim resists!\r\n");
  else if (AFF_FLAGGED(ch, AFF_CHARM))
    new_send_to_char(ch,"You can't have any followers of your own!\r\n");
  else if (AFF_FLAGGED(victim, AFF_CHARM) || level < GET_LEVEL(victim))
    new_send_to_char(ch,"You fail.\r\n");
  /* player charming another player - no legal reason for this */
  else if ((!CONFIG_PK_ALLOWED)  && !IS_NPC(victim))
    new_send_to_char(ch,"You fail - shouldn't be doing it anyway.\r\n");
  else if (circle_follow(victim, ch))
    new_send_to_char(ch,"Sorry, following in circles can not be allowed.\r\n");
  else if (mag_savingthrow(victim, SAVING_PARA, 0))
    new_send_to_char(ch,"Your victim resists!\r\n");
  else
  {
    if (victim->master)
      stop_follower(victim);

    add_follower(victim, ch);
    SET_BIT_AR(AFF_FLAGS(victim), AFF_GROUP);

    af.type = SPELL_CHARM;

    if (GET_INT(victim))
      af.expire = HOURS_TO_EXPIRE(24 * 18 / GET_INT(victim));
    else
      af.expire = HOURS_TO_EXPIRE(24 * 18);

    af.modifier = 0;
    af.location = 0;
    af.bitvector = AFF_CHARM;
    affect_to_char(victim, &af);

    act("Isn't $n just such a nice fellow?", FALSE, ch, 0, victim,
        TO_VICT);
    if (IS_NPC(victim))
    {
      REMOVE_BIT_AR(MOB_FLAGS(victim), MOB_AGGRESSIVE);
      REMOVE_BIT_AR(MOB_FLAGS(victim), MOB_SPEC);
    }
  }
}

ASPELL(spell_psi_panic)
{
  if (mag_savingthrow(victim, SAVING_PARA, 0) || GET_LEVEL(victim) > number(0, 100))
  {
    act("$N leers at you in contempt!\r\n", FALSE, ch, 0, victim, TO_CHAR);
    act("You leer at $n in contempt!\r\n", FALSE, ch, 0, victim, TO_VICT);
  }
  else
  {
    act("A cold lick of fear runs down your spine!", FALSE, victim, 0, 0, TO_CHAR);
    act("A look of fear crosses $n's face!", FALSE, victim, 0, 0, TO_CHAR);
    do_flee(victim, "", 0, 0);
    GET_WAIT_STATE(ch) += 2 RL_SEC;
    GET_WAIT_STATE(victim) += 4 RL_SEC;
  }
}

ASPELL(spell_polymorph)
{
  struct affected_type af;

  if (affected_by_spell(victim, SPELL_POLYMORPH))
    new_send_to_char(ch,"You fail.\r\n");
  else if (mag_savingthrow(victim, SAVING_PARA, 0))
    new_send_to_char(ch,"Your victim resists!\r\n");
  else
  {

    af.type = SPELL_POLYMORPH;

    if (GET_INT(victim))
      af.expire = HOURS_TO_EXPIRE(24 * 18 / GET_INT(victim));
    else
      af.expire = HOURS_TO_EXPIRE(24 * 18);

    af.modifier = 0;
    af.location = 0;
    switch (number(1, 5))
    {
    case 1:
      af.bitvector = AFF_POLY_LION;
      break;
    case 2:
      af.bitvector = AFF_POLY_WOLF;
      break;
    case 3:
      af.bitvector = AFF_POLY_BEAR;
      break;
    case 4:
      af.bitvector = AFF_POLY_BOAR;
      break;
    case 5:
      af.bitvector = AFF_POLY_TOAD;
      break;
    }
    affect_to_char(victim, &af);
    if (SELF(victim, ch))
      new_send_to_char(ch, "Now see what ya did!\r\n");
    act("You feel REALLY funny.", FALSE, ch, 0, victim,  TO_VICT);
    act("$N looks REALLY funny.", FALSE, ch, 0, victim,  TO_CHAR);
    if (IS_NPC(victim))
    {
      REMOVE_BIT_AR(MOB_FLAGS(victim), MOB_AGGRESSIVE);
      REMOVE_BIT_AR(MOB_FLAGS(victim), MOB_SPEC);
    }
  }
}
int has_identifier(struct obj_data *obj, long id)
{
  struct ident_list *n;
  if (!obj->idents)
    return FALSE;

  for (n = obj->idents;n;n = n->next)
    if (n->id == id)
      return TRUE;

  return FALSE;
}
void add_identifier(struct obj_data *obj, long id)
{
  struct ident_list *te;
  if (!obj) return;
  if (has_identifier(obj, id))
    return;
  CREATE(te, struct ident_list, 1);
  te->id = id;
  te->next = obj->idents;
  obj->idents = te;
}
void free_all_identifier(struct ident_list *iden)
{
  if (!iden) return;
  if (iden->next) free_all_identifier(iden->next);
  free(iden);
}
void free_identifier(struct obj_data *obj)
{
  free_all_identifier(obj->idents);
  obj->idents = NULL;
}


void identify_object(CHAR_DATA *ch, OBJ_DATA *obj)
{
  int i;
  int found;
  float att[6], total = 0;
  int w_type;
  int form = 0;
  int get_weapon_speed(OBJ_DATA *wep);
  int get_weapon_attack(OBJ_DATA *wep);
  int get_weapon_defence(OBJ_DATA *wep);
  char buf[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];
  zone_rnum zone;

  strcpy(buf2, obj->short_description);
  strip_color(buf2, sizeof(buf2));
  /*
      'a Kruesraker' is a type of 'weapon' made from 'unknown'
  It is glowing humming magic cursed
  With a weight of about 15 and a worth of 50000 gold coins.
  If worn it will give you --
     000 to your ooooooooooooooooo, 000 to your ooooooooooooooooo
     000 to your ooooooooooooooooo, 000 to your ooooooooooooooooo
     000 to your ooooooooooooooooo, 000 to your ooooooooooooooooo
   */
  new_send_to_char(ch,
                   "----------------------------------------------------------------------\r\n");
  sprinttype(GET_OBJ_TYPE(obj), item_types, buf, sizeof(buf));
  new_send_to_char(ch, "{cc%s{cy is a type of {cc%s{cy made from {cc%s{c0\r\n",
                   buf2, buf, material_name(GET_OBJ_MATERIAL(obj)));
  zone = real_zone(GET_OBJ_VNUM(obj));
  if (zone > 0 && dimension_types[zone_table[zone].dimension])
    new_send_to_char(ch, "{cyIt is from the dimension {cy%s{c0\r\n",dimension_types[zone_table[zone].dimension]);
  sprintbitarray(GET_OBJ_WEAR(obj), wear_bits, TW_ARRAY_MAX, buf, sizeof(buf));
  if (strncmp(buf, "NOBITS", 6))
  {
    if (str_str(buf, (char *)"TAKE"))
      new_send_to_char(ch, "{cyIt can be taken and worn on {cc%s{c0\r\n", buf);
    else
      new_send_to_char(ch, "{cyIt can and worn on {cc%s{c0\r\n", buf);
  }
  sprintbitarray(GET_OBJ_EXTRA(obj), extra_bits, EF_ARRAY_MAX, buf, sizeof(buf));
  if (strncmp(buf, "NOBITS", 6))
    new_send_to_char(ch, "{cyIt is {cc%s{c0\r\n", buf);

  new_send_to_char(ch, "{cyIts weight is {cC%d{cy and its valued at {cC%d{cy coins",
                   GET_OBJ_WEIGHT(obj), GET_OBJ_COST(obj));
  if (GET_OBJ_LEVEL(obj))
    new_send_to_char(ch, ", and its Min-level is {cC%d{c0\r\n" ,GET_OBJ_LEVEL(obj));
  else
    new_send_to_char(ch, ".{c0\r\n");

  if (GET_OBJ_TIMER(obj) >= 0)
    new_send_to_char(ch, "{cyIt has {cC%d{cy hours left till it disintergrates{c0\r\n",   GET_OBJ_TIMER(obj));

  found = FALSE;
  for (i = 0; i < MAX_OBJ_AFFECT; i++)
  {
    if ((obj->affected[i].location != APPLY_NONE) &&
        (obj->affected[i].modifier != 0))
    {
      form++;
      if (!found)
      {
        new_send_to_char(ch, "{cyIf worn it will give you --{c0\r\n");
        found = TRUE;
      }
      sprinttype(obj->affected[i].location, apply_types, buf2, sizeof(buf2));
      new_send_to_char(ch, "   {cC%3d{cy to your {cc%s{c0\r\n",
                       obj->affected[i].modifier,  buf2);
    }
  }

  new_send_to_char(ch, "\r\n");

  switch (GET_OBJ_TYPE(obj))
  {
  case ITEM_SCROLL:
  case ITEM_POTION:
    new_send_to_char(ch, "{cyThis %s casts ",
                     item_types[(int) GET_OBJ_TYPE(obj)]);

    if (GET_OBJ_VAL(obj, 1) >= 1)
      new_send_to_char(ch, " {cc%s{cy", skill_name(GET_OBJ_VAL(obj, 1)));
    if (GET_OBJ_VAL(obj, 2) >= 1)
      new_send_to_char(ch, ", {cc%s{cy", skill_name(GET_OBJ_VAL(obj, 2)));
    if (GET_OBJ_VAL(obj, 3) >= 1)
      new_send_to_char(ch, ", {cc%s{cy", skill_name(GET_OBJ_VAL(obj, 3)));
    new_send_to_char(ch, "{c0\r\n");
    break;
  case ITEM_WAND:
  case ITEM_STAFF:
    new_send_to_char(ch, "{cyThis %s casts",
                     item_types[(int) GET_OBJ_TYPE(obj)]);
    new_send_to_char(ch, " {cc%s{cy\r\n",
                     skill_name(GET_OBJ_VAL(obj, 3)));
    new_send_to_char(ch,
                     "It has {cC%d{cy maximum charge%s and {cC%d{cy remaining.{c0\r\n",
                     GET_OBJ_VAL(obj, 1), GET_OBJ_VAL(obj,
                                                      1) == 1 ? "" : "s",
                     GET_OBJ_VAL(obj, 2));
    break;
  case ITEM_WEAPON:

    new_send_to_char(ch, "{cyWanted Weapon Balance: {cg(base){cc-%s-{cg(tip)\r\n"
                     "{cyActual Weapon Balance: {cg(base){cc-%s-{cg(tip)\r\n",
                     balance_display(perf_balance(GET_WEP_TYPE(obj))),
                     balance_display(GET_WEP_BALANCE(obj))
                    );
    new_send_to_char(ch, "This balance gives the weapon %d speed, %d attack and %d defence.\r\n",
                     get_weapon_speed(obj),
                     get_weapon_attack(obj),
                     get_weapon_defence(obj));

    w_type = GET_OBJ_VAL(obj, 3);
    new_send_to_char(ch, "{cyThe %s handed weapon is a {cC%d{cycm{cc %s{cy that can {cc%s{cy at {cC%d{cyD{cC%d{cy damage.{c0\r\n",
                     wep_hands(obj) == 2 ? "two" : "one",GET_WEP_LENGTH(obj), weapon_type_name(obj),
                     attack_hit_text[w_type].singular, GET_OBJ_VAL(obj, 1),  GET_OBJ_VAL(obj, 2));
    w_type += TYPE_HIT;
    att[0] = weapon_type_mod(w_type, PART_TOP_CENTER);
    att[1] = weapon_type_mod(w_type, PART_TOP_LEFT);
    att[2] = weapon_type_mod(w_type, PART_TOP_RIGHT);
    att[3] = weapon_type_mod(w_type, PART_CENTER);
    att[4] = weapon_type_mod(w_type, PART_LOWER_LEFT);
    att[5] = weapon_type_mod(w_type, PART_LOWER_RIGHT);

    total = (att[0] +att[1] +att[2] +att[3] +att[4] +att[5]);

    new_send_to_char(ch,
                     "\r\n{cyThis weapon gives a chance of landing attacks to{cc\r\n"
                     "                (Head)\r\n"
                     "                ({cR%2d%%{cc)\r\n"
                     "            (Upper)-(Upper)  \r\n"
                     "  ({cR%2d%%{cc)(Left)   (Torso)   (Right)({cR%2d%%{cc)\r\n"
                     "                ({cR%2d%%{cc)\r\n"
                     "            (Lower)-(Lower)\r\n"
                     "         (Left )       (Right)\r\n"
                     "          ({cR%2d%%{cc)         ({cR%2d%%{cc){c0 \r\n",
                     (int)((att[0]*100)/total) , (int)((att[1]*100)/total), (int)((att[2]*100)/total),
                     (int)((att[3]*100)/total), (int)((att[4]*100)/total), (int)((att[5]*100)/total)
                    );




    break;
  case ITEM_ARMOR:
    new_send_to_char(ch,"{cyAC-apply is {cC%d{c0\r\n", GET_OBJ_VAL(obj, 0));
    break;
  case ITEM_LIGHTSABRE_HILT:
    new_send_to_char(ch, "{cyPossable Saber blades: {cC%d{c0\r\n"
                     "{cyDamage dice: {cC%d{cyD{cC%d{c0\r\n"
                     "{cySaber Color: {cc%s{c0\r\n",
                     (int) GET_OBJ_VAL(obj, 0),(int) GET_OBJ_VAL(obj, 1), (int) GET_OBJ_VAL(obj, 2),
                     color_option_name((int) GET_OBJ_VAL(obj, 3)));
    break;
  }

  new_send_to_char(ch,
                   "----------------------------------------------------------------------\r\n");

}

void identify_character(CHAR_DATA *ch, CHAR_DATA *victim)
{

  new_send_to_char(ch, "Name: %s\r\n", GET_NAME(victim));
  if (!IS_NPC(victim))
  {
    new_send_to_char(ch,
                     "%s is %d years, %d months, %d days and %d hours old.\r\n",
                     GET_NAME(victim), age(victim)->year,
                     age(victim)->month, age(victim)->day,
                     age(victim)->hours);
  }
  new_send_to_char(ch, "Height %d cm, Weight %d pounds\r\n",
                   GET_HEIGHT(victim), GET_WEIGHT(victim));
  new_send_to_char(ch,
                   "Str: %d/%d, Int: %d, Wis: %d, Dex: %d, Con: %d, Cha: %d\r\n",
                   GET_STR(victim), GET_ADD(victim), GET_INT(victim),
                   GET_WIS(victim), GET_DEX(victim), GET_CON(victim),
                   GET_CHA(victim));
}

void identify_room(CHAR_DATA *ch, room_rnum room)
{}

ASPELL(spell_identify)
{

  if (obj)
  {
    identify_object(ch, obj);
    add_identifier(obj, GET_ID(ch));
  }
  else if (victim)
    identify_character(ch, victim);
}




/*
 * Cannot use this spell on an equipped object or it will mess up the
 * wielding character's hit/dam totals.
 */
ASPELL(spell_enchant_weapon)
{
  int i;

  if (ch == NULL || obj == NULL)
    return;

  /* Either already enchanted or not a weapon. */
  if (GET_OBJ_TYPE(obj) != ITEM_WEAPON || OBJ_FLAGGED(obj, ITEM_MAGIC))
    return;

  /* Make sure no other affections. */
  for (i = 0; i < MAX_OBJ_AFFECT; i++)
    if (obj->affected[i].location != APPLY_NONE)
      return;

  SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_MAGIC);
  SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);

  obj->affected[0].location = APPLY_HITROLL;
  obj->affected[0].modifier = (current_class_is_tier_num(ch) * 0.5) + (level >= 18);

  obj->affected[1].location = APPLY_DAMROLL;
  obj->affected[1].modifier = (current_class_is_tier_num(ch) * 0.5) + (level >= 20);

  if (IS_GOOD(ch))
  {
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_ANTI_EVIL);
    act("$p glows blue.", FALSE, ch, obj, 0, TO_CHAR);
  }
  else if (IS_EVIL(ch))
  {
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_ANTI_GOOD);
    act("$p glows red.", FALSE, ch, obj, 0, TO_CHAR);
  }
}


ASPELL(spell_detect_poison)
{
  if (victim)
  {
    if (victim == ch)
    {
      if (AFF_FLAGGED(victim, AFF_POISON_1))
        new_send_to_char(ch,"You can sense poison in your blood.\r\n");
      else
        new_send_to_char(ch,"You feel healthy.\r\n");
    }
    else
    {
      if (AFF_FLAGGED(victim, AFF_POISON_1))
        act("You sense that $E is poisoned.", FALSE, ch, 0, victim,
            TO_CHAR);
      else
        act("You sense that $E is healthy.", FALSE, ch, 0, victim,
            TO_CHAR);
    }
  }

  if (obj)
  {
    switch (GET_OBJ_TYPE(obj))
    {
    case ITEM_DRINKCON:
    case ITEM_FOUNTAIN:
    case ITEM_FOOD:
      if (GET_OBJ_VAL(obj, 3))
        act("You sense that $p has been contaminated.", FALSE, ch,
            obj, 0, TO_CHAR);
      else
        act("You sense that $p is safe for consumption.", FALSE,
            ch, obj, 0, TO_CHAR);
      break;
    default:
      new_send_to_char(ch,"You sense that it should not be consumed.\r\n");
    }
  }
}

/* Remove Alignment by Thelonius for EnvyMud */
ASPELL(spell_remove_alignment)
{
  if (victim && !obj)
  {
    if (number(0, 120) < GET_SKILL(ch, SPELL_REMOVE_ALIGNMENT))
    {
      GET_ALIGNMENT(victim) = 0;
      act("$N vibrates then becomes dull.", FALSE, ch, 0, victim, TO_ROOM);
      act("You vibrate then become dull.", FALSE, ch, 0, victim, TO_VICT);
    }
    else
    {
      act("$N smiles but nothing happens.", FALSE, ch, 0, victim, TO_ROOM);
      act("You feel a tingle in a happy place, but nothing happens.", FALSE, ch, 0, victim, TO_VICT);
      if (ch)
        GET_WAIT_STATE(ch) += 4 RL_SEC;
    }
  }
  else if (obj)
  {
    if (IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_MAGIC))
    {
      act("$p vibrates against your foreign magic!", FALSE, ch, obj, 0, TO_CHAR);
      act("$p vibrates against $n's foreign magic!", FALSE, ch, obj, 0, TO_ROOM);
      return;
    }
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_MAGIC);
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
    if (number(0, 120) < GET_SKILL(ch, SPELL_REMOVE_ALIGNMENT))
    {
      REMOVE_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_ANTI_GOOD);
      REMOVE_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_ANTI_EVIL);
      REMOVE_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_ANTI_NEUTRAL);
      act("$p hums briefly, then lies quiet.", FALSE, ch, obj, NULL,
          TO_CHAR);
      act("$p hums briefly, then lies quiet.", FALSE, ch, obj, NULL,
          TO_ROOM);
      return;
    }

    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_NODROP);
    obj->obj_flags.wear_flags[0] = 1;	/* Useless   */
    GET_OBJ_COST(obj) = 0;	/* Worthless */
    act("$p blazes brightly, then turns grey.", FALSE, ch, obj, NULL,
        TO_CHAR);
    act("$p blazes brightly, then turns grey.", FALSE, ch, obj, NULL,
        TO_ROOM);

    return;
  }

}

ASPELL(spell_enchant_armor)
{

  if (IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_MAGIC) || GET_OBJ_TYPE(obj) != ITEM_ARMOR)
  {
    act("$p vibrates against your foreign magic!", FALSE, ch, obj, 0, TO_CHAR);
    act("$p vibrates against $n's foreign magic!", FALSE, ch, obj, 0, TO_ROOM);
    return;
  }

  SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
  SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_MAGIC);
  if (number(0, 120) < GET_SKILL(ch, SPELL_ENCHANT_ARMOR))
  {
    GET_OBJ_VAL(obj, 0) *= 2;
    act("$p hums briefly, then lies quiet.", FALSE, ch, obj, NULL,
        TO_CHAR);
    act("$p hums briefly, then lies quiet.", FALSE, ch, obj, NULL,
        TO_ROOM);
    return;
  }

  GET_OBJ_TYPE(obj) = ITEM_WORN;
  GET_OBJ_COST(obj) = 0;	/* Worthless */
  act("$p blazes brightly, then turns grey.", FALSE, ch, obj, NULL,
      TO_CHAR);
  act("$p blazes brightly, then turns grey.", FALSE, ch, obj, NULL,
      TO_ROOM);

  return;

}


ASPELL(spell_control_weather)
{
  int i;
  struct message_event_obj *msg = NULL;
  if (!OUTSIDE(ch))
  {
    new_send_to_char(ch, "You are unable to concentrate enough to take control over nature.\r\n");
    return;
  }
  if (!strarg || !*strarg)
  {
    new_send_to_char(ch, "You must specify BETTER or WORSE.\r\n");
    return;
  }

  if (GET_INT(ch) < number(1, 19))
  {
    new_send_to_char(ch,"You fail.\r\n");
    return;
  }
  i = GET_ROOM_ZONE(IN_ROOM(ch));
  if (!str_cmp(strarg, "better"))
  {
  }
  else if (!str_cmp(strarg, "worse"))
  {
  }
  else
  {
    new_send_to_char(ch, "You must specify BETTER or WORSE.\r\n");
    return;
  }
  GET_MSG_RUN(ch) = 1;
  CREATE(msg, struct message_event_obj, 1);
  msg->ch = ch;
  msg->skill = SPELL_CONTROL_WEATHER;
  msg->type = THING_SKILL;
  msg->msg_num = 8;
  if (GET_EQ(ch, WEAR_FOCUS))
    msg->id = GET_ID(GET_EQ(ch, WEAR_FOCUS));
  else
    msg->id = -1;
  strlcpy(msg->args, strarg, 512);
  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, 0);
  return;
}

ASPELL(spell_minor_identify)
{
  char *min_id[] = {
                     "It would bring %s if you sold it!\r\n",	/* NONE */
                     "your physique!\r\n",	/* STR */
                     "your speed!\r\n",	/* DEX */
                     "your thoughts!\r\n",	/* INT */
                     "your knowledge of things!\r\n",	/* WIS */
                     "your endurance!\r\n",	/* CON */
                     "your appearance!\r\n",	/* CHA */
                     "!CLASS!\r\n",		/* CLASS */
                     "!LEVEL!\r\n",		/* LEVEL */
                     "the time!\r\n",	/* AGE */
                     "your burden of life!\r\n",	/* WEIGHT */
                     "your vertical stance!\r\n",	/* HEIGHT */
                     "your magical aura!\r\n",	/* MANA */
                     "your physical resistance!\r\n",	/* HIT */
                     "your ability to move!\r\n",	/* MOVE */
                     "your wealth!\r\n",	/* GOLD */
                     "your conscious perception!\r\n",	/* EXP */
                     "your armor!\r\n",	/* AC */
                     "the way you hit!\r\n",	/* HITROLL */
                     "the damage you give!\r\n",	/* DAMROLL */
                     "your ability to withstand paralysis!\r\n",	/* PARA */
                     "your ability to withstand rod attacks!\r\n",	/* ROD */
                     "your ability to withstand petrification!\r\n",	/* PETRI */
                     "your ability to withstand breath Attacks!\r\n",	/* BREATH */
                     "your ability to withstand spells!\r\n",	/* SPELL */
                     "!RACES!\r\n"		/* RACE */
                     "\n"
                   };

  int cost, i, x, mes_get = 0;
  bool found = FALSE, sent = FALSE;

  if (!obj)
  {
    new_send_to_char(ch,"You can only cast this on objects.\r\n");
    return;
  }

  cost = GET_OBJ_COST(obj);

  if (cost == 0)
    cost = number(1, 1000);

  if (!obj->affected[0].modifier)
  {
    new_send_to_char(ch, "%s cannot help you in any special way.\r\nBut it might \
                     bring %s if you sold it.\r\n", obj->short_description,
                     money_desc(cost));
    return;
  }
  new_send_to_char(ch,"%s can help you in the following way:\r\n",
                   obj->short_description);
  for (i = 0; i < MAX_OBJ_AFFECT; i++)
    if (obj->affected[i].modifier)
    {
      if (number(0, 20) > GET_INT(ch) && GET_LEVEL(ch) < LVL_GOD)
        continue;

      switch (obj->affected[i].location)
      {
      case APPLY_NONE:
        // case APPLY_RACE:
        //	    case APPLY_CLASS:
        //	    case APPLY_LEVEL:
        if (!found)
        {
          new_send_to_char(ch, min_id[0], money_desc(cost));
          found = TRUE;
          sent = TRUE;
        }
        break;
      default:
        mes_get = obj->affected[i].location;
        x = number(0, 1);
        new_send_to_char(ch, "%s%s",
                         (x ? "it might do something about " :
                          "It could do something about "), min_id[mes_get]);
        sent = TRUE;
        break;
      }
    }


  if (!sent)
  {
    new_send_to_char(ch,
                     "It seems to you that %s cannot help you in any special way.\r\n",
                     obj->short_description);

    return;
  }


}

void fchar_init_flags_room(struct char_data *ch, int *numtargets)
{
  struct char_data *vict;

  for (vict = IN_ROOM(ch)->people; (vict);
       vict = vict->next_in_room)
  {
    if (vict == ch || (!IS_NPC(ch) && GET_LEVEL(ch) >= LVL_GOD)
        || (!IS_NPC(ch) && !IS_NPC(vict) && !CONFIG_PK_ALLOWED))
      SET_BIT(INTERNAL(vict), INT_MARK);
    else
    {
      REMOVE_BIT(INTERNAL(vict), INT_MARK);
      (*numtargets)++;
    }
  }
}

struct char_data *fchar_next(struct char_data *ch, int *numtargets)
{
  int i;
  struct char_data *vict;

  i = number(0, *numtargets - 1);
  if (*numtargets <= 0)
    return NULL;
  for (vict = IN_ROOM(ch)->people; (vict);)
  {
    if (i == 0)
    {
      SET_BIT(INTERNAL(vict), INT_MARK);
      (*numtargets)--;
      return vict;
    }
    do
    {
      vict = vict->next_in_room;
    }
    while (vict && IS_SET(INTERNAL(vict), INT_MARK));
  }
  return NULL;
}

struct char_data *FindNext(struct char_data *ch, struct char_data *v[])
{
  int inx, j, k, numch;
  struct char_data *vict;

  numch = 0;
  for (vict = IN_ROOM(ch)->people; vict; vict = vict->next_in_room)
  {
    for (j = 0; j <= 3; j++)
      if (v[j] == vict)
      {
        numch--;
        break;
      }
    if (vict != ch && !(!CONFIG_PK_ALLOWED && !IS_NPC(vict) && !IS_NPC(ch))
        && (IS_NPC(vict) || GET_LEVEL(ch) < LVL_GOD))
      numch++;
  }
  if (!numch)
    return NULL;

  inx = number(1, numch);
  k = 0;
  for (vict = IN_ROOM(ch)->people; vict; vict = vict->next_in_room)
  {
    for (j = 0; j <= 3; j++)
      if (v[j] == vict)
      {
        k--;
        break;
      }
    if (vict != ch && !(!CONFIG_PK_ALLOWED && !IS_NPC(vict) && !IS_NPC(ch))
        && (IS_NPC(vict) || GET_LEVEL(ch) < LVL_GOD))
      k++;
    if (k == inx)
      return vict;
  }
  return NULL;
}

ASPELL(spell_chain_lightning)
{
  int dam;
  struct char_data *v[4] = { NULL, NULL, NULL, NULL };



  if (ch == NULL)
    return;

  if (victim)
  {

    if (GET_LEVEL(victim) >= LVL_GOD)
    {
      new_send_to_char(ch,"You fool...\r\n");
      return;
    }

    if (!IS_NPC(ch) && !IS_NPC(victim) && !CONFIG_PK_ALLOWED)
    {
      new_send_to_char(ch,"You rather shouldn't do this...\r\n");
      return;
    }

    v[0] = victim;
    act("Your chain lightning strikes $N with full power!", FALSE, ch,
        0, v[0], TO_CHAR);
    act("$N's chain lightning strikes you with full power!", FALSE,
        v[0], 0, ch, TO_CHAR);
    act("$n's chain lightning strikes $N with full power!", FALSE, ch,
        0, v[0], TO_NOTVICT);
    dam = dice(7, 8 + GET_LEVEL(ch) / 9) + 8;
    if (mag_savingthrow(v[0], 1, 0))
      dam = dam / 2;
    damage(ch, v[0], dam, TYPE_UNDEFINED);

    v[1] = FindNext(ch, v);
    if (!v[1])
      return;
    act("Your chain lightning strikes $N with half power!", FALSE, ch,
        0, v[1], TO_CHAR);
    act("$N's chain lightning strikes you with half power!", FALSE,
        v[1], 0, ch, TO_CHAR);
    act("$n's chain lightning strikes $N with half power!", FALSE, ch,
        0, v[1], TO_NOTVICT);
    dam = dam / 2;
    if (mag_savingthrow(v[1], 1, 0))
      dam = dam / 2;
    damage(ch, v[1], dam, TYPE_UNDEFINED);

    v[2] = FindNext(ch, v);
    if (!v[2])
      return;
    act("Your chain lightning strikes $N with quarter power!", FALSE,
        ch, 0, v[2], TO_CHAR);
    act("$N's chain lightning strikes you with quarter power!", FALSE,
        v[2], 0, ch, TO_CHAR);
    act("$n's chain lightning strikes $N with quarter power!", FALSE,
        ch, 0, v[2], TO_NOTVICT);
    dam = dam / 2;
    if (mag_savingthrow(v[2], 1, 0))
      dam = dam / 2;
    damage(ch, v[2], dam, TYPE_UNDEFINED);
  }
  else
    return;
}

ASPELL(spell_recharge)
{
  int restored_charges = 0, explode = 0;

  if (ch == NULL || obj == NULL)
    return;

  if (GET_OBJ_TYPE(obj) == ITEM_WAND)
  {
    if (GET_OBJ_VAL(obj, 2) < GET_OBJ_VAL(obj, 1))
    {
      new_send_to_char(ch,"You attempt to recharge the wand.\r\n");
      restored_charges = number(1, 5);
      GET_OBJ_VAL(obj, 2) += restored_charges;
      if (GET_OBJ_VAL(obj, 2) > GET_OBJ_VAL(obj, 1))
      {
        new_send_to_char(ch, "The wand is overcharged and explodes!\r\n");
        act("$n overcharges $p and it explodes!", TRUE, ch, obj, 0, TO_ROOM);
        explode = dice(GET_OBJ_VAL(obj, 2), 2);
        extract_obj(obj);
        damage(ch, ch, explode, TYPE_UNDEFINED);
        return;
      }
      else
      {
        new_send_to_char(ch, "You restore %d charges to the wand.\r\n",
                         restored_charges);
        return;
      }
    }
    else
    {
      new_send_to_char(ch,"That item is already at full charges!\r\n");
      return;
    }
  }
  else if (GET_OBJ_TYPE(obj) == ITEM_STAFF)
  {
    if (GET_OBJ_VAL(obj, 2) < GET_OBJ_VAL(obj, 1))
    {
      new_send_to_char(ch,"You attempt to recharge the staff.\r\n");
      restored_charges = number(1, 3);
      GET_OBJ_VAL(obj, 2) += restored_charges;
      if (GET_OBJ_VAL(obj, 2) > GET_OBJ_VAL(obj, 1))
      {
        new_send_to_char(ch, "The staff is overcharged and explodes!\r\n");
        act("$n overcharges $p and it explodes!", TRUE, ch, obj, 0, TO_ROOM);
        explode = dice(GET_OBJ_VAL(obj, 2), 3);
        extract_obj(obj);
        damage(ch, ch, explode, TYPE_UNDEFINED);
        return;
      }
      else
      {
        new_send_to_char(ch, "You restore %d charges to the staff.\r\n",
                         restored_charges);
        return;
      }
    }
    else
    {
      new_send_to_char(ch,"That item is already at full charges!\r\n");
      return;
    }
  }
}

ASPELL(spell_knock)
{
  int cnt = 0, i;

  for (i = 0; i < NUM_OF_DIRS; i++)
  {
    /** from this way **/
    if (!EXIT(ch, i))
      continue;
    if (!IS_SET(EXIT(ch, i)->exit_info, EX_ISDOOR))
      continue;
    if (!IS_SET(EXIT(ch, i)->exit_info, EX_CLOSED))
      continue;
    if (IS_SET(EXIT(ch, i)->exit_info, EX_PICKPROOF))
      continue;
    if (IS_SET(EXIT(ch, i)->exit_info, EX_HIDDEN))
      continue;
    if (EXIT(ch, i)->to_room == NULL)
      continue;
    /** and from that way **/
    if (!EXIT(ch, rev_dir[i]))
      continue;
    if (!IS_SET(EXIT(ch, rev_dir[i])->exit_info, EX_ISDOOR))
      continue;
    if (!IS_SET(EXIT(ch, rev_dir[i])->exit_info, EX_CLOSED))
      continue;
    if (IS_SET(EXIT(ch, rev_dir[i])->exit_info, EX_PICKPROOF))
      continue;
    if (IS_SET(EXIT(ch, rev_dir[i])->exit_info, EX_HIDDEN))
      continue;
    if (EXIT(ch, rev_dir[i])->to_room == NULL)
      continue;
    /** lets make it so they can only open doors that are on the inside **/
    if (!(ROOM_FLAGGED(EXIT(ch, rev_dir[i])->to_room, ROOM_INDOORS)||SECT(EXIT(ch, rev_dir[i])->to_room)==SECT_INSIDE) )
      continue;

    cnt++;
    send_to_room(IN_ROOM(ch), "The exit %s creaks open under the force of some etherial hand.", dirs[i]);
    send_to_room(EXIT(ch, i)->to_room, "The exit %s creaks open under the force of some etherial hand.", dirs[rev_dir[i]]);
    if (IS_SET(EXIT(ch, rev_dir[i])->exit_info, EX_LOCKED))
      TOGGLE_BIT(EXIT(ch, rev_dir[i])->exit_info, EX_LOCKED);
    if (IS_SET(EXIT(ch, i)->exit_info, EX_LOCKED))
      TOGGLE_BIT(EXIT(ch, i)->exit_info, EX_LOCKED);
    TOGGLE_BIT(EXIT(ch, rev_dir[i])->exit_info, EX_CLOSED);
    TOGGLE_BIT(EXIT(ch, i)->exit_info, EX_CLOSED);
  }

  if (cnt == 0)
  {
    new_send_to_char(ch, "The room grows chilly, but nothing seems to happen.\r\n");
  }

}
