/* ************************************************************************
*   File: limits.c                                      Part of CircleMUD *
*  Usage: limits & gain funcs for HMV, exp, hunger/thirst, idle time      *
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
#include "spells.h"
#include "comm.h"
#include "db.h"
#include "handler.h"
#include "interpreter.h"
#include "dg_scripts.h"
#include "clan.h"

/* local functions */
int graf(int grafage, int p0, int p1, int p2, int p3, int p4, int p5,
         int p6);
void check_autowiz(struct char_data *ch);
int dexp = 0;

/* External Functions */
int automeld(struct obj_data *obj);
void save_corpses(void);
void Crash_rentsave(struct char_data *ch, int cost);
void update_char_objects(struct char_data *ch);	/* handler.c */
void reboot_wizlists(void);
void symptoms(struct char_data *ch);
void crumble_obj(struct char_data *ch, struct obj_data *obj);
char *title_male(int chclass, int level);
char *title_female(int chclass, int level);
int has_space_suit(struct char_data *ch);
int can_breathe_underwater(struct char_data *ch);
void send_not_to_spam(char *buf, struct char_data *ch,
                      struct char_data *victim, struct obj_data *weap,
                      int spam);
/* When age < 15 return the value p0 */
/* When age in 15..29 calculate the line between p1 & p2 */
/* When age in 30..44 calculate the line between p2 & p3 */
/* When age in 45..59 calculate the line between p3 & p4 */
/* When age in 60..79 calculate the line between p4 & p5 */
/* When age >= 80 return the value p6 */
int graf(int grafage, int p0, int p1, int p2, int p3, int p4, int p5,
         int p6)
{

  if (grafage < 15)
    return (p0);		/* < 15   */
  else if (grafage <= 29)
    return (p1 + (((grafage - 15) * (p2 - p1)) / 15));	/* 15..29 */
  else if (grafage <= 44)
    return (p2 + (((grafage - 30) * (p3 - p2)) / 15));	/* 30..44 */
  else if (grafage <= 59)
    return (p3 + (((grafage - 45) * (p4 - p3)) / 15));	/* 45..59 */
  else if (grafage <= 79)
    return (p4 + (((grafage - 60) * (p5 - p4)) / 20));	/* 60..79 */
  else
    return (p6);		/* >= 80 */
}

/*
 * The hit_limit, mana_limit, and move_limit functions are gone.  They
 * added an unnecessary level of complexity to the internal structure,
 * weren't particularly useful, and led to some annoying bugs.  From the
 * players' point of view, the only difference the removal of these
 * functions will make is that a character's age will now only affect
 * the HMV gain per tick, and _not_ the HMV maximums.
 */

/* several affects like poison, freezing, burning and acid affect regen
 * i would like to give these affects their own events in time. but for now this will do. - mord
 */

/* manapoint gain pr. game hour */
int mana_gain(struct char_data *ch)
{
  int gain = 0;

  if (ch == NULL)
    return (0);

  if (IN_ROOM(ch)==NULL)
    return 0;
  if (DEAD(ch))
    return 0;

  if (IS_NPC(ch))
  {
    /* Neat and fast */
    gain = GET_LEVEL(ch);
  }
  else
  {

    /*if (!(ch->desc))
        return 0;
    if (ch->desc && ch->desc->original)
        return 0;*/



    gain = 25;

    /* Class calculations */
    if (IS_MAGE(ch) || IS_PRIEST(ch) || IS_ESPER(ch))
      gain *= 5;

    /* Skill/Spell calculations */

    gain += GET_REGEN_MANA(ch);

    /* Position calculations    */
    switch (GET_POS(ch))
    {
    case POS_SLEEPING:
      gain *= 3;
      break;
    case POS_RESTING:
      gain += (gain * 0.5);	/* Divide by 2 */
      break;
    case POS_SITTING:
      gain += (gain * 0.25);	/* Divide by 4 */
      break;
    }
    if (SITTING(ch))
      gain += (gain * 0.25);

    if ((GET_COND(ch, FULL) == 0) || (GET_COND(ch, THIRST) == 0))
      gain *= (0.015);
  }

  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_GOOD)
      && ROOM_FLAGGED(IN_ROOM(ch), ROOM_MANA))
    gain *= 4;
  else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_EVIL)
           && ROOM_FLAGGED(IN_ROOM(ch), ROOM_MANA))
    gain *= 0.018;
  else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_MANA))
    gain *= 2;

  if (AFF_FLAGGED(ch, AFF_MEDITATE))
    gain *=3;
  if (get_sub_status(ch, SUB_NIGHT_REGEN))
    gain *=2;

  return (abs(gain));
}



int hit_gain(struct char_data *ch)
{
  int gain = 0;
  //bool poison = FALSE;

  if (ch == NULL)
    return (0);

  if (IN_ROOM(ch)==NULL)
    return 0;
  if (DEAD(ch))
    return 0;

  if (IS_NPC(ch))
  {
    /* Neat and fast */
    gain = ((GET_LEVEL(ch)) * 2);
  }
  else
  {
    if (!(ch->desc))
      return 0;


    gain = 40;

    /* Class/Level calculations */
    if ((IS_MAGE(ch) || IS_PRIEST(ch) || IS_ESPER(ch)) && gain > 2)
      gain /= 2;		/* Ouch. */
    else
      gain *= 2;
    /* Skill/Spell calculations */
    gain += GET_REGEN_HIT(ch);
    /* Position calculations    */

    switch (GET_POS(ch))
    {
    case POS_SLEEPING:
      if (gain > 2)
        gain += (gain / 2);	/* Divide by 2 */
      break;
    case POS_RESTING:
      if (gain > 4)
        gain += (gain / 4);	/* Divide by 4 */
      break;
    case POS_SITTING:
      if (gain > 8)
        gain += (gain / 8);	/* Divide by 8 */
      break;
    }
    if (SITTING(ch))
      gain += (gain * 0.25);



    if ((GET_COND(ch, FULL) == 0) || (GET_COND(ch, THIRST) == 0))
      gain /= 4;
  }
  if (IN_ROOM(ch) != NULL)
  {
    if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_GOOD)
        && ROOM_FLAGGED(IN_ROOM(ch), ROOM_HP))
      gain *= 4;
    else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_EVIL)
             && ROOM_FLAGGED(IN_ROOM(ch), ROOM_HP))
    {
      if (gain > 6)
        gain /= 6;
    }
    else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_HP))
      gain *= 2;
  }

  if (AFF_FLAGGED(ch, AFF_MEDITATE))
    gain *=2;
  if (get_sub_status(ch, SUB_NIGHT_REGEN))
    gain *=2;
  return (abs(gain));
}



/* move gain pr. game hour */
int move_gain(struct char_data *ch)
{
  int gain = 0;


  if (ch == NULL)
    return 0;

  if (IN_ROOM(ch)==NULL)
    return 0;
  if (DEAD(ch))
    return 0;

  if (IS_NPC(ch))
  {
    /* Neat and fast */
    gain = GET_LEVEL(ch);
  }
  else
  {

    if (!(ch->desc))
      return 0;


    if (ch->desc && ch->desc->original)
      return 0;


    gain = 50;

    /* Class/Level calculations */

    /* Skill/Spell calculations */
    gain += GET_REGEN_MOVE(ch);

    /* Position calculations    */
    switch (GET_POS(ch))
    {
    case POS_SLEEPING:
      gain *= (2);	/* Divide by 2 */
      break;
    case POS_RESTING:
      gain += (gain / 2);	/* Divide by 4 */
      break;
    case POS_SITTING:
      gain += (gain / 4);	/* Divide by 8 */
      break;
    }
    if (SITTING(ch))
      gain += (gain * 0.25);

    if ((GET_COND(ch, FULL) == 0) || (GET_COND(ch, THIRST) == 0))
      gain /= 4;
  }

  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_GOOD)
      && ROOM_FLAGGED(IN_ROOM(ch), ROOM_MOVE))
    gain *= 4;
  else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_EVIL)
           && ROOM_FLAGGED(IN_ROOM(ch), ROOM_MOVE))
    gain /= 6;
  else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_MOVE))
    gain *= 2;

  if (AFF_FLAGGED(ch, AFF_MEDITATE))
    gain *=2;
  if (get_sub_status(ch, SUB_NIGHT_REGEN))
    gain *=2;
  return (abs(gain));
}

/* move gain pr. game hour */
int stamina_gain(struct char_data *ch)
{
  int gain = 0;


  if (ch == NULL)
    return 0;

  if (IN_ROOM(ch)==NULL)
    return 0;
  if (DEAD(ch))
    return 0;

  if (IS_NPC(ch))
  {
    /* Neat and fast */
    gain = MAX(5, GET_LEVEL(ch) * 0.25);
  }
  else
  {

    if (!(ch->desc))
      return 0;


    if (ch->desc && ch->desc->original)
      return 0;


    gain = 50;

    /* Class/Level calculations */

    /* Skill/Spell calculations */
    gain += GET_REGEN_STAMINA(ch);

    /* Position calculations    */
    switch (GET_POS(ch))
    {
    case POS_SLEEPING:
      gain *= (2);	/* Divide by 2 */
      break;
    case POS_RESTING:
      gain += (gain / 2);	/* Divide by 4 */
      break;
    case POS_SITTING:
      gain += (gain / 4);	/* Divide by 8 */
      break;
    }


    if ((GET_COND(ch, FULL) == 0) || (GET_COND(ch, THIRST) == 0))
      gain *= 0.25;
  }

  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_GOOD)
      && ROOM_FLAGGED(IN_ROOM(ch), ROOM_MOVE))
    gain *= 4;
  else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_EVIL)
           && ROOM_FLAGGED(IN_ROOM(ch), ROOM_MOVE))
    gain *= 0.15;
  else if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_MOVE))
    gain *= 2;

  if (AFF_FLAGGED(ch, AFF_MEDITATE))
    gain *=2;
  if (get_sub_status(ch, SUB_NIGHT_REGEN))
    gain *=2;
  return (abs(gain));
}

void set_title(struct char_data *ch, char *title)
{
  if (!PRF_FLAGGED(ch, PRF_KEEPTITLE))
  {
    if (title == NULL)
    {
      if (GET_SEX(ch) == SEX_FEMALE)
        title = title_female(GET_CLASS(ch), GET_LEVEL(ch));
      else
        title = title_male(GET_CLASS(ch), GET_LEVEL(ch));
    }

    if (strlen(title) > MAX_TITLE_LENGTH)
      title[MAX_TITLE_LENGTH] = '\0';

    if (GET_TITLE(ch) != NULL)
      free(GET_TITLE(ch));

    GET_TITLE(ch) = str_dup(title);
  }
}

void set_pretitle(struct char_data *ch, char *title)
{


  if (strlen(title) > 20)
    title[20] = '\0';

  if (PRETITLE(ch) != NULL)
    free(PRETITLE(ch));

  PRETITLE(ch) = str_dup(title);
}


void run_autowiz(void)
{

  extern int create_wizlist(int wizlevel,char *file1, int immlevel, char *file2);
#if defined(CIRCLE_UNIX) || defined(CIRCLE_WINDOWS)
  if (CONFIG_USE_AUTOWIZ)
  {


    mudlog("Initiating autowiz.", CMP, LVL_GOD, FALSE);
    create_wizlist(CONFIG_MIN_WIZLIST_LEV, WIZLIST_FILE, LVL_GOD, IMMLIST_FILE);
    reboot_wizlists();
  }
#endif				/* CIRCLE_UNIX || CIRCLE_WINDOWS */

}


void check_autowiz(struct char_data *ch)
{

  extern int create_wizlist(int wizlevel,char *file1, int immlevel, char *file2);
#if defined(CIRCLE_UNIX) || defined(CIRCLE_WINDOWS)
  if (CONFIG_USE_AUTOWIZ && GET_LEVEL(ch) >= LVL_IMMORT)
  {
#if 0
#if defined(CIRCLE_UNIX)
    sprintf(buf, "%d %s %d %s",
            min_wizlist_lev, WIZLIST_FILE, LVL_GOD, IMMLIST_FILE);
#elif defined(CIRCLE_WINDOWS)
    sprintf(buf, "%d %s %d %s", CONFIG_MIN_WIZLIST_LEV, WIZLIST_FILE, LVL_IMMORT, IMMLIST_FILE);
#endif				/* CIRCLE_WINDOWS */
#endif

    mudlog("Initiating autowiz.", CMP, LVL_GOD, FALSE);
    create_wizlist(CONFIG_MIN_WIZLIST_LEV, WIZLIST_FILE, LVL_GOD, IMMLIST_FILE);
    reboot_wizlists();
  }
#endif				/* CIRCLE_UNIX || CIRCLE_WINDOWS */
}


int can_level(struct char_data *ch)
{
  gold_int needed;
  if (GET_EXP(ch) >= (needed = level_exp(GET_CLASS(ch), GET_LEVEL(ch) + 1, current_class_is_tier_num(ch), REMORTS(ch)))||
      ((GET_GROUP_EXP(ch) >= (needed * 0.2)) && GET_LEVEL(ch) >= 30))
    return 1;
  else
    return 0;

}

void gain_exp(struct char_data *ch, gold_int gain)
{
  int is_altered = FALSE;
  int num_levels = 0;
  int newb = 0;
  int num_hp, num_ma, num_mv;

  if (!IS_NPC(ch))
  {
    if ((GET_LEVEL(ch) < 1 || GET_LEVEL(ch) >= LVL_HERO))
      return;
  }
  else
    return;

  SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);
  if (gain > 0)
  {

    if (REMORTS(ch) == 0)
    {
      gain *= 2;
      newb = 1;
    }
    if (dexp)
      gain *= 2;

    GET_EXP(ch) += gain;
    num_hp = GET_MAX_HIT(ch);
    num_ma = GET_MAX_MANA(ch);
    num_mv = GET_MAX_MOVE(ch);
    while (!((GET_LEVEL(ch) + 1) >= LVL_HERO)&&can_level(ch))
    {
      GET_LEVEL(ch) += 1;
      num_levels++;
      advance_level(ch);
      is_altered = TRUE;
    }

    if (PRF_FLAGGED(ch, PRF_BATTLESPAM))
      new_send_to_char(ch,"You receive %s%lld experience points.\r\n", dexp ? "a massive double " : "", dexp ? (gold_int)(0.5 * gain) : gain);

    if (is_altered && (GET_LEVEL(ch) < LVL_HERO))
    {
      new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s advanced %d level%s to level %d.",
                 GET_NAME(ch), num_levels, num_levels == 1 ? "" : "s",
                 GET_LEVEL(ch));
      GET_GROUP_EXP(ch) = (level_exp(GET_CLASS(ch), GET_LEVEL(ch), current_class_is_tier_num(ch), REMORTS(ch)) * 0.2);
      if (newb)
        new_send_to_char(ch, "You are gaining DOUBLE exp, as this is your first class.\r\n");
      if (dexp)
        new_send_to_char(ch, "{cy+-+-+DOUBLE EXP DAY+-+-+{c0\r\n");
      if (num_levels == 1)
        new_send_to_char(ch, "{cG[ You rise a level! ]{c0\r\n");
      else
        new_send_to_char(ch, "{cG[ You rise %d levels! ]{c0\r\n", num_levels);
      num_hp = GET_MAX_HIT(ch) - num_hp;
      num_ma = GET_MAX_MANA(ch) - num_ma;
      num_mv = GET_MAX_MOVE(ch) - num_mv;
      new_send_to_char(ch, "You gain: %d-HP %d-MANA %d-MOVE.\r\n", num_hp, num_ma, num_mv);
      save_char(ch);
    }
  }
  else if (gain < 0)
  {
    if (GET_LEVEL(ch) < 20 && newb)
      gain = 0;
    gain = MAX(-CONFIG_MAX_EXP_LOSS, gain);	/* Cap max exp lost per death */
    GET_EXP(ch) += gain;
    if (GET_EXP(ch) < 0)
      GET_EXP(ch) = 0;
  }
}


void gain_exp_regardless(struct char_data *ch, gold_int gain)
{
  int is_altered = FALSE;
  int num_levels = 0;

  int num_hp, num_ma, num_mv;

  if (!IS_NPC(ch))
  {
    if ((GET_LEVEL(ch) < 1 || GET_LEVEL(ch) >= LVL_HERO))
      return;
  }
  else
    return;

  SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);
  GET_EXP(ch) += gain;
  if (GET_EXP(ch) < 0)
    GET_EXP(ch) = 0;

  if (!IS_NPC(ch))
  {

    num_hp = GET_MAX_HIT(ch);
    num_ma = GET_MAX_MANA(ch);
    num_mv = GET_MAX_MOVE(ch);

    while (!((GET_LEVEL(ch) + 1) >= LVL_HERO)&&can_level(ch))
    {
      GET_LEVEL(ch) += 1;
      num_levels++;
      advance_level(ch);
      is_altered = TRUE;
    }


    new_send_to_char(ch, "You receive %lld exp.\r\n", gain);

    if (is_altered && (GET_LEVEL(ch) < LVL_GOD))
    {

      new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s advanced %d level%s to level %d.",
                 GET_NAME(ch), num_levels, num_levels == 1 ? "" : "s",
                 GET_LEVEL(ch));
      if (num_levels == 1)
        new_send_to_char(ch, "{cR[ You rise a level! ]{c0\r\n");
      else
        new_send_to_char(ch, "{cR[ You rise %d levels! ]{c0\r\n", num_levels);
      num_hp = GET_MAX_HIT(ch) - num_hp;
      num_ma = GET_MAX_MANA(ch) - num_ma;
      num_mv = GET_MAX_MOVE(ch) - num_mv;
      new_send_to_char(ch, "You gain: %d-HP %d-MANA %d-MOVE.\r\n", num_hp, num_ma, num_mv);

      save_char(ch);


    }
    if (!IS_NPC(ch) && (GET_LEVEL(ch) + 1) >= LVL_HERO)
    {
      send_to_char("You must see an Implementor to advance anymore.\r\n", ch);
      return;
    }

  }
}



void gain_group_exp(struct char_data *ch, gold_int gain)
{
  int is_altered = FALSE;
  int num_levels = 0;
  int newb = 0;

  if (!IS_NPC(ch))
  {
    if ((GET_LEVEL(ch) < 1 || GET_LEVEL(ch) >= LVL_HERO))
      return;
  }
  else
    return;

  if (gain > 0)
  {

    if (REMORTS(ch) == 0)
    {
      gain *= 2;
      newb = 1;
    }
    if (dexp)
      gain *=2;

    GET_GROUP_EXP(ch) += gain;

    if (GET_LEVEL(ch) < 30)
      return;

    while (!((GET_LEVEL(ch) + 1) >= LVL_HERO)&&can_level(ch))
    {
      GET_LEVEL(ch) += 1;
      num_levels++;
      advance_level(ch);
      is_altered = TRUE;
    }

    if (PRF_FLAGGED(ch, PRF_BATTLESPAM))
      new_send_to_char(ch,"You receive %s%lld grouping points.\r\n", dexp ? "a massive double " : "", dexp ? (gold_int)(0.5 * gain) : gain);

    if (is_altered && (GET_LEVEL(ch) < LVL_HERO))
    {
      new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s advanced %d level%s to level %d.",
                 GET_NAME(ch), num_levels, num_levels == 1 ? "" : "s", GET_LEVEL(ch));
      GET_EXP(ch) = (level_exp(GET_CLASS(ch), GET_LEVEL(ch), current_class_is_tier_num(ch), REMORTS(ch)));
      if (newb)
        new_send_to_char(ch, "You are gaining DOUBLE group points, as this is your first class.\r\n");
      if (dexp)
        new_send_to_char(ch, "{cy+-+-+DOUBLE EXP DAY+-+-+{c0\r\n");
      if (num_levels == 1)
        new_send_to_char(ch, "{cG[ You rise a level! ]{c0\r\n");
      else
        new_send_to_char(ch, "{cG[ You rise %d levels! ]{c0\r\n",
                         num_levels);

      save_char(ch);

    }
  }
}
#if 0
void gain_condition(struct char_data *ch, int condition, int value)
{
  bool intoxicated;

  if (IS_NPC(ch) || GET_COND(ch, condition) == -1)	/* No change */
    return;

  intoxicated = (GET_COND(ch, DRUNK) > 0);

  GET_COND(ch, condition) += value;

  GET_COND(ch, condition) = MAX(0, GET_COND(ch, condition));
  GET_COND(ch, condition) = MIN(24, GET_COND(ch, condition));

  if (GET_COND(ch, condition) || PLR_FLAGGED(ch, PLR_WRITING))
    return;

  switch (condition)
  {
  case FULL:
    send_to_char(ch, "You are hungry.\r\n");
    break;
  case THIRST:
    send_to_char(ch, "You are thirsty.\r\n");
    break;
  case DRUNK:
    if (intoxicated)
      send_to_char(ch, "You are now sober.\r\n");
    break;
  default:
    break;
  }

}
#endif
void gain_condition(struct char_data *ch, int condition, int value)
{
  bool intoxicated;

  if (IS_NPC(ch))	/* No change */
    return;
  if (GET_LEVEL(ch) >= LVL_IMMORT)
    return;



  intoxicated = (GET_COND(ch, DRUNK) > 0);

  GET_COND(ch, condition) += value;

  GET_COND(ch, condition) = IRANGE(0, GET_COND(ch, condition), 24);

  if (GET_COND(ch, condition) || PLR_FLAGGED(ch, PLR_WRITING))
    return;

  switch (condition)
  {
  case FULL:
    if (PRF_FLAGGED(ch, PRF_BATTLESPAM))
      send_to_char("You are hungry.\r\n", ch);
    check_regen_rates(ch);
    break;
  case THIRST:
    if (PRF_FLAGGED(ch, PRF_BATTLESPAM))
      send_to_char("You are thirsty.\r\n", ch);
    check_regen_rates(ch);
    break;
  case DRUNK:
    if (intoxicated)
      if (PRF_FLAGGED(ch, PRF_BATTLESPAM))
        send_to_char("You are now sober.\r\n", ch);
    break;
  default:
    log("default reached in gain_condition! %d", condition);
    break;
  }

}


void check_idling(struct char_data *ch)
{
  if (++(ch->char_specials.timer) > CONFIG_IDLE_VOID && !IS_NPC(ch))
  {
    if (GET_WAS_IN(ch) == NULL && IN_ROOM(ch) != NULL)
    {
      GET_WAS_IN(ch) = IN_ROOM(ch);
      if (FIGHTING(ch))
      {
        stop_fighting(FIGHTING(ch));
        stop_fighting(ch);
      }
      act("$n fades from sight as $e blends with $s surroundings.", TRUE, ch, 0, 0, TO_ROOM);
      send_to_char("You fade from sight as you blend with your surroundings.\r\n", ch);
      save_char(ch);
      Crash_crashsave(ch);
      char_from_room(ch);
      char_to_room(ch, world_vnum[1]);
    }
    else if (ch->char_specials.timer > CONFIG_IDLE_RENT_TIME)
    {
      if (IN_ROOM(ch) != NULL)
        char_from_room(ch);
      char_to_room(ch, world_vnum[3]);
      if (ch->desc)
      {
        STATE(ch->desc) = CON_DISCONNECT;
        /*
         * For the 'if (d->character)' test in close_socket().
         * -gg 3/1/98 (Happy anniversary.)
         */
        ch->desc->character = NULL;
        ch->desc = NULL;
      }

      Crash_rentsave(ch, 0);
      new_mudlog(CMP, LVL_GOD, TRUE, "%s force-rented and extracted (idle).", GET_NAME(ch));
      extract_char(ch);
    }
  }
}

/* Update PCs, NPCs, and objects */
void point_update(void)
{
  struct char_data *i, *next_char;
  struct obj_data *j, *next_thing, *jj, *next_thing2;
  int tleft = 0;

  /* characters */
  for (i = character_list; i; i = next_char)
  {
    next_char = i->next;
    if (GET_POS(i) == POS_RESTING)
    {
      if (IS_NPC(i) && GET_STAMINA(i) == GET_MAX_STAMINA(i))
      {
        act("$n stops panting and gets to $s feet.", FALSE, i, 0, 0, TO_ROOM);
        GET_POS(i) = POS_STANDING;
      }
    }
    else if (GET_POS(i) >= POS_STUNNED)
    {
      update_pos(i);
    }
    else if (GET_POS(i) == POS_INCAP)
    {
      if (damage(i, i, 1, TYPE_SUFFERING) == -1)
        continue;
    }
    else if (GET_POS(i) == POS_MORTALLYW)
    {
      if (damage(i, i, 2, TYPE_SUFFERING) == -1)
        continue;
    }
    if (!IS_NPC(i) || IS_AFFECTED(i, AFF_CHARM) || i->master || i->followers)
    {
      update_char_objects(i);
      if (!IS_NPC(i) && GET_LEVEL(i) < CONFIG_IDLE_MAX_LEVEL)
        check_idling(i);
    }
    if (!IS_NPC(i))
    {
      /* MatingMod - Extra Hunger/Thirst */
      if (GET_LEVEL(i) > 5)
      {
        if (PREG(i) > -1)
        {
          gain_condition(i, FULL, -3);
          gain_condition(i, THIRST, -2);
          gain_condition(i, DRUNK, -1);
        }
        else
        {
          gain_condition(i, FULL, -1);
          gain_condition(i, DRUNK, -1);
          gain_condition(i, THIRST, -1);
        }			/* End MatingMod modifications */

      }

      /* Romance Module -- Timeout Functions */
      /* If someone's been propositioned, and not yet accepted, then time it out. */
      if (ROMANCE(i) > 3)
      {	/* Higher than 3 = been propositioned */
        if (ROMANCE(i) == 4)
        {	/* They've been asked out.. */
          ROMANCE(i) = 0;	/* Back to "Single" */
          send_to_char("The dating offer has expired..\r\n", i);
        }
        else if (ROMANCE(i) == 5)
        {	/* They've been proposed to.. */
          ROMANCE(i) = 1;	/* Back to "Dating" */
          send_to_char("The engagement offer has expired.. \r\n", i);
        }
        else if (ROMANCE(i) == 6)
        {	// They've asked someone out..
          ROMANCE(i) = 0;
          send_to_char("Your dating offer has expired.\r\n", i);
        }
        else if (ROMANCE(i) == 7)
        {	// They've proposed.
          ROMANCE(i) = 1;
          send_to_char("Your proposal has expired.\r\n", i);
        }
        else
        {		/* We should never get here. Something is wrong. */
          send_to_char
          ("ERROR IN ROMANCE MODULE. Romance Value > 7\r\n", i);
        }
      }
      /* End Romance Module entry */
      /* MatingMod Entry -- Timeout Function */
      if (PREG(i) == -3)
      {
        if (tleft == 1)
        {
          tleft = 0;
        }
        else
        {
          tleft = 1;
          PREG(i) = NOT_PREG;
          send_to_char("Your mating offer has expired.\r\n", i);
        }
      }
      /* End Timeout Function */
      /* Begin Pregnancy Countdown Function */
      if (PREG(i) > 0)
      {
        /* Greater than 0 means there is time left */
        PREG(i) = PREG(i) - 1;
        /* Decrement by 1 */
        symptoms(i);	/* Perform Symptoms */
      }
      /* End Pregnancy Countdown */
      /* End MatingMod snipplet */

    }
  }

  /* objects */
  for (j = object_list; j; j = next_thing)
  {
    next_thing = j->next;	/* Next in object list */

    if (GET_OBJ_TIMER(j) == -1)
      continue;
    else if (GET_OBJ_TIMER(j) > 0)
    {
      GET_OBJ_TIMER(j)--;
      if (IN_ROOM(j) != NULL)
        if (ROOM_FLAGGED(IN_ROOM(j), ROOM_HOUSE))
          SET_BIT_AR(IN_ROOM(j)->room_flags, ROOM_HOUSE_CRASH);
      continue;
    }
    if (automeld(j))
      continue;
    if (timer_otrigger(j) != -1) /* j was purged if == -1*/
    {
      for (jj = j->contains; jj; jj = next_thing2)
      {
        next_thing2 = jj->next_content;	/* Next in inventory */
        obj_from_obj(jj);

        if (j->in_obj)
          obj_to_obj(jj, j->in_obj);
        else if (j->carried_by && j->carried_by->in_room != NULL)
          obj_to_room(jj, j->carried_by->in_room);
        else if (j->in_room != NULL)
        {
          obj_to_room(jj, j->in_room);
        }
        else
          core_dump();
      }
      crumble_obj(0, j);
    }

  }
}
