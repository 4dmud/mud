/* ************************************************************************
*   File: spec_procs.c                                  Part of CircleMUD *
*  Usage: implementation of special procedures for mobiles/objects/rooms  *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */
/*
 * $Log: spec_procs.c,v $
 * Revision 1.11  2006/05/01 11:29:26  w4dimenscor
 * I wrote a typo checker that automaticly corrects typos in the comm channels. I have also been fixing shadowed variables. There may be residual issues with it.
 *
 * Revision 1.10  2006/02/23 18:41:50  w4dimenscor
 * added a few needed files to cvs
 *
 * Revision 1.9  2005/11/19 06:18:39  w4dimenscor
 * Fixed many bugs, and added features
 *
 * Revision 1.8  2005/05/28 05:52:14  w4dimenscor
 * Fixed some errors in copyover, added MXP
 *
 * Revision 1.7  2005/02/05 05:26:17  w4dimenscor
 * Added tsearch command to full text search triggers
 *
 * Revision 1.6  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.5  2004/12/07 09:31:26  w4dimenscor
 * Trees modularized, fix added to message event
 *
 * Revision 1.4  2004/12/05 09:46:52  w4dimenscor
 * fixed mtransform, fixed format in clan tell, and added limit on magic items carried, and lowered weight of magic items, and increased cost
 *
 * Revision 1.3  2004/11/20 02:33:25  w4dimenscor
 * updated and cleaned up the script system
 *
 * Revision 1.2  2004/11/17 05:13:05  w4dimenscor
 * updated pets so that they dont have weight problems, updated award points
 *
 * Revision 1.1.1.1  2004/11/12 02:16:09  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.38  2004/09/05 04:29:59  molly
 * fixed comparison function to sort correctly, made prompt show comma formated numbers
 *
 * Revision 1.36  2004/08/15 01:12:29  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */
#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "dg_scripts.h"
#include "constants.h"
#include "damage.h"
#include "fight.h"



/*   external vars  */
extern struct corpse_list_data *corpse_list;
extern struct time_info_data time_info;
extern struct spell_info_type spell_info[];
extern int guild_info[][3];
extern const char *cmd_door[];

extern struct sub_skill_info_type sub_info[TOP_SUB_DEFINE];

/* extern functions */
int mag_manacost(struct char_data *ch, int spellnum);
void add_follower(struct char_data *ch, struct char_data *leader);
int get_pidx_from_name(struct char_data *ch);
int find_door(struct char_data *ch, const char *type, char *dir,
              const char *cmdname);
void do_doorcmd(struct char_data *ch, struct obj_data *obj, int door,
                int scmd);
void play_triples(struct char_data *ch, struct char_data *dealer,
                  char *guess, int bet);
void play_slots(struct char_data *ch);
void play_high_dice(struct char_data *ch, struct char_data *dealer,
                    int bet);
void play_seven(struct char_data *ch, struct char_data *dealer,
                char *guess, int bet);
void play_craps(struct char_data *ch, struct char_data *dealer, int bet);
struct obj_data *find_vehicle_by_vnum(int vnum);
struct obj_data *get_obj_in_list_type(int type,
            struct obj_data *list);
void explosion_messages(room_rnum room, int damage, struct obj_data *target);
void set_race(struct char_data *ch, int race);
int has_class(struct char_data *ch, int chclass);
int tier_level(struct char_data *ch, int chclass);
void remove_corpse_from_list(OBJ_DATA *corpse);

ACMD(do_drop);
ACMD(do_gen_door);
ACMD(do_say);

/* local functions */
void sort_spells(void);
int compare_spells(const void *x, const void *y);
const char *how_good(int percent);
void list_skills(struct char_data *ch, int skillspell);
void npc_steal(struct char_data *ch, struct char_data *victim);
void sort_spell_data(void);
void sort_skill_data(void);

SPECIAL(bank);
SPECIAL(bottle);
SPECIAL(cityguard);
SPECIAL(cleric);
SPECIAL(craps);
SPECIAL(door_down);
SPECIAL(door_down_7377);
SPECIAL(dragon_acid);
SPECIAL(dragon_fire);
SPECIAL(dragon_frost);
SPECIAL(dragon_gas);
SPECIAL(dragon_lightning);
SPECIAL(dump);
SPECIAL(fido);
SPECIAL(fire);
SPECIAL(guard_black);
SPECIAL(guard_white);
SPECIAL(guild);
SPECIAL(guild_guard);
SPECIAL(high_dice);
SPECIAL(janitor);
SPECIAL(magic_user);
SPECIAL(mayor);
SPECIAL(pet_shops);
SPECIAL(puff);
SPECIAL(scorpion);
SPECIAL(seven);
SPECIAL(slots);
SPECIAL(snake);
SPECIAL(spider);
SPECIAL(thief);
SPECIAL(triples);
void sort_sub_data(void);

/* ********************************************************************
*  Special procedures for mobiles                                     *
******************************************************************** */



//Attempt at sorting spells/skills by what type they are:

/*
#define MAG_DAMAGE	(1 <<  0)
#define MAG_AFFECTS	(1 <<  1)
#define MAG_UNAFFECTS	(1 <<  2)
#define MAG_POINTS	(1 <<  3)
#define MAG_ALTER_OBJS	(1 <<  4)
#define MAG_GROUPS	(1 <<  5)
#define MAG_MASSES	(1 <<  6)
#define MAG_AREAS	(1 <<  7)
#define MAG_SUMMONS	(1 <<  8)
#define MAG_CREATIONS	(1 <<  9)
#define MAG_MANUAL	(1 << 10)
#define MAG_ROOM	(1 << 11)
 
IS_SET(spell_info[spellnum].routines, MAG_DAMAGE)
 
int max_in_group(int group);
void sort_spells_to_list();
int number_in_group, lastspell;
int spell_group[MAX_SKILLS + 1];
 
int max_in_group(int group)
{
number_in_group = 0;
int a;
  for (a = 1; a <= MAX_SKILLS; a++)
  if (IS_SET(spell_info[a].routines, (1 << group)))
  number_in_group++;
  
  return (number_in_group);
}
 
void sort_spells_to_list()
{}
 
 
*/
const char *unlearnedsub = "Undiscovered SubSkill";



int spell_sorted_info[TOP_SPELL_DEFINE + 2];
int spell_sort_info[MAX_SKILLS + TOP_SUB_DEFINE + 1];
int spell_sort_data[MAX_SPELLS + 1];
int skill_sort_data[MAX_SKILLS - MAX_SPELLS + 2];
int sub_sort_data[TOP_SUB_DEFINE +1];

int compare_spells(const void *x, const void *y)
{
  int a = *(const int *) x, b = *(const int *) y;

  return strcmp(skill_name(a), skill_name(b));
}
int compare_sub(const void *x, const void *y)
{
  int a = *(const int *) x, b = *(const int *) y;

  return strcmp(sub_name(a), sub_name(b));
}

void sort_all_spell_data(void)
{
  int a;

  /* initialize array, avoiding reserved. */
  for (a = 0; a < TOP_SPELL_DEFINE; a++)
    spell_sorted_info[a] = a;

  qsort(spell_sorted_info, TOP_SPELL_DEFINE, sizeof(int), compare_spells);
}

void sort_sub_data(void)
{
  int a;

  /* initialize array, avoiding reserved. */
  for (a = 1; a < TOP_SUB_DEFINE; a++)
    sub_sort_data[a] = a;

  qsort(&sub_sort_data[0], TOP_SUB_DEFINE, sizeof(int), compare_sub);
}
void sort_spell_data(void)
{
  int a;

  /* initialize array, avoiding reserved. */
  for (a = 1; a <= MAX_SPELLS; a++)
    spell_sort_data[a] = a;

  qsort(&spell_sort_data[0], MAX_SPELLS, sizeof(int), compare_spells);
}
void sort_skill_data(void)
{
  int a, b = 0;

  /* initialize array, avoiding reserved. */
  for (a = MAX_SPELLS + 1; a <= MAX_SKILLS; a++)
    skill_sort_data[++b] = a;

  qsort(&skill_sort_data[0], b, sizeof(int), compare_spells);
}


void sort_spells(void)
{
  int a, b = 1;

  sort_skill_data();
  sort_spell_data();
  sort_sub_data();
  sort_all_spell_data();

  /* initialize array, avoiding reserved. */
  for (a = 1; a <= MAX_SPELLS; a++)
    spell_sort_info[a] = spell_sort_data[a];

  for (; a <= MAX_SKILLS; a++)
    spell_sort_info[a] = skill_sort_data[b++];

  for (b = 0; a <= (TOP_SUB_DEFINE + MAX_SKILLS); a++)
    spell_sort_info[a] = sub_sort_data[b++];


}

const char *how_good(int percent)
{
  if (percent < 0)
    return " (error)";
  if (percent == 0)
    return "\x1B[0m----------\x1B[0m";
  if (percent <= 10)
    return "\x1B[0m#---------\x1B[0m";
  if (percent <= 20)
    return "\x1B[32m##\x1B[0m--------\x1B[0m";
  if (percent <= 30)
    return "\x1B[32m###\x1B[0m-------\x1B[0m";
  if (percent <= 40)
    return "\x1B[33m####\x1B[0m------\x1B[0m";
  if (percent <= 50)
    return "\x1B[33m#####\x1B[0m-----\x1B[0m";
  if (percent <= 60)
    return "\x1B[34m######\x1B[0m----\x1B[0m";
  if (percent <= 70)
    return "\x1B[34m#######\x1B[0m---\x1B[0m";
  if (percent <= 80)
    return "\x1B[35m########\x1B[0m--\x1B[0m";
  if (percent <= 90)
    return "\x1B[35m#########\x1B[0m-\x1B[0m";

  return "\x1B[36m##########\x1B[0m";
}

char * how_good_perc(struct char_data *ch, int perc)
{
  static char  perc_buf[80];
  if (GET_INT(ch) < 22)
    return (char *)how_good(perc);
  sprintf(perc_buf, "%-3d%%", perc);
  return perc_buf;

}


const char *prac_types[] =
  {
    "spell",
    "skill",
    "subskill"
  };

#define LEARNED_LEVEL	0	/* % known which is considered "learned" */
#define MAX_PER_PRAC	1	/* max percent gain in skill per practice */
#define MIN_PER_PRAC	2	/* min percent gain in skill per practice */
#define PRAC_TYPE	3	/* should it say 'spell' or 'skill'?     */

/* actual prac_params are in class.c */
extern int prac_params[4][NUM_CLASSES];

#define LEARNED(ch) (prac_params[LEARNED_LEVEL][(int)GET_CLASS(ch)])
#define MINGAIN(ch) (prac_params[MIN_PER_PRAC][(int)GET_CLASS(ch)])
#define MAXGAIN(ch) (prac_params[MAX_PER_PRAC][(int)GET_CLASS(ch)])
#define SPLSKL(ch) (prac_types[prac_params[PRAC_TYPE][(int)GET_CLASS(ch)]])

#if defined(WIN32)

void list_skills(struct char_data *ch, int skillspell)
{
  int i, sortpos;
  DYN_DEFINE;
  *buf = 0;

  if (!GET_PRACTICES(ch))
    strcpy(buf, "You have no practice sessions remaining.\r\n");
  else
    sprintf(buf, "You have %d practice session%s remaining.\r\n",
            GET_PRACTICES(ch), (GET_PRACTICES(ch) == 1 ? "" : "s"));

  sprintf(buf + strlen(buf), "You have the following abilties:\r\n");


  DYN_CREATE;
  *dynbuf = 0;
  DYN_RESIZE(buf);

  for (sortpos = 1; sortpos <= MAX_SKILLS; sortpos++)
  {
    i = spell_sort_info[sortpos];

    if (GET_LEVEL(ch) >= spell_info[i].min_level[(int) GET_CLASS(ch)])
    {
      sprintf(buf, "(\x1B[31m%-30s  %s", spell_info[i].name,
              how_good_perc(ch, GET_SKILL(ch, i))
              /* The above, ^ should always be safe to do. */
              if (!(sortpos % 2))
              strcat(buf, "\r\n");
              DYN_RESIZE(buf)
            }
        }

        page_string(ch->desc, dynbuf, DYN_BUFFER);
}

#else
void list_skills(struct char_data *ch, int skillspell)
{
  extern struct spell_info_type spell_info[];
  int i, sortpos, h = 0, imm, ending, sub;
  int count= 0;
  char buf[MAX_INPUT_LENGTH];
  DYN_DEFINE;
  *buf = 0;



  if (skillspell == 0)
  {
    sortpos = MAX_SPELLS + 1;
    ending = MAX_SKILLS;
  }
  else if (skillspell == 1)
  {
    sortpos = 1;
    ending = MAX_SPELLS;
  }
  else
  {
    sortpos = MAX_SKILLS + 1;
    ending = MAX_SKILLS + TOP_SUB_DEFINE;
  }
  DYN_CREATE;
  *dynbuf = 0;

  imm = -1;

  if (!GET_PRACTICES(ch))
    strcpy(buf, "You have no practice sessions remaining.\r\n");
  else
    sprintf(buf, "You have %d practice session%s remaining.\r\n",
            GET_PRACTICES(ch), (GET_PRACTICES(ch) == 1 ? "" : "s"));
  if (skillspell < 2)
    sprintf(buf + strlen(buf), "You have the following %s:\r\n",skillspell == 1 ? "spells" : "skills");
  else
    sprintf(buf + strlen(buf), "You have the following extra abilities:\r\n");
  DYN_RESIZE(buf);
  /*need to add dyn buff here*/
  for (; sortpos <= ending; sortpos++)
  {
    i = spell_sort_info[sortpos];

    if (skillspell > 1)
    {
      if (i==0)
        continue;
      if (i==TOP_SUB_DEFINE)
        continue;
      sub = GET_SUB(ch, i);

      /*if (sub < 1)
      continue;*/

      if (GET_LEVEL(ch) > LVL_IMMORT)
      {
        if (sub < 100)
          improve_sub(ch, (enum subskill_list)i, 100 - sub);
        sub = 100;
      }
      if (sub == 0)
        continue;
      count++;
      if (GET_LEVEL(ch) > LVL_IMMORT)
      {
        sprintf(buf, "%-3d)", i);
        DYN_RESIZE(buf);
      }
      sprintf(buf, " \x1B[1;31m%-28s  \x1B[0m[%s] ", sub_name(i), ((sub > 0) ? how_good_perc(ch, sub) : unlearnedsub));
      DYN_RESIZE(buf);




      if (!IS_SET(sub_info[i].flags, SUB_TYPE_PROF) && sub_info[i].perent!=TYPE_UNDEFINED)
      {
        sprintf(buf, " Parent Skill: %s", skill_name(sub_info[i].perent));
        DYN_RESIZE(buf);
      }
      if (IS_SET(sub_info[i].flags, SUB_TYPE_PROF) && sub_info[i].perent!=TYPE_UNDEFINED)
      {
        sprintf(buf, " Profession: %s", profession_names[sub_info[i].perent]);
        DYN_RESIZE(buf);
      }
      sprintf(buf, "\r\n");
      DYN_RESIZE(buf);
    }
    else
    {


      if (knows_spell(ch, i))
      {

        if ((FIRST_PRE(i) != TYPE_UNDEFINED)
            && (SECOND_PRE(i) != TYPE_UNDEFINED))
          h = 2;
        else if (((FIRST_PRE( i) != TYPE_UNDEFINED)
                  && (SECOND_PRE( i) == TYPE_UNDEFINED)))
          h = 1;
        else
          h = 0;
        if (GET_LEVEL(ch) == LVL_IMPL)
        {
          sprintf(buf, "%-3d)", i);
          DYN_RESIZE(buf);
        }


        sprintf(buf, " %s%-20s  \x1B[0m[%s] ",
                (!(i < MAX_SPELLS) ? "\x1B[32m" : "\x1B[33m"),
                skill_name(i), how_good_perc(ch, total_chance(ch, i)));
        DYN_RESIZE(buf);

        sprintf(buf, "[L:%2d T:%d]",spell_info[i].min_level, spell_info[i].tier);
        DYN_RESIZE(buf);



        if (skillspell == 1)
        {
          sprintf(buf, "[Mana:%-4d]\x1B[0m",
                  mag_manacost(ch, i));
          DYN_RESIZE(buf);
        }
        if (skillspell == 1 && elemental_type(i) != ELEM_NONE)
        {
          snprintf(buf, sizeof(buf), "[{cp%-11s{c0]", elemental_types[elemental_type(i)]);
          DYN_RESIZE(buf);
        }

        if (spell_info[i].wait)
        {
          sprintf(buf, "[Delay of %3d sec]",spell_info[i].wait);
          DYN_RESIZE(buf);
        }

        if (GET_SPELL_WAIT(ch, i))
        {
          sprintf(buf, "[%d seconds till recast]",
                  GET_SPELL_WAIT(ch, i));
          DYN_RESIZE(buf);
        }

        if (h == 1)
        {
          sprintf(buf, "\x1B[1;34m  Requires: %s \x1B[0;0m",
                  skill_name(FIRST_PRE( i)));
          DYN_RESIZE(buf);
        }
        else if (h == 2)
        {
          sprintf(buf,
                  "\x1B[1;34m  Requires: %s and %s \x1B[0;0m",
                  skill_name(FIRST_PRE(i)),
                  skill_name(SECOND_PRE(i)));
          DYN_RESIZE(buf);
        }

        sprintf(buf, "\r\n");
        DYN_RESIZE(buf);
        count++;

        /*if (!(GET_LEVEL(ch) >= LVL_IMMORT))
        break;*/
      }


    }
  }
  if (count == 0)
  {
    sprintf(buf, "You don't know any!\r\n");
    DYN_RESIZE(buf);
  }

  page_string(ch->desc, dynbuf, DYN_BUFFER);
}
#endif

SPECIAL(guild)
{
  int skill_num, percent, learned;
  struct char_data *mob = (struct char_data *) me;
  int spell_num(const char *name);

  if (IS_NPC(ch) || !CMD_IS("practice"))
    return (0);

  skip_spaces(&argument);

  if (!*argument)
  {
    send_to_char
    ("Either type: \"practice skills\" or type \"practice spells\"\r\n",
     ch);
    send_to_char("Otherwise try: practice <skill/spell name>\r\n", ch);
    return (1);
  }

  if (is_abbrev("skills", argument))
  {
    list_skills(ch, 0);
    return (1);
  }
  if (is_abbrev("spells", argument))
  {
    list_skills(ch, 1);
    return (1);
  }
  if (is_abbrev( "subskills", argument))
  {
    list_skills(ch, 2);
    return (1);
  }

  if (GET_PRACTICES(ch) <= 0)
  {
    send_to_char("You do not seem to be able to practice now.\r\n",
                 ch);
    return (1);
  }
  learned = IRANGE(30, (20*(TIERNUM)), 80);
  skill_num = find_skill_num(argument);

  if ((skill_num == SKILL_BREW) && (GET_MOB_VNUM(mob) != 7043))
  {
    act("$N says 'I'm not skilled in brew.  You must find someone else to teach you that.", FALSE, ch, 0, mob, TO_CHAR);
    return (1);
  }
  else if ((skill_num == SKILL_SCRIBE) && (GET_MOB_VNUM(mob) != 26060))
  {
    act("$N says 'I'm not skilled in scribe.  You must find someone else to teach you that.", FALSE, ch, 0, mob, TO_CHAR);
    return (1);
  }
  else if ((skill_num == SKILL_TINKER) && (GET_MOB_VNUM(mob) != 21819))
  {
    act("$N says 'I'm not skilled in tinker.  You must find someone else to teach you that.", FALSE, ch, 0, mob, TO_CHAR);
    return (1);
  }

  if (skill_num < 1 ||
      !knows_spell(ch, skill_num))
  {
    new_send_to_char(ch,  "You do not know of that %s.\r\n", SPLSKL(ch));
    return (1);
  }
  if (GET_SKILL(ch, skill_num) >= learned)
  {
    send_to_char("You can't memorize that skill any further for now, come back when you remort.\r\n", ch);
    return (1);
  }


  percent = GET_SKILL(ch, skill_num);
  percent +=
    MIN(MAXGAIN(ch), MAX(MINGAIN(ch), int_app[GET_INT(ch)].learn));

  SET_SKILL(ch, skill_num, MIN(learned, percent));
  new_send_to_char(ch, "You practice for a while...(%d)\r\n", GET_SKILL(ch,skill_num));
  GET_PRACTICES(ch)--;

  if (GET_SKILL(ch, skill_num) >= learned)
    send_to_char("You are now learned in that area.\r\n", ch);

  return (1);
}



SPECIAL(dump)
{
  struct obj_data *k;
  gold_int value = 0;

  for (k = IN_ROOM(ch)->contents; k;
       k = IN_ROOM(ch)->contents)
  {
    act("$p vanishes in a puff of smoke!", FALSE, 0, k, 0, TO_ROOM);
    obj_from_room(k);
    extract_obj(k);
  }

  if (!CMD_IS("drop"))
    return (0);

  do_drop(ch, argument, cmd, 0);

  for (k = IN_ROOM(ch)->contents; k;
       k = IN_ROOM(ch)->contents)
  {
    act("$p vanishes in a puff of smoke!", FALSE, 0, k, 0, TO_ROOM);
    value += MAX(1, MIN(50, GET_OBJ_COST(k) / 10));
    obj_from_room(k);
    extract_obj(k);
  }

  if (value)
  {
    send_to_char("You are awarded for outstanding performance.\r\n", ch);
    act("$n has been awarded for being a good citizen.", TRUE, ch, 0, 0, TO_ROOM);

    if (GET_LEVEL(ch) < 3)
      gain_exp(ch, value);
    else
      char_gold(ch, value, GOLD_HAND);
  }
  return (1);
}


SPECIAL(mayor)
{
  const char open_path[] =
    "W3a3003b33000c111d0d111Oe333333Oe22c222112212111a1S.";
  const char close_path[] =
    "W3a3003b33000c111d0d111CE333333CE22c222112212111a1S.";

  static const char *path = NULL;
  static int mindex;
  static bool move = FALSE;

  if (!move)
  {
    if (time_info.hours == 6)
    {
      move = TRUE;
      path = open_path;
      mindex = 0;
    }
    else if (time_info.hours == 20)
    {
      move = TRUE;
      path = close_path;
      mindex = 0;
    }
  }
  if (cmd || !move || (GET_POS(ch) < POS_SLEEPING) ||
      (GET_POS(ch) == POS_FIGHTING))
    return (FALSE);

  switch (path[mindex])
  {
  case '0':
  case '1':
  case '2':
  case '3':
    perform_move(ch, path[mindex] - '0', 1);
    break;

  case 'W':
    GET_POS(ch) = POS_STANDING;
    act("$n awakens and groans loudly.", FALSE, ch, 0, 0, TO_ROOM);
    break;

  case 'S':
    GET_POS(ch) = POS_SLEEPING;
    act("$n lies down and instantly falls asleep.", FALSE, ch, 0, 0,
        TO_ROOM);
    break;

  case 'a':
    act("$n says 'Hello Honey!'", FALSE, ch, 0, 0, TO_ROOM);
    act("$n smirks.", FALSE, ch, 0, 0, TO_ROOM);
    break;

  case 'b':
    act("$n says 'What a view!  I must get something done about that dump!'", FALSE, ch, 0, 0, TO_ROOM);
    break;

  case 'c':
    act("$n says 'Vandals!  Youngsters nowadays have no respect for anything!'", FALSE, ch, 0, 0, TO_ROOM);
    break;

  case 'd':
    act("$n says 'Good day, citizens!'", FALSE, ch, 0, 0, TO_ROOM);
    break;

  case 'e':
    act("$n says 'I hereby declare the bazaar open!'", FALSE, ch, 0, 0,
        TO_ROOM);
    break;

  case 'E':
    act("$n says 'I hereby declare Midgaard closed!'", FALSE, ch, 0, 0,
        TO_ROOM);
    break;

  case 'O':
    do_gen_door(ch, "gate", 0, SCMD_UNLOCK);
    do_gen_door(ch, "gate", 0, SCMD_OPEN);
    break;

  case 'C':
    do_gen_door(ch, "gate", 0, SCMD_CLOSE);
    do_gen_door(ch, "gate", 0, SCMD_LOCK);
    break;

  case '.':
    move = FALSE;
    break;

  }

  mindex++;
  return (FALSE);
}


/* ********************************************************************
*  General special procedures for mobiles                             *
******************************************************************** */


void npc_steal(struct char_data *ch, struct char_data *victim)
{
  gold_int gold;

  if (IS_NPC(victim))
    return;
  if (GET_LEVEL(victim) >= LVL_IMMORT)
    return;

  if (AWAKE(victim) && (number(0, GET_LEVEL(ch)) == 0))
  {
    act("You discover that $n has $s hands in your wallet.", FALSE, ch,
        0, victim, TO_VICT);
    act("$n tries to steal gold from $N.", TRUE, ch, 0, victim,
        TO_NOTVICT);
  }
  else
  {
    /* Steal some gold coins */
    gold =  ((char_gold(victim, 0, GOLD_HAND) * number(1, 10)) / 100);
    if (gold > 0)
    {
      char_gold(ch, gold, GOLD_HAND);
      char_gold(victim, -gold, GOLD_HAND);
    }
  }
}

SPECIAL(spider)
{
  if (cmd)
    return (FALSE);

  if (GET_POS(ch) != POS_FIGHTING)
    return (FALSE);

  if (FIGHTING(ch) && (FIGHTING(ch)->in_room == IN_ROOM(ch)) &&
      (number(0, 60 - GET_LEVEL(ch)) == 0))
  {
    act("$n bites $N!", 1, ch, 0, FIGHTING(ch), TO_NOTVICT);
    act("$n bites you!", 1, ch, 0, FIGHTING(ch), TO_VICT);
    call_magic(ch, FIGHTING(ch), 0, 0, SPELL_POISON, GET_LEVEL(ch),
               CAST_SPELL);
    return (TRUE);
  }
  return (FALSE);
}

SPECIAL(scorpion)
{
  if (cmd)
    return (FALSE);

  if (GET_POS(ch) != POS_FIGHTING)
    return (FALSE);

  if (FIGHTING(ch) && (FIGHTING(ch)->in_room == IN_ROOM(ch)) &&
      (number(0, 62 - GET_LEVEL(ch)) == 0))
  {
    act("$n stings $N!", 1, ch, 0, FIGHTING(ch), TO_NOTVICT);
    act("$n stings you!", 1, ch, 0, FIGHTING(ch), TO_VICT);
    call_magic(ch, FIGHTING(ch), 0, 0, SPELL_POISON_2, GET_LEVEL(ch),
               CAST_SPELL);
    return (TRUE);
  }
  return (FALSE);
}

SPECIAL(snake)
{
  if (cmd)
    return (FALSE);

  if (GET_POS(ch) != POS_FIGHTING)
    return (FALSE);

  if (FIGHTING(ch) && (FIGHTING(ch)->in_room == IN_ROOM(ch)) &&
      (number(0, 62 - GET_LEVEL(ch)) == 0))
  {
    act("$n bites $N!", 1, ch, 0, FIGHTING(ch), TO_NOTVICT);
    act("$n bites you!", 1, ch, 0, FIGHTING(ch), TO_VICT);
    call_magic(ch, FIGHTING(ch), 0, 0, SPELL_POISON_3, GET_LEVEL(ch),
               CAST_SPELL);
    return (TRUE);
  }
  return (FALSE);
}

SPECIAL(thief)
{
  struct char_data *cons;

  if (cmd)
    return (FALSE);

  if (GET_POS(ch) != POS_STANDING)
    return (FALSE);

  for (cons = IN_ROOM(ch)->people; cons; cons = cons->next_in_room)
    if (!IS_NPC(cons) && (GET_LEVEL(cons) < LVL_IMMORT)
        && (!number(0, 4)))
    {
      npc_steal(ch, cons);
      return (TRUE);
    }
  return (FALSE);
}


SPECIAL(magic_user)
{
  struct char_data *vict;

  if (cmd || GET_POS(ch) != POS_FIGHTING)
    return (FALSE);

  /* pseudo-randomly choose someone in the room who is fighting me */
  for (vict = IN_ROOM(ch)->people; vict; vict = vict->next_in_room)
    if (FIGHTING(vict) == ch && !number(0, 4))
      break;

  /* if I didn't pick any of those, then just slam the guy I'm fighting */
  if (vict == NULL && IN_ROOM(FIGHTING(ch)) == IN_ROOM(ch))
    vict = FIGHTING(ch);

  /* Hm...didn't pick anyone...I'll wait a round. */
  if (vict == NULL)
    return (TRUE);

  if ((GET_LEVEL(ch) > 13) && (number(0, 10) == 0))
    cast_spell(ch, vict, NULL, 0, SPELL_SLEEP);

  if ((GET_LEVEL(ch) > 7) && (number(0, 8) == 0))
    cast_spell(ch, vict, NULL, 0, SPELL_BLINDNESS);

  if ((GET_LEVEL(ch) > 12) && (number(0, 12) == 0))
  {
    if (IS_EVIL(ch))
      cast_spell(ch, vict, NULL, 0, SPELL_ENERGY_DRAIN);
    else if (IS_GOOD(ch))
      cast_spell(ch, vict, NULL, 0, SPELL_DISPEL_EVIL);
  }
  if (number(0, 4))
    return (TRUE);

  switch (GET_LEVEL(ch))
  {
  case 4:
  case 5:
    cast_spell(ch, vict, NULL, 0, SPELL_MAGIC_MISSILE);
    break;
  case 6:
  case 7:
    cast_spell(ch, vict, NULL, 0, SPELL_CHILL_TOUCH);
    break;
  case 8:
  case 9:
    cast_spell(ch, vict, NULL, 0, SPELL_BURNING_HANDS);
    break;
  case 10:
  case 11:
    cast_spell(ch, vict, NULL, 0, SPELL_SHOCKING_GRASP);
    break;
  case 12:
  case 13:
    cast_spell(ch, vict, NULL, 0, SPELL_LIGHTNING_BOLT);
    break;
  case 14:
  case 15:
  case 16:
  case 17:
    cast_spell(ch, vict, NULL, 0, SPELL_COLOR_SPRAY);
    break;
  default:
    cast_spell(ch, vict, NULL, 0, SPELL_FIREBALL);
    break;
  }
  return (TRUE);

}


/* ********************************************************************
*  Special procedures for mobiles                                      *
******************************************************************** */

SPECIAL(guild_guard)
{
  int i;
  struct char_data *guard = (struct char_data *) me;
  const char *buf = "The guard humiliates you, and blocks your way.\r\n";
  const char *buf2 = "The guard humiliates $n, and blocks $s way.";

  if (!IS_MOVE(cmd) || AFF_FLAGGED(guard, AFF_BLIND))
    return (FALSE);

  if (GET_LEVEL(ch) >= LVL_IMMORT)
    return (FALSE);

  for (i = 0; guild_info[i][0] != -1; i++)
  {
    if ((IS_NPC(ch) || GET_CLASS(ch) != guild_info[i][0]) &&
        GET_ROOM_VNUM(IN_ROOM(ch)) == guild_info[i][1] &&
        cmd == guild_info[i][2])
    {
      send_to_char(buf, ch);
      act(buf2, FALSE, ch, 0, 0, TO_ROOM);
      return (TRUE);
    }
  }

  return (FALSE);
}



SPECIAL(puff)
{
  if (cmd)
    return (0);

  return 0;

  switch (number(0, 60))
  {
  case 0:
    do_say(ch, "My god!  Its full of stars!", 0, 0);
    return (1);
  case 1:
    do_say(ch, "Howd all those fish get up here?", 0, 0);
    return (1);
  case 2:
    do_say(ch, "Im a very female dragon.", 0, 0);
    return (1);
  case 3:
    do_say(ch, "Ive got a peaceful, easy feeling.", 0, 0);
    return (1);
  default:
    return (0);
  }
}



SPECIAL(fido)
{

  struct obj_data *i, *temp, *next_obj;

  if (cmd || !AWAKE(ch))
    return (FALSE);

  for (i = IN_ROOM(ch)->contents; i; i = i->next_content)
  {
    if (IS_CORPSE(i) &&  !(IS_OBJ_STAT(i, ITEM_PC_CORPSE)))
    {
      act("$n savagely devours a corpse.", FALSE, ch, 0, 0, TO_ROOM);
      for (temp = i->contains; temp; temp = next_obj)
      {
        next_obj = temp->next_content;
        obj_from_obj(temp);
        obj_to_room(temp, IN_ROOM(ch));
      }
      extract_obj(i);
      return (TRUE);
    }
  }
  return (FALSE);
}



SPECIAL(janitor)
{
  struct obj_data *i;

  if (cmd || !AWAKE(ch))
    return (FALSE);

  for (i = IN_ROOM(ch)->contents; i; i = i->next_content)
  {
    if (!CAN_WEAR(i, ITEM_WEAR_TAKE))
      continue;
    if (GET_OBJ_TYPE(i) != ITEM_DRINKCON && GET_OBJ_COST(i) >= 15 )
      continue;
    if ( (IS_OBJ_STAT(i, ITEM_PC_CORPSE)))
      continue;
    act("$n picks up some trash.", FALSE, ch, 0, 0, TO_ROOM);
    obj_from_room(i);
    obj_to_char(i, ch);
    return (TRUE);
  }

  return (FALSE);
}


SPECIAL(cityguard)
{
  struct char_data *tch, *evil;
  int max_evil;

  if (cmd || !AWAKE(ch) || FIGHTING(ch))
    return (FALSE);

  max_evil = 1000;
  evil = 0;

  for (tch = IN_ROOM(ch)->people; tch; tch = tch->next_in_room)
  {
    if (!IS_NPC(tch) && CAN_SEE(ch, tch)
        && PLR_FLAGGED(tch, PLR_KILLER))
    {
      act("$n screams 'HEY!!!  You're one of those PLAYER KILLERS!!!!!!'", FALSE, ch, 0, 0, TO_ROOM);
      start_fighting(ch, tch);
      return (TRUE);
    }
  }

  for (tch = IN_ROOM(ch)->people; tch; tch = tch->next_in_room)
  {
    if (!IS_NPC(tch) && CAN_SEE(ch, tch)
        && PLR_FLAGGED(tch, PLR_THIEF))
    {
      act("$n screams 'HEY!!!  You're one of those PLAYER THIEVES!!!!!!'", FALSE, ch, 0, 0, TO_ROOM);
      start_fighting(ch, tch);
      return (TRUE);
    }
  }

  for (tch = IN_ROOM(ch)->people; tch; tch = tch->next_in_room)
  {
    if (CAN_SEE(ch, tch) && FIGHTING(tch))
    {
      if ((GET_ALIGNMENT(tch) < max_evil) &&
          (IS_NPC(tch) || IS_NPC(FIGHTING(tch))))
      {
        max_evil = GET_ALIGNMENT(tch);
        evil = tch;
      }
    }
  }

  if (evil && (GET_ALIGNMENT(FIGHTING(evil)) >= 0))
  {
    act("$n screams 'PROTECT THE INNOCENT!  BANZAI!  CHARGE!  ARARARAGGGHH!'", FALSE, ch, 0, 0, TO_ROOM);
    start_fighting(ch, evil);
    return (TRUE);
  }
  return (FALSE);
}


#define PET_PRICE(pet) ((gold_int)(GET_LEVEL(pet) * (MOB_TIER(pet)+1) * GET_MAX_HIT(pet)))
#define MAX_PETS 6

SPECIAL(pet_shops)
{
  char buf[MAX_STRING_LENGTH], pet_name[256];
  room_rnum pet_room;
  struct char_data *pet, *k, *tch;
  int num_of_pets = 0;
const char *simple_class_name(struct char_data *ch);
const char *race_name(struct char_data *ch);


  struct follow_type *f, *f_next;


  pet_room = world_vnum[IN_ROOM(ch)->number + 1];
  if (pet_room == NULL)
    return 0;

  if (CMD_IS("list"))
  {
    send_to_char("To buy a pet and name it, use: \r\n", ch);
    send_to_char("buy <pet> <the name you give it>\r\n", ch);
    send_to_char("Available pets are:\r\n", ch);
    for (pet = pet_room->people; pet; pet = pet->next_in_room)
    {
      if (IS_NPC(pet))
      {
        sprintf(buf, "%8lld - %-20s (Class: %s, Race: %s)\r\n", PET_PRICE(pet),
                GET_NAME(pet), simple_class_name(pet), race_name(pet) );
        send_to_char(buf, ch);
      }
    }
    return (TRUE);
  }
  else if (CMD_IS("buy"))
  {

    if (IS_NPC(ch))
    {
      send_to_char("You are a mob: No pets for you!", ch);
      return (TRUE);
    }

    two_arguments(argument, buf, pet_name);

    if (!(pet = get_char_room(buf, NULL, pet_room)))
    {
      send_to_char("There is no such pet!\r\n", ch);
      return (TRUE);
    }
    if (ch->master != NULL)
      k = ch->master;
    else
      k = ch;
    for (f = k->followers; f; f = f_next)
    {
      f_next = f->next;
      tch = f->follower;
      if (!AFF_FLAGGED(tch, AFF_CHARM))
        continue;
      num_of_pets++;
    }
    if (num_of_pets > MAX_PETS)
    {
      send_to_char
      ("You already have the maximum number of that kind of pet.\r\n",
       ch);
      return (TRUE);
    }
    if (char_gold(ch, 0, GOLD_HAND) < PET_PRICE(pet))
    {
      send_to_char("You don't have enough gold!\r\n", ch);
      return (TRUE);
    }
    char_gold(ch, -PET_PRICE(pet), GOLD_HAND);;

    pet = read_mobile(GET_MOB_RNUM(pet), REAL);
    GET_EXP(pet) = 0;
    SET_BIT_AR(AFF_FLAGS(pet), AFF_CHARM);
    char_gold(pet, -GET_GOLD(pet), GOLD_HAND);


    if (*pet_name)
    {
      sprintf(buf, "%s %s", pet->player.name, pet_name);
      /* free(pet->player.name); don't free the prototype! */
      pet->player.name = str_dup(buf);

      sprintf(buf,
              "%sA small sign on a chain around the neck says 'My name is %s.\r\n",
              pet->player.description, pet_name);
      /* free(pet->player.description); don't free the prototype! */
      pet->player.description = str_dup(buf);

      sprintf(buf, "%s named %s owned by %s stands here.\r\n",
              pet->player.short_descr, pet_name, GET_NAME(ch));
      pet->player.long_descr = str_dup(buf);
    }
    else
    {
      sprintf(buf, "%s owned by %s stands here.\r\n",
              pet->player.short_descr, GET_NAME(ch));
      pet->player.long_descr = str_dup(buf);
    }
    char_to_room(pet, IN_ROOM(ch));
    add_follower(pet, ch);
    SET_BIT_AR(AFF_FLAGS(pet), AFF_GROUP);
    load_mtrigger(pet);

    /* Be certain that pets can't get/carry/use/wield/wear items */
    GET_GOLD(pet) = 0;
    GET_EXP(pet) = 0;

    send_to_char("May you enjoy your pet.\r\n", ch);
    act("$n buys $N as a pet.", FALSE, ch, 0, pet, TO_ROOM);

    return (1);
  }
  /* All commands except list and buy */
  return (0);
}


/* ********************************************************************
*  Special procedures for objects                                     *
******************************************************************** */


SPECIAL(bank)
{
  gold_int amount = 0;
  int all = !strcmp(argument, "all");

  if (CMD_IS("balance"))
  {
    if (GET_BANK_GOLD(ch) > 0)
      new_send_to_char(ch, "Your current balance is %lld coins.\r\n",
                       GET_BANK_GOLD(ch));
    else
      new_send_to_char(ch, "You currently have no money deposited.\r\n");
    return (1);
  }
  else if (CMD_IS("deposit"))
  {
    if (!all)
    {
      if (atoll(argument) <= 0)
      {
        send_to_char("How much do you want to deposit?\r\n", ch);
        return (1);

      }
      else
        amount = atoll(argument);
    }
    else
      amount = char_gold(ch, 0, GOLD_HAND);

    if (char_gold(ch, 0, GOLD_HAND) < amount)
    {
      send_to_char("You don't have that many coins!\r\n", ch);
      return (1);
    }
    if (char_gold(ch, 0, GOLD_BANK) + amount > 100000000)
      new_send_to_char(ch, "With bank accounts with more then 100mil a 10 percent fee is charged on withdrawl. Thankyou.\r\n");
    char_gold(ch, -amount, GOLD_HAND);
    char_gold(ch, amount, GOLD_BANK);
    new_send_to_char(ch, "You deposit %lld coins.\r\n", amount);
    act("$n makes a bank transaction.", TRUE, ch, 0, FALSE, TO_ROOM);
    return (1);
  }
  else if (CMD_IS("withdraw"))
  {
    if (!all)
    {
      if ((amount = atoll(argument)) <= 0)
      {
        send_to_char("How much do you want to withdraw?\r\n", ch);
        return (1);
      }
    }
    else
      amount = char_gold(ch, 0, GOLD_BANK);
    if (char_gold(ch, 0, GOLD_BANK) < amount)
    {
      send_to_char("You don't have that many coins deposited!\r\n",
                   ch);
      return (1);
    }
    if (char_gold(ch, 0, GOLD_BANK) > 100000000)
    {
      new_send_to_char(ch, "Because of your huge bank investment, and the untimely withdraw,\r\n"
                       "You are charged bank fees summing to 10 percent of the amount.\r\n");
      if ((char_gold(ch, 0, GOLD_BANK)-(amount + 1)) < (amount/10))
      {
        new_send_to_char(ch, "Which you cant afford. Please withdraw a smaller amount.\r\n");
        return (1);
      }
      char_gold(ch, -((amount/10)+1), GOLD_BANK);
    }
    char_gold(ch, amount, GOLD_HAND);
    char_gold(ch, -amount, GOLD_BANK);
    new_send_to_char(ch, "You withdraw %lld coins.\r\n", amount);
    act("$n makes a bank transaction.", TRUE, ch, 0, FALSE, TO_ROOM);
    return (1);
  }
  else
    return (0);
}

/* This special procedure makes a mob into a 'rent-a-cleric', who sells spells
   by the sea shore... uuh, maybe not.  Anyway, the mob will also cast certain
   spells on low-level characters in the room for free.  
   By:  Wyatt Bode	Date:  April, 1996
*/
SPECIAL(cleric)
{
  int i;
  char buf[MAX_STRING_LENGTH];
  struct char_data *vict;
  struct price_info
  {
    short int number;
    char name[25];
    gold_int price;
  }
  prices[] = {
               /* Spell Num (defined)      Name shown        Price  */
               {
                 SPELL_ARMOR, "armor", 5000}, {
                 SPELL_BLESS, "bless", 5000}, {
                 SPELL_ANTIDOTE_1, "remove poison", 5000}, {
                 SPELL_CURE_BLIND, "cure blindness", 5000}, {
                 SPELL_CURE_CRITIC, "critic", 7000},
               {SPELL_SANCTUARY, "sanctuary", 3500},
               {SPELL_HEAL, "heal", 10000},
               {SPELL_SHIELD, "shield", 500000},
               {SPELL_STONESKIN, "stoneskin", 1000000},
               {SPELL_VITALIZE, "vitalize", 5000000},
               /* The next line must be last, add new spells above. */
               {-1, "\r\n", 0}
             };

  /* NOTE:  In interpreter.c, you must define a command called 'heal' for this
     spec_proc to work.  Just define it as do_not_here, and the mob will take 
     care of the rest.  (If you don't know what this means, look in interpreter.c
     for a clue.)
  */

  if (CMD_IS("heal"))
  {
    argument = one_argument(argument, buf);

    if (GET_POS(ch) == POS_FIGHTING)
      return TRUE;

    if (*buf)
    {
      for (i = 0; prices[i].number > SPELL_RESERVED_DBC; i++)
      {
        if (is_abbrev(buf, prices[i].name))
        {
          if (char_gold(ch, 0, GOLD_HAND) < prices[i].price)
          {
            act("$n tells you, 'You don't have enough gold for that spell!'", FALSE, (struct char_data *) me, 0, ch, TO_VICT);
            return TRUE;
          }
          else
          {

            act("$N gives $n some money.",  FALSE, (struct char_data *) me, 0, ch, TO_NOTVICT);
            new_send_to_char(ch, "You give %s %lld coins.\r\n", GET_NAME((struct char_data *) me), prices[i].price);
            char_gold(ch, -prices[i].price, GOLD_HAND);
            char_gold((struct char_data *) me, prices[i].price, GOLD_HAND);
            cast_spell((struct char_data *) me, ch, NULL, 0, prices[i].number);
            return TRUE;

          }
        }
      }
      act("$n tells you, 'I do not know of that spell!"
          "  Type 'heal' for a list.'", FALSE,
          (struct char_data *) me, 0, ch, TO_VICT);

      return TRUE;
    }
    else
    {
      act("$n tells you, 'Here is a listing of the prices for my services.'", FALSE, (struct char_data *) me, 0, ch, TO_VICT);
      for (i = 0; prices[i].number > SPELL_RESERVED_DBC; i++)
      {
        new_send_to_char(ch, "{cc%-15s {cg- {cy%lld{c0\r\n", prices[i].name, prices[i].price);
      }
      return TRUE;
    }
  }

  if (cmd)
    return FALSE;

  /* pseudo-randomly choose someone in the room */
  for (vict = IN_ROOM(ch)->people; vict; vict = vict->next_in_room)
    if (!number(0, 3))
      break;

  /* change the level at the end of the next line to control free spells */
  if (vict == NULL || IS_NPC(vict) || (GET_LEVEL(vict) > 10))
    return FALSE;

  switch (number(1, GET_LEVEL(vict)))
  {
  case 1:
    cast_spell(ch, vict, NULL, 0, SPELL_CURE_LIGHT);
    break;
  case 2:
    cast_spell(ch, vict, NULL, 0, SPELL_BLESS);
    break;
  case 3:
    cast_spell(ch, vict, NULL, 0, SPELL_ARMOR);
    break;
  case 4:
    cast_spell(ch, vict, NULL, 0, SPELL_CURE_LIGHT);
    break;
  case 5:
    cast_spell(ch, vict, NULL, 0, SPELL_BLESS);
    break;
  case 6:
    cast_spell(ch, vict, NULL, 0, SPELL_CURE_CRITIC);
    break;
  case 7:
    cast_spell(ch, vict, NULL, 0, SPELL_ARMOR);
    break;
  case 8:
    cast_spell(ch, vict, NULL, 0, SPELL_CURE_CRITIC);
    break;
  case 9:
    cast_spell(ch, vict, NULL, 0, SPELL_ARMOR);
    break;
  case 10:
    /* special wacky thing, your mileage may vary */
    act("$n utters the words, 'energizer'.", TRUE, ch, 0, vict,
        TO_ROOM);
    act("You feel invigorated!", FALSE, ch, 0, vict, TO_VICT);
    alter_move(ch,
               -MIN(GET_MAX_MOVE(vict),
                    MAX((GET_MOVE(vict) + 10), number(50, 200))));
    break;
  }
  return TRUE;
}

/* corpse recovery utility
 * written by osiris for wintermute 10/11/98
 */
ACMD(do_recover)
{
  int num = 0, found = 0;
  gold_int amt;
  struct obj_data *obj = NULL;
  void perform_meld(CHAR_DATA *ch, OBJ_DATA *corpse);

  struct corpse_list_data *temp = NULL, *tnext;

  /* check to see if the character is at the altar */
  if (IN_ROOM(ch)->number != 3083)
  {
    send_to_char("You can not do that here!\r\n", ch);
    return;
  }


  /* determine the cost per recovery */
  if (GET_LEVEL(ch) <= 20)
    amt = 100000;
  else
    amt = GET_LEVEL(ch) * 50000;

  amt += REMORTS(ch) * 500000;
  /* charge the character */
  if (char_gold(ch, 0, GOLD_BANK) < amt)
  {
    send_to_char("Your bank account can not afford this service!\r\n", ch);
    return;
  }
  /* search the world for the corpse, if it still exists */
  for (num = 0, temp = corpse_list; temp; temp = tnext)
  {
    tnext = temp->next;
    obj = temp->corpse;

    if (GET_OBJ_VAL(obj, 0) != get_pidx_from_name(ch))
      continue;
    if (!obj->contains)
    {
      new_send_to_char(ch, "An empty corpse is found. Not Recovered. Removed From List.\r\n");
      remove_corpse_from_list(obj);
      continue;
    }
    else if (obj->carried_by)
    {
      new_send_to_char(ch, "Your corpse was found being carried by %s and can't be recovered.\r\n", GET_NAME(obj->carried_by));
      continue;
      remove_corpse_from_list(obj);
    }
    if (IN_ROOM(obj) == NULL)
      continue;


    found = 1;


    perform_meld(ch, obj);
    /*
    // send the message out to the character
       send_to_char("You recover your corpse!\r\n", ch);

    // perform the move 
    obj_from_room(obj);
    obj_to_room(obj, IN_ROOM(ch));


    // save the character 
    save_char(ch, NOWHERE);
    Crash_crashsave(ch);
    */
  }

  if (!found)
    send_to_char("Your corpse can not be found. Sorry!/r/n", ch);
  else
  {

    new_send_to_char(ch, "You are charged %lld gold for the recovery.\r\n",amt);
    char_gold(ch, -amt, GOLD_BANK);
  }
}

SPECIAL(guard_white)
{
  char buf[MAX_STRING_LENGTH];
  struct char_data *victim;
  struct char_data *ech;
  char *crime;
  int max_evil;

  if (GET_POS(ch) < POS_SLEEPING || FIGHTING(ch) != NULL || !IS_NPC(ch))
    return FALSE;

  max_evil = 300;
  ech = NULL;
  crime = "";

  for (victim = IN_ROOM(ch)->people; victim != NULL;
       victim = victim->next_in_room)
  {
    if (!IS_NPC(victim) && PLR_FLAGGED(victim, PLR_KILLER))
    {
      crime = "KILLER";
      break;
    }

    if (!IS_NPC(victim) && PLR_FLAGGED(victim, PLR_THIEF))
    {
      crime = "THIEF";
      break;
    }

    if (FIGHTING(victim) != NULL
        && FIGHTING(victim) != ch
        && GET_ALIGNMENT(victim) < max_evil)
    {
      max_evil = GET_ALIGNMENT(victim);
      ech = victim;
    }
  }

  if (victim != NULL)
  {
    sprintf(buf, "%s is a %s!  How DARE you come to the Temple!!!!",
            GET_NAME(victim), crime);
    do_say(ch, buf, 0, 0);
    start_fighting(ch, victim);
    return TRUE;
  }

  if (ech != NULL)
  {
    act("$n screams ' Now you DIE you Bastard!!!!",
        FALSE, ch, NULL, NULL, TO_ROOM);
    start_fighting(ch, ech);
    return TRUE;
  }

  return FALSE;
}

SPECIAL(guard_black)
{
  char buf[MAX_STRING_LENGTH];
  struct char_data *victim;
  struct char_data *ech;
  char *crime;
  int max_good;

  if (GET_POS(ch) < POS_SLEEPING || FIGHTING(ch) != NULL || !IS_NPC(ch))
    return FALSE;

  max_good = -300;
  ech = NULL;
  crime = "";

  for (victim = IN_ROOM(ch)->people; victim != NULL;
       victim = victim->next_in_room)
  {
    if (!IS_NPC(victim) && PLR_FLAGGED(victim, PLR_KILLER))
    {
      crime = "KILLER";
      break;
    }

    if (!IS_NPC(victim) && PLR_FLAGGED(victim, PLR_THIEF))
    {
      crime = "THIEF";
      break;
    }

    if (FIGHTING(victim) != NULL
        && FIGHTING(victim) != ch
        && GET_ALIGNMENT(victim) < max_good)
    {
      max_good = GET_ALIGNMENT(victim);
      ech = victim;
    }
  }

  if (victim != NULL)
  {
    sprintf(buf, "%s is a %s!  How DARE you come to the Temple!!!!",
            GET_NAME(victim), crime);
    do_say(ch, buf, 0, 0);
    start_fighting(ch, victim);
    return TRUE;
  }

  if (ech != NULL)
  {
    act("$n screams ' Now you DIE you Bastard!!!!",
        FALSE, ch, NULL, NULL, TO_ROOM);
    start_fighting(ch, ech);
    return TRUE;
  }

  return FALSE;
}

SPECIAL(bottle)
{
  int vnum = 0;
  struct obj_data *obj = (struct obj_data *) me;
  struct char_data *mob;
  ACMD(do_hit);

  if (CMD_IS("open"))
  {
    skip_spaces(&argument);
    if (isname(argument, obj->name) && CAN_SEE_OBJ(ch, obj)
        && GET_OBJ_VAL(obj, 1) != 0)
    {
      if (GET_OBJ_VNUM(obj) == 216)
      {
        GET_OBJ_VAL(obj, 1) = 0;	// open the object
        vnum = real_mobile(204);
        mob = read_mobile(vnum, REAL);
        act("As you open the bottle, $N jumps out and begins biting you!", FALSE, ch, 0, mob, TO_CHAR);
        act("As $n opens the bottle, $N jumps out and begins biting $m!", FALSE, ch, 0, mob, TO_ROOM);
        act("Going into a blood frenzy, $E attacks you!",
            FALSE, ch, 0, mob, TO_CHAR);
        act("Going into a blood frenzy, $E attacks $m!", FALSE, ch, 0, mob, TO_ROOM);	/* Ahh, flavor text. */
        GET_POS(ch) = POS_SITTING;	/* Bash, essentially. */
        WAIT_STATE(ch, PULSE_VIOLENCE * 2);	/* Please allow 2 to 3 tics for delivery. */
        char_to_room(mob, IN_ROOM(ch));
        start_fighting(mob, ch);	// Start the mob fighting the player. Hiiiiyyaaa! */
        return TRUE;
      }
      else if (GET_OBJ_VNUM(obj) == 1144)
      {
        GET_OBJ_VAL(obj, 1) = 0;	// open the bottle
        vnum = real_mobile(1163);
        mob = read_mobile(vnum, REAL);
        act("$N crawls out of the box and stings your hand!",
            FALSE, ch, 0, mob, TO_CHAR);
        act("$N crawls out of the box and stings $n on the hand.",
            FALSE, ch, 0, mob, TO_ROOM);
        GET_POS(ch) = POS_SITTING;
        WAIT_STATE(ch, PULSE_VIOLENCE * 2);
        char_to_room(mob, IN_ROOM(ch));
        start_fighting(mob, ch);
        return TRUE;
      }
      else if (GET_OBJ_VNUM(obj) == 7563)
      {
        GET_OBJ_VAL(obj, 1) = 0;	// open the bottle
        vnum = real_mobile(7557);
        mob = read_mobile(vnum, REAL);
        act("$N crawls out of the box and stings your hand!",
            FALSE, ch, 0, mob, TO_CHAR);
        act("$N crawls out of the box and stings $n on the hand.",
            FALSE, ch, 0, mob, TO_ROOM);
        GET_POS(ch) = POS_SITTING;
        WAIT_STATE(ch, PULSE_VIOLENCE * 2);
        char_to_room(mob, IN_ROOM(ch));
        start_fighting(mob, ch);
        return TRUE;
      }
      else if (GET_OBJ_VNUM(obj) == 1144)
      {
        GET_OBJ_VAL(obj, 1) = 0;	// open the bottle
        vnum = real_mobile(1163);
        mob = read_mobile(vnum, REAL);
        act("$N crawls out of the box and stings your hand!",
            FALSE, ch, 0, mob, TO_CHAR);
        act("$N crawls out of the box and stings $n on the hand.",
            FALSE, ch, 0, mob, TO_ROOM);
        GET_POS(ch) = POS_SITTING;
        WAIT_STATE(ch, PULSE_VIOLENCE * 2);
        char_to_room(mob, IN_ROOM(ch));
        start_fighting(mob, ch);
        return TRUE;
      }
      else if (GET_OBJ_VNUM(obj) == 11245)
      {
        GET_OBJ_VAL(obj, 1) = 0;
        mob = read_mobile(real_mobile(11229), REAL);
        act("The valut guard rushes into the room, with his great long sword, ready to strike.", FALSE, ch, 0, mob, TO_ROOM);
        act("The vault guard rushes into the room, with his great long sword, ready to attack you.", FALSE, ch, 0, mob, TO_VICT);
        GET_POS(ch) = POS_SITTING;
        WAIT_STATE(ch, PULSE_VIOLENCE * 2);
        char_to_room(mob, IN_ROOM(ch));
        start_fighting(mob, ch);
        return TRUE;
      }
      else if (GET_OBJ_VNUM(obj) == 11244)
      {
        GET_OBJ_VAL(obj, 1) = 0;
        mob = read_mobile(real_mobile(11096), REAL);
        act("The ghost of the fallen warrior emerges from hell to seek his revenge!", FALSE, ch, 0, mob, TO_VICT);
        act("The ghost of the fallen warrior emerges from hell to seek his revenge!", FALSE, ch, 0, mob, TO_ROOM);
        GET_POS(ch) = POS_SITTING;
        WAIT_STATE(ch, PULSE_VIOLENCE * 2);
        char_to_room(mob, IN_ROOM(ch));
        start_fighting(mob, ch);
        return TRUE;
      }
    }
  }
  return FALSE;
}

SPECIAL(door_down)
{
  int vnum = 0;
  int door = -1;
  char type[MAX_INPUT_LENGTH], dir[MAX_INPUT_LENGTH];
  struct obj_data *obj = NULL;
  struct char_data *victim = NULL;
  struct char_data *mob;

  if (CMD_IS("open"))
  {
    skip_spaces(&argument);
    two_arguments(argument, type, dir);
    if (isname(type, "trapdoor"))
    {
      // vnum = real_mobile(number(2300, 2302));
      /* This is just to have it random pick between the three vampires
         using their vnums. You should probably change it to the vnum
         (or random vnums) you want it to load. */
      if (!generic_find
          (type, FIND_OBJ_INV | FIND_OBJ_ROOM, ch, &victim, &obj))
        door = find_door(ch, type, dir, cmd_door[SCMD_OPEN]);
      if (EXIT_FLAGGED(EXIT(ch, door), EX_CLOSED))
      {
        do_doorcmd(ch, obj, door, SCMD_OPEN);
        vnum = real_mobile(204);
        mob = read_mobile(vnum, REAL);
        act("As you open the trapdoor, $N jumps out and begins biting you!", FALSE, ch, 0, mob, TO_CHAR);
        act("As $n opens the trapdoor, $N jumps out and begins biting $m!", FALSE, ch, 0, mob, TO_ROOM);
        act("Going into a blood frenzy, $E attacks you!", FALSE,
            ch, 0, mob, TO_CHAR);
        act("Going into a blood frenzy, $E attacks $m!", FALSE, ch, 0, mob, TO_ROOM);	/* Ahh, flavor text. */
        GET_POS(ch) = POS_SITTING;	/* Bash, essentially. */
        WAIT_STATE(ch, PULSE_VIOLENCE * 2);	/* Please allow 2 to 3 tics for delivery. */
        char_to_room(mob, IN_ROOM(ch));
        start_fighting(mob, ch);	// Start the mob fighting the player. Hiiiiyyaaa! */
        return TRUE;
      }
    }
  }
  return FALSE;
}

SPECIAL(door_down_7377)
{
  int vnum = 0;
  int door = -1;
  char type[MAX_INPUT_LENGTH], dir[MAX_INPUT_LENGTH];
  struct obj_data *obj = NULL;
  struct char_data *victim = NULL;
  struct char_data *mob;


  if (CMD_IS("open"))
  {
    skip_spaces(&argument);
    two_arguments(argument, type, dir);
    if (isname(type, "lid"))
    {
      // vnum = real_mobile(number(2300, 2302));
      /* This is just to have it random pick between the three vampires
         using their vnums. You should probably change it to the vnum
         (or random vnums) you want it to load. */
      if (!generic_find
          (type, FIND_OBJ_INV | FIND_OBJ_ROOM, ch, &victim, &obj))
        door = find_door(ch, type, dir, cmd_door[SCMD_OPEN]);
      if (EXIT_FLAGGED(EXIT(ch, door), EX_CLOSED))
      {
        do_doorcmd(ch, obj, door, SCMD_OPEN);
        vnum = real_mobile(7376);
        mob = read_mobile(vnum, REAL);
        act("A green, slimy THING slithers out of the toilet and bites off your... whatever!", FALSE, ch, 0, mob, TO_CHAR);
        act("As $n opens the lid, a green, slimy THING slithers out of the toilet and bites $m. Ouch!", FALSE, ch, 0, mob, TO_ROOM);
        act("You feel {cWDIMINISHED!{c0", FALSE, ch, 0, mob,
            TO_CHAR);
        act("$n seems to have diminished!", FALSE, ch, 0, mob, TO_ROOM);	/* Ahh, flavor text. */
        GET_POS(ch) = POS_SITTING;	/* Bash, essentially. */
        WAIT_STATE(ch, PULSE_VIOLENCE * 2);	/* Please allow 2 to 3 tics for delivery. */
        char_to_room(mob, IN_ROOM(ch));
        start_fighting(mob, ch);	// Start the mob fighting the player. Hiiiiyyaaa! */
        return TRUE;
      }
    }
  }
  return FALSE;
}


SPECIAL(triples)
{
  int bet;
  char buf[256], buf2[10];
  struct char_data *mob = (struct char_data *) me;

  if (CMD_IS("bet"))
  {
    two_arguments(argument, buf, buf2);

    if (!*buf || !*buf2)
    {
      send_to_char("bet <upper|lower|triple> <amt>.\r\n", ch);
      return TRUE;
    }

    bet = atoi(buf2);
    play_triples(ch, mob, buf, bet);
    return TRUE;
  }

  return FALSE;
}

SPECIAL(slots)
{
  if (CMD_IS("pull"))
  {
    play_slots(ch);
    return TRUE;
  }

  return FALSE;
}

SPECIAL(high_dice)
{
  int bet;
  char buf[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];
  struct char_data *mob = (struct char_data *) me;

  if (CMD_IS("bet"))
  {
    two_arguments(argument, buf, buf2);

    if (!*buf)
    {
      send_to_char("bet <amt>.\r\n", ch);
      return TRUE;
    }

    bet = atoi(buf);
    play_high_dice(ch, mob, bet);
    return TRUE;
  }

  return FALSE;
}

SPECIAL(seven)
{
  int bet;
  char buf[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];;
  struct char_data *mob = (struct char_data *) me;

  if (CMD_IS("bet"))
  {
    two_arguments(argument, buf, buf2);

    if (!*buf || !*buf2)
    {
      send_to_char("bet <over|under|seven> <amt>.\r\n", ch);
      return TRUE;
    }

    bet = atoi(buf2);
    play_seven(ch, mob, buf, bet);
    return TRUE;
  }

  return FALSE;
}

SPECIAL(craps)
{
  int bet;
  char buf[MAX_INPUT_LENGTH];
  struct char_data *mob = (struct char_data *) me;

  if (CMD_IS("bet"))
  {
    one_argument(argument, buf);

    if (!*buf)
    {
      send_to_char("bet <amt>.\r\n", ch);
      return TRUE;
    }

    bet = atoi(buf);
    play_craps(ch, mob, bet);
    return TRUE;
  }

  return FALSE;
}

/* Dragon's breath procedures */
SPECIAL(dragon_fire)
{
  struct char_data *dragon = (struct char_data *) me;

  /* I don't know what 'cmd' is but we never do anything if we don't breathe
   * fire unless we're fighting.
   */
  if (cmd || GET_POS(ch) != POS_FIGHTING)
    return FALSE;

  /* Only breathe fire 20% of the time */
  if (number(0, 4))
    return FALSE;

  /* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
   * breath so we could have tougher dragons.  Right now, it does damage
   * equal to a fireball in all cases.
   */
  /* call_magic(ch, NULL, NULL, SPELL_FIRE_BREATH, 0, CAST_BREATH); */
  damage(dragon, FIGHTING(dragon), dice(GET_LEVEL(dragon), GET_LEVEL(dragon)),SPELL_FIRE_BREATH);

  /* If you use the damage call, you don't need the spell, but if you use
   * the spell, you should add the no_magic room information below.
   */
  return TRUE;

}


SPECIAL(dragon_gas)
{
  struct char_data *dragon = (struct char_data *) me;

  /* I don't know what 'cmd' is but we never do anything if we don't breathe
   * gas unless we're fighting.
   */
  if (cmd || GET_POS(ch) != POS_FIGHTING)
    return FALSE;

  /* Only breathe gas 20% of the time */
  if (number(0, 4))
    return FALSE;

  /* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
   * breath so we could have tougher dragons.  Right now, it does damage
   * equal to a fireball in all cases.
   */
  /* call_magic(ch, NULL, NULL, SPELL_GAS_BREATH, 0, CAST_BREATH); */
  damage(dragon, FIGHTING(dragon), dice(GET_LEVEL(dragon), GET_LEVEL(dragon)),
         SPELL_GAS_BREATH);

  /* If you use the damage call, you don't need the spell, but if you use
   * the spell, you should add the no_magic room information below.
   */

  return TRUE;

}


SPECIAL(dragon_frost)
{
  struct char_data *dragon = (struct char_data *) me;

  /* I don't know what 'cmd' is but we never do anything if we don't breathe
   * frost unless we're fighting.
   */
  if (cmd || GET_POS(ch) != POS_FIGHTING)
    return FALSE;

  /* Only breathe frost 20% of the time */
  if (number(0, 4))
    return FALSE;

  /* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
   * breath so we could have tougher dragons.  Right now, it does damage
   * equal to a fireball in all cases.
   */
  /* call_magic(ch, NULL, NULL, SPELL_FROST_BREATH, 0, CAST_BREATH); */
  damage(dragon, FIGHTING(dragon), dice(GET_LEVEL(dragon), GET_LEVEL(dragon)),
         SPELL_FROST_BREATH);
  /* If you use the damage call, you don't need the spell, but if you use
   * the spell, you should add the no_magic room information below.
   */

  return TRUE;

}


SPECIAL(dragon_acid)
{
  struct char_data *dragon = (struct char_data *) me;

  /* I don't know what 'cmd' is but we never do anything if we don't breathe
   * acid unless we're fighting.
   */

  if (cmd || GET_POS(ch) != POS_FIGHTING)
    return FALSE;

  /* Only breathe acid 20% of the time */
  if (number(0, 4))
    return FALSE;

  /* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
   * breath so we could have tougher dragons.  Right now, it does damage
   * equal to a fireball in all cases.
   */
  /* call_magic(ch, NULL, NULL, SPELL_ACID_BREATH, 0, CAST_BREATH); */
  damage(dragon, FIGHTING(dragon), dice(GET_LEVEL(dragon), GET_LEVEL(dragon)),
         SPELL_ACID_BREATH);

  /* If you use the damage call, you don't need the spell, but if you use
   * the spell, you should add the no_magic room information below.
   */
  return TRUE;
}

SPECIAL(dragon_lightning)
{
  struct char_data *dragon = (struct char_data *) me;

  /* I don't know what 'cmd' is but we never do anything if we don't breathe
   * lightning unless we're fighting.
   */
  if (cmd || GET_POS(ch) != POS_FIGHTING)
    return FALSE;

  /* Only breathe lightning 20% of the time */
  if (number(0, 4))
    return FALSE;

  /* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
   * breath so we could have tougher dragons.  Right now, it does damage
   * equal to a fireball in all cases.
   */
  /* call_magic(ch, NULL, NULL, SPELL_LIGHTNING_BREATH, 0, CAST_BREATH); */
  damage(dragon, FIGHTING(dragon), dice(GET_LEVEL(dragon), GET_LEVEL(dragon)),
         SPELL_LIGHTNING_BREATH);

  /* If you use the damage call, you don't need the spell, but if you use
   * the spell, you should add the no_magic room information below.
   */
  return TRUE;
}

/* Stuff I have coded -- kalten */
SPECIAL(fire)
{
  struct obj_data *target, *viewport, *vehicle;
  bool found = FALSE;
  int dir;
  room_rnum room, nextroom;
  int distance;
  room_rnum was_in;
  int range = 5;
  int dam = 0, percent, chance = 100;
  char arg1[MAX_INPUT_LENGTH];	/* target */
  char arg2[MAX_INPUT_LENGTH];	/* direction */

  if (!CMD_IS("fire"))
    return (0);

  /* since inside a vehicle, need to save the room they were in */
  was_in = IN_ROOM(ch);

  /* now, find the room the vehicle is in and put the character there */
  viewport =
    get_obj_in_list_type(ITEM_V_WINDOW, IN_ROOM(ch)->contents);
  if (viewport)
  {
    vehicle = find_vehicle_by_vnum(GET_OBJ_VAL(viewport, 0));
    IN_ROOM(ch) = vehicle->in_room;
  }

  room = IN_ROOM(ch);

  two_arguments(argument, arg1, arg2);

  /* can they even fire that way */
  if ((dir = search_block(arg2, dirs, FALSE)) < 0)
  {
    send_to_char("What direction?\r\n", ch);
    IN_ROOM(ch) = was_in;
    return (1);
  }

  /* can't go that way */
  if (!CAN_GO(ch, dir))
  {
    send_to_char("Something blocks the way!\r\n", ch);
    IN_ROOM(ch) = was_in;
    return (1);
  }

  /* now that you can go that way, find the next room */
  if CAN_GO2
  (room, dir)
    nextroom = EXIT2(room, dir)->to_room;
  else
    nextroom = NULL;

  /* now, find the target */
  for (distance = 1; ((nextroom != NULL) && (distance <= range));
       distance++)
  {
    /* search for the target in the room */
    for (target = nextroom->contents; target;
         target = target->next_content)
    {
      if ((isname(arg1, GET_OBJ_NAME(target)))
          && (CAN_SEE_OBJ(ch, target)))
      {
        found = TRUE;
        break;
      }
    }
    /* we have a target now */
    if (found)
    {
      percent = number(0, 101);
      chance /= distance;
      if (percent < chance)
      {	/* the closer the ship is, the better chance of hitting it */
        if ((dam = number(0, 2)) > 0)
        {
          /* damage the vehicle */
          GET_OBJ_VAL(target, 2) -= dam;
          /* check to see if it should blow up now */
          if (GET_OBJ_VAL(target, 2) <= 0)
          {
            /* Blow it up */
            IN_ROOM(ch) = was_in;
            act("The $p was blown up.", FALSE, ch, target,
                NULL, TO_CHAR);
            act("$n blows up the $p.", TRUE, ch, target, NULL,
                TO_ROOM);

            extract_obj(target);
            return (1);
          }
          else
          {	/* needs more damage */
            IN_ROOM(ch) = was_in;
            act("Your missile strikes the $p.", FALSE, ch,
                target, NULL, TO_CHAR);
            act("$n's missile strikes the $p.", TRUE, ch,
                target, NULL, TO_ROOM);
            explosion_messages(real_room
                               (GET_OBJ_VAL(target, 0)),
                               dam, target);
            WAIT_STATE(ch, PULSE_VIOLENCE * 2);
            return (1);
          }
        }
        else
        {	/* didn't do any damage */
          IN_ROOM(ch) = was_in;
          act("The missile misses the $p.", FALSE, ch, target,
              NULL, TO_CHAR);
          act("$n misses the $p.", TRUE, ch, target, NULL,
              TO_ROOM);
          explosion_messages(real_room(GET_OBJ_VAL(target, 0)),
                             dam, target);
          WAIT_STATE(ch, PULSE_VIOLENCE * 2);
          return (1);
        }
      }
      else
      {		/* missed it completely */
        IN_ROOM(ch) = was_in;
        act("The missile misses the $p.", FALSE, ch, target, NULL,
            TO_CHAR);
        act("$n misses the $p.", TRUE, ch, target, NULL, TO_ROOM);
        explosion_messages(real_room(GET_OBJ_VAL(target, 0)),
                           dam, target);
        WAIT_STATE(ch, PULSE_VIOLENCE * 2);
        return (1);
      }
    }

    /* target wasnt in the room, move to next room */
    room = nextroom;
    if CAN_GO2
    (room, dir)
      nextroom = EXIT2(room, dir)->to_room;
    else
      nextroom = NULL;
  }

  send_to_char("Cant find your target!\r\n", ch);
  IN_ROOM(ch) = was_in;
  return (1);
}

SPECIAL(radar)
{
  struct char_data *i;
  struct obj_data *viewport, *vehicle;
  room_rnum was_in,is_in;
  int dir, dis, maxdis, found = 0;

  const char *distance[] =
    {
      "right here",
      "immediately ",
      "nearby ",
      "a ways ",
      "far ",
      "very far ",
      "extremely far ",
      "impossibly far ",
    };

  if (!CMD_IS("radar"))
    return (0);

  if (IS_AFFECTED(ch, AFF_BLIND))
  {
    send_to_char("You can't see anything, you're blind!", ch);
    return (1);
  }

  /* since inside a vehicle, need to save the room they were in */
  was_in = IN_ROOM(ch);

  /* now, find the room the vehicle is in and put the character there */
  viewport =
    get_obj_in_list_type(ITEM_V_WINDOW, IN_ROOM(ch)->contents);
  if (viewport)
  {
    if ((vehicle =
           find_vehicle_by_vnum(GET_OBJ_VAL(viewport, 0))) != NULL)
      IN_ROOM(ch) = vehicle->in_room;
    else
      return (0);
  }

  is_in = IN_ROOM(ch);

  if (GET_LEVEL(ch) >= LVL_IMMORT)
    maxdis = 7;
  else
    maxdis = 5;

  IN_ROOM(ch) = was_in;
  send_to_char("You begin watching the radar screen and see:\r\n", ch);
  act("$n begins watching the radar screen.", TRUE, ch, 0, 0, TO_ROOM);

  for (dir = 0; dir < NUM_OF_DIRS; dir++)
  {
    IN_ROOM(ch) = is_in;
    for (dis = 0; dis <= maxdis; dis++)
    {
      if (((dis == 0) && (dir == 0)) || (dis > 0))
      {
        for (i = IN_ROOM(ch)->people; i; i = i->next_in_room)
        {
          if ((!((ch == i) && (dis == 0))) && CAN_SEE(ch, i))
          {
            new_send_to_char(ch, "%33s: %s%s%s%s", GET_NAME(i),
                             distance[dis], ((dis > 0)
                                             && (dir <
                                                 (NUM_OF_DIRS -
                                                  2))) ? "to the " : "",
                             (dis > 0) ? dirs[dir] : "", ((dis > 0)
                                                          && (dir >
                                                              (NUM_OF_DIRS
                                                               -
                                                               3))) ?
                             "wards" : "");
            found++;
          }
        }
      }
      if (!CAN_GO(ch, dir)
          || (IN_ROOM(ch)->dir_option[dir]->to_room == is_in))
        break;
      else
        IN_ROOM(ch) = IN_ROOM(ch)->dir_option[dir]->to_room;
    }
  }
  if (found == 0)
    send_to_char("Nobody anywhere near you.", ch);
  IN_ROOM(ch) = is_in;

  IN_ROOM(ch) = was_in;
  return (1);
}
