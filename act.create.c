/* ************************************************************************
*   File: act.create.c					Part of CircleMUD *
*  Usage: Player-level object creation stuff				  *
*									  *
*  All rights reserved.	 See license.doc for complete information.	  *
*									  *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.		  *
************************************************************************ */
/*
 * $Log: act.create.c,v $
 * Revision 1.2  2004/11/20 02:33:25  w4dimenscor
 * updated and cleaned up the script system
 *
 * Revision 1.1.1.1  2004/11/12 02:16:52  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.37  2004/08/15 01:12:23  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */
 
#include "conf.h"
#include "sysdep.h"

#include <sys/stat.h>

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "constants.h"
#include "dg_scripts.h"
#include "dg_event.h"
#include "genolc.h"


#define TIER (current_class_is_tier_num(ch)+1)


ASKILL(skill_brew);
ASKILL(skill_scribe);
ASKILL(skill_tinker);
ASKILL(skill_sing_wood);
EVENTFUNC(message_event);
ASKILL(skill_manifest);
void make_manifest(struct char_data *ch,struct obj_data *obj);
ASKILL(skill_manipulate);
struct obj_data *make_tree(void);
room_rnum find_forest_rand(void);
void load_trees(void);
void parse_tree_name(struct obj_data *tree);
void create_trees(void);
extern struct char_data *find_char(long n);
extern struct obj_data *find_obj(long n);
extern struct room_data *find_room(long n);
void run_task(struct char_data *ch);
int perf_balance(int weapon_type);
int curr_balance(OBJ_DATA *wep);
int save_forest(void);

/* struct for syls */
struct syllable
{
  char *org;
  char *current;
};



/* extern variables */
extern struct spell_info_type spell_info[];
extern struct syllable syls[];
extern struct index_data *obj_index;
extern struct room_data *world;
extern struct forest_data *forest;
extern int tree_total;
extern int forest_room;

/* extern procedures */
int mag_manacost(struct char_data *ch, int spellnum);
void improve_skill(struct char_data *ch, int skill);
ACTION(thing_lumberjack);
ACTION(thing_manifest);
ACTION(thing_singwood);
ACTION(thing_juggle);
ACTION(thing_throttle);
ACTION(thing_tunneling);

char *get_spell_name(char *argument)
{
  char *s;

  s = strtok(argument, "'");
  s = strtok(NULL, "'");

  return s;
}


#define MAX_TREE_TYPES 9



void make_potion(struct char_data *ch, int potion,
                 struct obj_data *container)
{
  struct obj_data *final_potion;
  struct extra_descr_data *new_descr;
  int can_make = TRUE, mana, dam, num = 0;
  char buf2[MAX_INPUT_LENGTH];

  /* Modify this list to suit which spells you
   * want to be able to mix. */
  switch (potion)
  {
  case SPELL_CURE_BLIND:
    num = 0;
    break;
  case SPELL_CURE_LIGHT:
    num = 1;
    break;
  case SPELL_CURE_CRITIC:
    num = 2;
    break;
  case SPELL_DETECT_MAGIC:
    num = 3;
    break;
  case SPELL_DETECT_INVIS:
    num = 4;
    break;
  case SPELL_DETECT_POISON:
    num = 5;
    break;
  case SPELL_ANTIDOTE_1:
    num = 6;
    break;
  case SPELL_STRENGTH:
    num = 7;
    break;
  case SPELL_WORD_OF_RECALL:
    num = 8;
    break;
  case SPELL_SENSE_LIFE:
    num = 9;
    break;
  case SPELL_WATERWALK:
    num = 10;
    break;
  case SPELL_INFRAVISION:
    num = 11;
    break;
  case SPELL_HEAL:
    num = 12;
    break;
  case SPELL_SANCTUARY:
    num = 13;
    break;
  default:
    can_make = FALSE;
    break;
  }

  if (can_make == FALSE)
  {
    send_to_char("That spell cannot be mixed into a"
                 " potion.\r\n", ch);
    return;
  }
  else if ((number(1, 3) == 3) && (GET_LEVEL(ch) < LVL_HERO))
  {
    send_to_char("As you begin mixing the potion, it violently"
                 " explodes!\r\n", ch);
    act("$n begins to mix a potion, but it suddenly explodes!",
        FALSE, ch, 0, 0, TO_ROOM);
    extract_obj(container);
    dam = number(15, mag_manacost(ch, potion) * 2);
    damage(ch, ch, dam, TYPE_UNDEFINED);
    return;
  }

  /* requires x3 mana to mix a potion than the spell */
  mana = mag_manacost(ch, potion) * 3;
  if (GET_MANA(ch) - mana > 0)
  {
    if (GET_LEVEL(ch) < LVL_HERO)
      alter_mana(ch, mana);
    new_send_to_char(ch, "You create a %s potion.\r\n", skill_name(potion));
    act("$n creates a potion!", FALSE, ch, 0, 0, TO_ROOM);
    extract_obj(container);
  }
  else
  {
    send_to_char("You don't have enough mana to mix"
                 " that potion!\r\n", ch);
    return;
  }

  final_potion = create_obj();

  final_potion->item_number = NOTHING;
  final_potion->in_room = NULL;
  snprintf(buf2, sizeof(buf2), "%s %s potion", potion_names[num], skill_name(potion));
  final_potion->name = str_dup(buf2);

  snprintf(buf2, sizeof(buf2), "A %s potion lies here.", potion_names[num]);
  final_potion->description = str_dup(buf2);

  snprintf(buf2, sizeof(buf2), "a %s potion", potion_names[num]);
  final_potion->short_description = str_dup(buf2);

  /* extra description coolness! */
  CREATE(new_descr, struct extra_descr_data, 1);
  new_descr->keyword = str_dup(final_potion->name);
  snprintf(buf2, sizeof(buf2), "It appears to be a %s potion.", skill_name(potion));
  new_descr->description = str_dup(buf2);
  new_descr->next = NULL;
  final_potion->ex_description = new_descr;

  GET_OBJ_TYPE(final_potion) = ITEM_POTION;
  SET_BIT_AR(GET_OBJ_WEAR(final_potion), ITEM_WEAR_TAKE);
  SET_BIT_AR(GET_OBJ_EXTRA(final_potion), ITEM_NORENT);
  SET_BIT_AR(GET_OBJ_EXTRA(final_potion), ITEM_UNIQUE_SAVE);
  GET_OBJ_VAL(final_potion, 0) = GET_LEVEL(ch);
  GET_OBJ_VAL(final_potion, 1) = potion;
  GET_OBJ_VAL(final_potion, 2) = -1;
  GET_OBJ_VAL(final_potion, 3) = -1;
  GET_OBJ_COST(final_potion) = GET_LEVEL(ch) * 500;
  GET_OBJ_WEIGHT(final_potion) = 10 + (GET_LEVEL(ch) * 0.1);
  GET_OBJ_RENT(final_potion) = 0;
  GET_OBJ_TIMER(final_potion) = 100;

  obj_to_char(final_potion, ch);
  improve_skill(ch, SKILL_BREW);
}

ASKILL(skill_brew)
{
  struct obj_data *container = NULL;
  struct obj_data *next_obj;
  char bottle_name[MAX_STRING_LENGTH];
  char spell_name[MAX_STRING_LENGTH];
  char *temp1, *temp2;
  int potion, found = FALSE;

  if (!knows_spell(ch, SKILL_BREW))
  {
    send_to_char("You are not schooled enough to brew anything!\r\n",
                 ch);
    return 0;
  }

  temp1 = one_argument(argument, bottle_name);



  /* sanity check */
  if (temp1)
  {
    temp2 = get_spell_name(temp1);
    if (temp2)
      strcpy(spell_name, temp2);
  }
  else
  {
    bottle_name[0] = '\0';
    spell_name[0] = '\0';
  }

  if (!*bottle_name || !*spell_name)
  {
    send_to_char("What do you wish to mix in where?\r\n", ch);
    return 0;
  }


  for (obj = ch->carrying; obj; obj = next_obj)
  {
    next_obj = obj->next_content;
    if (obj == NULL)
      return 0;
    else if (!(container = get_obj_in_list_vis(ch, bottle_name, NULL,
                           ch->carrying)))
      continue;
    else
      found = TRUE;
  }
  if (found != FALSE && (GET_OBJ_VNUM(container) != 3044))
  {
    send_to_char("You don't have the proper container!\r\n", ch);
    return 0;
  }
  if (found == FALSE)
  {
    new_send_to_char(ch, "You don't have %s in your inventory!\r\n",
                     bottle_name);
    return 0;
  }

  if (!spell_name || !*spell_name)
  {
    send_to_char("Spell names must be enclosed in single quotes!\r\n",
                 ch);
    return 0;
  }

  potion = find_skill_num(spell_name);

  if ((potion < 1) || (potion > MAX_SPELLS))
  {
    send_to_char("Mix what spell?!?\r\n", ch);
    return 0;
  }
  if (!knows_spell(ch, potion))
  {
    send_to_char("You do not know how to make that potion!\r\n", ch);
    return 0;
  }
  if (GET_SKILL(ch, potion) == 0)
  {
    new_send_to_char(ch, "You are unfamiliar brewing %s.\r\n", skill_name(potion));
    return 0;
  }
  make_potion(ch, potion, container);
  return (SKILL_BREW);
}



void make_scroll(struct char_data *ch, int scroll, struct obj_data *paper)
{
  struct obj_data *final_scroll;
  struct extra_descr_data *new_descr;
  int can_make = TRUE, mana, dam = 0;
  char buf2[MAX_INPUT_LENGTH];

  /* add a case statement here for prohibited spells */

  /* Modify this list to suit which spells you
   * want to be able to mix. */
  switch (scroll)
  {
  case SPELL_CURE_BLIND:
  case SPELL_CURE_LIGHT:
  case SPELL_CURE_CRITIC:
  case SPELL_DETECT_MAGIC:
  case SPELL_DETECT_POISON:
  case SPELL_STRENGTH:
  case SPELL_WORD_OF_RECALL:
  case SPELL_SENSE_LIFE:
  case SPELL_WATERWALK:
  case SPELL_INFRAVISION:
  case SPELL_HEAL:
  case SPELL_SANCTUARY:
  case SPELL_FORTIFY_BODY:
  case SPELL_FLAME_ARROW:
  case SPELL_DEMONSHREAK:
  case SPELL_SHIELD:
    break;

  default:
    can_make = FALSE;
    break;
  }

  if (can_make == FALSE)
  {
    new_send_to_char(ch, "That spell cannot be scribed into a scroll.\r\n");
    return;
  }
  else if ((number(1, 3) == 3) && (GET_LEVEL(ch) < LVL_HERO))
  {
    send_to_char("As you begin inscribing the final rune, the"
                 " scroll violently explodes!\r\n", ch);
    act("$n tries to scribe a spell, but it explodes!",  FALSE, ch, 0, 0, TO_ROOM);
    extract_obj(paper);
    dam = number(15, mag_manacost(ch, scroll) * 2);
    damage(ch, ch, dam, TYPE_UNDEFINED);
    return;
  }
  /* requires x3 mana to scribe a scroll than the spell */
  mana = mag_manacost(ch, scroll) * 3;

  if (GET_MANA(ch) - mana > 0)
  {
    if (GET_LEVEL(ch) < LVL_HERO)
      alter_mana(ch, mana);
    new_send_to_char(ch, "You create a scroll of %s.\r\n", skill_name(scroll));
    act("$n creates a scroll!", FALSE, ch, 0, 0, TO_ROOM);
    extract_obj(paper);
  }
  else
  {
    send_to_char("You don't have enough mana to scribe such"
                 " a powerful spell!\r\n", ch);
    return;
  }

  final_scroll = create_obj();

  final_scroll->item_number = NOTHING;
  final_scroll->in_room = NULL;
  snprintf(buf2, sizeof(buf2), "%s scroll", skill_name(scroll));
  final_scroll->name = str_dup(buf2);

  snprintf(buf2, sizeof(buf2),
           "Some parchment inscribed with the runes '%s' lies here.",
           skill_name(scroll));
  final_scroll->description = str_dup(buf2);

  snprintf(buf2, sizeof(buf2), "a %s scroll", skill_name(scroll));
  final_scroll->short_description = str_dup(buf2);

  /* extra description coolness! */
  CREATE(new_descr, struct extra_descr_data, 1);
  new_descr->keyword = str_dup(final_scroll->name);
  snprintf(buf2, sizeof(buf2), "It appears to be a %s scroll.", skill_name(scroll));
  new_descr->description = str_dup(buf2);
  new_descr->next = NULL;
  final_scroll->ex_description = new_descr;

  GET_OBJ_TYPE(final_scroll) = ITEM_SCROLL;
  SET_BIT_AR(GET_OBJ_WEAR(final_scroll), ITEM_WEAR_TAKE);
  SET_BIT_AR(GET_OBJ_EXTRA(final_scroll), ITEM_NORENT);
  SET_BIT_AR(GET_OBJ_EXTRA(final_scroll), ITEM_UNIQUE_SAVE);
  GET_OBJ_VAL(final_scroll, 0) = GET_LEVEL(ch);
  GET_OBJ_VAL(final_scroll, 1) = scroll;
  GET_OBJ_VAL(final_scroll, 2) = -1;
  GET_OBJ_VAL(final_scroll, 3) = -1;
  GET_OBJ_COST(final_scroll) = GET_LEVEL(ch) * 500;
  GET_OBJ_WEIGHT(final_scroll) = 10 + (GET_LEVEL(ch) * 0.1);;
  GET_OBJ_RENT(final_scroll) = 0;
  GET_OBJ_TIMER(final_scroll) = -1;

  obj_to_char(final_scroll, ch);
}


ASKILL(skill_scribe)
{
  struct obj_data *paper = NULL;
  struct obj_data *next_obj;
  char paper_name[MAX_STRING_LENGTH];
  char spell_name[MAX_STRING_LENGTH];
  char *temp1, *temp2;
  int scroll = 0, found = FALSE;

  temp1 = one_argument(argument, paper_name);

  if (!knows_spell(ch, SKILL_SCRIBE))
  {
    send_to_char("You are not schooled enough to scribe anything!\r\n",
                 ch);
    return 0;
  }

  /* sanity check */
  if (temp1)
  {
    temp2 = get_spell_name(temp1);
    if (temp2)
      strcpy(spell_name, temp2);
  }
  else
  {
    paper_name[0] = '\0';
    spell_name[0] = '\0';
  }


  if (!*paper_name || !*spell_name)
  {
    send_to_char("What do you wish to scribe where?\r\n", ch);
    return 0;
  }

  for (obj = ch->carrying; obj; obj = next_obj)
  {
    next_obj = obj->next_content;
    if (obj == NULL)
      return 0;
    else if (!(paper = get_obj_in_list_vis(ch, paper_name, NULL,
                                           ch->carrying)))
      continue;
    else
      found = TRUE;
  }
  if (found && (GET_OBJ_VNUM(paper) != 3043))
  {
    send_to_char("You can't write on that!\r\n", ch);
    return 0;
  }
  if (found == FALSE)
  {
    new_send_to_char(ch, "You don't have %s in your inventory!\r\n",  paper_name);
    return 0;
  }

  if (!spell_name || !*spell_name)
  {
    new_send_to_char(ch,"Spell names must be enclosed in single quotes!\r\n");
    return 0;
  }

  scroll = find_skill_num(spell_name);

  if ((scroll < 1) || (scroll > MAX_SPELLS))
  {
    send_to_char("Scribe what spell?!?\r\n", ch);
    return 0;
  }
  if (!knows_spell(ch, scroll))
  {
    new_send_to_char(ch, "You are not schooled enough to cast that spell!\r\n");
    return 0;
  }

  make_scroll(ch, scroll, paper);
  improve_skill(ch, SKILL_SCRIBE);
  return scroll;
}


ASKILL(skill_tinker)
{
  /* PLEASE NOTE!!!  This command alters the object_values of the target
   * weapon, and this will save to the rent files.  It should not cause
   * a problem with stock Circle, but if your weapons use the first 
   * position [ GET_OBJ_VAL(weapon, 0); ], then you WILL have a problem.
   * This command stores the character's level in the first value to 
   * prevent the weapon from being "forged" more than once by mortals.
   * Install at your own risk.  You have been warned...
   */
  struct obj_data *weapon = NULL;
  struct obj_data *next_obj;
  char weapon_name[MAX_STRING_LENGTH];
  int found = FALSE, prob = 0, dam = 0, time;

  one_argument(argument, weapon_name);

  if (!knows_spell(ch, SKILL_TINKER))
  {
    send_to_char("You are not schooled enough to tinker anything!\r\n",
                 ch);
    return 0;
  }
  if (!*weapon_name)
  {
    send_to_char("What do you wish to tinker on?\r\n", ch);
    return 0;
  }


  for (obj = ch->carrying; obj; obj = next_obj)
  {
    next_obj = obj->next_content;
    if (obj == NULL)
      return 0;
    else if (!(weapon = get_obj_in_list_vis(ch, weapon_name, NULL,
                                            ch->carrying)))
      continue;
    else
      found = TRUE;
  }

  if (found == FALSE)
  {
    new_send_to_char(ch, "You don't have %s in your inventory!\r\n",
                     weapon_name);
    return 0;
  }

  if (found && (GET_OBJ_TYPE(weapon) != ITEM_WEAPON))
  {
    new_send_to_char(ch, "It doesn't look like %s would make a"
                     " good weapon...\r\n", weapon_name);
    return 0;
  }

  if (IS_SET_AR(GET_OBJ_EXTRA(weapon), ITEM_TINKERED) || IS_SET_AR(GET_OBJ_EXTRA(weapon), ITEM_MAGIC))
  {
    new_send_to_char
    (ch,"The weapon is imbued with magical powers beyond your grasp.\r\n"
     "You can not further affect its form.\r\n");
    return 0;
  }

  if (IS_SET_AR(GET_OBJ_EXTRA(weapon), ITEM_POISONED_1) ||
      IS_SET_AR(GET_OBJ_EXTRA(weapon), ITEM_POISONED_2) ||
      IS_SET_AR(GET_OBJ_EXTRA(weapon), ITEM_POISONED_3) ||
      IS_SET_AR(GET_OBJ_EXTRA(weapon), ITEM_POISONED_4))
  {
    new_send_to_char(ch,"The weapon is poisoned.\r\n You cannot further affect its form.\r\n");
    return 0;
  }

  /* determine success probability */
  prob += (GET_LEVEL(ch) << 1) + ((GET_DEX(ch) - 11) << 1);
  prob += ((GET_STR(ch) - 11) << 1) + (GET_ADD(ch) >> 3);
  prob /= 2;			// with 50 levels, tinker always works

  if ((number(10, 100) > prob) && (GET_LEVEL(ch) < LVL_HERO))
  {
    new_send_to_char(ch, "As you pound out the dents in the weapon,"
                     " you hit a weak spot and it explodes!\r\n");
    new_send_to_char(ch, "Hot broken shards go in your eyes!\r\n");
    act("$n tries to forge a weapon, but it explodes!",
        FALSE, ch, 0, 0, TO_ROOM);
    extract_obj(weapon);
    dam = number(20, 60) * 3;
    damage(ch, ch, dam, TYPE_UNDEFINED);
    return 0;
  }
  time = 1000;
  if (IS_SET_AR(GET_OBJ_EXTRA(weapon), ITEM_BLESS))
    time *= 2;
  if (IS_SET_AR(GET_OBJ_EXTRA(weapon), ITEM_HUM))
    time *= 2;
  if (IS_SET_AR(GET_OBJ_EXTRA(weapon), ITEM_GLOW))
    time *= 2;

  GET_OBJ_VAL(weapon, 1) += number(-1, (GET_LEVEL(ch)/23)+(GET_DEX(ch)>17)+(TIERNUM-2)+1);
  GET_OBJ_VAL(weapon, 2) += number(-1, (GET_LEVEL(ch)/19)+(GET_DEX(ch)>16)+(TIERNUM-2)+1);
  GET_OBJ_RENT(weapon) += (GET_LEVEL(ch) << 3);
  GET_OBJ_TIMER(weapon) = time;	// dont want tinkered weapons lasting
  // forever
  SET_BIT_AR(GET_OBJ_EXTRA(weapon), ITEM_MAGIC);
  SET_BIT_AR(GET_OBJ_EXTRA(weapon), ITEM_TINKERED);

  send_to_char("You have forged new life into the weapon!\r\n", ch);
  act("$n vigorously pounds on a weapon!", FALSE, ch, 0, 0, TO_ROOM);
  return SKILL_TINKER;
}

struct obj_data *make_tree(void)
{

  struct obj_data *final_tree;

  int num = 0, age = 0;

  if (tree_total > TREE_MAX)
    return (NULL);

  num = number(0, MAX_TREE_TYPES - 1);
  final_tree = create_obj();

  final_tree->item_number = NOTHING;
  final_tree->in_room = NULL;

  GET_OBJ_TYPE(final_tree) = ITEM_TREE;
  SET_BIT_AR(GET_OBJ_EXTRA(final_tree), ITEM_GLOW);
  //SET_BIT_AR(GET_OBJ_EXTRA(final_tree), ITEM_UNIQUE_SAVE);
  GET_OBJ_VAL(final_tree, 0) = (int) time(0);
  GET_OBJ_VAL(final_tree, 1) = age;
  GET_OBJ_VAL(final_tree, 2) = num;
  GET_OBJ_VAL(final_tree, 3) = 0;
  GET_OBJ_COST(final_tree) = 500;
  GET_OBJ_WEIGHT(final_tree) = 1;
  GET_OBJ_RENT(final_tree) = 0;
  GET_OBJ_TIMER(final_tree) = -1;
  parse_tree_name(final_tree);

  return (final_tree);
}


void parse_tree_name(struct obj_data *tree)
{
  struct extra_descr_data *new_descr = NULL;
  char buf2[MAX_INPUT_LENGTH];

  free_string(tree->name);
  free_string(tree->description);
  free_string(tree->short_description);
  free_string(tree->smell);
  free_string(tree->feel);

  tree->smell = strdup("It smells good!\r\n");
  tree->feel = strdup("It feels alive.\r\n");

  if (tree->ex_description)
    free_ex_descriptions(tree->ex_description);

  snprintf(buf2, sizeof(buf2), "%s %s tree magictree",
           age_desc_tree[GET_OBJ_VAL(tree, 1)],
           tree_names[IRANGE(0, GET_OBJ_VAL(tree, 2), 8)]);
  tree->name = str_dup(buf2);


  snprintf(buf2, sizeof(buf2), "%s %s %s tree grows here.",
           CANA(age_desc_tree[GET_OBJ_VAL(tree, 1)]),
           age_desc_tree[GET_OBJ_VAL(tree, 1)],
           tree_names[IRANGE(0, GET_OBJ_VAL(tree, 2), 8)]);
  tree->description = str_dup(buf2);

  snprintf(buf2, sizeof(buf2), "%s %s %s tree",
           LANA(age_desc_tree[GET_OBJ_VAL(tree, 1)]),
           age_desc_tree[GET_OBJ_VAL(tree, 1)],
           tree_names[IRANGE(0, GET_OBJ_VAL(tree, 2), 8)]);
  tree->short_description = str_dup(buf2);

  /* extra description coolness! */
  CREATE(new_descr, struct extra_descr_data, 1);
  new_descr->keyword = str_dup(buf2);
  snprintf(buf2, sizeof(buf2), "It appears to be %s %s %s tree.",
           LANA(age_desc_tree[GET_OBJ_VAL(tree, 1)]),
           age_desc_tree[GET_OBJ_VAL(tree, 1)],
           tree_names[IRANGE(0, GET_OBJ_VAL(tree, 2), 8)]);
  new_descr->description = str_dup(buf2);
  new_descr->next = NULL;
  tree->ex_description = new_descr;
}

room_rnum find_forest_rand(void)
{

  int i = 0, r = number(0, forest_room);
  struct forest_data *temp;

  if (forest == NULL)
  {
    log("ERROR: forest rooms not initialized");
    return NULL;
  }

  temp = forest;
  while (i < r)
  {
    i++;
    if (i == r)
      break;
    temp = temp->next;
  }
  return (temp->room);
}

ACMD(forest_find)
{
  OBJ_DATA *tree, *next_tree;
  skip_spaces(&argument);
  if (!*argument)
  {
    room_rnum ffr = find_forest_rand();
    if (ffr)
      new_send_to_char(ch, "A forest room is [%d]\r\n", ffr->number);
  }
  else if (!strcmp(argument, "clear"))
  {
    for (tree = object_list; tree; tree = next_tree)
    {
      next_tree = tree->next;
      if ((GET_OBJ_TYPE(tree) == ITEM_TREE) && (GET_OBJ_VNUM(tree) == NOTHING))
        extract_obj(tree);
    }
    save_forest();
    new_send_to_char(ch, "Cleared and saved.\r\n");
  }
  else
  {
    new_send_to_char(ch, "Either forest, or forest clear\r\n");
  }
}

void make_focus(struct char_data *ch, int type, struct obj_data *o)
{
  struct obj_data *final_focus;
  struct extra_descr_data *new_descr;
  int can_make = TRUE, num2 = number(0, 16);
  int v0, v1, v2, v3;
  char *msg, *msgroom, msgbuf[MAX_STRING_LENGTH],
  msgroombuf[MAX_STRING_LENGTH];
  char buf2[MAX_INPUT_LENGTH];

  msg = msgbuf;
  msgroom = msgroombuf;


  if (can_make == FALSE)
  {
    new_send_to_char(ch, "That item cannot be made into a focus.\r\n");
    return;
  }

  /*new_send_to_char(ch, "You sing %s %s %s %s focus staff from %s.\r\n",
     LANA(age_desc_staff[GET_OBJ_VAL(o, 1)]),
     age_desc_staff[GET_OBJ_VAL(o, 1)], random_desc[num2],
     tree_names[GET_OBJ_VAL(o, 2)], o->short_description);
  act("$n sings a focus staff from $p!", FALSE, ch, o, 0, TO_ROOM);*/
  v0 = GET_OBJ_VAL(o, 0);
  v1 = GET_OBJ_VAL(o, 1);
  v2 = GET_OBJ_VAL(o, 2);
  v3 = GET_OBJ_VAL(o, 3);
  extract_obj(o);
  create_trees();

  final_focus = create_obj();

  final_focus->item_number = NOTHING;
  final_focus->in_room = NULL;
  if (final_focus->name)
    free(final_focus->name);
  snprintf(buf2, sizeof(buf2), " %s %s focus staff", age_desc_staff[v1],
           tree_names[v2]);
  final_focus->name = str_dup(buf2);
  if (final_focus->description)
    free(final_focus->description);
  snprintf(buf2, sizeof(buf2), "%s %s %s %s staff lies here.",
           CANA(age_desc_staff[v1]), age_desc_staff[v1],
           random_desc[num2], tree_names[v2]);
  final_focus->description = str_dup(buf2);
  if (final_focus->short_description)
    free(final_focus->short_description);
  snprintf(buf2, sizeof(buf2), "%s %s %s %s staff",
           LANA(age_desc_staff[v1]), age_desc_staff[v1],
           random_desc[num2], tree_names[v2]);
  final_focus->short_description = str_dup(buf2);

  /* extra description coolness! */
  CREATE(new_descr, struct extra_descr_data, 1);
  new_descr->keyword = str_dup(buf2);
  snprintf(buf2, sizeof(buf2), "It appears to be %s %s %s %s focus staff.",
           LANA(age_desc_staff[v1]), age_desc_staff[v1],
           random_desc[num2], tree_names[v2]);
  new_descr->description = str_dup(buf2);
  new_descr->next = NULL;
  final_focus->ex_description = new_descr;
  if (number(0, 400))
    GET_OBJ_TYPE(final_focus) = ITEM_FOCUS_MINOR;
  else
    GET_OBJ_TYPE(final_focus) = ITEM_FOCUS_MAJOR;
  SET_BIT_AR(GET_OBJ_WEAR(final_focus), ITEM_WEAR_FOCUS);
  SET_BIT_AR(GET_OBJ_WEAR(final_focus), ITEM_WEAR_TAKE);
  SET_BIT_AR(GET_OBJ_EXTRA(final_focus), ITEM_HUM);
  SET_BIT_AR(GET_OBJ_EXTRA(final_focus), ITEM_UNIQUE_SAVE);
  GET_OBJ_VAL(final_focus, 0) = (v1+1);
  GET_OBJ_VAL(final_focus, 1) = v2;
  GET_OBJ_VAL(final_focus, 2) = FOCUS_STAFF;
  GET_OBJ_VAL(final_focus, 3) = GET_LEVEL(ch)*TIER*2000;
  GET_OBJ_COST(final_focus) = GET_LEVEL(ch) * 500;
  GET_OBJ_WEIGHT(final_focus) = 3;
  GET_OBJ_RENT(final_focus) = 0;
  GET_OBJ_TIMER(final_focus) = GET_LEVEL(ch) * 300;

  obj_to_char(final_focus, ch);
  if (type == SKILL_SING_WOOD)
    improve_skill(ch, SKILL_SING_WOOD);

}


ASKILL(skill_sing_wood)
{
  struct obj_data *o = NULL;
  char tree_name[MAX_STRING_LENGTH];
  char *temp1;
  int type, found = FALSE;
  struct message_event_obj *msg = NULL;

  skip_spaces(&argument);
  temp1 = one_argument(argument, tree_name);
  type = SKILL_SING_WOOD;

  /* sanity check */
  if (!tree_name)
  {
    new_send_to_char(ch, "What do you wish to sing to?\r\n");
    return 0;
  }
  if (IS_HERO(ch))
  {
    new_send_to_char(ch, "Sorry, heros can't sing wood.\r\n");
    return 0;
  }
  /*
      for (obj = ch->carrying; obj && (!found); obj = next_obj) {
  	next_obj = obj->next_content;
  	if (obj == NULL)
  	    return;
  	else if (!(o = get_obj_in_list_vis(ch, tree_name,NULL,ch->carrying)))
   
  	    continue;
  	else
  	    found = TRUE;
      }
  */
  if ((o =
         get_obj_in_list_vis(ch, tree_name, 0,IN_ROOM(ch)->contents)) == NULL)
  {
    new_send_to_char(ch, "The tree you need is not here.\r\n");
    return 0;
  }
  else
  {
    found = TRUE;
  }


  if (found != FALSE &&  ((GET_OBJ_TYPE(o) != ITEM_TREE) || GET_OBJ_VNUM(o) != NOTHING))
  {
    new_send_to_char(ch, "You can't see the proper tree!\r\n");
    return 0;
  }
  if (found == FALSE)
  {
    new_send_to_char(ch, "There is no tree here!\r\n");
    return 0;
  }
  if (GET_MESSAGE_EVENT(ch)!=NULL)
  {
    new_send_to_char(ch, "You are in the middle of something else!\r\n");
    return 0;
  }
  if (GET_MSG_RUN(ch))
  {
    new_send_to_char(ch, "You are already working on something else!\r\n");
    return 0;
  }

  new_send_to_char(ch, "You take a deap breath and clear your mind.\r\n");

  GET_MSG_RUN(ch) = 1;

  CREATE(msg, struct message_event_obj, 1);
  msg->ch_id = GET_ID(ch);
  msg->skill = SKILL_SING_WOOD;
  msg->type = THING_SKILL;
  msg->msg_num = 11;
  msg->id = GET_ID(o);

  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, (1 RL_SEC));
  return SKILL_SING_WOOD;


}

ASKILL(skill_manifest)
{
  char buf[512];

  int pos;
  struct message_event_obj *msg = NULL;

  one_argument(argument, buf);


  if (!*buf)
  {
    send_to_char("Usage: manifest <weapon>\r\n", ch);
    return 0;
  }

  if (!(obj = get_obj_vis(ch, buf, NULL)))
  {
    new_send_to_char(ch, "No such object around.\r\n");
    return 0;
  }
  if (IS_HERO(ch))
  {
    new_send_to_char(ch, "Sorry, heros can't manifest.\r\n");
    return 0;
  }


  if (GET_OBJ_TYPE(obj) != ITEM_WEAPON)
  {
    new_send_to_char(ch, "No such weapon around\r\n");
    return 0;
  }

  if (GET_MSG_RUN(ch))
  {
    new_send_to_char(ch, "You are already working on something else!\r\n");
    return 0;
  }


  if (obj->worn_by && obj->worn_by == ch)
  {
    pos = obj->worn_on;
    unequip_char(obj->worn_by, pos);
    obj_to_char(obj, ch);
  }
  else if (obj->carried_by != ch)
  {
    new_send_to_char(ch, "You are not wearing that weapon or don't have it in your inventory.\r\n");
    return 0;
  }
  GET_MSG_RUN(ch) = 1;
  CREATE(msg, struct message_event_obj, 1);
  msg->ch_id = GET_ID(ch);
  msg->skill = SKILL_MANIFEST;
  msg->type = THING_SKILL;
  msg->msg_num = 8;
  msg->id = GET_ID(obj);

  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, 0);
  return SKILL_MANIFEST;
}

void make_manifest(struct char_data *ch,struct obj_data *obj)
{

  char buf[MAX_STRING_LENGTH];
  struct obj_data *final_focus;
  struct extra_descr_data *new_descr;
  int v1 = GET_OBJ_VAL(obj, 1), v2 = GET_OBJ_VAL(obj, 2);
  int counter = 0;
  char buf2[MAX_INPUT_LENGTH];
  final_focus = create_obj();

  final_focus->item_number = NOTHING;
  final_focus->in_room = NULL;

  for (counter = 0; counter < MAX_OBJ_AFFECT; counter++)
    if (obj->affected[counter].modifier)
    {
      final_focus->affected[counter].location = obj->affected[counter].location;
      final_focus->affected[counter].modifier = obj->affected[counter].modifier;
    }

  GET_OBJ_INNATE(final_focus) = GET_OBJ_INNATE(obj);
  snprintf(buf, sizeof(buf),  "%s orb", obj->name);
  if (final_focus->name)
    free(final_focus->name);
  final_focus->name = strdup(buf);

  snprintf(buf, sizeof(buf), "a shimmering orb with %s inside", obj->short_description);
  if (final_focus->description)
    free(final_focus->description);
  final_focus->description = strdup(buf);

  if (final_focus->short_description)
    free(final_focus->short_description);
  final_focus->short_description = strdup(buf);

  /* extra description coolness! */
  CREATE(new_descr, struct extra_descr_data, 1);
  new_descr->keyword = str_dup(final_focus->name);
  snprintf(buf2, sizeof(buf2), "It's a cool to the touch magical glass orb\r\n"
           "It appears to have a tiny %s inside.",obj->short_description);
  new_descr->description = str_dup(buf2);
  new_descr->next = NULL;
  final_focus->ex_description = new_descr;


  if (!IS_SET_AR(GET_OBJ_EXTRA(final_focus), ITEM_UNIQUE_SAVE))
    SET_BIT_AR(GET_OBJ_EXTRA(final_focus), ITEM_UNIQUE_SAVE);

  GET_OBJ_WEIGHT(final_focus) =  12;
  GET_OBJ_TIMER(final_focus) = (420 - GET_LEVEL(ch) * TIER) * 10;

  if (IS_SET_AR(GET_OBJ_WEAR(obj), ITEM_WEAR_WIELD))
  {
    SET_BIT_AR(GET_OBJ_WEAR(final_focus), ITEM_WEAR_TAKE);
    SET_BIT_AR(GET_OBJ_WEAR(final_focus), ITEM_WEAR_FOCUS);
  }
  GET_OBJ_VAL(final_focus, 0) = 1000 + (GET_LEVEL(ch) * TIER * 5) + v1 + v2;
  GET_OBJ_VAL(final_focus, 1) = -1;
  GET_OBJ_VAL(final_focus, 2) = FOCUS_ORB;
  GET_OBJ_VAL(final_focus, 3) = GET_LEVEL(ch)*TIER*1000;
  if (number(0, 1000))
    GET_OBJ_TYPE(final_focus) = ITEM_FOCUS_MINOR;
  else
    GET_OBJ_TYPE(final_focus) = ITEM_FOCUS_MAJOR;

  improve_skill(ch, SKILL_MANIFEST);
  obj_to_char(final_focus, ch);
  extract_obj(obj);
  return;
}

#define THING(name)  \
   name(ch, vict, obj, room, &msg->msg_num)

EVENTFUNC(message_event)
{
  struct message_event_obj *msg = (struct message_event_obj *) event_obj;
  struct char_data *vict = NULL;
  struct obj_data *obj = NULL;
  struct room_data *room = NULL;
  long time = 0;

  long uid = msg->id;
  struct char_data *ch = find_char(msg->ch_id);
  short type = msg->type;
  int skill = msg->skill;



  if (ch == NULL )
  {
    free(event_obj);
    return 0;
  }

  GET_MESSAGE_EVENT(ch) = NULL;

  if (msg->msg_num == 0)
  {
    free(event_obj);
    return 0;
  }

  if (uid == NOBODY)
  {
    vict = ch;
  }
  else if ((room = find_room(uid)))
  {}
  else if ((vict = find_char(uid)))
  {}
  else if ((obj = find_obj(uid)));

  if (type == THING_SKILL)
  {
    switch (skill)
    {
    case SKILL_SING_WOOD:
      time = THING(thing_singwood);
      break;
    case SKILL_MANIFEST:
      time = THING(thing_manifest);
      break;

    case SKILL_PICK_LOCK:
      break;
    default:
      break;
    }
  }
  else if (type == THING_SUB)
  {
    switch (skill)
    {
    case SUB_LUMBERJACK:
      time = THING(thing_lumberjack);
      break;
    case SUB_JUGGLE:
      time = THING(thing_juggle);
      break;
    case SUB_THROTTLE:
      time = THING(thing_throttle);
      break;
    case SUB_TUNNELING:
      time = THING(thing_tunneling);
      break;

    }


  }
  else
  {
    log("SYSERR: unknown type passed to message event");
  }
  if (msg->msg_num == 0 || (--msg->msg_num) <= 0 || (ch && GET_MSG_RUN(ch) == 0))
  {
    if (ch)
      GET_MSG_RUN(ch) = 0;
    free(event_obj);

    if (ch)
    {
      if (type == THING_SUB)
        toggle_sub_status(ch, skill, STATUS_OFF);
      run_task(ch);
    }

    return 0;
  }
  else
    return time;
}

ACMD(do_fell)
{
  struct obj_data *o = NULL;
  char tree_name[MAX_STRING_LENGTH];
  char *temp1;
  int found = FALSE;
  struct message_event_obj *msg = NULL;

  if (GET_SUB(ch, SUB_LUMBERJACK) <= 0)
  {
    new_send_to_char(ch, "You have no idea how to use that command!\r\n");
    return;
  }

  if (IS_HERO(ch))
  {
    new_send_to_char(ch, "Sorry, heros can't fell.\r\n");
    return;
  }

  skip_spaces(&argument);
  temp1 = one_argument(argument, tree_name);

  /* sanity check */
  if (!tree_name)
  {
    new_send_to_char(ch, "What do you wish to fell?\r\n");
    return;
  }

  if ((o =
         get_obj_in_list_vis(ch, tree_name, 0,IN_ROOM(ch)->contents)) == NULL)
  {
    new_send_to_char(ch, "The tree you need is not here.\r\n");
    return;
  }
  else
  {
    found = TRUE;
  }


  if (found != FALSE && (GET_OBJ_TYPE(o) != ITEM_TREE))
  {
    new_send_to_char(ch, "You can't see the proper tree!\r\n");
    return;
  }
  if (found == FALSE)
  {
    new_send_to_char(ch, "There is no tree here!\r\n");
    return;
  }
  if (GET_MSG_RUN(ch) || GET_MESSAGE_EVENT(ch)!=NULL)
  {
    new_send_to_char(ch, "You are in the middle of something else!\r\n");
    return;
  }


  new_send_to_char(ch, "You flex your muscles and swing your axe.\r\n");
  GET_MSG_RUN(ch) = 1;

  CREATE(msg, struct message_event_obj, 1);
  msg->ch_id = GET_ID(ch);
  msg->skill = SUB_LUMBERJACK;
  msg->type = THING_SUB;
  msg->msg_num = 12;
  msg->id = GET_ID(o);

  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, (1 RL_SEC));


}




ASKILL(skill_manipulate)
{
  char arg[MAX_INPUT_LENGTH];
  OBJ_DATA *o = NULL;

  one_argument(argument, arg);
  /* sanity check */
  if (!arg)
  {
    new_send_to_char(ch, "What do you wish to manipulate?\r\n");
    return 0;
  }

  if ((o =
         get_obj_in_list_vis(ch, arg, 0, ch->carrying)) == NULL)
  {
    new_send_to_char(ch, "The weapon must be in your inventory.\r\n");
    return 0;
  }

  if (GET_OBJ_TYPE(o) != ITEM_WEAPON)
  {
    new_send_to_char(ch, "That isnt a weapon!\r\n");
    return 0;
  }

  if (OBJ_FLAGGED(o, ITEM_ENHANCED))
  {
    new_send_to_char(ch, "That item has been enhanced already!\r\n");
    return 0;
  }
  if (IS_SET_AR(GET_OBJ_EXTRA(o), ITEM_MAGIC))
  {
    new_send_to_char(ch, "That item has been changed already!\r\n");
    return 0;
  }
  if (GET_MANA(ch) < 600)
  {
    new_send_to_char(ch, "You don't have enough mana to manipulate!\r\n");
    return 0;
  }
  obj_from_char(o);
  SET_BIT_AR(GET_OBJ_EXTRA(o), ITEM_UNIQUE_SAVE);
  GET_OBJ_WEIGHT(o) += 10;
  obj_to_char(o, ch);
  if (GET_SKILL(ch, SKILL_MANIPULATE) < number(0, 101))
  {
    GET_WEP_BALANCE(o) = number(0, 100);

    new_send_to_char(ch, "Your concentration slips and you throw the balance in randomly!\r\n");
  }
  else
  {
    GET_WEP_BALANCE(o) = (curr_balance(o) + perf_balance(GET_WEP_TYPE(o)))/2;
    new_send_to_char(ch, "You pour all your energy into changing the balance of the weapon by re-weighting it.\r\n");
  }
  alter_mana(ch, GET_MANA(ch)/2);

  return SKILL_MANIPULATE;
}


void load_trees(void)
{}
