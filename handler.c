/* ************************************************************************
*   File: handler.c                                     Part of CircleMUD *
*  Usage: internal funcs: moving and finding chars/objs                   *
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
#include "db.h"
#include "handler.h"
#include "interpreter.h"
#include "spells.h"
#include "dg_scripts.h"
#include "dg_event.h"


/* local vars */
int extractions_pending = 0;
int obj_extractions_pending = 0;
bool LS_REMOVE = 0;

/* external vars */
extern const char *MENU;
struct hunter_data *hunter_list = NULL;

/* local functions */
void affect_modify_ar(struct char_data *ch, byte loc, sbyte mod,
                      int bitv[], bool add);
int apply_ac(struct char_data *ch, int eq_pos);
//void update_object(struct obj_data *obj, int use);
void update_object(struct char_data *ch, struct obj_data *obj, int use);
void update_char_objects(struct char_data *ch);


/* external functions */
void purge_qic(obj_rnum rnum);
void die_link(CHAR_DATA *mob);
void extract_all_in_list(OBJ_DATA *obj);
int delete_pobj_file(char *name);
void stop_fusion(CHAR_DATA *ch);
void remove_corpse_from_list(OBJ_DATA *corpse);
int stop_task(struct char_data *ch) ;
extern struct char_data *ch_selling;
void stop_auction(int type, struct char_data *ch);
void parse_tree_name(struct obj_data *tree);
int save_forest(void);
size_t strlcpy(char *dest, const char *source, size_t totalsize);
int invalid_class(struct char_data *ch, struct obj_data *obj);
int invalid_race(struct char_data *ch, struct obj_data *obj);
void remove_follower(struct char_data *ch);
void clearMemory(struct char_data *ch);
void die_follower(struct char_data *ch);
void set_race(struct char_data *ch, int race);
ACMD(do_return);
void save_corpses(void);
void reset_zone(int zone);
void zap_char(struct char_data *victim);
void clean_events2(void *pointer);
int speed_update(struct char_data *ch);
void halt_fighting(struct char_data *ch);
int OBJ_INNATE;
void extract_linked_mob(CHAR_DATA *mob);
void dismount_char(struct char_data *ch);
int move_link_room(CHAR_DATA *mob, room_rnum room);
void eq_to_room(CHAR_DATA *ch);
void extract_obj_final(struct obj_data *obj);

char *fname(const char *namelist)
{
  static char holder[READ_SIZE];
  register char *point;

  for (point = holder; isalpha(*namelist); namelist++, point++)
    *point = *namelist;

  *point = '\0';

  return (holder);
}

/* allow abbreviations */
#define WHITESPACE " \t"

int isname(const char *str, const char *namelist)
{
  char *newlist;
  char *curtok;

  if (!str || !*str || !namelist || !*namelist)
    return 0;
  if (!strcmp(str, namelist)) /* the easy way */
    return 1;

  newlist = strdup(namelist);	/* make a copy since strtok 'modifies' strings */
  for (curtok = strtok(newlist, WHITESPACE); curtok; curtok = strtok(NULL, WHITESPACE))
    if (curtok && is_abbrev(str, curtok))
    {
      free(newlist);
      return 1;
    }
  free(newlist);
  return 0;
}


/*
int isname(const char *str, const char *namelist)
{
  const char *curname, *curstr;
 
  curname = namelist;
  for (;;) {
    for (curstr = str;; curstr++, curname++) {
      if (!*curstr && !isalpha(*curname))
	return (1);
 
      if (!*curname)
	return (0);
 
      if (!*curstr || *curname == ' ')
	break;
 
      if (LOWER(*curstr) != LOWER(*curname))
	break;
    }
 
 
    for (; isalpha(*curname); curname++);
    if (!*curname)
      return (0);
    curname++;			
  }
}
*/

/* Stock isname().  Leave this here even if you put in a newer  *
 * isname().  Used for OasisOLC.                                */
int is_name(const char *str, const char *namelist)
{
  const char *curname, *curstr;

  if (!*str || !*namelist || !str || !namelist)
    return (0);

  curname = namelist;
  for (;;)
  {
    for (curstr = str;; curstr++, curname++)
    {
      if (!*curstr && !isalpha(*curname))
        return (1);

      if (!*curname)
        return (0);

      if (!*curstr || *curname == ' ')
        break;

      if (LOWER(*curstr) != LOWER(*curname))
        break;
    }

    /* skip to next name */

    for (; isalpha(*curname); curname++);
    if (!*curname)
      return (0);
    curname++;		/* first char of new name */
  }
}



void affect_modify(struct char_data *ch, byte loc, int mod,
                   bitvector_t bitv, bool add)
{

  if (loc < 0)
    return ;
  if (add)
    SET_BIT_AR(AFF_FLAGS(ch), bitv);
  else
  {
    REMOVE_BIT_AR(AFF_FLAGS(ch), bitv);
    mod = -mod;
  }

  switch (loc)
  {
  case APPLY_NONE:
    break;

  case APPLY_STR:
    GET_STR(ch) += mod;
    break;
  case APPLY_DEX:
    GET_DEX(ch) += mod;
    break;
  case APPLY_INT:
    GET_INT(ch) += mod;
    break;
  case APPLY_WIS:
    GET_WIS(ch) += mod;
    break;
  case APPLY_CON:
    GET_CON(ch) += mod;
    break;
  case APPLY_CHA:
    GET_CHA(ch) += mod;
    break;
  case APPLY_MINE_SPEED:
    if (!IS_NPC(ch)) MINE_SPEED(ch) += mod;
    break;
  case APPLY_MINE_BONUS:
    if (!IS_NPC(ch)) MINE_BONUS(ch) += mod;
    break;
  case APPLY_MINE_DAMAGE:
    if (!IS_NPC(ch)) MINE_DAMAGE(ch) += mod;
    break;
  case APPLY_MINE_STEALTH:
    if (!IS_NPC(ch)) MINE_STEALTH(ch) += mod;
    break;

  case APPLY_REGEN_MOVE://class
    GET_REGEN_MOVE(ch) += mod;
    break;

  case APPLY_REGEN_HIT://lev
    GET_REGEN_HIT(ch) += mod;
    break;

  case APPLY_AGE:
    ch->player.time.birth -= (mod * SECS_PER_MUD_YEAR);
    break;

  case APPLY_CHAR_WEIGHT:
    GET_WEIGHT(ch) += mod;
    break;

  case APPLY_CHAR_HEIGHT:
    GET_HEIGHT(ch) += mod;
    break;

  case APPLY_MANA:
    GET_MAX_MANA(ch) += mod;
    break;

  case APPLY_HIT:
    GET_MAX_HIT(ch) += mod;
    break;

  case APPLY_MOVE:
    GET_MAX_MOVE(ch) += mod;
    break;

  case APPLY_REGEN_MANA://gold
    GET_REGEN_MANA(ch) += mod;
    break;

  case APPLY_EXP:
    break;

  case APPLY_AC:
    GET_AC(ch) += mod;
    break;

  case APPLY_HITROLL:
    GET_HITROLL(ch) += mod;
    break;

  case APPLY_DAMROLL:
    GET_DAMROLL(ch) += mod;
    break;

  case APPLY_SAVING_PARA:
    GET_SAVE(ch, SAVING_PARA) += mod;
    break;

  case APPLY_SAVING_ROD:
    GET_SAVE(ch, SAVING_ROD) += mod;
    break;

  case APPLY_SAVING_PETRI:
    GET_SAVE(ch, SAVING_PETRI) += mod;
    break;

  case APPLY_SAVING_BREATH:
    GET_SAVE(ch, SAVING_BREATH) += mod;
    break;

  case APPLY_SAVING_SPELL:
    GET_SAVE(ch, SAVING_SPELL) += mod;
    break;

  case APPLY_SPEED:
    AFF_SPEED(ch) += mod;
    break;

  default:
    log("SYSERR: Unknown apply adjust %d attempt (%s, affect_modify).",
        loc, __FILE__);
    break;

  }				/* switch */
}


void aff_apply_modify(struct char_data *ch, byte loc, int mod, char *msg)
{


  switch (loc)
  {
  case APPLY_NONE:
    break;

  case APPLY_STR:
    GET_STR(ch) += mod;
    break;
  case APPLY_DEX:
    GET_DEX(ch) += mod;
    break;
  case APPLY_INT:
    GET_INT(ch) += mod;
    break;
  case APPLY_WIS:
    GET_WIS(ch) += mod;
    break;
  case APPLY_CON:
    GET_CON(ch) += mod;
    break;
  case APPLY_CHA:
    GET_CHA(ch) += mod;
    break;

  case APPLY_REGEN_MOVE:
    GET_REGEN_MOVE(ch) += mod;
    break;

    /*
     * My personal thoughts on these two would be to set the person to the
     * value of the apply.  That way you won't have to worry about people
     * making +1 level things to be imp (you restrict anything that gives
     * immortal level of course).  It also makes more sense to set someone
     * to a class rather than adding to the class number. -gg
     */

  case APPLY_REGEN_HIT:
    GET_REGEN_HIT(ch) += mod;
    break;

  case APPLY_AGE:
    ch->player.time.birth -= (mod * SECS_PER_MUD_YEAR);
    break;

  case APPLY_CHAR_WEIGHT:
    GET_WEIGHT(ch) += mod;
    break;

  case APPLY_CHAR_HEIGHT:
    GET_HEIGHT(ch) += mod;
    break;

  case APPLY_MANA:
    GET_MAX_MANA(ch) += mod;
    break;

  case APPLY_HIT:
    GET_MAX_HIT(ch) += mod;
    break;

  case APPLY_MOVE:
    GET_MAX_MOVE(ch) += mod;
    break;

  case APPLY_REGEN_MANA:
    GET_REGEN_MANA(ch) += mod;
    break;

  case APPLY_EXP:
    break;

  case APPLY_AC:
    GET_AC(ch) += mod;
    break;

  case APPLY_HITROLL:
    GET_HITROLL(ch) += mod;
    break;

  case APPLY_DAMROLL:
    GET_DAMROLL(ch) += mod;
    break;

  case APPLY_SAVING_PARA:
    GET_SAVE(ch, SAVING_PARA) += mod;
    break;

  case APPLY_SAVING_ROD:
    GET_SAVE(ch, SAVING_ROD) += mod;
    break;

  case APPLY_SAVING_PETRI:
    GET_SAVE(ch, SAVING_PETRI) += mod;
    break;

  case APPLY_SAVING_BREATH:
    GET_SAVE(ch, SAVING_BREATH) += mod;
    break;

  case APPLY_SAVING_SPELL:
    GET_SAVE(ch, SAVING_SPELL) += mod;
    break;

  case APPLY_RACE:
    // GET_RACE(ch) += mod; */
    break;

  case APPLY_SPEED:
    AFF_SPEED(ch) += mod;
    break;

  case APPLY_COOLNESS:
    GET_COOLNESS(ch) += mod;
    break;
  case APPLY_MINE_SPEED:
    if (!IS_NPC(ch)) MINE_SPEED(ch) += mod;
    break;
  case APPLY_MINE_BONUS:
    if (!IS_NPC(ch)) MINE_BONUS(ch) += mod;
    break;
  case APPLY_MINE_DAMAGE:
    if (!IS_NPC(ch)) MINE_DAMAGE(ch) += mod;
    break;
  case APPLY_MINE_STEALTH:
    if (!IS_NPC(ch)) MINE_STEALTH(ch) += mod;
    break;

  default:
    log("SYSERR: Unknown apply adjust %d attempt (%s, affect_modify).",
        loc, __FILE__);
    break;

  }				/* switch */
}

/*
void affect_modify(struct char_data * ch, byte loc, sbyte mod,
		   bitvector_t bitv, bool add)
{
  if (add) 
    SET_BIT_AR(AFF_FLAGS(ch), bitv);
  else {
    REMOVE_BIT_AR(AFF_FLAGS(ch), bitv);
    mod = -mod;
  }
 
  aff_apply_modify(ch, loc, mod, "affect_modify");
}
*/

/* This updates a character by subtracting everything he is affected by */
/* restoring original abilities, and then affecting all again           */
void affect_total(struct char_data *ch)
{
  struct affected_type *af;
  int i, j;

  for (i = 0; i < NUM_WEARS; i++)
  {
    if (GET_EQ(ch, i))
      for (j = 0; j < MAX_OBJ_AFFECT; j++)
        affect_modify_ar(ch, GET_EQ(ch, i)->affected[j].location,
                         GET_EQ(ch, i)->affected[j].modifier,
                         GET_EQ(ch, i)->obj_flags.bitvector,
                         FALSE);
  }



  for (af = ch->affected; af; af = af->next)
    affect_modify(ch, af->location, af->modifier, af->bitvector,
                  FALSE);

  ch->aff_abils = ch->real_abils;

  for (i = 0; i < NUM_WEARS; i++)
  {
    if (GET_EQ(ch, i))
      for (j = 0; j < MAX_OBJ_AFFECT; j++)
        affect_modify_ar(ch, GET_EQ(ch, i)->affected[j].location,
                         GET_EQ(ch, i)->affected[j].modifier,
                         GET_EQ(ch, i)->obj_flags.bitvector, TRUE);
  }




  for (af = ch->affected; af; af = af->next)
    affect_modify(ch, af->location, af->modifier, af->bitvector, TRUE);

  /* Make certain values are between 0..25, not < 0 and not > 25! */

  i = (IS_NPC(ch) || (!IS_NPC(ch) && GET_LEVEL(ch)>= LVL_IMMORT) ? MAX_IMM_BASE : MAX_MORTAL_BASE);

  GET_DEX(ch) = IRANGE(0, GET_DEX(ch), i);
  GET_INT(ch) = IRANGE(0, GET_INT(ch), i);
  GET_WIS(ch) = IRANGE(0, GET_WIS(ch), i);
  GET_CON(ch) = IRANGE(0, GET_CON(ch), i);
  GET_CHA(ch) = IRANGE(0, GET_CHA(ch), 100);
  GET_STR(ch) = MAX(0, GET_STR(ch));


  if (IS_NPC(ch))
  {
    GET_STR(ch) = MIN(GET_STR(ch), i);
  }
  else
  {
    if (GET_STR(ch) > i)
    {
      i = GET_ADD(ch) + ((GET_STR(ch) - i) * 10);
      GET_ADD(ch) = MIN(i, 100);
      GET_STR(ch) = MAX_MORTAL_BASE;
    }
  }


  //check_regen_rates(ch);	/* update regen rates (for age) */

}


void affect_modify_ar(struct char_data *ch, byte loc, sbyte mod,
                      int bitv[], bool add)
{
  int i, j;

  if (add)
  {
    for (i = 0; i < AF_ARRAY_MAX; i++)
      for (j = 0; j < 32; j++)
        if (IS_SET_AR(bitv, (i * 32) + j))
          SET_BIT_AR(AFF_FLAGS(ch), (i * 32) + j);
  }
  else
  {
    for (i = 0; i < AF_ARRAY_MAX; i++)
      for (j = 0; j < 32; j++)
        if (IS_SET_AR(bitv, (i * 32) + j))
          REMOVE_BIT_AR(AFF_FLAGS(ch), (i * 32) + j);
    mod = -mod;
  }

  aff_apply_modify(ch, loc, mod, "affect_modify_ar");
}



/* Insert an affect_type in a char_data structure
   Automatically sets apropriate bits and apply's */
void affect_to_char(struct char_data *ch, struct affected_type *af)
{
  struct affected_type *affected_alloc;

  if (GET_CLASS(ch) > NUM_CLASSES || GET_RACE(ch) > NUM_RACES || (af->expire != -2 && (time_to_sec(af->expire) < 0)))
    return;

  CREATE(affected_alloc, struct affected_type, 1);

  *affected_alloc = *af;
  affected_alloc->next = ch->affected;
  ch->affected = affected_alloc;

  affect_modify(ch, af->location, af->modifier, af->bitvector, TRUE);
  affect_total(ch);
    if (!IS_NPC(ch)) SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);
}



/*
 * Remove an affected_type structure from a char (called when duration
 * reaches zero). Pointer *af must never be NIL!  Frees mem and calls
 * affect_location_apply
 */
void affect_remove(struct char_data *ch, struct affected_type *af)
{
  struct affected_type *temp;

  if (ch->affected == NULL)
  {
    core_dump();
    return;
  }

  affect_modify(ch, af->location, af->modifier, af->bitvector, FALSE);
  REMOVE_FROM_LIST(af, ch->affected, next);
  free(af);
  af = NULL;
  affect_total(ch);

}

void subs_remove(struct char_data *ch, struct sub_list *af)
{
  struct sub_list *temp = NULL;

  if (ch->subs == NULL)
  {
    core_dump();
    return;
  }

  REMOVE_FROM_LIST(af, ch->subs, next);
  free(af);
  af = NULL;

}

void skills_remove(struct char_data *ch, struct skillspell_data *af)
{

  struct skillspell_data *temp = NULL;

  if (ch->skills == NULL)
  {
    core_dump();
    return;
  }

  REMOVE_FROM_LIST(af, ch->skills, next);
  free(af);
  af = NULL;

}



/* Call affect_remove with every spell of spelltype "skill" */
void affect_from_char(struct char_data *ch, int type)
{
  struct affected_type *hjp = NULL, *next = NULL;

  for (hjp = ch->affected; hjp; hjp = next)
  {
    next = hjp->next;
    if (hjp->type == type)
      affect_remove(ch, hjp);
  }
}



/*
 * Return TRUE if a char is affected by a spell (SPELL_XXX),
 * FALSE indicates not affected.
 */
bool affected_by_spell(struct char_data *ch, int type)
{
  struct affected_type *hjp;

  for (hjp = ch->affected; hjp; hjp = hjp->next)
    if (hjp->type == type)
      return (TRUE);

  return (FALSE);
}



void affect_join(struct char_data *ch, struct affected_type *af,
                 bool add_dur, bool avg_dur, bool add_mod, bool avg_mod)
{
  struct affected_type *hjp, *a_next;
  bool found = FALSE;

  for (hjp = ch->affected; !found && hjp; hjp = a_next)
  {
    a_next = hjp->next;

    if ((hjp->type == af->type) && (hjp->location == af->location))
    {
      if (add_dur && af->expire != -2)
        af->expire += time_to_sec(hjp->expire);
      if (avg_dur && af->expire != -2)
        af->expire -= time_to_sec(af->expire)/2;

      if (add_mod)
        af->modifier += hjp->modifier;
      if (avg_mod)
        af->modifier /= 2;

      affect_remove(ch, hjp);
      affect_to_char(ch, af);
      found = TRUE;
    }
  }
  if (!found)
    affect_to_char(ch, af);
}

int move_char_to(struct char_data *ch, room_rnum room)
{
  room_rnum cur;
  if (!ch)
    return 0;

  if (room == NULL)
    return 0;

  cur = IN_ROOM(ch);

  if (cur != NULL)
    char_from_room(ch);

  if (cur == IN_ROOM(ch))
    return 0;


  char_to_room(ch, room);

  if (IN_ROOM(ch) == NULL)
  {
    char_to_room(ch, cur);
    IN_ROOM(ch) = cur;
    return 0;
  }
  else if (IN_ROOM(ch) == cur)
  {
    return 0;
  }

  return 1;



}


/* move a player out of a room */
void char_from_room(struct char_data *ch)
{
  struct char_data *temp;


  if (ch == NULL)
  {
    log("SYSERR: NULL character in %s, char_from_room",   __FILE__);
    ALERT_1;
    exit(1);
  }
  else if (IN_ROOM(ch) == NULL)
  {
    return;
  }

  char_from_chair(ch);

  if (FIGHTING(ch) != NULL)
    halt_fighting(ch);
  else if (GET_FIGHT_EVENT(ch) != NULL)
    stop_fighting(ch);


  if (GET_EQ(ch, WEAR_LIGHT) != NULL)
    if (GET_OBJ_TYPE(GET_EQ(ch, WEAR_LIGHT)) == ITEM_LIGHT)
      if (GET_OBJ_VAL(GET_EQ(ch, WEAR_LIGHT), 2))	/* Light is ON */
        IN_ROOM(ch)->light--;

  REMOVE_FROM_LIST(ch, IN_ROOM(ch)->people, next_in_room);
  IN_ROOM(ch) = NULL;
  ch->next_in_room = NULL;
}


/* place a character in a room */
void char_to_room(struct char_data *ch, room_rnum room)
{

  if (IN_ROOM(ch) != NULL)
  {
    log("char_to_room called when current room doesnt equal null");
    char_from_room(ch);
  }

  if (IS_NPC(ch))
    move_link_room(ch, room);

  if (ch == NULL || room == NULL || room->number < 0 || room->number > top_of_world)
  {
    log("SYSERR: Illegal value(s) passed to char_to_room. (Room: %d/%d Ch: %p)", room != NULL ? room->number : -1, top_of_world, ch);
    //send_to_all("{cRALERT: Stability issue!\r\n");
    room = real_room(3001);
  }
  if (room == NULL)
  {
    log("SYSERR: Illegal value(s) passed to char_to_room. Ch: %p)", ch);
    exit(0);
  }
  ch->next_in_room = room->people;
  room->people = ch;
  IN_ROOM(ch) = room;

  if (GET_EQ(ch, WEAR_LIGHT))
    if (GET_OBJ_TYPE(GET_EQ(ch, WEAR_LIGHT)) == ITEM_LIGHT)
      if (GET_OBJ_VAL(GET_EQ(ch, WEAR_LIGHT), 2))	/* Light ON */
        room->light++;

  /* Stop fighting now, if we left. */
  if (FIGHTING(ch) && IN_ROOM(ch) != IN_ROOM(FIGHTING(ch)))
  {
    stop_fighting(FIGHTING(ch));
    stop_fighting(ch);
  }

}


/* give an object to a char   */
void obj_to_char(struct obj_data *object, struct char_data *ch)
{


  if (object && ch)
  {
    object->next_content = ch->carrying;
    ch->carrying = object;
    object->carried_by = ch;
    IN_ROOM(object) = NULL;
    IS_CARRYING_W(ch) += GET_OBJ_WEIGHT(object);
    IS_CARRYING_N(ch)++;

    /* set flag for crash-save system, but not on mobs! */
    if (!IS_NPC(ch))
      SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);

    if (GET_OBJ_RNUM(object) != NOTHING && obj_index[GET_OBJ_RNUM(object)].qic)
    {
      if (!IS_NPC(ch) && object->owner == 0 && GET_LEVEL(ch) < LVL_HERO)
      {
        new_send_to_char(ch, "%s binds itself to you.\r\n", object->short_description);
        object->owner = GET_IDNUM(ch);
      }
      new_mudlog(CMP, MAX(LVL_SEN, GET_INVIS_LEV(ch)), TRUE, "%s to character %s (Owner: %s)",
                 object->short_description, GET_NAME(ch), object->owner == 0 ? "nobody" : get_name_by_id(object->owner));

    }


  }
  else
    log("SYSERR: NULL or dead obj (%p) or char (%p) passed to obj_to_char.",
        object, ch);
}


/* give an object to a char -- don't add the weight back */
void obj_to_char_no_weight(struct obj_data *object, struct char_data *ch)
{
  if (object && ch)
  {
    object->next_content = ch->carrying;
    ch->carrying = object;
    object->carried_by = ch;
    IN_ROOM(object) = NULL;
    IS_CARRYING_N(ch)++;

    /* set flag for crash-save system, but not on mobs! */
    if (!IS_NPC(ch))
      SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);
  }
  else
    log("SYSERR: NULL or dead obj (%p) or char (%p) passed to obj_to_char.",
        object, ch);
}

/* take an object from a char */
int obj_from_char(struct obj_data *object)
{
  struct obj_data *temp;

  if (object == NULL)
  {
    log("SYSERR: NULL object passed to obj_from_char.");
    return 0;
  }

  if (object->carried_by == NULL)
  {
    log("SYSERR: Trying to remove obj from char that isn't held.");
    return 0;
  }

  REMOVE_FROM_LIST(object, object->carried_by->carrying, next_content);

  /* set flag for crash-save system, but not on mobs! */
  if (!IS_NPC(object->carried_by))
    SET_BIT_AR(PLR_FLAGS(object->carried_by), PLR_CRASH);

  IS_CARRYING_W(object->carried_by) -= GET_OBJ_WEIGHT(object);
  IS_CARRYING_N(object->carried_by)--;
  object->carried_by = NULL;
  object->next_content = NULL;
  return 1;
}



/* Return the effect of a piece of armor in position eq_pos */
int apply_ac(struct char_data *ch, int eq_pos)
{
  int factor;

  if (GET_EQ(ch, eq_pos) == NULL)
  {
    return (0);
  }

  if (!(GET_OBJ_TYPE(GET_EQ(ch, eq_pos)) == ITEM_ARMOR))
  {
    if (IS_OBJ_STAT(GET_EQ(ch, eq_pos), ITEM_NODROP))
      return -5;
    else
      return (0);
  }

  switch (eq_pos)
  {
  case WEAR_SHIELD:
    factor = 4;
    break;
  case WEAR_BODY:
    factor = 3;
    break;			/* 30% */
  case WEAR_HEAD:
  case WEAR_LEGS:
    factor = 2;
    break;			/* 20% */
  default:
    factor = 1;
    break;			/* all others 10% */
  }

  if (IS_OBJ_STAT(GET_EQ(ch, eq_pos), ITEM_NODROP))
    return -(abs(factor * GET_OBJ_VAL(GET_EQ(ch, eq_pos), 0)));
  else
    return (factor * GET_OBJ_VAL(GET_EQ(ch, eq_pos), 0));
}

int invalid_align(struct char_data *ch, struct obj_data *obj)
{
  if (IS_OBJ_STAT(obj, ITEM_ANTI_EVIL) && IS_EVIL(ch))
    return TRUE;
  if (IS_OBJ_STAT(obj, ITEM_ANTI_GOOD) && IS_GOOD(ch))
    return TRUE;
  if (IS_OBJ_STAT(obj, ITEM_ANTI_NEUTRAL) && IS_NEUTRAL(ch))
    return TRUE;
  return FALSE;
}

int equip_char(struct char_data *ch, struct obj_data *obj, int pos)
{
  int j;
  struct affected_type *aff;

  if (pos < 0 || pos >= NUM_WEARS)
  {
    core_dump();
    return 0;
  }

  if (GET_EQ(ch, pos))
  {
    log("SYSERR: Char is already equipped: %s, %s", GET_NAME(ch),
        obj->short_description);
    return 0;
  }
  if (obj->carried_by)
  {
    log("SYSERR: EQUIP: Obj is carried_by when equip.");
    return 0;
  }
  if (IN_ROOM(obj) != NULL)
  {
    log("SYSERR: EQUIP: Obj is in_room when equip.");
    return 0;
  }

  if (invalid_align(ch, obj) || invalid_class(ch, obj)
      || invalid_race(ch, obj))
  {
    act("You are zapped by $p and instantly let go of it.", FALSE, ch, obj, 0, TO_CHAR);
    act("$n is zapped by $p and instantly lets go of it.", FALSE, ch, obj, 0, TO_ROOM);
    obj_to_char(obj, ch);	// changed to drop in inventory instead of
    zap_char(ch);		// ground
    return 0;
  }

  IS_CARRYING_W(ch) += GET_OBJ_WEIGHT(obj);
  GET_EQ(ch, pos) = obj;
  obj->worn_by = ch;
  obj->worn_on = pos;

  if (GET_OBJ_TYPE(obj) == ITEM_ARMOR)
    GET_AC(ch) -= apply_ac(ch, pos);

  if (IN_ROOM(ch) != NULL)
  {
    if (pos == WEAR_LIGHT && GET_OBJ_TYPE(obj) == ITEM_LIGHT)
      if (GET_OBJ_VAL(obj, 2))	/* if light is ON */
        IN_ROOM(ch)->light++;
  }
  else if (ch->loader == NOBODY)
    log("SYSERR: IN_ROOM(ch) = NOWHERE when equipping char %s.",
        GET_NAME(ch));

  for (j = 0; j < MAX_OBJ_AFFECT; j++)
    affect_modify_ar(ch, obj->affected[j].location,
                     obj->affected[j].modifier,
                     obj->obj_flags.bitvector, TRUE);

  if (obj->obj_flags.obj_innate)
  {
    for (aff = ch->affected; aff; aff = aff->next)	/* run through list */
      if (aff->type == obj->obj_flags.obj_innate)	/* if already affected */
        continue;
    OBJ_INNATE = TRUE;
    mag_affects(90, NULL, ch, obj->obj_flags.obj_innate, NOBODY);
    OBJ_INNATE = FALSE;
  }


  affect_total(ch);
  return 1;

}

struct obj_data *unequip_char(struct char_data *ch, int pos)
{
  int j;
  struct obj_data *obj = NULL;
  struct affected_type *aff = NULL, *anext;

  if (ch == NULL)
    log("Null ch passed to unequip_char");


  if ((pos < 0 || pos >= NUM_WEARS))
  {
    log("Pos %d outside position range of %d - %d passed to unequip_char", pos, 0, NUM_WEARS);
    core_dump();
    return (NULL);
  }
  if (GET_EQ(ch, pos) == NULL)
  {
    log("unequip_char asked to remove a position already unequipped!");
    return NULL;
  }
  obj = GET_EQ(ch, pos);


  obj->worn_by = NULL;
  obj->worn_on = -1;


  if (GET_OBJ_TYPE(obj) == ITEM_ARMOR)
    GET_AC(ch) += apply_ac(ch, pos);
  GET_EQ(ch, pos) = NULL;

  if (IN_ROOM(ch) != NULL)
  {
    if (pos == WEAR_LIGHT && GET_OBJ_TYPE(obj) == ITEM_LIGHT)
      if (GET_OBJ_VAL(obj, 2))	/* if light is ON */
        IN_ROOM(ch)->light--;
  }
  else
    log("SYSERR: IN_ROOM(ch) = NOWHERE when unequipping char %s.", GET_NAME(ch));

  IS_CARRYING_W(ch) -= GET_OBJ_WEIGHT(obj);

  for (j = 0; j < MAX_OBJ_AFFECT; j++)
    affect_modify_ar(ch, obj->affected[j].location,
                     obj->affected[j].modifier,
                     obj->obj_flags.bitvector, FALSE);

  if (obj->obj_flags.obj_innate > 0)
  {
    for (aff = ch->affected; aff; aff = anext)
    {
      anext = aff->next;
      if (aff->type == obj->obj_flags.obj_innate)
        affect_remove(ch, aff);
    }
  }
  affect_total(ch);

  return (LS_REMOVE ? revert_object(obj) : obj);
}


int get_number(char **name)
{
  int i = 0, plen = 0;
  char *ppos;
  char number[MAX_INPUT_LENGTH];


  *number = '\0';

  if (!*name || !**name)
    return 1;

  if ((ppos = strchr(*name, '.')) != NULL)
  {
    *ppos++ = '\0';
    strlcpy(number, *name, sizeof(number));
    //strcpy(*name, ppos);	/* strcpy: OK (always smaller) */
    memmove(*name, ppos, (plen = strlen(ppos)));
    *((*name + (plen))) = '\0';

    for (i = 0; *(number + i); i++)
      if (!isdigit(*(number + i)))
        return (0);

    return (atoi(number));
  }
  return (1);
}



/* Search a given list for an object number, and return a ptr to that obj */
struct obj_data *get_obj_in_list_num(int num, struct obj_data *list)
{
  struct obj_data *i;

  for (i = list; i; i = i->next_content)
    if (GET_OBJ_RNUM(i) == num)
      return (i);

  return (NULL);
}



/* search the entire world for an object number, and return a pointer  */
struct obj_data *get_obj_num(obj_rnum nr)
{
  struct obj_data *i;

  for (i = object_list; i; i = i->next)
    if (GET_OBJ_RNUM(i) == nr)
      return (i);

  return (NULL);
}



/* search a room for a char, and return a pointer if found..  */
struct char_data *get_char_room(char *name, int *number, room_rnum room)
{
  struct char_data *i;
  int num;

  if (!name || !*name)
    return NULL;

  if (!number)
  {
    number = &num;
    num = get_number(&name);
  }

  if (*number == 0 || !room)
    return (NULL);

  for (i = room->people; i && *number; i = i->next_in_room)
    if (isname(name, i->player.name))
      if (--(*number) == 0)
        return (i);

  return (NULL);
}




/* search all over the world for a char num, and return a pointer if found */
struct char_data *get_char_num(mob_rnum nr)
{
  struct char_data *i;

  for (i = character_list; i; i = i->next)
    if (GET_MOB_RNUM(i) == nr)
      return (i);

  return (NULL);
}



/* put an object in a room */
void obj_to_room(struct obj_data *object, room_rnum room)
{
  if (!object || !room || room->number < 0 || room->number > top_of_world )
    log("SYSERR: Illegal value(s) passed to obj_to_room.\r\n (Room #%d/%d, obj %p)", room ? room->number : -1, top_of_world, object);
  else
  {
    object->next_content = room->contents;
    room->contents = object;
    IN_ROOM(object) = room;
    object->carried_by = NULL;
    object->worn_by = NULL;
    if (ROOM_FLAGGED(room, ROOM_HOUSE))
      SET_BIT_AR(ROOM_FLAGS(room), ROOM_HOUSE_CRASH);
    if (GET_OBJ_RNUM(object) != NOTHING && obj_index[(int) GET_OBJ_RNUM(object)].qic)
    {
      new_mudlog( CMP, LVL_SEN, TRUE, "%s, vnum: %d, to room %s.",
                  object->short_description, GET_OBJ_VNUM(object),
                  room->name);
    }
  }
}


/* Take an object from a room */
void obj_from_room(struct obj_data *object)
{
  struct obj_data *temp = NULL;

  if (!object || IN_ROOM(object) == NULL)
  {
    log("SYSERR: NULL object (%p) or obj not in a room (%p) passed to obj_from_room", object, IN_ROOM(object));
    return;
  }

  if (IS_OBJ_STAT(object, ITEM_PC_CORPSE))
  {
    remove_corpse_from_list(object);
    REMOVE_BIT_AR(GET_OBJ_EXTRA(object), ITEM_PC_CORPSE);
    SET_BIT_AR(GET_OBJ_EXTRA(object), ITEM_NPC_CORPSE);
    save_corpses();
  }

  REMOVE_FROM_LIST(object, IN_ROOM(object)->contents,
                   next_content);

  if (ROOM_FLAGGED(IN_ROOM(object), ROOM_HOUSE))
    SET_BIT_AR(ROOM_FLAGS(IN_ROOM(object)), ROOM_HOUSE_CRASH);

  /*can remove this section to clean up the code*/
  if (GET_OBJ_RNUM(object) != NOTHING && obj_index[GET_OBJ_RNUM(object)].qic)
    new_mudlog(CMP, LVL_SEN, TRUE, "%s from room %s.", object->short_description, IN_ROOM(object)->name);



  IN_ROOM(object) = NULL;
  object->next_content = NULL;
}


/* put an object in an object (quaint)  */
void obj_to_obj(struct obj_data *obj, struct obj_data *obj_to)
{
  struct obj_data *tmp_obj = NULL;

  if (!obj || !obj_to || obj == obj_to)
  {
    log("SYSERR: NULL object (%s)(%p) or same source (%p) and target (%p) obj passed to obj_to_obj.", obj->short_description, obj, obj, obj_to);
    return;
  }
  if (IN_ROOM(obj) != NULL && ROOM_FLAGGED(IN_ROOM(obj), ROOM_HOUSE))
    SET_BIT_AR(ROOM_FLAGS(IN_ROOM(obj)), ROOM_HOUSE_CRASH);

  obj->next_content = obj_to->contains;
  obj_to->contains = obj;
  obj->in_obj = obj_to;

  for (tmp_obj = obj->in_obj; tmp_obj->in_obj; tmp_obj = tmp_obj->in_obj)
    GET_OBJ_WEIGHT(tmp_obj) += GET_OBJ_WEIGHT(obj);

  /* top level object.  Subtract weight from inventory if necessary. */
  GET_OBJ_WEIGHT(tmp_obj) += GET_OBJ_WEIGHT(obj);
  if (tmp_obj->carried_by)
    IS_CARRYING_W(tmp_obj->carried_by) += GET_OBJ_WEIGHT(obj);
}


/* remove an object from an object */
int obj_from_obj(struct obj_data *obj)
{
  struct obj_data *temp, *obj_from = NULL;

  if (!obj || !obj->in_obj)
  {
    log("SYSERR: (%s): trying to illegally extract obj from obj.", __FILE__);
    core_dump();
    return 0;
  }

  obj_from = obj->in_obj;
  REMOVE_FROM_LIST(obj, obj_from->contains, next_content);

  /* Subtract weight from containers container */
  for (temp = obj->in_obj; temp->in_obj; temp = temp->in_obj)
    GET_OBJ_WEIGHT(temp) -= GET_OBJ_WEIGHT(obj);

  /* Subtract weight from char that carries the object */
  GET_OBJ_WEIGHT(temp) -= GET_OBJ_WEIGHT(obj);
  if (temp->carried_by)
    IS_CARRYING_W(temp->carried_by) -= GET_OBJ_WEIGHT(obj);

  if (IN_ROOM(obj) != NULL && ROOM_FLAGGED(IN_ROOM(obj), ROOM_HOUSE))
    SET_BIT_AR(ROOM_FLAGS(IN_ROOM(obj)), ROOM_HOUSE_CRASH);

  obj->in_obj = NULL;
  obj->next_content = NULL;
  return 1;
}


/* Set all carried_by to point to new owner */
void object_list_new_owner(struct obj_data *list, struct char_data *ch)
{
  if (list)
  {
    object_list_new_owner(list->contains, ch);
    object_list_new_owner(list->next_content, ch);
    list->carried_by = ch;
  }
}
#if 0
void extract_obj(struct obj_data *obj)
{
  struct obj_data *tobj, *next;
  struct char_data *tch;


  if (!obj)
    return;
  if (OBJ_FLAGGED(obj, ITEM_NOTDEADYET))
    return;

  SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_NOTDEADYET);
  obj_extractions_pending++;

  if (GET_OBJ_RNUM(obj) != NOTHING)
    (obj_index[GET_OBJ_RNUM(obj)].number)--;


  /* Get rid of the contents of the object, as well. */
  for (tobj = obj->contains; tobj; tobj = next)
  {
    next = tobj->next_content;
    obj_from_obj(tobj);
    extract_obj(tobj);
    tobj->in_obj = NULL;
  }

}

void extract_pending_objects(void)
{
  struct obj_data *obj, *next_obj, *prev_obj;
  if (obj_extractions_pending < 0)
    log("SYSERR: Negative (%d) object extractions pending.", extractions_pending);

  for (obj = object_list, prev_obj = NULL; obj && obj_extractions_pending ; obj = next_obj)
  {
    next_obj = obj->next;
    if (OBJ_FLAGGED(obj, ITEM_NOTDEADYET))
      REMOVE_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_NOTDEADYET);
    else
    {
      prev_obj = obj;
      continue;
    }

    extract_obj_final(obj);
    obj_extractions_pending--;
    if (prev_obj)
      prev_obj->next = next_obj;
    else
      object_list = next_obj;
  }
  if (obj_extractions_pending > 0)
    log("SYSERR: Couldn't find %d object extractions as counted.",
        obj_extractions_pending);

  obj_extractions_pending = 0;
}
void extract_object_final(OBJ_DATA *obj)
#else
/* Extract an object from the world */
void extract_obj(struct obj_data *obj)
#endif
{
  struct obj_data *temp;
  struct char_data *ch, *next = NULL;
  struct obj_data *tobj, *onext, *tnext;
  struct char_data *tch;
  int chance = 20;
  room_rnum room = NULL, from, target;

  if (!obj)
  {
    log("Null pointer given to extract object.");
    return;
  }
  if (GET_ID(obj) == 0) {
  log("extracting object that hasn't been initilised");
  }
  if (IS_OBJ_STAT(obj, ITEM_PC_CORPSE))
  {

    save_corpses();
  }

  /* Normal extract_obj code */
  if (obj->worn_by != NULL)
    if (unequip_char(obj->worn_by, obj->worn_on) != obj)
    {
      log("SYSERR: Inconsistent worn_by and worn_on pointers!!");
      return;
    }
  if ((room = IN_ROOM(obj)) != NULL)
    obj_from_room(obj);
  else if (obj->carried_by)
  {
    obj_from_char(obj);
  }
  else if (obj->in_obj)
    obj_from_obj(obj);

  if (GET_OBJ_RNUM(obj) != NOTHING)
    (obj_index[GET_OBJ_RNUM(obj)].number)--;


  /* Get rid of the contents of the object, as well. */
  for (tobj = obj->contains; tobj; tobj = onext)
  {
    onext = tobj->next_content;
    //obj_from_obj(tobj);
    extract_obj(tobj);
    //tobj->in_obj = NULL;
  }

  /* Purge a vehicle and leave the contents in the room -- Kalten */
  if (GET_OBJ_TYPE(obj) == ITEM_VEHICLE && room != NULL)
  {
    if (room->number != NOWHERE)
    {
      if (GET_OBJ_VAL(obj, 0) == 0)
      {
        new_mudlog(NRM, LVL_SEN, FALSE, "Vehicle %d has no room set.", GET_OBJ_VNUM(obj));

      }
      else
      {
        target = room;	// the room the vehicle is in
        from = real_room(GET_OBJ_VAL(obj, 0));	// the room where the mobs/objs are in

        // First, get all the people out of the room
        if (from != NULL)
        {
          while (from->people != NULL)
          {
            tch = from->people;
            if (tch != NULL)
            {
              if (damage(tch, tch, dice(10, 20), TYPE_UNDEFINED) >= 0)
              {
                move_char_to(tch, target);
              }
            }
          }


          send_to_room(target, "The ship bursts at the seams from the powerful strike.\r\n");

          // Now get all the objects from the room

          for (tobj = from->contents; tobj; tobj = tnext)
          {
            tnext = tobj->next_content;
            obj_from_room(tobj);
            if (GET_OBJ_TYPE(tobj) != ITEM_V_CONTROLS &&
                GET_OBJ_TYPE(tobj) != ITEM_V_HATCH &&
                GET_OBJ_TYPE(tobj) != ITEM_V_WINDOW)
            {
              if (chance < number(1, 100))
              {
                obj_to_room(tobj, target);
              }
              else
              {
                extract_obj(tobj);
              }
            }
          }
        }
      }
    }
  }

  free_travel_points(TRAVEL_LIST(obj));
  TRAVEL_LIST(obj) = NULL;


  for (ch = OBJ_SAT_IN_BY(obj); ch; ch = next)
  {
    next = NEXT_SITTING(ch);
    SITTING(ch) = NULL;
    NEXT_SITTING(ch) = NULL;
  }
  OBJ_SAT_IN_BY(obj) = NULL;

  REMOVE_FROM_LIST(obj, object_list, next);
  obj->next = NULL;
  obj->carried_by = NULL;
  obj->next_content = NULL;

  if (SCRIPT(obj))
    extract_script(obj, OBJ_TRIGGER);

  if (GET_OBJ_RNUM(obj) == NOTHING || obj->proto_script != obj_proto[GET_OBJ_RNUM(obj)].proto_script)
    free_proto_script(obj, OBJ_TRIGGER);


  if (GET_OBJ_RNUM(obj) != NOTHING && obj_index[GET_OBJ_RNUM(obj)].qic)
  {
    new_mudlog(CMP, LVL_SEN, FALSE, "%s purged", obj->short_description);
    purge_qic(GET_OBJ_RNUM(obj));
  }
#if 0
  free_obj(obj);
  obj = NULL;
#else
  free_obj_delayed(obj);
#endif
}

void crumble_obj(struct char_data *ch, struct obj_data *obj)
{
  struct obj_data *loop;
  int index;

  if (IN_ROOM(obj) != NULL)
    if (ROOM_FLAGGED(IN_ROOM(obj), ROOM_HOUSE))
      SET_BIT_AR(IN_ROOM(obj)->room_flags, ROOM_HOUSE_CRASH);

  if (GET_OBJ_TYPE(obj) == ITEM_PORTAL)
  {	/* If it is a portal */
    if (GET_OBJ_VAL(obj, 2) > 0)
      GET_OBJ_VAL(obj, 2)--;
    if (!GET_OBJ_VAL(obj, 2))
    {
      if (obj->carried_by == NULL && IN_ROOM(obj) != NULL)
      {
        act("A glowing portal fades from existence.",
            TRUE, IN_ROOM(obj)->people, obj, 0, TO_ROOM);
        act("A glowing portal fades from existence.",
            TRUE, IN_ROOM(obj)->people, obj, 0, TO_CHAR);
      }
      else if (obj->carried_by)
      {
        obj_from_char(obj);
      }
      extract_obj(obj);

    }
  }
  else if (IN_ROOM(obj) != NULL)
  {	/* In a room */
    if (IN_ROOM(obj)->people)
    {
      act("A quivering horde of maggots consumes $p.",
          TRUE, IN_ROOM(obj)->people, obj, 0, TO_ROOM);
      act("A quivering horde of maggots consumes $p.",
          TRUE, IN_ROOM(obj)->people, obj, 0, TO_CHAR);
    }
    for (loop = obj->contains; loop; loop = obj->contains)
    {
      obj_from_obj(loop);
      obj_to_room(loop, IN_ROOM(obj));
    }

    obj_from_room(obj);

  }
  else if (!obj->in_obj && obj->carried_by)
  {	/* Worn or inventory */

    for (loop = obj->contains; loop; loop = obj->contains)
    {
      obj_from_obj(loop);
      obj_to_char(loop, ch);
    }
    if (!obj->carried_by)
    {	/* Equipped */
      for (index = 0; index < NUM_WEARS; index++)
        if (GET_EQ(ch, index) == obj)
        {
          obj = unequip_char(ch, index);
          act("$p decays in your hands.", FALSE, ch, obj, 0,              TO_CHAR);
          act("$p decays in $n's hands.", FALSE, ch, obj, 0,              TO_ROOM);
        }
    }
    else
    {
      act("$p crumbles into dust.", FALSE, (obj->carried_by), obj, 0, TO_CHAR);
      obj_from_char(obj);
    }
  }
  else if (obj->in_obj)
  {			/* In an object */
    for (loop = obj->contains; loop; loop = obj->contains)
    {
      obj_from_obj(loop);
      obj_to_obj(loop, obj->in_obj);
    }
    obj_from_obj(obj);
  }
  extract_obj(obj);
}
void check_all_trees(void)
{
  //TODO: this, .. i don't think works right
  struct obj_data *obj;
  time_t tm = time(0);
  int ext = 0, add = 0;

  log("TREES: Updating all trees...");

  for (obj = object_list; obj; obj = obj->next)
  {
    /*tree check*/
    if (GET_OBJ_TYPE(obj) == ITEM_TREE && GET_OBJ_VNUM(obj) == NOTHING)
    {
      if ((tm - GET_OBJ_VAL(obj, 0)) > (7 * SECS_PER_REAL_DAY))
      {
        if (GET_OBJ_VAL(obj, 1) <= MAX_TREE_AGE)
        {
          add++;
          GET_OBJ_VAL(obj, 0) = (int) time(0);
          GET_OBJ_VAL(obj, 1)++;
          parse_tree_name(obj);
        }
        else
        {
          ext++;
          if (IN_ROOM(obj) != NULL)
            send_to_room(IN_ROOM(obj), "With a sigh and a whisper %s collapses to the forest floor.\r\n", obj->short_description);
          extract_obj(obj);
        }
      }
      /*tree check*/
    }
  }

  save_forest();
  log("TREES: Update complete. Created: %d Destroyed: %d", add, ext);

}

void update_object(struct char_data *ch, struct obj_data *obj, int use)
{



  if (GET_OBJ_TIMER(obj) == -1)
    return;
  // log("(update_obj.1) obj: %s, timer: %d.", obj->name, GET_OBJ_TIMER(obj));
  /* dont update objects with a timer trigger */

  // log("(update_obj.2) obj: %s, timer: %d.", obj->name, GET_OBJ_TIMER(obj));
  if (obj->next_content)
    update_object(ch, obj->next_content, use);
  // log("(update_obj.3) obj: %s, timer: %d.", obj->name, GET_OBJ_TIMER(obj));
  if (obj->contains)
    update_object(ch, obj->contains, use);
  // log("(update_obj.4) obj: %s, timer: %d.", obj->name, GET_OBJ_TIMER(obj));
  if ((GET_OBJ_TIMER(obj) -= use) < 1)
  {
    if (timer_otrigger(obj) == -1)
      return;

    crumble_obj(ch, obj);
  }


  // log("(update_obj.6) obj: %s, timer: %d", obj->name, GET_OBJ_TIMER(obj));
}


void update_char_objects(struct char_data *ch)
{
  int i;

  if (IN_ROOM(ch) != NULL && GET_EQ(ch, WEAR_LIGHT) != NULL)
    if (GET_OBJ_TYPE(GET_EQ(ch, WEAR_LIGHT)) == ITEM_LIGHT)
      if (GET_OBJ_VAL(GET_EQ(ch, WEAR_LIGHT), 2) > 0)
      {
        i = --GET_OBJ_VAL(GET_EQ(ch, WEAR_LIGHT), 2);
        if (i == 1)
        {
          send_to_char("Your light begins to flicker and fade.\r\n", ch);
          act("$n's light begins to flicker and fade.", FALSE,ch, 0, 0, TO_ROOM);
        }
        else if (i == 0)
        {
          send_to_char("Your light sputters out and dies.\r\n", ch);
          act("$n's light sputters out and dies.", FALSE, ch, 0, 0, TO_ROOM);
          IN_ROOM(ch)->light--;
        }
      }
#if 0
  /* this is just doubling up the full object scan done in point update */
  for (i = 0; i < NUM_WEARS; i++)
    if (GET_EQ(ch, i))
      update_object(ch, GET_EQ(ch, i), 1);

  if (ch->carrying)
    update_object(ch, ch->carrying, 0);
#endif
}

void eq_to_room(CHAR_DATA *ch)
{
  OBJ_DATA *obj;
  int i;
  /* transfer objects to room, if any */
  while (ch->carrying)
  {
    obj = ch->carrying;
    if (obj_from_char(obj) == 0)
      extract_obj(obj);
    if (IN_ROOM(ch) == NULL)
      extract_obj(obj);
    else
      obj_to_room(obj, IN_ROOM(ch));
  }

  /* transfer equipment to room, if any */
  for (i = 0; i < NUM_WEARS; i++)
    if (GET_EQ(ch, i))
    {
      obj = unequip_char(ch, i);
      if (IN_ROOM(ch) == NULL)
        extract_obj(obj);
      else
        obj_to_room(obj, IN_ROOM(ch));
    }
}

void death_room(struct char_data *ch)
{
  struct hunter_data *hunt = NULL, *hnext;
 

  if (IS_NPC(ch))
  {
    extract_char(ch);
    return;
  }

  write_to_output(ch->desc, "{cYAs your last breath passes, time rolls back to just before you died\r\n"
                  "and you find yourself transfered to a temple of healing.{c0\r\n");

  GET_HIT(ch) = 1;

  if (RIDING(ch) || RIDDEN_BY(ch))
    dismount_char(ch);

  stop_fusion(ch);

  /** leave people grouped when they die **/
  /*if (ch->followers || ch->master)
   die_follower(ch);*/

  /*ends fight event*/
  halt_fighting(ch);

  /* cancel point updates */
 /** no need to cancle regen!**/
 /* 
 {
  int i;
  for (i = 0; i < 4; i++)
    if (GET_POINTS_EVENT(ch, i))
    {
      event_cancel(GET_POINTS_EVENT(ch, i));
      GET_POINTS_EVENT(ch, i) = NULL;
    }
    }
    */
  /* cancel message updates */
  if (GET_MESSAGE_EVENT(ch))
  {
    event_cancel(GET_MESSAGE_EVENT(ch));
    GET_MESSAGE_EVENT(ch) = NULL;
  }
  GET_MSG_RUN(ch) = 0;
  /* cancel the task */
  stop_task(ch);
  remove_hunter(ch);

  /* we can't forget the hunters either... */
  for (hunt = hunter_list; hunt; hunt = hnext)
  {
    hnext = hunt->next;
    if (!hunt->hunter)
      continue;
    if (HUNTING(hunt->hunter) != ch)
      continue;

    forget(hunt->hunter, ch);
    forget(ch, hunt->hunter);
    remove_hunter(hunt->hunter);

  }

  char_from_room(ch);
  /* remove any pending event for/from this character */
  clean_events2(ch);
  char_to_room(ch, real_room(1205));
  LOOK(ch);
  GET_WAIT_STATE(ch) = 2 RL_SEC;
  affect_total(ch);
  GET_HIT(ch) = 3;
  check_regen_rates(ch);
  save_char(ch);

}
void free_hunter_list(void)
{

  struct hunter_data *hunt, *hnext;
  /* we can't forget the hunters either... */
  for (hunt = hunter_list; hunt; hunt = hnext)
  {
    hnext = hunt->next;
    remove_hunter(hunt->hunter);
  }
  hunter_list = NULL;
}

#if 1
/* Extract a ch completely from the world, and leave his stuff behind */
void extract_char_final(struct char_data *ch)
{

  struct descriptor_data *d = NULL;
  struct hunter_data *hunt = NULL, *hnext;
  int i;

  if (IN_ROOM(ch) == NULL)
  {
    log("SYSERR: NOWHERE extracting char %s. (%s, extract_char_final)",
        GET_NAME(ch), __FILE__);
    ALERT_1;
    abort();
  }

  if (ch == ch_selling)
    stop_auction(AUC_QUIT_CANCEL, NULL);



  /* Forget snooping, if applicable */
  if (ch->desc)
  {



    if (ch->desc->snooping)
    {
      ch->desc->snooping->snoop_by = NULL;
      ch->desc->snooping = NULL;
    }
    if (ch->desc->snoop_by)
    {
      SEND_TO_Q("Your victim is no longer among us.\r\n",
                ch->desc->snoop_by);
      ch->desc->snoop_by->snooping = NULL;
      ch->desc->snoop_by = NULL;
    }
  }
  /*
   * We're booting the character of someone who has switched so first we
   * need to stuff them back into their own body.  This will set ch->desc
   * we're checking below this loop to the proper value.
   */
  if (!IS_NPC(ch) && !ch->desc)
  {
    for (d = descriptor_list; d; d = d->next)
      if (d->original == ch)
      {
        do_return(d->character, NULL, 0, 0);
        break;
      }
  }

  if (ch->desc)
  {
    /*
     * This time we're extracting the body someone has switched into
     * (not the body of someone switching as above) so we need to put
     * the switcher back to their own body.
     *
     * If this body is not possessed, the owner won't have a
     * body after the removal so dump them to the main menu.
     */
    if (ch->desc->original)
      do_return(ch, NULL, 0, 0);
    else
    {
      /*
       * Now we boot anybody trying to log in with the same character, to
       * help guard against duping.  CON_DISCONNECT is used to close a
       * descriptor without extracting the d->character associated with it,
       * for being link-dead, so we want CON_CLOSE to clean everything up.
       * If we're here, we know it's a player so no IS_NPC check required.
       */
      for (d = descriptor_list; d; d = d->next)
      {
        if (d == ch->desc)
          continue;
        if (d->character  && GET_ACC(ch) == GET_ACC(d->character))
          STATE(d) = CON_CLOSE;
      }
      STATE(ch->desc) = CON_MENU;
      write_to_output(ch->desc, "%s", MENU);
    }
  }

  /*ends fight event*/
  halt_fighting(ch);
  
  if (RIDING(ch) || RIDDEN_BY(ch))
    dismount_char(ch);

  stop_fusion(ch);

  die_link(ch);

  if (ch->followers || ch->master)
    die_follower(ch);

  eq_to_room(ch);

  /* remove the locker memory*/
  extract_all_in_list(LOCKER(ch));
  LOCKER(ch) = NULL;
  char_from_room(ch);
   if (GET_FIGHT_EVENT(ch))
    {
      event_cancel(GET_FIGHT_EVENT(ch));
      GET_FIGHT_EVENT(ch) = NULL;
    }
  /* cancel point updates */
  for (i = 0; i < 4; i++)
    if (GET_POINTS_EVENT(ch, i))
    {
      event_cancel(GET_POINTS_EVENT(ch, i));
      GET_POINTS_EVENT(ch, i) = NULL;
    }
  /* cancel message updates */
  if (GET_MESSAGE_EVENT(ch))
  {
    event_cancel(GET_MESSAGE_EVENT(ch));
    GET_MESSAGE_EVENT(ch) = NULL;
  }
  GET_MSG_RUN(ch) = 0;
  /* cancel the task */
  stop_task(ch);

  remove_hunter(ch);

  /* we can't forget the hunters either... */
  for (hunt = hunter_list; hunt; hunt = hnext)
  {
    hnext = hunt->next;

    if (!hunt->hunter)
      continue;
    if (HUNTING(hunt->hunter) != ch)
      continue;

    //HUNTING(hunt->hunter) = NULL;
    forget(hunt->hunter, ch);
    forget(ch, hunt->hunter);
    remove_hunter(hunt->hunter);

  }

  /* remove any pending event for/from this character */
  clean_events2(ch);

  if (IS_NPC(ch))
  {
    if (GET_MOB_RNUM(ch) != NOTHING)	/* prototyped */
      mob_index[GET_MOB_RNUM(ch)].number--;
    clearMemory(ch);
    if (SCRIPT(ch))
      extract_script(ch, MOB_TRIGGER);
    if (GET_MOB_RNUM(ch) == NOTHING || ch->proto_script != mob_proto[GET_MOB_RNUM(ch)].proto_script)
      free_proto_script(ch, MOB_TRIGGER);
    if (SCRIPT_MEM(ch))
      extract_script_mem(SCRIPT_MEM(ch));
  }
  else
  {
    save_char(ch);
  }

  /* If there's a descriptor, they're in the menu now. */
   if (IS_NPC(ch) || !ch->desc)
    free_char(ch);
}
#endif
/*
 * Q: Why do we do this?
 * A: Because trying to iterate over the character
 *    list with 'ch = ch->next' does bad things if
 *    the current character happens to die. The
 *    trivial workaround of 'vict = next_vict'
 *    doesn't work if the _next_ person in the list
 *    gets killed, for example, by an area spell.
 *
 * Q: Why do we leave them on the character_list?
 * A: Because code doing 'vict = vict->next' would
 *    get really confused otherwise.
 
 put all the files back 
 except the files from the world directory
 exept files taht already exist that are older then the ones in the tar
 
 
 */
#if 1
void extract_char(struct char_data *ch)
{
  if (DEAD(ch))
  {
    if (IN_ROOM(ch))
      log("Extracting char more then once (vnum:%d : name:%s : room:%d)", GET_MOB_VNUM(ch), GET_NAME(ch), GET_ROOM_VNUM(IN_ROOM(ch)));
      else
      log("Extracting char more then once (vnum:%d : name:%s)", GET_MOB_VNUM(ch), GET_NAME(ch));
    return;
  }
  if (IS_NPC(ch))
  {
    SET_BIT_AR(MOB_FLAGS(ch), MOB_NOTDEADYET);
    extract_linked_mob(ch);
  }
  else
    SET_BIT_AR(PLR_FLAGS(ch), PLR_NOTDEADYET);

  extractions_pending++;
}
#endif

/*
 * I'm not particularly pleased with the MOB/PLR
 * hoops that have to be jumped through but it
 * hardly calls for a completely new variable.
 * Ideally it would be its own list, but that
 * would change the '->next' pointer, potentially
 * confusing some code. Ugh. -gg 3/15/2001
 *
 * NOTE: This doesn't handle recursive extractions.
 */
void extract_pending_chars(void)
{
  struct char_data *vict, *next_vict, *prev_vict;

  if (extractions_pending < 0)
    log("SYSERR: Negative (%d) extractions pending.",
        extractions_pending);

  for (vict = character_list, prev_vict = NULL;
       vict && extractions_pending; vict = next_vict)
  {
    next_vict = vict->next;

    if (MOB_FLAGGED(vict, MOB_NOTDEADYET))
      REMOVE_BIT_AR(MOB_FLAGS(vict), MOB_NOTDEADYET);
    else if (PLR_FLAGGED(vict, PLR_NOTDEADYET))
      REMOVE_BIT_AR(PLR_FLAGS(vict), PLR_NOTDEADYET);
    else
    {
      /* Last non-free'd character to continue chain from. */
      prev_vict = vict;
      continue;
    }

    extract_char_final(vict);
    extractions_pending--;

    if (prev_vict)
      prev_vict->next = next_vict;
    else
      character_list = next_vict;
  }

  if (extractions_pending > 0)
    log("SYSERR: Couldn't find %d extractions as counted.",
        extractions_pending);

  extractions_pending = 0;
}

/* ***********************************************************************
* Here follows high-level versions of some earlier routines, ie functions*
* which incorporate the actual player-data                               *.
*********************************************************************** */



struct char_data *get_player_vis(struct char_data *ch, char *name,
                                       int *number, int inroom)
{
  struct char_data *i;
  DESCRIPTOR_DATA *d;
  int num;

  if (!number)
  {
    number = &num;
    num = get_number(&name);
  }
  if (*name == UID_CHAR)
  {
    i = find_char(atoi(name + 1));

    if (i && valid_dg_target(i, TRUE))
      return i;
  }

  for (d = descriptor_list; d; d = d->next)
  {
    if (!IS_PLAYING(d))
      continue;
    if (inroom == FIND_CHAR_ROOM && !HERE(d->character, ch))
      continue;
    if (IS_NPC(ch))
    {
      if (str_cmp(d->character->player.name, name))	/* If not same, continue */
        continue;
    }
    else
    {
      if (!isname(name, d->character->player.name))
        continue;
    }
    if (!CAN_SEE(ch, d->character))
      continue;
    if (--(*number) != 0)
      continue;

    return (d->character);

  }

  for (i = character_list; i; i = i->next)
  {
    if (inroom == FIND_CHAR_ROOM && !HERE(i, ch))
      continue;
    if (IS_NPC(i))
      continue;
    if (IS_NPC(ch))
    {
      if (str_cmp(i->player.name, name))	/* If not same, continue */
        continue;
    }
    else
    {
      if (!isname(name, i->player.name))
        continue;
    }
    if (!CAN_SEE(ch, i))
      continue;
    if (--(*number) != 0)
      continue;
    return (i);
  }

  return (NULL);
}


struct char_data *get_player_room(room_rnum room, char *name, int *number,
                                        int inroom)
{
  struct descriptor_data *i;
  struct char_data *ch;
  int num;

  if (!number)
  {
    number = &num;
    num = get_number(&name);
  }

  for (i = descriptor_list; i; i = i->next)
  {
    ch = i->character;
    if (!IS_PLAYING(i))
      continue;
    if (inroom == FIND_CHAR_ROOM && IN_ROOM(ch) != room)
      continue;
    if (str_cmp(ch->player.name, name))	/* If not same, continue */
      continue;
    if (--(*number) != 0)
      continue;
    return (ch);
  }

  return (NULL);
}

struct char_data *get_room_vis(room_rnum room, char *name, int *number)
{
  struct char_data *i;
  int num;

  if (!number)
  {
    number = &num;
    num = get_number(&name);
  }

  if (room == NULL)
    return NULL;

  /* 0.<name> means PC with name */
  if (*number == 0)
    return (get_player_room(room, name, NULL, FIND_CHAR_ROOM));

  for (i = room->people; i && *number; i = i->next_in_room)
    if (isname(name, i->player.name))
      if (--(*number) == 0)
        return (i);

  return (NULL);
}

struct char_data *get_char_room_vis(struct char_data *ch, char *name,
                                          int *number)
{
  struct char_data *i;
  int num = 1;

  if (!number)
  {
    number = &num;
    if (name && *name)
      num = get_number(&name);
  }

  if (!ch)
    return NULL;

  /* JE 7/18/94 :-) :-) */
  if (!str_cmp(name, "self") || !str_cmp(name, "me"))
    return (ch);

  /* 0.<name> means PC with name */
  if (*number == 0)
    return (get_player_vis(ch, name, NULL, FIND_CHAR_ROOM));

  for (i = IN_ROOM(ch)->people; i && *number; i = i->next_in_room)
    if (isname(name, i->player.name))
      if (CAN_SEE(ch, i))
        if (--(*number) == 0)
          return (i);

  return (NULL);
}



struct char_data *get_char_world_vis(struct char_data *ch, char *name,
                                           int *number)
{
  struct char_data *i;
  int num;

  if (!ch)
    return NULL;

  if (!number)
  {
    number = &num;
    num = get_number(&name);
  }

  if ((i = get_char_room_vis(ch, name, number)) != NULL)
    return (i);

  if (*number == 0)
    return get_player_vis(ch, name, NULL, 0);

  for (i = character_list; i && *number; i = i->next)
  {
    if (IN_ROOM(ch) == IN_ROOM(i))
      continue;
    if (!isname(name, i->player.name))
      continue;
    if (!CAN_SEE(ch, i))
      continue;
    if (--(*number) != 0)
      continue;

    return (i);
  }
  return (NULL);
}


struct char_data *get_char_vis(struct char_data *ch, char *name,
                                     int *number, int where)
{
  if (where == FIND_CHAR_ROOM)
    return get_char_room_vis(ch, name, number);
  else if (where == FIND_CHAR_WORLD)
    return get_char_world_vis(ch, name, number);
  else
    return (NULL);
}



struct obj_data *get_obj_in_list_vis(struct char_data *ch, char *name,
                                           int *number, struct obj_data *list)
{
  struct obj_data *i;
  int num;

  if (!number)
  {
    number = &num;
    num = get_number(&name);
  }

  if (*number == 0)
    return (NULL);

  for (i = list; i && *number; i = i->next_content)
    if (isname(name, i->name))
      if (CAN_SEE_OBJ(ch, i))
        if (--(*number) == 0)
          return (i);

  return (NULL);
}




/* search the entire world for an object, and return a pointer  */
struct obj_data *get_obj_vis(struct char_data *ch, char *name, int *number)
{
  struct obj_data *i;
  int num;

  if (!number)
  {
    number = &num;
    num = get_number(&name);
  }

  if (*number == 0)
    return (NULL);

  /* scan items carried */
  if ((i = get_obj_in_list_vis(ch, name, number, ch->carrying)) != NULL)
    return (i);

  /* scan room */
  if ((i =
         get_obj_in_list_vis(ch, name, number,
                             IN_ROOM(ch)->contents)) != NULL)
    return (i);

  /* ok.. no luck yet. scan the entire obj list   */
  for (i = object_list; i && *number; i = i->next)
    if (isname(name, i->name))
      if (CAN_SEE_OBJ(ch, i))
        if (--(*number) == 0)
          return (i);

  return (NULL);
}


struct obj_data *get_obj_in_equip_vis(struct char_data *ch, char *arg,
                                            int *number,
                                            struct obj_data *equipment[])
{
  int j, num;

  if (!number)
  {
    number = &num;
    num = get_number(&arg);
  }

  if (*number == 0)
    return (NULL);

  for (j = 0; j < NUM_WEARS; j++)
    if (equipment[j] && CAN_SEE_OBJ(ch, equipment[j])
        && isname(arg, equipment[j]->name))
      if (--(*number) == 0)
        return (equipment[j]);

  return (NULL);
}
int get_obj_pos_in_equip_vis(struct char_data *ch, char *arg, int *number,
                             struct obj_data *equipment[])
{
  int j, num;

  if (!number)
  {
    number = &num;
    num = get_number(&arg);
  }

  if (*number == 0)
    return (-1);

  for (j = 0; j < NUM_WEARS; j++)
    if (equipment[j] && CAN_SEE_OBJ(ch, equipment[j])
        && isname(arg, equipment[j]->name))
      if (--(*number) == 0)
        return (j);

  return (-1);
}


const char *money_desc(gold_int amount)
{
  gold_int cnt;
  struct
  {
    gold_int limit;
    const char *description;
  }
  money_table[] = {
                    {
                      1, "a gold coin"}, {
                      10, "a tiny pile of gold coins"}, {
                      20, "a handful of gold coins"}, {
                      75, "a little pile of gold coins"}, {
                      200, "a small pile of gold coins"}, {
                      1000, "a pile of gold coins"}, {
                      5000, "a big pile of gold coins"}, {
                      10000, "a large heap of gold coins"}, {
                      20000, "a huge mound of gold coins"}, {
                      75000, "an enormous mound of gold coins"}, {
                      150000, "a small mountain of gold coins"}, {
                      250000, "a mountain of gold coins"}, {
                      500000, "a huge mountain of gold coins"}, {
                      1000000, "an enormous mountain of gold coins"}, {
                      0, NULL},};

  if (amount <= 0)
  {
    log("SYSERR: Try to create negative or 0 money (%lld).", amount);
    return (NULL);
  }

  for (cnt = 0; money_table[cnt].limit; cnt++)
    if (amount <= money_table[cnt].limit)
      return (money_table[cnt].description);

  return ("an absolutely colossal mountain of gold coins");
}


struct obj_data *create_money(gold_int gamount)
{
  int y;
  struct obj_data *obj;
  struct extra_descr_data *new_descr;
  char buf[200];
  int amount;

  if (gamount > 2000000000)
    amount = 2000000000;
  else
    amount = (int) gamount;

  if (amount <= 0 )
  {
    log("SYSERR: Try to create negative or 0 money, changing to 1(%lld)", gamount);
    return (NULL);
  }
  obj = create_obj();
  CREATE(new_descr, struct extra_descr_data, 1);

  if (amount == 1)
  {
    obj->name = str_dup("coin gold");
    obj->short_description = str_dup("a gold coin");
    obj->description =
      str_dup("One miserable gold coin is lying here.");
    new_descr->keyword = str_dup("coin gold");
    new_descr->description =
      str_dup("It's just one miserable little gold coin.");
  }
  else
  {
    obj->name = str_dup("coins gold");
    obj->short_description = str_dup(money_desc(amount));
    sprintf(buf, "%s is lying here.", money_desc(amount));
    obj->description = str_dup(CAP(buf));

    new_descr->keyword = str_dup("coins gold");
    if (amount < 10)
    {
      sprintf(buf, "There are %d coins.", amount);
      new_descr->description = str_dup(buf);
    }
    else if (amount < 100)
    {
      sprintf(buf, "There are about %d coins.",
              10 * (amount / 10));
      new_descr->description = str_dup(buf);
    }
    else if (amount < 1000)
    {
      sprintf(buf, "It looks to be about %d coins.",
              100 * (amount / 100));
      new_descr->description = str_dup(buf);
    }
    else if (amount < 100000)
    {
      sprintf(buf, "You guess there are, maybe, %d coins.",
              1000 * (((int) amount / 1000) +
                      number(0, ((int) amount / 1000))));
      new_descr->description = str_dup(buf);
    }
    else
      new_descr->description = str_dup("There are a LOT of coins.");
  }

  new_descr->next = NULL;
  obj->ex_description = new_descr;

  GET_OBJ_TYPE(obj) = ITEM_MONEY;
  for (y = 0; y < TW_ARRAY_MAX; y++)
    obj->obj_flags.wear_flags[y] = 0;
  SET_BIT_AR(GET_OBJ_WEAR(obj), ITEM_WEAR_TAKE);
  GET_OBJ_VAL(obj, 0) = (int) amount;
  GET_OBJ_COST(obj) = (int) amount;
  obj->item_number = NOTHING;
  if (GET_OBJ_RNUM(obj) != NOTHING) //this is because it has a negitive rnum.
    obj_index[GET_OBJ_RNUM(obj)].qic = NULL;


  return (obj);
}


/* Generic Find, designed to find any object/character
 *
 * Calling:
 *  *arg     is the pointer containing the string to be searched for.
 *           This string doesn't have to be a single word, the routine
 *           extracts the next word itself.
 *  bitv..   All those bits that you want to "search through".
 *           Bit found will be result of the function
 *  *ch      This is the person that is trying to "find"
 *  **tar_ch Will be NULL if no character was found, otherwise points
 * **tar_obj Will be NULL if no object was found, otherwise points
 *
 * The routine used to return a pointer to the next word in *arg (just
 * like the one_argument routine), but now it returns an integer that
 * describes what it filled in.
 */
int generic_find(char *arg, bitvector_t bitvector, struct char_data *ch,
                 struct char_data **tar_ch, struct obj_data **tar_obj)
{
  int i, found, number;
  char name_val[MAX_INPUT_LENGTH];
  char *name = name_val;

  *tar_ch = NULL;
  *tar_obj = NULL;

  one_argument(arg, name);

  if (!*name)
    return (0);
  if (!(number = get_number(&name)))
    return (0);

  if (IS_SET(bitvector, FIND_CHAR_ROOM))
  {	/* Find person in room */
    if ((*tar_ch = get_char_room_vis(ch, name, &number)) != NULL)
      return (FIND_CHAR_ROOM);
  }

  if (IS_SET(bitvector, FIND_CHAR_WORLD))
  {
    if ((*tar_ch = get_char_world_vis(ch, name, &number)) != NULL)
      return (FIND_CHAR_WORLD);
  }

  if (IS_SET(bitvector, FIND_OBJ_EQUIP))
  {
    for (found = FALSE, i = 0; i < NUM_WEARS && !found; i++)
      if (GET_EQ(ch, i) && isname(name, GET_EQ(ch, i)->name)
          && --number == 0)
      {
        *tar_obj = GET_EQ(ch, i);
        found = TRUE;
      }
    if (found)
      return (FIND_OBJ_EQUIP);
  }

  if (IS_SET(bitvector, FIND_OBJ_INV))
  {
    if ((*tar_obj =
           get_obj_in_list_vis(ch, name, &number, ch->carrying)) != NULL)
      return (FIND_OBJ_INV);
  }

  if (IS_SET(bitvector, FIND_OBJ_ROOM))
  {
    if ((*tar_obj =
           get_obj_in_list_vis(ch, name, &number,
                               IN_ROOM(ch)->contents)) != NULL)
      return (FIND_OBJ_ROOM);
  }

  if (IS_SET(bitvector, FIND_OBJ_WORLD))
  {
    if ((*tar_obj = get_obj_vis(ch, name, &number)))
      return (FIND_OBJ_WORLD);
  }

  return (0);
}


/* a function to scan for "all" or "all.x" */
int find_all_dots(char *arg)
{
  char *ppos;
  int plen;
  if (!arg || !*arg)
    return (FIND_INDIV);

  if (!strcmp(arg, "all"))
    return (FIND_ALL);
  else if (!strncmp(arg, "all.", 4))
  {
    ppos = arg+4;
    memmove(arg, ppos, (plen = strlen(ppos)));
    *(arg + plen) = '\0';
    /*strcpy(arg, arg + 4);*/
    return (FIND_ALLDOT);
  }
  else
    return (FIND_INDIV);
}


// dismount_char() / fr: Daniel Koepke (dkoepke@california.com)
//   If a character is mounted on something, we dismount them.  If
//   someone is mounting our character, then we dismount that someone.
//   This is used for cleaning up after a mount is cancelled by
//   something (either intentionally or by death, etc.)
void dismount_char(struct char_data *ch)
{
  if (RIDING(ch))
  {
    RIDDEN_BY(RIDING(ch)) = NULL;
    RIDING(ch) = NULL;
  }

  if (RIDDEN_BY(ch))
  {
    RIDING(RIDDEN_BY(ch)) = NULL;
    RIDDEN_BY(ch) = NULL;
  }
}


// mount_char() / fr: Daniel Koepke (dkoepke@california.com)
//   Sets _ch_ to mounting _mount_.  This does not make any checks
//   what-so-ever to see if the _mount_ is mountable, etc.  That is
//   left up to the calling function.  This does not present any
//   messages, either.
void mount_char(struct char_data *ch, struct char_data *mount)
{
  RIDING(ch) = mount;
  RIDDEN_BY(mount) = ch;
}

/* Search the given list for an object type, and return a ptr to that obj */
struct obj_data *get_obj_in_list_type(int type, struct obj_data *list)
{
  struct obj_data *i;

  for (i = list; i; i = i->next_content)
    if (GET_OBJ_TYPE(i) == type)
      return i;
  return NULL;
}

void add_hunter(struct char_data *ch)
{
  struct hunter_data *temp, *hunt, *nhunt;
  if (!ch)
    return;
  for (hunt = hunter_list; hunt; hunt = nhunt)
  {
    nhunt = hunt->next;
    if (hunt == NULL)
      continue;
    if (hunt->hunter == ch)
      return;
  }
  CREATE(temp, struct hunter_data, 1);
  temp->next = hunter_list;
  hunter_list = temp;
  temp->hunter = ch;
  HUNT_COUNT(ch) = (GET_LEVEL(ch) * 0.1) + 2;
}

void remove_hunter(struct char_data *ch)
{
  struct hunter_data *temp, *hunt, *nhunt = NULL;
  if (!ch)
    return;

  for (hunt = hunter_list; hunt; hunt = nhunt)
  {
    nhunt = hunt->next;
    if (hunt == NULL)
      continue;
    if (hunt->hunter == ch)
    {
      HUNTING(hunt->hunter) = NULL;
      HUNT_COUNT(hunt->hunter) = 0;
      REMOVE_FROM_LIST(hunt, hunter_list, next);
      free(hunt);
      hunt = NULL;
      //return;
    }
  }
}



