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
#include "clan.h"
#include "fight.h"

void list_subs_in_prof(Character *ch,int pro);
int tot_subs_in_prof(int pro);
int knows_subs_in_prof(Character *ch, int pro);
const char * prof_name(int pro);
const char * prof_group_name(int pro);
int prof_group(int pro);
int skill_cost(int h, int m, int v, Character *ch);
extern struct sub_skill_info_type sub_info[TOP_SUB_DEFINE];

static int prof_group_data[] = {
                                 PGRP_MISC, //misc
                                 PGRP_MISC, //PROF_NECROMANCER,
                                 PGRP_MISC, //PROF_DRAGONMASTER,
                                 PGRP_MISC, //PROF_PERFORMER,
                                 PGRP_WOODWORKER, //PROF_WOODSMAN,
                                 PGRP_MISC, //PROF_BEASTMASTER,
                                 PGRP_STONEWORKER, //PROF_GLAZIER,
                                 PGRP_MISC, //PROF_SCRIBE,
                                 PGRP_MISC, //PROF_BUSINESS,
                                 PGRP_MISC, //PROF_COMMERCE,
                                 PGRP_STONEWORKER, //PROF_MINER,
                 PGRP_FERMENTER, //PROF_BREWER,
                                 PGRP_COMBAT, //PROF_COMBATANT,
                                 PGRP_LEATHERWORKER, //PROF_TANNER,
                                 PGRP_WOODWORKER, //PROF_MILLER,
                                 PGRP_TEXTILEWORKER, //PROF_WEAVER,
                                 PGRP_METALWORKER, //PROF_BLACKSMITH,
                                 PGRP_TEXTILEWORKER, //PROF_TAILOR,
                                 PGRP_MISC, //PROF_TECH,
                                 PGRP_MISC, //PROF_MANAGEMENT,
                                 PGRP_MISC, //PROF_PILOT,
                                 PGRP_METALWORKER, //PROF_FARRIER,
                                 PGRP_METALWORKER, //PROF_GOLDSMITH,
                                 PGRP_METALWORKER, //PROF_METALLURGIST,
                                 PGRP_METALWORKER, //PROF_WEAPONSMITH,
                                 PGRP_LEATHERWORKER, //PROF_FURRIER,
                                 PGRP_LEATHERWORKER, //PROF_SHOEMAKER,
                                 PGRP_LEATHERWORKER, //PROF_GLOVEMAKER,
                                 PGRP_LEATHERWORKER, //PROF_ARMOURER,
                                 PGRP_TEXTILEWORKER, //PROF_CARDER,
                                 PGRP_TEXTILEWORKER, //PROF_SPINNER,
                                 PGRP_TEXTILEWORKER, //PROF_DRESSMAKER,
                                 PGRP_WOODWORKER, //PROF_CARPENTER,
                                 PGRP_WOODWORKER, //PROF_CABINETMAKER,
                                 PGRP_WOODWORKER, //PROF_SHIPBUILDER,
                                 PGRP_STONEWORKER, //PROF_QUARRIER,
                                 PGRP_STONEWORKER, //PROF_COLLIER,
                                 PGRP_STONEWORKER, //PROF_BRICKMAKER,
                                 PGRP_STONEWORKER, //PROF_MASON,
                                 PGRP_STONEWORKER, //PROF_MIRRORMAKER,
                                 PGRP_STONEWORKER, //PROF_CONTRACTOR,
                 PGRP_ARTIST, // PROF_ARTISAN,
                 PGRP_GLASSWORKER, //PROF_GLASSWORK,
                 PGRP_METALWORKER, //PROF_METALWORK,
                 PGRP_STONEWORKER, // PROF_STONEWORK,
                 PGRP_TEXTILEWORKER, // PROF_TEXTILEWORK,
                 PGRP_WOODWORKER, // PROF_WOODWORK,
                                 PGRP_MISC, //PROF_PALADIN,
                            PGRP_MISC, //PROF_JEDI,
                 PGRP_MISC, //PROF_BRAVO,
                 PGRP_MISC, //PROF_SPACEPILOT,
                             PGRP_AFFLICTION, //PROF_AFFLICATION_VAMPIRE,
                 PGRP_AFFLICTION //PROF_AFFLICTION_WEREWOLF
                               };

const char * prof_name(int pro)
{
  if (pro < 0 || pro >= PROF_MAX)
    return "<none>";

  return  profession_names[pro];
}
const char * prof_group_name(int pro)
{
  if (pro < 0 || pro >= PGRP_MAX)
    return "<none>";

  return  profession_group_names[pro];
}

int prof_group(int pro)
{
  if (pro < 0 || pro >= PROF_MAX)
    return PGRP_MISC;

  return prof_group_data[pro];
}

ACMD(do_professions)
{
  int i, j;
  char buff[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];
  int pro = -1;
  DYN_DEFINE;
  *buf = 0;

  one_argument(argument, buff);

  if (*buff)
    pro = search_block(buff, profession_names, FALSE);

  if (pro == -1)
  {
    DYN_CREATE;
    *dynbuf = 0;

    ch->Send( "professions ::\r\n");
    for (j = 0; j < PGRP_MAX; j++)
    {
      sprintf(buf, "{cY<In the %s profession group>{c0\r\n", prof_group_name(j));
      DYN_RESIZE(buf);
      for (i = 0; i < PROF_MAX; i++)
      {
        if (j == prof_group(i))
        {
          sprintf(buf, "  {cgYou know %2d of a total %2d subskills -- {cW%s{c0\r\n", knows_subs_in_prof(ch, i), tot_subs_in_prof(i), prof_name(i));
          DYN_RESIZE(buf);
        }
      }
    }
    sprintf(buf, "\r\nThere is a total of %d professions and %d subskills.\r\nType: PROF <profession name>\r\nTo see the subskills available.\r\n",
            PROF_MAX, TOP_SUB_DEFINE);
    DYN_RESIZE(buf);
    page_string(ch->desc, dynbuf, DYN_BUFFER);
    return;
  }
  else
  {
    ch->Send( "  You know %2d of a total %2d subskills -- %s\r\n", knows_subs_in_prof(ch, pro), tot_subs_in_prof(pro), prof_name(pro));
    list_subs_in_prof(ch, pro);
  }
}

int knows_subs_in_prof(Character *ch, int pro)
{
  int count = 0, i;
  for (i = 0; i < TOP_SUB_DEFINE; i++)
  {
    if (!IS_SET(sub_info[i].flags, SUB_TYPE_PROF))
      continue;
    if (sub_info[i].parent == pro && GET_SUB(ch, i))
      count++;
  }
  return count;
}

int tot_subs_in_prof(int pro)
{
  int count = 0, i;
  for (i = 0; i < TOP_SUB_DEFINE; i++)
  {
    if (!IS_SET(sub_info[i].flags, SUB_TYPE_PROF))
      continue;
    if (sub_info[i].parent == pro)
      count++;
  }
  return count;

}

void list_subs_in_prof(Character *ch,int pro)
{
  int i;
  for (i = 0; i < TOP_SUB_DEFINE; i++)
  {
    if (!IS_SET(sub_info[i].flags, SUB_TYPE_PROF))
      continue;
    if (sub_info[i].parent != pro)
      continue;

    if (GET_SUB(ch, i))
    {
      ch->Send( "  {cg(known)  :: {cc%-20s{c0\r\n", sub_name(i));
    }
    else
    {
      ch->Send( " {cy(unknown) :: {cc%-20s{c0\r\n", sub_name(i));
    }
  }
}
#if 0
ACMD(do_skin)
{
  extern void crumble_obj(Character *ch, OBJ_DATA *obj);
  OBJ_DATA *scalp = NULL, *obj = NULL;
  Character *tch = NULL;
  struct extra_descr_data *new_descr = NULL;
  char /*buf1[MAX_STRING_LENGTH],*/ buf2[MAX_STRING_LENGTH];
  char scalpb[MAX_STRING_LENGTH];
  char *scalpa = scalpb;
  int pc = 0, k;

  skip_spaces(&argument);
  k = generic_find(argument, FIND_OBJ_ROOM, ch, &tch, &obj);
  if (obj)
  {
    if (!IS_CORPSE(obj))
    {
      ch->Send( "You slice the knife in to... nothing really! Fear your stupidity!\r\n");
      return;
    }
    if (!skill_cost(0, 0, 10, ch))
    {
      ch->Send( "You are exhausted!");
      return;
    }
    pc = OBJ_FLAGGED(obj, ITEM_PC_CORPSE);

    act("You slice the hide from $p.", FALSE, ch, obj, NULL, TO_CHAR);
    act("$n slices the hide from $p.", FALSE, ch, obj, NULL, TO_ROOM);

    strcpy(scalpa, obj->short_description);
    crumble_obj(ch, obj);
    scalpa = scalpa+13;
    scalp = create_obj();

    scalp->item_number = NOTHING;
    scalp->in_room = NULL;
    snprintf(buf2, sizeof(buf2), "skin %s", scalpa);
    scalp->name = str_dup(buf2);

    snprintf(buf2, sizeof(buf2), "The hide of %s lies here.", scalpa);

    scalp->description = str_dup(buf2);

    sprintf(buf2, "the hide of %s", scalpa);

    scalp->short_description = str_dup(buf2);

    /* extra description coolness! */
    CREATE(new_descr, struct extra_descr_data, 1);
    new_descr->keyword = str_dup(buf2);
    sprintf(buf2, "It appears to be the skin of %s.",scalpa);
    new_descr->description = str_dup(buf2);
    new_descr->next = NULL;
    scalp->ex_description = new_descr;
    GET_OBJ_TYPE(scalp) = ITEM_TREASURE;

    SET_BIT_AR(GET_OBJ_WEAR(scalp), ITEM_WEAR_HOLD);
    SET_BIT_AR(GET_OBJ_WEAR(scalp), ITEM_WEAR_TAKE);
    SET_BIT_AR(GET_OBJ_EXTRA(scalp), ITEM_UNIQUE_SAVE);
    GET_OBJ_WEIGHT(scalp) = 3;
    GET_OBJ_RENT(scalp) = 0;
    GET_OBJ_TIMER(scalp) = 600;
    GET_OBJ_VAL(scalp, 0) = number(0, 100);

    obj_to_char(scalp, ch);
    return;// SKILL_SCALP;

  }

  /*short_desc is +14, then all the way to 0
  OBJ_FLAGGED(obj, ITEM_NPC_CORPSE)
  PC_CORPSE
  */
  return;

}
#endif

