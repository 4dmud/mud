/************************************************************************/
/** OasisOLC - medit.c						v1.5	**/
/** Copyright 1996 Harvey Gilpin.					**/
/************************************************************************/

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "interpreter.h"
#include "comm.h"
#include "spells.h"
#include "utils.h"
#include "db.h"
#include "shop.h"
#include "genolc.h"
#include "genmob.h"
#include "genzon.h"
#include "genshp.h"
#include "oasis.h"
#include "handler.h"
#include "constants.h"
#include "improved-edit.h"
#include "screen.h"
#include "dg_olc.h"
#include "screen.h"
#include "constants.h"

/*-------------------------------------------------------------------*/

/*
 * External variable declarations.
 */
extern struct attack_hit_type attack_hit_text[];
extern struct shop_data *shop_index;
extern const char *mob_races[];
#if CONFIG_OASIS_MPROG
extern const char *mobprog_types[];
#endif
float mob_hitpoint_multi(int chclass);
/*
 * External function prototypes/
 */
void smash_tilde(char *str);
const char * race_name(struct char_data *ch);
const char *simple_class_name(struct char_data *ch);

struct combine_data *add_base_link_mob(CHAR_DATA *mob, int vnum);
void free_join_list(struct combine_data *list);
int join_count(CHAR_DATA *mob);
struct combine_data *copy_proto_link(struct combine_data *proto);

void delete_one_join(CHAR_DATA *mob, int i);

/*-------------------------------------------------------------------*/

/*
 * Handy internal macros.
 */
#if CONFIG_OASIS_MPROG
#define GET_MPROG(mob)		(mob_index[(mob)->nr].mobprogs)
#define GET_MPROG_TYPE(mob)	(mob_index[(mob)->nr].progtypes)
#endif

/*-------------------------------------------------------------------*/

/*
 * Function prototypes.
 */
#if CONFIG_OASIS_MPROG
void medit_disp_mprog(struct descriptor_data *d);
void medit_change_mprog(struct descriptor_data *d);
const char *medit_get_mprog_type(struct mob_prog_data *mprog);
#endif

/*********************************************************************/
/**  utility functions. **/
/*-------------------------------------------------------------------*/

ACMD(do_oasis_medit)
{
  int number = NOBODY, save = 0, real_num;
  struct descriptor_data *d;
  char *buf3;
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];

  /****************************************************************************/
  /** Parse any arguments.                                                   **/
  /****************************************************************************/
  buf3 = two_arguments(argument, buf1, buf2);

  if (!*buf1)
  {
    new_send_to_char(ch, "Specify a mobile VNUM to edit.\r\n");
    return;
  }
  else if (!isdigit(*buf1))
  {
    if (str_cmp("save", buf1) != 0)
    {
      new_send_to_char(ch, "Yikes!  Stop that, someone will get hurt!\r\n");
      return;
    }

    save = TRUE;

    if (is_number(buf2))
      number = atoi(buf2);
    else if (GET_OLC_ZONE(ch) > 0)
    {
      zone_rnum zlok;

      if ((zlok = real_zone(GET_OLC_ZONE(ch))) == NOWHERE)
        number = NOWHERE;
      else
        number = genolc_zone_bottom(zlok);
    }

    if (number == NOWHERE)
    {
      new_send_to_char(ch, "Save which zone?\r\n");
      return;
    }
  }

  /****************************************************************************/
  /** If a numeric argument was given (like a room number), get it.          **/
  /****************************************************************************/
  if (number == NOBODY)
    number = atoi(buf1);

  /****************************************************************************/
  /** Check that whatever it is isn't already being edited.                  **/
  /****************************************************************************/
  for (d = descriptor_list; d; d = d->next)
  {
    if (STATE(d) == CON_MEDIT)
    {
      if (d->olc && OLC_NUM(d) == number)
      {
        new_send_to_char(ch, "That mobile is currently being edited by %s.\r\n",
                         GET_NAME(d->character));
        return;
      }
    }
  }

  d = ch->desc;

  /****************************************************************************/
  /** Give descriptor an OLC structure.                                      **/
  /****************************************************************************/
  if (d->olc)
  {
    new_mudlog(BRF, LVL_IMMORT, TRUE,
               "SYSERR: do_oasis_medit: Player already had olc structure.");
    free(d->olc);
  }

  CREATE(d->olc, struct oasis_olc_data, 1);

  /****************************************************************************/
  /** Find the zone.                                                         **/
  /****************************************************************************/
  OLC_ZNUM(d) = save ? real_zone(number) : real_zone_by_thing(number);
  if (OLC_ZNUM(d) == NOWHERE)
  {
    new_send_to_char(ch, "Sorry, there is no zone for that number!\r\n");
    free(d->olc);
    d->olc = NULL;
    return;
  }

  /****************************************************************************/
  /** Everyone but IMPLs can only edit zones they have been assigned.        **/
  /****************************************************************************/
  if (!can_edit_zone(ch, OLC_ZNUM(d)))
  {
    new_send_to_char(ch, "You do not have permission to edit this zone.\r\n");
    new_mudlog(BRF, LVL_IMPL, TRUE, "OLC: %s tried to edit zone %d allowed zone %d",
               GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));
    free(d->olc);
    d->olc = NULL;
    return;
  }

  /****************************************************************************/
  /** If save is TRUE, save the mobiles.                                     **/
  /****************************************************************************/
  if (save)
  {
    new_send_to_char(ch, "Saving all mobiles in zone %d.\r\n",
                     zone_table[OLC_ZNUM(d)].number);
    new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(ch)), TRUE,
               "OLC: %s saves mobile info for zone %d.",
               GET_NAME(ch), zone_table[OLC_ZNUM(d)].number);

    /**************************************************************************/
    /** Save the mobiles.                                                    **/
    /**************************************************************************/
    save_mobiles(OLC_ZNUM(d));

    /**************************************************************************/
    /** Free the olc structure stored in the descriptor.                     **/
    /**************************************************************************/
    free(d->olc);
    d->olc = NULL;
    return;
  }

  OLC_NUM(d) = number;

  /****************************************************************************/
  /** If this is a new mobile, setup a new one, otherwise, setup the         **/
  /** existing mobile.                                                       **/
  /****************************************************************************/
  if ((real_num = real_mobile(number)) == NOBODY)
    medit_setup_new(d);
  else
    medit_setup_existing(d, real_num);

  STATE(d) = CON_MEDIT;

  /****************************************************************************/
  /** Display the OLC messages to the players in the same room as the        **/
  /** builder and also log it.                                               **/
  /****************************************************************************/
  act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);

  new_mudlog(BRF, LVL_IMMORT, TRUE,"OLC: %s starts editing zone %d allowed zone %d",
             GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));
}

void medit_save_to_disk(zone_vnum foo)
{
  save_mobiles(real_zone(foo));
}

void medit_setup_new(struct descriptor_data *d)
{
  struct char_data *mob;

  /*
   * Allocate a scratch mobile structure.  
   */
  CREATE(mob, struct char_data, 1);

  init_mobile(mob);

  GET_MOB_RNUM(mob) = NOBODY;
  /*
   * Set up some default strings.
   */
  GET_ALIAS(mob) = strdup("mob unfinished");
  GET_SDESC(mob) = strdup("the unfinished mob");
  GET_LDESC(mob) = strdup("An unfinished mob stands here.\r\n");
  GET_DDESC(mob) = strdup("It looks unfinished.\r\n");
#if CONFIG_OASIS_MPROG
  OLC_MPROGL(d) = NULL;
  OLC_MPROG(d) = NULL;
#endif
  SCRIPT(mob) = NULL;
  mob->proto_script = OLC_SCRIPT(d) = NULL;

  OLC_MOB(d) = mob;
  /* Has changed flag. (It hasn't so far, we just made it.) */
  OLC_VAL(d) = FALSE;
  OLC_ITEM_TYPE(d) = MOB_TRIGGER;

  medit_disp_menu(d);
}

/*-------------------------------------------------------------------*/

void medit_setup_existing(struct descriptor_data *d, int rmob_num)
{
  struct char_data *mob;

  /*
   * Allocate a scratch mobile structure. 
   */
  CREATE(mob, struct char_data, 1);

  copy_mobile(mob, mob_proto + rmob_num);

#if CONFIG_OASIS_MPROG
  {
    MPROG_DATA *temp;
    MPROG_DATA *head;

    if (GET_MPROG(mob))
      CREATE(OLC_MPROGL(d), MPROG_DATA, 1);
    head = OLC_MPROGL(d);
    for (temp = GET_MPROG(mob); temp; temp = temp->next)
    {
      OLC_MPROGL(d)->type = temp->type;
      OLC_MPROGL(d)->arglist = strdup(temp->arglist);
      OLC_MPROGL(d)->comlist = strdup(temp->comlist);
      if (temp->next)
      {
        CREATE(OLC_MPROGL(d)->next, MPROG_DATA, 1);
        OLC_MPROGL(d) = OLC_MPROGL(d)->next;
      }
    }
    OLC_MPROGL(d) = head;
    OLC_MPROG(d) = OLC_MPROGL(d);
  }
#endif

  OLC_MOB(d) = mob;
  OLC_ITEM_TYPE(d) = MOB_TRIGGER;
  dg_olc_script_copy(d);
  /*
   * The edited mob must not have a script.
   * It will be assigned to the updated mob later, after editing.
   */
  SCRIPT(mob) = NULL;
  OLC_MOB(d)->proto_script = NULL;

  medit_disp_menu(d);
}

/*-------------------------------------------------------------------*/

/*
 * Ideally, this function should be in db.c, but I'll put it here for
 * portability.
 */
void init_mobile(struct char_data *mob)
{
  clear_char(mob);

  GET_HIT(mob) = GET_MANA(mob) = 1;
  GET_MAX_MANA(mob) = GET_MAX_MOVE(mob) = 100;
  GET_NDD(mob) = GET_SDD(mob) = 1;
  GET_WEIGHT(mob) = 200;
  GET_HEIGHT(mob) = 198;
  GET_CLASS(mob) = CLASS_NORMAL;
  MOB_TIER(mob) = 0;
  MOB_SUBSKILL(mob) = TYPE_UNDEFINED;
  MOB_SKIN(mob) = TYPE_UNDEFINED;


  mob->real_abils.str = mob->real_abils.intel = mob->real_abils.wis = 11;
  mob->real_abils.dex = mob->real_abils.con = mob->real_abils.cha = 11;
  mob->aff_abils = mob->real_abils;

  SET_BIT_AR(MOB_FLAGS(mob), MOB_ISNPC);
  mob->player_specials = &dummy_mob;
}

/*-------------------------------------------------------------------*/

/*
 * Save new/edited mob to memory.
 */
void medit_save_internally(struct descriptor_data *d)
{
  int i;
  mob_rnum new_rnum;
  struct descriptor_data *dsc;
  struct char_data *mob;

  i = (real_mobile(OLC_NUM(d)) == NOBODY);

  if ((new_rnum = add_mobile(OLC_MOB(d), OLC_NUM(d))) == NOBODY)
  {
    log("medit_save_internally: add_mobile failed.");
    return;
  }


  /* Update triggers */
  /* Free old proto list  */
  if (mob_proto[new_rnum].proto_script &&
      mob_proto[new_rnum].proto_script != OLC_SCRIPT(d))
    free_proto_script(&mob_proto[new_rnum], MOB_TRIGGER);

  mob_proto[new_rnum].proto_script = OLC_SCRIPT(d);

  /* this takes care of the mobs currently in-game */
  for (mob = character_list; mob; mob = mob->next)
  {
    if (GET_MOB_RNUM(mob) != new_rnum)
      continue;

    /* remove any old scripts */
    if (SCRIPT(mob))
      extract_script(mob, MOB_TRIGGER);

    free_proto_script(mob, MOB_TRIGGER);
    copy_proto_script(&mob_proto[new_rnum], mob, MOB_TRIGGER);
    assign_triggers(mob, MOB_TRIGGER);
  }
  /* end trigger update */

  if (!i)	/* Only renumber on new mobiles. */
    return;

  /*
   * Update keepers in shops being edited and other mobs being edited.
   */
  for (dsc = descriptor_list; dsc; dsc = dsc->next)
  {
    if (STATE(dsc) == CON_SEDIT)
      S_KEEPER(OLC_SHOP(dsc)) += (S_KEEPER(OLC_SHOP(dsc)) >= new_rnum);
    else if (STATE(dsc) == CON_MEDIT)
      GET_MOB_RNUM(OLC_MOB(dsc)) += (GET_MOB_RNUM(OLC_MOB(dsc)) >= new_rnum);
  }

  /*
   * Update other people in zedit too. From: C.Raehl 4/27/99
   */
  for (dsc = descriptor_list; dsc; dsc = dsc->next)
    if (STATE(dsc) == CON_ZEDIT)
      for (i = 0; OLC_ZONE(dsc)->cmd[i].command != 'S'; i++)
        if (OLC_ZONE(dsc)->cmd[i].command == 'M')
          if (OLC_ZONE(dsc)->cmd[i].arg1 >= new_rnum)
            OLC_ZONE(dsc)->cmd[i].arg1++;
}

/**************************************************************************
 Menu functions 
 **************************************************************************/
void medit_disp_mob_stats(struct descriptor_data *d, int state)
{
  switch (state)
  {
  case (MEDIT_CHA):
          write_to_output(d, "How much Charisma should this mob have:");
    break;
  }

}
/*
 * Display positions. (sitting, standing, etc)
 */
void medit_disp_positions(struct descriptor_data *d)
{
  int i;

  get_char_colors(d->character);
  clear_screen(d);

  for (i = 0; *position_types[i] != '\n'; i++)
  {
    write_to_output(d, "%s%2d%s) %s\r\n", grn, i, nrm, position_types[i]);
  }
  write_to_output(d, "Enter position number : ");
}

/*-------------------------------------------------------------------*/

#if CONFIG_OASIS_MPROG
/*
 * Get the type of MobProg.
 */
const char *medit_get_mprog_type(struct mob_prog_data *mprog)
{
  switch (mprog->type)
  {
  case IN_FILE_PROG:	return ">in_file_prog";
  case ACT_PROG:	return ">act_prog";
  case SPEECH_PROG:	return ">speech_prog";
  case RAND_PROG:	return ">rand_prog";
  case FIGHT_PROG:	return ">fight_prog";
  case HITPRCNT_PROG:	return ">hitprcnt_prog";
  case DEATH_PROG:	return ">death_prog";
  case ENTRY_PROG:	return ">entry_prog";
  case GREET_PROG:	return ">greet_prog";
  case ALL_GREET_PROG:	return ">all_greet_prog";
  case GIVE_PROG:	return ">give_prog";
  case BRIBE_PROG:	return ">bribe_prog";
  }
  return ">ERROR_PROG";
}

/*-------------------------------------------------------------------*/

/*
 * Display the MobProgs.
 */
void medit_disp_mprog(struct descriptor_data *d)
{
  struct mob_prog_data *mprog = OLC_MPROGL(d);

  OLC_MTOTAL(d) = 1;

  clear_screen(d);
  while (mprog)
  {
    write_to_output(d, "%d) %s %s\r\n", OLC_MTOTAL(d), medit_get_mprog_type(mprog),
                    (mprog->arglist ? mprog->arglist : "NONE"));
    OLC_MTOTAL(d)++;
    mprog = mprog->next;
  }
  write_to_output(d, "%d) Create New Mob Prog\r\n"
                  "%d) Purge Mob Prog\r\n"
                  "Enter number to edit [0 to exit]:  ",
                  OLC_MTOTAL(d), OLC_MTOTAL(d) + 1);
  OLC_MODE(d) = MEDIT_MPROG;
}

/*-------------------------------------------------------------------*/

/*
 * Change the MobProgs.
 */
void medit_change_mprog(struct descriptor_data *d)
{
  clear_screen(d);
  write_to_output(d, "1) Type: %s\r\n"
                  "2) Args: %s\r\n"
                  "3) Commands:\r\n%s\r\n\r\n"
                  "Enter number to edit [0 to exit]: ",
                  medit_get_mprog_type(OLC_MPROG(d)),
                  (OLC_MPROG(d)->arglist ? OLC_MPROG(d)->arglist: "NONE"),
                  (OLC_MPROG(d)->comlist ? OLC_MPROG(d)->comlist : "NONE"));

  OLC_MODE(d) = MEDIT_CHANGE_MPROG;
}

/*-------------------------------------------------------------------*/

/*
 * Change the MobProg type.
 */
void medit_disp_mprog_types(struct descriptor_data *d)
{
  int i;

  get_char_colors(d->character);
  clear_screen(d);

  for (i = 0; i < NUM_PROGS-1; i++)
  {
    write_to_output(d, "%s%2d%s) %s\r\n", grn, i, nrm, mobprog_types[i]);
  }
  write_to_output(d, "Enter mob prog type : ");
  OLC_MODE(d) = MEDIT_MPROG_TYPE;
}
#endif

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
void medit_disp_mob_race(struct descriptor_data *d)
{
  int i;

  get_char_colors(d->character);

  for (i = 0; i < NUM_MOB_RACES; i++)
    write_to_output(d, "%s%2d%s) %s\r\n", grn, i, nrm, mob_races[i]);

  write_to_output(d,"Enter race number : ");
}
void medit_disp_mob_skin(struct descriptor_data *d)
{
  obj_vnum skin = MOB_SKIN(OLC_MOB(d));

  get_char_colors(d->character);
  write_to_output(d, "When mob is skinned it will currently load: %d (%s)\r\n", skin ,  real_object(skin) != NOTHING ? "exists" : "doesn't exist yet");

  write_to_output(d,"Enter skin vnum : ");
}

void medit_disp_mob_joins(struct descriptor_data *d)
{
  int i, j = 0;
  struct combine_data *temp = (OLC_MOB(d)->mob_specials.join_list);

  get_char_colors(d->character);
  write_to_output(d, "Mob segments linked:\r\n");
  if (!temp)
    write_to_output(d, "None.\r\n");
  else
  {

    while (temp)
    {
      i = real_mobile(temp->vnum);
      if (i != NOTHING)
        write_to_output(d, "%s%2d%s) [%5d] %s\r\n\r\n", grn, ++j, nrm, temp->vnum, mob_proto[i].player.short_descr);
      temp = temp->next;
    }
  }
  write_to_output(d, "Enter vnum of new mob segment ('d' to delete, -1 to quit): ");

}

/*-------------------------------------------------------------------*/
void medit_disp_mob_subskill(struct descriptor_data *d)
{
  int i;

  get_char_colors(d->character);

  for (i = 1; i < TOP_SUB_DEFINE; i++)
  {
    write_to_output(d, "%s%2d%s) %20s", grn, i, nrm, sub_name(i));
    if (!(i%3))
      write_to_output(d,"\r\n");
  }
  write_to_output(d,"Enter subskill number (-1 for none): ");
}
void medit_disp_mob_tier(struct descriptor_data *d)
{
  write_to_output(d,"Enter a Tier: 0 to 4: ");
}
/*-------------------------------------------------------------------*/
void medit_disp_mob_class(struct descriptor_data *d)
{
  int i;

  get_char_colors(d->character);

  for (i = 0; i < NUM_MOB_CLASSES; i++)
    write_to_output(d, "%s%2d%s) %s\r\n", grn, i, nrm, npc_class_types[i]);

  write_to_output(d,"Enter class number : ");
}

/*
 * Display the gender of the mobile.
 */
void medit_disp_sex(struct descriptor_data *d)
{
  int i;

  get_char_colors(d->character);
  clear_screen(d);

  for (i = 0; i < NUM_GENDERS; i++)
  {
    write_to_output(d, "%s%2d%s) %s\r\n", grn, i, nrm, genders[i]);
  }
  write_to_output(d, "Enter gender number : ");
}

/*-------------------------------------------------------------------*/

/*
 * Display attack types menu.
 */
void medit_disp_attack_types(struct descriptor_data *d)
{
  int i;

  get_char_colors(d->character);
  clear_screen(d);

  for (i = 0; i < NUM_ATTACK_TYPES; i++)
  {
    write_to_output(d, "%s%2d%s) %s\r\n", grn, i, nrm, attack_hit_text[i].singular);
  }
  write_to_output(d, "Enter attack type : ");
}

/*-------------------------------------------------------------------*/

/*
 * Display mob-flags menu.
 */
void medit_disp_mob_flags(struct descriptor_data *d)
{
  int i, columns = 0;
  char flags[MAX_STRING_LENGTH];

  get_char_colors(d->character);
  clear_screen(d);
  for (i = 0; i < NUM_MOB_FLAGS; i++)
  {
    write_to_output(d, "%s%2d%s) %-20.20s  %s", grn, i + 1, nrm, action_bits[i],
                    !(++columns % 2) ? "\r\n" : "");
  }
  sprintbitarray(MOB_FLAGS(OLC_MOB(d)), action_bits, PM_ARRAY_MAX, flags, sizeof(flags));
  write_to_output(d, "\r\nCurrent flags : %s%s%s\r\nEnter mob flags (0 to quit) : ",
                  cyn, flags, nrm);
}

/*-------------------------------------------------------------------*/

/*
 * Display affection flags menu.
 */
void medit_disp_aff_flags(struct descriptor_data *d)
{
  int i, columns = 0;
  char flags[MAX_STRING_LENGTH];

  get_char_colors(d->character);
  clear_screen(d);
  for (i = 0; i < NUM_AFF_FLAGS; i++)
  {
    write_to_output(d, "%s%2d%s) %-20.20s  %s", grn, i + 1, nrm, affected_bits[i],
                    !(++columns % 2) ? "\r\n" : "");
  }
  sprintbitarray(AFF_FLAGS(OLC_MOB(d)), affected_bits, AF_ARRAY_MAX, flags, sizeof(flags));
  write_to_output(d, "\r\nCurrent flags   : %s%s%s\r\nEnter aff flags (0 to quit) : ",
                  cyn, flags, nrm);
}

/*-------------------------------------------------------------------*/

/*
 * Display main menu.
 */
void medit_disp_menu(struct descriptor_data *d)
{
  struct char_data *mob;
  char flags[MAX_STRING_LENGTH], flag2[MAX_STRING_LENGTH];

  mob = OLC_MOB(d);
  get_char_colors(d->character);
  clear_screen(d);

  write_to_output(d,
                  "-- Mob Number:  [%s%d%s]\r\n"
                  "%s1%s) Sex: %s%-7.7s%s	         %s2%s) Alias: %s%s\r\n"
                  "%s3%s) S-Desc: %s%s\r\n"
                  "%s4%s) L-Desc:-\r\n%s%s"
                  "%s5%s) D-Desc:-\r\n%s%s"
                  "%s6%s) Level:       [%s%4d%s],  %s7%s) Alignment:    [%s%4d%s]\r\n"
                  "%s8%s) Hitroll:     [%s%4d%s],  %s9%s) Damroll:      [%s%4d%s]\r\n"
                  "%sA%s) NumDamDice:  [%s%4d%s],  %sB%s) SizeDamDice:  [%s%4d%s]\r\n"
                  "%sC%s) Num HP Dice: [%s%4d%s],  %sD%s) Size HP Dice: [%s%4d%s],  %sE%s) HP Bonus: [%s%5d%s]\r\n"
                  "%sF%s) Armor Class: [%s%4d%s],  %sG%s) Exp:     [%s%9lld%s],  %sH%s) Gold:  [%s%8lld%s]\r\n",

                  cyn, OLC_NUM(d), nrm,
                  grn, nrm, yel, genders[(int)GET_SEX(mob)], nrm,
                  grn, nrm, yel, GET_ALIAS(mob),
                  grn, nrm, yel, GET_SDESC(mob),
                  grn, nrm, yel, GET_LDESC(mob),
                  grn, nrm, yel, GET_DDESC(mob),
                  grn, nrm, cyn, GET_LEVEL(mob), nrm,
                  grn, nrm, cyn, GET_ALIGNMENT(mob), nrm,
                  grn, nrm, cyn, GET_HITROLL(mob), nrm,
                  grn, nrm, cyn, GET_DAMROLL(mob), nrm,
                  grn, nrm, cyn, GET_NDD(mob), nrm,
                  grn, nrm, cyn, GET_SDD(mob), nrm,
                  grn, nrm, cyn, GET_HIT(mob), nrm,
                  grn, nrm, cyn, GET_MANA(mob), nrm,
                  grn, nrm, cyn, GET_MOVE(mob), nrm,
                  grn, nrm, cyn, GET_AC(mob), nrm,
                  grn, nrm, cyn, GET_EXP(mob), nrm,
                  grn, nrm, cyn, GET_GOLD(mob), nrm
                 );

  sprintbitarray(MOB_FLAGS(mob), action_bits, AF_ARRAY_MAX, flags, sizeof(flags));
  sprintbitarray(AFF_FLAGS(mob), affected_bits, AF_ARRAY_MAX, flag2, sizeof(flag2));
  write_to_output(d,
                  "%sI%s) Position  : %s%-9s %sJ%s) Default   : %s%s\r\n"
                  "%sK%s) Attack    : %s%s\r\n"
                  "%sL%s) NPC Flags : %s%s\r\n"
                  "%sM%s) AFF Flags : %s%s\r\n"
                  "%sN%s) Segments  : %s%s\r\n"
                  "%sR%s) Mob Race  : %s%-9s      %sV%s) Mob Class : %s%s\r\n"
                  "%sT%s) Tier      : %s%-9d      %sO%s) Subskill  : %s%s\r\n"
                  "%sU%s) Charisma  : %s%-9d      %sW%s) Skin Vnum : %s%d\r\n"
#if CONFIG_OASIS_MPROG
                  "%sP%s) Mob Progs : %s%s\r\n"
#endif
                  "%sS%s) Script    : %s%s\r\n"
                  "%sQ%s) Quit\r\n"
                  "Enter choice : ",

                  grn, nrm, yel, position_types[(int)GET_POS(mob)],
                  grn, nrm, yel, position_types[(int)GET_DEFAULT_POS(mob)],
                  grn, nrm, yel, attack_hit_text[(int)GET_ATTACK(mob)].singular,
                  grn, nrm, cyn, flags,	  grn, nrm, cyn, flag2,
                  grn, nrm, cyn, mob->mob_specials.join_list ? "Yes" : "No",
                  grn, nrm, cyn, race_name(mob),
                  grn, nrm, cyn, simple_class_name(mob),
                  grn, nrm, cyn, MOB_TIER(mob),
                  grn, nrm, cyn, sub_name(MOB_SUBSKILL(mob)),
                  grn, nrm, cyn, GET_CHA(mob),
                  grn, nrm, cyn, MOB_SKIN(mob),
#if CONFIG_OASIS_MPROG
                  grn, nrm, cyn, (OLC_MPROGL(d) ? "Set." : "Not Set."),
#endif
                  grn, nrm, cyn, OLC_SCRIPT(d) ?"Set.":"Not Set.",
                  grn, nrm
                 );

  OLC_MODE(d) = MEDIT_MAIN_MENU;
}

/************************************************************************
 *			The GARGANTAUN event handler			*
 ************************************************************************/

void medit_parse(struct descriptor_data *d, char *arg)
{
  int i = -1;
  char *oldtext = NULL;

  if (OLC_MODE(d) > MEDIT_NUMERICAL_RESPONSE)
  {
    i = atoi(arg);
    if (!*arg || (!isdigit(arg[0]) && ((*arg == '-') && !isdigit(arg[1]))))
    {
      write_to_output(d, "Field must be numerical, try again : ");
      return;
    }
  }
  else
  {	/* String response. */
    if (!genolc_checkstring(d, arg))
      return;
  }
  switch (OLC_MODE(d))
  {
    /*-------------------------------------------------------------------*/
  case MEDIT_CONFIRM_SAVESTRING:
    /*
     * Ensure mob has MOB_ISNPC set or things will go pear shaped.
     */
    SET_BIT_AR(MOB_FLAGS(OLC_MOB(d)), MOB_ISNPC);
    switch (*arg)
    {
    case 'y':
    case 'Y':
      /*
       * Save the mob in memory and to disk.
       */
      medit_save_internally(d);
      new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(d->character)), TRUE,
                 "OLC: %s edits mob %d", GET_NAME(d->character), OLC_NUM(d));
      if (CONFIG_OLC_SAVE)
      {
        medit_save_to_disk(zone_table[real_zone_by_thing(OLC_NUM(d))].number);
        write_to_output(d, "Mobile saved to disk.\r\n");
      }
      else
        write_to_output(d, "Mobile saved to memory.\r\n");
      /* FALL THROUGH */
    case 'n':
    case 'N':
      cleanup_olc(d, CLEANUP_ALL);
      return;
    default:
      write_to_output(d, "Invalid choice!\r\n");
      write_to_output(d, "Do you wish to save the mobile? : ");
      return;
    }
    break;

    /*-------------------------------------------------------------------*/
  case MEDIT_MAIN_MENU:
    i = 0;
    switch (*arg)
    {
    case 'q':
    case 'Q':
      if (OLC_VAL(d))
      {	/* Anything been changed? */
        write_to_output(d, "Do you wish to save the changes to the mobile? (y/n) : ");
        OLC_MODE(d) = MEDIT_CONFIRM_SAVESTRING;
      }
      else
        cleanup_olc(d, CLEANUP_ALL);
      return;
    case '1':
      OLC_MODE(d) = MEDIT_SEX;
      medit_disp_sex(d);
      return;
    case '2':
      OLC_MODE(d) = MEDIT_ALIAS;
      i--;
      break;
    case '3':
      OLC_MODE(d) = MEDIT_S_DESC;
      i--;
      break;
    case '4':
      OLC_MODE(d) = MEDIT_L_DESC;
      i--;
      break;
    case '5':
      OLC_MODE(d) = MEDIT_D_DESC;
      send_editor_help(d);
      write_to_output(d, "Enter mob description:\r\n\r\n");
      if (OLC_MOB(d)->player.description)
      {
        write_to_output(d, "%s", OLC_MOB(d)->player.description);
        oldtext = strdup(OLC_MOB(d)->player.description);
      }
      string_write(d, &OLC_MOB(d)->player.description, MAX_MOB_DESC, 0, oldtext);
      OLC_VAL(d) = 1;
      return;
    case '6':
      OLC_MODE(d) = MEDIT_LEVEL;
      i++;
      break;
    case '7':
      OLC_MODE(d) = MEDIT_ALIGNMENT;
      i++;
      break;
    case '8':
      OLC_MODE(d) = MEDIT_HITROLL;
      i++;
      break;
    case '9':
      OLC_MODE(d) = MEDIT_DAMROLL;
      i++;
      break;
    case 'a':
    case 'A':
      OLC_MODE(d) = MEDIT_NDD;
      i++;
      break;
    case 'b':
    case 'B':
      OLC_MODE(d) = MEDIT_SDD;
      i++;
      break;
    case 'c':
    case 'C':
      OLC_MODE(d) = MEDIT_NUM_HP_DICE;
      i++;
      break;
    case 'd':
    case 'D':
      OLC_MODE(d) = MEDIT_SIZE_HP_DICE;
      i++;
      break;
    case 'e':
    case 'E':
      OLC_MODE(d) = MEDIT_ADD_HP;
      i++;
      break;
    case 'f':
    case 'F':
      OLC_MODE(d) = MEDIT_AC;
      i++;
      break;
    case 'g':
    case 'G':
      OLC_MODE(d) = MEDIT_EXP;
      i++;
      break;
    case 'h':
    case 'H':
      OLC_MODE(d) = MEDIT_GOLD;
      i++;
      break;
    case 'i':
    case 'I':
      OLC_MODE(d) = MEDIT_POS;
      medit_disp_positions(d);
      return;
    case 'j':
    case 'J':
      OLC_MODE(d) = MEDIT_DEFAULT_POS;
      medit_disp_positions(d);
      return;
    case 'k':
    case 'K':
      OLC_MODE(d) = MEDIT_ATTACK;
      medit_disp_attack_types(d);
      return;
    case 'l':
    case 'L':
      OLC_MODE(d) = MEDIT_NPC_FLAGS;
      medit_disp_mob_flags(d);
      return;
    case 'm':
    case 'M':
      OLC_MODE(d) = MEDIT_AFF_FLAGS;
      medit_disp_aff_flags(d);
      return;
    case 'o':
    case 'O':
      OLC_MODE(d) = MEDIT_SUBSKILL;
      medit_disp_mob_subskill(d);
      return;
    case 'w':
    case 'W':
      OLC_MODE(d) = MEDIT_SKIN;
      medit_disp_mob_skin(d);
      return;
    case 't':
    case 'T':
      OLC_MODE(d) = MEDIT_TIER;
      medit_disp_mob_tier(d);
      return;
    case 'r':
    case 'R':
      OLC_MODE(d) = MEDIT_RACE;
      medit_disp_mob_race(d);
      return;
    case 'v':
    case 'V':
      OLC_MODE(d) = MEDIT_CLASS;
      medit_disp_mob_class(d);
      return;
    case 'n':
    case 'N':
      OLC_MODE(d) = MEDIT_SEGMENTS;
      medit_disp_mob_joins(d);
      return;

    case 'u':
    case 'U':
      OLC_MODE(d) = MEDIT_CHA;
      medit_disp_mob_stats(d, MEDIT_CHA);
      return;
#if CONFIG_OASIS_MPROG
    case 'p':
    case 'P':
      OLC_MODE(d) = MEDIT_MPROG;
      medit_disp_mprog(d);
      return;
#endif
    case 's':
    case 'S':
      OLC_SCRIPT_EDIT_MODE(d) = SCRIPT_MAIN_MENU;
      dg_script_menu(d);
      return;
    default:
      medit_disp_menu(d);
      return;
    }
    if (i == 0)
      break;
    else if (i == 1)
      write_to_output(d, "\r\nEnter new value : ");
    else if (i == -1)
      write_to_output(d, "\r\nEnter new text :\r\n] ");
    else
      write_to_output(d, "Oops...\r\n");
    return;
    /*-------------------------------------------------------------------*/
  case OLC_SCRIPT_EDIT:
    if (dg_script_edit_parse(d, arg)) return;
    break;
    /*-------------------------------------------------------------------*/
  case MEDIT_ALIAS:
    smash_tilde(arg);
    if (GET_ALIAS(OLC_MOB(d)))
      free(GET_ALIAS(OLC_MOB(d)));
    GET_ALIAS(OLC_MOB(d)) = str_udup(arg);
    break;
    /*-------------------------------------------------------------------*/
  case MEDIT_S_DESC:
    smash_tilde(arg);
    if (GET_SDESC(OLC_MOB(d)))
      free(GET_SDESC(OLC_MOB(d)));
    GET_SDESC(OLC_MOB(d)) = str_udup(arg);
    break;
    /*-------------------------------------------------------------------*/
  case MEDIT_L_DESC:
    smash_tilde(arg);
    if (GET_LDESC(OLC_MOB(d)))
      free(GET_LDESC(OLC_MOB(d)));
    if (arg && *arg)
    {
      char buf[MAX_INPUT_LENGTH];
      snprintf(buf, sizeof(buf), "%s\r\n", arg);
      GET_LDESC(OLC_MOB(d)) = strdup(buf);
    }
    else
      GET_LDESC(OLC_MOB(d)) = strdup("undefined");

    break;
    /*-------------------------------------------------------------------*/
  case MEDIT_D_DESC:
    /*
     * We should never get here.
     */
    cleanup_olc(d, CLEANUP_ALL);
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: medit_parse(): Reached D_DESC case!");
    write_to_output(d, "Oops...\r\n");
    break;
    /*-------------------------------------------------------------------*/
#if CONFIG_OASIS_MPROG
  case MEDIT_MPROG_COMLIST:
    /*
     * We should never get here, but if we do, bail out.
     */
    cleanup_olc(d, CLEANUP_ALL);
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: medit_parse(): Reached MPROG_COMLIST case!");
    break;
#endif
    /*-------------------------------------------------------------------*/
  case MEDIT_NPC_FLAGS:
    if ((i = atoi(arg)) <= 0)
      break;
    else if (i <= NUM_MOB_FLAGS)
    {
      switch (i)
      {
      case MOB_AGGRESSIVE:
        if (!IS_SET_AR(MOB_FLAGS(OLC_MOB(d)), MOB_AGGRESSIVE))
        {
          SET_BIT_AR(MOB_FLAGS(OLC_MOB(d)), MOB_NOCHARM);
          SET_BIT_AR(MOB_FLAGS(OLC_MOB(d)), MOB_NOSUMMON);
          REMOVE_BIT_AR(MOB_FLAGS(OLC_MOB(d)), MOB_AGGR_GOOD);
          REMOVE_BIT_AR(MOB_FLAGS(OLC_MOB(d)), MOB_AGGR_EVIL);
          REMOVE_BIT_AR(MOB_FLAGS(OLC_MOB(d)), MOB_AGGR_NEUTRAL);
        }
        break;
      case MOB_QUEST:
        if (!IS_SET_AR(MOB_FLAGS(OLC_MOB(d)), MOB_QUEST))
        {
          SET_BIT_AR(MOB_FLAGS(OLC_MOB(d)), MOB_NOCHARM);
          SET_BIT_AR(MOB_FLAGS(OLC_MOB(d)), MOB_NOSUMMON);
          SET_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_INFRAVISION);
          SET_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_DETECT_INVIS);
          SET_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_SENSE_LIFE);
          SET_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_NOTRACK);
        }
      }
      TOGGLE_BIT_AR(MOB_FLAGS(OLC_MOB(d)), (i - 1));
    }
    medit_disp_mob_flags(d);
    return;
    /*-------------------------------------------------------------------*/
  case MEDIT_AFF_FLAGS:
    if ((i = atoi(arg)) <= 0)
      break;
    else if (i <= NUM_AFF_FLAGS)
      TOGGLE_BIT_AR(AFF_FLAGS(OLC_MOB(d)), (i - 1));
    /* Remove unwanted bits right away. */
    REMOVE_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_CHARM);
    REMOVE_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_POISON_1);
    REMOVE_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_POISON_2);
    REMOVE_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_POISON_3);
    REMOVE_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_POISON_4);
    REMOVE_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_GROUP);
    REMOVE_BIT_AR(AFF_FLAGS(OLC_MOB(d)), AFF_SLEEP);
    medit_disp_aff_flags(d);
    return;
    /*-------------------------------------------------------------------*/
#if CONFIG_OASIS_MPROG
  case MEDIT_MPROG:
    if ((i = atoi(arg)) == 0)
      medit_disp_menu(d);
    else if (i == OLC_MTOTAL(d))
    {
      struct mob_prog_data *temp;
      CREATE(temp, struct mob_prog_data, 1);
      temp->next = OLC_MPROGL(d);
      temp->type = -1;
      temp->arglist = NULL;
      temp->comlist = NULL;
      OLC_MPROG(d) = temp;
      OLC_MPROGL(d) = temp;
      OLC_MODE(d) = MEDIT_CHANGE_MPROG;
      medit_change_mprog (d);
    }
    else if (i < OLC_MTOTAL(d))
    {
      struct mob_prog_data *temp;
      int x = 1;
      for (temp = OLC_MPROGL(d); temp && x < i; temp = temp->next)
        x++;
      OLC_MPROG(d) = temp;
      OLC_MODE(d) = MEDIT_CHANGE_MPROG;
      medit_change_mprog (d);
    }
    else if (i == (OLC_MTOTAL(d) + 1))
    {
      write_to_output(d, "Which mob prog do you want to purge? ");
      OLC_MODE(d) = MEDIT_PURGE_MPROG;
    }
    else
      medit_disp_menu(d);
    return;

  case MEDIT_PURGE_MPROG:
    if ((i = atoi(arg)) > 0 && i < OLC_MTOTAL(d))
    {
      struct mob_prog_data *temp;
      int x = 1;

      for (temp = OLC_MPROGL(d); temp && x < i; temp = temp->next)
        x++;
      OLC_MPROG(d) = temp;
      REMOVE_FROM_LIST(OLC_MPROG(d), OLC_MPROGL(d), next);
      free(OLC_MPROG(d)->arglist);
      free(OLC_MPROG(d)->comlist);
      free(OLC_MPROG(d));
      OLC_MPROG(d) = NULL;
      OLC_VAL(d) = 1;
    }
    medit_disp_mprog(d);
    return;

  case MEDIT_CHANGE_MPROG:
    if ((i = atoi(arg)) == 1)
      medit_disp_mprog_types(d);
    else if (i == 2)
    {
      write_to_output(d, "Enter new arg list: ");
      OLC_MODE(d) = MEDIT_MPROG_ARGS;
    }
    else if (i == 3)
    {
      write_to_output(d, "Enter new mob prog commands:\r\n");
      /*
       * Pass control to modify.c for typing.
       */
      OLC_MODE(d) = MEDIT_MPROG_COMLIST;
      if (OLC_MPROG(d)->comlist)
      {
        write_to_output(d, "%s", OLC_MPROG(d)->comlist);
        oldtext = strdup(OLC_MPROG(d)->comlist);
      }
      string_write(d, &OLC_MPROG(d)->comlist, MAX_STRING_LENGTH, 0, oldtext);
      OLC_VAL(d) = 1;
    }
    else
      medit_disp_mprog(d);
    return;
#endif

    /*-------------------------------------------------------------------*/

    /*
     * Numerical responses.
     */

#if CONFIG_OASIS_MPROG
  case MEDIT_MPROG_TYPE:
    /*
     * This calculation may be off by one too many powers of 2?
     * Someone who actually uses MobProgs will have to check.
     */
    OLC_MPROG(d)->type = (1 << LIMIT(atoi(arg), 0, NUM_PROGS - 1));
    OLC_VAL(d) = 1;
    medit_change_mprog(d);
    return;

  case MEDIT_MPROG_ARGS:
    OLC_MPROG(d)->arglist = strdup(arg);
    OLC_VAL(d) = 1;
    medit_change_mprog(d);
    return;
#endif

  case MEDIT_SEX:
    GET_SEX(OLC_MOB(d)) = LIMIT(i, 0, NUM_GENDERS - 1);
    break;

  case MEDIT_HITROLL:
    GET_HITROLL(OLC_MOB(d)) = LIMIT(i, 0, 50);
    break;

  case MEDIT_DAMROLL:
    GET_DAMROLL(OLC_MOB(d)) = LIMIT(i, 0, 50);
    break;

  case MEDIT_NDD:
    GET_NDD(OLC_MOB(d)) = LIMIT(i, 0, 30);
    break;

  case MEDIT_SDD:
    GET_SDD(OLC_MOB(d)) = LIMIT(i, 0, 127);
    break;

  case MEDIT_NUM_HP_DICE:
    GET_HIT(OLC_MOB(d)) = LIMIT(i, 0, 30);
    break;

  case MEDIT_SIZE_HP_DICE:
    GET_MANA(OLC_MOB(d)) = LIMIT(i, 0, 1000);
    break;

  case MEDIT_ADD_HP:
    GET_MOVE(OLC_MOB(d)) = LIMIT(i, 0, 30000);
    break;

  case MEDIT_AC:
    GET_AC(OLC_MOB(d)) = LIMIT(i, -200, 200);
    break;

  case MEDIT_EXP:
    GET_EXP(OLC_MOB(d)) = LIMIT(i, 0, MAX_MOB_EXP);
    break;

  case MEDIT_GOLD:
    if (GET_MRACE(OLC_MOB(d)) == MOB_RACE_ANIMAL || GET_MRACE(OLC_MOB(d)) == MOB_RACE_EXOTIC)
      GET_GOLD(OLC_MOB(d)) = 0;
    else
      GET_GOLD(OLC_MOB(d)) = LIMIT(i, 0, MAX_MOB_GOLD);
    break;

  case MEDIT_POS:
    GET_POS(OLC_MOB(d)) = LIMIT(i, 0, NUM_POSITIONS - 1);
    break;

  case MEDIT_DEFAULT_POS:
    GET_DEFAULT_POS(OLC_MOB(d)) = LIMIT(i, 0, NUM_POSITIONS - 1);
    break;

  case MEDIT_ATTACK:
    GET_ATTACK(OLC_MOB(d)) = LIMIT(i, 0, NUM_ATTACK_TYPES - 1);
    break;
  case MEDIT_CHA:
    GET_CHA(OLC_MOB(d)) = LIMIT(i, 0, 100);
    break;

  case MEDIT_LEVEL:
    GET_LEVEL(OLC_MOB(d)) = LIMIT(i, 1, MAX_MOB_LEVELS - 1);
    GET_HITROLL(OLC_MOB(d)) = mob_stats[IRANGE(1,atoi(arg), MAX_MOB_LEVELS- 1)].hitroll;
    GET_DAMROLL(OLC_MOB(d)) = mob_stats[IRANGE(1,atoi(arg), MAX_MOB_LEVELS- 1)].dam_bonus;
    GET_NDD(OLC_MOB(d)) = mob_stats[IRANGE(1,atoi(arg), MAX_MOB_LEVELS- 1)].dam_dice;
    GET_SDD(OLC_MOB(d)) = mob_stats[IRANGE(1,atoi(arg), MAX_MOB_LEVELS- 1)].dam_sides;
    GET_HIT(OLC_MOB(d)) = mob_stats[IRANGE(1,atoi(arg), MAX_MOB_LEVELS- 1)].hp_dice;
    GET_MANA(OLC_MOB(d)) = mob_stats[IRANGE(1,atoi(arg), MAX_MOB_LEVELS- 1)].hp_sides;
    GET_MOVE(OLC_MOB(d)) = mob_stats[IRANGE(1,atoi(arg), MAX_MOB_LEVELS- 1)].hp_bonus;
    GET_AC(OLC_MOB(d)) = mob_stats[IRANGE(1,atoi(arg), MAX_MOB_LEVELS- 1)].ac;
    GET_EXP(OLC_MOB(d)) = mob_stats[IRANGE(1,atoi(arg), MAX_MOB_LEVELS - 1)].exp;
    if (GET_MRACE(OLC_MOB(d)) == MOB_RACE_ANIMAL || GET_MRACE(OLC_MOB(d)) == MOB_RACE_EXOTIC)
      GET_GOLD(OLC_MOB(d)) = 0;
    else
      GET_GOLD(OLC_MOB(d)) = (mob_stats[atoi(arg)].gold / 2);
    break;
  case MEDIT_ALIGNMENT:
    GET_ALIGNMENT(OLC_MOB(d)) = LIMIT(i, -1000, 1000);
    break;
  case MEDIT_RACE:
    i = 0;
    i = GET_LEVEL(OLC_MOB(d));
    GET_MRACE(OLC_MOB(d)) = MAX(0, MIN(3, atoi(arg)));
    if (GET_MRACE(OLC_MOB(d)) == MOB_RACE_ANIMAL || GET_MRACE(OLC_MOB(d)) == MOB_RACE_EXOTIC)
      GET_GOLD(OLC_MOB(d)) = 0;
    else
      GET_GOLD(OLC_MOB(d)) = (mob_stats[i].gold);
    break;
  case MEDIT_CLASS:
    GET_CLASS(OLC_MOB(d)) = IRANGE(0, atoi(arg), NUM_MOB_CLASSES-1);
    break;
  case MEDIT_TIER:
    MOB_TIER(OLC_MOB(d)) = IRANGE(0, atoi(arg), 4);
    break;
  case MEDIT_SUBSKILL:
    MOB_SUBSKILL(OLC_MOB(d)) = IRANGE(-1, atoi(arg), TOP_SUB_DEFINE-1);
    break;
  case MEDIT_SKIN:
    MOB_SKIN(OLC_MOB(d)) = IRANGE(-1, atoi(arg), 999999-1);
    break;
  case MEDIT_SEGMENTS:
    switch (*arg)
    {
    case 'd':
    case 'D':
      write_to_output(d, "Delete which segment number?: ");
      OLC_MODE(d) = MEDIT_DELETE_SEGMENT;
      return;
    default:
      if (((i = atoi(arg)) == 0 || i >= 999999) || i == GET_MOB_VNUM(OLC_MOB(d)))
      {
        write_to_output(d, "That vnum is invalid!\r\n");
        return;
      }
      else if (i == -1)
      {
        break;
      }
      else if (!real_mobile(i))
      {
        write_to_output(d, "That mob doesn't exist!\r\n");
        return;
      }
      else
        add_base_link_mob(OLC_MOB(d), i);

      medit_disp_mob_joins(d);
      return;
    }
    break;
  case MEDIT_DELETE_SEGMENT:
    if ((i = atoi(arg)) <= join_count(OLC_MOB(d)) && i > 0)
    {
      delete_one_join(OLC_MOB(d), i);
    }
    else
    {
      write_to_output(d, "That number is invalid!\r\n");
      medit_disp_mob_joins(d);
      return;
    }
    break;
    /*-------------------------------------------------------------------*/
  default:
    /*
     * We should never get here.
     */
    cleanup_olc(d, CLEANUP_ALL);
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: medit_parse(): Reached default case!");
    write_to_output(d, "Oops...\r\n");
    break;
  }
  /*-------------------------------------------------------------------*/

  /*
   * END OF CASE 
   * If we get here, we have probably changed something, and now want to
   * return to main menu.  Use OLC_VAL as a 'has changed' flag  
   */

  OLC_VAL(d) = TRUE;
  medit_disp_menu(d);
}

void medit_string_cleanup(struct descriptor_data *d, int terminator)
{
  switch (OLC_MODE(d))
  {

#if CONFIG_OASIS_MPROG
  case MEDIT_MPROG_COMLIST:
    medit_change_mprog(d);
    break;
#endif

  case MEDIT_D_DESC:
  default:
    medit_disp_menu(d);
    break;
  }
}


float mob_hitpoint_multi(int chclass)
{


  switch (chclass)
  {
  case  CLASS_NORMAL:
    return 1.0f;
    break;
  case CLASS_UNDEAD:
    return 1.0f;
    break;
  case CLASS_CASTER:
    return 0.5f;
    break;
  case CLASS_FIGHTER:
    return 1.5f;
    break;
  case CLASS_ROGUE:
    return 0.9f;
    break;
  case CLASS_ANIMAL:
    return 0.7f;
    break;
  }

  return 1.0f;


}

