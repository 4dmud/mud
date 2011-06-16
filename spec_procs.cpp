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
 * Revision 1.30  2007/07/10 19:12:37  w4dimenscor
 * Ships don't give the purge message anymore when they are purged,
 * except when this happens because of a missile.
 * --Thotter
 *
 * Revision 1.29  2007/06/26 10:48:05  w4dimenscor
 * Fixed context in scripts so that it works again, changed mounted combat so that it is about 2/3rds player one third mount damage, updated the way skills get read using total_chance, stopped things with a PERC of 0 assisting, made it so that the ungroup command disbanded charmies
 *
 * Revision 1.28  2007/06/17 20:32:42  w4dimenscor
 * was deducting gold twice for practicing.
 *
 * Revision 1.27  2007/06/17 10:44:53  w4dimenscor
 * Fixed a bug in a mounted persons movepoints going below 0, fixed abuse of animate dead, fixed group spells to allow for inclusion of charmies, and set a limit on total charmies in a group.
 *
 * Revision 1.26  2007/06/17 04:34:38  w4dimenscor
 * updated combat for charmies. Made it split the damage among the group better
 *
 * Revision 1.25  2007/06/15 07:11:40  w4dimenscor
 * changed the display message for practicing to include what you spent.
 *
 * Revision 1.24  2007/06/10 02:18:39  w4dimenscor
 * changed all entries in the code of 'color' to 'colour', but i now regret it.
 *
 * Revision 1.23  2007/06/09 04:34:55  w4dimenscor
 * Fixed practice so that it takes money from players rather then gives them money, fixed a crash bug with furniture, added 'discard' as a junk alternitive, initialised the INTERNAL(ch) variable on Character objects
 *
 * Revision 1.22  2007/06/08 10:28:23  w4dimenscor
 * added a cost of 5k x current percentage to the cost of learning skills and spells
 *
 * Revision 1.21  2007/02/02 15:32:19  w4dimenscor
 * Fixed the guild guards. --Thotter
 *
 * Revision 1.20  2006/08/31 10:39:17  w4dimenscor
 * Fixe dthe crash bug in medit. and also changed the mob proto list. there is still a memory leak in medit, which is being fixed now
 *
 * Revision 1.19  2006/08/25 10:22:44  w4dimenscor
 * added command to fix peoples skills back to the practiced amount they were at
 *
 * Revision 1.18  2006/08/21 09:51:53  w4dimenscor
 * Fixed bug with zedit that caused a crash
 *
 * Revision 1.17  2006/08/18 11:09:59  w4dimenscor
 * updated some clan functions to use vectors instead of malloccing memory, and also sorted clan lists and updated their layout
 *
 * Revision 1.16  2006/08/17 10:53:49  w4dimenscor
 * moved the subs and skills from the char class to the player specials struct, converted them to vectors, and made them sorted.
 *
 * Revision 1.15  2006/08/13 06:26:55  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.14  2006/06/19 06:25:40  w4dimenscor
 * Changed the player saved mount feature so that all players can load mounts from houses
 *
 * Revision 1.13  2006/05/30 09:14:20  w4dimenscor
 * rewrote the color code, process_output, and vwrite_to_output so that they use strings and have better buffer checks
 *
 * Revision 1.12  2006/05/21 11:02:27  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
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
 * updated pets so that they don't have weight problems, updated award points
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
#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "clan.h"
#include "spells.h"
#include "dg_scripts.h"
#include "constants.h"
#include "damage.h"
#include "fight.h"
#include "descriptor.h"


/*   external vars  */
extern struct corpse_list_data *corpse_list;
extern struct time_info_data time_info;
extern int guild_info[][3];
extern const char *cmd_door[];

extern struct sub_skill_info_type sub_info[TOP_SUB_DEFINE];

/* extern functions */
void add_follower ( Character *ch, Character *leader );
int get_pidx_from_name ( Character *ch );
int find_door ( Character *ch, const char *type, char *dir,
                const char *cmdname );
void do_doorcmd ( Character *ch, struct obj_data *obj, int door,
                  int scmd );
void play_triples ( Character *ch, Character *dealer,
                    char *guess, int bet );
void play_slots ( Character *ch );
void play_high_dice ( Character *ch, Character *dealer,
                      int bet );
void play_seven ( Character *ch, Character *dealer,
                  char *guess, int bet );
void play_craps ( Character *ch, Character *dealer, int bet );
struct obj_data *find_vehicle_by_vnum ( int vnum );
struct obj_data *get_obj_in_list_type ( int type,
			                                        struct obj_data *list );
void explosion_messages ( room_rnum room, int damage, struct obj_data *target );
void set_race ( Character *ch, int race );
int has_class ( Character *ch, int chclass );
int tier_level ( Character *ch, int chclass );
void remove_corpse_from_list ( OBJ_DATA *corpse );
bool can_have_follower ( Character *ch, mob_vnum mob_num );
bool can_have_follower ( Character *ch, Character *vict );
room_rnum find_target_room(Character *ch, char *rawroomstr);
void look_in_obj(Character *ch, char *arg, struct obj_data *item);
bool is_same_zone(int dv, int cv);
void perform_wear(Character *ch, struct obj_data *obj, int where);

ACMD ( do_drop );
ACMD ( do_gen_door );
ACMD ( do_say );

bool check_token(OBJ_DATA *obj);

/* local functions */
void sort_spells ( void );
int compare_spells ( const void *x, const void *y );
const char *how_good ( int percent );
void list_skills ( Character *ch, int skillspell , Character *mob );
void npc_steal ( Character *ch, Character *victim );
void sort_spell_data ( void );
void sort_skill_data ( void );

SPECIAL (antidt);
SPECIAL ( bank );
SPECIAL ( bottle );
SPECIAL ( cityguard );
SPECIAL ( clan_deeds);
SPECIAL ( cleric );
SPECIAL ( craps );
SPECIAL ( door_down );
SPECIAL ( door_down_7377 );
SPECIAL ( dragon_acid );
SPECIAL ( dragon_fire );
SPECIAL ( dragon_frost );
SPECIAL ( dragon_gas );
SPECIAL ( dragon_lightning );
SPECIAL ( dump );
SPECIAL ( fido );
SPECIAL ( fire );
SPECIAL ( guard_black );
SPECIAL ( guard_white );
SPECIAL ( guild_guard );
SPECIAL ( high_dice );
SPECIAL ( janitor );
SPECIAL ( magic_user );
SPECIAL ( mayor );
SPECIAL ( pet_shops );
SPECIAL ( puff );
SPECIAL ( scorpion );
SPECIAL ( seven );
SPECIAL ( slots );
SPECIAL ( snake );
SPECIAL ( spider );
SPECIAL ( thief );
SPECIAL ( triples );
void sort_sub_data ( void );

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

#define BRONZE_TOKEN       3301
#define SILVER_TOKEN       3302

int calc_tp(Character *ch, int type) 
{
  if (type == 0) {
    if (REMORTS(ch) < 1)
      return 500;
    else if (REMORTS(ch) <= 4)
      return 2500;
    else if (REMORTS(ch) <= 10)
      return 5000;
    else
      return 7500;
  }
  if (REMORTS(ch) < 1)
    return 1;
  else if (REMORTS(ch) <= 4)
    return 5;
  else if (REMORTS(ch) <= 10)
    return 10;
  else
    return 15;
}

int find_tokens(Character *ch, int type, int take)
{
  struct obj_data *obj, *obj_next;
  int count = 0;

  for (obj = ch->carrying; obj; obj = obj_next) {
      obj_next = obj->next_content;
      if (GET_OBJ_VNUM(obj) == type) {
        count++;
        if (take) {
            obj_from_char(obj);
            extract_obj(obj);
            take--;
        }
      }
  }

  return count;

}

bool check_owner(Character *ch, struct obj_data *obj)
{
    if (obj->owner <= 0) {
        obj->owner = GET_IDNUM(ch);
        return TRUE;
    }

    if (obj->owner == GET_IDNUM(ch))
        return TRUE;

    return FALSE;
}

SPECIAL(antidt)
{
  struct obj_data *obj;
  int tokens;

  if (argument[0] == ' ')
    argument++;
  if (CMD_IS("trade")) {
    if (argument[0] == '\0') {
      ch->Send("You can either trade full protection or item protection:\r\n");
      ch->Send("trade full\r\n");
      ch->Send("trade <item name>\r\n");
      return 1;
    }

    if (!str_cmp(argument, "full")) {
        if (TRADEPOINTS(ch) < calc_tp(ch, 0)) {
          ch->Send("You do not have enough tradepoints for a full protection.\r\n");
          return 1;
        }
        TRADEPOINTS(ch) -= calc_tp(ch, 0);

        SET_BIT_AR(PLR_FLAGS(ch), PLR_ANTI_DT);
        act("An aura of protection surrounds you!", FALSE, ch, 0, 0, TO_CHAR);
        act("an aura of protection surrounds $n!", FALSE, ch, 0, 0, TO_ROOM);
        return 1;
    }
    else {
        if (!(obj = get_obj_in_list_vis(ch, argument, NULL, ch->carrying))) {
            ch->Send("You do not seem to be carrying any %s.\r\n", argument);
            return 1;
        }
        if (check_token(obj)) {
          ch->Send("You cannot protect tokens.\r\n");
          return 1;
        }

        if (TRADEPOINTS(ch) < calc_tp(ch, 0)/10) {
          ch->Send("You do not have enough tradepoints to protect your item.\r\n");
          return 1;
        }
     
        if (!check_owner(ch, obj)) {
           act("You are not the original owner of $p!", FALSE, ch, obj, 0, TO_CHAR);
           return 1;
        }
 
        TRADEPOINTS(ch) -= calc_tp(ch, 0)/10;
        SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_ANTI_DT);
        act("$p briefly glows bright red.", FALSE, ch, obj, 0, TO_CHAR);
        return 1;
    }
  }
  else if (CMD_IS("buy")) {
      if (!*argument) {
          ch->Send("You can either buy full protection or item protection:\r\n");
          ch->Send("buy full\r\n");
          ch->Send("buy <item name>\r\n");
          return 1;
      }
   
      if (!str_cmp(argument, "full")) {
          tokens = find_tokens(ch, SILVER_TOKEN, 0);
              if (tokens < calc_tp(ch, 1)) {
                  ch->Send("You do not have enough silver tokens!\r\n");
                  return 1;
              }
          find_tokens(ch, SILVER_TOKEN, calc_tp(ch, 1));
          SET_BIT_AR(PLR_FLAGS(ch), PLR_ANTI_DT);
          act("An aura of protection surrounds you!", FALSE, ch, 0, 0, TO_CHAR);
          act("an aura of protection surrounds $n!", FALSE, ch, 0, 0, TO_ROOM);
          return 1;
      }
      else {
          if (!(obj = get_obj_in_list_vis(ch, argument, NULL, ch->carrying))) {
              ch->Send("You do not seem to be carrying any %s.\r\n", argument);
              return 1;
          }  
          if (check_token(obj)) {
              ch->Send("You cannot protect tokens.\r\n");
              return 1;
          }
          tokens = find_tokens(ch, BRONZE_TOKEN, 0);
          if (tokens < calc_tp(ch, 1)) {
              ch->Send("You do not have enough bronze tokens!\r\n");
              return 1;
          }
          if (!check_owner(ch, obj)) {
             act("You are not the original owner of $p!", FALSE, ch, obj, 0, TO_CHAR);
             return 1;
          }
          find_tokens(ch, BRONZE_TOKEN, calc_tp(ch, 1));
          SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_ANTI_DT);
          act("$p briefly glows bright red.", FALSE, ch, obj, 0, TO_CHAR);
          return 1;
      }
  }
  else if (CMD_IS("list")) {
      ch->Send("For full protection, you need %d tradepoints or %d silver tokens.\r\n", calc_tp(ch, 0), calc_tp(ch, 1));
      ch->Send("For each item protection, you need %d tradepoints or %d bronze tokens.\r\n", calc_tp(ch, 0)/10, calc_tp(ch, 1));
      return 1;
  }

  return 0;
  

}

/* Clan deed boxes in each clan hall are fakes. They are just there so
   this spec_proc can run, transferring the PC to room 5 and make them
   look at the real box.                            
*/
SPECIAL(deed_box)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH], buf[MAX_STRING_LENGTH];
  struct clan_deed_type *cl;
  int i, clan_num;

  skip_spaces(&argument);
  two_arguments(argument, arg1, arg2);

  if (CMD_IS("look") && !strcmp(arg1, "box")) {
      ch->Send("You look examine the Ultimate Deed Box\r\n");
      ch->Send("Special Commands:\r\n");
      ch->Send("{cYdeed                - {cxlist all deeds claimed\r\n");
      ch->Send("{cYdeed clan <clan name>    - {cxlist all deeds claimed by clan\r\n");
      ch->Send("{cYdeed player <player name>  - {cxlist all deeds claimed by player\r\n");
      return TRUE;
  }

  if (strcmp(cmd_arg, "deed")) return FALSE;

  if (arg1[0] != '\0' && strcmp(arg1, "clan") && strcmp(arg1, "player")) {
      ch->Send("Type look box to see the correct commands\r\n");
      return TRUE;
  }

  if (arg1[0] != '\0' && arg2[0] == '\0') {
      ch->Send("You need to specify a name.\r\n");
      return TRUE;
  }

  if (arg1[0] != '\0' && !strcmp(arg1, "clan")) {
      clan_num = find_clan(arg2);
      if (clan_num < 0) {
          ch->Send("This clan does not exist!\r\n");
          return TRUE;
      }
  }

  DYN_DEFINE;
  *buf = '\0';
  DYN_CREATE;
  *dynbuf = 0;

  ch->Send("You look inside the Ultimate Deed Box:\r\n");

  for (i = 0; i < num_of_clans; i++) {
      if (!clan[i].deeds) continue;
      if (arg1[0] != '\0' && !strcmp(arg1, "clan") && i != clan_num) continue;
      if (arg1[0] == '\0' || !strcmp(arg1, "clan")) {
          snprintf(buf, sizeof(buf), "{cYClan: %s{cx\r\n", clan[i].name);
          DYN_RESIZE(buf);
      }
      for (cl = clan[i].deeds; cl; cl = cl->next) {
          if (arg1[0] != '\0' && !strcmp(arg1, "player") && 
          strncasecmp(cl->name, arg2, sizeof(arg2))) continue;
          snprintf(buf, sizeof(buf), "{cM%-40s  {cYClaimed by: %s{cx\r\n", zone_table[real_zone(cl->zone)].name, cl->name);
          DYN_RESIZE(buf);
      }
  }

  page_string(ch->desc, dynbuf, DYN_BUFFER);
  return TRUE;
}

SPECIAL(clan_deeds)
{
  struct obj_data *box = NULL;
  struct obj_data *deed = (struct obj_data *)me;
  char arg1[256], arg2[256];
  struct clan_deed_type *cd, *cl, *cl_next, *temp;
  int i = 0, j = 0;

  if (!CMD_IS("put"))
      return FALSE;

  argument = two_arguments(argument, arg1, arg2);

  if (strcmp(arg1, "deed") || strcmp(arg2, "box"))
      return FALSE;

  /* Check if there is a deed box here */
  for (box = IN_ROOM(ch)->contents; box; box = box->next_content)
      if (box->item_number == 8) break;

  if (!box) return FALSE;  // Maybe put in a different box?

  /* Lets now check if clan already has deed */
  i = find_clan_by_id(GET_CLAN(ch));
  if (i < 0) {
      ch->Send("You are not in any clan!\r\n");
      return TRUE;
  }
  for (cl = clan[i].deeds; cl; cl = cl_next) {
      cl_next = cl->next;
      if (is_same_zone(cl->zone, GET_OBJ_VAL(deed, 0))) {
          obj_from_char(deed);
          extract_obj(deed);
          ch->Send("Your clan already has claimed this deed.\r\n");
          return TRUE;
      }
  }
          
  /* Assume the clan does not have this deed, lets put the deed */
  /* in the box and give the clan the bonuses.                  */
  CREATE(cd, struct clan_deed_type, 1);
  cd->zone = GET_OBJ_VAL(deed, 0);
  cd->name = strdup(GET_NAME(ch));
  cd->next = clan[i].deeds;
  clan[i].deeds = cd;  

  for (j = 0; j < num_of_clans; j++) {
      if (j == i) continue;
      for (cl = clan[j].deeds; cl; cl = cl_next) {
          cl_next = cl->next;
          if (is_same_zone(cl->zone, GET_OBJ_VAL(deed, 0))) {
              REMOVE_FROM_LIST(cl, clan[j].deeds, next);
              free(cl->name);
              free(cl);
          }
      }
  }

  obj_from_char(deed);
  extract_obj(deed);
  ch->Send("You have now claimed a new deed for your clan!\r\n");
  save_clans();
  return TRUE;
}

SPECIAL ( dump )
{
	struct obj_data *k;
	gold_int value = 0;

	for ( k = IN_ROOM ( ch )->contents; k;
	        k = IN_ROOM ( ch )->contents )
	{
		act ( "$p vanishes in a puff of smoke!", FALSE, 0, k, 0, TO_ROOM );
		obj_from_room ( k );
		extract_obj ( k );
	}

	if ( !CMD_IS ( "drop" ) )
		return ( 0 );

	do_drop ( ch, argument, cmd, 0 );

	for ( k = IN_ROOM ( ch )->contents; k;
	        k = IN_ROOM ( ch )->contents )
	{
		act ( "$p vanishes in a puff of smoke!", FALSE, 0, k, 0, TO_ROOM );
		value += MAX ( 1, MIN ( 50, GET_OBJ_COST ( k ) / 10 ) );
		obj_from_room ( k );
		extract_obj ( k );
	}

	if ( value )
	{
		*ch << "You are awarded for outstanding performance.\r\n";
		act ( "$n has been awarded for being a good citizen.", TRUE, ch, 0, 0, TO_ROOM );

		if ( GET_LEVEL ( ch ) < 3 )
			gain_exp ( ch, value );
		else
			ch->Gold ( value, GOLD_HAND );
	}
	return ( 1 );
}


SPECIAL ( mayor )
{
	const char open_path[] =
	    "W3a3003b33000c111d0d111Oe333333Oe22c222112212111a1S.";
	const char close_path[] =
	    "W3a3003b33000c111d0d111CE333333CE22c222112212111a1S.";

	static const char *path = NULL;
	static int mindex;
	static bool move = FALSE;

	if ( !move )
	{
		if ( time_info.hours == 6 )
		{
			move = TRUE;
			path = open_path;
			mindex = 0;
		}
		else if ( time_info.hours == 20 )
		{
			move = TRUE;
			path = close_path;
			mindex = 0;
		}
	}
	if ( cmd || !move || ( GET_POS ( ch ) < POS_SLEEPING ) ||
	        ( GET_POS ( ch ) == POS_FIGHTING ) )
		return ( FALSE );

	switch ( path[mindex] )
	{
		case '0':
		case '1':
		case '2':
		case '3':
			perform_move ( ch, path[mindex] - '0', 1 );
			break;

		case 'W':
			GET_POS ( ch ) = POS_STANDING;
			act ( "$n awakens and groans loudly.", FALSE, ch, 0, 0, TO_ROOM );
			break;

		case 'S':
			GET_POS ( ch ) = POS_SLEEPING;
			act ( "$n lies down and instantly falls asleep.", FALSE, ch, 0, 0,
			      TO_ROOM );
			break;

		case 'a':
			act ( "$n says 'Hello Honey!'", FALSE, ch, 0, 0, TO_ROOM );
			act ( "$n smirks.", FALSE, ch, 0, 0, TO_ROOM );
			break;

		case 'b':
			act ( "$n says 'What a view!  I must get something done about that dump!'", FALSE, ch, 0, 0, TO_ROOM );
			break;

		case 'c':
			act ( "$n says 'Vandals!  Youngsters nowadays have no respect for anything!'", FALSE, ch, 0, 0, TO_ROOM );
			break;

		case 'd':
			act ( "$n says 'Good day, citizens!'", FALSE, ch, 0, 0, TO_ROOM );
			break;

		case 'e':
			act ( "$n says 'I hereby declare the bazaar open!'", FALSE, ch, 0, 0,
			      TO_ROOM );
			break;

		case 'E':
			act ( "$n says 'I hereby declare Midgaard closed!'", FALSE, ch, 0, 0,
			      TO_ROOM );
			break;

		case 'O':
			do_gen_door ( ch, ( char * ) "gate", 0, SCMD_UNLOCK );
			do_gen_door ( ch, ( char * ) "gate", 0, SCMD_OPEN );
			break;

		case 'C':
			do_gen_door ( ch, ( char * ) "gate", 0, SCMD_CLOSE );
			do_gen_door ( ch, ( char * ) "gate", 0, SCMD_LOCK );
			break;

		case '.':
			move = FALSE;
			break;

	}

	mindex++;
	return ( FALSE );
}


/* ********************************************************************
*  General special procedures for mobiles                             *
******************************************************************** */


void npc_steal ( Character *ch, Character *victim )
{
	gold_int gold;

	if ( IS_NPC ( victim ) )
		return;
	if ( GET_LEVEL ( victim ) >= LVL_IMMORT )
		return;

	if ( AWAKE ( victim ) && ( number ( 0, GET_LEVEL ( ch ) ) == 0 ) )
	{
		act ( "You discover that $n has $s hands in your wallet.", FALSE, ch,
		      0, victim, TO_VICT );
		act ( "$n tries to steal gold from $N.", TRUE, ch, 0, victim,
		      TO_NOTVICT );
	}
	else
	{
		/* Steal some gold coins */
		gold = ( ( victim->Gold ( 0, GOLD_HAND ) * number ( 1, 10 ) ) / 100 );
		if ( gold > 0 )
		{
			ch->Gold ( gold, GOLD_HAND );
			victim->Gold ( -gold, GOLD_HAND );
		}
	}
}

SPECIAL ( spider )
{
	if ( cmd )
		return ( FALSE );

	if ( GET_POS ( ch ) != POS_FIGHTING )
		return ( FALSE );

	if ( FIGHTING ( ch ) && ( FIGHTING ( ch )->in_room == IN_ROOM ( ch ) ) &&
	        ( number ( 0, 60 - GET_LEVEL ( ch ) ) == 0 ) )
	{
		act ( "$n bites $N!", 1, ch, 0, FIGHTING ( ch ), TO_NOTVICT );
		act ( "$n bites you!", 1, ch, 0, FIGHTING ( ch ), TO_VICT );
		call_magic ( ch, FIGHTING ( ch ), 0, 0, SPELL_POISON, GET_LEVEL ( ch ),
		             CAST_SPELL );
		return ( TRUE );
	}
	return ( FALSE );
}

SPECIAL ( scorpion )
{
	if ( cmd )
		return ( FALSE );

	if ( GET_POS ( ch ) != POS_FIGHTING )
		return ( FALSE );

	if ( FIGHTING ( ch ) && ( FIGHTING ( ch )->in_room == IN_ROOM ( ch ) ) &&
	        ( number ( 0, 62 - GET_LEVEL ( ch ) ) == 0 ) )
	{
		act ( "$n stings $N!", 1, ch, 0, FIGHTING ( ch ), TO_NOTVICT );
		act ( "$n stings you!", 1, ch, 0, FIGHTING ( ch ), TO_VICT );
		call_magic ( ch, FIGHTING ( ch ), 0, 0, SPELL_POISON_2, GET_LEVEL ( ch ),
		             CAST_SPELL );
		return ( TRUE );
	}
	return ( FALSE );
}

SPECIAL ( snake )
{
	if ( cmd )
		return ( FALSE );

	if ( GET_POS ( ch ) != POS_FIGHTING )
		return ( FALSE );

	if ( FIGHTING ( ch ) && ( FIGHTING ( ch )->in_room == IN_ROOM ( ch ) ) &&
	        ( number ( 0, 62 - GET_LEVEL ( ch ) ) == 0 ) )
	{
		act ( "$n bites $N!", 1, ch, 0, FIGHTING ( ch ), TO_NOTVICT );
		act ( "$n bites you!", 1, ch, 0, FIGHTING ( ch ), TO_VICT );
		call_magic ( ch, FIGHTING ( ch ), 0, 0, SPELL_POISON_3, GET_LEVEL ( ch ),
		             CAST_SPELL );
		return ( TRUE );
	}
	return ( FALSE );
}

SPECIAL ( thief )
{
	Character *cons;

	if ( cmd )
		return ( FALSE );

	if ( GET_POS ( ch ) != POS_STANDING )
		return ( FALSE );

	for ( cons = IN_ROOM ( ch )->people; cons; cons = cons->next_in_room )
		if ( !IS_NPC ( cons ) && ( GET_LEVEL ( cons ) < LVL_IMMORT )
		        && ( !number ( 0, 4 ) ) )
		{
			npc_steal ( ch, cons );
			return ( TRUE );
		}
	return ( FALSE );
}


SPECIAL ( magic_user )
{
	Character *vict;

	if ( cmd || GET_POS ( ch ) != POS_FIGHTING )
		return ( FALSE );

	/* pseudo-randomly choose someone in the room who is fighting me */
	for ( vict = IN_ROOM ( ch )->people; vict; vict = vict->next_in_room )
		if ( FIGHTING ( vict ) == ch && !number ( 0, 4 ) )
			break;

	/* if I didn't pick any of those, then just slam the guy I'm fighting */
	if ( vict == NULL && IN_ROOM ( FIGHTING ( ch ) ) == IN_ROOM ( ch ) )
		vict = FIGHTING ( ch );

	/* Hm...didn't pick anyone...I'll wait a round. */
	if ( vict == NULL )
		return ( TRUE );

	if ( ( GET_LEVEL ( ch ) > 13 ) && ( number ( 0, 10 ) == 0 ) )
		cast_spell ( ch, vict, NULL, 0, SPELL_SLEEP );

	if ( ( GET_LEVEL ( ch ) > 7 ) && ( number ( 0, 8 ) == 0 ) )
		cast_spell ( ch, vict, NULL, 0, SPELL_BLINDNESS );

	if ( ( GET_LEVEL ( ch ) > 12 ) && ( number ( 0, 12 ) == 0 ) )
	{
		if ( IS_EVIL ( ch ) )
			cast_spell ( ch, vict, NULL, 0, SPELL_ENERGY_DRAIN );
		else if ( IS_GOOD ( ch ) )
			cast_spell ( ch, vict, NULL, 0, SPELL_DISPEL_EVIL );
	}
	if ( number ( 0, 4 ) )
		return ( TRUE );

	switch ( GET_LEVEL ( ch ) )
	{
		case 4:
		case 5:
			cast_spell ( ch, vict, NULL, 0, SPELL_MAGIC_MISSILE );
			break;
		case 6:
		case 7:
			cast_spell ( ch, vict, NULL, 0, SPELL_CHILL_TOUCH );
			break;
		case 8:
		case 9:
			cast_spell ( ch, vict, NULL, 0, SPELL_BURNING_HANDS );
			break;
		case 10:
		case 11:
			cast_spell ( ch, vict, NULL, 0, SPELL_SHOCKING_GRASP );
			break;
		case 12:
		case 13:
			cast_spell ( ch, vict, NULL, 0, SPELL_LIGHTNING_BOLT );
			break;
		case 14:
		case 15:
		case 16:
		case 17:
			cast_spell ( ch, vict, NULL, 0, SPELL_COLOUR_SPRAY );
			break;
		default:
			cast_spell ( ch, vict, NULL, 0, SPELL_FIREBALL );
			break;
	}
	return ( TRUE );

}


/* ********************************************************************
*  Special procedures for mobiles                                      *
******************************************************************** */

SPECIAL ( guild_guard )
{
	int i;
	Character *guard = ( Character * ) me;
	const char *buf = "$N blocks every attempt you make to get past.";
	const char *buf2 = "$N blocks every attempt $n makes to get past.";

	if ( !IS_MOVE ( cmd ) || AFF_FLAGGED ( guard, AFF_BLIND ) )
		return ( FALSE );

	if ( GET_LEVEL ( ch ) >= LVL_IMMORT )
		return ( FALSE );

	for ( i = 0; guild_info[i][0] != -1; i++ )
	{
		if ( ( IS_NPC ( ch ) || GET_CLASS ( ch ) != guild_info[i][0] ) &&
		        GET_ROOM_VNUM ( IN_ROOM ( ch ) ) == guild_info[i][1] &&
		        cmd == guild_info[i][2] )
		{
			act ( buf, FALSE, ch, 0, guard, TO_CHAR );
			act ( buf2, FALSE, ch, 0, guard, TO_ROOM );
			return ( TRUE );
		}
	}

	return ( FALSE );
}



SPECIAL ( puff )
{
	if ( cmd )
		return ( 0 );

	return 0;

	switch ( number ( 0, 60 ) )
	{
		case 0:
			do_say ( ch, ( char * ) "My god!  Its full of stars!", 0, 0 );
			return ( 1 );
		case 1:
			do_say ( ch, ( char * ) "Howd all those fish get up here?", 0, 0 );
			return ( 1 );
		case 2:
			do_say ( ch, ( char * ) "Im a very female dragon.", 0, 0 );
			return ( 1 );
		case 3:
			do_say ( ch, ( char * ) "Ive got a peaceful, easy feeling.", 0, 0 );
			return ( 1 );
		default:
			return ( 0 );
	}
}



SPECIAL ( fido )
{

	struct obj_data *i, *temp, *next_obj;

	if ( cmd || !AWAKE ( ch ) )
		return ( FALSE );

	for ( i = IN_ROOM ( ch )->contents; i; i = i->next_content )
	{
		if ( IS_CORPSE ( i ) &&  ! ( IS_OBJ_STAT ( i, ITEM_PC_CORPSE ) ) )
		{
			act ( "$n savagely devours a corpse.", FALSE, ch, 0, 0, TO_ROOM );
			for ( temp = i->contains; temp; temp = next_obj )
			{
				next_obj = temp->next_content;
				obj_from_obj ( temp );
				obj_to_room ( temp, IN_ROOM ( ch ) );
			}
			extract_obj ( i );
			return ( TRUE );
		}
	}
	return ( FALSE );
}



SPECIAL ( janitor )
{
	struct obj_data *i;

	if ( cmd || !AWAKE ( ch ) )
		return ( FALSE );

	for ( i = IN_ROOM ( ch )->contents; i; i = i->next_content )
	{
		if ( !CAN_WEAR ( i, ITEM_WEAR_TAKE ) )
			continue;
		if ( GET_OBJ_TYPE ( i ) != ITEM_DRINKCON && GET_OBJ_COST ( i ) >= 15 )
			continue;
		if ( ( IS_OBJ_STAT ( i, ITEM_PC_CORPSE ) ) )
			continue;
		act ( "$n picks up some trash.", FALSE, ch, 0, 0, TO_ROOM );
		obj_from_room ( i );
		obj_to_char ( i, ch );
		return ( TRUE );
	}

	return ( FALSE );
}


SPECIAL ( cityguard )
{
	Character *tch, *evil;
	int max_evil;

	if ( cmd || !AWAKE ( ch ) || FIGHTING ( ch ) )
		return ( FALSE );

	max_evil = 1000;
	evil = 0;

	for ( tch = IN_ROOM ( ch )->people; tch; tch = tch->next_in_room )
	{
		if ( !IS_NPC ( tch ) && CAN_SEE ( ch, tch )
		        && PLR_FLAGGED ( tch, PLR_KILLER ) )
		{
			act ( "$n screams 'HEY!!!  You're one of those PLAYER KILLERS!!!!!!'", FALSE, ch, 0, 0, TO_ROOM );
			start_fighting ( ch, tch );
			return ( TRUE );
		}
	}

	for ( tch = IN_ROOM ( ch )->people; tch; tch = tch->next_in_room )
	{
		if ( !IS_NPC ( tch ) && CAN_SEE ( ch, tch )
		        && PLR_FLAGGED ( tch, PLR_THIEF ) )
		{
			act ( "$n screams 'HEY!!!  You're one of those PLAYER THIEVES!!!!!!'", FALSE, ch, 0, 0, TO_ROOM );
			start_fighting ( ch, tch );
			return ( TRUE );
		}
	}

	for ( tch = IN_ROOM ( ch )->people; tch; tch = tch->next_in_room )
	{
		if ( CAN_SEE ( ch, tch ) && FIGHTING ( tch ) )
		{
			if ( ( GET_ALIGNMENT ( tch ) < max_evil ) &&
			        ( IS_NPC ( tch ) || IS_NPC ( FIGHTING ( tch ) ) ) )
			{
				max_evil = GET_ALIGNMENT ( tch );
				evil = tch;
			}
		}
	}

	if ( evil && ( GET_ALIGNMENT ( FIGHTING ( evil ) ) >= 0 ) )
	{
		act ( "$n screams 'PROTECT THE INNOCENT!  BANZAI!  CHARGE!  ARARARAGGGHH!'", FALSE, ch, 0, 0, TO_ROOM );
		start_fighting ( ch, evil );
		return ( TRUE );
	}
	return ( FALSE );
}


#define PET_PRICE(pet) ((gold_int)(GET_LEVEL(pet) * (MOB_TIER(pet)+1) * GET_MAX_HIT(pet)))
#define MAX_PETS 6

SPECIAL ( pet_shops )
{
	char buf[MAX_INPUT_LENGTH], pet_name[256];
	room_rnum pet_room;
	Character *pet, *k, *tch;
	int num_of_pets = 0;
	const char *simple_class_name ( Character *ch );
	const char *race_name ( Character *ch );


	struct follow_type *f, *f_next;


	pet_room = world_vnum[IN_ROOM ( ch )->number + 1];
	if ( pet_room == NULL )
		return 0;

	if ( CMD_IS ( "list" ) )
	{
		*ch << "To buy a pet and name it, use: \r\n";
		*ch << "buy <pet> <the name you give it>\r\n";
		*ch << "Available pets are:\r\n";
		for ( pet = pet_room->people; pet; pet = pet->next_in_room )
		{
			if ( IS_NPC ( pet ) )
			{
				ch->Send ( "%8lld - %-20s (Class: %s, Race: %s)\r\n", PET_PRICE ( pet ),
				           GET_NAME ( pet ), simple_class_name ( pet ), race_name ( pet ) );
			}
		}
		return ( TRUE );
	}
	else if ( CMD_IS ( "buy" ) )
	{

		if ( IS_NPC ( ch ) )
		{
			*ch << "You are a mob: No pets for you!";
			return ( TRUE );
		}

		two_arguments ( argument, buf, pet_name );

		if ( ! ( pet = get_char_room ( buf, NULL, pet_room ) ) )
		{
			*ch << "There is no such pet!\r\n";
			return ( TRUE );
		}
		if ( ch->master != NULL )
			k = ch->master;
		else
			k = ch;
		for ( f = k->followers; f; f = f_next )
		{
			f_next = f->next;
			tch = f->follower;
			if ( !AFF_FLAGGED ( tch, AFF_CHARM ) )
				continue;
			num_of_pets++;
		}
		if ( num_of_pets > MAX_PETS )
		{
			*ch << "You already have the maximum number of that kind of pet.\r\n";
			return ( TRUE );
		}
		if ( ch->Gold ( 0, GOLD_HAND ) < PET_PRICE ( pet ) )
		{
			*ch << "You don't have enough gold!\r\n";
			return ( TRUE );
		}
		if ( !can_have_follower ( ch, pet ) )
		{
			*ch << "You can't control that many charmed followers!\r\n";
			return TRUE;
		}
		ch->Gold ( -PET_PRICE ( pet ), GOLD_HAND );

		pet = read_mobile ( GET_MOB_VNUM ( pet ) );
		GET_EXP ( pet ) = 0;
		SET_BIT_AR ( AFF_FLAGS ( pet ), AFF_CHARM );
		pet->Gold ( -GET_GOLD ( pet ), GOLD_HAND );


		if ( *pet_name )
		{
			snprintf ( buf, sizeof ( buf ), "%s %s", pet->player.name, pet_name );
			/* free(pet->player.name); don't free the prototype! */
			pet->player.name = strdup ( buf );

			snprintf ( buf, sizeof ( buf ),
			           "%sA small sign on a chain around the neck says 'My name is %s.\r\n",
			           pet->player.description, pet_name );
			/* free(pet->player.description); don't free the prototype! */
			pet->player.description = strdup ( buf );

			snprintf ( buf, sizeof ( buf ), "%s named %s owned by %s stands here.\r\n",
			           pet->player.short_descr, pet_name, GET_NAME ( ch ) );
			pet->player.long_descr = strdup ( buf );
		}
		else
		{
			snprintf ( buf, sizeof ( buf ), "%s owned by %s stands here.\r\n",
			           pet->player.short_descr, GET_NAME ( ch ) );
			pet->player.long_descr = strdup ( buf );
		}
		char_to_room ( pet, IN_ROOM ( ch ) );
		add_follower ( pet, ch );
		SET_BIT_AR ( AFF_FLAGS ( pet ), AFF_GROUP );
		load_mtrigger ( pet );

		/* Be certain that pets can't get/carry/use/wield/wear items */
		GET_GOLD ( pet ) = 0;
		GET_EXP ( pet ) = 0;

		*ch << "May you enjoy your pet.\r\n";
		act ( "$n buys $N as a pet.", FALSE, ch, 0, pet, TO_ROOM );

		return ( 1 );
	}
	/* All commands except list and buy */
	return ( 0 );
}


/* ********************************************************************
*  Special procedures for objects                                     *
******************************************************************** */


SPECIAL ( bank )
{
	gold_int amount = 0;
	int all = !strcmp ( argument, "all" );

	if ( CMD_IS ( "balance" ) )
	{
		if ( GET_BANK_GOLD ( ch ) > 0 )
			ch->Send ( "Your current balance is %lld coins.\r\n",GET_BANK_GOLD ( ch ) );
		else
			ch->Send ( "You currently have no money deposited.\r\n" );
		return ( 1 );
	}
	else if ( CMD_IS ( "deposit" ) )
	{
		if ( !all )
		{
			if ( atoll ( argument ) <= 0 )
			{
				*ch << "How much do you want to deposit?\r\n";
				return ( 1 );

			}
			else
				amount = atoll ( argument );
		}
		else
			amount = ch->Gold ( 0, GOLD_HAND );

		if ( ch->Gold ( 0, GOLD_HAND ) < amount )
		{
			*ch << "You don't have that many coins!\r\n";
			return ( 1 );
		}
		if ( ch->Gold ( 0, GOLD_BANK ) + amount > 100000000 )
			ch->Send ( "With bank accounts with more then 100mil a 10 percent fee is charged on withdrawal. Thank you.\r\n" );
		ch->Gold ( -amount, GOLD_HAND );
		ch->Gold ( amount, GOLD_BANK );
		ch->Send ( "You deposit %lld coins.\r\n", amount );
		act ( "$n makes a bank transaction.", TRUE, ch, 0, FALSE, TO_ROOM );
		return ( 1 );
	}
	else if ( CMD_IS ( "withdraw" ) )
	{
		if ( !all )
		{
			if ( ( amount = atoll ( argument ) ) <= 0 )
			{
				*ch << "How much do you want to withdraw?\r\n";
				return ( 1 );
			}
		}
		else
			amount = ch->Gold ( 0, GOLD_BANK );
		if ( ch->Gold ( 0, GOLD_BANK ) < amount )
		{
			*ch << "You don't have that many coins deposited!\r\n";
			return ( 1 );
		}
		if ( ch->Gold ( 0, GOLD_BANK ) > 100000000 )
		{
			ch->Send ( "Because of your huge bank investment, and the untimely withdraw,\r\n"
			           "You are charged bank fees summing to 10 percent of the amount.\r\n" );
			if ( ( ch->Gold ( 0, GOLD_BANK )- ( amount + 1 ) ) < ( amount/10 ) )
			{
				ch->Send ( "Which you can't afford. Please withdraw a smaller amount.\r\n" );
				return ( 1 );
			}
			ch->Gold ( - ( ( amount/10 ) +1 ), GOLD_BANK );
		}
		ch->Gold ( amount, GOLD_HAND );
		ch->Gold ( -amount, GOLD_BANK );
		ch->Send ( "You withdraw %lld coins.\r\n", amount );
		act ( "$n makes a bank transaction.", TRUE, ch, 0, FALSE, TO_ROOM );
		return ( 1 );
	}
	else
		return ( 0 );
}

/* This special procedure makes a mob into a 'rent-a-cleric', who sells spells
   by the sea shore... uuh, maybe not.  Anyway, the mob will also cast certain
   spells on low-level characters in the room for free.
   By:  Wyatt Bode	Date:  April, 1996
*/
SPECIAL ( cleric )
{
	int i;
	char buf[MAX_STRING_LENGTH];
	Character *vict;
	struct price_info
	{
		short int number;
		char name[25];
		gold_int price;
	}
	prices[] =
	{
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

	if ( CMD_IS ( "heal" ) )
	{
		argument = one_argument ( argument, buf );

		if ( GET_POS ( ch ) == POS_FIGHTING )
			return TRUE;

		if ( *buf )
		{
			for ( i = 0; prices[i].number > SPELL_RESERVED_DBC; i++ )
			{
				if ( is_abbrev ( buf, prices[i].name ) )
				{
					if ( ch->Gold ( 0, GOLD_HAND ) < prices[i].price )
					{
						act ( "$n tells you, 'You don't have enough gold for that spell!'", FALSE, ( Character * ) me, 0, ch, TO_VICT );
						return TRUE;
					}
					else
					{

						act ( "$N gives $n some money.",  FALSE, ( Character * ) me, 0, ch, TO_NOTVICT );
						ch->Send ( "You give %s %lld coins.\r\n", GET_NAME ( ( Character * ) me ), prices[i].price );
						ch->Gold ( -prices[i].price, GOLD_HAND );
						( ( Character * ) me )->Gold ( prices[i].price, GOLD_HAND );
						cast_spell ( ( Character * ) me, ch, NULL, 0, prices[i].number );
						return TRUE;

					}
				}
			}
			act ( "$n tells you, 'I do not know of that spell!"
			      "  Type 'heal' for a list.'", FALSE,
			      ( Character * ) me, 0, ch, TO_VICT );

			return TRUE;
		}
		else
		{
			act ( "$n tells you, 'Here is a listing of the prices for my services.'", FALSE, ( Character * ) me, 0, ch, TO_VICT );
			for ( i = 0; prices[i].number > SPELL_RESERVED_DBC; i++ )
			{
				ch->Send ( "{cc%-15s {cg- {cy%lld{c0\r\n", prices[i].name, prices[i].price );
			}
			return TRUE;
		}
	}

	if ( cmd )
		return FALSE;

	/* pseudo-randomly choose someone in the room */
	for ( vict = IN_ROOM ( ch )->people; vict; vict = vict->next_in_room )
		if ( !number ( 0, 3 ) )
			break;

	/* change the level at the end of the next line to control free spells */
	if ( vict == NULL || IS_NPC ( vict ) || ( GET_LEVEL ( vict ) > 10 ) )
		return FALSE;

	switch ( number ( 1, GET_LEVEL ( vict ) ) )
	{
		case 1:
			cast_spell ( ch, vict, NULL, 0, SPELL_CURE_LIGHT );
			break;
		case 2:
			cast_spell ( ch, vict, NULL, 0, SPELL_BLESS );
			break;
		case 3:
			cast_spell ( ch, vict, NULL, 0, SPELL_ARMOR );
			break;
		case 4:
			cast_spell ( ch, vict, NULL, 0, SPELL_CURE_LIGHT );
			break;
		case 5:
			cast_spell ( ch, vict, NULL, 0, SPELL_BLESS );
			break;
		case 6:
			cast_spell ( ch, vict, NULL, 0, SPELL_CURE_CRITIC );
			break;
		case 7:
			cast_spell ( ch, vict, NULL, 0, SPELL_ARMOR );
			break;
		case 8:
			cast_spell ( ch, vict, NULL, 0, SPELL_CURE_CRITIC );
			break;
		case 9:
			cast_spell ( ch, vict, NULL, 0, SPELL_ARMOR );
			break;
		case 10:
			/* special wacky thing, your mileage may vary */
			act ( "$n utters the words, 'energizer'.", TRUE, ch, 0, vict, TO_ROOM );
			act ( "You feel invigorated!", FALSE, ch, 0, vict, TO_VICT );
			alter_move ( ch,
			             -MIN ( GET_MAX_MOVE ( vict ),
			                    MAX ( ( GET_MOVE ( vict ) + 10 ), number ( 50, 200 ) ) ) );
			break;
	}
	return TRUE;
}

/* corpse recovery utility
 * written by osiris for wintermute 10/11/98
 */
ACMD ( do_recover )
{
	int num = 0, found = 0;
	gold_int amt;
	struct obj_data *obj = NULL;
	int remorts = MIN(REMORTS(ch), 50);
	void perform_meld ( Character *ch, OBJ_DATA *corpse );

	struct corpse_list_data *temp = NULL, *tnext;

	/* check to see if the character is at the altar */
	if ( IN_ROOM ( ch )->number != 3083 )
	{
		*ch << "You can only recover in the crypt east and down from the Recall Point!\r\n";
		return;
	}


	/* determine the cost per recovery */
	if ( GET_LEVEL ( ch ) <= 20 )
		amt = 100000;
	else
		amt = GET_LEVEL ( ch ) * 50000;

	amt += remorts * 500000;
	/* charge the character */
	if ( ch->Gold ( 0, GOLD_BANK ) < amt )
	{
		*ch << "Your bank account can not afford this service!\r\n";
		return;
	}
	/* search the world for the corpse, if it still exists */
	for ( num = 0, temp = corpse_list; temp; temp = tnext )
	{
		tnext = temp->next;
		obj = temp->corpse;

		if ( GET_OBJ_VAL ( obj, 0 ) != get_pidx_from_name ( ch ) )
			continue;
		if ( !obj->contains )
		{
			ch->Send ( "An empty corpse is found. Not Recovered. Removed From List.\r\n" );
			remove_corpse_from_list ( obj );
			continue;
		}
		else if ( obj->carried_by )
		{
			ch->Send ( "Your corpse was found being carried by %s and can't be recovered.\r\n", GET_NAME ( obj->carried_by ) );
			continue;
			remove_corpse_from_list ( obj );
		}
		if ( IN_ROOM ( obj ) == NULL )
			continue;


		found = 1;

		obj_from_room ( obj );
		obj_to_room ( obj, IN_ROOM ( ch ) );
		perform_meld ( ch, obj );
	}

	if ( !found )
		*ch << "Your corpse can not be found. Sorry!\r\n";
	else
	{

		ch->Send ( "You are charged %lld gold for the recovery.\r\n",amt );
		ch->Gold ( -amt, GOLD_BANK );
	}
}

SPECIAL ( guard_white )
{
	char buf[MAX_STRING_LENGTH];
	Character *victim;
	Character *ech;
	char *crime;
	int max_evil;

	if ( GET_POS ( ch ) < POS_SLEEPING || FIGHTING ( ch ) != NULL || !IS_NPC ( ch ) )
		return FALSE;

	max_evil = 300;
	ech = NULL;
	crime = ( char * ) "";

	for ( victim = IN_ROOM ( ch )->people; victim != NULL;
	        victim = victim->next_in_room )
	{
		if ( !IS_NPC ( victim ) && PLR_FLAGGED ( victim, PLR_KILLER ) )
		{
			crime = ( char * ) "KILLER";
			break;
		}

		if ( !IS_NPC ( victim ) && PLR_FLAGGED ( victim, PLR_THIEF ) )
		{
			crime = ( char * ) "THIEF";
			break;
		}

		if ( FIGHTING ( victim ) != NULL
		        && FIGHTING ( victim ) != ch
		        && GET_ALIGNMENT ( victim ) < max_evil )
		{
			max_evil = GET_ALIGNMENT ( victim );
			ech = victim;
		}
	}

	if ( victim != NULL )
	{
		snprintf ( buf, sizeof ( buf ), "%s is a %s!  How DARE you come to the Temple!!!!",
		           GET_NAME ( victim ), crime );
		do_say ( ch, buf, 0, 0 );
		start_fighting ( ch, victim );
		return TRUE;
	}

	if ( ech != NULL )
	{
		act ( "$n screams ' Now you DIE you Bastard!!!!",
		      FALSE, ch, NULL, NULL, TO_ROOM );
		start_fighting ( ch, ech );
		return TRUE;
	}

	return FALSE;
}

SPECIAL ( guard_black )
{
	char buf[MAX_STRING_LENGTH];
	Character *victim;
	Character *ech;
	char *crime;
	int max_good;

	if ( GET_POS ( ch ) < POS_SLEEPING || FIGHTING ( ch ) != NULL || !IS_NPC ( ch ) )
		return FALSE;

	max_good = -300;
	ech = NULL;
	crime = ( char * ) "";

	for ( victim = IN_ROOM ( ch )->people; victim != NULL;
	        victim = victim->next_in_room )
	{
		if ( !IS_NPC ( victim ) && PLR_FLAGGED ( victim, PLR_KILLER ) )
		{
			crime = ( char * ) "KILLER";
			break;
		}

		if ( !IS_NPC ( victim ) && PLR_FLAGGED ( victim, PLR_THIEF ) )
		{
			crime = ( char * ) "THIEF";
			break;
		}

		if ( FIGHTING ( victim ) != NULL
		        && FIGHTING ( victim ) != ch
		        && GET_ALIGNMENT ( victim ) < max_good )
		{
			max_good = GET_ALIGNMENT ( victim );
			ech = victim;
		}
	}

	if ( victim != NULL )
	{
		snprintf ( buf, sizeof ( buf ), "%s is a %s!  How DARE you come to the Temple!!!!",
		           GET_NAME ( victim ), crime );
		do_say ( ch, buf, 0, 0 );
		start_fighting ( ch, victim );
		return TRUE;
	}

	if ( ech != NULL )
	{
		act ( "$n screams ' Now you DIE you Bastard!!!!",
		      FALSE, ch, NULL, NULL, TO_ROOM );
		start_fighting ( ch, ech );
		return TRUE;
	}

	return FALSE;
}

SPECIAL ( bottle )
{
	struct obj_data *obj = ( struct obj_data * ) me;
	Character *mob;
	ACMD ( do_hit );

	if ( CMD_IS ( "open" ) )
	{
		skip_spaces ( &argument );
		if ( isname ( argument, obj->name ) && CAN_SEE_OBJ ( ch, obj )
		        && GET_OBJ_VAL ( obj, 1 ) != 0 )
		{
			if ( GET_OBJ_VNUM ( obj ) == 216 )
			{
				GET_OBJ_VAL ( obj, 1 ) = 0;	// open the object
				mob = read_mobile ( 204 );
				act ( "As you open the bottle, $N jumps out and begins biting you!", FALSE, ch, 0, mob, TO_CHAR );
				act ( "As $n opens the bottle, $N jumps out and begins biting $m!", FALSE, ch, 0, mob, TO_ROOM );
				act ( "Going into a blood frenzy, $E attacks you!",
				      FALSE, ch, 0, mob, TO_CHAR );
				act ( "Going into a blood frenzy, $E attacks $m!", FALSE, ch, 0, mob, TO_ROOM );	/* Ahh, flavor text. */
				GET_POS ( ch ) = POS_SITTING;	/* Bash, essentially. */
				WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );	/* Please allow 2 to 3 tics for delivery. */
				char_to_room ( mob, IN_ROOM ( ch ) );
				start_fighting ( mob, ch );	// Start the mob fighting the player. Hiiiiyyaaa! */
				return TRUE;
			}
			else if ( GET_OBJ_VNUM ( obj ) == 1144 )
			{
				GET_OBJ_VAL ( obj, 1 ) = 0;	// open the bottle
				mob = read_mobile ( 1163 );
				act ( "$N crawls out of the box and stings your hand!",
				      FALSE, ch, 0, mob, TO_CHAR );
				act ( "$N crawls out of the box and stings $n on the hand.",
				      FALSE, ch, 0, mob, TO_ROOM );
				GET_POS ( ch ) = POS_SITTING;
				WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );
				char_to_room ( mob, IN_ROOM ( ch ) );
				start_fighting ( mob, ch );
				return TRUE;
			}
			else if ( GET_OBJ_VNUM ( obj ) == 7563 )
			{
				GET_OBJ_VAL ( obj, 1 ) = 0;	// open the bottle
				mob = read_mobile ( 7557 );
				act ( "$N crawls out of the box and stings your hand!",
				      FALSE, ch, 0, mob, TO_CHAR );
				act ( "$N crawls out of the box and stings $n on the hand.",
				      FALSE, ch, 0, mob, TO_ROOM );
				GET_POS ( ch ) = POS_SITTING;
				WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );
				char_to_room ( mob, IN_ROOM ( ch ) );
				start_fighting ( mob, ch );
				return TRUE;
			}
			else if ( GET_OBJ_VNUM ( obj ) == 1144 )
			{
				GET_OBJ_VAL ( obj, 1 ) = 0;	// open the bottle
				mob = read_mobile ( 1163 );
				act ( "$N crawls out of the box and stings your hand!",
				      FALSE, ch, 0, mob, TO_CHAR );
				act ( "$N crawls out of the box and stings $n on the hand.",
				      FALSE, ch, 0, mob, TO_ROOM );
				GET_POS ( ch ) = POS_SITTING;
				WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );
				char_to_room ( mob, IN_ROOM ( ch ) );
				start_fighting ( mob, ch );
				return TRUE;
			}
			else if ( GET_OBJ_VNUM ( obj ) == 11245 )
			{
				GET_OBJ_VAL ( obj, 1 ) = 0;
				mob = read_mobile ( 11229 );
				act ( "The vault guard rushes into the room, with his great long sword, ready to strike.", FALSE, ch, 0, mob, TO_ROOM );
				act ( "The vault guard rushes into the room, with his great long sword, ready to attack you.", FALSE, ch, 0, mob, TO_VICT );
				GET_POS ( ch ) = POS_SITTING;
				WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );
				char_to_room ( mob, IN_ROOM ( ch ) );
				start_fighting ( mob, ch );
				return TRUE;
			}
			else if ( GET_OBJ_VNUM ( obj ) == 11244 )
			{
				GET_OBJ_VAL ( obj, 1 ) = 0;
				mob = read_mobile ( 11096 );
				act ( "The ghost of the fallen warrior emerges from hell to seek his revenge!", FALSE, ch, 0, mob, TO_VICT );
				act ( "The ghost of the fallen warrior emerges from hell to seek his revenge!", FALSE, ch, 0, mob, TO_ROOM );
				GET_POS ( ch ) = POS_SITTING;
				WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );
				char_to_room ( mob, IN_ROOM ( ch ) );
				start_fighting ( mob, ch );
				return TRUE;
			}
		}
	}
	return FALSE;
}

SPECIAL ( door_down )
{
	int door = -1;
	char type[MAX_INPUT_LENGTH], dir[MAX_INPUT_LENGTH];
	struct obj_data *obj = NULL;
	Character *victim = NULL;
	Character *mob;

	if ( CMD_IS ( "open" ) )
	{
		skip_spaces ( &argument );
		two_arguments ( argument, type, dir );
		if ( isname ( type, "trapdoor" ) )
		{
			// vnum = real_mobile(number(2300, 2302));
			/* This is just to have it random pick between the three vampires
			   using their vnums. You should probably change it to the vnum
			   (or random vnums) you want it to load. */
			if ( !generic_find
			        ( type, FIND_OBJ_INV | FIND_OBJ_ROOM, ch, &victim, &obj ) )
				door = find_door ( ch, type, dir, cmd_door[SCMD_OPEN] );
			if ( EXIT_FLAGGED ( EXIT ( ch, door ), EX_CLOSED ) )
			{
				do_doorcmd ( ch, obj, door, SCMD_OPEN );
				mob = read_mobile ( 204 );
				act ( "As you open the trapdoor, $N jumps out and begins biting you!", FALSE, ch, 0, mob, TO_CHAR );
				act ( "As $n opens the trapdoor, $N jumps out and begins biting $m!", FALSE, ch, 0, mob, TO_ROOM );
				act ( "Going into a blood frenzy, $E attacks you!", FALSE,
				      ch, 0, mob, TO_CHAR );
				act ( "Going into a blood frenzy, $E attacks $m!", FALSE, ch, 0, mob, TO_ROOM );	/* Ahh, flavor text. */
				GET_POS ( ch ) = POS_SITTING;	/* Bash, essentially. */
				WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );	/* Please allow 2 to 3 tics for delivery. */
				char_to_room ( mob, IN_ROOM ( ch ) );
				start_fighting ( mob, ch );	// Start the mob fighting the player. Hiiiiyyaaa! */
				return TRUE;
			}
		}
	}
	return FALSE;
}

SPECIAL ( door_down_7377 )
{
	int door = -1;
	char type[MAX_INPUT_LENGTH], dir[MAX_INPUT_LENGTH];
	struct obj_data *obj = NULL;
	Character *victim = NULL;
	Character *mob;


	if ( CMD_IS ( "open" ) )
	{
		skip_spaces ( &argument );
		two_arguments ( argument, type, dir );
		if ( isname ( type, "lid" ) )
		{
			// vnum = real_mobile(number(2300, 2302));
			/* This is just to have it random pick between the three vampires
			   using their vnums. You should probably change it to the vnum
			   (or random vnums) you want it to load. */
			if ( !generic_find
			        ( type, FIND_OBJ_INV | FIND_OBJ_ROOM, ch, &victim, &obj ) )
				door = find_door ( ch, type, dir, cmd_door[SCMD_OPEN] );
			if ( EXIT_FLAGGED ( EXIT ( ch, door ), EX_CLOSED ) )
			{
				do_doorcmd ( ch, obj, door, SCMD_OPEN );
				mob = read_mobile ( 7376 );
				act ( "A green, slimy THING slithers out of the toilet and bites off your... whatever!", FALSE, ch, 0, mob, TO_CHAR );
				act ( "As $n opens the lid, a green, slimy THING slithers out of the toilet and bites $m. Ouch!", FALSE, ch, 0, mob, TO_ROOM );
				act ( "You feel {cWDIMINISHED!{c0", FALSE, ch, 0, mob,
				      TO_CHAR );
				act ( "$n seems to have diminished!", FALSE, ch, 0, mob, TO_ROOM );	/* Ahh, flavor text. */
				GET_POS ( ch ) = POS_SITTING;	/* Bash, essentially. */
				WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );	/* Please allow 2 to 3 tics for delivery. */
				char_to_room ( mob, IN_ROOM ( ch ) );
				start_fighting ( mob, ch );	// Start the mob fighting the player. Hiiiiyyaaa! */
				return TRUE;
			}
		}
	}
	return FALSE;
}


SPECIAL ( triples )
{
	int bet;
	char buf[256], buf2[10];
	Character *mob = ( Character * ) me;

	if ( CMD_IS ( "bet" ) )
	{
		two_arguments ( argument, buf, buf2 );

		if ( !*buf || !*buf2 )
		{
			*ch << "bet <upper|lower|triple> <amt>.\r\n";
			return TRUE;
		}

		bet = atoi ( buf2 );
		play_triples ( ch, mob, buf, bet );
		return TRUE;
	}

	return FALSE;
}

SPECIAL ( slots )
{
	if ( CMD_IS ( "pull" ) )
	{
		play_slots ( ch );
		return TRUE;
	}

	return FALSE;
}

SPECIAL ( high_dice )
{
	int bet;
	char buf[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];
	Character *mob = ( Character * ) me;

	if ( CMD_IS ( "bet" ) )
	{
		two_arguments ( argument, buf, buf2 );

		if ( !*buf )
		{
			*ch << "bet <amt>.\r\n";
			return TRUE;
		}

		bet = atoi ( buf );
		play_high_dice ( ch, mob, bet );
		return TRUE;
	}

	return FALSE;
}

SPECIAL ( seven )
{
	int bet;
	char buf[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];
	;
	Character *mob = ( Character * ) me;

	if ( CMD_IS ( "bet" ) )
	{
		two_arguments ( argument, buf, buf2 );

		if ( !*buf || !*buf2 )
		{
			*ch << "bet <over|under|seven> <amt>.\r\n";
			return TRUE;
		}

		bet = atoi ( buf2 );
		play_seven ( ch, mob, buf, bet );
		return TRUE;
	}

	return FALSE;
}

SPECIAL ( craps )
{
	int bet;
	char buf[MAX_INPUT_LENGTH];
	Character *mob = ( Character * ) me;

	if ( CMD_IS ( "bet" ) )
	{
		one_argument ( argument, buf );

		if ( !*buf )
		{
			*ch << "bet <amt>.\r\n";
			return TRUE;
		}

		bet = atoi ( buf );
		play_craps ( ch, mob, bet );
		return TRUE;
	}

	return FALSE;
}

/* Dragon's breath procedures */
SPECIAL ( dragon_fire )
{
	Character *dragon = ( Character * ) me;

	/* I don't know what 'cmd' is but we never do anything if we don't breathe
	 * fire unless we're fighting.
	 */
	if ( cmd || GET_POS ( ch ) != POS_FIGHTING )
		return FALSE;

	/* Only breathe fire 20% of the time */
	if ( number ( 0, 4 ) )
		return FALSE;

	/* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
	 * breath so we could have tougher dragons.  Right now, it does damage
	 * equal to a fireball in all cases.
	 */
	/* call_magic(ch, NULL, NULL, SPELL_FIRE_BREATH, 0, CAST_BREATH); */
	damage ( dragon, FIGHTING ( dragon ), dice ( GET_LEVEL ( dragon ), GET_LEVEL ( dragon ) ),SPELL_FIRE_BREATH );

	/* If you use the damage call, you don't need the spell, but if you use
	 * the spell, you should add the no_magic room information below.
	 */
	return TRUE;

}


SPECIAL ( dragon_gas )
{
	Character *dragon = ( Character * ) me;

	/* I don't know what 'cmd' is but we never do anything if we don't breathe
	 * gas unless we're fighting.
	 */
	if ( cmd || GET_POS ( ch ) != POS_FIGHTING )
		return FALSE;

	/* Only breathe gas 20% of the time */
	if ( number ( 0, 4 ) )
		return FALSE;

	/* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
	 * breath so we could have tougher dragons.  Right now, it does damage
	 * equal to a fireball in all cases.
	 */
	/* call_magic(ch, NULL, NULL, SPELL_GAS_BREATH, 0, CAST_BREATH); */
	damage ( dragon, FIGHTING ( dragon ), dice ( GET_LEVEL ( dragon ), GET_LEVEL ( dragon ) ),
	         SPELL_GAS_BREATH );

	/* If you use the damage call, you don't need the spell, but if you use
	 * the spell, you should add the no_magic room information below.
	 */

	return TRUE;

}


SPECIAL ( dragon_frost )
{
	Character *dragon = ( Character * ) me;

	/* I don't know what 'cmd' is but we never do anything if we don't breathe
	 * frost unless we're fighting.
	 */
	if ( cmd || GET_POS ( ch ) != POS_FIGHTING )
		return FALSE;

	/* Only breathe frost 20% of the time */
	if ( number ( 0, 4 ) )
		return FALSE;

	/* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
	 * breath so we could have tougher dragons.  Right now, it does damage
	 * equal to a fireball in all cases.
	 */
	/* call_magic(ch, NULL, NULL, SPELL_FROST_BREATH, 0, CAST_BREATH); */
	damage ( dragon, FIGHTING ( dragon ), dice ( GET_LEVEL ( dragon ), GET_LEVEL ( dragon ) ),
	         SPELL_FROST_BREATH );
	/* If you use the damage call, you don't need the spell, but if you use
	 * the spell, you should add the no_magic room information below.
	 */

	return TRUE;

}


SPECIAL ( dragon_acid )
{
	Character *dragon = ( Character * ) me;

	/* I don't know what 'cmd' is but we never do anything if we don't breathe
	 * acid unless we're fighting.
	 */

	if ( cmd || GET_POS ( ch ) != POS_FIGHTING )
		return FALSE;

	/* Only breathe acid 20% of the time */
	if ( number ( 0, 4 ) )
		return FALSE;

	/* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
	 * breath so we could have tougher dragons.  Right now, it does damage
	 * equal to a fireball in all cases.
	 */
	/* call_magic(ch, NULL, NULL, SPELL_ACID_BREATH, 0, CAST_BREATH); */
	damage ( dragon, FIGHTING ( dragon ), dice ( GET_LEVEL ( dragon ), GET_LEVEL ( dragon ) ),
	         SPELL_ACID_BREATH );

	/* If you use the damage call, you don't need the spell, but if you use
	 * the spell, you should add the no_magic room information below.
	 */
	return TRUE;
}

SPECIAL ( dragon_lightning )
{
	Character *dragon = ( Character * ) me;

	/* I don't know what 'cmd' is but we never do anything if we don't breathe
	 * lightning unless we're fighting.
	 */
	if ( cmd || GET_POS ( ch ) != POS_FIGHTING )
		return FALSE;

	/* Only breathe lightning 20% of the time */
	if ( number ( 0, 4 ) )
		return FALSE;

	/* We could actually pass GET_LEVEL(ch) instead of 0 for the level of the
	 * breath so we could have tougher dragons.  Right now, it does damage
	 * equal to a fireball in all cases.
	 */
	/* call_magic(ch, NULL, NULL, SPELL_LIGHTNING_BREATH, 0, CAST_BREATH); */
	damage ( dragon, FIGHTING ( dragon ), dice ( GET_LEVEL ( dragon ), GET_LEVEL ( dragon ) ),
	         SPELL_LIGHTNING_BREATH );

	/* If you use the damage call, you don't need the spell, but if you use
	 * the spell, you should add the no_magic room information below.
	 */
	return TRUE;
}

/* Stuff I have coded -- kalten */
SPECIAL ( fire )
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

	if ( !CMD_IS ( "fire" ) )
		return ( 0 );

	/* since inside a vehicle, need to save the room they were in */
	was_in = IN_ROOM ( ch );

	/* now, find the room the vehicle is in and put the character there */
	viewport =
	    get_obj_in_list_type ( ITEM_V_WINDOW, IN_ROOM ( ch )->contents );
	if ( viewport )
	{
		vehicle = find_vehicle_by_vnum ( GET_OBJ_VAL ( viewport, 0 ) );
		IN_ROOM ( ch ) = vehicle->in_room;
	}

	room = IN_ROOM ( ch );

	two_arguments ( argument, arg1, arg2 );

	/* can they even fire that way */
	if ( ( dir = search_block ( arg2, dirs, FALSE ) ) < 0 )
	{
		*ch << "What direction?\r\n";
		IN_ROOM ( ch ) = was_in;
		return ( 1 );
	}

	/* can't go that way */
	if ( !CAN_GO ( ch, dir ) )
	{
		*ch << "Something blocks the way!\r\n";
		IN_ROOM ( ch ) = was_in;
		return ( 1 );
	}

	/* now that you can go that way, find the next room */
	if ( CAN_GO2 ( room, dir ) )
		nextroom = EXIT2 ( room, dir )->to_room;
	else
		nextroom = NULL;

	/* now, find the target */
	for ( distance = 1; ( ( nextroom != NULL ) && ( distance <= range ) );
	        distance++ )
	{
		/* search for the target in the room */
		for ( target = nextroom->contents; target;
		        target = target->next_content )
		{
			if ( ( isname ( arg1, GET_OBJ_NAME ( target ) ) )
			        && ( CAN_SEE_OBJ ( ch, target ) ) )
			{
				found = TRUE;
				break;
			}
		}
		/* we have a target now */
		if ( found )
		{
			percent = number ( 0, 101 );
			chance /= distance;
			if ( percent < chance )  	/* the closer the ship is, the better chance of hitting it */
			{
				if ( ( dam = number ( 0, 2 ) ) > 0 )
				{
					/* damage the vehicle */
					GET_OBJ_VAL ( target, 2 ) -= dam;
					/* check to see if it should blow up now */
					if ( GET_OBJ_VAL ( target, 2 ) <= 0 )
					{
						/* Blow it up */
						IN_ROOM ( ch ) = was_in;
						act ( "The $p was blown up.", FALSE, ch, target,
						      NULL, TO_CHAR );
						act ( "$n blows up the $p.", TRUE, ch, target, NULL,
						      TO_ROOM );
						Room *explosion_room = IN_ROOM ( target );
						extract_obj ( target );
						send_to_room ( explosion_room, "The ship bursts at the seams from the powerful strike.\r\n" );
						return ( 1 );
					}
					else  	/* needs more damage */
					{
						IN_ROOM ( ch ) = was_in;
						act ( "Your missile strikes the $p.", FALSE, ch,
						      target, NULL, TO_CHAR );
						act ( "$n's missile strikes the $p.", TRUE, ch,
						      target, NULL, TO_ROOM );
						explosion_messages ( real_room
						                     ( GET_OBJ_VAL ( target, 0 ) ),
						                     dam, target );
						WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );
						return ( 1 );
					}
				}
				else  	/* didn't do any damage */
				{
					IN_ROOM ( ch ) = was_in;
					act ( "The missile misses the $p.", FALSE, ch, target,
					      NULL, TO_CHAR );
					act ( "$n misses the $p.", TRUE, ch, target, NULL,
					      TO_ROOM );
					explosion_messages ( real_room ( GET_OBJ_VAL ( target, 0 ) ),
					                     dam, target );
					WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );
					return ( 1 );
				}
			}
			else  		/* missed it completely */
			{
				IN_ROOM ( ch ) = was_in;
				act ( "The missile misses the $p.", FALSE, ch, target, NULL,
				      TO_CHAR );
				act ( "$n misses the $p.", TRUE, ch, target, NULL, TO_ROOM );
				explosion_messages ( real_room ( GET_OBJ_VAL ( target, 0 ) ),
				                     dam, target );
				WAIT_STATE ( ch, PULSE_VIOLENCE * 2 );
				return ( 1 );
			}
		}

		/* target wasnt in the room, move to next room */
		room = nextroom;
		if ( CAN_GO2 ( room, dir ) )
			nextroom = EXIT2 ( room, dir )->to_room;
		else
			nextroom = NULL;
	}

	*ch << "Cant find your target!\r\n";
	IN_ROOM ( ch ) = was_in;
	return ( 1 );
}

SPECIAL ( radar )
{
	Character *i;
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

	if ( !CMD_IS ( "radar" ) )
		return ( 0 );

	if ( IS_AFFECTED ( ch, AFF_BLIND ) )
	{
		*ch << "You can't see anything, you're blind!";
		return ( 1 );
	}

	/* since inside a vehicle, need to save the room they were in */
	is_in = was_in = IN_ROOM ( ch );

	/* now, find the room the vehicle is in and put the character there */
	viewport =
	    get_obj_in_list_type ( ITEM_V_WINDOW, IN_ROOM ( ch )->contents );
	if ( viewport )
	{
		if ( ( vehicle =
		            find_vehicle_by_vnum ( GET_OBJ_VAL ( viewport, 0 ) ) ) != NULL )
			is_in = vehicle->in_room;
		else
			return ( 0 );
	}

	if ( GET_LEVEL ( ch ) >= LVL_IMMORT )
		maxdis = 7;
	else
		maxdis = 5;

	IN_ROOM ( ch ) = was_in;
	*ch << "You begin watching the radar screen and see:\r\n";
	act ( "$n begins watching the radar screen.", TRUE, ch, 0, 0, TO_ROOM );

	for ( dir = 0; dir < NUM_OF_DIRS; dir++ )
	{
		IN_ROOM ( ch ) = is_in;
		for ( dis = 0; dis <= maxdis; dis++ )
		{
			if ( ( ( dis == 0 ) && ( dir == 0 ) ) || ( dis > 0 ) )
			{
				for ( i = IN_ROOM ( ch )->people; i; i = i->next_in_room )
				{
					if ( ( ! ( ( ch == i ) && ( dis == 0 ) ) ) && CAN_SEE ( ch, i ) )
					{
						ch->Send ( "%33s: %s%s%s%s", GET_NAME ( i ),
						           distance[dis], ( ( dis > 0 )
						                            && ( dir <
						                                 ( NUM_OF_DIRS -
						                                   2 ) ) ) ? "to the " : "",
						           ( dis > 0 ) ? dirs[dir] : "", ( ( dis > 0 )
						                                           && ( dir >
						                                                ( NUM_OF_DIRS
						                                                  -
						                                                  3 ) ) ) ?
						           "wards" : "" );
						found++;
					}
				}
			}
			if ( !CAN_GO ( ch, dir )
			        || ( IN_ROOM ( ch )->dir_option[dir]->to_room == is_in ) )
				break;
			else
				IN_ROOM ( ch ) = IN_ROOM ( ch )->dir_option[dir]->to_room;
		}
	}
	if ( found == 0 )
		*ch << "Nobody anywhere near you.";
	IN_ROOM ( ch ) = was_in;
	return ( 1 ); /** this function doesnt updatethe zones num_players value, watch for this if getting fancy - mord**/
}

/* Slave collar for the Saints Clan
   Val 0 - AC of the item
   Val 1 - ID of the Mistress
   Val 2 - ID of the Slave
*/
SPECIAL(slave_collar)
{
  struct obj_data *obj = (struct obj_data *)me;

  if (!obj->carried_by)
      return FALSE;

  if (!strcmp(cmd_arg, "submit")) {
      if (obj->worn_by) {
          ch->Send("You are already wearing the collar!\r\n");
          return TRUE;
      }
      if (GET_OBJ_VAL(obj, 1) == 0 || GET_OBJ_VAL(obj, 2) != GET_IDNUM(ch)) {
          ch->Send("This collar needs to be RESET before you can submit to it.\r\n");
          return TRUE;
      }     
      GET_OBJ_VAL(obj, 2) = GET_IDNUM(ch);
      act("$n has submitted to the slave collar.", FALSE, ch, NULL, NULL, TO_ROOM);
      ch->Send("You have submitted to the slave collar!\r\n");
      return TRUE;
  }
  else if (!strcmp(cmd_arg, "strap")) {
      if (obj->worn_by) {
          ch->Send("You are already wearing the collar!\r\n");
          return TRUE;
      }
      if (GET_OBJ_VAL(obj, 2) != GET_IDNUM(ch)) {
          ch->Send("This slave collar does not belong to you.\r\n");
          return TRUE;
      }
      perform_wear(ch, obj, WEAR_NECK_1);
      return TRUE;
  }
  else if (GET_EQ(ch, WEAR_NECK_1) == obj && (CMD_IS("north") || 
  CMD_IS("south") || CMD_IS("west") || CMD_IS("east") || CMD_IS("up") ||
  CMD_IS("down"))) {
      if (GET_OBJ_VAL(obj, 3) == 0) return FALSE;
      }
  return FALSE;
}


