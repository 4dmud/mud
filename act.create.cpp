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
 * Revision 1.22  2007/06/26 10:48:04  w4dimenscor
 * Fixed context in scripts so that it works again, changed mounted combat so that it is about 2/3rds player one third mount damage, updated the way skills get read using total_chance, stopped things with a PERC of 0 assisting, made it so that the ungroup command disbanded charmies
 *
 * Revision 1.21  2007/01/28 21:18:18  w4dimenscor
 * Fixed item desc of Manifest item when it is looked it.
 * It is now more english. 01/28/2007 Hal.
 *
 * Revision 1.20  2006/10/06 22:25:30  w4dimenscor
 * fixed staves crested from woodsing
 *
 * Revision 1.19  2006/09/15 08:01:11  w4dimenscor
 * Changed a large amount of send_to_char's to ch->Send and d->Output. fixed namechange command
 *
 * Revision 1.18  2006/08/18 11:09:58  w4dimenscor
 * updated some clan functions to use vectors instead of malloccing memory, and also sorted clan lists and updated their layout
 *
 * Revision 1.17  2006/08/13 06:26:50  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.16  2006/05/21 11:02:25  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.15  2006/04/21 12:46:43  w4dimenscor
 * Fixed gcc 4.1 compile time errors. Game will now compile in GCC4
 *
 * Revision 1.14  2006/01/23 05:23:19  w4dimenscor
 * sorry self. another. _can't remember the changes_ entry
 *
 * Revision 1.13  2005/11/30 18:47:12  w4dimenscor
 * changed slightly some gains you get from remorts
 *
 * Revision 1.12  2005/11/19 06:18:38  w4dimenscor
 * Fixed many bugs, and added features
 *
 * Revision 1.11  2005/11/01 18:43:37  w4dimenscor
 * Tradepoints have been added to players and saved, compare command has been updated, the login accounts thing works again, and when you can't see your attacker your attacker you get half defense points
 *
 * Revision 1.10  2005/06/18 12:20:52  w4dimenscor
 * changed a bunch of send_to_char's to new_send_to_chars, adjusted some mxp code
 *
 * Revision 1.9  2005/05/28 05:52:14  w4dimenscor
 * Fixed some errors in copyover, added MXP
 *
 * Revision 1.8  2005/04/26 10:15:18  w4dimenscor
 * fixed the player timeouts, so we will no longer have thousands of users that don't play and yet still slow us down. requirelents to be deleted: any seeker who hasn't logged in within 90 days and is less then level 40 will be deleted. these requirements wiped about 8000 players from our list hehe.
 *
 * Revision 1.7  2005/04/23 12:18:12  w4dimenscor
 * Fixed some buffer read errors in the fread_string function, also fixed (temp) an index search issue for real_trigger()
 *
 * Revision 1.6  2005/02/05 05:26:17  w4dimenscor
 * Added tsearch command to full text search triggers
 *
 * Revision 1.5  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.4  2004/12/17 07:13:20  w4dimenscor
 * A few little updates.
 *
 * Revision 1.3  2004/12/07 09:31:26  w4dimenscor
 * Trees modularized, fix added to message event
 *
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

#include "config.h"
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
#include "trees.h"
#include "damage.h"
#include "action.h"


#define TIER (current_class_is_tier_num(ch)+1)


ASKILL ( skill_brew );
ASKILL ( skill_scribe );
ASKILL ( skill_tinker );
ASKILL ( skill_sing_wood );
EVENTFUNC ( message_event );
ASKILL ( skill_manifest );
void make_manifest ( Character *ch,struct obj_data *obj );
ASKILL ( skill_manipulate );
ASPELL ( spell_control_weather );
ASPELL ( spell_call_lightning );
extern struct stave_stat_table stave_table[9];

Character *find_char ( long n );
struct obj_data *find_obj ( long n );
Room *find_room ( long n );
void run_task ( Character *ch );
int perf_balance ( int weapon_type );
int curr_balance ( OBJ_DATA *wep );
int save_forest ( void );

/* extern procedures */
int mag_manacost ( Character *ch, int spellnum );
void improve_skill ( Character *ch, int skill );
ACTION ( thing_lumberjack );
ACTION ( thing_manifest );
ACTION ( thing_singwood );
ACTION ( thing_juggle );
ACTION ( thing_throttle );
ACTION ( thing_tunneling );
ACTION ( thing_control_weather_better );
ACTION ( thing_control_weather_worse );
ACTION ( thing_call_lightning );

/**
Tree      Affect    Max  Chance    Start Age
Willow    speed     240  1 in 20   5
elder     speed     110  1 in 5    1
pine      cha       9    1 in 10   1
dogwood   wis       5    1 in 15   6
ironwood  hp        600  1 in 15   3
fir       mana      2000 1 in 20   1
maple     int       5    1 in 10   3
elm       mana      1000 1 in 10   1
Oak       hp        400  1 in 5    2

**/



char *get_spell_name ( char *argument )
{
	char *s;

	s = strtok ( argument, "'" );
	s = strtok ( NULL, "'" );

	return s;
}

void make_potion ( Character *ch, int potion,
                   struct obj_data *container )
{
	struct obj_data *final_potion;
	struct extra_descr_data *new_descr;
	int can_make = TRUE, mana, dam, num = 0;
	char buf2[MAX_INPUT_LENGTH];

	/* Modify this list to suit which spells you
	 * want to be able to mix. */
	switch ( potion )
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

	if ( can_make == FALSE )
	{
		ch->Send ( "That spell cannot be mixed into a potion.\r\n" );
		return;
	}
	else if ( ( number ( 1, 3 ) == 3 ) && ( GET_LEVEL ( ch ) < LVL_HERO ) )
	{
		ch->Send ( "As you begin mixing the potion, it violently explodes!\r\n" );
		act ( "$n begins to mix a potion, but it suddenly explodes!",        FALSE, ch, 0, 0, TO_ROOM );
		extract_obj ( container );
		dam = number ( 15, mag_manacost ( ch, potion ) * 2 );
		damage ( ch, ch, dam, TYPE_UNDEFINED );
		return;
	}

	/* requires x3 mana to mix a potion than the spell */
	mana = mag_manacost ( ch, potion ) * 3;
	if ( GET_MANA ( ch ) - mana > 0 )
	{
		if ( GET_LEVEL ( ch ) < LVL_HERO )
			alter_mana ( ch, mana );
		ch->Send ( "You create a %s potion.\r\n", skill_name ( potion ) );
		act ( "$n creates a potion!", FALSE, ch, 0, 0, TO_ROOM );
		extract_obj ( container );
	}
	else
	{
		ch->Send ( "You don't have enough mana to mix that potion!\r\n" );
		return;
	}

	final_potion = create_obj ( NOTHING );

	snprintf ( buf2, sizeof ( buf2 ), "%s %s potion", potion_names[num], skill_name ( potion ) );
	final_potion->name = str_dup ( buf2 );

	snprintf ( buf2, sizeof ( buf2 ), "A %s potion lies here.", potion_names[num] );
	final_potion->description = str_dup ( buf2 );

	snprintf ( buf2, sizeof ( buf2 ), "a %s potion", potion_names[num] );
	final_potion->short_description = str_dup ( buf2 );

	/* extra description coolness! */
	CREATE ( new_descr, struct extra_descr_data, 1 );
	new_descr->keyword = str_dup ( final_potion->name );
	snprintf ( buf2, sizeof ( buf2 ), "It appears to be a %s potion.", skill_name ( potion ) );
	new_descr->description = str_dup ( buf2 );
	new_descr->next = NULL;
	final_potion->ex_description = new_descr;

	GET_OBJ_TYPE ( final_potion ) = ITEM_POTION;
	SET_BIT_AR ( GET_OBJ_WEAR ( final_potion ), ITEM_WEAR_TAKE );
	SET_BIT_AR ( GET_OBJ_EXTRA ( final_potion ), ITEM_NORENT );
	SET_BIT_AR ( GET_OBJ_EXTRA ( final_potion ), ITEM_UNIQUE_SAVE );
	GET_OBJ_VAL ( final_potion, 0 ) = GET_LEVEL ( ch );
	GET_OBJ_VAL ( final_potion, 1 ) = potion;
	GET_OBJ_VAL ( final_potion, 2 ) = -1;
	GET_OBJ_VAL ( final_potion, 3 ) = -1;
	GET_OBJ_COST ( final_potion ) = GET_LEVEL ( ch ) * 500;
	GET_OBJ_WEIGHT ( final_potion ) = FTOI ( 10 + ( GET_LEVEL ( ch ) * 0.1 ) );
	GET_OBJ_RENT ( final_potion ) = 0;
	GET_OBJ_TIMER ( final_potion ) = 100;

	obj_to_char ( final_potion, ch );
	improve_skill ( ch, SKILL_BREW );
}

ASKILL ( skill_brew )
{
	struct obj_data *container = NULL;
	struct obj_data *next_obj;
	char bottle_name[MAX_INPUT_LENGTH];
	char spell_name[MAX_INPUT_LENGTH];
	char *temp1, *temp2;
	int potion, found = FALSE;

	if ( !knows_spell ( ch, SKILL_BREW ) )
	{
		ch->Send ( "You are not schooled enough to brew anything!\r\n" );
		return 0;
	}

	temp1 = one_argument ( argument, bottle_name );



	/* sanity check */
	if ( temp1 )
	{
		temp2 = get_spell_name ( temp1 );
		if ( temp2 )
			strcpy ( spell_name, temp2 );
	}
	else
	{
		bottle_name[0] = '\0';
		spell_name[0] = '\0';
	}

	if ( !*bottle_name || !*spell_name )
	{
		ch->Send ( "What do you wish to mix in where?\r\n" );
		return 0;
	}


	for ( obj = ch->carrying; obj; obj = next_obj )
	{
		next_obj = obj->next_content;
		if ( obj == NULL )
			return 0;
		else if ( ! ( container = get_obj_in_list_vis ( ch, bottle_name, NULL,
		                          ch->carrying ) ) )
			continue;
		else
			found = TRUE;
	}
	if ( found != FALSE && ( GET_OBJ_VNUM ( container ) != 3044 ) )
	{
		ch->Send ( "You don't have the proper container!\r\n" );
		return 0;
	}
	if ( found == FALSE )
	{
		ch->Send ( "You don't have %s in your inventory!\r\n",
		           bottle_name );
		return 0;
	}

	if ( !*spell_name )
	{
		ch->Send ( "Spell names must be enclosed in single quotes!\r\n" );
		return 0;
	}

	potion = find_skill_num ( spell_name );

	if ( ( potion < 1 ) || ( potion > MAX_SKILLS ) || spell_info[potion].type != 1 )
	{
		ch->Send ( "Mix what spell?!?\r\n" );
		return 0;
	}
	if ( !knows_spell ( ch, potion ) )
	{
		ch->Send ( "You do not know how to make that potion!\r\n" );
		return 0;
	}
	if ( GET_SKILL ( ch, potion ) == 0 )
	{
		ch->Send ( "You are unfamiliar brewing %s.\r\n", skill_name ( potion ) );
		return 0;
	}
	make_potion ( ch, potion, container );
	return ( SKILL_BREW );
}



void make_scroll ( Character *ch, int scroll, struct obj_data *paper )
{
	struct obj_data *final_scroll;
	struct extra_descr_data *new_descr;
	int can_make = TRUE, mana, dam = 0;
	char buf2[MAX_INPUT_LENGTH];

	/* add a case statement here for prohibited spells */

	/* Modify this list to suit which spells you
	 * want to be able to mix. */
	if ( IS_SET ( class_elem_strength ( GET_CLASS ( ch ) ), ( 1 << elemental_type ( scroll ) ) ) )
		can_make = TRUE;
	else
		can_make = FALSE;

	if ( can_make == FALSE )
	{
		ch->Send ( "That spell cannot be scribed into a scroll.\r\n" );
		return;
	}

	if ( !knows_spell ( ch, scroll ) )
	{
		ch->Send ( "You must know a spell in order to scribe it.\r\n" );
		return;
	}

	else if ( ( number ( 1, 3 ) == 3 ) && ( GET_LEVEL ( ch ) < LVL_HERO ) )
	{
		ch->Send ( "As you begin inscribing the final rune, the scroll violently explodes!\r\n" );
		act ( "$n tries to scribe a spell, but it explodes!",  FALSE, ch, 0, 0, TO_ROOM );
		extract_obj ( paper );
		dam = number ( 15, mag_manacost ( ch, scroll ) * 2 );
		damage ( ch, ch, dam, TYPE_UNDEFINED );
		return;
	}
	/* requires x3 mana to scribe a scroll than the spell */
	mana = mag_manacost ( ch, scroll ) * 3;

	if ( GET_MANA ( ch ) - mana > 0 )
	{
		if ( GET_LEVEL ( ch ) < LVL_HERO )
			alter_mana ( ch, mana );
		ch->Send ( "You create a scroll of %s.\r\n", skill_name ( scroll ) );
		act ( "$n creates a scroll!", FALSE, ch, 0, 0, TO_ROOM );
		extract_obj ( paper );
	}
	else
	{
		ch->Send ( "You don't have enough mana to scribe such a powerful spell!\r\n" );
		return;
	}

	final_scroll = create_obj ( NOTHING );

	snprintf ( buf2, sizeof ( buf2 ), "%s scroll", skill_name ( scroll ) );
	final_scroll->name = str_dup ( buf2 );

	snprintf ( buf2, sizeof ( buf2 ),
	           "Some parchment inscribed with the runes '%s' lies here.",
	           skill_name ( scroll ) );
	final_scroll->description = str_dup ( buf2 );

	snprintf ( buf2, sizeof ( buf2 ), "a %s scroll", skill_name ( scroll ) );
	final_scroll->short_description = str_dup ( buf2 );

	/* extra description coolness! */
	CREATE ( new_descr, struct extra_descr_data, 1 );
	new_descr->keyword = str_dup ( final_scroll->name );
	snprintf ( buf2, sizeof ( buf2 ), "It appears to be a %s scroll.", skill_name ( scroll ) );
	new_descr->description = str_dup ( buf2 );
	new_descr->next = NULL;
	final_scroll->ex_description = new_descr;

	GET_OBJ_TYPE ( final_scroll ) = ITEM_SCROLL;
	SET_BIT_AR ( GET_OBJ_WEAR ( final_scroll ), ITEM_WEAR_TAKE );
	SET_BIT_AR ( GET_OBJ_EXTRA ( final_scroll ), ITEM_NORENT );
	SET_BIT_AR ( GET_OBJ_EXTRA ( final_scroll ), ITEM_UNIQUE_SAVE );
	GET_OBJ_VAL ( final_scroll, 0 ) = GET_LEVEL ( ch );
	GET_OBJ_VAL ( final_scroll, 1 ) = scroll;
	GET_OBJ_VAL ( final_scroll, 2 ) = -1;
	GET_OBJ_VAL ( final_scroll, 3 ) = -1;
	GET_OBJ_COST ( final_scroll ) = GET_LEVEL ( ch ) * 500;
	GET_OBJ_WEIGHT ( final_scroll ) = FTOI ( 10 + ( GET_LEVEL ( ch ) * 0.1 ) );
	GET_OBJ_RENT ( final_scroll ) = 0;
	GET_OBJ_TIMER ( final_scroll ) = -1;

	obj_to_char ( final_scroll, ch );
}

int blank_scroll ( struct obj_data *obj )
{
	return GET_OBJ_VAL ( obj, 0 ) > 0 || GET_OBJ_VAL ( obj, 1 ) > 0 || GET_OBJ_VAL ( obj, 2 ) > 0 ||GET_OBJ_VAL ( obj, 3 ) > 0;
}

ASKILL ( skill_scribe )
{
	struct obj_data *paper = NULL;
	struct obj_data *next_obj;
	char paper_name[MAX_INPUT_LENGTH];
	char spell_name[MAX_INPUT_LENGTH];
	char *temp1, *temp2;
	int scroll = 0, found = FALSE;

	temp1 = one_argument ( argument, paper_name );

	if ( !knows_spell ( ch, SKILL_SCRIBE ) )
	{
		ch->Send ( "You are not schooled enough to scribe anything!\r\n" );
		return 0;
	}

	/* sanity check */
	if ( temp1 )
	{
		temp2 = get_spell_name ( temp1 );
		if ( temp2 )
			strcpy ( spell_name, temp2 );
	}
	else
	{
		paper_name[0] = '\0';
		spell_name[0] = '\0';
	}


	if ( !*paper_name || !*spell_name )
	{
		ch->Send ( "What do you wish to scribe where?\r\n" );
		return 0;
	}

	for ( obj = ch->carrying; obj; obj = next_obj )
	{
		next_obj = obj->next_content;
		if ( obj == NULL )
			return 0;
		else if ( ! ( paper = get_obj_in_list_vis ( ch, paper_name, NULL,
		                      ch->carrying ) ) )
			continue;
		else
			found = TRUE;
	}
	if ( found && ( GET_OBJ_TYPE ( paper ) != ITEM_SCROLL || !blank_scroll ( paper ) ) )
	{
		ch->Send ( "You can't write on that!\r\n" );
		return 0;
	}
	if ( found == FALSE )
	{
		ch->Send ( "You don't have %s in your inventory!\r\n",  paper_name );
		return 0;
	}

	if ( !*spell_name )
	{
		ch->Send ( "Spell names must be enclosed in single quotes!\r\n" );
		return 0;
	}

	scroll = find_skill_num ( spell_name );

	if ( ( scroll < 1 ) || ( scroll > MAX_SKILLS ) || spell_info[scroll].type != 1 )
	{
		ch->Send ( "Scribe what spell?!?\r\n" );
		return 0;
	}
	if ( !knows_spell ( ch, scroll ) )
	{
		ch->Send ( "You are not schooled enough to cast that spell!\r\n" );
		return 0;
	}

	make_scroll ( ch, scroll, paper );
	improve_skill ( ch, SKILL_SCRIBE );
	return scroll;
}


ASKILL ( skill_tinker )
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
	char weapon_name[MAX_INPUT_LENGTH];
	int found = FALSE, prob = 0, dam = 0, time = 0;

	one_argument ( argument, weapon_name );

	if ( !knows_spell ( ch, SKILL_TINKER ) )
	{
		ch->Send ( "You are not schooled enough to tinker anything!\r\n" );
		return 0;
	}
	if ( !*weapon_name )
	{
		ch->Send ( "What do you wish to tinker on?\r\n" );
		return 0;
	}


	for ( obj = ch->carrying; obj; obj = next_obj )
	{
		next_obj = obj->next_content;
		if ( obj == NULL )
			return 0;
		else if ( ! ( weapon = get_obj_in_list_vis ( ch, weapon_name, NULL,
		                       ch->carrying ) ) )
			continue;
		else
			found = TRUE;
	}

	if ( found == FALSE )
	{
		ch->Send ( "You don't have %s in your inventory!\r\n",
		           weapon_name );
		return 0;
	}

	if ( found && ( GET_OBJ_TYPE ( weapon ) != ITEM_WEAPON ) )
	{
		ch->Send ( "It doesn't look like %s would make a"
		           " good weapon...\r\n", weapon_name );
		return 0;
	}

	if ( IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_TINKERED ) || IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_MAGIC ) )
	{
		ch->Send ( "The weapon is imbued with magical powers beyond your grasp.\r\n"
		           "You can not further affect its form.\r\n" );
		return 0;
	}

	if ( IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_POISONED_1 ) ||
	        IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_POISONED_2 ) ||
	        IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_POISONED_3 ) ||
	        IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_POISONED_4 ) )
	{
		ch->Send ( "The weapon is poisoned.\r\n You cannot further affect its form.\r\n" );
		return 0;
	}

	/* determine success probability */
	prob += ( GET_LEVEL ( ch ) << 1 ) + ( ( GET_DEX ( ch ) - 11 ) << 1 );
	prob += ( ( GET_STR ( ch ) - 11 ) << 1 ) + ( GET_ADD ( ch ) >> 3 );
	prob /= 2;			// with 50 levels, tinker always works

	if ( ( number ( 10, 100 ) > prob ) && ( GET_LEVEL ( ch ) < LVL_HERO ) )
	{
		ch->Send ( "As you pound out the dents in the weapon,"
		           " you hit a weak spot and it explodes!\r\n" );
		ch->Send ( "Hot broken shards go in your eyes!\r\n" );
		act ( "$n tries to forge a weapon, but it explodes!",
		      FALSE, ch, 0, 0, TO_ROOM );
		extract_obj ( weapon );
		dam = number ( 20, 60 ) * 3;
		damage ( ch, ch, dam, TYPE_UNDEFINED );
		return 0;
	}
	time = 6000;
	if ( IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_BLESS ) )
		time *= 2;
	if ( IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_HUM ) )
		time *= 2;
	if ( IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_GLOW ) )
		time *= 2;

	GET_OBJ_VAL ( weapon, 1 ) += number ( -1, ( GET_LEVEL ( ch ) /23 ) + ( GET_DEX ( ch ) >17 ) + ( TIERNUM-2 ) +1 );
	GET_OBJ_VAL ( weapon, 2 ) += number ( -1, ( GET_LEVEL ( ch ) /19 ) + ( GET_DEX ( ch ) >16 ) + ( TIERNUM-2 ) +1 );
	GET_OBJ_RENT ( weapon ) += ( GET_LEVEL ( ch ) << 3 );
	GET_OBJ_TIMER ( weapon ) = time;	// don't want tinkered weapons lasting
	// forever
	// Putting this skeleton in for possible future changes
	// This is from woodsing but it will be changed 
	// Changed from 300 to 180 if this is added. --> Prom
	//GET_OBJ_TIMER ( weapon ) = GET_LEVEL ( ch ) * 180; 
	SET_BIT_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_MAGIC );
	SET_BIT_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_TINKERED );

	ch->Send ( "You have forged new life into the weapon!\r\n" );
	act ( "$n vigorously pounds on a weapon!", FALSE, ch, 0, 0, TO_ROOM );
	return SKILL_TINKER;
}

void make_focus ( Character *ch, int type, struct obj_data *o )
{
	struct obj_data *final_focus;
	struct extra_descr_data *new_descr;
	int can_make = TRUE, num2 = number ( 0, 16 );
	int v0, v1, v2, v3;
	char *msg, *msgroom, msgbuf[MAX_INPUT_LENGTH],  msgroombuf[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];

	msg = msgbuf;
	msgroom = msgroombuf;


	if ( can_make == FALSE )
	{
		ch->Send ( "That item cannot be made into a focus.\r\n" );
		return;
	}

	/*ch->Send( "You sing %s %s %s %s focus staff from %s.\r\n",
	   LANA(age_desc_staff[GET_OBJ_VAL(o, 1)]),
	   age_desc_staff[GET_OBJ_VAL(o, 1)], random_desc[num2],
	   tree_names[GET_OBJ_VAL(o, 2)], o->short_description);
	act("$n sings a focus staff from $p!", FALSE, ch, o, 0, TO_ROOM);*/
	v0 = GET_OBJ_VAL ( o, 0 );
	v1 = MIN ( GET_OBJ_VAL ( o, 1 ), 8 );
	v2 = MIN ( GET_OBJ_VAL ( o, 2 ), 8 );
	v3 = GET_OBJ_VAL ( o, 3 );
	extract_obj ( o );
	create_trees();

	final_focus = create_obj ( NOTHING );

	if ( final_focus->name )
		free ( final_focus->name );
	snprintf ( buf2, sizeof ( buf2 ), " %s %s focus staff", age_desc_staff[v1],
	           tree_names[v2] );
	final_focus->name = str_dup ( buf2 );
	if ( final_focus->description )
		free ( final_focus->description );
	snprintf ( buf2, sizeof ( buf2 ), "%s %s %s %s staff lies here.",
	           CANA ( age_desc_staff[v1] ), age_desc_staff[v1],
	           random_desc[num2], tree_names[v2] );
	final_focus->description = str_dup ( buf2 );
	if ( final_focus->short_description )
		free ( final_focus->short_description );
	snprintf ( buf2, sizeof ( buf2 ), "%s %s %s %s staff",
	           LANA ( age_desc_staff[v1] ), age_desc_staff[v1],
	           random_desc[num2], tree_names[v2] );
	final_focus->short_description = str_dup ( buf2 );

	/* extra description coolness! */
	CREATE ( new_descr, struct extra_descr_data, 1 );
	new_descr->keyword = str_dup ( buf2 );
	snprintf ( buf2, sizeof ( buf2 ), "It appears to be %s %s %s %s focus staff.",
	           LANA ( age_desc_staff[v1] ), age_desc_staff[v1],
	           random_desc[num2], tree_names[v2] );
	new_descr->description = str_dup ( buf2 );
	new_descr->next = NULL;
	final_focus->ex_description = new_descr;
	if ( number ( 0, 400 - ( GET_CHA ( ch ) + total_chance ( ch, SKILL_SING_WOOD ) ) ) )
		GET_OBJ_TYPE ( final_focus ) = ITEM_FOCUS_MINOR;
	else
		GET_OBJ_TYPE ( final_focus ) = ITEM_FOCUS_MAJOR;
	SET_BIT_AR ( GET_OBJ_WEAR ( final_focus ), ITEM_WEAR_FOCUS );
	SET_BIT_AR ( GET_OBJ_WEAR ( final_focus ), ITEM_WEAR_TAKE );
	SET_BIT_AR ( GET_OBJ_EXTRA ( final_focus ), ITEM_HUM );
	SET_BIT_AR ( GET_OBJ_EXTRA ( final_focus ), ITEM_UNIQUE_SAVE );
	GET_OBJ_VAL ( final_focus, 0 ) = ( v1+1 );
	GET_OBJ_VAL ( final_focus, 1 ) = v2;
	GET_OBJ_VAL ( final_focus, 2 ) = FOCUS_STAFF;
	GET_OBJ_VAL ( final_focus, 3 ) = GET_LEVEL ( ch ) *TIER*2000;
	GET_OBJ_COST ( final_focus ) = GET_LEVEL ( ch ) * 500;
	GET_OBJ_WEIGHT ( final_focus ) = 3;

	GET_OBJ_RENT ( final_focus ) = number(100+(v1*number(7,10)), 240+(MIN(REMORTS(ch)/2, 50)));

        if (v2 == 0)
        SET_BIT_AR ( GET_OBJ_EXTRA (final_focus), ITEM_ELEC_FOCUS);
        if (v2 == 1)
        SET_BIT_AR(GET_OBJ_EXTRA (final_focus),  ITEM_FIRE_FOCUS);
        if (v2 == 2)
	SET_BIT_AR(GET_OBJ_EXTRA (final_focus), ITEM_WATER_FOCUS);
	if (v2 == 3)	
	SET_BIT_AR(GET_OBJ_EXTRA (final_focus), ITEM_AIR_FOCUS);
	if (v2 == 4)
        SET_BIT_AR(GET_OBJ_EXTRA (final_focus), ITEM_EARTH_FOCUS);
	if (v2 == 5)
        SET_BIT_AR(GET_OBJ_EXTRA (final_focus), ITEM_AIR_FOCUS);
	if (v2 == 6)
        SET_BIT_AR(GET_OBJ_EXTRA (final_focus), ITEM_DEATH_FOCUS);
	if (v2 == 7)
        SET_BIT_AR(GET_OBJ_EXTRA (final_focus), ITEM_SPIRIT_FOCUS);
	if (v2 == 8) 
        SET_BIT_AR(GET_OBJ_EXTRA (final_focus), ITEM_ICE_FOCUS);






	GET_OBJ_TIMER ( final_focus ) = GET_LEVEL ( ch ) * 300;
	/** new affect addition - mord (from discussion with Azreal)**/
	if ( v1 >= stave_table[v2].start )
	{
		final_focus->affected[0].location = stave_table[v2].affect;
		final_focus->affected[0].modifier =
     stave_table[v2].max  * ( v1 - stave_table[v2].start ) / 
    ( 9 - stave_table[v2].start );
	}
	obj_to_char ( final_focus, ch );
	if ( type == SKILL_SING_WOOD )
		improve_skill ( ch, SKILL_SING_WOOD );

}


ASKILL ( skill_sing_wood )
{
	struct obj_data *o = NULL;
	char tree_name[MAX_INPUT_LENGTH];
	char *temp1;
	int type, found = FALSE;
	struct message_event_obj *msg = NULL;

	skip_spaces ( &argument );
	temp1 = one_argument ( argument, tree_name );
	type = SKILL_SING_WOOD;

	/* sanity check */
	if ( !*tree_name )
	{
		ch->Send ( "What do you wish to sing to?\r\n" );
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
	if ( ( o =
	            get_obj_in_list_vis ( ch, tree_name, 0,IN_ROOM ( ch )->contents ) ) == NULL )
	{
		ch->Send ( "The tree you need is not here.\r\n" );
		return 0;
	}
	else
	{
		found = TRUE;
	}


	if ( found != FALSE && ( ( GET_OBJ_TYPE ( o ) != ITEM_TREE ) || GET_OBJ_VNUM ( o ) != NOTHING ) )
	{
		ch->Send ( "You can't see the proper tree!\r\n" );
		return 0;
	}
	if ( found == FALSE )
	{
		ch->Send ( "There is no tree here!\r\n" );
		return 0;
	}
	if ( ch->HasMessageEvent(ME_SINGWOOD) )
	{
		ch->Send ( "You are doing that already!\r\n" );
		return 0;
	}

	ch->Send ( "You take a deap breath and clear your mind.\r\n" );

	msg = new message_event_obj ( ch, SKILL_SING_WOOD, THING_SKILL, 11, GET_ID ( o ), ( char * ) "" );
	ch->AddMessageEvent(event_create ( message_event, msg, ( 1 RL_SEC ), EVENT_TYPE_MESSAGE ), ME_SINGWOOD);
	return SKILL_SING_WOOD;


}

ASKILL ( skill_manifest )
{
	char buf[512];

	int pos;
	struct message_event_obj *msg = NULL;

	one_argument ( argument, buf );


	if ( !*buf )
	{
		ch->Send ( "Usage: manifest <weapon>\r\n" );
		return 0;
	}

	if ( ! ( obj = get_obj_vis ( ch, buf, NULL ) ) )
	{
		ch->Send ( "No such object around.\r\n" );
		return 0;
	}

	if ( GET_OBJ_TYPE ( obj ) != ITEM_WEAPON )
	{
		ch->Send ( "No such weapon around\r\n" );
		return 0;
	}

	if ( ch->HasMessageEvent(ME_MANIFEST) )
	{
		ch->Send ( "You are manifesting a weapon!\r\n" );
		return 0;
	}


	if ( obj->worn_by && obj->worn_by == ch )
	{
		pos = obj->worn_on;
		unequip_char ( obj->worn_by, pos );
		obj_to_char ( obj, ch );
	}
	else if ( obj->carried_by != ch )
	{
		ch->Send ( "You are not wearing that weapon or don't have it in your inventory.\r\n" );
		return 0;
	}

	msg = new message_event_obj ( ch, SKILL_MANIFEST, THING_SKILL, 8, GET_ID ( obj ), ( char * ) "" );
	ch->AddMessageEvent ( event_create ( message_event, msg, 0, EVENT_TYPE_MESSAGE ), ME_MANIFEST );

	return SKILL_MANIFEST;
}

void make_manifest ( Character *ch,struct obj_data *obj )
{

	char buf[MAX_STRING_LENGTH];
	struct obj_data *final_focus;
	struct extra_descr_data *new_descr;
	int v1 = GET_OBJ_VAL ( obj, 1 ), v2 = GET_OBJ_VAL ( obj, 2 );
	int counter = 0;
	char buf2[MAX_INPUT_LENGTH];
	final_focus = create_obj ( NOTHING );

	for ( counter = 0; counter < MAX_OBJ_AFFECT; counter++ )
		if ( obj->affected[counter].modifier )
		{
			final_focus->affected[counter].location = obj->affected[counter].location;
			final_focus->affected[counter].modifier = obj->affected[counter].modifier;
		}

	GET_OBJ_INNATE ( final_focus ) = GET_OBJ_INNATE ( obj );
	snprintf ( buf, sizeof ( buf ),  "%s orb", obj->name );
	if ( final_focus->name )
		free ( final_focus->name );
	final_focus->name = strdup ( buf );

	snprintf ( buf, sizeof ( buf ), "a shimmering orb with %s inside", obj->short_description );
	if ( final_focus->description )
		free ( final_focus->description );
	final_focus->description = strdup ( buf );

	if ( final_focus->short_description )
		free ( final_focus->short_description );
	final_focus->short_description = strdup ( buf );

	/* extra description coolness! */
	CREATE ( new_descr, struct extra_descr_data, 1 );
	new_descr->keyword = str_dup ( final_focus->name );
	snprintf ( buf2, sizeof ( buf2 ), "It's cool to the touch. \r\n"
	           "It appears to have a tiny %s inside.",obj->short_description );
	new_descr->description = str_dup ( buf2 );
	new_descr->next = NULL;
	final_focus->ex_description = new_descr;


	if ( !IS_SET_AR ( GET_OBJ_EXTRA ( final_focus ), ITEM_UNIQUE_SAVE ) )
		SET_BIT_AR ( GET_OBJ_EXTRA ( final_focus ), ITEM_UNIQUE_SAVE );

	GET_OBJ_WEIGHT ( final_focus ) =  12;
	GET_OBJ_TIMER ( final_focus ) = ( 420 - GET_LEVEL ( ch ) * TIER ) * 10;

	if ( IS_SET_AR ( GET_OBJ_WEAR ( obj ), ITEM_WEAR_WIELD ) )
	{
		SET_BIT_AR ( GET_OBJ_WEAR ( final_focus ), ITEM_WEAR_TAKE );
		SET_BIT_AR ( GET_OBJ_WEAR ( final_focus ), ITEM_WEAR_FOCUS );
	}
	GET_OBJ_VAL ( final_focus, 0 ) = FTOI ( 1000 + ( ( ( ( v1 * v2 ) +v1 ) /2.0 ) * ( TIER - 0.60 ) * ( total_chance ( ch, SKILL_MANIFEST ) *0.01 ) ) );
	GET_OBJ_VAL ( final_focus, 1 ) = -1;
	GET_OBJ_VAL ( final_focus, 2 ) = FOCUS_ORB;
	GET_OBJ_VAL ( final_focus, 3 ) = GET_LEVEL ( ch ) *TIER*1000;
	if ( number ( 0, 800 - ( GET_CHA ( ch ) + ( 2* total_chance ( ch, SKILL_MANIFEST ) ) ) ) )
		GET_OBJ_TYPE ( final_focus ) = ITEM_FOCUS_MINOR;
	else
		GET_OBJ_TYPE ( final_focus ) = ITEM_FOCUS_MAJOR;

	improve_skill ( ch, SKILL_MANIFEST );
	obj_to_char ( final_focus, ch );
	extract_obj ( obj );
	return;
}

#define THING(name)  \
   name(ch, vict, obj, room, &msg->msg_num, freq)

EVENTFUNC ( message_event )
{
	struct message_event_obj *msg = ( struct message_event_obj * ) event_obj;
	Character *vict = NULL;
	struct obj_data *obj = NULL;
	Room *room = msg->rm;
	long time = 0;
        int freq = 1 RL_SEC;

	long uid = msg->id;
	Character *ch = msg->ch;
	short type = msg->type;
	int skill = msg->skill;
	int msg_id = -1;

	if ( ch == NULL )
	{
		log ( "Message event called with no character found!" );
		delete msg;
		return 0;
	}

	if ( msg->msg_num == 0 )
	{
		log ( "Message event called with msg_num at 0!" );
		ch->ClearMessageEvent ( (event *)msg );
		delete msg;
		return 0;
	}
	ch->ClearMessageEvent ( (event *)msg );

	if ( uid == NOBODY )
	{
		vict = ch;
	}
	else if ( ( room = find_room ( uid ) ) )
		{}
	else if ( ( vict = find_char ( uid ) ) )
		{}
	else if ( ( obj = find_obj ( uid ) ) );

	if ( type == THING_SKILL )
	{
		switch ( skill )
		{
			case SKILL_SING_WOOD:
				time = THING ( thing_singwood );
				msg_id = ME_SINGWOOD;
				break;
			case SPELL_CALL_LIGHTNING:
				time = THING ( thing_call_lightning );
				msg_id = ME_CALL_LIGHTNING;
				break;
			case SKILL_MANIFEST:
				time = THING ( thing_manifest );
				msg_id = ME_MANIFEST;
				break;
			case SPELL_CONTROL_WEATHER:
				msg_id = ME_CONTROL_WEATHER;
				if ( *msg->args && is_abbrev ( msg->args, "worse" ) )
					time = THING ( thing_control_weather_worse );
				else if ( *msg->args && is_abbrev ( msg->args, "better" ) )
					time = THING ( thing_control_weather_better );
				else
				{
					time = 0;
					ch->Send ( "You need to specify, 'better' or 'worse'.\r\n" );
				}
				break;
			case SKILL_PICK_LOCK:
				msg_id = ME_PICK_LOCK;
				break;
			default:
				break;
		}
	}
	else if ( type == THING_SUB )
	{
		switch ( skill )
		{
			case SUB_LUMBERJACK:
				time = THING ( thing_lumberjack );
				msg_id = ME_LUMBERJACK;
				break;
			case SUB_JUGGLE:
				time = THING ( thing_juggle );
				msg_id = ME_JUGGLE;
				break;
			case SUB_THROTTLE:
				time = THING ( thing_throttle );
				msg_id = ME_THROTTLE;
				break;
			case SUB_TUNNELING:
				time = THING ( thing_tunneling );
				msg_id = ME_TUNNELING;
				break;
		}
	}
	else
	{
		log ( "SYSERR: unknown type passed to message event" );
	}
	if ( time == 0 || msg->msg_num == 0 || ( --msg->msg_num ) <= 0 )
	{
		if ( ch )
		{
			ch->ClearMessageEvent ( (event *)msg );
			delete msg;
			if ( type == THING_SUB )
			{
				toggle_sub_status ( ch, skill, STATUS_OFF );
				run_task ( ch );
			}
		}

		return 0;
	}
	else if ( time == -1 )
	{
		/* this means player died while doing action */
		/* and so event_obj was already freed */
		return 0;
	}
	else if ( msg_id != -1 )
	{
		ch->AddMessageEvent ( ( event * ) event_obj, msg_id );
		return time;
	}
	else
		return 0;
}

ACMD ( do_fell )
{
	struct obj_data *o = NULL;
	char tree_name[MAX_INPUT_LENGTH];
	char *temp1;
	int found = FALSE;
	struct message_event_obj *msg = NULL;

	struct obj_data *axe = GET_EQ ( ch, WEAR_WIELD );

	if ( GET_SUB ( ch, SUB_LUMBERJACK ) <= 0 )
	{
		ch->Send ( "You have no idea how to use that command!\r\n" );
		return;
	}

	// Going to remove the !axe to see if lubmerjack will work now -- Prom
	//if ( !axe || GET_OBJ_TYPE ( axe ) != ITEM_AXE )
	if (GET_OBJ_TYPE ( axe ) != ITEM_AXE )
	{
		ch->Send ( "You can't chop trees without a good axe!\r\n" );
		return;
	}
	skip_spaces ( &argument );
	temp1 = one_argument ( argument, tree_name );

	/* sanity check */
	if ( !*tree_name )
	{
		ch->Send ( "What do you wish to fell?\r\n" );
		return;
	}

	if ( ( o =
	            get_obj_in_list_vis ( ch, tree_name, 0,IN_ROOM ( ch )->contents ) ) == NULL )
	{
		ch->Send ( "The tree you need is not here.\r\n" );
		return;
	}
	else
	{
		found = TRUE;
	}


	if ( found != FALSE && ( GET_OBJ_TYPE ( o ) != ITEM_TREE ) )
	{
		ch->Send ( "You can't see the proper tree!\r\n" );
		return;
	}
	if ( found == FALSE )
	{
		ch->Send ( "There is no tree here!\r\n" );
		return;
	}
	if ( ch->HasMessageEvent ( ME_LUMBERJACK ) )
	{
		ch->Send ( "You are in the middle of something else!\r\n" );
		return;
	}


	ch->Send ( "You flex your muscles and swing your axe.\r\n" );

	msg= new message_event_obj ( ch, SUB_LUMBERJACK, THING_SUB, 12, GET_ID ( o ), ( char * ) "" );
	ch->AddMessageEvent ( event_create ( message_event, msg, ( 1 RL_SEC ), EVENT_TYPE_MESSAGE ), ME_LUMBERJACK );


}




ASKILL ( skill_manipulate )
{
	char arg[MAX_INPUT_LENGTH];
	OBJ_DATA *o = NULL;

	one_argument ( argument, arg );
	/* sanity check */
	if ( !*arg )
	{
		ch->Send ( "What do you wish to manipulate?\r\n" );
		return 0;
	}

	if ( ( o =
	            get_obj_in_list_vis ( ch, arg, 0, ch->carrying ) ) == NULL )
	{
		ch->Send ( "The weapon must be in your inventory.\r\n" );
		return 0;
	}

	if ( GET_OBJ_TYPE ( o ) != ITEM_WEAPON )
	{
		ch->Send ( "That isnt a weapon!\r\n" );
		return 0;
	}

	if ( OBJ_FLAGGED ( o, ITEM_ENHANCED ) )
	{
		ch->Send ( "That item has been enhanced already!\r\n" );
		return 0;
	}
	if ( IS_SET_AR ( GET_OBJ_EXTRA ( o ), ITEM_MAGIC ) )
	{
		ch->Send ( "That item has been changed already!\r\n" );
		return 0;
	}
	if ( GET_MANA ( ch ) < 600 )
	{
		ch->Send ( "You don't have enough mana to manipulate!\r\n" );
		return 0;
	}
	obj_from_char ( o );
	SET_BIT_AR ( GET_OBJ_EXTRA ( o ), ITEM_UNIQUE_SAVE );
	GET_OBJ_WEIGHT ( o ) += 10;
	obj_to_char ( o, ch );
	if ( total_chance ( ch, SKILL_MANIPULATE ) < number ( 0, 101 ) )
	{
		GET_WEP_BALANCE ( o ) = number ( 0, 100 );

		ch->Send ( "Your concentration slips and you throw the balance in randomly!\r\n" );
	}
	else
	{
		GET_WEP_BALANCE ( o ) = ( curr_balance ( o ) + perf_balance ( GET_WEP_TYPE ( o ) ) ) /2;
		ch->Send ( "You pour all your energy into changing the balance of the weapon by re-weighting it.\r\n" );
	}
	// Changing this from 1/2 mana to 1/12 of mana
	// Prom 
	// Old -> alter_mana( ch, GET_MANA( ch) /2)
	alter_mana ( ch, GET_MANA ( ch ) /12 );

	return SKILL_MANIPULATE;
}

