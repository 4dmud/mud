/*************************************************************************
*   File: act.informative.c                             Part of CircleMUD *
*  Usage: Player-level commands of an informative nature                  *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

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
#include "descriptor.h"
#include "compressor.h"
#include "romance.h"

/*
New layout for commands
Look/Examine
Get
wear
drop
quaff
drink
kill

for look/examine:
Look <item/player/direction single word>
Look at <item/Mobile <item multiple words>
Look at <item multiple words> in <item multiple words>
Look at <item multiple words> on <item multiple words>
Look in <item/direction multiple words>

get <item one word> <item one word>
Get <item multiple words> from <item multiple words>
Put <item multiple words> in <item multiple words>



*/

/* extern variables */
extern struct help_index_element *help_table;
extern char *help;
extern struct time_info_data time_info;

extern char *credits;
extern char *news;
extern char *info;
extern char *motd;
extern char *imotd;
extern char *wizlist;
extern char *immlist;
extern char *policies;
extern char *handbook;
extern const char *class_abbrevs[];
extern const char *race_abbrevs[];
extern const char *moon_types[];
extern const char *season_types[];
extern const char *fuse_locations[];

/* global */

int boot_high = 0;
int colour_space = 0;

/* extern functions */
char * mob_name_by_vnum ( mob_vnum &v );
int highest_tier ( Character *ch );
const char *how_good ( int percent );
int grand_master ( Character *ch );
const char *simple_class_name ( Character *ch );
size_t proc_colour ( char *inbuf, int colour_lvl, size_t len );
ACMD ( do_action );
ACMD ( do_insult );
int average_damage ( Character *ch );
long find_class_bitvector ( char arg );
//long find_race_bitvector(char arg);
const char *title_male ( int chclass, int level );
const char *title_female ( int chclass, int level );
int speed_update ( Character *ch );
int tier_level ( Character *ch, int chclass );
int parse_class ( char arg );
int spell_size_dice ( Character *ch );
int spell_num_dice ( Character *ch );
int update_award ( Character *ch );
int update_reward ( Character *ch );
char * print_elemental ( int chcl, int weak, char * buf, size_t len );
void container_disp ( Character *ch,OBJ_DATA * obj );

struct time_info_data *real_time_passed ( time_t t2, time_t t1 );
int compute_armor_class ( Character *ch );

/* local functions */
void display_map ( Character *ch );
void look_around ( Character *ch );
void skill_spell_help ( Character *ch, int spell );
int sort_commands_helper ( const void *a, const void *b );
void print_object_location ( int num, struct obj_data *obj,
                             Character *ch, int recur, char *buffer );
void show_obj_to_char ( struct obj_data *object, Character *ch,
                        int mode );
void list_obj_to_char ( struct obj_data *list, Character *ch,
                        int mode, int show );
void show_obj_modifiers ( struct obj_data *obj, Character *ch );
bool is_fused ( Character *ch );
int fused_accuracy ( Character *ch );
int fused_evasion ( Character *ch );
ACMD ( do_look );
ACMD ( do_examine );
ACMD ( do_gold );
ACMD ( do_score );
ACMD ( do_inventory );
ACMD ( do_equipment );
ACMD ( do_time );
ACMD ( do_weather );
ACMD ( do_help );
ACMD ( do_who );
ACMD ( do_users );
ACMD ( do_clients );
ACMD ( do_gen_ps );
void perform_mortal_where ( Character *ch, char *arg );
void perform_immort_where ( Character *ch, char *arg );
ACMD ( do_where );
ACMD ( do_levels );
ACMD ( do_consider );
ACMD ( do_diagnose );
ACMD ( do_colour );
ACMD ( do_toggle );
ACMD ( do_worth );
void sort_commands ( void );
ACMD ( do_commands );
void look_at_char ( Character *i, Character *ch );
void list_one_char ( Character *i, Character *ch );
void list_char_to_char ( Character *list, Character *ch );
void do_auto_exits ( Character *ch );
ACMD ( do_exits );
void look_in_direction ( Character *ch, int dir );
void look_in_obj ( Character *ch, char *arg, struct obj_data *item );
char *find_exdesc ( char *word, struct extra_descr_data *list );
void look_at_target ( Character *ch, char *arg );
void look_above_target ( Character *ch, char *arg );
void look_behind_target ( Character *ch, char *arg );
void look_under_target ( Character *ch, char *arg );
char * primary_class ( Character *ch, char * blank );
char * secondary_class ( Character *ch, char * blank );
char * tertary_class ( Character *ch, char * blank );
char * quatry_class ( Character *ch, char * blank );
void list_damage ( Character *ch );
int even_group ( Character *ch );
const char *class_group_name ( Character *ch );
/* local globals */
int *cmd_sort_info;
bool examine_on = FALSE;

/* For show_obj_to_char 'mode'.    /-- arbitrary */
#define SHOW_OBJ_LONG         0
#define SHOW_OBJ_SHORT        1
#define SHOW_OBJ_ACTION       2
#define MAX_NOTE_LENGTH 1024

ACMD ( do_settime );
int fighter_damroll ( Character *ch );
int caster_damroll ( Character *ch );

/*
 * This function screams bitvector... -gg 6/45/98
 */
void show_obj_to_char ( struct obj_data *obj, Character *ch,
                        int mode )
{

	bool found = FALSE;
	Character *temp;

	if ( !obj || !ch )
	{
		log ( "SYSERR: NULL pointer in show_obj_to_char(): obj=%p ch=%p", obj, ch );
		return;
	}


	switch ( mode )
	{

		case SHOW_OBJ_LONG://start
			ch->Send ( "{cy" );
			if ( GET_LEVEL ( ch ) > LVL_IMMORT )
			{
				ch->Send ( "[%5d]", GET_OBJ_VNUM ( obj ) );

				if ( !IS_NPC ( ch ) && SCRIPT ( obj ) && PRF_FLAGGED ( ch, PRF_HOLYLIGHT ) )
					ch->Send ( "[Trig]" );
				ch->Send ( " " );
			}
			if ( GET_OBJ_VAL ( obj, 1 ) == 0 || !OBJ_SAT_IN_BY ( obj ) )
			{
				if ( !OBJ_FLAGGED ( obj, ITEM_NODISPLAY ) || ( GET_LEVEL ( ch ) > LVL_IMMORT ) )
				{
					if ( GET_OBJ_TYPE ( obj ) == ITEM_SPACEBIKE )
						ch->Send ( "%s [FUEL: %d%%]",obj->description, GET_FUEL ( obj ) > 0 ? GET_FUEL_PERCENTAGE ( obj ) : 0 );
					else
						ch->Send ( "%s", obj->description );
				}
			}
			else if ( GET_OBJ_TYPE ( obj ) == ITEM_SPACEBIKE || GET_OBJ_TYPE ( obj ) == ITEM_FURNITURE )
			{
				for ( temp = OBJ_SAT_IN_BY ( obj ); temp;temp = NEXT_SITTING ( temp ) )
				{
					if ( temp == ch )
						found = TRUE;
				}
				if ( found == TRUE )
				{
					if ( GET_OBJ_TYPE ( obj ) != ITEM_SPACEBIKE )
						ch->Send ( "You are %s upon %s.",
						           GET_POS ( ch ) == POS_SITTING ? "sitting" : "resting", obj->short_description );
					else
						ch->Send ( "You are mounted upon %s.", obj->short_description );
				}
				else ch->Send ( "%s", obj->description );
			}

			break;

		case SHOW_OBJ_SHORT:
		case 3:
		case 4:
			if ( GET_LEVEL ( ch ) > LVL_IMMORT )
			{
				ch->Send ( "[%5d]", GET_OBJ_VNUM ( obj ) );
				if ( !IS_NPC ( ch ) && SCRIPT ( obj ) && PRF_FLAGGED ( ch, PRF_HOLYLIGHT ) )
					ch->Send ( "[Trig]" );
				ch->Send ( " " );
			}
			ch->Send ( "%s", obj->short_description );
			break;

		case SHOW_OBJ_ACTION:
		case 5:
			switch ( GET_OBJ_TYPE ( obj ) )
			{
				case ITEM_NOTE:
					if ( obj->action_description )
					{
						char notebuf[MAX_NOTE_LENGTH + 64];

						snprintf ( notebuf, sizeof ( notebuf ), "There is something written on it:\r\n\r\n%s", obj->action_description );
						page_string ( ch->desc, notebuf, TRUE );
					}
					else
						ch->Send ( "It's blank.\r\n" );
					return;

				case ITEM_DRINKCON:
					ch->Send ( "It looks like a drink container." );
					break;

				default:
					ch->Send ( "You see nothing special.." );
					break;
			}
			break;

		default:
			log ( "SYSERR: Bad display mode (%d) in show_obj_to_char().", mode );
			return;
	}//end of switch

	if ( !OBJ_FLAGGED ( obj, ITEM_NODISPLAY ) || GET_LEVEL ( ch ) > LVL_IMMORT )
		show_obj_modifiers ( obj, ch );
	if ( mode == SHOW_OBJ_LONG )
	{
		if ( !OBJ_FLAGGED ( obj, ITEM_NODISPLAY ) || ( GET_LEVEL ( ch ) > LVL_IMMORT ) )
			ch->Send ( "{c0\r\n" );
	}
	else
		ch->Send ( "{c0\r\n" );


}

// Changing t2 to a different color so it is easier to tell -- Prom
// tier == 1 ? "{cC|" to "{cW|"
#define TIER_COLOUR_LIST(tier) (tier == 0 ? " " : (tier == 1 ? "{cW|" : (tier == 2 ? "{cc|" : (tier == 3 ? "{cB|" : "{cr|"))))
#define TIER_COLOUR_WHO(tier) (tier == 0 ? " " : (tier == 1 ? "{cW" : (tier == 2 ? "{cc" : (tier == 3 ? "{cB" : "{cr"))))

void show_obj_modifiers ( struct obj_data *obj, Character *ch )
{
	if ( IS_OBJ_STAT ( obj, ITEM_BURIED ) && CAN_SEE_OBJ ( ch, obj ) )
		ch->Send ( " (buried)" );
	if ( OBJ_FLAGGED ( obj, ITEM_NODISPLAY ) && GET_LEVEL ( ch ) >= LVL_IMMORT )
		ch->Send ( " (!display)" );

	if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
	{
		if ( IS_OBJ_STAT ( obj, ITEM_INVISIBLE ) )
			ch->Send ( " ...it is invisible" );
		if ( IS_OBJ_STAT ( obj, ITEM_BLESS )
		        && AFF_FLAGGED ( ch, AFF_DETECT_ALIGN ) )
			ch->Send ( " ...it has a holy aura" );  // dark cyan
		if ( IS_OBJ_STAT ( obj, ITEM_MAGIC )
		        && AFF_FLAGGED ( ch, AFF_DETECT_MAGIC ) )
			ch->Send ( " ...it sparkles with magic" ); // dark yellow
		if ( IS_OBJ_STAT ( obj, ITEM_GLOW ) )
			ch->Send ( " ...it is glowing with an inner light" );  //dark yellow
		if ( IS_OBJ_STAT ( obj, ITEM_HUM ) )
			ch->Send ( " ...it is humming with energy" );   //dark cyan
		if ( IS_OBJ_STAT ( obj, ITEM_NODROP ) )
			ch->Send ( " ...it has a dull red aura" ); //dark cyan
	}

}

void list_obj_to_char ( struct obj_data *list, Character *ch,
                        int mode, int show )
{
	struct obj_data *i, *j;
	bool found = FALSE;
	int num;

	for ( i = list; i; i = i->next_content )
	{
		num = 0;

		for ( j = list; j != i; j = j->next_content )
			if ( j->item_number == NOTHING || IS_SET_AR ( GET_OBJ_EXTRA ( j ), ITEM_UNIQUE_SAVE ) )
			{
				if ( strcmp ( j->short_description, i->short_description ) == 0 )
					break;
			}
			else if ( j->item_number == i->item_number )
				break;

		if ( ( GET_OBJ_TYPE ( i ) != ITEM_CONTAINER ) && ( GET_OBJ_TYPE ( i ) != ITEM_SPACEBIKE ) && ( j != i ) )
			continue;


		for ( j = i; j; j = j->next_content )
			if ( j->item_number == NOTHING || IS_SET_AR ( GET_OBJ_EXTRA ( j ), ITEM_UNIQUE_SAVE ) )
			{
				if ( strcmp ( j->short_description, i->short_description ) == 0 )
					num++;
			}
			else if ( j->item_number == i->item_number )
				num++;

		if ( CAN_SEE_OBJ ( ch, i ) )
		{
			if ( ( GET_OBJ_TYPE ( i ) != ITEM_CONTAINER ) && ( GET_OBJ_TYPE ( i ) != ITEM_SPACEBIKE ) && ( num != 1 ) && ( mode!=SHOW_OBJ_LONG || !OBJ_FLAGGED ( i, ITEM_NODISPLAY ) || GET_LEVEL ( ch ) >LVL_IMMORT ) )
			{
				ch->Send ( "(%2i) ", num );
			}
			show_obj_to_char ( i, ch, mode );
			found = TRUE;
		}
	}
	if ( !found && show )
		ch->Send ( " Nothing.\r\n" );
}


void diag_char_to_char ( Character *i, Character *ch )
{
	static struct
	{
		sbyte percent;
		const char *text;
	}
	diagnosis[] =
	{
		{100, "is in excellent condition."},
		{ 90, "has a few scratches."},
		{ 75, "has some small wounds and bruises."},
		{ 50, "has quite a few wounds."},
		{ 30, "has some big nasty wounds and scratches."},
		{ 15, "looks pretty hurt."},
		{  0, "is in awful condition."},
		{ -1, "is bleeding awfully from big wounds."},
	};

	int percent, ar_index;
	const char *pers = PERS ( i, ch );

	if ( GET_MAX_HIT ( i ) > 0 )
		percent = MIN ( 100, ( 100 * GET_HIT ( i ) ) / GET_MAX_HIT ( i ) );
	else
		percent = -1;        /* How could MAX_HIT be < 1?? */


	for ( ar_index = 0; diagnosis[ar_index].percent >= 0; ar_index++ )
		if ( percent >= diagnosis[ar_index].percent )
			break;
	if ( GET_LEVEL ( ch ) > LVL_IMMORT )
		ch->Send ( "%c%s %s {cY(at %d%%){c0\r\n", UPPER ( *pers ), pers + 1,
		           diagnosis[ar_index].text, percent );
	else
		ch->Send ( "%c%s %s\r\n", UPPER ( *pers ), pers + 1,
		           diagnosis[ar_index].text );
}

void show_affect_to_char ( Character *i, Character *ch )
{
	if ( !IS_NPC ( i ) )
	{


		if ( AFK_MSG ( i ) )
			ch->Send ( "   -- {cgAFK: %s{c0\r\n", AFK_MSG ( i ) );
		if ( BUSY_MSG ( i ) )
			ch->Send ( "   -- {cgBUSY: %s{c0\r\n", BUSY_MSG ( i ) );


		if ( GET_LEVEL ( i ) >=LVL_IMMORT && GET_LEVEL ( i ) <=LVL_IMMORT+1 )
			act ( "...$e crackles with creative energy!", FALSE, i, 0, ch, TO_VICT );
		if ( GET_LEVEL ( i ) >=LVL_IMMORT+2 && GET_LEVEL ( i ) <=LVL_IMMORT+3 )
			act ( "...$e is surrounded by a divine light!", FALSE, i, 0, ch,
			      TO_VICT );
		if ( GET_LEVEL ( i ) >LVL_IMMORT+3 )
			act ( "...$e is surrounded by a heavenly aura!", FALSE, i, 0, ch,
			      TO_VICT );
	}

	if ( AFF_FLAGGED ( i, AFF_BURNING ) && !AFF_FLAGGED ( i, AFF_PROT_FIRE ) )
		act ( "\x1B[1;31m...$e is in flames!\x1B[0m", FALSE, i, 0, ch,
		      TO_VICT );

	if ( AFF_FLAGGED ( i, AFF_FLY ) && GET_ALIGNMENT ( i ) <0 )
		act ( "...$e has enormous sooty black wings!", FALSE, i, 0, ch,
		      TO_VICT );
	else  if ( !IS_NPC ( i ) && AFF_FLAGGED ( i, AFF_FLY ) && GET_ALIGNMENT ( i ) >=0 )
		act ( "...$e has enormous silky white wings!", FALSE, i, 0, ch,
		      TO_VICT );

	if ( AFF_FLAGGED ( i, AFF_FREEZING ) && !AFF_FLAGGED ( i, AFF_PROT_COLD ) )
		act ( MXPTAG ( "affect lightskyblue" ) "...$e is stuck inside a huge glacier!"MXPTAG ( "/affect " ),
		      FALSE, i, 0, ch, TO_VICT );
	if ( PLR_FLAGGED ( i, PLR_FROZEN ) )
		act ( MXPTAG ( "affect paleturquoise" ) "...$e's stuck inside a huge iceburg!"MXPTAG ( "/affect " ),
		      FALSE, i, 0, ch, TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_ACIDED ) )
		act ( MXPTAG ( "affect snow" ) "...$e is covered with concentrated acid!"MXPTAG ( "/affect " ), FALSE, i, 0, ch,
		      TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_HYPERACTIVITY ) )
		act ( MXPTAG ( "affect ivory" ) "...$e is bouncing off the walls!"MXPTAG ( "/affect " ), FALSE, i, 0, ch,
		      TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_MIND_FIRE ) )
		act ( MXPTAG ( "affect orange" ) "...$s eyes glow like embers!"MXPTAG ( "/affect " ), FALSE, i, 0, ch,
		      TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_MIND_ICE ) )
		act ( MXPTAG ( "affect lightcyan" ) "...$s eyes are frosted with ice!"MXPTAG ( "/affect " ), FALSE, i, 0, ch,
		      TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_MIND_WATER ) )
		act ( MXPTAG ( "affect aquamarine" ) "...$s eyes hold the ocean!"MXPTAG ( "/affect " ), FALSE, i, 0, ch,
		      TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_MIND_ELEC ) )
		act ( MXPTAG ( "affect lavender" ) "...$s eyelashes crackle with electricity!"MXPTAG ( "/affect " ), FALSE, i, 0, ch,
		      TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_SHIELD_HOLY ) )
		act ( MXPTAG ( "affect" ) "...$e is under a geas."MXPTAG ( "/affect " ), FALSE, i, 0, ch,
		      TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_SANCTUARY ) )
		act ( MXPTAG ( "affect oldlace" ) "...$e glows with a bright light!"MXPTAG ( "/affect " ), FALSE, i,
		      0, ch, TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_BLIND ) )
		act ( MXPTAG ( "affect" ) "...$e is groping around blindly"MXPTAG ( "/affect " ), FALSE, i, 0, ch,
		      TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_FIRE_SHIELD ) )
		act ( MXPTAG ( "affect orangered" ) "...$e is surrounded by a flaming red shield!" MXPTAG ( "/affect " ),
		      FALSE, i, 0, ch, TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_SHIELD ) )
		act ( MXPTAG ( "affect mediumturquoise" ) "...$e is surrounded by a humming blue shield!"MXPTAG ( "/affect " ),
		      FALSE, i, 0, ch, TO_VICT );
	if ( AFF_FLAGGED ( i, AFF_HASTE ) )
		act ( MXPTAG ( "affect snow" ) "...$e is moving with unnatural speed!"MXPTAG ( "/affect " ), FALSE, i, 0,
		      ch, TO_VICT );
}


void look_at_char ( Character *i, Character *ch )
{
	int j, found;
	struct obj_data *tmp_obj;
	char buf2[MAX_STRING_LENGTH];

	if ( !ch->desc )
		return;

	if ( AFF_FLAGGED ( i, AFF_POLY_TOAD ) )
	{
		*ch << "You see " << GET_NAME ( i ) << " in the form of a slimy toad!\r\n";
		return;
	}
	if ( AFF_FLAGGED ( i, AFF_POLY_WOLF ) )
	{
		*ch << "You see " << GET_NAME ( i ) << " in the form of a grey wolf!\r\n";
		return;
	}
	if ( AFF_FLAGGED ( i, AFF_POLY_BOAR ) )
	{
		*ch << "You see " << GET_NAME ( i ) << " in the form of a frisky boar!\r\n";
		return;
	}
	if ( AFF_FLAGGED ( i, AFF_POLY_BEAR ) )
	{
		*ch << "You see " << GET_NAME ( i ) << " in the form of a shaggy bear!\r\n";
		return;
	}
	if ( AFF_FLAGGED ( i, AFF_POLY_LION ) )
	{
		*ch << "You see " << GET_NAME ( i ) << " in the form of a black lion!\r\n";
		return;
	}

	if ( !examine_on )       /*split do_look into two parts */
	{
		if ( i->player.description )
			*ch << i->player.description;
		else
		{
			if ( i == ch )
				ch->Send ( "You see nothing special about yourself.\r\n" );
			else
				act ( "You see nothing special about $m.", FALSE, i, 0, ch, TO_VICT );
		}

		if ( AFK_MSG ( i ) )
			ch->Send ( "{cYAFK: %s{c0\r\n", AFK_MSG ( i ) );
		if ( BUSY_MSG ( i ) )
			ch->Send ( "{cYBUSY: %s{c0\r\n", BUSY_MSG ( i ) );

		if ( !IS_NPC ( i ) )
		{
			if ( ROMANCE ( i ) == 2 )
			{
				/* Char is engaged */
				if ( i == ch )
					ch->Send ( "You have an engagement ring from %s.\r\n", pi.NameById ( PARTNER ( i ) ) );
				else
					ch->Send ( "%s has an engagement ring from %s.\r\n",
					           GET_NAME ( i ), ( ( PARTNER ( i ) ) ==GET_IDNUM ( ch ) ) ? "you" : pi.NameById ( PARTNER ( i ) ) );

			}
			else if ( ROMANCE ( i ) == 3 )
			{
				/* Char is married */
				if ( i == ch )
					ch->Send ( "You have a wedding ring from %s.\r\n", pi.NameById ( PARTNER ( i ) ) );
				else
					ch->Send ( "%s has a wedding ring from %s.\r\n",
					           GET_NAME ( i ), ( ( PARTNER ( i ) ) ==GET_IDNUM ( ch ) ) ? "you" : pi.NameById ( PARTNER ( i ) ) );


			}
			else if ( ROMANCE ( i ) == 1 )
			{
				/* Char is dating */
				if ( i == ch )
					ch->Send ( "You are dating %s.\r\n", pi.NameById ( PARTNER ( i ) ) );
				else
					ch->Send ( "%s is dating %s.\r\n", GET_NAME ( i ),
					           ( ( PARTNER ( i ) ) ==GET_IDNUM ( ch ) ) ? "you" : pi.NameById ( PARTNER ( i ) ) );


			}
			else if ( ROMANCE ( i ) == 0 )
			{
				/* Char is single */
				if ( i == ch )
					ch->Send ( "You are single.\r\n" );
				else
					*ch << GET_NAME ( i ) << " is single.\r\n";

			}
		} /*is npc*/

		/* MatingMod Addition - Essential! */
		if ( ( PREG ( i ) > NOT_PREG ) && !IS_NPC ( i ) )
		{

			if ( GET_LEVEL ( ch ) >= LVL_GOD )
			{
				ch->Send ( "%s %s %d hours away from giving birth.\r\n",
				           i == ch ? "You" : GET_NAME ( i ), i == ch ? "are" : "is", PREG ( i ) );
			}

			if ( PREG ( i ) < 7 )
				ch->Send ( "%s %s giving birth!\r\n",
				           i == ch ? "You" : GET_NAME ( i ), i == ch ? "are" : "is" );
			else if ( PREG ( i ) < ( MONTHS_2 ) )    // Customize these messages, if you want.
				ch->Send ( "%s look%s as if %s may give birth to a healthy baby sometime soon.\r\n",
				           i == ch ? "You" : GET_NAME ( i ), i == ch ? "" : "s", i == ch ? "you" : "she" );    // They're pretty cheesy.
			else if ( PREG ( i ) < ( MONTHS_3 ) )    // Month 8
				ch->Send (
				    "%s %s a large, cumbersome bulge in %s waist.\r\n",
				    i == ch ? "You" : GET_NAME ( i ), i == ch ? "have" : "has", i == ch ? "you" : "her" );
			else if ( PREG ( i ) < ( MONTHS_4 ) )    // Month 7
				ch->Send ( "%s %s already had to let out %s armor.\r\n",
				           i == ch ? "You" : GET_NAME ( i ), i == ch ? "have" : "has", i == ch ? "your" : "her" );
			else if ( PREG ( i ) < ( MONTHS_5 ) )    // Month 6
				ch->Send ( "%s %s a small bulge in %s midsection.\r\n",
				           i == ch ? "You" : GET_NAME ( i ), i == ch ? "have" : "has", i == ch ? "you" : "her" );
			else if ( PREG ( i ) < ( MONTHS_6 ) )    // Month 5
				ch->Send ( "%s %s just beginning to show.\r\n",
				           i == ch ? "You" : GET_NAME ( i ), i == ch ? "are" : "is" );
			else
			{
				ch->Send ( "%s %s a motherly glow...\r\n",
				           i == ch ? "You" : GET_NAME ( i ), i == ch ? "have" : "has" );
			}
		}

		diag_char_to_char ( i, ch );
		show_affect_to_char ( i, ch );

	}
	else
	{

		if ( i->player.description && IS_NPC ( i ) )
			*ch << i->player.description;
		                diag_char_to_char ( i, ch );


		if ( RIDING ( i ) && RIDING ( i )->in_room == i->in_room )
		{
			if ( RIDING ( i ) == ch )
				act ( "$e is mounted on you.", FALSE, i, 0, ch, TO_VICT );
			else
			{
				snprintf ( buf2, sizeof ( buf2 ), "$e is mounted upon %s.", PERS ( RIDING ( i ), ch ) );
				act ( buf2, FALSE, i, 0, ch, TO_VICT );
			}
		}
		else if ( RIDDEN_BY ( i ) && RIDDEN_BY ( i )->in_room == i->in_room )
		{
			if ( RIDDEN_BY ( i ) == ch )
				act ( "You are mounted upon $m.", FALSE, i, 0, ch, TO_VICT );
			else
			{
				snprintf ( buf2, sizeof ( buf2 ), "$e is mounted by %s.",
				           PERS ( RIDDEN_BY ( i ), ch ) );
				act ( buf2, FALSE, i, 0, ch, TO_VICT );
			}
		}

		found = FALSE;
		for ( j = 0; !found && j < NUM_WEARS; j++ )
			if ( GET_EQ ( i, j ) && CAN_SEE_OBJ ( ch, GET_EQ ( i, j ) ) )
				found = TRUE;

		if ( found )
		{
			if ( i == ch )
				act ( "\r\nYou are using:", FALSE, i, 0, ch, TO_VICT );
			else
			{
				ch->Send ( "\r\n" );
				act ( "$n is using:", FALSE, i, 0, ch, TO_VICT );
			}
			for ( j = 0; j < NUM_WEARS; j++ )
				if ( GET_EQ ( i, wear_order_index[j] )
				        && CAN_SEE_OBJ ( ch, GET_EQ ( i, wear_order_index[j] ) ) )
				{
					if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
						ch->Send ( "\x1B[33m" );
					send_to_char ( disp_where ( wear_order_index[j], i ), ch );
					if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
						ch->Send ( "\x1B[0m" );
					show_obj_to_char ( GET_EQ ( i, wear_order_index[j] ), ch,  SHOW_OBJ_SHORT );
				}
		}
		else
			if ( !IS_NPC ( i ) )
				act ( "$n is naked!", FALSE, i, 0, ch, TO_VICT );
		*buf2 = '\0';

		/* The thief path can see what another has in their inventory */
		if ( ch != i && ( ( IS_THIEF ( ch ) || GET_LEVEL ( ch ) >= LVL_GOD ) || ( ( PLR_FLAGGED ( ch, PLR_NEWBIE_HLPR ) ) && GET_CLAN ( i ) == 12 ) ) )
		{
			found = FALSE;
			act ( "\r\nYou attempt to peek at $s inventory:", FALSE, i, 0,
			      ch, TO_VICT );
			for ( tmp_obj = i->carrying; tmp_obj;
			        tmp_obj = tmp_obj->next_content )
			{
				if ( CAN_SEE_OBJ ( ch, tmp_obj )
				        && ( number ( 0, 45 ) < GET_LEVEL ( ch ) ) )
				{
					show_obj_to_char ( tmp_obj, ch, SHOW_OBJ_SHORT );
					found = TRUE;
				}
			}

			if ( !found )
				ch->Send ( "You can't see anything.\r\n" );
		}

	}                 //end of examine on
}

void list_one_char ( Character *i, Character *ch )
{
	struct obj_data *chair = NULL;
	int wizinvis = FALSE;
	char tier_string[] = "T0.";

	static const char *positions[] =
	{
		" is lying here, dead.",
		" is lying here, mortally wounded.",
		" is lying here, incapacitated.",
		" is lying here, stunned.",
		" is sleeping here.",
		" is resting here.",
		" is sitting here.",
		"!FIGHTING!",
		" is standing here.",
		" is floating here."
	};
	if ( IS_NPC ( i ) )
	{
		if ( !i->player.long_descr )
		{
			new_mudlog ( BRF, 51, TRUE, "SYSERR: Mob Vnum %d has NULL (empty) long description!", GET_MOB_VNUM ( i ) );
			return;
		}
		if ( MOB_FLAGGED ( i, MOB_WIZINVIS ) || ( i->player.long_descr && i->player.long_descr[0] == '{' && strlen ( i->player.long_descr ) <= 3 ) )
		{
			if ( GET_LEVEL ( ch ) < LVL_IMMORT )
				return;
			else
				wizinvis = TRUE;
		}
	}

	if ( IS_NPC ( i ) )
	{
		if ( i->mob_specials.head_join )
		{
			ch->Send ( "      {cc({cCLINKED{cc)  [{cw%d - {cy%-15s{cc] linked to [{cg%s{cc]{c0\r\n",GET_MOB_VNUM ( i ), GET_NAME ( i ), GET_NAME ( i->mob_specials.head_join ) );
			return;
		}
	}

	if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
	{  
	   tier_string[1] += ( IS_NPC ( i ) ? MOB_TIER ( i ) : current_class_is_tier_num ( i ) );
	   ch->Send ( "%s%s", tier_string, CBCYN ( ch, C_NRM ) );
	}
	else ch->Send ( "%s%s", TIER_COLOUR_LIST ( ( IS_NPC ( i ) ? MOB_TIER ( i ) : current_class_is_tier_num ( i ) ) ) , CBCYN ( ch, C_NRM ) );
 
	if ( AFF_FLAGGED ( i, AFF_POLY_TOAD ) )
	{
		*ch << "A slimy toad stands here looking vaguely like " << GET_NAME ( i ) << ".\r\n";
		return;
	}
	if ( AFF_FLAGGED ( i, AFF_POLY_WOLF ) )
	{
		*ch << "A grey wolf stands here looking vaguely like " << GET_NAME ( i ) << ".\r\n";
		return;
	}
	if ( AFF_FLAGGED ( i, AFF_POLY_BOAR ) )
	{
		*ch << "A frisky boar stands here looking vaguely like " << GET_NAME ( i ) << ".\r\n";
		return;
	}
	if ( AFF_FLAGGED ( i, AFF_POLY_BEAR ) )
	{
		*ch << "A shaggy bear stands here looking vaguely like " << GET_NAME ( i ) << ".\r\n";
		return;
	}
	if ( AFF_FLAGGED ( i, AFF_POLY_LION ) )
	{
		*ch << "A black lion stands here looking vaguely like " << GET_NAME ( i ) << ".\r\n";
		return;
	}

	if ( IS_NPC ( i ) && i->player.long_descr
	        && GET_POS ( i ) == GET_DEFAULT_POS ( i ) )
	{
		if ( GET_LEVEL ( ch ) > LVL_IMMORT )
		{
			ch->Send ( "%s[%5d]", CBCYN ( ch, C_NRM ), GET_MOB_VNUM ( i ) );
			if ( !IS_NPC ( ch ) && SCRIPT ( i ) && PRF_FLAGGED ( ch, PRF_HOLYLIGHT ) )
				ch->Send ( "[Trig]" );
			ch->Send ( " " );
		}
		if ( AFF_FLAGGED ( i, AFF_INVISIBLE ) )
			ch->Send ( "*" );

		if ( AFF_FLAGGED ( ch, AFF_DETECT_ALIGN ) )
		{
			if ( IS_EVIL ( i ) )
				ch->Send ( "(Red Aura) " );
			else if ( IS_GOOD ( i ) )
				ch->Send ( "(Blue Aura) " );
		}
		if ( wizinvis )
			*ch << "(wizinvis) ";
		if ( !i->mob_specials.teaches_skills.empty() )
			ch->Send ( "[T] " );
		ch->Send ( "%s%s", ( i->mob_specials.join_list != NULL ? CCCYN ( ch, C_NRM ) : "" ),
		           ( wizinvis ? GET_NAME ( i ) : i->player.long_descr ) );

		if ( wizinvis )
			*ch << "\r\n";
		show_affect_to_char ( i, ch );
		return;
	}
	if ( IS_NPC ( i ) )
	{
		if ( GET_LEVEL ( ch ) > LVL_IMMORT )
			ch->Send ( "%s[%5d]",CBCYN ( ch, C_NRM ), GET_MOB_VNUM ( i ) );
		if ( !IS_NPC ( ch ) && SCRIPT ( i ) && PRF_FLAGGED ( ch, PRF_HOLYLIGHT ) )
			ch->Send ( "[Trig]" );
		*ch << " " << CBCYN ( ch, C_NRM );
		*ch << i->player.short_descr;
		*ch << CBCYN ( ch, C_NRM );
	}
	else
	{
		if ( PRF_FLAGGED ( ch, PRF_NOTITLE ) )
			ch->Send ( "{cY@ {cC%s%s%s%s%s%s%s%s",
			           ( PRETITLE ( i ) == NULL ? "{cC" : PRETITLE ( i ) ), ( PRETITLE ( i ) == NULL ? "{cC" : " " ) ,
            CBCYN(ch, C_NRM), GET_NAME ( i ), CBCYN ( ch, C_NRM ), 
        (GET_TITLE(i) == NULL ? "" : " "), GET_TITLE(i), CBCYN ( ch, C_NRM ) );
		else
			ch->Send ( "%s%s%s",
			           CBCYN ( ch, C_NRM ), i->player.name,
			            CBCYN ( ch, C_NRM ) );
	}

	if ( IS_NPC ( i ) && !i->mob_specials.teaches_skills.empty() )
		ch->Send ( " [Master]" );
	if ( AFF_FLAGGED ( i, AFF_INVISIBLE ) )
		ch->Send ( " (invis)" );
	if ( AFF_FLAGGED ( i, AFF_HIDE ) )
		ch->Send ( " (hidden)" );
	if ( !IS_NPC ( i ) && !i->desc )
		ch->Send ( " (linkless)" );
	if ( !IS_NPC ( i ) && PLR_FLAGGED ( i, PLR_WRITING ) )
		ch->Send ( " (writing)" );
	if ( PRF_FLAGGED ( i, PRF_AFK ) )
		ch->Send ( " (AFK)" );
	if ( PRF_FLAGGED ( i, PRF_RP ) )
		ch->Send ( " (RolePlaying)" );

	if ( RIDING ( i ) && HERE ( RIDING ( i ), i ) )
	{
		ch->Send ( " is here, mounted upon " );
		if ( RIDING ( i ) == ch )
			ch->Send ( "you" );
		else
			ch->Send ( "%s", PERS ( RIDING ( i ), ch ) );
		ch->Send ( "." );
	}
	else if ( GET_POS ( i ) != POS_FIGHTING )
	{
		if ( !SITTING ( i ) )
		{
			if ( GET_POS ( i ) == POS_STANDING && i->Flying() )
				ch->Send ( " is flying here." );
			else
				ch->Send ( "%s", positions[ ( int ) GET_POS ( i ) ] );

		}
		else
		{
			chair = SITTING ( i );
			ch->Send ( " is %s upon %s.",
			           ( ( GET_POS ( i ) ==
			               POS_SITTING ) ? "sitting" : "resting" ),
			           ( CAN_SEE_OBJ ( ch, chair ) ? chair->
			             short_description : "something" ) );
		}
	}
	else
	{
		if ( FIGHTING ( i ) )
		{
			ch->Send ( " is here, fighting " );
			if ( FIGHTING ( i ) == ch )
				ch->Send ( "YOU!" );
			else
			{
				if ( i->in_room == FIGHTING ( i )->in_room )
					ch->Send ( "%s!", PERS ( FIGHTING ( i ), ch ) );
				else
					ch->Send ( "someone who has already left!" );

			}
		}
		else            /* NIL fighting pointer */
			ch->Send ( " is here struggling with thin air." );
	}

	if ( AFF_FLAGGED ( ch, AFF_DETECT_ALIGN ) )
	{
		if ( IS_EVIL ( i ) )
			ch->Send ( " (Red Aura)" );
		else if ( IS_GOOD ( i ) )
			ch->Send ( " (Blue Aura)" );
	}
	ch->Send ( "\r\n" );
	if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
		show_affect_to_char ( i, ch );

	ch->Send ( "%s", CBCYN ( ch, C_NRM ) );
}



void list_char_to_char ( Character *list, Character *ch )
{
	Character *i;

	for ( i = list; i; i = i->next_in_room )
		if ( ( ch != i ) && ( i != NULL ) )
		{
			if ( RIDDEN_BY ( i ) && RIDDEN_BY ( i )->in_room == i->in_room )
				continue;
			if ( GET_LEVEL ( ch ) < LVL_IMMORT && IS_NPC ( i ) && i->mob_specials.head_join )
				continue;

			if ( CAN_SEE ( ch, i ) )
				list_one_char ( i, ch );
			else if ( IS_DARK ( IN_ROOM ( ch ) ) && !CAN_SEE_IN_DARK ( ch ) &&
			          AFF_FLAGGED ( i, AFF_INFRAVISION ) )
				*ch << "You see a pair of glowing red eyes looking your way.\r\n";
		}
}


void do_auto_exits ( Character *ch )
{
	int door, slen = 0;
	char tag_open[10], tag_close[10];
	Room * view_room;
	if ( VEHICLE_ROOM == NULL )
	{
		view_room = IN_ROOM ( ch );
		strcpy ( tag_open, "Ex" );
		strcpy ( tag_close, "/Ex" );
	}
	else
	{
		view_room = VEHICLE_ROOM;
		strcpy ( tag_open, "VEx" );
		strcpy ( tag_close, "/VEx" );
	}

	//  ch->Send( "%s", MXPTAG("EXPIRE Exits"));
	ch->Send ( "%s[ Exits: %s", CBGRN ( ch, C_NRM ),
	           CBWHT ( ch, C_NRM ) );

	for ( door = 0; door < NUM_OF_DIRS; door++ )
		if ( W_EXIT ( view_room, door ) && W_EXIT ( view_room, door )->to_room != NULL )
		{
			if ( IS_SET ( W_EXIT ( view_room, door )->exit_info, EX_HIDDEN ) && GET_LEVEL ( ch ) < LVL_IMMORT )
				continue;

			if ( EXIT_FLAGGED ( W_EXIT ( view_room, door ), EX_CLOSED ) )
			{
//@TODO:PROTOCOL: MXP is much easier to do now.  See below 3 commented sections.
				if ( GET_LEVEL ( ch ) > LVL_IMMORT )
				{
					if ( IS_SET ( W_EXIT ( view_room, door )->exit_info, EX_HIDDEN ) )
//						ch->Send ( "{cg{cu%c{c0 {cW", UPPER ( *dirs[door] ) );
						ch->Send ( "{cg{cu%c{c0 {cW", UPPER ( *dirs[door] ) );
					else
//						ch->Send ( "{cg%s%s %s%s%c%s%s%s {cW", MXP_BEG, tag_open, dirs[door], MXP_END, UPPER ( *dirs[door] ), MXP_BEG, tag_close, MXP_END );
						ch->Send ( "{cg\t(%c\t){cW ", toupper( *dirs[door] ) );
					slen++;
				}
			}
			else
			{
//				ch->Send ( "%s%s %s%s%c%s%s%s ", MXP_BEG, tag_open, dirs[door], MXP_END, LOWER ( *dirs[door] ), MXP_BEG, tag_close, MXP_END );
				ch->Send ( "\t(%c\t) ", tolower( *dirs[door] ) );
				slen++;
			}


		}
	if ( !slen )
		ch->Send ( "None!" );

	ch->Send ( "%s]%s\r\n", CBGRN ( ch, C_NRM ),  CCNRM ( ch, C_NRM ) );
}

ACMD ( do_search )
{
	int door, chance = 1;

	if ( IS_AFFECTED ( ch, AFF_BLIND ) )
	{
		ch->Send ( "You're blind, you can't see a damned thing!" );
		return;
	}

	ch->Send ( "You begin to search for hidden exits.\r\n" );

	for ( door = 0; door < NUM_OF_DIRS; door++ )
	{
		if ( EXIT ( ch, door ) && EXIT ( ch, door )->to_room != NULL )
		{
			if ( IS_SET ( EXIT ( ch, door )->exit_info, EX_HIDDEN ) )
			{
				if ( GET_INT ( ch ) >= 17 )
					chance += 1;
				if ( GET_DEX ( ch ) >= 17 )
					chance += 1;

				if ( number ( 1, 6 ) <= chance || GET_LEVEL ( ch ) > LVL_IMMORT )
				{
					ch->Send ( "\r\n{cWYou have found a secret door %s{cx.\r\n", dirs[door] );
					REMOVE_BIT ( EXIT ( ch, door )->exit_info, EX_HIDDEN );
					return;
				}
			}
		}
	}
	ch->Send ( "\r\n{cWYou don't find anything.\r\n{c0" );
}


ACMD ( do_exits )
{
	int door, len = 0;
	char dirnameb[50];
	char *dirname;
	;

	//if (GET_LEVEL(ch) > LVL_IMMORT)
	//ch->Send( MXPTAG("hp") "%d" MXPTAG("/hp") MXPTAG("xhp") "%d" MXPTAG("/xhp"), GET_HIT(ch), GET_MAX_HIT(ch));

	if ( AFF_FLAGGED ( ch, AFF_BLIND ) )
	{
		ch->Send ( "You can't see a damned thing, you're blind!\r\n" );
		return;
	}
	ch->Send ( "Obvious exits:\r\n" );

	for ( door = 0; door < NUM_OF_DIRS; door++ )
	{
		if ( !EXIT ( ch, door ) || EXIT ( ch, door )->to_room == NULL )
			continue;


		len++;
		snprintf ( dirnameb, sizeof ( dirnameb ), "%s", dirs[door] );
		dirname = dirnameb;
		if ( GET_LEVEL ( ch ) >= LVL_IMMORT )
		{

			ch->Send ( "%s%-5s%s - [%5d] %s",MXPTAG ( "Ex" ), CAP ( dirname ), MXPTAG ( "/Ex" ),
			           GET_ROOM_VNUM ( EXIT ( ch, door )->to_room ),
			           EXIT ( ch, door )->to_room->name );
			if ( EXIT_FLAGGED ( EXIT ( ch, door ), EX_CLOSED ) )
				ch->Send ( " (%sClosed%s)", MXPTAG ( "C salmon" ), MXPTAG ( "/C" ) );
			if ( EXIT_FLAGGED ( EXIT ( ch, door ), EX_HIDDEN ) )
				ch->Send ( " (Hidden)" );
			ch->Send ( "\r\n" );
		}
		else
		{
			if ( EXIT_FLAGGED ( EXIT ( ch, door ), EX_HIDDEN ) )
				continue;
			if ( EXIT_FLAGGED ( EXIT ( ch, door ), EX_CLOSED ) )
				ch->Send ( "%s%-5s%s - (%sClosed%s)", MXPTAG ( "Ex" ), CAP ( dirname ), MXPTAG ( "/Ex" ), MXPTAG ( "C salmon" ), MXPTAG ( "/C" ) );
			else
				ch->Send ( "%s%-5s%s - %s", MXPTAG ( "Ex" ), CAP ( dirname ), MXPTAG ( "/Ex" ),
				           IS_DARK ( EXIT ( ch, door )->to_room )
				           && !CAN_SEE_IN_DARK ( ch ) ? "Too dark to tell."
				           : EXIT ( ch, door )->to_room->name );
			ch->Send ( "\r\n" );
		}
	}

	if ( !len )
		ch->Send ( " None.\r\n" );
	else
		ch->Send ( "\r\n" );

}


void parse_room_name ( room_rnum in_room, char *bufptr, size_t len )
{
	if ( !strncmp ( in_room->name, "Room #", 6 ) || !strcmp ( in_room->name, "An unfinished room" ) )
		switch ( SECT ( in_room ) )
		{
			case SECT_SPACE:
				snprintf ( bufptr, len, "Outer Space" );
				break;
			case SECT_FOREST:
				snprintf ( bufptr, len, "Within A Forest" );
				break;
			case SECT_FIELD:
				snprintf ( bufptr, len, "In An Open Field" );
				break;
			case SECT_HILLS:
				snprintf ( bufptr, len, "On A Hill" );
				break;
			case SECT_ROAD:
				snprintf ( bufptr, len, "On A Road" );
				break;
			case SECT_MOUNTAIN:
				snprintf ( bufptr, len, "Climbing A Mountain{cx" );
				break;
			case SECT_WATER_SWIM:
				snprintf ( bufptr, len, "In Shallow Water" );
				break;
			case SECT_WATER_NOSWIM:
				snprintf ( bufptr, len, "Adrift In Deep Water" );
				break;
			case SECT_INSIDE:
				snprintf ( bufptr, len, "The Wilderness" );
				break;
			case SECT_CITY:
				snprintf ( bufptr, len, "Within A City" );
				break;
			case SECT_FLYING:
				snprintf ( bufptr, len, "Floating In The Air" );
				break;
			case SECT_UNDERWATER:
				snprintf ( bufptr, len, "Underwater" );
				break;
			case SECT_ENTRANCE:
				snprintf ( bufptr, len, "An Entrance" );
				break;
			case SECT_ATMOSPHERE:
				snprintf ( bufptr, len, "The Atmosphere Of A Planet" );
				break;
			case SECT_SUN:
				snprintf ( bufptr, len, "A Blazing Sun" );
				break;
			case SECT_BLACKHOLE:
				snprintf ( bufptr, len, "A Black Hole" );
				break;
			case SECT_ICE:
				snprintf ( bufptr, len, "Surrounded By Ice" );
				break;
			case SECT_VEHICLE:
				snprintf ( bufptr, len, "Driving" );
				break;
			case SECT_SWAMP:
				snprintf ( bufptr, len, "A Sticky Swamp" );
				break;
			case SECT_REEF:
				snprintf ( bufptr, len, "A Reef" );
				break;
			// Removing the trailing ! since nothing else
			// has an none alphanumeric symbol. --> Prom
			case SECT_DEATHTRAP:
				snprintf ( bufptr, len, "Death Trap" );
				break;
			case SECT_SNOW:
				snprintf ( bufptr, len, "Snowy Terrain" );
				break;
			case SECT_BADLANDS:
				snprintf ( bufptr, len, "Badlands" );
				break;
			case SECT_RAIL:
				snprintf ( bufptr, len, "Rail" );
				break;

			default:
				snprintf ( bufptr, len,
				           "Unknown sector type. Fix parse_room_name function." );
				break;
		}
	else
		snprintf ( bufptr, len, "%s", in_room->name );
}

void parse_room_description ( room_rnum in_room, char *bufptr, size_t len )
{
	if ( !strncmp ( in_room->GetDescription(), "This description is yet unfinished...\r\n", 25 ) ||
	        compares ( "wilderness", in_room->GetDescription() ) )
		switch ( SECT ( in_room ) )
		{
			case SECT_SPACE:
				snprintf ( bufptr, len,
				           "Darkness surrounds you.  In the distance, you can make out dim points of\r\n"
				           "of light, possibly distant suns in far off galaxies.\r\n" );
				break;
			case SECT_FOREST:
				snprintf ( bufptr, len,
				           "Trees of all different kinds surround the path you are travelling.\r\n" );
				break;
			case SECT_FIELD:
				snprintf ( bufptr, len,
				           "All around you is knee high grass. When the wind blows, the grass sways\r\n"
				           "making you think of waves on the ocean.\r\n" );
				break;
			case SECT_HILLS:
				snprintf ( bufptr, len,
				           "Walking up and down the hills is making you tired.  Luckily the ground\r\n"
				           "is pretty hard, providing you with solid footing.\r\n" );
				break;
			case SECT_ROAD:
				snprintf ( bufptr, len,
				           "As you travel along the road, you begin to wonder if it actually leads\r\n"
				           "anywhere.\r\n" );
				break;
			case SECT_MOUNTAIN:
				snprintf ( bufptr, len,
				           "Travelling here is becoming increasingly more difficult as you continue\r\n"
				           "your journey.\r\n" );
				break;
			case SECT_WATER_SWIM:
				snprintf ( bufptr, len,
				           "The water is shallow here, maybe ten feet or so, making it difficult for\r\n"
				           "large ships to sail here.\r\n" );
				break;
			case SECT_WATER_NOSWIM:
				snprintf ( bufptr, len,
				           "The water is very deep here and very cold making travel by anything other\r\n"
				           "than a ship impossible.\r\n" );
				break;
			case SECT_INSIDE:
				snprintf ( bufptr, len,
				           "Not much to see here.  The last occupants weren't very tidy.  Trash is\r\n"
				           "strewn all over the place.\r\n" );
				break;
			case SECT_CITY:
				snprintf ( bufptr, len,
				           "The smell of sewage permeates the air as you move through the place.\r\n "
				           "Dogs are fighting over food scraps in the alleyways.\r\n" );
				break;
			case SECT_FLYING:
				snprintf ( bufptr, len,
				           "Being light on your feet, you fly about a foot above the ground. Hope\r\n"
				           "there are no low branches to knock you out of the air.\r\n" );
				break;
			case SECT_UNDERWATER:
				snprintf ( bufptr, len,
				           "It's rather peaceful under the water. Every now and then you can feel\r\n"
				           "the currents pushing against you.\r\n" );
				break;
			case SECT_ENTRANCE:
				snprintf ( bufptr, len,
				           "Off in the distance, you hear the sounds of what possibly could be a\r\n"
				           "community of some type.\r\n" );
				break;
			case SECT_ATMOSPHERE:
				snprintf ( bufptr, len,
				           "Air surrounds you and you can feel the gravity from the planet below too.\r\n"
				           "Again the world has an up and down, you are not weightless any more.\r\n" );
				break;
			case SECT_SUN:
				snprintf ( bufptr, len,
				           "The immense heat emerging from the fusion of the sun burns your ship to\r\n"
				           "cinders in a spilt second.  Nothing can survive this scorching heat.\r\n" );
				break;
			case SECT_BLACKHOLE:
				snprintf ( bufptr, len, "No light escapes from here, infact nothing escapes. Ever." );
				break;
			case SECT_VEHICLE:
				snprintf ( bufptr, len,
				           "This is a Vehicle\r\n" );
				break;
			case SECT_SWAMP:
				snprintf ( bufptr, len,
				           "Tufts of coarse sedge and higher reed alternate with patches of sleek mud.\r\n"
				           "Here and there are pools of deeper water, dark and slimy with algae.\r\n" );
				break;
			case SECT_REEF:
				snprintf ( bufptr, len,
				           "The waves are breaking over some submerged rocks closed to the surface. In\r\n"
				           "the swirling, white foam you get glimpses of sharp-toothed, jagged rocks.\r\n" );
				break;
			case SECT_DEATHTRAP:
				snprintf ( bufptr, len,
					   "This is a Death trap!\r\n" );

// Remarked this out and Adding one for DT above this. -> Prom
//			case SECT_TUNDRA:
//				snprintf ( bufptr, len,
//				           "This hard land of permanently frozen subsoil is almost barren, but not quite.\r\n"
//				           "Only the hardiest of lifeforms can survive here. Sometimes the freezing\r\n"
//				           "temperatures of the night cracks the land and offers a place for lichens,\r\n"
//				           "mosses, and even an occasional shrub that becomes stunted by the bitter cold,\r\n"
//				           "manages to spring to life here.\r\n" );
//				break;
			case SECT_SNOW:
				snprintf ( bufptr, len,
				           "The landscape would be boring if not for the higher drifts of snow that have\r\n"
				           "built up in places. It's too deep to dig down and find out what is under there\r\n"
				           "to know what the snow has built up around. Most likely it's just more snow.\r\n"
				           "It's easy to get lost in the monotony of white.\r\n" );
				break;
			case SECT_ICE:
				snprintf ( bufptr, len,
				           "Sheets of treacherously slippery ice sparkle like jewels during the day.\r\n"
				           "Sometimes jagged blocks of expanded ice surge up through cracks and break the\r\n"
				           "otherwise monotonous surroundings. During the day the ice can be blinding,\r\n"
				           "the night is no better as the temperatures drastically fall.\r\n" );
				break;
			case SECT_BADLANDS:
				snprintf ( bufptr, len, 
					   "Dry terrian where softer sedimentary rocks and clay-rich \r\n"
					   "soils have been erroded by water and wind. \r\n");
				break;
			case SECT_RAIL:
				snprintf ( bufptr, len, 
					   "Two parralel rails track through the land forming \r\n" 
					   "a system of transportation.\r\n");
				break;
			default:
				snprintf ( bufptr, len,
				           "Unknown sector type. Fix parse_room_description function.\r\n" );
				break;
		}
	else
		snprintf ( bufptr, len,  "%s", in_room->GetDescription() );

}

void look_at_room ( Character *ch, int ignore_brief )
{
	char tbuf[MAX_STRING_LENGTH];
	Room *rm, *view_room;
	void update_mxp_map ( Character *ch );
	*tbuf = '\0';
	char buf[MAX_INPUT_LENGTH];
	int dir;
	if ( VEHICLE_ROOM == NULL )
		view_room = IN_ROOM ( ch );
	else
		view_room = VEHICLE_ROOM;

	rm = view_room;

	if ( !ch->desc )
		return;

	if ( (is_room_affected(view_room, ROOM_AFF_DARK) && !PRF_FLAGGED(ch, PRF_HOLYLIGHT)) || (IS_DARK ( view_room ) && !CAN_SEE_IN_DARK ( ch )) )
	{
		ch->Send ( "It is %s...\r\n", number ( 0,1 ) ? "dark in here" : "pitch black" );
		return;
	}
	else if ( AFF_FLAGGED ( ch, AFF_BLIND ) )
	{
		ch->Send ( "You see nothing but infinite darkness...\r\n" );
		return;
	}
	ch->Send ( "%s", CBCYN ( ch, C_NRM ) );
	if ( !IS_NPC ( ch ) && PRF_FLAGGED ( ch, PRF_ROOMFLAGS ) )
	{
		sprintbitarray ( ROOM_FLAGS ( view_room ), room_bits, RF_ARRAY_MAX, buf, sizeof ( buf ) );
		if ( ROOM_FLAGGED ( view_room, ROOM_WILDERNESS ) )
		{
			parse_room_name ( view_room, tbuf, sizeof ( tbuf ) );
			ch->Send ( "[%s%5d%s]%s %s [%s %s %s] (WILDERNESS)",
			           CBWHT ( ch, C_NRM ), GET_ROOM_VNUM ( view_room ),
			           CCCYN ( ch, C_NRM ), SCRIPT ( rm ) ? "[{cWTRIG{cC] " : "",
			           tbuf, CBWHT ( ch, C_NRM ), buf,
			           CCCYN ( ch, C_NRM ) );
		}
		else
			ch->Send ( "[%s%5d%s]%s %s [%s %s %s]",
			           CBWHT ( ch, C_NRM ), GET_ROOM_VNUM ( view_room ),
			           CCCYN ( ch, C_NRM ), SCRIPT ( rm ) ? "[{cWTRIG{cC] " : "",
			           view_room->name,
			           CBWHT ( ch, C_NRM ), buf, CCCYN ( ch, C_NRM ) );
		sprinttype ( rm->sector_type, sector_types, tbuf, sizeof ( tbuf ) );
		ch->Send ( " (%s)", tbuf );
	}
	else if ( ROOM_FLAGGED ( view_room, ROOM_WILDERNESS ) )
	{
		parse_room_name ( view_room, tbuf, sizeof ( tbuf ) );
		ch->Send ( "%s", tbuf );
	}
	else if ( ROOM_FLAGGED ( view_room, ROOM_ARENA ) )
	{
		ch->Send ( "%s (%sARENA%s)", view_room->name,
		           CBWHT ( ch, C_NRM ), CCCYN ( ch, C_NRM ) );
	}
	else
		ch->Send ( "%s", view_room->name );

	ch->Send ( "%s\r\n", CCNRM ( ch, C_NRM ) );
	/* autoexits */
	if ( !IS_NPC ( ch ) && PRF_FLAGGED ( ch, PRF_AUTOEXIT ) )
		do_auto_exits ( ch );

	if ( ( !IS_NPC ( ch ) && !PRF_FLAGGED ( ch, PRF_BRIEF ) ) || ignore_brief ||
	        ROOM_FLAGGED ( view_room, ROOM_DEATH ) )
	{
		ch->Send ( "%s", CCGRN ( ch, C_NRM ) );
		if ( ROOM_FLAGGED ( view_room, ROOM_WILDERNESS ) )
		{
			parse_room_description ( view_room, tbuf, sizeof ( tbuf ) );
			ch->Send ( "%s", tbuf );
		}
		else
		{
                    bool found = FALSE;
                    if (view_room->q_description && ch->script && ch->script->global_vars) 
                    {
                       struct trig_var_data *tv;
                       struct q_descr_data *qv;
                       for (tv = ch->script->global_vars; tv; tv = tv->next) {
                           if (found) break;
                       for (qv = view_room->q_description; qv; qv = qv->next){
                           if (!strncmp(tv->name.c_str(), &qv->flag[2], strlen(tv->name.c_str()))) {
                               found = TRUE;
                               ch->Send("%s\r\n", qv->description);
                               break;
                           }
                       }
                       }
                    }
                    if (!found) {
                      if (view_room->n_description && (time_info.hours < 6 || time_info.hours >21)) {
                        if (view_room->tmp_n_description)
                            ch->Send("%s", view_room->tmp_n_description);
                        else
                            ch->Send("%s", view_room->n_description);
                      }
                      else {
                        if (view_room->tmp_description)
                            ch->Send("%s", view_room->tmp_description);
                        else
			    ch->Send ( "%s", view_room->GetDescription() );
                    }
                    }
		}

		ch->Send ( "%s", CCNRM ( ch, C_NRM ) );
	}

	/* list fences and gates */
	for ( dir = 0; dir < NUM_OF_DIRS; dir++ )
	{
		if ( !EXIT ( ch, dir ) )
			continue;
		if ( IS_SET ( EXIT( ch, dir )->exit_info, EX_FENCE_WIRE ) )
			ch->Send ( " There is a fence %s.\r\n", dirs[ dir ] );
		if ( IS_SET ( EXIT( ch, dir )->exit_info, EX_FENCE_GATE_OPEN ) )
			ch->Send ( " There is an open gate in the fence leading %s.\r\n", dirs[ dir ] );
		if ( IS_SET ( EXIT( ch, dir )->exit_info, EX_FENCE_GATE_CLOSED ) )
			ch->Send ( " There is a closed gate in the fence %s.\r\n", dirs[ dir ] );
	}

	/* now list characters & objects */
	ch->Send ( "%s", CCYEL ( ch, C_NRM ) );
	list_obj_to_char ( view_room->contents, ch, 0, FALSE );
	ch->Send ( "%s", CBCYN ( ch, C_NRM ) );
	list_char_to_char ( view_room->people, ch );
	ch->Send ( "%s", CCNRM ( ch, C_NRM ) );

	if ( ch->desc && ch->desc->mxp )
		update_mxp_map ( ch );

	if ( KILL_ALL_ENABLED && PRF_FLAGGED ( ch, PRF_AGGRO ) && view_room == IN_ROOM ( ch ))
	{
	    // This needs to be in an array instead of being defined as a constant.
	    // This is because command_interpreter might try to modify.
	    char temp[] = "kill all";
	    command_interpreter ( ch, temp );
	}
}


void list_scanned_chars ( Character *list, Character *ch,
                          int distance, int door )
{

	const char *how_far[] =
	{
		"close by",
		"a ways off",
		"far off to the"
	};

	Character *i, *j;
	int count = 0, count2 = 0, unique_mob_count = 0, counter, counter2;
	room_rnum is_in;
	vector<int> mob_count;
	bool mob_seen;

	if ( !list )
		return;
	/* this loop is a quick, easy way to help make a grammatical sentence
	   (i.e., "You see x, x, y, and z." with commas, "and", etc.) */
	is_in = IN_ROOM ( ch );
	IN_ROOM ( ch ) = IN_ROOM ( list );
	for ( i = list, counter = 0; i; i = i->next_in_room, counter++ )
	{
		if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
			mob_count.push_back(0);

		/* put any other conditions for scanning someone in this if statement -
		   i.e., if (CAN_SEE(ch, i) && condition2 && condition3) or whatever */
		if ( ! ( MOB_FLAGGED ( i, MOB_WIZINVIS ) || ( i->player.long_descr && i->player.long_descr[0] == '{' && strlen ( i->player.long_descr ) <= 3 ) ) && CAN_SEE ( ch, i ) )
		{
			count++;
			if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
			{
				mob_seen = false;
				for ( j = list, counter2 = 0; counter2 < counter; j = j->next_in_room, counter2++ )
				{
					if ( !str_cmp ( GET_NAME (j), GET_NAME (i) ) )
					{ 
						mob_count[ counter2 ]++; 
						mob_seen = true; 
						break; 
					}	
				}
				if ( !mob_seen )
				{
					mob_count[ counter ] = 1;
					unique_mob_count++;  
				}
			}
		}
	}

	if ( !count )
	{
		IN_ROOM ( ch ) = is_in;
		return;
	}

	for ( i = list, counter = 0; i; i = i->next_in_room, counter++ )
	{

		/* make sure to add changes to the if statement above to this one also, using
		   or's to join them.. i.e.,
		   if (!CAN_SEE(ch, i) || !condition2 || !condition3) */
		if ( MOB_FLAGGED ( i, MOB_WIZINVIS ) || ( i->player.long_descr && i->player.long_descr[0] == '{' && strlen ( i->player.long_descr ) <= 3 ) )
			continue;
		if ( !CAN_SEE ( ch, i ) )
			continue;
		if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) && !mob_count [ counter ] )
			continue;
		if ( !count2++ )
			*ch << "You see " << GET_NAME ( i );
		else
			*ch << GET_NAME ( i );
		if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) ) 
		{
			if ( mob_count [ counter ] > 1 )
				ch->Send ( " (%d)", mob_count [ counter ] );
			if ( --unique_mob_count > 1 )
				ch->Send ( ", " );
			else if ( unique_mob_count == 1 )
				ch->Send ( " and " );
			else
			{
				ch->Send ( " %s %s.\r\n", how_far[distance], dirs[door] );
				break;
			}	
			continue;
		}
		if ( --count > 1 )
			ch->Send ( ", " );
		else if ( count == 1 )
			ch->Send ( " and " );
		else
			ch->Send ( " %s %s.\r\n", how_far[distance], dirs[door] );
	}
	IN_ROOM ( ch ) = is_in;
}


void look_in_direction ( Character *ch, int dir )
{
	Room *room, *nextroom, *orig_room = IN_ROOM ( ch );
	int distance;

	if ( EXIT ( ch, dir ) && !EXIT_FLAGGED ( EXIT ( ch, dir ), EX_HIDDEN ) )
	{
		if ( EXIT ( ch, dir )->general_description
		        && !EXIT_FLAGGED ( EXIT ( ch, dir ), EX_HIDDEN ) )
			ch->Send ( "%s", EXIT ( ch, dir )->general_description );
		else
			*ch << "You see nothing special.\r\n";

		if ( EXIT_FLAGGED ( EXIT ( ch, dir ), EX_CLOSED )
		        && EXIT ( ch, dir )->keyword
		        && !EXIT_FLAGGED ( EXIT ( ch, dir ), EX_HIDDEN ) )
		{
			ch->Send ( "The %s is closed.\r\n",
			           fname ( EXIT ( ch, dir )->keyword ) );
		}
		else if ( EXIT_FLAGGED ( EXIT ( ch, dir ), EX_ISDOOR )
		          && EXIT ( ch, dir )->keyword )
		{
			ch->Send ( "The %s is open.\r\n",
			           fname ( EXIT ( ch, dir )->keyword ) );
		}

		if ( CAN_GO2 ( orig_room, dir ) )
			nextroom = EXIT2 ( orig_room, dir )->to_room;
		else
			nextroom = NULL;

		for ( distance = 0; ( ( nextroom != NULL ) && ( distance < 3 ) );
		        distance++ )
		{

			if ( nextroom->people )
				list_scanned_chars ( nextroom->people, ch, distance, dir );

			room = nextroom;
			if ( CAN_GO2 ( room, dir ) )
				nextroom = EXIT2 ( room, dir )->to_room;
			else
				nextroom = NULL;

		}

	}
	else
		ch->Send ( "Nothing special there...\r\n" );
}



void look_in_obj ( Character *ch, char *arg, struct obj_data *item )
{
	struct obj_data *obj = item;
	Character *dummy = NULL;
	int amt, bits = -100;
	char buf2[MAX_INPUT_LENGTH];

	if ( !obj && !arg && !*arg )
	{
		ch->Send ( "Look in what?\r\n" );
		return;
	}
	else if ( !obj && ! ( bits = generic_find ( arg, FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP, ch, &dummy, &obj ) ) )
		ch->Send ( "There doesn't seem to be %s %s here.\r\n", AN ( arg ), arg );
	else if ( ( GET_OBJ_TYPE ( obj ) != ITEM_DRINKCON ) && ( GET_OBJ_TYPE ( obj ) != ITEM_FOUNTAIN ) && ( GET_OBJ_TYPE ( obj ) != ITEM_CONTAINER ) )
		ch->Send ( "There's nothing inside that!\r\n" );
	else
	{
		if ( GET_OBJ_TYPE ( obj ) == ITEM_CONTAINER )
		{
			if ( OBJVAL_FLAGGED ( obj, CONT_CLOSED ) )
				*ch << "It is closed.\r\n";
			else
			{
				*ch << obj->short_description;
				if ( bits == -100 )
				{
					if ( obj->carried_by )
						bits = FIND_OBJ_INV;
					else if ( obj->in_room )
						bits = FIND_OBJ_ROOM;
					else if ( obj->worn_by )
						bits = FIND_OBJ_EQUIP;
					else
						bits = FIND_OBJ_ROOM;
				}

				switch ( bits )
				{
					case FIND_OBJ_INV:
						*ch << " (carried): \r\n";
						break;
					case FIND_OBJ_ROOM:
						*ch << " (here): \r\n";
						break;
					case FIND_OBJ_EQUIP:
						*ch << " (used): \r\n";
						break;
				}
				container_disp ( ch, obj );
				if ( !PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )				
					*ch << "-------------------------\r\n";
				list_obj_to_char ( obj->contains, ch, SHOW_OBJ_SHORT, TRUE );
			}
		}
		else            /* item must be a fountain or drink container */
		{
			if ( GET_OBJ_VAL ( obj, 1 ) <= 0 )
				*ch << "It is empty.\r\n";
			else
			{
				if ( GET_OBJ_VAL ( obj, 0 ) <= 0
				        || GET_OBJ_VAL ( obj, 1 ) > GET_OBJ_VAL ( obj, 0 ) )
				{
					*ch << "Its contents seem somewhat murky.\r\n";   /* BUG */
				}
				else
				{
					container_disp ( ch, obj );
					amt = ( GET_OBJ_VAL ( obj, 1 ) * 3 ) / GET_OBJ_VAL ( obj, 0 );
					sprinttype ( GET_OBJ_VAL ( obj, 2 ), colour_liquid, buf2, sizeof ( buf2 ) );
					*ch << "It's " << fullness[amt] << "full of a " << buf2 << " liquid.\r\n";
				}
			}
		}
	}
}



char *find_exdesc ( char *word, struct extra_descr_data *list )
{
	struct extra_descr_data *i;

	for ( i = list; i; i = i->next )
		if ( isname ( word, i->keyword ) )
			return ( i->description );

	return ( NULL );
}


/*
 * Given the argument "look at <target>", figure out what object or char
 * matches the target.  First, see if there is another char in the room
 * with the name.  Then check local objs for exdescs.
 *
 * Thanks to Angus Mezick <angus@EDGIL.CCMAIL.COMPUSERVE.COM> for the
 * suggested fix to this problem.
 */
void look_at_target ( Character *ch, char *arg )
{
	int bits, found = FALSE, j, fnum, i = 0;
	Character *found_char = NULL;
	struct obj_data *obj, *found_obj = NULL;
	char *desc;

	if ( !ch->desc )
		return;

	skip_spaces ( &arg );

	if ( !*arg )
	{
		*ch << "Look at what?\r\n";
		return;
	}

	bits = generic_find ( arg,FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP |
	                      FIND_CHAR_ROOM, ch, &found_char, &found_obj );

	/* Strip off "number." from 2.foo and friends. */
	if ( ! ( fnum = get_number ( &arg ) ) )
	{
		*ch << "Look at what?\r\n";
		return;
	}

	/* Does the argument match an extra desc in the char's inventory? */
	for ( obj = ch->carrying; obj && !found; obj = obj->next_content )
	{
		if ( CAN_SEE_OBJ ( ch, obj ) )
			if ( ( desc = find_exdesc ( arg, obj->ex_description ) ) != NULL
			        && ++i == fnum )
			{
				*ch << desc;
				if ( GET_OBJ_TYPE ( obj ) == ITEM_GUN )
				{
					if ( GET_OBJ_VAL ( obj, 2 ) == 0 )
						ch->Send ( "It is unloaded.\r\n" );
					if ( GET_OBJ_VAL ( obj, 2 ) == 1 )
						ch->Send ( "It has one shot left.\r\n" );
					if ( GET_OBJ_VAL ( obj, 2 ) > 1 )
						ch->Send ( "It has %d shots left.\r\n", GET_OBJ_VAL ( obj, 2 ) );
				}
				if ( GET_OBJ_TYPE ( obj ) == ITEM_AMMO )
				{
					if ( GET_OBJ_VAL ( obj, 0 ) == 0 )
						ch->Send ( "Bah, these are blanks.\r\n" );
					if ( GET_OBJ_VAL ( obj, 0 ) == 1 )
						ch->Send ( "You can take one shot with this.\r\n" );
					if ( GET_OBJ_VAL ( obj, 0 ) > 1 )
						ch->Send ( "You can take %d shots with this.\r\n", GET_OBJ_VAL ( obj, 0 ) );
				}
				found = TRUE;
			}
	}

	/* Is the target a character? */
	if ( found_char != NULL )
	{
		look_at_char ( found_char, ch );
		if ( ch != found_char )
		{
			if ( CAN_SEE ( found_char, ch ) )
				act ( "$n looks at you.", TRUE, ch, 0, found_char, TO_VICT );
			act ( "$n looks at $N.", TRUE, ch, 0, found_char, TO_NOTVICT );
		}
		return;
	}

	/* Does the argument match an extra desc of an object in the room? */
	for ( obj = IN_ROOM ( ch )->contents; obj && !found;
	        obj = obj->next_content )
		if ( CAN_SEE_OBJ ( ch, obj ) )
			if ( ( desc = find_exdesc ( arg, obj->ex_description ) ) != NULL
			        && ++i == fnum )
			{
				*ch << desc;
				found = TRUE;
			}

	/* Does the argument match an extra desc in the room? */
	if ( ( desc =
	            find_exdesc ( arg, IN_ROOM ( ch )->ex_description ) ) != NULL
	        && ++i == fnum )
	{
		page_string ( ch->desc, desc, FALSE );
		return;
	}

	/* Does the argument match an extra desc in the char's equipment? */
	for ( j = 0; j < NUM_WEARS && !found; j++ )
		if ( GET_EQ ( ch, j ) && CAN_SEE_OBJ ( ch, GET_EQ ( ch, j ) ) )
			if ( ( desc =
			            find_exdesc ( arg, GET_EQ ( ch, j )->ex_description ) ) != NULL
			        && ++i == fnum )
			{
				*ch << desc;
				if ( GET_OBJ_TYPE ( GET_EQ ( ch, j ) ) == ITEM_GUN )
				{
					if ( GET_OBJ_VAL ( GET_EQ ( ch, j ), 2 ) == 0 )
						ch->Send ( "It is unloaded.\r\n" );
					if ( GET_OBJ_VAL ( GET_EQ ( ch, j ), 2 ) == 1 )
						ch->Send ( "It has one shot left.\r\n" );
					if ( GET_OBJ_VAL ( GET_EQ ( ch, j ), 2 ) > 1 )
						ch->Send ( "It has %d shots left.\r\n", GET_OBJ_VAL ( GET_EQ ( ch, j ), 2 ) );
				}
				found = TRUE;
			}

	/* If an object was found back in generic_find */
	if ( bits )
	{
		if ( !found )
			show_obj_to_char ( found_obj, ch, SHOW_OBJ_ACTION );
		else
		{
			show_obj_modifiers ( found_obj, ch );
			*ch << "\r\n";
		}
	}
	else if ( !found )
		*ch << "You do not see that here.\r\n";
}


ACMD ( do_look )
{

	int look_type;

	if ( !ch->desc )
		return;

	if ( GET_POS ( ch ) < POS_SLEEPING )
		*ch << "You can't see anything but stars!\r\n";
	else if ( AFF_FLAGGED ( ch, AFF_BLIND ) )
		*ch << "You can't see a damned thing, you're blind!\r\n";
	else if ( IS_DARK ( IN_ROOM ( ch ) ) && !CAN_SEE_IN_DARK ( ch ) )
	{
		*ch << "It is pitch black...\r\n";
		list_char_to_char ( IN_ROOM ( ch )->people, ch );   /* glowing red eyes */
	}
	else if ( subcmd == SCMD_READ )
	{

		skip_spaces ( &argument );
		if ( !*argument )
			*ch << "Read what?\r\n";
		else
			look_at_target ( ch, argument );

		return;
	}
	else
	{
		char arg[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH], sdir[MAX_INPUT_LENGTH];
		char *trail;

		int look_at = 0;
		int look_in = 0;
		skip_spaces ( &argument );
		strlcpy ( arg2,argument,sizeof ( arg2 ) );
		//half_chop(argument, arg, arg2);
		argument = any_one_arg ( argument, arg );
		skip_spaces ( &argument );

		if ( !str_cmp ( arg, "at" ) )
			look_at = 1;
		if ( !str_cmp ( arg, "in" ) )
			look_in = 1;
		else
			argument = arg2;
		trail = any_one_arg ( argument, sdir );
		skip_spaces ( &trail );

		examine_on = TRUE;
		if ( !*arg )    /* "look" alone, without an argument at all */
			look_at_room ( ch, 1 );
		else if ( look_in )
			look_in_obj ( ch, argument, NULL );
		/* did the char type 'look <direction>?' */
		else if ( ( look_type = search_block ( sdir, dirs, FALSE ) ) >= 0 )
			look_in_direction ( ch, look_type );
		else if ( look_at )
			look_at_target ( ch, argument );
		else if ( is_abbrev ( sdir, "above" ) )
		{
			look_above_target ( ch, trail );
		}
		else if ( is_abbrev ( sdir, "behind" ) )
		{
			look_behind_target ( ch, trail );
		}
		else if ( is_abbrev ( sdir, "under" ) )
		{
			look_under_target ( ch, trail );
		}
		else if ( is_abbrev ( sdir, "around" ) )
		{
			look_around ( ch );
		}
		else
		{
			look_at_target ( ch, argument );
		}
		examine_on = FALSE;
	}
}



ACMD ( do_examine )
{
	Character *tmp_char;
	struct obj_data *tmp_object;
	char tempsave[MAX_INPUT_LENGTH];
	char arg[MAX_STRING_LENGTH];
	char arg2[MAX_STRING_LENGTH];
	char *arg_copy = arg2;
	char *desc;

	one_argument ( argument, arg );

	if ( !*arg )
	{
		*ch << "Examine what?\r\n";
		return;
	}
	/* look_at_target() eats the number. */
	strcpy ( tempsave, arg );

	generic_find ( arg, FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_CHAR_ROOM |
	               FIND_OBJ_EQUIP, ch, &tmp_char, &tmp_object );



	if ( tmp_object )
	{
		int has_identifier ( struct obj_data *obj, long id );
		void identify_object ( Character *ch, OBJ_DATA *obj );

		*ch << "You examine " << tmp_object->short_description << ":\r\n";

		strcpy ( arg2, arg );
		get_number ( &arg_copy );
		if ( ( desc = find_exdesc ( arg_copy, tmp_object->ex_description ) ) != NULL )
			page_string ( ch->desc, desc, FALSE );
		else ch->Send ( "You see nothing special.\r\n" );

		if ( GET_OBJ_INNATE ( tmp_object ) && affected_by_spell ( ch, SPELL_DETECT_MAGIC ) )
			*ch << "\r\nYou sense that this item is further embued with the spell "<<  skill_name ( GET_OBJ_INNATE ( tmp_object ) ) << ".\r\n";
		if ( ( GET_OBJ_TYPE ( tmp_object ) == ITEM_DRINKCON ) ||
		        ( GET_OBJ_TYPE ( tmp_object ) == ITEM_FOUNTAIN ) ||
		        ( GET_OBJ_TYPE ( tmp_object ) == ITEM_CONTAINER ) )
		{
			*ch << "When you look inside, you see:\r\n";
			look_in_obj ( ch, NULL, tmp_object );
		}

		if ( has_identifier ( tmp_object, GET_ID ( ch ) ) )
		{
			*ch << "You remember the time you identified " << tmp_object->short_description << ":" << "\r\n";
			identify_object ( ch, tmp_object );
		}
	}
	else if ( tmp_char )
	{
		*ch << "You examine " << GET_NAME ( tmp_char ) << ":\r\n";

		examine_on = FALSE;
		look_at_target ( ch, tempsave );  /* strcpy: OK */
	}
	else
	{
		*ch << "You can't see that here.\r\n";
	}
}

char *scan_zone_mobs ( zone_rnum zone_nr, char *buf, size_t len )
{
	Character *i;
	int low = 70, high = 0, t_m = 0, t_l = 0, avg;


	for ( i = character_list; i; i = i->next )
	{
		if ( IN_ROOM ( i ) == NULL )
			continue;
		if ( GET_LEVEL ( i ) <= 0 || GET_LEVEL ( i ) > 70 )
			continue;
		if ( IN_ROOM ( i )->zone != zone_nr )
			continue;
		if ( !IS_NPC ( i ) )
			continue;
		t_m++;
		t_l += GET_LEVEL ( i );
		if ( GET_LEVEL ( i ) < low )
			low = GET_LEVEL ( i );
		if ( GET_LEVEL ( i ) > high )
			high = GET_LEVEL ( i );

	}
	if ( t_m == 0 )
		snprintf ( buf, len, "No mobs in this zone." );
	else
	{
		avg = ( ( ( t_l + t_m ) > 0 ) ? ( t_l/t_m ) : 0 );
		snprintf ( buf, len, "Level Range: (%d - %d) Avg: %d", low, high, avg );
	}

	return buf;
}

void look_around ( Character *ch )
{

	char zonename[MAX_INPUT_LENGTH];
	size_t len, x;

	if ( IN_ROOM ( ch ) == NULL )
	{
		return;
	}
	if ( IN_ROOM ( ch )->zone < 0 )
	{
		return;
	}

	if ( !zone_table[IN_ROOM ( ch )->zone].name || !*zone_table[IN_ROOM ( ch )->zone].name )
		return;

	strlcpy ( zonename, zone_table[IN_ROOM ( ch )->zone].name, sizeof ( zonename ) );
	if ( !*zonename )
		return;
	len = strlen ( zonename );
	for ( x = 1; x < len; x++ )
		if ( zonename[x] == '-' && zonename[x-1] == ' ' )
		{
			zonename[x] = '\0';
			break;
		}

	if ( zone_table[IN_ROOM ( ch )->zone].dimension )
		ch->Send ( "\r\n{cRYou are in the %s dimension.{c0", dimension_types[zone_table[IN_ROOM ( ch )->zone].dimension] );
	ch->Send ( "\r\n{cyYou are in %s{c0\r\n", zonename );
	ch->Send ( "{cc%s{C0\r\n", scan_zone_mobs ( IN_ROOM ( ch )->zone, zonename, sizeof ( zonename ) ) );
}

ACMD ( do_gold )
{
	char goldhand[50], goldbank[50], goldtot[50];

	commafmt ( goldhand, sizeof ( goldhand ), ch->Gold ( 0, GOLD_HAND ) );
	commafmt ( goldbank, sizeof ( goldbank ), ch->Gold ( 0, GOLD_BANK ) );
	commafmt ( goldtot, sizeof ( goldtot ), ch->Gold ( 0, GOLD_HAND ) + ch->Gold ( 0, GOLD_BANK ) );

	if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
	{
	   ch->Send ( "\r\n{cc4 DIMENSIONS FINANCIAL SERVICES{c0\r\n" );
 	   ch->Send ( "{ccGold in Bank{cW: [{cy%19s{cW]{c0\r\n", goldbank );
	   ch->Send ( "{ccGold Carried{cW: [{cy%19s{cW]{c0\r\n", goldhand );
	   ch->Send ( "{ccTotal Balance{cW: [{cy%19s{cW]{c0\r\n", goldtot );
	   ch->Send ( "{ccTokens on file{cW: %5d brass, %5d bronze, %5d silver, %5d gold.{c0\r\n",
	           GET_BRASS_TOKEN_COUNT ( ch ), GET_BRONZE_TOKEN_COUNT ( ch ),
	           GET_SILVER_TOKEN_COUNT ( ch ), GET_GOLD_TOKEN_COUNT ( ch ) );
	   ch->Send ( "{ccTradepoints{cW: %5d{c0\r\n", TRADEPOINTS ( ch ) );
	   ch->Send ( "{ccTotal Deeds{cW: %5d{c0\r\n", GET_DEED_COUNT( ch ) );
	   return;
	}

	ch->Send ( "\r\n{cY        _       {cc            4 DIMENSIONS FINANCIAL SERVICES{c0\r\n" );
	ch->Send ( "{cY  _    |-|  _   {cw-------------------------------------------{c0\r\n" );
	ch->Send ( "{cY |-|_  |-|_|-|  {ccGold in Bank{cW:         [{cy%19s{cW]{c0\r\n",goldbank );
	ch->Send ( "{cY |_|-| |_|-|_|  {cw-------------------------------------------{c0  \r\n" );
	ch->Send ( "{cY |_|-| |_|-|_|  {ccGold Carried{cW:         [{cy%19s{cW]{c0\r\n",goldhand );
	ch->Send ( "{cY   |_|   |_|    {cw-------------------------------------------{c0\r\n" );
	ch->Send ( "                {ccTotal Balance{cW:        [{cy%19s{cW]{c0\r\n",goldtot );
	ch->Send ( "                {cw-------------------------------------------{c0\r\n" );
	ch->Send ( "                {ccTokens on file{cW: %5d brass,  %5d bronze.\r\n"
	           "                                %5d silver, %5d gold.{c0\r\n",
	           GET_BRASS_TOKEN_COUNT ( ch ), GET_BRONZE_TOKEN_COUNT ( ch ),
	           GET_SILVER_TOKEN_COUNT ( ch ), GET_GOLD_TOKEN_COUNT ( ch ) );
	ch->Send ( "                {cw-------------------------------------------{c0\r\n" );
	ch->Send ( "                {ccTradepoints   {cW: %5d  {c0\r\n",TRADEPOINTS ( ch ) );
	ch->Send ( "                {ccTotal Deeds   {cW: %5d  {c0\r\n", GET_DEED_COUNT(ch));
}

ACMD ( do_tiername )
{
	int i, j;
	char thebuf[512];
	one_argument ( argument, thebuf );
	j = parse_class ( *thebuf );
	if ( j==-1 )
	{
		ch->Send ( "Invalid name" );
		return;
	}

	ch->Send ( "Classes in the %s Specialization:\r\n", class_name[j].name[0] );
	for ( i = 0; i < 5;i++ )
		ch->Send ( "Class Name:%-12s Tier Num:%d\r\n", class_name[j].name[i], i );

}

#define TIER (current_class_is_tier_num(ch))
#define TIER_FORMAT "[ %d. %s%2s%s - %s%-12s%s - %s%.1d%s ]"



char * primary_class ( Character *ch, char * blank )
{
	sh_int t = TIER, cl = GET_CLASS ( ch );
	size_t len;

	//t = (GET_CLASS_TIER(ch) ? tier_level(ch, t) : 0);
	if ( PLR_FLAGGED ( ch, PLR_NEEDS_CLASS ) )
		len = sprintf ( blank,  TIER_FORMAT, 1,
		                CCYEL ( ch,_clrlevel ( ch ) ), CLASS_ABBR ( ch ),CCWHT ( ch,_clrlevel ( ch ) ),
		                CCCYN ( ch,_clrlevel ( ch ) ), class_group_name ( ch ),CCWHT ( ch,_clrlevel ( ch ) ),
		                CCYEL ( ch,_clrlevel ( ch ) ),t,CCWHT ( ch,_clrlevel ( ch ) ) );
	else
		len = sprintf ( blank,  TIER_FORMAT, 1,
		                CCYEL ( ch,_clrlevel ( ch ) ), CLASS_ABBR ( ch ),CCWHT ( ch,_clrlevel ( ch ) ),
		                CCCYN ( ch,_clrlevel ( ch ) ),grand_master ( ch ) ? "Grand Master" :class_name[cl].name[t],CCWHT ( ch,_clrlevel ( ch ) ),
		                CCYEL ( ch,_clrlevel ( ch ) ),t,CCWHT ( ch,_clrlevel ( ch ) ) );
	blank[len--] = '\0';
	return blank;

}

char * secondary_class ( Character *ch, char * blank )
{
	sh_int cl_next = -1;
	sh_int tl = 0;
	size_t len;

	cl_next = GET_REMORT ( ch );
	if ( cl_next == -1 )
		return ( char * ) "";

	tl = ( GET_REMORT_TIER ( ch ) ? tier_level ( ch, cl_next ) : 0 );
	len = sprintf ( blank, TIER_FORMAT, 2,
	                CCYEL ( ch,_clrlevel ( ch ) ), class_abbrevs[cl_next],CCWHT ( ch,_clrlevel ( ch ) ),
	                CCCYN ( ch,_clrlevel ( ch ) ),grand_master ( ch ) ? "Grand Master" :class_name[cl_next].name[tl],CCWHT ( ch,_clrlevel ( ch ) ),
	                CCYEL ( ch,_clrlevel ( ch ) ),tl,CCWHT ( ch,_clrlevel ( ch ) ) );
	blank[len--] = '\0';
	return blank;
}

char * tertary_class ( Character *ch, char * blank )
{
	sh_int cl_next = -1, tl = 0;
	size_t len;

	cl_next = GET_REMORT_TWO ( ch );
	if ( cl_next == -1 )
		return ( char * ) "";

	tl = ( GET_REMORT_TWO_TIER ( ch ) ? tier_level ( ch, cl_next ) : 0 );
	len = sprintf ( blank, TIER_FORMAT, 3,
	                CCYEL ( ch,_clrlevel ( ch ) ), class_abbrevs[cl_next],CCWHT ( ch,_clrlevel ( ch ) ),
	                CCCYN ( ch,_clrlevel ( ch ) ),grand_master ( ch ) ? "Grand Master" :class_name[cl_next].name[tl],CCWHT ( ch,_clrlevel ( ch ) ),
	                CCYEL ( ch,_clrlevel ( ch ) ),tl,CCWHT ( ch,_clrlevel ( ch ) ) );
	blank[len--] = '\0';
	return blank;

}

char * quatry_class ( Character *ch, char * blank )
{
	sh_int cl_next = -1, tl = 0;
	size_t len;

	cl_next = GET_REMORT_THREE ( ch );
	if ( cl_next == -1 )
		return ( char * ) " ";

	tl = ( GET_REMORT_THREE_TIER ( ch ) ? tier_level ( ch, cl_next ) : 0 );
	len = sprintf ( blank, TIER_FORMAT, 4,
	                CCYEL ( ch,_clrlevel ( ch ) ), class_abbrevs[cl_next],CCWHT ( ch,_clrlevel ( ch ) ),
	                CCCYN ( ch,_clrlevel ( ch ) ), grand_master ( ch ) ? "Grand Master" : class_name[cl_next].name[tl],CCWHT ( ch,_clrlevel ( ch ) ),
	                CCYEL ( ch,_clrlevel ( ch ) ),tl,CCWHT ( ch,_clrlevel ( ch ) ) );

	blank[len--] = '\0';
	return blank;

}
/*
(*)-----------------------------------------Class--TierName-----Tier--(*)
| | Name: Elenya              Level: 14     [ Gy - Gypsy        - 0 ] | |
| | Race: Elf     Age: 23     Align: 87                               | |
| |  Sex: Female   Practices: 2                                       | |
| |-------------------------------------------------------------------| |
| | Hit  Points: [   131][   131]     Speed: [   376]    AC: [  80]   | |
| | Mana Points: [   202][   202]  Hitroll: [   5]    Damroll: [   0] | |
| | Move Points: [   111][   111]    Attack: [  0]    Defence: [  0]  | |
| |-------------------------------------------------------------------| |
| |    Kills: [   237]     Deaths: [     5]     Deathtraps: [   0]    | |
| |->                              <|>                              <-| |
| |-------------------------------------------------------------------| |
| |   STR: 12/0     INT: 18   WIS: 18   CON: 13   DEX: 11   CHA: 10   | |
| |-------------------------------------------------------------------| |
| |     Hunger: [100%]      Thirst:[  5%]      Intoxication:[ 41%]    | |
| |-------------------------------------------------------------------| |
| |   Exp. Total: 1,644,211        Till Next Level: -186,373          | |
| |-------------------------------------------------------------------| |
| |        You have been playing for [ 70] days and [11] hours        | |
-------------------------------------------------------------------------
*/

ACMD ( do_score )
{
	struct time_info_data playing_time;

	//char goldy[50], goldy2[50];
	char *blank, blankbuf[100];

	int class_damroll ( Character *ch );
	char webp[250];
	char webs[250], shld[250];
	int primin = 0, primax = 0, secmin = 0, secmax = 0;
	int wep_num = 0;
	float staff = 0.0;
	float shortmulti = 1.0;
	//  float damspeed = (DAM_SPEED_MULTI(ch));
	float blocking = 0.0;
	int remorts = MIN(REMORTS(ch), 50);
bool is_casting = GET_CLASS ( ch ) == CLASS_PRIEST || GET_CLASS ( ch ) == CLASS_MAGE || GET_CLASS ( ch ) == CLASS_ESPER || has_staff ( ch );

	
	blank = blankbuf;

	if ( IS_NPC ( ch ) )
		return;

	if ( AFF_FLAGGED ( ch, AFF_POLY_TOAD ) )
	{
		ch->Send ( "You puzzle, but your toad mind can't comprehend that!\r\n" );
		return;
	}
	if ( AFF_FLAGGED ( ch, AFF_POLY_WOLF ) )
	{
		ch->Send ( "You puzzle, but your wolf mind can't comprehend that!\r\n" );
		return;
	}
	if ( AFF_FLAGGED ( ch, AFF_POLY_BOAR ) )
	{
		ch->Send ( "You puzzle, but your boar mind can't comprehend that!\r\n" );
		return;
	}
	if ( AFF_FLAGGED ( ch, AFF_POLY_BEAR ) )
	{
		ch->Send ( "You puzzle, but your bear mind can't comprehend that!\r\n" );
		return;
	}
	if ( AFF_FLAGGED ( ch, AFF_POLY_LION ) )
	{
		ch->Send ( "You puzzle, but your lion mind can't comprehend that!\r\n" );
		return;
	}

	if ( ( blocking = MIN ( 50, FTOI ( ( AFF_FLAGGED ( ch, AFF_SHIELD_STATIC ) ? 2.5f*apply_ac ( ch, WEAR_SHIELD ) : apply_ac ( ch, WEAR_SHIELD ) ) ) ) ) )
		snprintf ( shld, sizeof ( shld ), "Shield Block Chance: %2.2f%%", blocking );
	else
		strcpy ( shld, "  " );


	if ( ( wep_num = has_weapon ( ch ) ) > 0 )
	{
		if ( wep_num >= 1 )
		{
			primin = size_dice_wep ( ch, WEAPON_PRIM_AFF );
			primax =
			    size_dice_wep ( ch, WEAPON_PRIM_AFF ) + ( size_dice_wep ( ch, WEAPON_PRIM_AFF ) * num_dice_wep ( ch, WEAPON_PRIM_AFF ) );

			if ( total_chance ( ch, SKILL_LONGARM ) > 0 && !is_short_wep ( GET_EQ ( ch, WEAR_WIELD ) ) )
				shortmulti += ( LONG_WEP_MULTI * ( ( float ) total_chance ( ch, SKILL_LONGARM ) ) ) /100.0;
			else if ( total_chance ( ch, SKILL_SHORT_BLADE ) > 0 && is_short_wep ( GET_EQ ( ch, WEAR_WIELD ) ) )
				shortmulti += ( SHORT_WEP_MULTI_ROGUE * ( ( float ) total_chance ( ch, SKILL_SHORT_BLADE ) ) ) /100.0;
			else
				shortmulti = 1.0f;

			primin += fighter_damroll ( ch );
			primax += fighter_damroll ( ch );
			primin = FTOI ( primin * ( ( wep_num==1 ) ? 1.1f : 1.0f ) );
			primax = FTOI ( primax * ( ( wep_num==1 ) ? 1.1f : 1.0f ) );
			primin = FTOI ( primin * shortmulti );
			primax = FTOI ( primax * shortmulti );

			primin = FTOI ( primin * race_dam_mod ( GET_RACE ( ch ), 0 ) );
			primax = FTOI ( primax * race_dam_mod ( GET_RACE ( ch ), 0 ) );
			primin += FTOI ( ( ( float ) primin * ( ( ( float ) remorts * 0.005f ) ) ) );
			primax += FTOI ( ( ( float ) primax * ( ( ( float ) remorts * 0.005f ) ) ) );

			if ( GET_MASTERY ( ch, CLASS_GYPSY ) )
			{
				primin += primin/5;
				primax += primax/5;
			}
		}
		if ( wep_num == 2 )
		{
			secmin = size_dice_wep ( ch, WEAPON_SECO_AFF );
			secmax =
			    size_dice_wep ( ch, WEAPON_SECO_AFF ) + ( size_dice_wep ( ch, WEAPON_SECO_AFF ) * num_dice_wep ( ch, WEAPON_SECO_AFF ) );
			if ( total_chance ( ch, SKILL_LONGARM ) > 0 && !is_short_wep ( GET_EQ ( ch, WEAR_WIELD_2 ) ) )
				shortmulti += ( ( LONG_WEP_MULTI * ( ( float ) total_chance ( ch, SKILL_LONGARM ) ) ) /100.0 );
			else if ( total_chance ( ch, SKILL_SHORT_BLADE ) > 0 && is_short_wep ( GET_EQ ( ch, WEAR_WIELD_2 ) ) )
				shortmulti += ( ( SHORT_WEP_MULTI_ROGUE * ( ( float ) total_chance ( ch, SKILL_SHORT_BLADE ) ) ) /100.0 );
			else
				shortmulti = 1.0f;
			secmin += fighter_damroll ( ch );
			secmax += fighter_damroll ( ch );
			secmin = FTOI ( secmin * shortmulti );
			secmax = FTOI ( secmax * shortmulti );
			secmin = FTOI ( secmin * race_dam_mod ( GET_RACE ( ch ), 0 ) );
			secmax = FTOI ( secmax * race_dam_mod ( GET_RACE ( ch ), 0 ) );
			secmin += FTOI ( ( float ) secmin * ( ( ( float ) remorts * 0.005f ) ) );
			secmax += FTOI ( ( float ) secmax * ( ( ( float ) remorts * 0.005f ) ) );

			if ( GET_MASTERY ( ch, CLASS_GYPSY ) )
			{
				secmin += secmin/5;
				secmax += secmin/5;
			}
		}


		snprintf ( webp, sizeof ( webp ), "Primary Weapon: %4d to %-4d", primin, primax );
		if ( wep_num == 2 )
			snprintf ( webs, sizeof ( webs ), "Secondary Weapon: %4d to %-4d", secmin, secmax );
		else if ( blocking )
			snprintf ( webs, sizeof ( webs ), "%s", shld );
		else
			snprintf ( webs, sizeof ( webs ), " " );
	}
	else if ( is_casting )
	{
		if ( ( staff = has_staff ( ch ) ) != 0.0f )
			snprintf ( webs, sizeof ( webs ), " Focus Multiplier:  %3.2f", staff );
		else if ( blocking )
			snprintf ( webs, sizeof ( webs ), "%s", shld );
		else
			snprintf ( webs, sizeof ( webs ), " " );
		primin = spell_size_dice ( ch );
		primin += caster_damroll ( ch );
		primax = ( spell_size_dice ( ch ) + ( spell_size_dice ( ch ) *spell_num_dice ( ch ) ) );
		primax += caster_damroll ( ch );
		primin = FTOI ( primin * ( staff ? staff : 1.0 ) );
		primax = FTOI ( primax * ( staff ? staff : 1.0 ) );
		/*
		if (GET_RACE(ch) == RACE_CENTAUR && !RIDDEN_BY(ch))
		{
		  primin *= (1.0 + (total_chance(ch, SKILL_MOUNTED_COMBAT) * 0.003));
		  primax *= (1.0 + (total_chance(ch, SKILL_MOUNTED_COMBAT) * 0.003));
		}

		if  (total_chance(ch, SKILL_MOUNTED_COMBAT) && RIDING(ch))
		{

		  primin *= (1.0 + (total_chance(ch, SKILL_MOUNTED_COMBAT) * 0.003));
		  primax *= (1.0 + (total_chance(ch, SKILL_MOUNTED_COMBAT) * 0.003));
		  primin += average_damage(RIDING(ch));
		  primax += average_damage(RIDING(ch));
		}
		*/
		primin = FTOI ( primin * race_dam_mod ( GET_RACE ( ch ), TRUE ) );
		primax = FTOI ( primax * race_dam_mod ( GET_RACE ( ch ), TRUE ) );
		primin += FTOI ( primin * ( ( remorts * 0.005 ) ) );
		primax += FTOI ( primax * ( ( remorts * 0.005 ) ) );
		/*
		primin *= damspeed;
		primax *= damspeed;
		*/


		if ( GET_MASTERY ( ch, CLASS_MAGE ) )
		{
			primax += primax/5;
			primin += primin/5;
		}

		snprintf ( webp, sizeof ( webp ), "Magic Damage: %4d to %-4d", primin, primax );
	}
	else
	{
		snprintf ( webp, sizeof ( webp ), " " );
		snprintf ( webs, sizeof ( webs ), " " );
	}

	if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
	{
	ch->Send ( "{cwName:{cc %-16s{cw Level: {cy%2d {cw%-29s\r\n",
	           GET_NAME ( ch ),GET_LEVEL ( ch ), primary_class ( ch, blank ) );
	ch->Send (
	    "{cwRace:{cc %-9s        {cwAlign: {cc%-5d {cw%-29s\r\n",
	    race_name ( ch ),  GET_ALIGNMENT ( ch ), secondary_class ( ch, blank ) );

	ch->Send (
	    "{cwSex:{cy %-7s       {cwPractices: {cc%-6d {cw%-29s\r\n",
	    ( GET_SEX ( ch ) == SEX_MALE ? "Male" : ( GET_SEX ( ch ) ? "Female" :  "Neutral" ) ),
	    GET_PRACTICES ( ch ), tertary_class ( ch, blank ) );

	ch->Send (
	    "{cwClan: {cy%-17s {cwRank: {cc%-2d {cw%-29s\r\n",
	    ( ( !GET_CLAN ( ch ) ) ? "<none>" : clan[find_clan_by_id ( GET_CLAN ( ch ) ) ].name ),
	    GET_CLAN_RANK ( ch ), quatry_class ( ch, blank ) );

	ch->Send (
	    "{cwHit  Points: [{cc%6d{cw][{cc%6d{cw] Speed: [{cc%6d{cw] AC: [{cc%4d{cw]\r\n"
	    "{cwMana Points: [{cc%6d{cw][{cc%6d{cw] Hitroll: [{cc%4d{cw] Dam-Bonus: [{cc%4d{cw]\r\n"
	    "{cwMove Points: [{cc%6d{cw][{cc%6d{cw] Accuracy: [{cy%4d{cw] Evasion: [{cy%4d{cw]{c0\r\n",
	    GET_HIT ( ch ), GET_MAX_HIT ( ch ),speed_update ( ch ),ch->compute_armor_class(),
	    GET_MANA ( ch ), GET_MAX_MANA ( ch ), GET_HITROLL ( ch ),   is_casting ? caster_damroll ( ch ) : fighter_damroll( ch ),
	    GET_MOVE ( ch ), GET_MAX_MOVE ( ch ), accuracy_tot ( ch ), evasion_tot ( ch ) );

	if ( IS_PK ( ch ) )
		ch->Send ( "{cwPKwin: [{cc%6d{cw] PKloss: [{cc%6d{cw] PK-Points: [{cc%6d{cw]\r\n",
		           GET_PK_CNT ( ch ), GET_PK_RIP ( ch ), GET_PK_POINTS ( ch ) );

	ch->Send (
	    "{cwKills: [{cc%6d{cw] Deaths: [{cc%6d{cw] Deathtraps: [{cc%4d{cw]\r\n",
	    GET_KILL_CNT ( ch ), GET_RIP_CNT ( ch ), GET_DT_CNT ( ch ) );
	if ( *webp || *webs )
		ch->Send ( "{cC%-30s {cC%-30s{cw\r\n", webp,  webs );
	if ( ( staff || wep_num > 1 ) && GET_EQ ( ch, WEAR_SHIELD ) && *shld )
		ch->Send ( "              {cC%30s{cg                   {cw\r\n", shld );
	if ( GET_EQ ( ch, WEAR_WIELD ) &&
	        ( GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_WIELD ) ) == ITEM_SHOVEL ||
	          GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_WIELD ) ) == ITEM_PICKAXE ) )
		ch->Send (
		    "{cC[Mining] {cwSpeed: {cc%3d {cwBonus: {cc%3d {cwProtection: {cc%3d {cwStealth: {cc%3d\r\n",
		    MINE_SPEED ( ch ), MINE_BONUS ( ch ), MINE_DAMAGE ( ch ), MINE_STEALTH ( ch ) );

	ch->Send ( "{cwSTR: {cy%2d{cw/{cy%-3d {cwINT: {cy%-2d {cwWIS: {cy%-2d {cwCON: {cy%-2d {cwDEX: {cy%-2d {cwCHA: {cy%-2d\r\n", 
	    GET_STR ( ch ), GET_ADD ( ch ), GET_INT ( ch ), GET_WIS ( ch ),
	    GET_CON ( ch ),GET_DEX ( ch ), GET_CHA ( ch ) );

	ch->Send ( "{cwHunger: [{cy%3d%%{cw] Thirst: [{cy%3d%%{cw] Intoxication: [{cy%3d%%{cw]\r\n",
	    GET_COND ( ch,
	               FULL ) ==
	    -1 ? 0 : 100 - ( ( GET_COND ( ch, FULL ) * 100 ) / 48 ),
	    GET_COND ( ch,
	               THIRST ) ==
	    -1 ? 0 : 100 - ( ( GET_COND ( ch, THIRST ) * 100 ) / 48 ),
	    GET_COND ( ch,
	               DRUNK ) ==
	    -1 ? 0 : ( ( GET_COND ( ch, DRUNK ) * 100 ) / 48 ) );

	if ( GET_LEVEL ( ch ) < LVL_HERO )
	{
		char exphave[50], expneed[50];
		commafmt ( exphave, sizeof ( exphave ),GET_EXP ( ch ) );
		commafmt ( expneed, sizeof ( expneed ),exp_needed ( ch ) );
		ch->Send (
		    "{cwExp. Total: {cy%-15s {cgNeeded To Level: {cC%-15s\r\n",
		    exphave, exp_needed ( ch ) > 0 ? expneed : "No More" );
		if ( GET_LEVEL ( ch ) >= 60 )
		{
			commafmt ( exphave, sizeof ( exphave ),GET_GROUP_EXP ( ch ) );
			commafmt ( expneed, sizeof ( expneed ),group_exp_needed ( ch ) );
			ch->Send ( "{cwGroup Pts: {cy%-15s {cgNeeded To Level: {cC%-15s\r\n",
			           exphave, group_exp_needed ( ch ) > 0 ? expneed : "No More" );
		}

	}



	playing_time = *real_time_passed ( ( time ( 0 ) - ch->player.time.logon ) +
	                                   ch->player.time.played, 0 );
	ch->Send (
	    "{cwAge: {cy%-3d {cwYou have been playing for [{cy%3d{cw] day%s and [{cy%2d{cw] hour%-s\r\n",
	    GET_AGE ( ch ),
	    playing_time.day, playing_time.day == 1 ? "" : "s",
	    playing_time.hours,
	    playing_time.hours == 1 ? " " : "s" );
	}
	else // PRF_NOGRAPHICS == OFF
	{
	/* yikes, nasty hack job here testing for a player name but hey... -mord */
	if ( !str_cmp ( "thotter", GET_NAME ( ch ) ) )
		ch->Send (
		    "\r\n{cg(*)---[{cC{cuKing Of Pie{cg]---------------------Slot-Class---TierName---Tier--(*){c0\r\n" );
	else
		ch->Send (
		    "\r\n{cg(*)-------------------------------------Slot-Class---TierName---Tier--(*){c0\r\n" );
	ch->Send ( "{cg| |{cw Name:{cc %-16s{cw Level:{cy %2d{cw    %-29s {cg| |\r\n",
	           GET_NAME ( ch ),GET_LEVEL ( ch ), primary_class ( ch, blank ) );
	ch->Send (
	    "{cg| |{cw Race:{cc %-9s{cw        {cwAlign: {cc%-5d{cw %-29s {cg| |\r\n",
	    race_name ( ch ),  GET_ALIGNMENT ( ch ), secondary_class ( ch, blank ) );

	ch->Send (
	    "{cg| |  {cwSex:{cy %-7s{cw      Practices: {cc%-6d{cw%-29s {cg| |\r\n",
	    ( GET_SEX ( ch ) == SEX_MALE ? "Male" : ( GET_SEX ( ch ) ? "Female" :  "Neutral" ) ),
	    GET_PRACTICES ( ch ), tertary_class ( ch, blank ) );

	ch->Send (
	    "{cg| | {cwClan: {cy%-17s {cwRank: {cc%-2d{cw    %-29s {cg| |\r\n",
	    ( ( !GET_CLAN ( ch ) ) ? "<none>" : clan[find_clan_by_id ( GET_CLAN ( ch ) ) ].name ),
	    GET_CLAN_RANK ( ch ), quatry_class ( ch, blank ) );

	ch->Send (
	    "{cg| |-------------------------------------------------------------------| |{cw\r\n" );

	ch->Send (
	    "{cg| | {cwHit  Points: [{cc%6d{cw][{cc%6d{cw]     Speed: [{cc%6d{cw]      AC: [{cc%4d{cw] {cg| |\r\n"
	    "{cg| | {cwMana Points: [{cc%6d{cw][{cc%6d{cw]   Hitroll: [{cc%4d{cw] Dam-Bonus: [{cc%4d{cw] {cg| |\r\n"
	    "{cg| | {cwMove Points: [{cc%6d{cw][{cc%6d{cw]  Accuracy: [{cy%4d{cw]   Evasion: [{cy%4d{cw] {cg| |{c0\r\n",
	    GET_HIT ( ch ), GET_MAX_HIT ( ch ),speed_update ( ch ),ch->compute_armor_class(),
	    GET_MANA ( ch ), GET_MAX_MANA ( ch ), GET_HITROLL ( ch ),   is_casting ? caster_damroll ( ch ) : fighter_damroll( ch ),
	    GET_MOVE ( ch ), GET_MAX_MOVE ( ch ), accuracy_tot ( ch ), evasion_tot ( ch ) );

	ch->Send (
	    "{cg| |-------------------------------------------------------------------| |{cw\r\n" );
	if ( IS_PK ( ch ) )
		ch->Send ( "{cg| | {cw   PKwin: [{cc%6d{cw]     PKloss: [{cc%6d{cw]     PK-Points:[{cc%6d{cw]    {cg| |\r\n",
		           GET_PK_CNT ( ch ), GET_PK_RIP ( ch ), GET_PK_POINTS ( ch ) );

	ch->Send (
	    "{cg| | {cw   Kills: [{cc%6d{cw]     Deaths: [{cc%6d{cw]     Deathtraps: [{cc%4d{cw]    {cg| |\r\n",
	    GET_KILL_CNT ( ch ), GET_RIP_CNT ( ch ), GET_DT_CNT ( ch ) );
	if ( *webp || *webs )
		ch->Send ( "{cg| |->{cC%-30s{cg<|>{cC%-30s{cg<-| |{cw\r\n", webp,  webs );
	if ( ( staff || wep_num > 1 ) && GET_EQ ( ch, WEAR_SHIELD ) && *shld )
		ch->Send ( "{cg| |->              {cC%30s{cg                   <-| |{cw\r\n", shld );
	if ( GET_EQ ( ch, WEAR_WIELD ) &&
	        ( GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_WIELD ) ) == ITEM_SHOVEL ||
	          GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_WIELD ) ) == ITEM_PICKAXE ) )
		ch->Send (
		    "{cg| |{cC[Mining] {cwSpeed: {cc%3d{cw   Bonus: {cc%3d{cw   Protection: {cc%3d   {cwStealth: {cc%3d{cg  | |\r\n",
		    MINE_SPEED ( ch ), MINE_BONUS ( ch ), MINE_DAMAGE ( ch ), MINE_STEALTH ( ch ) );

	ch->Send (
	    "{cg| |-------------------------------------------------------------------| |\r\n"
	    "| |   {cwSTR: {cy%2d{cw/{cy%-3d{cw   INT: {cy%-2d{cw   WIS: {cy%-2d{cw   CON: {cy%-2d{cw   DEX: {cy%-2d{cw   CHA: {cy%-2d{cg   | |\r\n",
	    GET_STR ( ch ), GET_ADD ( ch ), GET_INT ( ch ), GET_WIS ( ch ),
	    GET_CON ( ch ),GET_DEX ( ch ), GET_CHA ( ch ) );

	ch->Send (
	    "{cg| |-------------------------------------------------------------------| |\r\n"
	    "{cg| |     Hunger: [{cy%3d%%{cg]      Thirst:[{cy%3d%%{cg]      Intoxication:[{cy%3d%%{cg]    | |\r\n",
	    GET_COND ( ch,
	               FULL ) ==
	    -1 ? 0 : 100 - ( ( GET_COND ( ch, FULL ) * 100 ) / 48 ),
	    GET_COND ( ch,
	               THIRST ) ==
	    -1 ? 0 : 100 - ( ( GET_COND ( ch, THIRST ) * 100 ) / 48 ),
	    GET_COND ( ch,
	               DRUNK ) ==
	    -1 ? 0 : ( ( GET_COND ( ch, DRUNK ) * 100 ) / 48 ) );

	if ( GET_LEVEL ( ch ) < LVL_HERO )
	{
		char exphave[50], expneed[50];
		commafmt ( exphave, sizeof ( exphave ),GET_EXP ( ch ) );
		commafmt ( expneed, sizeof ( expneed ),exp_needed ( ch ) );
		ch->Send (
		    "{cg| |-------------------------------------------------------------------| |\r\n"
		    "{cg| |   {cwExp. Total: {cy%-15s{cg  Needed To Level: {cC%-15s{cg   | |\r\n",
		    exphave, exp_needed ( ch ) > 0 ? expneed : "No More" );
		if ( GET_LEVEL ( ch ) >= 60 )
		{
			commafmt ( exphave, sizeof ( exphave ),GET_GROUP_EXP ( ch ) );
			commafmt ( expneed, sizeof ( expneed ),group_exp_needed ( ch ) );
			ch->Send ( "{cg| |    {cwGroup Pts: {cy%-15s{cg  Needed To Level: {cC%-15s{cg   | |\r\n",
			           exphave, group_exp_needed ( ch ) > 0 ? expneed : "No More" );
		}

	}



	playing_time = *real_time_passed ( ( time ( 0 ) - ch->player.time.logon ) +
	                                   ch->player.time.played, 0 );
	ch->Send (
	    "{cg| |-------------------------------------------------------------------| |\r\n"
	    "| | {cwAge: {cy%-3d{cg  --  {cwYou have been playing for [{cy%3d{cw] day%s and [{cy%2d{cw] hour%s%s{cg | |\r\n",
	    GET_AGE ( ch ),
	    playing_time.day, playing_time.day == 1 ? "" : "s",
	    playing_time.hours,
	    playing_time.hours == 1 ? " " : "s",
	    playing_time.day == 1 ? " " : "" );
	ch->Send (
	    "{cg-------------------------------------------------------------------------{c0\r\n" );
	}

	if ( GET_RP_GROUP ( ch ) )
		ch->Send ( "Your RP group is: %s\r\n", rp_group_names[GET_RP_GROUP ( ch ) ] );
	if ( GET_LEVEL ( ch ) >= LVL_HERO )
		ch->Send ( "Pretitle: %s\r\n", ( PRETITLE ( ch ) != NULL ? PRETITLE ( ch ) : "none" ) );
	if ( GET_LEVEL ( ch ) >= LVL_IMMORT )
	{

		if ( POOFIN ( ch ) )
			ch->Send ( "Poofin :%s\r\n", POOFIN ( ch ) );
		if ( POOFOUT ( ch ) )
			ch->Send ( "Poofout:%s\r\n", POOFOUT ( ch ) );
		if ( GET_LOADROOM ( ch ) != -1 )
			ch->Send ( "Loadroom: %s [vnum: %d]\r\n",
			           real_room ( GET_LOADROOM ( ch ) )->name,
			           GET_LOADROOM ( ch ) );
	}
	if ( GET_LOGINMSG ( ch ) )
		ch->Send ( "Login Message: %s\r\n", GET_LOGINMSG ( ch ) );             /*THOTTER EDIT*/
	if ( GET_LOGOUTMSG ( ch ) )
		ch->Send ( "Logout Message: %s\r\n", GET_LOGOUTMSG ( ch ) );         /*THOTTER EDIT*/
	/** Display info about your scooter **/
	if ( has_vehicle ( ch ) )
	{
		struct obj_data *v = has_vehicle ( ch );
		ch->Send ( "Vehicle Fuel: [%d]\r\n", GET_FUEL ( v ) > 0 ? GET_FUEL_PERCENTAGE ( v ) : 0 );
	}
	if ( ch->pet != -1 )
		*ch << "Pet: " << mob_name_by_vnum ( ch->pet ) << "\r\n";

	/** End scooter info **/
	if ( PRF_FLAGGED ( ch, PRF_RP ) )
	{
		ch->Send ( "You are RolePlaying!\r\n" );
	}
	if ( PRF_FLAGGED ( ch, PRF_BUSY ) )
	{
		if ( ch->player_specials->busy_msg )
			ch->Send ( "You are BUSY: %s\r\n", ch->player_specials->busy_msg );
		else
			ch->Send ( "You are BUSY!\r\n" );
	}
	if ( PRF_FLAGGED ( ch, PRF_AFK ) )
	{
		if ( ch->player_specials->afk_msg )
			ch->Send ( "You are AFK: %s\r\n", ch->player_specials->afk_msg );
		else
			ch->Send ( "You are AFK!\r\n" );
	}
	switch ( GET_POS ( ch ) )
	{
		case POS_DEAD:
			ch->Send ( "You are DEAD!\r\n" );
			break;
		case POS_MORTALLYW:
			ch->Send (
			    "You are mortally wounded!  You should seek help!\r\n" );
			break;
		case POS_INCAP:
			ch->Send (
			    "You are incapacitated, slowly fading away...\r\n" );
			break;
		case POS_STUNNED:
			ch->Send ( "You are stunned!  You can't move!\r\n" );
			break;
		case POS_SLEEPING:
			ch->Send ( "You are sleeping.\r\n" );
			break;
		case POS_RESTING:
			ch->Send ( "You are resting.\r\n" );
			break;
		case POS_SITTING:
			if ( !SITTING ( ch ) )
				ch->Send ( "You are sitting.\r\n" );
			else
			{
				struct obj_data *chair = SITTING ( ch );
				ch->Send ( "You are sitting upon %s.\r\n",
				           chair->short_description );
			}
			break;
		case POS_FIGHTING:
			if ( FIGHTING ( ch ) )
				ch->Send ( "You are fighting %s.\r\n",
				           PERS ( FIGHTING ( ch ), ch ) );
			else
				ch->Send ( "You are fighting thin air.\r\n" );
			break;
		case POS_STANDING:
			ch->Send ( "You are standing.\r\n" );
			break;
		default:
			ch->Send ( "You are floating.\r\n" );
			break;
	}



	if ( AFF_FLAGGED ( ch, AFF_BLIND ) )
		ch->Send ( "You have been blinded!\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_INVISIBLE ) )
		ch->Send ( "You are invisible.\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_DETECT_INVIS ) )
		ch->Send (
		    "You are sensitive to the presence of invisible things.\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_SANCTUARY ) )
		ch->Send ( "You are protected by Sanctuary.\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_POISON_1 ) || AFF_FLAGGED ( ch, AFF_POISON_2 ) ||
	        AFF_FLAGGED ( ch, AFF_POISON_3 ) || AFF_FLAGGED ( ch, AFF_POISON_4 ) )
		ch->Send ( "You are poisoned!\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_CHARM ) )
		ch->Send ( "You have been charmed!\r\n" );

	if ( affected_by_spell ( ch, SPELL_ARMOR ) )
		ch->Send ( "You feel protected.\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_INFRAVISION ) )
		ch->Send ( "Your eyes are glowing {cRred{cx.\r\n" );

	if ( PRF_FLAGGED ( ch, PRF_SUMMONABLE ) )
		ch->Send ( "You are summonable by other players.\r\n" );
	if ( PRF_FLAGGED ( ch, PRF_GATEABLE ) )
		ch->Send ( "You are gateable by other players.\r\n" );
	if ( PRF_FLAGGED ( ch, PRF_TELEPORTABLE ) )
		ch->Send ( "You are teleportable.\r\n" );
	if ( AFF_FLAGGED ( ch, AFF_PROT_FIRE ) )
		ch->Send ( "You are protected against fire.\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_PROT_COLD ) )
		ch->Send ( "You are protected against freezing.\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_BURNING ) && !AFF_FLAGGED ( ch, AFF_PROT_FIRE ) )
		ch->Send ( "You are {cRburning{cx!\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_FREEZING ) && !AFF_FLAGGED ( ch, AFF_PROT_COLD ) )
		ch->Send ( "You are {cCfreezing{cx!\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_ACIDED ) )
		ch->Send ( "You are acided!\r\n" );

	if ( AFF_FLAGGED ( ch, AFF_FIRE_SHIELD ) )
		ch->Send ( "You are protected by a fire shield!\r\n" );
	if ( ch->Flying() )
		ch->Send ( "You are flying!\r\n" );

	/* Romance Module Additions */
	switch ( ROMANCE ( ch ) )
	{
		case 0:
			ch->Send ( "You are single.\r\n" );
			break;
		case 1:
			ch->Send ( "You are dating %s.\r\n", pi.NameById ( PARTNER ( ch ) ) );
			break;
		case 2:
			ch->Send ( "You are engaged to %s.\r\n", pi.NameById ( PARTNER ( ch ) ) );
			break;
		case 3:
			ch->Send ( "You are married to %s.\r\n", pi.NameById ( PARTNER ( ch ) ) );
			break;
		case 4:
			ch->Send ( "You are asking %s out.\r\n", pi.NameById ( PARTNER ( ch ) ) );
			break;
		case 5:
			ch->Send ( "You are proposing to %s.\r\n", pi.NameById ( PARTNER ( ch ) ) );
			break;
	}
	/* MatingMod Additions */
	if ( PREG ( ch ) > NOT_PREG )
	{
		if ( GET_LEVEL ( ch ) >= LVL_HERO )  /* THIS ELSE-IF OPTIONAL */
		{
			ch->Send (
			    "You are %d hours away from giving birth.\r\n",
			    PREG ( ch ) );
		}
		else
		{
			ch->Send ( "You are " );
			if ( PREG ( ch ) < 7 )
			{
				ch->Send ( "giving birth!\r\n" );
			}
			else if ( PREG ( ch ) < MONTHS_2 )
			{
				ch->Send ( "9 months pregnant.\r\n" );
			}
			else if ( PREG ( ch ) < MONTHS_3 )
			{
				ch->Send ( "8 months pregnant.\r\n" );
			}
			else if ( PREG ( ch ) < MONTHS_4 )
			{
				ch->Send ( "7 months pregnant.\r\n" );
			}
			else if ( PREG ( ch ) < MONTHS_5 )
			{
				ch->Send ( "6 months pregnant.\r\n" );
			}
			else if ( PREG ( ch ) < MONTHS_6 )
			{
				ch->Send ( "5 months pregnant.\r\n" );
			}
			else if ( PREG ( ch ) < MONTHS_7 )
			{
				ch->Send ( "4 months pregnant.\r\n" );
			}
			else if ( PREG ( ch ) < MONTHS_8 )
			{
				ch->Send ( "3 months pregnant.\r\n" );
			}
			else if ( PREG ( ch ) < NINE_MONTHS )
			{
				ch->Send ( "2 months pregnant.\r\n" );
			}
			else
			{
				ch->Send ( "1 month pregnant.\r\n" );
			}
		}
	}
	/* End Romance Module Modifications */


}

ACMD ( do_inventory )
{
	ch->Send (
	    "\r\n{cy[You are carrying {cC%d{cy of {cc%d {cyitem%s] \r\n"
	    "[You have a total weight of {cC%d{cy/{cc%d{cy]{c0\r\n",
	    IS_CARRYING_N ( ch ), CAN_CARRY_N ( ch ),
	    ( ( CAN_CARRY_N ( ch ) > 1 ) ? "s" : "" ),
	    IS_CARRYING_W ( ch ), CAN_CARRY_W ( ch ) );

	list_obj_to_char ( ch->carrying, ch, 1, TRUE );
}

void list_damage ( Character *ch )
{
	//TODO: list damage of all the players on
}


ACMD ( do_equipment )
{
	int i, found = 0, all = FALSE;
	if ( AFF_FLAGGED ( ch, AFF_POLY_TOAD ) )
	{
		ch->Send ( "You look great in warty green brown skin!\r\n" );
		return;
	}
	if ( AFF_FLAGGED ( ch, AFF_POLY_WOLF ) )
	{
		ch->Send ( "You look great in shaggy fur!\r\n" );
		return;
	}
	if ( AFF_FLAGGED ( ch, AFF_POLY_BOAR ) )
	{
		ch->Send ( "You look great covered in straight short hairs!\r\n" );
		return;
	}
	if ( AFF_FLAGGED ( ch, AFF_POLY_BEAR ) )
	{
		ch->Send ( "You look great covered in a heavy shaggy fur!\r\n" );
		return;
	}
	if ( AFF_FLAGGED ( ch, AFF_POLY_LION ) )
	{
		ch->Send ( "You look great in huge claws and teeth. Woo!\r\n" );
		return;
	}
	if ( isname ( "all", argument ) )
		all = TRUE;
	ch->Send ( "You are using:\r\n" );
	for ( i = 0; i < NUM_WEARS; i++ )
	{
		if ( !HAS_BODY ( ch, wear_order_index[i] ) )
			continue;
		if ( all )
		{
			if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
				ch->Send ( "\x1B[33m" );
			ch->Send ( "%s ", disp_where ( wear_order_index[i], ch ) );
			if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
				ch->Send ( "\x1B[0m" );
			found = TRUE;
		}
		if ( GET_EQ ( ch, wear_order_index[i] ) )
		{

			if ( CAN_SEE_OBJ ( ch, GET_EQ ( ch, wear_order_index[i] ) ) )
			{
				if ( !all )
				{
					if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
						ch->Send ( "\x1B[33m" );
					ch->Send ( "%s ", disp_where ( wear_order_index[i], ch ) );
					if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
						ch->Send ( "\x1B[0m" );
				}
				show_obj_to_char ( GET_EQ ( ch, wear_order_index[i] ), ch, 1 );
				found = TRUE;
			}
			else
			{
				ch->Send ( "%s Something.\r\n",
				           all ? "" : disp_where ( wear_order_index[i], ch ) );
				found = TRUE;
			}
		}
		else if ( all )
		{
			ch->Send ( " Empty. \r\n" );
		}
		/*else if (PRF_FLAGGED(ch, PRF_BATTLESPAM)) {
		   ch->Send( "%s Nothing.\r\n", where[wear_order_index[i]]);
		   } */
	}
	if ( !found )
	{
		ch->Send ( " Nothing.\r\n" );
	}
}

ACMD ( do_settime )
{
	int time;
	char buf[MAX_INPUT_LENGTH];

	one_argument ( argument, buf );

	time = abs ( atoi ( argument ) );
	time_info.hours = time;
	ch->Send ( "Time has been set.\r\n" );
	check_time_triggers();
	return;
}

ACMD ( do_time )
{
	const char *suf;
	int weekday, day;

	/* day in [1..35] */
	day = time_info.day + 1;

	/* 35 days in a month, 7 days a week */
	weekday = ( ( 35 * time_info.month ) + day ) % 7;

	ch->Send ( "It is %d o'clock %s, on %s\r\n",
	           ( ( time_info.hours % 12 ==
	               0 ) ? 12 : ( ( time_info.hours ) % 12 ) ),
	           ( ( time_info.hours >= 12 ) ? "pm" : "am" ),
	           weekdays[weekday] );


	/*
	 * Peter Ajamian <peter@PAJAMIAN.DHS.ORG> supplied the following as a fix
	 * for a bug introduced in the ordinal display that caused 11, 12, and 13
	 * to be incorrectly displayed as 11st, 12nd, and 13rd.  Nate Winters
	 * <wintersn@HOTMAIL.COM> had already submitted a fix, but it hard-coded a
	 * limit on ordinal display which I want to avoid.    -dak
	 */

	suf = "th";

	if ( ( ( day % 100 ) / 10 ) != 1 )
	{
		switch ( day % 10 )
		{
			case 1:
				suf = "st";
				break;
			case 2:
				suf = "nd";
				break;
			case 3:
				suf = "rd";
				break;
		}
	}

	ch->Send ( "The %d%s Day of the %s, Year %d.\r\n",
	           day, suf, month_name[time_info.month],
	           time_info.year );
	// Added this to see if moon phase will display - Prom
	ch->Send ( "The moon is %s. \r\n", moon_types[time_info.moon]);
	// Added this for season display - Prom
	ch->Send ( "The season is %s. \r\n", season_types[time_info.season]);
}


ACMD ( do_weather )
{
	static const char *sky_look[] =
	{
		"cloudless",
		"cloudy",
		"rainy",
		"lit by flashes of lightning"
	};

	if ( OUTSIDE ( ch ) )
	{
		ch->Send ( "The sky is %s and %s.\r\n",
		           sky_look[zone_table[IN_ROOM ( ch )->zone].sky],
		           ( zone_table[IN_ROOM ( ch )->zone].change >= 0 ?
		             "you feel a warm wind from the south" :
		             "your foot tells you bad weather is due" ) );
	}
	else
		ch->Send ( "You have no feeling about the weather at all.\r\n" );

	if ( GET_LEVEL ( ch ) >= LVL_GOD )
	{
		ch->Send ( "Pressure: {cC%d{cx, Change: {cC%d{cx\r\n",
		           zone_table[GET_ROOM_ZONE ( IN_ROOM ( ch ) ) ].pressure,
		           zone_table[GET_ROOM_ZONE ( IN_ROOM ( ch ) ) ].change );
	}
}

#if 1
struct help_index_element *find_help ( char *keyword )
{
	unsigned int i;

	for ( i = 0; i <= top_of_helpt; i++ )
		if ( is_name ( keyword, help_table[i].keywords ) )
			return ( help_table + i );
	for ( i = 0; i <= top_of_helpt; i++ )
		if ( isname_full ( keyword, help_table[i].keywords ) )
			return ( help_table + i );

	return NULL;
}
#endif

void display_help ( Character *ch, unsigned int i )
{
	if ( ch )
	{
		DYN_DEFINE;
		DYN_CREATE;
		*dynbuf = '\0';
		DYN_RESIZE ( "----------------------------------------------------------------------------\r\n" );
		DYN_RESIZE ( help_table[i].keywords );
		DYN_RESIZE ( "\r\n" );
		DYN_RESIZE ( "----------------------------------------------------------------------------\r\n" );
		DYN_RESIZE ( help_table[i].entry );
		page_string ( ch->desc, dynbuf, DYN_BUFFER );
		/*
		ch->Send(
		                   "----------------------------------------------------------------------------\r\n"
		                   "%s\r\n"
		                   "----------------------------------------------------------------------------\r\n"
		                   "%s\r\n",
		                   help_table[i].keywords, help_table[i].entry);
		*/
	}
}

void display_help_list ( Descriptor *d, char *keywords )
{
	unsigned int i, cnt = 0;
	char buf[MAX_STRING_LENGTH];
	char buf1[1024];
	//size_t len = 0;
	strcpy ( buf, "  Help files similar\r\n---------------------------------------\r\n" );
	//len = strlen ( buf );

	for ( i=0;i<top_of_helpt;i++ )
	{
		if ( is_number ( keywords ) && i == ( unsigned int ) atoi ( keywords ) )
		{
			snprintf ( buf1, sizeof ( buf1 ), "{cW%-5d{cg){cy %s{c0\r\n", i, help_table[i].keywords );
			strlcat ( buf, buf1, sizeof ( buf ) );
			cnt++;
		}
		else if ( isname_full ( keywords, help_table[i].keywords ) )
		{
			snprintf ( buf1, sizeof ( buf1 ), "{cW%-5d{cg){cy %s{c0\r\n", i, help_table[i].keywords );
			strlcat ( buf, buf1, sizeof ( buf ) );
			cnt++;
		}
	}

	if ( cnt == 1 )
	{
		for ( i=0;i<top_of_helpt;i++ )
		{
			if ( is_number ( keywords ) && i == ( unsigned int ) atoi ( keywords ) )
			{
				display_help ( d->character, i );
				return;
			}
			else if ( isname_full ( keywords, help_table[i].keywords ) )
			{
				display_help ( d->character, i );
				return;
			}
		}
	}
	if ( cnt == 0 )
	{
		d->Output ( "No help found on that topic, this has been logged please check for this help file in the future.\r\n" );
		log ( "HELP: %s tried to find help on the word %s, but nothing available.", GET_NAME ( d->character ), keywords );
		return;
	}
	snprintf ( buf1, sizeof ( buf1 ),
	           "\r\n\r\nPlease type the number of the help file to view it\r\nOr type anything else to cancel\r\nWhich Help file: " );
	strlcat ( buf, buf1, sizeof ( buf ) );
	page_string ( d, buf, 0 );
	ORIG_STATE ( d ) = STATE ( d );
	STATE ( d ) = CON_FIND_HELP;


}

ACMD ( do_help )
{

	int chk, bot, top, mid, minlen;

	if ( !ch->desc )
		return;

	skip_spaces ( &argument );

	if ( !*argument )
	{
		page_string ( ch->desc, help, 0 );
		return;
	}
	if ( !help_table )
	{
		ch->Send ( "No help available.\r\n" );
		return;
	}
	display_help_list ( ch->desc, argument );
	return;

	bot = 0;
	top = top_of_helpt;
	minlen = strlen ( argument );

	for ( ;; )
	{
		mid = ( bot + top ) / 2;

		if ( bot > top )
		{
			//TODO: add in random examples
			ch->Send ( "There is no help on that (yet).\r\nUse QUESTION to ask the other players.\r\neg: question what is magic missile?\r\n" );
			log ( "HELP: %s tried to find help on the word %s, but nothing available.", GET_NAME ( ch ), argument );
			return;
		}
		else
			if ( ! ( chk = strn_cmp ( argument, help_table[mid].keywords, minlen ) ) )
			{
				/* trace backwards to find first matching entry. Thanks Jeff Fink! */
				while ( ( mid > 0 ) && ( ! ( chk =strn_cmp ( argument, help_table[mid - 1].keywords,minlen ) ) ) )
					mid--;

				//strip_string(help_table[mid].entry);
				page_string ( ch->desc, help_table[mid].entry, 0 );
				return;
			}
			else
			{
				if ( chk > 0 )
					bot = mid + 1;
				else
					top = mid - 1;
			}
	}
}

ACMD ( do_spellinfo )
{
	int i;
	skip_spaces ( &argument );
	if ( !*argument || argument == NULL )
	{
		ch->Send ( "Which skill or spell?\r\n" );
		return;
	}
	if ( ( i = find_skill_num ( argument ) ) == TYPE_UNDEFINED )
		ch->Send ( "Unrecognised spell or skill.\r\n" );
	else
		skill_spell_help ( ch, i );

}

void skill_spell_help ( Character *ch, int spell )
{
	int i;
	extern const char *pc_class_types[];
	if ( spell == TYPE_UNDEFINED )
		return;

	ch->Send ( "{cg----------------------------------------------------\r\n" );
	ch->Send ( "                Name: {cy%s{cg\r\n", skill_name ( spell ) );
	if ( spell_info[spell].first_prereq != TYPE_UNDEFINED )
		ch->Send ( "First Pre Requisite : {cy%s{cg\r\n", skill_name ( spell_info[spell].first_prereq ) );
	if ( spell_info[spell].second_prereq != TYPE_UNDEFINED )
		ch->Send ( "Second Pre Requisite: {cy%s{cg\r\n", skill_name ( spell_info[spell].second_prereq ) );
	ch->Send ( "Tier: {cc%d{cg     Level: {cc%2d{cg    \r\n"
	           "MinPosition: {cy%s{cg   Elemental Type: {cy%s{cg\r\n\r\n",
	           spell_info[spell].tier, spell_info[spell].min_level, position_types[ ( int ) spell_info[spell].min_position],
	           elemental_types[elemental_type ( spell ) ] );

	ch->Send (
	    "Directional: {cy%-3s{cg            Offensive: {cy%-3s{cg\r\n"
	    "    Affects: {cy%-3s{cg            Unaffects: {cy%-3s{cg\r\n"
	    "     Points: {cy%-3s{cg       Alters Objects: {cy%-3s{cg\r\n"
	    "     Groups: {cy%-3s{cg               Masses: {cy%-3s{cg\r\n"
	    "      Areas: {cy%-3s{cg            Summoning: {cy%-3s{cg\r\n"
	    "   Creation: {cy%-3s{cg\r\n\r\n",
	    YESNO ( IS_SET ( spell_info[spell].targets, TAR_AREA_DIR ) ),
	    YESNO ( spell_info[spell].violent ),
	    YESNO ( IS_SET ( spell_info[spell].routines, MAG_AFFECTS ) ),
	    YESNO ( IS_SET ( spell_info[spell].routines, MAG_UNAFFECTS ) ),
	    YESNO ( IS_SET ( spell_info[spell].routines, MAG_POINTS ) ),
	    YESNO ( IS_SET ( spell_info[spell].routines, MAG_ALTER_OBJS ) ),
	    YESNO ( IS_SET ( spell_info[spell].routines, MAG_GROUPS ) ),
	    YESNO ( IS_SET ( spell_info[spell].routines, MAG_MASSES ) ),
	    YESNO ( IS_SET ( spell_info[spell].routines, MAG_AREAS ) ),
	    YESNO ( IS_SET ( spell_info[spell].routines, MAG_SUMMONS ) ),
	    YESNO ( IS_SET ( spell_info[spell].routines, MAG_CREATIONS ) )
	);
	if ( IS_SET ( spell_info[spell].targets, TAR_SELF_ONLY ) )
		ch->Send ( "This spell can only be cast upon yourself.\r\n" );
	if ( IS_SET ( spell_info[spell].targets, TAR_NOT_SELF ) )
		ch->Send ( "This spell can not be cast on yourself.\r\n" );
	ch->Send ( "Classes: {cy" );
	for ( i = 0; i < NUM_CLASSES ; i++ )
		if ( IS_SET ( spell_info[spell].classes, ( 1 << i ) ) )
			ch->Send ( "%s  ", pc_class_types[i] );
	ch->Send ( "{cg\r\n" );



	if ( knows_spell ( ch, spell ) )
		ch->Send ( "{cCYou know this spell{cg\r\n" );
	ch->Send ( "----------------------------------------------------{c0\r\n" );
}

#if 0
ACMD ( do_help_old )
{
	struct help_index_element *this_help;
	char entry[MAX_STRING_LENGTH];

	if ( !ch->desc )
		return;

	skip_spaces ( &argument );

	if ( !*argument )
	{
		page_string ( ch->desc, help, 0 );
		return;
	}
	if ( !help_table )
	{
		send_to_char ( "No help available.\r\n", ch );
		return;
	}

	if ( ! ( this_help = find_help ( argument ) ) )
	{
		send_to_char ( "There is no help on that word.\r\n", ch );
		log ( "HELP: %s tried to get help on %s", GET_NAME ( ch ),
		      argument );
		return;
	}

	if ( this_help->min_level > GET_LEVEL ( ch ) )
	{
		send_to_char ( "There is no help on that word.\r\n", ch );
		return;
	}

	snprintf ( entry, sizeof ( entry ), "%s\r\n%s", this_help->keywords, this_help->entry );
	//strip_string(entry);

	page_string ( ch->desc, entry, 0 );

}
#endif
int who_sort_helper_level ( const void *a, const void *b )
{
	Character *a1, *b1;

	if ( !a || !b )
		return 0;

	a1 = * ( Character ** ) a;
	b1 = * ( Character ** ) b;

	if ( a1 == NULL && b1 == NULL )
		return 0;
	else if ( a1 == NULL )
		return -1;
	else if ( b1 == NULL )
		return 1;
	else
	{
		if ( GET_LEVEL ( b1 ) >= LVL_IMMORT && GET_LEVEL ( a1 ) >= LVL_IMMORT )
			return GET_LEVEL ( b1 ) - GET_LEVEL ( a1 );
		else if ( GET_LEVEL ( a1 ) >= LVL_IMMORT )
			return -1;
		else if ( GET_LEVEL ( b1 ) >= LVL_IMMORT )
			return -1;
		int ret_val = 0;
		ret_val = ( current_class_is_tier_num ( b1 ) - current_class_is_tier_num ( a1 ) );

		if ( ret_val ==0 )
			ret_val = ( GET_LEVEL ( b1 ) - GET_LEVEL ( a1 ) );

		return ret_val;
	}

}
int who_sort_helper_class ( const void *a, const void *b )
{
	Character *a1, *b1;

	if ( !a || !b )
		return 0;

	a1 = * ( Character ** ) a;
	b1 = * ( Character ** ) b;

	if ( a1 == NULL && b1 == NULL )
		return 0;
	else if ( a1 == NULL )
		return -1;
	else if ( b1 == NULL )
		return 1;
	else
		return GET_CLASS ( b1 ) - GET_CLASS ( a1 );


}
int who_sort_helper_race ( const void *a, const void *b )
{
	Character *a1, *b1;

	if ( !a || !b )
		return 0;

	a1 = * ( Character ** ) a;
	b1 = * ( Character ** ) b;

	if ( a1 == NULL && b1 == NULL )
		return 0;
	else if ( a1 == NULL )
		return -1;
	else if ( b1 == NULL )
		return 1;
	else
		return GET_RACE ( b1 ) - GET_RACE ( a1 );


}

/*********************************************************************
* New 'do_who' by Daniel Koepke [aka., "Argyle Macleod"] of The Keep *
******************************************************************* */
string WHO_USAGE (
    "Usage: who [minlev[-maxlev]] [-n name] [-c classes] [-d races] [-rzqimo]\r\n"
    "\r\n"
    "Classes: (M)age,   (P)riest, (T)hief, (W)arrior,\r\n"
    "         (H)unter, (R)anger, (G)yspy, (E)sper,\r\n"
    "\r\n"
    " Switches: \r\n"
    "_.,-'^'-,._\r\n"
    "\r\n"
    "  -r = who is in the current room\r\n"
    "  -z = who is in the current zone\r\n"
    "\r\n"
    "  -q = only show questers\r\n"
    "  -i = only show immortals\r\n"
    "  -m = only show mortals\r\n"
    "  -o = show in order of login\r\n"
    "  -p = show in order of class\r\n"
    "  -j = show in order of race\r\n"
    "  -b = show just player names in human readable format. Suitable for screen readers.\r\n"
    "\r\n" );

ACMD ( do_who )
{
	Descriptor *d;
	Character *wch;
	char name_search[MAX_INPUT_LENGTH + 1];
	char mode;
	char *clan_name ( int idnum );
	char buf[MAX_STRING_LENGTH];
	char arg[MAX_STRING_LENGTH];
	char buf1[MAX_STRING_LENGTH];
	std::stringstream out;
	extern int max_players;
	Character *sort_desc[max_players];
	int num_desc = 0, j;
	int mb = 0, ib = 0;
	int seperate = 0;
	size_t len = 0;
	time_t nw = time ( 0 );
	*buf = 0;
	/*
	-T-Lv-Rac-Class-PK-RP-S--
	[4-30 Fau Thief PK RP F]
	*/

	int low = 0, high = LVL_IMPL, showclass = 0, showrace = 0;
	bool who_room = FALSE, who_zone = FALSE, who_quest = 0;
	bool noimm = FALSE, nomort = FALSE;
	bool sort_login = FALSE, sort_race = FALSE, sort_class = FALSE, readout_who = FALSE;

	int Wizards = 0, Mortals = 0;
	int tWizards = 0, tMortals = 0;
	size_t i;

	const char *WizLevels_male[LVL_IMPL - ( LVL_HERO - 1 ) ] =
	{
		"         Hero        ",
		"         Deity       ",
		"        Builder      ",
		"-       Creator     -",
		"*       Senior      *",
		"**   Implementor   **"
	};

	const char *WizLevels_female[LVL_IMPL - ( LVL_HERO - 1 ) ] =
	{
		"        Hero         ",
		"        Deity        ",
		"       Builder       ",
		"-      Creator      -",
		"*      Senior       *",
		"**   Implementor   **"
	};
	const char *Imm_buf = "\r\n    =Immortals online=\r\n"
	                      "-------------------------\r\n";
	const char *Mort_buf = "\r\n"
	                       "    =Mortals online=\r\n"
	                       "-T-Lv-Rac-Cls-PK-RP-S--H-\r\n";



	skip_spaces ( &argument );
	strcpy ( buf, argument );
	name_search[0] = '\0';

	/* the below is from stock CircleMUD  -- found no reason to rewrite it */
	while ( *buf )
	{
		half_chop ( buf, arg, buf1 );
		if ( isdigit ( *arg ) )
		{
			sscanf ( arg, "%d-%d", &low, &high );
			strcpy ( buf, buf1 );
		}
		else if ( *arg == '-' )
		{
			mode = * ( arg + 1 ); /* just in case; we destroy arg in the switch */
			switch ( mode )
			{
				case 'o':
					sort_login = TRUE;
					strcpy ( buf, buf1 );
					break;
				case 'z':
					who_zone = TRUE;
					strcpy ( buf, buf1 );
					break;
				case 'q':
					who_quest = TRUE;
					strcpy ( buf, buf1 );
					break;
				case 'l':
					half_chop ( buf1, arg, buf );
					sscanf ( arg, "%d-%d", &low, &high );
					break;
				case 'n':
					half_chop ( buf1, name_search, buf );
					break;
				case 'r':
					who_room = TRUE;
					strcpy ( buf, buf1 );
					break;
				case 'j':
					sort_race = TRUE;
					strcpy ( buf, buf1 );
					break;
				case 'c':
					half_chop ( buf1, arg, buf );
					for ( i = 0; i < strlen ( arg ); i++ )
						showclass |= find_class_bitvector ( arg[i] );
					break;
				case 'p':
					sort_class = TRUE;
					strcpy ( buf, buf1 );
					break;
				case 'd':
					half_chop ( buf1, arg, buf );
					for ( i = 0; i < strlen ( arg ); i++ )
						showrace |= find_class_bitvector ( arg[i] );
					break;
				case 'i':
					nomort = TRUE;
					strcpy ( buf, buf1 );
					break;
				case 'm':
					noimm = TRUE;
					strcpy ( buf, buf1 );
					break;
				case 'b':
					readout_who = TRUE;
					strcpy ( buf, buf1 );
					break;
				default:
					*ch << WHO_USAGE;
					return;
			}             /* end of switch */

		}
		else            /* endif */
		{
			*ch << WHO_USAGE;
			return;
		}
	}                 /* end while (parser) */
	

	for ( j = 0; j < max_players; j++ )
		sort_desc[j] = NULL;

	while ( seperate <= 1 )
	{
		for ( d = descriptor_list; d; d = d->next )
		{
			if ( !IS_PLAYING ( d ) )
				continue;


			if ( d->original )
				wch = d->original;
			else if ( ! ( wch = d->character ) )
				continue;

			if ( seperate == 0 )
			{
				if ( GET_LEVEL ( wch ) < LVL_IMMORT )
					continue;
			}
			else
			{
				if ( GET_LEVEL ( wch ) >= LVL_IMMORT )
					continue;
			}

			if ( !CAN_SEE ( ch, wch ) )
				continue;
			if ( GET_LEVEL ( wch ) < low || GET_LEVEL ( wch ) > high )
				continue;
			if ( ( noimm && GET_LEVEL ( wch ) >= LVL_IMMORT )
			        || ( nomort && GET_LEVEL ( wch ) < LVL_IMMORT ) )
				continue;
			if ( *name_search && str_cmp ( GET_NAME ( wch ), name_search )
			        && !strstr ( GET_TITLE ( wch ), name_search ) )
				continue;
			if ( who_quest && !PRF_FLAGGED ( wch, PRF_QUEST ) )
				continue;
			if ( who_zone
			        && IN_ROOM ( ch )->zone != IN_ROOM ( wch )->zone )
				continue;
			if ( who_room && ( IN_ROOM ( wch ) != IN_ROOM ( ch ) ) )
				continue;
			if ( showclass && ! ( showclass & ( 1 << GET_CLASS ( wch ) ) ) )
				continue;
			if ( showrace && ! ( showrace & ( 1 << GET_RACE ( wch ) ) ) )
				continue;

			sort_desc[num_desc] = wch;
			num_desc++;
			if ( GET_LEVEL ( wch ) >= LVL_HERO )
			  tWizards++;
			else
			  tMortals++;
		}
		seperate++;
	}
	if ( !sort_login )
	{
		if ( sort_class )
			qsort ( sort_desc, max_players, sizeof ( Character * ), who_sort_helper_class );
		else if ( sort_race )
			qsort ( sort_desc, max_players, sizeof ( Character * ), who_sort_helper_race );
		else
			qsort ( sort_desc, max_players, sizeof ( Character * ), who_sort_helper_level );
	}

	for ( j = 0; j < max_players; j++ )
	{
		if ( ( wch = sort_desc[j] ) == NULL )
			continue;
		if ( ( d = wch->desc ) == NULL )
			continue;

		if ( !ib && GET_LEVEL ( wch ) >= LVL_IMMORT )
		{
			ib = 1;
			if (readout_who)
			  out << "The Immortals online are: ";
			else
			out << Imm_buf;
		}
		if ( !mb && GET_LEVEL ( wch ) < LVL_IMMORT )
		{
			mb = 1;
			if (readout_who) 
			  out << "The Mortals online are: ";
			else
			out << Mort_buf;
		}



		if ( GET_LEVEL ( wch ) >= LVL_HERO )
		{
		  if (readout_who) {
		    snprintf ( buf, sizeof ( buf ), MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ) ,  GET_NAME ( wch ));
		    } else {
		      if ( GET_SEX ( wch ) != SEX_FEMALE ) {
			  snprintf ( buf, sizeof ( buf ), "%s[%23s]  %s%s" MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ) " %s",  CCYEL ( ch, C_SPR ),
				    center_align ( IMMTITLE ( wch ) ? IMMTITLE ( wch ) : ( char * ) WizLevels_male[GET_LEVEL ( wch ) - LVL_HERO], 20 ),
				    ( PRETITLE ( wch ) == NULL ? "" : PRETITLE ( wch ) ),
				    ( PRETITLE ( wch ) == NULL ? "" : " " ),
				    GET_NAME ( wch ), GET_TITLE ( wch ) );
		    
		      } else {
				snprintf ( buf, sizeof ( buf ), "%s[%23s]  %s%s" MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ) " %s", CCYEL ( ch, C_SPR ),
					  center_align ( IMMTITLE ( wch ) ? IMMTITLE ( wch ) : ( char * ) WizLevels_female[GET_LEVEL ( wch ) - LVL_HERO], 20 ),
					  ( PRETITLE ( wch ) == NULL ? "" : PRETITLE ( wch ) ),
					  ( PRETITLE ( wch ) == NULL ? "" : " " ),
					  GET_NAME ( wch ), GET_TITLE ( wch ) );
				
			}
		  }
		  Wizards++;
		  
		}
		else
		{
			if (readout_who) {
			  snprintf ( buf, sizeof ( buf ), MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ),  GET_NAME ( wch ));
			}
			else if ( AFF_FLAGGED ( wch, AFF_POLY_TOAD ) )
			{
				snprintf ( buf, sizeof ( buf ), "[----------TOAD---------] A slimy toad looking vaguely like " MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ) ".",  GET_NAME ( wch ) );
			}
			else     if ( AFF_FLAGGED ( wch, AFF_POLY_WOLF ) )
			{
				snprintf ( buf, sizeof ( buf ), "[----------WOLF---------] A grey wolf looking vaguely like " MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ) ".", GET_NAME ( wch ) );
			}
			else     if ( AFF_FLAGGED ( wch, AFF_POLY_BOAR ) )
			{
				snprintf ( buf, sizeof ( buf ), "[----------BOAR---------] A frisky boar looking vaguely like " MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ) ".", GET_NAME ( wch ) );
			}
			else     if ( AFF_FLAGGED ( wch, AFF_POLY_BEAR ) )
			{
				snprintf ( buf, sizeof ( buf ), "[----------BEAR---------] A shaggy bear looking vaguely like " MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ) ".",  GET_NAME ( wch ) );
			}
			else     if ( AFF_FLAGGED ( wch, AFF_POLY_LION ) )
			{
				snprintf ( buf, sizeof ( buf ), "[----------LION---------] A black lion looking vaguely like " MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ) ".",  GET_NAME ( wch ) );
			}
			else     if ( get_sub_status ( wch, SUB_SHADOWCLOAK ) )
			{
				snprintf ( buf, sizeof ( buf ), "[---------SHADOWS-------] A swirling tower of shadows." );
			}
			else     if ( get_sub_status ( wch, SUB_CLOAK ) )
			{
				snprintf ( buf, sizeof ( buf ), "[---------CLOAKED-------] A cloaked figure." );
			}
			else     if ( get_sub_status ( wch, SUB_MASK ) )
			{
				snprintf ( buf, sizeof ( buf ), "[---------MASKED--------] A masked figure." );
			}
			else
			{
				if ( PRF_FLAGGED ( ch, PRF_NOTITLE ) )
				{
					snprintf ( buf, sizeof ( buf ), "[%s%d{c0 %2d %s {cy%-3s %s %s %s  %s]{cW%s%s{c0%s%s{c0" MXPTAG ( "B" ) "%s" MXPTAG ( "/B" ) " %s",
					           TIER_COLOUR_WHO ( current_class_is_tier_num ( wch ) ),
					           current_class_is_tier_num ( wch ), GET_LEVEL ( wch ), RACE_ABBR ( wch ), CLASS_ABBR ( wch ),
					           ( PLR_FLAGGED ( wch, PLR_PK ) ? "{crPK{c0" : "{cr--{c0" ),   //is PK or not? bold red
					           ( PLR_FLAGGED ( wch, PLR_ROLEPLAYER ) ? "{cgRP{c0" : "{cg--{c0" ),     //is RP or not? dark green
					           ( ( GET_SEX ( wch ) == SEX_FEMALE ) ? "F" : ( GET_SEX ( wch ) == SEX_MALE ) ? "M" : "N" ),
						   PLR_FLAGGED ( wch, PLR_NEWBIE_HLPR ) ? "{cgH{c0" : "{cg-{c0",
					           ( wch->desc && ( ( nw - 60 ) < wch->desc->login_time ) ) ? "{cy>{cW" : " ",
					           ( GET_REMORT ( wch ) != -1 ? " " : "*" ),
					           ( PRETITLE ( wch ) == NULL ? "" : PRETITLE ( wch ) ),
					           ( PRETITLE ( wch ) == NULL ? "" : " " ),
					           GET_NAME ( wch ),
					           GET_TITLE ( wch ) ); //has a star if hasnt remorted
				}
				else
				{
					snprintf ( buf, sizeof ( buf ), "[%s%d{c0 %2d %s {cy%-3s %s %s %s  %s]{cW%s{c0" MXPTAG ( "B" ) "%-15s" MXPTAG ( "/B" ) ,
					           TIER_COLOUR_WHO ( current_class_is_tier_num ( wch ) ),
					           current_class_is_tier_num ( wch ),GET_LEVEL ( wch ), RACE_ABBR ( wch ), CLASS_ABBR ( wch ),
					           ( PLR_FLAGGED ( wch, PLR_PK ) ? "{crPK{c0" : "{cr--{c0" ),   //is PK or not? bold red
					           ( PLR_FLAGGED ( wch, PLR_ROLEPLAYER ) ? "{cgRP{c0" : "{cg--{c0" ),     //is RP or not? dark green
					           ( ( GET_SEX ( wch ) == SEX_FEMALE ) ? "F" : ( GET_SEX ( wch ) == SEX_MALE ) ? "M" : "N" ),
						   PLR_FLAGGED ( wch, PLR_NEWBIE_HLPR ) ? "{cgH{c0" : "{cg-{c0",
					           ( GET_REMORT ( wch ) != -1 ? "  " : " *" ),
					           GET_NAME ( wch ) ); //they have a star if they haven't yet remorted
				}
			}
			Mortals++;

		}
		len = 0;
		/**put the formatted line into the output buffer -mord **/
		out << buf;
if (!readout_who) {
		if ( ( GET_LEVEL ( wch ) < LVL_HERO ) && GET_CLAN ( wch ) )       /*added ' leader' to the clan name, gave it round brackets and light cyan colour that wont bleed a imms title */
		{
			len += snprintf ( buf + len, sizeof ( buf ) - len, " %s(" MXPTAG ( "em" ) "%s%s%s%s" MXPTAG ( "/em" ) ")",
			                  ( GET_LEVEL ( wch ) >= LVL_HERO ? CCYEL ( ch, C_SPR ) :CCNRM ( ch, C_NRM ) ), CCCYN ( ch, C_NRM ),
			                  clan_name ( find_clan_by_id ( GET_CLAN ( wch ) ) ),
			                  ( ( clan[find_clan_by_id ( GET_CLAN ( wch ) ) ].ranks ==
			                      ( GET_CLAN_RANK ( wch ) ) ) ? " Leader" : PRF_FLAGGED(wch, PRF_RETIRED) ? " Retired CL" : "" ),
			                  ( ( GET_LEVEL ( wch ) >= LVL_HERO ) ? CCYEL ( ch,C_NRM ) : CCNRM ( ch, C_NRM ) ) );
		}

		if ( AFF_FLAGGED ( wch, AFF_OUTCAST ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (%sOutcast%s)", CCRED ( ch, C_NRM ), CCNRM ( ch, C_NRM ) ) ;  

		if ( PLR_FLAGGED ( wch, PLR_NEWBIE_HLPR ) && ( GET_LEVEL ( wch ) >= LVL_HERO ))
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (%sHelper%s)", CCGRN ( ch, C_NRM ),  CCYEL ( ch, C_NRM ) );   //not displaying helper flag yet
		if ( PLR_FLAGGED ( wch, PLR_RP_LEADER ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (%sRPL%s)", CCGRN ( ch, C_NRM ), ( ( GET_LEVEL ( wch ) >= LVL_HERO ) ? CCYEL ( ch, C_NRM ) : CCNRM ( ch, C_NRM ) ) ); //not displaying helper flag yet
		if ( GET_INVIS_LEV ( wch ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (i%d)", GET_INVIS_LEV ( wch ) );
		if ( IS_AFFECTED ( wch, AFF_INVISIBLE ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (invis)" );

		if ( ( GET_LEVEL ( wch ) < LVL_HERO ) && PLR_FLAGGED ( wch, PLR_HERO ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (%sHERO%s)", CCYEL ( ch, C_NRM ), ( ( GET_LEVEL ( wch ) >= LVL_HERO ) ? CCYEL ( ch, C_NRM ) : CCNRM ( ch, C_NRM ) ) );     //not displaying helper flag yet
		if ( affected_by_spell ( wch, SPELL_SILENCED ) && ( GET_LEVEL ( ch ) > LVL_HERO || PLR_FLAGGED ( ch, PLR_HERO ) ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (S)" );

		if ( GET_RP_GROUP ( wch ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (%s%s)", rp_group_names[GET_RP_GROUP ( wch ) ], GET_LEVEL ( wch ) > 50 ? "{cy" : "{c0" );
		if ( grand_master ( wch ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (GM)" );
		if ( PLR_FLAGGED ( wch, PLR_MAILING ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (mailing)" );
		else if ( PLR_FLAGGED ( wch, PLR_WRITING ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (writing)" );
		if ( ROOM_FLAGGED ( IN_ROOM ( wch ), ROOM_SOUNDPROOF ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (!sound)" );
		if ( PRF_FLAGGED ( wch, PRF_DEAF ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (deaf)" );
		if ( GET_ORIG_LEV ( wch ) >= 1 )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (IMM)" );
		if ( PRF_FLAGGED ( wch, PRF_NOGOSS ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (nogos)" ); //Threw in a Nogos flag
		if ( PRF_FLAGGED ( wch, PRF_NOTELL ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (notell)" );
		if ( PRF_FLAGGED ( wch, PRF_QUEST ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (quest)" );
		if ( PLR_FLAGGED ( wch, PLR_THIEF ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (THIEF)" );
		if ( PLR_FLAGGED ( wch, PLR_KILLER ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (KILLER)" );
		if ( PRF_FLAGGED ( wch, PRF_AFK ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (AFK)" );
		if ( PRF_FLAGGED ( wch, PRF_RP ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " ({cyRolePlaying{c0)" );
		if ( PRF_FLAGGED ( wch, PRF_BUSY ) )
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (BUSY)" );
		if ( PRF_FLAGGED ( wch, PRF_DEED_MASTER ) ) 
			len += snprintf ( buf + len, sizeof ( buf ) - len, " ({cYDM{c0)" );
		if ( wch->char_specials.timer > 15 && !GET_INVIS_LEV ( wch ) )
		{
			len += snprintf ( buf + len, sizeof ( buf ) - len, " (idle)" );
		}
                if ( GET_IDNUM(wch) == CHAMPION) 
                        len += snprintf (buf + len, sizeof (buf) - len, " [{cW%s{c0]", dimension_types[zone_table[IN_ROOM ( wch )->zone].dimension]);


		if ( GET_LEVEL ( wch ) >= LVL_HERO )
		{
			len += snprintf ( buf + len, sizeof ( buf ) - len, CCNRM ( ch, C_SPR ) );
		}
		else
		{
			if ( GET_LEVEL ( ch ) >= LVL_HERO || PLR_FLAGGED ( ch, PLR_HERO ) || PLR_FLAGGED ( ch, PLR_NEWBIE_HLPR ) )
			{
				if ( REMORTS ( wch ) == 0 )
				{
					switch ( GET_NEWBIE_STATUS ( wch ) )
					{
						case NEWB_NEW:
							len += snprintf ( buf + len, sizeof ( buf ) - len, " (new to mudding)" );
							break;
						case NEWB_4DNEW:
							len += snprintf ( buf + len, sizeof ( buf ) - len, " (new to 4d)" );
							break;
					}
				}
			}
		}

		if ( GET_LEVEL ( ch ) >= LVL_HERO )
		{
			if ( STATE ( wch->desc ) == CON_OEDIT )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Obj Edit)" );
			if ( STATE ( wch->desc ) == CON_ZEDIT )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Zone Edit)" );
			if ( STATE ( wch->desc ) == CON_MEDIT )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Mob Edit)" );
			if ( STATE ( wch->desc ) == CON_HEDIT )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Help Edit)" );
			if ( STATE ( wch->desc ) == CON_REDIT )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Room Edit)" );
			if ( STATE ( wch->desc ) == CON_TEDIT )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Text Edit)" );
			if ( STATE ( wch->desc ) == CON_TRIGEDIT )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Trigger Edit)" );
			if ( STATE ( wch->desc ) == CON_AEDIT )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Social Edit)" );
			if ( d->connected == CON_CEDIT )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Configuration Edit)" );
			if ( d->original )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (out of body)" );
			if ( PRF_FLAGGED ( wch, PRF_BUILDWALK ) )
				len += snprintf ( buf + len, sizeof ( buf ) - len, " (Buildwalking)" );
		}
		len += snprintf ( buf + len, sizeof ( buf ) - len, "\r\n" );
} else if (readout_who) {
  
		  if ( GET_LEVEL ( wch ) >= LVL_HERO ) {
		    if (tWizards > 1 && (tWizards-1) == Wizards)
		      len += snprintf ( buf + len, sizeof ( buf ) - len, " and " );
		    else if (Wizards < tWizards)
				len += snprintf ( buf + len, sizeof ( buf ) - len, ", " ); 
		    else
		      len += snprintf ( buf + len, sizeof ( buf ) - len, "\r\n" );
		  } else   {
		    if (tMortals > 1 && (tMortals-1) == Mortals)
		      len += snprintf ( buf + len, sizeof ( buf ) - len, " and " );
		    else if (Mortals < tMortals)
				len += snprintf ( buf + len, sizeof ( buf ) - len, ", " );		    
		    else
		      len += snprintf ( buf + len, sizeof ( buf ) - len, "\r\n" );
		  } 
		  
		} 
		
		/** put the current formatted line into the output buffer - mord **/
		out << buf;
		*buf = '\0';
	}                 /* end of for */
	
	/** put the current formatted line into the output buffer, and make a new line - mord **/
	out << buf << endl;
	/** clear the line buffer ready to use again -mord**/
	*buf = '\0';
	len = 0;
	
	if ( ( Wizards + Mortals ) == 0 )
	{
		len += snprintf ( buf + len, sizeof ( buf ) - len, "No immortals or mortals are currently visible to you.\r\n" );

	}
	else if (!readout_who)
	{
	  
		if ( Wizards )
		{
			len += snprintf ( buf + len, sizeof ( buf ) - len, "There %s %d visible immortal%s%s",
			                  ( Wizards == 1 ? "is" : "are" ), Wizards,
			                  ( Wizards == 1 ? "" : "s" ), ( Mortals ? " and there" : "." ) );

		}

		if ( Mortals )
		{
			len += snprintf ( buf + len, sizeof ( buf ) - len, "%s %s %d visible mortal%s.",
			                  ( Wizards ? "" : "There" ), ( Mortals == 1 ? "is" : "are" ),
			                  Mortals, ( Mortals == 1 ? "" : "s" ) );

		}
	}
	len += snprintf ( buf + len, sizeof ( buf ) - len, "\r\n" );

	if ( ( Wizards + Mortals ) > boot_high )
		boot_high = Wizards + Mortals;
	len += snprintf ( buf + len, sizeof ( buf ) - len, "There is a boot time high of %d player%s.\r\n",
	                  boot_high, ( boot_high == 1 ? "" : "s" ) );
	if (!readout_who) {
	len += snprintf ( buf + len, sizeof (buf) - len, "The latest PK champion is {cR%s{cn.\r\n", 
                          CHAMPION  <= 0 ? "Nobody yet" :  LAST_PK); // Name of the Champion

			struct clan_deed_type *cl;
                        int clan_num, deeds_amt = 0, winner_amt = 0, highest_clan = 0;
                        clan_num = 0;
                        while ((clan_num >= 0) &&  (clan_num <= num_of_clans)) {
                              for (cl = clan[clan_num].deeds; cl; cl = cl->next) {
                                deeds_amt += 1;
                        }
			if (deeds_amt > winner_amt) {
			highest_clan = clan_num;
			winner_amt = deeds_amt;
			}
			deeds_amt = 0;
			clan_num += 1;
		     }
	len += snprintf ( buf + len, sizeof (buf) - len, "The best clan currently is {cR%s{cn with {cC%d{cn deeds. \r\n", 
			clan[highest_clan].name, winner_amt);
	}
	/** pass the last line buffer into the string - mord **/
	out << buf << endl;
	*ch << out.str();
}

#define USERS_FORMAT \
"format: users [-l minlevel[-maxlevel]] [-n name] [-h host] [-c classlist] [-r racelist] [-o] [-p]\r\n"

ACMD ( do_users )
{
	char line[200], line2[220], idletime[10], classname[20];
	char state[30], *timeptr, mode;
	char name_search[MAX_INPUT_LENGTH], host_search[MAX_INPUT_LENGTH];
	Character *tch;
	Descriptor *d;
	size_t i;
	int low = 0, high = LVL_IMPL, num_can_see = 0;
	int showclass = 0, outlaws = 0, playing = 0, deadweight = 0;
	int showrace = 0;
	char buf[MAX_STRING_LENGTH];
	char arg[MAX_STRING_LENGTH];
	char buf1[MAX_STRING_LENGTH];

	host_search[0] = name_search[0] = '\0';

	strcpy ( buf, argument );
	while ( *buf )
	{
		half_chop ( buf, arg, buf1 );
		if ( *arg == '-' )
		{
			mode = * ( arg + 1 ); /* just in case; we destroy arg in the switch */
			switch ( mode )
			{
				case 'o':
				case 'k':
					outlaws = 1;
					playing = 1;
					strcpy ( buf, buf1 );
					break;
				case 'p':
					playing = 1;
					strcpy ( buf, buf1 );
					break;
				case 'd':
					deadweight = 1;
					strcpy ( buf, buf1 );
					break;
				case 'l':
					playing = 1;
					half_chop ( buf1, arg, buf );
					sscanf ( arg, "%d-%d", &low, &high );
					break;
				case 'n':
					playing = 1;
					half_chop ( buf1, name_search, buf );
					break;
				case 'h':
					playing = 1;
					half_chop ( buf1, host_search, buf );
					break;
				case 'c':
					playing = 1;
					half_chop ( buf1, arg, buf );
					for ( i = 0; i < strlen ( arg ); i++ )
						showclass |= find_class_bitvector ( arg[i] );
					break;
					/*
					      case 'r':
					        playing = 1;
					        half_chop(buf1, arg, buf);
					        for (i = 0; i < strlen(arg); i++)
					          showrace |= find_race_bitvector(arg[i]);
					        break;
					*/
				default:
					ch->Send ( USERS_FORMAT );
					return;
			}             /* end of switch */

		}
		else            /* endif */
		{
			ch->Send ( USERS_FORMAT );
			return;
		}
	}                 /* end while (parser) */
	// Tweaking this for longer names.
	// Prometheus
	ch->Send (
	    "Num Class       Name         State          Idl Login@   Site\r\n"
	    "--- ----------- ------------ -------------- --- -------- ------------------------\r\n" );


	one_argument ( argument, arg );

	for ( d = descriptor_list; d; d = d->next )
	{
		if ( STATE ( d ) != CON_PLAYING && playing )
			continue;
		if ( STATE ( d ) == CON_PLAYING && deadweight )
			continue;
		if ( IS_PLAYING ( d ) )
		{
			if ( d->original )
				tch = d->original;
			else if ( ! ( tch = d->character ) )
				continue;

			if ( *host_search && !strstr ( d->host.c_str(), host_search ) )
				continue;
			if ( *name_search && str_cmp ( GET_NAME ( tch ), name_search ) )
				continue;
			if ( !CAN_SEE ( ch, tch ) || GET_LEVEL ( tch ) < low
			        || GET_LEVEL ( tch ) > high )
				continue;
			if ( outlaws && !PLR_FLAGGED ( tch, PLR_KILLER ) &&
			        !PLR_FLAGGED ( tch, PLR_THIEF ) )
				continue;
			if ( showclass && ! ( showclass & ( 1 << GET_CLASS ( tch ) ) ) )
				continue;
			if ( showrace && ! ( showrace & ( 1 << GET_RACE ( tch ) ) ) )
				continue;
			if ( GET_INVIS_LEV ( ch ) > GET_LEVEL ( ch ) )
				continue;

			if ( d->original )
				snprintf ( classname, sizeof ( classname ), "[%2d %s %s]", GET_LEVEL ( d->original ),
				           RACE_ABBR ( d->original ), CLASS_ABBR ( d->original ) );
			else
				snprintf ( classname, sizeof ( classname ), "[%2d %s %s]", GET_LEVEL ( d->character ),
				           RACE_ABBR ( d->character ), CLASS_ABBR ( d->character ) );
		}
		else
			strcpy ( classname, "     -      " );

		timeptr = asctime ( localtime ( &d->login_time ) );
		timeptr += 11;
		* ( timeptr + 8 ) = '\0';

		if ( STATE ( d ) == CON_PLAYING && d->original )
			strcpy ( state, "Switched" );
		else
		{
			switch ( STATE ( d ) )
			{
				case CON_CREATE_NEW:
					strcpy ( state, creation_state_types[SUB_STATE ( d ) ] );
					break;
				default:
					strcpy ( state, connected_types[STATE ( d ) ] );
					break;
			}
		}

		if ( d->character && STATE ( d ) == CON_PLAYING
		        && GET_LEVEL ( d->character ) < LVL_GOD )
			sprintf ( idletime, "%3d",
			          d->character->char_specials.timer * SECS_PER_MUD_HOUR / SECS_PER_REAL_MIN );
		else
			strcpy ( idletime, "" );

		snprintf ( line, sizeof ( line ), "%3d %-7s %-12s %-14s %-3s %-8s ", d->desc_num, classname,
		           d->original && d->original->player.name ? d->original->player.name :
		           d->character && d->character->player.name ? d->character->player.name :
		           "UNDEFINED",
		           state, idletime, timeptr );

		if ( !d->host.empty() )
			snprintf ( line + strlen ( line ), sizeof ( line ) - strlen ( line ), "[%s]\r\n", d->host.c_str() );
		else
			strlcat ( line, "[Hostname unknown]\r\n", sizeof ( line ) );

		if ( STATE ( d ) != CON_PLAYING )
		{
			snprintf ( line2, sizeof ( line2 ), "%s%s%s", CCGRN ( ch, C_SPR ), line,
			           CCNRM ( ch, C_SPR ) );
			strcpy ( line, line2 );
		}
		if ( STATE ( d ) != CON_PLAYING ||
		        ( STATE ( d ) == CON_PLAYING && CAN_SEE ( ch, d->character ) ) )
		{
			*ch << line;
			num_can_see++;
		}
	}

	ch->Send ( "\r\n%d visible sockets connected.\r\n",
	           num_can_see );

}

ACMD ( do_clients )
{
	for ( Descriptor *d = descriptor_list; d; d = d->next )
	{
		if ( IS_PLAYING ( d ) )
		{
			ch->Send ( "\tO%-15s\tn %s (%s)%s%s%s%s%s%s%s%s%s\r\n", 
				GET_NAME( d->character ), 
				d->pProtocol->pVariables[eMSDP_CLIENT_ID]->pValueString, 
				d->pProtocol->pVariables[eMSDP_CLIENT_VERSION]->pValueString, 
				d->pProtocol->bTTYPE ? " TTYPE" : "", 
				d->pProtocol->bNAWS ? " NAWS" : "", 
				d->pProtocol->bCHARSET ? " CHARSET" : "", 
				d->pProtocol->bMSDP ? " MSDP" : "", 
				d->pProtocol->bATCP ? " ATCP" : "", 
				d->pProtocol->bMSP ? " MSP" : "", 
				d->pProtocol->bMXP ? " MXP" : "",
				d->pProtocol->bMCCP ? " MCCP" : "",
				d->pProtocol->pVariables[eMSDP_XTERM_256_COLORS]->ValueInt ? " 256-COLORS" : "" );
		}
	}
}

ACMD ( do_ipstat )
{

	if ( !ch->desc || IS_NPC ( ch ) )
	{
		ch->Send ( "Um... yeah your host is crap all.\r\n" );
		return;
	}
	if ( !ch->desc->host.empty() )
		ch->Send ( "{cGYour ip is [%s] host [%s]\r\n{c0", ch->desc->host_ip.c_str(), ch->desc->host.c_str() );
	else
		ch->Send ( "{cGYour ip is [Hostname unknown]{c0\r\n" );

	ch->Send ( "\tOYour client is [%s] version [%s]\tn\r\n", 
		ch->desc->pProtocol->pVariables[eMSDP_CLIENT_ID]->pValueString, 
		ch->desc->pProtocol->pVariables[eMSDP_CLIENT_VERSION]->pValueString );

	ch->Send ( "\t[F210]Your protocols are %s%s%s%s%s%s%s%s%s\tn\r\n", 
		ch->desc->pProtocol->bTTYPE ? "[TTYPE] " : "", 
		ch->desc->pProtocol->bNAWS ? "[NAWS] " : "", 
		ch->desc->pProtocol->bCHARSET ? "[CHARSET] " : "", 
		ch->desc->pProtocol->bMSDP ? "[MSDP] " : "", 
		ch->desc->pProtocol->bATCP ? "[ATCP] " : "", 
		ch->desc->pProtocol->bMSP ? "[MSP] " : "", 
		ch->desc->pProtocol->bMXP ? "[MXP] " : "",
		ch->desc->pProtocol->bMCCP ? "[MCCP] " : "",
		ch->desc->pProtocol->pVariables[eMSDP_XTERM_256_COLORS]->ValueInt ? "[256-COLORS] " : "" );

	if ( ch->desc->pProtocol->bNAWS )
	{
		ch->Send ( "\t[F245]Your screen is %d characters wide and %d characters high.\tn\r\n", 
		ch->desc->pProtocol->ScreenWidth, 
		ch->desc->pProtocol->ScreenHeight );
	}
}

/* Generic page_string function for displaying text */
ACMD ( do_gen_ps )
{
	switch ( subcmd )
	{
		case SCMD_CREDITS:
			page_string ( ch->desc, credits, 0 );
			break;
		case SCMD_NEWS:
			page_string ( ch->desc, news, 0 );
			break;
		case SCMD_INFO:
			page_string ( ch->desc, info, 0 );
			break;
		case SCMD_WIZLIST:
			page_string ( ch->desc, wizlist, 0 );
			break;
		case SCMD_IMMLIST:
			page_string ( ch->desc, immlist, 0 );
			break;
		case SCMD_HANDBOOK:
			page_string ( ch->desc, handbook, 0 );
			break;
		case SCMD_POLICIES:
			page_string ( ch->desc, policies, 0 );
			break;
		case SCMD_MOTD:
			page_string ( ch->desc, motd, 0 );
			break;
		case SCMD_IMOTD:
			page_string ( ch->desc, imotd, 0 );
			break;
		case SCMD_CLEAR:
			ch->Send ( "\033[H\033[J" );
			break;
		case SCMD_VERSION:
			ch->Send ( "%s\r\n", fourdimensions_version );
			ch->Send ( "%s\r\n", oasisolc_version );
			ch->Send ( "%s\r\n", circlemud_version );
			ch->Send ( "%s\r\n", DG_SCRIPT_VERSION );
			break;
		case SCMD_WHOAMI:
			ch->Send ( "%s\r\n", GET_NAME ( ch ) );
			break;
		default:
			log ( "SYSERR: Unhandled case in do_gen_ps. (%d)", subcmd );
			return;
	}
}


void perform_mortal_where ( Character *ch, char *arg )
{
	register Character *i;
	register Descriptor *d;
        struct clan_deed_type *cl;
        int found = -1, clan_num;
	
        if ( !*arg )
	{
		ch->Send ( "Players in your Zone [%s]\r\n--------------------\r\n", zone_table[IN_ROOM ( ch )->zone].name );
                for (clan_num = 0; clan_num < num_of_clans; clan_num++ ) {
                        if (!clan[clan_num].deeds || found > -1)
                                continue;
                        found = -1;
                        for (cl = clan[clan_num].deeds; cl; cl = cl->next) {
                               // if ( !strcmp(zone_table[real_zone(cl->zone)].name, zone_table[IN_ROOM ( ch )->zone].name ) )
                                if ((real_zone(cl->zone) == IN_ROOM ( ch )->zone))
                                        found = clan_num;
                                continue;
                        }
                }
                        ch->Send ( "This zone is currently controlled by %s{cx. \r\n", (found > -1) ? clan[found].name   : "no clan");
			ch->Send ( "You have %d tracked kills in this zone at the moment. \r\n", ch->player.deeds.kills);

		for ( d = descriptor_list; d; d = d->next )
		{
			if ( STATE ( d ) != CON_PLAYING || d->character == ch )
				continue;
			if ( ( i = ( d->original ? d->original : d->character ) ) == NULL )
				continue;
			if ( i->in_room == NULL || !CAN_SEE ( ch, i ) )
				continue;
			if ( IN_ROOM ( ch )->zone != IN_ROOM ( i )->zone )
				continue;
			if ( get_sub_status ( i, SUB_SHADOWCLOAK ) )
				continue;
			if ( get_sub_status ( i, SUB_CLOAK ) )
				continue;
			if ( get_sub_status ( i, SUB_MASK ) )
				continue;
			ch->Send ( "{cg%-20s{cw - {cc%s{c0\r\n", GET_NAME ( i ),
			           IN_ROOM ( i )->name );
		}
	}
	else              /* print only FIRST char, not all. */
	{
		for ( i = character_list; i; i = i->next )
		{
			if ( i->in_room == NULL || i == ch )
				continue;
			if ( !CAN_SEE ( ch, i )
			        || i->in_room->zone != IN_ROOM ( ch )->zone )
				continue;
			if ( !isname_full ( arg, i->player.name ) )
				continue;
			ch->Send ( "{cg%-25s{cw - {cc%s{c0\r\n", GET_NAME ( i ),
			           IN_ROOM ( i )->name );
			return;
		}
		ch->Send ( "No-one around by that name.\r\n" );
	}
}

const char * hidden_name ( Character *ch )
{

	if ( get_sub_status ( ch, SUB_SHADOWCLOAK ) )
		return "a shadowy figure";
	if ( get_sub_status ( ch, SUB_CLOAK ) )
		return "a cloaked figure";
	if ( get_sub_status ( ch, SUB_MASK ) )
		return "a masked figure";

	return GET_NAME ( ch );
}


void print_object_location ( int num, struct obj_data *obj,
                             Character *ch, int recur, char *buffer )
{
	if ( num > 0 )
		sprintf ( buffer, "O%3d. [%5d] %-25s - ", num, GET_OBJ_VNUM ( obj ), obj->short_description );
	else
		sprintf ( buffer, "%33s", " - " );

	if ( obj->proto_script )
		sprintf ( buffer + strlen ( buffer ), "[Trig]" );

	if ( obj->owner != 0 )
		sprintf ( buffer + strlen ( buffer ), " Ownr:%s ", pi.NameById ( obj->owner ) );

	if ( obj->in_room != NULL )
	{
		sprintf ( buffer + strlen ( buffer ), "[%5d] %s\r\n",
		          GET_ROOM_VNUM ( IN_ROOM ( obj ) ), IN_ROOM ( obj )->name );
	}
	else if ( obj->carried_by )
	{
		sprintf ( buffer + strlen ( buffer ), "carried by %s\r\n",
		          PERS ( obj->carried_by, ch ) );
	}
	else if ( obj->worn_by )
	{
		sprintf ( buffer + strlen ( buffer ), "worn by %s\r\n",
		          PERS ( obj->worn_by, ch ) );
	}
	else if ( obj->in_locker )
	{
		sprintf ( buffer + strlen ( buffer ), "in locker of %s\r\n",
		          PERS ( obj->in_locker, ch ) );
	}
	else if ( obj->in_obj )
	{
		sprintf ( buffer + strlen ( buffer ), "inside %s%s\r\n",
		          obj->in_obj->short_description,
		          ( recur ? ", which is" : " " ) );
		if ( recur )
			print_object_location ( 0, obj->in_obj, ch, recur, buffer + strlen ( buffer ) );
	}
	else
	{
		sprintf ( buffer + strlen ( buffer ), "in an unknown location\r\n" );
	}
}



void perform_immort_where ( Character *ch, char *arg )
{
	register Character *i;
	register struct obj_data *k;
	Descriptor *d;
	int num = 0, counter = 0, found = 0;

	if ( !*arg )
	{
		new_mudlog ( CMP, ( GET_LEVEL ( ch ) == LVL_IMPL ? GET_LEVEL ( ch ) :GET_LEVEL ( ch ) +1 ), TRUE, "(GC) %s did a \"where\".",
		             GET_NAME ( ch ) );
		ch->Send ( "Players\r\n-------\r\n" );
		for ( d = descriptor_list; d; d = d->next )
		{
			lock_desc ( d );
			if ( STATE ( d ) == CON_PLAYING )
			{
				i = ( d->original ? d->original : d->character );
				if ( i && CAN_SEE ( ch, i ) && ( i->in_room != NULL ) )
				{
					if ( d->original )
						ch->Send ( "%-15s - [%5d] %-25s (in %s)\r\n",
						           GET_NAME ( i ),
						           GET_ROOM_VNUM ( IN_ROOM ( d->character ) ),
						           d->character->in_room->name,
						           GET_NAME ( d->character ) );
					else
						ch->Send ( "%-15s - [%5d] %-25s - {cC%s{cx\r\n", GET_NAME ( i ),
						           GET_ROOM_VNUM ( IN_ROOM ( i ) ),
						           i->in_room->name,  zone_table[ ( i->in_room )->zone].name );
				}
			}
			unlock_desc ( d );
		}
	}
	else
	{
		char buf[ MAX_STRING_LENGTH];
		DYN_DEFINE;
		*buf = 0;

		DYN_CREATE;
		*dynbuf = 0;

		new_mudlog ( CMP, ( GET_LEVEL ( ch ) == LVL_IMPL ? GET_LEVEL ( ch ) :GET_LEVEL ( ch ) +1 ), TRUE, "(GC) %s did a \"where %s\".",
		             GET_NAME ( ch ),arg );
		counter = 0;
		for ( i = character_list; i; i = i->next )
			if ( CAN_SEE ( ch, i ) && i->in_room != NULL
			        && isname_full ( arg, i->player.name ) )
			{
				found = 1;
				snprintf ( buf, sizeof ( buf ), "M%3d. [%5d] %-25s - [%5d] %-25s %s\r\n", ++num, GET_MOB_VNUM ( i ),
				           GET_NAME ( i ), GET_ROOM_VNUM ( IN_ROOM ( i ) ),
				           IN_ROOM ( i )->name,
				           ( IS_NPC ( i ) && i->proto_script ) ? "[TRIG]" : "" );
				DYN_RESIZE ( buf );

			}
		counter = 0;
		for ( olt_it ob = object_list.begin(); ob != object_list.end(); ob++ )
		{
			k = ( ob->second );
			if ( CAN_SEE_OBJ ( ch, k ) && isname_full ( arg, k->name ) )
			{
				found = 1;
				print_object_location ( ++num, k, ch, TRUE, buf );
				DYN_RESIZE ( buf );
			}
		}
		counter = 0;
		room_rnum rm;
		int j;
		for ( j = 0; j < top_of_world; j++ )
		{
			if ( ( rm = world_vnum[j] ) == NULL )
				continue;
			if ( isname_full ( arg, rm->name ) )
			{
				found = 1;
				snprintf ( buf, sizeof ( buf ), "W%3d. [%6d][%-8s][%c:%c] {cy%-25s {cg-{cC %s{c0\r\n",
				           ++counter, GET_ROOM_VNUM ( rm ), sector_types[SECT ( rm ) ],
				           ROOM_FLAGGED ( rm, ROOM_WILDERNESS ) ? 'W' : '-',
				           SCRIPT ( rm ) ? 'T' : '-',
				           rm->name,
				           zone_table[rm->zone].name );

				DYN_RESIZE ( buf );
			}
		}


		if ( !found )
		{
			if ( dynbuf )
				;
			free ( dynbuf );
			ch->Send ( "Couldn't find any such thing.\r\n" );
		}
		else
			page_string ( ch->desc, dynbuf, DYN_BUFFER );
	}
}



ACMD ( do_where )
{
	//  char arg[MAX_STRING_LENGTH];
	skip_spaces ( &argument );

	if ( GET_LEVEL ( ch ) > LVL_HERO && ( ( CMD_FLAGGED ( ch, WIZ_IMM2_GRP ) )
	                                      || ( CMD_FLAGGED2 ( ch, WIZ_IMM2_GRP ) ) ) )
		perform_immort_where ( ch, argument );
	else
		perform_mortal_where ( ch, argument );
}



ACMD ( do_levels )
{
	int i;
	char buf[MAX_STRING_LENGTH];
	size_t len = 0;

	if ( IS_NPC ( ch ) )
	{
		ch->Send ( "You ain't nothin' but a hound-dog.\r\n" );
		return;
	}

	for ( i = 1; i < LVL_HERO; i++ )
	{
		len += snprintf ( buf + len, sizeof ( buf ) - len,
		                  "[{cg%2d{c0] {cy%10lld{c0-{cy%-10lld{c0 ({cR%-9lld{c0): ",
		                  i, level_exp ( GET_CLASS ( ch ), i,
		                                 current_class_is_tier_num ( ch ), REMORTS ( ch ) ),
		                  level_exp ( GET_CLASS ( ch ), i + 1,
		                              current_class_is_tier_num ( ch ), REMORTS ( ch ) ),
		                  level_exp ( GET_CLASS ( ch ), i + 1,
		                              current_class_is_tier_num ( ch ), REMORTS ( ch ) ) -
		                  level_exp ( GET_CLASS ( ch ), i,
		                              current_class_is_tier_num ( ch ), REMORTS ( ch ) ) );
		switch ( GET_SEX ( ch ) )
		{
			case SEX_MALE:
			case SEX_NEUTRAL:
				len += snprintf ( buf + len, sizeof ( buf ) - len, "%s", title_male ( GET_CLASS ( ch ), i ) );
				break;
			case SEX_FEMALE:
				len += snprintf ( buf + len, sizeof ( buf ) - len, "%s", title_female ( GET_CLASS ( ch ), i ) );
				break;
			default:
				ch->Send ( "Oh dear.  You seem to be sexless.\r\n" );
				break;
		}
		len += snprintf ( buf + len, sizeof ( buf ) - len, "%s", "\r\n" );
	}
	len += snprintf ( buf + len, sizeof ( buf ) - len,  "[{cg%2d{c0] {cy%8lld{c0                         : Immortality\r\n",
	                  LVL_HERO, level_exp ( GET_CLASS ( ch ), LVL_HERO,
	                                        current_class_is_tier_num ( ch ), REMORTS ( ch ) ) );
	page_string ( ch->desc, buf, 1 );
}


/*
ACMD(do_consider)
{
  Character *victim;
  int diff;

  one_argument(argument, buf);

  if (!(victim = get_char_vis(ch, buf, FIND_CHAR_ROOM))) {
    send_to_char("Consider killing who?\r\n", ch);
    return;
  }
  if (victim == ch) {
    send_to_char("Easy!  Very easy indeed!\r\n", ch);
    return;
  }
  if (!IS_NPC(victim)) {
    send_to_char("Would you like to borrow a cross and a shovel?\r\n", ch);
    return;
  }
  diff = (GET_LEVEL(victim) - GET_LEVEL(ch));

  if (diff <= -10)
    send_to_char("Now where did that chicken go?\r\n", ch);
  else if (diff <= -5)
    send_to_char("You could do it with a needle!\r\n", ch);
  else if (diff <= -2)
    send_to_char("Easy.\r\n", ch);
  else if (diff <= -1)
    send_to_char("Fairly easy.\r\n", ch);
  else if (diff == 0)
    send_to_char("The perfect match!\r\n", ch);
  else if (diff <= 1)
    send_to_char("You would need some luck!\r\n", ch);
  else if (diff <= 2)
    send_to_char("You would need a lot of luck!\r\n", ch);
  else if (diff <= 3)
    send_to_char("You would need a lot of luck and great equipment!\r\n", ch);
  else if (diff <= 5)
    send_to_char("Do you feel lucky, punk?\r\n", ch);
  else if (diff <= 10)
    send_to_char("Are you mad!?\r\n", ch);
  else if (diff <= 100)
    send_to_char("You ARE mad!\r\n", ch);

}
*/
const char * speed_consider[] =
{
	"$E is slow like a slug.",
	"$E is slow like a turtle.",
	"$E is moving as if in water.",
	"$E is looking tired.",
	"$E seems quite limber!",
	"$E is quick like a fox!",
	"$E moves faster than a speeding bullet!",
	"$E strikes faster than lightning!"
};
#define NUM_CONS_SPEED (8)


#define NUM_CONS_ATTACK 8
const char * hitp_consider[] =
{
	"$E has so few hitpoints that they should be legally dead.",
	"$E has so few hitpoints that it's almost ridiculous.",
	"$S hitpoints are in the hundreds.",
	"$S hitpoints are in the thousands.",
	"$S hitpoints are in the tens of thousands.",
	"$S hitpoints are in the hundred thousands.",
	"$E has some fucking insane amount of hitpoints!",
	"$E has so many more hitpoints than you, it's obscene!!!"
};
#define NUM_CONS_HITP 8
const char * damage_consider[] =
{
	"Very Easy : Your nickname is genocide.",
	"Reasonably Easy : Your nickname is mass slaughter.",
	"Simple : Your nickname is serial killer.",
	"Not Too Simple : Your nickname is tough bastard.",
	"Hard : Your nickname is lightly bruised.",
	"Quite Hard : Your nickname is eyes bigger than your sword.",
	"Very Hard : Your nickname is slightly broken person.",
	"Very Very Hard : Your nickname is has hospital insurance.",
	"Ultra difficult : Your nickname is can afford corpse recovery.",
	"Uber : Your nickname is Here Lies One Dead Player."
};

const char * dam_consider[] =
{
	""
};

#define NUM_CONS_DAMAGE 10

int arena_ok ( Character *ch, Character *victim );

ACMD ( do_consider )
{
	Character *victim;
	int diff, diff2, diff3, evasion_tot_ch, evasion_tot_vict, accuracy_tot_ch, accuracy_tot_vict;
	//  char buf[MAX_INPUT_LENGTH];
	skip_spaces ( &argument );


	if ( ! ( victim = get_char_vis ( ch, argument, NULL, FIND_CHAR_ROOM ) ) )
	{
		ch->Send ( "Consider killing who?\r\n" );
		return;
	}
	if ( victim == ch )
	{
		ch->Send ( "Easy!  Very easy indeed!\r\n" );
		return;
	}
	ch->Send ( "%s is a tier %d %s\r\n", GET_NAME ( victim ), current_class_is_tier_num ( victim ), simple_class_name ( victim ) );
	if ( !IS_NPC ( victim ) )
	{

		if ( ( PLR_FLAGGED ( ch, PLR_PK ) && PLR_FLAGGED ( victim, PLR_PK ) )
		        || ( PLR_FLAGGED ( victim, PLR_KILLER ) )
		        || ( arena_ok ( ch, victim ) ) )
		{
			victim->Send ( "You get considered by somebody.\r\n" );
			diff = ( GET_MAX_HIT ( ch ) - GET_MAX_HIT ( victim ) );
			diff2 = ( GET_MAX_MANA ( ch ) - GET_MAX_MANA ( victim ) );
			diff3 = ( GET_DAMROLL ( ch ) - GET_DAMROLL ( victim ) );
			if ( diff <= -400 )
				ch->Send ( "They are WAY tougher than you, uh ohh.\r\n" );
			else if ( diff <= -200 )
				ch->Send ( "They are quite reasonably tougher than you.\r\n" );
			else if ( diff <= -100 )
				ch->Send ( "They are moderately tougher than you.\r\n" );
			else if ( diff < -10 )
				ch->Send ( "They are a bit tougher than you.\r\n" );
			else if ( diff >= ( -10 ) && diff <= 10 )
				ch->Send ( "They are as tough as you!\r\n" );
			else if ( diff <= ( 100 ) )
				ch->Send ( "They are not nearly as tough as you.\r\n" );
			else if ( diff <= ( 200 ) )
				ch->Send ( "They are tough but not up to your standard.\r\n" );
			else if ( diff <= ( 400 ) )
				ch->Send ( "You are way tougher than they are.\r\n" );
			else if ( diff > 400 )
				ch->Send ( "Is it even worth fighting for?\r\n" );

			if ( diff2 <= -5000 )
				ch->Send ( "Comparing your magical energy is like comparing a match to a super nova.\r\n" );
			else if ( diff2 <= -2500 )
				ch->Send ( "They have more magical ability in one finger than you have in your entire being.\r\n" );
			else if ( diff2 <= -1000 )
				ch->Send ( "They have enchanting energy like you could only dream of.\r\n" );
			else if ( diff2 < -500 )
				ch->Send ( "They have magical energy to spare!\r\n" );
			else if ( diff2 < -100 )
				ch->Send ( "They are what wishes are made of.\r\n" );
			else if ( diff2 > -100 && diff2 < 100 )
				ch->Send ( "Your magical energys are about the same.\r\n" );
			else if ( diff2 < 500 )
				ch->Send ( "They have lower magical energy than you.\r\n" );
			else if ( diff2 < 1000 )
				ch->Send ( "They have much lower magical energy than you.\r\n" );
			else if ( diff2 < 2500 )
				ch->Send ( "They have magical energy that almost reaches the knees of your magical energy.\r\n" );
			else if ( diff2 <= 5000 )
				ch->Send ( "You have substantially more magical energy.\r\n" );
			else if ( diff2 > 5000 )
				ch->Send ( "You have the most magical energy by far.\r\n" );

			if ( diff3 < -60 )
				ch->Send ( "EPITAPH: Here Lies One Stupid Player.\r\n" );
			else if ( diff3 < -40 )
				ch->Send ( "You have more chance in a Deathtrap\r\n" );
			else if ( diff3 < -20 )
				ch->Send ( "They probably eat fools like you for breakfast.\r\n" );
			else if ( diff3 < -10 )
				ch->Send ( "They gonna hurt you bad.\r\n" );
			else if ( diff3 >= -10 && diff3 <= 10 )
				ch->Send ( "You are about the same damage.\r\n" );
			else if ( diff3 < 20 )
				ch->Send ( "You are gonna hurt them more.\r\n" );
			else if ( diff3 < 40 )
				ch->Send ( "You eat puppies like this for breakfast.\r\n" );
			else if ( diff3 < 60 )
				ch->Send ( "It won't hurt a bit.\r\n" );
			else if ( diff > 60 )
				ch->Send ( "They will be honored by death if you fight them.\r\n" );

		}
		else
		{
			ch->Send ( "Would you like to borrow a cross and a shovel?\r\n" );
			return;
		}
	}
	/*
	speed
	defence
	attack
	hitp
	damage
	*/

	diff = IRANGE ( 0, ( ( speed_update ( victim ) +1000 ) /250 ), 7 );
	act ( speed_consider[diff], FALSE, ch, 0, victim, TO_CHAR );

	if ( is_fused ( ch ) )
	{
		evasion_tot_ch = fused_evasion ( ch );
		accuracy_tot_ch = fused_accuracy ( ch );
	}
	else
	{
		evasion_tot_ch = evasion_tot ( ch );
		accuracy_tot_ch = accuracy_tot ( ch );
	}

	if ( is_fused ( victim ) )
	{
		evasion_tot_vict = fused_evasion ( victim );
		accuracy_tot_vict = fused_accuracy ( victim );
	}
	else
	{
		evasion_tot_vict = evasion_tot ( victim );
		accuracy_tot_vict = accuracy_tot ( victim );
	}

	ch->Send ( "%s would land about %.1f%% of %s attacks on you.\r\n",
		   // Changing the sex check to HSHR(victim) instead of 
		   //GET_SEX ( victim ) == SEX_MALE ? "his" : "her" -- Prom
	           GET_NAME ( victim ), 100 - ( float ) ( ( evasion_tot_ch * 100.0 ) / ( evasion_tot_ch + accuracy_tot_vict ) ), HSHR ( victim ) );
	ch->Send ( "You would land about %.1f%% of your attacks on %s.\r\n",
	           100- ( float ) ( ( evasion_tot_vict * 100.0 ) / ( evasion_tot_vict + accuracy_tot_ch ) ), GET_NAME ( victim ) );

	diff3 = GET_HIT ( victim );
	diff2 = 0;
	while ( 0 != ( diff3 /= 10 ) )
		diff2++;
	diff = IRANGE ( 0, diff2, 7 );
	act ( hitp_consider[diff], FALSE, ch, 0, victim, TO_CHAR );

	ch->Send ( "You guess %s would probably do around %d damage to you per hit if you had no protection.\r\n",
	           GET_NAME ( victim ), modify_dam ( average_damage ( victim ), victim, ch, find_fe_type ( victim ) ) );

}

bool is_fused ( Character *ch )
{
	int i;
	bool fused_self = FALSE, fused_other = FALSE;

	if ( IS_NPC ( ch ) )
		return FALSE;

	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
		if ( FUSE_LOC ( ch, i ) == ch )
			fused_self = TRUE;
		else if ( FUSE_LOC ( ch, i ) && FUSE_LOC ( ch, i ) != ch && IN_ROOM ( ch ) == IN_ROOM ( FUSE_LOC ( ch, i ) ) )
			fused_other = TRUE;

	if ( fused_self && fused_other )
		return TRUE;
	else return FALSE;
}

int fused_speed ( Character *ch )
{
	int speed = 0, i;
	Character *tmp_ch;

	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
	{

		tmp_ch = FUSE_LOC ( ch, i );
		if ( tmp_ch && IN_ROOM ( ch ) == IN_ROOM ( tmp_ch ) )
		{
			if ( i == 0 || i == 4 )
				speed += 2 * GET_SPEED ( tmp_ch );
			else
				speed += GET_SPEED ( tmp_ch );
		}
	}

	return MIN ( 1000, speed / TOP_FUSE_LOCATION );
}

int fused_AC ( Character *ch )
{
	int AC = 0, i;
	Character *tmp_ch;

	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
	{
		tmp_ch = FUSE_LOC ( ch, i );
		if ( tmp_ch && IN_ROOM ( ch ) == IN_ROOM ( tmp_ch ) )
		{
			if ( i == 3 || i == 5 )
				AC += 2 * tmp_ch->compute_armor_class();
			else
				AC += tmp_ch->compute_armor_class();
		}
	}

	return IRANGE ( -100, AC / TOP_FUSE_LOCATION, 100 );
}

int fused_hitroll ( Character *ch )
{
	int hitroll = 0, i;
	Character *tmp_ch;

	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
	{
		tmp_ch = FUSE_LOC ( ch, i );
		if ( tmp_ch && IN_ROOM ( ch ) == IN_ROOM ( tmp_ch ) )
		{
			if ( i == 2 || i == 5 )
				hitroll += 2 * GET_HITROLL ( tmp_ch );
			else
				hitroll += GET_HITROLL ( tmp_ch );
		}
	}

	return hitroll / TOP_FUSE_LOCATION;
}

int fused_dambonus ( Character *ch )
{
	int dambonus = 0, i;
	Character *tmp_ch;

	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
	{
		tmp_ch = FUSE_LOC ( ch, i );
		if ( tmp_ch && IN_ROOM ( ch ) == IN_ROOM ( tmp_ch ) )
		{
			if ( i == 0 || i == 3 )
				dambonus += 2 * ( ( IS_CASTER ( GET_CLASS ( tmp_ch ) ) || has_staff ( tmp_ch ) ) ? caster_damroll ( tmp_ch ) : fighter_damroll ( tmp_ch ) );
			else
				dambonus += ( IS_CASTER ( GET_CLASS ( tmp_ch ) ) || has_staff ( tmp_ch ) ) ? caster_damroll ( tmp_ch ) : fighter_damroll ( tmp_ch );
		}
	}

	return dambonus / TOP_FUSE_LOCATION;
}

int fused_accuracy ( Character *ch )
{
	int accuracy = 0, i;
	Character *tmp_ch;

	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
	{
		tmp_ch = FUSE_LOC ( ch, i );
		if ( tmp_ch && IN_ROOM ( ch ) == IN_ROOM ( tmp_ch ) )
		{
			if ( i == 1 || i == 4 )
				accuracy += 2 * accuracy_tot ( tmp_ch );
			else
				accuracy += accuracy_tot ( tmp_ch );
		}
	}

	return accuracy / TOP_FUSE_LOCATION;
}

int fused_evasion ( Character *ch )
{
	int evasion = 0, i;
	Character *tmp_ch;

	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
	{
		tmp_ch = FUSE_LOC ( ch, i );
		if ( tmp_ch && IN_ROOM ( ch ) == IN_ROOM ( tmp_ch ) )
		{
			if ( i == 1 || i == 2 )
				evasion += 2 * evasion_tot ( tmp_ch );
			else
				evasion += evasion_tot ( tmp_ch );
		}
	}

	return evasion / TOP_FUSE_LOCATION;
}

ACMD ( do_fuse )
{
	char buf[MAX_INPUT_LENGTH];
	int i;
	Character *tmp_ch;
	follow_type *f;

	if ( IS_NPC ( ch ) )
		return;

	one_argument ( argument, buf );

	// Show fused stats
	if ( !*buf )
	{
		speed_update ( ch );

		if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
			ch->Send ( "Fuse stats:\r\n"
				   "TopCenter(dr+sp): %s\r\n"
				   "TopLeft(ev+acc): %s\r\n"
				   "TopRight(hr+ev): %s\r\n"
				   "Center(AC+dr): %s\r\n"
				   "LowerLeft(sp+acc): %s\r\n"
				   "LowerRight(AC+hr): %s\r\n"
				   "Speed: %d AC: %d\r\n"
				   "Hitroll: %d Dam-bonus: %d\r\n"
				   "Accuracy: %d Evasion: %d\r\n",
		  FUSE_LOC ( ch, 0 ) && IN_ROOM ( FUSE_LOC ( ch, 0 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 0 ) ) : "empty",
		  FUSE_LOC ( ch, 1 ) && IN_ROOM ( FUSE_LOC ( ch, 1 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 1 ) ) : "empty",
		  FUSE_LOC ( ch, 2 ) && IN_ROOM ( FUSE_LOC ( ch, 2 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 2 ) ) : "empty",
		  FUSE_LOC ( ch, 3 ) && IN_ROOM ( FUSE_LOC ( ch, 3 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 3 ) ) : "empty",
		  FUSE_LOC ( ch, 4 ) && IN_ROOM ( FUSE_LOC ( ch, 4 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 4 ) ) : "empty",
		  FUSE_LOC ( ch, 5 ) && IN_ROOM ( FUSE_LOC ( ch, 5 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 5 ) ) : "empty",
		  fused_speed ( ch ), fused_AC ( ch ),
		  fused_hitroll ( ch ), fused_dambonus ( ch ),
		  fused_accuracy ( ch ), fused_evasion ( ch ) );
		else ch->Send ( "{cg(*)----------------{cwFuse Stats{cg------------------(*)\r\n"
				"| |                  {cgTopCenter(dr+sp)          {cg| |\r\n"
				"| |                  {cy%-26s{cg| |\r\n"
				"| | TopLeft(ev+acc)            TopRight(hr+ev) | |\r\n"
				"| | {cy%-21s%21s {cg| |\r\n"
				"| |                  Center(AC+dr)             | |\r\n"
				"| |                  {cy%-26s{cg| |\r\n"
				"| | LowerLeft(sp+acc)        LowerRight(AC+hr) | |\r\n"
				"| | {cy%-21s%21s {cg| |\r\n"
				"| |--------------------------------------------| |\r\n"
				"| |      {cwSpeed: [{cc%4d{cw]           AC: [{cc%4d{cw]    {cg| |\r\n"
				"| |    {cwHitroll: [{cc%4d{cw]    Dam-Bonus: [{cc%4d{cw]    {cg| |\r\n"
				"| |   {cwAccuracy: [{cc%4d{cw]      Evasion: [{cc%4d{cw]    {cg| |\r\n"
				"--------------------------------------------------{c0\r\n",
		  FUSE_LOC ( ch, 0 ) && IN_ROOM ( FUSE_LOC ( ch, 0 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 0 ) ) : "empty",
		  FUSE_LOC ( ch, 1 ) && IN_ROOM ( FUSE_LOC ( ch, 1 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 1 ) ) : "empty",
		  FUSE_LOC ( ch, 2 ) && IN_ROOM ( FUSE_LOC ( ch, 2 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 2 ) ) : "empty",
		  FUSE_LOC ( ch, 3 ) && IN_ROOM ( FUSE_LOC ( ch, 3 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 3 ) ) : "empty",
		  FUSE_LOC ( ch, 4 ) && IN_ROOM ( FUSE_LOC ( ch, 4 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 4 ) ) : "empty",
		  FUSE_LOC ( ch, 5 ) && IN_ROOM ( FUSE_LOC ( ch, 5 ) ) == IN_ROOM ( ch ) ? GET_NAME ( FUSE_LOC ( ch, 5 ) ) : "empty",
		  fused_speed ( ch ), fused_AC ( ch ),
		  fused_hitroll ( ch ), fused_dambonus ( ch ),
		  fused_accuracy ( ch ), fused_evasion ( ch ) );

		if ( !is_fused ( ch ) )
			ch->Send ( "NOTE: These stats do not apply since less than two fuse members are here.\r\n" );

		return;
	}

	// Add fuse location
	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
	{
		if ( !strcasecmp ( buf, fuse_locations[i] ) )
		{
			if ( FUSE_LOC ( ch, i ) == ch )
				FUSE_LOC ( ch, i ) = NULL;
			else FUSE_LOC ( ch, i ) = ch;

			if ( FUSE_LOC ( ch, i ) )
				ch->Send ( "You fuse with the %s.\r\n", fuse_locations[i] );
			else ch->Send ( "You stop fusing with the %s.\r\n", fuse_locations[i] );

			if ( MASTER ( ch ) )
			{
				tmp_ch = MASTER ( ch );
				FUSE_LOC ( tmp_ch, i ) = FUSE_LOC ( ch, i );
				if ( FUSE_LOC ( ch, i ) )
					tmp_ch->Send ( "%s fuses with the %s.\r\n", GET_NAME ( ch ), fuse_locations[i] );
				else tmp_ch->Send ( "%s stops fusing with the %s.\r\n", GET_NAME ( ch ), fuse_locations[i] );
			}
			else tmp_ch = ch;

			for ( f = tmp_ch->followers; f; f = f->next )
			{
				if ( f->follower == ch )
					continue;
				FUSE_LOC ( f->follower, i ) = FUSE_LOC ( ch, i );
				if ( FUSE_LOC ( ch, i ) )
					(f->follower)->Send ( "%s fuses with the %s.\r\n", GET_NAME ( ch ), fuse_locations[i] );
				else (f->follower)->Send ( "%s stops fusing with the %s.\r\n", GET_NAME ( ch ), fuse_locations[i] );
			}
			return;
		}
	}

	ch->Send ( "Unknown fuse location, choose one from: topcenter, topleft, topright, center, lowerleft, lowerright.\r\n" );
}

void stop_fusion ( Character *ch )
{
	int i;
	Character *tmp_ch;
	follow_type *f;
	bool was_fused = FALSE;

	if ( IS_NPC ( ch ) )
		return;

	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
		FUSE_LOC ( ch, i ) = NULL;
		if ( FUSE_LOC ( ch, i ) )
		{
			was_fused = TRUE;
			FUSE_LOC ( ch, i ) = NULL;
		}

	if ( !was_fused )
		return;

	if ( MASTER ( ch ) )
	{
		tmp_ch = MASTER ( ch );
		for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
			if ( FUSE_LOC ( tmp_ch, i ) == ch )
				FUSE_LOC ( tmp_ch, i ) = NULL;
		tmp_ch->Send ( "%s has left the fusion.\r\n", GET_NAME ( ch ) );
	}
	else tmp_ch = ch;

	for ( f = tmp_ch->followers; f; f = f->next )
	{
		if ( f->follower == ch )
			continue;
		for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
			if ( FUSE_LOC ( f->follower, i ) == ch )
				FUSE_LOC ( f->follower, i ) = NULL;
		(f->follower)->Send ( "%s has left the fusion.\r\n", GET_NAME ( ch ) );
	}
}

void enter_fusion ( Character *ch, Character *leader )
{
	int i;

	for ( i = 0; i < TOP_FUSE_LOCATION; i++ )
		FUSE_LOC ( ch, i ) = FUSE_LOC ( leader, i );

	return;
}

/*
So far, the idea is that you have 6 fusion locations.
Similar to say, equipment locations.
Also simmilar to a group formation.

When you are fused with 1 person, you get 3 location slots each.
Your damage, armor, attack, defence, and speed is combined and catylized.
The location of the slots are simmilar to body positions.
Top Center, Top Left, Top Right, Center, Lower Left, Lower Right
Each body position multiplys each attribute listed above, either positively or negitively.
For example, lower left and lower right (legs), would add a 2.0 times multiplyer
to the person who had those slots (in this example its 2 players fused together, so
each player would get 3 slots, so each players stats would be devided amoung the slots,
and each slot would provide a multiplyer to that stat, and add to the total,

If player 1, had 200 speed, and player 2 had 600 speed.
and player 2 got the 2 lower leg slots and the center slot.
then player 2's speed would be devided into those 3 slots evenly.
200 each slot.
And because the two legs slots give a 2.0 times multiplyer to speed
that 200 speed on each leg slot, would now equal 400 speed each.
and the center slot would remain unchanged.
so the total speed this player is bringing to the fusion is 1000 speed,
instead of 600.
This would be counterbalanced by other slots giving a low speed multiplyer, or ano speed multiplyer.

So depending on what player has what stats, you would assign different slots to them.
Any command a player types (this is still to be thought oversome more) while in a fused state
will be executed AS the fusion leader. (fusion leader being the person who initiated the first fusion)
so all players joined in the fusion can control what happens to the fusion as a whole.
Although, this feature may not be in the final copy of this feature, as the saying goes
too many chef's spoil the broth.

*/

ACMD ( do_diagnose )
{
	Character *vict;
	skip_spaces ( &argument );

	if ( *argument )
	{
		if ( ! ( vict = get_char_vis ( ch, argument, NULL, FIND_CHAR_ROOM ) ) )
			ch->Send ( "%s", CONFIG_NOPERSON );
		else
			diag_char_to_char ( vict, ch );
	}
	else
	{
		if ( FIGHTING ( ch ) )
			diag_char_to_char ( FIGHTING ( ch ), ch );
		else
			ch->Send ( "Diagnose who?\r\n" );
	}
}


const char *ctypes[] =
{
	"off", "sparse", "normal", "complete", "\n"
};

ACMD ( do_colour )
{
	char arg[MAX_INPUT_LENGTH];
	int tp;

	if ( IS_NPC ( ch ) )
		return;

	one_argument ( argument, arg );

	if ( !*arg )
	{
		ch->Send ( "Your current colour level is %s.\r\n",
		           ctypes[COLOUR_LEV ( ch ) ] );

		return;
	}
	if ( ( ( tp = search_block ( arg, ctypes, FALSE ) ) == -1 ) )
	{
		ch->Send ( "Usage: colour {{ Off | Sparse | Normal | Complete }\r\n" );
		return;
	}
	REMOVE_BIT_AR ( PRF_FLAGS ( ch ), PRF_COLOUR_1 );
	REMOVE_BIT_AR ( PRF_FLAGS ( ch ), PRF_COLOUR_2 );
	if ( tp & 1 )
		SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_COLOUR_1 );
	if ( tp & 2 )
		SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_COLOUR_2 );
	ch->Send ( "Your %scolour%s is now %s.\r\n", CCRED ( ch, C_SPR ),
	           CCNRM ( ch, C_OFF ), ctypes[tp] );
}


ACMD ( do_toggle )
{
	char buf2[4];
	if ( IS_NPC ( ch ) )
		return;
	if ( GET_WIMP_LEV ( ch ) == 0 )
		strcpy ( buf2, "OFF" );
	else
		snprintf ( buf2, sizeof ( buf2 ), "%-3.3d", GET_WIMP_LEV ( ch ) );

	if ( GET_LEVEL ( ch ) >= LVL_GOD )
	{
		char buf3[50];
		if ( GET_CSNP_LVL ( ch ) ==-2 )
			snprintf ( buf3,50,"None" );
		else if ( GET_CSNP_LVL ( ch ) ==-1 )
			snprintf ( buf3,50,"All" );
		else if ( GET_CSNP_LVL ( ch ) >=0 && GET_CSNP_LVL ( ch ) <=num_of_clans )
		  sprintf ( buf3,"%s", clan[GET_CSNP_LVL ( ch ) ].name );

		ch->Send (
		    "      Buildwalk: %-3s    "
		    "Clear Screen in OLC: %-3s"
		    "    Ctell snoop: %-3s\r\n",
		    ONOFF ( PRF_FLAGGED ( ch, PRF_BUILDWALK ) ),
		    ONOFF ( PRF_FLAGGED ( ch, PRF_CLS ) ),
		    buf3
		);


		ch->Send (
		    "      No Hassle: %-3s    "
		    "      Holylight: %-3s    "
		    "     Room Flags: %-3s\r\n",
		    ONOFF ( PRF_FLAGGED ( ch, PRF_NOHASSLE ) ),
		    ONOFF ( PRF_FLAGGED ( ch, PRF_HOLYLIGHT ) ),
		    ONOFF ( PRF_FLAGGED ( ch, PRF_ROOMFLAGS ) )
		);
	}

	ch->Send (
	    "Hit Pnt Display: %-3s    "
	    "     Brief Mode: %-3s    "
	    " Summon Protect: %-3s\r\n"
	    "      AUTOGROUP: %-3s    "
	    "   Compact Mode: %-3s    "
	    "       On Quest: %-3s\r\n"
	    "   Mana Display: %-3s    "
	    "         NoTell: %-3s    "
	    "   Repeat Comm.: %-3s\r\n"
	    " Auto Show Exit: %-3s    "
	    " Auto Splitting: %-3s    "
	    "   Auto Looting: %-3s\r\n"
	    "           Deaf: %-3s    "
	    "     Wimp Level: %-3s    "
	    " Gossip Channel: %-3s\r\n"
	    "Auction Channel: %-3s    "
	    "  Grats Channel: %-3s    "
	    "    Auto-Assist: %-3s\r\n"
	    "      Auto-Gold: %-3s    "
	    "     Keep Title: %-3s    "
	    "     IC Channel: %-3s\r\n"
	    "    Color Level: %-8s"
	    "  NoBattlespam: %-3s     "
	    " NoMail Prompt: %-3s \r\n"
	    "        NOCTalk: %-3s    "
	    "        AFKTELL: %-3s    "
	    "        MoveMsg: %-3s\r\n"
	    "   Hero Channel: %-3s    "
	    " Newbie Channel: %-3s    "
	    "   Time Display: %-3s\r\n"
	    "        Autosac: %-3s    "
	    " No-OOC Channel: %-3s    "
	    "      Mountable: %-3s\r\n"
	    "    Compression: %-3s    "
	    "     PageHeight: %-3d    "
	    "      PageWidth: %-3d\r\n"
	    "          Aggro: %-3s    "
	    "       PageWrap: %-3s    "
	    "         NoBrag: %-3s\r\n"
	    "         NOGATE: %-3s    "
	    "      FishTally: %-3s    "
	    "     NOTELEPORT: %-3s\r\n"
	    " NoDisplayTitle: %-3s    "
	    "     NoGraphics: %-3s\r\n",
	    ONOFF ( PRF_FLAGGED ( ch, PRF_DISPHP ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_BRIEF ) ),
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_SUMMONABLE ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_AUTOGROUP ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_COMPACT ) ),
	    YESNO ( PRF_FLAGGED ( ch, PRF_QUEST ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_DISPMANA ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_NOTELL ) ),
	    YESNO ( !PRF_FLAGGED ( ch, PRF_NOREPEAT ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_AUTOEXIT ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_AUTOSPLIT ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_AUTOLOOT ) ),
	    YESNO ( PRF_FLAGGED ( ch, PRF_DEAF ) ),
	    buf2,
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_NOGOSS ) ),
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_NOAUCT ) ),
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_NOGRATZ ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_AUTOASSIST ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_AUTOGOLD ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_KEEPTITLE ) ),
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_NOIC ) ),
	    ctypes[COLOUR_LEV ( ch ) ],
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_BATTLESPAM ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_MAIL ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_NOCTALK ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_AFKTELL ) ),
	    YESNO ( !PRF_FLAGGED ( ch, PRF_MOVEMSG ) ),
	    YESNO ( !PRF_FLAGGED ( ch, PRF_NOHERO ) ),
	    YESNO ( !PRF_FLAGGED ( ch, PRF_NONEWBIE ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_TIME ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_AUTOSAC ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_OOC ) ),
	    YESNO ( PRF_FLAGGED ( ch, PRF_MOUNTABLE ) ),
#if defined(HAVE_ZLIB)
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_NOCOMPRESS ) ),
#else
	    "N/A",
#endif
	    PAGEHEIGHT ( ch ),
	    PAGEWIDTH ( ch ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_AGGRO ) ),
	    YESNO ( PRF_FLAGGED ( ch, PRF_PAGEWRAP ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_NOBRAG ) ),
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_GATEABLE ) ),
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_FISHPROMPT ) ),
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_TELEPORTABLE ) ),
	    ONOFF ( !PRF_FLAGGED ( ch, PRF_NOTITLE ) ),
	    ONOFF ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
	    );

}


/*struct sort_struct {
  int sort_pos;
  byte is_social;
} *cmd_sort_info = NULL;*/

int num_of_cmds;

int sort_commands_helper ( const void *a, const void *b )
{
	return strcmp ( complete_cmd_info[* ( const int * ) a].sort_as,
	                complete_cmd_info[* ( const int * ) b].sort_as );
}

void sort_commands ( void )
{
	int a, cnum_of_cmds = 0;

	while ( complete_cmd_info[cnum_of_cmds].command[0] != '\n' )
		cnum_of_cmds++;
	cnum_of_cmds++;         /* \n */

	CREATE ( cmd_sort_info, int, cnum_of_cmds );

	for ( a = 0; a < cnum_of_cmds; a++ )
		cmd_sort_info[a] = a;

	/* Don't sort the RESERVED or \n entries. */
	qsort ( cmd_sort_info + 1, cnum_of_cmds - 2, sizeof ( int ),  sort_commands_helper );
}

ACMD ( do_commandslike )
{
	int no, i, cmd_num;
	int wizhelp = 0, socials = 0;
	Character *vict = ch;
	skip_spaces ( &argument );

	if ( subcmd == SCMD_SOCIALS )
		socials = 1;
	else if ( subcmd == SCMD_WIZHELP )
		wizhelp = 1;

	ch->Send ( "The following %s%s are available to %s:\r\n",
	           wizhelp ? "privileged " : "",
	           socials ? "socials" : "commands",
	           vict == ch ? "you" : GET_NAME ( vict ) );

	/* cmd_num starts at 1, not 0, to remove 'RESERVED' *
	   for (no = 1, cmd_num = 1; cmd_num < num_of_cmds; cmd_num++) {
	   i = cmd_sort_info[cmd_num].sort_pos;
	   if (complete_cmd_info[i].minimum_level >= 0 &&
	   GET_LEVEL(vict) >= complete_cmd_info[i].minimum_level &&
	   (complete_cmd_info[i].minimum_level >= LVL_HERO) == wizhelp &&
	   (wizhelp || socials == cmd_sort_info[i].is_social)) {
	   sprintf(buf + strlen(buf), "%-11s", complete_cmd_info[i].command);
	   if (!(no % 7))
	   strcat(buf, "\r\n");
	   no++;
	   }
	   } */

	/* cmd_num starts at 1, not 0, to remove 'RESERVED' */
	for ( no = 1, cmd_num = 1; complete_cmd_info[cmd_sort_info[cmd_num]].command[0] != '\n'; cmd_num++ )
	{
		i = cmd_sort_info[cmd_num];

		if ( complete_cmd_info[i].minimum_level < 0 || GET_LEVEL ( vict ) < complete_cmd_info[i].minimum_level )
			continue;

		if ( ( complete_cmd_info[i].minimum_level >= LVL_IMMORT ) != wizhelp )
			continue;

		if ( !wizhelp && socials != ( complete_cmd_info[i].command_pointer == do_action || complete_cmd_info[i].command_pointer == do_insult ) )
			continue;

		if ( !isname ( argument,complete_cmd_info[i].command ) )
			continue;

		ch->Send ( "%-15s%s", complete_cmd_info[i].command,                no++ % 7 == 0 ? "\r\n" : "" );
	}

	if ( no % 7 != 1 )
		ch->Send ( "\r\n" );
}


ACMD ( do_commands )
{
	int no, i, cmd_num;
	int wizhelp = 0, socials = 0;
	Character *vict;
	skip_spaces ( &argument );


	if ( *argument )
	{
		if ( ! ( vict = get_char_vis ( ch, argument, NULL, FIND_CHAR_WORLD ) ) || IS_NPC ( vict ) )
		{
			ch->Send ( "Who is that?\r\n" );
			return;
		}
		if ( GET_LEVEL ( ch ) < GET_LEVEL ( vict ) )
		{
			ch->Send ( "You can't see the commands of people above your level.\r\n" );
			return;
		}
	}
	else
		vict = ch;

	if ( subcmd == SCMD_SOCIALS )
		socials = 1;
	else if ( subcmd == SCMD_WIZHELP )
		wizhelp = 1;

	ch->Send ( "The following %s%s are available to %s:\r\n",
	           wizhelp ? "privileged " : "",
	           socials ? "socials" : "commands",
	           vict == ch ? "you" : GET_NAME ( vict ) );

	/* cmd_num starts at 1, not 0, to remove 'RESERVED' *
	   for (no = 1, cmd_num = 1; cmd_num < num_of_cmds; cmd_num++) {
	   i = cmd_sort_info[cmd_num].sort_pos;
	   if (complete_cmd_info[i].minimum_level >= 0 &&
	   GET_LEVEL(vict) >= complete_cmd_info[i].minimum_level &&
	   (complete_cmd_info[i].minimum_level >= LVL_HERO) == wizhelp &&
	   (wizhelp || socials == cmd_sort_info[i].is_social)) {
	   sprintf(buf + strlen(buf), "%-11s", complete_cmd_info[i].command);
	   if (!(no % 7))
	   strcat(buf, "\r\n");
	   no++;
	   }
	   } */

	/* cmd_num starts at 1, not 0, to remove 'RESERVED' */
	/* cmd_num starts at 1, not 0, to remove 'RESERVED' */
	for ( no = 1, cmd_num = 1; complete_cmd_info[cmd_sort_info[cmd_num]].command[0] != '\n'; cmd_num++ )
	{
		i = cmd_sort_info[cmd_num];

		if ( complete_cmd_info[i].minimum_level < 0 || GET_LEVEL ( vict ) < complete_cmd_info[i].minimum_level )
			continue;

		if ( ( complete_cmd_info[i].minimum_level >= LVL_IMMORT ) != wizhelp )
			continue;

		if ( !wizhelp && socials != ( complete_cmd_info[i].command_pointer == do_action || complete_cmd_info[i].command_pointer == do_insult ) )
			continue;

		ch->Send ( "%-15s%s", complete_cmd_info[i].command,                no++ % 7 == 0 ? "\r\n" : "" );
	}

	if ( no % 7 != 1 )
		ch->Send ( "\r\n" );
}

ACMD ( do_prereq )
{
	int level, i,skill_num;
	char msg1[MAX_STRING_LENGTH], msg2[MAX_STRING_LENGTH];
	size_t len = 0;
	// char msg[MAX_STRING_LENGTH];

	*msg1 = '\0';
	*msg2 = '\0';

	skip_spaces ( &argument );

	if ( !*argument )
	{
		
		send_to_char
		( "Min Level: Skill/Spell Name        First Requirement         Second Requirement\r\n",
		  ch );
		for ( level = 1; level < LVL_IMMORT; level++ )
		{
			//found = FALSE;
			len += snprintf ( msg1 + len, sizeof ( msg1 ) - len, "Level %3d:\r\n", level );
			for ( i = 1; i < MAX_SKILLS + 1; i++ )
			{
				if ( spell_info[i].min_level == level )
				{
					if ( spell_info[i].first_prereq !=
					        TYPE_UNDEFINED )
					{
						
						len += snprintf ( msg1 + len, sizeof ( msg1 ) - len,
						                  "            %-20s : ",
						                  spell_info[i].name );
						len += snprintf ( msg1 + len, sizeof ( msg1 ) - len,
						                  "    %-20s",
						                  spell_info[spell_info[i].first_prereq].name );
						if ( spell_info[i].second_prereq !=
						        TYPE_UNDEFINED )
						{
							len += snprintf ( msg1 + len, sizeof ( msg1 ) - len, "   %-20s",
							                  spell_info[spell_info[i].
							                             second_prereq].name );
						}

						len += snprintf ( msg1 + len, sizeof ( msg1 ) - len, "%s", "\r\n" );
					}
				}
			}
			if ( len )
			{
				len += snprintf ( msg1 + len, sizeof ( msg1 ) - len, "%s", "\r\n" );
			}
		}
		page_string ( ch->desc, msg1, 1 );
	}
	else
	{
		skill_num = find_skill_num ( argument );
		if ( skill_num == NOTHING )
			return;


		ch->Send ( "Pre-requisites for %s :-\r\n",
		           spell_info[skill_num].name );

		if ( spell_info[skill_num].first_prereq !=   TYPE_UNDEFINED )
		{
			len += snprintf ( msg1 + len, sizeof ( msg1 ) - len,  "    %s",
			                  spell_info[spell_info[skill_num].first_prereq].name );
			if ( spell_info[skill_num].second_prereq != TYPE_UNDEFINED )
			{
				len += snprintf ( msg1 + len, sizeof ( msg1 ) - len,  "   %s",
				                  spell_info[spell_info[skill_num].second_prereq].name );
			}
			ch->Send ( "%s", msg1 );
		}
		else
		{
			ch->Send ( "     None." );
		}
		ch->Send ( "\r\n" );
	}

}

/*--------
You have 26620 coins in cash, 0 in the bank.
You have scored 5072121054 exp, and need 417903376 exp to level.
Tokens on file:     0 brass,      0 bronze.
                    0 silver,     0 gold.
You have done a total of 0 remorts.
  Remort: Priest  Remort2: Priest  Remort3: Priest
Your Natural attack rating is 28.
Your Natural defence rating is 32
You are regenerating hitpoints at 18 per mud hour
You are regenerating mana at 66 per mud hour
You are regenerating move at 42 per mud hour
   Head And Neck Armor: -2147483648
 Upper Left Body Armor: -2147483648
Upper Right Body Armor: -2147483648
           Torso Armor: -2147483648
 Lower Left Body Armor: -2147483648
Lower Right Body Armor: -2147483648
----


O===================================================================O
|          .   :   .          | Remorts: 00  1:ABC 2:ABC 3:ABC
|      '.   .  :  .   .'      |
|   ._   '._.-'''-._.'   _.   | Natural Attack Rating : 000
|     '-..'         '..-'     | Natural Defence Rating: 000
|  --._ /.==.     .==.\ _.--  | Regenerating per mud hour:
|      ;/_o__\   /_o__\;      |     Hp:000  Ma:000  Mv:000
| -----|`     ) (     `|----- | ------------------------------------O
|     _: \_) (\_/) (_/ ;_     |    Head And Neck Armor: 000
|  --'  \  '._.=._.'  /  '--  |  Upper Left Body Armor: 000
|    _.-''.  '._.'  .''-._    | Upper Right Body Armor: 000
|   '    .''-.(_).-''.    '   |            Torso Armor: 000
|  jgs .'   '  :  '   '.      |  Lower Left Body Armor: 000
|         '    :   '          | Lower Right Body Armor: 000
|              '              |               Coolness: 000
O===================================================================O


---------*/
#define SUNNY (!(sunlight == SUN_SET || sunlight == SUN_DARK))

ACMD ( do_worth )
{
	char buf[40] = "", buf1[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];
        const char *ethos[MAX_INPUT_LENGTH], *detector[MAX_INPUT_LENGTH];
	size_t len = 0;
	int i;
	static const char *moonage[] =
	{
		"{cL ",
		"{cL ",
		"{cL             ____     ",
		"{cL          ,'' _,-`.   ",
		"{cL       ,'  ,'_|       ",
		"{cL      /   / <>|       ",
		"{cL     :   :    \\       ",
		"{cL     :   :    _\\      ",
		"{cL      \\   \\  -|       ",
		"{cL       `.  `._|       ",
		"{cL         `..__`-.'    ",
		"{cL ",
		"{cL ",
		"{cL ",
		"{cL "

	};
	static const char *sunnage[] =
	{
		"{cR          .   :   .          ",
		"{cR      '.   .  :  .   .'      ",
		"{cR   ._   '._.-'''-._.'   _.   ",
		"{cR     '-..'         '..-'     ",
		"{cR  --._ /.==.     .==.\\ _.--  ",
		"{cR      ;/_o__\\   /_o__\\;      ",
		"{cR -----|`     ) (     `|----- ",
		"{cR     _: \\_) (\\_/) (_/ ;_     ",
		"{cR  --'  \\  '._.=._.'  /  '--  ",
		"{cR    _.-''.  '._.'  .''-._    ",
		"{cR   '    .''-.(_).-''.    '   ",
		"{cR      .'   '  :  '   '.      ",
		"{cR         '    :   '          ",
		"{cR              '              ",
		"{cR"

	};
	if ( IS_NPC ( ch ) )
		return;

	  *ethos = ("None.");
        if (GET_ETHOS(ch) == 1)
           *ethos = ("{cWGood{cn");
        if (GET_ETHOS(ch) == 2)
           *ethos = ("{cBNeutral{cn");
        if (GET_ETHOS(ch) == 3)
           *ethos = ("{cREvil{cn");

	   *detector = ("{crLow{cn");
        if (GET_DETECTOR(ch) == 1)
           *detector = ("{cGHigh{cn");

	for ( i = 0; i < NUM_CLASSES; i++ )
		if ( GET_MASTERY ( ch, i ) )
			len += snprintf ( buf+len, sizeof ( buf ) - len, "%c ", UPPER ( *pc_class_types[i] ) );

	if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
	{
	   ch->Send ( "\r\n"
		      "{cwRemorts: {cg%-3d{cy 1:%3s 2:%3s 3:%3s      \r\n"
	              "{cwNatural Accuracy Rating: {cg%-3d{cy         \r\n"
	              "{cwNatural Evasion Rating: {cg%-3d{cy         \r\n"
	              "{cwRegenerating per mud hour: {cyHp:{cc%-3d {cyMa:{cc%-3d {cyMv:{cc%-3d {cySt:{cc%-3d{cy\r\n"
	              "{cwHead And Neck Armor: {cL%%%-3d{cy        \r\n"
	              "{cwUpper Left Body Armor: {cL%%%-3d{cy        \r\n"
	              "{cwUpper Right Body Armor: {cL%%%-3d{cy        \r\n"
	              "{cwTorso Armor: {cL%%%-3d{cy        \r\n"
	              "{cwLower Left Body Armor: {cL%%%-3d{cy        \r\n"
	              "{cwLower Right Body Armor: {cL%%%-3d{cy        \r\n"
	              "{cwCoolness: {cg%-3d{cy       \r\n"
	              "{cwAward Points: {cg%-3d{cw Ethos: {cg%-3s{cy   \r\n"
	              "{cwStamina: {cC%d/%d{cw Detector: {cg%-3s{cy \r\n"
                      "{cwMastered Classes: {cg%s{cy\r\n"
	              "{cwElemental Weakness: {cr%s{cy\r\n"
	              "{cwElemental Strength: {cc%s{cy\r\n",
	              REMORTS ( ch ),
	              GET_REMORT ( ch ) == -1 ? "---" : class_abbrevs[ ( int ) GET_REMORT ( ch ) ],
	              GET_REMORT_TWO ( ch ) == -1 ? "---" : class_abbrevs[ ( int ) GET_REMORT_TWO ( ch ) ],
	              GET_REMORT_THREE ( ch ) == -1 ? "---" : class_abbrevs[ ( int ) GET_REMORT_THREE ( ch ) ],
	              GET_PERM_ACCURACY ( ch ),
	              GET_PERM_EVASION ( ch ),
	      	      hit_gain ( ch ), mana_gain ( ch ), move_gain ( ch ), stamina_gain ( ch ),
 		      chance_hit_part ( ch, PART_HEAD ),
	              chance_hit_part ( ch, PART_LEFT_ARM ),
	              chance_hit_part ( ch, PART_RIGHT_ARM ),
	              chance_hit_part ( ch, PART_TORSO ),
	              chance_hit_part ( ch, PART_LEFT_LEG ),
	              chance_hit_part ( ch, PART_LEFT_LEG ),
	              GET_COOLNESS ( ch ),
	              update_award ( ch ), *ethos,
	              GET_STAMINA ( ch ), GET_MAX_STAMINA ( ch ), *detector,
                      buf,
	              print_elemental ( GET_CLASS ( ch ), TRUE, buf1, sizeof ( buf1 ) ),
	              print_elemental ( GET_CLASS ( ch ), FALSE, buf2, sizeof ( buf2 ) ) 
		    );
	   return;
	}

	ch->Send ( "\r\n{cy"
	           "O=====================================================================O\r\n"
	           "|%-32s{cy|#|   {cwRemorts: {cg%-3d{cy 1:%3s 2:%3s 3:%3s      \r\n"
	           "|%-32s{cy|#|                                       \r\n"
	           "|%-32s{cy|#|    {cwNatural Accuracy Rating : {cg%-3d{cy         \r\n"
	           "|%-32s{cy|#|    {cw Natural Evasion Rating : {cg%-3d{cy         \r\n"
	           "|%-32s{cy|#|   {cwRegenerating per mud hour:{cy          \r\n"
	           "|%-32s{cy|#| Hp:{cc%-3d{cy  Ma:{cc%-3d{cy  Mv:{cc%-3d{cy  St:{cc%-3d{cy        \r\n"
	           "|%-32s{cy|#|=====================================O\r\n"
	           "|%-32s{cy|#|      {cwHead And Neck Armor: {cL%%%-3d{cy        \r\n"
	           "|%-32s{cy|#|    {cwUpper Left Body Armor: {cL%%%-3d{cy        \r\n"
	           "|%-32s{cy|#|   {cwUpper Right Body Armor: {cL%%%-3d{cy        \r\n"
	           "|%-32s{cy|#|              {cwTorso Armor: {cL%%%-3d{cy        \r\n"
	           "|%-32s{cy|#|    {cwLower Left Body Armor: {cL%%%-3d{cy        \r\n"
	           "|%-32s{cy|#|   {cwLower Right Body Armor: {cL%%%-3d{cy        \r\n"
	           "|%-32s{cy|#|                 {cwCoolness:  {cg%-3d{cy       \r\n"
	           "|%-32s{cy|#| {cwAward Points: {cg%-3d{cw   | Ethos: {cg%-3s{cy   \r\n"
	           "|%-32s{cy|#| {cwStamina: {cC%d/%d{cw    | Detector: {cg%-3s{cy \r\n"

                   "|%-32s{cy|#| {cwMastered Classes: {cg%s{cy\r\n"
	           "|%-32s{cy|#| {cw Elemental Weakness: {cr%s{cy\r\n"
	           "|%-32s{cy|#| {cw Elemental Strength: {cc%s{cy\r\n"
	           "O=====================================================================O{c0\r\n",
	           SUNNY ? sunnage[0] : moonage[0], REMORTS ( ch ),
	           GET_REMORT ( ch ) == -1 ? "---" : class_abbrevs[ ( int ) GET_REMORT ( ch ) ],
	           GET_REMORT_TWO ( ch ) == -1 ? "---" : class_abbrevs[ ( int ) GET_REMORT_TWO ( ch ) ],
	           GET_REMORT_THREE ( ch ) == -1 ? "---" : class_abbrevs[ ( int ) GET_REMORT_THREE ( ch ) ],
	           SUNNY ? sunnage[ 1] : moonage[ 1],
	           SUNNY ? sunnage[ 2] : moonage[ 2], GET_PERM_ACCURACY ( ch ),
	           SUNNY ? sunnage[ 3] : moonage[ 3], GET_PERM_EVASION ( ch ),
	           SUNNY ? sunnage[ 4] : moonage[ 4],
	           SUNNY ? sunnage[ 5] : moonage[ 5], hit_gain ( ch ), mana_gain ( ch ), move_gain ( ch ), stamina_gain ( ch ),
	           SUNNY ? sunnage[ 6] : moonage[ 6],
	           SUNNY ? sunnage[ 7] : moonage[ 7], chance_hit_part ( ch, PART_HEAD ),
	           SUNNY ? sunnage[ 8] : moonage[ 8], chance_hit_part ( ch, PART_LEFT_ARM ),
	           SUNNY ? sunnage[ 9] : moonage[ 9], chance_hit_part ( ch, PART_RIGHT_ARM ),
	           SUNNY ? sunnage[10] : moonage[10], chance_hit_part ( ch, PART_TORSO ),
	           SUNNY ? sunnage[11] : moonage[11], chance_hit_part ( ch, PART_LEFT_LEG ),
	           SUNNY ? sunnage[12] : moonage[12], chance_hit_part ( ch, PART_LEFT_LEG ),
	           SUNNY ? sunnage[13] : moonage[13], GET_COOLNESS ( ch ),
	           SUNNY ? sunnage[14] : moonage[14], update_award ( ch ), *ethos,
	           SUNNY ? sunnage[14] : moonage[14], GET_STAMINA ( ch ), GET_MAX_STAMINA ( ch ), *detector,
                   SUNNY ? sunnage[14] : moonage[14], buf,
	           SUNNY ? sunnage[14] : moonage[14], print_elemental ( GET_CLASS ( ch ), TRUE, buf1, sizeof ( buf1 ) ),
	           SUNNY ? sunnage[14] : moonage[14], print_elemental ( GET_CLASS ( ch ), FALSE, buf2, sizeof ( buf2 ) ) );

}

int frozen_time ( Character *ch )
{
	struct affected_type *aff;
	if ( ch->affected )
	{
		for ( aff = ch->affected; aff; aff = aff->next )
		{
			if ( aff->type == SPELL_IMMFREEZE )
				return time_to_sec ( aff->expire + 1 );

		}
	}
	return -1;
}

ACMD ( do_affects )
{
	struct affected_type *aff;
	int i = FALSE;
	int minsec = 0;

	if ( IS_NPC ( ch ) )
		return;

	/* Routine to show what spells a char is affected by */
	if ( ch->affected )
	{
		i = TRUE;
		for ( aff = ch->affected; aff; aff = aff->next )
		{
			if ( aff->expire == -2 )
				ch->Send ( "SPL: (  {cPinnate{c0  )  %s%-21s%s ",
				           CCCYN ( ch, C_NRM ), skill_name ( aff->type ),
				           CCNRM ( ch, C_NRM ) );
			else
			{
				if ( ( minsec = time_to_sec ( aff->expire + 1 ) ) <= 0 )
					continue;
				ch->Send ( "SPL: ({cy%-6d{c0 %s)  %s%-21s%s ",
				           ( minsec > 60 ? minsec/60 : minsec ),
				           ( minsec/60 ?   "min"   : "sec" ),
				           CCCYN ( ch, C_NRM ), skill_name ( aff->type ), CCNRM ( ch, C_NRM ) );
			}
			if ( aff->modifier && aff->location )
			{
				ch->Send ( " %+d to %s", aff->modifier,
				           apply_types[ ( int ) aff->location] );
			}
			if ( aff->bitvector )
			{
				if ( aff->modifier && aff->location )
					ch->Send ( ", sets " );
				else
					ch->Send ( " sets " );
				ch->Send ( "%s", affected_bits[aff->bitvector] );
			}
			ch->Send ( "\r\n" );
		}
	}
	if ( !i )
		ch->Send ( "You are not affected by anything!\r\n" );
}





/*group functions*/
int group_size ( Character *ch )
{
	Character *k;
	struct follow_type *f;
	int i = 1;

	k = ( ch->master ? ch->master : ch );

	for ( f = k->followers; f; f = f->next )
		if ( ( k != f->follower ) )
			i++;

	return i;

}

void total_perc ( Character *ch )
{

	Character *k;
	struct follow_type *f;
	float temp = 0.0, temp2 = 0.0;
	int i = 1;

	//log("calling total_perc for %s", GET_NAME(ch));

	k = ( ch->master ? ch->master : ch );
	temp = GET_PERC ( k );

	for ( f = k->followers; f; f = f->next )
		if ( ( k != f->follower ) )
		{
			i++;
			temp += GET_PERC ( f->follower );
		}
	if ( temp > 99.9 && temp < 101.0 )
		return;

	if ( temp < 99.9 )
	{
		GET_PERC ( k ) += ( 100.0 - temp );
		total_perc ( k );
	}
	else
	{

		temp2 = ( 100.0 / ( float ) i );
		GET_PERC ( k ) = temp2;
		for ( f = k->followers; f; f = f->next )
			GET_PERC ( f->follower ) = temp2;
	}


	return;
}

Character *rand_group ( Character *ch )
{

	Character *k;
	struct follow_type *f;
	float rand, max, temp;

	k = ( ch->master ? ch->master : ch );
	rand = ( ( float ) number ( 1, 1000 ) / 10 );
	temp = GET_PERC ( k );

	if ( rand <= temp )
		return k;
	else
	{
		max = temp;
		for ( f = k->followers; f; f = f->next )
			if ( ( k != f->follower ) )
			{
				max += GET_PERC ( f->follower );
				if ( rand > temp && rand <= max )
					return f->follower;
				temp = max;
			}
	}

	return k;
}


int even_group ( Character *ch )
{

	Character *k;
	struct follow_type *f;
	float share;


	k = ( ch->master ? ch->master : ch );
	share = ( 1000.0 / group_size ( k ) ) /10.0;


	for ( f = k->followers; f; f = f->next )
		GET_PERC ( f->follower ) = share;

	GET_PERC ( k ) = share;

	return ( int ) share;

}


ACMD ( set_perc )
{
	Character *tch;
	//struct follow_type *f;
	char buf[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];
	int amount = 0, temp;
	/*TODO: change this to handle mutliple args for group members name */
	
	ch->Send("No need to use this command at the moment, all group members will be evenly involved.\r\n");
	return;	

	two_arguments ( argument, buf, buf2 );
	if ( !*buf )
	{
		ch->Send (
		    "involve <groupmember> <amount (1 to %3d)>\r\n"
		    "To split involvement evenly: \r\ninvolve even\r\n",
		    90 - group_size ( ch ) );
		return;
	}

	if ( isname ( buf, "even split auto" ) )
	{
		even_group ( ch );
		ch->Send ( "You split the involvement evenly.\r\n" );
		return;
	}
	else if ( is_number ( buf2 ) )
		amount = atoi ( buf2 );
	else
	{
		ch->Send ( "involve <groupmember> <amount (1 to %3d)>\r\n", 90 - group_size ( ch ) );
		return;
	}

	if ( amount < 0 || amount > ( 90 - group_size ( ch ) ) )
	{
		ch->Send ( "%d is not a valid involvement amount.\r\n", amount );
		return;
	}



	if ( ! ( tch = get_char_vis ( ch, buf, NULL, FIND_CHAR_ROOM ) ) )
	{
		ch->Send ( "There is no such person!\r\n" );
		return;
	}
	if ( tch->master != ch )
	{
		ch->Send ( "That person is not following you!\r\n" );
		return;
	}

	if ( amount > ( GET_PERC ( ch ) + GET_PERC ( tch ) )-10 )
	{
		ch->Send (
		    "That amount is higher than you have available to set for %s (%4.1f max).\r\n",
		    GET_NAME ( tch ), GET_PERC ( ch ) + GET_PERC ( tch ) - 10 );
		return;
	}
	temp = ( int ) GET_PERC ( tch );
	GET_PERC ( ch ) += ( float ) temp;
	GET_PERC ( tch ) = ( float ) amount;
	GET_PERC ( ch ) -= ( float ) amount;
	total_perc ( ch );

	ch->Send ( "You set %s's involvement to %4.1f%%, your involvement is now %4.1f%%.\r\n",
	           GET_NAME ( tch ), GET_PERC ( tch ), GET_PERC ( ch ) );
	tch->Send ( "%s sets your involvement to %4.1f%%.\r\n", GET_NAME ( ch ), GET_PERC ( tch ) );

}

/*
molly:$1$XmCBLRQX$ut53Cto7qi/4bs.KIWhMy/
*/
/*
void do_auto_exits(Character *ch)
{
    int door, slen = 0;

    ch->Send( "%s[ Exits: %s", CBGRN(ch, C_NRM),
               CBWHT(ch, C_NRM));

    for (door = 0; door < NUM_OF_DIRS; door++)
     if (EXIT(ch, door) && EXIT(ch, door)->to_room != NULL &&
         !IS_SET(EXIT(ch, door)->exit_info, EX_HIDDEN)) {
         if (EXIT_FLAGGED(EXIT(ch, door), EX_CLOSED)) {
         if (GET_LEVEL(ch) > LVL_IMMORT){
          ch->Send( "{cg%c {cW", UPPER(*dirs[door]));
          slen++;
          }
         } else {
             ch->Send( "%c ", LOWER(*dirs[door]));
          slen++;
          }


     }
     if (!slen)
     ch->Send( "None!");

    ch->Send( "%s]%s\r\n", CBGRN(ch, C_NRM),  CCNRM(ch, C_NRM));
}
*/
void fill_exit_list ( room_rnum list[], Character *ch )
{
	int door;
	for ( door = 0; door < NUM_OF_DIRS; door++ )
	{
		if ( EXIT ( ch, door ) && EXIT ( ch, door )->to_room != NULL &&
		        ! ( EXIT_FLAGGED ( EXIT ( ch, door ), EX_HIDDEN ) ) &&
		        ! ( EXIT_FLAGGED ( EXIT ( ch, door ), EX_CLOSED ) ) )
			list[door] = EXIT ( ch, door )->to_room;
		else
			list[door] = NULL;
	}
}

void display_map ( Character *ch )
{
	room_rnum rmv[6];


	fill_exit_list ( rmv, ch );

	/**
	    [N] [U]
	     | /
	[W]-[*]-[E]
	   / |
	[D] [S]
	 
	TODO: change it to be this one

	           |
	        --[N]--
	      |    | [U]|
	    -[W]--[*]--[E]-
	      |[D] |    |
	        --[S]--
	           |
	    
	    **/


	ch->Send (
	    "\r\n{cC     %3s {cc%3s   {c0\r\n",
	    ( rmv[NORTH] != NULL ) ? "[N]" : "   ",
	    ( rmv[UP] != NULL ) ? "[U]" : "   " );
	ch->Send (
	    "{cy      %1s %1s      {c0\r\n",
	    ( rmv[NORTH] != NULL ) ? "|" : " ",
	    ( rmv[UP] != NULL ) ? "/" : " " );
	ch->Send (
	    "{cC %4s{cy[{cR*{cy]%4s   {c0\r\n",
	    ( rmv[WEST] != NULL ) ? "[W]{cy-" : "    ",
	    ( rmv[EAST] != NULL ) ? "-{cC[E]" : "    " );
	ch->Send (
	    "{cy    %1s %1s        {c0\r\n",
	    ( rmv[DOWN] != NULL ) ? "/" : " ",
	    ( rmv[SOUTH] != NULL ) ? "|" : " " );
	ch->Send (
	    "{cc %3s{cC %3s       {c0\r\n",
	    ( rmv[DOWN] != NULL ) ? "[D]" : "   ",
	    ( rmv[SOUTH] != NULL ) ? "[S]" : "   " );

}

ACMD ( do_ipsearch ) {}

void container_disp ( Character *ch,OBJ_DATA * obj )
{

	int max, percent = 0;
	obj_data* temp;
	if ( GET_OBJ_TYPE ( obj ) == ITEM_CONTAINER )
	{
		for ( temp = obj->contains; temp; temp = temp->next_content )
		{
			percent += GET_OBJ_WEIGHT ( temp );
		}
		max = GET_OBJ_VAL ( obj, 0 );
	}
	else
	{
		percent = GET_OBJ_VAL ( obj, 1 );
		max = GET_OBJ_VAL ( obj, 0 );
	}
	if ( max == 0 )
		return;
	if ( max >= percent )
	{
		percent = ( int ) ( ( percent*100.0f ) / ( float ) max );
		ch->Send ( "%3d%% - max %d\r\n", percent, max );		
	}

}
/*
ACMD(do_compresstest) {
	long id;
	char *ret;
	char *test = "The C++ strings library provides the definitions of the basic_string class,\r\n which is a class template specifically designed to manipulate strings of characters\r\n of any character type. It also include two specific instantiations:\r\n string and wstring, which respectively use char and wchar_t as character types.\r\n";
	ch->Send("Start: ");
	ch->Send(test);
	id = compressor.CompressToId(test);
	ret = compressor.InflateFromId(id);
	ch->Send("End: ");
	ch->Send(ret);
	free(ret);
}
*/
