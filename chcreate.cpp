#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "comm.h"
#include "interpreter.h"
#include "db.h"
#include "utils.h"
#include "spells.h"
#include "handler.h"
#include "mail.h"
#include "screen.h"
#include "dg_scripts.h"
#include "clan.h"
#include "constants.h"
#include "genolc.h"
#include "oasis.h"
#include "tedit.h"
#include "improved-edit.h"
#include "descriptor.h"

/* local */
void con_display_spec ( Descriptor *d );
void con_display_class ( Descriptor *d );
void con_display_class_group ( Descriptor *d );
void con_display_race ( Descriptor *d );
void con_display_stats ( Descriptor *d );
void con_display_sex ( Descriptor *d, int valid );
void con_display_ansi ( Descriptor *d );
void con_display_new_here ( Descriptor *d );
void explain_stats ( Descriptor *d );
void con_display_email ( Descriptor *d );
void line_sep ( Descriptor *d );
void class_selection ( Character *ch );
void stats_selection ( Character *ch );
void QueryClassInfo ( Descriptor *d, char * arg );
/*external*/
extern const char *class_menu;
extern const char *class_group_menu;
extern const char *race_menu;
extern const char *pc_race_types[];
extern const char *pc_class_types[];
extern const char *pc_class_group_types[];
extern char *motd;

void Crash_rentsave ( Character *ch, int cost );
int enter_player_game ( Descriptor *d );
int parse_class ( char arg );
int parse_class_group ( char arg );
int parse_race ( char arg );
void readvanceplayer ( Character *ch );


ACMD ( do_help );
#define CLASSES 	0
#define ROGUE 		1
#define CASTER 		2
#define FIGHTER 	3

void con_character_creation ( Descriptor *d, char *arg )
{

	char race_selection[256];
	char class_selection[256];
	int load_result;

	void set_race ( Character *ch, int race );

	switch ( SUB_STATE ( d ) )
	{

		case STATE_ANSI:
			switch ( *arg )
			{
				case 'y':
				case 'Y':
					SET_BIT_AR ( PRF_FLAGS ( d->character ), PRF_COLOUR_1 );
					SET_BIT_AR ( PRF_FLAGS ( d->character ), PRF_COLOUR_2 );
					d->Output ( "\r\n{cRColor on.{c0\r\n" );
					break;
				case 'n':
				case 'N':
					d->Output ( "\r\nColor off.\r\n" );
					break;
				default:
					con_display_ansi ( d );
					return;
			}
			con_display_new_here ( d );
			break;
		case STATE_NEW_HERE:
			switch ( *arg )
			{
				case '1':
					GET_NEWBIE_STATUS ( d->character ) = NEWB_NEW;
					break;
				case '2':
					GET_NEWBIE_STATUS ( d->character ) = NEWB_4DNEW;
					break;
				case '3':
					GET_NEWBIE_STATUS ( d->character ) = NEWB_OLD;
					break;
				default:
					d->Output ( "\r\nInvalid answer sorry.\r\n" );
					con_display_new_here ( d );
					return;
					break;
			}
			con_display_sex ( d, TRUE );
			break;

		case STATE_QSEX:       /* query sex of new user         */
			switch ( *arg )
			{
				case 'm':
				case 'M':
					d->character->player.sex = SEX_MALE;
					break;
				case 'f':
				case 'F':
					d->character->player.sex = SEX_FEMALE;
					break;
#if 0

				case 'n':
				case 'N':
					d->character->player.sex = SEX_NEUTRAL;
					break;
#endif

				default:
					con_display_sex ( d, FALSE );
					return;
			}

			con_display_class_group ( d );
			break;



		case STATE_QRACE:
			load_result = parse_race ( *arg );
			GET_RACE ( d->character ) = load_result;
			if ( load_result == RACE_UNDEFINED )
			{
				d->Output ( "\r\nThat's not a race.\r\nRace: " );
				return;
			}
			strcpy ( race_selection, pc_race_types[load_result] );
			d->Output ( "\r\n" );

			do_help ( d->character, race_selection, 0, 0 );
			d->Output ( "\r\nAre you sure you want %s (Yes/No)? ", race_selection );
			SUB_STATE ( d ) = STATE_CONFIRM_QRACE;
			break;

		case STATE_CONFIRM_QRACE:
			if ( UPPER ( *arg ) == 'Y' )
			{
				void race_abils ( Character *ch );
				set_race ( d->character, GET_RACE ( d->character ) );

				d->character->real_abils.str_add = 0;
				d->character->real_abils.str = 15;
				d->character->real_abils.intel = 13 + ( IS_CASTER ( GET_CLASS ( d->character ) ) );
				d->character->real_abils.wis = 13;
				d->character->real_abils.dex = 13 + ( IS_ROGUE ( GET_CLASS ( d->character ) ) );
				d->character->real_abils.con = 13 + ( IS_FIGHTER ( GET_CLASS ( d->character ) ) );
				d->character->real_abils.cha = 13;
				CREATE_POINTS ( d->character ) = 10;
				race_abils ( d->character );
				d->character->aff_abils = d->character->real_abils;
				con_display_email ( d );

			}
			else
			{
				con_display_race ( d );
				return;
			}
			break;
		case STATE_QCLASSG:
			load_result = parse_class_group ( *arg );
			GET_CLASS ( d->character ) = load_result;

			if ( load_result == CLASS_UNDEFINED )
			{
				d->Output ( "\r\nThat's not a class.\r\nClass: " );
				return;
			}
			else
			{
				strcpy ( class_selection, pc_class_group_types[load_result] );
				d->Output ( "\r\n" );
			}
			QueryClassInfo ( d, class_selection );
			d->Output ( "\r\nAre you sure you want to be a %s (Yes/No)? ", class_selection );
			SUB_STATE ( d ) = STATE_CONFIRM_QCLASSG;
			break;

		case STATE_CONFIRM_QCLASSG:
			if ( UPPER ( *arg ) == 'Y' )
			{
				con_display_race ( d );
				break;
			}
			else
			{
				con_display_class_group ( d );
				break;
			}
			break;
#if defined(LOYAL)
		case STATE_QSPEC:

//con_display_spec(d);
			break;

		case STATE_LOYAL:
			if ( UPPER ( *arg ) == 'A' )
				improve_sub ( d->character, SUB_LOYALSPEED, 100 );
			else if ( UPPER ( *arg ) == 'B' )
				improve_sub ( d->character, SUB_LOYALATTACK, 100 );
			else  if ( UPPER ( *arg ) == 'C' )
				improve_sub ( d->character, SUB_LOYALDEFEND, 100 );
			else if ( UPPER ( *arg ) == 'D' )
				improve_sub ( d->character, SUB_LOYALDAMAGE, 100 );
			else
			{
				con_display_spec ( d );
				return;
			}
			con_display_email ( d );
			break;
#endif
		case STATE_EMAIL:
			if ( !arg || !*arg )
			{
				GET_EMAIL ( d->character ) = strdup ( "none" );
			}
			else
			{
				GET_EMAIL ( d->character ) = strdup ( arg );
			}

			if ( !pi.NameExists ( GET_PC_NAME ( d->character ) ) )
				pi.create_entry ( GET_PC_NAME ( d->character ) );
			SET_BIT_AR ( PLR_FLAGS ( d->character ), PLR_NEEDS_CLASS );
			SET_BIT_AR ( PLR_FLAGS ( d->character ), PLR_NEEDS_STATS );
			d->character->init();
			d->character->save();
			pi.Save();
			line_sep ( d );
			d->Output ( "%s", motd );
			d->Output ( "\r\n*** PRESS RETURN: " );
			STATE ( d ) = CON_RMOTD;

			new_mudlog ( NRM, LVL_GOD, TRUE, "%s [%s] new player.", GET_NAME ( d->character ), d->host.c_str() );
			//added this for test - Prometheus
			//new_mudlog ( NRM, PLR_NEWBIE_HLPR, TRUE, "%s [%s] new player.", GET_NAME( d->character ));
			break;
			/** These two, Stats and Class are now outside of the basic char creation process **/
		case STATE_QCLASS:
			load_result = parse_class ( *arg );
			GET_CLASS ( d->character ) = load_result;

			if ( load_result == CLASS_UNDEFINED )
			{
				d->Output ( "\r\nThat's not a class.\r\nClass: " );
				return;
			}
			else
			{
				strcpy ( class_selection, pc_class_types[load_result] );
				d->Output ( "\r\n" );
			}
			do_help ( d->character, class_selection, 0, 0 );
			d->Output ( "\r\nAre you sure you want to be a %s (Yes/No)? ", class_selection );
			SUB_STATE ( d ) = STATE_CONFIRM_QCLASS;
			break;

		case STATE_CONFIRM_QCLASS:
			if ( UPPER ( *arg ) == 'Y' )
			{
				REMOVE_BIT_AR ( PLR_FLAGS ( d->character ), PLR_NEEDS_CLASS );
				d->character->save();
				enter_player_game ( d );
				new_mudlog ( NRM, LVL_GOD, TRUE, "%s has just chosen to be class %s.", GET_NAME ( d->character ), class_selection );

			}
			else
			{
				con_display_class ( d );
				break;
			}
			break;
		case STATE_CONFIRM_STATS:
			if ( compares ( "DONE", arg ) )
			{
				REMOVE_BIT_AR ( PLR_FLAGS ( d->character ), PLR_NEEDS_STATS );
				enter_player_game ( d );
				readvanceplayer ( d->character );
				d->character->save();
				new_mudlog ( NRM, LVL_GOD, TRUE, "%s has just chosen their stats.", GET_NAME ( d->character ) );
			}
			else if ( compares ( "INFO", arg ) )
			{
				explain_stats ( d );
			}
			else
			{
				if ( *arg )
				{
					char abil[MAX_INPUT_LENGTH], *amou;
					amou = one_argument ( arg, abil );
					skip_spaces ( &amou );
					if ( ! ( *abil && amou && *amou && choose_real_abils ( d->character, *abil, atoi ( amou ) ) ) )
						d->Output ( "Invalid input: type DONE when finished, or the stat letter and an amount!\r\n" );

					d->character->aff_abils = d->character->real_abils;
				}
				con_display_stats ( d );
				return;
			}
			break;
	}

}

void con_display_class ( Descriptor *d )
{
	line_sep ( d );
	d->Output ( "\r\n%s\r\n{cyClass: {c0", class_menu );
	SUB_STATE ( d ) = STATE_QCLASS;
}
void con_display_class_group ( Descriptor *d )
{
	line_sep ( d );
	QueryClassInfo ( d, CLASSES );
	d->Output ( "\r\n%s\r\n{cyClass: {c0", class_group_menu );
	SUB_STATE ( d ) = STATE_QCLASSG;
}

void con_display_race ( Descriptor *d )
{
	line_sep ( d );
	d->Output ( "\r\n%s\r\n{cyRace: {c0",race_menu );
	SUB_STATE ( d ) = STATE_QRACE;
}
void explain_stats ( Descriptor *d )
{
	line_sep ( d );
	d->Output (
	    "{cyThe stats are as followed:{c0\r\n\r\n"
	    "{cgWisdom (WIS){cw\r\n"
	    "Gets you more practice sessions and mana when you level, and adds to magic damage.\r\n"
	    "Increases the chance of becoming better at a skill/spell through practice.\r\n\r\n{c0"
	    "{cgConstitution (CON){cw\r\n"
	    "Increases the max health gained when you level and health regeneration rate.\r\n"
	    "For fighters and rogues it increases the damage of their weapons.{c0\r\n\r\n"
	    "{cgStrength (STR){cw\r\n"
	    "Adds to your max carriable weight, and your speed.\r\n"
	    "For fighters and rogues it increases the damage of their weapons.{c0\r\n\r\n"
	    "{cgDexterity (DEX){cw\r\n"
	    "Increases attack rating and speed and armor class, lets you carry more items.\r\n"
	    "Dex is also good for thief skills, like picking locks and back stabbing.{c0\r\n\r\n"
	    "{cgIntelligence (INT){cw\r\n"
	    "Greater gain from practice sessions and gives you more mana when you level.\r\n"
	    "Increases the chance of becoming better at a skill/spell through practice.\r\n"
	    "Is the primary affect for increasing damage of all attack spells{c0\r\n\r\n"
	    "{cgCharisma (CHA){cw\r\n"
	    "Increases your minimum magic damage, extends the duration of spells,\r\n"
	    "lowers cost of items in shops, and increases max health gained when leveling.\r\n\r\n{c0"
	    "{cy------------When ready hit enter------------{c0 \r\n\r\n" );
	SUB_STATE ( d ) = STATE_CONFIRM_STATS;
}
void con_display_stats ( Descriptor *d )
{
	line_sep ( d );
	d->Output (
	    "{ccHere you can add extra points to your stats.\r\n\r\n"

	    "Within the game equipment is also used to increase these stats further.\r\n\r\n"

	    "You can also take points from a stat till it reaches 0.{c0\r\n"
	    "22 is the max usable amount in all stats except charisma, which is 100.\r\n"

	    "\r\n    {cy==================\r\n"
	    "      Points left: {cC%-2d{cy  -- Give your character some stats.\r\n"
	    "    ------------------\r\n"
	    "    {cg({cGS{cg)trength    : {cc%-2d{c0\r\n"
	    "    {cg({cGI{cg)nteligence : {cc%-2d{c0\r\n"
	    "    {cg({cGW{cg)isdom      : {cc%-2d{c0\r\n"
	    "    {cgc({cGO{cg)nstitution: {cc%-2d{c0\r\n"
	    "    {cg({cGD{cg)exterity   : {cc%-2d{c0\r\n"
	    "    {cg({cGC{cg)harisma    : {cc%-2d{c0\r\n"
	    "    {cy------------------\r\n"
	    "Type a letter then an amount. eg: S 3 or C -2 :\r\n"
	    "To see the info on stats again type INFO\r\n"
	    "When finished type DONE:  {c0",
	    CREATE_POINTS ( d->character ),
	    GET_STR ( d->character ), GET_INT ( d->character ), GET_WIS ( d->character ),
	    GET_CON ( d->character ), GET_DEX ( d->character ),  GET_CHA ( d->character ) );

	SUB_STATE ( d ) = STATE_CONFIRM_STATS;
}
#if defined(LOYAL)
void con_display_spec ( Descriptor *d )
{
	line_sep ( d );
	d->Output ( "\r\n"
	            "{cyChoose your starting bonus Subskill:\r\n\r\n"
	            "{cg[{cGA{cg] Speed -- {cP+50 to speed{c0\r\n\r\n"
	            "{cg[{cGB{cg] Attack - {cP+50 to attack rating{c0\r\n\r\n"
	            "{cg[{cGC{cg] Evade  - {cP+50 to evasion rating{c0\r\n\r\n"
	            "{cg[{cGD{cg] Damage - {cP+15%% to damage{c0\r\n\r\n"
	            "{cyPlease choose a letter: {c0" );
	SUB_STATE ( d ) = STATE_LOYAL;
}
#endif
void con_display_sex ( Descriptor *d, int valid )
{
	line_sep ( d );
	if ( !valid )
		d->Output ( "\r\nThat is not a valid sex...\r\n" );
	d->Output ( "\r\n{cyWhat is your sex {cGM{cgale{cy or {cGF{cgemale{cy?{c0 " );
	SUB_STATE ( d ) = STATE_QSEX;
}
void con_display_ansi ( Descriptor *d )
{
	line_sep ( d );
	d->Output ( "\r\nDoes your mudclient support colour (Y/N)? " );
	SUB_STATE ( d ) = STATE_ANSI;
}

void con_display_new_here ( Descriptor *d )
{
	line_sep ( d );
	d->Output ( "\r\n"
	            "  {cyPlease select from the following:{c0\r\n\r\n"
	            "  {cG1 {cc- I am new to mudding.\r\n"
	            "  {cG2 {cc- I am familiar with mudding, but new to 4Dimensions.\r\n"
	            "  {cG3 {cc- I am an old hand at 4Dimensions.{c0\r\n\r\n"
	            "  Number:" );
	SUB_STATE ( d ) = STATE_NEW_HERE;
}
void con_display_email ( Descriptor *d )
{
	line_sep ( d );
	d->Output ( "\r\n"
	            "  {cWGiving your email is optional, but it helps us provide better service\r\n"
	            "  and security. Your email if given means you will get a notification should\r\n"
	            "  anything happen with the game server (new address, extended downtime, etc)\r\n"
	            "  If you lose your password, having a valid email address is the only way to\r\n"
	            "  get it back. Your details won't be given to any 3rd parties, and is for use\r\n"
	            "  within this game only.\r\n\r\n"
	            "  (type {ccnone{cW if you do not wish to give your email)\r\n\r\n"
	            "  {cyWhat is your email address? {c0" );
	SUB_STATE ( d ) = STATE_EMAIL;
}

void line_sep ( Descriptor *d )
{
	d->Output (
	    "[H[J"
	    "\r\n"
	    "----------------------------------------------------------------------"
	    "\r\n"
	);
}

/** This command allows for new players to manually choose their class, and their stats.
They have to choose their class first, and then their stats after.
They have the ability to use the choose command from any level,
but they are not told about it until level 5.

They have to choose a class and stats by level 20 or they cannot progress any further.

When they complete the stat choice, they will have their Move, Mana and HP all set to that of a level one player.
They will then be advanced up through the levels automatically to the same level they are on now.
The player won't see any messages that all these things just happened.
**/

ACMD ( do_choose )
{
	skip_spaces ( &argument );
	if ( isname ( argument, "class" ) )
	{
		class_selection ( ch );
	}
	else if ( isname ( argument, "stats" ) )
	{
		stats_selection ( ch );
	}
	else
	{
		ch->Send ( "You can type:\r\n{cGchoose class{c0 or {cGchoose stats{c0\r\n" );
	}
}

void class_selection ( Character *ch )
{
	Descriptor *d = ch->desc;
	if ( !PLR_FLAGGED ( ch, PLR_NEEDS_CLASS ) )
	{
		ch->Send ( "You have already selected your class.\r\n" );
		return;
	}
	Crash_rentsave ( ch, 0 );
	GET_LOADROOM ( ch ) = GET_ROOM_VNUM ( IN_ROOM ( ch ) );

	extract_char ( ch, 1 );     /* Char is saved before extracting. */
	/** Inject the user back into the logon process
	    They will need to come back to the same room
	    they are in when they have finished choosing stats  - Mord**/
	con_display_class ( d );
	STATE ( d ) = CON_CREATE_NEW;
}
void stats_selection ( Character *ch )
{
	Descriptor *d = ch->desc;
	if ( PLR_FLAGGED ( ch, PLR_NEEDS_CLASS ) )
	{
		ch->Send ( "You have to choose your class first.\r\n" );
		return;
	}
	if ( !PLR_FLAGGED ( ch, PLR_NEEDS_STATS ) )
	{
		ch->Send ( "You have already selected your stats.\r\n" );
		return;
	}
	Crash_rentsave ( ch, 0 );
	GET_LOADROOM ( ch ) = GET_ROOM_VNUM ( IN_ROOM ( ch ) );

	extract_char ( ch, 1 );      /* Char is saved before extracting. */
	/** Inject the user back into the logon process
	    They will need to come back to the same room
	    they are in when they have finished choosing stats  - Mord**/
	explain_stats ( d );
	STATE ( d ) = CON_CREATE_NEW;

}

void ShowClassInfo ( Descriptor *d, int typ )
{
	switch ( typ )
	{
		case CLASSES:
			d->Output ( "There are 8 classes in the Mud; \r\n" );
			d->Output ( "Mage, Esper, Gypsy, Hunter, Warrior, Thief, Priest, Ranger.\r\n\r\n" );
			d->Output ( "Roughly they can be divided into three subtypes:\r\n" );
			d->Output ( "Rogue Classes           =  Gypsy, Thief, Ranger\r\n" );
			d->Output ( "Spell Casting Classes   =  Esper, Priest, Mage\r\n" );
			d->Output ( "Fighter Classes         =  Hunter, Warrior\r\n\r\n" );
			d->Output ( "Here you can choose a subtype to play as until level 20.\r\n" );
			d->Output ( "At that point you can choose the specific class you wish to be.\r\n" );
			break;
		case ROGUE:
			d->Output ( "The ROGUE CLASSES, get a slight speed bonus when they use the \r\n" );
			d->Output ( "lighter weapon types, like stabbing or piercing weapons. They \r\n" );
			d->Output ( "get good damage bonuses when they use skills to attack with, \r\n" );
			d->Output ( "but in general combat rely mostly on avoiding attacks, to be \r\n" );
			d->Output ( "able to whittle down opponents. They are very good as tanks \r\n" );
			d->Output ( "because of their low chance of being hit.\r\n" );
			d->Output ( "Ranger/Thief/Gypsy classes will like +dex +move +str items.\r\n" );
			break;
		case CASTER:
			d->Output ( "The SPELLCASTING CLASSES are lousy with a weapon, since \r\n" );
			d->Output ( "they need to concentrate and have both hands free to cast their \r\n" );
			d->Output ( "spells. If they are bare handed, they have a default magic attack, \r\n" );
			d->Output ( "which depending on their level, tier, and also some skills or \r\n" );
			d->Output ( "equipment can become faster and more powerful.\r\n" );
			d->Output ( "They also use ORBS and STAVES to enhance their magic. \r\n" );
			d->Output ( "The skill MANIFEST creates an ORB from an existing weapon. \r\n" );
			d->Output ( "A Staff, which is more powerful, has to be created by the \r\n" );
			d->Output ( "skill WOODSING  but not by the mages themselves.\r\n" );
			d->Output ( "Mage/Priest/Esper classes will like +int +cha +hp items.\r\n" );
			break;
		case FIGHTER:
			d->Output ( "The FIGHTER CLASSES have overall good defence, and focus primarily on melee\r\n" );
			d->Output ( "damage, where almost every skill they have goes towards increasing their\r\n" );
			d->Output ( "overall damage and battle ability, rather then a sudden burst of damage\r\n" );
			d->Output ( "or speed.\r\n" );
			d->Output ( "Hunter/Warrior classes will like +damroll +hp +str items.\r\n" );
			break;
	}
}
void QueryClassInfo ( Descriptor *d, char * arg )
{
	if ( !arg )
		ShowClassInfo ( d, CLASSES );
	else if ( is_abbrev ( arg, "rogue" ) )
		ShowClassInfo ( d, ROGUE );
	else if ( is_abbrev ( arg, "caster" ) )
		ShowClassInfo ( d, CASTER );
	else if ( is_abbrev ( arg, "fighter" ) )
		ShowClassInfo ( d, FIGHTER );
	else
		d->Output ( "That isn't a class: type rogue, caster or fighter\r\n" );
}

void readvanceplayer ( Character *ch )
{
	gold_int cur_exp = GET_EXP ( ch );

	GET_HIT ( ch ) 			= 25;
	GET_MAX_HIT ( ch ) 		= 25;
	GET_MANA ( ch ) 		= 100;
	GET_MAX_MANA ( ch ) 		= 100;
	GET_MOVE ( ch ) 		= 50;
	GET_MAX_MOVE ( ch ) 		= 50;
	GET_STAMINA ( ch ) 		= 100;
	GET_MAX_STAMINA ( ch ) 		= 100;
	GET_LEVEL ( ch ) 		= 1;
	GET_EXP(ch)			= 1;
	gain_exp_regardless ( ch, cur_exp , TRUE );
	GET_HIT ( ch ) 			= GET_MAX_HIT ( ch );
	GET_MOVE ( ch ) 		= GET_MAX_MOVE ( ch );
	GET_MANA ( ch ) 		= GET_MAX_MANA ( ch );
	GET_STAMINA ( ch ) 		= GET_MAX_STAMINA ( ch );
	ch->Send ( "{cMAll of your current levels have been refreshed as if you had done them with your current stats.{c0\r\n" );
}


