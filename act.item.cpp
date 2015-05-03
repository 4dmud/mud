/* ************************************************************************
*   File: act.item.c                                    Part of CircleMUD *
*  Usage: object handling routines -- get/drop and container handling     *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

/*
 * $Log: act.item.c,v $
 * Revision 1.61  2007/11/14 21:39:41  w4dimenscor
 * Added the Gladiator race for the gladiatorpits.
 * --Matthijs
 *
 * Revision 1.60  2007/11/14 09:37:16  w4dimenscor
 * Fixed unhitch failure to have a better message. Also fixed the donation room issue where the item would just dissapear
 *
 * Revision 1.59  2007/11/14 09:20:41  w4dimenscor
 * Fixed crash bug with hitch. Fixed crash bug with finger nonexistant player. and adding new players to the management screen
 *
 * Revision 1.58  2007/10/22 23:38:27  w4dimenscor
 * Fixed donate to do the right thing(tm).
 * --Matthijs
 *
 * Revision 1.57  2007/08/19 01:06:10  w4dimenscor
 * - Changed the playerindex to be a c++ object with search functions.
 * - changed the room descriptions to be searched from a MAP index, and
 * added Get and Set methods for room descriptions.
 * - changed the zone reset so that it doesnt search the entire object list
 * to find the object to PUT things into.
 * - rewrote other parts of the zone reset function, to make it give correct errors.
 * - rewrote the parts of the code to do with loading and searching for directorys and files.
 * - added a new dlib library.
 *
 * Revision 1.56  2007/06/26 10:48:04  w4dimenscor
 * Fixed context in scripts so that it works again, changed mounted combat so that it is about 2/3rds player one third mount damage, updated the way skills get read using total_chance, stopped things with a PERC of 0 assisting, made it so that the ungroup command disbanded charmies
 *
 * Revision 1.55  2007/06/14 23:55:39  w4dimenscor
 * Timers now work offline, keys can't be put in houses along with non-rent items. and the timers on items are run from the event system instead of 'ticks'
 *
 * Revision 1.54  2007/06/09 04:34:55  w4dimenscor
 * Fixed practice so that it takes money from players rather then gives them money, fixed a crash bug with furniture, added 'discard' as a junk alternitive, initialised the INTERNAL(ch) variable on Character objects
 *
 * Revision 1.53  2007/06/07 11:46:10  w4dimenscor
 * Updated the assemble code to give a good message that people can't assemble, which needs to be overridden in triggers, basically it is the same as 'do_not_here'
 *
 * Revision 1.52  2007/06/07 10:30:50  w4dimenscor
 * Added the ability for scripts to check assemblies for existance, and to create the products of assemblies
 *
 * Revision 1.51  2007/05/20 18:54:35  w4dimenscor
 * fixed beanbag pull crashbug.
 *
 * Revision 1.50  2007/03/03 22:42:27  w4dimenscor
 * added the get from container trigger.
 *
 * Revision 1.49  2007/03/01 23:48:55  w4dimenscor
 * Forgot to add %actor% in new trigger type. --Thotter
 *
 * Revision 1.48  2007/03/01 23:19:41  w4dimenscor
 * Added a new object trigger type for containers that checks wether something is being put into them. %object% refers to the object being put in. Also added %object.is_inobj% which returns the UID of the object it is in, or nothing if it isn't in a container. --Thotter
 *
 * Revision 1.47  2006/12/04 10:24:32  w4dimenscor
 * FIXED:take pie all would show up all undisplayed containers in the room.
 *
 * Revision 1.46  2006/08/13 06:26:50  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.45  2006/06/19 06:25:39  w4dimenscor
 * Changed the player saved mount feature so that all players can load mounts from houses
 *
 * Revision 1.44  2006/06/16 10:54:51  w4dimenscor
 * Moved several functions in fight.c into the Character object. Also removed all occurances of send_to_char from skills.c
 *
 * Revision 1.43  2006/06/11 10:10:11  w4dimenscor
 * Created the ability to use characters as a stream, so that you can do things like: *ch << "You have " << GET_HIT(ch) << "hp.\r\n";
 *
 * Revision 1.42  2006/05/30 09:14:19  w4dimenscor
 * rewrote the color code, process_output, and vwrite_to_output so that they use strings and have better buffer checks
 *
 * Revision 1.41  2006/05/22 10:50:48  w4dimenscor
 * Created 3 new files, mxp.cpp, mxp.h and descriptor.cpp
 * struct descriptor_data has been converted to class Descriptor
 *
 * Revision 1.40  2006/05/21 11:02:25  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.39  2006/05/15 16:16:12  w4dimenscor
 * Fixed the level you need to see undisplayed items. Some of it was equal to
 * LVL_IMMORT, some of it was higher then LVL_IMMORT. It is now all higher then
 * LVL_IMMORT. Fixed some other small things too.
 *
 * Revision 1.38  2006/05/15 15:45:19  w4dimenscor
 * the nodisplay flag prevents items from showing up with get all again. Imms can
 * still use get all to pick up a nodisplay item.
 *
 * Revision 1.37  2006/05/08 19:59:42  w4dimenscor
 * repatching the files sinc ethe backup wipe
 *
 * Revision 1.38  2006/05/01 23:33:08  w4dimenscor
 * Changed the fuel command, so that if you need less fuel to fill your spacebike then a gem provides, the bike will only use what it needs of the gem and not purge it
 *
 * Revision 1.37  2006/05/01 23:26:27  w4dimenscor
 * Fixed the skin command so that if the animals skin is 0 or less it means that it is unskinnable. Moved anif check in do_fuel
 *
 * Revision 1.36  2006/04/21 12:46:44  w4dimenscor
 * Fixed gcc 4.1 compile time errors. Game will now compile in GCC4
 *
 * Revision 1.35  2006/04/19 08:08:07  w4dimenscor
 * Added an \r\n that was missing in the fuel function.
 *
 * Revision 1.34  2006/04/18 21:48:54  w4dimenscor
 * Added the "amount of fuel" property to gemclusters.
 * Added a fuel command.
 * Added a GET_GEM_FUEL(obj) macro to get the amount of fuel from a gemstone.
 * Made the imm3 trust group and moved stat, syslog users and vstat there.
 * Fixed scan so that it doesn't scan through closed doors and doesn't throw you
 * off our spacebike.
 * Fixed a typo in the slay command.
 *
 * Revision 1.33  2006/02/24 20:09:02  w4dimenscor
 * * Fixed offline automeld so that if a player leaves their corpse and quits,
 *   it will still meld properly.
 * * Added the ability to see the stats of the object you are bidding on for 10pc
 *   of the current bid.
 * * Changed auction to check if people have enough money to complete the auction.
 * * Changed auction code to use the modular character gold commands.
 * * Fixed a bug in gold commands that returned the wrong value.
 *
 * Revision 1.32  2006/02/24 09:25:15  w4dimenscor
 * fixed bug in pouring that allowed you to create negative empty potions. Potions now change into object 3044 (empty vial), usable for brewing.
 *
 * Revision 1.31  2006/02/23 18:41:50  w4dimenscor
 * added a few needed files to cvs
 *
 * Revision 1.30  2006/02/17 22:19:54  w4dimenscor
 * Fixed error for ubuntu that doesnt like empty array declarations, moved ice shield to a better place and fixed its messages, added auto auction fixes, allowed mounts to gain exp properly
 *
 * Revision 1.29  2006/01/23 05:23:19  w4dimenscor
 * sorry self. another. _can't remember the changes_ entry
 *
 * Revision 1.28  2005/11/30 18:47:12  w4dimenscor
 * changed slightly some gains you get from remorts
 *
 * Revision 1.27  2005/11/19 06:18:38  w4dimenscor
 * Fixed many bugs, and added features
 *
 * Revision 1.26  2005/11/01 18:43:37  w4dimenscor
 * Tradepoints have been added to players and saved, compare command has been updated, the login accounts thing works again, and when you can't see your attacker your attacker you get half defense points
 *
 * Revision 1.25  2005/10/30 08:37:05  w4dimenscor
 * Updated compare command and fixed mining
 *
 * Revision 1.24  2005/10/23 13:53:30  w4dimenscor
 * Added thotters login/logout message concept
 *
 * Revision 1.23  2005/10/23 05:21:46  w4dimenscor
 * Altered assemblies, and fixed a few mem leaks
 *
 * Revision 1.22  2005/10/02 03:37:16  w4dimenscor
 * Removed prompt ends from the output because I am not diagnosing the client well enough to stop a misdisplay on some, also artifacts will no longer be auto owned.
 *
 * Revision 1.21  2005/09/24 08:52:33  w4dimenscor
 * finished the assemblies code
 *
 * Revision 1.20  2005/09/24 07:11:51  w4dimenscor
 * Added the ability to SKIN mobs, and the ability to add skin to mobs in olc, added ability to set what log a tree ill make and how many it will make
 *
 * Revision 1.19  2005/08/19 08:51:14  w4dimenscor
 * fixed the variables not working
 *
 * Revision 1.18  2005/08/14 02:27:13  w4dimenscor
 * added shiftable objects flag for the pull command, added to dg_variables ability to SET object values from variables, hopefully fixed issue where triggers would be removed from rooms after editing.
 *
 * Revision 1.17  2005/08/07 04:12:39  w4dimenscor
 * Manu changes and command have been made, sorry for the lack of description. Main changes include command landscape, fixes to helpfile stuff, subskill fixes
 *
 * Revision 1.16  2005/06/18 12:20:52  w4dimenscor
 * changed a bunch of send_to_char's to new_send_to_chars, adjusted some mxp code
 *
 * Revision 1.15  2005/05/28 05:52:14  w4dimenscor
 * Fixed some errors in copyover, added MXP
 *
 * Revision 1.14  2005/05/03 10:21:25  w4dimenscor
 * changed the free_string function to take a pointer to a pointer so it can nullk the string off properly now. Also, fixed a door loading error, that assumed that all door rooms existed when loading, and now it checks for existstance. Also, fixed the multi arg for 'get' command
 *
 * Revision 1.13  2005/05/01 12:31:07  w4dimenscor
 * added multi arg names to wear and remove
 *
 * Revision 1.12  2005/03/19 15:02:55  w4dimenscor
 * gave centaurs the innate skill mount and riding at 100 % also adjusted
 * damage and speed a little.
 *
 * Revision 1.11  2005/03/17 09:09:17  w4dimenscor
 * fixed a crash bug in the energize command
 *
 * Revision 1.10  2005/03/15 08:35:08  w4dimenscor
 * xml page update, and a few other bits
 *
 * Revision 1.9  2005/02/25 05:02:45  w4dimenscor
 * added new commands and a few little changes - i forget what eek
 *
 * Revision 1.8  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.7  2004/12/17 07:13:20  w4dimenscor
 * A few little updates.
 *
 * Revision 1.6  2004/12/07 09:31:26  w4dimenscor
 * Trees modularized, fix added to message event
 *
 * Revision 1.5  2004/12/05 09:46:51  w4dimenscor
 * fixed mtransform, fixed format in clan tell, and added limit on magic items carried, and lowered weight of magic items, and increased cost
 *
 * Revision 1.4  2004/11/27 20:16:46  w4dimenscor
 * fixed bug in 'get all all.corpse' that caused infinite loop, fixed up bug in combat where event wern't being canceled properly
 *
 * Revision 1.3  2004/11/20 20:16:51  w4dimenscor
 * removing olc.c and olc.h
 *
 * Revision 1.2  2004/11/20 02:33:25  w4dimenscor
 * updated and cleaned up the script system
 *
 * Revision 1.1.1.1  2004/11/12 02:16:46  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.56  2004/09/24 11:34:52  molly
 * fixed automeld
 *
 * Revision 1.55  2004/09/22 09:40:41  molly
 * automeld added so that corpses arent so easily lost, and also made pk corpses lootable
 *
 * Revision 1.54  2004/09/18 04:42:45  molly
 * cleared up some memory leaks again, possibly fixed the QIC miscounts
 *
 * Revision 1.50  2004/09/04 03:46:50  molly
 * made it so only one cost for recovering corpses, and skillist is sorted
 *
 * Revision 1.46  2004/08/22 01:31:16  molly
 * error in do_get where item not found returns a null pointer, but is not checked
 *
 * Revision 1.45  2004/08/18 14:02:25  malfestus
 * removed static int itemcounter. it's not used anymore
 *
 * Revision 1.44  2004/08/18 13:57:55  malfestus
 * Fixed ACMD(do_get) and ACMD(do_put)
 *  - some of the returned messages where wrong
 *  - reformated / changed the code
 *  - removed some not needed objects
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
#include "spells.h"
#include "constants.h"
#include "dg_scripts.h"
#include "screen.h"
#include "fight.h"
#include "assemblies.h"
#include "descriptor.h"
#include "dg_event.h"

/* extern variables */
extern bool LS_REMOVE;

/* External functions */

int invalid_class ( Character *ch, struct obj_data *obj );
int invalid_race ( Character *ch, struct obj_data *obj );
void remove_corpse_from_list ( OBJ_DATA *corpse );
void save_corpses ( void );
void House_crashsave ( room_vnum vnum );
void Crash_rentsave ( Character *ch, int cost );
int house_item_count ( room_vnum vnum );
int Crash_is_unrentable ( struct obj_data *obj );
void improve_skill ( Character *ch, int skill );
int has_weapon ( Character *ch );
int get_weapon_speed ( OBJ_DATA *wep );
int wep_hands ( OBJ_DATA *wep );
char *find_exdesc ( char *word, struct extra_descr_data *list );
int house_capacity ( room_vnum house );
int find_house ( room_vnum vnum );
void crumble_obj ( Character *ch, OBJ_DATA *obj );
int can_wear_on_pos ( struct obj_data *obj, int pos );
void perform_meld ( Character *ch, OBJ_DATA *corpse );
int count_magic ( struct obj_data *obj, Character *ch );
int is_magic ( OBJ_DATA *obj );

int check_potion_weight ( struct obj_data *obj );
C_FUNC ( push_object );
EVENTFUNC ( timer_event );

/* Local Variables */
int slipping = FALSE;
struct obj_data *obj_selling = NULL;    /* current object for sale */
Character *ch_selling = NULL;    /* current character selling obj */
Character *ch_buying = NULL;     /* current character buying the object */
static int wearall = 0;
/*gold tally*/
static long long shop_in = 0;
static long long shop_out = 0;
static long long dropped = 0;
static long long taken = 0;
static long long auction_in = 0;
static long long auction_out = 0;
static long long gold_given = 0;
static long long gold_received = 0;
static long long dg_in = 0;
static long long dg_out = 0;
/*speed functions*/
int speed_update ( Character *ch );
int class_speed ( Character *ch );
int race_speed ( Character *ch );
int alter_gold ( Character *ch, gold_int amount );
int where_to_worn ( int where );

/* local functions */
void update_timer ( struct obj_data *obj );
bool can_take_obj ( Character *ch, struct obj_data *obj );
bool perform_get_from_room ( Character *ch, struct obj_data *obj );
int perform_drop ( Character *ch, struct obj_data *obj, sbyte mode,
                   const char *sname, Room * RDR );
int find_eq_pos ( Character *ch, struct obj_data *obj, char *arg );
Character *give_find_vict ( Character *ch, char *arg );
void get_from_room ( Character *ch, char *arg, int amount );
void perform_give_gold ( Character *ch, Character *vict,
                         gold_int amount );
void perform_give ( Character *ch, Character *vict,
                    struct obj_data *obj );
void perform_drop_gold ( Character *ch, gold_int amount, sbyte mode,
                         Room *  RDR );
void get_check_money ( Character *ch, struct obj_data *obj );
void weight_change_object ( struct obj_data *obj, int weight );
bool perform_put ( Character *ch, struct obj_data *obj,
                   struct obj_data *cont );
void name_from_drinkcon ( struct obj_data *obj );
void get_from_container ( Character *ch, struct obj_data *cont,
                          int obj_dotmode, char *obj_desc, int mode, int amount );
void name_to_drinkcon ( struct obj_data *obj, int type );
void wear_message ( Character *ch, struct obj_data *obj, int where );
void perform_wear ( Character *ch, struct obj_data *obj, int where );
bool perform_get_from_container ( Character *ch, struct obj_data *obj,
                                  struct obj_data *cont, int mode );
void perform_remove ( Character *ch, int pos );
void auc_send_to_all ( char *messg, bool buyer );
void stop_auction ( int type, Character *ch );
void auc_stat ( Character *ch, struct obj_data *obj );
long long gold_data ( int type, long long amount );
ACMD ( do_assemble );
ACMD ( do_remove );
ACMD ( do_put );
ACMD ( do_get );
ACMD ( do_drop );
ACMD ( do_give );
ACMD ( do_drink );
ACMD ( do_eat );
ACMD ( do_pour );
ACMD ( do_wear );
ACMD ( do_wield );
ACMD ( do_grab );
ACMD ( do_fuel );

gold_int max_gold ( Character *ch );
int spill_gold_amount ( Character *ch );
void spill_gold ( Character *ch );

long long gold_data ( int type, long long amount )
{
	switch ( type )
	{
		case SHOP_IN:
			return shop_in += amount;
			break;
		case SHOP_OUT:
			return shop_out += amount;
			break;
		case DROPPED:
			return dropped += amount;
			break;
		case TAKEN:
			return taken += amount;
			break;
		case AUCTION_IN:
			return auction_in += amount;
			break;
		case AUCTION_OUT:
			return auction_out += amount;
			break;
		case GOLD_GIVEN:
			return gold_given += amount;
			break;
		case GOLD_RECEIVED:
			return gold_received += amount;
			break;
		case DG_GOLD_IN:
			return dg_in += amount;
			break;
		case DG_GOLD_OUT:
			return dg_out += amount;
			break;
	}
	return 0;
}

gold_int max_gold ( Character * ch )
{
	return ( GET_LEVEL ( ch ) * 5000000 );
}

int spill_gold_amount ( Character *ch )
{
	gold_int val;
	if ( ch->Gold ( 0, GOLD_HAND ) > max_gold ( ch ) )
	{
		val = ( ch->Gold ( 0, GOLD_HAND ) - max_gold ( ch ) );
		return val;
	}
	return ( -1 );
}

void spill_gold ( Character *ch )
{
	if ( spill_gold_amount ( ch ) <= 0 )
		return;

	//break this for now
	return;

	perform_drop_gold ( ch, spill_gold_amount ( ch ), SCMD_SPILL, 0 );
}

int getTypeFromSubskill ( int subskill )
{

	switch ( subskill )
	{
		case SUB_ASSEMBLE:
			return ASSM_ASSEMBLE;
		case SUB_BAKE:
			return ASSM_BAKE;
		case SUB_BREW:
			return ASSM_BREW;
		case SUB_CRAFT:
			return ASSM_CRAFT;
		case SUB_FLETCH:
			return ASSM_FLETCH;
		case SUB_KNIT:
			return ASSM_KNIT;
		case SUB_MAKE:
			return ASSM_MAKE;
		case SUB_MIX:
			return ASSM_MIX;
		case SUB_THATCH:
			return ASSM_THATCH;
		case SUB_WEAVE:
			return ASSM_WEAVE;
		case SUB_FORGE:
			return ASSM_FORGE;
			break;
	}
	return ASSM_ASSEMBLE;
}

ACMD ( do_assemble )
{
	long         lVnum = NOTHING;
	int type = getTypeFromSubskill ( subcmd );

	ch->Send ( "You either don't have all the parts you need to %s, or you can't do that here.\r\n", sub_name ( subcmd ) );
	return;
	/* the rest of this is not used at the moment, because it is all parsed by triggers - mord 7th June 2007*/
	skip_spaces ( &argument );

	if ( !get_sub ( ch, subcmd ) )
	{
		ch->Send ( "You know nothing of that art!\r\n" );
		return;
	}

	if ( *argument == '\0' )
	{
		ch->Send ( "What would you like to %s?\r\n", CMD_NAME );
		return;
	}
	else if ( ( lVnum = assemblyFindAssembly ( argument ) ) < 0 )
	{
		ch->Send ( "You can't %s %s %s.\r\n", CMD_NAME, AN ( argument ), argument );
		return;
	}
	else if ( assemblyGetType ( lVnum ) != ( type - 101 ) )
	{
		ch->Send ( "You can't %s %s %s.\r\n", CMD_NAME, AN ( argument ), argument );
		return;
	}
	else if ( !assemblyCheckComponents ( lVnum, ch, TRUE ) )
	{
		ch->Send ( "You haven't got all the things you need.\r\n" );
		return;
	}

	assemble_otrigger ( lVnum, ch, subcmd, cmd );

}


int perform_assemble ( obj_vnum lVnum, Character *ch, int subcmd, int cmd )
{
	int percent;
	struct obj_data *pObject = NULL;
	/* Create the assembled object. */
	if ( !assemblyCheckComponents ( lVnum, ch, FALSE ) )
	{
		ch->Send ( "You haven't got all the things you need.\r\n" );
		return 0;
	}
	if ( ( pObject = read_object ( lVnum, VIRTUAL ) ) == NULL )
	{
		ch->Send ( "You can't %s one of those.\r\n", CMD_NAME );
		return 0;
	}
	percent = number ( 1, 101 );

	if ( percent < 5 )
		improve_sub ( ch, ( enum subskill_list ) subcmd,1 );

	/* Now give the object to the character. */
	obj_to_char ( pObject, ch );
#if 0
	/* Tell the character they made something. */
	sprintf ( buf, "You %s $p.", CMD_NAME );
	act ( buf, FALSE, ch, pObject, NULL, TO_CHAR );

	/* Tell the room the character made something. */
	sprintf ( buf, "$n %ss $p.", CMD_NAME );
	act ( buf, FALSE, ch, pObject, NULL, TO_ROOM );
#endif

	return 1;
}

/*int assemble_otrigger(obj_vnum lVnum, Character *ch, int subcmd, int cmd) {return perform_assemble(lVnum, ch, subcmd, cmd);}*/

bool perform_put ( Character *ch, struct obj_data *obj, struct obj_data *cont )
{


	if ( drop_otrigger ( obj, ch ) <= 0 )
		return FALSE;

	if ( put_in_otrigger ( cont, obj, ch ) <= 0 )
		return FALSE;
	if ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_HOUSE ) && ( !cont || ( cont && IN_ROOM ( cont ) ) ) )
	{
		void count_items_in_list ( struct obj_data *obj, int& total_items );
		int value = house_item_count ( GET_ROOM_VNUM ( IN_ROOM ( ch ) ) );
		int plussage = 1;
		if ( GET_OBJ_TYPE ( obj ) == ITEM_CONTAINER )
			count_items_in_list ( obj->contains, plussage );
		int capa = house_capacity ( ( GET_ROOM_VNUM ( IN_ROOM ( ch ) ) ) );
		if ( value >= capa )
		{
			ch->Send ( "No matter how you try, you can't fit anything else in.[%d/%d]\r\n", value, capa );
			return FALSE;
		}
		else if ( ( value + plussage ) > capa )
		{
			ch->Send ( "That container's items would push your house capacity of %d over it's limit by %d.\r\n",capa, capa - ( value+plussage ) );
			return FALSE;
		}
	}
	/* Key hoarding isn't a problem anymore. Unrentable items may be saved as well.
	if ( ( Crash_is_unrentable ( obj ) && !IS_NPC ( ch ) ) || AFF_FLAGGED ( ch, AFF_CHARM ) )
	{
		ch->Send ( "You can't leave that here, it is too precious!\r\n" );
		return FALSE;
	}
	*/
	if ( IS_OBJ_STAT ( cont, ITEM_PC_CORPSE ) )
	{
		ch->Send ( "You can't put things in player corpses.\r\n" );
		return FALSE;
	}
	if ( IS_OBJ_STAT ( obj, ITEM_PC_CORPSE ) )
	{
		ch->Send ( "You can't put player corpses in things.\r\n" );
		return FALSE;
	}

	if ( ( GET_OBJ_TYPE ( cont ) == ITEM_CONTAINER )
	        && ( GET_OBJ_TYPE ( obj ) == ITEM_CONTAINER ) )
	{
		ch->Send ( "You can't put containers inside containers.\r\n" );
		return FALSE;
	}
	if ( GET_OBJ_WEIGHT ( cont ) + GET_OBJ_WEIGHT ( obj ) > GET_OBJ_VAL ( cont, 0 ) )
	{
		act ( "$p won't fit in $P.", FALSE, ch, obj, cont, TO_CHAR );
		return FALSE;
	}
	if ( GET_OBJ_VNUM ( obj ) >= 3300 && GET_OBJ_VNUM ( obj ) <= 3312 )
	{
		if ( cont->carried_by )
			new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s put %s into %s (inventory)",  GET_NAME ( ch ), obj->short_description, cont->short_description );
		else if ( IN_ROOM ( cont ) )
			new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s put %s into %s room %d",  GET_NAME ( ch ), obj->short_description,cont->short_description, GET_ROOM_VNUM ( IN_ROOM ( cont ) ) );
		else
			new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s put %s into %s",  GET_NAME ( ch ), obj->short_description, cont->short_description );
	}

	obj_from_char ( obj );
	obj_to_obj ( obj, cont );
	if ( !wearall && !slipping )
		act ( "$n puts $p in $P.", TRUE, ch, obj, cont, TO_ROOM );

	/* Yes, I realize this is strange until we have auto-equip on rent. -gg */
	if ( IS_OBJ_STAT ( obj, ITEM_NODROP ) && !IS_OBJ_STAT ( cont, ITEM_NODROP ) )
	{
		SET_BIT_AR ( GET_OBJ_EXTRA ( cont ), ITEM_NODROP );
		act ( "You get a strange feeling as you put $p in $P.", FALSE, ch, obj, cont, TO_CHAR );
	}
	else if ( !wearall || ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) ) )
	{
		act ( "You put $p in $P.", FALSE, ch, obj, cont, TO_CHAR );
	}
	if ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_HOUSE ) )
		SET_BIT_AR ( ROOM_FLAGS ( IN_ROOM ( ch ) ), ROOM_HOUSE_CRASH );

	return TRUE;
}

bool is_put_ok ( Character *ch, char *obj_desc, char *cont_desc, int obj_dotmode, int cont_dotmode, struct obj_data **obj_data, struct obj_data **cont_data )
{
	Character *tmp_Character;
	// no obj name defined
	if ( !*obj_desc )
	{
		ch->Send ( "Put what in what?\r\n" );
		return FALSE;
	}

	// no container name defined
	if ( !*cont_desc )
	{
		ch->Send ( "What do you want to put %s in?\r\n",
		           ( ( obj_dotmode == FIND_INDIV ) ? "it" : "them" ) );
		return FALSE;
	}

	// multiple (all.xxxx) container name used
	if ( cont_dotmode != FIND_INDIV )
	{
		ch->Send ( "You can only put things into one container at a time.\r\n" );
		return FALSE;
	}

	generic_find ( cont_desc, FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP, ch, &tmp_Character, cont_data );

	// container not found
	if ( !cont_data || !*cont_data )
	{
		ch->Send ( "You don't see %s %s here.\r\n", AN ( cont_desc ), cont_desc );
		return FALSE;
	}

	// container object not a container
	if ( GET_OBJ_TYPE ( *cont_data ) != ITEM_CONTAINER )
	{
		act ( "$p is not a container.", FALSE, ch, *cont_data, 0, TO_CHAR );
		return FALSE;
	}

	// container is closed
	if ( OBJVAL_FLAGGED ( *cont_data, CONT_CLOSED ) )
	{
		ch->Send ( "You'd better open it first!\r\n" );
		return FALSE;
	}

	if ( obj_dotmode == FIND_INDIV )
	{
		// no obj matching the desc found
		if ( ! ( *obj_data = get_obj_in_list_vis ( ch, obj_desc, NULL, ch->carrying ) ) )
		{
			ch->Send ( "You aren't carrying %s %s.\r\n", AN ( obj_desc ), obj_desc );
			return FALSE;
		}

		if ( obj_data == cont_data )
		{
			ch->Send ( "You attempt to fold it into itself, but fail.\r\n" );
			return FALSE;
		}
	}

	return TRUE;
}

/* The following put modes are supported by the code below:

     1) put <object one word> <container one word>
     2) put all.<object> <container>
     3) put all <container>
     4) put <number> <object> <container>

     <container> must be in inventory or on ground.
     all objects to be put into container must be in inventory.
*/
ACMD ( do_put )
{
	char arg1[MAX_INPUT_LENGTH] = "";
	char arg2[MAX_INPUT_LENGTH] = "";
	char buf[MAX_INPUT_LENGTH] = "";
	struct obj_data *next_obj_data, *obj_data, *cont_data;
	int obj_dotmode, cont_dotmode, howmany = 1, processed_put_counter = 0, failed_put_counter = 0;
	char *obj_desc, *cont_desc;

	skip_spaces ( &argument );

	if ( strncasecmp ( argument, "all ", 4 ) )
	{
		/** put all of the stuff that can fit, from inventory to container.
		    all the rest of argument is container description. **/
		argument = any_one_arg ( argument, arg1 );
		skip_spaces ( &argument );
		cont_desc = argument;
		obj_desc = arg1;
	}
	else
	{
		if ( begins_with_number ( argument ) )
		{
			argument = any_one_arg ( argument, arg1 );
			howmany = atoi ( arg1 );
		}

		if ( !str_str ( argument, " in " ) )
		{
			/** single argument mode **/
			two_arguments ( argument, arg1, arg2 );
			obj_desc = arg1;
			cont_desc = arg2;
		}
		else
		{
			/** dual argument mode **/
			obj_desc = str_until ( argument, "in", arg1, sizeof ( arg1 ) );
			cont_desc = arg1;
		}
	}

	obj_dotmode = find_all_dots ( obj_desc );
	cont_dotmode = find_all_dots ( cont_desc );

	if ( !is_put_ok ( ch, obj_desc, cont_desc, obj_dotmode, cont_dotmode, &obj_data, &cont_data ) )
		return;

	if ( !IS_NPC ( ch ) )
		SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_CRASH );

	// put <obj> <container>
	if ( obj_dotmode == FIND_INDIV )
	{
		if ( howmany > 1 )
			wearall = 1;
		/** this loop could end in tears if any trigger purges inventory on the perform_put -- mord**/
		while ( obj_data && howmany )
		{
			next_obj_data = obj_data->next_content;
			perform_put ( ch, obj_data, cont_data ) ? processed_put_counter++ : failed_put_counter++;
			obj_data = get_obj_in_list_vis ( ch, obj_desc, NULL, next_obj_data );
			howmany--;
		}
		wearall = 0;

		if ( processed_put_counter > 1 ||
		        ( processed_put_counter == 1 && failed_put_counter > 0 ) )
		{
			snprintf ( buf, sizeof ( buf ), "You put %d %s%s into $p.", processed_put_counter, arg2, ( processed_put_counter > 1 ) ? "s" : "" );
			act ( buf, FALSE, ch, cont_data, 0, TO_CHAR );
			if ( !slipping )
			{
				snprintf ( buf, sizeof ( buf ), "$n puts some %s into $p.", arg2 );
				act ( buf, FALSE, ch, cont_data, 0, TO_ROOM );
			}
		}
		// put all.<obj> <container>
		// put all <container>
	}
	else
	{
		wearall = 1;
		for ( obj_data = ch->carrying; obj_data; obj_data = next_obj_data )
		{
			next_obj_data = obj_data->next_content;
			if ( obj_data != cont_data &&
			        CAN_SEE_OBJ ( ch, obj_data ) &&
			        GET_OBJ_TYPE ( obj_data ) != ITEM_CONTAINER &&
			        ( obj_dotmode == FIND_ALL || isname_full ( obj_desc, obj_data->name ) ) )
			{
				perform_put ( ch, obj_data, cont_data ) ? processed_put_counter++ : failed_put_counter++;
			}
		}
		wearall = 0;

		if ( !processed_put_counter && !failed_put_counter )
		{
			if ( obj_dotmode == FIND_ALL )
			{
				ch->Send ( "You don't seem to have anything to put in it.\r\n" );
			}
			else
			{
				ch->Send ( "You don't seem to have any %ss.\r\n", obj_desc );
			}
		}
		else if ( processed_put_counter )
		{
			if ( obj_dotmode == FIND_ALL )
			{
				snprintf ( buf, sizeof ( buf ), "You put everything you can into $p." );
				act ( buf, FALSE, ch, cont_data, 0, TO_CHAR );
				if ( !slipping )
				{
					snprintf ( buf, sizeof ( buf ), "$n puts everything $e can into $p." );
					act ( buf, FALSE, ch, cont_data, 0, TO_ROOM );
				}
			}
			else
			{
				snprintf ( buf, sizeof ( buf ), "You put %d %s%s into $p.", processed_put_counter, obj_desc, ( processed_put_counter > 1 ) ? "s" : "" );
				act ( buf, FALSE, ch, cont_data, 0, TO_CHAR );
				if ( !slipping && *arg2 )
				{
					snprintf ( buf, sizeof ( buf ), "$n puts all the %ss $e can into $p.", arg2 );
					act ( buf, FALSE, ch, cont_data, 0, TO_ROOM );
				}
			}
		}
	}
}

int is_magic ( OBJ_DATA *obj )
{
	switch ( GET_OBJ_TYPE ( obj ) )
	{
		case ITEM_SCROLL:
		case ITEM_POTION:
		case ITEM_WAND:
		case ITEM_STAFF:
			return TRUE;
			break;
	}
	return count_magic ( obj->contains, NULL );
}
int count_magic ( struct obj_data *obj, Character *ch )
{
	if ( obj )
	{
		int i = 0;
		if ( obj->contains )
			i += count_magic ( obj->contains, ch );
		if ( obj->next_content )
			i += count_magic ( obj->next_content, ch );

		if ( is_magic ( obj ) )
			return i + 1;
	}
	return 0;
}

int count_magic_items ( Character *ch )
{
	int total = 0;
	int k;
	for ( k = 0;k < NUM_WEARS; k++ )
	{
		if ( GET_EQ ( ch, k ) )
		{
			total += count_magic ( GET_EQ ( ch, k ), ch );
		}
	}
	if ( ch->carrying )
	{
		total += count_magic ( ch->carrying, ch );
	}
	return total;
}

bool can_take_obj ( Character *ch, struct obj_data *obj )
{
//	int i;
	if ( ! ( CAN_WEAR ( obj, ITEM_WEAR_TAKE ) ) )
	{
		act ( "$p: you can't take that!", FALSE, ch, obj, 0, TO_CHAR );
		return FALSE;
	}
	else if ( OBJ_SAT_IN_BY ( obj ) != NULL )
	{
		if ( OBJ_SAT_IN_BY ( obj ) == ch )
			ch->Send ( "Maybe you should get on your feet first?\r\n" );
		else ch->Send ( "You can't take that, %s is sitting on it.\r\n", GET_NAME ( OBJ_SAT_IN_BY ( obj ) ) );
		return FALSE;
	}
	else if ( IS_CARRYING_N ( ch ) >= CAN_CARRY_N ( ch ) )
	{
		act ( "$p: you can't carry that many items.", FALSE, ch, obj, 0, TO_CHAR );
		return FALSE;
	}
	else if ( ( IN_ROOM ( obj ) || ( obj->in_obj && IN_ROOM ( obj->in_obj ) ) )
		&& ( IS_CARRYING_W ( ch ) + GET_OBJ_WEIGHT ( obj ) > CAN_CARRY_W ( ch ) ) )
	{
		act ( "$p: you can't carry that much weight.", FALSE, ch, obj, 0, TO_CHAR );
		return FALSE;
	}
	/* else if ( is_magic ( obj ) && ( i=count_magic_items ( ch ) ) >= MAX_MAGIC_ITEMS )
	{
		if ( i == MAX_MAGIC_ITEMS )
			act ( "$p: you have 45 magic items already.", FALSE, ch, obj, 0, TO_CHAR );
		else
			act ( "$p: you have more then 45 magic items already.", FALSE, ch, obj, 0, TO_CHAR );
		return FALSE;
	} */
	return TRUE;
}


void get_check_money ( Character *ch, struct obj_data *obj )
{
	gold_int value = MONEY ( obj );

	if ( GET_OBJ_TYPE ( obj ) != ITEM_MONEY || value <= 0 )
		return;

	obj_from_char ( obj );
	extract_obj ( obj );

	ch->Gold ( value, GOLD_HAND );
	gold_data ( TAKEN, value );

	if ( value == 1 )
		ch->Send ( "There was 1 coin.\r\n" );
	else
	{
		ch->Send ( "There were %lld coins.\r\n", value );
	}
}

ACMD ( do_meld )
{
	char arg[MAX_INPUT_LENGTH];
	struct obj_data *corpse;

	if ( IS_NPC ( ch ) )
		return;

	one_argument ( argument, arg );

	if ( !*arg )
	{
		ch->Send ( "Meld what?\r\n" );
		return;
	}

	if ( ! ( corpse = get_obj_in_list_vis ( ch, arg, NULL,IN_ROOM ( ch )->contents ) ) )
	{
		ch->Send ( "You don't see %s %s here.\r\n", AN ( arg ), arg );
		return;
	}
	if ( !IS_OBJ_STAT ( corpse, ITEM_PC_CORPSE ) )
	{
		ch->Send ( "You can't meld with that, it's not a player corpse!\r\n" );
		return ;
	}

	if ( !isname ( GET_NAME ( ch ), corpse->short_description ) )
	{
		ch->Send ( "That isn't your corpse! Hands off!\r\n" );
		return;
	}

	perform_meld ( ch, corpse );
}

void perform_meld ( Character *ch, OBJ_DATA *corpse )
{
	OBJ_DATA *item;
	int remorts = MIN(REMORTS(ch), 50);
	ch->Send ( "You meld with your corpse in a sudden flash of light.\r\nYou are dazed.\r\n" );
	act ( "$n melds with $s corpse in a sudden flash of light!", FALSE, ch, 0, 0, TO_ROOM );
	while ( ( item = corpse->contains ) )
	{
		obj_from_obj ( item );
		obj_to_char ( item, ch );
		get_check_money ( ch, item );
	}

	if ( remorts != 0 && !HERE ( corpse, ch ) )
	{
		if ( remorts < 5 )
			GET_WAIT_STATE ( ch ) = 5 RL_SEC;
		else
			GET_WAIT_STATE ( ch ) = remorts RL_SEC;
	}
	extract_obj ( corpse );
	save_corpses();
	Crash_crashsave ( ch );
}

int automeld ( struct obj_data *obj )
{
	Character *ch = NULL;//, *j;
	/*  Descriptor *d;*/
	int i;
	if ( !obj )
		return 0;
	if ( !IS_OBJ_STAT ( obj, ITEM_PC_CORPSE ) )
		return 0;

	if ( ( ch = find_char ( GET_OBJ_VAL ( obj, 0 ) ) ) != NULL )
	{
		perform_meld ( ch, obj );
		new_mudlog ( CMP, GET_LEVEL ( ch ), TRUE, "AUTOMELD: %s (online - fast link)", GET_NAME ( ch ) );
		return 1;
	}
	/**this is redundant it happens above in find_char **/
	/*
	for (d = descriptor_list; d; d = d->next)
	{
	  if (d->character && GET_ID(d->character) == GET_OBJ_VAL(obj, 0))
	  {
	    
	    perform_meld(d->character, obj);
	    new_mudlog( CMP, GET_LEVEL(d->character), TRUE, "AUTOMELD: %s (online - link)", GET_NAME(d->character));
	    return 1;
	  }
	}


	for (j = character_list; j; j = j->next)
	{
	  if (j && GET_ID(j) == GET_OBJ_VAL(obj, 0))
	  {
	    perform_meld(j, obj);
	    new_mudlog( CMP, GET_LEVEL(j), TRUE, "AUTOMELD: %s (online - linkless)", GET_NAME(j));
	    return 1;
	  }
	}
	*/
	for ( i = 0; i <= pi.TopOfTable(); i++ )
	{
		if ( *pi.NameByIndex ( i ) )
		{
			if ( pi.IdByIndex ( i ) == GET_OBJ_VAL ( obj, 0 ) )
			{
				ch = new Character ( FALSE );
				if ( pi.IdByIndex ( i ) )
					ch->loader = pi.IdByIndex ( i );
				if ( pi.LoadChar ( pi.NameByIndex ( i ), ch ) > -1 )
				{
					if ( !ch )
					{
						new_mudlog ( CMP, 51, TRUE, "AUTOMELD: %s (error)", pi.NameByIndex ( i ) );
						delete ( ch );
						return 0;
					}
					new_mudlog ( CMP, GET_LEVEL ( ch ), TRUE, "AUTOMELD: %s (offline)", GET_NAME ( ch ) );
					add_char_to_list ( ch );
					ch->desc = NULL;
					char_to_room ( ch, world_vnum[1200] );
					Crash_load ( ch );
					perform_meld ( ch, obj );
					Crash_rentsave ( ch, 0 );
					ch->loader = NOBODY;
					extract_char ( ch );
					return 1;
				}
				else
				{
					delete ( ch );
					return 1;
				}
			}
		}
	}
	return 0;
}

bool perform_get_from_container ( Character *ch, struct obj_data *obj,
                                  struct obj_data *cont, int mode )
{

	if ( mode != FIND_OBJ_INV && !can_take_obj ( ch, obj ) )
	{
		return FALSE;
	}

	if ( ! ( CAN_WEAR ( obj, ITEM_WEAR_TAKE ) ) )
	{
		act ( "$p: you can't take that!", FALSE, ch, obj, 0, TO_CHAR );
		return FALSE;
	}

	if ( IS_CARRYING_N ( ch ) >= CAN_CARRY_N ( ch ) )
	{
		act ( "$p: you can't hold any more items.", FALSE, ch, obj, 0, TO_CHAR );
		return FALSE;
	}

	if ( get_otrigger ( obj, ch ) <= 0 )
	{
		return FALSE;
	}
	if ( get_out_otrigger ( cont, obj, ch ) <= 0 )
	{
		return FALSE;
	}

	if ( GET_OBJ_VNUM ( obj ) >= 3300 && GET_OBJ_VNUM ( obj ) <= 3312 )
	{
		if ( cont->carried_by )
			new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s gets %s from %s (inventory)",  GET_NAME ( ch ), obj->short_description, cont->short_description );
		else if ( IN_ROOM ( cont ) )
			new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s gets %s from %s in room %d",  GET_NAME ( ch ), obj->short_description,cont->short_description, GET_ROOM_VNUM ( IN_ROOM ( cont ) ) );
		else
			new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s gets %s from %s",  GET_NAME ( ch ), obj->short_description, cont->short_description );
	}
	obj_from_obj ( obj );
	obj_to_char ( obj, ch );
	if ( !wearall || ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) ) )
		act ( "You get $p from $P.", FALSE, ch, obj, cont, TO_CHAR );
	if ( !wearall && !slipping )
		act ( "$n gets $p from $P.", TRUE, ch, obj, cont, TO_ROOM );

	if ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_HOUSE ) )
		SET_BIT_AR ( ROOM_FLAGS ( IN_ROOM ( ch ) ), ROOM_HOUSE_CRASH );
	get_check_money ( ch, obj );
	if ( IS_OBJ_STAT ( cont, ITEM_PC_CORPSE ) && GET_OBJ_VAL ( cont, 6 ) != 0 )
		save_corpses();
	return TRUE;
}


void get_from_container ( Character *ch, struct obj_data *cont,
                          int obj_dotmode, char *obj_desc, int mode, int howmany )
{
	struct obj_data *obj, *next_obj;
	int processed_get_counter = 0, failed_get_counter = 0;
	char buf[MAX_INPUT_LENGTH];

	if ( OBJVAL_FLAGGED ( cont, CONT_CLOSED ) )
	{
		act ( "$p is closed.", FALSE, ch, cont, 0, TO_CHAR );
		return;
	}
	if ( IS_OBJ_STAT ( cont, ITEM_PC_CORPSE ) )
		if ( ( long ) GET_OBJ_VAL ( cont, 0 ) == GET_IDNUM ( ch ) )
		{
			perform_meld ( ch, cont );
			return;
		}

	if ( obj_dotmode == FIND_INDIV )
	{
		if ( ! ( obj = get_obj_in_list_vis ( ch, obj_desc, NULL, cont->contains ) ) )
		{
			snprintf ( buf, sizeof ( buf ), "There doesn't seem to be %s %s in $p.", AN ( obj_desc ), obj_desc );
			act ( buf, FALSE, ch, cont, 0, TO_CHAR );
			return;
		}
		if ( howmany > 1 )
			wearall = 1;
		while ( obj && howmany )
		{
			next_obj = obj->next_content;
			perform_get_from_container ( ch, obj, cont, mode ) ? processed_get_counter++ : failed_get_counter++;
			obj = get_obj_in_list_vis ( ch, obj_desc, NULL, next_obj );
			howmany--;
		}
		wearall = 0;

		if ( processed_get_counter > 1 ||
		        ( processed_get_counter == 1 && failed_get_counter > 0 ) )
		{
			snprintf ( buf, sizeof ( buf ), "You get %d %s%s from $p.", processed_get_counter, obj_desc, ( processed_get_counter > 1 ) ? "s" : "" );
			act ( buf, FALSE, ch, cont, 0, TO_CHAR );
			if ( !slipping )
			{
				snprintf ( buf, sizeof ( buf ), "$n gets some %s from $p.", obj_desc );
				act ( buf, FALSE, ch, cont, 0, TO_ROOM );
			}
		}
	}
	else
	{
		if ( obj_dotmode == FIND_ALLDOT && !*obj_desc )
		{
			ch->Send ( "Get all of what?\r\n" );
			return;
		}
		wearall = 1;
		for ( obj = cont->contains; obj; obj = next_obj )
		{
			next_obj = obj->next_content;
			if ( CAN_SEE_OBJ ( ch, obj ) &&
			        ( obj_dotmode == FIND_ALL || isname_full ( obj_desc, obj->name ) ) )
			{
				perform_get_from_container ( ch, obj, cont, mode ) ? processed_get_counter++ : failed_get_counter++;
			}
		}
		wearall = 0;

		if ( !processed_get_counter && !failed_get_counter )
		{
			if ( obj_dotmode == FIND_ALL )
				act ( "$p seems to be empty.", FALSE, ch, cont, 0, TO_CHAR );
			else
			{
				snprintf ( buf, sizeof ( buf ), "You can't seem to find any %ss in $p.", obj_desc );
				act ( buf, FALSE, ch, cont, 0, TO_CHAR );
			}
		}
		else if ( processed_get_counter )
		{
			if ( obj_dotmode == FIND_ALL )
			{
				act ( "You get everything you can out of $p.", FALSE, ch, cont, 0, TO_CHAR );
				if ( !slipping )
				{
					act ( "$n gets everything $e can out of $p.", FALSE, ch, cont, 0, TO_ROOM );
				}
			}
			else
			{
				snprintf ( buf, sizeof ( buf ), "You get %d %s%s out of $p.", processed_get_counter, obj_desc, ( processed_get_counter > 1 ) ? "s" : "" );
				act ( buf, FALSE, ch, cont, 0, TO_CHAR );
				if ( !slipping )
				{
					snprintf ( buf, sizeof ( buf ), "$n gets all the %ss $e can out of $p.", obj_desc );
					act ( buf, FALSE, ch, cont, 0, TO_ROOM );
				}
			}
		}
	}
}

void perform_get ( Character *ch, char *num, char *arg2, char *arg3 )
{
	int cont_dotmode, obj_dotmode, found = 0, mode = 0;
	struct obj_data *cont;
	Character *tmp_char;
	int amount = 1;
	char *obj_desc, *cont_desc;


	amount = atoi ( num );
	obj_desc = arg2;
	cont_desc = arg3;


	cont_dotmode = find_all_dots ( cont_desc );
	obj_dotmode = find_all_dots ( obj_desc );

	if ( cont_dotmode == FIND_INDIV )
	{
		mode = generic_find ( cont_desc, FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP, ch, &tmp_char, &cont );
		if ( !cont )
		{
			ch->Send ( "You don't have %s %s.\r\n", AN ( cont_desc ), cont_desc );
			return;
		}
		if ( GET_OBJ_TYPE ( cont ) != ITEM_CONTAINER )
		{
			act ( "$p is not a container.", FALSE, ch, cont, 0, TO_CHAR );
			return;
		}
		if ( IS_OBJ_STAT ( cont, ITEM_PC_CORPSE ) && GET_OBJ_VAL ( cont, 0 ) == GET_IDNUM ( ch ) )
		{
			ch->Send ( "You can't take things from your corpse.\r\n"
			           "Use: meld corpse\r\n" );
			return;
		}
		if ( IS_OBJ_STAT ( cont, ITEM_PC_CORPSE ) )
		{
			if ( IS_NPC ( ch ) || GET_OBJ_VAL ( cont, 6 ) == 0 || !IS_PK ( ch ) || !IS_OBJ_STAT ( cont, ITEM_PK_CORPSE ) )
			{
				ch->Send ( "You can't take things from that player's corpse.\r\n" );
				return;
			}
		}
	}
	else if ( cont_dotmode == FIND_ALLDOT && !*cont_desc )
	{
		ch->Send ( "Get from all of what?\r\n" );
		return;
	}

	if ( cont_dotmode == FIND_INDIV )
	{
		get_from_container ( ch, cont, obj_dotmode, obj_desc, mode, amount );
	}
	else
	{
		wearall = 1;
		for ( cont = ch->carrying; cont; cont = cont->next_content )
		{
			if ( CAN_SEE_OBJ ( ch, cont )
			        && ( cont_dotmode == FIND_ALL
			             || isname ( cont_desc, cont->name ) ) )
			{
				if ( GET_OBJ_TYPE ( cont ) == ITEM_CONTAINER )
				{
					found = 1;
					get_from_container ( ch, cont, obj_dotmode, obj_desc, FIND_OBJ_INV, amount );
				}
				else if ( cont_dotmode == FIND_ALLDOT )
				{
					found = 1;
					act ( "$p is not a container.", FALSE, ch, cont, 0, TO_CHAR );
				}
			}
		}
		for ( cont = IN_ROOM ( ch )->contents; cont; cont = cont->next_content )
		{
			if ( ( ( CAN_SEE_OBJ ( ch, cont )
			         && !IS_SET_AR ( GET_OBJ_EXTRA ( cont ), ITEM_HIDDEN )
			         && !OBJ_FLAGGED ( cont, ITEM_NODISPLAY ) )
			        || GET_LEVEL ( ch ) >=LVL_IMMORT )
			        && ( cont_dotmode == FIND_ALL
			             || isname ( cont_desc, cont->name ) ) )
			{
				if ( GET_OBJ_TYPE ( cont ) == ITEM_CONTAINER )
				{
					found = 1;
					get_from_container ( ch, cont, obj_dotmode, obj_desc, FIND_OBJ_ROOM, amount );
				}
				else if ( cont_dotmode == FIND_ALLDOT )
				{
					found = 1;
					act ( "$p is not a container.", FALSE, ch, cont, 0, TO_CHAR );
				}
			}
		}
		wearall = 0;
		if ( !found )
		{
			if ( cont_dotmode == FIND_ALL )
				ch->Send ( "You can't seem to find any containers.\r\n" );
			else
			{
				ch->Send ( "You can't seem to find any %ss here.\r\n", cont_desc );
			}
		}
	}
}


bool perform_get_from_room ( Character *ch, struct obj_data *obj )
{
	if ( can_take_obj ( ch, obj ) && get_otrigger ( obj, ch ) > 0 )
	{
		if ( GET_OBJ_VNUM ( obj ) >= 3300 && GET_OBJ_VNUM ( obj ) <= 3312 )
		{
			if ( IN_ROOM ( obj ) )
				new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s gets %s from room %d",  GET_NAME ( ch ), obj->short_description, GET_ROOM_VNUM ( IN_ROOM ( obj ) ) );
			else
				new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s gets %s",  GET_NAME ( ch ), obj->short_description );
		}
		obj_from_room ( obj );
		obj_to_char ( obj, ch );
		if ( !wearall || ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) ) )
			act ( "You pick up $p.", FALSE, ch, obj, 0, TO_CHAR );
		if ( !wearall && !slipping )
		{
			act ( "$n picks up $p.", TRUE, ch, obj, 0, TO_ROOM );
		}

		get_check_money ( ch, obj );
		if ( IS_OBJ_STAT ( obj, ITEM_PC_CORPSE ) )
		{
			remove_corpse_from_list ( obj );
			save_corpses();
		}

		if ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_HOUSE ) )
			SET_BIT_AR ( ROOM_FLAGS ( IN_ROOM ( ch ) ), ROOM_HOUSE_CRASH );
		return TRUE;
	}
	return FALSE;
}


void get_from_room ( Character *ch, char *obj_desc, int howmany )
{
	struct obj_data *obj, *next_obj;
	int dotmode, processed_get_counter = 0, failed_get_counter = 0;
	char buf[MAX_INPUT_LENGTH];

	dotmode = find_all_dots ( obj_desc );

	if ( dotmode == FIND_INDIV )
	{
		if ( ! ( obj = get_obj_in_list_vis ( ch, obj_desc, NULL, IN_ROOM ( ch )->contents ) ) )
		{
			ch->Send ( "You don't see %s %s here.\r\n", AN ( obj_desc ), obj_desc );
		}
		else
		{
			if ( howmany > 1 )
				wearall = 1;
			while ( obj && howmany )
			{
				next_obj = obj->next_content;
				perform_get_from_room ( ch, obj ) ? processed_get_counter++ : failed_get_counter++;
				obj = get_obj_in_list_vis ( ch, obj_desc, NULL, next_obj );
				howmany--;
			}
			wearall = 0;
			if ( processed_get_counter > 1 ||
			        ( processed_get_counter == 1 && failed_get_counter > 0 ) )
			{
				snprintf ( buf, sizeof ( buf ),
				           "You pick up %d thing%s named %s%s off the ground.",
				           processed_get_counter, ( processed_get_counter > 1 ) ? "s" : "", obj_desc, ( processed_get_counter > 1 ) ? "s" : "" );
				act ( buf, FALSE, ch, 0, 0, TO_CHAR );
				if ( !slipping )
				{
					snprintf ( buf, sizeof ( buf ),
					           "$n picks up %s thing%s named %s%s from the room.",
					           ( processed_get_counter > 1 ) ? "some" : "a",
					           ( processed_get_counter > 1 ) ? "s" : "", obj_desc,
					           ( processed_get_counter > 1 ) ? "s" : "" );
					act ( buf, FALSE, ch, 0, 0, TO_ROOM );
				}
			}
		}
	}
	else
	{
		if ( dotmode == FIND_ALLDOT && !*obj_desc )
		{
			ch->Send ( "Get all of what?\r\n" );
			return;
		}

		wearall = 1;
		for ( obj = IN_ROOM ( ch )->contents; obj; obj = next_obj )
		{
			next_obj = obj->next_content;
			if ( CAN_SEE_OBJ ( ch, obj )
			        && !IS_SET_AR ( GET_OBJ_EXTRA ( obj ), ITEM_HIDDEN )
			        && ( !OBJ_FLAGGED ( obj, ITEM_NODISPLAY ) || GET_LEVEL ( ch ) >LVL_IMMORT )
			        && ( dotmode == FIND_ALL || isname ( obj_desc, obj->name ) ) )
			{
				perform_get_from_room ( ch, obj ) ? processed_get_counter++ : failed_get_counter++;
			}
		}
		wearall = 0;

		if ( !processed_get_counter && !failed_get_counter )
		{
			/* Are they trying to take something in a room extra description? */
			if ( find_exdesc ( obj_desc, IN_ROOM ( ch )->ex_description ) != NULL )
			{
				ch->Send ( "You can't take %s %s.\r\n", AN ( obj_desc ), obj_desc );
				return;
			}
			if ( dotmode == FIND_ALL )
				ch->Send ( "There doesn't seem to be anything here.\r\n" );
			else
			{
				ch->Send ( "You don't see any %ss here.\r\n", obj_desc );
			}
		}
		else if ( processed_get_counter )
		{
			if ( dotmode == FIND_ALL )
			{
				snprintf ( buf, sizeof ( buf ), "You get everything you can off the ground." );
				act ( buf, FALSE, ch, 0, 0, TO_CHAR );
				if ( !slipping )
				{
					snprintf ( buf, sizeof ( buf ), "$n gets everything $e can off the ground." );
					act ( buf, TRUE, ch, 0, 0, TO_ROOM );
				}
			}
			else
			{
				snprintf ( buf, sizeof ( buf ), "You get %d %s%s off the ground.",
				           processed_get_counter, obj_desc, ( processed_get_counter > 1 ) ? "s" : "" );
				act ( buf, FALSE, ch, 0, 0, TO_CHAR );
				if ( !slipping )
				{
					snprintf ( buf, sizeof ( buf ), "$n gets all the %ss $e can off the ground.", obj_desc );
					act ( buf, TRUE, ch, 0, 0, TO_ROOM );
				}
			}
		}
	}
}

int is_two_words ( char * str )
{
	char buf[MAX_INPUT_LENGTH];

	if ( !str || !*str )
		return 0;
	str = any_one_arg ( str, buf );
	skip_spaces ( &str );
	if ( !*str )
		return 0;
	str = any_one_arg ( str, buf );
	skip_spaces ( &str );

	if ( *buf && !*str )
		return 1;
	else
		return 0;
}


ACMD ( do_get )
{
	char arg1[MAX_INPUT_LENGTH];
	char arg3[MAX_INPUT_LENGTH] = "";
	char num[MAX_INPUT_LENGTH] = "1";
	char *from = NULL;

	skip_spaces ( &argument );
	if ( *argument == '\0' )
	{
		ch->Send ( "Get what?\n" );
		return;
	}
	if ( begins_with_number ( argument ) )
		argument = any_one_arg ( argument, num );
	from = str_until ( argument, "from", arg1, sizeof ( arg1 ) );
	skip_spaces ( &from );
	if ( from && *from )
	{
		strlcpy ( arg3, from, sizeof ( arg3 ) );

	}
	else
	{
		/**check if it is only 2 arguments, meaning it wants to get something from the room **/
		if ( is_two_words ( argument ) )
		{
			argument = any_one_arg ( argument, arg1 );
			argument = any_one_arg ( argument, arg3 );
		}
	}
	//one_argument(two_arguments(argument, arg1, arg2), arg3);




	if ( IS_CARRYING_N ( ch ) >= CAN_CARRY_N ( ch ) )
	{
		ch->Send ( "Your arms are already full!\r\n" );
		return;
	}

	if ( !IS_NPC ( ch ) )
		SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_CRASH );

	// get <item>
	if ( !*arg3 || !str_cmp ( arg3, "room" ) )
	{
		get_from_room ( ch, arg1, 1 );
		return;
	}

	// get <number> <item>
	if ( *num && !*arg3 )
	{
		get_from_room ( ch, arg3, atoi ( num ) );
		return;
	}

	// get <item> <container>
	// get <number> <item> from <container>
	perform_get ( ch, num, arg1, arg3 );

	// using SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH) instead of immediate saveing
	// Crash_crashsave(ch);
}


void perform_drop_gold ( Character *ch, gold_int amount,
                         sbyte mode, Room *  RDR )
{
	struct obj_data *obj;
	char buf[MAX_INPUT_LENGTH];

	if ( amount <= 0 )
		ch->Send ( "Heh heh heh.. we are jolly funny today, eh?\r\n" );
	else if ( ch->Gold ( 0, GOLD_HAND ) < amount )
		ch->Send ( "You don't have that many coins!\r\n" );
	//	else if ( amount > 2000000000 )
	//		ch->Send ( "You can't drop more then 2 billion.\r\n" );
	else
	{
		/*for the gold tally code */
		gold_data ( DROPPED, amount );

		if ( mode != SCMD_JUNK )
		{
			WAIT_STATE ( ch, PULSE_VIOLENCE );   /* to prevent coin-bombing */
			obj = create_money ( amount );
			if ( mode == SCMD_DONATE )
			{
				ch->Send ( "You throw some gold into the air where it disappears in a puff of smoke!\r\n" );
				act ( "$n throws some gold into the air where it disappears in a puff of smoke!", FALSE, ch, 0, 0, TO_ROOM );
				obj_to_room ( obj, RDR );
				act ( "$p suddenly appears in a puff of orange smoke!", 0, 0,
				      obj, 0, TO_ROOM );
			}
			else if ( mode == SCMD_SPILL )
			{
				int i, j = 0;
				ch->Send ( "Your gold spills all over the ground!\r\n" );
				snprintf ( buf, sizeof ( buf ), "$n's gold spills all over the ground!" );
				act ( buf, TRUE, ch, 0, 0, TO_ROOM );
				ch->Gold ( -amount, GOLD_HAND );
				for ( i = number ( 1, amount ); amount > 0;
				        i = number ( 1, amount ) )
				{
					if ( j < 10 )
					{
						obj = create_money ( i );
						amount -= i;
					}
					else
					{
						obj = create_money ( amount );
						amount = 0;
					}
					obj_to_room ( obj, IN_ROOM ( ch ) );
				}

			}
			else
			{
				switch ( drop_wtrigger ( obj, ch ) )   /* obj may be purged */
				{
					case  0:
						extract_obj ( obj );
						return;
					case -1:
						return;
				}
				ch->Send ( "You drop some gold.\r\n" );
				if ( !slipping )
				{
					snprintf ( buf, sizeof ( buf ), "$n drops %s.", money_desc ( amount ) );
					act ( buf, TRUE, ch, 0, 0, TO_ROOM );
				}
				obj_to_room ( obj, IN_ROOM ( ch ) );
			}
		}
		else
		{
			if ( !slipping )
			{
				snprintf ( buf, sizeof ( buf ),
				           "$n drops %s which disappears in a puff of smoke!",
				           money_desc ( amount ) );
				act ( buf, FALSE, ch, 0, 0, TO_ROOM );
			}
			ch->Send ( "You drop some gold which disappears in a puff of smoke!\r\n" );
		}
		ch->Gold ( -amount, GOLD_HAND );
	}
}


#define VANISH(mode) ((mode == SCMD_DONATE || mode == SCMD_JUNK) ? \
                      "  It vanishes in a puff of smoke!" : "")

int perform_drop ( Character *ch, struct obj_data *obj,
                   sbyte mode, const char *sname, Room *  RDR )
{
	int value;
	char buf[MAX_INPUT_LENGTH];

	if ( drop_otrigger ( obj, ch ) <= 0 )
		return ( 0 );

	if ( ( mode == SCMD_DROP ) && !drop_wtrigger ( obj, ch ) )
		return ( 0 );

	if ( IS_OBJ_STAT ( obj, ITEM_NODROP ) )
	{
		snprintf ( buf, sizeof ( buf ), "You can't %s $p, it must be CURSED!", sname );
		act ( buf, FALSE, ch, obj, 0, TO_CHAR );
		return ( 0 );
	}
	if ( SCMD_DROP == mode && ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_HOUSE ) )
	{
		void count_items_in_list ( struct obj_data *obj, int& total_items );
		int value = house_item_count ( GET_ROOM_VNUM ( IN_ROOM ( ch ) ) );
		int plussage = 1;
		if ( GET_OBJ_TYPE ( obj ) == ITEM_CONTAINER )
			count_items_in_list ( obj->contains, plussage );
		int capa = house_capacity ( ( GET_ROOM_VNUM ( IN_ROOM ( ch ) ) ) );
		if ( value >= capa )
		{
			ch->Send ( "No matter how you try, you can't fit anything else in.[%d/%d]\r\n", value, capa );
			return FALSE;
		}
		else if ( ( value + plussage ) > capa )
		{
			ch->Send ( "That container's items would push your house capacity of %d over it's limit by %d.\r\n",capa, capa - ( value+plussage ) );
			return FALSE;
		}
	}
	if ( !wearall || ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) ) )
	{
		snprintf ( buf, sizeof ( buf ), "You %s $p.%s", sname, VANISH ( mode ) );
		act ( buf, FALSE, ch, obj, 0, TO_CHAR );
	}
	if ( !wearall )
	{
		if ( !slipping )
		{
			snprintf ( buf, sizeof ( buf ), "$n %ss $p.%s", sname, VANISH ( mode ) );
			act ( buf, TRUE, ch, obj, 0, TO_ROOM );
		}
	}
	obj_from_char ( obj );

	if ( ( mode == SCMD_DONATE ) && IS_OBJ_STAT ( obj, ITEM_NODONATE ) )
		mode = SCMD_JUNK;

	switch ( mode )
	{
		case SCMD_DROP:
			if ( GET_OBJ_VNUM ( obj ) >= 3300 && GET_OBJ_VNUM ( obj ) <= 3312 )
			{
				if ( IN_ROOM ( ch ) )
					new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s drops %s in room %d",  GET_NAME ( ch ), obj->short_description, GET_ROOM_VNUM ( IN_ROOM ( ch ) ) );

			}
			obj_to_room ( obj, IN_ROOM ( ch ) );
			if ( IS_OBJ_STAT ( obj, ITEM_MELT_DROP ) )
			{
				if ( !slipping )
					act ( "$p dissolves into smoke.", FALSE, 0, obj, 0, TO_ROOM );
				act ( "$p dissolves into smoke.", FALSE, 0, obj, 0, TO_CHAR );
				extract_obj ( obj );
			}
			return ( 0 );
		case SCMD_DONATE:
			if ( GET_OBJ_VNUM ( obj ) >= 3300 && GET_OBJ_VNUM ( obj ) <= 3312 )
			{
				if ( IN_ROOM ( ch ) )
					new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s donates %s from room %d",  GET_NAME ( ch ), obj->short_description, GET_ROOM_VNUM ( IN_ROOM ( ch ) ) );
			}
			obj_to_room ( obj, RDR );
			act ( "$p suddenly appears in a puff a smoke!", FALSE, 0, obj, 0, TO_ROOM );
			return ( 0 );
		case SCMD_JUNK:
			if ( GET_OBJ_VNUM ( obj ) >= 3300 && GET_OBJ_VNUM ( obj ) <= 3312 )
			{
				if ( IN_ROOM ( ch ) )
					new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s junks %s in room %d",  GET_NAME ( ch ), obj->short_description, GET_ROOM_VNUM ( IN_ROOM ( ch ) ) );
			}
			value = MAX ( 1, (int) MIN ( (gold_int) 200, GET_OBJ_COST ( obj ) / 16 ) );
			extract_obj ( obj );
			ch->Gold ( value, GOLD_BANK );
			return ( value );
		default:
			log ( "SYSERR: Incorrect argument %d passed to perform_drop.", mode );
			break;
	}

	return ( 0 );
}



ACMD ( do_drop )
{
	struct obj_data *obj, *next_obj;
	Room *  RDR = NULL;
	sbyte mode = SCMD_DROP;
	int dotmode, amount = 0, multi, num_don_rooms = 0;
	gold_int multig;
	const char *sname;
	char buf[MAX_INPUT_LENGTH];
	char arg[MAX_INPUT_LENGTH];
	Room * donationrooms[3];

	switch ( subcmd )
	{
		case SCMD_DISCARD:
			sname = "discard";
			mode = SCMD_JUNK;
			break;
		case SCMD_JUNK:
			sname = "junk";
			mode = SCMD_JUNK;
			break;
		case SCMD_DONATE:

			if ( GET_RACE ( ch ) == RACE_GLADIATOR )
			{
				ch->Send ( "As a Gladiator you are far too busy to have time for that.\r\n" );
				return;
			}
			sname = "donate";
			mode = SCMD_DONATE;
//        /* fail + double chance for room 1   */
//        num_don_rooms = (CONFIG_DON_ROOM_1 != NULL) * 2 +
//                        (CONFIG_DON_ROOM_2 != NULL)     +
//                        (CONFIG_DON_ROOM_3 != NULL)     + 1 ;
//        switch (number(0, num_don_rooms)) {
			/* find the valid donation rooms */
			if ( CONFIG_DON_ROOM_1 != NULL && GET_ROOM_VNUM ( CONFIG_DON_ROOM_1 ) != 0 )
				donationrooms[num_don_rooms++] = CONFIG_DON_ROOM_1;
			if ( CONFIG_DON_ROOM_2 != NULL && GET_ROOM_VNUM ( CONFIG_DON_ROOM_2 ) != 0 )
				donationrooms[num_don_rooms++] = CONFIG_DON_ROOM_2;
			if ( CONFIG_DON_ROOM_3 != NULL && GET_ROOM_VNUM ( CONFIG_DON_ROOM_3 ) != 0 )
				donationrooms[num_don_rooms++] = CONFIG_DON_ROOM_3;

			switch ( number ( 0, num_don_rooms-1 ) )
			{
				case 0:
					RDR = donationrooms[0];
					break;
				case 1:
					RDR = donationrooms[1];
					break;
				case 2:
					RDR = donationrooms[2];
					break;
				default:
					RDR = NULL;
					break;
			}
			if ( RDR == NULL )
			{
				ch->Send ( "Sorry, you can't donate anything right now.\r\n" );
				return;
			}
			break;
		default:
			sname = "drop";
			break;
	}

	argument = one_argument ( argument, arg );

	if ( !*arg )
	{
		ch->Send ( "What do you want to %s?\r\n", sname );
		return;
	}
	else if ( is_number ( arg ) )
	{
  	        multi = atoi (arg);
		multig = atol ( arg );
		one_argument ( argument, arg );
		if ( !str_cmp ( "coins", arg ) || !str_cmp ( "coin", arg ) )
			perform_drop_gold ( ch, multig, mode, RDR );
		else if ( multi <= 0 )
			ch->Send ( "Yeah, that makes sense.\r\n" );
		else if ( !*arg )
		{
			ch->Send ( "What do you want to %s %d of?\r\n", sname,
			           multi );
		}
		else
			if ( ! ( obj = get_obj_in_list_vis ( ch, arg, NULL, ch->carrying ) ) )
			{
				ch->Send ( "You don't seem to have any %ss.\r\n", arg );
			}
			else
			{
				do
				{
					next_obj =
					    get_obj_in_list_vis ( ch, arg, NULL, obj->next_content );
					amount += perform_drop ( ch, obj, mode, sname, RDR );
					obj = next_obj;
				}
				while ( obj && --multi );
			}
	}
	else
	{
		dotmode = find_all_dots ( arg );

		/* Can't junk or donate all */
		if ( ( dotmode == FIND_ALL )
		        && ( subcmd == SCMD_JUNK || subcmd == SCMD_DONATE ) )
		{
			if ( subcmd == SCMD_JUNK )
				ch->Send ( "Go to the dump if you want to junk EVERYTHING!\r\n" );
			else
				ch->Send ( "Go do the donation room if you want to donate EVERYTHING!\r\n" );
			return;
		}
		if ( dotmode == FIND_ALL )
		{
			if ( !ch->carrying )
				ch->Send ( "You don't seem to be carrying anything.\r\n" );
			else
				for ( obj = ch->carrying; obj; obj = next_obj )
				{
					next_obj = obj->next_content;
					amount += perform_drop ( ch, obj, mode, sname, RDR );
				}
		}
		else if ( dotmode == FIND_ALLDOT )
		{
			if ( !*arg )
			{
				ch->Send ( "What do you want to %s all of?\r\n", sname );
				return;
			}
			if ( ! ( obj = get_obj_in_list_vis ( ch, arg, NULL, ch->carrying ) ) )
			{
				ch->Send ( "You don't seem to have any %ss.\r\n",
				           arg );
			}
			wearall = 1;
			if ( obj )
			{
				ch->Send ( "You %s all of %s.\r\n", sname, arg );
				if ( !slipping )
				{
					snprintf ( buf, sizeof ( buf ), "$n %ss all of %s.", sname, arg );
					act ( buf, TRUE, ch, 0, 0, TO_ROOM );
				}
			}

			while ( obj )
			{
				next_obj =
				    get_obj_in_list_vis ( ch, arg, NULL, obj->next_content );
				amount += perform_drop ( ch, obj, mode, sname, RDR );
				obj = next_obj;
			}
			wearall = 0;
		}
		else
		{
			if ( ! ( obj = get_obj_in_list_vis ( ch, arg, NULL, ch->carrying ) ) )
			{
				ch->Send ( "You don't seem to have %s %s.\r\n", AN ( arg ),
				           arg );
			}
			else
				amount += perform_drop ( ch, obj, mode, sname, RDR );
		}
	}

	if ( amount && ( subcmd == SCMD_JUNK ) )
	{
		ch->Send ( "You have been rewarded by the gods!\r\n" );
		if ( !slipping )
		{
			act ( "$n has been rewarded by the gods!", TRUE, ch, 0, 0, TO_ROOM );
		}
		ch->Gold ( amount, GOLD_HAND );
		gold_data ( TAKEN, amount );
	}
}


void perform_give ( Character *ch, Character *vict,
                    struct obj_data *obj )
{

	if ( give_otrigger ( obj, ch, vict ) <= 0 )
		return;
	if ( receive_mtrigger ( vict, ch, obj ) <= 0 )
		return;
	if ( IS_OBJ_STAT ( obj, ITEM_NODROP ) )
	{
		act ( "You can't let go of $p!!  Yeech!", FALSE, ch, obj, 0,
		      TO_CHAR );
		return;
	}
	if ( IS_CARRYING_N ( vict ) >= CAN_CARRY_N ( vict ) )
	{
		act ( "$N seems to have $S hands full.", FALSE, ch, 0, vict,
		      TO_CHAR );
		if ( !slipping )
			act ( "$n just tried to give you $p but your hands were full.",
			      FALSE, ch, obj, vict, TO_VICT );
		return;
	}
	if ( GET_OBJ_WEIGHT ( obj ) + IS_CARRYING_W ( vict ) > CAN_CARRY_W ( vict ) )
	{
		act ( "$E can't carry that much weight.", FALSE, ch, 0, vict, TO_CHAR );
		if ( !slipping )
			act ( "$n just tried to give you $p but you can't carry that much weight.", FALSE, ch, obj, vict, TO_VICT );
		return;
	}

	obj_from_char ( obj );
	obj_to_char ( obj, vict );
	if ( GET_OBJ_VNUM ( obj ) >= 3300 && GET_OBJ_VNUM ( obj ) <= 3312 )
	{
		if ( IN_ROOM ( ch ) )
			new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "[TOKEN] %s gives %s to %s in %d",  GET_NAME ( ch ), obj->short_description, GET_NAME ( vict ), GET_ROOM_VNUM ( IN_ROOM ( ch ) ) );
	}
	if ( !slipping )
	{
		act ( "You give $p to $N.", FALSE, ch, obj, vict, TO_CHAR );
		act ( "$n gives you $p.", FALSE, ch, obj, vict, TO_VICT );
		act ( "$n gives $p to $N.", TRUE, ch, obj, vict, TO_NOTVICT );
	}
	else
		act ( "You slip $p to $N.", FALSE, ch, obj, vict, TO_CHAR );
	SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_CRASH );
	SET_BIT_AR ( PLR_FLAGS ( vict ), PLR_CRASH );
}

/* utility function for give */
Character *give_find_vict ( Character *ch, char *arg )
{
	Character *vict;

	if ( !*arg )
	{
		*ch << "To who?\r\n";
		return ( NULL );
	}
	else if ( ! ( vict = get_char_vis ( ch, arg, NULL, FIND_CHAR_ROOM ) ) )
	{
		*ch <<  CONFIG_NOPERSON;
		return ( NULL );
	}
	else if ( vict == ch )
	{
		*ch << "What's the point of that?\r\n";
		return ( NULL );
	}
	else
		return ( vict );
}


void perform_give_gold ( Character *ch, Character *vict,
                         gold_int amount )
{
	char buf[MAX_INPUT_LENGTH];

	if ( amount <= 0 )
	{
		ch->Send ( "Heh heh heh ... we are jolly funny today, eh?\r\n" );
		return;
	}
	//	else if ( amount > 2000000000 )
	//	{
	//		ch->Send ( "You can't give more then 2 billion.\r\n" );
	//		return;
	//	}

	if ( ( ch->Gold ( 0, GOLD_HAND ) < amount )
	        && ( IS_NPC ( ch ) || ( GET_LEVEL ( ch ) < LVL_GOD ) ) )
	{
		ch->Send ( "You don't have that many coins!\r\n" );
		return;
	}
	ch->Send ( "%s", CONFIG_OK );
	if ( !slipping )
	{
		snprintf ( buf, sizeof ( buf ), "$n gives you %lld gold coin%s.", amount,
		           amount == 1 ? "" : "s" );
		act ( buf, FALSE, ch, 0, vict, TO_VICT );

		snprintf ( buf, sizeof ( buf ), "$n gives %s to $N.", money_desc ( amount ) );
		act ( buf, TRUE, ch, 0, vict, TO_NOTVICT );
	}
	if ( IS_NPC ( ch ) || ( GET_LEVEL ( ch ) < LVL_GOD ) )
	{
		ch->Gold ( -amount, GOLD_HAND );
		gold_data ( GOLD_GIVEN, amount );
	}
	gold_data ( GOLD_RECEIVED, amount );
	vict->Gold ( amount, GOLD_HAND );

	bribe_mtrigger ( vict, ch, amount );
}


ACMD ( do_give )
{
	gold_int amount;
	char arg[MAX_INPUT_LENGTH];
	int dotmode;
	Character *vict = NULL;
	struct obj_data *obj, *next_obj;

	argument = one_argument ( argument, arg );

	if ( !*arg )
		ch->Send ( "Give what to who?\r\n" );
	else if ( is_number ( arg ) )
	{
		amount = atol ( arg );
		argument = one_argument ( argument, arg );
		if ( !str_cmp ( "coins", arg ) || !str_cmp ( "coin", arg ) )
		{
			one_argument ( argument, arg );
			if ( ( vict = give_find_vict ( ch, arg ) ) != NULL )
				perform_give_gold ( ch, vict, amount );
			return;
		}
		else if ( !*arg )     /* Give multiple code. */
		{
			ch->Send ( "What do you want to give %lld of?\r\n", amount );
		}
		else if ( ! ( vict = give_find_vict ( ch, argument ) ) )
		{
			return;
		}
		else
			if ( ! ( obj = get_obj_in_list_vis ( ch, arg, NULL, ch->carrying ) ) )
			{
				ch->Send ( "You don't seem to have any %ss.\r\n", arg );
			}
			else
			{
				while ( obj && amount-- )
				{
					next_obj =
					    get_obj_in_list_vis ( ch, arg, NULL, obj->next_content );
					perform_give ( ch, vict, obj );
					obj = next_obj;
				}
			}
	}
	else
	{
		char buf1[MAX_INPUT_LENGTH];
		one_argument ( argument, buf1 );
		if ( ! ( vict = give_find_vict ( ch, buf1 ) ) )
			return;
		dotmode = find_all_dots ( arg );
		if ( dotmode == FIND_INDIV )
		{
			if ( ! ( obj = get_obj_in_list_vis ( ch, arg, NULL, ch->carrying ) ) )
			{
				ch->Send ( "You don't seem to have %s %s.\r\n", AN ( arg ),
				           arg );
			}
			else
				perform_give ( ch, vict, obj );
		}
		else
		{
			if ( dotmode == FIND_ALLDOT && !*arg )
			{
				ch->Send ( "All of what?\r\n" );
				return;
			}
			if ( !ch->carrying )
				ch->Send ( "You don't seem to be holding anything.\r\n" );
			else
				for ( obj = ch->carrying; obj; obj = next_obj )
				{
					next_obj = obj->next_content;
					if ( CAN_SEE_OBJ ( ch, obj ) &&
					        ( ( dotmode == FIND_ALL || isname ( arg, obj->name ) ) ) )
						perform_give ( ch, vict, obj );
				}
		}
	}
	if ( vict )
		SET_BIT_AR ( PLR_FLAGS ( vict ), PLR_CRASH );
	SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_CRASH );
}



void weight_change_object ( struct obj_data *obj, int weight )
{
	struct obj_data *tmp_obj;
	Character *tmp_ch;

	if ( obj->in_room != NULL )
	{
		GET_OBJ_WEIGHT ( obj ) += weight;
	}
	else if ( ( tmp_ch = obj->carried_by ) )
	{
		obj_from_char ( obj );
		GET_OBJ_WEIGHT ( obj ) += weight;
		obj_to_char ( obj, tmp_ch );
	}
	else if ( ( tmp_obj = obj->in_obj ) )
	{
		obj_from_obj ( obj );
		GET_OBJ_WEIGHT ( obj ) += weight;
		obj_to_obj ( obj, tmp_obj );
	}
	else if ( ( tmp_ch = obj->worn_by ) )
	{
		int pos = obj->worn_on;
		obj = unequip_char ( tmp_ch, pos );
		GET_OBJ_WEIGHT ( obj ) += weight;
		equip_char ( tmp_ch, obj, pos );
	}
	else
	{
		log ( "SYSERR: Unknown attempt to subtract weight from an object." );
	}
}

void weight_to_object ( struct obj_data *obj, int weight )
{
	struct obj_data *tmp_obj;
	Character *tmp_ch;

	if ( obj->in_room != NULL )
	{
		GET_OBJ_WEIGHT ( obj ) = weight;
	}
	else if ( ( tmp_ch = obj->carried_by ) )
	{
		obj_from_char ( obj );
		GET_OBJ_WEIGHT ( obj ) = weight;
		obj_to_char ( obj, tmp_ch );
	}
	else if ( ( tmp_obj = obj->in_obj ) )
	{
		obj_from_obj ( obj );
		GET_OBJ_WEIGHT ( obj ) = weight;
		obj_to_obj ( obj, tmp_obj );
	}
	else if ( ( tmp_ch = obj->worn_by ) )
	{
		int pos = obj->worn_on;
		obj = unequip_char ( tmp_ch, pos );
		GET_OBJ_WEIGHT ( obj ) = weight;
		equip_char ( tmp_ch, obj, pos );

	}
	else
	{
		log ( "SYSERR: Unknown attempt to subtract weight from an object." );
	}
}

void name_from_drinkcon ( struct obj_data *obj )
{
	char *new_name, *cur_name, *next;
	const char *liqname;
	int liqlen, cpylen;
	return;
	if ( !obj
	        || ( GET_OBJ_TYPE ( obj ) != ITEM_DRINKCON
	             && GET_OBJ_TYPE ( obj ) != ITEM_FOUNTAIN ) )
		return;

	liqname = drinknames[GET_OBJ_VAL ( obj, 2 ) ];
	if ( !isname ( liqname, obj->name ) )
	{
		log ( "SYSERR: Can't remove liquid '%s' from '%s' (%d) item.",
		      liqname, obj->name, obj->item_number );
		return;
	}

	liqlen = strlen ( liqname );
	CREATE ( new_name, char, strlen ( obj->name ) - strlen ( liqname ) );   /* +1 for NUL, -1 for space */

	for ( cur_name = obj->name; cur_name; cur_name = next )
	{
		if ( *cur_name == ' ' )
			cur_name++;

		if ( ( next = strchr ( cur_name, ' ' ) ) )
			cpylen = next - cur_name;
		else
			cpylen = strlen ( cur_name );

		if ( !strn_cmp ( cur_name, liqname, liqlen ) )
			continue;

		if ( *new_name )
			strcat ( new_name, " " );  /* strcat: OK (size precalculated) */
		strncat ( new_name, cur_name, cpylen );  /* strncat: OK (size precalculated) */
	}

	if ( GET_OBJ_RNUM ( obj ) == NOTHING
	        || obj->name != obj_proto[GET_OBJ_RNUM ( obj ) ].name )
		free ( obj->name );
	obj->name = new_name;
}



void name_to_drinkcon ( struct obj_data *obj, int type )
{
	char *new_name;
	return;
	if ( !obj
	        || ( GET_OBJ_TYPE ( obj ) != ITEM_DRINKCON
	             && GET_OBJ_TYPE ( obj ) != ITEM_FOUNTAIN ) )
		return;

	CREATE ( new_name, char,
	         strlen ( obj->name ) + strlen ( drinknames[type] ) + 2 );
	sprintf ( new_name,  "%s %s", obj->name, drinknames[type] ); /* sprintf: OK */

	if ( GET_OBJ_RNUM ( obj ) == NOTHING
	        || obj->name != obj_proto[GET_OBJ_RNUM ( obj ) ].name )
		free ( obj->name );

	obj->name = new_name;
}



ACMD ( do_drink )
{
	struct obj_data *temp;
	struct affected_type af;
	int amount = 0, weight;
	int on_ground = 0;
	char arg[MAX_INPUT_LENGTH];
	char buf[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );

	if ( IS_NPC ( ch ) )   /* Cannot use GET_COND() on mobs. */
		return;

	if ( !*arg )
	{
		ch->Send ( "Drink from what?\r\n" );
		return;
	}
	if ( ! ( temp = get_obj_in_list_vis ( ch, arg, NULL, ch->carrying ) ) )
	{
		if ( !
		        ( temp =
		              get_obj_in_list_vis ( ch, arg, NULL,
		                                    IN_ROOM ( ch )->contents ) ) )
		{
			ch->Send ( "You can't find it!\r\n" );
			return;
		}
		else
			on_ground = 1;
	}
	if ( ( GET_OBJ_TYPE ( temp ) != ITEM_DRINKCON ) &&
	        ( GET_OBJ_TYPE ( temp ) != ITEM_FOUNTAIN ) &&
	        ( GET_OBJ_TYPE ( temp ) != ITEM_VIAL ) )
	{
		ch->Send ( "You can't drink from that!\r\n" );
		return;
	}
	if ( on_ground && ( GET_OBJ_TYPE ( temp ) != ITEM_FOUNTAIN ) )
	{
		ch->Send ( "You have to be holding that to drink from it.\r\n" );
		return;
	}

	if ( ( GET_OBJ_TYPE ( temp ) == ITEM_VIAL ) )
	{

		switch ( GET_OBJ_VAL ( temp, 0 ) )
		{
			case VIAL_NONE:
				ch->Send ( "It has nothing in it.\r\n" );
				return;
				break;
			case VIAL_HITP:
				if ( GET_HIT ( ch ) == GET_MAX_HIT ( ch ) )
				{
					ch->Send ( "Your health is already full.\r\n" );
					return;
				}
				break;
			case VIAL_MANA:
				if ( GET_MANA ( ch ) == GET_MAX_MANA ( ch ) )
				{
					ch->Send ( "Your mana energy is already full.\r\n" );
					return;
				}
				break;
			case VIAL_MOVE:
				if ( GET_MOVE ( ch ) == GET_MAX_MOVE ( ch ) )
				{
					ch->Send ( "Your move energy is already full.\r\n" );
					return;
				}
				break;
			case VIAL_STAM:
				if ( GET_STAMINA ( ch ) == GET_MAX_STAMINA ( ch ) )
				{
					ch->Send ( "Your stamina is already full.\r\n" );
					return;
				}
				break;
		}

	}
	else
	{
		if ( ( GET_COND ( ch, DRUNK ) > ( 5+ ( total_chance ( ch, SKILL_DRUNK ) / 4 ) ) ) && ( GET_COND ( ch, THIRST ) > 0 ) )
		{
			/* The pig is drunk */
			ch->Send ( "You can't seem to get close enough to your mouth.\r\n" );
			act ( "$n tries to drink but misses $s mouth!", TRUE, ch, 0, 0,
			      TO_ROOM );
			return;
		}
		if ( ( GET_COND ( ch, FULL ) > 47 ) && ( GET_COND ( ch, THIRST ) > 0 ) )
		{
			ch->Send ( "Your stomach can't contain anymore!\r\n" );
			return;
		}
	}
	if ( !GET_OBJ_VAL ( temp, 1 ) )
	{
		ch->Send ( "It's empty.\r\n" );
		return;
	}


	if ( consume_otrigger ( temp, ch, OCMD_DRINK ) <= 0 )  /* check trigger */
		return;

	if ( subcmd == SCMD_DRINK )
	{
		message_type = REST_MOVE;
		if ( GET_OBJ_TYPE ( temp ) != ITEM_VIAL )
		{
			snprintf ( buf, sizeof ( buf ), "$n drinks %s from $p.", drinks[GET_OBJ_VAL ( temp, 2 ) ] );
			act ( buf, TRUE, ch, temp, 0, TO_ROOM );
			message_type = NOTHING;
			ch->Send ( "You drink the %s.\r\n", drinks[GET_OBJ_VAL ( temp, 2 ) ] );

			if ( drink_aff[GET_OBJ_VAL ( temp, 2 ) ][DRUNK] > 0 )
			{
				amount = ( 49 - GET_COND ( ch, THIRST ) ) / drink_aff[GET_OBJ_VAL ( temp,2 ) ][DRUNK];
			}
			else
				amount = number ( 3, 10 );

		}
		else
		{
			const char *to_char = NULL;
			const char *to_room = NULL;
			switch ( GET_OBJ_VAL ( temp, 0 ) )
			{
				case VIAL_NONE:
					ch->Send ( "It has nothing in it." );
					return;
					break;
				case VIAL_HITP:
					amount = ( ( GET_HIT ( ch ) - GET_MAX_HIT ( ch ) ) < GET_OBJ_VAL ( temp, 1 ) ? ( GET_HIT ( ch ) - GET_MAX_HIT ( ch ) ) : GET_OBJ_VAL ( temp, 1 ) );
					to_char = "A million bright green sparks encircle your body as you drink.";
					to_room = "A million bright green sparks encircle $n's body as $e drinks.";
					break;
				case VIAL_MANA:
					amount = ( ( GET_MANA ( ch ) - GET_MAX_MANA ( ch ) ) < GET_OBJ_VAL ( temp, 1 ) ? ( GET_MANA ( ch ) - GET_MAX_MANA ( ch ) ) : GET_OBJ_VAL ( temp, 1 ) );
					to_char = "Dark purple flames flow into your body as you drink.";
					to_room = "Dark purple flames flow into $n's body as $e drinks.";
					break;
				case VIAL_MOVE:
					amount = ( ( GET_MOVE ( ch ) - GET_MAX_MOVE ( ch ) ) < GET_OBJ_VAL ( temp, 1 ) ? ( GET_MOVE ( ch ) - GET_MAX_MOVE ( ch ) ) : GET_OBJ_VAL ( temp, 1 ) );
					to_char = "Blue crystals form over your body as you drink.";
					to_room = "Blue crystals forms over $n's body as $e drinks.";
					break;
				case VIAL_STAM:
					amount = ( ( GET_STAMINA ( ch ) - GET_MAX_STAMINA ( ch ) ) < GET_OBJ_VAL ( temp, 1 ) ? ( GET_STAMINA ( ch ) - GET_MAX_STAMINA ( ch ) ) : GET_OBJ_VAL ( temp, 1 ) );
					to_char = "Thousands of black shadows slide into your body as you drink.";
					to_room = "Thousands of black shadows slide into $n's body as $e drinks.";
					break;
			}
			if ( to_char )
				act ( to_char, FALSE, ch, 0, 0, TO_CHAR );
			if ( to_room )
				act ( to_room, FALSE, ch, 0, 0, TO_ROOM );
		}

	}
	else
	{
		act ( "$n sips from $p.", TRUE, ch, temp, 0, TO_ROOM );
		ch->Send ( "It tastes like %s.\r\n",
		           ( GET_OBJ_TYPE ( temp ) == ITEM_VIAL ) ? vial_types[GET_OBJ_VAL ( temp, 0 ) ] : drinks[GET_OBJ_VAL ( temp, 2 ) ] );
		amount = ( int ) ( ( float ) GET_OBJ_VAL ( temp, 1 ) /10.0 );
	}

	amount = MIN ( amount, GET_OBJ_VAL ( temp, 1 ) );

	/* You can't subtract more than the object weighs */
	if ( GET_OBJ_TYPE ( temp ) == ITEM_VIAL )
	{
		weight = GET_OBJ_WEIGHT ( temp ) - ( ( ( GET_OBJ_WEIGHT ( temp ) * 100 ) - amount ) /100 );
	}
	else
	{
		weight = MIN ( amount, GET_OBJ_WEIGHT ( temp ) );
	}

	weight_change_object ( temp, -weight );  /* Subtract amount */

	if ( GET_OBJ_TYPE ( temp ) == ITEM_VIAL )
	{
		switch ( GET_OBJ_VAL ( temp, 0 ) )
		{
			case VIAL_NONE:
				ch->Send ( "It has nothing in it.\r\n" );
				return;
				break;
			case VIAL_HITP:
				alter_hit ( ch, -amount );
				break;
			case VIAL_MANA:
				alter_mana ( ch, -amount );
				break;
			case VIAL_MOVE:
				alter_move ( ch, -amount );
				break;
			case VIAL_STAM:
				alter_stamina ( ch, -amount );
				break;
		}
		update_pos ( ch );
	}
	else
	{

		if ( drink_aff[GET_OBJ_VAL ( temp, 2 ) ][DRUNK] && number ( 1, 100 ) < total_chance ( ch, SKILL_DRUNK ) )
		{
			improve_skill ( ch, SKILL_DRUNK );
			gain_condition ( ch, DRUNK,  drink_aff[GET_OBJ_VAL ( temp, 2 ) ][DRUNK]  * amount / 4 );
		}
		gain_condition ( ch, FULL,   drink_aff[GET_OBJ_VAL ( temp, 2 ) ][FULL]   * amount / 4 );
		gain_condition ( ch, THIRST, drink_aff[GET_OBJ_VAL ( temp, 2 ) ][THIRST] * amount / 4 );

		if ( GET_COND ( ch, DRUNK ) > 10 )
			ch->Send ( "You feel drunk.\r\n" );

		if ( GET_COND ( ch, THIRST ) > 47 )
			ch->Send ( "You REALLY don't feel thirsty any more.\r\n" );
		else if ( GET_COND ( ch, THIRST ) > 23 )
			ch->Send ( "You don't feel thirsty any more.\r\n" );
		if ( GET_COND ( ch, FULL ) > 47 )
			ch->Send ( "You REALLY don't feel hungry any more.\r\n" );
		else if ( GET_COND ( ch, FULL ) > 23 )
			ch->Send ( "You are full.\r\n" );

		if ( GET_OBJ_VAL ( temp, 3 ) )  /* The shit was poisoned ! */
		{
			ch->Send ( "Oops, it tasted rather strange!\r\n" );
			act ( "$n chokes and utters some strange sounds.", TRUE, ch, 0, 0, TO_ROOM );

			af.type = SPELL_POISON;
			af.expire = HOURS_TO_EXPIRE ( ( amount * 3 ) );
			af.modifier = 0;
			af.location = APPLY_NONE;
			af.bitvector = AFF_POISON_1;
			affect_join ( ch, &af, FALSE, FALSE, FALSE, FALSE );
		}

	}
	/* empty the container, and no longer poison. */
	GET_OBJ_VAL ( temp, 1 ) -= amount;
	if ( !GET_OBJ_VAL ( temp, 1 ) )    /* The last bit */
	{
		if ( GET_OBJ_TYPE ( temp ) == ITEM_VIAL )
		{
			GET_OBJ_VAL ( temp, 0 ) = -1;
		}
		else
		{
			GET_OBJ_VAL ( temp, 2 ) = 0;
			GET_OBJ_VAL ( temp, 3 ) = 0;
			name_from_drinkcon ( temp );
		}
	}
	return;
}



ACMD ( do_eat )
{
	struct obj_data *food;
	struct affected_type af;
	int amount;
	char arg[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );

	if ( IS_NPC ( ch ) )   /* Cannot use GET_COND() on mobs. */
		return;

	if ( !*arg )
	{
		ch->Send ( "Eat what?\r\n" );
		return;
	}
	if ( ! ( food = get_obj_in_list_vis ( ch, arg, NULL, ch->carrying ) ) )
	{
		ch->Send ( "You don't seem to have %s %s.\r\n", AN ( arg ), arg );
		return;
	}
	if ( subcmd == SCMD_TASTE && ( ( GET_OBJ_TYPE ( food ) == ITEM_DRINKCON ) ||
	                               ( GET_OBJ_TYPE ( food ) == ITEM_FOUNTAIN ) ) )
	{
		do_drink ( ch, argument, 0, SCMD_SIP );
		return;
	}
	if ( ( GET_OBJ_TYPE ( food ) != ITEM_FOOD ) && ( GET_LEVEL ( ch ) < LVL_GOD ) )
	{
		ch->Send ( "You can't eat THAT!\r\n" );
		return;
	}
	if ( GET_COND ( ch, FULL ) > 47 )    /* Stomach full */
	{
		ch->Send ( "You are too full to eat more!\r\n" );
		return;
	}

	if ( consume_otrigger ( food, ch, OCMD_EAT ) <= 0 )  /* check trigger */
		return;

	if ( subcmd == SCMD_EAT )
	{
		act ( "You eat $p.", FALSE, ch, food, 0, TO_CHAR );
		act ( "$n eats $p.", TRUE, ch, food, 0, TO_ROOM );
	}
	else
	{
		act ( "You nibble a little bit of $p.", FALSE, ch, food, 0, TO_CHAR );
		act ( "$n tastes a little bit of $p.", TRUE, ch, food, 0, TO_ROOM );
	}

	amount = ( subcmd == SCMD_EAT ? GET_OBJ_VAL ( food, 0 ) : 1 );

	gain_condition ( ch, FULL, amount );
	if ( GET_COND ( ch, FULL ) > 47 )
		ch->Send ( "You are REALLY full.\r\n" );
	else if ( GET_COND ( ch, FULL ) > 20 )
		ch->Send ( "You are full.\r\n" );

	if ( GET_OBJ_VAL ( food, 3 ) && ( GET_LEVEL ( ch ) < LVL_GOD ) )
	{
		/* The shit was poisoned ! */
		ch->Send ( "Oops, that tasted rather strange!\r\n" );
		act ( "$n coughs and utters some strange sounds.", FALSE, ch, 0, 0,
		      TO_ROOM );

		af.type = SPELL_POISON;
		af.expire = HOURS_TO_EXPIRE ( ( amount * 2 ) );
		af.modifier = 0;
		af.location = APPLY_NONE;
		af.bitvector = AFF_POISON_1;
		affect_join ( ch, &af, FALSE, FALSE, FALSE, FALSE );
	}
	if ( subcmd == SCMD_EAT )
		extract_obj ( food );
	else
	{
		if ( ! ( --GET_OBJ_VAL ( food, 0 ) ) )
		{
			ch->Send ( "There's nothing left now.\r\n" );
			extract_obj ( food );
		}
	}
}


ACMD ( do_analyze )
{
	struct obj_data *item;
	struct obj_data *item2;
	char arg[MAX_INPUT_LENGTH];
        char gemname[MAX_INPUT_LENGTH];
	int i;

	one_argument ( argument, arg );

	if ( IS_NPC ( ch ) )   /* Cannot use GET_COND() on mobs. */
		return;

	if ( !*arg )
	{
		ch->Send ( "Analyze what exactly?\r\n" );
		return;
	}
	if ( ! ( item = get_obj_in_list_vis ( ch, arg, NULL, ch->carrying ) ) )
	{
		ch->Send ( "You don't seem to have %s %s.\r\n", AN ( arg ), arg );
		return;
	}	
	if (OBJ_FLAGGED(item, ITEM_NORENT) || OBJ_FLAGGED(item, ITEM_ARTIFACT))
     {
		ch->Send ( "You can not analyze that.\r\n");
		return;
	}
	ch->Send ( "You begin to analyze the object.\r\n");
	if (!number(0, 10))
	{
	ch->Send ( "Your analysis returns a breakthrough!\r\n");
        item2 = create_obj ( NOTHING );
                        snprintf ( gemname, sizeof ( gemname ), "a template of %s", item->short_description );
                        item2->name = strdup("template");
                        item2->short_description = str_dup ( gemname );
                        item2->description = strdup("A crafting template is lying upon the ground here.");
                        GET_OBJ_TYPE(item2) = ITEM_TEMPLATE;
                        SET_BIT_AR ( GET_OBJ_WEAR ( item2 ), ITEM_WEAR_TAKE);
			SET_BIT_AR ( GET_OBJ_EXTRA ( item2 ), ITEM_NORENT);
	   for (i = 0; i < MAX_OBJ_AFFECT; i++) {
        if ((item->affected[i].location != APPLY_NONE) &&
                (item->affected[i].modifier != 0)) { 
	item2->affected[i].location = item->affected[i].location;
	item2->affected[i].modifier = item->affected[i].modifier;
     }
    }
	obj_to_char(item2, ch);
   }
    ch->Send("%s crumbles into dust.\r\n", item->short_description);
	extract_obj(item);

}

ACMD ( do_pour )
{
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	struct obj_data *from_obj = NULL, *to_obj = NULL;
	int amount = 0, type = VIAL_NONE;

	two_arguments ( argument, arg1, arg2 );

	if ( subcmd == SCMD_POUR )
	{
		if ( !*arg1 )          /* No arguments */
		{
			ch->Send ( "From what do you want to pour?\r\n" );
			return;
		}
		if ( !
		        ( from_obj =
		              get_obj_in_list_vis ( ch, arg1, NULL, ch->carrying ) ) )
		{
			ch->Send ( "You can't find it!\r\n" );
			return;
		}
		if ( GET_OBJ_TYPE ( from_obj ) != ITEM_DRINKCON && GET_OBJ_TYPE ( from_obj ) != ITEM_VIAL && GET_OBJ_TYPE ( from_obj ) != ITEM_POTION )
		{
			ch->Send ( "You can't pour from that!\r\n" );
			return;
		}
	}
	else if ( subcmd == SCMD_FILL )
	{
		if ( !*arg1 )          /* no arguments */
		{
			ch->Send ( "What do you want to fill?  And what are you filling it from?\r\n" );
			return;
		}
		if ( ! ( to_obj = get_obj_in_list_vis ( ch, arg1, NULL, ch->carrying ) ) )
		{
			ch->Send ( "You can't find it!" );
			return;
		}
		if ( GET_OBJ_TYPE ( to_obj ) != ITEM_DRINKCON )
		{
			act ( "You can't fill $p!", FALSE, ch, to_obj, 0, TO_CHAR );
			return;
		}
		if ( !*arg2 )          /* no 2nd argument */
		{
			act ( "What do you want to fill $p from?", FALSE, ch, to_obj, 0,
			      TO_CHAR );
			return;
		}
		if ( !
		        ( from_obj =
		              get_obj_in_list_vis ( ch, arg2, NULL,
		                                    IN_ROOM ( ch )->contents ) ) )
		{
			ch->Send ( "There doesn't seem to be %s %s here.\r\n",
			           AN ( arg2 ), arg2 );
			return;
		}
		if ( GET_OBJ_TYPE ( from_obj ) != ITEM_FOUNTAIN )
		{
			act ( "You can't fill something from $p.", FALSE, ch, from_obj,
			      0, TO_CHAR );
			return;
		}
	}
	if ( GET_OBJ_VAL ( from_obj, 1 ) == 0 && GET_OBJ_TYPE ( from_obj ) != ITEM_POTION )
	{
		act ( "The $p is empty.", FALSE, ch, from_obj, 0, TO_CHAR );
		return;
	}
	if ( subcmd == SCMD_POUR )  /* pour */
	{
		if ( !*arg2 )
		{
			ch->Send ( "Where do you want it?  Out or in what?\r\n" );
			return;
		}
		if ( !str_cmp ( arg2, "out" ) )
		{
			act ( "$n empties $p.", TRUE, ch, from_obj, 0, TO_ROOM );
			act ( "You empty $p.", FALSE, ch, from_obj, 0, TO_CHAR );
			if ( GET_OBJ_TYPE ( from_obj ) == ITEM_VIAL )
				weight_change_object ( from_obj, GET_OBJ_WEIGHT ( from_obj ) );
			else if ( GET_OBJ_TYPE ( from_obj ) == ITEM_POTION )
				GET_OBJ_WEIGHT ( from_obj ) =check_potion_weight ( from_obj );
			else
				weight_change_object ( from_obj, -GET_OBJ_VAL ( from_obj, 1 ) );    /* Empty */
			if ( GET_OBJ_TYPE ( from_obj ) == ITEM_VIAL )
				GET_OBJ_VAL ( from_obj, 0 ) = -1; /* vial type */
			else if ( GET_OBJ_TYPE ( from_obj ) == ITEM_POTION )
			{
				obj_vnum vialvnum;
				vialvnum=3044;
				extract_obj ( from_obj );
				from_obj = read_object ( vialvnum, VIRTUAL );
				if ( from_obj )
					obj_to_char ( from_obj, ch );
				else
					return;
			}
			else
			{
				GET_OBJ_VAL ( from_obj, 2 ) = 0;
				GET_OBJ_VAL ( from_obj, 3 ) = 0;
				name_from_drinkcon ( from_obj );
			}
			/* amount*/
			if ( GET_OBJ_TYPE ( from_obj ) !=ITEM_POTION )
				GET_OBJ_VAL ( from_obj, 1 ) = 0;

			return;
		}
		if ( ! ( to_obj = get_obj_in_list_vis ( ch, arg2, NULL, ch->carrying ) ) )
		{
			ch->Send ( "You can't find it!\r\n" );
			return;
		}
		if ( ( GET_OBJ_TYPE ( to_obj ) != ITEM_DRINKCON ) &&
		        ( GET_OBJ_TYPE ( to_obj ) != ITEM_FOUNTAIN ) &&
		        ( GET_OBJ_TYPE ( to_obj ) != ITEM_VIAL ) )
		{
			ch->Send ( "You can't pour anything into that.\r\n" );
			return;
		}
	}
	if ( to_obj == from_obj )
	{
		ch->Send ( "A most unproductive effort.\r\n" );
		return;
	}
	if ( ( GET_OBJ_TYPE ( to_obj ) != ITEM_VIAL ) )
	{
		if ( GET_OBJ_TYPE ( from_obj ) == ITEM_POTION )
		{
			act ( "That is not a vial.", FALSE, ch, 0, 0, TO_CHAR );
			return;
		}
		if ( ( GET_OBJ_VAL ( to_obj, 1 ) != 0 ) &&
		        ( GET_OBJ_VAL ( to_obj, 2 ) != GET_OBJ_VAL ( from_obj, 2 ) ) )
		{
			ch->Send ( "There is already another liquid in it!\r\n" );
			return;
		}

		if ( ! ( GET_OBJ_VAL ( to_obj, 1 ) < GET_OBJ_VAL ( to_obj, 0 ) ) )
		{
			ch->Send ( "There is no room for more.\r\n" );
			return;
		}
	}
	else
	{

		if ( GET_OBJ_VAL ( to_obj, 2 ) == GET_OBJ_VAL ( to_obj, 1 ) )
		{
			act ( "No more can be fit into $p for now.", FALSE, ch, to_obj, 0, TO_CHAR );
			return;
		}
		if ( GET_OBJ_TYPE ( from_obj ) == ITEM_VIAL && GET_OBJ_VAL ( to_obj, 0 ) != VIAL_NONE &&
		        ( GET_OBJ_VAL ( from_obj, 0 ) != GET_OBJ_VAL ( to_obj, 0 ) ) )
		{
			act ( "Those two vials are of different energys and can't be mixed.", FALSE, ch, 0, 0, TO_CHAR );
			return;
		}
		if ( GET_OBJ_TYPE ( from_obj ) == ITEM_POTION && GET_OBJ_TYPE ( to_obj ) == ITEM_VIAL )
		{
			int i;
			for ( i=1;i<4;i++ )
			{
				switch ( GET_OBJ_VAL ( from_obj, i ) )
				{
					case SPELL_HEAL:
						if ( !type || type == VIAL_HITP )
						{
							amount += 300;
							type = VIAL_HITP;
						}
						break;
					case SPELL_CURE_CRITIC:
						if ( !type || type == VIAL_HITP )
						{
							amount += 150;
							type = VIAL_HITP;
						}
						break;
					case SPELL_CURE_LIGHT:
						if ( !type || type == VIAL_HITP )
						{
							amount += 75;
							type = VIAL_HITP;
						}
						break;
					case SPELL_VITALIZE:
						if ( number ( 0, 1 ) )
						{
							if ( !type || type == VIAL_MOVE )
							{
								amount += 100;
								type = VIAL_MOVE;
							}
						}
						else
						{
							if ( !type || type == VIAL_STAM )
							{
								amount += 20;
								type = VIAL_STAM;
							}
						}

						break;
				}
			}
			if ( amount == 0 && type == VIAL_NONE )
			{
				act ( "That potion does not have the properties needed to fill $p.", FALSE, ch, to_obj, 0, TO_CHAR );
				return;
			}
		}
		/* vials*/
	}
	if ( subcmd == SCMD_POUR )
	{
		ch->Send ( "You pour the %s into the %s.",
		           ( GET_OBJ_TYPE ( to_obj ) == ITEM_DRINKCON ) ? drinks[GET_OBJ_VAL ( from_obj, 2 ) ] : from_obj->short_description, to_obj->short_description );
	}
	else if ( subcmd == SCMD_FILL )
	{
		act ( "You gently fill $p from $P.", FALSE, ch, to_obj, from_obj,TO_CHAR );
		act ( "$n gently fills $p from $P.", TRUE, ch, to_obj, from_obj,TO_ROOM );
	}
	/* New alias */
	if ( GET_OBJ_TYPE ( to_obj ) == ITEM_DRINKCON )
	{
		if ( GET_OBJ_VAL ( to_obj, 1 ) == 0 )
			name_to_drinkcon ( to_obj, GET_OBJ_VAL ( from_obj, 2 ) );

		/* First same type liq. */
		GET_OBJ_VAL ( to_obj, 2 ) = GET_OBJ_VAL ( from_obj, 2 );


		/* Then how much to pour */
		GET_OBJ_VAL ( from_obj, 1 ) -= ( amount =
		                                     ( GET_OBJ_VAL ( to_obj, 0 ) -
		                                       GET_OBJ_VAL ( to_obj, 1 ) ) );

		GET_OBJ_VAL ( to_obj, 1 ) = GET_OBJ_VAL ( to_obj, 0 );

		if ( GET_OBJ_VAL ( from_obj, 1 ) < 0 )  /* There was too little */
		{
			GET_OBJ_VAL ( to_obj, 1 ) += GET_OBJ_VAL ( from_obj, 1 );
			amount += GET_OBJ_VAL ( from_obj, 1 );
			GET_OBJ_VAL ( from_obj, 1 ) = 0;
			GET_OBJ_VAL ( from_obj, 2 ) = 0;
			GET_OBJ_VAL ( from_obj, 3 ) = 0;
			name_from_drinkcon ( from_obj );
		}
		/* Then the poison boogie */
		GET_OBJ_VAL ( to_obj, 3 ) =
		    ( GET_OBJ_VAL ( to_obj, 3 ) || GET_OBJ_VAL ( from_obj, 3 ) );

		/* And the weight boogie */
		weight_change_object ( from_obj, -amount );
		weight_change_object ( to_obj, amount ); /* Add weight */
	}
	else
	{
		int weight;
		if ( !amount )
		{
			GET_OBJ_VAL ( from_obj, 1 ) -= ( amount = ( MAX ( GET_OBJ_VAL ( to_obj, 2 ) - GET_OBJ_VAL ( to_obj, 1 ) , GET_OBJ_VAL ( from_obj, 1 ) ) ) );
			GET_OBJ_VAL ( to_obj, 1 ) += amount;
			GET_OBJ_VAL ( to_obj, 0 ) = GET_OBJ_VAL ( from_obj, 0 );
		}
		else
		{
			extract_obj ( from_obj );
			from_obj = NULL;
			GET_OBJ_VAL ( to_obj, 1 ) += amount;
			GET_OBJ_VAL ( to_obj, 1 ) = MAX ( GET_OBJ_VAL ( to_obj, 2 ), GET_OBJ_VAL ( to_obj, 1 ) );
			GET_OBJ_VAL ( to_obj, 0 ) = type;
		}
		/* And the weight boogie */
		weight = GET_OBJ_WEIGHT ( to_obj );
		weight *= 100;
		weight = FTOI ( ( GET_OBJ_VAL ( to_obj, 1 ) - weight ) /100.0 );
		weight_change_object ( to_obj, weight ); /* Add weight */
		weight = GET_OBJ_WEIGHT ( from_obj );
		weight *= 100;
		weight = FTOI ( ( weight - GET_OBJ_VAL ( from_obj, 1 ) ) /100.0 );
		weight_change_object ( from_obj, -weight ); /* subtract weight */
	}

}
ACMD ( do_energize )
{
	const char *to_char = NULL;
	const char *to_room = NULL;
	int  amount = 0, type = VIAL_NONE;
	char arg[MAX_INPUT_LENGTH], arg1[MAX_INPUT_LENGTH];
	OBJ_DATA *temp;
	two_arguments ( argument, arg, arg1 );

	if ( !*arg )
	{
		ch->Send ( "USAGE: energize <item> [HIT|MANA|MOVE|STAMINA]\r\n" );
		return;
	}
	if ( ( temp = get_obj_in_list_vis ( ch, arg, NULL, ch->carrying ) ) == NULL )
	{
		ch->Send ( "Nothing about by that name.\r\n" );
		return;
	}
	if ( GET_OBJ_TYPE ( temp ) != ITEM_VIAL )
	{
		ch->Send ( "You can't energize that item!\r\n" );
		return;
	}

	if ( GET_OBJ_VAL ( temp, 0 ) == VIAL_NONE )
	{
		ch->Send ( "That is an empty vial - you must specify a new type for it." );
		return;
	}
	else
	{
		if ( is_abbrev ( arg1, "hitpoints" ) || is_abbrev ( arg1, "health" ) )
			type = VIAL_HITP;
		else if ( is_abbrev ( arg1, "mana" ) )
			type = VIAL_MANA;
		else if ( is_abbrev ( arg1, "move" ) )
			type = VIAL_MOVE;
		else if ( is_abbrev ( arg1, "stamina" ) )
			type = VIAL_STAM;

		if ( type == VIAL_NONE )
		{
			ch->Send ( "Sorry, I don't recognise that type.\r\n" );
			return;
		}

	}

	type = type ? type : GET_OBJ_VAL ( temp, 0 );
	switch ( type )
	{
		case VIAL_NONE:
			ch->Send ( "It has nothing in it." );
			return;
			break;
		case VIAL_HITP:
			amount = MAX ( 900, ( int ) ( GET_HIT ( ch ) * 0.25 ) );
			to_char = "A million bright green sparks leave your body.";
			to_room = "A million bright green sparks leave $n's body.";
			break;
		case VIAL_MANA:
			amount = MAX ( 3000, ( int ) ( GET_MANA ( ch ) * 0.25 ) );
			to_char = "Dark purple flames flow out of your body.";
			to_room = "Dark purple flames flow out of $n's body.";
			break;
		case VIAL_MOVE:
			amount = MAX ( 1000, ( int ) ( GET_MOVE ( ch ) * 0.25 ) );
			to_char = "Blue crystals sluice from your body.";
			to_room = "Blue crystals sluices from $n's body.";
			break;
		case VIAL_STAM:
			amount = MAX ( 50, ( int ) ( GET_STAMINA ( ch ) * 0.25 ) );
			to_char = "Thousands of black shadows slide out of your body.";
			to_room = "Thousands of black shadows slide out of $n's body.";
			break;
	}
	if ( amount <= 0 )
	{
		act ( "You are too weak to do that.", FALSE, ch, 0, 0, TO_CHAR );
		return;
	}
	if ( to_char )
		act ( to_char, FALSE, ch, 0, 0, TO_CHAR );
	if ( to_room )
		act ( to_room, FALSE, ch, 0, 0, TO_ROOM );

	act ( "$p glows bright with a flash!", FALSE, ch, temp, 0, TO_CHAR );
	act ( "$p glows bright with a flash!", FALSE, ch, temp, 0, TO_ROOM );

	amount = MAX ( GET_OBJ_VAL ( temp, 2 ) - GET_OBJ_VAL ( temp, 1 ), amount );

	GET_OBJ_VAL ( temp, 0 ) = type;
	GET_OBJ_VAL ( temp, 1 ) += amount;
	amount *= 4;

	switch ( type )
	{
		case VIAL_NONE:
			return;
			break;
		case VIAL_HITP:
			alter_hit ( ch, -amount );
			break;
		case VIAL_MANA:
			alter_mana ( ch, -amount );
			break;
		case VIAL_MOVE:
			alter_move ( ch, -amount );
			break;
		case VIAL_STAM:
			alter_stamina ( ch, -amount );
			break;
	}


}


OBJ_DATA * create_vial ( void )
{

	struct obj_data *vial;
	int type = VIAL_NONE;
	int size = 0;
	register int i;

	for ( i = 0; i < 2000;i++ )
	{
		if ( ! ( number ( 0, 10 ) ) )
			size++;
	}

	type = number ( VIAL_HITP, VIAL_STAM );
	vial = create_obj ( NOTHING );

	GET_OBJ_TYPE ( vial ) = ITEM_VIAL;
	SET_BIT_AR ( GET_OBJ_EXTRA ( vial ), ITEM_UNIQUE_SAVE );
	GET_OBJ_VAL ( vial, 0 ) = type;
	GET_OBJ_VAL ( vial, 1 ) = number ( 0, size );
	GET_OBJ_VAL ( vial, 2 ) = size;
	GET_OBJ_VAL ( vial, 3 ) = 0;
	GET_OBJ_COST ( vial ) = 1000 * size;
	GET_OBJ_WEIGHT ( vial ) = FTOI ( ( float ) GET_OBJ_VAL ( vial, 1 ) /100.0 );
	GET_OBJ_RENT ( vial ) = 0;
	GET_OBJ_TIMER ( vial ) = -1;

	return ( vial );

}

void update_timer ( struct obj_data *obj )
{
	struct timer_event_data *tmr;
	unsigned long t;
	time_t ct = 0;

	if ( !obj )
		return;
	if ( GET_OBJ_TIMER ( obj ) != -1 )
		GET_OBJ_EXPIRE ( obj ) = ( GET_OBJ_TIMER ( obj ) * SECS_PER_MUD_HOUR ) + time ( 0 );

	ct = time ( 0 );

	t = PASSES_PER_SEC * ( GET_OBJ_EXPIRE ( obj ) - ct );

	if ( GET_TIMER_EVENT ( obj ) == NULL ||
	        ( t < event_time ( GET_TIMER_EVENT ( obj ) ) ) )
	{

		/* take off old event, create updated event */
		if ( GET_TIMER_EVENT ( obj ) != NULL )
			event_cancel ( GET_TIMER_EVENT ( obj ) );
		else
			GET_TIMER_EVENT ( obj ) = NULL;

		tmr = new timer_event_data ( obj );
		GET_TIMER_EVENT ( obj ) = event_create ( timer_event, tmr, t, EVENT_TYPE_TIMER );
	}
}


void wear_message ( Character *ch, struct obj_data *obj, int where )
{
	const char *wear_messages[][2] =
	{
		{"$n lights $p and holds it.",
			"You light $p and hold it."
		},

		{"$n slides $p on to $s right ring finger.",
		 "You slide $p on to your right ring finger."},

		{"$n slides $p on to $s left ring finger.",
		 "You slide $p on to your left ring finger."},

		{"$n wears $p around $s neck.",
		 "You wear $p around your neck."},

		{"$n wears $p around $s neck.",
		 "You wear $p around your neck."},

		{"$n wears $p on $s body.",
		 "You wear $p on your body."},

		{"$n wears $p on $s head.",
		 "You wear $p on your head."},

		{"$n puts $p on $s legs.",
		 "You put $p on your legs."},

		{"$n wears $p on $s feet.",
		 "You wear $p on your feet."},

		{"$n puts $p on $s hands.",
		 "You put $p on your hands."},

		{"$n wears $p on $s arms.",
		 "You wear $p on your arms."},

		{"$n straps $p around $s arm as a shield.",
		 "You start to use $p as a shield."},

		{"$n wears $p about $s body.",
		 "You wear $p around your body."},

		{"$n wears $p around $s waist.",
		 "You wear $p around your waist."},

		{"$n puts $p on around $s right wrist.",
		 "You put $p on around your right wrist."},

		{"$n puts $p on around $s left wrist.",
		 "You put $p on around your left wrist."},

		{"$n wields $p.",
		 "You wield $p."},

		{"$n grabs $p.",
		 "You grab $p."},

		{"$n wears $p on $s face.",
		 "You wear $p on your face."},

		{"$n wears $p over $s eyes.",
		 "You wear $p over your eyes."},

		{"$n wears $p on $s hips.",
		 "You wear $p on your hips."},

		{"$n sticks $p in $s ear.",
		 "You stick $p in your ear."},

		{"$n sticks $p in $s ear.",
		 "You stick $p in your ear."},

		{"$n wears $p on $s right ankle.",
		 "You wear $p on your right ankle."},

		{"$n wears $p on $s left ankle.",
		 "You wear $p on your left ankle."},

		{"$n wears $p on $s horns.",
		 "You wear $p on your horns."},

		{"$n wears $p on $s antenna.",
		 "You wear $p on your antenna."},

		{"$n slips $p on $s tail.",
		 "You slip $p on your tail."},

		{"$n wields $p as a secondary weapon.",
		 "You wield $p as a secondary weapon."},

		{"$n wears $p on $s hind legs.",
		 "You wear $p on your hind legs."},

		{"$n wears $p on $s hind feet.",
		 "You wear $p on your hind feet."},

		{"$n starts focusing on $p.",
		 "You start focusing on $p."},

		{"$n wears $p on $s left thumb.",
		 "You wear $p on your left thumb."},

		{"$n wears $p on $s right thumb.",
		 "You wear $p on your right thumb."},

		{"$n wears $p over $s back.",
		 "You wear $p over your back."},

		{"$n wears $p through $s ear tip.",
		 "You wear $p through your ear tip."},

		{"$n wears $p on $s left shoulder.",
		 "You wear $p on your left shoulder."},

		{"$n wears $p on $s right shoulder.",
		 "You wear $p on your right shoulder."},

		{"$n wears $p as a crest.",
		 "You wear $p as a crest."},

		{"$n straps $p on $s left thigh.",
		 "You strap $p on your left thigh."},

		{"$n straps $p on $s right thigh.",
		 "You strap $p on your right thigh."},

		{"$n binds $p on to $s left knee.",
		 "You bind $p on to your left knee."},

		{"$n binds $p on to $s right knee.",
		 "You bind $p on to your right knee."},

		{"$n lets $p go and it floats beside $m.",
		 "You let $p go and it floats beside you."}
	};

	if ( GET_EQ ( ch, WEAR_FOCUS ) && where == WEAR_SHIELD )
	{
		act ( "$n concentrates and $p floats up infront of $m.", TRUE, ch, obj, 0, TO_ROOM );
		act ( "You concentrate and $p floats up infront of you.", FALSE, ch, obj, 0, TO_CHAR );
	}
	else
	{
		act ( wear_messages[where][0], TRUE, ch, obj, 0, TO_ROOM );
		act ( wear_messages[where][1], FALSE, ch, obj, 0, TO_CHAR );
	}
}



void perform_wear ( Character *ch, struct obj_data *obj, int where )
{
	/*
	 * ITEM_WEAR_TAKE is used for objects that do not require special bits
	 * to be put into that position (e.g. you can hold any object, not just
	 * an object with a HOLD bit.)
	 */



	static const char *already_wearing[] =
	{
		"You're already using a light.\r\n",
		"YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT.\r\n",
		"You're already wearing something on both of your ring fingers.\r\n",
		"You can't wear anything else around your neck.\r\n",
		"You can't wear anything else around your neck.\r\n",
		"You're already wearing something on your body.\r\n",
		"You're already wearing something on your head.\r\n",
		"You're already wearing something on your legs.\r\n",
		"You're already wearing something on your feet.\r\n",
		"You're already wearing something on your hands.\r\n",
		"You're already wearing something on your arms.\r\n",
		"You're already using a shield.\r\n",
		"You're already wearing something about your body.\r\n",
		"You already have something around your waist.\r\n",
		"YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT.\r\n",
		"You're already wearing something around both of your wrists.\r\n",
		"You're already wielding a weapon.\r\n",
		"You're already holding something.\r\n",
		"You're already wearing something on your face.\r\n",
		"You're already wearing something on your eyes.\r\n",
		"You're already wearing something on your hips.\r\n",
		"YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT.\r\n",
		"You're already wearing something in both your ears.\r\n",
		"YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT.\r\n",
		"You're already wearing something on both your ankles.\r\n",
		"You're already wearing something on your horns.\r\n",
		"You're already wearing something on your antenna.\r\n",
		"You're already wearing something on your tail.\r\n",
		"You're already wielding a second weapon.\r\n",
		"You're already wearing something on your hind legs.\r\n",
		"You're already wearing something on your hind feet.\r\n",
		"You're already focusing on something.\r\n",
		"---------BAD MESSAGE----------\r\n",
		"You're already wearing something on both your thumbs.\r\n",
		"You're already saddled.\r\n",
		"You're already wearing something through the tip of your ear.\r\n",
		"---------BAD MESSAGE-----------\r\n",
		"You're already wearing something on both your shoulders.\r\n",
		"You're already wearing something as your crest.\r\n",
		"---------BAD MESSAGE-----------\r\n",
		"You're already wearing something on both your thighs.\r\n",
		"---------BAD_MESSAGE-----------\r\n",
		"You're already wearing something on both your knees.\r\n",
		"You're already have something floating around you.\r\n"
	};

	if ( !HAS_BODY ( ch, where ) )
	{
		ch->Send ( "You do not have that body position.\r\n" );
		return;
	}

	/* first, make sure that the wear position is valid. */
	if ( !can_wear_on_pos ( obj, where ) )
	{
		act ( "You can't wear $p there.", FALSE, ch, obj, 0, TO_CHAR );
		return;
	}

	/*
	if ((IS_CARRYING_W(ch) + GET_OBJ_WEIGHT(obj)) > CAN_CARRY_W(ch)) {
	    act("You aren't strong enough to wear $p at the moment.", FALSE, ch, obj, 0, TO_CHAR);
	    return;
	}
	*/
	if ( obj->owner != 0 && !IS_NPC ( ch ) && GET_IDNUM ( ch ) != obj->owner )
	{
		ch->Send ( "You can't wear that! It is owned by %s\r\n", pi.NameById ( obj->owner ) );
		return;
	}

	/* for neck, finger, and wrist, try pos 2 if pos 1 is already full */
	if ( ( where == WEAR_FINGER_R ) || ( where == WEAR_NECK_1 )
	        || ( where ==  WEAR_WRIST_R ) || ( where == WEAR_EAR_R )
	        || ( where ==  WEAR_ANKLE_R ) || ( where == WEAR_THUMB_R )
	        || ( where == WEAR_SHOULDER_L ) || ( where == WEAR_THIGH_L )
	        || ( where == WEAR_KNEE_L ) )
		if ( GET_EQ ( ch, where ) && HAS_BODY ( ch, ( where + 1 ) ) )
			where++;

	if ( where == WEAR_FINGER_L )
	{
		if ( GET_EQ ( ch, where ) && HAS_BODY ( ch, WEAR_THUMB_R ) )
			where = WEAR_THUMB_R;
		if ( GET_EQ ( ch, where ) && HAS_BODY ( ch, WEAR_THUMB_L ) )
			where = WEAR_THUMB_L;
	}

	if ( where == WEAR_LEGS )
		if ( GET_EQ ( ch, where ) && HAS_BODY ( ch, WEAR_LEGS_2 ) )
			where = WEAR_LEGS_2;

	if ( where == WEAR_FEET )
		if ( GET_EQ ( ch, where ) && HAS_BODY ( ch, WEAR_FEET_2 ) )
			where = WEAR_FEET_2;


	if ( where == WEAR_FOCUS )
	{
		if ( ( GET_EQ ( ch, WEAR_WIELD ) ) || ( GET_EQ ( ch, WEAR_WIELD_2 ) ) )
		{
			ch->Send (
			    "You can't focus on anything while wielding something.\r\n" );
			return;
		}
	}

	if ( where == WEAR_WIELD )
	{
		obj_data *w1 = GET_EQ ( ch, WEAR_WIELD );
		obj_data *w2 = GET_EQ ( ch, WEAR_WIELD_2 );
		obj_data *sh = GET_EQ ( ch, WEAR_SHIELD );

		if ( GET_EQ ( ch, WEAR_FOCUS ) )
		{
			ch->Send (
			    "You can't wield anything while focusing on something.\r\n" );
			return;
		}

		if ( wep_hands ( obj ) == 2 && w1 )
		{
			*ch << "You can't wield a two-handed weapon if you are already wielding one.\r\n";
			*ch << "Two-handed weapons can only be wielded as the primary weapon.\r\n";
			return;
		}

		if ( wep_hands ( obj ) == 2 && ( !total_chance ( ch, SKILL_LONGARM ) && ( w2 || w1 ) ) )
		{
			*ch << "You can't wield a two-handed weapon without two hands free, or the longarm skill.\r\n";
			return;
		}

		if ( !sh && GET_EQ ( ch, where ) && total_chance ( ch, SKILL_DUAL ) )
			where = WEAR_WIELD_2;
		else if ( sh && GET_EQ ( ch, where ) && total_chance ( ch, SKILL_DUAL ) )
		{
			*ch << "You can't wield a second weapon while wearing a shield.\r\n";
			return;
		}
		else if ( w1 && wep_hands ( GET_EQ ( ch, WEAR_WIELD ) ) )
		{
			*ch << "You can't wield a second weapon while wielding a two handed weapon.\r\n";
			return;
		}
	}

	if ( ( where == WEAR_SHIELD ) && ( ( GET_EQ ( ch, WEAR_WIELD_2 ) ) ) )
	{
		*ch << "You can't wear a shield while wielding two weapons.\r\n";
		return;
	}

	if ( GET_EQ ( ch, where ) )
	{
		if ( !wearall && !OBJ_FLAGGED ( GET_EQ ( ch, where ), ITEM_NODROP ) )
		{
			if ( can_take_obj ( ch, GET_EQ ( ch, where ) ) )
			{
				perform_remove ( ch, where );
			}
			else
				return;
		}
		else
		{
			*ch << already_wearing[where];   //mord
			return;
		}
	}

	if ( wear_otrigger ( obj, ch, where ) <= 0 )
		return;

	wear_message ( ch, obj, where );

	/* so that the first time newbie equip works */
	if ( obj->carried_by )
		obj_from_char ( obj );
	equip_char ( ch, obj, where );
}



int find_eq_pos ( Character *ch, struct obj_data *obj, char *arg )
{
	int where = -1;

	if ( !arg || !*arg )
	{
		if ( CAN_WEAR ( obj, ITEM_WEAR_FINGER ) )
			where = WEAR_FINGER_R;
		if ( CAN_WEAR ( obj, ITEM_WEAR_NECK ) )
			where = WEAR_NECK_1;
		if ( CAN_WEAR ( obj, ITEM_WEAR_BODY ) )
			where = WEAR_BODY;
		if ( CAN_WEAR ( obj, ITEM_WEAR_HEAD ) )
			where = WEAR_HEAD;
		if ( CAN_WEAR ( obj, ITEM_WEAR_LEGS ) )
			where = WEAR_LEGS;
		if ( CAN_WEAR ( obj, ITEM_WEAR_FEET ) )
			where = WEAR_FEET;
		if ( CAN_WEAR ( obj, ITEM_WEAR_HANDS ) )
			where = WEAR_HANDS;
		if ( CAN_WEAR ( obj, ITEM_WEAR_ARMS ) )
			where = WEAR_ARMS;
		if ( CAN_WEAR ( obj, ITEM_WEAR_SHIELD ) )
			where = WEAR_SHIELD;
		if ( CAN_WEAR ( obj, ITEM_WEAR_ABOUT ) )
			where = WEAR_ABOUT;
		if ( CAN_WEAR ( obj, ITEM_WEAR_WAIST ) )
			where = WEAR_WAIST;
		if ( CAN_WEAR ( obj, ITEM_WEAR_WRIST ) )
			where = WEAR_WRIST_R;
		if ( CAN_WEAR ( obj, ITEM_WEAR_FACE ) )
			where = WEAR_FACE;
		if ( CAN_WEAR ( obj, ITEM_WEAR_HIPS ) )
			where = WEAR_HIPS;
		if ( CAN_WEAR ( obj, ITEM_WEAR_EYES ) )
			where = WEAR_EYES;
		if ( CAN_WEAR ( obj, ITEM_WEAR_EAR ) )
			where = WEAR_EAR_R;
		if ( CAN_WEAR ( obj, ITEM_WEAR_ANKLE ) )
			where = WEAR_ANKLE_R;
		if ( CAN_WEAR ( obj, ITEM_WEAR_HORNS ) )
			where = WEAR_HORNS;
		if ( CAN_WEAR ( obj, ITEM_WEAR_ANTENNA ) )
			where = WEAR_ANTENNA;
		if ( CAN_WEAR ( obj, ITEM_WEAR_TAIL ) )
			where = WEAR_TAIL;
		if ( CAN_WEAR ( obj, ITEM_WEAR_FOCUS ) )
			where = WEAR_FOCUS;
		if ( CAN_WEAR ( obj, ITEM_WEAR_SHOULDER ) )
			where = WEAR_SHOULDER_L;
		if ( CAN_WEAR ( obj, ITEM_WEAR_CREST ) )
			where = WEAR_CREST;
		if ( CAN_WEAR ( obj, ITEM_WEAR_THIGH ) )
			where = WEAR_THIGH_L;
		if ( CAN_WEAR ( obj, ITEM_WEAR_KNEE ) )
			where = WEAR_KNEE_L;
		if ( CAN_WEAR ( obj, ITEM_WEAR_FLOATING ) )
			where = WEAR_FLOATING;

	}
	else
	{
		char arg1[MAX_INPUT_LENGTH];
		one_argument ( arg, arg1 );
		where = search_block ( arg1, body, FALSE );
		if ( !CAN_WEAR ( obj, where_to_worn ( where ) ) )
			return -1;
		if ( ( ( where ) < 0 ) || ( *arg == '!' ) )
		{
			// sprintf(buf, "'%s'?  What part of your body is THAT?\r\n", arg);
			//ch->Send("%s", buf);
			return -1;
		}
	}

	return ( where );
}


ACMD ( do_wear )
{
	char arg1[MAX_INPUT_LENGTH]; /* item name */
	char *arg2; /* location name */
	struct obj_data *obj, *next_obj;
	int where, dotmode, items_worn = 0;

	skip_spaces ( &argument );
	arg2 = str_until ( argument, "on", arg1, sizeof ( arg1 ) );


	if ( !*arg1 )
	{
		*ch << "Wear what?\r\n";
		return;
	}
	dotmode = find_all_dots ( arg1 );

	if ( arg2 && *arg2 && ( dotmode != FIND_INDIV ) )
	{
		*ch << "You can't specify the same body location for more than one item!\r\n";
		return;
	}
	if ( dotmode == FIND_ALL )
	{
		wearall = 1;
		for ( obj = ch->carrying; obj; obj = next_obj )
		{
			next_obj = obj->next_content;
			if ( ( GET_LEVEL ( ch ) >= GET_OBJ_LEVEL ( obj ) ) &&CAN_SEE_OBJ ( ch, obj )
			        && ( where = find_eq_pos ( ch, obj, 0 ) ) >= 0 )
			{
				items_worn++;
				perform_wear ( ch, obj, where );
			}
		}
		wearall = 0;
		if ( !items_worn )
			*ch << "You don't seem to have anything wearable.\r\n";
	}
	else if ( dotmode == FIND_ALLDOT )
	{
		if ( !*arg1 )
		{
			*ch << "Wear all of what?\r\n";
			return;
		}
		if ( ! ( obj = get_obj_in_list_vis ( ch, arg1, NULL, ch->carrying ) ) )
			ch->Send ( "You don't seem to have any %ss.\r\n", arg1 );
		else if ( GET_LEVEL ( ch ) < GET_OBJ_LEVEL ( obj ) )
			ch->Send ( "You are not experienced enough to use that.\r\n" );
		else
			wearall = 1;
		while ( obj )
		{
			next_obj =
			    get_obj_in_list_vis ( ch, arg1, NULL, obj->next_content );
			if ( ( where = find_eq_pos ( ch, obj, 0 ) ) >= 0 )
			{
				perform_wear ( ch, obj, where );
			}
			else
				act ( "You can't wear $p.", FALSE, ch, obj, 0, TO_CHAR );
			obj = next_obj;
		}
		wearall = 0;
	}
	else
	{
		if ( ! ( obj = get_obj_in_list_vis ( ch, arg1, NULL, ch->carrying ) ) )
		{
			ch->Send ( "You don't seem to have %s %s.\r\n", AN ( arg1 ),
			           arg1 );
		}
		else
		{
			if ( ( where = find_eq_pos ( ch, obj, arg2 ) ) >= 0 )
			{
				perform_wear ( ch, obj, where );
			}
			else if ( !*arg2 )
				act ( "You can't wear $p.", FALSE, ch, obj, 0, TO_CHAR );
			else
				ch->Send ( "You can't wear that there!\r\n" );
		}
	}
}



ACMD ( do_wield )
{
	struct obj_data *obj;
	skip_spaces ( &argument );

	if ( !*argument )
		ch->Send ( "Wield what?\r\n" );
	else if ( ! ( obj = get_obj_in_list_vis ( ch, argument, NULL, ch->carrying ) ) )
	{
		ch->Send ( "You don't seem to have %s %s.\r\n", AN ( argument ), argument );
	}
	else
	{
		if ( !CAN_WEAR ( obj, ITEM_WEAR_WIELD ) )
			ch->Send ( "You can't wield that.\r\n" );
		else if ( GET_OBJ_WEIGHT ( obj ) >
		          str_app[STRENGTH_APPLY_INDEX ( ch ) ].wield_w )
			ch->Send ( "It's too heavy for you to use.\r\n" );
		else
			perform_wear ( ch, obj, WEAR_WIELD );
	}
}

int where_to_worn ( int where )
{
	int worn = -1;

	switch ( where )
	{
		case WEAR_NECK_1:
		case WEAR_NECK_2:
			worn = ITEM_WEAR_NECK;
			break;
		case WEAR_BODY:
			worn = ITEM_WEAR_BODY;
			break;
		case WEAR_HEAD:
			worn = ITEM_WEAR_HEAD;
			break;
		case WEAR_LEGS:
			worn = ITEM_WEAR_LEGS;
			break;
		case WEAR_FEET:
			worn = ITEM_WEAR_FEET;
			break;
		case WEAR_HANDS:
			worn = ITEM_WEAR_HANDS;
			break;
		case WEAR_ARMS:
			worn = ITEM_WEAR_ARMS;
			break;
		case WEAR_SHIELD:
			worn = ITEM_WEAR_SHIELD;
			break;
		case WEAR_ABOUT:
			worn = ITEM_WEAR_ABOUT;
			break;
		case WEAR_WAIST:
			worn = ITEM_WEAR_WAIST;
			break;
		case WEAR_WRIST_R:
		case WEAR_WRIST_L:
			worn = ITEM_WEAR_WRIST;
			break;
		case WEAR_FACE:
			worn = ITEM_WEAR_FACE;
			break;
		case WEAR_HIPS:
			worn = ITEM_WEAR_HIPS;
			break;
		case WEAR_EYES:
			worn = ITEM_WEAR_EYES;
			break;
		case WEAR_EAR_R:
		case WEAR_EAR_L:
			worn = ITEM_WEAR_EAR;
			break;
		case WEAR_ANKLE_R:
		case WEAR_ANKLE_L:
			worn = ITEM_WEAR_ANKLE;
			break;
		case WEAR_HORNS:
			worn = ITEM_WEAR_HORNS;
			break;
		case WEAR_ANTENNA:
			worn = ITEM_WEAR_ANTENNA;
			break;
		case WEAR_TAIL:
			worn = ITEM_WEAR_TAIL;
			break;
		case WEAR_FOCUS:
			worn = ITEM_WEAR_FOCUS;
			break;
		case WEAR_THUMB_L:
		case WEAR_THUMB_R:
		case WEAR_FINGER_R:
		case WEAR_FINGER_L:
			worn = ITEM_WEAR_FINGER;
			break;
		case WEAR_SADDLE:
			worn = ITEM_WEAR_ABOUT;
			break;
		case WEAR_EAR_TIP:
			worn = ITEM_WEAR_EAR;
			break;
		case WEAR_SHOULDER_L:
		case WEAR_SHOULDER_R:
			worn = ITEM_WEAR_SHOULDER;
			break;
		case WEAR_CREST:
			worn = ITEM_WEAR_CREST;
			break;
		case WEAR_THIGH_L:
		case WEAR_THIGH_R:
			worn = ITEM_WEAR_THIGH;
			break;
		case WEAR_KNEE_L:
		case WEAR_KNEE_R:
			worn = ITEM_WEAR_KNEE;
			break;
		case WEAR_FLOATING:
			worn = ITEM_WEAR_FLOATING;
			break;
	}
	return worn;
}

ACMD ( do_grab )
{
	struct obj_data *obj;
	skip_spaces ( &argument );

	if ( !*argument )
		ch->Send ( "Hold what?\r\n" );
	else if ( ! ( obj = get_obj_in_list_vis ( ch, argument, NULL, ch->carrying ) ) )
	{
		ch->Send ( "You don't seem to have %s %s.\r\n", AN ( argument ), argument );
	}
	else
	{
		if ( GET_OBJ_TYPE ( obj ) == ITEM_LIGHT )
			perform_wear ( ch, obj, WEAR_LIGHT );
		else
		{
			if ( !CAN_WEAR ( obj, ITEM_WEAR_HOLD )
			        && GET_OBJ_TYPE ( obj ) != ITEM_WAND
			        && GET_OBJ_TYPE ( obj ) != ITEM_STAFF
			        && GET_OBJ_TYPE ( obj ) != ITEM_SCROLL
			        && GET_OBJ_TYPE ( obj ) != ITEM_POTION
			        && GET_OBJ_TYPE ( obj ) != ITEM_ANTIDOTE_1
			        && GET_OBJ_TYPE ( obj ) != ITEM_ANTIDOTE_2
			        && GET_OBJ_TYPE ( obj ) != ITEM_ANTIDOTE_3 )
				ch->Send ( "You can't hold that.\r\n" );
			else
				perform_wear ( ch, obj, WEAR_HOLD );
		}
	}
}



void perform_remove ( Character *ch, int pos )
{
	struct obj_data *obj;


	if ( ! ( obj = GET_EQ ( ch, pos ) ) )
		log ( "SYSERR: perform_remove: bad pos %d passed.", pos );
	else if ( IS_OBJ_STAT ( obj, ITEM_NODROP ) )
		act ( "You can't remove $p, it must be CURSED!", FALSE, ch, obj, 0,
		      TO_CHAR );
	else if ( IS_CARRYING_N ( ch ) >= CAN_CARRY_N ( ch ) )
		act ( "$p: you can't carry that many items!", FALSE, ch, obj, 0,
		      TO_CHAR );
	else
	{
		if ( ( pos == WEAR_WIELD ) && GET_EQ ( ch, WEAR_WIELD_2 ) )
		{
			obj = GET_EQ ( ch, WEAR_WIELD_2 );
			pos = WEAR_WIELD_2;
		}
		if ( remove_otrigger ( obj, ch ) <=0 )
			return;
		LS_REMOVE = 1;
		obj_to_char ( ( obj = unequip_char ( ch, pos ) ), ch );
		LS_REMOVE = 0;
		if ( obj != NULL )
		{
			act ( "You stop using $p.", FALSE, ch, obj, 0, TO_CHAR );
			act ( "$n stops using $p.", TRUE, ch, obj, 0, TO_ROOM );
		}
		else
		{
			ch->Send ( "Somethings wrong.\r\n" );
		}
	}



}



ACMD ( do_remove )
{
	int i, dotmode, found;
	char arg[MAX_INPUT_LENGTH];

	skip_spaces ( &argument );

	if ( !*argument )
	{
		ch->Send ( "Remove what?\r\n" );
		return;
	}
	dotmode = find_all_dots ( argument );

	if ( dotmode == FIND_ALL )
	{
		found = 0;
		for ( i = 0; i < NUM_WEARS; i++ )
			if ( GET_EQ ( ch, i ) )
			{
				perform_remove ( ch, i );
				found = 1;
			}
		if ( !found )
			ch->Send ( "You're not using anything.\r\n" );
	}
	else if ( dotmode == FIND_ALLDOT )
	{
		if ( !*arg )
			ch->Send ( "Remove all of what?\r\n" );
		else
		{
			found = 0;
			for ( i = 0; i < NUM_WEARS; i++ )
				if ( GET_EQ ( ch, i ) && CAN_SEE_OBJ ( ch, GET_EQ ( ch, i ) ) &&
				        isname_full ( argument, GET_EQ ( ch, i )->name ) )
				{
					perform_remove ( ch, i );
					found = 1;
				}
			if ( !found )
			{
				ch->Send ( "You don't seem to be using any %ss.\r\n",
				           arg );
			}
		}
	}
	else
	{
		if ( ( i =
		            get_obj_pos_in_equip_vis ( ch, argument, NULL, ch->equipment ) ) < 0 )
			ch->Send ( "You don't seem to be using %s %s.\r\n",
			           AN ( argument ), argument );
		else
			perform_remove ( ch, i );
	}
}
C_FUNC ( pull_object )
{
	struct obj_data *obj;
	//room_rnum rm;
	int dir = 0;
	obj = find_obj ( d->character->pulling );
	d->character->pulling = -1;
	if ( !obj || !HERE ( d->character, obj ) )
	{
		d->Output ( "It isn't here any longer.\r\n" );
		return;
	}
	if ( !arg || !*arg )
	{
		d->Output ( "That isn't a valid direction.\r\n" );
		return;
	}
	switch ( LOWER ( *arg ) )
	{
		case 'n':
			dir = NORTH;
			break;
		case 's':
			dir = SOUTH;
			break;
		case 'e':
			dir = EAST;
			break;
		case 'w':
			dir = WEST;
			break;
		case 'u':
			dir = UP;
			break;
		case 'd':
			dir = DOWN;
			break;
		default:
			d->Output ( "That isn't a valid direction.\r\n" );
			return;
			break;
	}

	if ( !EXIT ( d->character, dir ) )
	{
		d->Output ( "No exit that way.\r\n" );
		return;
	}
	if ( IS_SET ( EXIT ( d->character, dir )->exit_info, EX_CLOSED ) )
	{
		d->Output ( "You need to open the door first.\r\n" );
		return;
	}
	/*  if (!IS_SET_AR(ROOM_FLAGS(EXIT(d->character, dir)->to_room), ROOM_VEHICLE))
	  {
	    d->Output( "It doesn't look like you can pull it that direction.\r\n");
	    return;
	  }
	*/

	//rm = EXIT ( d->character, dir )->to_room;
	if ( do_simple_move ( d->character, dir, FALSE ) )
	{
		do_simple_obj_move ( obj,dir, d->character );
	}


}

ACMD ( do_hitch )
{
	char arg1[MAX_INPUT_LENGTH], *arg2;
	struct obj_data *obj;
	Character *vict = NULL;
	skip_spaces ( &argument );
	arg2 = str_until ( argument, "to", arg1, sizeof ( arg1 ) );
	skip_spaces ( &arg2 );

	if ( !*arg1 )
	{
		ch->Send ( "Hitch <cart or other item> to <centaur or animal>\r\n" );
		return;
	}
	if ( ( !*arg2 || isname ( arg2, "me self" ) ) && ! ( ( !IS_NPC ( ch ) && GET_RACE ( ch ) == RACE_CENTAUR ) || ( IS_NPC ( ch ) && GET_CLASS ( ch ) == CLASS_ANIMAL ) ) )
	{
		ch->Send ( "You can't hitch that to yourself since you are not an animal or a Centaur.\r\n" );
		return;
	}
	else
	{
		if ( !IS_NPC ( ch ) )
		{
			if ( ( GET_RACE ( ch ) == RACE_CENTAUR ) && PRF_FLAGGED ( ch, PRF_MOUNTABLE ) )
				vict = ch;
			else
			{
				ch->Send ( "You must be mountable to hitch things to yourself.\r\n" );
				return;
			}
		}
		else
			vict = ch;
	}


	if ( NULL == ( obj = get_obj_in_list_vis ( ch, arg1, NULL, IN_ROOM ( ch )->contents ) ) )
	{
		ch->Send ( "That isn't here.\r\n" );
		return;
	}
	if ( !OBJ_FLAGGED ( obj, ITEM_SHIFTABLE ) )
	{
		ch->Send ( "That isn't hitchable!\r\n" );
		return;
	}

	if ( !vict && ! ( vict = get_char_vis ( ch, arg2, NULL, FIND_CHAR_ROOM ) ) )
	{
		ch->Send ( "%s", CONFIG_NOPERSON );
		return;
	}

	if ( ! ( ( !IS_NPC ( vict ) && GET_RACE ( vict ) == RACE_CENTAUR && PRF_FLAGGED ( vict, PRF_MOUNTABLE ) ) || ( IS_NPC ( vict ) && GET_CLASS ( vict ) == CLASS_ANIMAL ) ) )
	{
		ch->Send ( "You can only hitch things to mountable players and animals.\r\n" );
		if ( !IS_NPC ( vict ) )
			vict->Send ( "%s just tried to hitch something to you but you are not mountable.\r\n", PERS ( vict, ch ) );
		return;
	}
	if ( obj->hitched )
	{
		ch->Send ( "%s is already hitched to something. Unhitch it first.\r\n", obj->short_description );
		return;
	}
	if ( vict->hitched )
	{
		ch->Send ( "%s is already hitched to something. Unhitch it first.\r\n", GET_NAME ( vict ) );
		return;
	}
	if ( vict == ch )
	{
		act ( "You hitch $p to yourself.", TRUE, ch, obj, vict, TO_CHAR );
		act ( "$n hitches $p to $mself.", TRUE, ch, obj, vict, TO_CHAR );
	}
	else
	{
		act ( "$n hitches $p to $N.", TRUE, ch, obj, vict, TO_NOTVICT );
		act ( "You hitch $p to $N.", TRUE, ch, obj, vict, TO_CHAR );
		act ( "$n hitches $p to you.", TRUE, ch, obj, vict, TO_VICT );
	}
	obj->hitched = vict;
	ch->hitched = obj;


}

void unhitch_item ( struct obj_data *obj )
{
	Character *tmp;
	if ( obj && obj->hitched )
	{
		for ( tmp = character_list;tmp;tmp = tmp->next )
		{
			if ( obj->hitched == tmp )
			{
				tmp->hitched = NULL;
				break;
			}
		}
		obj->hitched = NULL;
	}
}
void unhitch_mob ( Character *ch )
{
	if ( ch && ch->hitched )
	{
		for ( olt_it i = object_list.begin(); i != object_list.end(); i++ )
		{
			if ( ch->hitched == ( i->second ) )
			{
				( i->second )->hitched = NULL;
				break;
			}
		}
		ch->hitched = NULL;
	}
}

ACMD ( do_unhitch )
{
	struct obj_data *obj;
	Character *vict = NULL;
	skip_spaces ( &argument );

	if ( !*argument )
	{
		ch->Send ( "Unhitch what?\r\n" );
		return;
	}

	if ( NULL == ( obj = get_obj_in_list_vis ( ch, argument, NULL, IN_ROOM ( ch )->contents ) ) )
	{
		if ( ! ( vict = get_char_vis ( ch, argument, NULL, FIND_CHAR_ROOM ) ) )
		{
			ch->Send ( "I can't find that item, animal or Centaur here." );
			return;
		}
		else
		{
			if ( vict->hitched )
				obj = vict->hitched;
		}
	}
	else
	{
		if ( obj->hitched )
			vict = obj->hitched;
	}
	if ( ! ( obj && obj->hitched ) || ! ( vict && vict->hitched ) )
	{
		ch->Send ( "Sorry but that isn't hitched.\r\n" );
		return;
	}
	if ( vict == ch )
	{
		act ( "You unhitch $p from yourself.", TRUE, ch, obj, vict, TO_CHAR );
		act ( "$n unhitches $p from $mself.", TRUE, ch, obj, vict, TO_ROOM );
	}
	else
	{
		act ( "$n unhitches $p from $N.", TRUE, ch, obj, vict, TO_NOTVICT );
		act ( "You unhitch $p from $N.", TRUE, ch, obj, vict, TO_CHAR );
		act ( "$n unhitches $p from you.", TRUE, ch, obj, vict, TO_VICT );
	}
	unhitch_item ( obj );
	unhitch_mob ( vict );
	if ( obj && obj->hitched )
		log ( "Obj didn't unhitch properly." );
	if ( vict && vict->hitched )
		log ( "Mob didn't unhitch properly" );
}

ACMD ( do_pull )
{
	//  char arg[MAX_INPUT_LENGTH];
	struct obj_data *obj;
	int cnt=0,i;
	skip_spaces ( &argument );

	if ( IS_NPC ( ch ) )
		return;

	if ( !*argument )
	{
		ch->Send ( "Pull what?\r\n" );
		return;
	}

	if ( NULL != ( obj = get_obj_in_list_vis ( ch, argument, NULL, IN_ROOM ( ch )->contents ) ) )
	{
		if ( !OBJ_FLAGGED ( obj, ITEM_SHIFTABLE ) )
		{
			ch->Send ( "That isn't pullable!\r\n" );
			return;
		}
		if ( obj->hitched )
		{
			ch->Send ( "You will need to unhitch it first!\r\n" );
			return;
		}
		/* check each direction to see if it can be pulled into. */
		ch->Send ( "You can pull %s: ",obj->short_description );
		for ( i = 0;i < NUM_OF_DIRS;i++ )
		{
			if ( !EXIT ( ch, i ) )
				continue;
			if ( IS_SET ( EXIT ( ch, i )->exit_info, EX_CLOSED ) )
				continue;
			//if (!IS_SET_AR(ROOM_FLAGS(EXIT(ch, i)->to_room), ROOM_VEHICLE))
			// continue;
			cnt++;
			ch->Send ( "%s ", dirs[i] );
		}
		if ( !cnt )
		{
			ch->Send ( "Nowhere\r\n" );
			return;
		}
		else
			ch->Send ( "\r\n" );
		ch->Send ( "Please type a direction:\r\n" );
		ch->pulling = GET_ID ( obj );
		line_input ( ch->desc, argument, pull_object, NULL );
	}
	/* for now only pull pin will work and must be wielding a grenade */
	else if ( !str_cmp ( argument, "pin" ) )
	{

		if ( GET_EQ ( ch, WEAR_WIELD ) )
		{
			if ( GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_WIELD ) ) == ITEM_GRENADE )
			{
				ch->Send ( "You pull the pin, the grenade is activated!\r\n" );
				SET_BIT_AR ( GET_OBJ_EXTRA ( GET_EQ ( ch, WEAR_WIELD ) ),
				             ITEM_LIVE_GRENADE );
				if ( IS_PK ( ch ) )
					GET_EQ ( ch, WEAR_WIELD )->obj_flags.value[3] = 1;
				else
					GET_EQ ( ch, WEAR_WIELD )->obj_flags.value[3] = 0;
			}
			else
				ch->Send ( "That's NOT a grenade!\r\n" );
		}
		else
			ch->Send ( "You aren't wielding anything!\r\n" );
	}
	else
	{
		ch->Send ( "Pull what?\r\n" );
	}

	/* put other 'pull' options here later */
	return;
}

int get_obj_aff ( struct obj_data *obj, int location )
{
	int i, t = 0;
	for ( i = 0; i < MAX_OBJ_AFFECT; i++ )
		if ( obj->affected[i].modifier )
			if ( location == obj->affected[i].location )
				t += obj->affected[i].modifier;

	return t;
}


// Changing the compares to use than instead of and -- Prom

void compare_weapons ( Character *ch,struct obj_data *item, struct obj_data *comp )
{
	float wep_multi[2] = { 1.0f, 1.0f};
	int value1, value2;
	if ( !item || !comp )
		return;
	if ( total_chance ( ch, SKILL_LONGARM ) > 0 && !is_short_wep ( item ) )
		wep_multi[0] += ( LONG_WEP_MULTI * ( ( float ) total_chance ( ch, SKILL_LONGARM ) ) ) /100.0;
	else if ( total_chance ( ch, SKILL_SHORT_BLADE ) > 0 && is_short_wep ( item ) )
		wep_multi[0] += ( SHORT_WEP_MULTI_ROGUE * ( ( float ) total_chance ( ch, SKILL_SHORT_BLADE ) ) ) /100.0;
	else
		wep_multi[0] = 1.0f;

	if ( GET_SKILL ( ch, SKILL_LONGARM ) > 0 && !is_short_wep ( comp ) )
		wep_multi[1] += ( LONG_WEP_MULTI * ( ( float ) total_chance ( ch, SKILL_LONGARM ) ) ) /100.0;
	else if ( GET_SKILL ( ch, SKILL_SHORT_BLADE ) > 0 && is_short_wep ( comp ) )
		wep_multi[1] += ( SHORT_WEP_MULTI_ROGUE * ( ( float ) total_chance ( ch, SKILL_SHORT_BLADE ) ) ) /100.0;
	else
		wep_multi[1] = 1.0f;

	value1 = FTOI ( ( ( item->obj_flags.value[1]+1 ) * item->obj_flags.value[2] * wep_multi[0] ) /2 );
	value2 = FTOI ( ( ( comp->obj_flags.value[1]+1 ) * comp->obj_flags.value[2] * wep_multi[1] ) /2 );

	ch->Send ( "      {cgYou would do %s damage with ({cRB{cg) %s ({cGA{cg).{c0\r\n",
	           value2 > value1 ? "more" : value2 == value1 ? "the same" : "less", value2 == value1 ? "than" : "then" );
}
void compare_staves ( Character *ch,struct obj_data *item, struct obj_data *comp )
{
	float staff_multi ( Character *ch, struct obj_data *staff );
	if ( !item || !comp )
		return;
	if ( staff_multi ( ch, comp ) > staff_multi ( ch, item ) )
	{
		ch->Send ( "      {cgYou would get a higher multiplyer from ({cRB{cg) then ({cGA{cg).{c0\r\n" );
	}
	else if ( staff_multi ( ch, comp ) == staff_multi ( ch, item ) )
	{
		ch->Send ( "      {cgYou would get the same multiplyer from ({cRB{cg) than ({cGA{cg).{c0\r\n" );
	}
	else
	{
		ch->Send ( "      {cgYou would get a lower multiplyer from ({cRB{cg) then ({cGA{cg).{c0\r\n" );
	}
}
void compare_armor ( Character *ch,struct obj_data *item, struct obj_data *comp )
{
	if ( !item || !comp )
		return;

	if ( GET_OBJ_VAL ( comp, 0 ) > GET_OBJ_VAL ( item, 0 ) )
	{
		ch->Send ( "      {cgYou would get more armor from ({cRB{cg) then ({cGA{cg).{c0\r\n" );
	}
	else if ( GET_OBJ_VAL ( comp, 0 ) == GET_OBJ_VAL ( item, 0 ) )
	{
		ch->Send ( "      {cgYou would get the same from ({cRB{cg) then ({cGA{cg).{c0\r\n" );
	}
	else
	{
		ch->Send ( "      {cgYou would get less armor from ({cRB{cg) than ({cGA{cg).{c0\r\n" );
	}

}
void compare_food ( Character *ch,struct obj_data *item, struct obj_data *comp )
{
	if ( !item || !comp )
		return;

	if ( GET_OBJ_VAL ( comp, 0 ) > GET_OBJ_VAL ( item, 0 ) )
	{
		ch->Send ( "      {cgYou would get more food from ({cRB{cg) then ({cGA{cg).{c0\r\n" );
	}
	else if ( GET_OBJ_VAL ( comp, 0 ) == GET_OBJ_VAL ( item, 0 ) )
	{
		ch->Send ( "      {cgYou would get the same food from ({cRB{cg) then ({cGA{cg).{c0\r\n" );
	}
	else
	{
		ch->Send ( "      {cgYou would get less food from ({cRB{cg) than ({cGA{cg).{c0\r\n" );
	}

}
void compare_affects ( Character *ch, struct obj_data *item, struct obj_data *comp )
{
	int i;
	int points = 0;
	if ( !item || !comp )
		return;
	for ( i = 0; i < MAX_APPLY; i++ )
		switch ( i )
		{
			case APPLY_NONE:
				break;
			case APPLY_HIT:
			case APPLY_MANA:
			case APPLY_MOVE:
			case APPLY_REGEN_HIT:
			case APPLY_REGEN_MOVE:
			case APPLY_REGEN_MANA:
				points += FTOI ( ( 0.2 * get_obj_aff ( comp, i ) ) - ( 0.2 * get_obj_aff ( item, i ) ) );

				break;
			case APPLY_AC:

				points += FTOI ( ( -1 * get_obj_aff ( comp, i ) ) - ( -1 * get_obj_aff ( item, i ) ) );

				break;
			case APPLY_DAMROLL:
			case APPLY_STR:
			case APPLY_CON:
				if ( IS_FIGHTER ( GET_CLASS ( ch ) ) || IS_ROGUE ( GET_CLASS ( ch ) ) )
				{
					points += FTOI ( ( 2 * get_obj_aff ( comp, i ) ) - ( 2 * get_obj_aff ( item, i ) ) );
				}
				break;
			case APPLY_CHA:
			case APPLY_INT:
			case APPLY_WIS:
				if ( IS_CASTER ( GET_CLASS ( ch ) ) )
				{
					points += ( 2 * get_obj_aff ( comp, i ) ) - ( 2 * get_obj_aff ( item, i ) );
				}
				else
					points += get_obj_aff ( comp, i ) - get_obj_aff ( item, i );
				break;
			default:
				points += get_obj_aff ( comp, i ) - get_obj_aff ( item, i );
				break;
		}

	ch->Send ( "      {cg({cRB{cg) has %s beneficial affects %s ({cGA{cg) with a difference score of %d.{c0\r\n",
	           points > 0 ? "more" : points ==  0 ? "the same" : "less",  points ==  0 ? "and" : "then", points );

}

int same_location ( struct obj_data *a, struct obj_data *b )
{
	int cnt = 0, i;
	for ( i = 1;i < NUM_WEARS;i++ )
		if ( CAN_WEAR ( a, i ) && CAN_WEAR ( b, i ) )
			cnt++;
	return cnt;
}

ACMD ( do_compare )
{
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	struct obj_data *obj1;
	struct obj_data *obj2;

	two_arguments ( argument, arg1, arg2 );

	if ( arg1[0] == '\0' )
	{
		ch->Send ( "Compare what to what?\r\n" );
		return;
	}

	if ( ! ( obj1 = get_obj_in_list_vis ( ch, arg1, NULL, ch->carrying ) ) )
	{
		ch->Send ( "You do not have that item.\r\n" );
		return;
	}

	if ( arg2[0] == '\0' )
	{
		int it, cnt = 0;
		for ( obj2 = ch->carrying; obj2; obj2 = obj2->next_content )
		{
			if ( obj1 == obj2 )
				continue;
			if ( same_location ( obj1, obj2 ) )
				continue;
			if ( CAN_SEE_OBJ ( ch, obj2 ) && GET_OBJ_TYPE ( obj2 ) == GET_OBJ_TYPE ( obj1 ) && CAN_GET_OBJ ( ch, obj2 ) )
			{
				ch->Send ( "\r\n{cyComparing %s {cy({cGA{cy) to %s {cy({cRB{cy) : {cc[inventory]{c0\r\n",
				           obj2->short_description, obj1->short_description );
				switch ( GET_OBJ_TYPE ( obj1 ) )
				{
					case ITEM_WEAPON:
						compare_weapons ( ch, obj1, obj2 );
						break;
					case ITEM_ARMOR:
						compare_armor ( ch, obj1, obj2 );
						break;
					case ITEM_FOOD:
						compare_food ( ch, obj1, obj2 );
						break;
					case ITEM_FOCUS_MAJOR:
					case ITEM_FOCUS_MINOR:
						compare_staves ( ch, obj1, obj2 );
						break;
				}
				compare_affects ( ch, obj1, obj2 );
				cnt++;
			}
		}
		for ( it = 0; it < NUM_WEARS; it++ )
		{
			obj2 = GET_EQ ( ch, it );
			if ( !obj2 )
				continue;
			if ( !CAN_SEE_OBJ ( ch, obj2 ) )
				continue;
			if ( CAN_WEAR ( obj1, where_to_worn ( it ) ) )
			{
				ch->Send ( "\r\n{cyComparing %s {cy({cGA{cy) to %s {cy({cRB{cy) : {cc[equipment: %s]{c0\r\n", obj2->short_description,obj1->short_description, body[it] );
				if ( GET_OBJ_TYPE ( obj2 ) == GET_OBJ_TYPE ( obj1 ) )
				{
					switch ( GET_OBJ_TYPE ( obj1 ) )
					{
						case ITEM_WEAPON:
							compare_weapons ( ch, obj1, obj2 );
							break;
						case ITEM_ARMOR:
							compare_armor ( ch, obj1, obj2 );
							break;
						case ITEM_FOOD:
							compare_food ( ch, obj1, obj2 );
							break;
						case ITEM_FOCUS_MAJOR:
						case ITEM_FOCUS_MINOR:
							compare_staves ( ch, obj1, obj2 );
							break;
					}
				}
				compare_affects ( ch, obj1, obj2 );
				cnt++;
			}
		}

		if ( !cnt )
		{
			ch->Send ( "You aren't wearing anything comparable.\r\n" );
			return;
		}
	}
	else
	{
		if ( ! ( obj2 = get_obj_in_list_vis ( ch, arg2, NULL, ch->carrying ) ) )
		{
			if ( ! ( obj2 = get_obj_in_equip_vis ( ch, arg2, NULL, ch->equipment ) ) )
			{
				ch->Send ( "You do not have that item.\r\n" );
				return;
			}
		}
		ch->Send ( "{cyComparing %s {cy({cGA{cy) to %s {cy({cRB{cy):\r\n", obj2->short_description, obj1->short_description );
		if ( GET_OBJ_TYPE ( obj2 ) == GET_OBJ_TYPE ( obj1 ) )
		{
			switch ( GET_OBJ_TYPE ( obj1 ) )
			{
				case ITEM_WEAPON:
					compare_weapons ( ch, obj1, obj2 );
					break;
				case ITEM_ARMOR:
					compare_armor ( ch, obj1, obj2 );
					break;
				case ITEM_FOOD:
					compare_food ( ch, obj1, obj2 );
					break;
				case ITEM_FOCUS_MAJOR:
				case ITEM_FOCUS_MINOR:
					compare_staves ( ch, obj1, obj2 );
					break;
			}
		}
		compare_affects ( ch, obj1, obj2 );
	}

	return;
}


/*speed functions*/
int speed_update ( Character *ch )
{

	float speed = 0.0;
	float var = 0.0;
	int weps = has_weapon ( ch );



	if ( IS_NPC ( ch ) ) //npc speed
		speed += ( ( GET_LEVEL ( ch )- ( 30 - ( 2 * MOB_TIER ( ch ) ) ) ) * ( 5 + ( 2 *  MOB_TIER ( ch ) ) ) );
	else   // start of player speed
	{
			speed = 50;  /* Beginning of Base Speed, as move modifier (which was a penalty more than it was a bonus and makes
					no sense from a combat design standpoint) was removed in lieu of not encouraging players to sit around
					without ever typing any commands. */


		if ( 0 < total_chance ( ch, SKILL_SECOND_ATTACK ) )
		{
			speed += total_chance ( ch, SKILL_SECOND_ATTACK ) /2;

		}
		if ( 0 <total_chance ( ch, SKILL_THIRD_ATTACK ) )
		{
			speed += total_chance ( ch, SKILL_THIRD_ATTACK ) /2;
		}

		speed += class_speed ( ch );
		speed += race_speed ( ch );


		speed += ( ( ( GET_DEX ( ch ) * 200 ) / 22 )-130 );
		speed += AFF_SPEED ( ch );
		if ( ( GET_SUB ( ch, SUB_LOYALSPEED ) ) > 0 )
			speed += 50;
		if ( CAN_CARRY_W ( ch ) > 0 && IS_CARRYING_W ( ch ) >= 0 )
		{
			if ( IS_CARRYING_W ( ch ) == 0 )
				var = 200.0f;
			else if ( IS_CARRYING_W ( ch ) < CAN_CARRY_W ( ch ) )
				var = 300  - ( ( ( IS_CARRYING_W ( ch ) * 400 ) / CAN_CARRY_W ( ch ) ) );
			else
				var = -200;

		}
		if (REMORTS(ch) == 0) 
                speed += 200;
                else
           	speed += var;


		if ( RIDING ( ch ) && HERE ( RIDING ( ch ), ch ) && total_chance ( ch, SKILL_MOUNTED_COMBAT ) )
			speed += 25 + ( total_chance ( ch, SKILL_MOUNTED_COMBAT ) /3 );
		else if ( GET_RACE ( ch ) == RACE_CENTAUR && GET_SKILL ( ch, SKILL_MOUNTED_COMBAT ) )
			speed += 25 + ( total_chance ( ch, SKILL_MOUNTED_COMBAT ) /3 );
		// end of player speed
	}
	if ( weps )
	{
		speed += get_weapon_speed ( GET_EQ ( ch, WEAR_WIELD ) );
		speed += get_weapon_speed ( GET_EQ ( ch, WEAR_WIELD_2 ) );
	}


	if ( RIDDEN_BY ( ch ) && HERE ( RIDDEN_BY ( ch ), ch ) )
		speed *= 0.75;

	if ( !IS_NPC ( ch ) )
		speed += ( GET_HITROLL ( ch ) * 2 );
	else
		speed += ( GET_HITROLL ( ch ) * ( MOB_TIER ( ch ) * 0.5 ) );

	/*add in spell modifyers below*/

	if ( AFF_FLAGGED ( ch, AFF_FROZEN ) )
		speed -= abs ( ( int ) speed ) / 3.0f;
	if ( AFF_FLAGGED ( ch, AFF_FREEZING ) )
		speed *= 0.5;

	speed = IRANGE ( -1000.0, speed, 1000.0 );

	if ( !IS_NPC ( ch ) )
		GET_SPEED ( ch ) = ( int ) speed;

	return ( int ) speed;

}



int class_speed ( Character *ch )
{
	int c, speed = 0;
	float armorcost = 0.0;
	int compute_armor_class ( Character *ch );
	c = GET_CLASS ( ch );
	switch ( c )
	{
		case CLASS_MAGE:
		case CLASS_PRIEST:
		case CLASS_ESPER:
			speed = 40;
			armorcost = 2.5;
			break;
		case CLASS_RANGER:
		case CLASS_GYPSY:
		case CLASS_THIEF:
			speed = 130;
			armorcost = 1;
			break;
		case CLASS_WARRIOR:
		case CLASS_HUNTER:
			speed = 10;
			armorcost = 0.5;
			break;
		default:
			speed = 10;
			break;
	}

	speed *= FTOI ( ( 1.0  + ( current_class_is_tier_num ( ch ) * 0.5 ) ) );

	speed += FTOI ( ( ( 100 + ( ch->compute_armor_class() ) )-200 ) * armorcost );

	return ( speed );
}

int race_speed ( Character *ch )
{
	/*
	"Select a race:\r\n"
	"  [F]aun\r\n"
	"  [C]entaur\r\n"
	"  [E]lf\r\n"
	"  [D]warf\r\n"
	"  [I]ndian\r\n"
	"  [G]ringo\r\n"
	"  [M]artian\r\n"
	"  [S]pace-wolf\r\n";
	*/
	/*
		switch ( GET_RACE ( ch ) )
		{
			case RACE_FAUN:
				return 100;
				break;
			case RACE_CENTAUR:
				return 150;
				break;
			case RACE_ELF:
				return 150;
				break;
			case RACE_DWARF:
				return 100;
				break;
			case RACE_INDIAN:
				return 100;
				break;
			case RACE_GRINGO:
				return 100;
				break;
			case RACE_MARTIAN:
				return 180;
				break;
			case RACE_SPACE_WOLF:
				return 120;
				break;
			case RACE_GLADIATOR:
				return 100;
				break;
			default:
				return 50;
				break;
		}
		*/
	return 0;

}



ACMD ( do_skin )
{
	OBJ_DATA *skin = NULL, *obj = NULL;
	//  char arg[MAX_INPUT_LENGTH];
	skip_spaces ( &argument );

	if ( !*argument )
	{
		*ch << "What do you want to skin?\r\n";
		return;
	}

	if ( ! ( obj = get_obj_in_list_vis ( ch, argument, NULL, IN_ROOM ( ch )->contents ) ) )
	{
		*ch << "There is nothing like that here. Try again.\r\n";
		return;
	}

	if ( !IS_OBJ_STAT ( obj, ITEM_NPC_CORPSE ) )
	{
		*ch << "The skin from that would fall apart too fast.\r\n";
		return;
	}
	if ( use_stamina ( ch, 50 ) < 0 )
	{
		*ch << "You are far too exausted!";
		return;
	}
	if ( obj->skin <= 0 )
	{
		*ch << "You don't seem to be able to remove the skin from the corpse.\r\n";
		return;
	}


	/* we've got an obj that is a mob corpse and can be skinned */
	if ( ( skin = read_object ( obj->skin, VIRTUAL ) ) != NULL )
	{
		*ch << "You skillfully slice the skin from the corpse and pick it up.\r\n";
		act ( "$n skillfully slices the skin from from the corpse and picks it up.\r\n", FALSE, ch, 0, 0, TO_ROOM );
		obj_to_char ( skin, ch );
		extract_obj ( obj );
	}
	else
		*ch << "Your hands turn numb and you fumble.\r\n";
}

ACMD ( do_fuel )
{
	obj_data *spacebike, *gemcluster;
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
	int fuel_amt;
	two_arguments ( argument,arg1,arg2 );
	if ( !*arg1 || !*arg2 )
	{
		ch->Send ( "Yes, fuelling, fine, fuelling we must, but WHAT???\r\n" );
		return;
	}
	if ( ! ( spacebike=get_obj_in_list_vis ( ch, arg1, NULL, IN_ROOM ( ch )->contents ) ) )
	{
		ch->Send ( "There is no %s here!\r\n", arg1 );
		return;
	}
	if ( GET_OBJ_TYPE ( spacebike ) !=ITEM_SPACEBIKE )
	{
		ch->Send ( "That doesn't look like a spacebike!\r\n" );
		return;
	}
	if ( GET_FUEL ( spacebike ) >=GET_MAX_FUEL ( spacebike ) )
	{
		ch->Send ( "%s is already fueled to its maximum capacity!\r\n",OBJS ( spacebike,ch ) );
		return;
	}
	if ( ! ( gemcluster=get_obj_in_list_vis ( ch, arg2, NULL, ch->carrying ) ) )
	{
		ch->Send ( "There is no %s in your inventory!\r\n",arg2 );
		return;
	}
	if ( GET_OBJ_TYPE ( gemcluster ) !=ITEM_GEM_CLUSTER )
	{
		ch->Send ( "That is not a gemcluster!\r\n" );
		return;
	}
	if ( GET_MAX_FUEL ( spacebike ) - GET_FUEL ( spacebike ) < GET_GEM_FUEL ( gemcluster ) )
		fuel_amt = GET_MAX_FUEL ( spacebike ) - GET_FUEL ( spacebike );
	else
		fuel_amt = GET_GEM_FUEL ( gemcluster );
	GET_FUEL ( spacebike ) +=fuel_amt;
	if ( GET_FUEL ( spacebike ) >=GET_MAX_FUEL ( spacebike ) )
	{
		GET_FUEL ( spacebike ) =GET_MAX_FUEL ( spacebike );
		ch->Send ( "You fill up %s to its maximum capacity, using %s.\r\n",OBJS ( spacebike,ch ),OBJS ( gemcluster,ch ) );
	}
	else
	{
		int filled=GET_FUEL_PERCENTAGE ( spacebike );
		if ( filled <25 )
			ch->Send ( "You fill up %s using %s, but it is still very empty.\r\n",OBJS ( spacebike,ch ),OBJS ( gemcluster,ch ) );
		else if ( filled<50 )
			ch->Send ( "You fill up %s using %s, filling it about quarter full.\r\n",OBJS ( spacebike,ch ),OBJS ( gemcluster,ch ) );
		else if ( filled<75 )
			ch->Send ( "You fill up %s using %s, filling it about half full.\r\n",OBJS ( spacebike,ch ),OBJS ( gemcluster,ch ) );
		else if ( filled<=100 )
			ch->Send ( "You fill up %s using %s, filling it about three quarter full.\r\n",OBJS ( spacebike,ch ),OBJS ( gemcluster,ch ) );
	}
	if ( fuel_amt == GET_GEM_FUEL ( gemcluster ) )
		extract_obj ( gemcluster );
	else
		GET_GEM_FUEL ( gemcluster ) -= fuel_amt;
}


void check_timer ( obj_data *obj )
{
	time_t ct = 0;
	unsigned long t;

	if ( obj == NULL || GET_OBJ_TIMER ( obj ) == -1 )
		return;

	ct = time ( 0 );

	if ( GET_OBJ_EXPIRE ( obj ) == 0 )
		GET_OBJ_EXPIRE ( obj ) = ( GET_OBJ_TIMER ( obj ) * SECS_PER_MUD_HOUR ) + ct;

	t = PASSES_PER_SEC * ( GET_OBJ_EXPIRE ( obj ) - ct );

	if ( GET_TIMER_EVENT ( obj ) == NULL ||
	        ( t < event_time ( GET_TIMER_EVENT ( obj ) ) ) )
	{

		/* take off old event, create updated event */
		if ( GET_TIMER_EVENT ( obj ) != NULL )
			event_cancel ( GET_TIMER_EVENT ( obj ) );
		else
			GET_TIMER_EVENT ( obj ) = NULL;

		GET_TIMER_EVENT ( obj ) = event_create ( timer_event, new timer_event_data ( obj ), t, EVENT_TYPE_TIMER );
	}
}
/*
 void FillNames() {
	 if (name && *name) {
		 char newlistbuf[strlen(name)+1], *newlist, *curtok;
		 strlcpy(newlistbuf, name, sizeof(newlistbuf));
		 newlist = newlistbuf;
		 Names.clear();
		 for (curtok = strsep(&newlist, WHITESPACE); curtok; curtok = strsep(&newlist, WHITESPACE))
			 if (curtok)
				 Names.push_back(curtok);
		 sort(Names.begin(), Names.end());
	 }
}*/

EVENTFUNC ( timer_event )
{
	struct timer_event_data *timer_event_obj = ( struct timer_event_data * ) event_obj;
	obj_data *obj;

	obj = timer_event_obj->obj;
	/* debugging to see if the obj is in the game can be done here, or we can find the obj based on ID instead of pointer - mord*/

	delete timer_event_obj;
	GET_TIMER_EVENT ( obj ) = NULL;

	if ( automeld ( obj ) )
		return 0;

	if ( timer_otrigger ( obj ) == -1 )
		return 0; /* item purged */

	if ( obj->carried_by != NULL )
		crumble_obj ( obj->carried_by, obj );
	else if ( obj->worn_by != NULL )
		crumble_obj ( obj->worn_by, obj );
	else
		crumble_obj ( NULL, obj );

	return 0;
}

void zap_char ( Character *victim )
{
	struct obj_data *obj = NULL;

	for ( int i = 0; i < NUM_WEARS; i++ )
	{
		obj = GET_EQ ( victim, i );
		if ( obj )
		{
			if ( invalid_align ( victim, obj ) || invalid_class ( victim, obj )
	        || invalid_race ( victim, obj ) )
			{
				obj = unequip_char ( victim, i );
				if (obj) {
					act ( "You are zapped by $p and instantly let go of it.",
					FALSE, victim, obj, 0, TO_CHAR );
					act ( "$n is zapped by $p and instantly lets go of it.",
					FALSE, victim, obj, 0, TO_ROOM );
					obj_to_char ( obj, victim );
				}
			}
			else if ( i == WEAR_WIELD_2 )
			{
				if ( wep_hands ( obj ) == 2 )
				{
					obj = unequip_char ( victim, i );
					if (obj) {
						act ( "You fumble with $p and instantly let go of it.",
						FALSE, victim, obj, 0, TO_CHAR );
						act ( "$n fumbles with $p and instantly lets go of it.",
						FALSE, victim, obj, 0, TO_ROOM );
						obj_to_char ( obj, victim );
					}
				}
			}
		}
	}
}
