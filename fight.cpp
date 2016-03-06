/**************************************************************************
*   File: fight.c                                       Part of CircleMUD *
*  Usage: Combat system                                                   *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
*  4dimensions.org combat enhancements, Copyright (C) 1999 - 2004         *
***************************************************************************/
/*
 * $Log: fight.c,v $
 * Revision 1.78  2007/11/23 22:09:39  w4dimenscor
 * changed the display of multi in the identify spell for orbs and staves
 *
 * Revision 1.77  2007/11/23 21:23:14  w4dimenscor
 * changed ORBSTAFF to be devided by 1000 in their multi so that they can have decimals
 *
 * Revision 1.76  2007/11/23 05:37:28  w4dimenscor
 * fixed staff multi
 *
 * Revision 1.75  2007/08/19 01:06:10  w4dimenscor
 * - Changed the playerindex to be a c++ object with search functions.
 * - changed the room descriptions to be searched from a MAP index, and
 * added Get and Set methods for room descriptions.
 * - changed the zone reset so that it doesnt search the entire object list
 * to find the object to PUT things into.
 * - rewrote other parts of the zone reset function, to make it give correct errors.
 * - rewrote the parts of the code to do with loading and searching for directorys and files.
 * - added a new dlib library.
 *
 * Revision 1.74  2007/06/26 10:48:05  w4dimenscor
 * Fixed context in scripts so that it works again, changed mounted combat so that it is about 2/3rds  one third mount damage, updated the way skills get read using total_chance, stopped things with a PERC of 0 assisting, made it so that the ungroup command disbanded charmies
 *
 * Revision 1.73  2007/06/17 10:44:53  w4dimenscor
 * Fixed a bug in a mounted persons movepoints going below 0, fixed abuse of animate dead, fixed group spells to allow for inclusion of charmies, and set a limit on total charmies in a group.
 *
 * Revision 1.72  2007/06/17 04:34:37  w4dimenscor
 * updated combat for charmies. Made it split the damage among the group better
 *
 * Revision 1.71  2007/06/15 00:12:03  w4dimenscor
 * Set timers on mob corpses too.
 *
 * Revision 1.70  2007/06/12 04:46:21  w4dimenscor
 * Evasion was converting to 0 for mobs still. Bad Math
 *
 * Revision 1.69  2007/06/11 09:20:28  w4dimenscor
 * fixed evasion rating
 *
 * Revision 1.68  2007/06/11 08:33:12  w4dimenscor
 * text_processed was accedentally moved to the wrong part of find_replacement
 *
 * Revision 1.67  2007/06/10 06:59:18  w4dimenscor
 * added the ability for scripts to toggle body parts on and off, and imms to do so too
 *
 * Revision 1.66  2007/06/10 04:51:20  w4dimenscor
 * Changed it so that mobs can assist in fights - still beta testing this, but safe to put in the game
 *
 * Revision 1.65  2007/06/10 02:18:39  w4dimenscor
 * changed all entries in the code of 'color' to 'colour', but i now regret it.
 *
 * Revision 1.64  2007/06/08 10:19:05  w4dimenscor
 * Added a way to check for real time, and time passed in dgscripts, REALDAY and REALHOUR and NOW are all variables, and also made it so tha the newbie channel doesnt sound drunk, also, practicing spells and skills need more practices
 *
 * Revision 1.63  2007/05/24 20:29:16  w4dimenscor
 * Last few fixes to hunting and directionals.
 * --Thotter
 *
 * Revision 1.62  2007/05/24 20:25:16  w4dimenscor
 * lots of hunt changes. Should all work now.
 *
 * Revision 1.61  2007/05/20 21:01:46  w4dimenscor
 * Fixed the spamcast bug.
 * -- Thotter
 *
 * Revision 1.60  2007/04/19 10:04:35  w4dimenscor
 * Updated subskills so they wouldn;t report a max over 100
 *
 * Revision 1.59  2006/09/27 10:52:35  w4dimenscor
 * Changed the way trees check how old they are. Fixed zedit crash bug when updating objects
 *
 * Revision 1.58  2006/09/24 08:24:22  w4dimenscor
 * Fixed combat in groups
 *
 * Revision 1.57  2006/09/23 07:06:47  w4dimenscor
 * Checked for null values being passed to can_fight
 *
 * Revision 1.56  2006/09/22 22:41:00  w4dimenscor
 * Added a check for numm values to assist and joined mobs
 *
 * Revision 1.55  2006/09/21 13:51:57  w4dimenscor
 * After the last change, which prevented riding sleeping/sitting mounts, mounted players couldn't flee anymore. apparently charmed mounts were assisting their master in the fight! The reason for this was fight_event_hit, which apparently made all charmies assist (recent code change?). I changed it so that charmies that have someone mounted on them won't assist.
 *
 * Revision 1.54  2006/09/21 08:37:50  w4dimenscor
 * Fixed uninitialised value in start-fighting
 *
 * Revision 1.53  2006/09/21 07:37:57  w4dimenscor
 * added a check for riders into combat
 *
 * Revision 1.52  2006/09/21 07:29:33  w4dimenscor
 * changed the combat things a little, to put riders as the one getting hit in combat
 *
 * Revision 1.51  2006/09/20 09:53:32  w4dimenscor
 * Fixed issue where players starting combat with a spell would not attack, and visa versa
 *
 * Revision 1.50  2006/09/19 10:56:16  w4dimenscor
 * fixed crash bug on extracting mob links
 *
 * Revision 1.49  2006/09/14 08:05:42  w4dimenscor
 * added convey tradepoints, and fixed iceshield
 *
 * Revision 1.48  2006/08/31 10:39:16  w4dimenscor
 * Fixe dthe crash bug in medit. and also changed the mob proto list. there is still a memory leak in medit, which is being fixed now
 *
 * Revision 1.47  2006/08/25 06:39:43  w4dimenscor
 * fixed the way skills would be deleted when you quit
 *
 * Revision 1.46  2006/08/23 09:01:26  w4dimenscor
 * Changed some of the std::vectors to std::map, killlist, and the lookup tables for id nums
 *
 * Revision 1.45  2006/08/13 06:26:51  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.44  2006/06/19 06:25:39  w4dimenscor
 * Changed the player saved mount feature so that all players can load mounts from houses
 *
 * Revision 1.43  2006/06/16 10:54:51  w4dimenscor
 * Moved several functions in fight.c into the Character object. Also removed all occurances of send_to_char from skills.c
 *
 * Revision 1.42  2006/06/16 06:28:35  w4dimenscor
 * converted the functions to load fight messages to C++ streams
 *
 * Revision 1.41  2006/05/30 09:14:19  w4dimenscor
 * rewrote the color code, process_output, and vwrite_to_output so that they use strings and have better buffer checks
 *
 * Revision 1.40  2006/05/22 10:50:49  w4dimenscor
 * Created 3 new files, mxp.cpp, mxp.h and descriptor.cpp
 * struct descriptor_data has been converted to class Descriptor
 *
 * Revision 1.39  2006/05/21 11:02:26  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.38  2006/04/21 12:46:44  w4dimenscor
 * Fixed gcc 4.1 compile time errors. Game will now compile in GCC4
 *
 * Revision 1.37  2006/04/06 14:00:57  w4dimenscor
 * fixed a memory bug in dg scripts
 *
 * Revision 1.36  2006/04/03 23:31:35  w4dimenscor
 * Added new commands called pclean, it removes the files of anyone who is not in the player index from the lib directory.
 *
 * Revision 1.35  2006/03/22 20:27:20  w4dimenscor
 * Changed all references to attack and defence and changed them to be accuracy and evasion, which more closely explains their role. Fixed up some errors in the defence roll part where the addition of dex to defence was backwards, lowering defence instead of adding to it the more dex you had (now called evasion).
 * Completed the autogroup toggle to work as expected (still untested though)
 * For your evasion rating, i added some more points based on level and tier.
 *
 * Revision 1.34  2006/03/13 19:07:40  w4dimenscor
 * Added a toggle for autogroup so you don't type Y to accept people in your group, and a commandthat lets you split involvement evenly, involve even
 *
 * Revision 1.33  2006/03/06 09:38:28  w4dimenscor
 * Changed it so that you can clan expel people who are offline and in another room
 *
 * Revision 1.32  2006/02/26 00:33:42  w4dimenscor
 * Fixed issue where ridden mobs took half exp, fixed issue where auctioneer couldnt talk on open channels or use color codes
 *
 * Revision 1.31  2006/02/25 23:42:45  w4dimenscor
 * Directional spells work now, BUT mana blast doesn't
 *
 * Revision 1.30  2006/02/24 20:09:02  w4dimenscor
 * * Fixed offline automeld so that if a player leaves their corpse and quits,
 *   it will still meld properly.
 * * Added the ability to see the stats of the object you are bidding on for 10pc
 *   of the current bid.
 * * Changed auction to check if people have enough money to complete the auction.
 * * Changed auction code to use the modular character gold commands.
 * * Fixed a bug in gold commands that returned the wrong value.
 *
 * Revision 1.29  2006/02/17 22:19:54  w4dimenscor
 * Fixed error for ubuntu that doesnt like empty array declarations, moved ice shield to a better place and fixed its messages, added auto auction fixes, allowed mounts to gain exp properly
 *
 * Revision 1.28  2006/01/23 05:23:19  w4dimenscor
 * sorry self. another. _cant remember the changes_ entry
 *
 * Revision 1.27  2005/11/30 18:47:12  w4dimenscor
 * changed slightly some gains you get from remorts
 *
 * Revision 1.26  2005/11/20 06:10:00  w4dimenscor
 * Fixed Directional spells, and exp
 *
 * Revision 1.25  2005/11/19 06:18:38  w4dimenscor
 * Fixed many bugs, and added features
 *
 * Revision 1.24  2005/11/01 18:43:37  w4dimenscor
 * Tradepoints have been added to players and saved, compare command has been updated, the login accounts thing works again, and when you can't see your attacker your attacker you get half defense points
 *
 * Revision 1.23  2005/10/30 08:37:05  w4dimenscor
 * Updated compare command and fixed mining
 *
 * Revision 1.22  2005/09/24 07:11:51  w4dimenscor
 * Added the ability to SKIN mobs, and the ability to add skin to mobs in olc, added ability to set what log a tree ill make and how many it will make
 *
 * Revision 1.21  2005/09/16 10:20:10  w4dimenscor
 * Added a snippet for making the obj and mob list hashed for fast lookups, i fixed a bug in the mccp and mxp protocols, added to objects the ability to remember who has ID'd them before so that when that person examines the item, they 'remember' what the stats are
 *
 * Revision 1.20  2005/08/28 09:04:08  w4dimenscor
 * fixed redit to stop it removing existing scripts
 *
 * Revision 1.19  2005/08/28 02:59:21  w4dimenscor
 * fixed a memory leak situation that could appear with overlapping rooms, adjusted the ignore save to not save certain words, adjusted the mobs to it a little softer
 *
 * Revision 1.18  2005/08/14 02:27:13  w4dimenscor
 * added shiftable objects flag for the pull command, added to dg_variables ability to SET object values from variables, hopefully fixed issue where triggers would be removed from rooms after editing.
 *
 * Revision 1.17  2005/08/07 04:12:39  w4dimenscor
 * Manu changes and command have been made, sorry for the lack of description. Main changes include command landscape, fixes to helpfile stuff, subskill fixes
 *
 * Revision 1.16  2005/05/28 05:52:14  w4dimenscor
 * Fixed some errors in copyover, added MXP
 *
 * Revision 1.15  2005/04/26 10:15:18  w4dimenscor
 * fixed the player timeouts, so we will no longer have thousands of users that don't play and yet still slow us down. requirelents to be deleted: any seeker who hasn't logged in within 90 days and is less than level 40 will be deleted. these requirements wiped about 8000 players from our list hehe.
 *
 * Revision 1.14  2005/04/23 12:18:13  w4dimenscor
 * Fixed some buffer read errors in the fread_string function, also fixed (temp) an index search issue for real_trigger()
 *
 * Revision 1.13  2005/02/25 07:33:47  w4dimenscor
 * reformatted some code, fixed up coventry to ignore socials
 *
 * Revision 1.12  2005/02/25 05:02:45  w4dimenscor
 * added new commands and a few little changes - i forget what eek
 *
 * Revision 1.11  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.10  2004/12/17 07:13:20  w4dimenscor
 * A few little updates.
 *
 * Revision 1.9  2004/12/07 09:31:26  w4dimenscor
 * Trees modularized, fix added to message event
 *
 * Revision 1.8  2004/12/05 09:46:52  w4dimenscor
 * fixed mtransform, fixed format in clan tell, and added limit on magic items carried, and lowered weight of magic items, and increased cost
 *
 * Revision 1.7  2004/12/04 07:42:36  w4dimenscor
 * fixed the locker bug, and the format error in clan tells, and a few other cleanups
 *
 * Revision 1.6  2004/11/27 22:34:58  w4dimenscor
 * fixed the skills attacks, added an extra multi to elemental spells for when they are affected by mind enhancements
 *
 * Revision 1.5  2004/11/27 20:16:46  w4dimenscor
 * fixed bug in 'get all all.corpse' that caused infinite loop, fixed up bug in combat where event wern't being canceled properly
 *
 * Revision 1.4  2004/11/26 23:02:45  w4dimenscor
 * Added more notes into fight code, and more debugging checks, made combat reuse the event
 *
 * Revision 1.3  2004/11/20 04:43:17  w4dimenscor
 * Added more combat messages and disabled aggro and kill all for the moment
 *
 * Revision 1.2  2004/11/20 02:33:25  w4dimenscor
 * updated and cleaned up the script system
 *
 * Revision 1.1.1.1  2004/11/12 02:15:35  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.122  2004/09/24 11:34:53  molly
 * fixed automeld
 *
 * Revision 1.121  2004/09/22 09:40:42  molly
 * automeld added so that corpses arent so easily lost, and also made pk corpses lootable
 *
 * Revision 1.120  2004/08/31 10:06:55  molly
 * make speed bonus from mounts only when you know mounted combat, changed max multi of magicmissile from 1.6 to 1.7, change who layout, fix error with room editing
 *
 * Revision 1.117  2004/08/22 00:50:48  molly
 * removed all the origional help code, added the start of the xml reader.
 *
 * Revision 1.116  2004/08/15 01:12:28  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */

/**TODO:
- need to make it so that spells when cast in battle are passed completely to the combat handler
so that spell affects arent done at time of casting and damage is done at time of combat. - mord

**/
#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "handler.h"
#include "interpreter.h"
#include "clan.h"
#include "db.h"
#include "spells.h"
#include "screen.h"
#include "constants.h"
#include "dg_scripts.h"
#include "arena.h"
#include "dg_event.h"
#include "fight.h"
#include "damage.h"
#include "descriptor.h"
#include "strutil.h"

#define EXP_GAIN_SYSTEM_2 1
/* External structures */
extern struct message_list fight_messages[MAX_MESSAGES];

int corpse_mod = 0;
float skillmulti = 0;

/* Daniel Houghton's revision */
int skill_roll ( Character *ch, int skill_num );
ACMD ( do_sac );

/* External procedures */
void add_event2 ( int delay, EVENT2(*func), void *causer, void *victim, void *info );
char *str_udup(const char *txt);
void spill_gold ( Character *ch );
void add_corpse_to_list ( OBJ_DATA *corpse );
void dismount_char ( Character *ch );
int highest_tier ( Character *ch );
void Crash_crashsave ( Character *ch );
ACMD ( do_get );
ACMD ( do_split );
ACMD ( do_flee );
ACMD ( do_assist );
int thaco ( int ch_class, int level );
int ok_damage_shopkeeper ( Character *ch, Character *victim );
void save_corpses ( void );
void send_to_prf ( char *messg, Character *nosend, int prf_flags );
void brag ( Character *ch, Character *victim );
int get_pidx_from_name ( Character *ch );
void arena_kill ( Character *ch );
void diag_char_to_char ( Character *i, Character *ch );
bool is_fused ( Character *ch );
int fused_speed ( Character *ch );
int fused_AC ( Character *ch );
int fused_hitroll ( Character *ch );
int fused_dambonus ( Character *ch );
int fused_accuracy ( Character *ch );
int fused_evasion ( Character *ch );
void update_affects ( struct obj_data *obj );

/* local functions */
void delay_die ( Character *ch, Character *killer );
int shield_check ( Character *ch, Character *vict, int type, int w_type, int dam = 0 );
void make_half ( Character *ch );
void make_head ( Character *ch );
int class_min_strike ( Character *ch );
int class_max_strike ( Character *ch );
void perform_group_gain ( Character *ch, gold_int base,
                          Character *victim );
void dam_message ( int dam, Character *ch, Character *victim,
                   int w_type );
void appear ( Character *ch );
void load_messages ( void );
void free_messages ( void );
void free_messages_type ( struct msg_type *msg );
void check_killer ( Character *ch, Character *vict );
void make_corpse ( Character *ch, Character *killer );
void change_alignment ( Character *ch, Character *victim );
void death_cry ( Character *ch );
void raw_kill ( Character *ch, Character *killer );
char *replace_string ( const char *str, const char *weapon_singular,
                       const char *weapon_plural, const char *strike_sing,
                       const char *strike_plural, const char *hitcount_word );
void perform_violence ( void );
void improve_skill ( Character *ch, int skill );
int compute_armor_class ( Character *ch );
void send_not_to_spam ( const char *buf, Character *ch,
                        Character *victim, struct obj_data *weap,
                        int spam );

EVENTFUNC ( fight_event );
int calc_fight_speed ( Character* ch );
long fight_timeout_calc ( Character* ch, short type, short number );
int fight_event_hit ( Character* ch, Character* vict, short type, short number );
int fe_group_damage ( Character* ch, Character* vict, int damage, int w_type );
int fe_after_damage ( Character* ch, Character* vict, int damage, int w_type );
int fe_solo_damage ( Character* ch, Character* vict, int damage, int w_type );


int evade_hit_check ( Character *ch, Character *vict, int w_type );
int one_hit_damage ( Character *ch, int type );
int speed_update ( Character *ch );
void kill_list ( Character *ch, Character *vict );
int attack_group = 1;

bool is_same_zone(int dv, int cv);

/*mord*/
void check_timer ( obj_data *obj );
void perform_assist ( Character *ch, Character *helpee );
int speed_msg = 0;
int ch_speed = 0;
int global_dam = 0;
char buff[MAX_INPUT_LENGTH];
int speed_info ( int input, int type );
int attack_roll ( Character *attacker, Character *vict, int type );
int spell_size_dice ( Character *ch );
int spell_num_dice ( Character *ch );
int class_damroll ( Character *ch );
void poison_wep_check ( Character *ch, Character *vict,int w_type, int dam );
void kill_points ( Character *ch, Character *vict );
int next_attack_type ( Character *ch );
void halt_fighting ( Character *ch );
int next_round ( Character* ch );
int num_dice_wep ( Character *ch, short dual );
int size_dice_wep ( Character *ch, short dual );
int num_melee_tier ( Character *ch );//remort
int find_body_part ( Character *ch, int w_type );
float area_damage_multi ( int area );
int find_part_area ( int part );
int chance_hit_part ( Character *ch, int part );
void start_fighting_delay ( Character *vict, Character *ch );
int steal_affects ( Character *ch,int dam, int w_type, Character *vict );
int valid_perc ( Character *ch );
void gain_group_exp ( Character *ch, gold_int gain );
int fe_special_hit ( Character* ch, Character* vict, int type );
float skill_type_multi ( Character *ch, Character *vict, int type );
float atk_chance_multi ( int acm );
int num_casting ( Character *ch );
void death_room ( Character *ch );

/* weapon balance protos */
int perf_balance ( int weapon_type );
int curr_balance ( OBJ_DATA *wep );
int fuzzy_balance ( OBJ_DATA *wep );
const char *weapon_type_name ( OBJ_DATA *wep );
int generate_wep_length ( OBJ_DATA *wep );
int generate_wep_type ( char *name );
int gen_wep_type_from_attack ( OBJ_DATA *obj );
int get_weapon_speed ( OBJ_DATA *wep );
int get_weapon_accuracy ( OBJ_DATA *wep );
int get_weapon_evasion ( OBJ_DATA *wep );
int wep_hands ( OBJ_DATA *wep );

int fighter_damroll ( Character *ch );
int caster_damroll ( Character *ch );



struct attack_hit_type attack_hit_text[] =
{
	{"hit", "hits"
	}
	,          /* 0 */
	{"sting", "stings"},
	{"whip", "whips"},
	{"slash", "slashes"},
	{"bite", "bites"},        /*  5 */
	{"bludgeon", "bludgeons"},
	{"crush", "crushes"},
	{"pound", "pounds"},
	{"claw", "claws"},
	{"maul", "mauls"},        /* 10 */
	{"thrash", "thrashes"},
	{"pierce", "pierces"},
	{"blast", "blasts"},
	{"punch", "punches"},
	{"stab", "stabs"},        /* 15 */
	{"kick", "kicks"},
	{"gore", "gores"},
	/* spell attacks */
	{"orb", "orbs"},
	{"spark", "sparks"},
	{"pulse", "pulses"},
	{"beam", "beams"},
	{"spear", "spears"},
	{"bolt", "bolts"},
	{"blast", "blasts"},
	{"burst", "bursts"},
	{"discharge", "discharge"},
	{"eruption", "eruption"},
	{"torrent", "torrent"},
	{"torpedo", "torpedo"},
	{"thrust", "thrusts"}
};
struct hit_chance_type chance_message[] =
{
	{"", ""}
	,               /*should never see this one */
	{"graze", "grazes"},
	{"strike", "strikes"},
	{"penetrate", "penetrates"}
};
struct weapon_type_data weapon_type_info[MAX_WEAPON_TYPES] =
{
	{
		ONE_HANDED,  0,   0,  0,   0,  0,   0, 25, "Standard"
	},
	{ONE_HANDED, 30, -130,  0, 0, 40, -80, 25, "Knife"},
	{ONE_HANDED, 20, -90,  5, -20, 40, -80, 30, "Dagger"},
	{ONE_HANDED, 15, -70, 10, -30, 45, -90, 20, "Shortsword"},
	{TWO_HANDED, 30, -130, 40, -90, 120,  -160, 25, "Longsword"},
	{TWO_HANDED, 50, -130, 60, -90, 120,  -160, 25, "Lightsaber"},
	{ONE_HANDED, 40, -60, 45, -100, 100, -140, 25, "Katana"},
	{ONE_HANDED, 15, -70, 10, -30, 40, -80, 20, "Rapier"},
	{ONE_HANDED, 15, -70, 20, -50, 35, -70, 30, "Cutlass"},
	{TWO_HANDED, 30, -130, 55, -120, 140, -180, 45, "Broadsword"},
	{ONE_HANDED, 20, -90,  5, -20, 40, -80, 60, "HalfAxe"},
	{TWO_HANDED, 25, -110, 35, -80, 80, -100, 60, "Axe"},
	{ONE_HANDED, 15, -70,  5, -20, 40, -80, 60, "WarHammer"},
	{ONE_HANDED, 20, -60,  5, -20, 45, -80, 65, "Mace"},
	{TWO_HANDED,50, -210, 50, -110, 80, -90, 25, "Shortstaff"},
	{TWO_HANDED,50, -210, 60, -130, 120, -160, 40, "Staff"},
	{ONE_HANDED,  0,   0,  0,   0,  0,   -30, 15, "Whip"},
	{ONE_HANDED, 15, -70, 10, -30, 40, -80, 65, "Club"},
	{ONE_HANDED, 15, -70, 10, -30, 40, -80, 40, "Teeth"},
	{ONE_HANDED, 15, -70, 10, -30, 40, -80, 35, "Claws"},
	{ONE_HANDED, 15, -70, 10, -30, 40, -80, 40, "Projectile"}
};

float staff_multi ( Character *ch, struct obj_data *staff )
{

	int staff_type;
	float multi = 0;
	if ( staff && ( GET_OBJ_TYPE ( staff ) == ITEM_FOCUS_MINOR||GET_OBJ_TYPE ( staff ) == ITEM_FOCUS_MAJOR ) )
	{
		switch ( ( staff_type = GET_OBJ_VAL ( staff, 2 ) ) )
		{
			case FOCUS_STAFF:
				multi += ( ( ( ( float ) ( GET_OBJ_VAL ( staff, 3 ) /1000 ) +num_casting ( ch ) ) * 100.0 ) + ( ( GET_LEVEL ( ch ) * 6.0 ) ) );
				multi = IRANGE ( 1.55, ( multi/1000.0 ), 2.15 );
				break;
			case FOCUS_ORB:
				multi += ( ( float ) GET_OBJ_VAL ( staff, 0 ) );
				if ( multi > 3000 && GET_OBJ_TIMER ( staff ) > 1 )
					GET_OBJ_TIMER ( staff ) = 1;
				multi = IRANGE ( 1.05, ( multi/700.0 ), 1.50 );
				break;
			case FOCUS_ORBSTAFF:
				multi += ( GET_OBJ_VAL ( staff, 0 ) *0.001 );
				break;
		}
		if ( GET_OBJ_TYPE ( staff ) == ITEM_FOCUS_MAJOR )
			multi *=2.0;

		return multi;
	}
	else
		return 0;
}
float has_staff ( Character *ch )
{
	if ( !ch ) //its being called from an object spell
		return 1.0;
	return staff_multi ( ch,GET_EQ ( ch, WEAR_FOCUS ) );
}
/*Affects from spells should change these dice*/

float has_staff_multi ( Character *ch, int wtype)
{
	if (!ch) 
		return 1.0;
       // Insert Logic here for elemental_type(wtype) vs staff's var for elements 
          if (GET_EQ(ch, WEAR_FOCUS)) {
	if ((elemental_type(wtype) == ELEM_FIRE) &&  (IS_SET_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_FOCUS)), ITEM_FIRE_FOCUS))) {
        return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS))+float(((float)GET_OBJ_RENT(GET_EQ(ch, WEAR_FOCUS))/(float)100.0));  }
 
        if ((elemental_type(wtype) == ELEM_ICE) &&  (IS_SET_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_FOCUS)), ITEM_ICE_FOCUS))) {
        return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS))+float(((float)GET_OBJ_RENT(GET_EQ(ch, WEAR_FOCUS))/(float)100.0));  }

        if ((elemental_type(wtype) == ELEM_EARTH) &&  (IS_SET_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_FOCUS)), ITEM_EARTH_FOCUS))) {
        return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS))+float(((float)GET_OBJ_RENT(GET_EQ(ch, WEAR_FOCUS))/(float)100.0));  }

       if ((elemental_type(wtype) == ELEM_WATER) &&  (IS_SET_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_FOCUS)), ITEM_WATER_FOCUS))) {
        return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS))+float(((float)GET_OBJ_RENT(GET_EQ(ch, WEAR_FOCUS))/(float)100.0));  }

       if ((elemental_type(wtype) == ELEM_ELEC) &&  (IS_SET_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_FOCUS)), ITEM_ELEC_FOCUS))) {
        return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS))+float(((float)GET_OBJ_RENT(GET_EQ(ch, WEAR_FOCUS))/(float)100.0));  }

       if ((elemental_type(wtype) == ELEM_AIR) &&  (IS_SET_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_FOCUS)), ITEM_AIR_FOCUS))) {
        return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS))+float(((float)GET_OBJ_RENT(GET_EQ(ch, WEAR_FOCUS))/(float)100.0));  }

       if ((elemental_type(wtype) == ELEM_SPIRIT) &&  (IS_SET_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_FOCUS)), ITEM_SPIRIT_FOCUS))) {
        return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS))+float(((float)GET_OBJ_RENT(GET_EQ(ch, WEAR_FOCUS))/(float)100.0));  }

      if ((elemental_type(wtype) == ELEM_MIND) &&  (IS_SET_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_FOCUS)), ITEM_MIND_FOCUS))) {
        return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS))+float(((float)GET_OBJ_RENT(GET_EQ(ch, WEAR_FOCUS))/(float)100.0));  }

       if ((elemental_type(wtype) == ELEM_DEATH) &&  (IS_SET_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_FOCUS)), ITEM_DEATH_FOCUS))) {
        return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS))+float(((float)GET_OBJ_RENT(GET_EQ(ch, WEAR_FOCUS))/(float)100.0));  }

       else 
	return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS));



     }
       else 
       return staff_multi (ch, GET_EQ(ch, WEAR_FOCUS));
}

int spell_size_dice ( Character *ch )
{
	int chclass = GET_CLASS ( ch );
	int sdice = 0;

	/*affects*/
	if ( AFF_FLAGGED ( ch, AFF_NUMB_MIND ) )
		sdice -=2;

	if ( AFF_FLAGGED ( ch, AFF_MIND_ELEC ) )
		sdice += 7;
	else if ( AFF_FLAGGED ( ch, AFF_MIND_FIRE ) )
		sdice += 7;
	else if ( AFF_FLAGGED ( ch, AFF_MIND_WATER ) )
		sdice += 7;
	else if ( AFF_FLAGGED ( ch, AFF_MIND_ICE ) )
		sdice += 7;

	if ( AFF_FLAGGED ( ch, AFF_BATTLE_RAGE ) )
		sdice += 1;

	if ( IS_NPC ( ch ) )
		return ( GET_LEVEL ( ch ) /3 ) + sdice;

	switch ( chclass )
	{
		case CLASS_MAGE:
		case CLASS_PRIEST:
		case CLASS_ESPER:
			sdice += 8  + highest_tier ( ch );
			sdice += ( GET_LEVEL ( ch ) >30 );
			sdice += ( GET_LEVEL ( ch ) >45 );
			sdice += ( GET_INT ( ch ) > 14 );
			sdice += ( GET_INT ( ch ) > 18 );
			sdice += ( GET_INT ( ch ) > 21 );
			break;
		case CLASS_RANGER:
		case CLASS_GYPSY:
			sdice = ( GET_LEVEL ( ch ) /6 );
			break;
		case CLASS_WARRIOR:
		case CLASS_THIEF:
		case CLASS_HUNTER:
			sdice = ( GET_LEVEL ( ch ) /10 );
			break;
		default:
			log ( "ERROR: spell_dice_size can't find someone class!" );
			return 1;
			break;
	}

	if ( is_fused ( ch ) )
		sdice += fused_speed ( ch ) / 100;
	else sdice += GET_SPEED ( ch ) / 100;

	return MAX ( 0, ( ( int ) sdice ) );
}

int spell_num_dice ( Character *ch )
{

	int chclass = GET_CLASS ( ch );
	int ndice = 0;

	if ( AFF_FLAGGED ( ch, AFF_NUMB_MIND ) )
		ndice -=2;

	if ( AFF_FLAGGED ( ch, AFF_MIND_ELEC ) )
		ndice += 7;
	else if ( AFF_FLAGGED ( ch, AFF_MIND_FIRE ) )
		ndice += 7;
	else if ( AFF_FLAGGED ( ch, AFF_MIND_WATER ) )
		ndice += 7;
	else if ( AFF_FLAGGED ( ch, AFF_MIND_ICE ) )
		ndice += 7;

	if ( AFF_FLAGGED ( ch, AFF_BATTLE_RAGE ) )
		ndice += 1;

	if ( IS_NPC ( ch ) )
		return ( GET_LEVEL ( ch ) /2 ) + ndice;

	switch ( chclass )
	{
		case CLASS_MAGE:
		case CLASS_PRIEST:
		case CLASS_ESPER:
			ndice += 7;
			ndice += ( GET_LEVEL ( ch ) >10 );
			ndice += ( GET_LEVEL ( ch ) >30 );
			ndice += ( GET_LEVEL ( ch ) >45 );
			ndice += ( GET_INT ( ch ) > 14 );
			ndice += ( GET_INT ( ch ) > 18 );
			ndice += ( GET_INT ( ch ) > 21 );
			break;
		case CLASS_RANGER:
			ndice = ( GET_LEVEL ( ch ) /  8);
                        break;
		case CLASS_GYPSY:
			ndice = ( GET_LEVEL ( ch ) / 6 );
			break;
		case CLASS_WARRIOR:
		case CLASS_THIEF:
		case CLASS_HUNTER:
			ndice = ( GET_LEVEL ( ch ) /11 );
			break;
		default:
			log ( "ERROR: spell_num_size can't find %s's class!", GET_NAME ( ch ) );
			return 1;
			break;

	}

	return MAX ( 0, ( ( int ) ndice ) );
}

int has_weapon ( Character *ch )
{

	int count = 0;
	struct obj_data *wep1 = GET_EQ ( ch, WEAR_WIELD );
	struct obj_data *wep2 = GET_EQ ( ch, WEAR_WIELD_2 );

	/* Find the wielded weapon's type */
	if ( wep1 && GET_OBJ_TYPE ( wep1 ) == ITEM_WEAPON )
		count++;
	if ( count && wep2 && GET_OBJ_TYPE ( wep2 ) == ITEM_WEAPON )
		count++;

	return count;

}

/* -1 == no object and no valid w_type
    0 == isn't short
    1 == is short
*/
int is_short_wep ( struct obj_data *obj )
{
	if ( obj && ( GET_OBJ_TYPE ( obj ) == ITEM_WEAPON ) && GET_WEP_LENGTH ( obj ) <= 60 )
		return 1;
	else
		return 0;
}


int size_dice_wep ( Character *ch, short dual )
{
	struct obj_data *weapon;
	int d_add = 0, mnt = 0;

	if ( dual == WEAPON_PRIM_AFF || dual == WEAPON_PRIM_NOAFF )
		weapon = GET_EQ ( ch, WEAR_WIELD );
	else
		weapon = GET_EQ ( ch, WEAR_WIELD_2 );

	if ( !weapon )
		return 1;

	if ( GET_OBJ_TYPE ( weapon ) != ITEM_WEAPON )
		return 1;
	d_add = GET_OBJ_VAL ( weapon, 1 );

	if ( dual == WEAPON_PRIM_AFF || dual == WEAPON_SECO_AFF )
	{

		d_add += ( GET_CON ( ch ) >  21 );
		d_add += ( GET_CON ( ch ) >  18 );
		d_add += ( GET_CON ( ch ) >  14 );
		d_add += ( GET_DEX ( ch ) >= 18 );
		if ( GET_CLASS ( ch ) == CLASS_WARRIOR || GET_CLASS ( ch ) == CLASS_HUNTER )
		{
			d_add += ( GET_DEX ( ch ) >= 22 );

			d_add += ( GET_ADD ( ch ) >  50 );
		}

		d_add += ( wep_hands ( weapon ) == 2 ? 2 : 0 );

		/*affects*/
		if ( AFF_FLAGGED ( ch, AFF_BLADEDANCE ) )
			d_add += 2;
		if ( AFF_FLAGGED ( ch, AFF_TRUE_STRIKING ) )
			d_add += 2;
		if ( AFF_FLAGGED ( ch, AFF_GRIP ) )
			d_add += 2;
		if ( AFF_FLAGGED ( ch, AFF_BESERK ) )
			d_add += 2;
		if ( is_fused ( ch ) )
			d_add += fused_speed ( ch ) / 100;
		else d_add += GET_SPEED ( ch ) /100;


		if ( RIDING ( ch ) && HERE ( ch, RIDING ( ch ) ) )
		{
			mnt = total_chance ( ch, SKILL_MOUNTED_COMBAT );
			if ( mnt )
				d_add += ( ( int ) GET_PERC ( RIDING ( ch ) ) /25 ) + ( mnt/25 );
		}


		if ( ( GET_SUB ( ch, SUB_LOYALDAMAGE ) ) > 0 )
			d_add += 3;


	}
	if ( !IS_NPC ( ch ) )
	{
		switch ( ( int ) GET_CLASS ( ch ) )
		{
			case CLASS_MAGE:
			case CLASS_PRIEST:
			case CLASS_ESPER:
				d_add -= 7;
		}
	}
	return d_add;
}

int num_dice_wep ( Character *ch, short dual )
{
	struct obj_data *weapon;
	int d_add = 0;

	if ( dual == WEAPON_PRIM_AFF || dual == WEAPON_PRIM_NOAFF )
		weapon = GET_EQ ( ch, WEAR_WIELD );
	else
		weapon = GET_EQ ( ch, WEAR_WIELD_2 );

	if ( !weapon )
		return 1;

	if ( GET_OBJ_TYPE ( weapon ) != ITEM_WEAPON )
		return 1;

	d_add = GET_OBJ_VAL ( weapon, 2 );


	if ( dual == WEAPON_PRIM_AFF || dual == WEAPON_SECO_AFF )
	{
		d_add += ( wep_hands ( weapon ) == 3 ? 3 : 0 );
		if ( GET_CLASS ( ch ) == CLASS_WARRIOR || GET_CLASS ( ch ) == CLASS_HUNTER )
		{
			d_add += ( GET_ADD ( ch ) > 80 );
			d_add += ( GET_CON ( ch ) > 18 );
		}
		d_add += ( GET_DEX ( ch ) >= 22 );
		d_add += ( GET_CON ( ch ) >= 22 );

		d_add += ( GET_DEX ( ch ) >= 18 );
		d_add += ( GET_STR ( ch ) >= 22 );
		/*affects*/
		if ( AFF_FLAGGED ( ch, AFF_TRUE_STRIKING ) )
			d_add += 2;
		if ( AFF_FLAGGED ( ch,AFF_GRIP ) )
			d_add += 2;
		//Take more if attacker is a tier
		d_add += highest_tier ( ch );
	}
	if ( !IS_NPC ( ch ) )
	{
		switch ( ( int ) GET_CLASS ( ch ) )
		{
			case CLASS_MAGE:
			case CLASS_PRIEST:
			case CLASS_ESPER:
				d_add -= 3;
		}
	}
	if ( IS_SET_AR ( GET_OBJ_EXTRA ( weapon ), ITEM_LIGHTSABRE ) && GET_SUB ( ch, SUB_LIGHTSABER_PROF ) > 0 )
		d_add += GET_SUB ( ch, SUB_LIGHTSABER_PROF ) / 49;

	return d_add;
}


int average_damage ( Character *ch )
{
	float dam = 0;

	if ( !IS_NPC ( ch ) )
	{
		switch ( find_fe_type ( ch ) )
		{
			case FE_TYPE_SPELL:
				if ( is_fused ( ch ) )
					dam = fused_dambonus ( ch );
				else dam = caster_damroll ( ch );
				dam += 0.5 * ( ( ( spell_size_dice ( ch ) +1 ) ) * spell_num_dice ( ch ) );
				break;
			default:
				if ( is_fused ( ch ) )
					dam = fused_dambonus ( ch ) * 0.75;
				else dam = fighter_damroll ( ch ) * 0.75;
				dam += 0.5 * ( ( ( size_dice_wep ( ch, WEAPON_PRIM_AFF ) +1 ) ) * num_dice_wep ( ch, WEAPON_PRIM_AFF ) );
				break;
		}
	}
	else
	{
		dam += ( ( ( ch->mob_specials.damsizedice +1 ) ) * ch->mob_specials.damnodice ) * 0.5;
		dam += dam * ( MOB_TIER ( ch ) * 0.25 );
	}


	return ( int ) dam;
}


void start_fighting ( Character* ch, Character* vict )
{
	struct combine_data *temp, *tnext;
	Character *k;
	struct follow_type *f, *fnext;
	Character *victim;
	long vict_id = -1;

	/*to stop recursion*/
	if ( AFF_FLAGGED ( ch, AFF_SNEAK ))
        {
                affect_from_char ( ch, SKILL_SNEAK );
                *ch << "You come out of concealment and attack!\r\n";
        }
	if (AFF_FLAGGED ( vict, AFF_SNEAK))
	{
		affect_from_char ( vict, SKILL_SNEAK);
		*vict <<"You've been spotted!\r\n";
	}

	if ( !ch || !vict )
		return;
#if defined(EXP_GAIN_SYSTEM_1)
	if ( RIDDEN_BY ( vict ) )
		victim = vict->RiderHere() ? RIDDEN_BY ( vict ) : vict;
	else
#endif
		victim = vict;

	if ( !victim || DEAD ( ch ) || DEAD ( victim ) || !HERE ( ch, victim ) || SELF ( ch, victim ) )
		return;

	if ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_PEACEFUL ) )
	{
		*ch << "This room just has such a peaceful, easy feeling...\r\n";
		return;
	}

	if ( AFF_FLAGGED ( ch, AFF_INVISIBLE ) )
	{
		ch->appear();
		*ch << "You slowly fade into existence.\r\n";
	}

	if ( AFF_FLAGGED ( victim, AFF_SLEEP ) )
		affect_from_char ( victim, SPELL_SLEEP );

	if ( AFF_FLAGGED ( victim, AFF_SWEET_DREAMS ) )
		affect_from_char ( victim, SPELL_SWEET_DREAMS );

	if ( !can_fight ( ch, victim, FALSE ) )
		return;

	vict_id = GET_ID ( victim );

	if ( GET_POS ( ch ) > POS_STUNNED && FIGHTING ( ch ) == NULL )
	{

		if ( FIGHTING ( victim ) )
			start_fighting_delay ( ch, victim );
		else
		{
			long ch_id = GET_ID ( ch );
			FIGHTING ( ch ) = victim;
			if ( GET_POS ( ch ) > POS_STUNNED )
				GET_POS ( ch ) = POS_FIGHTING;

			if ( fight_event_hit ( ch, victim, find_fe_type ( ch ), next_attack_type ( ch ) ) >= 0 )
			{
				if ( ( ch = find_char ( ch_id ) ) )
				{
					next_round ( ch );
				}
				else
					return;
			}
		}
	}
	if ( ( victim = find_char ( vict_id ) ) )
	{
		if ( IS_NPC ( ch ) && !DEAD ( ch ) && victim )
		{
			if ( ch->mob_specials.head_join )
				temp = ch->mob_specials.head_join->mob_specials.join_list;
			else
				temp = ch->mob_specials.join_list;
			while ( temp )
			{
				tnext = temp->next;
				if ( temp->joined )
					start_fighting_delay ( temp->joined, victim );
				temp = tnext;
			}
		}
	}
	if ( FIGHTING ( ch ) )
	{
		k = ( ch->master ? ch->master : ch );
		for ( f = k->followers; f; f = fnext )
		{
			fnext = f->next;
			if ( !f->follower )
				continue;
			if ( !FIGHTING ( ch ) )
				break;
			if ( DEAD ( FIGHTING ( ch ) ) )
				break;
			if ( !HERE ( f->follower,victim ) )
				continue;
			if ( SELF ( f->follower, ch ) )
				continue;
			if ( GET_PERC ( f->follower ) == 0 )
				continue;
			if ( IS_NPC ( f->follower ) && !AFF_FLAGGED ( f->follower, AFF_CHARM ) )
				continue;
			if ( IS_NPC ( f->follower ) && RIDDEN_BY ( f->follower ))
			        continue;

			perform_assist ( f->follower, ch );
		}
	}
}


Character *find_random_victim ( Character *ch )
{
	Character *vict;
	for ( vict = IN_ROOM ( ch )->people; vict ; vict = vict->next_in_room )
	{
		if ( SELF ( ch, vict ) )
			continue;
		if ( AFF_FLAGGED ( ch, AFF_CHARM ) )
		{
			if ( !IS_NPC ( ch->master ) && !IS_NPC ( vict ) )
				continue;
			if ( ch->master == vict )
				continue;
		}
		//continue;
		if ( vict->master == ch )
			continue;
		if ( ch->master == vict )
			if ( !CAN_SEE ( ch, vict ) )
				continue;
		if ( !can_fight ( ch, vict, TRUE ) )
			continue;
		if ( !CONFIG_PK_ALLOWED && !arena_ok ( ch, vict ) )
		{
			if ( !IS_NPC ( vict ) && !IS_NPC ( ch ) )
				continue;
		}
		if ( FIGHTING ( ch ) != vict )
			return vict;

	}

	return NULL;
}

void start_fighting_delay ( Character *ch, Character *vict )
{
	Character *victim = vict;
	/*to stop recursion*/
	if ( !ch || !vict )
		return;
	if ( DEAD ( ch ) || DEAD ( vict ) )
		return;
#if defined(EXP_GAIN_SYSTEM_1)
	victim = vict->RiderHere() ? RIDDEN_BY ( vict ) : vict;
#endif

	if ( !HERE ( ch, victim ) || SELF ( ch, victim ) )
		return;

	if ( AFF_FLAGGED ( victim, AFF_SLEEP ) )
		affect_from_char ( victim, SPELL_SLEEP );

	if ( AFF_FLAGGED ( victim, AFF_SWEET_DREAMS ) )
		affect_from_char ( victim, SPELL_SWEET_DREAMS );

	if ( AFF_FLAGGED ( ch, AFF_INVISIBLE ) )
	{
		ch->appear();
		*ch << "You slowly fade into existence.\r\n";
	}

	if ( !can_fight ( ch, victim, FALSE ) )
		return;

	if ( GET_POS ( ch ) > POS_STUNNED )
	{
		FIGHTING ( ch ) = victim;
		GET_POS ( ch ) = POS_FIGHTING;
		next_round ( ch );
	}

}


int next_round ( Character* ch )
{
	Character *victim;

	/*to stop recursion*/
	if ( !ch )
		return 0;
	if ( DEAD ( ch ) )
		return 0;
	victim = FIGHTING ( ch );
#if defined(EXP_GAIN_SYSTEM_1)
	if ( ( victim = FIGHTING ( ch ) ) != NULL )
		victim = victim->RiderHere() ? RIDDEN_BY ( victim ) : victim;
#endif

	/* check to see if there is anyone in the room fighting you however! */
	if ( !victim )
		victim = ch->NextFightingMe();

	/*to stop recursion*/
	if ( !victim || !can_fight ( ch, victim, FALSE ) )
	{
		stop_fighting ( ch );
		return 0;
	}

	if ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_PEACEFUL ) )
	{
		stop_fighting ( ch );
		send_to_char ( "This room just has such a peaceful, easy feeling...\r\n", ch );
		return 0;
	}

	if ( GET_FIGHT_EVENT ( ch ) != NULL )
	{
		/*log ( "%s has %ld more time left till fight event", GET_NAME ( ch ), event_time ( ( struct event * ) GET_FIGHT_EVENT ( ch ) ) );*/
		return 0;
	}


	if ( GET_POS ( ch ) > POS_STUNNED )
	{

		struct fight_event_obj *ch_event = NULL;
		int fe_t = find_fe_type ( ch );
		long time = fight_timeout_calc ( ch, fe_t, 0 );

		ch_event = new fight_event_obj ( GET_ID ( ch ) );
		GET_FIGHT_EVENT ( ch ) = event_create ( fight_event, ch_event, time, EVENT_TYPE_FIGHT );
	}
	else
	{
		stop_fighting ( ch );
	}
	if ( IS_NPC ( ch ) )
	{

		struct combine_data *temp, *tnext;
		if ( ch->mob_specials.head_join )
			temp = ch->mob_specials.head_join->mob_specials.join_list;
		else
			temp = ch->mob_specials.join_list;
		while ( temp )
		{
			tnext = temp->next;
			if ( temp->joined && !FIGHTING ( temp->joined ) )
				start_fighting_delay ( temp->joined, victim );
			temp = tnext;
		}
	}
	return 1;
}


EVENTFUNC ( fight_event )
{
	struct fight_event_obj* fight = ( struct fight_event_obj* ) event_obj;
	long id = fight->id;
	Character *ch = find_char ( id );
	if ( ch )
	{
		/** debugging **/
#if 0
		{
			int found = FALSE;
			Character *tch;
			for ( tch = character_list;tch&&!found;tch = tch->next )
			{
				/** Assume Unique **/
				if ( tch == ch )
					found = TRUE;
			}
			if ( !found )
			{
				log ( "Character not found for combat!\r\n" );
				return 0;
			}
		}
#endif
		GET_FIGHT_EVENT ( ch ) = NULL;
		if ( fight )
			delete fight;
		/* removed because this part is where gdb would sometimes get segfaults - mord */
		//    if (!DEAD(ch) && FIGHTING(ch) && !DEAD(FIGHTING(ch)))
		// if (RIDDEN_BY(FIGHTING(ch)) && HERE(RIDDEN_BY(FIGHTING(ch)), FIGHTING(ch)))
		//   FIGHTING(ch) = RIDDEN_BY(FIGHTING(ch));

		if ( FIGHTING ( ch ) && can_fight ( ch, FIGHTING ( ch ), FALSE ) && GET_POS ( ch ) > POS_STUNNED )
		{
			int fe_t = 0;
			int a_t = 0;
			fe_t = find_fe_type ( ch );
			a_t = next_attack_type ( ch );
			Character * tch;

			if ( fight_event_hit ( ch, FIGHTING ( ch ), fe_t, a_t ) >= 0 )
			{
				if ( ( tch = find_char ( id ) ) != NULL )
				{
					if ( tch == ch && !DEAD ( ch ) )
						next_round ( ch );
					else
						return 0;
				}
				else
					return 0;
			}
			else
			{
				if ( !IS_NPC ( ch ) && GET_SWEEP_DAM ( ch ) )
				{
					Character *victnext = find_random_victim ( ch );
					if ( victnext )
					{
						act ( "You sweep around into $N!", TRUE, ch, NULL, victnext, TO_CHAR );
						act ( "$n sweeps around into you!", TRUE, ch, NULL, victnext, TO_VICT );
						FIGHTING ( ch ) = victnext;
						GET_POS ( ch ) = POS_FIGHTING;
						fight_event_hit ( ch, victnext, find_fe_type ( ch ), next_attack_type ( ch ) );
					}
					else
						GET_SWEEP_DAM ( ch ) = 0;
				}
				stop_fighting ( ch );
				GET_NEXT_SKILL ( ch ) = TYPE_UNDEFINED;
			}
		}
	}
	return 0;
}

int next_attack_type ( Character *ch )
{

	/* Attack type, this calls special attacks
	   from subskills or normal skill that are
	   counted as a single move when fighting. -mord
	*/

	return GET_NEXT_SKILL ( ch );
}


void skill_attack ( Character *ch, Character *vict, int skill, int pass )
{
	if ( pass )
	{
		if ( !FIGHTING ( ch ) )
			fight_event_hit ( ch, vict, 0, skill );
		else if ( GET_FIGHT_EVENT ( ch ) == NULL )
			fight_event_hit ( ch, vict, 0, skill );
		else if ( IS_SPELL_CAST ( skill ) && ( IS_SET ( spell_info[skill].routines, MAG_AREAS ) || GET_SPELL_DIR ( ch ) != NOWHERE ) )
			fight_event_hit ( ch, vict, 0, skill );
		else
		{
			if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
			{
				if ( ch->Flying() )
					ch->Send ( "%s", stance_change[number ( 0, 2 ) ] );
				else
					ch->Send ( "%s", fly_stance_change[number ( 0, 2 ) ] );
			}

			GET_NEXT_SKILL ( ch ) = skill;
			GET_NEXT_VICTIM ( ch ) = GET_ID ( vict );
		}
	}
	else
		fe_after_damage ( ch, vict, 0, skill );

}


int calc_fight_speed ( Character* ch )
{
	float to_ret;

	if ( is_fused ( ch ) )
		to_ret = fused_speed ( ch );
	else
		to_ret = speed_update ( ch );

	if ( IS_NPC ( ch ) )
		to_ret += ( number ( GET_LEVEL ( ch ), GET_LEVEL ( ch ) * 5 ) );
	else if ( total_chance ( ch, SKILL_MELEE ) > number ( 0, 101 ) )
	{
		if ( !number ( 0, 10 ) )
			improve_skill ( ch, SKILL_MELEE );
	}
	if ( total_chance ( ch, SKILL_SECOND_ATTACK ) > number ( 0, 101 ) )
	{
		if ( !number ( 0, 10 ) )
			improve_skill ( ch, SKILL_SECOND_ATTACK );
	}
	if ( total_chance ( ch, SKILL_THIRD_ATTACK ) > number ( 0, 101 ) )
	{
		if ( !number ( 0, 10 ) )
			improve_skill ( ch, SKILL_THIRD_ATTACK );
	}
	if ( !IS_NPC ( ch ) )
		return ( int ) IRANGE ( -1000.0, to_ret, TOP_SPEED_VALUE );
	else
		return ( int ) IRANGE ( -1000.0, to_ret, TOP_MOB_SPEED_VALUE );
}

float race_dam_mod ( int race, int magic )
{
	float damage = 1.0f;

	switch ( race )
	{
		case ( RACE_SPACE_WOLF ) :
						if ( !magic )
							damage = 1.15f;
				else
					damage = 0.80f;
			break;
		case ( RACE_DWARF ) :
						damage = 0.9f;
			break;
		case ( RACE_ELF ) :
						if ( !magic )
							damage = 0.9f;
				else
					damage = 1.1f;
			break;
		case ( RACE_MARTIAN ) :
						if ( magic )
							damage = 1.05f;
			break;
		case ( RACE_CENTAUR ) :
						if ( !magic )
							damage = 1.10f;
			break;
	}

	return damage;
}


int modify_dam ( int dam, Character *ch, Character *vict , int w_type )
{
	float damage = dam;
	Character *k = NULL;
	struct follow_type *f;
	int skill_cost ( int h, int m, int v, Character *ch );
	int wep = IS_WEAPON ( w_type );
	//  Character *mount = (RIDING(ch) && (HERE(ch, RIDING(ch)))) ? RIDING(ch) : NULL;

	if ( !IS_NPC ( vict ) )
	{
		/** Oh god what was i thinking?? - mord **/
		damage *= 0.6;
	}
	/**Take less if victim is a tier**/
	switch ( highest_tier ( vict ) )
	{
		case 4:
			damage -= damage/4;
			break;
		case 3:
			damage -= damage/5;
			break;
		case 2:
			damage -= damage/6;
			break;
	}


	/* if (damage && (IS_SPELL_ATK(w_type) || wep))
	 {
	   if ((mount || (!mount && !IS_NPC(ch) && GET_RACE(ch) == RACE_CENTAUR && ((mount = ch) != NULL)))&& GET_SKILL(ch, SKILL_MOUNTED_COMBAT))
	   {
	     //damage *= (1.0 + (total_chance(ch, SKILL_MOUNTED_COMBAT) * (0.003 + (mount != ch ? 0.001 : 0.0))));

	     //if (mount != ch)
	     //  damage += average_damage(mount);
	     if (!number(0, 100))
	       improve_skill(ch, SKILL_MOUNTED_COMBAT);
	   }
	 }*/

	if ( !IS_NPC ( ch ) )
	{
		int remorts = MIN ( REMORTS ( ch ), 50 );
		damage *= race_dam_mod ( GET_RACE ( ch ), IS_SPELL_ATK ( w_type ) || IS_SPELL_CAST ( w_type ) );
		damage += ( ( float ) damage * ( ( float ) ( remorts * 0.005 ) ) );
	}

	if ( wep && AFF_FLAGGED ( vict, AFF_BRACE ) && damage > 3 )
	{
		if ( !skill_cost ( 0, 0, 20, vict ) )
		{
			vict->Send ( "You try and brace but are too exhausted!!\r\n" );
		}
		else
		{

			damage -= damage/3;

			vict->Send ( "You were braced against the damage!\r\n" );
		}
	}
	if ( !IS_NPC ( vict ) && GET_COND ( vict, DRUNK ) > 10 && damage > 5 )
		damage -= damage/5;



	if ( ( GET_ALIGNMENT ( ch ) > 350 ) && affected_by_spell ( vict, SPELL_PROT_FROM_GOOD ) )
		damage -= damage/4;
	else if ( ( GET_ALIGNMENT ( ch ) < -350 ) && affected_by_spell ( vict, SPELL_PROT_FROM_EVIL ) )
		damage -= damage/4;


	if ( ( elemental_type ( w_type ) == ELEM_FIRE || affected_by_spell ( ch, SPELL_MIND_FIRE ) ) && 
        (AFF_FLAGGED(vict, AFF_RESIST_FIRE) || affected_by_spell ( vict, SPELL_PROT_FIRE )) )
		damage -= damage/4;
	else if ( ( elemental_type ( w_type ) == ELEM_ICE || affected_by_spell ( ch, SPELL_MIND_ICE ) ) && (AFF_FLAGGED(vict, AFF_RESIST_COLD) || 
            affected_by_spell ( vict, SPELL_PROT_COLD )) )
		damage -= damage/4;
        else if (elemental_type (w_type) == ELEM_ELEC && AFF_FLAGGED(vict, AFF_RESIST_ELEC))
		damage -= damage/4;


	k = ( vict->master ? vict->master : vict );
	for ( f = k->followers; f; f = f->next )
		if ( !DEAD ( vict ) && HERE ( f->follower,ch ) && FIGHTING ( f->follower ) == vict )
			damage -= damage/10;



	/**TODO: this needs to be fixed up and finished (currently crashes us)**/
	/** FIXED - Nov 9th 08 Mord */

	if ( immune_to ( vict, elemental_type ( w_type ) ) )
		damage = 0;

	damage += ( damage * ( resist_elem ( vict, elemental_type ( w_type ) / 100 ) ) );

	if ( AFF_FLAGGED ( vict, AFF_SANCTUARY ) )
	{
		if ( ( GET_CLASS ( ch ) == CLASS_WARRIOR || GET_MASTERY ( ch, CLASS_WARRIOR ) ) &&
			( GET_SUB ( ch, SUB_REPEL_SANC ) > number ( 0, 150 ) ) )
		{
			act ( "$n moves in past your white aura!", FALSE, ch, 0, vict, TO_VICT );
			act ( "You move in past $N's white aura!", FALSE, ch, 0, vict, TO_CHAR );
		}
		else
			damage *= ( 0.60 );
	}

	if ( wep && AFF_FLAGGED ( vict, AFF_SHIELD ) )
		damage -= ( damage/4 ); //25% reduction

	if ( AFF_FLAGGED ( vict, AFF_SHIELD_HOLY ) )
		damage -= ( damage/5 ); //20% reduction

	if ( affected_by_spell ( vict, SPELL_STEELSKIN ) )
		damage -= ( damage/5 ); //20% reduction

	if ( AFF_FLAGGED ( vict, AFF_STONESKIN ) )
		damage -= ( damage/5 ); //20% reduction


	if ( ( IS_SPELL_ATK ( w_type ) || IS_SPELL_CAST ( w_type ) ) &&
	        ( AFF_FLAGGED ( vict, AFF_SHIELD_MANA ) || AFF_FLAGGED ( vict, AFF_BESERK ) ) )
		damage /= 2;

	if ( AFF_FLAGGED ( vict, AFF_NUMB_MIND ) )
		damage += ( damage/6 ); //16% addition


	if ( wep )
	{
		damage *= ( 100.0 - ( float ) chance_hit_part ( vict, GET_ATTACK_POS ( ch ) ) ) /100.0;
		damage *= area_damage_multi ( find_part_area ( GET_ATTACK_POS ( ch ) ) );
	}

	/* mastery */
	// Changing this per Horus from IS_NPC(ch) to !IS_NPC(ch) - Prom
	if ( !IS_NPC(ch) )
	{
		if ( wep && GET_MASTERY ( ch, CLASS_GYPSY ) )
			damage += ( damage/5 );
	}
	// Changing this from IS_NPC(vict) to !IS_NPC(vict) - Prom
	if ( !IS_NPC(vict) )
	{
		if ( wep && GET_MASTERY ( vict, CLASS_RANGER ) )
			damage -= ( damage/5 );
		if ( !wep && GET_MASTERY ( vict, CLASS_PRIEST ) )
			damage -= ( damage/5 );
	}

	if ( GET_SPELL_DIR ( ch ) != NOWHERE && !HERE ( ch, vict ) )
	{
		int v, dist = magic_distance ( ch, w_type, GET_SPELL_DIR ( ch ), vict );
		for ( v = 0; v < dist; v++ )
			damage /= 2;
	}
#if defined(EXP_GAIN_SYSTEM_1)
	/* half of your involvement percentage is taken from the dam you do */
	damage -= ( ( 100 - valid_perc ( ch ) ) * damage ) /200;
#endif

		/* Ethos System, provides bonuses for being in alignment and penalites 
 		   for being out of alignment. in_align is a variable to easily handle the "alignment"
                   logic. Not globalizing this because currently in_align is only really being considered
                   in the fight code. */

		 sh_int  in_align = 0;
        if (( ( GET_ALIGNMENT ( ch ) > 350) && (GET_ETHOS(ch) == 1))  && (GET_ALIGNMENT(vict) < -350))
 	            in_align = 1; //good

	if (((GET_ALIGNMENT(ch) < 350) && (GET_ALIGNMENT(ch) > -350)) && ((GET_ALIGNMENT(vict) > 350) || (GET_ALIGNMENT(vict) < -350)))
		    in_align = 2; //neutral

        if (((GET_ALIGNMENT(ch) < -350) && (GET_ETHOS(ch) == 3))  && (GET_ALIGNMENT(vict) > 350)) 
                    in_align = 3; //evil


               /* Here we check if in_align is greater than zero so that we know their align actually matches
                  but we aren't checking whether they're good or evil. If we wanted to though we could query
                  if in_align == 2 and provide bonuses for evil and bonuses for good (say holy vs unholy attack
                  types or the like. */

		if (in_align)
                damage += damage/10;

		if (!in_align)
                damage -= damage/10;


	     /* Champion System (BBV Replacement) by Once.  Here we're going to give the Champion 10% more damage
                while he still holds the throne. */
                if (GET_IDNUM(ch) == CHAMPION)
                damage += damage/10;
       


	return ( int ) damage;
}
int fighter_damroll ( Character *ch )
{
	int dam = 0;
	if ( !IS_NPC ( ch ) )
	{
		switch ( GET_CLASS ( ch ) )
		{
			case CLASS_WARRIOR:
			case CLASS_HUNTER:
				if ( GET_MASTERY ( ch, CLASS_WARRIOR ) )
					dam += ( 9*GET_DAMROLL ( ch ) );
				else
					dam += ( 8*GET_DAMROLL ( ch ) );
				break;
			case CLASS_RANGER:
			case CLASS_THIEF:
			case CLASS_GYPSY:
				if ( GET_MASTERY ( ch, CLASS_WARRIOR ) )
					dam += ( 6*GET_DAMROLL ( ch ) );
				else
					dam += ( 5*GET_DAMROLL ( ch ) );
				break;
			default:
				dam += GET_DAMROLL ( ch );
		}
	}
	else
		dam += GET_DAMROLL ( ch );
	return dam;
}

int caster_damroll ( Character *ch )
{
	int dam = 0;
	if ( !IS_NPC ( ch ) )
	{
		switch ( GET_CLASS ( ch ) )
		{
			default:
			case CLASS_WARRIOR:
			case CLASS_HUNTER:
			case CLASS_RANGER:
			case CLASS_THIEF:
			case CLASS_GYPSY:
			case CLASS_ESPER:
			case CLASS_MAGE:
			case CLASS_PRIEST:
				dam += GET_INT ( ch );
				dam += GET_WIS ( ch );
				dam += GET_CHA ( ch );
				break;
		}
	}
	else
		dam += GET_DAMROLL ( ch );
	return dam;
}

/* attack_roll returns:
0 for complete miss. dam = 0
1 for graze          dam = (dam/4)
2 for strike         dam = (dam/2)
3 for penetrate      dam = dam

This function works for all attack types.
*/

int attack_roll ( Character *attacker, Character *vict, int type )
{

	int accuracy_roll = 0;
	int evasion_roll = 0;
	int totalchance = 0;
	int diceroll = 0;
	int attack_chance = 0;
	int mins=0, maxs=0;

	/* So the idea is that the function produces a number between -100 and +100
	   the higher the better.
	   
	   And depending on your class's range you will get a
	   miss, graze, strike, or penetrate... attack roll
	  
	   Thats about it, now the hard part is jugling all these numbers to give that answer
	*/
	if ( PLR_FLAGGED ( attacker, PLR_FROZEN ) )
		return ( ATK_CHANCE ( attacker ) = 0 );

	if ( IS_SKILL ( type ) || IS_SPELL_CAST ( type ) )
		return ( ATK_CHANCE ( attacker ) = 3 );

	/*gotta be a moron to not reeally fuck up a sleeping person*/
	if ( !AWAKE ( vict ) )
		return ( ATK_CHANCE ( attacker ) = 3 );

	if ( is_fused ( vict ) )
		evasion_roll = fused_evasion ( vict );
	else evasion_roll = evasion_tot ( vict );
	if ( !CAN_SEE ( vict, attacker ) && evasion_roll )
		evasion_roll /= 2;

	if ( is_fused ( attacker ) )
		accuracy_roll = fused_accuracy ( attacker );
	else accuracy_roll = accuracy_tot ( attacker );

	totalchance = FTOI ( ( ( evasion_roll * 100.0 ) / ( accuracy_roll + evasion_roll ) ) );

	mins = class_min_strike ( attacker );
	maxs = class_max_strike ( attacker );

	if ( ( diceroll = number ( 0, 101 ) ) <= 1 )
		return ( ATK_CHANCE ( attacker ) = 0 );

	/* lower totalchance, better chance of success */

	if ( diceroll < ( totalchance + mins ) )
		attack_chance = 0;
	else if ( diceroll < ( totalchance ) )
		attack_chance = 1;
	else if ( diceroll <= ( totalchance + maxs ) )
		attack_chance = 2;
	else
		attack_chance = 3;


	return ( ATK_CHANCE ( attacker ) = attack_chance );
}

float pos_multi ( int pos )
{
	switch ( pos )
	{
		case POS_STANDING:
		case POS_FIGHTING:
		default:
			return 1.0f;
			break;
		case POS_DEAD:
		case POS_MORTALLYW: /* mortally wounded     */
		case POS_INCAP:   /* incapacitated        */
			return 6.0f;
			break;
		case POS_STUNNED:
			return 4.0f;  /* stunned              */
		case POS_SLEEPING:
			return 3.0f;  /* sleeping             */
		case POS_RESTING:
			return 2.0f;  /* resting              */
		case POS_SITTING:
			return 1.2f;  /* sitting              */
	}

}


int evasion_tot ( Character *vict )
{
	//yeah i know its a typo
	int evasion_roll = 0;
	int victim_ac = 0;
	int part=0;

	if ( !AWAKE ( vict ) )
		return 0;


	evasion_roll = FTOI ( ( IS_NPC ( vict ) ? ( ( MOB_TIER ( vict ) ) + 1 ) * ( GET_LEVEL ( vict ) * 2.0 ) * ( 1 + ( GET_LEVEL ( vict ) > 30 ) + ( GET_LEVEL ( vict ) > 40 ) + ( GET_LEVEL ( vict ) > 60 ) + ( GET_LEVEL ( vict ) > 65 ) )   : GET_PERM_EVASION ( vict ) ) );

	victim_ac = ( 200 - ( 100 + vict->compute_armor_class() ) );
	if ( victim_ac != 0 )
		evasion_roll += ( victim_ac/3 ); // between 0 and 66
	if ( ( part = GET_SUB ( vict, SUB_LOYALDEFEND ) ) > 0 )
		evasion_roll += FTOI ( part * 0.5 );

	if ( !IS_NPC ( vict ) )
	{
		switch ( ( int ) GET_RACE ( vict ) )
		{
			case RACE_DWARF:
				evasion_roll += 30;
				break;
			case RACE_ELF:
				evasion_roll -= 30;
				break;
			case RACE_FAUN:
				evasion_roll += 0;
				break;
			case RACE_CENTAUR:
				evasion_roll += 40;
				break;
			case RACE_MARTIAN:
				evasion_roll -= 40;
				break;

		}
	}

	evasion_roll += ( ( GET_LEVEL ( vict ) /4 ) * highest_tier ( vict ) );

	if ( AWAKE ( vict ) )
		evasion_roll += ( 12 - ( 6 + dex_app[GET_DEX ( vict ) ].defensive ) ) * 10;
	if ( GET_MASTERY ( vict, CLASS_HUNTER ) )
		evasion_roll += 100;
	if ( AFF_FLAGGED ( vict, AFF_BESERK ) ) /* Char has gone Beserk      */
		evasion_roll -= 30;
	if ( AFF_FLAGGED ( vict, AFF_JUDO ) ) /*fighting style - rogue*/
		evasion_roll += 15;
	if ( AFF_FLAGGED ( vict, AFF_SHIELD_HOLY ) )  /*lowers chance to be hit*/
		evasion_roll += 5;
	if ( AFF_FLAGGED ( vict, AFF_SHIELD_STATIC ) ) /*lowers melee hit chance */
		evasion_roll += 10;
	if ( AFF_FLAGGED ( vict, AFF_BLUR ) ) /*lowers melee hit chance */
		evasion_roll += 40;
	if ( AFF_FLAGGED ( vict, AFF_FORSEE ) ) /*lowers melee hit chance */
		evasion_roll += 15;
	if ( AFF_FLAGGED ( vict, AFF_FORTIFY_BODY ) ) /*lowers melee hit chance */
		evasion_roll += 20;
	if ( affected_by_spell ( vict, SPELL_STEELSKIN ) ) /*lowers melee hit chance */
		evasion_roll += 25;
	if ( AFF_FLAGGED ( vict, AFF_STONESKIN ) ) /*lowers melee hit chance */
		evasion_roll += 25;
	if ( affected_by_spell ( vict, SPELL_ARMOR ) ) /*lowers melee hit chance */
		evasion_roll += 15;

	evasion_roll += get_weapon_evasion ( GET_EQ ( vict, WEAR_WIELD ) );
	evasion_roll += get_weapon_evasion ( GET_EQ ( vict, WEAR_WIELD_2 ) );

	if ( AFF_FLAGGED ( vict, AFF_CURSE ) )
		evasion_roll -= ( evasion_roll / 4 );

	evasion_roll = ( evasion_roll <= 0 ? 1 : evasion_roll );

	return evasion_roll;
}

int accuracy_tot ( Character *attacker )
{
	int accuracy_roll = 0;
	int calc_thaco;
	Character *k = NULL;
	struct follow_type *f;

	if ( !IS_NPC ( attacker ) )
		calc_thaco = ( thaco ( ( int ) GET_CLASS ( attacker ), ( int ) GET_LEVEL ( attacker ) ) );
	else
		calc_thaco = GET_LEVEL ( attacker );

	calc_thaco += str_app[STRENGTH_APPLY_INDEX ( attacker ) ].tohit; // 5 max
	calc_thaco += GET_HITROLL ( attacker ); // 70ish max, 30 avg
	calc_thaco += ( int ) ( ( GET_INT ( attacker ) - 10 ) ); // 8 max
	calc_thaco += ( int ) ( ( GET_WIS ( attacker ) - 10 ) ); //8 max

	calc_thaco = IRANGE ( -10, calc_thaco, 70 );


	accuracy_roll = FTOI ( ( IS_NPC ( attacker ) ? ( ( MOB_TIER ( attacker ) ) ) *
	                         ( GET_LEVEL ( attacker ) * 2.0 ) * ( 0.5 + ( GET_LEVEL ( attacker ) >30 ) + ( GET_LEVEL ( attacker ) >40 ) +
	                                                              ( GET_LEVEL ( attacker ) >=50 ) + ( GET_LEVEL ( attacker ) >=60 ) ) : GET_PERM_ACCURACY ( attacker ) ) );
	accuracy_roll += calc_thaco;
	if ( AFF_FLAGGED ( attacker, AFF_BESERK ) ) /* Char has gone Beserk      */
		accuracy_roll += 10;
	if ( GET_SUB ( attacker, SUB_LOYALATTACK ) )
		accuracy_roll += 50;

	accuracy_roll += total_chance ( attacker, SKILL_MELEE ) /2;
	accuracy_roll += total_chance ( attacker, SKILL_SECOND_ATTACK );
	accuracy_roll += total_chance ( attacker, SKILL_THIRD_ATTACK );
	/** This means that if you have a big group, you have more chance of hitting the victim **/
	if ( FIGHTING ( attacker ) )
	{
		k = ( FIGHTING ( attacker )->master ? FIGHTING ( attacker )->master : FIGHTING ( attacker ) );
		for ( f = k->followers; f; f = f->next )
			if ( !DEAD ( FIGHTING ( attacker ) ) && HERE ( f->follower,attacker ) && FIGHTING ( f->follower ) == attacker )
				accuracy_roll += 10;
	}



	if ( !IS_NPC ( attacker ) )
	{
		switch ( ( int ) GET_RACE ( attacker ) )
		{
			case RACE_DWARF:
				accuracy_roll += 10;
				break;
			case RACE_ELF:
				accuracy_roll += 15;
				break;
			case RACE_FAUN:
				accuracy_roll += 25;
				break;
			case RACE_CENTAUR:
				accuracy_roll += 10;
				break;
			case RACE_MARTIAN:
				accuracy_roll -= 5;
				break;

		}
	}

	if ( GET_MASTERY ( attacker, CLASS_THIEF ) )
		accuracy_roll += 100;
	if ( AFF_FLAGGED ( attacker, AFF_JUDO ) ) /*fighting style - rogue*/
		accuracy_roll += 25;
	if ( AFF_FLAGGED ( attacker, AFF_BLADEDANCE ) ) /*fighting style - rogue*/
		accuracy_roll += 25;
	if ( AFF_FLAGGED ( attacker, AFF_GODLY_BLESSING ) ) /*fighting style - rogue*/
		accuracy_roll += 5;

	if ( AFF_FLAGGED ( attacker, AFF_TRUE_STRIKING ) ) /*fighting style - rogue*/
		accuracy_roll += 30;
	if ( AFF_FLAGGED ( attacker, AFF_MARTIAL_ARTS ) ) /*fighting style - rogue*/
		accuracy_roll += 25;
	if ( AFF_FLAGGED ( attacker, AFF_FORSEE ) ) /*fighting style - rogue*/
		accuracy_roll += 10;
	if ( AFF_FLAGGED ( attacker, AFF_CONFUSED ) ) /*fighting style - rogue*/
		accuracy_roll -= 60;
	if ( AFF_FLAGGED ( attacker, AFF_CORRUPTED ) ) /*fighting style - rogue*/
		accuracy_roll -= 40;
	if ( AFF_FLAGGED ( attacker, AFF_FOCUS ) ) /*fighting style - rogue*/
		accuracy_roll += 15;
	if ( AFF_FLAGGED ( attacker, AFF_BATTLE_RAGE ) ) /*fighting style - rogue*/
		accuracy_roll += 15;
	if ( AFF_FLAGGED ( attacker, AFF_NUMB_MIND ) ) /*fighting style - rogue*/
		accuracy_roll -= 45;

	accuracy_roll += get_weapon_accuracy ( GET_EQ ( attacker, WEAR_WIELD ) );
	accuracy_roll += get_weapon_accuracy ( GET_EQ ( attacker, WEAR_WIELD_2 ) );
#if defined(EXP_GAIN_SYSTEM_1)
	accuracy_roll = ( valid_perc ( attacker ) * accuracy_roll ) /100;
#endif
	accuracy_roll = ( accuracy_roll <= 0 ? 1 : accuracy_roll );

	return accuracy_roll;
}

long fight_timeout_calc ( Character* ch, short type, short number )
{
	/* This function needs to calculate the number of heartbeats we should wait
	   through until we hit the guy again.  This calculation involves player
	   speed, skills, spells, etc.  Question is, how do we get it into heartbeat
	   terms...
	*/
	float fspeed = calc_fight_speed ( ch ) +1000.0;
	int to_ret = 0;
	float partial = 0.0f;


	partial = ( 100.0f- ( fspeed * 0.05 ) ); // puts the speed between 1 and 100 - 100 being fastest
	to_ret = ( int ) ( ( PULSES_PER_FIGHT*partial ) /100.0f ); // puts the time of the next hit between .001 and 6 seconds from now.

	return ( long ) IRANGE ( 1, FTOI ( to_ret ), PULSES_PER_FIGHT );
}

int fight_event_hit ( Character* ch, Character* vict, short type, short num )
{
	Character *k;
	struct follow_type *f;

#if defined(EXP_GAIN_SYSTEM_1)
	int perc = 0;
	int shortwep = 0;
	if ( vict && vict->RiderHere() )
		vict = RIDDEN_BY ( vict );
#endif
	if ( !can_fight ( ch, vict, FALSE ) )
		return -1;


	if ( AFF_FLAGGED ( ch, AFF_INVISIBLE ) )
	{
		ch->appear();
		*ch << "You slowly fade into existence.\r\n";
	}

	if ( RIDING ( ch ) && RIDING ( ch ) == vict )
		dismount_char ( ch );

	if ( RIDDEN_BY ( ch ) && RIDDEN_BY ( ch ) == vict )
		dismount_char ( vict );

	if ( affected_by_spell ( vict, SPELL_SLEEP ) )
	{
		affect_from_char ( vict, SPELL_SLEEP );
		act ( "$n wakes up!", TRUE, vict, 0, 0, TO_ROOM );
		act ( "You are brutally woken by $N!", TRUE, vict, 0, ch, TO_CHAR );
		GET_POS ( vict ) = POS_STANDING;
		update_pos ( vict );
	}

	if ( affected_by_spell ( vict, SPELL_SWEET_DREAMS ) )
	{
		affect_from_char ( vict, SPELL_SWEET_DREAMS );
		act ( "$n wakes up!", TRUE, vict, 0, 0, TO_ROOM );
		act ( "You are brutally woken by $N!", TRUE, vict, 0, ch, TO_CHAR );
		GET_POS ( vict ) = POS_STANDING;
		update_pos ( vict );
	}

	if ( !FIGHTING ( ch ) && HERE ( vict, ch ) )
		start_fighting_delay ( ch, vict );
	if ( !FIGHTING ( vict ) && HERE ( vict, ch ) )
		start_fighting_delay ( vict, ch );

	if ( IS_NPC ( ch ) )
	{

		fight_mtrigger ( ch );

		if ( GET_MOB_WAIT ( ch ) > 0 )
		{
			GET_MOB_WAIT ( ch ) -= ( 1 RL_SEC );
			if ( ( GET_POS ( ch ) < POS_FIGHTING ) &&GET_MOB_WAIT ( ch ) == ( 1 RL_SEC ) )
				act ( "$n gets on $s knees!", TRUE, ch, 0, 0, TO_ROOM );
			return 0;
		}

		if ( GET_MOB_WAIT ( ch ) < 0 )
			GET_MOB_WAIT ( ch ) = 0;

		if ( GET_POS ( ch ) < POS_FIGHTING )
		{
			GET_POS ( ch ) = POS_FIGHTING;
			act ( "$n climbs to $s feet!", TRUE, ch, 0, 0, TO_ROOM );
			return 0;
		}

	}
	else
	{

		/** I want this to happen but the skills still need to pass through **/
		// if (GET_WAIT_STATE(ch) > 0)
		//   return 0;


		if ( GET_POS ( ch ) == POS_SITTING )
		{
			ch->Send ( "You can't fight while sitting!" );
			return 0;
		}
		if ( GET_POS ( ch ) == POS_RESTING )
		{
			ch->Send ( "You can't fight while resting!" );
			return 0;
		}
	}

	GET_NEXT_SKILL ( ch ) = TYPE_UNDEFINED;
#if defined(EXP_GAIN_SYSTEM_1)
	/** Make sure that:
	    - You have group members
	    - That are in the same room as you
	    - That they are fighting the same thing as you

	    If so, then check if the group is going to get in the way.
	-Mord
	**/
	if ( ch->master || ch->followers )
	{
		int fols = 0;
		k = ( ch->master ? ch->master : ch );
		if ( k != ch && HERE ( k, ch ) )
			fols += 1;
		for ( f = k->followers;f;f=f->next )
			if ( f->follower != ch && HERE ( f->follower, ch ) && FIGHTING ( f->follower ) == FIGHTING ( ch ) )
				fols += 1;
		if ( fols > 0 )
		{
			shortwep = is_short_wep ( GET_EQ ( ch, WEAR_WIELD ) );
			perc = valid_perc ( ch ) * 3;
			perc += ( IS_WEAPON ( num ) ? ( perc > 60  ? ( !shortwep ? 0 : 20 ) : ( !shortwep ? 20 : 0 ) ) : 0 );
			perc += ( IS_SPELL_ATK ( num ) ? 20 : 0 );
			perc += ( IS_SPELL_CAST ( num ) ? 25 : 0 );
			perc += ( IS_SKILL ( num ) ? 20 : 0 );


			if ( number ( 0, 40 ) > perc )
			{
				act ( "Your group members get in the way of your attack!", FALSE, ch, 0, 0, TO_CHAR );
				return 0;
			}
		}
	}
#endif


	/* no auto assist if in same group */
	if ( ! ( ( !ch->master && vict->master == ch ) || ( ch->master && ch->master == vict->master ) || ( !vict->master && ch->master == vict ) ) )
	{

		if ( !IS_NPC ( ch ) )
		{
			k = ( ch->master ? ch->master : ch );  /* Find the master */
			/* Not an NPC, Not Yourself, Not Fighting, Above standing */
			if ( k != ch && GET_POS ( k ) == POS_STANDING )
			{
				if ( ( ( AFF_FLAGGED ( k, AFF_CHARM ) ) || PRF_FLAGGED ( k, PRF_AUTOASSIST ) ) && HERE ( k, vict ) && !FIGHTING ( k ) )   /* If he is flagged to assist */
				{
					start_fighting_delay ( k, vict );
				}
			}

			for ( f = k->followers; f; f = f->next )
			{
				if ( ( f->follower == ch )
				        || ( GET_POS ( f->follower ) != POS_STANDING ) )
					continue; /* Skip if any of these are true */
				if ( FIGHTING ( f->follower ) )
					continue;
				if ( RIDDEN_BY ( f->follower ) )
				        continue;
				if ( ( ( AFF_FLAGGED ( f->follower, AFF_CHARM ) ) || PRF_FLAGGED ( f->follower, PRF_AUTOASSIST ) ) && HERE ( f->follower,vict ) )
				  {
				    start_fighting_delay ( f->follower, vict );
				  }
			}
		}
	}

	if ( IS_WEAPON ( num ) || IS_SPELL_ATK ( num ) || num == TYPE_UNDEFINED )
	{

		switch ( type )
		{
			case FE_TYPE_MELEE:
				return ( fe_melee_hit ( ch, vict, num, TRUE ) );
				break;
			case FE_TYPE_SKILL:
				return ( fe_melee_hit ( ch, vict, num, FALSE ) );
				break;
			case FE_TYPE_SPELL:
				if ( AFF_FLAGGED ( vict, AFF_MAGIC_BUBBLE ) )
				{
					act ( "Your magic washes over $N without touching $M.", FALSE, ch, 0, vict, TO_CHAR );
					act ( "$n's magic washes over you without touching you.", FALSE, ch, 0, vict, TO_VICT );
					act ( "$n's magic washes over $N without touching $M.", FALSE, ch, 0, vict, TO_NOTVICT );
					return ( fe_melee_hit ( ch, vict, TYPE_HIT, TRUE ) );
				}
				if ( AFF_FLAGGED ( ch, AFF_MAGIC_BUBBLE ) )
				{
					ch->Send ( "Your magic can't penetrate your own magic bubble.\r\n" );
					act ( "$n's magic doesn't penetrate $s own magic bubble!", FALSE, ch, 0 , 0, TO_ROOM );
					return ( fe_melee_hit ( ch, vict, TYPE_HIT, TRUE ) );
				}

				return ( fe_spell_hit ( ch, vict, num ) );
				break;
			default:
				return ( fe_melee_hit ( ch, vict, num, TRUE ) );
				/* undead/animal*/
				break;
		}
	}
	else
	{
		return fe_special_hit ( ch, vict, num );
	}

	return 0;
}


int melee_type_dam ( Character *ch, Character *vict, int attack_chance, int weps, int second )
{
	static int randomizer = 0;
	int dam;
	/* at least have between 0 and 3 damage */
	dam = randomizer % 3;
	randomizer++;
	if ( randomizer > 1000 )
		randomizer = 2;

	if ( !IS_NPC ( ch ) )
	{
		dam = str_app[STRENGTH_APPLY_INDEX ( ch ) ].todam;
		if ( second ) //hitting with secondary weapon
		{
			dam += dice ( size_dice_wep ( ch, WEAPON_SECO_AFF ), num_dice_wep ( ch, WEAPON_SECO_AFF ) );
			if ( !number ( 0, 100 ) )
				improve_skill ( ch, SKILL_DUAL );
		}
		else  //hitting with primary weapon
			dam +=  dice ( size_dice_wep ( ch, WEAPON_PRIM_AFF ), num_dice_wep ( ch, WEAPON_PRIM_AFF ) );
	}
	else
		dam += dice ( ch->mob_specials.damnodice,ch->mob_specials.damsizedice );

	if ( is_fused ( ch ) )
		dam += fused_dambonus ( ch );
	else dam += fighter_damroll ( ch );

	dam = FTOI ( dam * atk_chance_multi ( attack_chance ) );

	if ( weps == SINGLE_WEP )
		dam += dam / 10;

	dam = FTOI ( dam * pos_multi ( GET_POS ( vict ) ) );

	return dam;
}

int followers_assisting ( Character *ch )
{
	Character *k;
	struct follow_type *f;
	int cnt = 0;
	k = ( ch->master ? ch->master : ch );

	if ( k != ch )
	{
		if ( HERE ( k, ch ) && ( FIGHTING ( k ) != NULL ) )
			if ( FIGHTING ( ch ) == FIGHTING ( k ) )
				cnt++;
	}

	for ( f = k->followers;f != NULL;f = f->next )
	{
		if ( f->follower != ch )
		{
			if ( HERE ( f->follower, ch ) && ( FIGHTING ( f->follower ) != NULL ) )
				if ( FIGHTING ( ch ) == FIGHTING ( f->follower ) )
					cnt++;
		}
	}
	return cnt;
}

int fe_melee_hit ( Character* ch, Character* vict,
                   int type, int melee )
{
	struct obj_data* wielded = NULL;
	int w_type = type;
	int attack_chance;
	int dam = 0;
	int damage_ret = -1;
	bool wield_2 = 0;
	float wep_multi = 1.0f;
	int weps = has_weapon ( ch );

	if ( type == TYPE_UNDEFINED )
	{

		if ( weps == SINGLE_WEP )
			wielded = GET_EQ ( ch, WEAR_WIELD );
		else if ( weps == DUAL_WEP )
			wielded = ( ( wield_2 = number ( 0, 1 ) ) ? GET_EQ ( ch, WEAR_WIELD ) : GET_EQ ( ch, WEAR_WIELD_2 ) );

		/* Find the wielded weapon's type */
		if ( !IS_NPC ( ch ) )
		{ 
			if ( weps )
			{
				w_type = GET_OBJ_VAL ( wielded, 3 ) + TYPE_HIT;
				if ( total_chance ( ch, SKILL_LONGARM ) > 0 && !is_short_wep ( wielded ) )
					wep_multi = ( 1.0f + ( LONG_WEP_MULTI * ( ( float ) total_chance ( ch, SKILL_LONGARM ) ) ) /100.0 );
				else if ( total_chance ( ch, SKILL_SHORT_BLADE ) > 0 && is_short_wep ( wielded ) )
					wep_multi = ( 1.0f + ( SHORT_WEP_MULTI_ROGUE * ( ( float ) total_chance ( ch, SKILL_SHORT_BLADE ) ) ) /100.0 );
				else
					wep_multi = 1.0f;
			}
			else
				w_type = TYPE_HIT;

		} 
		else
		{
			if ( IS_NPC ( ch ) && ( ch->mob_specials.attack_type != 0 ) )
				w_type = ch->mob_specials.attack_type + TYPE_HIT;
			else
				w_type = TYPE_HIT;
		}
		if ( !IS_WEAPON ( w_type ) && !IS_SKILL ( w_type ) )
			w_type = TYPE_HIT;
	}

	GET_ATTACK_POS ( ch ) =  find_body_part ( ch, w_type );
	if ( IS_NPC ( vict ) && ( vict->followers || vict->master ) )
	{
		if ( followers_assisting ( vict ) == 0 )
			attack_chance = 3;
		else
			attack_chance = attack_roll ( ch, vict, w_type );
	}
	else
		attack_chance = attack_roll ( ch, vict, w_type );

	if ( evade_hit_check ( ch, vict, w_type ) ||attack_chance == 0 )
		damage_ret = fe_after_damage ( ch, vict, 0, w_type );
	else /*yep, landing a hit*/
	{
		dam = melee_type_dam ( ch, vict, attack_chance, weps, wield_2 );
		/* Depending on weapon type change the damage */
		dam = FTOI ( dam * wep_multi );
		damage_ret = fe_deal_damage ( ch, vict, dam, w_type );
	}

	if ( damage_ret != -1 )
		hitprcnt_mtrigger ( vict );

	return damage_ret;

}

int fe_special_hit ( Character* ch, Character* vict, int type )
{
	int dam = 0, attack_chance = 3, damage_ret;
	GET_ATTACK_POS ( ch ) = TYPE_UNDEFINED;

	if ( evade_hit_check ( ch, vict, type ) )
	{
		if ( type == SKILL_BASH )
		{
			GET_POS ( ch ) = POS_SITTING;
			update_pos ( ch );
		}
		damage_ret = fe_after_damage ( ch, vict, 0, type );
	}
	else
	{
		if ( IS_SKILL ( type ) )
		{
			dam = melee_type_dam ( ch, vict, attack_chance, has_weapon ( ch ), SINGLE_WEP );
		}
		else if ( IS_SPELL_CAST ( type ) )
		{
			dam += dice ( spell_size_dice ( ch ), spell_num_dice ( ch ) );

			if ( IS_NPC ( ch ) )
				dam += dice ( ch->mob_specials.damnodice, ch->mob_specials.damsizedice );

			if ( is_fused ( ch ) )
				dam += fused_dambonus ( ch );
			else dam += caster_damroll ( ch );

			if ( has_staff_multi ( ch, type ) > 0 )
				dam = FTOI ( dam * has_staff_multi ( ch, type ) );

			dam = FTOI ( dam * pos_multi ( GET_POS ( vict ) ) );

			dam = FTOI ( dam * atk_chance_multi ( attack_chance ) );
		}

		dam = FTOI ( dam * ( GET_SKILLMULTI ( ch ) = skill_type_multi ( ch, vict, type ) ) );

		// Make sure skills/spells actually do damage so that the right messages will be shown
		if ( dam < 10 ) dam = 10;

		damage_ret = fe_solo_damage ( ch, vict, dam, type ); //change this in future?
	}
	if ( damage_ret != -1 )
		hitprcnt_mtrigger ( vict );
	if ( !number ( 0, 20 ) )
		improve_skill ( vict, type );

	return damage_ret;
}

int fe_spell_hit ( Character* ch, Character* vict, int type )
{
	int attack_chance;
	int dam = 0;
	int damage_ret = 0;

	/* for now lets make type random */
	if ( type == TYPE_UNDEFINED )
		type = number ( TYPE_ATK_ORB, TYPE_ATK_TORPEDO );

	attack_chance = attack_roll ( ch, vict, type );

	GET_ATTACK_POS ( ch ) = TYPE_UNDEFINED;

	if ( evade_hit_check ( ch, vict, type ) || attack_chance == 0 )
		damage_ret = fe_after_damage ( ch, vict, 0, type );
	else
	{
		dam += dice ( spell_size_dice ( ch ), spell_num_dice ( ch ) );

		if ( IS_NPC ( ch ) )
			dam += dice ( ch->mob_specials.damnodice, ch->mob_specials.damsizedice );

		if ( is_fused ( ch ) )
			dam += fused_dambonus ( ch );
		else dam += caster_damroll ( ch );

		dam = FTOI ( dam * has_staff_multi ( ch, type ) );

		dam = FTOI ( dam * pos_multi ( GET_POS ( vict ) ) );

		dam = FTOI ( dam * atk_chance_multi ( attack_chance ) );

		damage_ret = fe_deal_damage ( ch, vict, dam, type ); //change this in future?
	}

	if ( damage_ret != -1 )
		hitprcnt_mtrigger ( vict );
	if ( !number ( 0, 20 ) )
		improve_skill ( vict, type );

	return damage_ret;
}

float atk_chance_multi ( int acm )
{
	switch ( acm )
	{
		case 1:
			return 0.25f;
		case 2:
			return 0.5f;
		case 3:
			return 1.0f;
	}
	return 0.0;
}


/* Handles ch hitting vict for x amount of damage with w_type.
   This function has no thrills or anything...calculate how much
   damage we really should be taking off, taking into account
   SANCT, SHIELD, etc., and then just take it off.  We call
   fe_after_damage() to handle everything, this function just
   takes those affects into account before the calling.
*/

int fe_solo_damage ( Character* ch, Character* vict,
                     int damage, int w_type )
{
	if ( ch == NULL || vict == NULL )
		return ( -1 );

	if ( damage == 0 ) // no damage:: lets not mess with more equations
		return fe_after_damage ( ch, vict, damage, w_type );

	if ( ( IS_WEAPON ( w_type ) || IS_SPELL_ATK ( w_type ) || IS_SPELL_CAST ( w_type ) ) && shield_check ( ch, vict, SHIELD_BLOCK, w_type, damage ) )
		return fe_after_damage ( ch, vict, 0, w_type );


	damage = modify_dam ( damage, ch, vict, w_type );
#if defined(EXP_GAIN_SYSTEM_1)
	if ( IS_NPC ( vict ) )
		damage = IRANGE ( 0, damage, MAX_MOB_DAM );
	else
	{
		if ( IS_NPC ( ch ) )
			damage = IRANGE ( 0, damage, MAX_MOB_DAM );
		else
			damage = IRANGE ( 0, damage, MAX_PLAYER_DAM );
	}
#endif
	return fe_after_damage ( ch, vict, damage, w_type );
}
#if defined(EXP_GAIN_SYSTEM_1)
Character *find_next_target ( Character *ch )
{
	Character *k = ( ch->master != NULL ? ch->master : ch );
	float total_perc = 0, cur_perc = 0;
	float rr;
	struct follow_type* f = NULL;
	if ( GET_PERC ( k ) != 0 && HERE ( k, ch ) && FIGHTING ( ch ) && FIGHTING ( ch ) == FIGHTING ( k ) )
		total_perc += GET_PERC ( k );
	/*check the followers*/
	for ( f = k->followers; f; f = f->next )
	{
		if ( !HERE ( f->follower, ch ) )
			continue;
		if ( !IS_NPC ( f->follower ) && ( PLR_FLAGGED ( f->follower, PLR_DYING ) || GET_LEVEL ( f->follower ) >= LVL_IMMORT ) )
			continue;
		if ( GET_PERC ( f->follower ) ==0 )
			continue;
		if ( FIGHTING ( f->follower ) ==FIGHTING ( ch ) )
		{
			total_perc += GET_PERC ( f->follower );
		}
	}
	rr = number ( 1, ( int ) ( ( total_perc+0.5 ) *10 ) ) /10;

	if ( GET_PERC ( k ) != 0 && HERE ( k, ch ) && FIGHTING ( ch ) && FIGHTING ( ch ) == FIGHTING ( k ) )
	{
		if ( rr > cur_perc && rr <= ( cur_perc + GET_PERC ( k ) ) )
			return k;
		else
			cur_perc += GET_PERC ( k );
	}

	for ( f = k->followers; f; f = f->next )
	{
		if ( !HERE ( f->follower, ch ) )
			continue;
		if ( !IS_NPC ( f->follower ) && ( PLR_FLAGGED ( f->follower, PLR_DYING ) || GET_LEVEL ( f->follower ) >= LVL_IMMORT ) )
			continue;
		if ( GET_PERC ( f->follower ) ==0 )
			continue;
		if ( FIGHTING ( f->follower ) !=FIGHTING ( ch ) )
			continue;
		if ( rr > cur_perc && rr <= ( cur_perc + GET_PERC ( f->follower ) ) )
			return f->follower;
		else
			cur_perc += GET_PERC ( f->follower );
	}

	/* somehow didn't find anything right */
	return ch;
}
#endif
#if defined(EXP_GAIN_SYSTEM_1)
/* When we're hitting a group of actual players, this gets
   called so that we break up the damage between them.
   ch is hitting vict and all the people in his group that
   are fighting ch for a total of damage amount with weapon
   type w_type.  After we get the percentage of the damage
   that should be hitting each player, we just call
   fe_solo_damage....no thrills here.
 */
int fe_group_damage ( Character* ch, Character* vict,
                      int damage, int w_type )
{
	struct follow_type* f = NULL;
	Character* master = NULL;


	float total_perc  = 0;
	float temp_damage = 0;
	int   to_ret      = 0;
	int i;
	/* this is probably a bad way to do it - FIXME */
	int hit_list[30];
	int count = 0;
	Character *list;

	/* also make sure that noone that is attacked and has 0 involvement but isnt with the group is counted.
	   if you have 0 involvement and are alone you should get full attacks.
	   */

	/* initilise the list */
	for ( i = 0; i < 30 ; i++ )
		hit_list[i] = -1;

	/*find the head of the group you are hitting */
	master = ( ( vict->master != NULL ) ? vict->master : vict );

	/*check if the master is a valid target - and add the value of  */
	if ( FIGHTING ( master ) == ch )
		if ( HERE ( master, ch ) )
			if ( GET_LEVEL ( master ) < LVL_IMMORT && !IS_NPC ( master ) && !PLR_FLAGGED ( master, PLR_DYING ) )
			{
				total_perc += GET_PERC ( master );
				hit_list[count++] = GET_ID ( master );
			}

	/*check the followers*/
	for ( f = master->followers; f; f = f->next )
	{
		if ( !HERE ( f->follower, ch ) )
			continue;
		if ( !IS_NPC ( f->follower ) && ( PLR_FLAGGED ( f->follower, PLR_DYING ) || GET_LEVEL ( f->follower ) >= LVL_IMMORT ) )
			continue;
		if ( GET_PERC ( f->follower ) <=5 )
			continue;
		if ( count >= 30 )
			break;
		if ( FIGHTING ( f->follower ) ==ch )
		{
			total_perc += GET_PERC ( f->follower );
			hit_list[count++] = GET_ID ( f->follower );
		}
	}

	if ( total_perc == 0 )
		to_ret = fe_solo_damage ( ch, vict, damage, w_type );

	if ( to_ret == -1 )
		return to_ret;

	/* so now we have our list of targets, and their percentage total.
	lets run through that list, find them and deal the damage!
	*/
	for ( i = 0; i < count; i++ )
	{

		if ( ( list = find_char ( hit_list[i] ) ) == NULL )
			continue;

		temp_damage = ( ( ( 100*GET_PERC ( list ) ) /total_perc ) * ( ( float ) damage ) ) /100;

		temp_damage = IRANGE ( 1, temp_damage, MAX_MOB_DAM );

		if ( list == vict )
		{
			to_ret = fe_solo_damage ( ch, list, ( int ) temp_damage, w_type );
			if ( to_ret == -1 )
				return to_ret;
		}
		else if ( fe_solo_damage ( ch, list, ( int ) temp_damage, w_type ) == -1 )
			return -1;

	}

	return to_ret;
}

#endif
int valid_perc ( Character *ch )
{

	struct follow_type* f = NULL;
	Character *vict = FIGHTING ( ch );
	Character *master = ( ch ? ( ch->master ? ch->master : ch ) : NULL );
	room_rnum rm = IN_ROOM ( ch );
	float total_perc = 0.0;

	/* also make sure that noone that is attacked and has 0 involvement but isn't with the group is counted.
	   if you have 0 involvement and are alone you should get full attacks.
	   */

	if ( !ch || GET_POS ( ch ) == POS_DEAD || DEAD ( ch ) )
		return 0;

	if ( vict == NULL || ( !ch->master && master->followers == NULL ) )
		return FTOI ( ( GET_PERC ( ch ) = 100 ) );

	/*check if the master is a valid target - and add the value of  */
	if ( SELF ( master,ch ) || ( ( FIGHTING ( master ) == vict ) && ( IN_ROOM ( master ) == rm ) ) )
		total_perc += GET_PERC ( master );


	/*check the followers*/
	for ( f = master->followers; f; f = f->next )
	{

		if ( IN_ROOM ( f->follower ) !=rm )
			continue;

		/*if (!AFF_FLAGGED(f->follower, AFF_GROUP))
		    continue;*/

		if ( FIGHTING ( f->follower ) ==vict )
			total_perc += GET_PERC ( f->follower );

	}

	if ( total_perc == 0 )
		return 0;

	return FTOI ( ( ( GET_PERC ( ch ) *100 ) /total_perc ) );
}

/* Gets called from fe_melee_hit and handles ch hitting vict
   for dam amount with w_type.  Really, the function just
   offloads the work to fe_solo_damage or fe_group_damage,
   dependant upon wether vict is a mob or not.
 */
int fe_deal_damage ( Character* ch, Character* vict,
                     int dam, int w_type )
{
	Character *master = NULL;
	if ( !vict )
	{
		return -1;
	}
	if ( HERE ( ch, vict ) && !FIGHTING ( ch ) )
	{
		start_fighting_delay ( ch, vict );
		return -1;
	}
	master = vict->master;


	if ( ( !master && vict->followers == NULL ) ||
	        SELF ( ch,vict )  ||
	        HERE ( ch, vict ) ||
	        ( RIDDEN_BY ( ch ) && ( HERE ( ch, RIDDEN_BY ( ch ) ) ) ) )   /* solo artest or normal damage -- no master whack em! */
	{
#if defined(EXP_GAIN_SYSTEM_1)
		if ( RIDDEN_BY ( vict ) && HERE ( RIDDEN_BY ( vict ), ch ) )
		{
			FIGHTING ( ch ) = RIDDEN_BY ( vict );
			//stop_fighting(ch);
			//start_fighting_delay(ch, RIDDEN_BY(vict));
			///** return 0 so stop fighting isn't called again **/
			//return 0;
		}
		else
#endif
			return fe_solo_damage ( ch, vict, dam, w_type );

	}
	else if ( master )
	{
#if defined(EXP_GAIN_SYSTEM_1)
		if ( HERE ( master, vict ) )   /* victim isnt the master! -- find the master and whack em! */
		{
			//stop_fighting(ch);
			//start_fighting_delay(ch, master);
			/** return 0 so stop fighting isn't called again **/
			//return 0;
			FIGHTING ( ch ) = master;
		}
		else /* victim is not in the same room as their master -- whack em only */
#endif
			return fe_solo_damage ( ch, vict, dam, w_type );
	}
	/*victim is the master of the group -- so whack the group */
	//return fe_group_damage(ch, vict, dam, w_type);

	return fe_solo_damage ( ch, vict, dam, w_type );

}


void halt_fighting ( Character *ch )
{

	Character* people, *p_next;

	/*stop everyone in this room fighting them */
	stop_fighting ( ch );

	for ( people = character_list;people; people = p_next )
	{
		p_next = people->next;
		if ( people == ch )
			continue;
		if ( FIGHTING ( people ) == ch )
			stop_fighting ( people );
	}
}
/*
Now, ring positions have been added to the list of positions
that can support vampiric drainage of mana, move or hitpoints..

{cuPosition         Max Drainage{c0
Focus            - 6%
Wield Primary    - 4%
Wield Secondary  - 4%
Shield           - 5%
Ring Left        - 3%
Ring Right       - 3%

The Subskill Drain Blood is a 15% hp drainage bonus.

So for every 100 damage you do to someone with these items
you get back that amount of Ma/Mv/Hp depending on the type
of item.


*/
int steal_affects ( Character *ch, int dam, int w_type, Character *vict )
{
	int ret_val = 0;
	float hp = 0, ma = 0, mv = 0;
	//const char * to_vict, *to_ch;

	struct obj_data *shield = GET_EQ ( ch, WEAR_SHIELD );

	if ( ! ( ( GET_ALIGNMENT ( ch ) > 350 && GET_ALIGNMENT ( vict ) < -350 ) || ( GET_ALIGNMENT ( ch ) < -350 && GET_ALIGNMENT ( vict ) > 350 ) ) )
		return ret_val;

	if ( has_staff_multi ( ch, w_type ) )
	{
		struct obj_data *staff = GET_EQ ( ch, WEAR_FOCUS );
		if ( staff )
		{
			if ( OBJ_FLAGGED ( staff, ITEM_LIFESTEAL ) )
				hp += 2;
			if ( OBJ_FLAGGED ( staff, ITEM_MANASTEAL ) )
				ma += 2;
			if ( OBJ_FLAGGED ( staff, ITEM_MOVESTEAL ) )
				mv += 2;

		}
	}
	else if ( IS_WEAPON ( w_type ) )
	{
		struct obj_data* wielded = GET_EQ ( ch, WEAR_WIELD );
		struct obj_data* wielded_2 = GET_EQ ( ch, WEAR_WIELD_2 );

		if ( wielded && GET_OBJ_TYPE ( wielded ) == ITEM_WEAPON )
		{
			if ( OBJ_FLAGGED ( wielded, ITEM_LIFESTEAL ) )
				hp += 1;
			if ( OBJ_FLAGGED ( wielded, ITEM_MANASTEAL ) )
				ma += 2;
			if ( OBJ_FLAGGED ( wielded, ITEM_MOVESTEAL ) )
				mv += 2;
		}

		if ( wielded_2 && GET_OBJ_TYPE ( wielded_2 ) == ITEM_WEAPON )
		{
			if ( OBJ_FLAGGED ( wielded_2, ITEM_LIFESTEAL ) )
				hp += 1;
			if ( OBJ_FLAGGED ( wielded_2, ITEM_MANASTEAL ) )
				ma += 2;
			if ( OBJ_FLAGGED ( wielded_2, ITEM_MOVESTEAL ) )
				mv += 2;
		}
	}

	if ( shield && GET_OBJ_TYPE ( shield ) == ITEM_ARMOR )
	{
		if ( OBJ_FLAGGED ( shield, ITEM_LIFESTEAL ) )
			hp += 1;
		if ( OBJ_FLAGGED ( shield, ITEM_MANASTEAL ) )
			ma += 3;
		if ( OBJ_FLAGGED ( shield, ITEM_MOVESTEAL ) )
			mv += 3;
	}

	if ( GET_EQ ( ch, WEAR_FINGER_L ) && GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_FINGER_L ) ) == ITEM_ARMOR )
	{
		if ( OBJ_FLAGGED ( GET_EQ ( ch, WEAR_FINGER_L ), ITEM_LIFESTEAL ) )
			hp += 1;
		if ( OBJ_FLAGGED ( GET_EQ ( ch, WEAR_FINGER_L ), ITEM_MANASTEAL ) )
			ma += 3;
		if ( OBJ_FLAGGED ( GET_EQ ( ch, WEAR_FINGER_L ), ITEM_MOVESTEAL ) )
			mv += 2;
	}
	if ( GET_EQ ( ch, WEAR_FINGER_R ) && GET_OBJ_TYPE ( GET_EQ ( ch, WEAR_FINGER_R ) ) == ITEM_ARMOR )
	{
		if ( OBJ_FLAGGED ( GET_EQ ( ch, WEAR_FINGER_R ), ITEM_LIFESTEAL ) )
			hp += 1;
		if ( OBJ_FLAGGED ( GET_EQ ( ch, WEAR_FINGER_R ), ITEM_MANASTEAL ) )
			ma += 2;
		if ( OBJ_FLAGGED ( GET_EQ ( ch, WEAR_FINGER_R ), ITEM_MOVESTEAL ) )
			mv += 1;
	}

	if ( IS_NPC ( ch ) && ( MOB_SUBSKILL ( ch ) == ( SUB_DRAIN_BLOOD ) || ( ( GET_CLASS ( ch ) == CLASS_UNDEAD ) && GET_LEVEL ( ch ) > number ( 50, 300 ) ) ) )
		hp +=10;
	if ( !IS_NPC ( ch ) )
		hp += GET_SUB ( ch, SUB_DRAIN_BLOOD ) * 0.04;




	if ( GET_MANA ( ch ) < GET_MAX_MANA ( ch ) && ( ma= ( ( dam*ma ) /100.0 ) ) > 0.0 )
	{
		alter_mana ( ch, FTOI ( -ma ) );
		alter_mana ( vict, FTOI ( ma ) );
	}

	if ( GET_MOVE ( ch ) < GET_MAX_MOVE ( ch ) && ( mv= ( ( dam*mv ) /100.0 ) ) > 0.0 )
	{
		alter_move ( ch, FTOI ( -mv ) );
		alter_move ( vict, FTOI ( mv ) );
	}
	if ( GET_HIT ( ch ) < GET_MAX_HIT ( ch ) && ( hp= ( ( dam*hp ) /100.0 ) ) > 0.0 )
	{
		/* TODO: add random message's for this */
		act ( "$n steals your energy.", FALSE, ch, 0, vict, TO_VICT );
		act ( "You steal $N's energy.", FALSE, ch, 0, vict, TO_CHAR );
		damage ( vict, ch, FTOI ( -hp ), TYPE_UNDEFINED );
		ret_val = damage ( ch, vict, int ( hp ), TYPE_UNDEFINED );
	}


	return ret_val;

}

void reduce_quality ( Character *ch, Character *vict, int damage, int w_type, obj_data *shield )
{
	obj_data *weapon, *focus, *eq;
	vector< vector<int> > loc;
	int area;

	/* damage the weapon/focus of the attacker */
	if ( IS_WEAPON ( w_type ) && ( weapon = GET_EQ ( ch, WEAR_WIELD ) ) != NULL && GET_OBJ_QUALITY ( weapon ) > 0 )
	{
		GET_OBJ_QUALITY ( weapon ) -= damage / 50000.0 * ( 1 + GET_OBJ_REPAIRS ( weapon ) / 5.0 );
		if ( GET_OBJ_QUALITY ( weapon ) < 0 )
			GET_OBJ_QUALITY ( weapon ) = 0;
		update_affects ( weapon );
	}
	else if ( ( IS_SPELL_ATK ( w_type ) || IS_SPELL_CAST ( w_type ) ) && ( focus = GET_EQ ( ch, WEAR_FOCUS ) ) != NULL && GET_OBJ_QUALITY ( focus ) > 0 )
	{
		GET_OBJ_QUALITY ( focus ) -= damage / 50000.0 * ( 1 + GET_OBJ_REPAIRS ( focus ) / 5.0 );
		if ( GET_OBJ_QUALITY ( focus ) < 0 )
			GET_OBJ_QUALITY ( focus ) = 0;
		update_affects ( focus );
	}

	/* damage vict's shield if it blocked */
	if ( shield )
	{
		if ( GET_OBJ_QUALITY ( shield ) > 0 )
		{
			GET_OBJ_QUALITY ( shield ) -= damage / 25000.0 * ( 1 + GET_OBJ_REPAIRS ( shield ) / 5.0 );
			if ( GET_OBJ_QUALITY ( shield ) < 0 )
				GET_OBJ_QUALITY ( shield ) = 0;
			update_affects ( shield );
		}
		return;
	}

	/* damage all eq of vict in the area */
	if ( IS_WEAPON ( w_type ) )
		area = GET_ATTACK_POS ( ch );
	else area = number ( PART_HEAD, PART_RIGHT_LEG );

	loc = { { WEAR_HEAD, WEAR_HORNS, WEAR_ANTENNA },
			{ WEAR_FACE, WEAR_EYES, WEAR_EAR_L, WEAR_EAR_R },
			{ WEAR_NECK_1, WEAR_NECK_2 },
			{ WEAR_ARMS, WEAR_HANDS, WEAR_WRIST_L, WEAR_FINGER_L, WEAR_THUMB_L, WEAR_LIGHT },
			{ WEAR_SHOULDER_L },
			{ WEAR_ARMS, WEAR_HANDS, WEAR_WRIST_R, WEAR_FINGER_R, WEAR_THUMB_R, WEAR_HOLD },
			{ WEAR_SHOULDER_R },
			{ WEAR_ABOUT, WEAR_CHEST, WEAR_BODY },
			{ WEAR_WAIST, WEAR_HIPS },
			{ WEAR_LEGS, WEAR_FEET, WEAR_ANKLE_L, WEAR_THIGH_L, WEAR_KNEE_L },
			{ WEAR_LEGS, WEAR_FEET_2, WEAR_ANKLE_R, WEAR_THIGH_R, WEAR_KNEE_R } };

	for ( int part : loc[ area ] )
	{
		eq = GET_EQ ( vict, part );
		if ( eq && GET_OBJ_QUALITY ( eq ) > 0 )
		{
			GET_OBJ_QUALITY ( eq ) -= damage / 10000.0 * ( 1 + GET_OBJ_REPAIRS ( eq ) / 5.0 );
			if ( GET_OBJ_QUALITY ( eq ) < 0 )
				GET_OBJ_QUALITY ( eq ) = 0;
			update_affects ( eq );
		}
	}
}

/* Right now this function is still really really dirty, and needs
   to be refactored some more.  None-the-less, it will always perform
   the same tasks, it just won't be as large (hopefully).  Basically
   it handles outputting the messages to the screen, deducting the
   actual hp, exp gains for PCs, poisoning the vict if the weapon is
   poisoned, saving link dead people getting killed, and handling
   when vict dies from the damage.

   The function has been cleaned up alot, looking much better.
*/

int fe_after_damage ( Character* ch, Character* vict,
                      int dam, int w_type )
{
	gold_int local_gold = 0, bonus_gold = 0;
	char local_buf[100] = "";
	int partial = 0;
	//int dam_exp = 0;
	//Character *victnext;
	//int sweeping;

	//victnext = NULL;
	//sweeping = FALSE;

	if ( IS_NPC ( vict ) && ( vict->master || vict->followers ) )
		if ( followers_assisting ( vict ) == 0 )
			dam = 100 + ( dam*2 );

	if ( !can_fight ( ch, vict, FALSE ) )
	{
		//stop_fighting(ch);
		return -1;
	}

	if ( GET_POS ( vict ) <= POS_DEAD  || DEAD ( vict ) )
	{
		log ( "SYSERR: Attempt to damage corpse '%s' in room #%d by '%s'.",
		      GET_NAME ( vict ), GET_ROOM_VNUM ( IN_ROOM ( vict ) ), GET_NAME ( ch ) );
		die ( vict, ch );
		return ( -1 );
	}

	if ( vict->master && ( vict->master == ch || vict->master == ch->master ) )
		stop_follower ( vict );

	if ( dam )
	{
#if defined(EXP_GAIN_SYSTEM_1)
		if ( RIDING ( vict ) && HERE ( RIDING ( vict ), vict ) && RIDING ( vict ) != ch )
		{
			partial = ( ( ( dam/2 ) * ( 250 - total_chance ( vict, SKILL_MOUNTED_COMBAT ) ) ) /250 );
			damage ( ch, RIDING ( vict ), partial, w_type );
			partial *= 2;
			// return -1;
		}
		else
#endif
			partial = dam;

		if ( !IS_NPC ( ch ) && GET_SUB ( ch, SUB_SWEEP_ATTACK ) > 0 && partial > 0 )
		{
			if ( get_sub_status ( ch, SUB_SWEEP_ATTACK ) == STATUS_ON )
			{
				if ( GET_SWEEP_DAM ( ch ) )
				{
					//partial *= 0.6;
					partial = FTOI ( partial * ( GET_SUB ( ch, SUB_SWEEP_ATTACK ) * 0.01 ) );
					partial += GET_SWEEP_DAM ( ch );
				}
				if ( GET_HIT ( vict ) < partial )
					GET_SWEEP_DAM ( ch ) = GET_HIT ( vict ) - partial;
				else
					GET_SWEEP_DAM ( ch ) = 0;
			}
		}
		else
			GET_SWEEP_DAM ( ch ) = 0;

/* Horus - disabling this ridiculous subskill */
/*		if ( partial > 0 && ( GET_HIT ( vict ) - partial ) <= 0 && GET_SUB ( vict, SUB_UNDYING ) > number ( 0, 600 ) )
		{
			act ( "{cGYou concentrate your energy on the on coming killing blow,\r\nand before it lands you move out of range of combat.{c0", FALSE ,vict, 0,0,TO_CHAR );
			return -1;

		} */

		if ( w_type == MOB_BACKSTAB && IS_NPC ( ch ) && !IS_NPC( vict ) )
			partial = MIN ( partial, (int) ( 0.75 * GET_MAX_HIT ( vict ) ) );

		reduce_quality ( ch, vict, partial, w_type, NULL );

		alter_hit ( vict, partial );
		if ( w_type == SPELL_LIFESUCK )
			alter_hit ( ch, -partial * 0.05 );

		update_pos ( vict );
		if ( !ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_ARENA ) )
		{

#if defined(EXP_GAIN_SYSTEM_1)
			if ( RIDING ( ch ) && HERE ( ch, RIDING ( ch ) ) )
			{
				int dam_exp = partial;
				if ( IS_NPC ( vict ) && !IS_NPC ( RIDING ( ch ) ) && partial > 2 )
				{
					dam_exp /= 4;
					damage_count ( vict, IS_NPC ( RIDING ( ch ) ) ? -1 : GET_ID ( RIDING ( ch ) ), dam_exp );
					improve_skill ( ch, SKILL_MOUNTED_COMBAT );
					damage_count ( vict, IS_NPC ( ch ) ? -1 : GET_ID ( ch ), dam_exp * 3 );
				}
				else
					damage_count ( vict, IS_NPC ( ch ) ? -1 : GET_ID ( ch ), dam_exp );

			}
			else
			{
				if ( IS_NPC ( vict ) && partial > 0 )
					damage_count ( vict, IS_NPC ( ch ) ? -1 : GET_ID ( ch ), partial );
			}
#else

			if ( IS_NPC ( vict ) && partial > 0 )
				damage_count ( vict, GET_ID ( ch ), partial );
#endif
		}

		if ( !SELF ( ch, vict ) )
		{
		        if ( !IS_NPC ( vict ) )
                                GET_LAST_DAM_T ( vict ) = partial;
                        if ( !IS_NPC ( ch ) )
                                GET_LAST_DAM_D ( ch )   = partial;

			if ( partial > 5 && steal_affects ( ch, partial, w_type, vict ) == -1 )
				return -1;
		}
	}
	/*
	  * skill_message sends a message from the messages file in lib/misc.
	  * dam_message just sends a generic "You hit $n extremely hard.".
	  * skill_message is preferable to dam_message because it is more
	  * descriptive.
	  *
	  * If we are _not_ attacking with a weapon (i.e. a spell), always use
	  * skill_message. If we are attacking with a weapon: If this is a miss or a
	  * death blow, send a skill_message if one exists; if not, default to a
	  * dam_message. Otherwise, always send a dam_message.
	  */

	if ( w_type != TYPE_UNDEFINED && w_type != MOB_BACKSTAB )
	{
		if ( IS_SPELL_CAST ( w_type ) || IS_SKILL ( w_type ) || IS_OTHERDAM ( w_type ) )
			skill_message ( partial, ch, vict, w_type );
		else
			dam_message ( partial, ch, vict, w_type );
	}
	if ( !SELF ( ch, vict ) )
	{

		if ( IS_NPC ( vict ) )
		{
			if ( CAN_HUNT ( vict ) )
			{
				add_hunter ( vict );
				HUNTING ( vict ) = ch;
			}
			remember ( vict, ch );
		}
		if ( partial )
		{


			/* learn */
			if ( !IS_NPC ( ch ) )
			{
				switch ( has_weapon ( ch ) )
				{
					case 2:
						if ( !is_short_wep ( GET_EQ ( ch, WEAR_WIELD_2 ) ) )
						{
							if ( number ( 0, 10 ) < 2 )
								improve_skill ( ch, SKILL_LONGARM );
						}
						else
						{
							if ( number ( 0, 10 ) < 2 )
								improve_skill ( ch, SKILL_SHORT_BLADE );
						}
						improve_skill ( ch, SKILL_DUAL );
					case 1:
						if ( !is_short_wep ( GET_EQ ( ch, WEAR_WIELD ) ) )
						{
							if ( number ( 0, 10 ) < 2 )
								improve_skill ( ch, SKILL_LONGARM );
						}
						else
						{
							if ( number ( 0, 10 ) < 2 )
								improve_skill ( ch, SKILL_SHORT_BLADE );
						}

						break;
				}


				if ( number ( 1,100 ) > GET_PERM_ACCURACY ( ch ) && number ( 1, 1000 ) < 5 )
				{
					GET_PERM_ACCURACY ( ch ) ++;
					ch->Send ( "Your natural accuracy rating just increased to %d!\r\n", GET_PERM_ACCURACY ( ch ) );
				}
			}

			if ( !IS_NPC ( vict ) )
			{
				if ( number ( 1,100 ) > GET_PERM_EVASION ( vict ) && number ( 1, 1000 ) < 5 )
				{
					GET_PERM_EVASION ( vict ) ++;
					vict->Send ( "Your natural evasion rating just increased to %d!\r\n", GET_PERM_EVASION ( vict ) );
				}
			}


			global_dam = partial;
			if ( shield_check ( ch, vict, SHIELD_REFLECT, w_type ) == -1 )
			{
				//stop_fighting(vict);
				return -1;
			}
			poison_wep_check ( ch, vict, w_type, partial );
		}

	}

	update_pos ( vict );
	vict->send_char_pos ( dam );

	if ( !SELF ( ch, vict ) )
	{
		if ( IS_NPC ( vict ) )
		{
			if ( ( GET_HIT ( vict ) << 2 ) < GET_MAX_HIT ( vict ) && !AFF_FLAGGED ( vict, AFF_CHARM ) &&
			        MOB_FLAGGED ( vict, MOB_WIMPY ) && ! ( AFF_FLAGGED ( vict, AFF_HOLD ) && ( number ( 1,4 ) != 3 ) ) )
				do_flee ( vict, NULL, 0, 0 );

		}
		else if ( GET_WIMP_LEV ( vict ) && GET_HIT ( vict ) < GET_WIMP_LEV ( vict ) && GET_HIT ( vict ) > 0 )
			do_flee ( vict, NULL, 0, 0 );
	}

	if ( !IS_NPC ( vict ) && ! ( vict->desc ) && GET_POS ( vict ) > POS_STUNNED )
	{
		do_flee ( vict, NULL, 0, 0 );
		act ( "$n is rescued by divine forces.", FALSE, vict, 0, 0, TO_ROOM );
		GET_WAS_IN ( vict ) = IN_ROOM ( vict );
		move_char_to ( vict, world_vnum[0] );

		return -1;
	}

	if ( GET_POS ( vict ) == POS_DEAD )
	{
		halt_fighting ( vict );

		if ( !IS_NPC ( vict ) )
		{
			new_mudlog ( BRF, LVL_GOD, TRUE, "%s killed by %s at %s [%d]", GET_NAME ( vict ),
			             GET_NAME ( ch ), IN_ROOM ( vict )->name, GET_ROOM_VNUM ( IN_ROOM ( vict ) ) );

			if ( GET_MRACE ( ch ) == 0 )
				brag ( ch, vict );

			//            if (MOB_FLAGGED(ch, MOB_MEMORY))
			forget ( ch, vict );
		}
		else
		{
#if 1
			if ( IS_NPC ( vict ) && HERE ( vict, ch ) && GET_SUB ( ch, SUB_PILLAGE ) > number ( 1, 101 ) )
			{
				//mob_rnum mrn = real_mobile(GET_MOB_VNUM(vict));
				if ( MobProtoExists ( GET_MOB_VNUM ( vict ) ) )
					bonus_gold = ( GET_GOLD ( GetMobProto ( GET_MOB_VNUM ( vict ) ) ) * 0.25 )/number(1, 10);

				ch->Gold ( bonus_gold, GOLD_HAND );
				if ( !number ( 0, 200 ) )
					improve_sub ( ch, SUB_PILLAGE, 1 );
			}
#endif
			local_gold = vict->Gold ( 0, GOLD_HAND );
			snprintf ( local_buf, sizeof ( local_buf ), "%lld", local_gold );
			if ( !IS_NPC ( ch ) && vict->mob_specials.bragged_about.size() > 0 && find ( vict->mob_specials.bragged_about.begin(), vict->mob_specials.bragged_about.end(), GET_ID ( ch ) ) != vict->mob_specials.bragged_about.end() )
				brag ( ch, vict );
		}

		kill_points ( ch, vict );
		die ( vict, ch );

		if ( GET_FIGHT_EVENT ( vict ) )
		{
			event_cancel ( GET_FIGHT_EVENT ( vict ) );
			GET_FIGHT_EVENT ( vict ) = NULL;
		}


		if ( !IS_NPC ( ch ) )
		{
			gold_int gld = ch->Gold ( 0, GOLD_HAND );

			if ( PRF_FLAGGED ( ch, PRF_AUTOGOLD ) && local_gold > 0 )
				do_get ( ch, ( char * ) "coin corpse", 0, 0 );

			if ( bonus_gold )
				ch->Send ( "{cbYou pillage the corpse and find an extra %lld coins!{c0\r\n",  bonus_gold );

			if ( PRF_FLAGGED ( ch, PRF_AUTOLOOT ) )
				do_get ( ch, ( char * ) "all corpse", 0, 0 );

			if ( PRF_FLAGGED ( ch, PRF_AUTOSAC ) )
				do_sac ( ch, ( char * ) "corpse", 0, 0 );

			if ( IS_AFFECTED ( ch, AFF_GROUP ) && ( ch->master || ch->followers ) && local_gold > 0 &&
			        PRF_FLAGGED ( ch, PRF_AUTOSPLIT ) &&
			        ( PRF_FLAGGED ( ch, PRF_AUTOLOOT ) || PRF_FLAGGED ( ch, PRF_AUTOGOLD ) ) &&
			        gld < ch->Gold ( 0, GOLD_HAND ) )
				do_split ( ch, local_buf, 0, 0 );
		}

		return -1;
	}
#if defined(EXP_GAIN_SYSTEM_1)
	/** switch targets in groups **/
	if ( vict->master || vict->followers )
		FIGHTING ( ch ) = find_next_target ( vict );
#endif

	if ( !FIGHTING ( vict ) && !SELF ( ch, vict ) && HERE ( ch, vict ) )
		start_fighting_delay ( vict, ch );

	if ( !FIGHTING ( ch ) && !SELF ( ch, vict ) && HERE ( ch, vict ) )
		start_fighting_delay ( ch, vict );

	return dam;
}


void poison_wep_check ( Character *ch, Character *vict, int w_type, int dam )
{

	if ( MOB_FLAGGED ( vict, MOB_NOPOISON ) )
		return;
	/* thieves and rogue mobs should get a good chance at poisoning
	   everyone else, sure! But not quite as good - mord*/
	if ( IS_NPC ( ch ) )
	{
		if ( (GET_CLASS ( ch ) == CLASS_THIEF) ||( GET_CLASS(ch) == CLASS_ROGUE))
		{
			if ( number ( 0, 200 ) > 5 )
				return; //Failed to poison.
		}

		else


		{
			if ( number ( 0, 300 ) > 5 )
				return; //Failed to poison.
		}
	}
	else
	{
	
		{
			if ( number ( 0, 10 ) > 5 )
				return; //Failed to poison.
		}
	}

	/* do the poison shuffle */

	struct obj_data* wielded = GET_EQ ( ch, WEAR_WIELD );
	if ( wielded && IS_WEAPON ( w_type ) && dam > 0 )
	{
		if ( IS_SET_AR ( GET_OBJ_EXTRA ( wielded ), ITEM_POISONED_1 ) )
		{
			struct affected_type af;

			af.location = APPLY_STR;
			af.expire = -2;
			af.modifier = -2;
			af.bitvector = AFF_POISON_1;
			af.type = SPELL_POISON;

			act ( "You feel very sick.", FALSE, vict, 0, 0, TO_VICT );
			act ( "A light white substance from $N's weapon burns into $n!", FALSE, vict, 0, ch, TO_ROOM );

			affect_join ( vict, &af, TRUE, FALSE, FALSE, FALSE );
		}
		if ( IS_SET_AR ( GET_OBJ_EXTRA ( wielded ), ITEM_POISONED_2 ) )
		{
			struct affected_type af;

			af.location = APPLY_STR;
			af.expire = -2;
			af.modifier = -4;
			af.bitvector = AFF_POISON_2;
			af.type = SPELL_POISON_2;

			act ( "You feel very sick.", FALSE, vict, 0, 0, TO_VICT );
			act ( "A green gas cloud from $N's weapon burns into $n!", FALSE, vict, 0, ch, TO_ROOM );

			affect_join ( vict, &af, TRUE, FALSE, FALSE, FALSE );
		}
		if ( IS_SET_AR ( GET_OBJ_EXTRA ( wielded ), ITEM_POISONED_3 ) )
		{
			struct affected_type af;

			af.location = APPLY_STR;
			af.expire = -2;
			af.modifier = -6;
			af.bitvector = AFF_POISON_3;
			af.type = SPELL_POISON_3;

			act ( "You feel very sick.", FALSE, vict, 0, 0, TO_VICT );
			act ( "A fine black powder from $N's weapon burns into $n!", FALSE, vict, 0, ch, TO_ROOM );

			affect_join ( vict, &af, TRUE, FALSE, FALSE, FALSE );
		}
		if ( IS_SET_AR ( GET_OBJ_EXTRA ( wielded ), ITEM_POISONED_4 ) )
		{
			struct affected_type af;

			af.location = APPLY_STR;
			af.expire = -2;
			af.modifier = -8;
			af.bitvector = AFF_POISON_4;
			af.type = SPELL_POISON_4;

			act ( "You feel your heart stopping.", FALSE, vict, 0, 0, TO_VICT );
			act ( "A sticky yellow goo from $N's weapon sears into $n!", FALSE, vict, 0, ch, TO_ROOM );

			affect_join ( vict, &af, FALSE, FALSE, FALSE, FALSE );
		}
	}
}



int shield_check ( Character *ch, Character *vict, int type, int w_type, int dam )
{
	int success = FALSE;
	struct affected_type af;
	int armor_rating = 0; //highest armor rating is 50
	int lev = GET_LEVEL ( vict );
	int ded = ( GET_POS ( vict ) <= POS_STUNNED );
	int sht = is_short_wep ( GET_EQ ( ch, WEAR_WIELD ) );
	global_dam = ( global_dam/5 );
	if ( !HERE ( ch,vict ) )
		return 0;

	/**
	If the person Ch is hitting has Ice Shield, while the Ch has a shoet weapon, and hasn;t got prot from cold.
	There is a 1 in 6 chance that Ch will be affected by the freeze affect of the shield.
	- Mord**/
	if ( HERE ( ch, vict ) && is_short_wep ( GET_EQ ( ch, WEAR_WIELD ) ) && AFF_FLAGGED ( vict, AFF_SHIELD_ICE ) && !affected_by_spell ( ch, SPELL_PROT_COLD ) )
	{
		if ( !number ( 0, 5 ) && !AFF_FLAGGED ( ch, AFF_FROZEN ) )
		{
			//SET_BIT_AR(AFF_FLAGS(vict), AFF_FROZEN);
			act ( "You freeze as you touch $N's shield of ice.", FALSE, ch, 0 , vict, TO_CHAR );
			act ( "$n is frozen as $e touches $N's shield of ice.", FALSE, ch, 0, vict, TO_NOTVICT );
			act ( "$n is frozen as $e touches your shield of ice.", FALSE, ch, 0, vict, TO_VICT );
			af.location = APPLY_SPEED;
			af.expire = HOURS_TO_EXPIRE ( 1 );
			af.modifier = -2 * ( GET_LEVEL ( vict ) +1 );
			af.bitvector = AFF_FROZEN;
			af.type = SPELL_DG_AFFECT;
			affect_to_char ( ch, &af );
			if ( knows_spell ( ch, SPELL_SHIELD_ICE ) )
				improve_skill ( ch, SPELL_SHIELD_ICE );
			improve_skill ( vict, SPELL_SHIELD_ICE );
		}
	}

	switch ( type )
	{
		case SHIELD_BLOCK:
			armor_rating = MIN ( 40, ( AFF_FLAGGED ( vict, AFF_SHIELD_STATIC ) ? 2*apply_ac ( vict, WEAR_SHIELD ) : apply_ac ( vict, WEAR_SHIELD ) ) );

			success = ( armor_rating ? ( number ( 0, 100 ) < armor_rating ) : 0 );
			if ( success )
			{
				if ( IS_SPELL_ATK ( w_type ) || IS_SPELL_CAST ( w_type ) )
				{
					act ( "$N deflects your magic with $S shield!", FALSE, ch, 0, vict, TO_CHAR );
					act ( "You deflect $n's magic with your shield!", FALSE, ch, 0, vict, TO_VICT );
				}
				else
				{
					act ( "$N blocks your attack with $S shield!", FALSE, ch, 0, vict, TO_CHAR );
					act ( "You block $n's attack with your shield!", FALSE, ch, 0, vict, TO_VICT );
				}
				if ( knows_spell ( ch, SPELL_SHIELD_STATIC ) )
					improve_skill ( ch, SPELL_SHIELD_STATIC );
				improve_skill ( vict, SPELL_SHIELD_STATIC );
				reduce_quality ( NULL, NULL, dam, 0, GET_EQ ( vict, WEAR_SHIELD ) );
			}
			break;
		case SHIELD_REFLECT:
			if ( AFF_FLAGGED ( vict, AFF_FIRE_SHIELD ) && ( GET_EQ ( ch, WEAR_WIELD ) && !sht )  && ( number ( 1, 81 ) < lev || ded ) )
			{
				act ( "$N scorches you with $S fire shield.", FALSE, ch, 0, vict, TO_CHAR );
				act ( "You scorch $n with your fire shield.", FALSE, ch, 0, vict, TO_VICT );
				if ( knows_spell ( ch, SPELL_FIRE_SHIELD ) )
					improve_skill ( ch, SPELL_FIRE_SHIELD );
				improve_skill ( vict, SPELL_FIRE_SHIELD );
				return damage ( vict, ch, global_dam, TYPE_UNDEFINED );
			}
			else if ( AFF_FLAGGED ( vict, AFF_SHIELD_THORNS ) && ( GET_EQ ( ch, WEAR_WIELD ) && sht )  && ( number ( 1, 81 ) < lev || ded ) )
			{
				act ( "You are shredded by $N's whirling barrier of thorns!", FALSE, ch, 0, vict, TO_CHAR );
				act ( "You shred $n with your whirling barrier of thorns!", FALSE, ch, 0, vict, TO_VICT );
				if ( knows_spell ( ch, SPELL_SHIELD_THORN ) )
					improve_skill ( ch, SPELL_SHIELD_THORN );
				improve_skill ( vict, SPELL_SHIELD_THORN );
				return damage ( vict, ch, global_dam, TYPE_UNDEFINED );
			}
			else if ( AFF_FLAGGED ( vict, AFF_SHIELD_MIRROR ) && ( IS_SPELL_ATK ( w_type ) || IS_SPELL_CAST ( w_type ) ) && ( number ( 1, 81 ) < lev || ded ) )
			{
				act ( "$N's mirror shield reflects your magic back at you!", FALSE, ch, 0, vict, TO_CHAR );
				act ( "You bounce $n's magic right back at $m!", FALSE, ch, 0, vict, TO_VICT );
				if ( knows_spell ( ch, SPELL_SHIELD_MIRROR ) )
					improve_skill ( ch, SPELL_SHIELD_MIRROR );
				improve_skill ( vict, SPELL_SHIELD_MIRROR );
				return damage ( vict, ch, global_dam, TYPE_UNDEFINED );
			}

			break;
		case SHIELD_EVADE:
			if ( AFF_FLAGGED ( vict, AFF_SHIELD_HOLY ) && ( GET_EQ ( ch, WEAR_WIELD ) && !sht ) && ( number ( 1, 101 ) < lev || ded ) )
			{
				act ( "A golden light moves $N out of your reach!", FALSE, ch, 0, vict, TO_CHAR );
				act ( "A golden light moves you out of $n's reach!", FALSE, ch, 0, vict, TO_VICT );
				success = TRUE;
				if ( knows_spell ( ch, SPELL_SHIELD_HOLY ) )
					improve_skill ( ch, SPELL_SHIELD_HOLY );
				improve_skill ( vict, SPELL_SHIELD_HOLY );
			}

			break;
		default:
			break;
	}
	return success;
}



/* make this have multiple checks
seperate into its own function
add other evacive skills and spell checks here.
dodge, evade, phase, etc etc
*/
int evade_hit_check ( Character *ch, Character *vict, int w_type )
{
	int skill_cost ( int h, int m, int v, Character *ch );
	int parrychance = 0;
	int v_type = w_type;
	if ( ( !ch || !vict ) || GET_POS ( vict ) < POS_FIGHTING )
		return 0;


	if ( IS_WEAPON ( w_type ) && has_weapon ( ch ) && ( ( parrychance = has_weapon ( vict ) ) != 0 ) &&
	        ( number ( 1, 30 ) < GET_DEX ( vict ) ) &&
	        ( number ( 1, 300 ) < ( total_chance ( vict, SKILL_PARRY ) * parrychance ) ) )
	{
		if ( !IS_WEAPON ( v_type ) )
		{
			log ( "COMBAT: player %s, attacked with a weapon type of %d (%s)", GET_NAME ( vict ), v_type, skill_name ( v_type ) );
			return 0;
		}
		if ( ( v_type ) >= TYPE_HIT )
			v_type -= TYPE_HIT;

		ch->Send ( "%s parries your attack with a swift %s.\r\n",
		           PERS ( vict,ch ), attack_hit_text[v_type].singular );
		vict->Send ( "You parry %s's attack with a swift %s.\r\n",
		             PERS ( ch, vict ), attack_hit_text[v_type].singular );
		improve_skill ( ch, SKILL_PARRY );
		improve_skill ( vict, SKILL_PARRY );
		return 1;
	}
	if ( IS_WEAPON ( w_type ) && number ( 1, 30 ) < GET_DEX ( vict ) && AFF_FLAGGED ( vict, AFF_DODGE )  && number ( 1, 200 ) < total_chance ( vict, SKILL_DODGE ) )
	{
		if ( skill_cost ( 0, 2, 20, vict ) )
		{
			ch->Send ( "%s dodges your attack.\r\n", PERS ( vict,ch ) );
			vict->Send ( "You dodge %s's attack.\r\n", PERS ( ch, vict ) );
			improve_skill ( ch, SKILL_DODGE );
			improve_skill ( vict, SKILL_DODGE );
			return 1;
		}
		else
			send_to_char ( "You try and dodge but just can't find the energy!!\r\n", ch );

	}

	if ( ( IS_SPELL_ATK ( w_type ) || IS_SPELL_CAST ( w_type ) ) && affected_by_spell ( vict, SPELL_DETECT_MAGIC ) && number ( 0, 100 ) < ( GET_INT ( vict ) /5 ) )
	{
		act ( "$N senses your oncoming magical attack and narrowly avoids it.", TRUE, ch, 0, vict, TO_CHAR );
		act ( "You sense $n's oncoming magical attack and narrowly avoid it.", TRUE, ch, 0, vict, TO_VICT );
		return 1;
	}

	if ( AFF_FLAGGED ( vict, AFF_PHASE ) && number ( 1, 30 ) < GET_DEX ( vict ) && IS_WEAPON ( v_type ) && number ( 1, 300 ) < total_chance ( vict, SKILL_PHASE ) )
	{
		if ( skill_cost ( 0, 2, 20, vict ) )
		{

			act ( "$N phases past your attack and strikes you!", FALSE, ch, 0, vict, TO_CHAR );
			act ( "You phase past $n's attack and strike $m.\r\n", FALSE, ch, 0, vict, TO_VICT );
			improve_skill ( ch, SKILL_PHASE );
			improve_skill ( vict, SKILL_PHASE );
			fight_event_hit ( vict, ch, find_fe_type ( vict ), GET_NEXT_SKILL ( vict ) );
			return 1;
		}
		else
			send_to_char ( "You try and phase but just can't find the energy!!\r\n", ch );

	}

	if ( number ( 1, 30 ) < GET_DEX ( vict ) && IS_WEAPON ( v_type ) && AFF_FLAGGED ( vict, AFF_DRUNKEN_MASTER ) && number ( 1, 200 ) < ( 70-GET_LEVEL ( vict ) ) )
	{
		ch->Send ( "%s weaves drunkenly out of your reach!\r\n", PERS ( vict,ch ) );
		vict->Send ( "You weave drunkenly out of %s's reach!\r\n",  PERS ( ch, vict ) );
		improve_skill ( ch, SKILL_MARTIAL_ARTS );
		improve_skill ( vict, SKILL_MARTIAL_ARTS );
		return 1;
	}

	return shield_check ( ch, vict, SHIELD_EVADE, w_type );
}


void send_not_to_spam ( const char *buf, Character *ch,
                        Character *victim, struct obj_data *weap,
                        int spam )
{
	Character *people;

	if ( !ch )
		return;


	for ( people = IN_ROOM ( ch )->people; people;
	        people = people->next_in_room )
	{
		if ( ( spam ==
		        TRUE ) ? ( !PRF_FLAGGED ( people,
		                                  PRF_BATTLESPAM ) ) : ( PRF_FLAGGED ( people,
		                                                                       PRF_BATTLESPAM ) ) )
			continue;
		if ( victim && people == victim )
			continue;
		if ( people == ch )
			continue;
		perform_act ( buf, ch, weap, victim, people );
	}
}


/* The Fight related routines */

void Character::appear()
{
	if ( affected_by_spell ( this, SPELL_INVISIBLE ) )
		affect_from_char ( this, SPELL_INVISIBLE );

	REMOVE_BIT_AR ( AFF_FLAGS ( this ), AFF_INVISIBLE );
	REMOVE_BIT_AR ( AFF_FLAGS ( this ), AFF_HIDE );

	if ( GET_LEVEL ( this ) <= LVL_HERO )
		act ( "$n slowly fades into existence.", FALSE, this, 0, 0, TO_ROOM );
	else
		act ( "You feel a strange presence as $n appears, seemingly from nowhere.", FALSE, this, 0, 0, TO_ROOM );
}




/*min from -10 to 0*/
int class_min_strike ( Character *ch )
{
	if ( !IS_NPC ( ch ) )
	{
		switch ( GET_CLASS ( ch ) )
		{
			case CLASS_MAGE:
			case CLASS_PRIEST:
			case CLASS_ESPER:
				return ( -15 );
				break;
			case CLASS_WARRIOR:
			case CLASS_HUNTER:
				return ( -25 );
				break;
			case CLASS_RANGER:
			case CLASS_THIEF:
			case CLASS_GYPSY:
				return ( -30 );
				break;
		}
	}
	else
	{
		switch ( GET_CLASS ( ch ) )
		{
			case CLASS_NORMAL:
			case CLASS_CASTER:
			case CLASS_ANIMAL:
				return ( -15 );
				break;
			case CLASS_UNDEAD:
				return ( -25 );
				break;
			case CLASS_ROGUE:
			case CLASS_FIGHTER:
				return ( -30 );
				break;
		}
	}
	return -10;

}

/*max from 0 to 10 */
int class_max_strike ( Character *ch )
{
	if ( !IS_NPC ( ch ) )
	{
		switch ( GET_CLASS ( ch ) )
		{
			case CLASS_MAGE:
			case CLASS_PRIEST:
			case CLASS_ESPER:
				return ( 18 );
				break;
			case CLASS_WARRIOR:
			case CLASS_HUNTER:
				return ( 5 );
				break;
			case CLASS_RANGER:
			case CLASS_THIEF:
			case CLASS_GYPSY:
				return ( 8 );
				break;
			default:
				return 0;
		}
	}
	else
	{
		switch ( GET_CLASS ( ch ) )
		{
			case CLASS_NORMAL:
			case CLASS_CASTER:
			case CLASS_ANIMAL:
				return ( 20 );
				break;
			case CLASS_UNDEAD:
				return ( 5 );
				break;
			case CLASS_ROGUE:
			case CLASS_FIGHTER:
				return ( 8 );
				break;

		}
	}
	return 5;
}

void free_messages ( void )
{
	for ( int i = 0; i < MAX_MESSAGES; i++ )
		while ( fight_messages[i].msg )
		{
			struct message_type *former = fight_messages[i].msg;
			fight_messages[i].msg = fight_messages[i].msg->next;
			delete former;
		}
}
string fread_fight_action ( std::ifstream &fl, int nr, string &chk )
{
	if ( !fl || fl.eof() )
	{
		log ( "SYSERR: fread_fight_action: unexpected EOF near action #%d", nr );
		exit ( 1 );
	}

	getline ( fl, chk );
	if ( chk.length() == 0 )
		return string();

	chk = Trim ( chk );
	if ( chk[0] == '#' )
		return string();
	else
		return chk;
}

void skip_to_msg ( std::ifstream &fl, string & chk )
{
	char buf[READ_SIZE];
	if ( chk == "M" )
		return;

	do
	{
		if ( fl.eof() )
			break;
		fl.getline ( buf, READ_SIZE );
		chk = Trim ( string ( buf ) );
	}
	while ( !fl.eof() && ( chk.length() == 0 || chk != "M" ) );
}

void load_messages ( void )
{
	int i;
	struct message_type *messages;
	string chk;
	stringstream ss;
	char buf[128];
	std::ifstream fl ( ( char * ) MESS_FILE );

	if ( !fl || !fl.is_open() )
	{
		log ( "SYSERR: Error reading combat message file %s: %s", MESS_FILE, strerror ( errno ) );
		exit ( 1 );
	}
	for ( i = 0; i < MAX_MESSAGES; i++ )
	{
		fight_messages[i].a_type = 0;
		fight_messages[i].number_of_attacks = 0;
		fight_messages[i].msg = NULL;
	}

	skip_to_msg ( fl, chk );

	while ( !fl.eof() && chk.length() > 0 &&  chk == "M" )
	{
		int type = -1;
		std::getline ( fl, chk ); /* read the line into a string*/

		if ( fl.eof() || chk.length() == 0 )
			break;

		strlcpy ( buf, Trim ( chk ).c_str(), 128 );
		type = atol ( buf );

		if ( type == -1 )
		{
			log ( "SYSERR: Error reading combat message file (type: %d ) %s: %s", type, MESS_FILE, strerror ( errno ) );
			exit ( 1 );
		}

		for ( i = 0; ( i < MAX_MESSAGES ) && ( fight_messages[i].a_type != type )
		        && ( fight_messages[i].a_type ); i++ )
			;

		if ( i >= MAX_MESSAGES )
		{
			log ( "SYSERR: Too many combat messages.  Increase MAX_MESSAGES and recompile." );
			exit ( 1 );
		}

		messages = new struct message_type();
		fight_messages[i].number_of_attacks++;
		fight_messages[i].a_type = type;
		messages->next = fight_messages[i].msg;
		fight_messages[i].msg = messages;

		messages->die_msg.attacker_msg = fread_fight_action ( fl, i, chk );
		messages->die_msg.victim_msg = fread_fight_action ( fl, i, chk );
		messages->die_msg.room_msg = fread_fight_action ( fl, i, chk );
		messages->miss_msg.attacker_msg = fread_fight_action ( fl, i, chk );
		messages->miss_msg.victim_msg = fread_fight_action ( fl, i, chk );
		messages->miss_msg.room_msg = fread_fight_action ( fl, i, chk );
		messages->hit_msg.attacker_msg = fread_fight_action ( fl, i, chk );
		messages->hit_msg.victim_msg = fread_fight_action ( fl, i, chk );
		messages->hit_msg.room_msg = fread_fight_action ( fl, i, chk );
		messages->god_msg.attacker_msg = fread_fight_action ( fl, i, chk );
		messages->god_msg.victim_msg = fread_fight_action ( fl, i, chk );
		messages->god_msg.room_msg = fread_fight_action ( fl, i, chk );
		skip_to_msg ( fl, chk );

	}
	fl.close();
}

/* given a body part num, a name for it is produced */

const char * body_part_name ( int part )
{
	/*
	PART_HEAD,
	PART_FACE,
	PART_THROAT,
	PART_LEFT_ARM,
	PART_LEFT_SHOULDER,
	PART_RIGHT_ARM,
	PART_RIGHT_SHOULDER,
	PART_TORSO,
	PART_ABDOMEN,
	PART_LEFT_LEG,
	PART_RIGHT_LEG
	 
	*/

	static const char * armor_parts[] =
	{
		"head",
		"face",
		"throat",
		"left arm",
		"left shoulder",
		"right arm",
		"right shoulder",
		"torso",
		"abdomen",
		"left leg",
		"right leg",
		"body"
	};

	if ( part > PART_MAX || part < PART_HEAD )
		return armor_parts[PART_MAX];
	else
		return armor_parts[part];
}

int weapon_type_mod ( int w_type, int area )
{
	int amount = 0;
	switch ( w_type )
	{
		case TYPE_HIT:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 5 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 10 );
					break;
				case PART_CENTER:
					amount = ( 3 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 5 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 5 );
					break;
			}
			break;
		case TYPE_STING:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 2 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 10 );
					break;
				case PART_CENTER:
					amount = ( 10 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 2 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 2 );
					break;
			}
			break;
		case TYPE_WHIP:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 20 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 10 );
					break;
				case PART_CENTER:
					amount = ( 20 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 10 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 10 );
					break;
			}
			break;
		case TYPE_SLASH:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 5 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 10 );
					break;
				case PART_CENTER:
					amount = ( 5 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 10 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 10 );
					break;
			}
			break;
		case TYPE_BITE:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 10 );
					break;
				case PART_TOP_LEFT:
					amount = ( 5 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 5 );
					break;
				case PART_CENTER:
					amount = ( 10 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 5 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 5 );
					break;
			}
			break;
		case TYPE_BLUDGEON:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 20 );
					break;
				case PART_TOP_LEFT:
					amount = ( 20 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 20 );
					break;
				case PART_CENTER:
					amount = ( 5 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 0 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 0 );
					break;
			}
			break;
		case TYPE_CRUSH:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 20 );
					break;
				case PART_TOP_LEFT:
					amount = ( 20 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 20 );
					break;
				case PART_CENTER:
					amount = ( 5 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 0 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 0 );
					break;
			}
			break;
		case TYPE_POUND:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 20 );
					break;
				case PART_TOP_LEFT:
					amount = ( 20 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 20 );
					break;
				case PART_CENTER:
					amount = ( 5 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 0 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 0 );
					break;
			}
			break;
		case TYPE_CLAW:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 5 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 10 );
					break;
				case PART_CENTER:
					amount = ( 10 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 5 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 5 );
					break;
			}
			break;
		case TYPE_MAUL:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 10 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 10 );
					break;
				case PART_CENTER:
					amount = ( 10 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 5 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 5 );
					break;
			}
			break;
		case TYPE_THRASH:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 10 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 10 );
					break;
				case PART_CENTER:
					amount = ( 10 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 10 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 10 );
					break;
			}
			break;
		case TYPE_PIERCE:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 5 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 20 );
					break;
				case PART_CENTER:
					amount = ( 10 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 5 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 5 );
					break;
			}
			break;
		case TYPE_BLAST:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 25 );
					break;
				case PART_TOP_LEFT:
					amount = ( 5 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 5 );
					break;
				case PART_CENTER:
					amount = ( 15 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 5 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 5 );
					break;
			}
			break;
		case TYPE_PUNCH:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 5 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 10 );
					break;
				case PART_CENTER:
					amount = ( 10 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 0 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 0 );
					break;
			}
			break;
		case TYPE_STAB:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 5 );
					break;
				case PART_TOP_LEFT:
					amount = ( 10 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 10 );
					break;
				case PART_CENTER:
					amount = ( 10 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 3 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 3 );
					break;
			}
			break;
		case TYPE_KICK:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 3 );
					break;
				case PART_TOP_LEFT:
					amount = ( 5 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 5 );
					break;
				case PART_CENTER:
					amount = ( 10 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 20 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 20 );
					break;
			}
			break;
		case TYPE_GORE:
			switch ( area )
			{
				case PART_TOP_CENTER:
					amount = ( 5 );
					break;
				case PART_TOP_LEFT:
					amount = ( 5 );
					break;
				case PART_TOP_RIGHT:
					amount = ( 5 );
					break;
				case PART_CENTER:
					amount = ( 20 );
					break;
				case PART_LOWER_LEFT:
					amount = ( 5 );
					break;
				case PART_LOWER_RIGHT:
					amount = ( 5 );
					break;
			}
			break;
	}
	return amount;
}

/* should be called only once per attack, returns a random part
   ch has no use yet, but may do in future.
*/
int find_body_part ( Character *ch, int w_type )
{
	int up_center = 10;
	int up_left = 10;
	int up_right = 10;
	int center = 10;
	int low_left = 10;
	int low_right = 10;
	int chance;
	int rand_area_part ( int area );
	int weapon_type_mod ( int w_type, int area );



	up_center = weapon_type_mod ( w_type, PART_TOP_CENTER );
	up_left   = weapon_type_mod ( w_type, PART_TOP_LEFT );
	up_right  = weapon_type_mod ( w_type, PART_TOP_RIGHT );
	center    = weapon_type_mod ( w_type, PART_CENTER );
	low_left  = weapon_type_mod ( w_type, PART_LOWER_LEFT );
	low_right = weapon_type_mod ( w_type, PART_LOWER_RIGHT );


	chance = ( number ( 0, ( up_center + up_left + up_right + center + low_left + low_right ) ) );


	if ( chance < up_center )
	{
		return rand_area_part ( PART_TOP_CENTER );
	}
	else if ( chance < ( up_center + up_left ) )
	{
		return rand_area_part ( PART_TOP_LEFT );
	}
	else if ( chance < ( up_center + up_left + up_right ) )
	{
		return rand_area_part ( PART_TOP_RIGHT );
	}
	else if ( chance < ( up_center + up_left + up_right + center ) )
	{
		return rand_area_part ( PART_CENTER );
	}
	else if ( chance < ( up_center + up_left + up_right + center + low_left ) )
	{
		return rand_area_part ( PART_LOWER_LEFT );
	}
	else if ( chance < ( up_center + up_left + up_right + center + low_left + low_right ) )
	{
		return rand_area_part ( PART_LOWER_RIGHT );
	}

	return rand_area_part ( PART_AREA_MAX );


}

/* returns the area that body part

*/
int find_part_area ( int part )
{
	/*
	PART_TOP_CENTER,
	PART_TOP_LEFT,
	PART_TOP_RIGHT,
	PART_CENTER,
	PART_LOWER_LEFT,
	PART_LOWER_RIGHT
	*/
	switch ( part )
	{
		case PART_HEAD:
		case PART_FACE:
		case PART_THROAT:
			return PART_TOP_CENTER;
			break;
		case PART_LEFT_ARM:
		case PART_LEFT_SHOULDER:
			return PART_TOP_LEFT;
			break;
		case PART_RIGHT_ARM:
		case PART_RIGHT_SHOULDER:
			return PART_TOP_RIGHT;
			break;
		case PART_TORSO:
		case PART_ABDOMEN:
			return PART_CENTER;
			break;
		case PART_LEFT_LEG:
			return PART_LOWER_LEFT;
			break;
		case PART_RIGHT_LEG:
			return PART_LOWER_RIGHT;
			break;
	}
	return PART_CENTER;
}

int rand_area_part ( int area )
{
	/*
	PART_TOP_CENTER,
	PART_TOP_LEFT,
	PART_TOP_RIGHT,
	PART_CENTER,
	PART_LOWER_LEFT,
	PART_LOWER_RIGHT
	*/
	switch ( area )
	{
		case PART_TOP_CENTER:
			return number ( PART_HEAD, PART_THROAT );
			break;
		case PART_TOP_LEFT:
			return number ( PART_LEFT_ARM, PART_LEFT_SHOULDER );
			break;
		case PART_TOP_RIGHT:
			return number ( PART_RIGHT_ARM, PART_RIGHT_SHOULDER );
			break;
		case PART_CENTER:
			return number ( PART_TORSO, PART_ABDOMEN );
			break;
		case PART_LOWER_LEFT:
			return PART_LEFT_LEG;
			break;
		case PART_LOWER_RIGHT:
			return PART_RIGHT_LEG;
			break;
		default:
			return number ( PART_HEAD, PART_RIGHT_LEG );
	}
}



/* returns a number between 0 and 100 as a percentage chance */
int chance_hit_part ( Character *ch, int part )
{
	int area = find_part_area ( part );
	int apply_ac ( Character *ch, int eq_pos );
	int pos;
	float t_c = 1;
	float t_l = 1;
	float t_r = 1;
	float c   = 1;
	float l_l = 1;
	float l_r = 1;
	float total = 0;
	int AC;
	if ( is_fused ( ch ) )
		AC = fused_AC ( ch );
	else AC = ch->compute_armor_class();
	float ac_tot = ( float ) ( 200 - ( AC + 100 ) ) *2;

	/* this can be sped up alot in future but for now
	   we will just go through this each time it gets called.
	*/
	for ( pos = 0; pos < NUM_WEARS; pos++ )
	{
		if ( GET_EQ ( ch, pos ) != NULL )
		{
			if ( IS_TOP_CENTER ( pos ) )
				t_c += apply_ac ( ch, pos );
			if ( IS_TOP_LEFT ( pos ) )
				t_l += apply_ac ( ch, pos );
			if ( IS_TOP_RIGHT ( pos ) )
				t_r += apply_ac ( ch, pos );
			if ( IS_CENTER ( pos ) )
				c   += apply_ac ( ch, pos );
			if ( IS_LOWER_LEFT ( pos ) )
				l_l += apply_ac ( ch, pos );
			if ( IS_LOWER_RIGHT ( pos ) )
				l_r += apply_ac ( ch, pos );
		}
	}

	total = ( t_c + t_l + t_r + c + l_l + l_r );

	switch ( area )
	{
		case PART_TOP_CENTER:
			return IRANGE ( 0, total != 6 ? FTOI ( ac_tot * ( t_c/total ) ) : 0, 100 );
			break;
		case PART_TOP_LEFT:
			return IRANGE ( 0, total != 6 ? FTOI ( ac_tot * ( t_l/total ) ) : 0, 100 );
			break;
		case PART_TOP_RIGHT:
			return IRANGE ( 0, total != 6 ? FTOI ( ac_tot * ( t_r/total ) ) : 0, 100 );
			break;
		case PART_CENTER:
			return IRANGE ( 0, total != 6 ? FTOI ( ac_tot * ( c  /total ) ) : 0, 100 );
			break;
		case PART_LOWER_LEFT:
			return IRANGE ( 0, total != 6 ? FTOI ( ac_tot * ( l_l/total ) ) : 0, 100 );
			break;
		case PART_LOWER_RIGHT:
			return IRANGE ( 0, total != 6 ? FTOI ( ac_tot * ( l_r/total ) ) : 0, 100 );
			break;
		default:
			return FTOI ( ac_tot );
	}
}

float area_damage_multi ( int area )
{

	switch ( area )
	{
		case PART_TOP_CENTER:
			return 1.25;
			break;
		case PART_TOP_LEFT:
			return 0.95;
			break;
		case PART_TOP_RIGHT:
			return 0.95;
			break;
		case PART_CENTER:
			return 1.15;
			break;
		case PART_LOWER_LEFT:
			return 0.90;
			break;
		case PART_LOWER_RIGHT:
			return 0.90;
			break;
	}
	return 1.0;
}

char *replace_string2 ( char *str, const char *str2 )
{
	char *cp = buff;


	for ( ; *str; str++ )
	{
		if ( *str == '#' )
		{
			switch ( * ( ++str ) )
			{
				case 'W':
					for ( ; *str2; * ( cp++ ) = * ( str2++ ) )
						;
					break;
				default:
					* ( cp++ ) = '#';
					break;
			}
		}
		else
			* ( cp++ ) = *str;

		*cp = 0;
	}                 /* For */
	return ( buff );
}

#define FE_TO_ROOM 0
#define FE_TO_VICT 1
#define FE_TO_CHAR 2


/* attacker is the person hitting victim
type is either a default message of melee, skill or magic, or a specific skill or spell value.
w_type for weapon type
fst is either to room, to vict or to char
*/
char *fight_type_message ( char *str, size_t len, Character *attacker, Character *victim, int type, int w_type, int fst )
{

	int ran = number ( 1, 1000 );

	int run_type = w_type;
	/*
	static char * skill_msg_default[] =
	{
	"deadly",
	"massive",
	"cunning",
	"flowing",
	"silent",
	"fearful"
	 
	} ;*/

	static const char * magic_msg_default[] =
	{
		"concentrated #W of energy",
		"multiple #W of energy",
		"#W of energy",
		"energy #W"

	} ;

	static const char * undead_msg_default[] =
	{
		"unholy shrieks",
		"clawing hands",
		"shadow breath",
		"goring bite",
		"beam of black energy",
		"petrifying glare"

	} ;

	static const char * animal_msg_default[] =
	{
		"snapping teeth",
		"frantic headbutt",
		"jugular bite",
		"tail whip"

	};

	static const char *mount_melee_melee[] =
	{
		"supercharged combo trample attack",
		"combined crushing weight",
		"sneaky double crossover"
	};
	static const char *mount_magic_melee[] =
	{
		"daze of flashing blades and fireworks",
		"trample of dark magic and death",
		"massive magic enhanced kick",
		"electric punch"
	};
	/*
	static char * melee_msg_default[] =
	{
	"calculated",
	"sure",
	"powerful"
	};*/



	/* if ya cast a spell or use a skill, hell... why not say so! */
	/*if (IS_SPELL(run_type) || IS_SKILL(run_type)) {
	sprintf(str, "%s", skill_name(run_type));
	return str;
	} */
	if ( RIDING ( attacker ) && HERE ( attacker, RIDING ( attacker ) ) && total_chance ( attacker, SKILL_MOUNTED_COMBAT ) )
	{
		switch ( find_fe_type ( RIDING ( attacker ) ) )
		{
			case FE_TYPE_MELEE:
			case FE_TYPE_SKILL:
			default:
				switch ( fst )
				{
					case FE_TO_CHAR:
						snprintf ( str, len, "$M with your %s",  mount_melee_melee[ran%3] );
						break;
					case FE_TO_ROOM:
						snprintf ( str, len, " with $s %s", mount_melee_melee[ran%3] );
						break;
					case FE_TO_VICT:
						snprintf ( str, len, " with $s %s", mount_melee_melee[ran%3] );
						break;
				}
				break;
			case FE_TYPE_SPELL:
				switch ( fst )
				{
					case FE_TO_CHAR:
						snprintf ( str, len, "$M with your %s", mount_magic_melee[ran%4] );
						break;
					case FE_TO_ROOM:
						snprintf ( str, len, " with $s %s", mount_magic_melee[ran%4] );
						break;
					case FE_TO_VICT:
						snprintf ( str, len, " with $s %s", mount_magic_melee[ran%4] );
						break;
				}
				break;
		}
		return str;
	}

	switch ( type )
	{
		case FE_TYPE_MELEE:
		case FE_TYPE_SKILL:
			if ( run_type >= TYPE_HIT )
				run_type -= TYPE_HIT;        /* Change to base of table with text */
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$S %s with your %s", body_part_name ( GET_ATTACK_POS ( attacker ) ), attack_hit_text[abs ( run_type ) ].singular );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, "'s %s with $s %s",  body_part_name ( GET_ATTACK_POS ( attacker ) ), attack_hit_text[abs ( run_type ) ].singular );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, "r %s with $s %s", body_part_name ( GET_ATTACK_POS ( attacker ) ),  attack_hit_text[abs ( run_type ) ].singular );
					break;
				default:
					snprintf ( str, len, "error" );
					break;
			}
			return str;
			break;
		case FE_TYPE_SPELL:

			if ( run_type >= TYPE_HIT )
				run_type -= TYPE_HIT;
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$M with your %s", magic_msg_default[ ( ran%4 ) ] );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, " with $s %s", magic_msg_default[ ( ran%4 ) ] );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, " with $s %s", magic_msg_default[ ( ran%4 ) ] );
					break;
			}
			strcpy ( str,  replace_string2 ( str, attack_hit_text[abs ( run_type ) ].plural ) ) ;
			return str;
			break;
		case FE_TYPE_UNDEAD:
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$M with your %s", undead_msg_default[ran%6] );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, " with $s %s", undead_msg_default[ran%6] );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, " with $s %s", undead_msg_default[ran%6] );
					break;
			}
			return str;
			break;
		case FE_TYPE_ANIMAL:
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$M with your %s", animal_msg_default[ran%4] );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, " with $s %s", animal_msg_default[ran%4] );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, " with $s %s", animal_msg_default[ran%4] );
					break;
			}

			return str;
			break;
			/*add affect cases below*/


		case AFF_MIND_ELEC:
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$M with your arcing bolts of electricity" );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, " with $s arcing bolts of electricity" );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, " with $s arcing bolts of electricity" );
					break;
			}

			return str;
			break;
		case AFF_MIND_FIRE:
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$M with your sizzling balls of white fire" );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, " with $s sizzling balls of white fire" );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, " with $s sizzling balls of white fire" );
					break;
			}

			return str;
			break;
		case AFF_MIND_WATER:
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$M with your whirling jets of water" );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, " with $s whirling jets of water" );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, " with $s whirling jets of water" );
					break;
			}

			return str;
			break;
		case AFF_MIND_ICE:
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$M with your flying razor sharp icicles" );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, " with $s flying razor sharp icicles" );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, " with $s flying razor sharp icicles" );
					break;
			}
			return str;
			break;
		case AFF_FURY_ATTACKS:
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$M with your furious attacks" );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, " with $s furious attacks" );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, " with $s furious attacks" );
					break;
			}

			return str;
			break;
		case AFF_DRAIN_BLOOD:
			switch ( fst )
			{
				case FE_TO_CHAR:
					snprintf ( str, len, "$M with your wickedly sharp fangs" );
					break;
				case FE_TO_ROOM:
					snprintf ( str, len, " with $s wickedly sharp fangs" );
					break;
				case FE_TO_VICT:
					snprintf ( str, len, " with $s wickedly sharp fangs" );
					break;
			}

			return str;
			break;
		default:
			break;
	}
	snprintf ( str, len, "$M with attacks" );
	return str;
}

int find_fe_type ( Character *ch )
{

	int chcl;
	if ( AFF_FLAGGED ( ch, AFF_MAGIC_BUBBLE ) )
		return FE_TYPE_MELEE;

	if ( !IS_NPC ( ch ) )
	{
		chcl = GET_CLASS ( ch );

		if ( AFF_FLAGGED ( ch, AFF_POLY_TOAD ) )
			return FE_TYPE_ANIMAL;
		if ( AFF_FLAGGED ( ch, AFF_POLY_WOLF ) )
			return FE_TYPE_ANIMAL;
		if ( AFF_FLAGGED ( ch, AFF_POLY_BOAR ) )
			return FE_TYPE_ANIMAL;
		if ( AFF_FLAGGED ( ch, AFF_POLY_BEAR ) )
			return FE_TYPE_ANIMAL;
		if ( AFF_FLAGGED ( ch, AFF_POLY_LION ) )
			return FE_TYPE_ANIMAL;


		if ( ( attack_type ( chcl ) == ATTACK_MAGIC && !has_weapon ( ch ) ) || ( has_staff (ch) ) ) 
			return FE_TYPE_SPELL;
		else if ( attack_type ( chcl ) == ATTACK_SKILL )
			return FE_TYPE_SKILL;
		else
			return FE_TYPE_MELEE;
	}
	else
	{
		switch ( GET_CLASS ( ch ) )
		{
			case CLASS_NORMAL:
				return FE_TYPE_MELEE;
			case CLASS_UNDEAD:
				return FE_TYPE_UNDEAD;
			case CLASS_CASTER:
				return FE_TYPE_SPELL;
			case CLASS_FIGHTER:
				return FE_TYPE_MELEE;
			case CLASS_ROGUE:
				return FE_TYPE_SKILL;
			case CLASS_ANIMAL:
				return FE_TYPE_ANIMAL;
				break;
			default:
				return FE_TYPE_MELEE;
				break;
		}
	}
}
/* //Okay so i found this list of adjectives, i feel so...
Aberrant, Abominable, Absurd, Abysmal, Acidic, Acrocephalic*, Adhesive, Adipose*, Airy, Alien,
 Ambiguous, Amoebic*, Amorphous, Anarchic, Ancient, Angular, Animalistic, Animated, Anomalous,
 Antediluvian, Anthropophagous*, Appalling, Appendaged, Aquatic*, Ashen, Askew, Astounding,
 Atrocious, Awry, Baboon-Like, Baleful, Baneful, Bankrupt, Barbarous, Batrachian*, Beastly, Bellowing,
 Bestial*, Bilious, Bizarre*, Blasphemous, Bleating, Bloated, Bloodshot, Blubbery, Boiling, Bombilating*,
 Brutish, Bug-Eyed, Bulbous, Cacaphonous*, Cackling, Cadaverous, Cancerous, Cellular, Changeable, Chaotic,
 Chattering, Chelonian*, Chitinous*, Chthonic*, Clawed*, Coarse, Colorless, Colossal, Confusing, Congealed,
 Conical, Convoluted, Corpse-Like, Corpulent, Corrupt, Creamy, Criminal, Croaking, Crustaceous*, Crystalline,
 Cunning*, Cyclopean*, Cylindrical, Dank, Dark, Dazzling, Deafening, Deathless, Debased, Debauched, Decomposing,
 Deformed, Degenerate, Degraded, Delirious, Demonic*, Depraved, Deranged, Detestable, Deviant, Devilish*,
 Diabolical, Diffuse, Dire, Discordant, Diseased, Disfigured, Disgusting, Dislocated, Disordered, Dissolved,
 Distorted, Dreadful, Dripping, Effervescent, Effulgent*, Effusive, Elastic, Elephantine*, Elongated*,
 Emaciated*, Endless, Enlarged, Enormous, Enveloping, Evasive, Exaggerated, Excruciating, Extended,
 Fabulous, Faceless, Fantastic, Fearful, Fecund, Festering, Fetid, Fibrous, Fiendish, Fiery, Filthy, Fish-Like,
 Flabby, Flailing*, Flatulent*, Flowing, Fluctuating, Fluid, Foaming, Foul, Fractured, Fragrant, Frantic, Fungous,
 Furfuraceous*, Furious, Gangrenous, Gargantuan, Gaunt*, Ghastly, Ghoulish*, Gibbering, Gigantic, Globular,
 Glutinous, Gluttonous*, Gnashing, Gory, Grasping, Grayish, Greenish, Grim, Grisly, Gross, Grotesque*, Gushing,
 Hairy, Hallucinatory, Hapless, Hateful, Hazy, Heaving, Hellish, Herpetiform*, Hideous, Hircine*, Hissing, Horned,
 Horrible, Horrific*, Howling, Huge, Hump-Backed*, Hybrid, Ichorous, Idiotic, Illogical, Immaterial, Immense, Immoral,
 Incestous*, Incoherent, Incomplete, Incongruous, Incredible, Indistinct, Infected, Infernal, Infested, Inhuman,
 Insane, Insatiable*, Insidious*, Insipid, Invidious*, Iridescent, Irrational, Irregular, Jabbering, Jaded, Jangling,
 Jaundiced, Jellified, Jumbled, Jutting, Kleptomaniacal, Leprous, Limp, Liquefied, Loathsome, Lumbering, Luminescent,
 Lumpy, Lunatic, Lurking, Mad, Maggoty, Malevolent, Malformed*, Malicious, Malignant, Mangy*, Massive, Melancholic*,
 Membranous, Menacing, Mesmerizing, Metallic, Mildewed, Mindless, Miscarried, Misshapen*, Moaning, Molten, Molting*,
 Monstrous, Monumental, Morbid, Mordant*, Morose*, Mortifying, Mottled, Mouldering, Mucky, Mucous, Murderous*, Murmuring,
 Mutilated, Nagging, Nameless, Nauseous, Nearsighted, Nebulous, Necromantic, Necrotic*, Nictating*, Nightmarish*, Noiseless,
 Non-Euclidian*, Nonsensical, Noxious, Numbing, Obese*, Obscene, Obsequious, Octopoid, Odious, Odorous, Oily, Oleaginous*,
 Ominous, Oozing, Organic, Otiose*, Outlandish, Oval, Overgrown, Overripe, Pagan, Pale, Pallid, Palpitating, Palsied, Parasitic,
 Pasty, Peculiar, Pendulous*, Perfidious, Perverse, Pestilent*, Phlegmatic, Pitiless, Plastic, Pliable, Poisonous,
 Porous, Pregnant, Prodigious, Profane, Profuse, Prognathous*, Pronged, Protoplasmic, Protuberant, Prurient,
 Pseudopoidal, Puckered, Pudding-Like, Pulsating, Pulsing*, Pustular, Pustulent*, Putrid, Pyriform*, Quavering,
 Queasy, Quiescent, Quivering, Radiant, Rainbowed, Recrudescent*, Rectangular, Reeking, Refulgent*, Remorseless,
 Repellent, Reprehensible, Reptilian, Repugnant, Repulsive, Resplendent, Restless, Rheumy, Rigid, Rotten*,
 Rotting*, Rough, Rubbery, Rugose, Sacrilegious, Sallow, Sanguine, Satanic*, Scabby, Scabrous*, Scaly,
 Scintillating*, Screaming, Scrofulous*, Scummy, Sebaceous*, Seething, Segmented*, Senescent*, Senseless,
 Sepulchral, Shadowy, Shiny, Shivering*, Shrieking, Shuffling, Sickly, Sightless, Simian*, Sinewy, Singular,
 Skeletal, Sleepless, Slimy, Slippery, Slithering, Slobbering, Sluggish, Solemn, Sordid, Soundless, Spectral,
 Spherical, Spiky*, Sponge-Like, Squamous*, Stagnant, Stentorian*, Sticky, Stupefying, Stupendous, Stygian*,
 Sulphurous, Syrupy, Teeming, Tentacled, Terrible, Thickening, Thrashing, Throbbing, Toothy*, Transformed,
 Transparent, Tubular, Tumultuous, Turbid, Turbulent, Ugly, Ultimate, Umber*, Unclean, Uncouth, Undigested,
 Ungainly, Unhallowed*, Unholy*, Unknown, Unmasked, Unnatural*, Unripe, Unseen, Unspeakable, Unutterable,
 Vague, Vaporous, Vast, Venous*, Vermillion*, Verminous*,'no Vestigial*, Vibrating, Vile, Viperous, Viscous,
 Vivid, Voluminous, Vomiting, Voracious*, Vulpine*, Wailing, Wan, Warped, Waxen, Webbed, Wet, Whirling,
 Whithered, Worm-Eaten, Wormy, Wretched, Writhing, Xanthous*, Xenophobic, Yammering, Yonic*, Zodiacal, Zymotic

 -Mord
*/

/* message for doing damage with a weapon - from fight.c*/
#define MAX_DAM_MESSAGE 24
void dam_message ( int dam, Character *ch, Character *victim,
                   int w_type )
{
	char buf[MAX_INPUT_LENGTH] = "";
	char msgbuf[MAX_INPUT_LENGTH] = "";
	int msgnum = 0, tt;
	Character *people;
	int type_save = w_type;


	static  struct dam_size_data
	{
		const char * sing;
		const char * plu;
		const char * other;
	}
	dam_size[] =
	{
		{"miss", "misses", ""},//0
		{"scratch", "scratches", ""},//1
		{"blemish", "blemishes", ""},//2
		{"stub", "stubs", ""},//3
		{"slap", "slaps", ""},//4
		{"bruise", "bruises", ""},//5
		{"batter", "batters", ""},//6
		{"hurt", "hurts", ""},//7
		{"abuse", "abuses", ""},//8
		{"scathe", "scathes", ""},//9
		{"smash", "smashes", ""},//10
		{"injure", "injures", ""},//11
		{"mangle", "mangles", ""},//12
		{"damage", "damages", ""},//13
		{"maim", "maims", ""},//14
		{"lacerate", "lacerates", ""},//15
		{"massacre", "massacres", ""},//16
		{"cripple", "cripples", ""},//17
		{"{cROBLITERATE{c0", "{cROBLITERATES{c0", ""},//18
		{"{cRANNIHILATE{c0", "{cRANNIHILATES{c0", ""},//19
		{"{cRMUTILATE{c0", "{cRMUTILATES{c0", ""},//20
		{"{cRDISINTEGRATE{c0", "{cRDISINTEGRATES{c0", ""},//21
		{"{cRINCAPACITATE{c0", "{cRINCAPACITATES{c0", ""},//22
		{"{cRDESTROY{c0", "{cRDESTROYS{c0", ""},//23
		{"do {cRUNSPEAKABLE{cy things to", "does {cRUNSPEAKABLE{cr things to", "does {cRUNSPEAKABLE{c0 things to"},//24
		{"do {cRUNNATURAL{cy things to", "does {cRUNNATURAL{cr things to", "does {cRUNNATURAL{c0 things to"},//25
		{"do {cRDEVASTATING{cy things to", "does {cRDEVASTATING{cr things to", "does {cRDEVASTATING{c0 things to"},//26
		{"do {cRUNHOLY{cy things to", "does {cRUNHOLY{cr things to", "does {cRUNHOLY{c0 things to"},//27
		{"do {cRCATACLYSMIC{cy things to", "does {cRCATACLYSMIC{cr things to", "does {cRCATACLYSMIC{c0 things to"},//28
		{"do {cRDESOLATING{cy things to", "does {cRDESOLATING{cr things to", "does {cRDESOLATING{c0 things to"},//29
		{"do {cRSAVAGE{cy things to", "does {cRSAVAGE{cr things to", "does {cRSAVAGE{c0 things to"},//30
		{"do {cRDISGUSTING{cy things to", "does {cRDISGUSTING{cr things to", "does {cRDISGUSTING{c0 things to"},//31
		{"do {cRHELLISH{cy things to", "does {cRHELLISH{cr things to", "does {cRHELLISH{c0 things to"},//32
		{"do {cRTERMINAL{cy things to", "does {cRTERMINAL{cr things to", "does {cRTERMINAL{c0 things to"},//33
		{"do {cRTERMINAL{cy things to", "does {cPTERMINAL{cr things to", "does {cPTERMINAL{c0 things to"}//34

	};



	tt = find_fe_type ( ch );

	/*affects below*/
	if ( AFF_FLAGGED ( ch, AFF_MIND_ELEC ) )
		tt = AFF_MIND_ELEC;
	else if ( AFF_FLAGGED ( ch, AFF_MIND_FIRE ) )
		tt = AFF_MIND_FIRE;
	else if ( AFF_FLAGGED ( ch, AFF_MIND_WATER ) )
		tt = AFF_MIND_WATER;
	else if ( AFF_FLAGGED ( ch, AFF_MIND_ICE ) )
		tt = AFF_MIND_ICE;
	else if ( AFF_FLAGGED ( ch, AFF_FURY_ATTACKS ) )
		tt = AFF_FURY_ATTACKS;
	else if ( AFF_FLAGGED ( ch, AFF_DRAIN_BLOOD ) )
		tt = AFF_DRAIN_BLOOD;


	if ( dam == 0 )
		msgnum = 0;
	else if ( dam <= 10 )
		msgnum = 1;
	else if ( dam <= 30 )
		msgnum = 2;
	else if ( dam <= 60 )
		msgnum = 3;
	else if ( dam <= 90 )
		msgnum = 4;
	else if ( dam <= 120 )
		msgnum = 5;
	else if ( dam <= 150 )
		msgnum = 6;
	else if ( dam <= 180 )
		msgnum = 7;
	else if ( dam <= 210 )
		msgnum = 8;
	else if ( dam <= 240 )
		msgnum = 9;
	else if ( dam <= 270 )
		msgnum = 10;
	else if ( dam <= 300 )
		msgnum = 11;
	else if ( dam <= 330 )
		msgnum = 12;
	else if ( dam <= 360 )
		msgnum = 13;
	else if ( dam <= 390 )
		msgnum = 14;
	else if ( dam <= 420 )
		msgnum = 15;
	else if ( dam <= 450 )
		msgnum = 16;
	else if ( dam <= 480 )
		msgnum = 17;
	else if ( dam <= 510 )
		msgnum = 18;
	else if ( dam <= 540 )
		msgnum = 19;
	else if ( dam <= 570 )
		msgnum = 20;
	else if ( dam <= 600 )
		msgnum = 21;
	else if ( dam <= 800 )
		msgnum = 22;
	else if ( dam <= 950 )
		msgnum = 23;
	else  if ( dam <= 1100 )
		msgnum = MAX_DAM_MESSAGE;
	else  if ( dam <= 1600 )
		msgnum = MAX_DAM_MESSAGE + 1;
	else  if ( dam <= 1800 )
		msgnum = MAX_DAM_MESSAGE + 2;
	else  if ( dam <= 2300 )
		msgnum = MAX_DAM_MESSAGE + 3;
	else  if ( dam <= 2600 )
		msgnum = MAX_DAM_MESSAGE + 4;
	else  if ( dam <= 3200 )
		msgnum = MAX_DAM_MESSAGE + 5;
	else  if ( dam <= 3800 )
		msgnum = MAX_DAM_MESSAGE + 6;
	else  if ( dam <= 4600 )
		msgnum = MAX_DAM_MESSAGE + 7;
	else  if ( dam <= 5500 )
		msgnum = MAX_DAM_MESSAGE + 8;
	else
		msgnum = MAX_DAM_MESSAGE + 9;



	if ( dam )
	{
		/* damage message to damager */
		ch->Send ( "%s", CCYEL ( ch, C_CMP ) );

		if ( PRF_FLAGGED ( ch, PRF_BRIEF ) )
			act ( "You hurt $N.", FALSE, ch, NULL, victim, TO_CHAR );
		else
		{
			int chance_m = MAX ( MIN ( ( int ) ATK_CHANCE ( ch ), 3 ), 0 );
			fight_type_message ( buf, sizeof ( buf )-1, ch, victim, tt, type_save, FE_TO_CHAR );
			snprintf ( msgbuf, sizeof ( msgbuf ), "You %s $N and %s {cy%s. (%d)",
			           chance_message[ chance_m ].singular, dam_size[msgnum].sing, buf, dam );
			act ( msgbuf, FALSE, ch, NULL, victim, TO_CHAR );

		}

		ch->Send ( "%s", CCNRM ( ch, C_CMP ) );
	}
	else if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
		act ( "You miss $N!", FALSE, ch, NULL, victim, TO_CHAR );

	if ( dam )
	{
		/* damage message to damagee */
		send_to_char ( CCRED ( victim, C_CMP ), victim );
		if ( PRF_FLAGGED ( victim, PRF_BRIEF ) )
			act ( "$n hurts you.", FALSE, ch, NULL, victim, TO_VICT | TO_SLEEP );
		else
		{
			fight_type_message ( buf, sizeof ( buf )-1, ch, victim, tt,type_save, FE_TO_VICT );
			snprintf ( msgbuf, sizeof ( msgbuf ), "$n %s{cr you%s.",  dam_size[msgnum].plu, buf );
			act ( msgbuf, FALSE, ch, NULL, victim, TO_VICT | TO_SLEEP );
		}
		send_to_char ( CCNRM ( victim, C_CMP ), victim );
	}
	else if ( PRF_FLAGGED ( victim, PRF_BATTLESPAM ) )
		act ( "$n misses you!", FALSE, ch, NULL, victim, TO_VICT | TO_SLEEP );



	/* damage message to fighting onlookers
	 * Doesnt send message to non fighting people to reduce spam
	 */
	if ( dam )
	{
		fight_type_message ( buf, sizeof ( buf )-1, ch, victim, tt, type_save, FE_TO_ROOM );
		snprintf ( msgbuf, sizeof ( msgbuf ), "$n %s $N%s.", ( msgnum >=  MAX_DAM_MESSAGE ) ? dam_size[msgnum].other : dam_size[msgnum].plu , buf );

		for ( people = IN_ROOM ( ch )->people; people;
		        people = people->next_in_room )
		{
			if ( !PRF_FLAGGED ( people, PRF_BATTLESPAM ) )
				continue;
			if ( people == victim )
				continue;
			if ( people == ch )
				continue;

			if ( PRF_FLAGGED ( people, PRF_BRIEF ) )
				perform_act ( "$n hurts $N.", ch, NULL, victim, people );
			else
				perform_act ( msgbuf, ch, NULL, victim, people );

		}

	}

	if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
	{
		ch->Send ( "{cc" );
		diag_char_to_char ( victim, ch );
		ch->Send ( "{c0" );
	}

}


void update_pos ( Character *victim )
{
	if ( AFF_FLAGGED ( victim, AFF_MEDITATE ) && GET_POS ( victim ) > POS_SLEEPING )
	{
		victim->Send ( "You stop meditating and become more alert.\r\n" );
		affect_from_char ( victim, SKILL_MEDITATE );
	}

	if ( ( GET_HIT ( victim ) > 0 ) && ( GET_POS ( victim ) > POS_STUNNED ) )
		return;
	else if ( GET_HIT ( victim ) > 0 )
		GET_POS ( victim ) = POS_STANDING;
	else if ( GET_HIT ( victim ) <= HIT_DEAD )
		GET_POS ( victim ) = POS_DEAD;
	else if ( GET_HIT ( victim ) <= HIT_MORTALLYW )
		GET_POS ( victim ) = POS_MORTALLYW;
	else if ( GET_HIT ( victim ) <= HIT_INCAP )
		GET_POS ( victim ) = POS_INCAP;
	else
		GET_POS ( victim ) = POS_STUNNED;

	if ( GET_POS ( victim ) > POS_DEAD )
		REMOVE_BIT_AR ( PLR_FLAGS ( victim ), PLR_DYING );
}


void check_killer ( Character *ch, Character *vict )
{

	if ( !PLR_FLAGGED ( vict, PLR_KILLER ) && !PLR_FLAGGED ( vict, PLR_THIEF )
	        && !PLR_FLAGGED ( ch, PLR_KILLER ) && !IS_NPC ( ch ) && !IS_NPC ( vict ) &&
	        ( ch != vict ) )
	{

		if ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_ARENA ) )
			return ;

		if ( both_pk ( ch,vict ) )
			return ;

		SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_KILLER );
		new_mudlog ( BRF, LVL_GOD, TRUE,
		             "PC Killer bit set on %s for initiating attack on %s at %s.",
		             GET_NAME ( ch ), GET_NAME ( vict ), vict->in_room->name );
		ch->Send ( "If you want to be a PLAYER KILLER, so be it...\r\n" );
	}
}


/* start one char fighting another (yes, it is horrible, I know... )  */
void set_fighting ( Character *ch, Character *vict )
{

	//redundant
	start_fighting ( ch, vict );
	return;
}


void stop_fighting ( Character* ch )
{
	/* if (GET_FIGHT_EVENT(ch))
	 {
	   event_cancel(GET_FIGHT_EVENT(ch));
	   GET_FIGHT_EVENT(ch) = NULL;
	 }*/

	FIGHTING ( ch ) = NULL;
	GET_POS ( ch ) = POS_STANDING;
	update_pos ( ch );

	return;
}

EVENT2 ( deliver_bodybag )
{
	obj_data *corpse = ( obj_data* ) causer;
	Room *room = ( Room* ) victim;

	if ( !corpse )
	{
		new_mudlog ( CMP, LVL_GOD, TRUE, "SYSERR: a bodybag is empty!" );
		return;
	}

	for ( Descriptor *d = descriptor_list; d; d = d->next )
		if ( STATE (d ) == CON_PLAYING && d->character && IN_ROOM ( d->character ) == room )
			d->character->Send ( "A rescue team arrives, puts the corpse into a bodybag and heads for the\r\nemergency room.\r\n" );

	obj_from_room ( corpse );
	obj_to_room ( corpse, world_vnum[ 1205 ] ); // move corpse to The Emergency Room
	REMOVE_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NPC_CORPSE );
	SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_PC_CORPSE );
	add_corpse_to_list ( corpse );
	save_corpses();

	for ( Descriptor *d = descriptor_list; d; d = d->next )
		if ( STATE (d ) == CON_PLAYING && d->character && IN_ROOM ( d->character ) == world_vnum[ 1205 ] )
			d->character->Send ( "A rescue team arrives, removes a corpse from a bodybag, and rushes out to\r\nanother job.\r\n" );
}

void make_corpse ( Character *ch, Character *killer )
{
	struct obj_data *corpse = NULL, *o, *next_obj;
	struct obj_data *money;
	int remorts = MIN ( REMORTS ( ch ), 50 );
	int i, x, y;
	char buf2[MAX_INPUT_LENGTH];

	if ( !ch || DEAD ( ch ) )
	{
		return;
	}
	else
	{
		corpse = create_obj ( NOTHING );
		corpse->name = strdup ( "corpse" );
		if ( IS_NPC ( ch ) )
			corpse->skin = MOB_SKIN ( ch );
		else
			corpse->skin = NOTHING;

		if ( corpse_mod == 0 )
			snprintf ( buf2, sizeof ( buf2 ), "The corpse of %s is lying here.", GET_NAME ( ch ) );
		else if ( corpse_mod == 1 )
		{
			snprintf ( buf2, sizeof ( buf2 ), "The headless corpse of %s is lying here.", GET_NAME ( ch ) );
			make_head ( ch );
		}
		else
		{
			snprintf ( buf2, sizeof ( buf2 ), "Half the corpse of %s is lying here.", GET_NAME ( ch ) );
			make_half ( ch );
		}
		corpse->description = str_dup ( buf2 );

		snprintf ( buf2, sizeof ( buf2 ), "the corpse of %s", GET_NAME ( ch ) );
		corpse->short_description = str_dup ( buf2 );

		GET_OBJ_TYPE ( corpse ) = ITEM_CONTAINER;

		/* clear out the arrays for the 128 bit code */
		for ( x = y = 0; x < EF_ARRAY_MAX || y < TW_ARRAY_MAX; x++, y++ )
		{
			if ( x < EF_ARRAY_MAX )
				GET_OBJ_EXTRA_AR ( corpse, x ) = 0;
			if ( y < TW_ARRAY_MAX )
				corpse->obj_flags.wear_flags[y] = 0;
		}
		//SET_BIT_AR(GET_OBJ_WEAR(corpse), ITEM_WEAR_TAKE);
		SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NODONATE );
		GET_OBJ_VAL ( corpse, 0 ) = 0; /* You can't store stuff in a corpse */
		GET_OBJ_VAL ( corpse, 3 ) = 1; /* corpse identifier */


		GET_OBJ_WEIGHT ( corpse ) = GET_WEIGHT ( ch ) + IS_CARRYING_W ( ch );
		/*
		    if (GET_OBJ_WEIGHT(corpse) > 5000 || GET_OBJ_WEIGHT(corpse) < 0)
		 GET_OBJ_WEIGHT(corpse) = 500;*/

		GET_OBJ_RENT ( corpse ) = 100000;

		/* Corpse saving stuff */
		SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NOSELL );
		SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NORENT );
		SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_UNIQUE_SAVE );

		if ( MOB_FLAGGED ( ch, MOB_EDIBLE ) )
			SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_EDIBLE );

		if ( IS_NPC ( ch ) )
		{
			GET_OBJ_TIMER ( corpse ) = CONFIG_MAX_NPC_CORPSE_TIME;
			SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NPC_CORPSE );
			SET_BIT_AR ( GET_OBJ_WEAR ( corpse ), ITEM_WEAR_TAKE );
			if ( corpse_mod == 1 )
			{
				SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_BEHEADED_CORPSE );
			}
		}
		else
		{

			SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_PC_CORPSE );
			if (( IS_PK ( ch ) ) && CHAMPION != GET_IDNUM(ch))
			{
				SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_PK_CORPSE );
			}
			if ( corpse_mod == 1 )
			{
				SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_BEHEADED_CORPSE );
			}
			GET_OBJ_VROOM ( corpse ) = GET_ROOM_VNUM ( IN_ROOM ( ch ) );
			GET_OBJ_VAL ( corpse, 0 ) = MAX ( 1, get_pidx_from_name ( ch ) );
			GET_OBJ_VAL ( corpse, 1) = (GET_LEVEL(ch));
			GET_OBJ_VAL ( corpse, 4) = (REMORTS(ch));
			GET_OBJ_VAL ( corpse, 6 ) = ( killer && ( !IS_NPC ( ch ) && !IS_NPC ( killer ) ) && IS_PK ( ch ) && IS_PK ( killer ) && GET_LEVEL ( killer ) < LVL_IMMORT );
			if ( GET_LEVEL ( ch ) > LVL_IMMORT )
				GET_OBJ_TIMER ( corpse ) = 1;
			else
				GET_OBJ_TIMER ( corpse ) = IRANGE ( 10, 10 + ( remorts * 2 ), 20 );
		}

		/* transfer character's inventory to the corpse */
		bool use_bodybag = FALSE;
		for ( o = ch->carrying; o != NULL; o = next_obj )
		{
			next_obj = o->next_content;
			obj_from_char ( o );
			obj_to_obj ( o, corpse );

			/* subtract quality from bodybag if there is one */
			if ( !use_bodybag && !IS_NPC ( ch ) && GET_OBJ_TYPE ( o ) == ITEM_BODYBAG && GET_OBJ_QUALITY ( o ) >= 10 )
			{
				GET_OBJ_QUALITY ( o ) = IRANGE ( 0, GET_OBJ_QUALITY ( o ) - 10, GET_OBJ_QUALITY ( o ) );
				if ( GET_OBJ_QUALITY ( o ) < 10 )
				{
					string shortdesc = string ( o->short_description ) + " is torn from use";
					SET_BIT_AR ( GET_OBJ_EXTRA ( o ), ITEM_UNIQUE_SAVE );
					if ( o->short_description && o->short_description != obj_proto[ GET_OBJ_RNUM ( o ) ].short_description )
						free ( o->short_description );
					o->short_description = str_udup ( shortdesc.c_str() );
				}
				use_bodybag = TRUE;
			}
		}

		/* transfer character's equipment to the corpse */
		for ( i = 0; i < NUM_WEARS; i++ )
			if ( GET_EQ ( ch, i ) )
			{
				//remove_otrigger(GET_EQ(ch, i), ch);
				obj_to_obj ( unequip_char ( ch, i ), corpse );
			}

		/* transfer gold */
		if ( ch->Gold ( 0, GOLD_HAND ) > 0 )
		{
                /* Clan deeds - lets check if the clan gets a bonus */
                    if (IS_NPC(ch) && killer && !IS_NPC(killer) && IN_ROOM ( ch ) != NULL) {
                        struct clan_deed_type *cl;
                        int clan_num, zone_num;
                        zone_num = zone_table[IN_ROOM(ch)->zone].number;
                       

		    if (( clan_num = find_clan_by_id(GET_CLAN(killer)) >= 0) && (ch->vnum > (zone_table[IN_ROOM(ch)->zone].bot - 1)) && (ch->vnum < (zone_table[IN_ROOM(ch)->zone].top + 1))) {
                            for (cl = clan[clan_num].deeds; cl; cl = cl->next){
                                if (is_same_zone(cl->zone, zone_num)) {
                               //     clan[clan_num].treasury.coins += ch->Gold(0, GOLD_HAND)/100;
                                 //   killer->Send("Clan Deed: You have added %lld gold coins into the treasury.\r\n", ch->Gold(0, GOLD_HAND)/100);
                                    break;
                                }
                            }
                        }
                    }

			/* following 'if' clause added to fix gold duplication loophole */
			if ( IS_NPC ( ch ) || ( !IS_NPC ( ch ) && ch->desc ) )
			{
				spill_gold ( ch );
				money = create_money ( ch->Gold ( 0, GOLD_HAND ) );
				SET_BIT_AR ( GET_OBJ_EXTRA ( money ), ITEM_UNIQUE_SAVE );
				obj_to_obj ( money, corpse );
			}
			ch->Gold ( -ch->Gold ( 0, GOLD_HAND ), GOLD_HAND );
		}

		if ( !IS_NPC ( ch ) )
		{
			SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_LOADROOM );
			GET_LOADROOM ( ch ) = NOWHERE;
			//save_char(ch, 0);
		}

		if ( use_bodybag )
			add_event2 ( 10, deliver_bodybag, corpse, IN_ROOM ( ch ), 0 );

		obj_to_room ( corpse, IN_ROOM ( ch ) );

		if ( !IS_NPC ( ch ) )
		{
			add_corpse_to_list ( corpse );
			save_corpses();
		}
		else
			check_timer ( corpse );
	}
}

void make_head ( Character *ch )
{
	struct obj_data *corpse;
	int x, y;
	char buf2[MAX_INPUT_LENGTH];

	if ( !ch )
		return;

	corpse = create_obj ( NOTHING );
	corpse->name = str_dup ( "head" );


	snprintf ( buf2, sizeof ( buf2 ), "The head of %s rolls around here.", GET_NAME ( ch ) );
	corpse->description = str_dup ( buf2 );

	snprintf ( buf2, sizeof ( buf2 ), "the head of %s", GET_NAME ( ch ) );
	corpse->short_description = str_dup ( buf2 );

	GET_OBJ_TYPE ( corpse ) = ITEM_OTHER;

	/* clear out the arrays for the 128 bit code */
	for ( x = y = 0; x < EF_ARRAY_MAX || y < TW_ARRAY_MAX; x++, y++ )
	{
		if ( x < EF_ARRAY_MAX )
			GET_OBJ_EXTRA_AR ( corpse, x ) = 0;
		if ( y < TW_ARRAY_MAX )
			corpse->obj_flags.wear_flags[y] = 0;
	}
	SET_BIT_AR ( GET_OBJ_WEAR ( corpse ), ITEM_WEAR_TAKE );
	SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NODONATE );
	GET_OBJ_VAL ( corpse, 0 ) = 1;
	GET_OBJ_VAL ( corpse, 3 ) = 1;


	GET_OBJ_WEIGHT ( corpse ) = 25;

	GET_OBJ_RENT ( corpse ) = 100000;

	/* Corpse saving stuff */
	SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NOSELL );
	SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NORENT );
	SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_UNIQUE_SAVE );

	obj_to_room ( corpse, IN_ROOM ( ch ) );
}

void make_half ( Character *ch )
{
	struct obj_data *corpse;
	int x, y;
	char buf2[MAX_INPUT_LENGTH];

	if ( !ch )
		return;

	corpse = create_obj ( NOTHING );
	corpse->name = str_dup ( "half other" );


	snprintf ( buf2, sizeof ( buf2 ), "The other half of %s's corpse is lying here.", GET_NAME ( ch ) );
	corpse->description = str_dup ( buf2 );

	snprintf ( buf2, sizeof ( buf2 ), "half of %s's corpse", GET_NAME ( ch ) );
	corpse->short_description = str_dup ( buf2 );

	GET_OBJ_TYPE ( corpse ) = ITEM_OTHER;

	/* clear out the arrays for the 128 bit code */
	for ( x = y = 0; x < EF_ARRAY_MAX || y < TW_ARRAY_MAX; x++, y++ )
	{
		if ( x < EF_ARRAY_MAX )
			GET_OBJ_EXTRA_AR ( corpse, x ) = 0;
		if ( y < TW_ARRAY_MAX )
			corpse->obj_flags.wear_flags[y] = 0;
	}
	SET_BIT_AR ( GET_OBJ_WEAR ( corpse ), ITEM_WEAR_TAKE );
	SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NODONATE );
	GET_OBJ_VAL ( corpse, 0 ) = 1;
	GET_OBJ_VAL ( corpse, 3 ) = 1;


	GET_OBJ_WEIGHT ( corpse ) = 25;

	GET_OBJ_RENT ( corpse ) = 100000;

	/* Corpse saving stuff */
	SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_NOSELL );
	SET_BIT_AR ( GET_OBJ_EXTRA ( corpse ), ITEM_UNIQUE_SAVE );

	obj_to_room ( corpse, IN_ROOM ( ch ) );
}



/* When ch kills victim */
void change_alignment ( Character *ch, Character *victim )
{
	if ( ch && victim )
	{
		/*
		 * new alignment change algorithm: if you kill a monster with alignment A,
		 * you move 1/16th of the way to having alignment -A.  Simple and fast.
		 */
		if ( ch != victim )
			GET_ALIGNMENT ( ch ) += ( -GET_ALIGNMENT ( victim ) - GET_ALIGNMENT ( ch ) ) / 16;
	}
}



void death_cry ( Character *ch )
{
	int door;

	act ( "Your blood freezes as you hear $n's death cry.", FALSE, ch, 0, 0,
	      TO_ROOM );

	for ( door = 0; door < NUM_OF_DIRS; door++ )
		if ( CAN_GO ( ch, door ) )
			send_to_room ( IN_ROOM ( ch )->dir_option[door]->to_room,
			               "Your blood freezes as you hear someone's death cry.\r\n" );
}



void raw_kill ( Character *ch, Character *killer )
{
	float ct = 0;
	struct obj_data *obj;
	char buf[256];
	int zone_num;
	int legit = 0;

	if ( !ch || DEAD ( ch ) )
		return;

  /* Clan deeds - count the kills in the zone */
  /* Then check if the player has a good kill/time ratio so
     they dont cheat on just idling in the zone to get the deeds */
	if (ch && killer && !IS_NPC(killer) && IS_NPC(ch) && IN_ROOM ( ch ) != NULL) {
		zone_num = zone_table[IN_ROOM(ch)->zone].number;
		if ((ch->vnum > (zone_table[IN_ROOM(ch)->zone].bot - 1)) && (ch->vnum < (zone_table[IN_ROOM(ch)->zone].top + 1)))
			legit = 1;

		if (legit == 1) {
			killer->player.deeds.kills++;
			ct = (time(0) - killer->player.deeds.time_in)/300.00;
			ct = (float)killer->player.deeds.kills/ct;
			struct clan_deed_type *cl;
			int clan_num, deeds_amt = 0, winner_amt = 0, highest_clan = 0;
			for ( clan_num = 0; clan_num <= num_of_clans; ++clan_num ) {
				for (cl = clan[clan_num].deeds; cl; cl = cl->next)
					deeds_amt++;
				if (deeds_amt > winner_amt) {
					highest_clan = clan_num;
					winner_amt = deeds_amt;
				}
				deeds_amt = 0;
			}

			/* Now lets check percentage chance for deed to load */
			/* Percentage chance increases every 15 minutes      */
			ct = (time(0) - killer->player.deeds.time_in)/900.00;
			ct += 0;     // Always 1 percentage chance anyway
			if (GET_CLAN(killer) != highest_clan)
				ct += 9;
			ct += (killer->player.deeds.kills)/7;

			if (number(0, 100) < (int)ct) {
				obj = read_object(7, VIRTUAL);
				sprintf(buf, "Clan deed for %s", zone_table[IN_ROOM(killer)->zone].name);
				obj->short_description = strdup(buf);
				sprintf(buf, "Clan deed for %s is lying here!\r\n", zone_table[IN_ROOM(killer)->zone].name);
				obj->description = strdup(buf);
				GET_OBJ_VAL(obj, 0) = zone_num;
				GET_OBJ_TIMER(obj) = 50;
				SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
				check_timer(obj);
				obj_to_char(obj, killer);
				load_otrigger(obj);
				killer->player.deeds.kills = -100;
				killer->Send("{cY%s just won the deed for %s for %s!\r\n{cn",GET_NAME(killer), zone_table[IN_ROOM(killer)->zone].name,  clan_name ( find_clan_by_id ( GET_CLAN ( killer ) )));
			}
		}
	}

	remove_all_normal_affects ( ch );
	/* To make ordinary commands work in scripts.  welcor */
	GET_POS ( ch ) = POS_STANDING;

	if ( death_mtrigger ( ch, killer ) )
		death_cry ( ch );

	update_pos ( ch );
	make_corpse ( ch, killer );
	// clears out eq
	if ( !IS_NPC ( ch ) )
		Crash_crashsave ( ch );
	death_room ( ch );
}

void delay_die ( Character *ch, Character *killer )
{

	if ( !IS_NPC ( killer ) || SELF ( killer, ch ) || ( GET_LEVEL ( ch ) <=10 && REMORTS ( ch ) == 0 ) )
	{
		die ( ch, killer );
		return;
	}
	stop_fighting ( ch );
	ch->Send ( "{cWYou are dead, you have 60 seconds to be healed or you will\r\n"
	           "leave a corpse. To leave a corpse now, type DIE{c0\r\n" );
	SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_DYING );
	DIE_TIME ( ch ) = time ( 0 );

}
Character * char_by_id_desc_list ( long idnum )
{
	Descriptor *d;
	if ( !descriptor_list )
		return NULL;

	for ( d = descriptor_list;d;d = d->next )
	{
		if ( IS_PLAYING ( d ) && d->character && GET_IDNUM ( d->character ) == idnum )
			return d->character;
	}
	return NULL;
}

void die ( Character *ch, Character *killer )
{
	gold_int exp = ( GET_LEVEL ( ch ) <=10 && REMORTS ( ch ) == 0 );

	if ( PLR_FLAGGED ( ch, PLR_DYING ) )
		REMOVE_BIT_AR ( PLR_FLAGS ( ch ), PLR_DYING );

	if ( ch && !IS_NPC ( ch ) && ( ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_ARENA ) ) || ( exp ) ) )
	{
		halt_fighting ( ch );
		if ( exp )
			ch->Send ( "You die before your time and are restored by the gods!\r\n"
			           "This will only happen till level 10,\r\nafter that the gods will have no pity.\r\n" );
		new_mudlog ( NRM, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "%sDEATH: %s killed and restored.", ( exp ? "NEWBIE" : "ARENA" ),GET_NAME ( ch ) );

		arena_kill ( ch );

	}
	else
	{
		struct dam_from_list * t;
		//long idnum;
		short counter=0;
                char displayexp[50];
		Character *temp = NULL;
		/*if ( killer )
			idnum = GET_IDNUM ( killer );
		else
			idnum = -1;*/

		change_alignment ( killer, ch );

		if ( !IS_NPC ( ch ) )
		{
			exp =
			    ( level_exp ( GET_CLASS ( ch ), GET_LEVEL ( ch ) + 1,  current_class_is_tier_num ( ch ), REMORTS ( ch ) ) - ( level_exp ( GET_CLASS ( ch ), GET_LEVEL ( ch ),  current_class_is_tier_num ( ch ), REMORTS ( ch ) ) ) );
			gain_exp ( ch, - ( exp / 6 ) );
		}
		/** Count the total people who did damage to the mob - mord **/
		int player_damage_done = 0;
		for ( t = MOB_DAM_LIST ( ch ); t; t = t->next )	{
			if (t->id == -1)
				continue;
			temp = find_char (t->id);
			if (!temp)
				continue;
			if (IS_NPC(temp))
				continue;
			if (IN_ROOM(temp) != IN_ROOM(ch))
				continue;

			player_damage_done += t->damage;
			counter++;
		}

		for ( t = MOB_DAM_LIST ( ch ); t; t = t->next )
		{
			if ( t->id != -1 )
			{
				temp = find_char ( t->id );
				if ( !temp || IN_ROOM(temp) != IN_ROOM(ch))
					continue;

#if defined(EXP_GAIN_SYSTEM_1)
				exp = ( GET_EXP ( ch ) * t->damage ) /MOB_DAM_TAKEN ( ch );
				if ( exp < ( ( GET_EXP ( ch ) * ( ( 160 - GET_LEVEL ( ch ) ) /10 ) ) /100 ) ) /*(16% for level 1, 1% for level 150)*/
					exp = ( ( GET_EXP ( ch ) * ( ( 160 - GET_LEVEL ( ch ) ) /10 ) ) /100 );
#else
				exp = ( (GET_EXP ( ch ) * player_damage_done / MOB_DAM_TAKEN (ch) ) / ( counter>0?counter:1 ) );
                                if (counter > 1) exp = exp * 12 / 10;
#endif
                                commafmt ( displayexp, sizeof ( displayexp ), exp );
				if ( !PRF_FLAGGED ( temp, PRF_BATTLESPAM ) )
				{
					if ( HERE ( ch, temp ) )
					{
						if ( exp > 1 )
							temp->Send ( "You receive your share of experience -- %s points.\r\n", displayexp );
						else
							temp->Send ( "You receive your share of experience -- one measly little point!\r\n" );
					}
					else
					{
						if ( exp > 1 )
							temp->Send ( "As %s dies you receive your share of experience -- %s points.\r\n", GET_NAME ( ch ), displayexp );
						else
							temp->Send ( "As %s dies you receive your share of experience -- one measly little point!\r\n", GET_NAME ( ch ) );
					}
				}
				gain_exp ( temp, exp );
                    if (IS_NPC(ch) && !IS_NPC(temp) && find_clan_by_id(GET_CLAN(temp)) >= 0 && IN_ROOM ( ch ) != NULL ) {
                        struct clan_deed_type *cl;
                        int clan_num, zone_num, deeds_amt = 0, deeds_bonus = 0, deeds_total;
                        zone_num = zone_table[IN_ROOM(ch)->zone].number;
                        clan_num = find_clan_by_id(GET_CLAN(temp));
			if (clan_num >= 0) {
			      for (cl = clan[clan_num].deeds; cl; cl = cl->next) {
                                deeds_amt += 1;
			} 

			if (deeds_amt > 0) {
			if (deeds_amt < 30){
			deeds_bonus = (int)((float)exp*((float)MIN(15, deeds_amt)/(float)100));
			deeds_total = MIN(15, deeds_amt);
		}
			if (deeds_amt > 29){
			deeds_bonus = (int)((float)exp*((float)MIN(25, deeds_amt)/(float)100));
			deeds_total = 25;
		}

			if (deeds_amt > 39){
			deeds_bonus = (int)((float)exp*((float)MIN(35, deeds_amt)/(float)100));
			deeds_total = 35;
		}
			if (deeds_amt > 49){
			deeds_bonus = (int)((float)exp*((float)MIN(45, deeds_amt)/(float)100));
			deeds_total = 45;
		}
			if (deeds_amt > 59){
			deeds_bonus = (int)((float)exp*((float)MIN(55, deeds_amt)/(float)100));
			deeds_total = 55;
		}	

			if (deeds_amt > 99){
			deeds_bonus = (int)((float)exp*((float)MIN(85, deeds_amt)/(float)100));
			deeds_total = 85;

		}	  temp->Send("Your clan grants you a bonus of %d%% exp (%d total) due to your deeds. [%d total]\r\n", deeds_total, deeds_bonus, deeds_amt);
			  gain_exp(temp, deeds_bonus);
                         }








			}
			if ( IN_ROOM ( ch ) != NULL && ( clan_num  >= 0) && (ch->vnum > (zone_table[IN_ROOM(ch)->zone].bot - 1)) &&(ch->vnum < (zone_table[IN_ROOM(ch)->zone].top + 1))) {

                            for (cl = clan[clan_num].deeds; cl; cl = cl->next) {

                                if (is_same_zone(cl->zone, zone_num)) {
                                    gain_exp(temp, exp/10);
                                    temp->Send("Clan Deed Bonus XP: %lld.\r\n", exp/10);
                                    break;
                                }
                            }
                        }
                    }

			}
		}

		raw_kill ( ch, killer );
	}
}

#if 0

void perform_group_gain ( Character *ch, gold_int base,
                          Character *victim )
{

	gold_int share;

	if ( IS_NPC ( ch ) || AFF_FLAGGED ( ch, AFF_CHARM ) || ( GET_LEVEL ( ch ) >= LVL_IMMORT ) )
		return;

	/* Calculate level-difference bonus
	share = IRANGE((base * 0.7), (base), base  * (1.0 + ( (GET_LEVEL(victim) - GET_LEVEL(ch))/40.0)));*/

	share = MAX ( 1, base );
	if ( !PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
	{
		if ( share > 1 )
			ch->Send ( "You receive your share of experience -- %lld points.\r\n", share );
		else
			ch->Send ( "You receive your share of experience -- one measly little point!\r\n" );
	}

	gain_exp ( ch, share );
	change_alignment ( ch, victim );
}

/* Combat exp change:
set mobs to count the amount of damage/hits they take from
each person.
And when they die, the exp is distributed through the list
of people that had caused damage to that mob.

new value on the mob,
int damage_taken;

and a linked list which lists the ID nums of all the players/mobs
that caused damage to this mob.

If something caused damage but now doesn't exist, then that damage
is still removed from the total.

Involvement will need to control how likely people are to land their attacks better.

Havd a special id num of -1,
for things like poison and scripts and mobs that cause damage that doesn't get distributed.

This addition would mean that mobs/pets could be used to fight along side players again.

*/

void group_gain ( Character *ch, Character *victim )
{
	float tot_members = 0.0, group_bonus = 0.0;
	int count = 0;
	gold_int tot_gain = 0;
	Character *k = ( ch->master ? ch->master : ch );
	struct follow_type *f;


	if ( HERE ( k,victim ) )
	{
		tot_members += GET_PERC ( k );
		count++;
	}

	for ( f = k->followers; f; f = f->next )
	{
		if ( HERE ( f->follower,victim ) )
		{
			tot_members += GET_PERC ( f->follower );
			if ( !IS_NPC ( f->follower ) )
				count++;
		}
	}
	count--;
	if ( IS_NPC ( victim ) )
		tot_gain = GET_EXP ( victim ) ;
	else
	{

		gold_int exp = 666666;
		/*
		  (level_exp(GET_CLASS(victim), GET_LEVEL(victim) + 1,  current_class_is_tier_num(victim), REMORTS(victim)) - (level_exp(GET_CLASS(victim), GET_LEVEL(victim),  current_class_is_tier_num(victim), REMORTS(victim))));
		*/
		tot_gain = exp * 0.15;
	}


	/* add it as normal exp */
	tot_gain += group_bonus * 0.50;

        /* Group xp bonus */
        if (count > 1)
          tot_gain *= 1.15;

	/* prevent illegal xp creation when killing players */
	if ( !IS_NPC ( victim ) )
		tot_gain = MIN ( CONFIG_MAX_EXP_LOSS * 0.66, tot_gain );

	if ( HERE ( k, ch ) && GET_PERC ( k ) > 0 )
	{
		perform_group_gain ( k, ( gold_int ) ( tot_gain * ( ( GET_PERC ( k ) / tot_members ) ) ), victim );
		if ( !IS_NPC ( k ) && ( GET_LEVEL ( k ) >= 20 ) )
			gain_group_exp ( k, ( ( tot_gain ) * ( 0.1 * count ) ) * 1.1 );
	}


	for ( f = k->followers; f; f = f->next )
	{
		if ( HERE ( f->follower,ch ) && GET_PERC ( f->follower ) > 0 )
		{
			perform_group_gain ( f->follower, ( gold_int ) ( tot_gain * ( ( GET_PERC ( f->follower ) ) / tot_members ) ), victim );
			if ( !IS_NPC ( f->follower ) && group_bonus )
				gain_group_exp ( f->follower, group_bonus / count );
		}

	}


}


void solo_gain ( Character *ch, Character *victim,
                 bool missile )
{
	gold_int exp;

	if ( IS_NPC ( ch ) && GET_LEVEL ( ch ) >= LVL_IMMORT )
		return;


	if ( !AFF_FLAGGED ( victim, AFF_CHARM ) )
		exp =
		    MIN ( CONFIG_MAX_EXP_GAIN * ( 1 + current_class_is_tier_num ( ch ) ),
		          GET_EXP ( victim ) );
	else
		exp = 0;


	exp = MAX ( exp, 1 );
	if ( !PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )

		ch->Send ( "You receive %lld experience points.\r\n", exp );
	else
		ch->Send ( "You receive one lousy experience point.\r\n" );

	if ( !missile )
		gain_exp ( ch, exp );
	change_alignment ( ch, victim );
}
#endif

char *replace_string ( const char *str, const char *weapon_singular,
                       const char *weapon_plural, const char *strike_sing,
                       const char *strike_plural, const char *hitcount_word )
{
	static char buf[256];

	char *cp = buf;


	for ( ; *str; str++ )
	{
		if ( *str == '#' )
		{
			switch ( * ( ++str ) )
			{
				case 'W':
					for ( ; *weapon_plural; * ( cp++ ) = * ( weapon_plural++ ) )
						;
					break;
				case 'w':
					for ( ; *weapon_singular; * ( cp++ ) = * ( weapon_singular++ ) )
						;
					break;
				case 'C':
					for ( ; *strike_plural; * ( cp++ ) = * ( strike_plural++ ) )
						;
					break;
				case 'c':
					for ( ; *strike_sing; * ( cp++ ) = * ( strike_sing++ ) )
						;
					break;
				case 't':
					for ( ; *hitcount_word; * ( cp++ ) = * ( hitcount_word++ ) )
						;    /*added by mord */
					break;
				default:
					* ( cp++ ) = '#';
					break;
			}
		}
		else
			* ( cp++ ) = *str;

		*cp = 0;
	}                 /* For */

	return ( buf );
}

ACMD ( do_fightmsg )
{
	//struct message_type *msg = NULL;
	int i;
	for ( i = 0; i < MAX_MESSAGES; i++ )
	{
		ch->Send ( "Num: %-3d - %s\r\n", fight_messages[i].a_type, skill_name ( fight_messages[i].a_type ) );
	}
}

/*
 * message for doing damage with a spell or skill
 *  C3.0: Also used for weapon damage on miss and death blows
 */
int skill_message ( int dam, Character *ch, Character *vict, int attacktype )
{
	int i, j, nr;
	struct message_type *msg = NULL;
	Character *people = NULL;
	Character *victim = vict;
	;

	struct obj_data *weap = GET_EQ ( ch, WEAR_WIELD );

	for ( i = 0; i < MAX_MESSAGES; i++ )
	{
		if ( fight_messages[i].a_type == attacktype )
		{
			if ( dam && ch != victim && !IS_NPC ( ch ) )
				ch->Send ( "{cg%2.2fx) {cy", GET_SKILLMULTI ( ch ) );
			nr = number ( 1, fight_messages[i].number_of_attacks );
			for ( j = 1, msg = fight_messages[i].msg; ( j < nr ) && msg; j++ )
				msg = msg->next;

			if ( !IS_NPC ( vict ) && ( GET_LEVEL ( vict ) > LVL_HERO ) )
			{
				act ( msg->god_msg.attacker_msg.c_str(), FALSE, ch, weap, vict,
				      TO_CHAR );
				act ( msg->god_msg.victim_msg.c_str(), FALSE, ch, weap, vict,
				      TO_VICT );

				for ( people = IN_ROOM ( ch )->people; people; people = people->next_in_room )
				{
					if ( !PRF_FLAGGED ( people, PRF_BATTLESPAM ) )
						continue;
					if ( people == victim )
						continue;
					if ( people == ch )
						continue;
					perform_act ( msg->god_msg.room_msg.c_str(), ch, weap, victim, people );
				}
			}
			else if ( dam != 0 )
			{
				if ( GET_POS ( vict ) == POS_DEAD )
				{
					ch->Send ( "%s", CCYEL ( ch, C_CMP ) );
					act ( msg->die_msg.attacker_msg.c_str(), FALSE, ch, weap, vict, TO_CHAR );
					ch->Send ( "(%d) %s", dam,  CCNRM ( ch, C_CMP ) );

					vict->Send ( "%s", CCRED ( vict, C_CMP ) );
					act ( msg->die_msg.victim_msg.c_str(), FALSE, ch, weap, vict, TO_VICT | TO_SLEEP );
					vict->Send ( "%s", CCNRM ( vict, C_CMP ) );

					for ( people = IN_ROOM ( ch )->people; people; people = people->next_in_room )
					{
						if ( people == victim )
							continue;
						if ( people == ch )
							continue;
						perform_act ( msg->die_msg.room_msg.c_str(), ch, weap,  victim, people );
					}
				}
				else
				{
					ch->Send ( "%s", CCYEL ( ch, C_CMP ) );
					act ( msg->hit_msg.attacker_msg.c_str(), FALSE, ch, weap, vict, TO_CHAR );
					ch->Send ( "(%d) %s", dam, CCNRM ( ch, C_CMP ) );

					vict->Send ( "%s", CCRED ( vict, C_CMP ) );
					act ( msg->hit_msg.victim_msg.c_str(), FALSE, ch, weap, vict, TO_VICT | TO_SLEEP );
					vict->Send ( "%s", CCNRM ( vict, C_CMP ) );

					for ( people = IN_ROOM ( ch )->people; people; people = people->next_in_room )
					{
						if ( !PRF_FLAGGED ( people, PRF_BATTLESPAM ) )
							continue;
						if ( people == victim )
							continue;
						if ( people == ch )
							continue;
						perform_act ( msg->hit_msg.room_msg.c_str(), ch, weap, victim, people );
					}
				}
			}
			else if ( ch != vict )   /* Dam == 0 */
			{
				ch->Send ( "%s", CCYEL ( ch, C_CMP ) );
				act ( msg->miss_msg.attacker_msg.c_str(), FALSE, ch, weap, vict, TO_CHAR );
				ch->Send ( "%s", CCNRM ( ch, C_CMP ) );

				vict->Send ( "%s", CCRED ( vict, C_CMP ) );
				act ( msg->miss_msg.victim_msg.c_str(), FALSE, ch, weap, vict, TO_VICT | TO_SLEEP );
				vict->Send ( "%s", CCNRM ( vict, C_CMP ) );

				for ( people = IN_ROOM ( ch )->people; people; people = people->next_in_room )
				{
					if ( !PRF_FLAGGED ( people, PRF_BATTLESPAM ) )
						continue;
					if ( people == victim )
						continue;
					if ( people == ch )
						continue;
					perform_act ( msg->miss_msg.room_msg.c_str(), ch, weap, victim, people );
				}
			}
			if ( ch != victim && HERE ( ch, victim ) && PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
			{
				ch->Send ( "%s", CCCYN ( ch, C_CMP ) );
				diag_char_to_char ( victim, ch );
				ch->Send ( "%s", CCNRM ( ch, C_CMP ) );
			}
			return ( 1 );
		}
	}
	return ( 0 );
}


void strike_missile ( Character *ch, Character *tch,
                      struct obj_data *missile, int dir, int attacktype )
{
	int dam;
	char buf[MAX_INPUT_LENGTH];

	dam = str_app[STRENGTH_APPLY_INDEX ( ch ) ].todam;
	dam += dice ( missile->obj_flags.value[1], missile->obj_flags.value[2] );
	dam += GET_DAMROLL ( ch );
	if ( AFF_FLAGGED ( tch, AFF_SHIELD ) )
		dam /= 2;

	if ( attacktype == SKILL_FIREARM )
	{
		snprintf ( buf, sizeof ( buf ), "You hit $N!" );
		act ( buf, FALSE, ch, missile, tch, TO_CHAR );
		snprintf ( buf, sizeof ( buf ), "A bullet flies in from the %s and strikes %s.",
			dirs[rev_dir[dir]], GET_NAME ( tch ) );
		act ( buf, FALSE, tch, NULL, NULL, TO_ROOM );
		snprintf ( buf, sizeof ( buf ), "A bullet flies in from the %s and hits YOU!",
			dirs[rev_dir[dir]] );
		act ( buf, FALSE, ch, NULL, tch, TO_VICT );
	}
	else
	{
		snprintf ( buf, sizeof ( buf ), "$p strikes $N!" );
		act ( buf, FALSE, ch, missile, tch, TO_CHAR );
		snprintf ( buf, sizeof ( buf ), "$p flies in from the %s and strikes %s.",
			dirs[rev_dir[dir]], GET_NAME ( tch ) );
		act ( buf, FALSE, tch, missile, NULL, TO_ROOM );
		snprintf ( buf, sizeof ( buf ), "$p flies in from the %s and hits YOU!",
			dirs[rev_dir[dir]] );
		act ( buf, FALSE, ch, missile, tch, TO_VICT );
	}
	if ( damage ( ch, tch, dam, attacktype ) != -1 )
		if ( IS_NPC ( tch ) && !IS_NPC ( ch ) && GET_POS ( tch ) > POS_STUNNED )
		{
			if ( tch->mob_specials.head_join )
				tch = tch->mob_specials.head_join;
			SET_BIT_AR ( MOB_FLAGS ( tch ), MOB_MEMORY );
			remember ( tch, ch );
			if ( CAN_HUNT ( ch ) )
			{
				HUNTING ( tch ) = ch;
				add_hunter ( tch );
			}
		}
	return;
}


void miss_missile ( Character *ch, Character *tch,
                    struct obj_data *missile, int dir, int attacktype )
{
	char buf[MAX_INPUT_LENGTH];
	if ( attacktype == SKILL_FIREARM )
	{
		snprintf ( buf, sizeof ( buf ),
		           "A bullet flies in from the %s and narrowly misses $N!",
		           dirs[rev_dir[dir]] );
		act ( buf, FALSE, tch, 0, tch, TO_ROOM );
		snprintf ( buf, sizeof ( buf ),
		           "A bullet flies in from the %s and narrowly misses you!",
		           dirs[rev_dir[dir]] );
		act ( buf, FALSE, ch, 0, tch, TO_VICT );
		ch->Send ( "You narrowly miss %s. BAH!", PERS ( tch, ch ) );
	}
	else
	{
		snprintf ( buf, sizeof ( buf ),
		           "A $p flies in from the %s and hits the ground!",
		           dirs[rev_dir[dir]] );
		act ( buf, FALSE, tch, missile, 0, TO_ROOM );
		act ( buf, FALSE, tch, missile, 0, TO_CHAR );
		snprintf ( buf, sizeof ( buf ), "Your $p narrowly misses $N. Bah!" );
		act ( buf, FALSE, ch, missile, tch, TO_CHAR );
	}
}


void mob_reaction ( Character *ch, Character *vict, int dir, bool shot )
{
	if ( IS_NPC ( vict ) && !FIGHTING ( vict ) && GET_POS ( vict ) > POS_STUNNED )
	{

		/* can remember so charge! */
		if ( IS_SET_AR ( MOB_FLAGS ( vict ), MOB_MEMORY ) )
		{
			remember ( vict, ch );
			if ( shot )
				act ( "$n bellows in pain!", FALSE, vict, 0, 0, TO_ROOM );
			if ( GET_POS ( vict ) == POS_STANDING &&
			        !IS_SET_AR ( MOB_FLAGS ( vict ), MOB_SENTINEL ) )
			{
				if ( !do_simple_move ( vict, rev_dir[dir], 1 ) )
					act ( "$n stumbles while trying to run!", FALSE, vict, 0,
					      0, TO_ROOM );
			}
			else
				GET_POS ( vict ) = POS_STANDING;

			/* can't remember so try to run away */
		}
		else
		{
			do_flee ( vict, ( char * ) "", 0, 0 );
		}
	}
}


void fire_missile ( Character *ch, char arg1[MAX_INPUT_LENGTH],
                    struct obj_data *missile, int pos, int range, int dir )
{
	bool shot = FALSE, found = FALSE;
	int attacktype;
	room_rnum room, nextroom;
	int distance;
	//int r_num;
	Character *vict;
	char buf[MAX_INPUT_LENGTH];



	if ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_PEACEFUL ) )
	{
		send_to_char
		( "This room just has such a peaceful, easy feeling...\r\n",
		  ch );
		return;
	}

	room = IN_ROOM ( ch );

	if ( CAN_GO2 ( room, dir ) )
		nextroom = EXIT2 ( room, dir )->to_room;
	else
		nextroom = NULL;




	if ( GET_OBJ_TYPE ( missile ) == ITEM_GRENADE )
	{
		send_to_char ( "You throw it!\r\n", ch );
		sprintf ( buf, "$n throws %s %s.", missile->short_description, dirs[dir] ); //maybe it should be missile
		act ( buf, FALSE, ch, 0, 0, TO_ROOM );
		send_to_room ( nextroom, "%s flies in from the %s.\r\n",
		               missile->short_description, dirs[rev_dir[dir]] );
		if ( GET_OBJ_TYPE ( missile ) != ITEM_GUN )
			obj_to_room ( unequip_char ( ch, pos ), nextroom );
		return;
	}

	for ( distance = 1; ( ( nextroom != NULL ) && ( distance <= range ) );distance++ )
	{

		for ( vict = nextroom->people; vict;
		        vict = vict->next_in_room )
		{
			if ( ( isname ( arg1, GET_NAME ( vict ) ) ) && ( CAN_SEE ( ch, vict ) ) )
			{
				found = TRUE;
				break;
			}
		}

		if ( found == 1 )
		{
			if ( MOB_FLAGGED ( vict, MOB_NOSHOOT ) )
			{
				ch->Send ( "You can't get a clear shot.\r\n" );
				return;
			}

			/* Daniel Houghton's missile modification */
			if ( missile && ROOM_FLAGGED ( vict->in_room, ROOM_PEACEFUL ) )
			{
				send_to_char ( "Nah.  Leave them in peace.\r\n", ch );
				return;
			}

			// can_fight needs to know it's a directional attack
			GET_SPELL_DIR ( ch ) = 1;

			if ( !can_fight ( ch, vict, FALSE ) )
			{
				ch->Send ( "You can't do that to them!\r\n" );
				GET_SPELL_DIR ( ch ) = NOWHERE;
				return;
			}
			GET_SPELL_DIR ( ch ) = NOWHERE;

			/*its a gun, has ammo inside it, create the item */
#if 0
			if ( GET_OBJ_TYPE ( missile ) == ITEM_GUN )
				if ( ! ( ( r_num = real_object ( 252 ) ) < 0 ) )
					missile = read_object ( r_num, REAL );
#endif

			switch ( GET_OBJ_TYPE ( missile ) )
			{
				case ITEM_THROW:
					send_to_char ( "You throw it!\r\n", ch );
					sprintf ( buf, "$n throws %s %s.",
					          missile->short_description,
					          dirs[dir] );
					attacktype = SKILL_THROW;
					break;
				case ITEM_ARROW:
					act ( "$n aims and fires!", TRUE, ch, 0, 0, TO_ROOM );
					send_to_char ( "You aim and fire!\r\n", ch );
					attacktype = SKILL_BOW;
					break;
				case ITEM_ROCK:
					act ( "$n aims and fires!", TRUE, ch, 0, 0, TO_ROOM );
					send_to_char ( "You aim and fire!\r\n", ch );
					attacktype = SKILL_SLING;
					break;
				case ITEM_BOLT:
					act ( "$n aims and fires!", TRUE, ch, 0, 0, TO_ROOM );
					send_to_char ( "You aim and fire!\r\n", ch );
					attacktype = SKILL_CROSSBOW;
					break;
				case ITEM_AMMO:
					act ( "$n aims and fires!", TRUE, ch, 0, 0, TO_ROOM );
					send_to_char ( "You aim and fire!\r\n", ch );
					attacktype = SKILL_FIREARM;
					break;
				default:
					attacktype = TYPE_UNDEFINED;
					break;
			}

			if ( attacktype != TYPE_UNDEFINED )
			{
				shot = ch->skill_roll ( attacktype );
				improve_skill ( ch, attacktype );
			}
			else
				shot = FALSE;

			if ( shot == TRUE && !MOB_FLAGGED ( vict, MOB_NOSHOOT ) )
			{
				strike_missile ( ch, vict, missile, dir, attacktype );
				if ( attacktype != SKILL_FIREARM )
					extract_obj ( unequip_char ( ch, pos ) );
				else // remove a bullet from the gun
					GET_EQ ( ch, WEAR_WIELD )->obj_flags.value[2]--;
			}
			else
			{
				/* ok missed so move missile into new room, unless it's a bullet */
				miss_missile ( ch, vict, missile, dir, attacktype );
				if ( attacktype != SKILL_FIREARM )
					obj_to_room ( unequip_char ( ch, pos ), IN_ROOM ( vict ) );
				else // remove a bullet from the gun
					GET_EQ ( ch, WEAR_WIELD )->obj_flags.value[2]--;
			}

			/* either way mob remembers */
			mob_reaction ( ch, vict, dir, shot );
			WAIT_STATE ( ch, PULSE_VIOLENCE );
			return;

		}

		room = nextroom;
		if ( CAN_GO2 ( room, dir ) )
			nextroom = EXIT2 ( room, dir )->to_room;
		else
			nextroom = NULL;
	}

	send_to_char ( "Can't find your target!\r\n", ch );
	return;

}


void tick_grenade ( void )
{
	struct obj_data *i, *tobj;
	Character *tch, *next_tch;
	int s, dam, door;
	room_rnum t;

	/* grenades are activated by pulling the pin - ie, setting the
	   one of the extra flag bits. After the pin is pulled the grenade
	   starts counting down. once it reaches zero, it explodes. */

	for ( olt_it ob = object_list.begin(); ob != object_list.end(); ob++ )
	{
		i = ( ob->second );
		if ( i != NULL )
		{
			if ( IS_SET_AR ( GET_OBJ_EXTRA ( i ), ITEM_LIVE_GRENADE ) )
			{
				/* update ticks */
				if ( i->obj_flags.value[0] > 0 )
					i->obj_flags.value[0] -= 1;
				else
				{
					t = 0;

					/* blow it up */
					/* checks to see if inside containers */
					/* to avoid possible infinite loop add a counter variable */
					s = 0;    /* we'll jump out after 5 containers deep and just delete the grenade */

					for ( tobj = i; tobj; tobj = tobj->in_obj )
					{
						s++;
						if ( tobj->in_room != NULL )
						{
							t = tobj->in_room;
							break;
						}
						else if ( ( tch = tobj->carried_by ) )
						{
							t = IN_ROOM ( tch );
							break;
						}
						else if ( ( tch = tobj->worn_by ) )
						{
							t = IN_ROOM ( tch );
							break;
						}
						if ( s == 5 )
							break;
					}

					/* then truly this grenade is nowhere?!? */
					if ( !VALID_ROOM_RNUM ( t ) )
					{
						log ( "serious problem, grenade truly in nowhere\r\n" );
						extract_obj ( i );
						i = NULL;
						return;
					}
					else      /* ok we have a room to blow up */
					{

						/* peaceful rooms */
						if ( ROOM_FLAGGED ( t, ROOM_PEACEFUL ) )
						{
							send_to_room ( t,
							               "You hear %s explode harmlessly, with a loud POP!\r\n",
							               i->short_description );

							extract_obj ( i );
							return;
						}

						dam =
						    dice ( i->obj_flags.value[1],
						           i->obj_flags.value[2] );

						send_to_room ( t,
						               "Oh no - %s explodes!  KABOOOOOOOOOM!!!\r\n",
						               i->short_description );
						for ( door = 0; door < NUM_OF_DIRS; door++ )
							if ( CAN_GO2 ( t, door ) )
								send_to_room ( t->dir_option[door]->to_room,
								               "You hear a loud explosion!\r\n" );

						for ( tch = t->people; tch; tch = next_tch )
						{
							next_tch = tch->next_in_room;

							if ( GET_POS ( tch ) <= POS_DEAD )
							{
								log ( "SYSERR: Attempt to damage a corpse." );
								return;  /* -je, 7/7/92 */
							}

							/* Only damage mobs, players in arena, or PK */
							if ( ( IS_NPC ( tch ) && ok_damage_shopkeeper ( NULL, tch ) ) ||
								ROOM_FLAGGED ( IN_ROOM( tch ), ROOM_ARENA ) ||
								( IS_PK ( tch ) && i->obj_flags.value[3] ) )	
							{
								act ( "$n is blasted!", TRUE, tch, 0, 0,
								      TO_ROOM );
								act ( "You are caught in the blast!", TRUE,
								      tch, 0, 0, TO_CHAR );
								if ( AFF_FLAGGED ( tch, AFF_SHIELD ) )
									damage ( tch, tch, dam / 2, TYPE_UNDEFINED );
								else
									damage ( tch, tch, dam, TYPE_UNDEFINED );
							}
							else tch->Send ( "You duck and manage to avoid the blast!\r\n" );

						}
						/* ok hit all the people now get rid of the grenade and
						   any container it might have been in */

						extract_obj ( i );
						return;
					}
				}      /* end else stmt that took care of explosions */
			}             /* end if stmt that took care of live grenades */
		}               /* end loop that searches the mud for objects. */
	}
	return;

}

int can_fight ( Character *ch, Character *vict, int silent )
{

	if ( !ch || !vict )
		return 0;
	if ( IN_ROOM ( ch ) == NULL || IN_ROOM ( vict ) == NULL )
		return 0;
	if ( DEAD ( ch ) || DEAD ( vict ) )
		return 0;
	if ( PLR_FLAGGED ( vict, PLR_DYING ) || PLR_FLAGGED ( ch, PLR_DYING ) )
		return 0;
	if ( !IS_NPC ( ch ) && !ch->desc )
	{
		return 0;
	}



	if ( ch->master && ch->master == vict )
	{
		if ( !silent )
			ch->Send ( "You can't fight your leader!\r\n" );
		return 0;
	}

	/* if (!(RIDDEN_BY(ch) || RIDDEN_BY(vict)) && (AFF_FLAGGED(ch, AFF_CHARM) || AFF_FLAGGED(vict, AFF_CHARM))) {
	     if (ch->master)
	         //new_send_to_char(ch->master, "Charmed mobs cant fight! Silly mortal\r\n");
	         return 0;
	 }*/
	/* Do some sanity checking, in case someone flees, etc. */
	if ( !HERE ( ch, vict ) && GET_SPELL_DIR ( ch ) == NOWHERE )
	{
		if ( FIGHTING ( ch ) && FIGHTING ( ch ) == vict )
			stop_fighting ( ch );
                return 0;
	}

	if ( !ok_damage_shopkeeper ( ch, vict ) )
	{
		stop_fighting ( ch );
                return 0;
	}
        /* Horus - fixed the mess that was here */
	if ( !CONFIG_PK_ALLOWED )
	{
                if (IS_NPC(ch) || IS_NPC(vict))
                  return 1;
                if (PLR_FLAGGED(vict, PLR_KILLER))
                  return 1;
                if (!arena_ok(ch, vict))
                  return 0;
	}

	return 1;

}

/* weapon balance protos */
int perf_balance ( int weapon_type )
{
	if ( weapon_type < 0 || weapon_type >= MAX_WEAPON_TYPES )
		return weapon_type_info[0].balance;

	return weapon_type_info[weapon_type].balance;
}

int curr_balance ( OBJ_DATA *wep )
{
	int bal = GET_WEP_BALANCE ( wep );

	if ( bal < 0 || bal > 100 )
		return 0;

	return bal;
}

int fuzzy_balance ( OBJ_DATA *wep )
{
	int num = ( number ( 0, 100 ) );
	int count, wl;
	int base = perf_balance ( GET_WEP_TYPE ( wep ) );

	if ( base < 0 || base > 100 )
	{
		log ( "Error: Fuzzy balance passed a base number out of range." );
		return 0;
	}

	wl  = ( GET_OBJ_WEIGHT ( wep ) /10 );
	wl += ( GET_WEP_LENGTH ( wep ) /25 );
	wl  = ( 13 - wl );
	for ( count = 0; count < wl; count++ )
	{
		if ( number ( 1, 5 ) == 1 )
			num = ( num + base ) /2;
	}
	return num;
}


int generate_wep_length ( OBJ_DATA *wep )
{

	switch ( ( int ) GET_WEP_TYPE ( wep ) )
	{
		default:
			return 50;
			break;
		case WEP_KNIFE:
			return 20;
		case WEP_DAGGER:
			return 35;
		case WEP_SHORTSWORD:
			return 60;
		case WEP_LONGSWORD:
			return 120;
		case WEP_KATANA:
			return 110;
		case WEP_RAPIER:
			return 80;
		case WEP_CUTLASS:
			return 70;
		case WEP_BROADSWORD:
			return 120;
		case WEP_HALFAXE:
			return 60;
		case WEP_AXE:
			return 75;
		case WEP_WARHAMMER:
			return 50;
		case WEP_MACE:
			return 50;
		case WEP_SHORTSTAFF:
			return 120;
		case WEP_STAFF:
			return 195;
		case WEP_WHIP:
			return 200;
		case WEP_CLUB:
			return 60;
		case WEP_TEETH:
			return 15;
		case WEP_CLAWS:
			return 25;
		case WEP_PROJECTILE:
			return 25;
	}

}

int gen_wep_type_from_attack ( OBJ_DATA *obj )
{

	switch ( ( int ) GET_OBJ_VAL ( obj, 3 ) + TYPE_HIT )
	{
		case TYPE_HIT:
			return WEP_CLUB;
		case TYPE_STING:
			return WEP_RAPIER;
		case TYPE_WHIP:
			return WEP_WHIP;
		case TYPE_SLASH:
			return WEP_SHORTSWORD;
		case TYPE_BITE:
			return WEP_TEETH;
		case TYPE_BLUDGEON:
			return WEP_WARHAMMER;
		case TYPE_CRUSH:
			return WEP_MACE;
		case TYPE_POUND:
			return WEP_SHORTSTAFF;
		case TYPE_CLAW:
			return WEP_CLAWS;
		case TYPE_MAUL:
			return WEP_HALFAXE;
		case TYPE_THRASH:
			return WEP_STAFF;
		case TYPE_PIERCE:
			return WEP_DAGGER;
		case TYPE_BLAST:
			return WEP_PROJECTILE;
		case TYPE_PUNCH:
			return WEP_CLAWS;
		case TYPE_STAB:
			return WEP_CUTLASS;
		case TYPE_KICK:
			return WEP_CLAWS;
		case TYPE_GORE:
			return WEP_AXE;
		default:
			return WEP_STANDARD;
	}
}

float diff_balance ( OBJ_DATA *wep )
{
	int curr = curr_balance ( wep );
	int type = GET_WEP_TYPE ( wep );
	int perf = perf_balance ( type );

	return ( float ) 1.0 - ( float ) ( ( ( abs ( ( perf - curr ) ) ) ) / ( float ) 100 );


}

int get_weapon_accuracy ( OBJ_DATA *wep )
{
	int top, bot, retval;
	if ( !wep )
		return 0;

	if ( GET_OBJ_TYPE ( wep ) != ITEM_WEAPON )
		return 0;

	top = weapon_type_info[GET_WEP_TYPE ( wep ) ].accuracytop;
	bot = weapon_type_info[GET_WEP_TYPE ( wep ) ].accuracybot;
	bot = abs ( bot );
	retval = ( top + bot );
	retval = ( int ) ( retval*diff_balance ( wep ) );
	retval -= bot;
	return retval;
}

int wep_hands ( OBJ_DATA *wep )
{
	if ( !wep )
		return 0;
	if ( GET_OBJ_TYPE ( wep ) != ITEM_WEAPON )
		return 0;
	return weapon_type_info[GET_WEP_TYPE ( wep ) ].hands;
}

int get_weapon_evasion ( OBJ_DATA *wep )
{
	int top, bot, retval;
	if ( !wep )
		return 0;

	if ( GET_OBJ_TYPE ( wep ) != ITEM_WEAPON )
		return 0;

	top = weapon_type_info[GET_WEP_TYPE ( wep ) ].evasiontop;
	bot = weapon_type_info[GET_WEP_TYPE ( wep ) ].evasionbot;
	bot = abs ( bot );
	retval = ( ( bot ) + top );
	retval = ( int ) ( ( retval*diff_balance ( wep ) ) );
	retval -= bot;
	return retval;
}

int get_weapon_speed ( OBJ_DATA *wep )
{
	int top, bot, retval;
	if ( !wep )
		return 0;

	if ( GET_OBJ_TYPE ( wep ) != ITEM_WEAPON )
		return 0;

	top = weapon_type_info[GET_WEP_TYPE ( wep ) ].speedtop;
	bot = weapon_type_info[GET_WEP_TYPE ( wep ) ].speedbot;
	bot = abs ( bot );
	retval = ( bot + top );
	retval = ( int ) ( ( retval*diff_balance ( wep ) ) );
	retval -= bot;
	return retval;
}

const char *weapon_type_name ( OBJ_DATA *wep )
{
	if ( GET_OBJ_TYPE ( wep ) == ITEM_WEAPON )
		return weapon_type_info[GET_WEP_TYPE ( wep ) ].name;

	return "non-weapon";
}

#define CHNAME(word)  (compares((word), name))
int generate_wep_type ( char *name )
{

	if ( *name == '\0' )
		return WEP_STANDARD;

	switch ( LOWER ( *name ) )
	{
		case 'a':
			if CHNAME ( "axe" )
				return WEP_AXE;
			break;
		case 'b':
			if CHNAME ( "broadsword" )
				return WEP_BROADSWORD;
			if CHNAME ( "branch" )
				return WEP_CLUB;
			break;
		case 'c':
			if CHNAME ( "claymore" )
				return WEP_BROADSWORD;
			if CHNAME ( "cutlass" )
				return WEP_CUTLASS;
			if CHNAME ( "club" )
				return WEP_CLUB;
			if CHNAME ( "claw" )
				return WEP_CLAWS;
			if CHNAME ( "claws" )
				return WEP_CLAWS;
			break;
		case 'd':
			if CHNAME ( "dagger" )
				return WEP_DAGGER;
			if CHNAME ( "dirk" )
				return WEP_DAGGER;
			break;
		case 'e':
			break;
		case 'f':
			if CHNAME ( "flintstone" )
				return WEP_HALFAXE;
			break;
		case 'g':
			if CHNAME ( "gun" )
				return WEP_PROJECTILE;
			break;
		case 'h':
			if CHNAME ( "halfaxe" )
				return WEP_HALFAXE;
			break;
		case 'i':
			break;
		case 'j':
			break;
		case 'k':
			if CHNAME ( "knife" )
				return WEP_KNIFE;
			if CHNAME ( "katana" )
				return WEP_KATANA;
			break;
		case 'l':
			if CHNAME ( "longsword" )
				return WEP_LONGSWORD;
			break;
		case 'm':
			if CHNAME ( "mace" )
				return WEP_MACE;
			break;
		case 'n':
			break;
		case 'o':
			break;
		case 'p':
			if CHNAME ( "post" )
				return WEP_CLUB;
			break;
		case 'q':
			break;
		case 'r':
			if CHNAME ( "rapier" )
				return WEP_RAPIER;
			break;
		case 's':
			if CHNAME ( "sword" )
				return WEP_SHORTSWORD;
			if CHNAME ( "shortsword" )
				return WEP_SHORTSWORD;
			if CHNAME ( "shortstaff" )
				return WEP_SHORTSTAFF;
			if CHNAME ( "staff" )
				return WEP_STAFF;
			break;
		case 't':
			if CHNAME ( "teeth" )
				return WEP_TEETH;
			break;
		case 'u':
			break;
		case 'v':
			break;
		case 'w':
			if CHNAME ( "warhammer" )
				return WEP_WARHAMMER;
			if CHNAME ( "war-hammer" )
				return WEP_WARHAMMER;
			if CHNAME ( "whip" )
				return WEP_WHIP;
			if CHNAME ( "wood" )
				return WEP_CLUB;
			break;
		case 'x':
			break;
		case 'y':
			break;
		case 'z':
			break;
		default:
			return WEP_STANDARD;
	}
	return WEP_STANDARD;
}

void combat_skill ( Character *ch )
{
	if ( !IS_SKILL ( GET_NEXT_SKILL ( ch ) ) && ( !IS_SPELL_CAST ( GET_NEXT_SKILL ( ch ) ) ) )
		return;
	switch ( GET_NEXT_SKILL ( ch ) )
	{
		case TYPE_UNDEFINED:
			return;
	}
}
#define HPLEFT(vict) ((GET_HIT(vict)*100)/GET_MAX_HIT(vict))
float skill_type_multi ( Character *ch, Character *vict, int type )
{
	//int m_user = FALSE, chclass = TYPE_UNDEFINED;
	//int chcha = 1,
	int tier = 1;
	int good = FALSE, evil = FALSE;
	float dam = 1.0;
	int inside, hot, cold, underwater, sky, raining, day, night, sunny, stormy;
	// Main Magic users line below.
	//m_user = ( IS_MAGE ( ch ) || IS_ESPER ( ch ) || IS_PRIEST ( ch ) || IS_NPC ( ch ) );
	//chclass = GET_CLASS ( ch );
	//chcha = GET_CHA ( ch );
	tier = current_class_is_tier_num ( ch );
	good = IS_GOOD ( ch );
	evil = IS_EVIL ( ch );
	sky = zone_table[GET_ROOM_ZONE ( IN_ROOM ( vict ) ) ].sky;
	day = IS_DAY;
	night = IS_NIGHT;
	inside = !OUTSIDE ( vict );
	hot = IS_HOT ( IN_ROOM ( vict ) );
	cold = IS_COLD ( IN_ROOM ( vict ) );
	underwater = IS_IN_WATER ( IN_ROOM ( vict ) );
	raining = ( sky == SKY_RAINING || sky == SKY_LIGHTNING );
	sunny = ( sky == SKY_CLOUDLESS && IS_DAY );
	stormy = sky == SKY_LIGHTNING;

	ch->Send("{cg(");
	switch ( type )
	{
		default:
			return 1.0;
			break;
			/* skills */
		case SKILL_BACKSTAB:
			return backstab_mult ( HPLEFT ( vict ), tier ) + ( total_chance ( ch, SKILL_BACKSTAB ) /100.0f );
		case SKILL_CLEAVE:
			return cleave_mult ( HPLEFT ( vict ), tier ) + ( total_chance ( ch, SKILL_CLEAVE ) /100.0f );
		case SKILL_BEHEAD:
			return cleave_mult ( HPLEFT ( vict ), tier ) + ( total_chance ( ch, SKILL_BEHEAD ) /100.0f );
		case SKILL_THRUST:
			return cleave_mult ( HPLEFT ( vict ), tier ) + ( total_chance ( ch, SKILL_THRUST ) /100.0f );
		case SKILL_SLIT:
			return slit_mult ( HPLEFT ( vict ), tier ) + ( total_chance ( ch, SKILL_SLIT ) / 100.0f );
		case SKILL_KICK:
		{
			float spd = GET_SPEED ( ch ) - GET_SPEED ( vict );
			if (spd < 0) {
				dam = 0.2;
			}
			else {
				spd *= 0.001;
				spd += 1.3;

				dam = spd;
			}
			return dam;
			break;
		}
		case SKILL_BASH:
			GET_POS ( vict ) = POS_SITTING;
			GET_WAIT_STATE ( vict ) += 2 RL_SEC;
			return 0.2;
			break;
			/* spells */
		case SPELL_MAGIC_MISSILE:
		{
			float spd = ( GET_MANA ( ch ) *100.0 ) / ( float ) GET_MAX_MANA ( ch );
			if ( spd < 0 )
				dam = 0.2;
			else
			{
				spd *= 0.01;
				spd += 0.50;
			}
			dam = spd;

		}
		break;
		case SPELL_CHILL_TOUCH:     /* chill touch also has an affect */
			if ( cold )
			{	dam = 1.5; ch->Send("+Cold "); }
			else if ( hot )
			{	dam = 0.5; ch->Send("-Hot "); }
			else
				dam = 1.2;
			
			if ( !inside && sunny )
			{	dam *= 0.5; ch->Send("-Sunny "); }
			break;
		case SPELL_BURNING_HANDS:
			if ( hot )
			{	dam = 1.5; ch->Send("+Hot "); }
			else if ( cold )
			{	dam = 0.5; ch->Send("-Cold "); }
			else
				dam = 1.25;
			
			if ( underwater )
			{	dam *= 0.01; ch->Send("-Underwater "); }
			else if ( ( !inside && raining ))
			{	dam *= 0.5; ch->Send("-Raining "); }
			break;
		case SPELL_COLOUR_SPRAY:
			if ( inside )
			{	dam = 1.4; ch->Send("+Inside "); }
			else if ( day )
			{	dam = 1.5; ch->Send("+Day "); }
			else if ( night )
			{	dam = 0.75; ch->Send("-Night "); }
			else
			{	dam = 1.5; ch->Send("+Twilight "); }
			break;
		case SPELL_FIREBALL:
			if ( inside )  
			{	ch->Send("+Inside ");  dam = 1.5; }
			else if ( hot )
			{ 	ch->Send("+Hot "); dam = 1.6; }
			else if ( cold )
			{	ch->Send("-Cold "); dam = 0.5; }
			else
				dam = 1.30;

			if ( day && !inside )
			{	ch->Send("+Day "); dam += 0.25; }
			else if ( night && !inside )
			{	ch->Send("-Night "); dam -= 0.5; }
			else if ( !inside )
			{	ch->Send("-Twilight "); dam -= 0.1; }


			if ( underwater )
			{	ch->Send("-Underwater "); dam *= 0.01; }
			else if ( raining && !inside )
			{	ch->Send("-Rain "); dam *= 0.5; }
			break;
		case SPELL_SOULSMASH:
			if ( !good )
			{
				if ( IS_NPC ( vict ) )
				{
					switch ( GET_CLASS ( vict ) )
					{
						case CLASS_UNDEAD:
							return 0.0;
							break;
						case CLASS_ANIMAL:
							dam = 1.6;
							ch->Send("+Animal ");
							break;
						default:
							dam = 1.1;
					}
				}
				else
					dam = 1.1;

				if ( evil )
				{	ch->Send("+Evil "); dam += 0.5; }

				if ( IS_EVIL ( vict ) )
				{	ch->Send("-VictEvil "); dam *= 0.5; }
				else if ( IS_GOOD ( vict ) )
				{	ch->Send("+VictGood "); dam += 0.8; }

				if ( GET_WIS ( vict ) >= 22 )
					{	ch->Send("-VictHighWis "); dam *= 0.5; }
				else if ( !( IS_NPC ( vict ) ) ) 
				{
					if ( GET_WIS ( vict ) <= 8 )
						{	ch->Send("+VictLowWis "); dam += 1.0; }
					else if ( GET_WIS ( vict ) <= 16 )
						{	ch->Send("+VictLowWis "); dam += 0.5; }
				}
			}
			else
				return 0.0;
			break;
		case SPELL_DEMONSHRIEK:
			if ( !good )
			{
				if ( IS_NPC ( vict ) )
				{
					switch ( GET_CLASS ( vict ) )
					{
						case CLASS_UNDEAD:
							return 0.0;
							break;
						default:
							dam = 1.6;
					}
				}
				else
					dam = 1.5;

				if ( evil )
				{	ch->Send("+Evil "); dam += 0.3; }

				if ( IS_EVIL ( vict ) )
				{	ch->Send("-VictEvil "); dam *= 0.5; }
				else if ( IS_GOOD ( vict ) )
				{	ch->Send("+VictGood "); dam += 0.5; }
			}
			else
				return 0.0;
			break;
		case SPELL_LIFESUCK:
			if ( !good )
			{
				if ( IS_NPC ( vict ) )
				{
					switch ( GET_CLASS ( vict ) )
					{
						case CLASS_UNDEAD:
							return 0.0;
							break;
						case CLASS_FIGHTER:
							dam = 1.5;
							ch->Send("+Fighter ");
							break;
						default:
							dam = 1.0;
					}
				}
				else
					dam = 1.0;

				if ( evil )
				{	ch->Send("+Evil "); dam += 0.5; }

				if ( IS_EVIL ( vict ) )
				{	ch->Send("-VictEvil "); dam *= 0.5; }
				else if ( IS_GOOD ( vict ) )
				{	ch->Send("+VictGood "); dam += 0.8; }
			}
			else
				return 0.0;
			break;
		case SPELL_BURNINGSKULL:
			if ( !good )
			{
				if ( IS_NPC ( vict ) )
				{
					switch ( GET_CLASS ( vict ) )
					{
						case CLASS_UNDEAD:
							return 0.0;
							break;
						case CLASS_CASTER:
							ch->Send("+Caster ");
							dam = 2.1;
							break;
						default:
							dam = 1.1;
					}
				}
				else if ( IS_MAGE ( vict ) || IS_ESPER ( vict ) || IS_PRIEST ( vict ) )
				{	ch->Send("+Caster "); dam = 2.1; }
				else
					dam = 1.1;

				if ( evil )
				{	ch->Send("+Evil "); dam += 0.5; }

				if ( IS_EVIL ( vict ) )
				{	ch->Send("-VictEvil "); dam *= 0.5; }
				else if ( IS_GOOD ( vict ) )
				{	ch->Send("+VictGood "); dam += 0.8; }

				if ( night && !inside)
				{	ch->Send("-Night "); dam *= 0.5; }

				if ( cold )
				{	ch->Send("-Cold "); dam *= 0.5; }

				if ( underwater )
				{	dam *= 0.01; ch->Send("-Underwater "); }
				else if ( !inside && raining )
				{	dam *= 0.5; ch->Send("-Rain "); }
			}
			else
				return 0.0;
			break;
		case SPELL_HEARTSQUEEZE:
			if ( !good )
			{
				if ( IS_NPC ( vict ) )
				{
					switch ( GET_CLASS ( vict ) )
					{
						case CLASS_UNDEAD:
							return 0.0;
							break;
						case CLASS_FIGHTER:
							ch->Send("+Fighter ");
							dam = 3.1;
							break;
						default:
							dam = 2.1;
					}
				}
				else
					dam = 2.1;

				dam *= 1.0 + ( 1.0 - ( ( float ) GET_STAMINA ( vict ) / ( float ) GET_MAX_STAMINA ( vict ) ) );

				if ( evil )
				{	ch->Send("+Evil "); dam += 0.5; }

				if ( IS_EVIL ( vict ) )
				{	ch->Send("-VictEvil "); dam *= 0.5; }
				else if ( IS_GOOD ( vict ) )
				{	ch->Send("+VictGood "); dam += 1.0; }

				if ( GET_CON ( vict ) >= 22 )
				{	ch->Send("-VictHighCon "); dam *= 0.5; }
				else if ( !( IS_NPC ( vict ) ) )
				{
					if ( GET_CON ( vict ) <= 8 )
						{	ch->Send("+VictLowCon "); dam += 1.0; }
					if ( GET_CON ( vict ) <= 16 )
						{	ch->Send("+VictLowCon "); dam += 0.5; }
				}
			}
			else
				return 0.0;
			break;
		case SPELL_FACEMELT:
			if ( !good )
			{
				if ( IS_NPC ( vict ) )
				{
					switch ( GET_CLASS ( vict ) )
					{
						case CLASS_UNDEAD:
							return 0.0;
							break;
						case CLASS_ROGUE:
							dam = 1.6;
							ch->Send("+Rogue ");
							break;
						default:
							dam = 1.1;
					}
				}
				else
					dam = 1.1;

				if ( affected_by_spell ( ch, SPELL_MIND_FIRE ))
				{
					if ( affected_by_spell ( ch, SPELL_PROT_COLD ) && !(affected_by_spell ( ch, SPELL_PROT_FIRE )) )
					{	dam += 1.0; ch->Send("+AntiCold "); }
				}
				else if ( affected_by_spell ( ch, SPELL_MIND_WATER ) || (affected_by_spell ( ch, SPELL_MIND_ICE )) )
				{	dam *= 0.5; ch->Send("-ElementClash "); }

				if ( hot )
				{	dam += 0.5; ch->Send("+Hot "); }
				else if ( cold )
				{	dam *= 0.5; ch->Send("-Cold "); }

				if ( evil )
				{	ch->Send("+Evil "); dam += 0.5; }

				if ( IS_EVIL ( vict ) )
				{	ch->Send("-VictEvil "); dam *= 0.5; }
				else if ( IS_GOOD ( vict ) )
				{	ch->Send("+VictGood "); dam += 0.8; }

				if ( night && !inside)
				{	ch->Send("-Night "); dam *= 0.5; }

				if ( underwater )
				{	dam *= 0.01; ch->Send("-Underwater "); }
				else if ( !inside && raining )
				{	dam *= 0.5; ch->Send("-Rain "); }

			}
			else
				return 0.0;
			break;
		case SPELL_DISPEL_EVIL:
			if ( evil || IS_GOOD ( vict ))
				return 0.0;
			
			if ( IS_EVIL ( vict ) )
			{	dam = 2.1; ch->Send("+VictEvil "); }
			else
				dam = 1.3;
			
			if ( good )
			{	
				dam += 0.5;
				ch->Send("+Good ");
				if ( IS_EVIL ( vict ) )
				{
					if ( affected_by_spell ( ch, SPELL_PROT_FROM_EVIL ) && !(affected_by_spell ( ch, SPELL_PROT_FROM_GOOD )) )
					{	dam += 1.0; ch->Send("+AntiEvil "); }
				}
			}
			
			if ( affected_by_spell ( vict, SPELL_PROT_FROM_GOOD ) )
			{	dam *= 0.25; ch->Send("-VictProtection "); }
			break;
		case SPELL_DISPEL_GOOD:
			if ( good || IS_EVIL ( vict ))
				return 0.0;
			
			if ( IS_GOOD ( vict ) )
			{	dam = 2.1; ch->Send("+VictGood "); }
			else
				dam = 1.3;

			if ( evil  )
			{	
				dam += 0.5;
				ch->Send("+EVIL ");
				if ( IS_GOOD ( vict ) )
				{
					if ( affected_by_spell ( ch, SPELL_PROT_FROM_GOOD ) && !(affected_by_spell ( ch, SPELL_PROT_FROM_EVIL )) )
					{	dam += 1.0; ch->Send("+AntiGood "); }
				}
			}

			if ( affected_by_spell ( vict, SPELL_PROT_FROM_EVIL ) )
			{	dam *= 0.25; ch->Send("-VictProtection "); }
			break;
		case SPELL_SHOCKING_GRASP:
			if ( sky == SKY_LIGHTNING && !inside )
			{	dam = 1.5; ch->Send("+Lightning "); }
			else if ( raining && !inside )
			{	dam = 1.2; ch->Send("+Rain "); }
			else if ( sky == SKY_CLOUDLESS && !inside )
			{	dam = 0.25; ch->Send("-Cloudless "); }
			else
				dam = 1.0;

			if ( underwater )
			{	dam += 0.25; ch->Send("+Underwater "); }
			break;
		case SPELL_LIGHTNING_BOLT:
			if ( sky == SKY_LIGHTNING && !inside )
			{	dam = 2.6; ch->Send("+Lightning "); }
			else if ( raining && !inside )
			{	dam = 2.2; ch->Send("+Rain "); }
			else if ( sky == SKY_CLOUDLESS && !inside )
			{	dam = 0.5; ch->Send("-Cloudless "); }
			else
				dam = 1.8;

			if ( underwater )
			{	dam += 0.8; ch->Send("+Underwater "); }
			break;
		case SPELL_ELECTRIC_BLAST:
			// Changed Damage from 0.8 to 1.95 since it is an 
			// t4 spell. Changed sky to higher as well 
			//Prometheus
			
			// I don't believe an area spell, even at higher tier
			// should have higher multi than a single-target spell.
			// I nerfed lightning bolt a bit as well, so, as a
			// result, electric blast is twice nerfed.
			// I think the nerf was necessary, seeing as the old
			// formula produced a max multi of 10.37.
			// Perhaps the timer could be reduced as a result.
			// -- Graham
			
			if ( sky == SKY_LIGHTNING && !inside )
			{	dam = 2.0; ch->Send("+Lightning "); }
			else if ( raining && !inside )
			{	dam = 1.6; ch->Send("+Rain "); }
			else if ( sky == SKY_CLOUDLESS && !inside )
			{	dam = 0.5; ch->Send("-Cloudless "); }
			else if ( inside)
			{	dam = 1.6; ch->Send("+Inside "); }
			else
				dam = 1.4;

			if ( underwater )
			{	dam += 0.5; ch->Send("+Underwater "); }
			break;
		case SPELL_CALL_LIGHTNING:
			// Tweaking damage of this spell
			// Base dam to 1.0 from 1.4 since this is a t2 spell
			// Lightning damage from 1.75 to 1.30 --> Prom
			
			// Just changed the format to match the previous spells
			if ( sky == SKY_LIGHTNING && !inside )
			{	dam = 1.3; ch->Send("+Lightning "); }
			else if ( raining && !inside )
			{	dam = 1.2; ch->Send("+Rain "); }
			else if ( sky == SKY_CLOUDLESS && !inside )
			{	dam = 0.5; ch->Send("-Cloudless "); }
			else if ( inside)
				return 0.0;
			else
				dam = 1.0;

			if ( underwater )
			{	dam += 0.1; ch->Send("+Underwater "); }
			break;
		case SPELL_CHAIN_LIGHTNING:
			// Dunno if chain lightning is supposed to have a damage multi
			// So lets just see if this works :|
			
			if ( sky == SKY_LIGHTNING && !inside )
			{	dam = 1.6; ch->Send("+Lightning "); }
			else if ( raining && !inside )
			{	dam = 1.4; ch->Send("+Rain "); }
			else if ( sky == SKY_CLOUDLESS && !inside )
			{	dam = 0.5; ch->Send("-Cloudless "); }
			else
				dam = 1.2;

			if ( underwater )
			{	dam += 0.4; ch->Send("+Underwater "); }
			break;
		case SPELL_HARM:
			if ( good )
				return 0.0;
			dam = 1.6;
			if ( evil )
			{	dam += 0.5; ch->Send("+Evil "); }

			if ( IS_GOOD ( vict ) )
			{	dam += 0.8; ch->Send("+VictGood "); }
			else if ( IS_EVIL ( vict ) )
			{	dam *= 0.5; ch->Send("-VictEvil "); }

			// A strong T1 spell otherwise now neutered by a relatively easily gotten spell. -- Graham
			if ( affected_by_spell ( vict, SPELL_SANCTUARY) )
			{	dam *= 0.25; ch->Send("-VictSanct "); }
			break;
		case SPELL_ACID_ARROW:
			if ( hot )
				return 0.0;
			if ( underwater )
			{	dam = 2.2; ch->Send("+Underwater "); }
			else if ( raining && !inside )
			{	dam = 1.8; ch->Send("+Rain "); }
			else
				dam = 1.5; 
			if ( cold )
			{       dam += 0.8; ch->Send("+Cold "); }
			break;
		case SPELL_FLAME_ARROW:
			if ( hot )
			{ 	ch->Send("+Hot "); dam = 2.2; }
			else if ( cold )
			{	ch->Send("-Cold "); dam = 0.2; }
			else
				dam = 1.6;

			if ( affected_by_spell ( ch, SPELL_MIND_FIRE ))
			{
				if ( affected_by_spell ( ch, SPELL_PROT_COLD ) && !(affected_by_spell ( ch, SPELL_PROT_FIRE )) )
				{	dam += 1.0; ch->Send("+AntiCold "); }
			}
			else if ( affected_by_spell ( ch, SPELL_MIND_WATER ) || (affected_by_spell ( ch, SPELL_MIND_ICE )) )
			{	dam *= 0.5; ch->Send("-ElementClash "); }

			if ( day && !inside )
			{	ch->Send("+Day "); dam += 0.4; }
			else if ( night && !inside )
			{	ch->Send("-Night "); dam -= 1.0; }
			else if ( !inside )
			{	ch->Send("-Twilight "); dam -= 0.1; }

			if ( underwater )
			{	ch->Send("-Underwater "); dam *= 0.01; }
			else if ( raining && !inside )
			{	ch->Send("-Rain "); dam *= 0.5; }

			if ( affected_by_spell ( vict, SPELL_PROT_FIRE) )
			{	dam *= 0.25; ch->Send("-VictProtection "); }
			break;
		case SPELL_CONE_OF_COLD:
			if ( inside )
			{
				return 0.0;
			if ( cold )
			{	dam = 1.5; ch->Send("+Cold "); }
			else if ( hot )
			{	dam = 0.5; ch->Send("-Hot "); }
			else
				dam = 1.25;
			
			if ( night && !inside )
			{	dam += 0.25;  ch->Send("+Night "); }
			else if ( day && !inside)
			{	dam *= 0.5;  ch->Send("-Day "); }
			else if ( !inside )
			{	dam += 0.1;  ch->Send("+Twilight "); }

			if ( raining && !inside )
			{	dam += 0.25; ch->Send("+Raining "); }
			}
			break;
		case SPELL_HOLY_SHOUT:
			if ( good )
			{	dam = 1.6;  ch->Send("+Good "); }
			else
				dam = 1.2;

			if ( IS_EVIL ( vict ) )
			{
				dam += 0.4;
				ch->Send("+VictEvil ");
				if ( number ( 0, 3) )
				GET_POS ( vict ) = POS_SITTING;
				update_pos ( vict );
			}

			if ( IS_NPC ( vict ) && ( GET_CLASS ( vict ) == CLASS_UNDEAD ) )
			{	dam += 0.6; ch->Send("+VictUndead "); }
			break;
		case SPELL_HOLY_WORD:
			if ( good )
			{	dam = 1.5; ch->Send("+Good "); }
			else
				dam = 1.0;

			if ( IS_EVIL ( vict ) )
			{
				dam += 0.25;
				ch->Send("+VictEvil ");
				if ( number ( 0, 2) )
				GET_POS ( vict ) = POS_SITTING;
				update_pos ( vict );
			}

			if ( IS_NPC ( vict ) && ( GET_CLASS ( vict ) == CLASS_UNDEAD ) )
			{	dam += 0.5; ch->Send("+VictUndead "); }
			break;
		case  SPELL_INFERNO:
			if ( SECT ( IN_ROOM ( vict )) == SECT_INSIDE )
			//if ( inside )
			{	dam = 1.6; ch->Send("+Inside "); }
			else if ( hot )
			{	dam = 1.8; ch->Send("+Hot "); }
			else if ( cold )
			{	dam = 0.1; ch->Send("-Cold "); }
			else
				dam = 1.2;

			if ( affected_by_spell ( ch, SPELL_MIND_FIRE ))
			{
				if ( affected_by_spell ( ch, SPELL_PROT_COLD ) && !(affected_by_spell ( ch, SPELL_PROT_FIRE )) )
				{	dam += 0.5; ch->Send("+AntiCold "); }
			}
			else if ( affected_by_spell ( ch, SPELL_MIND_WATER ) || (affected_by_spell ( ch, SPELL_MIND_ICE )) )
			{	dam *= 0.2; ch->Send("-ElementClash "); }

			if ( day && !inside )
			{	ch->Send("+Day "); dam += 0.2; }
			else if ( night && !inside )
			{	ch->Send("-Night "); dam *= 0.2; }
			else if ( !inside )
			{	ch->Send("-Twilight "); dam -= 0.1; }

			if ( underwater )
			{	ch->Send("-Underwater "); dam *= 0.01; }
			else if ( raining && !inside )
			{	ch->Send("-Rain "); dam *= 0.2; }

			if ( affected_by_spell ( vict, SPELL_PROT_FIRE) )
			{	dam *= 0.8; ch->Send("-VictProtection "); }
			break;
		case SPELL_MANA_BLAST:
			// Changing Mana Blast to hopefully
			// making it an area spell. Prometheus

			// Not sure what was changed but
			// "if ( SECT ( IN_ROOM ( vict )))" by itself
			// doesn't make sense.  Hopefully it does now. -- Graham
			if ( SECT ( IN_ROOM ( vict )) == SECT_INSIDE )
			{	dam = 1.5; ch->Send("+Inside "); }
			else
				dam = 1.0;
			dam *= ( ( float ) GET_MANA ( ch ) * 3.0 ) / ( float ) GET_MAX_MANA ( ch );
			break;
		case SPELL_EARTHQUAKE:
			switch ( SECT ( IN_ROOM ( vict ) ) )
			{
				case SECT_MOUNTAIN:
				{	dam = 1.3; ch->Send("+Mountains "); }
				case SECT_SNOW:
				case SECT_ICE:
				{	dam = 1.3; ch->Send("+Ice "); }
					break;
				case SECT_WATER_SWIM:
				case SECT_WATER_NOSWIM:
				case SECT_UNDERWATER:
				{	dam = 1.6; ch->Send("+Underwater "); }
					break;
				case SECT_FLYING:
				case SECT_SPACE:
				case SECT_ATMOSPHERE:
					return 0.0;
					break;
				case SECT_FOREST:
				{	dam = 0.3;  ch->Send("-Forest "); }
					break;
				default:
					dam = 0.6;
					break;
			}
			break;
		case SPELL_METEOR_SHOWER:
			switch ( SECT ( IN_ROOM ( vict ) ) )
			{
				case SECT_INSIDE:
					return 0.0;
					break;
				case SECT_CITY:
				{	dam = 2.0;  ch->Send("+City "); }
					break;
				case SECT_SPACE:
				case SECT_ATMOSPHERE:
				{	dam = 2.25;  ch->Send("+Outerspace "); }
					break;
				default:
					dam = 1.5;
					break;
			}
			break;
		case SPELL_ACID_HOLD:
			if ( hot )
				return 0.0;
                        if ( underwater )
                        {       dam = 1.6; ch->Send("+Underwater "); }
                        else if ( raining && !inside )
                        {       dam = 1.4; ch->Send("+Rain "); }
                        else
                                dam = 1.2;
			if ( cold )
			{       dam += 0.4; ch->Send("+Cold "); }
                        break;
		case SPELL_ACIDBURST:
			if ( hot )
				return 0.0;
			if ( underwater )
                        {       dam = 2.0; ch->Send("+Underwater "); }
                        else if ( raining && !inside )
                        {       dam = 1.6; ch->Send("+Rain "); }
                        else
                                dam = 1.3;
			if ( cold )
			{       dam += 0.6; ch->Send("+Cold "); }
                        break;
		case SPELL_FROST_ARROW:
			if ( cold )
			{ 	ch->Send("+Cold "); dam = 2.6; }
			else if ( hot )
			{	ch->Send("-Hot "); dam = 0.4; }
			else
				dam = 2.0;

			if ( affected_by_spell ( ch, SPELL_MIND_ICE ))
			{
				if ( affected_by_spell ( ch, SPELL_PROT_FIRE ) && !(affected_by_spell ( ch, SPELL_PROT_COLD )) )
				{	dam += 1.2; ch->Send("+AntiCold "); }
			}
			else if ( affected_by_spell ( ch, SPELL_MIND_FIRE ) )
			{	dam *= 0.5; ch->Send("-ElementClash "); }

			if ( night && !inside)
			{	ch->Send("+Night "); dam += 0.4; }
			else if ( day && !inside)
			{	ch->Send("-Day "); dam *= 0.5; }
			else if ( !inside)
			{	ch->Send("+Twilight "); dam += 0.2; }

			if ( sunny && !inside)
			{	ch->Send("-Cloudless "); dam *= 0.01; }
			else if ( stormy && !inside )
			{	ch->Send("+Stormy "); dam += 0.2; }

			if ( affected_by_spell ( vict, SPELL_PROT_FIRE) )
			{	dam *= 0.25; ch->Send("-VictProtection "); }
			break;
		case  SPELL_HAIL_STORM:
			if ( inside )
				return 0.0;
			else if ( stormy )
			{	dam = 4.0; ch->Send("+Stormy "); }
			else if ( hot )
			{	dam = 0.1; ch->Send("-Hot "); }
			else
				dam = 2.0;

			if ( affected_by_spell ( ch, SPELL_MIND_ICE ))
			{
				if ( affected_by_spell ( ch, SPELL_PROT_FIRE ) && !(affected_by_spell ( ch, SPELL_PROT_COLD )) )
				{	dam += 0.6; ch->Send("+AntiCold "); }
			}
			else if ( affected_by_spell ( ch, SPELL_MIND_FIRE ) )
			{	dam *= 0.5; ch->Send("-ElementClash "); }

			if ( night && !inside)
			{	ch->Send("+Night "); dam += 0.5; }
			else if ( day && !inside)
			{	ch->Send("-Day "); dam -= 0.5; }
			else if ( !inside)
			{	ch->Send("+Twilight "); dam += 0.6; }

			if ( sunny && !inside)
			{	ch->Send("-Cloudless "); dam *= 0.01; }
			else if ( cold && !inside )
			{	ch->Send("+Cold "); dam += 0.4; }

			if ( affected_by_spell ( vict, SPELL_PROT_FIRE) )
			{	dam *= 0.25; ch->Send("-VictProtection "); }
			break;
		case SPELL_FIRE_BREATH:
		case SPELL_GAS_BREATH:
		case SPELL_FROST_BREATH:
		case SPELL_ACID_BREATH:
		case SPELL_LIGHTNING_BREATH:
			dam = 2.0;
			break;

	}

      /* Moved mastery bonus (multiplicative) before mind bonuses (additive).  Increased mind bonus.
         This effectively buffs newbies, while players with mage and esper masteries are more or less
         unaffected.  Slightly nerfed actually.  -- Graham */

        if ( GET_MASTERY ( ch, CLASS_ESPER ) )
                dam *= 1.25;
        if ( GET_MASTERY ( ch, CLASS_MAGE ) )
                dam *= 1.20;

	if ( affected_by_spell ( ch, SPELL_DIVINE_MIND ) )
		dam += 0.20;
	
	// added a debuff, yay for espers -- Graham
	if ( affected_by_spell ( ch, SPELL_NUMB_MIND ) )
		dam -= 0.75;

	switch ( elemental_type ( type ) )
	{
		default:
			break;
		case ELEM_FIRE:
			if ( affected_by_spell ( ch, SPELL_MIND_FIRE ) )
			{
				if (affected_by_spell ( ch, SPELL_MUTATED))
				{
				dam += 0.75;
				ch->Send("+Mutated +MindFire ");
				}
				else
				{
				dam += 0.25;
				ch->Send("+MindFire ");
				}
			}
			else if (affected_by_spell ( ch, SPELL_MUTATED))
			{
			dam += 0.25;
			ch->Send("+Mutated ");
			}
			break;
		case ELEM_ICE:
			if ( affected_by_spell ( ch, SPELL_MIND_ICE ) )
			{	dam += 0.25; ch->Send("+MindIce "); }
			break;
		case ELEM_ELEC:
			if ( affected_by_spell ( ch, SPELL_MIND_ELEC ) )
			{
                                if (affected_by_spell ( ch, SPELL_MUTATED))
                                {
                                dam += 0.50;
                                ch->Send("+Mutated +MindElec ");
                                }
                                else
                                {
                                dam += 0.25;
                                ch->Send("+MindElec ");
                                }
			}
                        else if (affected_by_spell ( ch, SPELL_MUTATED))
                        {
                        dam += 0.25;
                        ch->Send("+Mutated ");
                        }
                        break;

		case ELEM_WATER:
			if ( affected_by_spell ( ch, SPELL_MIND_WATER ) )
			{	dam += 0.25;  ch->Send("+MindWater "); }
			break;
	}
	
	// Yay insanity check -- Graham
	if ( dam <= 0 )
		dam = 0.01;
	
	//I have no idea why the below was in, but it has been here since before
	//I started coding here. It is pretty nasty, since it allows a player to
	//spamcast an offensive spell at a mob, and the mob won't be able to fight
	//back, because its wait state isn't less than or equal to 0. --Thotter
	//
	// It was meant to be for the ch, not vict, oops! :-P - Mord
	GET_WAIT_STATE ( ch ) += 1 RL_SEC;
	return dam;

}

void kill_list ( Character *ch, Character *vict )
{
	if ( !ch || !vict || !IS_NPC ( vict ) )
		return;
	int vnum = GET_MOB_VNUM ( vict );
	if ( vnum == NOBODY )
		return;

	SPECIALS ( ch )->UpdateKill ( vnum );
}


int attack_type ( char chclass )
{
	switch ( chclass )
	{
		case CLASS_MAGE:
			return ATTACK_MAGIC;
			break;
		case CLASS_PRIEST:
			return ATTACK_MAGIC;
			break;
		case CLASS_ESPER:
			return ATTACK_MAGIC;
			break;
		case CLASS_GYPSY:
			return ATTACK_SKILL;
			break;
		case CLASS_THIEF:
			return ATTACK_SKILL;
			break;
		case CLASS_RANGER:
			return ATTACK_SKILL;
			break;
		case CLASS_HUNTER:
			return ATTACK_MELEE;
			break;
		case CLASS_WARRIOR:
			return ATTACK_MELEE;
			break;
		default:
			return 0;
			break;
	}
}

float ctl_elemental ( Character *ch, Character *elem )
{
	if ( !IS_NPC ( elem ) )
		return 1.0f;

	if ( MOB_FLAGGED ( elem, MOB_ELEM_EARTH ) ) {}
	else if ( MOB_FLAGGED ( elem, MOB_ELEM_FIRE ) ) {}
	else if ( MOB_FLAGGED ( elem, MOB_ELEM_AIR ) ) {}
	else if ( MOB_FLAGGED ( elem, MOB_ELEM_WATER ) ) {}
	return 1.0f;
}
