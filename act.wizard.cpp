

/* ************************************************************************
*   File: act.wizard.c                                  Part of CircleMUD *
*  Usage: Player-level god commands and other goodies                     *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */
/*
 * $Log: act.wizard.c,v $
 * Revision 1.82  2007/11/22 08:36:52  w4dimenscor
 * Added more info to the tsearch function
 *
 * Revision 1.81  2007/11/14 21:39:41  w4dimenscor
 * Added the Gladiator race for the gladiatorpits.
 * --Matthijs
 *
 * Revision 1.80  2007/11/14 20:01:14  w4dimenscor
 * removed some threading from the code
 *
 * Revision 1.79  2007/09/01 19:09:24  w4dimenscor
 * Changed error message if you try to talk on wiznet while having it turned off.
 *
 * Revision 1.78  2007/08/23 20:41:29  w4dimenscor
 * - Created a new MudException class, so that we can try and throw and catch errors.
 * - Fixed room description editing in OLC so that it works with the new system.
 * - Removed called to ident.c from the code
 * - changed the hostname values on descriptors and characters from char arrays to strings.
 *
 * Revision 1.77  2007/08/19 01:06:10  w4dimenscor
 * - Changed the playerindex to be a c++ object with search functions.
 * - changed the room descriptions to be searched from a MAP index, and
 * added Get and Set methods for room descriptions.
 * - changed the zone reset so that it doesnt search the entire object list
 * to find the object to PUT things into.
 * - rewrote other parts of the zone reset function, to make it give correct errors.
 * - rewrote the parts of the code to do with loading and searching for directorys and files.
 * - added a new dlib library.
 *
 * Revision 1.76  2007/06/26 10:48:05  w4dimenscor
 * Fixed context in scripts so that it works again, changed mounted combat so that it is about 2/3rds player one third mount damage, updated the way skills get read using total_chance, stopped things with a PERC of 0 assisting, made it so that the ungroup command disbanded charmies
 *
 * Revision 1.75  2007/06/14 23:55:39  w4dimenscor
 * Timers now work offline, keys can't be put in houses along with non-rent items. and the timers on items are run from the event system instead of 'ticks'
 *
 * Revision 1.74  2007/06/11 08:33:12  w4dimenscor
 * text_processed was accedentally moved to the wrong part of find_replacement
 *
 * Revision 1.73  2007/06/10 08:18:13  w4dimenscor
 * added new body parts CHEST and BACK
 *
 * Revision 1.72  2007/06/10 06:59:18  w4dimenscor
 * added the ability for scripts to toggle body parts on and off, and imms to do so too
 *
 * Revision 1.71  2007/06/10 02:18:39  w4dimenscor
 * changed all entries in the code of 'color' to 'colour', but i now regret it.
 *
 * Revision 1.70  2007/06/08 08:19:11  w4dimenscor
 * Added the ability to see where mobs and objects reset, removed cpp_extern, needs testing, and changed assemblies so that you can specify a vnum instead of a name
 *
 * Revision 1.69  2006/10/23 13:34:42  w4dimenscor
 * Changed the code so that level 55 immortals in the IMPL trust group can do all the imp commands too.
 *
 * Revision 1.68  2006/09/15 08:25:46  w4dimenscor
 * Fixed an error in act.wiz
 *
 * Revision 1.67  2006/09/15 08:01:11  w4dimenscor
 * Changed a large amount of send_to_char's to ch->Send and d->Output. fixed namechange command
 *
 * Revision 1.66  2006/08/31 10:39:16  w4dimenscor
 * Fixe dthe crash bug in medit. and also changed the mob proto list. there is still a memory leak in medit, which is being fixed now
 *
 * Revision 1.65  2006/08/27 02:29:12  w4dimenscor
 * added a command to reset everyones skills based on logon time
 *
 * Revision 1.64  2006/08/26 09:01:37  w4dimenscor
 * Added a command to list logons in the last x days
 *
 * Revision 1.63  2006/08/26 08:28:47  w4dimenscor
 * Fixed the saving of skills and subskills finally
 *
 * Revision 1.62  2006/08/25 10:49:30  w4dimenscor
 * added command to fix peoples skills back to the practiced amount they were at
 *
 * Revision 1.61  2006/08/25 10:22:43  w4dimenscor
 * added command to fix peoples skills back to the practiced amount they were at
 *
 * Revision 1.60  2006/08/23 09:01:26  w4dimenscor
 * Changed some of the std::vectors to std::map, killlist, and the lookup tables for id nums
 *
 * Revision 1.59  2006/08/20 12:17:47  w4dimenscor
 * fixed  an uninitialised vale, and and removed an unused value fromthe code
 *
 * Revision 1.58  2006/08/20 12:12:32  w4dimenscor
 * Changed the lookup table buckets to use sorted vectors. exciting. Also changed ignore list to use vectors, and fixed the valgrind error with the sort algorithm. Also sped up top gold command
 *
 * Revision 1.57  2006/08/19 00:09:36  w4dimenscor
 * found more issues with uninitialised values. Hopefully fixed them. gah
 *
 * Revision 1.56  2006/08/13 06:26:50  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.55  2006/06/21 09:28:58  w4dimenscor
 * Added the ability for Mortals of imms to listen to the wizchat. it is a
 * flag with the name wizmort, so set player wizmort on should do the
 * trick.
 *
 * Revision 1.54  2006/06/19 06:25:39  w4dimenscor
 * Changed the player saved mount feature so that all players can load mounts from houses
 *
 * Revision 1.53  2006/06/16 10:54:51  w4dimenscor
 * Moved several functions in fight.c into the Character object. Also removed all occurances of send_to_char from skills.c
 *
 * Revision 1.52  2006/06/06 10:50:42  w4dimenscor
 * Working on the Descriptor class, have removed the close_socked and new_descriptor functions in favor of constructors and destructors in the class
 *
 * Revision 1.51  2006/05/30 09:14:19  w4dimenscor
 * rewrote the colour code, process_output, and vwrite_to_output so that they use strings and have better buffer checks
 *
 * Revision 1.50  2006/05/22 10:50:48  w4dimenscor
 * Created 3 new files, mxp.cpp, mxp.h and descriptor.cpp
 * struct descriptor_data has been converted to class Descriptor
 *
 * Revision 1.49  2006/05/21 11:02:25  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.48  2006/05/09 15:01:08  w4dimenscor
 * Fixed stat crashbug. It would crash if an implementor statted themselves,
 * because of the last command thing.
 *
 * Revision 1.47  2006/05/01 11:29:26  w4dimenscor
 * I wrote a typo checker that automaticly corrects typos in the comm channels. I have also been fixing shadowed variables. There may be residual issues with it.
 *
 * Revision 1.46  2006/04/30 15:32:01  w4dimenscor
 * added the addtp command, so that senior imms can add and deduct points.
 *
 * Revision 1.45  2006/04/30 13:36:18  w4dimenscor
 * stat on an imm no longer shows UNDEFINED instead of imm3, trust all adds the imm3 trust group too now and vstat uses pages.
 *
 * Revision 1.44  2006/04/21 12:46:44  w4dimenscor
 * Fixed gcc 4.1 compile time errors. Game will now compile in GCC4
 *
 * Revision 1.43  2006/04/18 21:48:54  w4dimenscor
 * Added the "amount of fuel" property to gemclusters.
 * Added a fuel command.
 * Added a GET_GEM_FUEL(obj) macro to get the amount of fuel from a gemstone.
 * Made the imm3 trust group and moved stat, syslog users and vstat there.
 * Fixed scan so that it doesn't scan through closed doors and doesn't throw you
 * off our spacebike.
 * Fixed a typo in the slay command.
 *
 * Revision 1.42  2006/04/03 23:31:35  w4dimenscor
 * Added new commands called pclean, it removes the files of anyone who is not in the player index from the lib directory.
 *
 * Revision 1.41  2006/03/22 22:18:23  w4dimenscor
 * Socials now work with a number (lick 2.flag) and ctell snooping is now a toggle for imps (csnoop).
 *
 * Revision 1.40  2006/03/22 20:27:20  w4dimenscor
 * Changed all references to attack and defence and changed them to be accuracy and evasion, which more closely explains their role. Fixed up some errors in the defence roll part where the addition of dex to defence was backwards, lowering defence instead of adding to it the more dex you had (now called evasion).
 * Completed the autogroup toggle to work as expected (still untested though)
 * For your evasion rating, i added some more points based on level and tier.
 *
 * Revision 1.39  2006/02/17 22:19:54  w4dimenscor
 * Fixed error for ubuntu that doesnt like empty array declarations, moved ice shield to a better place and fixed its messages, added auto auction fixes, allowed mounts to gain exp properly
 *
 * Revision 1.38  2005/11/30 18:47:12  w4dimenscor
 * changed slightly some gains you get from remorts
 *
 * Revision 1.37  2005/10/30 08:37:05  w4dimenscor
 * Updated compare command and fixed mining
 *
 * Revision 1.36  2005/10/23 05:21:46  w4dimenscor
 * Altered assemblies, and fixed a few mem leaks
 *
 * Revision 1.35  2005/10/09 01:54:08  w4dimenscor
 * Fixed the trigger type otrig-speech so it works as expected, make id num's save to the player index, fixed the trigger types
 *
 * Revision 1.34  2005/09/24 08:52:33  w4dimenscor
 * finished the assemblies code
 *
 * Revision 1.33  2005/09/16 10:20:10  w4dimenscor
 * Added a snippet for making the obj and mob list hashed for fast lookups, i fixed a bug in the mccp and mxp protocols, added to objects the ability to remember who has ID'd them before so that when that person examines the item, they 'remember' what the stats are
 *
 * Revision 1.32  2005/08/28 10:00:53  w4dimenscor
 * added RPL flag, RPL note group
 *
 * Revision 1.31  2005/08/28 02:59:21  w4dimenscor
 * fixed a memory leak situation that could appear with overlapping rooms, adjusted the ignore save to not save certain words, adjusted the mobs to it a little softer
 *
 * Revision 1.30  2005/08/07 04:12:39  w4dimenscor
 * Manu changes and command have been made, sorry for the lack of description. Main changes include command landscape, fixes to helpfile stuff, subskill fixes
 *
 * Revision 1.29  2005/06/26 04:37:00  w4dimenscor
 * Changed pose, possibly fixed namechange (untested), fixed up some help adding stuff
 *
 * Revision 1.28  2005/06/21 08:53:40  w4dimenscor
 * added in better help finder and help editor, a la mordecai
 *
 * Revision 1.27  2005/06/18 12:20:52  w4dimenscor
 * changed a bunch of send_to_char's to new_send_to_chars, adjusted some mxp code
 *
 * Revision 1.26  2005/06/14 11:38:48  w4dimenscor
 * fixed pose
 *
 * Revision 1.25  2005/06/14 11:29:36  w4dimenscor
 * new command added: pose
 *
 * Revision 1.24  2005/06/02 09:52:49  w4dimenscor
 * fixed function namechange'
 *
 * Revision 1.23  2005/06/02 08:21:03  w4dimenscor
 * update to save file
 *
 * Revision 1.22  2005/05/28 05:52:14  w4dimenscor
 * Fixed some errors in copyover, added MXP
 *
 * Revision 1.21  2005/05/03 10:21:25  w4dimenscor
 * changed the free_string function to take a pointer to a pointer so it can nullk the string off properly now. Also, fixed a door loading error, that assumed that all door rooms existed when loading, and now it checks for existstance. Also, fixed the multi arg for 'get' command
 *
 * Revision 1.20  2005/05/01 11:42:12  w4dimenscor
 * started a change in the server so that multiple arguments can be used when referencing items: have done this for locate object, look, goto, at and a few other things, havent done it for: get, put, drink, wear and a few others
 *
 * Revision 1.19  2005/03/16 18:47:03  w4dimenscor
 * updated some spacing and formatting
 *
 * Revision 1.18  2005/03/16 16:44:08  w4dimenscor
 * Added Award Points to stat display
 *
 * Revision 1.17  2005/03/16 14:20:14  w4dimenscor
 * added / changed:
 * set [char] mastery [class] : toggles the mastery of that class
 * set [char] remort  : removes the first remorted class and decreases the # of remorts
 * set [char] rtwo  : removes the second remorted class and decreases the # of remorts
 * set [char] rthree  : removes the third remorted class and decreases the # of remorts
 * stat [char] : added display of # of remorts and mastered classes
 *
 * Revision 1.16  2005/03/15 09:55:49  w4dimenscor
 * fixed error with mtransform and linked mobs
 *
 * Revision 1.15  2005/03/15 08:35:09  w4dimenscor
 * xml page update, and a few other bits
 *
 * Revision 1.14  2005/02/26 01:21:34  w4dimenscor
 * Changed more of the code to be more buffer safe using strlcpy and strlcat
 *
 * Revision 1.13  2005/02/25 07:33:47  w4dimenscor
 * reformatted some code, fixed up coventry to ignore socials
 *
 * Revision 1.12  2005/02/25 05:02:45  w4dimenscor
 * added new commands and a few little changes - i forget what eek
 *
 * Revision 1.11  2005/02/20 01:18:11  w4dimenscor
 * added extra check to add to lookup table, and cleaned up freeing of characters and objects
 *
 * Revision 1.10  2005/02/16 13:06:13  w4dimenscor
 * fixed update_award and update_reward to give immortals initial 30 points
 * to hand out. otherwise immortals can't award points without at least being
 * once a hero and giving award points to initialize the special player fields.
 *
 * Revision 1.9  2005/02/09 09:23:43  w4dimenscor
 * added new code for using olc to create new mine shafts, and cleaned up the tsearch command, fixed a bug where there is no description in the log if the game crashes because a zone file is wanting to remove  a item from a room using zedit, but the room doesnt exist, and fixed an exp bug in flee
 *
 * Revision 1.8  2005/02/05 05:26:17  w4dimenscor
 * Added tsearch command to full text search triggers
 *
 * Revision 1.7  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.6  2004/12/17 07:13:20  w4dimenscor
 * A few little updates.
 *
 * Revision 1.5  2004/12/05 09:46:52  w4dimenscor
 * fixed mtransform, fixed format in clan tell, and added limit on magic items carried, and lowered weight of magic items, and increased cost
 *
 * Revision 1.4  2004/12/04 07:42:36  w4dimenscor
 * fixed the locker bug, and the format error in clan tells, and a few other cleanups
 *
 * Revision 1.3  2004/11/20 02:33:25  w4dimenscor
 * updated and cleaned up the script system
 *
 * Revision 1.2  2004/11/17 05:13:05  w4dimenscor
 * updated pets so that they don't have weight problems, updated award points
 *
 * Revision 1.1.1.1  2004/11/12 02:15:40  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.62  2004/09/18 10:47:02  molly
 * fixed up more memory errors, added normal help back in.
 *
 * Revision 1.60  2004/09/14 10:09:43  molly
 * added better optimisations
 *
 * Revision 1.58  2004/08/31 10:06:55  molly
 * make speed bonus from mounts only when you know mounted combat, changed max multi of magicmissile from 1.6 to 1.7, change who layout, fix error with room editing
 *
 * Revision 1.55  2004/08/22 00:50:47  molly
 * removed all the origional help code, added the start of the xml reader.
 *
 * Revision 1.54  2004/08/15 01:12:24  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */

#include <arpa/telnet.h>

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "house.h"
#include "screen.h"
#include "constants.h"
#include "dg_scripts.h"
#include "arena.h"
#include "clan.h"
#include "oasis.h"
#include "assemblies.h"
#include "fight.h"
#include "descriptor.h"
#include "strutil.h"
//#include "dlib/threads.h"
#include "compressor.h"
#include "shop.h"

/*   external vars  */
extern int TEMP_LOAD_CHAR;
extern struct time_data time_info;
extern struct attack_hit_type attack_hit_text[];
extern const char *race_abbrevs[];
extern time_t boot_time;
extern int circle_shutdown, circle_reboot;
extern int circle_restrict;
extern int buf_switches, buf_largecount, buf_overflows;
extern const char *save_info_msg[];     /* In olc.c */
extern int forget_num_obj;
extern const char *wiz_groups[];
extern socket_t mother_desc;
extern ush_int port;
extern map<mob_vnum, Character *> mob_proto;
extern struct obj_data *obj_proto;
extern map < room_vnum, plrshop* > player_shop;

/* for chars */
extern char *credits;
extern char *news;
extern char *motd;
extern char *imotd;
extern char *help;
extern char *info;
extern char *background;
extern char *policies;
extern char *startup;
extern char *handbook;
extern const char *pc_class_types[];
extern const char *pc_race_types[];


/* extern functions */
SPECIAL ( playershop );
bool load_playershop_shopkeep ( room_vnum r, Character **shopkeep = NULL );
void save_player_shop (string owner );
void parse_train_group(Character *ch, Character *vict,char *val_arg);
int togglebody ( Character *ch, int flag );
int has_body ( Character *ch, int flag );
int bodypartname ( char *bpn );
char *one_arg ( char *arg, char *first_arg );
void weight_to_object ( struct obj_data *obj, int weight );
char *getline ( char *str, char *buf, size_t len );
void write_aliases ( Character *ch );
void do_show_corpses ( Character *ch );
void do_show_errors ( Character *ch );
void do_show_trainers ( Character *ch );
int get_weapon_speed ( OBJ_DATA *wep );
int get_weapon_accuracy ( OBJ_DATA *wep );
int get_weapon_evasion ( OBJ_DATA *wep );
const char *material_name ( int type );
int find_month ( void );
void show_shops ( Character *ch, char *value );
void hcontrol_list_houses ( Character *ch );
void do_start ( Character *ch );
void appear ( Character *ch );
void reset_zone ( zone_rnum zone );
void roll_real_abils ( Character *ch );
int parse_class ( char arg );
int parse_race ( char* arg, bool consider_gladiator );
void write_poofs ( Character *ch );
void save_corpses ( void );
void check_autowiz ( Character *ch );
int zone_number ( void *what, int type );
int can_edit_zone ( Character *ch, int number );
int real_zone ( int number );
long long gold_data ( int type, long long amount );
void olc_list_flags ( Character *ch, const char *apply_stuff[] );

const char * race_name ( Character *ch );
const char *simple_class_name ( Character *ch );
int show_vars = FALSE;
int save_all ( void );
void print_zone ( Character *ch, zone_vnum vnum );
void print_object_location ( int num, struct obj_data *obj, Character *ch, int recur, char *buffer );
zone_rnum real_zone_by_thing ( room_vnum vznum ); /* added for zone_checker */
SPECIAL ( shop_keeper );

int check_potion_weight ( struct obj_data *obj );
int check_potion_price ( struct obj_data *obj );
void write_ignorelist ( Character *ch );
void Crash_rentsave ( Character *ch, int cost );
void read_ignorelist ( Character *ch );
ACMD ( do_gen_ps );
obj_data* find_corpse(Character* ch);
int automeld ( OBJ_DATA *corpse );
void restore_all_corpses();
/* local functions */
void add_to_comm(const char *type, const char *text);
int spell_price ( struct obj_data *obj, int val );
void show_door_errors ( Character *ch );
C_FUNC ( delete_player );
C_FUNC(allow_snoop);
void perform_delete_player ( const char *charname );
const char *balance_display ( int balance );
ACMD ( do_potionweight );
int perform_set ( Character *ch, Character *vict, int mode,
                  char *val_arg );
void perform_immort_invis ( Character *ch, int level );
void do_connections ( Character *ch, char *arg );
ACMD ( do_echo );
ACMD ( do_send );
room_rnum find_target_room ( Character *ch, char *rawroomstr );
ACMD ( do_at );
ACMD ( do_goto );
ACMD ( do_trans );
ACMD ( do_teleport );
ACMD ( do_vnum );
void do_stat_room ( Character *ch );
void do_stat_object ( Character *ch, struct obj_data *j );
void do_stat_character ( Character *ch, Character *k );
void list_destinations ( struct travel_point_data *travel_list, Character *ch );
ACMD ( do_stat );
ACMD ( do_shutdown );
void stop_snooping ( Character *ch );
ACMD ( do_snoop );
ACMD ( do_switch );
ACMD ( do_return );
ACMD ( do_load );
ACMD ( do_vstat );
ACMD ( do_purge );
ACMD ( do_syslog );
ACMD ( do_advance );
ACMD ( do_restore );
int perform_immort_vis ( Character *ch );
ACMD ( do_invis );
ACMD ( do_gecho );
ACMD ( do_poofset );
ACMD ( do_dc );
ACMD ( do_wizlock );
ACMD ( do_date );
ACMD ( do_last );
ACMD ( do_force );
ACMD ( do_wiznet );
ACMD ( do_zreset );
ACMD ( do_wizutil );
size_t print_zone_to_buf ( char *bufptr, size_t left, zone_rnum zone, int listall );
ACMD ( do_show );
ACMD ( do_set );
ACMD ( do_peace );
void clearMemory ( Character *ch );
ACMD ( do_autowiz );
ACMD ( do_statinnate );
int mortal_player_info ( Character *ch,Character *vict );
void list_mob_resets ( Character *vict, Character *ch );
void list_obj_resets ( obj_data *obj, Character *ch );
int update_award ( Character *ch );
int update_reward ( Character *ch );
void snoop_check ( Character *ch );
ACMD ( do_saveall );
ACMD ( do_ps_aux );
ACMD ( do_deleteplayer );
ACMD ( do_search_triggers );
ACMD ( do_ctellsnoop );
ACMD ( do_addtp );
ACMD ( do_wizsplit );

#define PC   1
#define NPC 2
#define BOTH 3

#define MISC   0
#define BINARY 1
#define NUMBER 2

#define SET_OR_REMOVE(flagset, flags) { \
    if (on) SET_BIT_AR(flagset, flags); \
    else if (off) REMOVE_BIT_AR(flagset, flags); }

#define SET_OR_REMOVE_TRUST(flagset, flags) { \
    if (on) SET_BIT(flagset, flags); \
    else if (off) REMOVE_BIT(flagset, flags); }

#define SET_OR_REMOVE_AR(flagset, flags) { \
    if (on) SET_BIT_AR(flagset, flags); \
    else if (off) REMOVE_BIT_AR(flagset, flags); }

#define RANGE(low, high) (value = MAX((low), MIN((high), (value))))

/* mccp defines */
const char *balances[] =
{
	"0========================",
	"=0=======================",
	"==0======================",
	"===0=====================",
	"====0====================",
	"=====0===================",
	"======0==================",
	"=======0=================",
	"========0================",
	"=========0===============",
	"==========0==============",
	"===========0=============",
	"============0============",
	"=============0===========",
	"==============0==========",
	"===============0=========",
	"================0========",
	"=================0=======",
	"==================0======",
	"===================0=====",
	"====================0====",
	"=====================0===",
	"======================0==",
	"=======================0=",
	"========================0"
};

struct trust_struct
{
	char *cmd;
	char level;
	char pcnpc;
	char type;
}
trust_fields[] =
{
	{
		( char * ) "ban", LVL_IMPL, PC, BINARY},  /*  0 */
	{
		( char * ) "dspln", LVL_IMPL, PC, BINARY},   /*  1 */
	{
		( char * ) "edit", LVL_IMPL, PC, BINARY}, /*  2 */
	{
		( char * ) "heal", LVL_IMPL, PC, BINARY}, /*  3 */
	{
		( char * ) "house", LVL_IMPL, PC, BINARY},   /*  4 */
	{
		( char * ) "imm1", LVL_IMPL, PC, BINARY}, /*  5 */
	{
		( char * ) "imm2", LVL_IMPL, PC, BINARY}, /*  6 */
	{
		( char * ) "impl", LVL_IMPL, PC, BINARY}, /*  7 */
	{
		( char * ) "kill", LVL_IMPL, PC, BINARY}, /*  8 */
	{
		( char * ) "load", LVL_IMPL, PC, BINARY}, /*  9 */
	{
		( char * ) "olc", LVL_IMPL, PC, BINARY},  /* 10 */
	{
		( char * ) "quest", LVL_IMPL, PC, BINARY},   /* 11 */
	{
		( char * ) "sen", LVL_IMPL, PC, BINARY},  /* 12 */
	{
		( char * ) "tele", LVL_IMPL, PC, BINARY}, /* 13 */
	{
		( char * ) "trig", LVL_IMPL, PC, BINARY}, /* 14 */
	{
		( char * ) "all", LVL_IMPL, PC, BINARY},  /* 15 */
	{
		( char * ) "marry", LVL_IMPL, PC, BINARY},   /* 16 */
	{
		( char * ) "goto", LVL_IMPL, PC, BINARY}, /* 17 */
	{
		( char * ) "global", LVL_IMPL, PC, BINARY},  /* 18 */
	{
		( char * ) "hedit", LVL_IMPL, PC, BINARY},   /* 19 */
	{
		( char * ) "imm3", LVL_IMPL, PC, BINARY},  /* 20 */
	{
		( char * ) "\n", 0, PC, MISC}
};


int perform_trust ( Character *ch, Character *vict, int mode,
                    char *toggle, char *value )
{
	int on = 0, off = 0;
	char buf[MAX_STRING_LENGTH];
	int save = 0, nosave = 0;

	/* Find the value of the argument */
	if ( trust_fields[mode].type == BINARY )
	{

		if ( !strcmp ( toggle, "on" ) || !strcmp ( toggle, "yes" ) )
			on = 1;
		else if ( !strcmp ( toggle, "off" ) || !strcmp ( toggle, "no" ) )
			off = 1;

		if ( ! ( on || off ) )
		{
			send_to_char ( "Toggle must be 'on' or 'off'.\r\n", ch );
			return 0;
		}

		if ( !strcmp ( value, "save" ) )
			save = 1;
		else if ( !strcmp ( value, "nosave" ) )
			nosave = 1;

		if ( ! ( save || nosave ) )
		{
			send_to_char ( "Value must be 'save' or 'nosave'.\r\n", ch );
			return 0;
		}

		snprintf ( buf, sizeof ( buf ), "(GC) %s %s for %s by %s (%s in file).",
		           trust_fields[mode].cmd,
		           ONOFF ( on ), GET_NAME ( vict ), GET_NAME ( ch ), value );

	}

	switch ( mode )
	{
		case 0:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_BAN_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_BAN_GRP )
					break;
		case 1:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_DSPLN_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_DSPLN_GRP )
					break;
		case 2:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_EDIT_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_EDIT_GRP )
					break;
		case 3:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_HEAL_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_HEAL_GRP )
					break;
		case 4:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_HOUSE_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_HOUSE_GRP )
					break;
		case 5:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_IMM1_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_IMM1_GRP )
					break;
		case 6:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_IMM2_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_IMM2_GRP )
					break;
		case 7:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_IMPL_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_IMPL_GRP )
					break;
		case 8:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_KILL_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_KILL_GRP )
					break;
		case 9:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_LOAD_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_LOAD_GRP )
					break;
		case 10:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_OLC_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_OLC_GRP )
					break;
		case 11:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_QUEST_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_QUEST_GRP )
					break;
		case 12:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_SEN_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_SEN_GRP )
					break;
		case 13:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_TELE_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_TELE_GRP )
					break;
		case 14:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_TRIG_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_TRIG_GRP )
					break;
		case 15:
			if ( save == 1 )
			{
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_BAN_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_DSPLN_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_EDIT_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_HEAL_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_HOUSE_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_IMM1_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_IMM2_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_IMM3_GRP )
				//SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_IMPL_GRP)
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_KILL_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_LOAD_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_OLC_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_QUEST_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_SEN_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_TELE_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_TRIG_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_MARRY_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_GOTO_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_GLOBAL_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_HEDIT_GRP )
			}
			else
			{
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_BAN_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_DSPLN_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_EDIT_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_HEAL_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_HOUSE_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_IMM1_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_IMM2_GRP )
				//SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_IMPL_GRP)
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_IMM3_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_KILL_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_LOAD_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_OLC_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_QUEST_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_SEN_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_TELE_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_TRIG_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_MARRY_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_GOTO_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_GLOBAL_GRP )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_HEDIT_GRP )
			}
			break;
		case 16:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_MARRY_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_MARRY_GRP )
					break;
		case 17:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_GOTO_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_GOTO_GRP )
					break;
		case 18:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_GLOBAL_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_GLOBAL_GRP )
					break;
		case 19:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_HEDIT_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_HEDIT_GRP )
					break;
		case 20:
			if ( save == 1 )
				SET_OR_REMOVE_TRUST ( CMD_FLAGS ( vict ), WIZ_IMM3_GRP )
				else
					SET_OR_REMOVE_TRUST ( CMD_FLAGS2 ( vict ), WIZ_IMM3_GRP )
					break;
		default:
			send_to_char ( "That isn't a trust group!\r\n", ch );
			return 0;
			break;
	}
	new_mudlog ( NRM, GET_LEVEL ( ch ), TRUE, "%s", buf );
	return 1;
}

ACMD ( do_trust )
{
	char group[MAX_INPUT_LENGTH], name[MAX_INPUT_LENGTH],
	toggle[MAX_INPUT_LENGTH], value[MAX_INPUT_LENGTH];
	char buf[MAX_INPUT_LENGTH];

	Character *vict;
	int mode = 0;
	int len = 0;
	int retval = 0;

	half_chop ( argument, name, buf );
	half_chop ( buf, group, buf );
	half_chop ( buf, toggle, buf );
	strcpy ( value, buf );

	if ( !*name || !*group || !*toggle || !*value )
	{
		send_to_char
		( "Usage: trust <victim> <group> <on/off> <save/nosave>\r\n",
		  ch );
		send_to_char ( "Valid trust groups:\r\n", ch );
		send_to_char
		( "ban, dspln, edit, heal, house, imm1, imm2, imm3, impl, kill\r\nload, marry, olc, quest, sen, tele, trig, goto, global, hedit, all\r\n",
		  ch );
		return;
	}

	if ( ! ( vict = get_player_vis ( ch, name, NULL, 0 ) ) )
	{
		send_to_char ( "There is no such player.\r\n", ch );
		return;
	}

	if ( IS_NPC ( vict ) )
	{
		send_to_char ( "Trust is for Kids! Silly rabbit!\r\n", ch );
		return;
	}

	if ( ( vict == ch ) && ( GET_LEVEL ( ch ) != LVL_IMPL ) )
	{
		send_to_char ( "You can't trust yourself!\r\n", ch );
		return;
	}


	/* Check to make sure all the levels are correct */
	if ( ( GET_LEVEL ( ch ) <= GET_LEVEL ( vict ) ) && ( GET_LEVEL ( ch ) != LVL_IMPL ) )
	{
		send_to_char ( "Maybe that's not such a great idea...\r\n", ch );
		return;
	}

	if ( GET_LEVEL ( vict ) < LVL_GOD && !PLR_FLAGGED ( vict, PLR_HERO ) )
	{
		send_to_char ( "You can't trust mortals! hehe..\r\n", ch );
		return;
	}

	/* find the command in the list */
	len = strlen ( group );
	for ( mode = 0; * ( trust_fields[mode].cmd ) != '\n'; mode++ )
		if ( !strncmp ( group, trust_fields[mode].cmd, len ) )
			break;
	/* perform the godset */
	retval = perform_trust ( ch, vict, mode, toggle, value );

	/* save the character if a change was made */
	if ( retval )
	{
		vict->save();
		// log("(do_trust)Saving %s in room %d.", GET_NAME(ch), IN_ROOM(ch));
		send_to_char ( "Command Group Activated.\r\n", ch );
	}
}


ACMD ( do_autowiz )
{
	ch->Send ( "Processing...\r\n" );
	check_autowiz ( ch );
	ch->Send ( "done.\r\n" );
}

ACMD ( do_echo )
{
	skip_spaces ( &argument );

	if ( !*argument )
		ch->Send ( "Yes.. but what?\r\n" );
	else
	{
		char buf[MAX_INPUT_LENGTH + 4];
		int sp = 0;

		if ( *argument == '\'' && * ( argument+1 ) == 's' && * ( argument+2 ) == ' ' )
			sp = 1;
		else if ( *argument == 's' && * ( argument+1 ) == ' ' )
			sp = 1;


		if ( subcmd == SCMD_EMOTE )
			snprintf ( buf, sizeof ( buf ), "%s%s", sp ? "$n" : "$n ", argument );
		else if ( subcmd == SCMD_POSE )
		{

			snprintf ( buf, sizeof ( buf ), "[$n]\r\n%s", argument );

		}
		else if ( subcmd == SCMD_RECHO )
		{
			if ( !PLR_FLAGGED ( ch, PLR_RP_LEADER ) )
			{
				ch->Send ( "You can't do that!\r\n" );
				return;
			}
			strlcpy ( buf, argument, sizeof ( buf ) );
		}
		else
			strlcpy ( buf, argument, sizeof ( buf ) );

		if ( !PLR_FLAGGED ( ch, PLR_COVENTRY ) )
			act ( buf, FALSE, ch, 0, 0, TO_ROOM );


		if ( !IS_NPC ( ch ) && PRF_FLAGGED ( ch, PRF_NOREPEAT ) )
			ch->Send ( "%s", CONFIG_OK );
		else
			act ( buf, FALSE, ch, 0, 0, TO_CHAR );
	}
}


ACMD ( do_send )
{
	char arg[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH];
	Character *vict;

	half_chop ( argument, arg, buf );

	if ( !*arg )
	{
		ch->Send ( "Send what to who?\r\n" );
		return;
	}
	if ( ! ( vict = get_char_vis ( ch, arg, NULL, FIND_CHAR_WORLD ) ) )
	{
		ch->Send ( "%s", CONFIG_NOPERSON );
		return;
	}
	vict->Send ( "%s\r\n", buf );
	if ( PRF_FLAGGED ( ch, PRF_NOREPEAT ) )
		ch->Send ( "Sent.\r\n" );
	else
		ch->Send ( "You send '%s' to %s.\r\n", buf,
		           GET_NAME ( vict ) );
}

ACMD ( do_prompt_new )
{
	char **msg;

	if ( subcmd == SCMD_PROMPT )
		msg = & ( PROMPT ( ch ) );
	else
		msg = & ( BPROMPT ( ch ) );

	skip_spaces ( &argument );

	if ( !argument || !*argument )
	{
		if ( subcmd == SCMD_PROMPT )
			ch->Send ( "Your prompt: %s\r\nFor a default prompt type: prompt default\r\nFor no prompt type: prompt none\r\n", PROMPT ( ch ) );
		else
			ch->Send ( "Your battle prompt: %s\r\nFor a default battle prompt type: bprompt default\r\nFor no battle prompt type: bprompt none\r\n", BPROMPT ( ch ) );
		return;
	}

	if ( !strcmp ( argument, "default" ) || !strcmp ( argument, "all" ) )
	{
		if ( subcmd == SCMD_PROMPT )
			strcpy ( argument, "{cg%h{cwH {cc%m{cwM {cy%v{cwV {cW(%S) {cC%E{cyTNL{c0>" );
		else if ( subcmd == SCMD_BPROMPT )
			strcpy ( argument, "{cg%h{cwH {cc%m{cwM {cW(%S) {cC%E{cyTNL {cwVictHp:{cM%f%%{c0 >" );
	}
	else if ( ( subcmd == SCMD_BPROMPT ) && !strcmp ( argument, "copy" ) )
	{
		strcpy ( argument, PROMPT ( ch ) );
	}

	if ( strlen ( argument ) > MAX_PROMPT_LENGTH - 30 )
	{
		new_send_to_char
		( ch, "Your prompt must be shorter than %d characters.\r\n",
		  MAX_PROMPT_LENGTH - 30 );
		return;
	}


	if ( *msg )
		free ( *msg );

	if ( !*argument )
		*msg = str_dup ( "none" );
	else
		*msg = str_dup ( argument );

	ch->Send ( "%s", CONFIG_OK );
}

/* take a string, and return an rnum.. used for goto, at, etc.  -je 4/6/93 */
room_rnum find_target_room ( Character *ch, char *rawroomstr )
{
	room_rnum location = NULL;
	char roomstr[MAX_INPUT_LENGTH];

	skip_spaces ( &rawroomstr );
	one_argument ( rawroomstr, roomstr );

	if ( !*roomstr )
	{
		ch->Send ( "You must supply a room number or name.\r\n" );
		return ( NULL );
	}

	if ( isdigit ( *roomstr ) && !strchr ( roomstr, '.' ) )
	{
		if ( ( location = real_room ( atoi ( roomstr ) ) ) == NULL )
		{
			ch->Send ( "No room exists with that number.\r\n" );
			return ( NULL );
		}
	}
	else
	{
		Character *target_mob;
		struct obj_data *target_obj;
		Room *target_room;
		char *mobobjstr = rawroomstr;
		int num;

		if ( *roomstr == UID_CHAR )
		{

			if ( ( target_mob = find_char ( atoi ( roomstr + 1 ) ) ) != NULL )
				return IN_ROOM ( target_mob );
			else if ( ( target_room = find_room ( atoi ( roomstr + 1 ) ) ) != NULL )
				return ( target_room );
			else if ( ( target_obj = find_obj ( atoi ( roomstr + 1 ) ) ) != NULL )
			{
				if ( IN_ROOM ( target_obj ) != NULL )
					return IN_ROOM ( target_obj );
				else if ( target_obj->carried_by && IN_ROOM ( target_obj->carried_by ) != NULL )
					return IN_ROOM ( target_obj->carried_by );
				else if ( target_obj->worn_by && IN_ROOM ( target_obj->worn_by ) != NULL )
					return IN_ROOM ( target_obj->worn_by );
				else if ( target_obj->in_locker && IN_ROOM ( target_obj->in_locker ) != NULL )
					return IN_ROOM ( target_obj->in_locker );
			}
		}

		num = get_number ( &mobobjstr );
		if ( ( target_mob =
		            get_char_vis ( ch, mobobjstr, &num, FIND_CHAR_WORLD ) ) != NULL )
		{
			if ( ( location = IN_ROOM ( target_mob ) ) == NULL )
			{
				ch->Send ( "That character is currently lost.\r\n" );
				return ( NULL );
			}
		}
		else if ( ( target_obj = get_obj_vis ( ch, mobobjstr, &num ) ) != NULL )
		{
			if ( IN_ROOM ( target_obj ) != NULL )
				location = IN_ROOM ( target_obj );
			else if ( target_obj->carried_by && IN_ROOM ( target_obj->carried_by ) != NULL )
				location = IN_ROOM ( target_obj->carried_by );
			else if ( target_obj->worn_by && IN_ROOM ( target_obj->worn_by ) != NULL )
				location = IN_ROOM ( target_obj->worn_by );
			else if ( target_obj->in_locker && IN_ROOM ( target_obj->in_locker ) != NULL )
				location = IN_ROOM ( target_obj->in_locker );

			if ( location == NULL )
			{
				ch->Send (
				    "That object is currently not in a room.\r\n" );
				return ( NULL );
			}
		}

		if ( location == NULL )
		{
			ch->Send ( "Nothing exists by that name.\r\n" );
			return ( NULL );
		}
	}
	if ( IS_NPC ( ch ) )
	{
		if ( ch->desc && ch->desc->original && GET_LEVEL ( ch->desc->original ) < LVL_SEN )
			return NULL;
		else
			return ( location );
	}



	if ( ( !can_edit_zone ( ch, location->zone ) &&
	        ( location->number/100 ) != 12 && ( location->number/100 ) != 30 ) && ( ( !CMD_FLAGGED ( ch, WIZ_GOTO_GRP ) )
	                && ( !CMD_FLAGGED2 ( ch, WIZ_GOTO_GRP ) ) ) )
	{
		ch->Send ( "Sorry, that is out of limits.\r\nIf found there you may lose your imm.\r\n" );
		new_mudlog ( CMP, ( GET_LEVEL ( ch ) == LVL_IMPL ? GET_LEVEL ( ch ) :GET_LEVEL ( ch ) +1 ), TRUE, "(GC) %s tried to goto %d.",
		             GET_NAME ( ch ),location->number );

	}
	else if ( ROOM_FLAGGED ( location, ROOM_GODROOM ) && GET_LEVEL ( ch ) < LVL_SEN )
		ch->Send ( "You are not godly enough to use that room!\r\n" );
	else if ( ROOM_FLAGGED ( location, ROOM_PRIVATE ) && GET_LEVEL ( ch ) < LVL_IMPL && num_pc_in_room ( location ) >= 2 )
		ch->Send ( "There's a private conversation going on in that room.\r\n" );
	else if ( ROOM_FLAGGED ( location, ROOM_HOUSE )
	          && !House_can_enter ( ch, GET_ROOM_VNUM ( location ) ) )
		ch->Send ( "That's private property -- no trespassing!\r\n" );
	else
		return ( location );

	return ( NULL );
}



ACMD ( do_at )
{
	char command[MAX_INPUT_LENGTH];
	Room * location, *original_loc;
	char buf[MAX_INPUT_LENGTH];

	half_chop ( argument, buf, command );
	if ( !*buf )
	{
		send_to_char ( "You must supply a room number or a name.\r\n", ch );
		return;
	}

	if ( !*command )
	{
		send_to_char ( "What do you want to do there?\r\n", ch );
		return;
	}

	if ( ( location = find_target_room ( ch, buf ) ) ==NULL )
		return;

	/* a location has been found. */
	original_loc = IN_ROOM ( ch );
	if ( !move_char_to ( ch, location ) )
		return;
	command_interpreter ( ch, command );

	/* check if the char is still there */
	if ( IN_ROOM ( ch ) == location )
	{
		move_char_to ( ch, original_loc );
	}
}

ACMD ( do_atlvl )
{
	char command[MAX_INPUT_LENGTH];
	char buf[MAX_INPUT_LENGTH];
	int lev;

	half_chop ( argument, buf, command );
	if ( !*buf )
	{
		send_to_char ( "You must supply a level.\r\n", ch );
		return;
	}

	if ( GET_ORIG_LEV ( ch ) )
	{
		ch->Send ( "Return to your original level first.\r\n" );
		return;
	}



	lev = atoi ( buf );
	if ( lev < 1 || lev >= GET_LEVEL ( ch ) )
	{
		send_to_char ( "That is an invalid level.\r\n", ch );
		return;
	}
	ch->Send ( "{cYINFO:You are now at level %d, to return to original level type: return{c0\r\n", lev );

	GET_ORIG_LEV ( ch ) = GET_LEVEL ( ch );
	GET_LEVEL ( ch ) = lev;
	if ( *command )
		command_interpreter ( ch, command );

	/*if (GET_LEVEL(ch) != orig)
	GET_LEVEL(ch) = orig;*/

}


ACMD ( do_goto )
{
	room_rnum location;
	char buf[MAX_INPUT_LENGTH];

	if ( ( location = find_target_room ( ch, argument ) ) < 0 )
		return;

	if ( POOFOUT ( ch ) )
		snprintf ( buf, sizeof ( buf ), "%s", POOFOUT ( ch ) );
	else
		snprintf ( buf, sizeof ( buf ), "$n disappears in a puff of smoke." );

	act ( buf, TRUE, ch, 0, 0, TO_ROOM );
	if ( !move_char_to ( ch, location ) )
		return;

	if ( POOFIN ( ch ) )
		snprintf ( buf, sizeof ( buf ), "%s", POOFIN ( ch ) );
	else
		snprintf ( buf, sizeof ( buf ), "$n appears with an ear-splitting bang." );

	act ( buf, TRUE, ch, 0, 0, TO_ROOM );
	LOOK ( ch );
	entry_memory_mtrigger ( ch );
	greet_mtrigger ( ch, -1 );
	greet_memory_mtrigger ( ch );
	enter_wtrigger ( IN_ROOM ( ch ), ch, -1 );

}



ACMD ( do_trans )
{
	Descriptor *i;
	Character *victim;
	char buf[MAX_INPUT_LENGTH];


	one_argument ( argument, buf );
	if ( !*buf )
		send_to_char ( "Whom do you wish to transfer?\r\n", ch );
	else if ( str_cmp ( "all", buf ) )
	{
		if ( ! ( victim = get_char_vis ( ch, buf, NULL, FIND_CHAR_WORLD ) ) )
			ch->Send ( "%s", CONFIG_NOPERSON );
		else if ( victim == ch )
			send_to_char ( "That doesn't make much sense, does it?\r\n", ch );
		else
		{
			if ( ( GET_LEVEL ( ch ) < GET_LEVEL ( victim ) ) && !IS_NPC ( victim ) )
			{
				send_to_char ( "Go transfer someone your own size.\r\n", ch );
				return;
			}
			act ( "$n disappears in a mushroom cloud.", FALSE, victim, 0, 0,
			      TO_ROOM );
			ch->Send ( "You transfer %s from room %d.\r\n",
			           PERS ( victim, ch ),
			           IN_ROOM ( victim )->number );
			if ( !move_char_to ( victim, IN_ROOM ( ch ) ) )
				return;
			act ( "$n arrives from a puff of smoke.", FALSE, victim, 0, 0,
			      TO_ROOM );
			act ( "$n has transferred you!", FALSE, ch, 0, victim, TO_VICT );
			look_at_room ( victim, 0 );
			entry_memory_mtrigger ( victim );
			greet_mtrigger ( victim, -1 );
			greet_memory_mtrigger ( victim );
			enter_wtrigger ( IN_ROOM ( victim ), victim, -1 );
		}
	}
	else              /* Trans All */
	{
		if ( GET_LEVEL ( ch ) < LVL_GRGOD )
		{
			send_to_char ( "I think not.\r\n", ch );
			return;
		}

		for ( i = descriptor_list; i; i = i->next )
			if ( STATE ( i ) == CON_PLAYING && i->character
			        && i->character != ch )
			{
				victim = i->character;
				if ( GET_LEVEL ( victim ) >= GET_LEVEL ( ch ) )
					continue;
				ch->Send ( "You transfer %s from room %d.\r\n",
				           PERS ( victim, ch ),
				           IN_ROOM ( victim )->number );
				act ( "$n disappears in a mushroom cloud.", FALSE, victim, 0,
				      0, TO_ROOM );
				if ( move_char_to ( victim, IN_ROOM ( ch ) ) )
					return;
				act ( "$n arrives from a puff of smoke.", FALSE, victim, 0,
				      0, TO_ROOM );
				act ( "$n has transferred you!", FALSE, ch, 0, victim,
				      TO_VICT );
				look_at_room ( victim, 0 );
				entry_memory_mtrigger ( victim );
				greet_mtrigger ( victim, -1 );
				greet_memory_mtrigger ( victim );
				enter_wtrigger ( IN_ROOM ( victim ), victim, -1 );
			}
		ch->Send ( "%s", CONFIG_OK );
	}
}



ACMD ( do_teleport )
{
	Character *victim;
	room_rnum target;
	char buf[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];

	two_arguments ( argument, buf, buf2 );

	if ( !*buf )
		send_to_char ( "Whom do you wish to teleport?\r\n", ch );
	else if ( ! ( victim = get_char_vis ( ch, buf, NULL, FIND_CHAR_WORLD ) ) )
		ch->Send ( "%s", CONFIG_NOPERSON );
	else if ( victim == ch )
		send_to_char ( "Use 'goto' to teleport yourself.\r\n", ch );
	else if ( !IS_NPC ( victim ) && GET_LEVEL ( victim ) >= GET_LEVEL ( ch ) )
		send_to_char ( "Maybe you shouldn't do that.\r\n", ch );
	else if ( !*buf2 )
		send_to_char ( "Where do you wish to send this person?\r\n", ch );
	else if ( ( target = find_target_room ( ch, buf2 ) ) >= 0 )
	{
		ch->Send ( "%s", CONFIG_OK );
		act ( "$n disappears in a puff of smoke.", FALSE, victim, 0, 0,  TO_ROOM );
		if ( !move_char_to ( victim, target ) )
			return;
		act ( "$n arrives from a puff of smoke.", FALSE, victim, 0, 0, TO_ROOM );
		act ( "$n has teleported you!", FALSE, ch, 0, ( char * ) victim, TO_VICT );
		LOOK ( victim );
		entry_memory_mtrigger ( victim );
		greet_mtrigger ( victim, -1 );
		greet_memory_mtrigger ( victim );
		enter_wtrigger ( IN_ROOM ( victim ), victim, -1 );
	}
}

#define ZOCMD zone_table[zrnum].cmd[subcmd]

void list_zone_commands_room ( Character *ch, room_vnum rvnum )
{
	extern struct index_data **trig_index;
	zone_rnum zrnum = real_zone_by_thing ( rvnum );
	room_rnum rrnum = real_room ( rvnum ), cmd_room = NULL;
	int subcmd = 0, count = 0;

	if ( zrnum == NOWHERE || rrnum == NULL )
	{
		ch->Send ( "No zone information available.\r\n" );
		return;
	}

	get_char_colours ( ch );

	ch->Send ( "Zone commands in this room:%s\r\n", yel );
	while ( ZOCMD.command != 'S' )
	{
		switch ( ZOCMD.command )
		{
			case 'M':
			case 'O':
			case 'T':
			case 'V':
			case 'B':
				cmd_room = world_vnum[ZOCMD.arg3];
				break;
			case 'D':
			case 'R':
				cmd_room = world_vnum[ZOCMD.arg1];
				break;
			default:
				break;
		}
		if ( cmd_room == rrnum )
		{
			count++;
			/* start listing */
			switch ( ZOCMD.command )
			{
				case 'M':
					ch->Send ( "%sLoad %s [%s%d%s], Max : %d\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           GetMobProto ( ZOCMD.arg1 )->player.short_descr, cyn,
					           ZOCMD.arg1, yel, ZOCMD.arg2
					         );
					break;

				case 'G':
					ch->Send ( "%sGive it %s [%s%d%s], Max : %d\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           obj_proto[ZOCMD.arg1].short_description,
					           cyn, obj_index[ZOCMD.arg1].vnum, yel,
					           ZOCMD.arg2
					         );
					break;
				case 'O':
					ch->Send ( "%sLoad %s [%s%d%s], Max : %d\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           obj_proto[ZOCMD.arg1].short_description,
					           cyn, obj_index[ZOCMD.arg1].vnum, yel,
					           ZOCMD.arg2
					         );
					break;
				case 'B':
					ch->Send ( "%sBury %s [%s%d%s], Max : %d\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           obj_proto[ZOCMD.arg1].short_description,
					           cyn, obj_index[ZOCMD.arg1].vnum, yel,
					           ZOCMD.arg2
					         );
					break;
				case 'E':
					ch->Send ( "%sEquip with %s [%s%d%s], %s, Max : %d\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           obj_proto[ZOCMD.arg1].short_description,
					           cyn, obj_index[ZOCMD.arg1].vnum, yel,
					           equipment_types[ZOCMD.arg3],
					           ZOCMD.arg2
					         );
					break;
				case 'P':
					ch->Send ( "%sPut %s [%s%d%s] in %s [%s%d%s], Max : %d\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           obj_proto[ZOCMD.arg1].short_description,
					           cyn, obj_index[ZOCMD.arg1].vnum, yel,
					           obj_proto[ZOCMD.arg3].short_description,
					           cyn, obj_index[ZOCMD.arg3].vnum, yel,
					           ZOCMD.arg2
					         );
					break;
				case 'R':
					ch->Send ( "%sRemove %s [%s%d%s] from room.\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           obj_proto[ZOCMD.arg2].short_description,
					           cyn, obj_index[ZOCMD.arg2].vnum, yel
					         );
					break;
				case 'D':
					ch->Send ( "%sSet door %s as %s.\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           dirs[ZOCMD.arg2],
					           ZOCMD.arg3 ? ( ( ZOCMD.arg3 == 1 ) ? "closed" : "locked" ) : "open"
							         );
					break;
				case 'T':
					ch->Send ( "%sAttach trigger %s%s%s [%s%d%s] to %s\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           cyn, trig_index[ZOCMD.arg2]->proto->name, yel,
					           cyn, trig_index[ZOCMD.arg2]->vnum, yel,
					           ( ( ZOCMD.arg1 == MOB_TRIGGER ) ? "mobile" :
					             ( ( ZOCMD.arg1 == OBJ_TRIGGER ) ? "object" :
					               ( ( ZOCMD.arg1 == WLD_TRIGGER ) ? "room" : "????" ) ) ) );
					break;
				case 'V':
					ch->Send ( "%sAssign global %s:%s with context %d to the %s\r\n",
					           ZOCMD.if_flag ? " then " : "",
					           ZOCMD.sarg1, ZOCMD.sarg2, ZOCMD.arg2,
					           ( ( ZOCMD.arg1 == MOB_TRIGGER ) ? "mobile" :
					             ( ( ZOCMD.arg1 == OBJ_TRIGGER ) ? "object" :
					               ( ( ZOCMD.arg1 == WLD_TRIGGER ) ? "room" : "????" ) ) ) );
					break;
				default:
					ch->Send ( "<Unknown Command>\r\n" );
					break;
			}
		}
		subcmd++;
	}
	ch->Send ("%s", nrm );
	if ( !count )
		ch->Send ( "None!\r\n" );

}
#undef ZOCMD

ACMD ( do_vnum )
{
	char buf[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];
	argument = any_one_arg ( argument, buf );
	skip_spaces ( &argument );
	strlcpy ( buf2, argument, sizeof ( buf2 ) );

	if ( !*buf || !*buf2
	        || ( !is_abbrev ( buf, "mob" ) && !is_abbrev ( buf, "obj" )
                      && !is_abbrev ( buf, "prot") ) )
	{
		send_to_char ( "Usage: vnum { obj | mob | prot } <name>\r\n", ch );
		return;
	}
	if ( is_abbrev ( buf, "mob" ) )
		if ( !vnum_mobile ( buf2, ch ) )
			send_to_char ( "No mobiles by that name.\r\n", ch );

	if ( is_abbrev ( buf, "obj" ) )
		if ( !vnum_object ( buf2, ch, 0 ) )
			send_to_char ( "No objects by that name.\r\n", ch );

	if ( is_abbrev ( buf, "prot" ) )
		if ( !vnum_object ( buf2, ch, 1 ) )
			send_to_char ( "No objects by that name.\r\n", ch );
}



void do_stat_room ( Character *ch )
{
	struct extra_descr_data *desc;
	Room *rm = IN_ROOM ( ch );
	int i, found, column;
	struct obj_data *j;
	Character *k;
	char buf2[MAX_INPUT_LENGTH];

	new_mudlog ( CMP, ( GET_LEVEL ( ch ) == LVL_IMPL ? GET_LEVEL ( ch ) : GET_LEVEL ( ch ) +1 ), TRUE, "(GC) %s statted room %s [%d].", GET_NAME ( ch ),  rm->name, rm->number );

	ch->Send ( "Room name: %s%s%s\r\n", CCCYN ( ch, C_NRM ),
	           rm->name, CCNRM ( ch, C_NRM ) );

	sprinttype ( rm->sector_type, sector_types, buf2, sizeof ( buf2 ) );
	ch->Send (
	    "VNum: [%s%5d%s], RNum: [%5d], IDNum: %ld, Type: %s\r\n",
	    CCGRN ( ch, C_NRM ),
	    rm->number, CCNRM ( ch, C_NRM ), IN_ROOM ( ch )->number, ( long ) rm->number + ROOM_ID_BASE, buf2 );
	ch->Send ( "Dimension: %s\r\n",dimension_types[zone_table[rm->zone].dimension] );
	ch->Send ( "ZONE: (%-3d) %-30s [%s] \r\n",
	           zone_table[rm->zone].number,
	           zone_table[rm->zone].name,
	           zone_table[rm->zone].builders );
	if ( rm->mine.num != -1 )
		ch->Send ( "{cy[MINE: %3d LEVEL: %d Tool: %s]{c0\r\n", rm->mine.num, rm->mine.dif, rm->mine.num == -1 ? "None" : rm->mine.tool == TOOL_SHOVEL ? "Shovel" : "Pickaxe" );

	sprintbitarray ( rm->room_flags, room_bits, RF_ARRAY_MAX, buf2, sizeof ( buf2 ) );
	ch->Send ( "SpecProc: %s, Flags: %s\r\n",
	           ( rm->func == NULL ) ? "None" : "Exists", buf2 );

	if (rm->func == playershop )
		ch->Send ( "Playershop owned by %s.\r\n", pi.NameById ( player_shop[ rm->number ]->owner_id ) );

	ch->Send ( "Description:\r\n%s",
	           rm->HasDesc() ? rm->GetDescription() : "  None.\r\n" );

	if ( rm->ex_description )
	{
		ch->Send ( "Extra descs:%s", CCCYN ( ch, C_NRM ) );
		for ( desc = rm->ex_description; desc; desc = desc->next )
			ch->Send ( " [%s]", desc->keyword );
		ch->Send ( "%s\r\n", CCNRM ( ch, C_NRM ) );
	}
	if ( rm->smell )
		ch->Send ( "SMELL: %s", rm->smell );
	if ( rm->listen )
		ch->Send ( "LISTEN:%s", rm->listen );

	ch->Send ( "Chars present:%s", CCYEL ( ch, C_NRM ) );
	column = 14;      /* ^^^ strlen ^^^ */
	for ( found = FALSE, k = rm->people; k; k = k->next_in_room )
	{
		if ( !CAN_SEE ( ch, k ) )
			continue;

		column +=
		    ch->Send ( "%s %s(%s)", found++ ? "," : "",
		               GET_NAME ( k ),
		               !IS_NPC ( k ) ? "PC" : ( !IS_MOB ( k ) ? "NPC" :
		                                        "MOB" ) );
		if ( column >= 68 )
		{
			ch->Send ( "%s", k->next_in_room ? "," : "" );
			found = FALSE;
			column = 0;
		}
	}
	ch->Send ( "%s\r\n", CCNRM ( ch, C_NRM ) );

	if ( rm->contents )
	{
		ch->Send ( "Contents:%s", CCGRN ( ch, C_NRM ) );
		column = 9;          /* ^^^ strlen ^^^ */

		for ( found = 0, j = rm->contents; j; j = j->next_content )
		{
			if ( !CAN_SEE_OBJ ( ch, j ) )
				continue;

			column +=
			    ch->Send ( "%s %s", found++ ? "," : "",j->short_description );
			if ( column >= 62 )
			{
				ch->Send ( "%s\r\n", j->next_content ? "," : "" );
				found = FALSE;
				column = 0;
			}
		}
		ch->Send ( "%s\r\n", CCNRM ( ch, C_NRM ) );
	}

	for ( i = 0; i < NUM_OF_DIRS; i++ )
	{
		char buf1[128];

		if ( !rm->dir_option[i] )
			continue;

		if ( rm->dir_option[i]->to_room == NULL )
			snprintf ( buf1, sizeof ( buf1 ), " %sNONE%s", CCCYN ( ch, C_NRM ), CCNRM ( ch, C_NRM ) );
		else
			snprintf ( buf1, sizeof ( buf1 ), "%s%5d%s", CCCYN ( ch, C_NRM ),
			           rm->dir_option[i]->to_room->number, CCNRM ( ch, C_NRM ) );

		new_sprintbit ( rm->dir_option[i]->exit_info, exit_bits, buf2, sizeof ( buf2 ) );

		ch->Send (
		    "Exit %s%-5s%s:  To: [%s], Key: [%5d], Keywrd: %s, Type: %s\r\n%s",
		    CCCYN ( ch, C_NRM ), dirs[i], CCNRM ( ch, C_NRM ), buf1,
		    rm->dir_option[i]->key == NOTHING ? -1 : rm->dir_option[i]->key,
		    rm->dir_option[i]->keyword ? rm->dir_option[i]->
		    keyword : "None", buf2,
		    rm->dir_option[i]->general_description ? rm->
		    dir_option[i]->
		    general_description :
		    "  No exit description.\r\n" );
	}

	list_zone_commands_room ( ch, rm->number );
	/* check the room for a script */
	//if (GET_LEVEL(ch) >= LVL_SEN)
	do_sstat_room ( ch );
}

const char *balance_display ( int balance )
{

	if ( balance <= 0 || balance >= 100 )
		return "=========================";


	return balances[ ( balance/4 ) ];
}


void do_stat_object ( Character *ch, struct obj_data *j )
{
	int i, found;
	obj_vnum vnum;
	int zone;
	struct obj_data *j2;
	struct extra_descr_data *desc;
	Character *tempch;
	char buf[MAX_INPUT_LENGTH];
	char buf1[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];

	vnum = GET_OBJ_VNUM ( j );

	new_mudlog ( CMP, ( GET_LEVEL ( ch ) == LVL_IMPL ? GET_LEVEL ( ch ) : GET_LEVEL ( ch ) +1 ), TRUE, "(GC) %s statted obj %s [%d].", GET_NAME ( ch ), ( ( j->short_description ) ? j->short_description : "<None>" ), vnum );

	ch->Send ( "Name: '%s%s%s', Aliases: %s\r\n", CCYEL ( ch, C_NRM ),
	           ( ( j->short_description ) ? j->short_description : "<None>" ),
	           CCNRM ( ch, C_NRM ), j->name );

	sprinttype ( GET_OBJ_TYPE ( j ), item_types, buf1, sizeof ( buf1 ) );
	ch->Send (
	    "Material: [%s%10s%s] VNum: [%s%5d%s], RNum: [%5d], Type: %s, IDNum: %ld, SpecProc: %s\r\n",
	    CCGRN ( ch, C_NRM ), material_name ( GET_OBJ_MATERIAL ( j ) ),
	    CCNRM ( ch, C_NRM ), CCGRN ( ch, C_NRM ), vnum, CCNRM ( ch, C_NRM ), GET_OBJ_RNUM ( j ),
	    buf1, GET_ID ( j )  ,
	    ( GET_OBJ_RNUM ( j ) != NOTHING ? ( obj_index[GET_OBJ_RNUM ( j ) ].func ? "Exists" : "None" ) : "None" ) );

	if ( GET_OBJ_RNUM ( j ) != NOTHING && obj_index[GET_OBJ_RNUM ( j ) ].qic != NULL && ( GET_LEVEL ( ch ) >= LVL_SEN ) )
	{
		ch->Send ( " QIC: %d(%d)\r\n",obj_index[GET_OBJ_RNUM ( j ) ].qic->items ,
		           obj_index[GET_OBJ_RNUM ( j ) ].qic->limit );
	}
	zone = real_zone ( GET_OBJ_VNUM ( j ) );
	if ( zone > 0 )
	{
		ch->Send ( "Dimension: %s\r\n",dimension_types[zone_table[zone].dimension] );
		ch->Send ( "ZONE: (%-3d) %-30s [%s]\r\n",
		           zone_table[zone].number, zone_table[zone].name,zone_table[zone].builders );
	}

	ch->Send ( "Timer: %d mud hours. ", GET_OBJ_TIMER ( j ) );
	if ( GET_OBJ_SAVED_REMAINING_EXPIRE ( j ) != 0)
	  ch->Send ("Timer paused at: {cC%ld{c0 min and {cY%ld{c0 seconds\r\n", GET_OBJ_SAVED_REMAINING_EXPIRE(j)/60, GET_OBJ_SAVED_REMAINING_EXPIRE(j)%60);
	else if ( GET_OBJ_EXPIRE ( j ) == 0 )
		ch->Send ( "Expires in: Timer hasn't been started yet!\r\n" );
	else
	{
		if ( GET_OBJ_EXPIRE ( j ) > time ( 0 ) )
		{
			time_t diff = ( GET_OBJ_EXPIRE ( j ) - time ( 0 ) );
			ch->Send ( "Expires in: {cC%ld{c0 min and {cY%ld{c0 seconds\r\n",   diff/60, diff%60 );
		}
		else
		{
			ch->Send ( "Expires in: NOW!\r\n" );
		}

	}
	if ( j->owner != 0 )
		ch->Send ( "Object owned by: {cW%s{c0\r\n", pi.NameById ( j->owner ) );
	ch->Send ( "L-Des: %s\r\n",
	           ( ( j->description ) ? j->description : "None" ) );

	if ( j->ex_description )
	{
		ch->Send ( "Extra descs:%s", CCCYN ( ch, C_NRM ) );
		for ( desc = j->ex_description; desc; desc = desc->next )
			ch->Send ( " [%s]", desc->keyword );

		ch->Send ( "%s\r\n", CCNRM ( ch, C_NRM ) );
	}
	ch->Send ( "Can be worn on: " );
	sprintbitarray ( j->obj_flags.wear_flags, wear_bits, TW_ARRAY_MAX, buf, sizeof ( buf ) );
	ch->Send ( "%s\r\n", buf );

	ch->Send ( "Set char bits : " );
	sprintbitarray ( j->obj_flags.bitvector, affected_bits, AF_ARRAY_MAX, buf, sizeof ( buf ) );
	ch->Send ( "%s\r\n",buf );

	ch->Send ( "Extra flags   : " );
	sprintbitarray ( GET_OBJ_EXTRA ( j ), extra_bits, EF_ARRAY_MAX, buf, sizeof ( buf ) );
	ch->Send ( "%s\r\n",buf );

	ch->Send ( "Weight: %d, Value: %lld, Cost/day: %d, Timer: %d, Min level: %d\r\n",
	           GET_OBJ_WEIGHT ( j ), GET_OBJ_COST ( j ), GET_OBJ_RENT ( j ), GET_OBJ_TIMER ( j ), GET_OBJ_LEVEL ( j ) );

	ch->Send ( "In room: " );
	if ( j->in_room == NULL )
		ch->Send ( "Nowhere" );
	else
		ch->Send ( "%d", GET_ROOM_VNUM ( IN_ROOM ( j ) ) );
	/*
	 * NOTE: In order to make it this far, we must already be able to see the
	 *       character holding the object. Therefore, we do not need CAN_SEE().
	 */
	ch->Send ( ", In object: " );
	ch->Send ( "%s",  j->in_obj ? j->in_obj->short_description : "None" );
	ch->Send ( ", Carried by: " );
	ch->Send ( "%s",   j->carried_by ? GET_NAME ( j->carried_by ) : "Nobody" );
	ch->Send ( ", Worn by: " );
	ch->Send ( "%s\r\n",   j->worn_by ? GET_NAME ( j->worn_by ) : "Nobody" );
	ch->Send ( ", In Locker: " );
	ch->Send ( "%s\r\n",   j->in_locker ? GET_NAME ( j->in_locker ) : "Nobody" );

	if ( GET_OBJ_INNATE ( j ) )
		ch->Send ( "Innate: %s\r\n", skill_name ( GET_OBJ_INNATE ( j ) ) );

	switch ( GET_OBJ_TYPE ( j ) )
	{
		case ITEM_LIGHT:
			if ( GET_OBJ_VAL ( j, 2 ) == -1 )
				ch->Send ( "Hours left: Infinite\r\n" );
			else
				ch->Send ( "Hours left: [%d]\r\n", GET_OBJ_VAL ( j, 2 ) );
			break;
		case ITEM_SCROLL:
		case ITEM_POTION:
			ch->Send ( "Spells: (Level %d) %s, %s, %s\r\n", GET_OBJ_VAL ( j, 0 ),
			           skill_name ( GET_OBJ_VAL ( j, 1 ) ),
			           skill_name ( GET_OBJ_VAL ( j, 2 ) ),
			           skill_name ( GET_OBJ_VAL ( j, 3 ) ) );
			break;
		case ITEM_WAND:
		case ITEM_STAFF:
			ch->Send ( "Spell: %s at level %d, %d (of %d) charges remaining\r\n",
			           skill_name ( GET_OBJ_VAL ( j, 3 ) ), GET_OBJ_VAL ( j, 0 ),
			           GET_OBJ_VAL ( j, 2 ), GET_OBJ_VAL ( j, 1 ) );
			break;
		case ITEM_THROW:
		case ITEM_ROCK:
		case ITEM_BOLT:
		case ITEM_ARROW:
		case ITEM_AMMO:
			ch->Send ( "Number dam dice: %d Size dam dice: %d\r\n",
			           ( int ) GET_OBJ_VAL ( j, 1 ), ( int ) GET_OBJ_VAL ( j, 2 ) );
			break;
		case ITEM_GRENADE:
			ch->Send ( "Timer: %d Num dam dice: %d Size dam dice: %d\r\n",
			           ( int ) GET_OBJ_VAL ( j, 0 ), ( int ) GET_OBJ_VAL ( j, 1 ),
			           ( int ) GET_OBJ_VAL ( j, 2 ) );
			break;
		case ITEM_BOW:
		case ITEM_CROSSBOW:
		case ITEM_SLING:
		case ITEM_GUN:
			ch->Send ( "Range    : %d  Max Ammo : %d  Cur Ammo : %d, vnum of ammo: %d\r\n",
			           ( int ) GET_OBJ_VAL ( j, 0 ), ( int ) GET_OBJ_VAL ( j, 1 ),
			           ( int ) GET_OBJ_VAL ( j, 2 ), ( int ) GET_OBJ_VAL ( j, 3 ) );
			break;
		case ITEM_WEAPON:
			ch->Send ( "VAR: %d Todam: %dd%d, Message type: %d, Length: %dcm\r\n"
			           "Balance: (%d) %s\r\n",
			           ( int ) GET_OBJ_VAL ( j, 0 ), ( int ) GET_OBJ_VAL ( j, 1 ), ( int ) GET_OBJ_VAL ( j, 2 ),
			           ( int ) GET_OBJ_VAL ( j, 3 ), ( int ) GET_WEP_LENGTH ( j ), ( int ) GET_WEP_BALANCE ( j ),
			           balance_display ( GET_WEP_BALANCE ( j ) ) );
			ch->Send ( "This balance gives the weapon %d speed, %d accuracy and %d evasion.\r\n",
			           get_weapon_speed ( j ),
			           get_weapon_accuracy ( j ),
			           get_weapon_evasion ( j ) );
			break;
		case ITEM_ARMOR:
			ch->Send ( "AC-apply: [%d]\r\n", ( int ) GET_OBJ_VAL ( j, 0 ) );
			break;
		case ITEM_TRAP:
			ch->Send ( "Spell: %d, - Hitpoints: %d - IS SET:%s\r\n",
			           ( int ) GET_OBJ_VAL ( j, 0 ), ( int ) GET_OBJ_VAL ( j, 1 ), YESNO ( TRAP_IS_SET ( j ) ) );
			break;
		case ITEM_CONTAINER:
			sprintbit ( ( int ) GET_OBJ_VAL ( j, 1 ), container_bits, buf2, sizeof ( buf2 ) );
			ch->Send (
			    "Weight capacity: %d, Lock Type: %s, Key Num: %d, Corpse: %s\r\n",
			    ( int ) GET_OBJ_VAL ( j, 0 ), buf2, ( int ) GET_OBJ_VAL ( j, 2 ),
			    YESNO ( ( int ) GET_OBJ_VAL ( j, 3 ) ) );
			break;
		case ITEM_DRINKCON:
		case ITEM_FOUNTAIN:
			sprinttype ( ( int ) GET_OBJ_VAL ( j, 2 ), drinks, buf2, sizeof ( buf2 ) );
			ch->Send (
			    "Capacity: %d, Contains: %d, Poisoned: %s, Liquid: %s, Casts: %s\r\n",
			    ( int ) GET_OBJ_VAL ( j, 0 ), ( int ) GET_OBJ_VAL ( j, 1 ),
			    YESNO ( ( int ) GET_OBJ_VAL ( j, 3 ) ), buf2, skill_name ( GET_OBJ_VAL ( j, 4 ) ) );
			break;
		case ITEM_NOTE:
			ch->Send ( "Tongue: %d\r\n", ( int ) GET_OBJ_VAL ( j, 0 ) );
			break;
		case ITEM_KEY:
			break;
		case ITEM_FOOD:
			ch->Send ( "Makes full: %d, Poisoned: %s, Casts: %s\r\n",
			           ( int ) GET_OBJ_VAL ( j, 0 ), YESNO ( ( int ) GET_OBJ_VAL ( j, 3 ) ), skill_name ( GET_OBJ_VAL ( j, 4 ) ) );
			break;
		case ITEM_MONEY:
			ch->Send ( "Coins: %lld\r\n", MONEY ( j ) );
			break;
		case ITEM_FURNITURE:
			ch->Send ( "Can hold: [%d] Num. of People in Chair: [%d]\r\n",
			           GET_OBJ_VAL ( j, 0 ), GET_OBJ_VAL ( j, 1 ) );
			ch->Send ( "Holding : " );
			for ( tempch = OBJ_SAT_IN_BY ( j ); tempch;
			        tempch = NEXT_SITTING ( tempch ) )
			{
				ch->Send ( "%s ", GET_NAME ( tempch ) );
			}
			ch->Send ( "\r\n" );
			break;
		case ITEM_SPACEBIKE:
			ch->Send ( "FUEL: %d MAX: %d SITTING IN BIKE: %s\r\n",
			           GET_FUEL ( j ), GET_MAX_FUEL ( j ), OBJ_SAT_IN_BY ( j ) ? GET_NAME ( OBJ_SAT_IN_BY ( j ) ) : "Nobody" );
			break;
		case ITEM_LIGHTSABRE_HILT:
			ch->Send ( "Num Sabers: %d Number dam dice: %d Size dam dice: %d\r\nSaber Color: %s\r\n",
			           ( int ) GET_OBJ_VAL ( j, 0 ), ( int ) GET_OBJ_VAL ( j, 1 ), ( int ) GET_OBJ_VAL ( j, 2 ),
			           colour_option_name ( ( int ) GET_OBJ_VAL ( j, 3 ) ) );
			break;
		case ITEM_TREE:
			ch->Send ( "Logs: " );
			i = GET_OBJ_VAL ( j, 4 );
			if ( i == 0 )
				ch->Send ( "none\r\n" );
			else ch->Send ( "%d of [%d] %s\r\n", GET_OBJ_VAL ( j, 5 ), i, real_object ( i ) == NOTHING ? "bad vnum" : obj_proto[ real_object( i )].short_description );
		default:
			break;
	}

	ch->Send ( "Values:" );
	for ( i = 0; i < 8; ++i )
		if ( GET_OBJ_VAL ( j, i ) != 0 )
			ch->Send ( " [%d]:%d", i, GET_OBJ_VAL ( j, i ) );
	if ( GET_OBJ_MAX_QUALITY ( j ) > 0 )
			ch->Send ( " [8]:%.3f", GET_OBJ_QUALITY ( j ) );
	for ( i = 8; i < 13; ++i )
		if ( GET_OBJ_VAL ( j, i ) != 0 )
			ch->Send ( " [%d]:%d", i + 1, GET_OBJ_VAL ( j, i ) );
	if ( GET_OBJ_MAX_QUALITY ( j ) > 0 )
			ch->Send ( " [14]:%.3f", GET_OBJ_MAX_QUALITY ( j ) );
	ch->Send ( "\r\n" );

	if ( GET_OBJ_COLOUR ( j ) != 0 )
	{
		if ( GET_OBJ_COLOUR ( j ) > 0 && GET_OBJ_COLOUR ( j ) < NUM_COLOUR_NAMES )
			ch->Send ( "Colour: %s\r\n", colour_names[ GET_OBJ_COLOUR ( j )] );
		else ch->Send ( "Colour: out of range\r\n" );
	}

	if ( GET_OBJ_MAX_QUALITY ( j ) > 0 )
		ch->Send ( "Quality: %s (%.3f)\r\n", QUALITY_NAME ( j ), GET_OBJ_QUALITY ( j ) );

	if ( GET_OBJ_DYECOUNT ( j ) != 0 )
		ch->Send ( "Dyecount: %d\r\n", GET_OBJ_DYECOUNT ( j ) );

	if ( GET_OBJ_ORIGIN ( j ) != 0 )
	{
		ch->Send ( "Origin: " );
		if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_HARDWOOD && GET_OBJ_ORIGIN ( j ) < BEGIN_OF_SOFTWOOD )
			ch->Send ( "%s, hardwood", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_SOFTWOOD && GET_OBJ_ORIGIN ( j ) < BEGIN_OF_SPECIALWOOD )
			ch->Send ( "%s, softwood", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_SPECIALWOOD && GET_OBJ_ORIGIN ( j ) < BEGIN_OF_FRUITWOOD )
			ch->Send ( "%s, specialwood", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_FRUITWOOD && GET_OBJ_ORIGIN ( j ) < BEGIN_OF_JUNKWOOD )
			ch->Send ( "%s, fruitwood", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_JUNKWOOD && GET_OBJ_ORIGIN ( j ) < BEGIN_OF_LARGE_ANIMAL )
			ch->Send ( "%s, junkwood", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_LARGE_ANIMAL && GET_OBJ_ORIGIN ( j ) < BEGIN_OF_REPTILE )
			ch->Send ( "%s, large animal", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_REPTILE && GET_OBJ_ORIGIN ( j ) < BEGIN_OF_SMALL_ANIMAL )
			ch->Send ( "%s, reptile", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_SMALL_ANIMAL && GET_OBJ_ORIGIN ( j ) < BEGIN_OF_NORMAL_ANIMAL )
			ch->Send ( "%s, small animal", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_NORMAL_ANIMAL && GET_OBJ_ORIGIN ( j ) < BEGIN_OF_FURRY_ANIMAL )
			ch->Send ( "%s, normal animal", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else if ( GET_OBJ_ORIGIN ( j ) >= BEGIN_OF_FURRY_ANIMAL && GET_OBJ_ORIGIN ( j ) < NUM_ORIGIN_NAMES )
			ch->Send ( "%s, furry animal", origin_names[ GET_OBJ_ORIGIN ( j )] );
		else ch->Send ( "out of range" );
		ch->Send ( "\r\n" );
	}

	if ( GET_OBJ_STAGE ( j ) != 0 )
		ch->Send ( "Stage: %d\r\n", GET_OBJ_STAGE ( j ) );

	if ( GET_OBJ_REPAIRS ( j ) != 0 )
		ch->Send ( "Repairs: %d\r\n", GET_OBJ_REPAIRS ( j ) );

	if ( GET_OBJ_MAX_QUALITY ( j ) > 0 )
		ch->Send ( "Max. quality: %s (%.3f)\r\n", MAX_QUALITY_NAME ( j ), GET_OBJ_MAX_QUALITY ( j ) );

	/*
	 * I deleted the "equipment status" code from here because it seemed
	 * more or less useless and just takes up valuable screen space.
	 */

	if ( j->contains )
	{
		ch->Send ( "\r\nContents:%s", CCGRN ( ch, C_NRM ) );
		for ( found = 0, j2 = j->contains; j2; j2 = j2->next_content )
		{
			ch->Send ( "%s %s", found++ ? "," : "",
			           j2->short_description );
			if ( strlen ( buf ) >= 62 )
			{
				if ( j2->next_content )
					ch->Send ( ",\r\n" );
				else
					ch->Send ( "\r\n" );
			}
		}

		if ( found )
			ch->Send ( "\r\n%s", CCNRM ( ch, C_NRM ) );
	}
	found = 0;
	ch->Send ( "Affections:" );
	for ( i = 0; i < MAX_OBJ_AFFECT; i++ )
		if ( j->affected[i].modifier )
		{
			sprinttype ( j->affected[i].location, apply_types, buf2, sizeof ( buf2 ) );
			ch->Send ( "%s %+d to %s", found++ ? "," : "",
			           j->affected[i].modifier, buf2 );
		}
	if ( !found )
		ch->Send ( " None" );

	ch->Send ( "\r\n" );

	list_destinations ( TRAVEL_LIST ( j ), ch );
	list_obj_resets ( j, ch );

	/* check the object for a script */
	if ( GET_LEVEL ( ch ) >= LVL_CRT /* || is_name(GET_NAME(ch), zone_table[(real_zone(vnum))].builders)*/ )
		do_sstat_object ( ch, j );
}


void do_stat_character ( Character *ch, Character *k )
{
	int i, i2, found = 0;
	struct obj_data *j;
	struct follow_type *fol;
	struct affected_type *aff;
	int speed_update ( Character *ch );
	char *clan_name ( int idnum );
	char buf[MAX_INPUT_LENGTH];
	char buf1[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];
	int len = 0;
	int zone;
	char masteries[40] = "";
	size_t masteries_len = 0;

	new_mudlog ( CMP, ( GET_LEVEL ( ch ) == LVL_IMPL ? GET_LEVEL ( ch ) : GET_LEVEL ( ch ) +1 ), TRUE, "(GC) %s statted %s [%d].", GET_NAME ( ch ),  GET_NAME ( k ), GET_MOB_VNUM ( k ) );

	if ( !show_vars )
	{
		sprinttype ( GET_SEX ( k ), genders, buf, sizeof ( buf ) );
		ch->Send ( "Sex: %s  (%s) '%s'  IDNum: [%5ld], In room [%5d] \r\n",
		           buf, ( !IS_NPC ( k ) ? "PC" : ( !IS_MOB ( k ) ? "NPC" : "MOB" ) ),
		           GET_NAME ( k ),IS_NPC ( k ) ? GET_ID ( k ) : GET_IDNUM ( k ), GET_ROOM_VNUM ( IN_ROOM ( k ) ) );
		if ( IS_MOB ( k ) )
			ch->Send ( "Alias: %s, VNum: [%5d]\r\n", k->player.name, GET_MOB_VNUM ( k ) );


		if ( !IS_MOB ( k ) )
			ch->Send ( "Title: %s\r\n",
			           ( k->player.title ? k->player.title : "<None>" ) );



		if ( IS_MOB ( k ) )
		{

			ch->Send ( "L-Des: %s",
			           ( k->player.long_descr ? k->player.long_descr : "<None>\r\n" ) );
			zone = real_zone ( GET_MOB_VNUM ( k ) );
			if ( zone > 0 )
			{
				ch->Send ( "Dimension: %s\r\n",dimension_types[zone_table[zone].dimension] );
				ch->Send ( "ZONE: (%-3d) %-30s [%s] \r\n",
				           zone_table[zone].number, zone_table[zone].name,zone_table[zone].builders );
			}
		}


		ch->Send ( "Class: %s", simple_class_name ( k ) );
		if ( IS_NPC ( k ) )
			ch->Send ( " (Tier: %d)",  MOB_TIER ( k ) );
		else
			ch->Send ( " (Tier: %d)", current_class_is_tier_num ( k ) );


		ch->Send ( ", Race: %s\r\n", race_name ( k ) );



		ch->Send ( "Lev: [%s%2d%s], XP: [%s%15lld%s], Align: [%4d], Speed: [%5d]\r\n",
		           CCYEL ( ch, C_NRM ), GET_LEVEL ( k ), CCNRM ( ch, C_NRM ),
		           CCYEL ( ch, C_NRM ), GET_EXP ( k ), CCNRM ( ch, C_NRM ),
		           GET_ALIGNMENT ( k ), speed_update ( k ) );

		if ( !IS_NPC ( k ) )
		{
#if defined(HAVE_ZLIB)
			if ( k->desc && k->desc->comp )
			{
				if ( k->desc->comp->state >= 2 )
					ch->Send ( "Compression: Enabled    " );
				else
					ch->Send ( "Compression: Disabled    " );
			}
#endif
			if ( k->desc )
			{
				ch->Send ( "Telopt Prompts: %d    ", k->desc->eor );
				ch->Send ( "MXP: %d    ", k->desc->mxp );

				ch->Send ( "Wordwrap: %s - %d\r\n", ONOFF ( PRF_FLAGGED ( k->desc->character, PRF_PAGEWRAP ) ), PAGEWIDTH ( k->desc->character ) );
			}
			ch->Send ( "Total Remorts: %d", REMORTS ( k ) );
			if ( GET_REMORT ( k ) >= 0 )
			{
				ch->Send ( "  Remort: %s", pc_class_types[ ( int ) GET_REMORT ( k ) ] );
			}

			if ( GET_REMORT_TWO ( k ) >= 0 )
			{
				ch->Send ( "  Remort2: %s",pc_class_types[ ( int ) GET_REMORT_TWO ( k ) ] );
			}

			if ( GET_REMORT_THREE ( k ) >= 0 )
			{
				ch->Send ( "  Remort3: %s",pc_class_types[ ( int ) GET_REMORT_THREE ( k ) ] );
			}
			ch->Send ( "\r\n" );

			for ( i = 0; i < NUM_CLASSES; i++ )
				if ( GET_MASTERY ( k, i ) )
					masteries_len += snprintf ( masteries+masteries_len, sizeof ( masteries ) - masteries_len, "%c ", UPPER ( *pc_class_types[i] ) );
			ch->Send ( "MASTERED CLASSES: %s\r\n", ( strcmp ( masteries, "" ) == 0 ? "none" : masteries ) );

			if ( GET_EMAIL ( k ) && *GET_EMAIL ( k ) )
				ch->Send ( "EMAIL: %s\r\n", GET_EMAIL ( k ) );
			if ( GET_NEWBIE_STATUS ( k ) != -1 )
				ch->Send ( "NEWBIE STATUS: %s\r\n", newbie_status[GET_NEWBIE_STATUS ( k ) ] );

			ch->Send ( "Clan %d(%d) Rank: %d - %s\r\n",
			           GET_CLAN ( k ), find_clan_by_id ( GET_CLAN ( k ) ), GET_CLAN_RANK ( k ),
			           clan_name ( find_clan_by_id ( GET_CLAN ( k ) ) ) );

			ch->Send ( "TOKENS: G:%-2d S:%-2d BN:%-2d BS:%-2d      AWARD POINTS: %-3d      TRADE POINTS: %-3d\r\n",
			           GET_GOLD_TOKEN_COUNT ( k ), GET_SILVER_TOKEN_COUNT ( k ),GET_BRONZE_TOKEN_COUNT ( k ), GET_BRASS_TOKEN_COUNT ( k ), update_award ( k ), TRADEPOINTS ( k ) );


			strcpy ( buf1, ( char * ) asctime ( localtime ( & ( k->player.time.birth ) ) ) );
			strcpy ( buf2, ( char * ) asctime ( localtime ( & ( k->player.time.logon ) ) ) );
			buf1[10] = buf2[10] = '\0';

			ch->Send (
			    "Created: [%s], Last Logon: [%s], Played [%dh %dm], Age [%d]\r\n",
			    buf1, buf2, k->player.time.played / 3600,
			    ( ( k->player.time.played % 3600 ) / 60 ), age ( k )->year );

			ch->Send (
			    "Pracs: %d, Int_Learn: %d, Wis_bonus: %d\r\n",
			    GET_PRACTICES ( k ),
			    int_app[GET_INT ( k ) ].learn, wis_app[GET_WIS ( k ) ].bonus );

			/*. Display OLC zone for immorts .*/
			if ( GET_LEVEL ( k ) >= LVL_BUILDER )
			{
				if ( GET_OLC_ZONE ( k ) ==AEDIT_PERMISSION )
					ch->Send ( ", OLC[%sActions%s]", CCCYN ( ch, C_NRM ), CCNRM ( ch, C_NRM ) );
				else if ( GET_OLC_ZONE ( k ) ==NOWHERE )
					ch->Send ( ", OLC[%sOFF%s]", CCCYN ( ch, C_NRM ), CCNRM ( ch, C_NRM ) );
				else
					ch->Send ( ", OLC[%s%d%s]", CCCYN ( ch, C_NRM ), GET_OLC_ZONE ( k ), CCNRM ( ch, C_NRM ) );
			}
			ch->Send ( "\r\n" );
		}
		else
		{
			if ( k->mob_specials.head_join != NULL )
			{
				ch->Send ( "[{cWCombined Mob Segment of %s{c0]\r\n", GET_NAME ( k->mob_specials.head_join ) );
			}
			else
			{
				struct combine_data *joinpt = k->mob_specials.join_list;
				if ( joinpt != NULL )
				{
					ch->Send ( "[{cWCombined Mob Head{c0]\r\nVnums of Segments: " );
					while ( joinpt )
					{
						ch->Send ( "%d ", joinpt->vnum );
						joinpt = joinpt->next;
					}
					ch->Send ( "\r\n" );
				}
			}
		}
		ch->Send ( "Str: [%s%d/%d%s]  Int: [%s%d%s]  Wis: [%s%d%s]  "
		           "Dex: [%s%d%s]  Con: [%s%d%s]  Cha: [%s%d%s]\r\n",
		           CCCYN ( ch, C_NRM ), GET_STR ( k ), GET_ADD ( k ), CCNRM ( ch, C_NRM ),
		           CCCYN ( ch, C_NRM ), GET_INT ( k ), CCNRM ( ch, C_NRM ),
		           CCCYN ( ch, C_NRM ), GET_WIS ( k ), CCNRM ( ch, C_NRM ),
		           CCCYN ( ch, C_NRM ), GET_DEX ( k ), CCNRM ( ch, C_NRM ),
		           CCCYN ( ch, C_NRM ), GET_CON ( k ), CCNRM ( ch, C_NRM ),
		           CCCYN ( ch, C_NRM ), GET_CHA ( k ), CCNRM ( ch, C_NRM ) );

		ch->Send (
		    "Hit p.:[%s%d/%d+%d%s]  Mana p.:[%s%d/%d+%d%s]  Move p.:[%s%d/%d+%d%s] Stam.p[%s%d/%d+%d%s]\r\n",
		    CCGRN ( ch, C_NRM ), GET_HIT ( k ), GET_MAX_HIT ( k ), hit_gain ( k ),
		    CCNRM ( ch, C_NRM ), CCGRN ( ch, C_NRM ), GET_MANA ( k ),
		    GET_MAX_MANA ( k ), mana_gain ( k ), CCNRM ( ch, C_NRM ), CCGRN ( ch,   C_NRM ),
		    GET_MOVE ( k ), GET_MAX_MOVE ( k ), move_gain ( k ), CCNRM ( ch, C_NRM ),
		    CCGRN ( ch, C_NRM ), GET_STAMINA ( k ), GET_MAX_STAMINA ( k ), stamina_gain ( k ), CCNRM ( ch, C_NRM ) );

		ch->Send ( "Coins: [%9lld], Bank: [%9lld] (Total: %lld)\r\n",
		           k->Gold ( 0, GOLD_HAND ), k->Gold ( 0, GOLD_BANK ), k->Gold ( 0, GOLD_ALL ) );

		ch->Send (
		    "AC: [%d%+d/10], Hitroll: [%3d], Damroll: [%3d], Saving throws: [%d/%d/%d/%d/%d]\r\n",
		    GET_AC ( k ), dex_app[GET_DEX ( k ) ].defensive, k->points.hitroll,
		    k->points.damroll, GET_SAVE ( k, 0 ), GET_SAVE ( k, 1 ), GET_SAVE ( k, 2 ),
		    GET_SAVE ( k, 3 ), GET_SAVE ( k, 4 ) );

		sprinttype ( GET_POS ( k ), position_types, buf2, sizeof ( buf2 ) );
		ch->Send ( "Pos: %s, Fighting: %s", buf2,
		           ( FIGHTING ( k ) ? GET_NAME ( FIGHTING ( k ) ) : "Nobody" ) );

		if ( IS_NPC ( k ) )
			ch->Send ( ", Attack type: %s", attack_hit_text[k->mob_specials.attack_type].singular );

		if ( k->desc )
		{
			sprinttype ( STATE ( k->desc ), connected_types, buf2, sizeof ( buf2 ) );
			ch->Send ( ", Connected: %s\r\n", buf2 );
		}
		else
			ch->Send ( "\r\n" );

		if ( k->pet != -1 )
			ch->Send ( "Owns pet vnum: %d\r\n", k->pet );

		ch->Send ( "Default position: " );
		sprinttype ( ( k->mob_specials.default_pos ), position_types, buf2, sizeof ( buf2 ) );
		ch->Send ( "%s", buf2 );

		ch->Send ( ", Idle Timer (in tics) [%d]\r\n",
		           k->char_specials.timer );

		if ( IS_NPC ( k ) )
		{
			sprintbitarray ( MOB_FLAGS ( k ), action_bits, PM_ARRAY_MAX, buf2, sizeof ( buf2 ) );
			ch->Send ( "NPC flags: %s%s%s\r\n", CCCYN ( ch, C_NRM ), buf2,
			           CCNRM ( ch, C_NRM ) );
		}
		else
		{
			sprintbitarray ( PLR_FLAGS ( k ), player_bits, PM_ARRAY_MAX, buf2, sizeof ( buf2 ) );
			ch->Send ( "PLR: %s%s%s\r\n", CCCYN ( ch, C_NRM ), buf2, CCNRM ( ch, C_NRM ) );
			sprintbitarray ( PRF_FLAGS ( k ), preference_bits, PM_ARRAY_MAX, buf2, sizeof ( buf2 ) );
			ch->Send ( "PRF: %s%s%s\r\n", CCGRN ( ch, C_NRM ), buf2, CCNRM ( ch, C_NRM ) );


			if ( ( GET_LEVEL ( k ) >= LVL_GOD ) && ( GET_LEVEL ( k ) <= LVL_IMPL ) )
			{

				sprintbit ( CMD_FLAGS ( k ), wiz_groups, buf2, sizeof ( buf2 ) );
				ch->Send ( "WIZ-SAVE: %s%s%s\r\n", CCYEL ( ch, C_NRM ), buf2,
				           CCNRM ( ch, C_NRM ) );

				sprintbit ( CMD_FLAGS2 ( k ), wiz_groups, buf2, sizeof ( buf2 ) );
				ch->Send ( "WIZ-NOSAVE: %s%s%s\r\n", CCYEL ( ch, C_NRM ), buf2,
				           CCNRM ( ch, C_NRM ) );
			}
		}
		if ( !IS_NPC ( k ) && PREG ( k ) > 0 )
			ch->Send ( "%d hours away from giving birth.\r\n", PREG ( k ) );
		if ( IS_MOB ( k ) && MobIndexExists ( GET_MOB_VNUM ( k ) ) )
			ch->Send ( "Mob Spec-Proc: %s",
			           ( GetMobIndex ( GET_MOB_VNUM ( k ) )->func != NULL ? "Exists" : "None" ) );
		if ( IS_MOB ( k ) ) {
			ch->Send ( ", NPC Bare Hand Dam: %dd%d\r\n",
			           k->mob_specials.damnodice, k->mob_specials.damsizedice );
		if (!k->mob_specials.teaches_skills.empty())
			ch->Send ("It can teach these skills and spells:\r\n");
		for ( i = 0; i < k->mob_specials.teaches_skills.size();i++ )
			{
				ch->Send ( "%s[%5d] {cg%-15s{c0%s", 
				! ( i%2 ) ? "             " : " ",
				k->mob_specials.teaches_skills[i], 
				skill_name ( k->mob_specials.teaches_skills[i] ), 
				! ( i%2 ) ? " " : "\r\n" );
				
			}
			ch->Send ( "%s" , !( i%2 ) ? "" : "\r\n" );
}

		ch->Send ( "Carried: weight: %d, items: %d; ",   IS_CARRYING_W ( k ), IS_CARRYING_N ( k ) );

		for ( i = 0, j = k->carrying; j; j = j->next_content, i++ )
			;
		ch->Send ( "Items in: inventory: %d, ", i );

		for ( i = 0, i2 = 0; i < NUM_WEARS; i++ )
			if ( HAS_BODY ( k, i ) && GET_EQ ( k, i ) )
				i2++;
		ch->Send ( "eq: %d\r\n", i2 );

		if ( !IS_NPC ( k ) )
			ch->Send ( "Hunger: %d, Thirst: %d, Drunk: %d\r\n",
			           GET_COND ( k, FULL ), GET_COND ( k, THIRST ), GET_COND ( k,DRUNK ) );

		if ( !IS_NPC ( k ) )
			ch->Send ( "Rip: [%d], Kills: [%d], Death Traps: [%d]\r\n", GET_RIP_CNT ( k ), GET_KILL_CNT ( k ), GET_DT_CNT ( k ) );

		ch->Send ( "Master is: %s, Followers are:",
		           ( ( k->master ) ? GET_NAME ( k->master ) : "<none>" ) );

		for ( fol = k->followers; fol; fol = fol->next )
		{
			len += ch->Send ( "%s %s", found++ ? "," : "",PERS ( fol->follower, ch ) );
			if ( len >= 62 )
			{
				if ( fol->next )
					len += ch->Send ( ",\r\n" );
				else
					len += ch->Send ( "\r\n" );
				len = 0;
			}
		}

		ch->Send ( "\r\n" );

		if ( IS_MOB ( k ) && MOB_SKIN ( k ) !=  -1 )
		{
			i = real_object ( MOB_SKIN ( k ) );
			ch->Send ( "Skin: [%d] %s\r\n", MOB_SKIN ( k ), i < 0 ? "bad vnum" : obj_proto[ i ].short_description );
		}

		/* Showing the bitvector */
		sprintbitarray ( AFF_FLAGS ( k ), affected_bits, AF_ARRAY_MAX, buf2, sizeof ( buf2 ) );
		ch->Send ( "AFF: %s%s%s\r\n", CCYEL ( ch, C_NRM ), buf2,
		           CCNRM ( ch, C_NRM ) );

		/* Routine to show what spells a char is affected by */
		if ( k->affected )
		{
			for ( aff = k->affected; aff; aff = aff->next )
			{
				*buf2 = '\0';
				if ( aff->expire == -2 )
					ch->Send ( "SPL: ( innate) %s%-21s%s ", CCCYN ( ch, C_NRM ),
					           skill_name ( aff->type ), CCNRM ( ch, C_NRM ) );
				else
					ch->Send ( "SPL: (%4ldsec)  %s%-21s%s ", time_to_sec ( aff->expire + 1 ),
					           CCCYN ( ch, C_NRM ), skill_name ( aff->type ), CCNRM ( ch, C_NRM ) );
				if ( aff->modifier )
					ch->Send ( "%+d to %s", aff->modifier, apply_types[ ( int ) aff->location] );

				if ( aff->bitvector )
				{
					if ( aff->modifier )
						ch->Send ( ", sets " );
					else
						ch->Send ( "sets " );
					ch->Send ( "%s", affected_bits[aff->bitvector] );
				}
				ch->Send ( "\r\n" );
			}
		}
		list_destinations ( TRAVEL_LIST ( k ), ch );
		list_mob_resets ( k, ch );
		ch->Send ( "To see global variables: type\r\nvstat player <name>\r\n" );
		if ( GET_LEVEL ( ch ) == LVL_IMPL && k->desc )
		{
			int m, cnt = 0;
			ch->Send ( "Last commands typed, oldest at top, newest at bottom:\r\n" );
			for ( m = 0; m < HISTORY_SIZE;m++ )
			{
				if ( k->desc->history[m] )
					ch->Send ( "{cY%d:{cg %s{c0\r\n", cnt++, k->desc->history[m] );

			}
		}
	}

	/* check mobiles for a script */
	if ( !show_vars && IS_NPC ( k ) && ( GET_LEVEL ( ch ) >= LVL_CRT ) )
	{
		do_sstat_character ( ch, k );
		if ( SCRIPT_MEM ( k ) )
		{
			struct script_memory *mem = SCRIPT_MEM ( k );
			ch->Send ( "Script memory:\r\n  Remember             Command\r\n" );
			while ( mem )
			{
				Character *mc = find_char ( mem->id );
				if ( !mc )
					ch->Send ( "  ** Corrupted!\r\n" );
				else
				{
					if ( mem->cmd )
						ch->Send ( "  %-20.20s%s\r\n", GET_NAME ( mc ), mem->cmd );
					else
						ch->Send ( "  %-20.20s <default>\r\n", GET_NAME ( mc ) );
				}
				mem = mem->next;
			}
		}
	}
	else
	{
		/* this is a PC, display their global variables */
		if ( k->script && k->script->global_vars && show_vars )
		{
			struct trig_var_data *tv;
			char uname[MAX_INPUT_LENGTH];
			char buffer[MAX_INPUT_LENGTH];
			DYN_DEFINE;
			DYN_CREATE;
			*dynbuf=0;
			snprintf ( buffer,MAX_INPUT_LENGTH,"Global Variables for %s:\r\n", GET_NAME ( k ) );
			DYN_RESIZE ( buffer );
			//        ch->Send( "Global Variables for %s:\r\n", GET_NAME(k));

			/* currently, variable context for players is always 0, so it is */
			/* not displayed here. in the future, this might change */

			for ( tv = k->script->global_vars; tv; tv = tv->next )
			{
				if ( tv->value[0] == UID_CHAR )
				{
					find_uid_name ( tv->value, uname, sizeof ( uname ) );
					snprintf ( buffer,MAX_INPUT_LENGTH,"    %40s:  [UID]: %20s\r\n", tv->name.c_str(),
					           uname );
					DYN_RESIZE ( buffer );
					//            ch->Send( "    %40s:  [UID]: %20s\r\n", tv->name,
					//                             uname);
				}
				else
				{
					snprintf ( buffer,MAX_INPUT_LENGTH,"    %40s:  %20s\r\n", tv->name.c_str(),
					           tv->value.c_str() );
					DYN_RESIZE ( buffer );
					//           ch->Send( "    %40s:  %20s\r\n", tv->name,
					//                            tv->value);
				}
			}
			page_string ( ch->desc, dynbuf, DYN_BUFFER );
		}
	}

}


ACMD ( do_stat )
{
	Character *victim;
	struct obj_data *object;
	char buf1[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];
	//int tmp;

	half_chop ( argument, buf1, buf2 );

	if ( !*buf1 )
	{
		ch->Send ( "Stats on who or what?\r\n" );
		return;

	}
	else if ( is_abbrev ( buf1, "room" ) )
	{
		do_stat_room ( ch );
	}
	else if ( is_abbrev ( buf1, "mob" ) )
	{
		if ( !*buf2 )
		{
			ch->Send ( "Stats on which mobile?\r\n" );
			return;
		}
		else
		{
			if ( ( victim =
			            get_char_vis ( ch, buf2, NULL, FIND_CHAR_WORLD ) ) != NULL )
				do_stat_character ( ch, victim );
			else
			{
				ch->Send ( "No such mobile around.\r\n" );
				return;
			}
		}
	}
	else if ( is_abbrev ( buf1, "player" ) )
	{
		if ( !*buf2 )
		{
			ch->Send ( "Stats on which player?\r\n" );
			return;
		}
		else
		{
			if ( ( victim =
			            get_player_vis ( ch, buf2, NULL, FIND_CHAR_WORLD ) ) != NULL )
				do_stat_character ( ch, victim );
			else
			{
				ch->Send ( "No such player around.\r\n" );
				return;
			}
		}
	}
	else if ( is_abbrev ( buf1, "file" ) )
	{
		if ( !*buf2 )
		{
			ch->Send ( "Stats on which player?\r\n" );
			return;
		}
		else if ( !pi.IdByName ( buf1 ) )
		{
			send_to_char ( "There is no such player.\r\n", ch );
			return;
		}
		else
		{
			victim = new Character ( FALSE );
			TEMP_LOAD_CHAR = TRUE;
			if ( store_to_char ( buf2, victim ) > -1 )
			{
				if ( GET_LEVEL ( victim ) > GET_LEVEL ( ch ) )
				{
					ch->Send ( "Sorry, you can't do that.\r\n" );
					delete ( victim );
					TEMP_LOAD_CHAR = FALSE;
					return;
				}
				else
					do_stat_character ( ch, victim );
				delete ( victim );
			}
			else
			{
				ch->Send ( "There is no such player.\r\n" );
				delete ( victim );
				TEMP_LOAD_CHAR = FALSE;
				return;
			}
			TEMP_LOAD_CHAR = FALSE;
		}
	}
	else if ( is_abbrev ( buf1, "object" ) )
	{
		if ( !*buf2 )
		{
			ch->Send ( "Stats on which object?\r\n" );
			return;
		}
		else
		{
			if ( ( object = get_obj_vis ( ch, buf2, NULL ) ) != NULL )
				do_stat_object ( ch, object );
			else
			{
				ch->Send ( "No such object around.\r\n" );
				return;
			}
		}
	}
	else if ( is_abbrev ( buf1, "zone" ) )
	{
		if ( !*buf2 )
		{
			ch->Send ( "Stats on which zone?\r\n" );
			return;
		}
		else
		{
			zone_rnum rn = real_zone ( atoi ( buf2 ) );
			print_zone ( ch, rn );
			return;
		}
	}
	else
	{
		char *name = buf1;
		int num = get_number ( &name );

		if ( ( object =
		            get_obj_in_equip_vis ( ch, name, &num,
		                                   ch->equipment ) ) != NULL )
			do_stat_object ( ch, object );
		else if ( ( object =
		                get_obj_in_list_vis ( ch, name, &num,
		                                      ch->carrying ) ) != NULL )
			do_stat_object ( ch, object );
		else if ( ( victim =
		                get_char_vis ( ch, name, &num, FIND_CHAR_ROOM ) ) != NULL )
			do_stat_character ( ch, victim );
		else if ( ( object =
		                get_obj_in_list_vis ( ch, name, &num,
		                                      IN_ROOM ( ch )->contents ) ) !=
		          NULL )
			do_stat_object ( ch, object );
		else if ( ( victim =
		                get_char_vis ( ch, name, &num,
		                               FIND_CHAR_WORLD ) ) != NULL )
			do_stat_character ( ch, victim );
		else if ( ( object = get_obj_vis ( ch, name, &num ) ) != NULL )
			do_stat_object ( ch, object );
		else
		{
			ch->Send ( "Nothing around by that name.\r\n" );
			return;
		}
	}

}

ACMD ( do_account )
{
	char name[MAX_INPUT_LENGTH];
	int acc, pos, i = 0;
	one_argument ( argument, name );
	if ( !*name || GET_LEVEL ( ch ) < LVL_IMMORT )
	{
		try
		{
			pos = pi.TableIndexByName ( GET_NAME ( ch ) );
		}
		catch ( MudException &e )
		{
			ch->Send ( "%s%s", e.Message(), "\r\n" );
			return;
		}
	}
	else
	{
		try
		{
			pos = pi.TableIndexByName ( name );
		}
		catch ( MudException &e )
		{
			ch->Send ( "%s%s", e.Message(), "\r\n" );
			return;
		}
	}
	acc = pi.AccByIndex ( pos );

	for ( pos = 0; pos <= pi.TopOfTable(); pos++ )
	{
		if ( !pi.DeletedByIndex ( pos ) && pi.AccByIndex ( pos ) == acc )
		{
			ch->Send ( "{cy%2d{cg) %-15s%s{c0",
			           i, pi.NameByIndex ( pos ), ( ( i+1 ) %2 ) == 0 ? "\r\n" : "   " );
			i++;
		}
	}


}

ACMD ( do_shutdown )
{
	char arg[MAX_INPUT_LENGTH];
	Descriptor *d;
	if ( subcmd != SCMD_SHUTDOWN )
	{
		send_to_char ( "If you want to shut something down, say so!\r\n",
		               ch );
		return;
	}
	one_argument ( argument, arg );

	//saving all zones
	save_all();

	if ( !*arg )
	{
		log ( "(GC) Shutdown by %s.", GET_NAME ( ch ) );
		send_to_all ( "Shutting down.\r\n" );
		circle_shutdown = 1;
	}
	else if ( !str_cmp ( arg, "reboot" ) )
	{
		log ( "(GC) Reboot by %s.", GET_NAME ( ch ) );
		send_to_all ( "Rebooting.. come back in a minute or two.\r\n" );
		touch ( FASTBOOT_FILE );
		circle_shutdown = circle_reboot = 1;
	}
	else if ( !str_cmp ( arg, "now" ) )
	{
		log ( "(GC) Shutdown NOW by %s.", GET_NAME ( ch ) );
		send_to_all ( "Rebooting.. come back in a minute or two.\r\n" );
		circle_shutdown = 1;
		circle_reboot = 2;
	}
	else if ( !str_cmp ( arg, "die" ) )
	{
		log ( "(GC) Shutdown by %s.", GET_NAME ( ch ) );
		send_to_all ( "Shutting down for maintenance.\r\n" );
		touch ( KILLSCRIPT_FILE );
		circle_shutdown = 1;
	}
	else if ( !str_cmp ( arg, "pause" ) )
	{
		log ( "(GC) Shutdown by %s.", GET_NAME ( ch ) );
		send_to_all ( "Shutting down for maintenance.\r\n" );
		touch ( PAUSE_FILE );
		circle_shutdown = 1;
	}
	else
		send_to_char ( "Unknown shutdown option.\r\n", ch );

	for ( d = descriptor_list; d; d = d->next )
	{
		if ( d->character )
			d->character->save();
	}
	Crash_save_all();

}


void stop_snooping ( Character *ch )
{
	if ( !ch->desc->snooping )
		send_to_char ( "You aren't snooping anyone.\r\n", ch );
	else
	{
		send_to_char ( "You stop snooping.\r\n", ch );
		ch->desc->snooping->snoop_by = NULL;
		ch->desc->snooping = NULL;
	}
}


ACMD ( do_snoop )
{
	Character *victim, *tch;
	char arg[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	char buf[MAX_INPUT_LENGTH];


	if ( !ch->desc )
		return;

	argument = one_argument ( argument, arg );
        one_argument(argument, arg2);
        

	if ( !*arg )
		stop_snooping ( ch );
	else if ( ! ( victim = get_char_vis ( ch, arg, NULL, FIND_CHAR_WORLD ) ) )
		ch->Send ( "No such person around.\r\n" );
	else if ( !victim->desc )
		ch->Send ( "There's no link.. nothing to snoop.\r\n" );
	else if ( victim == ch )
		stop_snooping ( ch );
	else if ( victim->desc->snoop_by )
		ch->Send ( "Busy already. \r\n" );
	else if ( victim->desc->snooping == ch->desc )
		ch->Send ( "Don't be stupid.\r\n" );
	else
	{
		if ( victim->desc->original )
			tch = victim->desc->original;
		else
			tch = victim;

		if ( ( GET_ORIG_LEV ( tch ) ? GET_ORIG_LEV ( tch ) : GET_LEVEL ( tch ) ) > GET_LEVEL ( ch ) )
		{
			if ( GET_ORIG_LEV ( tch ) )
				new_send_to_char ( tch, "%s tried to snoop you but couldn't.\r\n", GET_NAME ( ch ) );
			ch->Send ( "You can't.\r\n" );
			return;
		}

                if (GET_LEVEL(ch) < LVL_IMPL || !str_cmp(arg2, "yes")) {
                    sprintf(buf, "%s wishes to snoop you: Allow? (Type: Y | N)", GET_NAME(ch));
                    victim->loader = GET_IDNUM(ch);
                    line_input(victim->desc, buf, allow_snoop, NULL); 
		    ch->Send ( "%s", CONFIG_OK );
                    return;
                }

		if ( ch->desc->snooping )
			ch->desc->snooping->snoop_by = NULL;

		ch->desc->snooping = victim->desc;
		victim->desc->snoop_by = ch->desc;
		ch->Send ( "%s", CONFIG_OK );
	}
}



ACMD ( do_switch )
{
	Character *victim;
	char arg[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );

	if ( ch->desc->original )
		ch->Send ( "You're already switched.\r\n" );
	else if ( !*arg )
		ch->Send ( "Switch with who?\r\n" );
	else if ( ! ( victim = get_char_vis ( ch, arg, NULL, FIND_CHAR_WORLD ) ) )
		ch->Send ( "No such character.\r\n" );
	else if ( ch == victim )
		ch->Send ( "Hee hee... we are jolly funny today, eh?\r\n" );
	else if ( victim->desc )
		ch->Send ( "You can't do that, the body is already in use!\r\n" );
	else if ( ( GET_LEVEL ( ch ) < LVL_IMPL ) && !IS_NPC ( victim ) )
		ch->Send ( "You aren't holy enough to use a mortal's body.\r\n" );
	else if ( GET_LEVEL ( ch ) < LVL_GRGOD
	          && ROOM_FLAGGED ( IN_ROOM ( victim ), ROOM_GODROOM ) )
		ch->Send ( "You are not godly enough to use that room!\r\n" );
	else if ( GET_LEVEL ( ch ) < LVL_GRGOD
	          && ROOM_FLAGGED ( IN_ROOM ( victim ), ROOM_HOUSE )
	          && !House_can_enter ( ch, GET_ROOM_VNUM ( IN_ROOM ( victim ) ) ) )
		ch->Send ( "That's private property -- no trespassing!\r\n" );
	else
	{
		ch->Send ( "%s", CONFIG_OK );

		ch->desc->character = victim;
		ch->desc->original = ch;

		victim->desc = ch->desc;
		ch->desc = NULL;
	}
}


ACMD ( do_return )
{
	if ( ch->desc && ch->desc->original )
	{
		send_to_char ( "You return to your original body.\r\n", ch );

		/*
		 * If someone switched into your original body, disconnect them.
		 *   - JE 2/22/95
		 *
		 * Zmey: here we put someone switched in our body to disconnect state
		 * but we must also NULL his pointer to our character, otherwise
		 * close_socket() will damage our character's pointer to our descriptor
		 * (which is assigned below in this function). 12/17/99
		 */
		if ( ch->desc->original->desc )
		{
			ch->desc->original->desc->character = NULL;
			STATE ( ch->desc->original->desc ) = CON_DISCONNECT;
		}

		/* Now our descriptor points to our original body. */
		ch->desc->character = ch->desc->original;
		ch->desc->original = NULL;

		/* And our body's pointer to descriptor now points to our descriptor. */
		ch->desc->character->desc = ch->desc;
		ch->desc = NULL;
		return;
	}
	if ( GET_ORIG_LEV ( ch ) )
	{
		GET_LEVEL ( ch ) = GET_ORIG_LEV ( ch );
		GET_ORIG_LEV ( ch ) = 0;
		ch->Send ( "You return to your original level.\r\n" );
	}
	else
	{
		ch->Send ( "Um... no.\r\n" );
	}

}



ACMD ( do_load )
{
	Character *mob = NULL;
	struct obj_data *obj = NULL;
	char timesb[MAX_STRING_LENGTH];
	char *times = timesb;
	int tnum = 0;
	mob_vnum num;
	char buf[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];

	times = two_arguments ( argument, buf, buf2 );
	tnum = ( atoi ( times ) );

	if ( !*buf || !*buf2 || !isdigit ( *buf2 ) )
	{
		ch->Send ( "Usage: load {{ obj | mob } <number> [<amount>]\r\n" );
		return;
	}

	if ( !is_number ( buf2 ) )
	{
		ch->Send ( "That is not a number.\r\n" );
		return;
	}

	if ( ( num = atoi ( buf2 ) ) < 0 )
	{
		ch->Send ( "A NEGATIVE number??\r\n" );
		return;
	}
	if ( is_abbrev ( buf, "mob" ) )
	{
		if ( !MobProtoExists ( num ) )
		{
			ch->Send ( "There is no monster with the number %d.\r\n", num );
			return;
		}
		for ( ( tnum <= 0 ? tnum = 1 : tnum ) ;tnum>0; tnum-- )
		{
			mob = read_mobile ( num );
			char_to_room ( mob, IN_ROOM ( ch ) );
		}

		act ( "$n makes a quaint, magical gesture with one hand.", TRUE, ch, 0, 0, TO_ROOM );
		act ( "$n has created $N!", FALSE, ch, 0, mob, TO_ROOM );
		act ( "You create $N.", FALSE, ch, 0, mob, TO_CHAR );
		load_mtrigger ( mob );
	}
	else if ( is_abbrev ( buf, "obj" ) )
	{
		obj_rnum r_num;
		if ( ( r_num = real_object ( num ) ) < 0 )
		{
			ch->Send ( "There is no object with the number %d.\r\n",num );
			return;
		}

		/* Are we allowed to load QIC? */
		if ( obj_index[r_num].qic != NULL && GET_LEVEL ( ch ) < LVL_IMPL )
		{
			ch->Send ( "You don't have the skill to create a new Artifact.\r\n" );
			return;
		}
		for ( ( tnum <= 0 ? tnum = 1 : tnum ) ;tnum>0; tnum-- )
		{
			obj = read_object ( r_num, REAL );
			if ( obj == NULL )
			{
				ch->Send ( "You are not allowed to load QIC's over the limit.\r\n" );
				return;
			}

			if ( CONFIG_LOAD_INVENTORY )
				obj_to_char ( obj, ch );
			else
				obj_to_room ( obj, IN_ROOM ( ch ) );
		}
		act ( "$n makes a strange magical gesture.", TRUE, ch, 0, 0, TO_ROOM );
		act ( "$n has created $p!", FALSE, ch, obj, 0, TO_ROOM );
		act ( "You create $p.", FALSE, ch, obj, 0, TO_CHAR );
		load_otrigger ( obj );
		log ( "%s loaded %s", GET_NAME ( ch ), obj->name );

	}
	else
		send_to_char ( "That'll have to be either 'obj' or 'mob'.\r\n", ch );

}



ACMD ( do_vstat )
{
	Character *mob;
	struct obj_data *obj;
	mob_vnum num;          /* or obj_vnum ... */
	mob_rnum r_num;        /* or obj_rnum ... */
	char buf[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];

	two_arguments ( argument, buf, buf2 );

	if ( !*buf || !*buf2 )
	{
		send_to_char ( "Usage: vstat {{ obj | mob } <number>\r\n"
		               "Or:    vstat player <name>  (this displays variables)\r\n", ch );
		return;
	}
	if ( is_abbrev ( buf, "player" ) )
	{
		if ( ( mob =
		            get_player_vis ( ch, buf2, NULL, FIND_CHAR_WORLD ) ) != NULL )
		{
			show_vars = TRUE;
			do_stat_character ( ch, mob );
			show_vars = FALSE;
		}
		else
			ch->Send ( "No such player around.\r\n" );
		return;
	}
	if ( ( num = atoi ( buf2 ) ) < 0 )
	{
		ch->Send ( "A NEGATIVE number??\r\n" );
		return;
	}
	if ( is_abbrev ( buf, "mob" ) )
	{
		if ( !MobProtoExists ( num ) )
		{
			ch->Send ( "There is no monster with the number %d.\r\n", num );
			return;
		}
		mob = read_mobile ( num );
		char_to_room ( mob, world_vnum[0] );
		do_stat_character ( ch, mob );
		extract_char ( mob );
	}
	else if ( is_abbrev ( buf, "obj" ) )
	{
		if ( ( r_num = real_object ( num ) ) < 0 )
		{
			ch->Send ( "There is no object with the number %d.\r\n", num );
			return;
		}
		obj = read_object ( r_num, REAL );
		do_stat_object ( ch, obj );
		extract_obj ( obj );
	}
	else
		send_to_char ( "That'll have to be either 'obj' or 'mob'.\r\n", ch );
}




/* clean a room of all mobiles and objects */
ACMD ( do_purge )
{
	Character *vict, *next_v;
	struct obj_data *obj, *next_o;
	char buf[MAX_INPUT_LENGTH];

	one_argument ( argument, buf );

	if ( *buf ) {            /* argument supplied. destroy single object
                                                                                                                                                                 * or char */
		if ( ( vict = get_char_vis ( ch, buf, NULL, FIND_CHAR_ROOM ) ) )
		{
			if ( !IS_NPC ( vict ) && ( GET_LEVEL ( ch ) <= GET_LEVEL ( vict ) ) )
			{
				ch->Send ( "Fuuuuuuuuu!\r\n" );
				return;
			}
			act ( "$n disintegrates $N.", FALSE, ch, 0, vict, TO_NOTVICT );

			if ( !IS_NPC ( vict ) )
			{
				new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) %s has purged %s.", GET_NAME ( ch ), GET_NAME ( vict ) );
				if ( vict->desc )
				{
					STATE ( vict->desc ) = CON_CLOSE;
					vict->desc->character = NULL;
					vict->desc = NULL;
				}
			}
			extract_char ( vict );
		}
		else
			if ( ( obj =
			            get_obj_in_list_vis ( ch, buf, NULL, IN_ROOM ( ch )->contents ) ) != NULL )
			{
				act ( "$n destroys $p.", FALSE, ch, obj, 0, TO_ROOM );
				extract_obj ( obj );
			}
			else
			{
				send_to_char ( "Nothing here by that name.\r\n", ch );
				return;
			}

		ch->Send ( "%s", CONFIG_OK );
	}
	else              /* no argument. clean out the room */
	{
		act ( "$n gestures... You are surrounded by scorching flames!", FALSE, ch, 0, 0, TO_ROOM );
		send_to_room ( IN_ROOM ( ch ), "The world seems a little cleaner.\r\n" );

		for ( vict = IN_ROOM ( ch )->people; vict; vict = next_v )
		{
			next_v = vict->next_in_room;
			if ( IS_NPC ( vict ) )
				extract_char ( vict );
		}

		for ( obj = IN_ROOM ( ch )->contents; obj; obj = next_o )
		{
			next_o = obj->next_content;
			extract_obj ( obj );
		}
	}
	save_corpses();
}

/* Stop all fighting in a room */
ACMD ( do_peace )
{
	Character *vict, *next_v;

	ch->Send ( "%s",CONFIG_OK );
	act ( "$n gestures... A Calming light flows from the sky.", FALSE, ch, 0,0, TO_ROOM );
	send_to_room ( IN_ROOM ( ch ), "The world seems a little more peaceful.\r\n" );

	for ( vict = IN_ROOM ( ch )->people; vict; vict = next_v )
	{
		next_v = vict->next_in_room;
		stop_fighting ( vict );
		if ( IS_NPC ( vict ) )
			clearMemory ( vict );
	}
}

const char *logtypes[] =
{
	"off", "brief", "normal", "complete", "\n"
};

ACMD ( do_syslog )
{
	int tp;
	char arg[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );

	if ( !*arg )
	{
		tp = ( ( PRF_FLAGGED ( ch, PRF_LOG1 ) ? 1 : 0 ) + ( PRF_FLAGGED ( ch, PRF_LOG2 ) ? 2 : 0 ) );
		ch->Send ( "Your syslog is currently %s.\r\n", logtypes[tp] );
		return;
	}
	if ( ( ( tp = search_block ( arg, logtypes, FALSE ) ) == -1 ) )
	{
		ch->Send ( "Usage: syslog {{ Off | Brief | Normal | Complete }\r\n" );
		return;
	}
	REMOVE_BIT_AR ( PRF_FLAGS ( ch ), PRF_LOG1 );
	REMOVE_BIT_AR ( PRF_FLAGS ( ch ), PRF_LOG2 );
	if ( tp & 1 )
		SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_LOG1 );
	if ( tp & 2 )
		SET_BIT_AR ( PRF_FLAGS ( ch ), PRF_LOG2 );

	ch->Send ( "Your syslog is now %s.\r\n", logtypes[tp] );
}

extern int mother_desc;
extern FILE *player_fl;
#define EXE_FILE "bin/circle" /* maybe use argv[0] but it's not reliable */

/* (c) 1996-97 Erwin S. Andreasen <erwin@pip.dknet.dk> */
ACMD ( do_copyover )
{
	FILE *fp;
	Descriptor *d, *d_next;
	char buf[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];
	int process_output ( Descriptor *t );


	fp = fopen ( COPYOVER_FILE, "w" );

	if ( !fp )
	{
		ch->Send ( "Copyover file not writeable, aborted.\r\n" );
		return;
	}

	restore_all_corpses();

	alarm(0);
	save_all(); //done
	/* For each playing descriptor, save its state */
	for ( d = descriptor_list; d; d = d_next )
	{
		Character *och = d->character;
		d_next = d->next;    /* We delete from the list , so need to save this */

		if ( !d->character || !IS_PLAYING ( d ) )
		{
			write_to_descriptor ( d->descriptor, "\n\rSorry, we are rebooting. Come back in a few minutes.\r\n", d->comp );
			delete d;   /* throw'em out */
		}
		else
		{
			char client_name[MAX_INPUT_LENGTH];
			char client_version[MAX_INPUT_LENGTH];

                        strcpy( client_name, d->pProtocol->pVariables[eMSDP_CLIENT_ID]->pValueString );
                        strcpy( client_version, d->pProtocol->pVariables[eMSDP_CLIENT_VERSION]->pValueString );

                        // Make sure there are no spaces.

			int i; // Loop counter.
			for ( i = 0; client_name[i] != '\0'; ++i )
			{
				if ( client_name[i] == ' ' )
					client_name[i] = '_';
			}
			for ( i = 0; client_version[i] != '\0'; ++i )
			{
				if ( client_version[i] == ' ' )
					client_version[i] = '_';
			}
                           
			room_rnum rm = GET_WAS_IN ( d->character ) ? GET_WAS_IN ( d->character ) : IN_ROOM ( d->character );
			fprintf ( fp, "%d %s %s %d %s %s %s\n", d->descriptor, GET_NAME ( och ), d->host.c_str(), rm->number, client_name, client_version, CopyoverGet(d) );

			/* save och */
			if ( !IS_IMM ( och ) )
				GET_LOADROOM ( och ) = GET_ROOM_VNUM ( GET_WAS_IN ( och ) == NULL? IN_ROOM ( och ) : GET_WAS_IN ( och ) );

			log ( "printing descriptor name and host of connected players" );
			och->save();
			Crash_rentsave ( och, 0 );

			REMOVE_BIT_AR ( PLR_FLAGS ( d->character ), PLR_CRASH );

			write_aliases ( och );
			snprintf ( buf, sizeof ( buf ), "The world is engulfed in flames....\r\n" );
#ifdef HAVE_ZLIB_H
			if ( d->comp->state == 2 )
			{
				d->comp->state = 3; /* Code to use Z_FINISH for deflate */
			}
#endif /* HAVE_ZLIB_H */

			write_to_descriptor ( d->descriptor, buf, d->comp );
			d->comp->state = 0;

#ifdef HAVE_ZLIB_H

			if ( d->comp->stream )
			{
				deflateEnd ( d->comp->stream );
				free ( d->comp->stream );
				free ( d->comp->buff_out );
				free ( d->comp->buff_in );
			}

#endif /* HAVE_ZLIB_H */
		}
	}
	fprintf ( fp, "-1\n" );
	fclose ( fp );


	/* exec - descriptors are inherited */

	snprintf ( buf, sizeof ( buf ), "%d", port );
	snprintf ( buf2, sizeof ( buf2 ), "-C%d", mother_desc );

	/* Ugh, seems it is expected we are 1 step above lib - this may be dangerous! */
	chdir ( ".." );
	execl ( EXE_FILE, "circle", buf2, buf, ( char * ) NULL );
	alarm(60);

	/* Failed - sucessful exec will not return */
	perror ( "do_copyover: execl" );

	new_send_to_char ( ch, "Copyover FAILED!\r\n" );
	exit ( 1 );            /* too much trouble to try to recover! */
}

ACMD ( do_advance )
{
	Character *victim;
	char arg[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];
	char *name = arg, *level = buf2;
	int newlevel, oldlevel;

	two_arguments ( argument, name, level );

	if ( *name )
	{
		if ( ! ( victim = get_char_vis ( ch, name, NULL, FIND_CHAR_WORLD ) ) )
		{
			ch->Send ( "That player is not here.\r\n" );
			return;
		}
	}
	else
	{
		ch->Send ( "Advance who?\r\n" );
		return;
	}
	if ( GET_LEVEL ( ch ) <= GET_LEVEL ( victim ) )
	{
		ch->Send ( "Maybe that's not such a great idea.\r\n" );
		return;
	}
	if ( IS_NPC ( victim ) )
	{
		ch->Send ( "NO!  Not on NPC's.\r\n" );
		return;
	}
	if ( !*level || ( newlevel = atoi ( level ) ) <= 0 )
	{
		ch->Send ( "That's not a level!\r\n" );
		return;
	}
	if ( newlevel > LVL_IMPL )
	{
		ch->Send ( "%d is the highest possible level.\r\n", LVL_IMPL );
		return;
	}
	if ( newlevel > GET_LEVEL ( ch ) )
	{
		ch->Send ( "Yeah, right.\r\n" );
		return;
	}
	if ( newlevel == GET_LEVEL ( victim ) )
	{
		ch->Send ( "They are already at that level.\r\n" );
		return;
	}
	oldlevel = GET_LEVEL ( victim );
	if ( newlevel < GET_LEVEL ( victim ) )
	{
		do_start ( victim );
		GET_LEVEL ( victim ) = newlevel;
		send_to_char ( "You are momentarily enveloped by darkness!\r\n"
		               "You feel somewhat diminished.\r\n", victim );
	}
	else
	{
		act ( "$n makes some strange gestures.\r\n"
		      "A strange feeling comes upon you,\r\n"
		      "Like a giant hand, light comes down\r\n"
		      "from above, grabbing your body, that\r\n"
		      "begins to pulse with coloured lights\r\n"
		      "from inside.\r\n\r\n"
		      "Your head seems to be filled with demons\r\n"
		      "from another plane as your body dissolves\r\n"
		      "to the elements of time and space itself.\r\n"
		      "Suddenly a silent explosion of light\r\n"
		      "snaps you back to reality.\r\n\r\n"
		      "You feel slightly different.", FALSE, ch, 0, victim, TO_VICT );
	}

	ch->Send ( "%s", CONFIG_OK );

	if ( newlevel < oldlevel )
		log ( "(GC) %s demoted %s from level %d to %d.",
		      GET_NAME ( ch ), GET_NAME ( victim ), oldlevel, newlevel );
	else
		log ( "(GC) %s has advanced %s to level %d (from %d)",
		      GET_NAME ( ch ), GET_NAME ( victim ), newlevel, oldlevel );

	gain_exp_regardless ( victim, level_exp ( GET_CLASS ( victim ), newlevel, current_class_is_tier_num ( victim ), REMORTS ( victim ) ) - GET_EXP ( victim ), true );
	victim->save();
	// log("(do_advance)Saved %s in room %d", GET_NAME(victim), victim->in_room);
}

ACMD ( do_powerplay )
{
	Descriptor *d;
	Character *tch;
	int count = 0;
	ch->Send ( "Lvl  Class  Room   Name                   Health  Position\r\n" );
	for ( d = descriptor_list; d; d = d->next )

		if ( STATE ( d ) == CON_PLAYING && d->character && d->character != ch )
		{
			tch = d->character;
			if ( !IS_NPC ( tch ) && GET_LEVEL ( tch ) < LVL_IMMORT )
			{
				ch->Send (
				    "%2d  %4s    %-5d  %-20s  %4.4f%%  %s\r\n",
				    GET_LEVEL ( tch ), CLASS_ABBR ( tch ),
				    IN_ROOM ( tch )->number, GET_NAME ( tch ),
				    ( float ) ( GET_HIT ( tch ) * 100 ) /
				    GET_MAX_HIT ( tch ),
				    position_types[ ( int ) GET_POS ( tch ) ] );
				count++;
			}
		}
	if ( !count )
		ch->Send ( "  No mortals are on!\r\n" );
}

ACMD ( do_restore )
{
	Descriptor *d;
	Character *vict;
	int i;
	char buf[MAX_INPUT_LENGTH];

	one_argument ( argument, buf );


	if ( !*buf )
		ch->Send ( "Whom do you wish to restore?\r\n" );
	else if ( ! ( vict = get_char_vis ( ch, buf, NULL, FIND_CHAR_WORLD ) ) )
		ch->Send ( "%s", CONFIG_NOPERSON );
	else if ( !strcmp ( buf, "all" ) )
	{

		for ( d = descriptor_list; d; d = d->next )
			if ( STATE ( d ) == CON_PLAYING && d->character
			        && d->character != ch )
			{
				vict = d->character;
				if ( ( GET_LEVEL ( vict ) < 51 ) )
					ch->Send ( "You restored %s.\r\n", GET_NAME ( vict ) );

				if ( GET_COND ( vict, THIRST ) != ( -1 ) )
					GET_COND ( vict, THIRST ) = 48;
				if ( GET_COND ( vict, FULL ) != ( -1 ) )
					GET_COND ( vict, FULL ) = 48;
				GET_HIT ( vict ) = ( GET_MAX_HIT ( vict ) );
				GET_MANA ( vict ) = ( GET_MAX_MANA ( vict ) );
				GET_MOVE ( vict ) = ( GET_MAX_MOVE ( vict ) );
				GET_STAMINA ( vict ) = ( GET_MAX_STAMINA ( vict ) );

				act ( "You have been fully restored by $N!", FALSE, vict, 0, ch, TO_CHAR );

			}
		GET_POS ( vict ) = POS_STANDING;
		update_pos ( vict );
		vict->check_regen_rates();
		ch->Send ( "%s",CONFIG_OK );
		vict->save();
		return;
	}
	else
	{
		GET_HIT ( vict ) = ( GET_MAX_HIT ( vict ) );
		GET_MANA ( vict ) = ( GET_MAX_MANA ( vict ) );
		GET_MOVE ( vict ) = ( GET_MAX_MOVE ( vict ) );
		GET_STAMINA ( vict ) = ( GET_MAX_STAMINA ( vict ) );

		if ( GET_LEVEL ( ch ) >= LVL_GRGOD && ( GET_LEVEL ( vict ) >= LVL_HERO ) )
		{
			if ( GET_COND ( vict, THIRST ) != ( -1 ) )
				GET_COND ( vict, THIRST ) = ( -1 );
			if ( GET_COND ( vict, FULL ) != ( -1 ) )
				GET_COND ( vict, FULL ) = ( -1 );



			for ( i = 1; i <= MAX_SKILLS; i++ )
				SET_SKILL ( vict, i, 100 );
			vict->real_abils.str_add = 100;
			vict->real_abils.intel = 25;
			vict->real_abils.wis = 25;
			vict->real_abils.dex = 25;
			vict->real_abils.str = 25;
			vict->real_abils.con = 25;
			vict->real_abils.cha = 25;

			vict->aff_abils = vict->real_abils;
		}
		GET_POS ( vict ) = POS_STANDING;
		update_pos ( vict );
		vict->check_regen_rates();
		vict->save();
		*ch << CONFIG_OK;
		act ( "You have been fully restored by $N!", FALSE, vict, 0, ch,
		      TO_CHAR );
	}
}


int perform_immort_vis ( Character *ch )
{
	if ( GET_INVIS_LEV ( ch ) == 0
	        && !AFF_FLAGGED ( ch, AFF_HIDE | AFF_INVISIBLE ) )
	{
		*ch << "You are already fully visible.\r\n";
		return 0;
	}

	GET_INVIS_LEV ( ch ) = 0;
	ch->appear();
	*ch << "You are now fully visible.\r\n";
return 1;
}


void perform_immort_invis ( Character *ch, int level )
{
	Character *tch;

	if ( IS_NPC ( ch ) )
		return;

	for ( tch = IN_ROOM ( ch )->people; tch; tch = tch->next_in_room )
	{
		if ( tch == ch )
			continue;
		if ( GET_LEVEL ( tch ) >= GET_INVIS_LEV ( ch ) && GET_LEVEL ( tch ) < level )
			act ( "You blink and suddenly realize that $n is gone.", FALSE,
			      ch, 0, tch, TO_VICT );
		if ( GET_LEVEL ( tch ) < GET_INVIS_LEV ( ch ) && GET_LEVEL ( tch ) >= level )
			act ( "You suddenly realize that $n is standing beside you.",
			      FALSE, ch, 0, tch, TO_VICT );
	}

	GET_INVIS_LEV ( ch ) = level;
	ch->Send ( "Your invisibility level is %d.\r\n", level );
}


ACMD ( do_invis )
{
	int level;
	char arg[MAX_INPUT_LENGTH];

	if ( IS_NPC ( ch ) )
	{
		ch->Send ( "You can't do that!\r\n" );
		return;
	}

	one_argument ( argument, arg );
	if ( !*arg )
	{
		if ( GET_INVIS_LEV ( ch ) > 0 )
			perform_immort_vis ( ch );
		else
			perform_immort_invis ( ch, GET_LEVEL ( ch ) );
	}
	else
	{
		level = atoi ( arg );
		if ( level > GET_LEVEL ( ch ) )
			ch->Send ( "You can't go invisible above your own level.\r\n" );
		else if ( level < 1 )
			perform_immort_vis ( ch );
		else
			perform_immort_invis ( ch, level );
	}
}


ACMD ( do_gecho )
{
	skip_spaces ( &argument );
	delete_doubledollar ( argument );

	if ( !*argument )
		ch->Send ( "That must be a mistake...\r\n" );
	else
		send_to_all ( "%s\r\n{c0", argument );
}

ACMD ( do_poofset )
{
	char **msg;

	switch ( subcmd )
	{
		case SCMD_POOFIN:
			msg = & ( POOFIN ( ch ) );
			break;
		case SCMD_POOFOUT:
			msg = & ( POOFOUT ( ch ) );
			break;
		default:
			return;
	}

	skip_spaces ( &argument );

	if ( strlen ( argument ) > 79 )
	{
		ch->Send ( "You poof message must be shorter than 80 characters.\r\n" );
		return;
	}

	if ( *msg )
		free ( *msg );

	if ( !*argument )
		*msg = NULL;
	else
		*msg = str_dup ( argument );

	//  write_poofs(ch);
	ch->Send ( "%s",CONFIG_OK );
}
int allowed_pretitle ( Character *ch )
{
	if ( GET_LEVEL ( ch ) > LVL_HERO )
		return TRUE;

	if ( !PLR_FLAGGED ( ch, PLR_ROLEPLAYER ) )
		return FALSE;

	if ( PLR_FLAGGED ( ch, PLR_HERO ) )
		return TRUE;
	if ( PLR_FLAGGED ( ch, PLR_RP_LEADER ) )
		return TRUE;

	if ( GET_AWARD ( ch ) >= 200 )
		return TRUE;

	return FALSE;

}

ACMD ( do_pretitle )
{
   if ( !allowed_pretitle ( ch ) )
   {
      ch->Send ( "Sorry, but you don't deserve a pretitle yet.\r\n" );
      return;
   }
   if ( strlen ( argument ) > 14 ) {
      ch->Send ( "Your pretitle must be fewer than 15 characters.\r\n" );
      return;
   }
   if ( !*argument ) {
      ch->Send ( "Your pretitle has been removed.\r\n" );
      free_string ( &PRETITLE ( ch ) );
      return;
   }
   free_string ( &PRETITLE ( ch ) );
   PRETITLE ( ch ) = strdup ( argument+1 );
   ch->Send ( "Your pretitle has been set to: %s\r\n", PRETITLE ( ch ) );
}

ACMD ( do_dc )
{
	Descriptor *d;
	int num_to_dc;
	char arg[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );
	if ( ! ( num_to_dc = atoi ( arg ) ) )
	{
		ch->Send ( "Usage: DC <user number> (type USERS for a list)\r\n" );
		return;
	}
	for ( d = descriptor_list; d && d->desc_num != num_to_dc; d = d->next )
		;

	if ( !d )
	{
		ch->Send ( "No such connection.\r\n" );
		return;
	}
	if ( d->character && GET_LEVEL ( d->character ) >= GET_LEVEL ( ch ) )
	{
		if ( !CAN_SEE ( ch, d->character ) )
			ch->Send ( "No such connection.\r\n" );
		else
			ch->Send ( "Umm.. maybe that's not such a good idea...\r\n" );
		return;
	}

	/* We used to just close the socket here using close_socket(), but
	 * various people pointed out this could cause a crash if you're
	 * closing the person below you on the descriptor list.  Just setting
	 * to CON_CLOSE leaves things in a massively inconsistent state so I
	 * had to add this new flag to the descriptor. -je
	 *
	 * It is a much more logical extension for a CON_DISCONNECT to be used
	 * for in-game socket closes and CON_CLOSE for out of game closings.
	 * This will retain the stability of the close_me hack while being
	 * neater in appearance. -gg 12/1/97
	 *
	 * For those unlucky souls who actually manage to get disconnected
	 * by two different immortals in the same 1/10th of a second, we have
	 * the below 'if' check. -gg 12/17/99
	 */
	if ( STATE ( d ) == CON_DISCONNECT || STATE ( d ) == CON_CLOSE )
		ch->Send ( "They're already being disconnected.\r\n" );
	else
	{
		/*
		 * Remember that we can disconnect people not in the game and
		 * that rather confuses the code when it expected there to be
		 * a character context.
		 */
		if ( STATE ( d ) == CON_PLAYING )
			STATE ( d ) = CON_DISCONNECT;
		else
			STATE ( d ) = CON_CLOSE;

		ch->Send ( "Connection #%d closed.\r\n", num_to_dc );
		log ( "(GC) Connection closed by %s.", GET_NAME ( ch ) );
	}
}



ACMD ( do_wizlock )
{
	int value;
	const char *when;
	char arg[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );
	if ( *arg )
	{
		value = atoi ( arg );
		if ( value < 0 || value > GET_LEVEL ( ch ) )
		{
			ch->Send ( "Invalid wizlock value.\r\n" );
			return;
		}
		circle_restrict = value;
		when = "now";
	}
	else
		when = "currently";

	switch ( circle_restrict )
	{
		case 0:
			ch->Send ( "The game is %s completely open.\r\n", when );
			break;
		case 1:
			ch->Send ( "The game is %s closed to new players.\r\n", when );
			break;
		default:
			ch->Send ( "Only level %d and above may enter the game %s.\r\n",circle_restrict, when );
			break;
	}
}

char * the_uptime ( char * buf, size_t len )
{
	char *tmstr;
	time_t ourtime = boot_time;
	int d, h, m;

	tmstr = ( char * ) asctime ( localtime ( &ourtime ) );
	* ( tmstr + strlen ( tmstr ) - 1 ) = '\0';

	ourtime = time ( 0 ) - boot_time;
	d = ourtime / 86400;
	h = ( ourtime / 3600 ) % 24;
	m = ( ourtime / 60 ) % 60;

	snprintf ( buf, len, "Up since %s: %d day%s, %d:%02d", tmstr,
	           d, ( ( d == 1 ) ? "" : "s" ), h, m );
	return buf;

}

char * the_date_now ( char * buf, size_t len )
{
	char *tmstr;
	time_t mytime = time ( 0 );
	tmstr = ( char * ) asctime ( localtime ( &mytime ) );
	* ( tmstr + strlen ( tmstr ) - 1 ) = '\0';
	snprintf ( buf, len, "%s", tmstr );
	return buf;

}

ACMD ( do_date )
{
	char *tmstr;
	time_t mytime;
	time_t ourtime;
	time_t login;
	int d, h, m;


	mytime = time ( 0 );
	ourtime = boot_time;

	tmstr = ( char * ) asctime ( localtime ( &mytime ) );
	* ( tmstr + strlen ( tmstr ) - 1 ) = '\0';


	ch->Send ( "Current machine time: {cy%s{c0\r\n", tmstr );

	tmstr = ( char * ) asctime ( localtime ( &ourtime ) );
	* ( tmstr + strlen ( tmstr ) - 1 ) = '\0';

	ourtime = time ( 0 ) - boot_time;
	d = ourtime / 86400;
	h = ( ourtime / 3600 ) % 24;
	m = ( ourtime / 60 ) % 60;

	ch->Send ( "Up since {cC%s{c0: %d day%s, %d:%02d\r\n", tmstr,
	           d, ( ( d == 1 ) ? "" : "s" ), h, m );

	login = ch->player.time.logon;
	tmstr = ( char * ) asctime ( localtime ( &login ) );
	* ( tmstr + strlen ( tmstr ) - 1 ) = '\0';

	ourtime = time ( 0 ) - login;
	d = ourtime / 86400;
	h = ( ourtime / 3600 ) % 24;
	m = ( ourtime / 60 ) % 60;

	ch->Send (
	    "Your session started {cc%s{c0: %d day%s, %d:%02d\r\n",
	    tmstr, d, ( ( d == 1 ) ? "" : "s" ), h, m );

	login = ch->player.time.birth;
	tmstr = ( char * ) asctime ( localtime ( &login ) );
	* ( tmstr + strlen ( tmstr ) - 1 ) = '\0';

	ourtime = time ( 0 ) - login;
	d = ourtime / 86400;
	h = ( ourtime / 3600 ) % 24;
	m = ( ourtime / 60 ) % 60;

	ch->Send (
	    "Your character started {cc%s{c0: %d day%s, %d:%02d\r\n",
	    tmstr, d, ( ( d == 1 ) ? "" : "s" ), h, m );
}



ACMD ( do_last )
{
	Character *vict = NULL;
	char arg[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );
	if ( !*arg )
	{
		ch->Send ( "For whom do you wish to search?\r\n" );
		return;
	}
	else if ( !pi.IdByName ( arg ) )
	{
		send_to_char ( "There is no such player.\r\n", ch );
		return;
	}
	vict = new Character ( FALSE );
	TEMP_LOAD_CHAR = TRUE;
	if ( store_to_char ( arg, vict ) < 0 )
	{
		ch->Send ( "There is no such player.\r\n" );
		delete vict;
		TEMP_LOAD_CHAR = FALSE;
		return;
	}
	TEMP_LOAD_CHAR = FALSE;

	if ( ( GET_LEVEL ( vict ) > GET_LEVEL ( ch ) ) && ( GET_LEVEL ( ch ) < LVL_IMPL ) )
	{
		ch->Send ( "You are not sufficiently godly for that!\r\n" );
		delete vict;
		return;
	}
	ch->Send ( "[%5ld] [%2d %s] %-12s \r\nHost:%-*s \r\nLast: %-20s\r\n",
	           GET_IDNUM ( vict ), ( int ) GET_LEVEL ( vict ),
	           CLASS_ABBR(vict), GET_NAME ( vict ),
	           HOST_LENGTH, !vict->player_specials->host.empty() ? vict->player_specials->
	           host.c_str() : "(NOHOST)", ctime ( &vict->player.time.logon ) );

	delete vict;
}


ACMD ( do_force )
{
	Descriptor *i, *next_desc;
	Character *vict, *next_force;
	char to_force[MAX_INPUT_LENGTH + 2];
	char buf1[MAX_INPUT_LENGTH ];
	char arg[MAX_INPUT_LENGTH];


	half_chop ( argument, arg, to_force );

	snprintf ( buf1, sizeof ( buf1 ), "$n has forced you to '%s'.", to_force );

	if ( !*arg || !*to_force )
		ch->Send ( "Whom do you wish to force do what?\r\n" );
	else if ( ( GET_LEVEL ( ch ) < LVL_GRGOD )
	          || ( str_cmp ( "all", arg ) && str_cmp ( "room", arg ) ) )
	{
		if ( ! ( vict = get_char_vis ( ch, arg, NULL, FIND_CHAR_WORLD ) ) )
			ch->Send ( "%s", CONFIG_NOPERSON );
		else if ( GET_LEVEL ( ch ) <= GET_LEVEL ( vict ) && !IS_NPC ( vict ) )
			ch->Send ( "No, no, no!\r\n" );
		else
		{
			ch->Send ( "%s", CONFIG_OK );
			act ( buf1, TRUE, ch, NULL, vict, TO_VICT );
			new_mudlog ( NRM, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) %s forced %s to %s", GET_NAME ( ch ),
			             GET_NAME ( vict ), to_force );
			command_interpreter ( vict, to_force );
		}
	}
	else if ( !str_cmp ( "room", arg ) )
	{
		ch->Send ( "%s", CONFIG_OK );
		new_mudlog ( NRM, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) %s forced room %d to %s",
		             GET_NAME ( ch ), GET_ROOM_VNUM ( IN_ROOM ( ch ) ), to_force );

		for ( vict = IN_ROOM ( ch )->people; vict; vict = next_force )
		{
			next_force = vict->next_in_room;
			if ( !IS_NPC ( vict ) && GET_LEVEL ( vict ) >= GET_LEVEL ( ch ) )
				continue;
			act ( buf1, TRUE, ch, NULL, vict, TO_VICT );
			command_interpreter ( vict, to_force );
		}
	}
	else              /* force all */
	{
		ch->Send ( "%s",CONFIG_OK );
		new_mudlog ( NRM, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) %s forced all to %s", GET_NAME ( ch ), to_force );

		for ( i = descriptor_list; i; i = next_desc )
		{
			next_desc = i->next;

			if ( STATE ( i ) != CON_PLAYING || ! ( vict = i->character )
			        || ( !IS_NPC ( vict ) && GET_LEVEL ( vict ) >= GET_LEVEL ( ch ) ) )
				continue;
			act ( buf1, TRUE, ch, NULL, vict, TO_VICT );
			command_interpreter ( vict, to_force );
		}
	}
}



ACMD ( do_wiznet )
{
	Descriptor *d;
	char emote = FALSE;
	char any = FALSE;
	int level = LVL_GOD;
	char buf1[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];

	skip_spaces ( &argument );
	delete_doubledollar ( argument );

// Removed the hero ability of hearing wiznet - Prometheus
	if ( PLR_FLAGGED ( ch,PLR_IMM_MORT ) || ( ( GET_LEVEL ( ch ) > LVL_HERO || GET_ORIG_LEV ( ch ) > LVL_HERO )
	     && ( CMD_FLAGGED ( ch, WIZ_IMM1_GRP ) || CMD_FLAGGED2 ( ch, WIZ_IMM1_GRP ) ) ) )
	{
		if ( !*argument )
		{
			ch->Send ( "       Usage: wiznet <text> | #<level> <text> | *<emotetext> |\r\n"
			           "       wiznet @<level> *<emotetext> | wiz @  (for wiz who list) | wiz %% (for arena info)\r\n"
			           "       use as backslash as an escape char.\r\n" );

			return;
		}
		switch ( *argument )
		{
			case '*':
				emote = TRUE;
			case '#':
				one_argument ( argument + 1, buf1 );
				if ( is_number ( buf1 ) )
				{
					half_chop ( argument + 1, buf1, argument );
					level = MAX ( atoi ( buf1 ), LVL_GOD );
					if ( level > ( GET_ORIG_LEV ( ch ) ? GET_ORIG_LEV ( ch ) :GET_LEVEL ( ch ) ) )
					{
						ch->Send ( "You can't wiznet above your own level.\r\n" );
						return;
					}
				}
				else if ( emote )
					argument++;
				break;
			case '@':
				for ( d = descriptor_list; d; d = d->next )
				{
					if ( STATE ( d ) == CON_PLAYING
					        && GET_LEVEL ( d->character ) >= LVL_GOD
					        && !PRF_FLAGGED ( d->character, PRF_NOWIZ )
					        && ( CAN_SEE ( ch, d->character )
					             || GET_LEVEL ( ch ) == LVL_IMPL ) )
					{
						if ( !any )
						{
							ch->Send ( "Gods online:\r\n" );
							any = TRUE;
						}
						ch->Send ( "  %s",GET_NAME ( d->character ) );
						if ( PLR_FLAGGED ( d->character, PLR_WRITING ) )
							ch->Send ( " (Writing)\r\n" );
						else if ( PLR_FLAGGED ( d->character, PLR_MAILING ) )
							ch->Send ( " (Writing mail)\r\n" );
						else
							ch->Send ( "\r\n" );
					}
				}
				any = FALSE;
				for ( d = descriptor_list; d; d = d->next )
				{
					if ( STATE ( d ) == CON_PLAYING
					        && GET_LEVEL ( d->character ) >= LVL_GOD
					        && PRF_FLAGGED ( d->character, PRF_NOWIZ )
					        && CAN_SEE ( ch, d->character ) )
					{
						if ( !any )
						{
							ch->Send ( "Gods offline:\r\n" );
							any = TRUE;
						}
						ch->Send ( "  %s\r\n",GET_NAME ( d->character ) );
					}
				}
				return;
			case '\\':
				++argument;
				break;
			case '%':              /* arena */
				if ( in_arena == ARENA_OFF )
				{
					ch->Send ( "The Arena is closed right now.\r\n" );
				}
				else if ( in_arena == ARENA_START )
				{
					ch->Send ( "Arena will start in %d hour(s)\r\n",   time_to_start );
					ch->Send ( "It will last for %d hour(s)\r\n",  game_length );
				}
				else if ( in_arena == ARENA_RUNNING )
				{
					ch->Send ( "Arena will end in %d hour(s)\r\n", time_left_in_game );
				}
				return;
			default:
				break;
		}
		if ( PRF_FLAGGED ( ch, PRF_NOWIZ ) )
		{
			ch->Send ( "You are not on the wiznet channel!\r\n" );
			return;
		}
		skip_spaces ( &argument );

		if ( !*argument )
		{
			ch->Send ( "Don't bother the gods like that!\r\n" );
			return;
		}
		if ( level > LVL_GOD )
		{
			snprintf ( buf1, sizeof ( buf1 ), "%s: <%d> %s%s\r\n", GET_NAME ( ch ), level,
			           emote ? "<--- " : "", argument );
			snprintf ( buf2, sizeof ( buf2 ), "Someone: <%d> %s%s\r\n", level,
			           emote ? "<--- " : "", argument );
		}
		else
		{
			snprintf ( buf1, sizeof ( buf1 ), "%s: %s%s\r\n", GET_NAME ( ch ), emote ? "<--- " : "",
			           argument );
			snprintf ( buf2, sizeof ( buf2 ), "Someone: %s%s\r\n", emote ? "<--- " : "", argument );
		}

		for ( d = descriptor_list; d; d = d->next )
		{
			if ( ( STATE ( d ) == CON_PLAYING ) && ( ( ( GET_ORIG_LEV ( d->character ) ? GET_ORIG_LEV ( d->character ) : GET_LEVEL ( d->character ) ) >= level ) || ((PLR_FLAGGED ( d->character, PLR_IMM_MORT ) && level == LVL_GOD)) )
			        && ( !PRF_FLAGGED ( d->character, PRF_NOWIZ ) )
			        && ( !PLR_FLAGGED ( d->character, PLR_MAILING )
			             || !PLR_FLAGGED ( d->character, PLR_WRITING ) )
			        && ( d != ch->desc
			             || ! ( PRF_FLAGGED ( d->character, PRF_NOREPEAT ) ) ) )
			{
				d->Output ( "%s", CCCYN ( d->character, C_NRM ) );
				if ( CAN_SEE ( d->character, ch ) )
					d->Output ( "%s", buf1 );
				else
					d->Output ( "%s", buf2 );
				d->Output ( "%s", CCNRM ( d->character, C_NRM ) );
			}
		}

		snprintf(buf1, sizeof(buf1), "%s%s %ss, '%s'%s", KCYN, GET_INVIS_LEV(ch) ? "Someone" : GET_NAME(ch), "wiznet", argument, KNRM);
    		add_to_comm( "wiznet", buf1);

		if ( PRF_FLAGGED ( ch, PRF_NOREPEAT ) )
			ch->Send ( "%s", CONFIG_OK );
	}
	else
		ch->Send ( "Huh?!?\r\n" );
}


ACMD ( do_zreset )
{
	zone_rnum i;
	zone_vnum j;
	char arg[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );
	if ( !*arg )
	{
		send_to_char ( "You must specify a zone.\r\n", ch );
		return;
	}
	if ( *arg == '*' )
	{
		for ( i = 0; i <= top_of_zone_table; i++ )
			reset_zone ( i );
		ch->Send ( "Reset world.\r\n" );
		new_mudlog ( NRM, MAX ( LVL_GRGOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) %s reset entire world.", GET_NAME ( ch ) );
		return;
	}
	else if ( *arg == '.' )
		i = IN_ROOM ( ch )->zone;
	else
	{
		j = atoi ( arg );
		for ( i = 0; i <= top_of_zone_table; i++ )
			if ( zone_table[i].number == j )
				break;
	}
	if ( i >= 0 && i <= top_of_zone_table )
	{
		reset_zone ( i );
		ch->Send ( "Reset zone %d (#%d): %s.\r\n", i,  zone_table[i].number, zone_table[i].name );

		new_mudlog ( NRM, MAX ( LVL_GRGOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) %s reset zone %d (%s)", GET_NAME ( ch ), zone_table[i].number,
		             zone_table[i].name );
	}
	else
		send_to_char ( "Invalid zone number.\r\n", ch );
}

ACMD ( do_heroutil )
{
	int immfreeze_affect ( Character *ch, Character *vict, char *argument );
	int silence_affect ( Character *ch, Character *vict, char *argument );
	Character *vict;
	char arg[MAX_INPUT_LENGTH];

	if ( GET_LEVEL ( ch ) < LVL_HERO && !PLR_FLAGGED ( ch, PLR_HERO ) )
	{
		ch->Send ( "Huh?\r\n" );
		return;
	}
	argument = one_argument ( argument, arg );

	if ( !*arg )
		ch->Send ( "Yes, but for whom?!?\r\n" );
	else if ( ! ( vict = get_char_vis ( ch, arg, NULL, FIND_CHAR_WORLD ) ) )
		ch->Send ( "There is no such player.\r\n" );
	else if ( IS_NPC ( vict ) )
		ch->Send ( "You can't do that to a mob!\r\n" );
	else if ( GET_LEVEL ( vict ) > GET_LEVEL ( ch ) )
		ch->Send ( "Hmmm...you'd better not.\r\n" );
	else
	{
		switch ( subcmd )
		{
			case SCMD_FREEZE:
				if ( ch == vict )
				{
					ch->Send ( "Oh, yeah, THAT'S real smart...\r\n" );
					return;
				}
				if ( GET_LEVEL ( vict ) > LVL_HERO )
				{
					new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(HC) %s attempted to be frozen by %s.", GET_NAME ( vict ), GET_NAME ( ch ) );
					ch->Send ( "Sorry, that attempt was logged. I would say goodbye to your hero.\r\n" );
					return;
				}
				if ( PLR_FLAGGED ( vict, PLR_FROZEN ) )
				{
					ch->Send ( "Your victim is already pretty cold.\r\n" );
					return;
				}
				if ( immfreeze_affect ( ch, vict, argument ) )
				{
					SET_BIT_AR ( PLR_FLAGS ( vict ), PLR_FROZEN );
					GET_FREEZE_LEV ( vict ) = GET_LEVEL ( ch );
					vict->Send ( "A bitter wind suddenly rises and drains every ounce of heat from your body!\r\nYou feel frozen!\r\n" );
					ch->Send ( "Frozen.\r\n" );
					act ( "A sudden cold wind conjured from nowhere freezes $n!", FALSE, vict, 0, 0, TO_ROOM );
					new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(HC) %s frozen by %s.", GET_NAME ( vict ), GET_NAME ( ch ) );
				}
				else
				{
					ch->Send ( "Freeze failed.\r\n" );
					new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(HC) %s attempted to be frozen by %s.", GET_NAME ( vict ), GET_NAME ( ch ) );
				}
				break;
			case SCMD_THAW:
				if ( !PLR_FLAGGED ( vict, PLR_FROZEN ) )
				{
					ch->Send ( "Sorry, your victim is not morbidly encased in ice at the moment.\r\n" );
					return;
				}
				if ( GET_FREEZE_LEV ( vict ) > GET_LEVEL ( ch ) && GET_FREEZE_LEV ( vict ) > LVL_HERO )
				{
					ch->Send ( "Sorry, a level %d God froze %s... You can't unfreeze %s.\r\n",
					           GET_FREEZE_LEV ( vict ), GET_NAME ( vict ), HMHR ( vict ) );
					return;
				}
				new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) %s un-frozen by %s.", GET_NAME ( vict ), GET_NAME ( ch ) );
				if ( !affected_by_spell ( vict, SPELL_IMMFREEZE ) )
					{
					REMOVE_BIT_AR ( PLR_FLAGS ( vict ), PLR_FROZEN );
					}
				else
					affect_from_char ( vict, SPELL_IMMFREEZE );

				vict->Send ( "A fireball suddenly explodes in front of you, melting the ice!\r\nYou feel thawed.\r\n" );
				ch->Send ( "Thawed.\r\n" );
				act ( "A sudden fireball conjured from nowhere thaws $n!", FALSE, vict, 0, 0, TO_ROOM );
				break;


			case SCMD_SILENCE:
				if ( ch == vict )
				{
					ch->Send ( "Oh, yeah, THAT'S real smart...\r\n" );
					return;
				}
				if ( GET_LEVEL ( vict ) > LVL_HERO )
				{
					new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(HC) %s attempted to be silenced by %s.", GET_NAME ( vict ), GET_NAME ( ch ) );
					ch->Send ( "Sorry, that attempt was logged. I would say goodbye to your hero.\r\n" );
					return;
				}
				if ( PLR_FLAGGED ( vict, PLR_COVENTRY ) )
				{
					ch->Send ( "Your victim is already pretty quiet.\r\n" );
					return;
				}
				if ( silence_affect ( ch, vict, argument ) )
				{
					act ( "Yellow chains of light wrap around $n's voicebox - $e has been silenced!", FALSE, vict, 0, 0, TO_ROOM );
					new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(HC) %s Silenced by %s.", GET_NAME ( vict ), GET_NAME ( ch ) );
				}
				else
				{
					ch->Send ( "Silence failed.\r\n" );
					new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(HC) %s attempted to be silenced by %s.", GET_NAME ( vict ), GET_NAME ( ch ) );
				}
				break;

                        case SCMD_UNSILENCE:
                                if ( !PLR_FLAGGED ( vict, PLR_COVENTRY ) )
                                {
                                        ch->Send ( "Sorry, your victim is not silenced.\r\n" );
                                        return;
                                }
                                new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) %s un-silenced by %s.", GET_NAME ( vict ), GET_NAME ( ch ) );
                                if ( !affected_by_spell ( vict, SPELL_SILENCED ) )
                                        {
                                        REMOVE_BIT_AR ( PLR_FLAGS ( vict ), PLR_COVENTRY);
                                        }
                                else
                                        affect_from_char ( vict, SPELL_SILENCED);

                                vict->Send ( "The yellow chains of light loosen from around your neck and disappear!\r\nYou regain your voice, this time watch what you say.\r\n" );
                                ch->Send ( "Unsilenced.\r\n" );
                                act ( "Yellow chains loosen and disappear from the neck of  $n!", FALSE, vict, 0, 0, TO_ROOM );
                                break;
				break;

			default:
				log ( "SYSERR: Unknown subcmd %d passed to do_wizutil (%s)", subcmd, __FILE__ );
				break;
		}
		vict->save();

	}
}
/*
 *  General fn for wizcommands of the sort: cmd <player>
 */

ACMD ( do_wizutil )
{
	Character *vict;
	long result;
	char arg[MAX_INPUT_LENGTH];
	argument = one_argument ( argument, arg );

	if ( !*arg )
		ch->Send ( "Yes, but for whom?!?\r\n" );
	else if ( ! ( vict = get_char_vis ( ch, arg, NULL, FIND_CHAR_WORLD ) ) )
		ch->Send ( "There is no such player.\r\n" );
	else if ( IS_NPC ( vict ) )
		ch->Send ( "You can't do that to a mob!\r\n" );
	else if ( GET_LEVEL ( vict ) > GET_LEVEL ( ch ) )
		ch->Send ( "Hmmm...you'd better not.\r\n" );
	else
	{
		switch ( subcmd )
		{
			case SCMD_REROLL:
				ch->Send ( "Rerolled...\r\n" );
				roll_real_abils ( vict );
				log ( "(GC) %s has rerolled %s.", GET_NAME ( ch ), GET_NAME ( vict ) );
				ch->Send (
				    "New stats: Str %d/%d, Int %d, Wis %d, Dex %d, Con %d, Cha %d\r\n",
				    GET_STR ( vict ), GET_ADD ( vict ), GET_INT ( vict ),
				    GET_WIS ( vict ), GET_DEX ( vict ), GET_CON ( vict ),
				    GET_CHA ( vict ) );
				vict->Send (
				    "New stats: Str %d/%d, Int %d, Wis %d, Dex %d, Con %d, Cha %d\r\n",
				    GET_STR ( vict ), GET_ADD ( vict ), GET_INT ( vict ),
				    GET_WIS ( vict ), GET_DEX ( vict ), GET_CON ( vict ),
				    GET_CHA ( vict ) );
				break;
			case SCMD_PARDON:
				if ( PLR_FLAGGED ( vict, PLR_THIEF )
				        || PLR_FLAGGED ( vict, PLR_KILLER ) )
				{
					REMOVE_BIT_AR ( PLR_FLAGS ( vict ), PLR_THIEF );
					REMOVE_BIT_AR ( PLR_FLAGS ( vict ), PLR_KILLER );
					ch->Send ( "Pardoned.\r\n" );
					vict->Send ( "You have been pardoned by the Gods!\r\n" );
					new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) %s pardoned by %s", GET_NAME ( vict ),
					             GET_NAME ( ch ) );
				}
				else
				{
					ch->Send ( "Your victim is not flagged.\r\n" );
					return;
				}
				break;
			case SCMD_NOTITLE:
				result = PLR_TOG_CHK ( vict, PLR_NOTITLE );
				new_mudlog ( NRM, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) Notitle %s for %s by %s.", ONOFF ( result ),
				             GET_NAME ( vict ), GET_NAME ( ch ) );
				ch->Send ( "%s", CONFIG_OK );
				break;
			case SCMD_SQUELCH:
				result = PLR_TOG_CHK ( vict, PLR_NOSHOUT );
				new_mudlog ( BRF, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "(GC) Squelch %s for %s by %s.", ONOFF ( result ),
				             GET_NAME ( vict ), GET_NAME ( ch ) );
				ch->Send ( "%s", CONFIG_OK );
				break;
			case SCMD_UNAFFECT:
				if ( vict->affected )
				{
					vict->remove_all_affects();
					vict->Send ( "There is a brief flash of light!\r\n"
					             "You feel slightly different.\r\n" );
					ch->Send ( "All spells removed.\r\n" );
					vict->check_regen_rates();
				}
				else
				{
					ch->Send ( "Your victim does not have any affections!\r\n" );
					return;
				}
				break;
			default:
				log ( "SYSERR: Unknown subcmd %d passed to do_wizutil (%s)", subcmd, __FILE__ );
				break;
		}
		vict->save();
		// log("(do_wizutil)Saved %s in room %d.", GET_NAME(vict), vict->in_room);
	}
}

int immfreeze_affect ( Character *ch, Character *vict, char *argument )
{
	int duration=0;
	char time_size[MAX_INPUT_LENGTH], duration_p[MAX_INPUT_LENGTH];
	int multi=0;
	struct affected_type af;

	argument = two_arguments ( argument, duration_p, time_size );

	if ( !*duration_p || !*time_size )
	{
		ch->Send ( "FREEZE <player> <time> (Days|Hours)\r\n" );
		return 0;
	}

	duration = atoi ( duration_p );
	if ( duration <= 0 )
	{
		ch->Send ( "You need positive duration!\r\n" );
		return 0;
	}
	if ( is_abbrev ( time_size, "days" ) )
		multi = SECS_PER_REAL_DAY;
	else if ( is_abbrev ( time_size, "hours" ) )
		multi = SECS_PER_REAL_HOUR;
	else
	{
		ch->Send ( "You must specify Days or Hours\r\n"
		           "FREEZE <player> <time> (Days|Hours)\r\n" );
		return 0;
	}

	/* add the affect */
	af.type = SPELL_IMMFREEZE;
	af.expire = sec_to_time ( duration * multi );
	af.modifier = 0;
	af.location = 0;
	af.bitvector = AFF_IMMFREEZE;


	affect_to_char ( vict, &af );
	return 1;
}

int silence_affect ( Character *ch, Character *vict, char *argument )
{
	int duration=0;
	char time_size[MAX_INPUT_LENGTH], duration_p[MAX_INPUT_LENGTH];
	int multi=0;
	struct affected_type af;

	argument = two_arguments ( argument, duration_p, time_size );

	if ( !*duration_p || !*time_size )
	{
		ch->Send ( "SILENCE <player> <time> (Days|Hours)\r\n" );
		return 0;
	}

	duration = atoi ( duration_p );
	if ( duration <= 0 )
	{
		ch->Send ( "You need positive duration!\r\n" );
		return 0;
	}
	if ( is_abbrev ( time_size, "days" ) )
		multi = SECS_PER_REAL_DAY;
	else if ( is_abbrev ( time_size, "hours" ) )
		multi = SECS_PER_REAL_HOUR;
	else
	{
		ch->Send ( "You must specify Days or Hours\r\n"
		           "SILENCE <player> <time> (Days|Hours)\r\n" );
		return 0;
	}
	SET_BIT_AR ( PLR_FLAGS ( vict ), PLR_COVENTRY );
	/* add the affect */
	af.type = SPELL_SILENCED;
	af.expire = sec_to_time ( duration * multi );
	af.modifier = 0;
	af.location = 0;
	af.bitvector = AFF_SILENCED;

	vict->Send ( "You have been silenced - You will be heard again in %s %s\r\n", duration_p, time_size );
	affect_to_char ( vict, &af );
	return 1;
}


ACMD ( do_topgold )
{
	int i,j = 20;
	plrindx pindex ( *pi.PlayerTable() );
	skip_spaces ( &argument );
	if ( *argument )
		j = atoi ( argument );
	if ( j < 1 || j > 100 )
		j = 20;

	ch->Send ( "Members of the top %d Gold Coin Tycoons\r\n", j );
	ch->Send ( "------------------------------------------\r\n" );
	sort ( pindex.begin(), pindex.end(), goldSort() );

	for ( i = 0; i < ( int ) pindex.size() && i < j; i++ )
		ch->Send ( "%-20s -- Gold Coins: %5lld million.\r\n", pindex[i].name,  pindex[i].gc_amount/1000000 );

	ch->Send ( "\r\nMembers of the top %d Gold Token Tycoons\r\n", j );
	ch->Send ( "------------------------------------------\r\n" );
	sort ( pindex.begin(), pindex.end(), tokenSort() );

	for ( i = 0; i < ( int ) pindex.size() && i < j; i++ )
		ch->Send ( "%-20s -- Gold Tokens: %d.\r\n", pindex[i].name,  pindex[i].gt_amount );

}
#if 0
class hosts_lookup : public threaded_object
{
	public:
		hosts_lookup ( string fn, Character *c )
		{
			_findname = fn;
			_ch = c;
			// Start our thread going in the thread() function
			start();
		}

		~hosts_lookup()
		{
			// Tell the thread() function to stop.  This will cause should_stop() to
			// return true so the thread knows what to do.
			stop();

			// Wait for the thread to stop before letting this object destruct itself.
			// Also note, you are *required* to wait for the thread to end before
			// letting this object destruct itself.
			wait();
		}

	private:
		string _findname;
		Character *_ch;
		void thread()
		{
			int j;
			Character *victim = NULL, *ch = NULL;
			string findname = _findname;
			ch = _ch;

			TEMP_LOAD_CHAR = TRUE;
			for ( j = 0; j < pi.Size(); j++ )
			{
				victim = new Character ( FALSE );

				if ( !pi.DeletedByIndex ( j ) && store_to_char ( pi.NameByIndex ( j ), victim ) > -1 )
					if ( stristr ( victim->player_specials->host.c_str(), findname.c_str() ) != NULL )
						ch->Send ( "%-10s -- %s\r\n", GET_NAME ( victim ), victim->player_specials->host.c_str() );

				delete ( victim );
			}
			TEMP_LOAD_CHAR = FALSE;
		}
};
#endif
#if 0
ACMD ( do_hostfind )
{
	skip_spaces ( &argument );
	if ( !argument || !*argument )
	{
		ch->Send ( "Needs an argument. Part of a host name.\r\n" );
		return;
	}
	ch->Send ( "Searching for hosts now, we will get back to you with results shortly.\r\n" );
	//hosts_lookup hl(argument, ch);
	return;
}
#endif
ACMD ( do_hostfind )
{
	int j;
	Character *victim = NULL;
	skip_spaces ( &argument );
	if ( !argument || !*argument )
	{
		ch->Send ( "Needs an argument. Part of a host name.\r\n" );
		return;
	}
	TEMP_LOAD_CHAR = TRUE;
	for ( j = 0; j < pi.Size(); j++ )
	{
		victim = new Character ( FALSE );

		if ( store_to_char ( pi.NameByIndex ( j ), victim ) > -1 )
			if ( stristr ( victim->player_specials->host.c_str(), argument ) != NULL )
				ch->Send ( "%-10s -- %s\r\n", GET_NAME ( victim ), victim->player_specials->host.c_str() );

		delete ( victim );
	}
	TEMP_LOAD_CHAR = FALSE;
	return;
}

/* single zone printing fn used by "show zone" so it's not repeated in the
   code 3 times ... -je, 4/6/93 */

/* FIXME: overflow possible */
size_t print_zone_to_buf ( char *bufptr, size_t left, zone_rnum zone, int listall )
{
	size_t tmp;
	if ( listall )
	{
		int i, j, k, l, m, n;
		extern struct index_data **trig_index;
		int count_shops ( shop_vnum low, shop_vnum high );
		char zflags[MAX_STRING_LENGTH];

		sprintbit ( zone_table[zone].zone_flags, zone_bits, zflags, sizeof ( zflags ) );

		tmp = snprintf ( bufptr, left,
		                 "%3d %-30.30s By: %-10.10s Age: %3d; Reset: %3d (%1d); Range: %5d-%5d\r\nZone Flags: %s\r\n",
		                 zone_table[zone].number, zone_table[zone].name, zone_table[zone].builders,
		                 zone_table[zone].age, zone_table[zone].lifespan,
		                 zone_table[zone].reset_mode,
		                 zone_table[zone].bot, zone_table[zone].top,
		                 zflags );
		i = j = k = l = m = n = 0;

		for ( i = 0; i < top_of_world; i++ )
			if ( world_vnum[i] && world_vnum[i]->number >= zone_table[zone].bot && world_vnum[i]->number <= zone_table[zone].top )
				j++;

		for ( i = 0; i < top_of_objt; i++ )
			if ( obj_index[i].vnum >= zone_table[zone].bot && obj_index[i].vnum <= zone_table[zone].top )
				k++;

		for ( i = zone_table[zone].bot; i <= zone_table[zone].top; i++ )
			if ( MobProtoExists ( i ) )
				l++;

		m = count_shops ( zone_table[zone].bot, zone_table[zone].top );

		for ( i = 0; i < ( int ) top_of_trigt; i++ )
			if ( trig_index[i]->vnum >= zone_table[zone].bot && trig_index[i]->vnum <= zone_table[zone].top )
				n++;

		tmp += snprintf ( bufptr + tmp, left - tmp,
		                  "       Zone stats:\r\n"
		                  "       ---------------\r\n"
		                  "         Rooms:    %2d\r\n"
		                  "         Objects:  %2d\r\n"
		                  "         Mobiles:  %2d\r\n"
		                  "         Shops:    %2d\r\n"
		                  "         Triggers: %2d\r\n",
		                  j, k, l, m, n );

		return tmp;
	}

	return snprintf ( bufptr, left,
	                  "%3d %-30.30s By: %-10.10s Range: %5d-%5d\r\n",
	                  zone_table[zone].number, zone_table[zone].name, zone_table[zone].builders,
	                  zone_table[zone].bot, zone_table[zone].top );

}

void show_last_logons ( Character *ch, int days, bool fix )
{
	time_t tm = time ( 0 ) - ( days * SECS_PER_REAL_DAY );
	for ( int tp = 0; tp < pi.Size(); tp++ )
	{
		if ( pi.DeletedByIndex ( tp ) )
			continue;
		if ( pi.LastByIndex ( tp ) > tm )
		{
			if ( fix )
			{
				pi.SetFlags ( tp, PINDEX_FIXSKILLS );
			}
			*ch << pi.NameByIndex ( tp );
			if ( pi.IsSet ( tp, PINDEX_FIXSKILLS ) )
				*ch << "- Skills Flagged to be fixed";

			*ch << "\r\n";

		}
	}
	if ( fix )
		pi.Save();
}


ACMD ( do_show )
{
	int i, j, k, l, con;   /* i, j, k to specifics? */
	
	zone_rnum zrn = NOWHERE;
	zone_vnum zvn = NOWHERE;
	sbyte self = FALSE;
	Character *vict = NULL;
	Descriptor *d;
	char field[MAX_INPUT_LENGTH], value[MAX_INPUT_LENGTH];
	char tbuf[MAX_STRING_LENGTH];
	char buf[MAX_STRING_LENGTH];
	char arg[MAX_INPUT_LENGTH];
	char *tptr;
	extern int max_actions;
	extern int min_actions;
	extern int total_commands_typed;
	extern int total_pcommands_typed;
	extern int total_trig_commands_typed;
	DYN_DEFINE;
	const char *gold_types[] = { "\n",
	                             "Shop Sold",
	                             "Shop Brought",
	                             "Dropped",
	                             "Taken",
	                             "Auction in",
	                             "Auction out",
	                             "Gold Given",
	                             "Gold Received",
	                             "Gold created DG",
	                             "Gold destroyed DG",
	                             "\n"
	                           };


	struct show_struct
	{
		const char *cmd;
		const char level;
	}
	fields[] =
	{
		{"nothing",     0},      /* 0 */
		{"zones",      LVL_GOD}, /* 1 */
		{"player",     LVL_GOD},
		{"rent",  LVL_GOD},
		{"stats",      LVL_GOD},
		{"roleplay",   LVL_IMPL},     /* 5 */
		{"death",      LVL_GOD},
		{"godrooms",   LVL_GOD},
		{"shops",      LVL_GOD},
		{"houses",     LVL_GOD},
		{"connections",     LVL_BLD}, /* 10 */
		{"arena",      LVL_GOD},
		{"snoop",      LVL_IMPL},
		{"gold",  LVL_GOD},
		{"builder",    LVL_IMPL},
		{"olc",        LVL_GOD},
		{"corpses",    LVL_GOD},
		{"errors",       LVL_GOD},
		{"assemblies",   LVL_BLD},
		{"logons", 	 LVL_BLD},
		{"trainers", LVL_BLD},
		{"\n", 0}
	};

	skip_spaces ( &argument );


	if ( !*argument )
	{
		ch->Send ( "Show options:\r\n" );
		for ( j = 0, i = 1; fields[i].level; i++ )
			if ( fields[i].level <= GET_LEVEL ( ch ) )
				ch->Send ( "%-15s%s", fields[i].cmd,
				           ( ! ( ++j % 5 ) ? "\r\n" : "" ) );
		ch->Send ( "\r\n" );
		return;
	}

	strcpy ( arg, two_arguments ( argument, field, value ) );
	tptr = tbuf;
	*tptr = '\0';

	for ( l = 0; * ( fields[l].cmd ) != '\n'; l++ )
		if ( !strncmp ( field, fields[l].cmd, strlen ( field ) ) )
			break;

	if ( GET_LEVEL ( ch ) < fields[l].level )
	{
		ch->Send ( "You are not godly enough for that!\r\n" );
		return;
	}
	if ( !strcmp ( value, "." ) )
		self = TRUE;
	buf[0] = '\0';
	switch ( l )
	{

		case 1:           // zone


			/* tightened up by JE 4/6/93 */
			if ( self )
			{
				DYN_CREATE;
				*dynbuf = 0;
				print_zone_to_buf ( buf, sizeof ( buf ), IN_ROOM ( ch )->zone, 1 );
				DYN_RESIZE ( buf );
			}
			else if ( *value && is_number ( value ) )
			{
				for ( zvn = atoi ( value ), zrn = 0; zrn <= top_of_zone_table && zone_table[zrn].number != zvn;zrn++ )
					;
				if ( zrn <= top_of_zone_table )
				{
					DYN_CREATE;
					*dynbuf = 0;
					print_zone_to_buf ( buf, sizeof ( buf ), zrn, 1 );
					DYN_RESIZE ( buf );
				}
				else
				{
					ch->Send ( "That is not a valid zone.\r\n" );
					return;
				}
			}
			else
			{
				DYN_CREATE;
				*dynbuf = 0;
				for (  zrn = 0; zrn <= top_of_zone_table; zrn++ )
				{
					print_zone_to_buf ( buf, sizeof ( buf ), zrn, 0 );
					DYN_RESIZE ( buf );
				}
			}
			page_string ( ch->desc, dynbuf, DYN_BUFFER );
			break;

		case 2:           /* player */
			if ( !*value )
			{
				ch->Send ( "A name would help.\r\n" );
				return;
			}
			else if ( !pi.IdByName ( value ) )
			{
				send_to_char ( "There is no such player.\r\n", ch );
				return;
			}


			vict = new Character ( FALSE );
			TEMP_LOAD_CHAR = TRUE;
			if ( store_to_char ( value, vict ) < 0 )
			{
				ch->Send ( "There is no such player.\r\n" );
				delete ( vict );
				TEMP_LOAD_CHAR = FALSE;
				return;
			}
			TEMP_LOAD_CHAR = FALSE;
			if ( GET_LEVEL ( ch ) <= LVL_IMMORT )
				mortal_player_info ( ch,vict );
			else
			{
				ch->Send ( "Player: %-12s (%s) [%2d %s %s]\r\n",
				           GET_NAME ( vict ), genders[ ( int ) GET_SEX ( vict ) ],
				           GET_LEVEL ( vict ),
				           race_abbrevs[ ( int ) GET_RACE ( vict ) ],
				           CLASS_ABBR(vict) );
				ch->Send (
				    "Au: %-8lld  Bal: %-8lld  Exp: %-8lld  Align: %-5d  Lessons: %-3d\r\n",
				    vict->Gold ( 0, GOLD_HAND ), vict->Gold ( 0, GOLD_BANK ),
				    GET_EXP ( vict ), GET_ALIGNMENT ( vict ),
				    GET_PRACTICES ( vict ) );
				ch->Send (
				    "Started: %-20.16s  Last: %-20.16s  Played: %3dh %2dm\r\n",
				    ctime ( &vict->player.time.birth ),
				    ctime ( &vict->player.time.logon ),
				    ( int ) ( vict->player.time.played / 3600 ),
				    ( int ) ( vict->player.time.played / 60 % 60 ) );
			}
			delete ( vict );
			break;
		case 3:
			if ( !*value )
			{
				ch->Send ( "A name would help.\r\n" );
				return;
			}
			Crash_listrent ( ch, value );
			break;
		case 4:
			i = 0;
			j = 0;
			k = 0;
			con = 0;
			for ( vict = character_list; vict; vict = vict->next )
			{
				if ( IS_NPC ( vict ) )
					j++;
				else if ( CAN_SEE ( ch, vict ) )
				{
					i++;
					if ( vict->desc )
						con++;
				}
			}
			k = object_list.size();
			ch->Send (
			    "Current stats:\r\n"
			    "  %5d players in game  %5d connected\r\n"
			    "  %5d registered\r\n"
			    "  %5d mobiles          %5d prototypes\r\n"
			    "  %5d objects          %5d prototypes\r\n"
			    "  %5d rooms            %5d zones\r\n"
			    "  %5d large bufs\r\n"
			    "  %5d buf switches     %5d overflows\r\n"
			    "  %5d max mob actions per 10 seconds\r\n"
			    "  %5d min mob actions per 10 seconds\r\n"
			    "  %5d game commands parsed\r\n"
			    "  %5d commands by players parsed\r\n"
			    "  %5d command trigger commands parsed\r\n"
			    "  %5d triggers written\r\n"
			    "  %5.2f %s compressed to %5.2f %s\r\n",

			    i, con,
			    pi.TopOfTable() + 1,
			    j, GetMobProtoCount(),
			    k, top_of_objt + 1,
			    top_of_world + 1, top_of_zone_table + 1,
			    buf_largecount,
			    buf_switches, buf_overflows,
			    max_actions,
			    min_actions,
			    total_commands_typed,
			    total_pcommands_typed,
			    total_trig_commands_typed, top_of_trigt,
			    bytesToSize ( compressor.Bigs() ), bytesToUnit ( compressor.Bigs() ), bytesToSize ( compressor.Smalls() ), bytesToUnit ( compressor.Smalls() ) );

			break;

		case 5:
			DYN_CREATE;
			*dynbuf = 0;
			strcpy ( buf, "Roleplay Rooms\r\n------------\r\n" );
			DYN_RESIZE ( buf );
			for ( i = 0, k = 0; i <= top_of_world; i++ )
				for ( j = 0; j < NUM_OF_DIRS; j++ )
					if ( world_vnum[i] != NULL )
						if ( ROOM_FLAGGED ( world_vnum[i], ROOM_ROLEPLAY ) )
						{
							snprintf ( buf, sizeof ( buf ), "%2d: [%5d] %s\r\n", ++j, i,   world_vnum[i]->name );
							DYN_RESIZE ( buf );
						}


			page_string ( ch->desc, dynbuf, DYN_BUFFER );
			break;
		case 6:
			DYN_CREATE;
			*dynbuf = 0;
			strcpy ( buf, "Death Traps\r\n-----------\r\n" );
			DYN_RESIZE ( buf );
			for ( i = 0, j = 0; i <= top_of_world; i++ )
				if ( world_vnum[i] != NULL )
					if ( ROOM_FLAGGED ( world_vnum[i], ROOM_DEATH ) )
					{
						snprintf ( buf, sizeof ( buf ), "%2d: [%5d] %s\r\n", ++j, GET_ROOM_VNUM ( world_vnum[i] ),  world_vnum[i]->name );
						DYN_RESIZE ( buf );
					}
			page_string ( ch->desc, dynbuf, DYN_BUFFER );
			break;
		case 7:
			DYN_CREATE;
			*dynbuf = 0;
			strcpy ( buf, "Godrooms\r\n--------------------------\r\n" );
			DYN_RESIZE ( buf );
			for ( i = 0, j = 0; i <= top_of_world; i++ )
				if ( world_vnum[i] != NULL )
					if ( ROOM_FLAGGED ( world_vnum[i], ROOM_GODROOM ) )
					{
						snprintf ( buf, sizeof ( buf ), "%2d: [%5d] %s\r\n", ++j, GET_ROOM_VNUM ( world_vnum[i] ),   world_vnum[i]->name );
						DYN_RESIZE ( buf );
					}
			page_string ( ch->desc, dynbuf, DYN_BUFFER );
			break;
		case 8:
			show_shops ( ch, value );
			break;
		case 9:
			hcontrol_list_houses ( ch );
			break;
		case 10:
			if ( *value && is_number ( value ) )
			{
				tptr = value;
				do_connections ( ch, tptr );
			}
			break;
		case 11:
			if ( in_arena == ARENA_OFF )
			{
				ch->Send ( "The Arena is closed right now.\r\n" );
			}
			else if ( in_arena == ARENA_START )
			{
				ch->Send ( "Arena will start in %d hour(s)\r\n",
				           time_to_start );
				ch->Send ( "%sIt will last for %d hour(s)\r\n", buf,
				           game_length );
			}
			else if ( in_arena == ARENA_RUNNING )
			{
				ch->Send ( "Arena will end in %d hour(s)\r\n",
				           time_left_in_game );
			}

			break;
		case 12:
			i = 0;
			ch->Send (
			    "People currently snooping:\r\n--------------------------\r\n" );
			for ( d = descriptor_list; d; d = d->next )
			{
				if ( d->snooping == NULL || d->character == NULL )
					continue;
				if ( STATE ( d ) != CON_PLAYING
				        || GET_LEVEL ( ch ) < GET_LEVEL ( d->character ) )
					continue;
				if ( !CAN_SEE ( ch, d->character )
				        || IN_ROOM ( d->character ) == NULL )
					continue;
				i++;
				ch->Send ( "%-10s - snooped by %s.\r\n",
				           GET_NAME ( d->snooping->character ),
				           GET_NAME ( d->character ) );
			}
			if ( i == 0 )
				ch->Send ( "No one is currently snooping.\r\n" );
			break;

		case 13:
			ch->Send ( "----Gold Statistics----\r\n" );
			for ( i = 1; i <= 10; i++ )
				ch->Send ( "%18s: %lld\r\n", gold_types[i],
				           gold_data ( i, 0 ) );
			break;
		case 14:
			i = 0;
			if ( !*value )
			{
				ch->Send ( "A name would help.\r\n" );
				return;
			}
			for ( zrn = 0; zrn <= top_of_zone_table; zrn++ )
			{


				if ( is_name ( value, zone_table[zrn].builders ) )
				{
					i++;
					ch->Send ( "%d  ", zone_table[zrn].number );
					if ( ! ( i%5 ) && i>0 )
						ch->Send ( "\r\n" );
				}

			}
			if ( !i )
				ch->Send ( "%s has no zone to his/her name.", value );

			break;
		case 15:
			i = 0;
			strcpy ( value, GET_NAME ( ch ) );
			for ( zrn = 0; zrn <= top_of_zone_table; zrn++ )
			{


				if ( is_name ( value, zone_table[zrn].builders ) )
				{
					i++;
					ch->Send ( "%d  ", zone_table[zrn].number );
					if ( ! ( i%5 ) && i>0 )
						ch->Send ( "\r\n" );
				}

			}
			if ( !i )
				ch->Send ( "You have no zone to your name." );

			break;
		case 16:
			do_show_corpses ( ch );
			break;
		case 17:
			do_show_errors ( ch );
			show_door_errors ( ch );
			break;
			/* show assembly */
		case 18:
			assemblyListToChar ( ch );
			break;
		case 19:
			i = atoi ( value );
			if ( i == 0 )
				i = 2;
			show_last_logons ( ch, i, *arg ? TRUE : FALSE );
			break;
		case 20:
			do_show_trainers ( ch );
			break;
			/* show what? */
		default:
			ch->Send ( "Sorry, I don't understand that.\r\n" );
			break;
	}
}


/***************** The do_set function ***********************************/




/* The set options available */
struct set_struct
{
	const char *cmd;
	const char level;
	const char pcnpc;
	const char type;
}
set_fields[] =
{
	{"brief", LVL_GOD, PC, BINARY}, /* 0 */
	{"invstart", LVL_GOD, PC, BINARY},   /* 1 */
	{"title", LVL_GOD, PC, MISC}, 
	{"nosummon", LVL_GRGOD, PC, BINARY}, 
	{"maxhit", LVL_GRGOD, BOTH, NUMBER}, 
	{"maxmana", LVL_GRGOD, BOTH, NUMBER},     /* 5 */
	{"maxmove", LVL_GRGOD, BOTH, NUMBER}, 
	{"hit", LVL_GRGOD, BOTH, NUMBER}, 
	{"mana", LVL_GRGOD, BOTH, NUMBER}, 
	{"move", LVL_GRGOD, BOTH, NUMBER}, 
	{"align", LVL_GOD, BOTH, NUMBER},    /* 10 */
	{"str", LVL_GRGOD, BOTH, NUMBER}, 
	{"stradd", LVL_GRGOD, BOTH, NUMBER}, 
	{"int", LVL_GRGOD, BOTH, NUMBER}, 
	{"wis", LVL_GRGOD, BOTH, NUMBER}, 
	{"dex", LVL_GRGOD, BOTH, NUMBER},    /* 15 */
	{"con", LVL_GRGOD, BOTH, NUMBER}, 
	{"cha", LVL_GRGOD, BOTH, NUMBER}, 
	{"ac", LVL_GRGOD, BOTH, NUMBER}, 
	{"gold", LVL_GOD, BOTH, NUMBER}, 
	{"bank", LVL_GOD, PC, NUMBER},  /* 20 */
	{"exp", LVL_GRGOD, BOTH, NUMBER}, 
	{"hitroll", LVL_GRGOD, BOTH, NUMBER}, 
	{"damroll", LVL_GRGOD, BOTH, NUMBER}, 
	{"invis", LVL_IMPL, PC, NUMBER}, \
	{"nohassle", LVL_GRGOD, PC, BINARY}, /* 25 */
	{"frozen", LVL_FREEZE, PC, BINARY}, 
	{"practices", LVL_GRGOD, PC, NUMBER}, 
	{"lessons", LVL_GRGOD, PC, NUMBER}, 
	{"drunk", LVL_GRGOD, BOTH, MISC}, 
	{"hunger", LVL_GRGOD, BOTH, MISC},   /* 30 */
	{"thirst", LVL_GRGOD, BOTH, MISC}, 
	{"killer", LVL_GOD, PC, BINARY}, 
	{"thief", LVL_GOD, PC, BINARY}, 
	{"level", LVL_IMPL, BOTH, NUMBER}, 
	{"room", LVL_IMPL, BOTH, NUMBER},    /* 35 */
	{"roomflag", LVL_GRGOD, PC, BINARY}, 
	{"siteok", LVL_GRGOD, PC, BINARY}, 
	{"deleted", LVL_IMPL, PC, BINARY}, 
	{"class", LVL_GRGOD, BOTH, MISC}, 
	{"nowizlist", LVL_GOD, PC, BINARY},  /* 40 */
	{"quest", LVL_GOD, PC, BINARY}, 
	{"loadroom", LVL_GRGOD, PC, MISC}, 
	{"colour", LVL_GOD, PC, BINARY}, 
	{"idnum", LVL_IMPL, PC, NUMBER}, 
	{"passwd", LVL_IMPL, PC, MISC}, /* 45 */
	{"nodelete", LVL_GOD, PC, BINARY}, 
	{"sex", LVL_GRGOD, BOTH, MISC}, 
	{"age", LVL_GRGOD, BOTH, NUMBER}, 
	{"remort", LVL_IMPL, PC, MISC}, 
	{"rtwo", LVL_IMPL, PC, MISC},   /* 50 */
	{"rthree", LVL_IMPL, PC, MISC}, 
	{"race", LVL_GRGOD, BOTH, MISC}, 
	{"pregnant", LVL_IMPL, PC, NUMBER}, 
	{"breakup", LVL_SEN, PC, MISC}, 
	{"rp", LVL_SEN, PC, BINARY},    /* 55 */
	{"pk", LVL_SEN, PC, BINARY}, 
	{"brasst", LVL_SEN, PC, NUMBER}, 
	{"bronzet", LVL_SEN, PC, NUMBER}, 
	{"silvert", LVL_SEN, PC, NUMBER}, 
	{"goldt", LVL_SEN, PC, NUMBER}, /* 60 */
	{"clan", LVL_SEN, PC, NUMBER}, 
	{"rank", LVL_SEN, PC, NUMBER}, 
	{"height", LVL_GOD, BOTH, NUMBER}, 
	{"weight", LVL_GOD, BOTH, NUMBER}, 
	{"helper", LVL_SEN, PC, BINARY},     /* 65 */
	{"spec01", LVL_SEN, PC, BINARY}, 
	{"spec02", LVL_SEN, PC, BINARY}, 
	{"spec03", LVL_SEN, PC, BINARY}, 
	{"spec04", LVL_SEN, PC, BINARY},     /* 69 */
	{"speed", LVL_SEN, BOTH, NUMBER},
	{"coventry", LVL_SEN, PC, BINARY},
	{"pretitle", LVL_SEN, PC, MISC},
	{"rpgroup", LVL_SEN, PC, MISC},
	{ "olc", LVL_IMPL, PC,  MISC },
	{ "hero", LVL_SEN,  PC,  BINARY }, /* 75 */
	{"immtitle", LVL_IMPL, PC, MISC},
	{"mastery", LVL_IMPL, PC, MISC}, /* 77 */
	{"rpl", LVL_GOD, PC, BINARY},
	{"tradepoints",LVL_GOD,PC,NUMBER},
	{"pet", LVL_SEN, PC, NUMBER},
	{"wizmort", LVL_SEN, PC, BINARY},
	{"body", LVL_SEN, PC, MISC},
	{"trains", LVL_SEN, NPC, MISC},
	{"outcast", LVL_SEN, PC, BINARY}, /* 84 */
        {"ethos", LVL_SEN, PC, NUMBER},
	{"automeld", LVL_CRT, PC, NUMBER},
	{   "\n", 0, BOTH, MISC}
};

int perform_set ( Character *ch, Character *vict, int mode,
                  char *val_arg )
{
	int i, on = 0, off = 0, value = 0;
	room_rnum rnum;
	room_vnum rvnum;
	char buf[MAX_STRING_LENGTH];
	int parse_race ( char* arg, bool consider_gladiator );
	void set_race ( Character *ch, int race );
	obj_data* tmp_obj;

	/* Check to make sure all the levels are correct */
	if ( GET_LEVEL ( ch ) != LVL_IMPL )
	{
		if ( !IS_NPC ( vict ) && GET_LEVEL ( ch ) <= GET_LEVEL ( vict )
		        && vict != ch )
		{
			ch->Send ( "Maybe that's not such a great idea...\r\n" );
			return ( 0 );
		}
	}
	if ( GET_LEVEL ( ch ) < set_fields[mode].level )
	{
		ch->Send ( "You are not godly enough for that!\r\n" );
		return ( 0 );
	}

	/* Make sure the PC/NPC is correct */
	if ( IS_NPC ( vict ) && ! ( set_fields[mode].pcnpc & NPC ) )
	{
		ch->Send ( "You can't do that to a beast!\r\n" );
		return ( 0 );
	}
	else if ( !IS_NPC ( vict ) && ! ( set_fields[mode].pcnpc & PC ) )
	{
		ch->Send ( "That can only be done to a beast!\r\n" );
		return ( 0 );
	}

	/* Find the value of the argument */
	if ( set_fields[mode].type == BINARY )
	{
		if ( !strcmp ( val_arg, "on" ) || !strcmp ( val_arg, "yes" ) )
			on = 1;
		else if ( !strcmp ( val_arg, "off" ) || !strcmp ( val_arg, "no" ) )
			off = 1;
		if ( ! ( on || off ) )
		{
			ch->Send ( "Value must be 'on' or 'off'.\r\n" );
			return ( 0 );
		}
		snprintf ( buf, sizeof ( buf ), "%s %s for %s.", set_fields[mode].cmd, ONOFF ( on ), GET_NAME ( vict ) );
	}
	else if ( set_fields[mode].type == NUMBER )
	{
		value = atoi ( val_arg );
		snprintf ( buf, sizeof ( buf ), "%s's %s set to %d.", GET_NAME ( vict ), set_fields[mode].cmd, value );
	}
	else
	{
		snprintf ( buf, sizeof ( buf ), "Okay." );    /* can't use OK macro here 'cause of \r\n */
	}

	switch ( mode )
	{
		case 0:
			SET_OR_REMOVE ( PRF_FLAGS ( vict ), PRF_BRIEF );
			break;
		case 1:
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_INVSTART );
			break;
		case 2:
			set_title ( vict, val_arg );
			snprintf ( buf, sizeof ( buf ), "%s's title is now: %s", GET_NAME ( vict ),
			           GET_TITLE ( vict ) );
			break;
		case 3:
			SET_OR_REMOVE ( PRF_FLAGS ( vict ), PRF_SUMMONABLE );
			snprintf ( buf, sizeof ( buf ), "Nosummon %s for %s.\r\n", ONOFF ( !on ),
			           GET_NAME ( vict ) );
			break;
		case 4:
			vict->points.max_hit = RANGE ( 1, 1000000 );
			vict->affect_total();  /* affect_total() handles regen updates */
			break;
		case 5:
			vict->points.max_mana = RANGE ( 1, 1000000 );
			vict->affect_total();  /* affect_total() handles regen updates */
			break;
		case 6:
			vict->points.max_move = RANGE ( 1, 1000000 );
			vict->affect_total();  /* affect_total() handles regen updates */
			break;
		case 7:
			vict->points.hit = RANGE ( -9, vict->points.max_hit );
			vict->affect_total();  /* affect_total() handles regen updates */
			break;
		case 8:
			vict->points.mana = RANGE ( 0, vict->points.max_mana );
			vict->affect_total();  /* affect_total() handles regen updates */
			break;
		case 9:
			vict->points.move = RANGE ( 0, vict->points.max_move );
			vict->affect_total();  /* affect_total() handles regen updates */
			break;
		case 10:
			GET_ALIGNMENT ( vict ) = RANGE ( -1000, 1000 );
			vict->affect_total();
			break;
		case 11:
			if ( IS_NPC ( vict ) || GET_LEVEL ( vict ) >= LVL_GRGOD )
				RANGE ( 3, 25 );
			else
				RANGE ( 3, 18 );
			vict->real_abils.str = value;
			vict->real_abils.str_add = 0;
			vict->affect_total();
			break;
		case 12:
			vict->real_abils.str_add = RANGE ( 0, 100 );
			if ( value > 0 )
				vict->real_abils.str = 18;
			vict->affect_total();
			break;
		case 13:
			if ( IS_NPC ( vict ) || GET_LEVEL ( vict ) >= LVL_GRGOD )
				RANGE ( 3, 25 );
			else
				RANGE ( 3, 18 );
			vict->real_abils.intel = value;
			vict->affect_total();
			break;
		case 14:
			if ( IS_NPC ( vict ) || GET_LEVEL ( vict ) >= LVL_GRGOD )
				RANGE ( 3, 25 );
			else
				RANGE ( 3, 18 );
			vict->real_abils.wis = value;
			vict->affect_total();
			break;
		case 15:
			if ( IS_NPC ( vict ) || GET_LEVEL ( vict ) >= LVL_GRGOD )
				RANGE ( 3, 25 );
			else
				RANGE ( 3, 18 );
			vict->real_abils.dex = value;
			vict->affect_total();
			break;
		case 16:
			if ( IS_NPC ( vict ) || GET_LEVEL ( vict ) >= LVL_GRGOD )
				RANGE ( 3, 25 );
			else
				RANGE ( 3, 18 );
			vict->real_abils.con = value;
			vict->affect_total();
			break;
		case 17:
			if ( IS_NPC ( vict ) || GET_LEVEL ( vict ) >= LVL_GRGOD )
				RANGE ( 3, 100 );
			else
				RANGE ( 3, 100 );
			vict->real_abils.cha = value;
			vict->affect_total();
			break;
		case 18:
			vict->points.armor = RANGE ( -100, 100 );
			vict->affect_total();
			break;
		case 19:
			vict->Gold ( RANGE ( 0, 1000000000 ) - vict->Gold ( 0, GOLD_HAND ), GOLD_HAND );
			break;
		case 20:
			vict->Gold ( RANGE ( 0, 1000000000 ) - vict->Gold ( 0, GOLD_BANK ), GOLD_BANK );
			break;
		case 21:
			vict->points.exp = RANGE ( 0, 2000000000 );
			break;
		case 22:
			vict->points.hitroll = RANGE ( -20, 120 );
			vict->affect_total();
			break;
		case 23:
			vict->points.damroll = RANGE ( -20, 120 );
			vict->affect_total();
			break;
		case 24:
			if ( GET_LEVEL ( ch ) < LVL_IMPL && ch != vict )
			{
				ch->Send ( "You aren't godly enough for that!\r\n" );
				return ( 0 );
			}
			GET_INVIS_LEV ( vict ) = RANGE ( 0, GET_LEVEL ( vict ) );
			break;
		case 25:
			if ( GET_LEVEL ( ch ) < LVL_IMPL && ch != vict )
			{
				ch->Send ( "You aren't godly enough for that!\r\n" );
				return ( 0 );
			}
			SET_OR_REMOVE ( PRF_FLAGS ( vict ), PRF_NOHASSLE );
			break;
		case 26:
			if ( ch == vict && on )
			{
				ch->Send ( "Better not -- could be a long winter!\r\n" );
				return ( 0 );
			}
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_FROZEN );
			break;
		case 27:
		case 28:
			GET_PRACTICES ( vict ) = RANGE ( 0, 2000 );
			break;
		case 29:
		case 30:
		case 31:
			if ( !str_cmp ( val_arg, "off" ) )
			{
				GET_COND ( vict, ( mode - 29 ) ) = ( char ) -1;    /* warning: magic number here */
				snprintf ( buf, sizeof ( buf ), "%s's %s now off.", GET_NAME ( vict ),
				           set_fields[mode].cmd );
			}
			else if ( is_number ( val_arg ) )
			{
				value = atoi ( val_arg );
				RANGE ( 0, 24 );
				GET_COND ( vict, ( mode - 29 ) ) = ( char ) value; /* and here too */
				snprintf ( buf, sizeof ( buf ), "%s's %s set to %d.", GET_NAME ( vict ),
				           set_fields[mode].cmd, value );
			}
			else
			{
				ch->Send ( "Must be 'off' or a value from 0 to 24.\r\n" );
				return ( 0 );
			}
			vict->check_regen_rates();
			break;
		case 32:
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_KILLER );
			break;
		case 33:
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_THIEF );
			break;
		case 34:
			if ( value > GET_LEVEL ( ch ) || value > LVL_IMPL )
			{
				ch->Send ( "You can't do that.\r\n" );
				return ( 0 );
			}
			RANGE ( 0, LVL_IMPL );
			GET_ORIG_LEV ( vict ) = 0;
			vict->player.level = ( sbyte ) value;
			break;
		case 35:
			if ( ( rnum = real_room ( value ) ) != NULL )
			{
				ch->Send ( "No room exists with the number %d.\r\n", value );
				return ( 0 );
			}
			move_char_to ( vict, rnum );
			break;
		case 36:
			SET_OR_REMOVE ( PRF_FLAGS ( vict ), PRF_ROOMFLAGS );
			break;
		case 37:
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_SITEOK );
			break;
		case 38:
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_DELETED );
			break;
		case 39:
			if ( ( i = parse_class ( *val_arg ) ) == CLASS_UNDEFINED )
			{
				ch->Send ( "That is not a class.\r\n" );
				return ( 0 );
			}
			GET_CLASS ( vict ) = i;
			vict->check_regen_rates();
			break;
		case 40:
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_NOWIZLIST );
			break;
		case 41:
			SET_OR_REMOVE ( PRF_FLAGS ( vict ), PRF_QUEST );
			break;
		case 42:
			if ( !str_cmp ( val_arg, "off" ) )
			{
				REMOVE_BIT_AR ( PLR_FLAGS ( vict ), PLR_LOADROOM );
			}
			else if ( is_number ( val_arg ) )
			{
				rvnum = atoi ( val_arg );
				if ( real_room ( rvnum ) != NULL )
				{
					SET_BIT_AR ( PLR_FLAGS ( vict ), PLR_LOADROOM );
					GET_LOADROOM ( vict ) = rvnum;
					snprintf ( buf, sizeof ( buf ), "%s will enter at room #%d.",
					           GET_NAME ( vict ), GET_LOADROOM ( vict ) );
				}
				else
				{
					ch->Send ( "That room does not exist!\r\n" );
					return ( 0 );
				}
			}
			else
			{
				ch->Send ( "Must be 'off' or a room's virtual number.\r\n" );
				return ( 0 );
			}
			break;
		case 43:
			SET_OR_REMOVE ( PRF_FLAGS ( vict ), PRF_COLOUR_1 );
			SET_OR_REMOVE ( PRF_FLAGS ( vict ), PRF_COLOUR_2 );
			break;
		case 44:
			if ( !IS_NPC ( vict ) )
				return ( 0 );
			GET_IDNUM ( vict ) = value;
			break;
		case 45:
			if ( GET_LEVEL ( ch ) < LVL_SEN )
			{
				ch->Send ( "You cannot change that.\r\n" );
				return ( 0 );
			}
			strncpy ( GET_PASSWD ( vict ), CRYPT ( val_arg, GET_NAME ( vict ) ), MAX_PWD_LENGTH );
			* ( GET_PASSWD ( vict ) + MAX_PWD_LENGTH ) = '\0';
			snprintf ( buf, sizeof ( buf ), "Password changed to '%s'.", val_arg );
			break;
		case 46:
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_NODELETE );
			break;
		case 47:
			if ( ( i = search_block ( val_arg, genders, FALSE ) ) < 0 )
			{
				ch->Send ( "Must be 'male', 'female', or 'neutral'.\r\n" );
				return ( 0 );
			}
			GET_SEX ( vict ) = i;
			break;
		case 48:               /* set age */
			if ( (value < 2 || value > 200) && value != -1 )     /* Arbitrary limits. */
			{
				ch->Send ( "Ages 2 to 200 accepted, or -1 to remove fixed age.\r\n" );
				return ( 0 );
			}
			/*
			 * NOTE: May not display the exact age specified due to the integer
			 * division used elsewhere in the code.  Seems to only happen for
			 * some values below the starting age (17) anyway. -gg 5/27/98
			 * 
			 * NOTE2: new code which doesn't care about this -Thotter
			 */
			//vict->player.time.birth =
			//    time ( 0 ) - ( value - 17 * SECS_PER_MUD_YEAR );
			//vict->check_regen_rates();
			SPECIALS(vict)->age = value;
			break;
		case 49:
			if ( ( i = parse_class ( *val_arg ) ) == CLASS_UNDEFINED )
			{
				send_to_char ( "Removing remort: ", ch );
				if ( REMORTS ( vict ) > 0 )
					REMORTS ( vict )--;
			}
			else if ( GET_REMORT ( vict ) == CLASS_UNDEFINED )
				REMORTS ( vict ) ++;

			GET_REMORT ( vict ) = i;
			vict->check_regen_rates();
			break;
		case 50:
			if ( ( i = parse_class ( *val_arg ) ) == CLASS_UNDEFINED )
			{
				send_to_char ( "Removing rtwo: ", ch );
				if ( REMORTS ( vict ) > 0 )
					REMORTS ( vict )--;
			}
			else if ( GET_REMORT_TWO ( vict ) == CLASS_UNDEFINED )
				REMORTS ( vict ) ++;

			GET_REMORT_TWO ( vict ) = i;
			vict->check_regen_rates();
			break;
		case 51:
			if ( ( i = parse_class ( *val_arg ) ) == CLASS_UNDEFINED )
			{
				send_to_char ( "Removing rthree: ", ch );
				if ( REMORTS ( vict ) > 0 )
					REMORTS ( vict )--;
			}
			else if ( GET_REMORT_THREE ( vict ) == CLASS_UNDEFINED )
				REMORTS ( vict ) ++;

			GET_REMORT_THREE ( vict ) = i;
			vict->check_regen_rates();
			break;
		case 52:
			if ( ( i = parse_race ( val_arg, true ) ) == RACE_UNDEFINED )
			{
				send_to_char ( "That is not a race.\r\n", ch );
				return 0;
			}
			else
				set_race ( vict, i );
			vict->check_regen_rates();
			break;

		case 53:

			if ( ( value < -1 ) || ( value >= 6600 ) ) /* Limits */
			{
				send_to_char ( "Preg must be between -1 and 6600.\r\n", ch );
				return 0;
			}
			if ( value == 0 )
				PREG ( vict ) = NOT_PREG;
			else
				if ( value == PREG ( vict ) )
				{
					send_to_char ( "But preg is already at that value!\r\n", ch );
					return 0;
				}

			if ( value > PREG ( vict ) )
			{
				act ( "You make an interesting, shamanistic gesture at $N's stomach.\r\nIt quickly shrinks down!\r\n", TRUE, ch, 0, vict, TO_CHAR );
				act ( "$n makes an interesting, shamanistic gesture at your stomach.\r\nYou feel a twinge of pain, and your stomach shrinks rapidly!\r\n", TRUE, ch, 0, vict, TO_VICT );
				act ( "$n makes an interesting, shamanistic gesture at $N's stomach.\r\nIt quickly shrinks down!\r\n", TRUE, ch, 0, vict, TO_NOTVICT );
				PREG ( vict ) = value;
			}
			else if ( value < PREG ( vict ) )
			{
				act ( "You make a mystical, shamanistic gesture at $N's stomach.\r\nIt bulges and rapidly grows!\r\n", TRUE, ch, 0, vict, TO_CHAR );
				act ( "$n makes a mystical, shamanistic gesture at your stomach.\r\nYou feel a twinge of pain, and your stomach expands!\r\n", TRUE, ch, 0, vict, TO_VICT );
				act ( "$n makes a mystical, shamanistic gesture at $N's stomach.\r\nIt quickly bulges and expands!\r\n", TRUE, ch, 0, vict, TO_NOTVICT );
				PREG ( vict ) = value;
			}
			vict->check_regen_rates();
			break;               /* End Mating Mod Debug Code */

		case 54:
			ROMANCE ( vict ) = 0;
			PARTNER ( vict ) = 0;
			send_to_char ( "Your victim is no longer romantically involved.\r\n",
			               ch );
			break;

		case 55:
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_ROLEPLAYER );
			send_to_char ( "Done.\r\n", ch );
			break;

		case 56:
			SET_OR_REMOVE ( PLR_FLAGS ( vict ), PLR_PK );
			send_to_char ( "Done.\r\n", ch );
			break;

		case 57:
			GET_BRASS_TOKEN_COUNT ( vict ) += value;
			break;

		case 58:
			GET_BRONZE_TOKEN_COUNT ( vict ) += value;
			break;

		case 59:
			GET_SILVER_TOKEN_COUNT ( vict ) += value;
			break;

		case 60:
			GET_GOLD_TOKEN_COUNT ( vict ) += value;
			break;

		case 61:
			if ( value != 0 && find_clan_by_id ( value ) == -1 )
				snprintf ( buf, sizeof ( buf ), "That clan id doesn't exist, see clan info." );
			else
				GET_CLAN ( vict ) = value;
			break;

		case 62:
			GET_CLAN_RANK ( vict ) = value;
			break;

		case 63:
			GET_HEIGHT ( vict ) = value;
			vict->affect_total();
			break;

		case 64:
			GET_WEIGHT ( vict ) = value;
			vict->affect_total();
			break;

    case 65:
        SET_OR_REMOVE(PLR_FLAGS(vict), PLR_NEWBIE_HLPR);
        send_to_char("Done.\r\n", ch);
        break;
    case 66:
        if (!str_cmp(val_arg, "on")) {
            send_to_char("Done.\r\n", ch);
            GET_CLASS_TIER(vict) = 1;
        } else if (!str_cmp(val_arg, "off")) {
            send_to_char("Done.\r\n", ch);
            GET_CLASS_TIER(vict) = 0;
        } else
            send_to_char("You Must Specify: spec or multi\r\n", ch);
        break;
    case 67:
        if (!str_cmp(val_arg, "on")) {
            send_to_char("Done.\r\n", ch);
            GET_REMORT_TIER(vict) = 1;
        } else if (!str_cmp(val_arg, "off")) {
            send_to_char("Done.\r\n", ch);
            GET_REMORT_TIER(vict) = 0;
        } else
            send_to_char("You Must Specify: spec or multi\r\n", ch);
        vict->check_regen_rates();
        break;
    case 68:
        if (!str_cmp(val_arg, "on")) {
            send_to_char("Done.\r\n", ch);
            GET_REMORT_TWO_TIER(vict) = 1;
        } else if (!str_cmp(val_arg, "off")) {
            send_to_char("Done.\r\n", ch);
            GET_REMORT_TWO_TIER(vict) = 0;
        } else
            send_to_char("You Must Specify: spec or multi\r\n", ch);
        vict->check_regen_rates();
        break;
    case 69:
        if (!str_cmp(val_arg, "on")) {
            send_to_char("Done.\r\n", ch);
            GET_REMORT_THREE_TIER(vict) = 1;
        } else if (!str_cmp(val_arg, "off")) {
            send_to_char("Done.\r\n", ch);
            GET_REMORT_THREE_TIER(vict) = 0;
        } else
            send_to_char("You Must Specify: spec or multi\r\n", ch);
        vict->check_regen_rates();
        break;
    case 70:
        AFF_SPEED(vict) = value;
        break;
    case 71:
        SET_OR_REMOVE_AR(PLR_FLAGS(vict), PLR_COVENTRY);
        break;
    case 72:
        send_to_char("unfinished\r\n", ch);
        break;
    case 73:
        i = 0;
        if (isname(val_arg, "jesters"))
            i = 1;
        else if (isname(val_arg, "bitches"))
            i = 2;
        else if (isname(val_arg, "riddlers"))
            i = 3;
        else if (isname(val_arg, "madmen"))
            i = 4;
        else if (isname(val_arg, "orsinis"))
            i = 5;
        else if (isname(val_arg, "Lolthite"))
            i = 6;
	else if (isname(val_arg, "Cthulytes"))
	    i = 7;
        else if (isname(val_arg, "Alderisio"))
            i = 8;
        else if (isname(val_arg, "Fearless"))
            i = 9;
        else if (isname(val_arg, "Galliano"))
            i = 10;
	else if (isname(val_arg, "Savages"))
	    i = 11;
        else if (!strcmp(val_arg, "none"))
            i = 0;
	else {
	  ch->Send ( "There is no group with that name.\r\n" );
	  return 0;
	}
        GET_RP_GROUP(vict) = i;
        break;
    case 74:
        if (is_abbrev(val_arg, "socials") || is_abbrev(val_arg, "actions"))
            GET_OLC_ZONE(vict) = AEDIT_PERMISSION;
        else if (is_abbrev(val_arg, "off"))
            GET_OLC_ZONE(vict) = NOWHERE;
        else if (!is_number(val_arg)) {
            ch->Send( "Value must be either 'socials', 'actions', 'off' or a zone number.\r\n");
            return (0);
        } else
            GET_OLC_ZONE(vict) = atoi(val_arg);
        break;
    case 75:
        SET_OR_REMOVE_AR(PLR_FLAGS(vict), PLR_HERO);
        break;
    case 76:
        if (strlen(val_arg) > 20) {
            ch->Send( "Sorry it must be 20 characters or less\r\n");
            return 0;
        }
        if (strstr(val_arg, "{")) {
            ch->Send( "Sorry, no colour codes please.\r\n");
            return 0;
        }
        free_string(&IMMTITLE(vict));
        if (val_arg && *val_arg)
            IMMTITLE(vict) = strdup(val_arg);
        snprintf(buf, sizeof(buf), "Imm Title of %s set to: %s",GET_NAME(vict), val_arg);
        break;
    case 77:
        if ((i = parse_class(*val_arg)) == CLASS_UNDEFINED) {
            send_to_char("That is not a class.\r\n", ch);
            return 0;
        }
        GET_MASTERY(vict, i) = !GET_MASTERY(vict, i);
        break;
    case 78:
        SET_OR_REMOVE_AR(PLR_FLAGS(vict), PLR_RP_LEADER);
        break;
    case 79:
        if(!isdigit(*val_arg)) {
            if(*val_arg=='-')
                send_to_char("Please specify only positive numbers.\r\n",ch);
            else
                send_to_char("That is not a number.\r\n", ch);
            return 0;
        }
        snprintf(buf, sizeof(buf), "TradeP of %s set to: %s",GET_NAME(vict), val_arg);
        TRADEPOINTS(vict)=atoi(val_arg);
        break;
    case 80:
        if(!isdigit(*val_arg)) {
            if(*val_arg=='-' || is_abbrev(val_arg, "off"))
                vict->pet = -1;
            else
                *ch << "That is not a number.\r\n";
            return 0;
        }
        snprintf(buf, sizeof(buf), "Pet of %s set to: %s",GET_NAME(vict), val_arg);
        vict->pet = atoi(val_arg);
        break;
    case 81:
        SET_OR_REMOVE_AR(PLR_FLAGS(vict), PLR_IMM_MORT);
        break;
    case 82:
        if (bodypartname(val_arg) != -1) {
            snprintf(buf, sizeof(buf), "%s's body part %s is now %s", GET_NAME(vict),
                     val_arg, ONOFF(togglebody(vict, bodypartname(val_arg))));
        } else {
            ch->Send("You can toggle these bits: \r\n");
            ch->Send("rthumb lthumb saddle eartip lshoulder\r\n");
            ch->Send("rshoulder crest lthigh rthigh lknee\r\n");
            ch->Send("rknee floating back chest\r\n");
        }
        break;
    case 83:
		parse_train_group(ch, vict, val_arg);
		break;
    case 84:
		SET_OR_REMOVE_AR(AFF_FLAGS(vict), AFF_OUTCAST);
		break;
    case 85:
                GET_ETHOS(vict) = value;
	        break;
    case 86:
	        tmp_obj = find_corpse(vict);
		if (!tmp_obj) {
			ch->Send("Corpse not found.\r\n");
			return 0;
		}
		else if (value <= 0) {
			automeld(tmp_obj);
		}
		else {
			GET_OBJ_TIMER(tmp_obj) = value;
			GET_OBJ_EXPIRE ( tmp_obj ) = ( GET_OBJ_TIMER ( tmp_obj ) * SECS_PER_MUD_HOUR ) + time ( 0 );
			save_corpses();
		}
		break;

    default:
        ch->Send( "Can't set that!\r\n");
        return (0);
    }


	ch->Send ( "%s\r\n", CAP ( buf ) );
	log ( "%s: %s", GET_NAME ( ch ), buf );
	return ( 1 );
}

ACMD ( do_saveall )
{
	if ( GET_LEVEL ( ch ) < LVL_BUILDER )
		*ch << "You are not holy enough to use this privelege.\r\n";
	else
	{
		save_all();
		*ch << "World files saved.\r\n";
	}
}

ACMD ( do_password )
{
	char arg[MAX_INPUT_LENGTH];
	char buf2[MAX_INPUT_LENGTH];

	argument = two_arguments ( argument, arg, buf2 );

	if ( !*arg || !*buf2 )
	{
		ch->Send ( "password <old pass> <new pass>\r\n" );
		return;
	}

	if ( !*buf2 || strlen ( buf2 ) > MAX_PWD_LENGTH || strlen ( buf2 ) < 3 ||
	        !str_cmp ( buf2, GET_PC_NAME ( ch ) ) )
	{
		ch->Send ( "Illegal password.\r\n " );
		return;
	}

	if ( strncmp ( CRYPT ( arg, GET_PASSWD ( ch ) ), GET_PASSWD ( ch ), MAX_PWD_LENGTH ) )
	{
		ch->Send ( "Invalid password.\r\n" );
		return;
	}

	strncpy ( GET_PASSWD ( ch ), CRYPT ( buf2, GET_NAME ( ch ) ), MAX_PWD_LENGTH );
	* ( GET_PASSWD ( ch ) + MAX_PWD_LENGTH ) = '\0';
	ch->Send ( "Password changed to '%s'.", buf2 );
	ch->save();

}


ACMD ( do_set )
{
	Character *vict = NULL, *cbuf = NULL;
	char field[MAX_INPUT_LENGTH], name[MAX_INPUT_LENGTH],
	val_arg[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH];
	int mode, len, player_i = 0, retval, i, n;
	char is_file = 0, is_player = 0;

	half_chop ( argument, name, buf );

	if ( !strcmp ( name, "file" ) )
	{
		is_file = 1;
		half_chop ( buf, name, buf );
	}
	else if ( !str_cmp ( name, "player" ) )
	{
		is_player = 1;
		half_chop ( buf, name, buf );
	}
	else if ( !str_cmp ( name, "mob" ) )
		half_chop ( buf, name, buf );

	half_chop ( buf, field, buf );
	strcpy ( val_arg, buf );

	if ( !*name || !*field )
	{
		ch->Send ( "Usage: set <victim> <field> <value>\r\n"
					"Fields available to you:\r\n" );
		for ( i = 0, n = 0; set_fields[ i ].cmd[ 0 ] != '\n'; ++i )
			if ( GET_LEVEL ( ch ) >= set_fields[ i ].level )
			{
				ch->Send ( "%18s", set_fields[ i ].cmd );
				if ( n++ % 4 == 3 )
					ch->Send ( "\r\n" );
			}
		if ( n % 4 != 0 )
			ch->Send ( "\r\n" );
		return;
	}

	/* find the target */
	if ( !is_file )
	{
		if ( is_player )
		{
			if ( ! ( vict = get_player_vis ( ch, name, NULL, FIND_CHAR_WORLD ) ) )
			{
				send_to_char ( "There is no such player.\r\n", ch );
				return;
			}
		}
		else            /* is_mob */
		{
			if ( ! ( vict = get_char_vis ( ch, name, NULL, FIND_CHAR_WORLD ) ) )
			{
				send_to_char ( "There is no such creature.\r\n", ch );
				return;
			}
		}
	}
	else if ( is_file )
	{
		/* try to load the player off disk */
		cbuf = new Character ( FALSE );
		if ( ( player_i = pi.LoadChar ( name, cbuf ) ) > -1 )
		{
			if ( GET_LEVEL ( cbuf ) >= GET_LEVEL ( ch ) )
			{
				*ch << "Sorry, you can't do that.\r\n";
				return;
			}
			vict = cbuf;
		}
		else
		{
			delete ( cbuf );
			*ch << "There is no such player.\r\n";
			return;
		}
	}

	/* find the command in the list */
	len = strlen ( field );
	for ( mode = 0; * ( set_fields[mode].cmd ) != '\n'; mode++ )
		if ( !strncmp ( field, set_fields[mode].cmd, len ) )
			break;

	/* perform the set */
	retval = perform_set ( ch, vict, mode, val_arg );

	/* save the character if a change was made */
	if ( retval )
	{
		if ( !is_file && !IS_NPC ( vict ) )
		{
			vict->save();
		}
		if ( is_file )
		{
			*ch << "Saved in file.\r\n";
		}
	}


}

void out_rent ( const char *name )
{
	FILE *fl, *fp;
	char ofname[MAX_INPUT_LENGTH], buf[MAX_STRING_LENGTH],  fname2[MAX_INPUT_LENGTH];
	struct obj_file_elem object;
	struct obj_data *obj;
	struct rent_info rent;

	if ( !get_filename ( name, ofname, CRASH_FILE ) )
		return;
	if ( ! ( fl = fopen ( ofname, "rb" ) ) )
	{
		return;
	}

	snprintf ( buf, sizeof ( buf ), "%s\r\n", ofname );
	if ( !feof ( fl ) )
		fread ( &rent, sizeof ( struct rent_info ), 1, fl );

	if ( !get_filename ( name, fname2, NEW_OBJ_FILES ) )
	{
		log ( "Unable to complete conversion - unable to get new object filename.\r\n" );
		return;
	}
	if ( ! ( fp = fopen ( fname2, "w+" ) ) )
	{
		log ( "Unable to open new object file.\r\n" );
		return;
	}

	fprintf ( fp, "%d %d %d %d %d %d\r\n", rent.rentcode, rent.time,
	          rent.net_cost_per_diem, rent.gold, rent.account, rent.nitems );
	/* for rent code */

	while ( !feof ( fl ) )
	{
		fread ( &object, sizeof ( struct obj_file_elem ), 1, fl );

		if ( ferror ( fl ) || ferror ( fp ) )
		{
			fclose ( fl );
			fclose ( fp );
			log ( "Error in conversion of rent files." );
			return;
		}

		if ( !feof ( fl ) )
		{
			if ( real_object ( object.item_number ) > 0 )
			{
				/* none of these will be unique items. just can't happen */
				obj = read_object ( object.item_number, VIRTUAL );
				if ( obj )
				{
					my_obj_save_to_disk ( fp, obj, 0 );
					extract_obj ( obj );
				}
			}
		}
	}
	fclose ( fl );

	/* write final line - this is never actually read.. but hey! */
	fprintf ( fp, "$~\n" );
	fclose ( fp );
}

/* Xap - To keep old rent files (binary) and not loose all objects, you'll
   have to convert to ascii files.  Guess what?  This doesn't work as well
   as you'd think.  Well, It works, but here's some limitations:
    1. Sucks up memory. I don't know why. Maybe its specific to my mud,
        but I'm pretty sure I'm freeing all that needs it.
    2. It takes time.  You'll notice I use the process output command
        a few times.  This is because with something like 2500 rent files,
        it can take up to 2 or 3 minutes.
                I guess, use at your own risk. */

ACMD ( do_objconv )
{
	int counter;
	float percent;
	Character *victim;
	int flag25 = 0, flag50 = 0, flag75 = 0, flag100 = 0;
	int process_output ( Descriptor *t );
	Descriptor *d;

	send_to_all ( "Please hold on - object conversion taking place.\r\n" );

	for ( d = descriptor_list; d; d = d->next )
	{
		process_output ( d );
	}

	if ( GET_LEVEL ( ch ) != LVL_IMPL && !IS_NPC ( ch ) )
	{
		send_to_char ( "You may not.\r\n", ch );
		return;
	}
	/* okay, this is where we load every char, apparently.. but we'll
	   do it one at a time, thank you very much */
	for ( counter = 0; counter < pi.Size(); counter++ )
	{
		victim = new Character ( FALSE );
		if ( pi.LoadChar ( pi.NameByIndex ( counter ), victim ) > -1 )
			out_rent ( GET_NAME ( victim ) );
		delete ( victim );
		percent = ( float ) ( ( float ) counter / ( float ) pi.TopOfTable() );
		if ( percent > .25 && flag25 == 0 )
		{
			send_to_char ( "25%...", ch );
			flag25 = 1;
			process_output ( ch->desc );
		}
		if ( percent > .50 && flag50 == 0 )
		{
			send_to_char ( "50%...", ch );
			flag50 = 1;
			process_output ( ch->desc );
		}
		if ( percent > .75 && flag75 == 0 )
		{
			send_to_char ( "75%...", ch );
			flag75 = 1;
			process_output ( ch->desc );
		}
		if ( percent > .90 && flag100 == 0 )
		{
			send_to_char ( "90%...", ch );
			flag100 = 1;
			process_output ( ch->desc );
		}
	}
	send_to_char ( "100%. Done.\r\n", ch );
}

ACMD ( do_own )
{
	char what[MAX_INPUT_LENGTH];
	char owner[MAX_INPUT_LENGTH];
	long idnum;
	OBJ_DATA *obj;

	two_arguments ( argument, what, owner );

	if ( !*what || !*owner )
	{
		ch->Send ( "OWN <objectname in inventory> <idnum of player or 0 for none>\r\n" );
		return;
	}

	if ( ! ( obj = get_obj_in_list_vis ( ch, what, NULL, ch->carrying ) ) )
	{
		ch->Send ( "You don't seem to have any %ss.\r\n", what );
	}
	if ( !is_number ( owner ) )
	{
		ch->Send ( "%s is not an ID number.\r\n", owner );
		return;
	}
	idnum = atol ( owner );
	if ( idnum == 0 )
	{
		ch->Send ( "Setting %s to NO owner.\r\n", obj->short_description );
		obj->owner = 0;
		return;
	}

	ch->Send ( "Setting %s's owner to %s.\r\n", obj->short_description, pi.NameById ( idnum ) );
	obj->owner = idnum;


}

ACMD ( do_hackinvis )
{
	int nr, found = 0;
	int check_item_hack_invis ( struct obj_data *obj, int fix );
	char buf[MAX_INPUT_LENGTH];
	DYN_DEFINE;
	*buf = 0;

	DYN_CREATE;
	*dynbuf = 0;

	for ( nr = 0; nr <= top_of_objt; nr++ )
	{
		if ( check_item_hack_invis ( &obj_proto[nr], FALSE ) )
		{
			snprintf ( buf, sizeof ( buf ), "%5d. [%5d] %s\r\n", ++found,
			           obj_index[nr].vnum, obj_proto[nr].short_description );
			DYN_RESIZE ( buf );
		}
	}

	if ( !found )
		send_to_char ( "No objects were found in those parameters.\r\n", ch );
	else
		page_string ( ch->desc, dynbuf, DYN_BUFFER );

}


int check_item_hack_invis ( struct obj_data *obj, int fix )
{
	char buf[MAX_INPUT_LENGTH];
	if ( strlen ( obj->description ) < 4 && strchr ( obj->description, '{' ) )
	{
		if ( fix )
		{
			SET_BIT_AR ( GET_OBJ_EXTRA ( obj ), ITEM_NODISPLAY );
			free_string ( &obj->description );
			snprintf ( buf, sizeof ( buf ), "%s lies here", obj->short_description );
			obj->description = strdup ( buf );
		}
		return 1;
	}
	else
		return 0;
}

ACMD ( do_potionweight )
{

	int nr, found = 0;
	char buf[MAX_INPUT_LENGTH];
	DYN_DEFINE;
	*buf = 0;

	DYN_CREATE;
	*dynbuf = 0;

	for ( nr = 0; nr <= top_of_objt; nr++ )
	{
		if ( GET_OBJ_TYPE ( &obj_proto[nr] ) == ITEM_POTION )
		{
			snprintf ( buf, sizeof ( buf ), "%5d. [%5d][L:%2d][W: %2d][Cst: %6lld] %s\r\n", ++found,
			           obj_index[nr].vnum, GET_OBJ_VAL ( &obj_proto[nr], 0 ),GET_OBJ_WEIGHT ( &obj_proto[nr] ),
			           GET_OBJ_COST ( &obj_proto[nr] ), obj_proto[nr].short_description );
			DYN_RESIZE ( buf );
		}
	}

	if ( !found )
		send_to_char ( "No objects were found in those parameters.\r\n", ch );
	else
		page_string ( ch->desc, dynbuf, DYN_BUFFER );

}

int check_potion_weight ( struct obj_data *obj )
{
	int spell_weight ( struct obj_data *obj, int val );
	int weight = 0;


	switch ( GET_OBJ_TYPE ( obj ) )
	{
		case ITEM_POTION:
		case ITEM_SCROLL:
			weight += spell_weight ( obj, 1 );
			weight += spell_weight ( obj, 2 );
			weight += spell_weight ( obj, 3 );
			weight_to_object ( obj,weight );
			return 1;
			break;
		case ITEM_WAND:
		case ITEM_STAFF:
			weight += spell_weight ( obj, 3 ) * GET_OBJ_VAL ( obj, 2 );
			weight_to_object ( obj,weight );
			return 1;
			break;
		default:
			return 0;
			break;
	}



}
int check_potion_price ( struct obj_data *obj )
{
	int weight = 0;


	switch ( GET_OBJ_TYPE ( obj ) )
	{
		case ITEM_POTION:
		case ITEM_SCROLL:
			weight += spell_price ( obj, 1 );
			weight += spell_price ( obj, 2 );
			weight += spell_price ( obj, 3 );
			weight *= 2000;
			GET_OBJ_COST ( obj ) = weight;
			return 1;
			break;
		case ITEM_WAND:
		case ITEM_STAFF:
			weight += spell_price ( obj, 3 ) * GET_OBJ_VAL ( obj, 2 );
			weight *= 2000;
			GET_OBJ_COST ( obj ) = weight;
			return 1;
			break;
		default:
			return 0;
			break;
	}



}

int spell_weight ( struct obj_data *obj, int val )
{
  int i;
 
  i = GET_OBJ_VAL(obj, val);

  if ( i == -1 ) /* i.e.: no spell */
	return 0;

	/*
	 * Check for negative spells, spells beyond the top define, and any
	 * spell which is actually a skill.
	 */
	if ( i <= 0 )
		return 0;
	if ( i >= MAX_SKILLS )
		return 0;
	if ( spell_info[i].type != 1) 
		return 0;

	switch ( GET_OBJ_VAL ( obj, val ) )
	{
		case SPELL_GROUP_HEAL:
			return 5;
			break;
		case SPELL_HEAL:
			return 4;
			break;
		case SPELL_CURE_CRITIC:
			return 3;
			break;
		default:
			return 2;
			break;
	}



}


int spell_price ( struct obj_data *obj, int val )
{
  int i;

  i = GET_OBJ_VAL(obj, val);

  if ( i == TYPE_UNDEFINED )    /* i.e.: no spell */
		return 0;

	/*
	 * Check for negative spells, spells beyond the top define, and any
	 * spell which is actually a skill.
	 */
	if ( i <= 0 )
		return 0;
	if ( i >= MAX_SKILLS )
		return 0;

	switch ( i )
	{
		case SPELL_GROUP_HEAL:
			return 7;
			break;
		case SPELL_HEAL:
			return 5;
			break;
		case SPELL_CURE_CRITIC:
			return 3;
			break;
		case SPELL_CURE_LIGHT:
			return 1;
                        break;
                case SPELL_IDENTIFY:
                        return 3;
                        break;
		default:
			return 9;
			break;
	}



}

void do_connections ( Character *ch, char *arg )
{
	int zone_num;
	int j, i, k;
	//int start_room;
	char tbuf[MAX_STRING_LENGTH];
	char buf[MAX_INPUT_LENGTH];
	DYN_DEFINE;
	*buf = 0;



	if ( !*arg )
	{
		send_to_char ( "USAGE: show connections .\r\n", ch );
		send_to_char ( "USAGE: show connections <zone_num>\r\n", ch );
		return;
	}
	else if ( *arg == '.' )
		zone_num = IN_ROOM ( ch )->zone;
	else
	{
		j = atoi ( arg );
		for ( zone_num = 0; zone_num <= top_of_zone_table; zone_num++ )
			if ( zone_table[zone_num].number == j )
				break;
	}
	DYN_CREATE;
	*dynbuf = 0;
	if ( zone_num >= 0 && zone_num <= top_of_zone_table )
	{
		//start_room = zone_table[zone_num].number * 100;
		snprintf ( tbuf, sizeof ( tbuf ), "Connections from %-30.30s\r\n"
		           "--------------------------------------------------------------------------------\r\n",
		           zone_table[zone_num].name );
		DYN_RESIZE ( tbuf );

		for ( i = 0, k = 0; i <= top_of_world; i++ )
		{
			if ( !world_vnum[i] )
				continue;
			for ( j = 0; j < NUM_OF_DIRS; j++ )
			{
				if ( world_vnum[i]->zone == zone_num &&
				        world_vnum[i]->dir_option[j] &&
				        world_vnum[i]->dir_option[j]->to_room != NULL &&
				        world_vnum[i]->dir_option[j]->to_room->zone != zone_num )
				{
					snprintf ( tbuf, sizeof ( tbuf ),
					           "%3d: [%5d] %-23.23s -(%-5.5s)-> [%5d] %-23.23s\r\n",
					           ++k, world_vnum[i]->number, world_vnum[i]->name, dirs[j],
					           world_vnum[i]->dir_option[j]->to_room->number,
					           world_vnum[i]->dir_option[j]->to_room->name );
					DYN_RESIZE ( tbuf );
				}
			}
		}

		snprintf ( tbuf, sizeof ( tbuf ),  "\r\nConnections to %-30.30s\r\n"
		           "--------------------------------------------------------------------------------\r\n",
		           zone_table[zone_num].name );
		DYN_RESIZE ( tbuf );
		for ( i = 0, k = 0; i <= top_of_world; i++ )
		{
			if ( !world_vnum[i] )
				continue;
			for ( j = 0; j < NUM_OF_DIRS; j++ )
			{
				if ( world_vnum[i]->zone != zone_num &&
				        world_vnum[i]->dir_option[j] &&
				        world_vnum[i]->dir_option[j]->to_room != NULL &&
				        world_vnum[i]->dir_option[j]->to_room->zone == zone_num )
				{
					snprintf ( tbuf, sizeof ( tbuf ),
					           "%3d: [%5d] %-23.23s -(%-5.5s)-> [%5d] %-23.23s\r\n",
					           ++k, world_vnum[i]->number, world_vnum[i]->name, dirs[j],
					           world_vnum[i]->dir_option[j]->to_room->number,
					           world_vnum[i]->dir_option[j]->to_room->name );
					DYN_RESIZE ( tbuf );
				}
			}
		}

		page_string ( ch->desc, dynbuf, DYN_BUFFER );
	}
}

ACMD ( do_innate )
{
	Character *victim;
	struct affected_type *aff = NULL;
	int found = FALSE;
	char buf[MAX_INPUT_LENGTH];
	char arg[MAX_INPUT_LENGTH];

	half_chop ( argument, arg, buf );

	if ( !*buf )
	{
		if ( subcmd == SCMD_SINNATE )
			send_to_char
			( "You must specify WHICH spell you'd like to make innate.\r\n",
			  ch );
		else
			send_to_char
			( "You must specify WHICH spell you'd like to remove innate from.\r\n",
			  ch );
		return;
	}

	if ( ! ( victim = get_player_vis ( ch, arg, NULL, 0 ) ) )
	{
		ch->Send ( "%s", CONFIG_NOPERSON );
		return;
	}

	/* lets make sure the spell exists. */
	if ( victim->affected )
	{
		for ( aff = victim->affected; aff && !found; aff = aff->next )
		{
			if ( is_abbrev ( buf, skill_name ( aff->type ) ) )
				found = TRUE;
		}
	}

	if ( !found )
	{
		send_to_char
		( "That person is not affected by that spell.Sorry.\r\n", ch );
		return;
	}

	/* this searches  out all instances of named spell - after  all, spells
	 * like bless cause  several affects, you'd  have to  make them all
	 * innate
	 */
	if ( subcmd == SCMD_SINNATE )
		for ( aff = victim->affected; aff; aff = aff->next )
		{
			if ( is_abbrev ( buf, skill_name ( aff->type ) ) )
				aff->expire = -2;     /* well, -1 is innate neh? never goes away */
		}
	else
		/* really, this is unaffect, with a specific target */
		for ( aff = victim->affected; aff; aff = aff->next )
		{
			if ( is_abbrev ( buf, skill_name ( aff->type ) ) )
				victim->affect_remove ( aff );
		}
	ch->Send ( "%s", CONFIG_OK );
}

void olc_list_flags ( Character *ch, const char *apply_stuff[] )
{

	int counter = 0, columns = 0;

	// clear_screen(d);
	while ( *apply_stuff[counter] != '\n' )
	{
		ch->Send ( "{cg%2d{c0) %-20.20s %s", counter,
		           apply_stuff[counter],
		           ! ( ++columns % 2 ) ? "\r\n" : "" );
		counter++;
	}
	ch->Send ( "{cg%2d{c0) Weapon\r\n", counter );
}

ACMD ( do_statinnate )
{
	int count = 0, object,  bot=0, top=0, s = -1;
	char buf[MAX_INPUT_LENGTH];
	string arg1, arg2;
	DYN_DEFINE;
	skip_spaces ( &argument );
	if ( !*argument )
	{
		ch->Send ( "You must supply an argument.\r\nstatinnate <bottom vnum> <top vnum>\r\nstatinnate <name of innate>\r\n" );
		return;
	}

	stringstream iss ( argument );
	iss >> arg1 >> arg2;

	if ( !is_number ( arg1.c_str() ) )
	{
		s = spell_num ( arg1.c_str() );
	}
	else
	{
		bot = atoi ( arg1.c_str() );
		top = atoi ( arg2.c_str() );
	}
	DYN_CREATE;
	*dynbuf = 0;
	ch->Send ( "You list the innates:\r\n" );
	for ( object = 0; object <= top_of_objt; object++ )
	{
		if ( !obj_proto[object].obj_flags.obj_innate )
			continue;
		if ( bot + top != 0 )
		{
			if ( obj_index[object].vnum >= bot && obj_index[object].vnum <= top )
			{
				count++;
				snprintf ( buf, sizeof ( buf ), "[vnum: %7d] innate: %s - %s\r\n",
				           obj_index[object].vnum,
				           skill_name ( obj_proto[object].obj_flags.obj_innate ),
				           obj_proto[object].short_description );
				DYN_RESIZE ( buf );
			}
		}
		else if ( s != -1 && obj_proto[object].obj_flags.obj_innate == s )
		{
			count++;
			snprintf ( buf, sizeof ( buf ), "[vnum: %7d] innate: %s - %s\r\n",
			           obj_index[object].vnum,
			           skill_name ( obj_proto[object].obj_flags.obj_innate ),
			           obj_proto[object].short_description );
			DYN_RESIZE ( buf );
		}

	}
	if ( !count )
		ch->Send ( "No innates!" );
	else
		page_string ( ch->desc, dynbuf, DYN_BUFFER );


}

inline bool Comparefunc ( pair < int, string > a, pair < int, string > b )
{
	return a.first > b.first;
}

void do_statlist_weaponsearch(Character *ch, char *arg1, char *argument)
{
  char arg2[MAX_INPUT_LENGTH];
  char buf[MAX_STRING_LENGTH];
  int die_num, die_size, object, sort_by_dice, i, count = 0;
  string line;
  vector < pair < int, string > > pairs;
  DYN_DEFINE;

  if ( !strcmp ( arg1, "first" ) || !strcmp ( arg1, "last" ) )
  {
	if ( !strcmp ( arg1, "first" ) )
		sort_by_dice = 1;
	else sort_by_dice = 2;	
  	
	DYN_CREATE;
	*dynbuf = 0;
  	for ( object = 0; object <= top_of_objt; object++ ) {
      		if ( GET_OBJ_TYPE(&obj_proto[object]) != ITEM_WEAPON ) 
			continue;
      		snprintf ( buf, sizeof(buf), "%6d %s %dd%d\r\n", obj_index[object].vnum, obj_proto[object].short_description, GET_OBJ_VAL ( &obj_proto[object], 1 ), GET_OBJ_VAL ( &obj_proto[object], 2 ) );
      		pairs.push_back ( make_pair ( GET_OBJ_VAL ( &obj_proto[object], sort_by_dice ), string ( buf ) ) );
  	}
	sort ( pairs.begin(), pairs.end(), Comparefunc );

	for ( i=0; i < pairs.size(); i++ )
	{
		snprintf ( buf, sizeof ( buf ), "%s", pairs[i].second.c_str() );
		DYN_RESIZE ( buf );
	}
	page_string ( ch->desc, dynbuf, DYN_BUFFER );	

	return;
  }
	
  one_argument(argument, arg2);

  if (!*arg1 || !*arg2 || !is_number(arg1) || !is_number(arg2)) {
      ch->Send ( "Format: statlist weapon <num of die> <size of die> %s %s\r\n", arg1, arg2 );
      ch->Send ( "Format: statlist weapon <first|last>\r\n" );
      return;
  }

  die_num = atoi(arg1);
  die_size = atoi(arg2);
  ch->Send("Weapons with %dd%d :\r\n", die_num, die_size);
 
  DYN_CREATE;
  *dynbuf = 0;
  for ( object = 0; object <= top_of_objt; object++ ) {
      if (GET_OBJ_TYPE(&obj_proto[object]) != ITEM_WEAPON) continue;
      if (GET_OBJ_VAL(&obj_proto[object], 1) != die_num ||
          GET_OBJ_VAL(&obj_proto[object], 2) != die_size) continue;
      snprintf(buf, sizeof(buf), "%6d %s\r\n", obj_index[object].vnum, obj_proto[object].short_description);
      DYN_RESIZE(buf);
      count++;
  }
  if (!count)
      ch->Send("No weapons with that dam dice.\r\n");
  else
      page_string(ch->desc, dynbuf, DYN_BUFFER);
  
}

ACMD ( do_statlist )
{
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	char buf[MAX_INPUT_LENGTH];
	char line[MAX_INPUT_LENGTH];
	int aff = 0, pos = -1, nr, count = 0, object, size = 0, i;
	string arg2_string, linestring;
	vector < pair < int, string > > pairs;

	DYN_DEFINE;
	*buf = 0;

	*buf = '\0';

	argument = two_arguments ( argument, arg1, arg2 );
	arg2_string = arg2;

	if ( !*arg1 )
	{
		send_to_char
		( "You must specify which affect you want. Options are:\r\n",
		  ch );
		olc_list_flags ( ch, apply_types );
		return;
	}
        else if (is_abbrev(arg1, "weapon")) {
            do_statlist_weaponsearch(ch, arg2, argument);
            return;
        }

	for ( aff = 0; *apply_types[aff] != '\n'; aff++ )
		if ( is_abbrev ( arg1, apply_types[aff] ) )
			break;

	if ( *apply_types[aff] == '\n' )
	{
		send_to_char ( "Unrecognized apply type, options are:\r\n", ch );
		olc_list_flags ( ch, apply_types );
		return;
	}

	if ( *arg2 )
	{
		for ( pos = 0; *wear_bits[pos] != '\n'; pos++ )
			if ( is_abbrev ( arg2, wear_bits[pos] ) )
				break;

		if ( arg2_string != "all" && *wear_bits[pos] == '\n' )
		{
			send_to_char ( "Unrecognized position, options are:", ch );
			olc_list_flags ( ch, wear_bits );
			ch->Send ("{cg31{c0) ALL\r\n");
			return;
		}
	}
	else
	{
		send_to_char ( "Unrecognized position, options are:", ch );
		olc_list_flags ( ch, wear_bits );
		ch->Send ("{cg31{c0) ALL\r\n");
		return;
	}
	DYN_CREATE;
	*dynbuf = 0;

	for ( object = 0; object <= top_of_objt; object++ )
	{

		if ( arg2_string != "all" && pos != -1 && !CAN_WEAR ( &obj_proto[object], pos ) )
			continue;

		for ( nr = 0; nr < MAX_OBJ_AFFECT; nr++ )
		{
			if ( obj_proto[object].affected[nr].location == aff )
			{
				count++;
				*line = '\0';
				snprintf ( buf, sizeof ( buf ),  "[%7d] ", obj_index[object].vnum );
				strcat ( line, buf );
				snprintf ( buf, sizeof ( buf ),  "Modifies %s by [", apply_types[aff] );
				strcat ( line, buf );
				size = obj_proto[object].affected[nr].modifier;
				if ( size < 0 )
					snprintf ( buf, sizeof ( buf ), "{cb%d{c0", size );
				else
				{
					switch ( size )
					{
						case 0:
							snprintf ( buf, sizeof ( buf ), "{cr0{c0" );
							break;
						case 1:
							snprintf ( buf, sizeof ( buf ), "{cy1{c0" );
							break;
						case 2:
							snprintf ( buf, sizeof ( buf ), "{cg2{c0" );
							break;
						case 3:
							snprintf ( buf, sizeof ( buf ), "{cG3{c0" );
							break;
						case 4:
							snprintf ( buf, sizeof ( buf ), "{cC4{c0" );
							break;
						default:
							snprintf ( buf, sizeof ( buf ), "{cW%d{c0", size );
							break;
					}

				}
				strcat ( line, buf );

				snprintf ( buf, sizeof ( buf ), "] %s - ", obj_proto[object].short_description );
				strcat ( line, buf );
				sprintbitarray ( obj_proto[object].obj_flags.wear_flags,
				                 wear_bits, TW_ARRAY_MAX, buf, sizeof ( buf ) );
				strcat ( line, buf );

				snprintf ( buf, sizeof ( buf ), "\r\n" );
				strcat ( line, buf );

				linestring = line;
				pairs.push_back ( make_pair ( size, linestring ) );
			}

		}
	}

	if ( !count )
	{
		snprintf ( buf, sizeof ( buf ), "No items that affect that stat.\r\n" );
		DYN_RESIZE ( buf );
	}
	else
	{
		sort ( pairs.begin(), pairs.end(), Comparefunc );
		for ( i=0; i < count; i++ )
		{
			snprintf ( buf, sizeof ( buf ), "%s", pairs[i].second.c_str() );
			DYN_RESIZE ( buf );
		}
	}

	page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

ACMD ( do_osnoop )
{
	char arg1[MAX_INPUT_LENGTH];
	char arg2[MAX_INPUT_LENGTH];
	int aff = 0, limit = 0, pos = ITEM_WEAR_TAKE, nr, count = 0, object, size = 0;
	char buf[MAX_STRING_LENGTH];
	two_arguments ( argument, arg1, arg2 );

	if ( !*arg1 )
	{
		send_to_char
		( "You must specify which affect you want. Options are:\r\n",
		  ch );
		olc_list_flags ( ch, apply_types );
		return;
	}

	for ( aff = 0; *apply_types[aff] != '\n'; aff++ )
		if ( is_abbrev ( arg1, apply_types[aff] ) )
			break;

	if ( *apply_types[aff] == '\n' )
	{
		send_to_char ( "Unrecognized apply type, options are:\r\n", ch );
		olc_list_flags ( ch, apply_types );
		return;
	}
	if ( *arg2 )
		limit = atoi ( arg2 );
	if ( !limit )
		limit = 5;



	for ( object = 0; object <= top_of_objt; object++ )
	{

		if ( pos != -1 && !CAN_WEAR ( &obj_proto[object], pos ) )
			continue;

		for ( nr = 0; nr < MAX_OBJ_AFFECT; nr++ )
		{
			if ( obj_proto[object].affected[nr].location == aff )
			{
				size = obj_proto[object].affected[nr].modifier;
				if ( size < limit )
					continue;
				count++;
				ch->Send ( "[%7d] ", obj_index[object].vnum );
				ch->Send ( "Modifies %s by [",
				           apply_types[aff] );

				if ( size < 0 )
					ch->Send ( "{cb%d{c0", size );
				else
				{
					switch ( size )
					{
						case 0:
							ch->Send ( "{cr0{c0" );
							break;
						case 1:
							ch->Send ( "{cy1{c0" );
							break;
						case 2:
							ch->Send ( "{cg2{c0" );
							break;
						case 3:
							ch->Send ( "{cG3{c0" );
							break;
						case 4:
							ch->Send ( "{cC4{c0" );
							break;
						default:
							ch->Send ( "{cW%d{c0", size );
							break;
					}

				}

				ch->Send ( "] %s - ",
				           obj_proto[object].short_description );
				sprintbitarray ( obj_proto[object].obj_flags.wear_flags,
				                 wear_bits, TW_ARRAY_MAX, buf, sizeof ( buf ) );

				ch->Send ( "%s\r\n", buf );
			}

		}
	}

	if ( !count )
		send_to_char ( "No items that affect that stat.\r\n", ch );
}


int update_award ( Character *ch )
{

	if ( ( PLR_FLAGGED ( ch, PLR_RP_LEADER ) || PLR_FLAGGED ( ch, PLR_HERO ) ) && ( SPECIALS ( ch )->last_reward ) != ( find_month() ) )
	{
		SPECIALS ( ch )->last_reward = ( find_month() );
		GET_REWARD ( ch ) = 15;
		GET_AWARD ( ch )  = 15;
	}
	if ( GET_LEVEL ( ch ) > LVL_IMMORT )
	{
		SPECIALS ( ch )->last_reward = ( find_month() );
		GET_REWARD ( ch ) = 30;
		GET_AWARD ( ch )  = 30;
	}
	return GET_AWARD ( ch );
}

int update_reward ( Character *ch )
{
	if ( PLR_FLAGGED ( ch, PLR_HERO ) && ( SPECIALS ( ch )->last_reward ) != ( find_month() ) )
	{
		SPECIALS ( ch )->last_reward = ( find_month() );
		GET_REWARD ( ch ) = 15;
		GET_AWARD ( ch )  = 15;
	}
	if ( GET_LEVEL ( ch ) > LVL_IMMORT )
	{
		SPECIALS ( ch )->last_reward = ( find_month() );
		GET_REWARD ( ch ) = 30;
		GET_AWARD ( ch )  = 30;
	}
	return GET_REWARD ( ch );
}

/* reward points
REWARDPOINTS:
Usage: reward <amount> <playername>
Each RewardPoint equals half a brass token, so don't be too free in doling them out.
Remember that the command will be logged, and use your common sense as to the size of the
reward.
There will also be no option to apply for extra RewardPoints, like there is with the
AwardPoints, unless you are running really big Quests. (see below).
- You may NOT give yourself or your mort any RewardPoints
- Heroes can design and run their own Quests, from the very simple ones, meant to
  introduce Newbies to the evil ways of 4D, to hard and elaborate ones for experienced
  and high-level players.
- The type of Quest can vary from 'Hangman', Mud or Trivia Quiz and more or less simple
  Treasure Hunts to advanced Roleplay Quests, that demand some work in setting up the
  plot, Background Story and rules for the Quest.
- Preferably use mobs and objects that already exist in the game, but if you need
  assistance in setting up any special items for the Quest, Sonya will usually be happy
  to help out with that part, provided she isn't too busy already.
- It is usually a good idea to make your Quests IC, so that you can also use the Award
  Points while running it too. Larger Quests, that need Imm assistance, should always
  be set up as RP Quests.
- If the RP Quest you are planning is a really big one, you can apply for a temporary
  imm, so that you'll be able to use Quest commands like world_echo, goto, trans and
  switch into a mob while running it. Quests like this need to be submitted and approved
  by the Staff first. If approved, you'll get all necessary assistance, including any
  extra rewards.

Below is a display of the RP rewards players can buy with AwardPoints, plus the
cost in points for each reward:
REWARD                                                             COST
----------------------------------------------------------------------------
A restrung item (no stats, addaffects or containers allowed)       100 points
A pretitle in the WHO list (example Lord Tynian)                   200 points
A personalized long decs (This is what shows of you in the room)   500 points
A personalised entrance/exit desc (this is like a poofin/out)      800 points
The ability to restring items yourself                            1000 points
The ability to award RP points yourself                           2000 points
----------------------------------------------------------------------------
More rewards may be added later.*/
ACMD ( do_reward )
{
	char arg[MAX_INPUT_LENGTH]; // for name, for points
	char arg2[MAX_INPUT_LENGTH];
	Character *vict;
	int pts;
	ch->Send ( "Use AWARD please.\r\n" );
	return;
	if ( IS_NPC ( ch ) )
		return;

	if ( PLR_FLAGGED ( ch, PLR_HERO ) || ( GET_LEVEL ( ch ) > LVL_IMMORT ) )
	{

		if ( update_reward ( ch ) == 0 )
		{
			ch->Send ( "You don't have enough points! \r\nPoints are refreshed every 17 hours.\r\n" );
			return;
		}
		argument = two_arguments ( argument, arg, arg2 );
		if ( !*arg || !*arg2 )
		{
			ch->Send ( "Usage: reward <amount> <playername>\r\n" );
			return;
		}
		if ( ! ( vict = get_player_vis ( ch, arg2, NULL, 0 ) ) )
		{
			ch->Send ( "%s", CONFIG_NOPERSON );
			return;
		}
		if ( PLR_FLAGGED ( vict, PLR_HERO ) )
		{
			ch->Send ( "You can only reward mortals.\r\n" );
			return;
		}

		if ( ( pts = atoi ( arg ) ) <= 0 )
		{
			ch->Send ( "Invalid number. Try something more than 0.\r\n" );
			return;
		}
		if ( pts > update_reward ( ch ) )
		{
			ch->Send ( "Invalid number. Try something less than %d.\r\n", GET_REWARD ( ch ) );
			return;
		}

		vict->Send ( "%s showers you in praise and rewards you %d point%s!", GET_NAME ( ch ), pts, pts > 1 ? "s" : "" );
		ch->Send ( "You shower %s in praise and reward %d point%s!", GET_NAME ( vict ), pts, pts > 1 ? "s" : "" );
		if ( ( GET_LEVEL ( ch ) < LVL_IMMORT ) )
			GET_REWARD ( ch )   -= pts;
		GET_REWARD ( vict ) += pts;

	}
	else
	{
		ch->Send ( "You will be able to use your points soon.\r\n" );
	}
}


/* AWARDPOINTS:
Usage: award <amount> <playername>

Imms and Heroes get this command.
To avoid abuse and favouritism, both the Award and Reward commands are logged. They
will be revoked if abused.

Each Hero gets a total of 10 Award and Reward points per day, which they can use at
their own discretion. Unused points do not accumulate; the total amount is reset at
10 at the beginning of each new day.

Players can collect AWARDPOINTS even if they are not registered as RP. But to actually
USE the points, they need to register first.

One of the most appreciated tasks of Heroes would be to initiate and monitor RP sessions.
You can do this as a form of Roleplay Quest, to which you invite all players on line to
take part (or possibly only registered roleplayers).

You could also just start to roleplay with a player or a group of players, anywhere in the
Mud. Or, if you happen to come along some players already roleplaying, stay and discreetly
join the session.
Then you appraise the result, and dole out the RP points in proportion to what you see.
This is no exact science, but use your common sense, and try to be as fair as you can, in
spite of possible likes and dislikes.
Example 1:
Three players took part, PlayerA was giving the best performance, PlayerB was pretty good
too, while PlayerC was mainly listening in. The Award could be; Player1 5 points, PlayerB
4, Player C 1, (because listen-and-learn is sometimes a good idea too).
Example 2:
PlayerA and B were as before, while PlayerC was sabotaging the session by acting like a
Twink. PlayerA gets 5, PlayerB gets 3 and PlayerC 0. (Regrettably you cannot award negative
points, but if PlayerC has a RP flag, he should be reported to the Staff).

If you already have used all your Apoints for a day, and come across another session that
you think should be awarded, you can log it, and e-mail it to the Roleplay Jury, for
assessment and possible award. The same thing applies if someone makes a performance so
outstanding that you think it deserves an extra award. Again the decision should be taken
by the Jury. One of the more important tasks of the Heroes would be to alert the Jury of
particularly good or promising Roleplayers.

- You should not award all 10 points to one single player, after all RP is about
  interacting so there should be at least 2 players apart from yourself involved.
- You don't need to use up all your points in one session.
- You may not award yourself or your mort any points, obviously.*/
ACMD ( do_award )
{
	char arg[MAX_INPUT_LENGTH]; // for name, for points
	char arg2[MAX_INPUT_LENGTH];
	Character *vict;
	int pts;

	if ( IS_NPC ( ch ) )
		return;

	if ( PLR_FLAGGED ( ch, PLR_HERO ) || ( GET_LEVEL ( ch ) > LVL_IMMORT ) || PLR_FLAGGED ( ch, PLR_RP_LEADER ) )
	{

		if ( update_award ( ch ) == 0 )
		{
			ch->Send ( "You don't have enough points! \r\nPoints are refreshed every 17 hours.\r\n" );
			return;
		}
		argument = two_arguments ( argument, arg, arg2 );
		if ( !*arg || !*arg2 )
		{
			ch->Send ( "Usage: award <amount> <playername>\r\n" );
			return;
		}
		if ( ! ( vict = get_player_vis ( ch, arg2, NULL, 0 ) ) )
		{
			ch->Send ( "%s", CONFIG_NOPERSON );
			return;
		}
		if ( PLR_FLAGGED ( vict, PLR_HERO ) )
		{
			ch->Send ( "You can only award mortals.\r\n" );
			return;
		}
		if ( ( pts = atoi ( arg ) ) <= 0 )
		{
			ch->Send ( "Invalid number. Try something more than 0.\r\n" );
			return;
		}
		if ( pts > update_award ( ch ) )
		{
			ch->Send ( "Invalid number. Try something less than %d.\r\n", GET_AWARD ( ch ) );
			return;
		}

		vict->Send ( "%s showers you in praise and awards you %d point%s!\r\n", GET_NAME ( ch ), pts, pts > 1 ? "s" : "" );
		ch->Send ( "You shower %s in praise and award %d point%s!\n\n", GET_NAME ( vict ), pts, pts > 1 ? "s" : "" );

		if ( GET_LEVEL ( ch ) < LVL_IMMORT )
			GET_AWARD ( ch )   -= pts;
		GET_AWARD ( vict ) += pts;


	}
	else
	{
		ch->Send ( "You will be able to use your award points soon.\r\n" );
	}
}


/*finish me*/
int mortal_player_info ( Character *ch, Character *vict )
{
	char buf1[MAX_INPUT_LENGTH];

	if ( GET_LEVEL ( vict ) <LVL_IMMORT )
	{
		strcpy ( buf1, ( char * ) asctime ( localtime ( & ( vict->player.time.logon ) ) ) );
		buf1[10] = '\0';

		ch->Send (
		    "\r\nLast Logon: [%s], Played [%dh %dm], Age [%d]\r\n",
		    buf1, vict->player.time.played / 3600,
		    ( ( vict->player.time.played % 3600 ) / 60 ), age ( vict )->year );
	}

	return 1;
}
ACMD ( do_ps_aux )
{
	char line[MAX_INPUT_LENGTH];
	FILE *fl;

	system ( "ps aux | grep circle > psaux" );

	if ( ( fl = fopen ( "psaux", "r" ) ) == NULL )
	{
		ch->Send ( "No file.\r\n" );
		return;
	}

	while ( get_line ( fl, line ) )
	{
		ch->Send ( "%s\r\n", line );
	}

	fclose ( fl );

}
ACMD ( do_get_free_mem )
{
	char line[MAX_INPUT_LENGTH];
	FILE *fl;

	system ( "free -m > freemem" );

	if ( ( fl = fopen ( "freemem", "r" ) ) == NULL )
	{
		ch->Send ( "No file.\r\n" );
		return;
	}

	while ( get_line ( fl, line ) )
	{
		ch->Send ( "%s\r\n", line );
	}

	fclose ( fl );

}



ACMD ( do_namechange )
{
	Descriptor *d;
	Character *tch = NULL;
	int loaded = 0;
	char newname[MAX_INPUT_LENGTH], oldname[MAX_INPUT_LENGTH], passw[MAX_INPUT_LENGTH];

	argument = two_arguments ( argument, oldname, newname );
	one_argument ( argument, passw );

	if ( GET_LEVEL ( ch ) < LVL_SEN )
	{
		ch->Send ( "Que?\r\n" );
		return;
	}

	if ( ( !*oldname || !*newname || !*passw ) )
	{
		ch->Send ( "namechange <oldname> <newname> <newpassword>\r\n" );
		return;
	}

	if ( ( strlen ( newname ) >= MAX_NAME_LENGTH ) || ( strlen ( newname ) < 3 ) )
	{
		ch->Send ( "Sorry but that name is the wrong length\r\n" );
		return;
	}
	newname[0] = UPPER ( newname[0] );
	if ( !str_cmp ( newname, passw ) )
	{
		ch->Send ( "\r\nIllegal password.\r\n" );
		return;
	}


	for ( d = descriptor_list; d; d = d->next )
	{
		if ( !IS_PLAYING ( d ) )
		{
			if ( compares ( GET_NAME ( d->character ), oldname ) )
			{
				ch->Send ( "This player is in a state that can't be changed just yet.\r\n" );
				return;
			}
		}
		else if ( compares ( GET_NAME ( d->character ), oldname ) )
		{
			tch = d->character;
			if ( GET_LEVEL ( tch ) >= GET_LEVEL ( ch ) )
			{
				ch->Send ( "Ah, Bugger off!\r\n" );
				return;
			}
			break;
		}
	}
	if ( !tch )
	{

		void load_locker ( Character *ch );

		if ( pi.IdByName ( oldname ) == -1 )
		{
			ch->Send ( "A player by that name doesn't exist here.\r\n" );
			return;
		}
		tch = new Character ( FALSE );
		if ( pi.LoadChar ( oldname, tch ) <= 0 )
		{
			delete ( tch );
			log ( "load char error in namechange" );
			return;
		}
		if ( GET_LEVEL ( tch ) >= GET_LEVEL ( ch ) )
		{
			ch->Send ( "Ah, Bugger off!\r\n" );
			delete tch;
			return;
		}
		tch->reset();
		read_aliases ( tch );
		GET_ID ( tch ) = GET_IDNUM ( tch );
		addChToLookupTable ( GET_IDNUM ( ch ), tch );
		tch->LoadKillList();
		read_ignorelist ( tch );
		load_locker ( tch );
		add_char_to_list ( tch );
		char_to_room ( tch, IN_ROOM ( ch ) );
		loaded = 1;
	}
	free_string ( &tch->player.name );
	tch->player.name = strdup ( strdup ( CAP ( newname ) ) );
	newname[0] = LOWER ( newname[0] );

	strncpy ( GET_PASSWD ( tch ), CRYPT ( passw, tch->player.name ), MAX_PWD_LENGTH );
	* ( GET_PASSWD ( tch ) + MAX_PWD_LENGTH ) = '\0';
	pi.change_plrindex_name ( GET_IDNUM ( tch ), newname );
	tch->save();
	if ( !loaded )
		Crash_crashsave ( tch );
	write_aliases ( tch );
	write_ignorelist ( tch );

	ch->Send ( "%s's name changed to %s. Password: %s\r\n", oldname, newname, passw );
	if ( !loaded )
		tch->Send ( "{cYYour name has been changed to %s. It's best if you quit and reenter to save.\r\n{c0", newname );

	if ( loaded )
	{
		Crash_rentsave ( tch, 0 );
		extract_char ( tch );
	}
}

ACMD ( do_decrypt )
{
	char arg1[256], arg2[256];
	argument = one_arg ( argument, arg1 );
	argument = one_arg ( argument, arg2 );

	if ( strlen ( arg1 ) < 2 || strlen ( arg2 ) < 2 )
	{
		ch->Send ( "Usage: decrypt <arg1> <arg2>\r\n" );
		return;
	}

	ch->Send ( "Arg1: %s Arg2: %s\r\n", arg1, arg2 );
	ch->Send ( "1: %s - %s - %d\r\n", CRYPT ( arg1, arg2 ), CRYPT ( arg1,CRYPT ( arg1, arg2 ) ),!strncmp ( CRYPT ( arg1, arg2 ), arg2, MAX_PWD_LENGTH ) );
	ch->Send ( "2: %s - %s - %d\r\n", CRYPT ( arg2, arg1 ), CRYPT ( arg1,CRYPT ( arg2, arg1 ) ),!strncmp ( CRYPT ( arg2, arg1 ), arg1, MAX_PWD_LENGTH ) );
}

void list_destinations ( struct travel_point_data *travel_list, Character *ch )
{
	size_t len = 0;
	struct travel_point_data *t;
	if ( !travel_list )
		return;
	ch->Send ( "Destination points (@ was last point reached):\r\n" );
	for ( t = travel_list; t; t = t->next )
	{
		len += ch->Send ( "%c%d ", t->last_stop ? '@' : ' ', t->dest );
		if ( len > 70 )
		{
			len = 0;
			ch->Send ( "\r\n" );
		}
	}
	ch->Send ( "\r\n" );
}

ACMD ( do_deleteplayer )
{

	char buf2[MAX_INPUT_LENGTH];
	one_argument ( argument, buf2 );
	ch->loader = -1;
	if ( !*buf2 )
	{
		ch->Send ( "Delete which player?\r\n" );
		return;
	}
	else if ( ! ( ch->loader = pi.IdByName ( buf2 ) ) )
	{
		send_to_char ( "There is no such player.\r\n", ch );
		return;
	}
	ch->Send ( "{cYAre you ABSOLUTELY certain you want to delete {cR%s{cY?{c0\r\n\r\n{cgIf you are certain, type: 'yes I am' --:{c0", buf2 );
	line_input ( ch->desc, "", delete_player, NULL );

}

C_FUNC ( delete_player )
{
	Character *tch = d->character;
	char *charname;

	if ( !tch )
		return;
	charname = pi.NameById ( tch->loader );

	if ( arg && *arg && !strcmp ( arg, "yes I am" ) )
	{
		d->Output ( "Deleting...\r\n" );
		perform_delete_player ( charname );
		d->Output ( "\r\n...Done\r\n" );
	}
	else
	{
		d->Output ( "You cancel the delete on %s.\r\n", charname );
	}
}

void perform_delete_player ( const char *charname )
{
	int player_i;
	Crash_delete_file ( charname );
	delete_pobj_file ( charname );
	delete_aliases ( charname );
	delete_variables ( charname );
	try
	{
		player_i = pi.TableIndexByName ( charname );
		pi.SetFlags ( player_i, PINDEX_SELFDELETE );
		pi.RemovePlayer ( pi.Begin() + player_i );
	}
	catch ( MudException &e )
	{
		log ( "Cannot delete: %s%s", e.Message(), "\r\n" );
		return;
	}

}

void show_door_errors ( Character *ch )
{
	int i, door;
	int found = 0;
	obj_vnum vkey;
	obj_rnum rkey;
	char buf[MAX_STRING_LENGTH];
	DYN_DEFINE;
	DYN_CREATE;
	for ( i = 0; i < top_of_world; i++ )
	{
		if ( world_vnum[i] == NULL )
			continue;

		for ( door = 0; door < NUM_OF_DIRS; door++ )
		{
			if ( world_vnum[i]->dir_option[door] )
			{
				vkey = world_vnum[i]->dir_option[door]->key;
				if ( vkey > 0 )
				{
					if ( ( rkey = real_object ( vkey ) ) != NOTHING )
					{
						if ( GET_OBJ_TYPE ( obj_proto + rkey ) != ITEM_KEY )
						{
							found++;
							snprintf ( buf, sizeof ( buf ), "Room [%5d] Dir [%5s] Key [%5d] - {cCKEY is not of type key (%s).{c0\r\n", i, dirs[door], vkey, item_types[ ( int ) GET_OBJ_TYPE ( obj_proto + rkey ) ] );
							DYN_RESIZE ( buf );
						}
					}
					else
					{
						found++;
						snprintf ( buf, sizeof ( buf ), "Room [%5d] Dir [%5s] Key [%5d] - {cRKEY vnum does not exist.{c0\r\n", i, dirs[door], vkey );
						DYN_RESIZE ( buf );
					}
				}
				/*
				        if (IS_SET(world_vnum[i]->dir_option[door]->exit_info, EX_ISDOOR) && vkey > 0)
				        {
				          found++;
				          snprintf(buf, sizeof(buf), "Room [%5d] Dir [%5s] Key [%5d] - {cyEXIT has key and no door.{c0\r\n", i, dirs[door], vkey);
				          DYN_RESIZE(buf);
				        }
				        if (IS_SET(world_vnum[i]->dir_option[door]->exit_info, EX_ISDOOR) &&
				  (!world_vnum[i]->dir_option[door]->keyword || !*world_vnum[i]->dir_option[door]->keyword))
				        {
				          found++;
				          snprintf(buf, sizeof(buf), "Room [%5d] Dir [%5s] Key [%5d] - EXIT has door and no name.\r\n", i, dirs[door], vkey);
				          DYN_RESIZE(buf);
				        }
				  */

			}
		}
	}
	if ( !found )
	{
		sprintf ( buf, "No door errors found!\r\n" );
		DYN_RESIZE ( buf );
	}
	page_string ( ch->desc, dynbuf, DYN_BUFFER );
}
/**
This function searches a string for a regular expression or keyword
and returns a positive number if it is found. and 0 if unfound.
regex : phrase/keyword (could be made regular expression compliant in future
(don't want to confuse my builders for now)
str   : the string to be searched
buf   : the new string with the expression hilighted
buflen: the size of the buf
--Mordecai : Sat Feb 5th 2005
**/
/*underline */
#define UND "\x1B[4m"
/* remove underline */
#define REM "\x1B[0m"
#define GRN "\x1B[0;32m"
#define YEL "\x1B[0;33m"
#define CYN "\x1B[0;36m"
int hilite ( const char *regex, const char *str, char *buf, size_t buflen )
{
	const char *p, *s = /*( char * )*/ str;
	size_t tmp = 0;
	size_t u = strlen ( UND );
	bool found = FALSE;
	int cnt = strlen ( regex );
	if ( ( p = strstr ( str, regex ) ) == 0 )
		return 0;

	while ( *s  && tmp < buflen )
	{
		if ( s == p )
		{
			if ( tmp + u < buflen )
			{
				snprintf ( buf + tmp, buflen - tmp, "%s", UND );
				tmp += u;
				found = TRUE;
			}
		}
		if ( found )
		{
			if ( !cnt-- )
			{
				if ( tmp + u < buflen )
				{
					snprintf ( buf + tmp, buflen - tmp, "%s", REM );
					tmp += u;
					found = FALSE;
				}

			}
		}

		buf[tmp++] = * ( s++ );
	}


	buf[tmp] = '\0';

	return 1;
}

#define T_ARG 0
#define T_BOD 1
#define T_NAM 2
#define T_ALL 3

int search_one_trig ( int stype, struct trig_data *trig, char *buf, size_t len, int vnum, const char *phrase )
{

	size_t nlen = 0;
	int found = 0, count = 0;
	char strtmp2[MAX_STRING_LENGTH], trty[100], trIS[100];
	if ( trig->attach_type==OBJ_TRIGGER )
	{
		snprintf ( trIS, sizeof ( trIS ), "Objects" );
		new_sprintbit ( GET_TRIG_TYPE ( trig ), otrig_types, trty, sizeof ( trty ) );
	}
	else if ( trig->attach_type==WLD_TRIGGER )
	{
		snprintf ( trIS, sizeof ( trIS ), "Rooms  " );
		new_sprintbit ( GET_TRIG_TYPE ( trig ), wtrig_types, trty, sizeof ( trty ) );
	}
	else
	{
		snprintf ( trIS, sizeof ( trIS ), "Mobiles" );
		new_sprintbit ( GET_TRIG_TYPE ( trig ), trig_types, trty, sizeof ( trty ) );
	}
	count = snprintf ( buf + nlen, len - nlen, "\r\n%sVnum: %-7d - %s%s - %s%s - Name: %-50s%s\r\n",GRN, vnum, CYN, trIS, trty,REM , trig->name, REM );
	if ( count > 0 )
		nlen += count;

	if ( ( stype == T_NAM || stype == T_ALL ) && trig->name )
	{
		if ( hilite ( phrase, ( const char * ) trig->name, strtmp2, sizeof ( strtmp2 ) ) )
		{
			count = snprintf ( buf + nlen, len - nlen, "      %s[NAME]%s %s%s\r\n",CYN, REM, strtmp2, REM );
			if ( count >= 0 )
				nlen += count;
			found++;
		}

	}
	if ( ( stype == T_ARG || stype == T_ALL ) && trig->arglist )
	{
		if ( hilite ( phrase, ( const char * ) trig->arglist, strtmp2, sizeof ( strtmp2 ) ) )
		{
			count = snprintf ( buf + nlen, len - nlen, "      %s[ARGS]%s %s%s\r\n",GRN, REM, strtmp2, REM );
			if ( count >= 0 )
				nlen += count;
			found++;
		}

	}
	if ( ( stype == T_BOD || stype == T_ALL ) && trig->cmdlist )
	{
		int n = 0;
		struct cmdlist_element *c;
		for ( c = trig->cmdlist; c; c=c->next )
		{
			n++;
			if ( hilite ( phrase, c->cmd, strtmp2, sizeof ( strtmp2 ) ) )
			{
				if ( strlen ( strtmp2 ) >= len - nlen - 100 )
					return found; // prevent possible buffer overflow

				count = snprintf ( buf + nlen, len - nlen, "      %s[BODY]%s Line %3d: %s%s\r\n",YEL, REM, n, strtmp2, REM );
				if ( count >= 0 )
					nlen += count;
				found++;
			}
		}

	}


	return found;
}
ACMD ( do_search_triggers )
{
	unsigned int i;
	int found = 0;
	int stype;
	char buf[MAX_STRING_LENGTH];
	DYN_DEFINE;
	DYN_CREATE;

	argument = one_argument ( argument, buf );
	skip_spaces ( &argument );
	if ( !*buf || !*argument )
	{
		ch->Send ( "TSEARCH <ALL | BODY | NAME | ARGS> <phrase to find>\r\n" );
		return;
	}
	if ( isname ( "all", buf ) )
	{
		stype = T_ALL;
	}
	else if ( isname ( "body", buf ) )
	{
		stype = T_BOD;
	}
	else if ( isname ( "name", buf ) )
	{
		stype = T_NAM;
	}
	else if ( isname ( "args", buf ) )
	{
		stype = T_ARG;
	}
	else
	{
		ch->Send ( "TSEARCH <ALL | BODY | NAME | ARGS> <phrase to find>\r\n" );
		return;
	}


	for ( i = 0; i < top_of_trigt; i++ )
	{
		if ( trig_index[i]->proto )
		{
			if ( search_one_trig ( stype, trig_index[i]->proto, buf, sizeof ( buf ), trig_index[i]->vnum, ( const char* ) argument ) )
			{
				DYN_RESIZE ( buf );
				found ++;
			}
		}

	}
	if ( !found )
		sprintf ( buf, "No triggers found!\r\n" );
	else
		sprintf ( buf, "%d triggers found!\r\n", found );
	DYN_RESIZE ( buf );
	page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

ACMD ( do_ctellsnoop )
{
	char arg[MAX_STRING_LENGTH];
	int num;
	if ( GET_LEVEL ( ch ) !=LVL_IMPL )
	{
		ch->Send ( "Only imps are allowed to use this command!\r\n" );
		return;
	}
	one_argument ( argument,arg );
	if ( isdigit ( *arg ) || *arg=='-' )
	{
		if ( ( num=atoi ( arg ) ) <0 || num>=num_of_clans )
		{
			ch->Send ( "That is not a valid clannumber.\r\n" );
			return;
		}
		GET_CSNP_LVL ( ch ) =num;
		ch->Send ( "Okay, you are now listening to the %s ctell.\r\n",clan[num].name );
	}
	else if ( !strcmp ( arg,"none" ) )
	{
		GET_CSNP_LVL ( ch ) =-2;
		ch->Send ( "Okay, you aren't snooping the ctells anymore.\r\n" );
	}
	else if ( !strcmp ( arg,"all" ) )
	{
		GET_CSNP_LVL ( ch ) =-1;
		ch->Send ( "Okay, you are now listening to all the ctells.\r\n" );
	}
	else
	{
		char buf3[50];
		int len=50;
		if ( GET_CSNP_LVL ( ch ) ==-2 )
			len-=snprintf ( buf3,50,"None" );
		else if ( GET_CSNP_LVL ( ch ) ==-1 )
			len-=snprintf ( buf3,50,"All" );
		else if ( GET_CSNP_LVL ( ch ) >=0 && GET_CSNP_LVL ( ch ) <=num_of_clans )
			snprintf ( buf3,len,"%s",clan[GET_CSNP_LVL ( ch ) ].name );
		ch->Send ( "Usage: csnoop clannumber/none/all.\r\nCurrent setting: %s.\r\n",buf3 );
	}
}

ACMD ( do_addtp )
{
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
	Character *vict;
	int amount, orig_tp;
	two_arguments ( argument,arg1,arg2 );
	vict = get_player_vis ( ch, arg1, NULL, FIND_CHAR_WORLD );
	if ( !vict )
	{
		ch->Send ( "There is no player around with that name.\r\n" );
		return;
	}

	if ( !isdigit ( *arg2 ) && ! ( *arg2=='-' && isdigit ( * ( arg2+sizeof ( char ) ) ) ) )
	{
		ch->Send ( "That is not a number.\r\n" );
		return;
	}
	amount=atoi ( arg2 );
	orig_tp=TRADEPOINTS ( vict );
	TRADEPOINTS ( vict ) +=amount;
	if ( TRADEPOINTS ( vict ) <orig_tp && *arg2!='-' )
	{
		TRADEPOINTS ( vict ) =orig_tp;
		ch->Send ( "The new amount of tradepoints would be too high, so it can't be done.\r\n" );
		return;
	}
	if ( TRADEPOINTS ( vict ) <0 || ( TRADEPOINTS ( vict ) >orig_tp && *arg2=='-' ) )
	{
		TRADEPOINTS ( vict ) =orig_tp;
		ch->Send ( "%s doesn't have enough tradepoints for that!\r\n", GET_NAME ( vict ) );
		return;
	}
	new_mudlog ( CMP, MAX ( LVL_SEN, GET_INVIS_LEV ( ch ) ), TRUE, "%s gives %s %d tradepoints.",  GET_NAME ( ch ), GET_NAME ( vict ), amount );
	ch->Send ( "Amount of tradepoints is now %d (was: %d).\r\n",TRADEPOINTS ( vict ),orig_tp );

}




ACMD ( do_wizsplit )
{
	if ( PLR_FLAGGED ( ch,PLR_IMM_MORT ) || ( GET_LEVEL ( ch ) >LVL_HERO+1 && ( CMD_FLAGGED ( ch, WIZ_IMM1_GRP ) || CMD_FLAGGED2 ( ch, WIZ_IMM1_GRP ) ) ) )
		do_wiznet ( ch, argument, cmd, subcmd );
	else
		do_gen_ps ( ch, argument, cmd, SCMD_WIZLIST );
}


void fixskills ( Character *ch, int lrn )
{
	int learned = lrn;
	if ( lrn <= 0 )
		learned = IRANGE ( 30, ( 20* ( TIERNUM ) ), 80 );

	for ( int i = 0;i < MAX_SKILLS;i++ )
		if ( knows_spell ( ch, i ) )
			SAVED ( ch ).SetSkillLearn ( i, learned );
	/* twice over to get the pre-req's */
	for ( int i = 0;i < MAX_SKILLS;i++ )
		if ( knows_spell ( ch, i ) )
			if ( SAVED ( ch ).GetSkillLearn ( i ) < learned )
				SAVED ( ch ).SetSkillLearn ( i, learned );

	SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_CRASH );

}
void fixskills ( Character *ch )
{
	int learned = IRANGE ( 30, ( 20* ( TIERNUM ) ), 80 );
	fixskills ( ch, learned );
}

void offline_fixskills ( Character *ch, char *arg )
{
	string str = string ( arg );
	str = tolower ( str );
	int idx = 0;
	try
	{
		idx = pi.TableIndexByName ( str.c_str() );
	}
	catch ( MudException &e )
	{
		ch->Send ( "%s%s", e.Message(), "\r\n" );
		return;
	}
	pi.SetFlags ( idx, PINDEX_FIXSKILLS );
	pi.Save();
	*ch << pi.NameByIndex ( idx ) << " will be fixed when they log in next!\r\n";

}

ACMD ( do_fixskills )
{
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
	Character *vict;
	int lrn = -1;
	two_arguments ( argument,arg1, arg2 );
	vict = get_player_vis ( ch, arg1, NULL, FIND_CHAR_WORLD );

	if ( vict == NULL )
	{
		*ch << "Sorry, but I can't find " << arg1 << "\r\n";
		if ( *arg1 )
		{
			*ch << "Checking in offline players!\r\n\r\n";
			offline_fixskills ( ch, arg1 );
		}
		return;
	}
	if ( *arg2 )
		lrn = atoi ( arg2 );

	fixskills ( vict, lrn );

	*ch << "You update " << GET_NAME ( vict ) << "'s skills and spells.\r\n";
	act ( "$N updates your skills and spells!", TRUE, vict, NULL, ch, TO_CHAR );
}
#define ZCMD zone_table[zone].cmd[cmd_no]
void list_mob_resets ( Character *vict, Character *ch )
{
	if ( !IS_NPC ( vict ) )
		return;

	int vnum = GET_MOB_VNUM ( vict );

	for ( zone_rnum zone = 0; zone <= top_of_zone_table; zone++ )
		for ( int cmd_no = 0; ZCMD.command != 'S'; cmd_no++ )
		{
			switch ( ZCMD.command )
			{
				case 'M':
					if ( ZCMD.arg1 == vnum )
						ch->Send ( "Will reset in %d\r\n", ZCMD.arg3 );
					break;
			}
		}
}
#define ZCMD2 zone_table[zone].cmd[cmd_num]
void list_obj_resets ( obj_data *obj, Character *ch )
{
	int vnum = GET_OBJ_VNUM ( obj );
	if ( vnum <= 0 )
		return;

	for ( zone_rnum zone = 0; zone <= top_of_zone_table; zone++ )
		for ( int cmd_no = 0; ZCMD.command != 'S'; cmd_no++ )
		{
			switch ( ZCMD.command )
			{
				case 'O':
					if ( obj_index[ZCMD.arg1].vnum == vnum )
						ch->Send ( "Will reset in room %d %s\r\n", ZCMD.arg3, world_vnum[ZCMD.arg3]->name );
					break;
				case 'G':
				case 'E':
					if ( obj_index[ZCMD.arg1].vnum == vnum )
						for ( int cmd_num = cmd_no - 1; cmd_num >= 0; --cmd_num )
							if ( ZCMD2.command == 'M' )
							{
								ch->Send ( "Will reset on mob %d %s\r\n", ZCMD2.arg1, GET_NAME ( mob_proto[ZCMD2.arg1] ) );
								break;
							}
					break;
				case 'P':
					if ( obj_index[ZCMD.arg1].vnum == vnum )
						ch->Send ( "Will reset in container %d %s\r\n", ZCMD.arg3, obj_proto[ZCMD.arg3].short_description );
					break;
			}
		}
}

ACMD(do_code_update) {
system ( "" );
}

ACMD ( do_searchtrig )
{
	Character *c;
	trig_data *t;
	int trig_vnum, nr, num;
	bool found = FALSE;
	char buf[MAX_INPUT_LENGTH], arg[MAX_INPUT_LENGTH];
	olt_it ob;
	map<mob_vnum, Character *>::iterator mob_it;

	one_argument ( argument, arg );

	if ( !*arg || !is_number ( arg ) )
	{
		ch->Send ( "Usage: searchtrig <vnum>\r\n" );
		return;
	}

	trig_vnum = atoi ( arg );

	if ( real_trigger ( trig_vnum ) < 0 )
	{
		ch->Send ( "There is no trigger with that vnum.\r\n" );
		return;
	}

	DYN_DEFINE;
	DYN_CREATE;

	// Look for the trigger in OLC

	ch->Send ( "Trigger %d is attached to:\r\n", trig_vnum );
	ch->Send ( "--------------------------\r\n" );
	ch->Send ( "In OLC:\r\n" );

	for ( mob_it = mob_proto.begin(), num = 0; mob_it != mob_proto.end(); mob_it++ )
	{
		if ( mob_it->second != NULL && mob_it->second->proto_script )
		{
			for ( vector<int>::iterator it2 = mob_it->second->proto_script->begin(); it2 != mob_it->second->proto_script->end(); it2++ )
			{
				if ( *it2 == trig_vnum )
				{
					snprintf ( buf, sizeof ( buf ), "M%3d. [%5d] %-25s\r\n", ++num, GET_MOB_VNUM ( mob_it->second ),
						GET_NAME ( mob_it->second ) );
					DYN_RESIZE ( buf );
					found = TRUE;
					break;
				}
			}
		}
	}

	for ( int i = 1, num = 0; i < top_of_objt; i++ )
	{
		if ( obj_proto[i].proto_script != NULL )
		{
			for ( vector<int>::iterator it2 = obj_proto[i].proto_script->begin(); it2 != obj_proto[i].proto_script->end(); it2++ )
			{
				if ( *it2 == trig_vnum )
				{
					snprintf ( buf, sizeof ( buf ), "O%3d. [%5d] %-25s\r\n", ++num, obj_index[i].vnum, obj_proto[i].short_description );
					DYN_RESIZE ( buf );
					found = TRUE;
					break;
				}
			}
		}
	}

	for ( nr = 0, num = 0; nr <= top_of_world; nr++ )
	{
		if ( world_vnum[nr] && world_vnum[nr]->proto_script != NULL )
		{
			for ( vector<int>::iterator it2 = world_vnum[nr]->proto_script->begin(); it2 != world_vnum[nr]->proto_script->end(); it2++ )
			{
				if ( *it2 == trig_vnum )
				{
					snprintf ( buf, sizeof ( buf ), "W%3d. [%6d] {cy%-25s {cg-{cC %s{c0\r\n",
				           ++num, GET_ROOM_VNUM ( world_vnum[nr] ), world_vnum[nr]->name, zone_table[world_vnum[nr]->zone].name );
					DYN_RESIZE ( buf );
					found = TRUE;
					break;
				}
			}
		}
	}

	if ( !found )
	{
		snprintf ( buf, sizeof ( buf ), "Nothing.\r\n" );
		DYN_RESIZE ( buf );
	}

	// Look for the trigger in the game

	snprintf ( buf, sizeof ( buf ), "Currently in the game:\r\n" );
	DYN_RESIZE ( buf );

	found = FALSE;

	for ( c = character_list, num = 0; c; c = c->next )
	{
		if ( SCRIPT ( c ) )
		{
			for ( t = TRIGGERS ( SCRIPT ( c ) ); t; t = t->next )
			{
				if ( GET_TRIG_VNUM ( t ) == trig_vnum )
				{
					snprintf ( buf, sizeof ( buf ), "M%3d. [%5d] %-25s - [%5d] %-25s\r\n", ++num, GET_MOB_VNUM ( c ),
						   GET_NAME ( c ), GET_ROOM_VNUM ( IN_ROOM ( c ) ), IN_ROOM ( c )->name );
					DYN_RESIZE ( buf );
					found = TRUE;
				}
			}
		}
	}

	for ( ob = object_list.begin(), num = 0; ob != object_list.end(); ob++ )
	{
		if ( SCRIPT ( ob->second ) )
		{
			for ( t = TRIGGERS ( SCRIPT ( ob->second ) ); t; t = t->next )
			{
				if ( GET_TRIG_VNUM ( t ) == trig_vnum )
				{
					print_object_location ( ++num, ob->second, ch, TRUE, buf );
					DYN_RESIZE ( buf );
					found = TRUE;
				}
			}
		}
	}

	for ( nr = 0, num = 0; nr <= top_of_world; nr++ )
	{
		if ( world_vnum[nr] && SCRIPT ( world_vnum[nr] ) )
		{
			for ( t = TRIGGERS ( SCRIPT ( world_vnum[nr] ) ); t; t = t->next )
			{
				if ( GET_TRIG_VNUM ( t ) == trig_vnum )
				{
					snprintf ( buf, sizeof ( buf ), "W%3d. [%6d] {cy%-25s {cg-{cC %s{c0\r\n",
				           ++num, GET_ROOM_VNUM ( world_vnum[nr] ), world_vnum[nr]->name, zone_table[world_vnum[nr]->zone].name );
					DYN_RESIZE ( buf );
					found = TRUE;
				}
			}
		}
	}

	if ( !found )
	{
		snprintf ( buf, sizeof ( buf ), "Nothing.\r\n" );
		DYN_RESIZE ( buf );
	}
	page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

ACMD ( do_searchroomflags )
{
	char buf[MAX_INPUT_LENGTH], arg[MAX_INPUT_LENGTH];
	set<int> flags;
	set<int>::iterator it;
	int i, nr, num = 0;
	bool match;
	vector<string> lowercase_roomflags;
	string argstr;

	for ( i = 0; i < NUM_ROOMFLAGS; ++i )
		lowercase_roomflags.push_back ( tolower ( string ( room_bits[ i ] ) ) );

	while ( *argument )
	{
		half_chop ( argument, arg, argument );

		if ( !*arg )
			break;

		argstr = string ( arg );
		for ( i = 0; i < lowercase_roomflags.size(); ++i )
			if ( lowercase_roomflags[ i ].find ( argstr ) != string::npos )
				flags.insert ( i );
	}

	if ( !flags.size() )
	{
		ch->Send ( "Usage: searchroomflags <flag1> <flag2> ...\r\n" );
		return;
	}

	DYN_DEFINE;
	DYN_CREATE;

	it = flags.begin();
	ch->Send ( "Searching for: %s", room_bits[ *it++ ] );
	for ( ; it != flags.end(); ++it )
		ch->Send ( ", %s", room_bits [ *it ] );
	ch->Send ( "\r\n" );

	for ( nr = 0; nr <= top_of_world; ++nr )
	{
		if ( world_vnum[ nr ] )
		{
			match = TRUE;
			for ( it = flags.begin(); it != flags.end(); ++it )
				if  ( !ROOM_FLAGGED ( world_vnum[ nr ], *it ) )
				{
					match = FALSE;
					break;
				}

			if ( match )
			{
				snprintf ( buf, sizeof ( buf ), "%3d. [%6d] {cy%-25s {cg-{cC %s{c0\r\n", ++num, GET_ROOM_VNUM ( world_vnum[nr] ), world_vnum[nr]->name, zone_table[world_vnum[nr]->zone].name );
				DYN_RESIZE ( buf );
			}
		}
	}

	if ( num )
		page_string ( ch->desc, dynbuf, DYN_BUFFER );
	else
		ch->Send ( "Nothing found.\r\n" );
}

ACMD ( do_plrshop )
{
	vector<string> args;
	string arg, name;
	long id;
	char shop_filename[1024];

	if ( !*argument )
	{
		ch->Send ( "Usage:\r\n"
					"plrshop create <player name> <room vnum> <mob vnum>\r\n"
					"plrshop delete <room vnum>\r\n"
					"plrshop list\r\n" );
		return;
	}

	stringstream ss ( argument );
	while ( ss >> arg )
		args.push_back ( arg );

	if ( is_abbrev ( args[0].c_str(), "create" ) )
	{
		if ( args.size() != 4 )
		{
			ch->Send ( "Wrong number of arguments.\r\n"
						"Usage: plrshop create <player name> <room vnum> <mob vnum>\r\n" );
			return;
		}

		name = args[1];
		if ( ( id = pi.IdByName ( name.c_str() ) ) == -1 )
		{
			ch->Send ( "There is no player called %s.\r\n", name.c_str() );
			return;
		}

		room_vnum r = atoi ( args[2].c_str() );
		if ( r < 0 || r >= world_vnum.size() || world_vnum[r] == NULL )
		{
			ch->Send ( "There is no room with vnum %d.\r\n", r );
			return;
		}

		if ( player_shop.find ( r ) != player_shop.end() )
		{
			ch->Send ( "There's already a playershop at room %d.\r\n", r );
			return;
		}

		mob_vnum m = atoi ( args[3].c_str() );
		if ( !MobProtoExists ( m ) )
		{
			ch->Send ( "There is no mob with vnum %d.\r\n", m );
			return;
		}

		bool new_owner = TRUE;
		for ( auto &p : player_shop )
			if ( p.second->owner_id == id )
			{
				new_owner = FALSE;
				break;
			}

		// add new owner name and shop file
		get_filename ( name.c_str(), shop_filename, PLRSHOP_FILE );
		ofstream file_shop;
		if ( new_owner )
		{
			ofstream file_owners ( PLRSHOP_OWNERS_FILE, ios_base::app );
			if ( file_owners.fail() )
				log ( "SYSERR: Couldn't write to %s", PLRSHOP_OWNERS_FILE );
			else
			{
				if ( file_owners.tellp() > 0 )
					file_owners << endl;
				file_owners << name;
			}
			file_owners.close();

			file_shop.open ( shop_filename );
		}
		else
			file_shop.open ( shop_filename, ios_base::app );

		file_shop << r << endl << m << endl << "end_of_shop" << endl << "end_of_file";
		file_shop.close();

		struct plrshop *pshop = new plrshop;
		pshop->owner_id = id;
		pshop->shop_room = r;
		pshop->shopkeep = m;
		player_shop[r] = pshop;
		world_vnum[r]->func = playershop;
		load_playershop_shopkeep ( r );

		log ( "%s created a playershop for %s at room %d with shopkeep %d", GET_NAME ( ch ), name.c_str(), r, m );
		ch->Send ( "You create a playershop for %s at room %d with shopkeep %d.\r\n", name.c_str(), r, m );
	}
	else if ( is_abbrev ( args[0].c_str(), "delete" ) )
	{
		if ( args.size() != 2 )
		{
			ch->Send ( "Wrong number of arguments.\r\n"
						"Usage: plrshop delete <room vnum>\r\n" );
			return;
		}

		room_vnum r = atoi ( args[1].c_str() );
		if ( r < 0 || r >= world_vnum.size() || world_vnum[r] == NULL )
		{
			ch->Send ( "Room %d does not exist.\r\n", r );
			return;
		}

		if ( player_shop.find ( r ) == player_shop.end() )
		{
			ch->Send ( "There is no playershop at room %d.\r\n", r );
			return;
		}

		name = string ( pi.NameById ( player_shop[r]->owner_id ) );
		world_vnum[r]->func = NULL;
		for ( auto &i : player_shop[r]->item )
			extract_obj ( i->obj );
		delete player_shop[r];
		player_shop.erase ( r );
		save_player_shop ( name );
		log ( "%s deleted %s's playershop at room %d", GET_NAME ( ch ), name.c_str(), r );
		ch->Send ( "You delete %s's playershop at room %d.\r\n", name.c_str(), r );
	}
	else if ( is_abbrev ( args[0].c_str(), "list" ) )
	{
		DYN_DEFINE;
		DYN_CREATE;
		*dynbuf = 0;
		char buf[MAX_INPUT_LENGTH];
		int i = 1;
		DYN_RESIZE ( "Playershops:\r\n" );
		for ( auto &p : player_shop )
		{
			snprintf ( buf, sizeof ( buf ), "%d. Owner: %s, Room: [%d] %s, Shopkeep: [%d] %s\r\n", i++, pi.NameById ( p.second->owner_id ), p.second->shop_room, world_vnum[p.second->shop_room]->name, p.second->shopkeep, GET_NAME ( mob_proto[p.second->shopkeep] ) );
			DYN_RESIZE ( buf );
		}
		if ( i == 1 )
		{
			DYN_RESIZE ( "None.\r\n" );
		}
		page_string ( ch->desc, dynbuf, DYN_BUFFER );
	}
	else
		ch->Send ( "Usage:\r\n"
					"plrshop create <player name> <room vnum> <mob vnum>\r\n"
					"plrshop delete <room vnum>\r\n"
					"plrshop list\r\n" );

	return;
}
