/* ************************************************************************
*   File: config.c                                      Part of CircleMUD *
*  Usage: Configuration of various aspects of CircleMUD operation         *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#define __CONFIG_C__

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "interpreter.h"	/* alias_data definition for structs.h */

#define TRUE	1
#define YES	1
#define FALSE	0
#define NO	0
/*
 * Update:  The following constants and variables are now the default values
 * for backwards compatibility with the new cedit game configurator.  If you
 * would not like to use the cedit command, you can change the values in
 * this file instead.  - Mythran
 */
/*
 * Below are several constants which you can change to alter certain aspects
 * of the way CircleMUD acts.  Since this is a .c file, all you have to do
 * to change one of the constants (assuming you keep your object files around)
 * is change the constant in this file and type 'make'.  Make will recompile
 * this file and relink; you don't have to wait for the whole thing to
 * recompile as you do if you change a header file.
 *
 * I realize that it would be slightly more efficient to have lots of
 * #defines strewn about, so that, for example, the autowiz code isn't
 * compiled at all if you don't want to use autowiz.  However, the actual
 * code for the various options is quite small, as is the computational time
 * in checking the option you've selected at run-time, so I've decided the
 * convenience of having all your options in this one file outweighs the
 * efficency of doing it the other way.
 *
 */

/****************************************************************************/
/****************************************************************************/


/* GAME PLAY OPTIONS */
#if !defined(NO)
#define NO 0
#endif

#if !defined(YES)
#define YES 1
#endif

#if !defined(FALSE)
#define FALSE 0
#endif

#if !defined(TRUE)
#define TRUE  (!FALSE)
#endif
 

/*
 * If you want mortals to level up to immortal once they have enough
 * experience, then set this to 0.  This is the stock behaviour for
 * CircleMUD because it was the stock DikuMud behaviour.  Subtracting
 * this from LVL_IMMORT gives the top level that people can advance to
 * in gain_exp() in limits.c
 * For example, to stop people from advancing to LVL_IMMORT, simply set
 * immort_level_ok to 1.
 */
int immort_level_ok = 0;
/*
 * pk_allowed sets the tone of the entire game.  If pk_allowed is set to
 * NO, then players will not be allowed to kill, summon, charm, or sleep
 * other players, as well as a variety of other "asshole player" protections.
 * However, if you decide you want to have an all-out knock-down drag-out
 * PK Mud, just set pk_allowed to YES - and anything goes.
 */
 
 
 
int pk_allowed = NO;

/* is playerthieving allowed? */
int pt_allowed = NO;

/* minimum level a player must be to shout/holler/gossip/auction */
int level_can_shout = 1;

/* number of movement points it costs to holler */
int holler_move_cost = 20;

/* exp change limits */
int max_exp_gain = 850000;	/* max gainable per kill */
int max_exp_loss = 100000000;	/* max losable per death */

/* number of tics (usually 75 seconds) before PC/NPC corpses decompose */
int max_npc_corpse_time = 10;
int max_pc_corpse_time = 20;

/* How many ticks before a player is sent to the void or idle-rented. */
int idle_void = 8;
int idle_rent_time = 48;

/* This level and up is immune to idling, LVL_IMPL+1 will disable it. */
int idle_max_level = LVL_HERO;

/* should items in death traps automatically be junked? */
int dts_are_dumps = NO;

/*
 * Whether you want items that immortals load to appear on the ground or not.
 * It is most likely best to set this to 'YES' so that something else doesn't
 * grab the item before the immortal does, but that also means people will be
 * able to carry around things like boards.  That's not necessarily a bad
 * thing, but this will be left at a default of 'NO' for historic reasons.
 */
int load_into_inventory = YES;

/* "okay" etc. */
const char *OK = "Okay.\r\n";
const char *NOPERSON = "No-one by that name here.\r\n";
const char *NOEFFECT = "Nothing seems to happen.\r\n";

/*
 * You can define or not define TRACK_THOUGH_DOORS, depending on whether
 * or not you want track to find paths which lead through closed or
 * hidden doors. A setting of 'NO' means to not go through the doors
 * while 'YES' will pass through doors to find the target.
 */
int track_through_doors = YES;

int double_exp = NO;
/****************************************************************************/
/****************************************************************************/


/* RENT/CRASHSAVE OPTIONS */

/*
 * Should the MUD allow you to 'rent' for free?  (i.e. if you just quit,
 * your objects are saved at no cost, as in Merc-type MUDs.)
 */
int free_rent = YES;

/* maximum number of items players are allowed to rent */
int max_obj_save = 60;

/* receptionist's surcharge on top of item costs */
int min_rent_cost = 100;

/*
 * Should the game automatically save people?  (i.e., save player data
 * every 4 kills (on average), and Crash-save as defined below.  This
 * option has an added meaning past bpl13.  If auto_save is YES, then
 * the 'save' command will be disabled to prevent item duplication via
 * game crashes.
 */
int auto_save = YES;

int tunnel_size = 2;
/*
 * if auto_save (above) is yes, how often (in minutes) should the MUD
 * Crash-save people's objects?   Also, this number indicates how often
 * the MUD will Crash-save players' houses.
 */
int autosave_time = 2;

/* Lifetime of crashfiles and forced-rent (idlesave) files in days */
int crash_file_timeout = 50;

/* Lifetime of normal rent files in days */
int rent_file_timeout = 60;

/* Do you want to automatically qipe players who've been gone too long? */
int auto_pwipe = NO;

/* Autowipe deletion criteria
 * This struct holds information used to determine which players to wipe
 * then the mud boots.  The levels must be in ascending order, with a
 * descening level marking the end of the araay.  A level -1 entry in the
 * beginning is the case for players with the PLR_DELETED flag.  The
 * values below match the stock purgeplay.c criteria.
 *
 * Detailed explanation by array element:
 * * Element 0, level -1, days 0: Players with PLR_DELETED flag are always
 *     wiped.
 * * Element 1, level 0, days 0: Players at level 0 have create a
 *     character, but have never actually entered the game, so always
 *     wipe them.
 * * Element 2, level 1, days 4: Players at level 1 are wiped if they
 *     haven't logged on in the past 4 days.
 * * Element 3, level 4, days 7: Players at level 2 through 4 are wiped if
 *     they haven't logged on in the past 7 days.
 * * Element 4, level 10, days 30: Players level 5-10 get 30 days.
 * * Element 5, level LVL_IMMORT - 1, days 60: All other mortals get 60 days.
 * * Element 6, level LVL_IMPL, days 90: Immortals get 90 days.
 * * Element 7, Because -2 is less than LVL_IMPL, this is assumed to be
 *     the end of the criteria.  The days entry is not used in this case.
 */
struct pclean_criteria_data pclean_criteria[] = {
/*	LEVEL		DAYS	*/
    {-1, 0},
    {0, 0},
    {1, 4},
    {4, 7},
    {10, 30},
    {LVL_IMMORT - 1, 60},
    {LVL_IMPL, 90},
    {-2, 0}
};

/* Do you want to save backups of players and their object and mail when
 * they are deleted by the pfile autocleaner?
 */
int backup_wiped_pfiles = YES;

/* Do you want players who self-delete to be wiped immediately with no
 * backup?
 */

int selfdelete_fastwipe = YES;

/* Minimum level for pfile backup to occur when wiping players.
 * Note: If you set selfdelete_fastwipe to NO, you'll need this to
 * prevent your backup directory from being filled with level 0 and
 * 1 deletions.
 */
int pfile_backup_minlevel = 5;

/* hard coded xap_objs */
const int xap_objs = YES;

/****************************************************************************/
/****************************************************************************/


/* ROOM NUMBERS */

/* virtual number of room that mortals should enter at */
room_vnum mortal_start_room = 3001;

/* virtual number of room that immorts should enter at by default */
room_vnum immort_start_room = 1204;

/* virtual number of room that frozen players should enter at */
room_vnum frozen_start_room = 1202;

/*
 * virtual numbers of donation rooms.  note: you must change code in
 * do_drop of act.item.c if you change the number of non-NOWHERE
 * donation rooms.
 */
room_vnum donation_room_1 = 3063;
room_vnum donation_room_2 = 3063;	/* unused - room for expansion */
room_vnum donation_room_3 = 3063;	/* unused - room for expansion */

room_vnum gladiator_death_room = 26783;

/****************************************************************************/
/****************************************************************************/


/* GAME OPERATION OPTIONS */

/*
 * This is the default port on which the game should run if no port is
 * given on the command-line.  NOTE WELL: If you're using the
 * 'autorun' script, the port number there will override this setting.
 * Change the PORT= line in autorun instead of (or in addition to)
 * changing this.
 */
ush_int DFLT_PORT = 6000;

/*
 * IP address to which the MUD should bind.  This is only useful if
 * you're running Circle on a host that host more than one IP interface,
 * and you only want to bind to *one* of them instead of all of them.
 * Setting this to NULL (the default) causes Circle to bind to all
 * interfaces on the host.  Otherwise, specify a numeric IP address in
 * dotted quad format, and Circle will only bind to that IP address.  (Of
 * course, that IP address must be one of your host's interfaces, or it
 * won't work.)
 */
const char *DFLT_IP = NULL;	/* bind to all interfaces */
/* const char *DFLT_IP = "192.168.1.1";  -- bind only to one interface */
// const char *DFLT_IP = "209.43.5.200";

/* default directory to use as data directory */
const char *DFLT_DIR = "lib";

/*
 * What file to log messages to (ex: "log/syslog").  Setting this to NULL
 * means you want to log to stderr, which was the default in earlier
 * versions of Circle.  If you specify a file, you don't get messages to
 * the screen. (Hint: Try 'tail -f' if you have a UNIX machine.)
 */
// const char *LOGNAME = NULL;
const char *LOGNAME = "log/syslog";
/* const char *LOGNAME = "log/syslog";  -- useful for Windows users */

/* maximum number of players allowed before game starts to turn people away */
int max_playing = 300;

/* maximum size of bug, typo and idea files in bytes (to prevent bombing) */
int max_filesize = 100000;

/* maximum number of password attempts before disconnection */
int max_bad_pws = 3;

/*
 * Rationale for enabling this, as explained by naved@bird.taponline.com.
 *
 * Usually, when you select ban a site, it is because one or two people are
 * causing troubles while there are still many people from that site who you
 * want to still log on.  Right now if I want to add a new select ban, I need
 * to first add the ban, then SITEOK all the players from that site except for
 * the one or two who I don't want logging on.  Wouldn't it be more convenient
 * to just have to remove the SITEOK flags from those people I want to ban
 * rather than what is currently done?
 */
int siteok_everyone = TRUE;

/*
 * Some nameservers are very slow and cause the game to lag terribly every 
 * time someone logs in.  The lag is caused by the gethostbyaddr() function
 * which is responsible for resolving numeric IP addresses to alphabetic names.
 * Sometimes, nameservers can be so slow that the incredible lag caused by
 * gethostbyaddr() isn't worth the luxury of having names instead of numbers
 * for players' sitenames.
 *
 * If your nameserver is fast, set the variable below to NO.  If your
 * nameserver is slow, of it you would simply prefer to have numbers
 * instead of names for some other reason, set the variable to YES.
 *


 * You can experiment with the setting of nameserver_is_slow on-line using
 * the SLOWNS command from within the MUD.
 */

int nameserver_is_slow = YES;

 /*
  * Shall we try to resolve hostnames?
  * NOTE: If use_external_lookup_process is set to FALSE, then
  * the mud might lag a little bit.  This lag should however not
  * be to severe on most systems.
  *
  * This option supercedes the nameserver_is_slow option.
  */
 int lookup_hostname = YES;
 
 /*
  * Shall we try to look up all connection users username?
  * If use_external_lookup_process is set to FALSE, then the 
  * mud might lag for considerable amount of time.
  */
 int lookup_username = YES;
 
 /*
  * Shall we use an external process to resolve hostnames
  * and lookup usernames?
  */
 int use_external_lookup_process = YES;
 
 
/*
 * Will changes save automaticaly in OLC ?
 */
int auto_save_olc = 1;

/*
 * if you wish to enable Aedit, set this to 1 
 * This will make the mud look for a file called socials.new,
 * which is in a different format than the stock socials file.
 */
int use_new_socials = 1;


const char *MENU =
"\r\n{cR  ,/         \\. \r\n"
" ((           )) \r\n"
"  \\`.       ,'/  \r\n"
"   )')     (`(       \r\n"
" ,'`/       \\,`.      {cCWelcome  To  4  Dimensions{cc            \r\n"
"{cR(`-(         )-')   {cc\r\n"
"{cR \\-'\\,-'\"`-./`-/     {cy0{cg) Exit from 4 Dimensions.\r\n"
"{cR  \\-')     (`-/      {cy1{cg) Enter the game.\r\n"
"{cR  /`'       `'\\      {cy2{cg) Choose another character from this account.\r\n"
"{cR (  _       _  )     {cc-------------------------------------------------\r\n"
"{cR | ( \\     / ) |     {cy3{cg) Manage Accounts.\r\n"
"{cR |  `.\\   /,'  |     {cy4{cg) Enter a description for this character.\r\n"
"{cR |    `\\ /'    |     {cy5{cg) Read the 4D background story.\r\n"
"{cR (             )     {cy6{cg) Change password.\r\n"
"{cR  \\           /      {cy7{cg) Delete this character.\r\n"
"{cR   \\         /      \r\n"
"{cR    `.     ,'       \r\n"
"{clhh{cR    `-.-'            \r\n"
"                     {cwMake your choice: {c0";


const char *WELC_MESSG =
    "\r\n"
    "Welcome to the land of 4 Dimensions!  May your visit here be... Interesting."
    "\r\n\r\n";

const char *START_MESSG =
    "Welcome.  This is your new 4 Dimensions character!  You can now earn gold,\r\n"
    "gain experience, find weapons and equipment, and much more -- while\r\n"
    "meeting people from around the world!\r\n\r\n"
    "{cYType HELP to see a list of help topics\r\n\r\n";

/****************************************************************************/
/****************************************************************************/


/* AUTOWIZ OPTIONS */

/*
 * Should the game automatically create a new wizlist/immlist every time
 * someone immorts, or is promoted to a higher (or lower) god level?
 * NOTE: this only works under UNIX systems.
 */
int use_autowiz = YES;

/* If yes, what is the lowest level which should be on the wizlist?  (All
   immort levels below the level you specify will go on the immlist instead.) */
int min_wizlist_lev = LVL_GOD;

/* virtual numbers for battle-field */
//room_vnum battle_start_room   = 9300;
//room_vnum battle_recall_room    = 9300;
//room_vnum battle_min_room     = 9300;
//room_vnum battle_max_room     = 9363;
