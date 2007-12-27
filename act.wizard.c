
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
 * updated pets so that they dont have weight problems, updated award points
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

#include "conf.h"
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

/*   external vars  */
extern struct index_data *obj_index;
extern struct obj_data *obj_proto;
extern int TEMP_LOAD_CHAR;
extern struct time_data time_info;
extern struct attack_hit_type attack_hit_text[];
extern char *class_abbrevs[];
extern char *race_abbrevs[];
extern time_t boot_time;
extern zone_rnum top_of_zone_table;
extern int circle_shutdown, circle_reboot;
extern int circle_restrict;
extern int buf_switches, buf_largecount, buf_overflows;
extern const char *save_info_msg[];	/* In olc.c */
extern int forget_num_obj;
extern const char *wiz_groups[];
extern socket_t mother_desc;
extern ush_int port;

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

char *one_arg(char *arg, char *first_arg);
void remove_player(int pfilepos);
int find_name(char *name);
void weight_to_object(struct obj_data *obj, int weight);
char *getline( char *str, char *buf, size_t len );
void write_aliases(struct char_data *ch);
void do_show_corpses(CHAR_DATA *ch);
void do_show_errors(CHAR_DATA *ch);
int get_weapon_speed(OBJ_DATA *wep);
int get_weapon_accuracy(OBJ_DATA *wep);
int get_weapon_evasion(OBJ_DATA *wep);
const char *material_name(int type);
int find_month(void);
void show_shops(struct char_data *ch, char *value);
void hcontrol_list_houses(struct char_data *ch);
void do_start(struct char_data *ch);
void appear(struct char_data *ch);
void reset_zone(zone_rnum zone);
void roll_real_abils(struct char_data *ch);
int parse_class(char arg);
int parse_race(char arg);
void write_poofs(struct char_data *ch);
void save_corpses(void);
void check_autowiz(struct char_data *ch);
int zone_number(void *what, int type);
int can_edit_zone(struct char_data *ch, int number);
int real_zone(int number);
long long gold_data(int type, long long amount);
void olc_list_flags(struct char_data *ch, const char *apply_stuff[]);

const char * race_name(struct char_data *ch);
const char *simple_class_name(struct char_data *ch);
int show_vars = FALSE;
int save_all(void);
void print_zone(struct char_data *ch, zone_vnum vnum);
zone_rnum real_zone_by_thing(room_vnum vznum); /* added for zone_checker */
SPECIAL(shop_keeper);

int check_potion_weight(struct obj_data *obj);
int check_potion_price(struct obj_data *obj);
void write_ignorelist(struct char_data *ch);
void Crash_rentsave(struct char_data *ch, int cost);

/* local functions */
int spell_price(struct obj_data *obj, int val);
void show_door_errors(struct char_data *ch);
C_FUNC(delete_player);
void perform_delete_player(char *charname);
const char *balance_display(int balance);
ACMD(do_potionweight);
int perform_set(struct char_data *ch, struct char_data *vict, int mode,
                char *val_arg);
void perform_immort_invis(struct char_data *ch, int level);
void do_connections(struct char_data *ch, char *arg);
ACMD(do_echo);
ACMD(do_send);
room_rnum find_target_room(struct char_data *ch, char *rawroomstr);
ACMD(do_at);
ACMD(do_goto);
ACMD(do_trans);
ACMD(do_teleport);
ACMD(do_vnum);
void do_stat_room(struct char_data *ch);
void do_stat_object(struct char_data *ch, struct obj_data *j);
void do_stat_character(struct char_data *ch, struct char_data *k);
void list_destinations(struct travel_point_data *travel_list, struct char_data *ch);
ACMD(do_stat);
ACMD(do_shutdown);
void stop_snooping(struct char_data *ch);
ACMD(do_snoop);
ACMD(do_switch);
ACMD(do_return);
ACMD(do_load);
ACMD(do_vstat);
ACMD(do_purge);
ACMD(do_syslog);
ACMD(do_advance);
ACMD(do_restore);
void perform_immort_vis(struct char_data *ch);
ACMD(do_invis);
ACMD(do_gecho);
ACMD(do_poofset);
ACMD(do_dc);
ACMD(do_wizlock);
ACMD(do_date);
ACMD(do_last);
ACMD(do_force);
ACMD(do_wiznet);
ACMD(do_zreset);
ACMD(do_wizutil);
size_t print_zone_to_buf(char *bufptr, size_t left, zone_rnum zone, int listall);
ACMD(do_show);
ACMD(do_set);
ACMD(do_peace);
void clearMemory(struct char_data *ch);
ACMD(do_autowiz);
ACMD(do_statinnate);
int mortal_player_info(struct char_data *ch,struct char_data *vict);
int update_award(struct char_data *ch);
int update_reward(struct char_data *ch);
void snoop_check(struct char_data *ch);
ACMD(do_saveall);
ACMD(do_ps_aux);
ACMD(do_deleteplayer);
ACMD(do_search_triggers);
ACMD(do_ctellsnoop);
ACMD(do_addtp);

struct player_gold_info
{
  int p_id;
  gold_int amount;
};

#define PC   1
#define NPC 2
#define BOTH 3

#define MISC	0
#define BINARY	1
#define NUMBER	2

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
const char *balances[] = {
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
trust_fields[] = {
                   {
                     "ban", LVL_IMPL, PC, BINARY},	/*  0 */
                   {
                     "dspln", LVL_IMPL, PC, BINARY},	/*  1 */
                   {
                     "edit", LVL_IMPL, PC, BINARY},	/*  2 */
                   {
                     "heal", LVL_IMPL, PC, BINARY},	/*  3 */
                   {
                     "house", LVL_IMPL, PC, BINARY},	/*  4 */
                   {
                     "imm1", LVL_IMPL, PC, BINARY},	/*  5 */
                   {
                     "imm2", LVL_IMPL, PC, BINARY},	/*  6 */
                   {
                     "impl", LVL_IMPL, PC, BINARY},	/*  7 */
                   {
                     "kill", LVL_IMPL, PC, BINARY},	/*  8 */
                   {
                     "load", LVL_IMPL, PC, BINARY},	/*  9 */
                   {
                     "olc", LVL_IMPL, PC, BINARY},	/* 10 */
                   {
                     "quest", LVL_IMPL, PC, BINARY},	/* 11 */
                   {
                     "sen", LVL_IMPL, PC, BINARY},	/* 12 */
                   {
                     "tele", LVL_IMPL, PC, BINARY},	/* 13 */
                   {
                     "trig", LVL_IMPL, PC, BINARY},	/* 14 */
                   {
                     "all", LVL_IMPL, PC, BINARY},	/* 15 */
                   {
                     "marry", LVL_IMPL, PC, BINARY},	/* 16 */
                   {
                     "goto", LVL_IMPL, PC, BINARY},	/* 17 */
                   {
                     "global", LVL_IMPL, PC, BINARY},	/* 18 */
                   {
                     "hedit", LVL_IMPL, PC, BINARY},	/* 19 */
                   {
                     "imm3", LVL_IMPL, PC, BINARY},     /* 20 */
                   {
                     "\n", 0, PC, MISC}
                 };


int perform_trust(struct char_data *ch, struct char_data *vict, int mode,
                  char *toggle, char *value)
{
  int on = 0, off = 0;
  char output[MAX_STRING_LENGTH];
  int save = 0, nosave = 0;

  /* Find the value of the argument */
  if (trust_fields[mode].type == BINARY)
  {

    if (!strcmp(toggle, "on") || !strcmp(toggle, "yes"))
      on = 1;
    else if (!strcmp(toggle, "off") || !strcmp(toggle, "no"))
      off = 1;

    if (!(on || off))
    {
      send_to_char("Toggle must be 'on' or 'off'.\r\n", ch);
      return 0;
    }

    if (!strcmp(value, "save"))
      save = 1;
    else if (!strcmp(value, "nosave"))
      nosave = 1;

    if (!(save || nosave))
    {
      send_to_char("Value must be 'save' or 'nosave'.\r\n", ch);
      return 0;
    }

    snprintf(output, sizeof(output), "(GC) %s %s for %s by %s (%s in file).",
             trust_fields[mode].cmd,
             ONOFF(on), GET_NAME(vict), GET_NAME(ch), value);

  }

  switch (mode)
  {
  case 0:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_BAN_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_BAN_GRP)
        break;
  case 1:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_DSPLN_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_DSPLN_GRP)
        break;
  case 2:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_EDIT_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_EDIT_GRP)
        break;
  case 3:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_HEAL_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_HEAL_GRP)
        break;
  case 4:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_HOUSE_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_HOUSE_GRP)
        break;
  case 5:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_IMM1_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_IMM1_GRP)
        break;
  case 6:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_IMM2_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_IMM2_GRP)
        break;
  case 7:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_IMPL_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_IMPL_GRP)
        break;
  case 8:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_KILL_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_KILL_GRP)
        break;
  case 9:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_LOAD_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_LOAD_GRP)
        break;
  case 10:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_OLC_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_OLC_GRP)
        break;
  case 11:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_QUEST_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_QUEST_GRP)
        break;
  case 12:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_SEN_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_SEN_GRP)
        break;
  case 13:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_TELE_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_TELE_GRP)
        break;
  case 14:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_TRIG_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_TRIG_GRP)
        break;
  case 15:
    if (save == 1)
    {
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_BAN_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_DSPLN_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_EDIT_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_HEAL_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_HOUSE_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_IMM1_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_IMM2_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_IMM3_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_IMPL_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_KILL_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_LOAD_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_OLC_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_QUEST_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_SEN_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_TELE_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_TRIG_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_MARRY_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_GOTO_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_GLOBAL_GRP)
    }
    else
    {
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_BAN_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_DSPLN_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_EDIT_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_HEAL_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_HOUSE_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_IMM1_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_IMM2_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_IMPL_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_KILL_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_LOAD_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_OLC_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_QUEST_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_SEN_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_TELE_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_TRIG_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_MARRY_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_GOTO_GRP)
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_GLOBAL_GRP)
    }
    break;
  case 16:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_MARRY_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_MARRY_GRP)
        break;
  case 17:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_GOTO_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_GOTO_GRP)
        break;
  case 18:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_GLOBAL_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_GLOBAL_GRP)
        break;
  case 19:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_HEDIT_GRP)
      else
        SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_HEDIT_GRP)
        break;
  case 20:
    if (save == 1)
      SET_OR_REMOVE_TRUST(CMD_FLAGS(vict), WIZ_IMM3_GRP)
    else
      SET_OR_REMOVE_TRUST(CMD_FLAGS2(vict), WIZ_IMM3_GRP)
    break;
  default:
    send_to_char("That isn't a trust group!\r\n", ch);
    return 0;
    break;
  }
  new_mudlog( NRM, GET_LEVEL(ch), TRUE, "%s", output);
  return 1;
}

ACMD(do_trust)
{
  char group[MAX_INPUT_LENGTH], name[MAX_INPUT_LENGTH],
  toggle[MAX_INPUT_LENGTH], value[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];

  struct char_data *vict;
  int mode = 0;
  int len = 0;
  int retval = 0;

  half_chop(argument, name, buf);
  half_chop(buf, group, buf);
  half_chop(buf, toggle, buf);
  strcpy(value, buf);

  if (!*name || !*group || !*toggle || !*value)
  {
    send_to_char
    ("Usage: trust <victim> <group> <on/off> <save/nosave>\r\n",
     ch);
    send_to_char("Valid trust groups:\r\n", ch);
    send_to_char
    ("ban, dspln, edit, heal, house, imm1, imm2, imm3, impl, kill\r\nload, marry, olc, quest, sen, tele, trig, goto, global, hedit, all\r\n",
     ch);
    return;
  }

  if (!(vict = get_player_vis(ch, name, NULL, 0)))
  {
    send_to_char("There is no such player.\r\n", ch);
    return;
  }

  if (IS_NPC(vict))
  {
    send_to_char("Trust is for Kids! Silly rabbit!\r\n", ch);
    return;
  }

  if ((vict == ch) && (GET_LEVEL(ch) != LVL_IMPL))
  {
    send_to_char("You can't trust yourself!\r\n", ch);
    return;
  }


  /* Check to make sure all the levels are correct */
  if ((GET_LEVEL(ch) <= GET_LEVEL(vict)) && (GET_LEVEL(ch) != LVL_IMPL))
  {
    send_to_char("Maybe that's not such a great idea...\r\n", ch);
    return;
  }

  if (GET_LEVEL(vict) < LVL_GOD && !PLR_FLAGGED(vict, PLR_HERO))
  {
    send_to_char("You can't trust mortals! hehe..\r\n", ch);
    return;
  }

  /* find the command in the list */
  len = strlen(group);
  for (mode = 0; *(trust_fields[mode].cmd) != '\n'; mode++)
    if (!strncmp(group, trust_fields[mode].cmd, len))
      break;
  /* perform the godset */
  retval = perform_trust(ch, vict, mode, toggle, value);

  /* save the character if a change was made */
  if (retval)
  {
    save_char(vict);
    // log("(do_trust)Saving %s in room %d.", GET_NAME(ch), IN_ROOM(ch));
    send_to_char("Command Group Activated.\r\n", ch);
  }
}


ACMD(do_autowiz)
{
  new_send_to_char(ch,"Processing...\r\n");
  check_autowiz(ch);
  new_send_to_char(ch,"done.\r\n");
}

ACMD(do_echo)
{
  skip_spaces(&argument);

  if (!*argument)
    new_send_to_char(ch, "Yes.. but what?\r\n");
  else
  {
    char buf[MAX_INPUT_LENGTH + 4];
    int sp = 0;

    if (*argument == '\'' && *(argument+1) == 's' && *(argument+2) == ' ')
      sp = 1;
    else if (*argument == 's' && *(argument+1) == ' ')
      sp = 1;


    if (subcmd == SCMD_EMOTE)
      snprintf(buf, sizeof(buf), "%s%s", sp ? "$n" : "$n ", argument);
    else if (subcmd == SCMD_POSE)
    {

      snprintf(buf, sizeof(buf), "[$n]\r\n%s", argument);

    }
    else if (subcmd == SCMD_RECHO)
    {
      if (!PLR_FLAGGED(ch, PLR_RP_LEADER))
      {
        new_send_to_char(ch, "You can't do that!\r\n");
        return;
      }
      strlcpy(buf, argument, sizeof(buf) );
    }
    else
      strlcpy(buf, argument, sizeof(buf) );

    if (!PLR_FLAGGED(ch, PLR_COVENTRY))
      act(buf, FALSE, ch, 0, 0, TO_ROOM);


    if (!IS_NPC(ch) && PRF_FLAGGED(ch, PRF_NOREPEAT))
      new_send_to_char(ch, "%s", CONFIG_OK);
    else
      act(buf, FALSE, ch, 0, 0, TO_CHAR);
  }
}


ACMD(do_send)
{
  char arg[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH];
  struct char_data *vict;

  half_chop(argument, arg, buf);

  if (!*arg)
  {
    new_send_to_char(ch, "Send what to who?\r\n");
    return;
  }
  if (!(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
  {
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    return;
  }
  new_send_to_char(vict, "%s\r\n", buf);
  if (PRF_FLAGGED(ch, PRF_NOREPEAT))
    new_send_to_char(ch, "Sent.\r\n");
  else
    new_send_to_char(ch, "You send '%s' to %s.\r\n", buf,
                     GET_NAME(vict));
}

ACMD(do_prompt_new)
{
  char **msg;

  if (subcmd == SCMD_PROMPT)
    msg = &(PROMPT(ch));
  else
    msg = &(BPROMPT(ch));

  skip_spaces(&argument);

  if (!argument || !*argument)
  {
    if (subcmd == SCMD_PROMPT)
      new_send_to_char(ch, "Your prompt: %s\r\nFor a default prompt type: prompt default\r\nFor no prompt type: prompt none\r\n", PROMPT(ch));
    else
      new_send_to_char(ch, "Your battle prompt: %s\r\nFor a default battle prompt type: bprompt default\r\nFor no battle prompt type: bprompt none\r\n", BPROMPT(ch));
    return;
  }

  if (!strcmp(argument, "default") || !strcmp(argument, "all"))
  {
    if (subcmd == SCMD_PROMPT)
      strcpy(argument, "{cg%h{cwH {cc%m{cwM {cy%v{cwV {cW(%S) {cC%E{cyTNL{c0>");
    else if (subcmd == SCMD_BPROMPT)
      strcpy(argument, "{cg%h{cwH {cc%m{cwM {cW(%S) {cC%E{cyTNL {cwVictHp:{cM%f%%{c0 >");
  }
  else if ((subcmd == SCMD_BPROMPT) && !strcmp(argument, "copy"))
  {
    strcpy(argument, PROMPT(ch));
  }

  if (strlen(argument) > MAX_PROMPT_LENGTH - 30)
  {
    new_send_to_char
    (ch, "Your prompt must be shorter than %d characters.\r\n",
     MAX_PROMPT_LENGTH - 30);
    return;
  }


  if (*msg)
    free(*msg);

  if (!*argument)
    *msg = str_dup("none");
  else
    *msg = str_dup(argument);

  new_send_to_char(ch, "%s", CONFIG_OK);
}

/* take a string, and return an rnum.. used for goto, at, etc.  -je 4/6/93 */
room_rnum find_target_room(struct char_data *ch, char *rawroomstr)
{
  room_rnum location = NULL;
  char roomstr[MAX_INPUT_LENGTH];

  skip_spaces(&rawroomstr);
  one_argument(rawroomstr, roomstr);

  if (!*roomstr)
  {
    new_send_to_char(ch, "You must supply a room number or name.\r\n");
    return (NULL);
  }

  if (isdigit(*roomstr) && !strchr(roomstr, '.'))
  {
    if ((location = real_room(atoi(roomstr))) == NULL)
    {
      new_send_to_char(ch, "No room exists with that number.\r\n");
      return (NULL);
    }
  }
  else
  {
    struct char_data *target_mob;
    struct obj_data *target_obj;
    struct room_data *target_room;
    char *mobobjstr = rawroomstr;
    int num;

    if (*roomstr == UID_CHAR)
    {

      if ((target_mob = find_char(atoi(roomstr + 1))) != NULL)
        return IN_ROOM(target_mob);
      else if ((target_room = find_room(atoi(roomstr + 1))) != NULL)
        return (target_room);
      else if ((target_obj = find_obj(atoi(roomstr + 1))) != NULL)
      {
        if (IN_ROOM(target_obj) != NULL)
          return IN_ROOM(target_obj);
        else if (target_obj->carried_by && IN_ROOM(target_obj->carried_by) != NULL)
          return IN_ROOM(target_obj->carried_by);
        else if (target_obj->worn_by && IN_ROOM(target_obj->worn_by) != NULL)
          return IN_ROOM(target_obj->worn_by);
        else if (target_obj->in_locker && IN_ROOM(target_obj->in_locker) != NULL)
          return IN_ROOM(target_obj->in_locker);
      }
    }

    num = get_number(&mobobjstr);
    if ((target_mob =
           get_char_vis(ch, mobobjstr, &num, FIND_CHAR_WORLD)) != NULL)
    {
      if ((location = IN_ROOM(target_mob)) == NULL)
      {
        new_send_to_char(ch,  "That character is currently lost.\r\n");
        return (NULL);
      }
    }
    else if ((target_obj = get_obj_vis(ch, mobobjstr, &num)) != NULL)
    {
      if (IN_ROOM(target_obj) != NULL)
        location = IN_ROOM(target_obj);
      else if (target_obj->carried_by && IN_ROOM(target_obj->carried_by) != NULL)
        location = IN_ROOM(target_obj->carried_by);
      else if (target_obj->worn_by && IN_ROOM(target_obj->worn_by) != NULL)
        location = IN_ROOM(target_obj->worn_by);
      else if (target_obj->in_locker && IN_ROOM(target_obj->in_locker) != NULL)
        location = IN_ROOM(target_obj->in_locker);

      if (location == NULL)
      {
        new_send_to_char(ch,
                         "That object is currently not in a room.\r\n");
        return (NULL);
      }
    }

    if (location == NULL)
    {
      new_send_to_char(ch, "Nothing exists by that name.\r\n");
      return (NULL);
    }
  }
  if (IS_NPC(ch))
  {
    if (ch->desc && ch->desc->original && GET_LEVEL(ch->desc->original) < LVL_SEN)
      return NULL;
    else
      return (location);
  }



  if ((!can_edit_zone(ch, location->zone) &&
       (location->number/100) != 12 && (location->number/100) != 30) && ((!CMD_FLAGGED(ch, WIZ_GOTO_GRP))
           && (!CMD_FLAGGED2(ch, WIZ_GOTO_GRP))) )
  {
    new_send_to_char(ch, "Sorry, that is out of limits.\r\nIf found there you may lose your imm.\r\n");
    new_mudlog(CMP,(GET_LEVEL(ch) == LVL_IMPL ? GET_LEVEL(ch):GET_LEVEL(ch)+1), TRUE, "(GC) %s tried to goto %d.",
               GET_NAME(ch),location->number);

  }
  else if (ROOM_FLAGGED(location, ROOM_GODROOM) && GET_LEVEL(ch) < LVL_SEN)
    new_send_to_char(ch, "You are not godly enough to use that room!\r\n");
  else if (ROOM_FLAGGED(location, ROOM_PRIVATE) && GET_LEVEL(ch) < LVL_IMPL && num_pc_in_room(location) >= 2)
    new_send_to_char(ch, "There's a private conversation going on in that room.\r\n");
  else if (ROOM_FLAGGED(location, ROOM_HOUSE)
           && !House_can_enter(ch, GET_ROOM_VNUM(location)))
    new_send_to_char(ch, "That's private property -- no trespassing!\r\n");
  else
    return (location);

  return (NULL);
}



ACMD(do_at)
{
  char command[MAX_INPUT_LENGTH];
  struct room_data * location, *original_loc;
  char buf[MAX_INPUT_LENGTH];

  half_chop(argument, buf, command);
  if (!*buf)
  {
    send_to_char("You must supply a room number or a name.\r\n", ch);
    return;
  }

  if (!*command)
  {
    send_to_char("What do you want to do there?\r\n", ch);
    return;
  }

  if ((location = find_target_room(ch, buf))==NULL)
    return;

  /* a location has been found. */
  original_loc = IN_ROOM(ch);
  if (!move_char_to(ch, location))
    return;
  command_interpreter(ch, command);

  /* check if the char is still there */
  if (IN_ROOM(ch) == location)
  {
    move_char_to(ch, original_loc);
  }
}

ACMD(do_atlvl)
{
  char command[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];
  int lev;

  half_chop(argument, buf, command);
  if (!*buf)
  {
    send_to_char("You must supply a level.\r\n", ch);
    return;
  }

  if (GET_ORIG_LEV(ch))
  {
    new_send_to_char(ch, "Return to your original level first.\r\n");
    return;
  }



  lev = atoi(buf);
  if (lev < 1 || lev >= GET_LEVEL(ch))
  {
    send_to_char("That is an invalid level.\r\n", ch);
    return;
  }
  new_send_to_char(ch, "{cYINFO:You are now at level %d, to return to original level type: return{c0\r\n", lev);

  GET_ORIG_LEV(ch) = GET_LEVEL(ch);
  GET_LEVEL(ch) = lev;
  if (*command)
    command_interpreter(ch, command);

  /*if (GET_LEVEL(ch) != orig)
  GET_LEVEL(ch) = orig;*/

}


ACMD(do_goto)
{
  room_rnum location;
  char buf[MAX_INPUT_LENGTH];

  if ((location = find_target_room(ch, argument)) < 0)
    return;

  if (POOFOUT(ch))
    snprintf(buf, sizeof(buf), "%s", POOFOUT(ch));
  else
    snprintf(buf, sizeof(buf), "$n disappears in a puff of smoke.");

  act(buf, TRUE, ch, 0, 0, TO_ROOM);
  if (!move_char_to(ch, location))
    return;

  if (POOFIN(ch))
    snprintf(buf, sizeof(buf), "%s", POOFIN(ch));
  else
    snprintf(buf, sizeof(buf), "$n appears with an ear-splitting bang.");

  act(buf, TRUE, ch, 0, 0, TO_ROOM);
  LOOK(ch);
  entry_memory_mtrigger(ch);
  greet_mtrigger(ch, -1);
  greet_memory_mtrigger(ch);
  enter_wtrigger(IN_ROOM(ch), ch, -1);

}



ACMD(do_trans)
{
  struct descriptor_data *i;
  struct char_data *victim;
  char buf[MAX_INPUT_LENGTH];


  one_argument(argument, buf);
  if (!*buf)
    send_to_char("Whom do you wish to transfer?\r\n", ch);
  else if (str_cmp("all", buf))
  {
    if (!(victim = get_char_vis(ch, buf, NULL, FIND_CHAR_WORLD)))
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    else if (victim == ch)
      send_to_char("That doesn't make much sense, does it?\r\n", ch);
    else
    {
      if ((GET_LEVEL(ch) < GET_LEVEL(victim)) && !IS_NPC(victim))
      {
        send_to_char("Go transfer someone your own size.\r\n", ch);
        return;
      }
      act("$n disappears in a mushroom cloud.", FALSE, victim, 0, 0,
          TO_ROOM);
      new_send_to_char(ch, "You transfer %s from room %d.\r\n",
                       PERS(victim, ch),
                       IN_ROOM(victim)->number);
      if (!move_char_to(victim, IN_ROOM(ch)))
        return;
      act("$n arrives from a puff of smoke.", FALSE, victim, 0, 0,
          TO_ROOM);
      act("$n has transferred you!", FALSE, ch, 0, victim, TO_VICT);
      look_at_room(victim, 0);
      entry_memory_mtrigger(victim);
      greet_mtrigger(victim, -1);
      greet_memory_mtrigger(victim);
      enter_wtrigger(IN_ROOM(victim), victim, -1);
    }
  }
  else
  {			/* Trans All */
    if (GET_LEVEL(ch) < LVL_GRGOD)
    {
      send_to_char("I think not.\r\n", ch);
      return;
    }

    for (i = descriptor_list; i; i = i->next)
      if (STATE(i) == CON_PLAYING && i->character
          && i->character != ch)
      {
        victim = i->character;
        if (GET_LEVEL(victim) >= GET_LEVEL(ch))
          continue;
        new_send_to_char(ch, "You transfer %s from room %d.\r\n",
                         PERS(victim, ch),
                         IN_ROOM(victim)->number);
        act("$n disappears in a mushroom cloud.", FALSE, victim, 0,
            0, TO_ROOM);
        if (move_char_to(victim, IN_ROOM(ch)))
          return;
        act("$n arrives from a puff of smoke.", FALSE, victim, 0,
            0, TO_ROOM);
        act("$n has transferred you!", FALSE, ch, 0, victim,
            TO_VICT);
        look_at_room(victim, 0);
        entry_memory_mtrigger(victim);
        greet_mtrigger(victim, -1);
        greet_memory_mtrigger(victim);
        enter_wtrigger(IN_ROOM(victim), victim, -1);
      }
    new_send_to_char(ch, "%s", CONFIG_OK);
  }
}



ACMD(do_teleport)
{
  struct char_data *victim;
  room_rnum target;
  char buf[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];

  two_arguments(argument, buf, buf2);

  if (!*buf)
    send_to_char("Whom do you wish to teleport?\r\n", ch);
  else if (!(victim = get_char_vis(ch, buf, NULL, FIND_CHAR_WORLD)))
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
  else if (victim == ch)
    send_to_char("Use 'goto' to teleport yourself.\r\n", ch);
  else if (!IS_NPC(victim) && GET_LEVEL(victim) >= GET_LEVEL(ch))
    send_to_char("Maybe you shouldn't do that.\r\n", ch);
  else if (!*buf2)
    send_to_char("Where do you wish to send this person?\r\n", ch);
  else if ((target = find_target_room(ch, buf2)) >= 0)
  {
    new_send_to_char(ch, "%s", CONFIG_OK);
    act("$n disappears in a puff of smoke.", FALSE, victim, 0, 0,  TO_ROOM);
    if (!move_char_to(victim, target))
      return;
    act("$n arrives from a puff of smoke.", FALSE, victim, 0, 0, TO_ROOM);
    act("$n has teleported you!", FALSE, ch, 0, (char *) victim, TO_VICT);
    LOOK(victim);
    entry_memory_mtrigger(victim);
    greet_mtrigger(victim, -1);
    greet_memory_mtrigger(victim);
    enter_wtrigger(IN_ROOM(victim), victim, -1);
  }
}

#define ZOCMD zone_table[zrnum].cmd[subcmd]

void list_zone_commands_room(struct char_data *ch, room_vnum rvnum)
{
  extern struct index_data **trig_index;
  zone_rnum zrnum = real_zone_by_thing(rvnum);
  room_rnum rrnum = real_room(rvnum), cmd_room = NULL;
  int subcmd = 0, count = 0;

  if (zrnum == NOWHERE || rrnum == NULL)
  {
    new_send_to_char(ch, "No zone information available.\r\n");
    return;
  }

  get_char_colors(ch);

  new_send_to_char(ch, "Zone commands in this room:%s\r\n", yel);
  while (ZOCMD.command != 'S')
  {
    switch (ZOCMD.command)
    {
    case 'M':
    case 'O':
    case 'T':
    case 'V':
      cmd_room = world_vnum[ZOCMD.arg3];
      break;
    case 'D':
    case 'R':
      cmd_room = world_vnum[ZOCMD.arg1];
      break;
    default:
      break;
    }
    if (cmd_room == rrnum)
    {
      count++;
      /* start listing */
      switch (ZOCMD.command)
      {
      case 'M':
        new_send_to_char(ch, "%sLoad %s [%s%d%s], Max : %d\r\n",
                         ZOCMD.if_flag ? " then " : "",
                         mob_proto[ZOCMD.arg1].player.short_descr, cyn,
                         mob_index[ZOCMD.arg1].vnum, yel, ZOCMD.arg2
                        );
        break;
      case 'G':
        new_send_to_char(ch, "%sGive it %s [%s%d%s], Max : %d\r\n",
                         ZOCMD.if_flag ? " then " : "",
                         obj_proto[ZOCMD.arg1].short_description,
                         cyn, obj_index[ZOCMD.arg1].vnum, yel,
                         ZOCMD.arg2
                        );
        break;
      case 'O':
        new_send_to_char(ch, "%sLoad %s [%s%d%s], Max : %d\r\n",
                         ZOCMD.if_flag ? " then " : "",
                         obj_proto[ZOCMD.arg1].short_description,
                         cyn, obj_index[ZOCMD.arg1].vnum, yel,
                         ZOCMD.arg2
                        );
        break;
      case 'E':
        new_send_to_char(ch, "%sEquip with %s [%s%d%s], %s, Max : %d\r\n",
                         ZOCMD.if_flag ? " then " : "",
                         obj_proto[ZOCMD.arg1].short_description,
                         cyn, obj_index[ZOCMD.arg1].vnum, yel,
                         equipment_types[ZOCMD.arg3],
                         ZOCMD.arg2
                        );
        break;
      case 'P':
        new_send_to_char(ch, "%sPut %s [%s%d%s] in %s [%s%d%s], Max : %d\r\n",
                         ZOCMD.if_flag ? " then " : "",
                         obj_proto[ZOCMD.arg1].short_description,
                         cyn, obj_index[ZOCMD.arg1].vnum, yel,
                         obj_proto[ZOCMD.arg3].short_description,
                         cyn, obj_index[ZOCMD.arg3].vnum, yel,
                         ZOCMD.arg2
                        );
        break;
      case 'R':
        new_send_to_char(ch, "%sRemove %s [%s%d%s] from room.\r\n",
                         ZOCMD.if_flag ? " then " : "",
                         obj_proto[ZOCMD.arg2].short_description,
                         cyn, obj_index[ZOCMD.arg2].vnum, yel
                        );
        break;
      case 'D':
        new_send_to_char(ch, "%sSet door %s as %s.\r\n",
                         ZOCMD.if_flag ? " then " : "",
                         dirs[ZOCMD.arg2],
                         ZOCMD.arg3 ? ((ZOCMD.arg3 == 1) ? "closed" : "locked") : "open"
                            );
        break;
      case 'T':
        new_send_to_char(ch, "%sAttach trigger %s%s%s [%s%d%s] to %s\r\n",
                         ZOCMD.if_flag ? " then " : "",
                         cyn, trig_index[ZOCMD.arg2]->proto->name, yel,
                         cyn, trig_index[ZOCMD.arg2]->vnum, yel,
                         ((ZOCMD.arg1 == MOB_TRIGGER) ? "mobile" :
                          ((ZOCMD.arg1 == OBJ_TRIGGER) ? "object" :
                           ((ZOCMD.arg1 == WLD_TRIGGER)? "room" : "????"))));
        break;
      case 'V':
        new_send_to_char(ch, "%sAssign global %s:%d to %s = %s\r\n",
                         ZOCMD.if_flag ? " then " : "",
                         ZOCMD.sarg1, ZOCMD.arg2,
                         ((ZOCMD.arg1 == MOB_TRIGGER) ? "mobile" :
                          ((ZOCMD.arg1 == OBJ_TRIGGER) ? "object" :
                           ((ZOCMD.arg1 == WLD_TRIGGER)? "room" : "????"))),
                         ZOCMD.sarg2);
        break;
      default:
        new_send_to_char(ch, "<Unknown Command>\r\n");
        break;
      }
    }
    subcmd++;
  }
  new_send_to_char(ch, nrm);
  if (!count)
    new_send_to_char(ch, "None!\r\n");

}
#undef ZOCMD

ACMD(do_vnum)
{
  char buf[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];
  argument = any_one_arg(argument, buf);
  skip_spaces(&argument);
  strlcpy(buf2, argument, sizeof(buf2));

  if (!*buf || !*buf2
      || (!is_abbrev(buf, "mob") && !is_abbrev(buf, "obj")))
  {
    send_to_char("Usage: vnum { obj | mob } <name>\r\n", ch);
    return;
  }
  if (is_abbrev(buf, "mob"))
    if (!vnum_mobile(buf2, ch))
      send_to_char("No mobiles by that name.\r\n", ch);

  if (is_abbrev(buf, "obj"))
    if (!vnum_object(buf2, ch))
      send_to_char("No objects by that name.\r\n", ch);
}



void do_stat_room(struct char_data *ch)
{
  struct extra_descr_data *desc;
  struct room_data *rm = IN_ROOM(ch);
  int i, found, column;
  struct obj_data *j;
  struct char_data *k;
  char buf2[MAX_INPUT_LENGTH];

  new_mudlog(CMP, (GET_LEVEL(ch) == LVL_IMPL ? GET_LEVEL(ch) : GET_LEVEL(ch)+1), TRUE, "(GC) %s statted room %s [%d].", GET_NAME(ch),  rm->name, rm->number);

  new_send_to_char(ch, "Room name: %s%s%s\r\n", CCCYN(ch, C_NRM),
                   rm->name, CCNRM(ch, C_NRM));

  sprinttype(rm->sector_type, sector_types, buf2, sizeof(buf2));
  new_send_to_char(ch,
                   "VNum: [%s%5d%s], RNum: [%5d], IDNum: %ld, Type: %s\r\n",
                   CCGRN(ch, C_NRM),
                   rm->number, CCNRM(ch, C_NRM), IN_ROOM(ch)->number,(long) rm->number + ROOM_ID_BASE, buf2);
  new_send_to_char(ch, "Dimension: %s\r\n",dimension_types[zone_table[rm->zone].dimension]);
  new_send_to_char(ch, "ZONE: (%-3d) %-30s [%s] \r\n",
                   zone_table[rm->zone].number,
                   zone_table[rm->zone].name,
                   zone_table[rm->zone].builders);
  if (rm->mine.num != -1)
    new_send_to_char(ch, "{cy[MINE: %3d LEVEL: %d Tool: %s]{c0\r\n", rm->mine.num, rm->mine.dif, rm->mine.num == -1 ? "None" : rm->mine.tool == TOOL_SHOVEL ? "Shovel" : "Pickaxe");

  sprintbitarray(rm->room_flags, room_bits, RF_ARRAY_MAX, buf2, sizeof(buf2));
  new_send_to_char(ch, "SpecProc: %s, Flags: %s\r\n",
                   (rm->func == NULL) ? "None" : "Exists", buf2);

  new_send_to_char(ch, "Description:\r\n%s",
                   rm->description ? rm->description : "  None.\r\n");

  if (rm->ex_description)
  {
    new_send_to_char(ch, "Extra descs:%s", CCCYN(ch, C_NRM));
    for (desc = rm->ex_description; desc; desc = desc->next)
      new_send_to_char(ch, " [%s]", desc->keyword);
    new_send_to_char(ch, "%s\r\n", CCNRM(ch, C_NRM));
  }
  if (rm->smell)
    new_send_to_char(ch, "SMELL: %s", rm->smell);
  if (rm->listen)
    new_send_to_char(ch, "LISTEN:%s", rm->listen);

  new_send_to_char(ch, "Chars present:%s", CCYEL(ch, C_NRM));
  column = 14;		/* ^^^ strlen ^^^ */
  for (found = FALSE, k = rm->people; k; k = k->next_in_room)
  {
    if (!CAN_SEE(ch, k))
      continue;

    column +=
      new_send_to_char(ch, "%s %s(%s)", found++ ? "," : "",
                       GET_NAME(k),
                       !IS_NPC(k) ? "PC" : (!IS_MOB(k) ? "NPC" :
                                            "MOB"));
    if (column >= 68)
    {
      new_send_to_char(ch, "%s", k->next_in_room ? "," : "");
      found = FALSE;
      column = 0;
    }
  }
  new_send_to_char(ch, "%s\r\n", CCNRM(ch, C_NRM));

  if (rm->contents)
  {
    new_send_to_char(ch, "Contents:%s", CCGRN(ch, C_NRM));
    column = 9;		/* ^^^ strlen ^^^ */

    for (found = 0, j = rm->contents; j; j = j->next_content)
    {
      if (!CAN_SEE_OBJ(ch, j))
        continue;

      column +=
        new_send_to_char(ch, "%s %s", found++ ? "," : "",j->short_description);
      if (column >= 62)
      {
        new_send_to_char(ch, "%s\r\n", j->next_content ? "," : "");
        found = FALSE;
        column = 0;
      }
    }
    new_send_to_char(ch, "%s\r\n", CCNRM(ch, C_NRM));
  }

  for (i = 0; i < NUM_OF_DIRS; i++)
  {
    char buf1[128];

    if (!rm->dir_option[i])
      continue;

    if (rm->dir_option[i]->to_room == NULL)
      snprintf(buf1, sizeof(buf1), " %sNONE%s", CCCYN(ch, C_NRM), CCNRM(ch, C_NRM));
    else
      snprintf(buf1, sizeof(buf1), "%s%5d%s", CCCYN(ch, C_NRM),
               rm->dir_option[i]->to_room->number, CCNRM(ch, C_NRM));

    new_sprintbit(rm->dir_option[i]->exit_info, exit_bits, buf2, sizeof(buf2));

    new_send_to_char(ch,
                     "Exit %s%-5s%s:  To: [%s], Key: [%5d], Keywrd: %s, Type: %s\r\n%s",
                     CCCYN(ch, C_NRM), dirs[i], CCNRM(ch, C_NRM), buf1,
                     rm->dir_option[i]->key == NOTHING ? -1 : rm->dir_option[i]->key,
                     rm->dir_option[i]->keyword ? rm->dir_option[i]->
                     keyword : "None", buf2,
                     rm->dir_option[i]->general_description ? rm->
                     dir_option[i]->
                     general_description :
                     "  No exit description.\r\n");
  }

  list_zone_commands_room(ch, rm->number);
  /* check the room for a script */
  if (GET_LEVEL(ch) >= LVL_SEN)
    do_sstat_room(ch);
}

const char *balance_display(int balance)
{

  if (balance <= 0 || balance >= 100)
    return "=========================";


  return balances[(balance/4)];
}


void do_stat_object(struct char_data *ch, struct obj_data *j)
{
  int i, found;
  obj_vnum vnum;
  int zone;
  struct obj_data *j2;
  struct extra_descr_data *desc;
  struct char_data *tempch;
  char buf[MAX_INPUT_LENGTH];
  char buf1[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];

  vnum = GET_OBJ_VNUM(j);

  new_mudlog( CMP, (GET_LEVEL(ch) == LVL_IMPL ? GET_LEVEL(ch) : GET_LEVEL(ch)+1), TRUE, "(GC) %s statted obj %s [%d].", GET_NAME(ch),  ((j->short_description) ? j->short_description : "<None>"), vnum);

  new_send_to_char(ch, "Name: '%s%s%s', Aliases: %s\r\n", CCYEL(ch, C_NRM),
                   ((j->short_description) ? j->short_description : "<None>"),
                   CCNRM(ch, C_NRM), j->name);

  sprinttype(GET_OBJ_TYPE(j), item_types, buf1, sizeof(buf1));
  new_send_to_char(ch,
                   "Material: [%s%10s%s] VNum: [%s%5d%s], RNum: [%5d], Type: %s, IDNum: %ld, SpecProc: %s\r\n",
                   CCGRN(ch, C_NRM), material_name(GET_OBJ_MATERIAL(j)),
                   CCNRM(ch, C_NRM), CCGRN(ch, C_NRM), vnum, CCNRM(ch, C_NRM), GET_OBJ_RNUM(j),
                   buf1, GET_ID(j)  ,
                   (GET_OBJ_RNUM(j) != NOTHING ? (obj_index[GET_OBJ_RNUM(j)].func ? "Exists" : "None") : "None"));

  if (GET_OBJ_RNUM(j) != NOTHING && obj_index[GET_OBJ_RNUM(j)].qic != NULL && (GET_LEVEL(ch) >= LVL_SEN))
  {
    new_send_to_char(ch," QIC: %d(%d)\r\n",obj_index[GET_OBJ_RNUM(j)].qic->items ,
                     obj_index[GET_OBJ_RNUM(j)].qic->limit );
  }
  zone = real_zone(GET_OBJ_VNUM(j));
  if (zone > 0)
  {
    new_send_to_char(ch, "Dimension: %s\r\n",dimension_types[zone_table[zone].dimension]);
    new_send_to_char(ch, "ZONE: (%-3d) %-30s [%s]\r\n",
                     zone_table[zone].number, zone_table[zone].name,zone_table[zone].builders);
  }
  if (j->owner != 0)
    new_send_to_char(ch, "Object owned by: {cW%s{c0\r\n", get_name_by_id(j->owner));
  new_send_to_char(ch, "L-Des: %s\r\n",
                   ((j->description) ? j->description : "None"));

  if (j->ex_description)
  {
    new_send_to_char(ch, "Extra descs:%s", CCCYN(ch, C_NRM));
    for (desc = j->ex_description; desc; desc = desc->next)
      new_send_to_char(ch, " [%s]", desc->keyword);

    new_send_to_char(ch, "%s\r\n", CCNRM(ch, C_NRM));
  }
  new_send_to_char(ch, "Can be worn on: ");
  sprintbitarray(j->obj_flags.wear_flags, wear_bits, TW_ARRAY_MAX, buf, sizeof(buf));
  new_send_to_char(ch, "%s\r\n", buf);

  new_send_to_char(ch, "Set char bits : ");
  sprintbitarray(j->obj_flags.bitvector, affected_bits, AF_ARRAY_MAX, buf, sizeof(buf));
  new_send_to_char(ch, "%s\r\n",buf);

  new_send_to_char(ch, "Extra flags   : ");
  sprintbitarray(GET_OBJ_EXTRA(j), extra_bits, EF_ARRAY_MAX, buf, sizeof(buf));
  new_send_to_char(ch, "%s\r\n",buf);

  new_send_to_char(ch, "Weight: %d, Value: %d, Cost/day: %d, Timer: %d, Min level: %d\r\n",
                   GET_OBJ_WEIGHT(j), GET_OBJ_COST(j), GET_OBJ_RENT(j), GET_OBJ_TIMER(j), GET_OBJ_LEVEL(j));

  new_send_to_char(ch, "In room: ");
  if (j->in_room == NULL)
    new_send_to_char(ch, "Nowhere");
  else
    new_send_to_char(ch, "%d", GET_ROOM_VNUM(IN_ROOM(j)));
  /*
   * NOTE: In order to make it this far, we must already be able to see the
   *       character holding the object. Therefore, we do not need CAN_SEE().
   */
  new_send_to_char(ch, ", In object: ");
  new_send_to_char(ch, "%s",  j->in_obj ? j->in_obj->short_description : "None");
  new_send_to_char(ch, ", Carried by: ");
  new_send_to_char(ch, "%s",   j->carried_by ? GET_NAME(j->carried_by) : "Nobody");
  new_send_to_char(ch,", Worn by: ");
  new_send_to_char(ch, "%s\r\n",   j->worn_by ? GET_NAME(j->worn_by) : "Nobody");
  new_send_to_char(ch,", In Locker: ");
  new_send_to_char(ch, "%s\r\n",   j->in_locker ? GET_NAME(j->in_locker) : "Nobody");


  switch (GET_OBJ_TYPE(j))
  {
  case ITEM_LIGHT:
    if (GET_OBJ_VAL(j, 2) == -1)
      new_send_to_char(ch, "Hours left: Infinite");
    else
      new_send_to_char(ch, "Hours left: [%d]", GET_OBJ_VAL(j, 2));
    break;
  case ITEM_SCROLL:
  case ITEM_POTION:
    new_send_to_char(ch, "Spells: (Level %d) %s, %s, %s", GET_OBJ_VAL(j, 0),
                     skill_name(GET_OBJ_VAL(j, 1)),
                     skill_name(GET_OBJ_VAL(j, 2)),
                     skill_name(GET_OBJ_VAL(j, 3)));
    break;
  case ITEM_WAND:
  case ITEM_STAFF:
    new_send_to_char(ch, "Spell: %s at level %d, %d (of %d) charges remaining",
                     skill_name(GET_OBJ_VAL(j, 3)), GET_OBJ_VAL(j, 0),
                     GET_OBJ_VAL(j, 2), GET_OBJ_VAL(j, 1));
    break;
  case ITEM_THROW:
  case ITEM_ROCK:
  case ITEM_BOLT:
  case ITEM_ARROW:
  case ITEM_AMMO:
    new_send_to_char(ch, "Number dam dice: %d Size dam dice: %d",
                     (int) GET_OBJ_VAL(j, 1), (int) GET_OBJ_VAL(j, 2));
    break;
  case ITEM_GRENADE:
    new_send_to_char(ch, "Timer: %d Num dam dice: %d Size dam dice: %d",
                     (int) GET_OBJ_VAL(j, 0), (int) GET_OBJ_VAL(j, 1),
                     (int) GET_OBJ_VAL(j, 2));
    break;
  case ITEM_BOW:
  case ITEM_CROSSBOW:
  case ITEM_SLING:
  case ITEM_GUN:
    new_send_to_char(ch, "Range    : %d  Max Ammo : %d  Cur Ammo : %d, vnum of ammo: %d",
                     (int) GET_OBJ_VAL(j, 0), (int) GET_OBJ_VAL(j, 1),
                     (int) GET_OBJ_VAL(j, 2), (int) GET_OBJ_VAL(j, 3));
    break;
  case ITEM_WEAPON:
    new_send_to_char(ch, "VAR: %d Todam: %dd%d, Message type: %d, Length: %dcm\r\n"
                     "Balance: (%d) %s\r\n",
                     (int) GET_OBJ_VAL(j, 0),(int) GET_OBJ_VAL(j, 1), (int) GET_OBJ_VAL(j, 2),
                     (int) GET_OBJ_VAL(j, 3), (int) GET_WEP_LENGTH(j), (int) GET_WEP_BALANCE(j),
                     balance_display(GET_WEP_BALANCE(j)));
    new_send_to_char(ch, "This balance gives the weapon %d speed, %d accuracy and %d evasion.\r\n",
                     get_weapon_speed(j),
                     get_weapon_accuracy(j),
                     get_weapon_evasion(j));
    break;
  case ITEM_ARMOR:
    new_send_to_char(ch, "AC-apply: [%d]", (int) GET_OBJ_VAL(j, 0));
    break;
  case ITEM_TRAP:
    new_send_to_char(ch, "Spell: %d, - Hitpoints: %d - IS SET:%s",
                     (int) GET_OBJ_VAL(j, 0), (int) GET_OBJ_VAL(j, 1), YESNO(TRAP_IS_SET(j)));
    break;
  case ITEM_CONTAINER:
    sprintbit((int) GET_OBJ_VAL(j, 1), container_bits, buf2, sizeof(buf2));
    new_send_to_char(ch,
                     "Weight capacity: %d, Lock Type: %s, Key Num: %d, Corpse: %s",
                     (int) GET_OBJ_VAL(j, 0), buf2, (int) GET_OBJ_VAL(j, 2),
                     YESNO((int) GET_OBJ_VAL(j, 3)));
    break;
  case ITEM_DRINKCON:
  case ITEM_FOUNTAIN:
    sprinttype((int) GET_OBJ_VAL(j, 2), drinks, buf2, sizeof(buf2));
    new_send_to_char(ch,
                     "Capacity: %d, Contains: %d, Poisoned: %s, Liquid: %s, Casts: %s",
                     (int) GET_OBJ_VAL(j, 0), (int) GET_OBJ_VAL(j, 1),
                     YESNO((int) GET_OBJ_VAL(j, 3)), buf2, skill_name(GET_OBJ_VAL(j, 4)));
    break;
  case ITEM_NOTE:
    new_send_to_char(ch, "Tongue: %d", (int) GET_OBJ_VAL(j, 0));
    break;
  case ITEM_KEY:
    break;
  case ITEM_FOOD:
    new_send_to_char(ch, "Makes full: %d, Poisoned: %s, Casts: %s",
                     (int) GET_OBJ_VAL(j, 0), YESNO((int) GET_OBJ_VAL(j, 3)), skill_name(GET_OBJ_VAL(j, 4)));
    break;
  case ITEM_MONEY:
    new_send_to_char(ch, "Coins: %d", GET_OBJ_VAL(j, 0));
    break;
  case ITEM_FURNITURE:
    new_send_to_char(ch, "Can hold: [%d] Num. of People in Chair: [%d]\r\n",
                     GET_OBJ_VAL(j, 0), GET_OBJ_VAL(j, 1));
    new_send_to_char(ch, "Holding : ");
    for (tempch = OBJ_SAT_IN_BY(j); tempch;
         tempch = NEXT_SITTING(tempch))
    {
      new_send_to_char(ch, "%s ", GET_NAME(tempch));
    }
    break;
  case ITEM_SPACEBIKE:
    new_send_to_char(ch, "FUEL: %d MAX: %d SITTING IN BIKE: %s\r\n",
                     GET_FUEL(j), GET_MAX_FUEL(j), OBJ_SAT_IN_BY(j) ? GET_NAME(OBJ_SAT_IN_BY(j)) : "Nobody");
    break;
  case ITEM_LIGHTSABRE_HILT:
    new_send_to_char(ch, "Num Sabers: %d Number dam dice: %d Size dam dice: %d\r\nSaber Color: %s ",
                     (int) GET_OBJ_VAL(j, 0),(int) GET_OBJ_VAL(j, 1), (int) GET_OBJ_VAL(j, 2),
                     color_option_name((int) GET_OBJ_VAL(j, 3)));
    break;

  default:
    new_send_to_char(ch, "Values 0-3: [%d] [%d] [%d] [%d]",
                     GET_OBJ_VAL(j, 0), GET_OBJ_VAL(j, 1),
                     GET_OBJ_VAL(j, 2), GET_OBJ_VAL(j, 3));
    break;
  }
  new_send_to_char(ch, "\r\n");

  /*
   * I deleted the "equipment status" code from here because it seemed
   * more or less useless and just takes up valuable screen space.
   */

  if (j->contains)
  {
    new_send_to_char(ch, "\r\nContents:%s", CCGRN(ch, C_NRM));
    for (found = 0, j2 = j->contains; j2; j2 = j2->next_content)
    {
      new_send_to_char(ch, "%s %s", found++ ? "," : "",
                       j2->short_description);
      if (strlen(buf) >= 62)
      {
        if (j2->next_content)
          new_send_to_char(ch,",\r\n");
        else
          new_send_to_char(ch, "\r\n");
      }
    }

    if (found)
      new_send_to_char(ch,"\r\n%s", CCNRM(ch, C_NRM));
  }
  found = 0;
  new_send_to_char(ch,"Affections:");
  for (i = 0; i < MAX_OBJ_AFFECT; i++)
    if (j->affected[i].modifier)
    {
      sprinttype(j->affected[i].location, apply_types, buf2, sizeof(buf2));
      new_send_to_char(ch, "%s %+d to %s", found++ ? "," : "",
                       j->affected[i].modifier, buf2);
    }
  if (!found)
    new_send_to_char(ch," None");

  new_send_to_char(ch,"\r\n");

  list_destinations(TRAVEL_LIST(j), ch);

  /* check the object for a script */
  if (GET_LEVEL(ch) >= LVL_SEN /* || is_name(GET_NAME(ch), zone_table[(real_zone(vnum))].builders)*/)
    do_sstat_object(ch, j);
}


void do_stat_character(struct char_data *ch, struct char_data *k)
{
  int i, i2, found = 0;
  struct obj_data *j;
  struct follow_type *fol;
  struct affected_type *aff;
  int speed_update(struct char_data *ch);
  char *clan_name(int idnum);
  char buf[MAX_INPUT_LENGTH];
  char buf1[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];
  int len = 0;
  int zone;
  char masteries[40] = "";
  size_t masteries_len = 0;

  new_mudlog(CMP, (GET_LEVEL(ch) == LVL_IMPL ? GET_LEVEL(ch) : GET_LEVEL(ch)+1), TRUE, "(GC) %s statted %s [%d].", GET_NAME(ch),  GET_NAME(k), GET_MOB_VNUM(k));

  if (!show_vars)
  {
    sprinttype(GET_SEX(k), genders, buf, sizeof(buf));
    new_send_to_char(ch, "Sex: %s  (%s) '%s'  IDNum: [%5ld], In room [%5d] \r\n",
                     buf, (!IS_NPC(k) ? "PC" : (!IS_MOB(k) ? "NPC" : "MOB")),
                     GET_NAME(k),IS_NPC(k) ? GET_ID(k) : GET_IDNUM(k), GET_ROOM_VNUM(IN_ROOM(k)));
    if (IS_MOB(k))
      new_send_to_char(ch, "Alias: %s, VNum: [%5d], RNum: [%5d]\r\n",
                       k->player.name, GET_MOB_VNUM(k), GET_MOB_RNUM(k));


    if (!IS_MOB(k))
      new_send_to_char(ch, "Title: %s\r\n",
                       (k->player.title ? k->player.title : "<None>"));



    if (IS_MOB(k))
    {

      new_send_to_char(ch, "L-Des: %s",
                       (k->player.long_descr ? k->player.long_descr : "<None>\r\n"));
      zone = real_zone(GET_MOB_VNUM(k));
      if (zone > 0)
      {
        new_send_to_char(ch, "Dimension: %s\r\n",dimension_types[zone_table[zone].dimension]);
        new_send_to_char(ch, "ZONE: (%-3d) %-30s [%s] \r\n",
                         zone_table[zone].number, zone_table[zone].name,zone_table[zone].builders);
      }
    }


    new_send_to_char(ch, "Class: %s", simple_class_name(k));
    if (IS_NPC(k))
      new_send_to_char(ch, " (Tier: %d)",  MOB_TIER(k));
    else
      new_send_to_char(ch, " (Tier: %d)", current_class_is_tier_num(k));


    new_send_to_char(ch, ", Race: %s\r\n", race_name(k));



    new_send_to_char(ch, "Lev: [%s%2d%s], XP: [%s%15lld%s], Align: [%4d], Speed: [%5d]\r\n",
                     CCYEL(ch, C_NRM), GET_LEVEL(k), CCNRM(ch, C_NRM),
                     CCYEL(ch, C_NRM), GET_EXP(k), CCNRM(ch, C_NRM),
                     GET_ALIGNMENT(k), speed_update(k));

    if (!IS_NPC(k))
    {
#if defined(HAVE_ZLIB)
      if (k->desc && k->desc->comp)
      {
        if (k->desc->comp->state >= 2)
          new_send_to_char(ch, "Compression: Enabled    ");
        else
          new_send_to_char(ch, "Compression: Disabled    ");
      }
#endif
      if (k->desc)
      {
        new_send_to_char(ch, "Telopt Prompts: %d    ", k->desc->eor);
        new_send_to_char(ch, "MXP: %d    ", k->desc->mxp);

        new_send_to_char(ch, "Wordwrap: %s - %d\r\n", ONOFF(PRF_FLAGGED(k->desc->character, PRF_PAGEWRAP)), PAGEWIDTH(k->desc->character));
      }
      new_send_to_char(ch, "Total Remorts: %d", REMORTS(k));
      if (GET_REMORT(k) >= 0)
      {
        new_send_to_char(ch, "  Remort: %s", pc_class_types[(int)GET_REMORT(k)]);
      }

      if (GET_REMORT_TWO(k) >= 0)
      {
        new_send_to_char(ch, "  Remort2: %s",pc_class_types[(int)GET_REMORT_TWO(k)]);
      }

      if (GET_REMORT_THREE(k) >= 0)
      {
        new_send_to_char(ch, "  Remort3: %s",pc_class_types[(int)GET_REMORT_THREE(k)]);
      }
      new_send_to_char(ch, "\r\n");

      for (i = 0; i < NUM_CLASSES; i++)
        if (GET_MASTERY(k, i))
          masteries_len += snprintf(masteries+masteries_len, sizeof(masteries) - masteries_len, "%c ", UPPER(*pc_class_types[i]));
      new_send_to_char(ch, "MASTERED CLASSES: %s\r\n", (strcmp(masteries, "") == 0 ? "none" : masteries));

      if (GET_EMAIL(k) && *GET_EMAIL(k))
        new_send_to_char(ch, "EMAIL: %s\r\n", GET_EMAIL(k));
      if (GET_NEWBIE_STATUS(k) != -1)
        new_send_to_char(ch, "NEWBIE STATUS: %s\r\n", newbie_status[GET_NEWBIE_STATUS(k)]);

      new_send_to_char(ch, "Clan %d(%d) Rank: %d - %s\r\n",
                       GET_CLAN(k), find_clan_by_id(GET_CLAN(k)), GET_CLAN_RANK(k),
                       clan_name(find_clan_by_id(GET_CLAN(k))));

      new_send_to_char(ch, "TOKENS: G:%-2d S:%-2d BN:%-2d BS:%-2d      AWARD POINTS: %-3d      TRADE POINTS: %-3d\r\n",
                       GET_GOLD_TOKEN_COUNT(k), GET_SILVER_TOKEN_COUNT(k),GET_BRONZE_TOKEN_COUNT(k), GET_BRASS_TOKEN_COUNT(k), update_award(k), TRADEPOINTS(k));


      strcpy(buf1, (char *) asctime(localtime(&(k->player.time.birth))));
      strcpy(buf2, (char *) asctime(localtime(&(k->player.time.logon))));
      buf1[10] = buf2[10] = '\0';

      new_send_to_char(ch,
                       "Created: [%s], Last Logon: [%s], Played [%dh %dm], Age [%d]\r\n",
                       buf1, buf2, k->player.time.played / 3600,
                       ((k->player.time.played % 3600) / 60), age(k)->year);

      new_send_to_char(ch,
                       "Pracs: %d, Int_Learn: %d, Wis_bonus: %d\r\n",
                       GET_PRACTICES(k),
                       int_app[GET_INT(k)].learn, wis_app[GET_WIS(k)].bonus);

      /*. Display OLC zone for immorts .*/
      if (GET_LEVEL(k) >= LVL_BUILDER)
      {
        if (GET_OLC_ZONE(k)==AEDIT_PERMISSION)
          new_send_to_char(ch, ", OLC[%sActions%s]", CCCYN(ch, C_NRM), CCNRM(ch, C_NRM));
        else if (GET_OLC_ZONE(k)==NOWHERE)
          new_send_to_char(ch, ", OLC[%sOFF%s]", CCCYN(ch, C_NRM), CCNRM(ch, C_NRM));
        else
          new_send_to_char(ch, ", OLC[%s%d%s]", CCCYN(ch, C_NRM), GET_OLC_ZONE(k), CCNRM(ch, C_NRM));
      }
      new_send_to_char(ch, "\r\n");
    }
    else
    {
      if (k->mob_specials.head_join != NULL)
      {
        new_send_to_char(ch, "[{cWCombined Mob Segment of %s{c0]\r\n", GET_NAME(k->mob_specials.head_join));
      }
      else
      {
        struct combine_data *joinpt = k->mob_specials.join_list;
        if (joinpt != NULL)
        {
          new_send_to_char(ch, "[{cWCombined Mob Head{c0]\r\nVnums of Segments: ");
          while (joinpt)
          {
            new_send_to_char(ch, "%d ", joinpt->vnum);
            joinpt = joinpt->next;
          }
          new_send_to_char(ch, "\r\n");
        }
      }
    }
    new_send_to_char(ch,"Str: [%s%d/%d%s]  Int: [%s%d%s]  Wis: [%s%d%s]  "
                     "Dex: [%s%d%s]  Con: [%s%d%s]  Cha: [%s%d%s]\r\n",
                     CCCYN(ch, C_NRM), GET_STR(k), GET_ADD(k), CCNRM(ch, C_NRM),
                     CCCYN(ch, C_NRM), GET_INT(k), CCNRM(ch, C_NRM),
                     CCCYN(ch, C_NRM), GET_WIS(k), CCNRM(ch, C_NRM),
                     CCCYN(ch, C_NRM), GET_DEX(k), CCNRM(ch, C_NRM),
                     CCCYN(ch, C_NRM), GET_CON(k), CCNRM(ch, C_NRM),
                     CCCYN(ch, C_NRM), GET_CHA(k), CCNRM(ch, C_NRM));

    new_send_to_char(ch,
                     "Hit p.:[%s%d/%d+%d%s]  Mana p.:[%s%d/%d+%d%s]  Move p.:[%s%d/%d+%d%s] Stam.p[%s%d/%d+%d%s]\r\n",
                     CCGRN(ch, C_NRM), GET_HIT(k), GET_MAX_HIT(k), hit_gain(k),
                     CCNRM(ch, C_NRM), CCGRN(ch, C_NRM), GET_MANA(k),
                     GET_MAX_MANA(k), mana_gain(k), CCNRM(ch, C_NRM), CCGRN(ch,   C_NRM),
                     GET_MOVE(k), GET_MAX_MOVE(k), move_gain(k), CCNRM(ch, C_NRM),
                     CCGRN(ch, C_NRM), GET_STAMINA(k), GET_MAX_STAMINA(k), stamina_gain(k), CCNRM(ch, C_NRM));

    new_send_to_char(ch, "Coins: [%9lld], Bank: [%9lld] (Total: %lld)\r\n",
                     char_gold(k, 0, GOLD_HAND), char_gold(k, 0, GOLD_BANK), char_gold(k, 0, GOLD_ALL));

    new_send_to_char(ch,
                     "AC: [%d%+d/10], Hitroll: [%3d], Damroll: [%3d], Saving throws: [%d/%d/%d/%d/%d]\r\n",
                     GET_AC(k), dex_app[GET_DEX(k)].defensive, k->points.hitroll,
                     k->points.damroll, GET_SAVE(k, 0), GET_SAVE(k, 1), GET_SAVE(k, 2),
                     GET_SAVE(k, 3), GET_SAVE(k, 4));

    sprinttype(GET_POS(k), position_types, buf2, sizeof(buf2));
    new_send_to_char(ch, "Pos: %s, Fighting: %s", buf2,
                     (FIGHTING(k) ? GET_NAME(FIGHTING(k)) : "Nobody"));

    if (IS_NPC(k))
      new_send_to_char(ch, ", Attack type: %s", attack_hit_text[k->mob_specials.attack_type].singular);

    if (k->desc)
    {
      sprinttype(STATE(k->desc), connected_types, buf2, sizeof(buf2));
      new_send_to_char(ch, ", Connected: %s\r\n", buf2);
    }
    else
      new_send_to_char(ch, "\r\n");

    new_send_to_char(ch, "Default position: ");
    sprinttype((k->mob_specials.default_pos), position_types, buf2, sizeof(buf2));
    new_send_to_char(ch, "%s", buf2);

    new_send_to_char(ch, ", Idle Timer (in tics) [%d]\r\n",
                     k->char_specials.timer);

    if (IS_NPC(k))
    {
      sprintbitarray(MOB_FLAGS(k), action_bits, PM_ARRAY_MAX, buf2, sizeof(buf2));
      new_send_to_char(ch, "NPC flags: %s%s%s\r\n", CCCYN(ch, C_NRM), buf2,
                       CCNRM(ch, C_NRM));
    }
    else
    {
      sprintbitarray(PLR_FLAGS(k), player_bits, PM_ARRAY_MAX, buf2, sizeof(buf2));
      new_send_to_char(ch, "PLR: %s%s%s\r\n", CCCYN(ch, C_NRM), buf2, CCNRM(ch, C_NRM));
      sprintbitarray(PRF_FLAGS(k), preference_bits, PM_ARRAY_MAX, buf2, sizeof(buf2));
      new_send_to_char(ch, "PRF: %s%s%s\r\n", CCGRN(ch, C_NRM), buf2, CCNRM(ch, C_NRM));


      if ((GET_LEVEL(k) >= LVL_GOD) && (GET_LEVEL(k) <= LVL_IMPL))
      {

        sprintbit(CMD_FLAGS(k), wiz_groups, buf2, sizeof(buf2));
        new_send_to_char(ch, "WIZ-SAVE: %s%s%s\r\n", CCYEL(ch, C_NRM), buf2,
                         CCNRM(ch, C_NRM));

        sprintbit(CMD_FLAGS2(k), wiz_groups, buf2, sizeof(buf2));
        new_send_to_char(ch, "WIZ-NOSAVE: %s%s%s\r\n", CCYEL(ch, C_NRM), buf2,
                         CCNRM(ch, C_NRM));
      }
    }
    if (!IS_NPC(k) && PREG(k) > 0)
      new_send_to_char(ch, "%d hours away from giving birth.\r\n", PREG(k));
    if (IS_MOB(k))
      new_send_to_char(ch, "Mob Spec-Proc: %s, NPC Bare Hand Dam: %dd%d\r\n",
                       (mob_index[GET_MOB_RNUM(k)].func ? "Exists" : "None"),
                       k->mob_specials.damnodice, k->mob_specials.damsizedice);

    new_send_to_char(ch, "Carried: weight: %d, items: %d; ",   IS_CARRYING_W(k), IS_CARRYING_N(k));

    for (i = 0, j = k->carrying; j; j = j->next_content, i++);
    new_send_to_char(ch, "Items in: inventory: %d, ", i);

    for (i = 0, i2 = 0; i < NUM_WEARS; i++)
      if (HAS_BODY(k, i) && GET_EQ(k, i))
        i2++;
    new_send_to_char(ch, "eq: %d\r\n", i2);

    if (!IS_NPC(k))
      new_send_to_char(ch, "Hunger: %d, Thirst: %d, Drunk: %d\r\n",
                       GET_COND(k, FULL), GET_COND(k, THIRST), GET_COND(k,DRUNK));

    if (!IS_NPC(k))
      new_send_to_char(ch, "Rip: [%d], Kills: [%d], Death Traps: [%d]\r\n", GET_RIP_CNT(k), GET_KILL_CNT(k), GET_DT_CNT(k));

    new_send_to_char(ch, "Master is: %s, Followers are:",
                     ((k->master) ? GET_NAME(k->master) : "<none>"));


    for (fol = k->followers; fol; fol = fol->next)
    {
      len += new_send_to_char(ch, "%s %s", found++ ? "," : "",PERS(fol->follower, ch));
      if (len >= 62)
      {
        if (fol->next)
          len += new_send_to_char(ch,",\r\n");
        else
          len += new_send_to_char(ch,"\r\n");
        len = 0;
      }
    }

    new_send_to_char(ch, "\r\n");

    /* Showing the bitvector */
    sprintbitarray(AFF_FLAGS(k), affected_bits, AF_ARRAY_MAX, buf2, sizeof(buf2));
    new_send_to_char(ch, "AFF: %s%s%s\r\n", CCYEL(ch, C_NRM), buf2,
                     CCNRM(ch, C_NRM));

    /* Routine to show what spells a char is affected by */
    if (k->affected)
    {
      for (aff = k->affected; aff; aff = aff->next)
      {
        *buf2 = '\0';
        if (aff->expire == -2)
          new_send_to_char(ch, "SPL: ( innate) %s%-21s%s ", CCCYN(ch, C_NRM),
                           skill_name(aff->type), CCNRM(ch, C_NRM));
        else
          new_send_to_char(ch, "SPL: (%4ldsec)  %s%-21s%s ", time_to_sec(aff->expire + 1),
                           CCCYN(ch, C_NRM), skill_name(aff->type), CCNRM(ch, C_NRM));
        if (aff->modifier)
          new_send_to_char(ch, "%+d to %s", aff->modifier, apply_types[(int) aff->location]);

        if (aff->bitvector)
        {
          if (aff->modifier)
            new_send_to_char(ch, ", sets ");
          else
            new_send_to_char(ch, "sets ");
          new_send_to_char(ch,"%s", affected_bits[aff->bitvector]);
        }
        new_send_to_char(ch, "\r\n");
      }
    }
    list_destinations(TRAVEL_LIST(k), ch);
    new_send_to_char(ch, "To see global variables: type\r\nvstat player <name>\r\n");
    if (GET_LEVEL(ch) == LVL_IMPL && k->desc)
    {
      int i, cnt = 0;
      new_send_to_char(ch, "Last commands typed, oldest at top, newest at bottom:\r\n");
      for (i = 0; i < HISTORY_SIZE;i++)
      {
        if (k->desc->history[i])
          new_send_to_char(ch, "{cY%d:{cg %s{c0\r\n", cnt++, k->desc->history[i]);

      }
    }
  }

  /* check mobiles for a script */
  if (!show_vars && IS_NPC(k) &&( GET_LEVEL(ch) >= LVL_SEN ))
  {
    do_sstat_character(ch, k);
    if (SCRIPT_MEM(k))
    {
      struct script_memory *mem = SCRIPT_MEM(k);
      new_send_to_char(ch, "Script memory:\r\n  Remember             Command\r\n");
      while (mem)
      {
        struct char_data *mc = find_char(mem->id);
        if (!mc)
          new_send_to_char(ch,"  ** Corrupted!\r\n");
        else
        {
          if (mem->cmd)
            new_send_to_char(ch, "  %-20.20s%s\r\n", GET_NAME(mc), mem->cmd);
          else
            new_send_to_char(ch, "  %-20.20s <default>\r\n", GET_NAME(mc));
        }
        mem = mem->next;
      }
    }
  }
  else
  {
    /* this is a PC, display their global variables */
    if (k->script && k->script->global_vars && show_vars)
    {
      struct trig_var_data *tv;
      char uname[MAX_INPUT_LENGTH];
      char buffer[MAX_INPUT_LENGTH];
          DYN_DEFINE;
          DYN_CREATE;
          *dynbuf=0;
      snprintf(buffer,MAX_INPUT_LENGTH,"Global Variables for %s:\r\n", GET_NAME(k));
      DYN_RESIZE(buffer);
//        new_send_to_char(ch, "Global Variables for %s:\r\n", GET_NAME(k));

      /* currently, variable context for players is always 0, so it is */
      /* not displayed here. in the future, this might change */

      for (tv = k->script->global_vars; tv; tv = tv->next)
      {
        if (*(tv->value) == UID_CHAR)
        {
          find_uid_name(tv->value, uname, sizeof(uname));
          snprintf(buffer,MAX_INPUT_LENGTH,"    %40s:  [UID]: %20s\r\n", tv->name,
                           uname);
          DYN_RESIZE(buffer);
//            new_send_to_char(ch, "    %40s:  [UID]: %20s\r\n", tv->name,
//                             uname);
        }
        else {
         snprintf(buffer,MAX_INPUT_LENGTH,"    %40s:  %20s\r\n", tv->name,
                          tv->value);
         DYN_RESIZE(buffer);
//           new_send_to_char(ch, "    %40s:  %20s\r\n", tv->name,
//                            tv->value);
        }
      }
      page_string(ch->desc, dynbuf, DYN_BUFFER);
    }
  }

}


ACMD(do_stat)
{
  struct char_data *victim;
  struct obj_data *object;
  char buf1[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];
  //int tmp;

  half_chop(argument, buf1, buf2);

  if (!*buf1)
  {
    new_send_to_char(ch,"Stats on who or what?\r\n");
    return;

  }
  else if (is_abbrev(buf1, "room"))
  {
    do_stat_room(ch);
  }
  else if (is_abbrev(buf1, "mob"))
  {
    if (!*buf2)
    {
      new_send_to_char(ch,"Stats on which mobile?\r\n");
      return;
    }
    else
    {
      if ((victim =
             get_char_vis(ch, buf2, NULL, FIND_CHAR_WORLD)) != NULL)
        do_stat_character(ch, victim);
      else
      {
        new_send_to_char(ch,"No such mobile around.\r\n");
        return;
      }
    }
  }
  else if (is_abbrev(buf1, "player"))
  {
    if (!*buf2)
    {
      new_send_to_char(ch,"Stats on which player?\r\n");
      return;
    }
    else
    {
      if ((victim =
             get_player_vis(ch, buf2, NULL, FIND_CHAR_WORLD)) != NULL)
        do_stat_character(ch, victim);
      else
      {
        new_send_to_char(ch,"No such player around.\r\n");
        return;
      }
    }
  }
  else if (is_abbrev(buf1, "file"))
  {
    if (!*buf2)
    {
      new_send_to_char(ch,"Stats on which player?\r\n");
      return;
    }
    else if (!get_id_by_name(buf1))
    {
      send_to_char("There is no such player.\r\n", ch);
      return;
    }
    else
    {
      CREATE(victim, struct char_data, 1);
      clear_char(victim);
      TEMP_LOAD_CHAR = TRUE;
      if (store_to_char(buf2, victim) > -1)
      {
        if (GET_LEVEL(victim) > GET_LEVEL(ch))
        {
          new_send_to_char(ch,"Sorry, you can't do that.\r\n");
          free_char(victim);
          TEMP_LOAD_CHAR = FALSE;
          return;
        }
        else
          do_stat_character(ch, victim);
        free_char(victim);
      }
      else
      {
        new_send_to_char(ch,"There is no such player.\r\n");
        free(victim);
        TEMP_LOAD_CHAR = FALSE;
        return;
      }
      TEMP_LOAD_CHAR = FALSE;
    }
  }
  else if (is_abbrev(buf1, "object"))
  {
    if (!*buf2)
    {
      new_send_to_char(ch,"Stats on which object?\r\n");
      return;
    }
    else
    {
      if ((object = get_obj_vis(ch, buf2, NULL)) != NULL)
        do_stat_object(ch, object);
      else
      {
        new_send_to_char(ch,"No such object around.\r\n");
        return;
      }
    }
  }
  else if (is_abbrev(buf1, "zone"))
  {
    if (!*buf2)
    {
      new_send_to_char(ch, "Stats on which zone?\r\n");
      return;
    }
    else
    {
      zone_rnum rn = real_zone(atoi(buf2));
      print_zone(ch, rn);
      return;
    }
  }
  else
  {
    char *name = buf1;
    int number = get_number(&name);

    if ((object =
           get_obj_in_equip_vis(ch, name, &number,
                                ch->equipment)) != NULL)
      do_stat_object(ch, object);
    else if ((object =
                get_obj_in_list_vis(ch, name, &number,
                                    ch->carrying)) != NULL)
      do_stat_object(ch, object);
    else if ((victim =
                get_char_vis(ch, name, &number, FIND_CHAR_ROOM)) != NULL)
      do_stat_character(ch, victim);
    else if ((object =
                get_obj_in_list_vis(ch, name, &number,
                                    IN_ROOM(ch)->contents)) !=
             NULL)
      do_stat_object(ch, object);
    else if ((victim =
                get_char_vis(ch, name, &number,
                             FIND_CHAR_WORLD)) != NULL)
      do_stat_character(ch, victim);
    else if ((object = get_obj_vis(ch, name, &number)) != NULL)
      do_stat_object(ch, object);
    else
    {
      new_send_to_char(ch, "Nothing around by that name.\r\n");
      return;
    }
  }

}

ACMD(do_account)
{
  char name[MAX_INPUT_LENGTH];
  int acc, pos, i = 0;
  int find_name(char *name);
  extern struct player_index_element *player_table;
  one_argument(argument, name);
  if (!*name || GET_LEVEL(ch) < LVL_IMMORT)
  {
    if ((pos = find_name(GET_NAME(ch))) == -1)
      return;
  }
  else
  {
    if ((pos = find_name(name)) == -1)
    {
      new_send_to_char(ch, "No one by that name found.\r\n" );
      return;
    }
  }
  acc = player_table[pos].account;

  for (pos = 0; pos <= top_of_p_table; pos++)
  {
    if (!IS_SET(player_table[pos].flags, PINDEX_DELETED) &&
        !IS_SET(player_table[pos].flags, PINDEX_SELFDELETE) &&
        player_table[pos].name[0] != '\0' && player_table[pos].account == acc)
    {
      new_send_to_char(ch, "{cy%2d{cg) %-15s%s{c0",
                       i, player_table[pos].name, ((i+1)%2) == 0 ? "\r\n" : "   ");
      i++;
    }
  }


}

ACMD(do_shutdown)
{
  char arg[MAX_INPUT_LENGTH];
  DESCRIPTOR_DATA *d;
  if (subcmd != SCMD_SHUTDOWN)
  {
    send_to_char("If you want to shut something down, say so!\r\n",
                 ch);
    return;
  }
  one_argument(argument, arg);

  //saving all zones
  save_all();

  if (!*arg)
  {
    log("(GC) Shutdown by %s.", GET_NAME(ch));
    send_to_all("Shutting down.\r\n");
    circle_shutdown = 1;
  }
  else if (!str_cmp(arg, "reboot"))
  {
    log("(GC) Reboot by %s.", GET_NAME(ch));
    send_to_all("Rebooting.. come back in a minute or two.\r\n");
    touch(FASTBOOT_FILE);
    circle_shutdown = circle_reboot = 1;
  }
  else if (!str_cmp(arg, "now"))
  {
    log( "(GC) Shutdown NOW by %s.", GET_NAME(ch));
    send_to_all("Rebooting.. come back in a minute or two.\r\n");
    circle_shutdown = 1;
    circle_reboot = 2;
  }
  else if (!str_cmp(arg, "die"))
  {
    log("(GC) Shutdown by %s.", GET_NAME(ch));
    send_to_all("Shutting down for maintenance.\r\n");
    touch(KILLSCRIPT_FILE);
    circle_shutdown = 1;
  }
  else if (!str_cmp(arg, "pause"))
  {
    log("(GC) Shutdown by %s.", GET_NAME(ch));
    send_to_all("Shutting down for maintenance.\r\n");
    touch(PAUSE_FILE);
    circle_shutdown = 1;
  }
  else
    send_to_char("Unknown shutdown option.\r\n", ch);

  for (d = descriptor_list; d; d = d->next)
  {
    if (d->character)
      save_char(d->character);
  }
  Crash_save_all();

}


void stop_snooping(struct char_data *ch)
{
  if (!ch->desc->snooping)
    send_to_char("You aren't snooping anyone.\r\n", ch);
  else
  {
    send_to_char("You stop snooping.\r\n", ch);
    ch->desc->snooping->snoop_by = NULL;
    ch->desc->snooping = NULL;
  }
}


ACMD(do_snoop)
{
  struct char_data *victim, *tch;
  char arg[MAX_INPUT_LENGTH];


  if (!ch->desc)
    return;

  one_argument(argument, arg);

  if (!*arg)
    stop_snooping(ch);
  else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
    new_send_to_char(ch,"No such person around.\r\n");
  else if (!victim->desc)
    new_send_to_char(ch,"There's no link.. nothing to snoop.\r\n");
  else if (victim == ch)
    stop_snooping(ch);
  else if (victim->desc->snoop_by)
    new_send_to_char(ch,"Busy already. \r\n");
  else if (victim->desc->snooping == ch->desc)
    new_send_to_char(ch,"Don't be stupid.\r\n");
  else
  {
    if (victim->desc->original)
      tch = victim->desc->original;
    else
      tch = victim;

    if ((GET_ORIG_LEV(tch) ? GET_ORIG_LEV(tch): GET_LEVEL(tch)) >= GET_LEVEL(ch))
    {
      if (GET_ORIG_LEV(tch))
        new_send_to_char(tch, "%s tried to snoop you but couldn't.\r\n", GET_NAME(ch));
      new_send_to_char(ch,"You can't.\r\n");
      return;
    }
    new_send_to_char(ch, "%s", CONFIG_OK);

    if (ch->desc->snooping)
      ch->desc->snooping->snoop_by = NULL;

    ch->desc->snooping = victim->desc;
    victim->desc->snoop_by = ch->desc;
  }
}



ACMD(do_switch)
{
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (ch->desc->original)
    new_send_to_char(ch,"You're already switched.\r\n");
  else if (!*arg)
    new_send_to_char(ch,"Switch with who?\r\n");
  else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
    new_send_to_char(ch,"No such character.\r\n");
  else if (ch == victim)
    new_send_to_char(ch,"Hee hee... we are jolly funny today, eh?\r\n");
  else if (victim->desc)
    new_send_to_char(ch,"You can't do that, the body is already in use!\r\n");
  else if ((GET_LEVEL(ch) < LVL_IMPL) && !IS_NPC(victim))
    new_send_to_char(ch,"You aren't holy enough to use a mortal's body.\r\n");
  else if (GET_LEVEL(ch) < LVL_GRGOD
           && ROOM_FLAGGED(IN_ROOM(victim), ROOM_GODROOM))
    new_send_to_char(ch,"You are not godly enough to use that room!\r\n");
  else if (GET_LEVEL(ch) < LVL_GRGOD
           && ROOM_FLAGGED(IN_ROOM(victim), ROOM_HOUSE)
           && !House_can_enter(ch, GET_ROOM_VNUM(IN_ROOM(victim))))
    new_send_to_char(ch,"That's private property -- no trespassing!\r\n");
  else
  {
    new_send_to_char(ch, "%s", CONFIG_OK);

    ch->desc->character = victim;
    ch->desc->original = ch;

    victim->desc = ch->desc;
    ch->desc = NULL;
  }
}


ACMD(do_return)
{
  if (ch->desc && ch->desc->original)
  {
    send_to_char("You return to your original body.\r\n", ch);

    /*
     * If someone switched into your original body, disconnect them.
     *   - JE 2/22/95
     *
     * Zmey: here we put someone switched in our body to disconnect state
     * but we must also NULL his pointer to our character, otherwise
     * close_socket() will damage our character's pointer to our descriptor
     * (which is assigned below in this function). 12/17/99
     */
    if (ch->desc->original->desc)
    {
      ch->desc->original->desc->character = NULL;
      STATE(ch->desc->original->desc) = CON_DISCONNECT;
    }

    /* Now our descriptor points to our original body. */
    ch->desc->character = ch->desc->original;
    ch->desc->original = NULL;

    /* And our body's pointer to descriptor now points to our descriptor. */
    ch->desc->character->desc = ch->desc;
    ch->desc = NULL;
    return;
  }
  if (GET_ORIG_LEV(ch))
  {
    GET_LEVEL(ch) = GET_ORIG_LEV(ch);
    GET_ORIG_LEV(ch) = 0;
    new_send_to_char(ch, "You return to your original level.\r\n");
  }
  else
  {
    new_send_to_char(ch, "Um... no.\r\n");
  }

}



ACMD(do_load)
{
  struct char_data *mob = NULL;
  struct obj_data *obj = NULL;
  char timesb[MAX_STRING_LENGTH];
  char *times = timesb;
  int tnum = 0;
  mob_vnum number;
  mob_rnum r_num;
  char buf[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];

  times = two_arguments(argument, buf, buf2);
  tnum = (atoi(times));

  if (!*buf || !*buf2 || !isdigit(*buf2))
  {
    new_send_to_char(ch, "Usage: load {{ obj | mob } <number> [<amount>]\r\n");
    return;
  }

  if (!is_number(buf2))
  {
    new_send_to_char(ch, "That is not a number.\r\n");
    return;
  }

  if ((number = atoi(buf2)) < 0)
  {
    new_send_to_char(ch,"A NEGATIVE number??\r\n");
    return;
  }
  if (is_abbrev(buf, "mob"))
  {
    if ((r_num = real_mobile(number)) < 0)
    {
      new_send_to_char(ch,"There is no monster with the number %d.\r\n", number);
      return;
    }
    for ((tnum == 0 ? tnum = 1 : tnum) ;tnum>0; tnum--)
    {
      mob = read_mobile(r_num, REAL);
      char_to_room(mob, IN_ROOM(ch));
    }

    act("$n makes a quaint, magical gesture with one hand.", TRUE, ch, 0, 0, TO_ROOM);
    act("$n has created $N!", FALSE, ch, 0, mob, TO_ROOM);
    act("You create $N.", FALSE, ch, 0, mob, TO_CHAR);
    load_mtrigger(mob);
  }
  else if (is_abbrev(buf, "obj"))
  {
    if ((r_num = real_object(number)) < 0)
    {
      new_send_to_char(ch, "There is no object with the number %d.\r\n",number);
      return;
    }

    /* Are we allowed to load QIC? */
    if (obj_index[r_num].qic != NULL && GET_LEVEL(ch) < LVL_IMPL)
    {
      new_send_to_char(ch,"You don't have the skill to create a new Artifact.\r\n");
      return;
    }
    for ((tnum == 0 ? tnum = 1 : tnum) ;tnum>0; tnum--)
    {
      obj = read_object(r_num, REAL);
      if (obj == NULL)
      {
        new_send_to_char(ch,"You are not allowed to load QIC's over the limit.\r\n");
        return;
      }

      if (CONFIG_LOAD_INVENTORY)
        obj_to_char(obj, ch);
      else
        obj_to_room(obj, IN_ROOM(ch));
    }
    act("$n makes a strange magical gesture.", TRUE, ch, 0, 0, TO_ROOM);
    act("$n has created $p!", FALSE, ch, obj, 0, TO_ROOM);
    act("You create $p.", FALSE, ch, obj, 0, TO_CHAR);
    load_otrigger(obj);
    log("%s loaded %s", GET_NAME(ch), obj->name);

  }
  else
    send_to_char("That'll have to be either 'obj' or 'mob'.\r\n", ch);

}



ACMD(do_vstat)
{
  struct char_data *mob;
  struct obj_data *obj;
  mob_vnum number;		/* or obj_vnum ... */
  mob_rnum r_num;		/* or obj_rnum ... */
  char buf[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];

  two_arguments(argument, buf, buf2);

  if (!*buf || !*buf2)
  {
    send_to_char("Usage: vstat {{ obj | mob } <number>\r\n"
                 "Or:    vstat player <name>  (this displays variables)\r\n", ch);
    return;
  }
  if (is_abbrev(buf, "player"))
  {
    if ((mob =
           get_player_vis(ch, buf2, NULL, FIND_CHAR_WORLD)) != NULL)
    {
      show_vars = TRUE;
      do_stat_character(ch, mob);
      show_vars = FALSE;
    }
    else
      new_send_to_char(ch,"No such player around.\r\n");
    return;
  }
  if ((number = atoi(buf2)) < 0)
  {
    new_send_to_char(ch,"A NEGATIVE number??\r\n");
    return;
  }
  if (is_abbrev(buf, "mob"))
  {
    if ((r_num = real_mobile(number)) < 0)
    {
      new_send_to_char(ch,"There is no monster with the number %d.\r\n", number);
      return;
    }
    mob = read_mobile(r_num, REAL);
    char_to_room(mob, world_vnum[0]);
    do_stat_character(ch, mob);
    extract_char(mob);
  }
  else if (is_abbrev(buf, "obj"))
  {
    if ((r_num = real_object(number)) < 0)
    {
      new_send_to_char(ch,"There is no object with the number %d.\r\n", number);
      return;
    }
    obj = read_object(r_num, REAL);
    do_stat_object(ch, obj);
    extract_obj(obj);
  }
  else
    send_to_char("That'll have to be either 'obj' or 'mob'.\r\n", ch);
}




/* clean a room of all mobiles and objects */
ACMD(do_purge)
{
  struct char_data *vict, *next_v;
  struct obj_data *obj, *next_o;
  char buf[MAX_INPUT_LENGTH];

  one_argument(argument, buf);

  if (*buf)
  {			/* argument supplied. destroy single object
                                                            				 * or char */
    if ((vict = get_char_vis(ch, buf, NULL, FIND_CHAR_ROOM)))
    {
      if (!IS_NPC(vict) && (GET_LEVEL(ch) <= GET_LEVEL(vict)))
      {
        new_send_to_char(ch,"Fuuuuuuuuu!\r\n");
        return;
      }
      act("$n disintegrates $N.", FALSE, ch, 0, vict, TO_NOTVICT);

      if (!IS_NPC(vict))
      {
        new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s has purged %s.", GET_NAME(ch), GET_NAME(vict));
        if (vict->desc)
        {
          STATE(vict->desc) = CON_CLOSE;
          vict->desc->character = NULL;
          vict->desc = NULL;
        }
      }
      extract_char(vict);
    }
    else
      if ((obj =
             get_obj_in_list_vis(ch, buf, NULL, IN_ROOM(ch)->contents)) != NULL)
      {
        act("$n destroys $p.", FALSE, ch, obj, 0, TO_ROOM);
        extract_obj(obj);
      }
      else
      {
        send_to_char("Nothing here by that name.\r\n", ch);
        return;
      }

    new_send_to_char(ch, "%s", CONFIG_OK);
  }
  else
  {			/* no argument. clean out the room */
    act("$n gestures... You are surrounded by scorching flames!", FALSE, ch, 0, 0, TO_ROOM);
    send_to_room(IN_ROOM(ch), "The world seems a little cleaner.\r\n");

    for (vict = IN_ROOM(ch)->people; vict; vict = next_v)
    {
      next_v = vict->next_in_room;
      if (IS_NPC(vict))
        extract_char(vict);
    }

    for (obj = IN_ROOM(ch)->contents; obj; obj = next_o)
    {
      next_o = obj->next_content;
      extract_obj(obj);
    }
  }
  save_corpses();
}

/* Stop all fighting in a room */
ACMD(do_peace)
{
  struct char_data *vict, *next_v;

  new_send_to_char(ch, "%s",CONFIG_OK);
  act("$n gestures... A Calming light flows from the sky.", FALSE, ch, 0,0, TO_ROOM);
  send_to_room(IN_ROOM(ch), "The world seems a little more peaceful.\r\n");

  for (vict = IN_ROOM(ch)->people; vict; vict = next_v)
  {
    next_v = vict->next_in_room;
    stop_fighting(vict);
    if (IS_NPC(vict))
      clearMemory(vict);
  }
}

const char *logtypes[] =
  {
    "off", "brief", "normal", "complete", "\n"
  };

ACMD(do_syslog)
{
  int tp;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!*arg)
  {
    tp = ((PRF_FLAGGED(ch, PRF_LOG1) ? 1 : 0) + (PRF_FLAGGED(ch, PRF_LOG2) ? 2 : 0));
    new_send_to_char(ch,"Your syslog is currently %s.\r\n", logtypes[tp]);
    return;
  }
  if (((tp = search_block(arg, logtypes, FALSE)) == -1))
  {
    new_send_to_char(ch, "Usage: syslog {{ Off | Brief | Normal | Complete }\r\n");
    return;
  }
  REMOVE_BIT_AR(PRF_FLAGS(ch), PRF_LOG1);
  REMOVE_BIT_AR(PRF_FLAGS(ch), PRF_LOG2);
  if (tp & 1)
    SET_BIT_AR(PRF_FLAGS(ch), PRF_LOG1);
  if (tp & 2)
    SET_BIT_AR(PRF_FLAGS(ch), PRF_LOG2);

  new_send_to_char(ch, "Your syslog is now %s.\r\n", logtypes[tp]);
}

extern int mother_desc;
extern FILE *player_fl;
#define EXE_FILE "bin/circle"	/* maybe use argv[0] but it's not reliable */

/* (c) 1996-97 Erwin S. Andreasen <erwin@pip.dknet.dk> */
ACMD(do_copyover)
{
  FILE *fp;
  struct descriptor_data *d, *d_next;
  char buf[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];
  int process_output(struct descriptor_data *t);


  fp = fopen(COPYOVER_FILE, "w");

  if (!fp)
  {
    new_send_to_char(ch,"Copyover file not writeable, aborted.\r\n");
    return;
  }

  save_all(); //done
  /* For each playing descriptor, save its state */
  for (d = descriptor_list; d; d = d_next)
  {
    struct char_data *och = d->character;
    d_next = d->next;	/* We delete from the list , so need to save this */

    if (!d->character || !IS_PLAYING(d))
    {
      write_to_descriptor(d->descriptor, "\n\rSorry, we are rebooting. Come back in a few minutes.\r\n", d->comp);
      close_socket(d);	/* throw'em out */
    }
    else
    {
      room_rnum rm = GET_WAS_IN(d->character) ? GET_WAS_IN(d->character) : IN_ROOM(d->character);
      fprintf(fp, "%d %s %s %d %d\n", d->descriptor, GET_NAME(och), d->host, rm->number, d->mxp);

      /* save och */
      if (!IS_IMM(och))
        GET_LOADROOM(och) = GET_ROOM_VNUM(GET_WAS_IN(och) == NULL? IN_ROOM(och) : GET_WAS_IN(och));

      log("printing descriptor name and host of connected players");
      save_char(och);
      Crash_rentsave(och, 0);

      REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_CRASH);

      write_aliases(och);
      snprintf(buf, sizeof(buf), "Colors drain away and the world slowly grinds to a halt...\r\n");
#ifdef HAVE_ZLIB_H
      if (d->comp->state == 2)
      {
        d->comp->state = 3; /* Code to use Z_FINISH for deflate */
      }
#endif /* HAVE_ZLIB_H */
      write_to_descriptor (d->descriptor, buf, d->comp);
      d->comp->state = 0;
#ifdef HAVE_ZLIB_H
      if (d->comp->stream)
      {
        deflateEnd(d->comp->stream);
        free(d->comp->stream);
        free(d->comp->buff_out);
        free(d->comp->buff_in);
      }
#endif /* HAVE_ZLIB_H */

    }
  }
  fprintf(fp, "-1\n");
  fclose(fp);


  /* exec - descriptors are inherited */

  snprintf(buf, sizeof(buf), "%d", port);
  snprintf(buf2, sizeof(buf2), "-C%d", mother_desc);

  /* Ugh, seems it is expected we are 1 step above lib - this may be dangerous! */
  chdir("..");
  execl(EXE_FILE, "circle", buf2, buf, (char *) NULL);

  /* Failed - sucessful exec will not return */
  perror("do_copyover: execl");

  new_send_to_char (ch, "Copyover FAILED!\r\n");
  exit(1);			/* too much trouble to try to recover! */
}

ACMD(do_advance)
{
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];
  char *name = arg, *level = buf2;
  int newlevel, oldlevel;

  two_arguments(argument, name, level);

  if (*name)
  {
    if (!(victim = get_char_vis(ch, name, NULL, FIND_CHAR_WORLD)))
    {
      new_send_to_char(ch,"That player is not here.\r\n");
      return;
    }
  }
  else
  {
    new_send_to_char(ch,"Advance who?\r\n");
    return;
  }

  if (GET_LEVEL(ch) <= GET_LEVEL(victim))
  {
    new_send_to_char(ch,"Maybe that's not such a great idea.\r\n");
    return;
  }
  if (IS_NPC(victim))
  {
    new_send_to_char(ch,"NO!  Not on NPC's.\r\n");
    return;
  }
  if (!*level || (newlevel = atoi(level)) <= 0)
  {
    new_send_to_char(ch,"That's not a level!\r\n");
    return;
  }
  if (newlevel > LVL_IMPL)
  {
    new_send_to_char(ch, "%d is the highest possible level.\r\n", LVL_IMPL);
    return;
  }
  if (newlevel > GET_LEVEL(ch))
  {
    new_send_to_char(ch,"Yeah, right.\r\n");
    return;
  }
  if (newlevel == GET_LEVEL(victim))
  {
    new_send_to_char(ch,"They are already at that level.\r\n");
    return;
  }
  oldlevel = GET_LEVEL(victim);
  if (newlevel < GET_LEVEL(victim))
  {
    do_start(victim);
    GET_LEVEL(victim) = newlevel;
    send_to_char("You are momentarily enveloped by darkness!\r\n"
                 "You feel somewhat diminished.\r\n", victim);
  }
  else
  {
    act("$n makes some strange gestures.\r\n"
        "A strange feeling comes upon you,\r\n"
        "Like a giant hand, light comes down\r\n"
        "from above, grabbing your body, that\r\n"
        "begins to pulse with colored lights\r\n"
        "from inside.\r\n\r\n"
        "Your head seems to be filled with demons\r\n"
        "from another plane as your body dissolves\r\n"
        "to the elements of time and space itself.\r\n"
        "Suddenly a silent explosion of light\r\n"
        "snaps you back to reality.\r\n\r\n"
        "You feel slightly different.", FALSE, ch, 0, victim, TO_VICT);
  }

  new_send_to_char(ch, "%s", CONFIG_OK);

  if (newlevel < oldlevel)
    log("(GC) %s demoted %s from level %d to %d.",
        GET_NAME(ch), GET_NAME(victim), oldlevel, newlevel);
  else
    log("(GC) %s has advanced %s to level %d (from %d)",
        GET_NAME(ch), GET_NAME(victim), newlevel, oldlevel);

  gain_exp_regardless(victim, level_exp(GET_CLASS(victim), newlevel, current_class_is_tier_num(victim), REMORTS(victim)) - GET_EXP(victim));
  save_char(victim);
  // log("(do_advance)Saved %s in room %d", GET_NAME(victim), victim->in_room);
}

ACMD(do_powerplay)
{
  struct descriptor_data *d;
  struct char_data *tch;
  int count = 0;
  new_send_to_char(ch, "Lvl  Class  Room   Name                   Health  Position\r\n");
  for (d = descriptor_list; d; d = d->next)

    if (STATE(d) == CON_PLAYING && d->character && d->character != ch)
    {
      tch = d->character;
      if (!IS_NPC(tch) && GET_LEVEL(tch) < LVL_IMMORT)
      {
        new_send_to_char(ch,
                         "%2d  %4s    %-5d  %-20s  %4.4f%%  %s\r\n",
                         GET_LEVEL(tch), CLASS_ABBR(tch),
                         IN_ROOM(tch)->number, GET_NAME(tch),
                         (float) (GET_HIT(tch) * 100) /
                         GET_MAX_HIT(tch),
                         position_types[(int) GET_POS(tch)]);
        count++;
      }
    }
  if (!count)
    new_send_to_char(ch, "  No mortals are on!\r\n");
}

ACMD(do_restore)
{
  struct descriptor_data *d;
  struct char_data *vict;
  int i;
  char buf[MAX_INPUT_LENGTH];

  one_argument(argument, buf);


  if (!*buf)
    new_send_to_char(ch,"Whom do you wish to restore?\r\n");
  else if (!(vict = get_char_vis(ch, buf, NULL, FIND_CHAR_WORLD)))
    new_send_to_char(ch,"%s", CONFIG_NOPERSON);
  else if (!strcmp(buf, "all"))
  {

    for (d = descriptor_list; d; d = d->next)
      if (STATE(d) == CON_PLAYING && d->character
          && d->character != ch)
      {
        vict = d->character;
        if ((GET_LEVEL(vict) < 51))
          new_send_to_char(ch,"You restored %s.\r\n", GET_NAME(vict));

        if (GET_COND(vict, THIRST) != (-1))
          GET_COND(vict, THIRST) = 48;
        if (GET_COND(vict, FULL) != (-1))
          GET_COND(vict, FULL) = 48;
        GET_HIT(vict) = (GET_MAX_HIT(vict));
        GET_MANA(vict) = (GET_MAX_MANA(vict));
        GET_MOVE(vict) = (GET_MAX_MOVE(vict));
        GET_STAMINA(vict) = (GET_MAX_STAMINA(vict));

        act("You have been fully restored by $N!", FALSE, vict, 0, ch, TO_CHAR);

      }
    GET_POS(vict) = POS_STANDING;
    update_pos(vict);
    check_regen_rates(vict);
    new_send_to_char(ch,"%s",CONFIG_OK);
    save_char(vict);
    return;
  }
  else
  {
    GET_HIT(vict) = (GET_MAX_HIT(vict));
    GET_MANA(vict) = (GET_MAX_MANA(vict));
    GET_MOVE(vict) = (GET_MAX_MOVE(vict));
    GET_STAMINA(vict) = (GET_MAX_STAMINA(vict));

    if (GET_LEVEL(ch) >= LVL_GRGOD && (GET_LEVEL(vict) >= LVL_HERO))
    {
      if (GET_COND(vict, THIRST) != (-1))
        GET_COND(vict, THIRST) = (-1);
      if (GET_COND(vict, FULL) != (-1))
        GET_COND(vict, FULL) = (-1);



      for (i = 1; i <= MAX_SKILLS; i++)
        SET_SKILL(vict, i, 100);
      vict->real_abils.str_add = 100;
      vict->real_abils.intel = 25;
      vict->real_abils.wis = 25;
      vict->real_abils.dex = 25;
      vict->real_abils.str = 25;
      vict->real_abils.con = 25;
      vict->real_abils.cha = 25;

      vict->aff_abils = vict->real_abils;
    }
    GET_POS(vict) = POS_STANDING;
    update_pos(vict);
    check_regen_rates(vict);
    save_char(vict);
    new_send_to_char(ch, "%s",CONFIG_OK);
    act("You have been fully restored by $N!", FALSE, vict, 0, ch,
        TO_CHAR);
  }
}


void perform_immort_vis(struct char_data *ch)
{
  if (GET_INVIS_LEV(ch) == 0
      && !AFF_FLAGGED(ch, AFF_HIDE | AFF_INVISIBLE))
  {
    new_send_to_char(ch, "You are already fully visible.\r\n");
    return;
  }

  GET_INVIS_LEV(ch) = 0;
  appear(ch);
  new_send_to_char(ch, "You are now fully visible.\r\n");
}


void perform_immort_invis(struct char_data *ch, int level)
{
  struct char_data *tch;

  if (IS_NPC(ch))
    return;

  for (tch = IN_ROOM(ch)->people; tch; tch = tch->next_in_room)
  {
    if (tch == ch)
      continue;
    if (GET_LEVEL(tch) >= GET_INVIS_LEV(ch) && GET_LEVEL(tch) < level)
      act("You blink and suddenly realize that $n is gone.", FALSE,
          ch, 0, tch, TO_VICT);
    if (GET_LEVEL(tch) < GET_INVIS_LEV(ch) && GET_LEVEL(tch) >= level)
      act("You suddenly realize that $n is standing beside you.",
          FALSE, ch, 0, tch, TO_VICT);
  }

  GET_INVIS_LEV(ch) = level;
  new_send_to_char(ch,  "Your invisibility level is %d.\r\n", level);
}


ACMD(do_invis)
{
  int level;
  char arg[MAX_INPUT_LENGTH];

  if (IS_NPC(ch))
  {
    new_send_to_char(ch, "You can't do that!\r\n");
    return;
  }

  one_argument(argument, arg);
  if (!*arg)
  {
    if (GET_INVIS_LEV(ch) > 0)
      perform_immort_vis(ch);
    else
      perform_immort_invis(ch, GET_LEVEL(ch));
  }
  else
  {
    level = atoi(arg);
    if (level > GET_LEVEL(ch))
      new_send_to_char(ch, "You can't go invisible above your own level.\r\n");
    else if (level < 1)
      perform_immort_vis(ch);
    else
      perform_immort_invis(ch, level);
  }
}


ACMD(do_gecho)
{
  skip_spaces(&argument);
  delete_doubledollar(argument);

  if (!*argument)
    new_send_to_char(ch, "That must be a mistake...\r\n");
  else
    send_to_all("%s\r\n{c0", argument);
}

ACMD(do_poofset)
{
  char **msg;

  switch (subcmd)
  {
  case SCMD_POOFIN:
    msg = &(POOFIN(ch));
    break;
  case SCMD_POOFOUT:
    msg = &(POOFOUT(ch));
    break;
  default:
    return;
  }

  skip_spaces(&argument);

  if (strlen(argument) > 79)
  {
    new_send_to_char(ch, "You poof message must be shorter than 80 characters.\r\n");
    return;
  }

  if (*msg)
    free(*msg);

  if (!*argument)
    *msg = NULL;
  else
    *msg = str_dup(argument);

  //  write_poofs(ch);
  new_send_to_char(ch, "%s",CONFIG_OK);
}
int allowed_pretitle(CHAR_DATA *ch)
{
  if (GET_LEVEL(ch) > LVL_HERO)
    return TRUE;

  if (!PLR_FLAGGED(ch, PLR_ROLEPLAYER))
    return FALSE;

  if (PLR_FLAGGED(ch, PLR_HERO))
    return TRUE;
  if (PLR_FLAGGED(ch, PLR_RP_LEADER))
    return TRUE;

  if (GET_AWARD(ch) >= 200)
    return TRUE;

  return FALSE;

}

ACMD(do_pretitle)
{
  char **msg;

  if (!allowed_pretitle(ch))
  {
    new_send_to_char(ch, "Sorry, but you don't deserve a pretitle yet.\r\n");
    return;
  }

  msg = &(PRETITLE(ch));

  strip_color(argument, strlen(argument));
  skip_spaces(&argument);

  if (strlen(argument) > 14)
  {
    new_send_to_char(ch, "You pretitle must be shorter than 15 characters.\r\n");
    return;
  }

  if (*msg)
    free(*msg);

  if (!*argument)
    *msg = NULL;
  else
    *msg = str_dup(argument);

  new_send_to_char(ch, "%s", CONFIG_OK);
}



ACMD(do_dc)
{
  struct descriptor_data *d;
  int num_to_dc;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);
  if (!(num_to_dc = atoi(arg)))
  {
    new_send_to_char(ch, "Usage: DC <user number> (type USERS for a list)\r\n");
    return;
  }
  for (d = descriptor_list; d && d->desc_num != num_to_dc; d = d->next);

  if (!d)
  {
    new_send_to_char(ch, "No such connection.\r\n");
    return;
  }
  if (d->character && GET_LEVEL(d->character) >= GET_LEVEL(ch))
  {
    if (!CAN_SEE(ch, d->character))
      new_send_to_char(ch, "No such connection.\r\n");
    else
      new_send_to_char(ch, "Umm.. maybe that's not such a good idea...\r\n");
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
  if (STATE(d) == CON_DISCONNECT || STATE(d) == CON_CLOSE)
    new_send_to_char(ch, "They're already being disconnected.\r\n");
  else
  {
    /*
     * Remember that we can disconnect people not in the game and
     * that rather confuses the code when it expected there to be
     * a character context.
     */
    if (STATE(d) == CON_PLAYING)
      STATE(d) = CON_DISCONNECT;
    else
      STATE(d) = CON_CLOSE;

    new_send_to_char(ch,  "Connection #%d closed.\r\n", num_to_dc);
    log("(GC) Connection closed by %s.", GET_NAME(ch));
  }
}



ACMD(do_wizlock)
{
  int value;
  const char *when;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);
  if (*arg)
  {
    value = atoi(arg);
    if (value < 0 || value > GET_LEVEL(ch))
    {
      new_send_to_char(ch, "Invalid wizlock value.\r\n");
      return;
    }
    circle_restrict = value;
    when = "now";
  }
  else
    when = "currently";

  switch (circle_restrict)
  {
  case 0:
    new_send_to_char(ch,  "The game is %s completely open.\r\n", when);
    break;
  case 1:
    new_send_to_char(ch,  "The game is %s closed to new players.\r\n", when);
    break;
  default:
    new_send_to_char(ch,  "Only level %d and above may enter the game %s.\r\n",circle_restrict, when);
    break;
  }
}

char * the_uptime(char * buf, size_t len)
{
  char *tmstr;
  time_t ourtime = boot_time;
  int d, h, m;

  tmstr = (char *) asctime(localtime(&ourtime));
  *(tmstr + strlen(tmstr) - 1) = '\0';

  ourtime = time(0) - boot_time;
  d = ourtime / 86400;
  h = (ourtime / 3600) % 24;
  m = (ourtime / 60) % 60;

  snprintf(buf, len, "Up since %s: %d day%s, %d:%02d", tmstr,
           d, ((d == 1) ? "" : "s"), h, m);
  return buf;

}

char * the_date_now(char * buf, size_t len)
{
  char *tmstr;
  time_t mytime = time(0);
  tmstr = (char *) asctime(localtime(&mytime));
  *(tmstr + strlen(tmstr) - 1) = '\0';
  snprintf(buf, len, "%s", tmstr);
  return buf;

}

ACMD(do_date)
{
  char *tmstr;
  time_t mytime;
  time_t ourtime;
  time_t login;
  int d, h, m;


  mytime = time(0);
  ourtime = boot_time;

  tmstr = (char *) asctime(localtime(&mytime));
  *(tmstr + strlen(tmstr) - 1) = '\0';


  new_send_to_char(ch, "Current machine time: {cy%s{c0\r\n", tmstr);

  tmstr = (char *) asctime(localtime(&ourtime));
  *(tmstr + strlen(tmstr) - 1) = '\0';

  ourtime = time(0) - boot_time;
  d = ourtime / 86400;
  h = (ourtime / 3600) % 24;
  m = (ourtime / 60) % 60;

  new_send_to_char(ch, "Up since {cC%s{c0: %d day%s, %d:%02d\r\n", tmstr,
                   d, ((d == 1) ? "" : "s"), h, m);

  login = ch->player.time.logon;
  tmstr = (char *) asctime(localtime(&login));
  *(tmstr + strlen(tmstr) - 1) = '\0';

  ourtime = time(0) - login;
  d = ourtime / 86400;
  h = (ourtime / 3600) % 24;
  m = (ourtime / 60) % 60;

  new_send_to_char(ch,
                   "Your session started {cc%s{c0: %d day%s, %d:%02d\r\n",
                   tmstr, d, ((d == 1) ? "" : "s"), h, m);

  login = ch->player.time.birth;
  tmstr = (char *) asctime(localtime(&login));
  *(tmstr + strlen(tmstr) - 1) = '\0';

  ourtime = time(0) - login;
  d = ourtime / 86400;
  h = (ourtime / 3600) % 24;
  m = (ourtime / 60) % 60;

  new_send_to_char(ch,
                   "Your character started {cc%s{c0: %d day%s, %d:%02d\r\n",
                   tmstr, d, ((d == 1) ? "" : "s"), h, m);
}



ACMD(do_last)
{
  struct char_data *vict = NULL;
  extern char *class_abbrevs[];
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);
  if (!*arg)
  {
    new_send_to_char(ch, "For whom do you wish to search?\r\n");
    return;
  }
  else if (!get_id_by_name(arg))
  {
    send_to_char("There is no such player.\r\n", ch);
    return;
  }
  CREATE(vict, struct char_data, 1);
  clear_char(vict);
  TEMP_LOAD_CHAR = TRUE;
  if (store_to_char(arg, vict) < 0)
  {
    new_send_to_char(ch, "There is no such player.\r\n");
    free(vict);
    TEMP_LOAD_CHAR = FALSE;
    return;
  }
  TEMP_LOAD_CHAR = FALSE;

  if ((GET_LEVEL(vict) > GET_LEVEL(ch)) && (GET_LEVEL(ch) < LVL_IMPL))
  {
    new_send_to_char(ch, "You are not sufficiently godly for that!\r\n");
    free_char(vict);
    return;
  }
  new_send_to_char(ch, "[%5ld] [%2d %s] %-12s \r\nHost:%-*s \r\nLast: %-20s\r\n",
                   GET_IDNUM(vict), (int) GET_LEVEL(vict),
                   class_abbrevs[(int) GET_CLASS(vict)], GET_NAME(vict),
                   HOST_LENGTH, vict->player_specials->host
                   && *vict->player_specials->host ? vict->player_specials->
                   host : "(NOHOST)", ctime(&vict->player.time.logon));

  free_char(vict);
}


ACMD(do_force)
{
  struct descriptor_data *i, *next_desc;
  struct char_data *vict, *next_force;
  char to_force[MAX_INPUT_LENGTH + 2];
  char buf1[MAX_INPUT_LENGTH ];
  char arg[MAX_INPUT_LENGTH];


  half_chop(argument, arg, to_force);

  snprintf(buf1, sizeof(buf1), "$n has forced you to '%s'.", to_force);

  if (!*arg || !*to_force)
    new_send_to_char(ch, "Whom do you wish to force do what?\r\n");
  else if ((GET_LEVEL(ch) < LVL_GRGOD)
           || (str_cmp("all", arg) && str_cmp("room", arg)))
  {
    if (!(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
      new_send_to_char(ch,"%s", CONFIG_NOPERSON);
    else if (GET_LEVEL(ch) <= GET_LEVEL(vict) && !IS_NPC(vict))
      new_send_to_char(ch,"No, no, no!\r\n");
    else
    {
      new_send_to_char(ch, "%s", CONFIG_OK);
      act(buf1, TRUE, ch, NULL, vict, TO_VICT);
      new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s forced %s to %s", GET_NAME(ch),
                 GET_NAME(vict), to_force);
      command_interpreter(vict, to_force);
    }
  }
  else if (!str_cmp("room", arg))
  {
    new_send_to_char(ch, "%s", CONFIG_OK);
    new_mudlog( NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s forced room %d to %s",
                GET_NAME(ch), GET_ROOM_VNUM(IN_ROOM(ch)), to_force);

    for (vict = IN_ROOM(ch)->people; vict; vict = next_force)
    {
      next_force = vict->next_in_room;
      if (!IS_NPC(vict) && GET_LEVEL(vict) >= GET_LEVEL(ch))
        continue;
      act(buf1, TRUE, ch, NULL, vict, TO_VICT);
      command_interpreter(vict, to_force);
    }
  }
  else
  {			/* force all */
    new_send_to_char(ch,"%s",CONFIG_OK);
    new_mudlog( NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s forced all to %s", GET_NAME(ch), to_force);

    for (i = descriptor_list; i; i = next_desc)
    {
      next_desc = i->next;

      if (STATE(i) != CON_PLAYING || !(vict = i->character)
          || (!IS_NPC(vict) && GET_LEVEL(vict) >= GET_LEVEL(ch)))
        continue;
      act(buf1, TRUE, ch, NULL, vict, TO_VICT);
      command_interpreter(vict, to_force);
    }
  }
}



ACMD(do_wiznet)
{
  struct descriptor_data *d;
  char emote = FALSE;
  char any = FALSE;
  int level = LVL_GOD;
  char buf1[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];

  skip_spaces(&argument);
  delete_doubledollar(argument);

  if (!*argument)
  {
    new_send_to_char(ch, "       Usage: wiznet <text> | #<level> <text> | *<emotetext> |\r\n"
                     "       wiznet @<level> *<emotetext> | wiz @  (for wiz who list) | wiz %% (for arena info)\r\n"
                     "       use as backslash as an escape char.\r\n");

    return;
  }
  switch (*argument)
  {
  case '*':
    emote = TRUE;
  case '#':
    one_argument(argument + 1, buf1);
    if (is_number(buf1))
    {
      half_chop(argument + 1, buf1, argument);
      level = MAX(atoi(buf1), LVL_GOD);
      if (level > (GET_ORIG_LEV(ch) ? GET_ORIG_LEV(ch) :GET_LEVEL(ch)))
      {
        new_send_to_char(ch,"You can't wiznet above your own level.\r\n");
        return;
      }
    }
    else if (emote)
      argument++;
    break;
  case '@':
    for (d = descriptor_list; d; d = d->next)
    {
      if (STATE(d) == CON_PLAYING
          && GET_LEVEL(d->character) >= LVL_GOD
          && !PRF_FLAGGED(d->character, PRF_NOWIZ)
          && (CAN_SEE(ch, d->character)
              || GET_LEVEL(ch) == LVL_IMPL))
      {
        if (!any)
        {
          new_send_to_char(ch, "Gods online:\r\n");
          any = TRUE;
        }
        new_send_to_char(ch, "  %s",GET_NAME(d->character));
        if (PLR_FLAGGED(d->character, PLR_WRITING))
          new_send_to_char(ch, " (Writing)\r\n");
        else if (PLR_FLAGGED(d->character, PLR_MAILING))
          new_send_to_char(ch, " (Writing mail)\r\n");
        else
          new_send_to_char(ch, "\r\n");

      }
    }
    any = FALSE;
    for (d = descriptor_list; d; d = d->next)
    {
      if (STATE(d) == CON_PLAYING
          && GET_LEVEL(d->character) >= LVL_GOD
          && PRF_FLAGGED(d->character, PRF_NOWIZ)
          && CAN_SEE(ch, d->character))
      {
        if (!any)
        {
          new_send_to_char(ch, "Gods offline:\r\n");
          any = TRUE;
        }
        new_send_to_char(ch, "  %s\r\n",GET_NAME(d->character));
      }
    }
    return;
  case '\\':
    ++argument;
    break;
  case '%':			/* arena */
    if (in_arena == ARENA_OFF)
    {
      new_send_to_char(ch, "The Arena is closed right now.\r\n");
    }
    else if (in_arena == ARENA_START)
    {
      new_send_to_char(ch, "Arena will start in %d hour(s)\r\n",   time_to_start);
      new_send_to_char(ch, "It will last for %d hour(s)\r\n",  game_length);
    }
    else if (in_arena == ARENA_RUNNING)
    {
      new_send_to_char(ch, "Arena will end in %d hour(s)\r\n", time_left_in_game);
    }
    return;
  default:
    break;
  }
  if (PRF_FLAGGED(ch, PRF_NOWIZ))
  {
    new_send_to_char(ch,"You are offline!\r\n");
    return;
  }
  skip_spaces(&argument);

  if (!*argument)
  {
    new_send_to_char(ch,"Don't bother the gods like that!\r\n");
    return;
  }
  if (level > LVL_GOD)
  {
    snprintf(buf1, sizeof(buf1), "%s: <%d> %s%s\r\n", GET_NAME(ch), level,
             emote ? "<--- " : "", argument);
    snprintf(buf2, sizeof(buf2), "Someone: <%d> %s%s\r\n", level,
             emote ? "<--- " : "", argument);
  }
  else
  {
    snprintf(buf1, sizeof(buf1), "%s: %s%s\r\n", GET_NAME(ch), emote ? "<--- " : "",
             argument);
    snprintf(buf2, sizeof(buf2), "Someone: %s%s\r\n", emote ? "<--- " : "", argument);
  }

  for (d = descriptor_list; d; d = d->next)
  {
    if ((STATE(d) == CON_PLAYING) && ((GET_ORIG_LEV(d->character) ? GET_ORIG_LEV(d->character) : GET_LEVEL(d->character)) >= level)
        && (!PRF_FLAGGED(d->character, PRF_NOWIZ))
        && (!PLR_FLAGGED(d->character, PLR_MAILING)
            || !PLR_FLAGGED(d->character, PLR_WRITING))
        && (d != ch->desc
            || !(PRF_FLAGGED(d->character, PRF_NOREPEAT))))
    {
      new_send_to_char(d->character, "%s", CCCYN(d->character, C_NRM));
      if (CAN_SEE(d->character, ch))
        new_send_to_char(d->character, "%s", buf1);
      else
        new_send_to_char(d->character, "%s", buf2);
      new_send_to_char(d->character, "%s", CCNRM(d->character, C_NRM));
    }
  }

  if (PRF_FLAGGED(ch, PRF_NOREPEAT))
    new_send_to_char(ch, "%s", CONFIG_OK);
}



ACMD(do_zreset)
{
  zone_rnum i;
  zone_vnum j;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);
  if (!*arg)
  {
    send_to_char("You must specify a zone.\r\n", ch);
    return;
  }
  if (*arg == '*')
  {
    for (i = 0; i <= top_of_zone_table; i++)
      reset_zone(i);
    new_send_to_char(ch, "Reset world.\r\n");
    new_mudlog(NRM, MAX(LVL_GRGOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s reset entire world.", GET_NAME(ch));
    return;
  }
  else if (*arg == '.')
    i = IN_ROOM(ch)->zone;
  else
  {
    j = atoi(arg);
    for (i = 0; i <= top_of_zone_table; i++)
      if (zone_table[i].number == j)
        break;
  }
  if (i >= 0 && i <= top_of_zone_table)
  {
    reset_zone(i);
    new_send_to_char(ch, "Reset zone %d (#%d): %s.\r\n", i,	zone_table[i].number, zone_table[i].name);

    new_mudlog(NRM, MAX(LVL_GRGOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s reset zone %d (%s)", GET_NAME(ch), zone_table[i].number,
               zone_table[i].name);
  }
  else
    send_to_char("Invalid zone number.\r\n", ch);
}

ACMD(do_heroutil)
{
  int immfreeze_affect(struct char_data *ch, struct char_data *vict, char *argument);
  int silence_affect(struct char_data *ch, struct char_data *vict, char *argument);
  struct char_data *vict;
  char arg[MAX_INPUT_LENGTH];

  if (GET_LEVEL(ch) < LVL_HERO && !PLR_FLAGGED(ch, PLR_HERO))
  {
    new_send_to_char(ch, "Huh?\r\n");
    return;
  }
  argument = one_argument(argument, arg);

  if (!*arg)
    new_send_to_char(ch, "Yes, but for whom?!?\r\n");
  else if (!(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
    new_send_to_char(ch,"There is no such player.\r\n");
  else if (IS_NPC(vict))
    new_send_to_char(ch,"You can't do that to a mob!\r\n");
  else if (GET_LEVEL(vict) > GET_LEVEL(ch))
    new_send_to_char(ch,"Hmmm...you'd better not.\r\n");
  else
  {
    switch (subcmd)
    {
    case SCMD_FREEZE:
      if (ch == vict)
      {
        new_send_to_char(ch, "Oh, yeah, THAT'S real smart...\r\n");
        return;
      }
      if (GET_LEVEL(vict) > LVL_HERO)
      {
        new_mudlog( BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(HC) %s attempted to be frozen by %s.", GET_NAME(vict), GET_NAME(ch));
        new_send_to_char(ch, "Sorry, that attempt was logged. I would say goodbye to your hero.\r\n");
        return;
      }
      if (PLR_FLAGGED(vict, PLR_FROZEN))
      {
        new_send_to_char(ch,"Your victim is already pretty cold.\r\n");
        return;
      }
      if (immfreeze_affect(ch, vict, argument))
      {
        SET_BIT_AR(PLR_FLAGS(vict), PLR_FROZEN);
        GET_FREEZE_LEV(vict) = GET_LEVEL(ch);
        new_send_to_char(vict,"A bitter wind suddenly rises and drains every ounce of heat from your body!\r\nYou feel frozen!\r\n");
        new_send_to_char(ch, "Frozen.\r\n");
        act("A sudden cold wind conjured from nowhere freezes $n!", FALSE, vict, 0, 0, TO_ROOM);
        new_mudlog( BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(HC) %s frozen by %s.", GET_NAME(vict), GET_NAME(ch));
      }
      else
      {
        new_send_to_char(ch, "Freeze failed.\r\n");
        new_mudlog( BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(HC) %s attempted to be frozen by %s.", GET_NAME(vict), GET_NAME(ch));
      }
      break;
    case SCMD_THAW:
      if (!PLR_FLAGGED(vict, PLR_FROZEN))
      {
        new_send_to_char(ch,"Sorry, your victim is not morbidly encased in ice at the moment.\r\n");
        return;
      }
      if (GET_FREEZE_LEV(vict) > GET_LEVEL(ch) && GET_FREEZE_LEV(vict) > LVL_HERO)
      {
        new_send_to_char(ch,"Sorry, a level %d God froze %s... You can't unfreeze %s.\r\n",
                         GET_FREEZE_LEV(vict), GET_NAME(vict), HMHR(vict));
        return;
      }
      new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s un-frozen by %s.", GET_NAME(vict), GET_NAME(ch));
      if (!affected_by_spell(vict, SPELL_IMMFREEZE))
        REMOVE_BIT_AR(PLR_FLAGS(vict), PLR_FROZEN);
      else
        affect_from_char(vict, SPELL_IMMFREEZE);

      new_send_to_char(vict, "A fireball suddenly explodes in front of you, melting the ice!\r\nYou feel thawed.\r\n");
      new_send_to_char(ch, "Thawed.\r\n");
      act("A sudden fireball conjured from nowhere thaws $n!", FALSE, vict, 0, 0, TO_ROOM);
      break;
    case SCMD_SILENCE:
      if (ch == vict)
      {
        new_send_to_char(ch, "Oh, yeah, THAT'S real smart...\r\n");
        return;
      }
      if (GET_LEVEL(vict) > LVL_HERO)
      {
        new_mudlog( BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(HC) %s attempted to be silenced by %s.", GET_NAME(vict), GET_NAME(ch));
        new_send_to_char(ch, "Sorry, that attempt was logged. I would say goodbye to your hero.\r\n");
        return;
      }
      if (PLR_FLAGGED(vict, PLR_COVENTRY))
      {
        new_send_to_char(ch,"Your victim is already pretty quiet.\r\n");
        return;
      }
      if (silence_affect(ch, vict, argument))
      {
        act("A yellow light centers around $n's voicebox - $e has been silenced!", FALSE, vict, 0, 0, TO_ROOM);
        new_mudlog( BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(HC) %s Silenced by %s.", GET_NAME(vict), GET_NAME(ch));
      }
      else
      {
        new_send_to_char(ch, "Silence failed.\r\n");
        new_mudlog( BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(HC) %s attempted to be silenced by %s.", GET_NAME(vict), GET_NAME(ch));
      }
      break;
      break;
    default:
      log("SYSERR: Unknown subcmd %d passed to do_wizutil (%s)", subcmd, __FILE__);
      break;
    }
    save_char(vict);

  }
}
/*
 *  General fn for wizcommands of the sort: cmd <player>
 */

ACMD(do_wizutil)
{
  struct char_data *vict;
  long result;
  char arg[MAX_INPUT_LENGTH];


  argument = one_argument(argument, arg);

  if (!*arg)
    new_send_to_char(ch, "Yes, but for whom?!?\r\n");
  else if (!(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
    new_send_to_char(ch,"There is no such player.\r\n");
  else if (IS_NPC(vict))
    new_send_to_char(ch,"You can't do that to a mob!\r\n");
  else if (GET_LEVEL(vict) > GET_LEVEL(ch))
    new_send_to_char(ch,"Hmmm...you'd better not.\r\n");
  else
  {
    switch (subcmd)
    {
    case SCMD_REROLL:
      new_send_to_char(ch,"Rerolled...\r\n");
      roll_real_abils(vict);
      log("(GC) %s has rerolled %s.", GET_NAME(ch), GET_NAME(vict));
      new_send_to_char(ch,
                       "New stats: Str %d/%d, Int %d, Wis %d, Dex %d, Con %d, Cha %d\r\n",
                       GET_STR(vict), GET_ADD(vict), GET_INT(vict),
                       GET_WIS(vict), GET_DEX(vict), GET_CON(vict),
                       GET_CHA(vict));
      new_send_to_char(vict,
                       "New stats: Str %d/%d, Int %d, Wis %d, Dex %d, Con %d, Cha %d\r\n",
                       GET_STR(vict), GET_ADD(vict), GET_INT(vict),
                       GET_WIS(vict), GET_DEX(vict), GET_CON(vict),
                       GET_CHA(vict));
      break;
    case SCMD_PARDON:
      if (PLR_FLAGGED(vict, PLR_THIEF)
          || PLR_FLAGGED(vict, PLR_KILLER))
      {
        REMOVE_BIT_AR(PLR_FLAGS(vict), PLR_THIEF);
        REMOVE_BIT_AR(PLR_FLAGS(vict), PLR_KILLER);
        new_send_to_char(ch,"Pardoned.\r\n");
        new_send_to_char(vict, "You have been pardoned by the Gods!\r\n");
        new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s pardoned by %s", GET_NAME(vict),
                   GET_NAME(ch));
      }
      else
      {
        new_send_to_char(ch,"Your victim is not flagged.\r\n");
        return;
      }
      break;
    case SCMD_NOTITLE:
      result = PLR_TOG_CHK(vict, PLR_NOTITLE);
      new_mudlog( NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(GC) Notitle %s for %s by %s.", ONOFF(result),
                  GET_NAME(vict), GET_NAME(ch));
      new_send_to_char(ch, "%s", CONFIG_OK);
      break;
    case SCMD_SQUELCH:
      result = PLR_TOG_CHK(vict, PLR_NOSHOUT);
      new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "(GC) Squelch %s for %s by %s.", ONOFF(result),
                 GET_NAME(vict), GET_NAME(ch));
      new_send_to_char(ch, "%s", CONFIG_OK);
      break;
    case SCMD_UNAFFECT:
      if (vict->affected)
      {
        while (vict->affected)
          affect_remove(vict, vict->affected);
        new_send_to_char(vict, "There is a brief flash of light!\r\n"
                         "You feel slightly different.\r\n");
        new_send_to_char(ch, "All spells removed.\r\n");
        check_regen_rates(vict);
      }
      else
      {
        new_send_to_char(ch, "Your victim does not have any affections!\r\n");
        return;
      }
      break;
    default:
      log("SYSERR: Unknown subcmd %d passed to do_wizutil (%s)", subcmd, __FILE__);
      break;
    }
    save_char(vict);
    // log("(do_wizutil)Saved %s in room %d.", GET_NAME(vict), vict->in_room);
  }
}

int immfreeze_affect(struct char_data *ch, struct char_data *vict, char *argument)
{
  int duration=0;
  char time_size[MAX_INPUT_LENGTH], duration_p[MAX_INPUT_LENGTH];
  int multi=0;
  struct affected_type af;

  argument = two_arguments(argument, duration_p, time_size);

  if (!*duration_p || !*time_size)
  {
    new_send_to_char(ch, "FREEZE <player> <time> (Days|Hours)\r\n");
    return 0;
  }

  duration = atoi(duration_p);
  if (duration <= 0)
  {
    new_send_to_char(ch, "You need positive duration!\r\n");
    return 0;
  }
  if (is_abbrev(time_size, "days"))
    multi = SECS_PER_REAL_DAY;
  else if (is_abbrev(time_size, "hours"))
    multi = SECS_PER_REAL_HOUR;
  else
  {
    new_send_to_char(ch, "You must specify Days or Hours\r\n"
                     "FREEZE <player> <time> (Days|Hours)\r\n");
    return 0;
  }

  /* add the affect */
  af.type = SPELL_IMMFREEZE;
  af.expire = sec_to_time(duration * multi);
  af.modifier = 0;
  af.location = 0;
  af.bitvector = AFF_IMMFREEZE;


  affect_to_char(vict, &af);
  return 1;
}

int silence_affect(struct char_data *ch, struct char_data *vict, char *argument)
{
  int duration=0;
  char time_size[MAX_INPUT_LENGTH], duration_p[MAX_INPUT_LENGTH];
  int multi=0;
  struct affected_type af;

  argument = two_arguments(argument, duration_p, time_size);

  if (!*duration_p || !*time_size)
  {
    new_send_to_char(ch, "SILENCE <player> <time> (Days|Hours)\r\n");
    return 0;
  }

  duration = atoi(duration_p);
  if (duration <= 0)
  {
    new_send_to_char(ch, "You need positive duration!\r\n");
    return 0;
  }
  if (is_abbrev(time_size, "days"))
    multi = SECS_PER_REAL_DAY;
  else if (is_abbrev(time_size, "hours"))
    multi = SECS_PER_REAL_HOUR;
  else
  {
    new_send_to_char(ch, "You must specify Days or Hours\r\n"
                     "SILENCE <player> <time> (Days|Hours)\r\n");
    return 0;
  }
  SET_BIT_AR(PLR_FLAGS(vict), PLR_COVENTRY);
  /* add the affect */
  af.type = SPELL_SILENCED;
  af.expire = sec_to_time(duration * multi);
  af.modifier = 0;
  af.location = 0;
  af.bitvector = AFF_SILENCED;

  new_send_to_char(vict,"You have been silenced - You will be heard again in %s %s\r\n", duration_p, time_size);
  affect_to_char(vict, &af);
  return 1;
}


int gsort(const void *a, const void *b)
{
  const struct player_gold_info *a1, *b1;

  a1 = (const struct player_gold_info *) a;
  b1 = (const struct player_gold_info *) b;

  return (a1->amount - b1->amount);
}

ACMD(do_topgold)
{
  int i, j;
  extern int top_of_p_table;
  extern struct player_index_element *player_table;
  struct char_data *victim = NULL;


  struct player_gold_info player_gold[top_of_p_table + 1];

  for (j = 0; j <= top_of_p_table; j++)
  {
    player_gold[j].amount = 0;
    player_gold[j].p_id = 0;
  }
  TEMP_LOAD_CHAR = TRUE;
  for (j = 0; j <= top_of_p_table; j++)
  {
    CREATE(victim, struct char_data, 1);
    clear_char(victim);

    if (store_to_char((player_table + j)->name, victim) > -1)
    {
      if (GET_LEVEL(victim) < LVL_HERO)
        player_gold[j].amount = ((GET_GOLD(victim)+GET_BANK_GOLD(victim))/1000000);
      player_gold[j].p_id = (player_table + j)->id;
      free_char(victim);
    }
    else
    {
      free(victim);
      continue;
    }


  }
  TEMP_LOAD_CHAR = FALSE;

  qsort(player_gold, top_of_p_table, sizeof(struct player_gold_info),   gsort);

  new_send_to_char(ch, "Members of the top 10 Gold Tycoons\r\n");
  send_to_char("------------------------------------------\r\n", ch);

  for (i = top_of_p_table - 1; i > (top_of_p_table - 16); i--)
    new_send_to_char(ch, "%-20s -- gold: %lld million.\r\n",
                     get_name_by_id(player_gold[i].p_id),  player_gold[i].amount);


  return;
}

ACMD(do_hostfind)
{
  int  j;
  extern int top_of_p_table;
  extern struct player_index_element *player_table;
  struct char_data *victim = NULL;

  skip_spaces(&argument);
  if (!argument || !*argument)
  {
    new_send_to_char(ch, "Needs an argument. Part of a host name.\r\n");
    return;
  }
  TEMP_LOAD_CHAR = TRUE;
  for (j = 0; j <= top_of_p_table; j++)
  {
    CREATE(victim, struct char_data, 1);
    clear_char(victim);

    if (store_to_char((player_table + j)->name, victim) > -1)
    {
      if (stristr(victim->player_specials->host, argument) != NULL)
        new_send_to_char(ch, "%-10s -- %s\r\n", GET_NAME(victim), victim->player_specials->host);
      free_char(victim);
    }
    else
    {
      free(victim);
      continue;
    }
  }
  TEMP_LOAD_CHAR = FALSE;
  return;
}

/* single zone printing fn used by "show zone" so it's not repeated in the
   code 3 times ... -je, 4/6/93 */

/* FIXME: overflow possible */
size_t print_zone_to_buf(char *bufptr, size_t left, zone_rnum zone, int listall)
{
  size_t tmp;
  if (listall)
  {
    int i, j, k, l, m, n;
    extern int top_of_trigt;
    extern struct index_data **trig_index;
    int count_shops(shop_vnum low, shop_vnum high);
    char zflags[MAX_STRING_LENGTH];

    sprintbit(zone_table[zone].zone_flags, zone_bits, zflags, sizeof(zflags));

    tmp = snprintf(bufptr, left,
                   "%3d %-30.30s By: %-10.10s Age: %3d; Reset: %3d (%1d); Range: %5d-%5d\r\nZone Flags: %s\r\n",
                   zone_table[zone].number, zone_table[zone].name, zone_table[zone].builders,
                   zone_table[zone].age, zone_table[zone].lifespan,
                   zone_table[zone].reset_mode,
                   zone_table[zone].bot, zone_table[zone].top,
                   zflags);
    i = j = k = l = m = n = 0;

    for (i = 0; i < top_of_world; i++)
      if (world_vnum[i] && world_vnum[i]->number >= zone_table[zone].bot && world_vnum[i]->number <= zone_table[zone].top)
        j++;

    for (i = 0; i < top_of_objt; i++)
      if (obj_index[i].vnum >= zone_table[zone].bot && obj_index[i].vnum <= zone_table[zone].top)
        k++;

    for (i = 0; i < top_of_mobt; i++)
      if (mob_index[i].vnum >= zone_table[zone].bot && mob_index[i].vnum <= zone_table[zone].top)
        l++;

    m = count_shops(zone_table[zone].bot, zone_table[zone].top);

    for (i = 0; i < top_of_trigt; i++)
      if (trig_index[i]->vnum >= zone_table[zone].bot && trig_index[i]->vnum <= zone_table[zone].top)
        n++;

    tmp += snprintf(bufptr + tmp, left - tmp,
                    "       Zone stats:\r\n"
                    "       ---------------\r\n"
                    "         Rooms:    %2d\r\n"
                    "         Objects:  %2d\r\n"
                    "         Mobiles:  %2d\r\n"
                    "         Shops:    %2d\r\n"
                    "         Triggers: %2d\r\n",
                    j, k, l, m, n);

    return tmp;
  }

  return snprintf(bufptr, left,
                  "%3d %-30.30s By: %-10.10s Range: %5d-%5d\r\n",
                  zone_table[zone].number, zone_table[zone].name, zone_table[zone].builders,
                  zone_table[zone].bot, zone_table[zone].top);

}



ACMD(do_show)
{
  int i, j, k, l, con;	/* i, j, k to specifics? */
  size_t len, nlen;
  zone_rnum zrn = NOWHERE;
  zone_vnum zvn = NOWHERE;
  byte self = FALSE;
  struct char_data *vict = NULL;
  struct obj_data *obj;
  struct descriptor_data *d;
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
  extern int top_of_trigt;
  DYN_DEFINE;
  const char *gold_types[] =
    { "\n",
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
  fields[] = {
               {"nothing",	 0},		/* 0 */
               {"zones", 	LVL_GOD},	/* 1 */
               {"player", 	LVL_GOD},
               {"rent", 	LVL_GOD},
               {"stats", 	LVL_GOD},
               {"roleplay", 	LVL_IMPL},	/* 5 */
               {"death", 	LVL_GOD},
               {"godrooms", 	LVL_GOD},
               {"shops", 	LVL_GOD},
               {"houses", 	LVL_GOD},
               {"connections", 	LVL_BLD},	/* 10 */
               {"arena", 	LVL_GOD},
               {"snoop", 	LVL_IMPL},
               {"gold", 	LVL_GOD},
               {"builder", 	LVL_IMPL},
               {"olc", 		LVL_GOD},
               {"corpses", 	LVL_GOD},
               {"errors",       LVL_GOD},
               {"assemblies",   LVL_BLD},
               {"\n", 0}
             };

  skip_spaces(&argument);


  if (!*argument)
  {
    new_send_to_char(ch, "Show options:\r\n");
    for (j = 0, i = 1; fields[i].level; i++)
      if (fields[i].level <= GET_LEVEL(ch))
        new_send_to_char(ch, "%-15s%s", fields[i].cmd,
                         (!(++j % 5) ? "\r\n" : ""));
    new_send_to_char(ch, "\r\n");
    return;
  }

  strcpy(arg, two_arguments(argument, field, value));
  tptr = tbuf;
  *tptr = '\0';

  for (l = 0; *(fields[l].cmd) != '\n'; l++)
    if (!strncmp(field, fields[l].cmd, strlen(field)))
      break;

  if (GET_LEVEL(ch) < fields[l].level)
  {
    new_send_to_char(ch,"You are not godly enough for that!\r\n");
    return;
  }
  if (!strcmp(value, "."))
    self = TRUE;
  buf[0] = '\0';
  switch (l)
  {

  case 1:			// zone


    /* tightened up by JE 4/6/93 */
    if (self)
    {
      DYN_CREATE;
      *dynbuf = 0;
      print_zone_to_buf(buf, sizeof(buf), IN_ROOM(ch)->zone, 1);
      DYN_RESIZE(buf);
    }
    else if (*value && is_number(value))
    {
      for (zvn = atoi(value), zrn = 0; zrn <= top_of_zone_table && zone_table[zrn].number != zvn;zrn++);
      if (zrn <= top_of_zone_table)
      {
        DYN_CREATE;
        *dynbuf = 0;
        print_zone_to_buf(buf, sizeof(buf), zrn, 1);
        DYN_RESIZE(buf);
      }
      else
      {
        new_send_to_char(ch, "That is not a valid zone.\r\n");
        return;
      }
    }
    else
    {
      DYN_CREATE;
      *dynbuf = 0;
      for (len = zrn = 0; zrn <= top_of_zone_table; zrn++)
      {
        nlen =
          print_zone_to_buf(buf, sizeof(buf), zrn, 0);
        DYN_RESIZE(buf);
      }
    }
    page_string(ch->desc, dynbuf, DYN_BUFFER);
    break;

  case 2:			/* player */
    if (!*value)
    {
      new_send_to_char(ch,"A name would help.\r\n");
      return;
    }
    else if (!get_id_by_name(value))
    {
      send_to_char("There is no such player.\r\n", ch);
      return;
    }


    CREATE(vict, struct char_data, 1);
    clear_char(vict);
    TEMP_LOAD_CHAR = TRUE;
    if (store_to_char(value, vict) < 0)
    {
      new_send_to_char(ch,"There is no such player.\r\n");
      free(vict);
      TEMP_LOAD_CHAR = FALSE;
      return;
    }
    TEMP_LOAD_CHAR = FALSE;
    if (GET_LEVEL(ch) <= LVL_IMMORT)
      mortal_player_info(ch,vict);
    else
    {
      new_send_to_char(ch, "Player: %-12s (%s) [%2d %s %s]\r\n",
                       GET_NAME(vict), genders[(int) GET_SEX(vict)],
                       GET_LEVEL(vict),
                       race_abbrevs[(int) GET_RACE(vict)],
                       class_abbrevs[(int) GET_CLASS(vict)]);
      new_send_to_char(ch,
                       "Au: %-8lld  Bal: %-8lld  Exp: %-8lld  Align: %-5d  Lessons: %-3d\r\n",
                       char_gold(vict, 0, GOLD_HAND), char_gold(vict, 0, GOLD_BANK),
                       GET_EXP(vict), GET_ALIGNMENT(vict),
                       GET_PRACTICES(vict));
      new_send_to_char(ch,
                       "Started: %-20.16s  Last: %-20.16s  Played: %3dh %2dm\r\n",
                       ctime(&vict->player.time.birth),
                       ctime(&vict->player.time.logon),
                       (int) (vict->player.time.played / 3600),
                       (int) (vict->player.time.played / 60 % 60));
    }
    free_char(vict);
    break;
  case 3:
    if (!*value)
    {
      new_send_to_char(ch,"A name would help.\r\n");
      return;
    }
    Crash_listrent(ch, value);
    break;
  case 4:
    i = 0;
    j = 0;
    k = 0;
    con = 0;
    for (vict = character_list; vict; vict = vict->next)
    {
      if (IS_NPC(vict))
        j++;
      else if (CAN_SEE(ch, vict))
      {
        i++;
        if (vict->desc)
          con++;
      }
    }
    for (obj = object_list; obj; obj = obj->next)
      k++;
    new_send_to_char(ch,
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
                     "  %5d triggers written\r\n",
                     i, con,
                     top_of_p_table + 1,
                     j, top_of_mobt + 1,
                     k, top_of_objt + 1,
                     top_of_world + 1, top_of_zone_table + 1,
                     buf_largecount,
                     buf_switches, buf_overflows,
                     max_actions,
                     min_actions,
                     total_commands_typed,
                     total_pcommands_typed,
                     total_trig_commands_typed, top_of_trigt);

    break;

  case 5:
    DYN_CREATE;
    *dynbuf = 0;
    strcpy(buf, "Roleplay Rooms\r\n------------\r\n");
    DYN_RESIZE(buf);
    for (i = 0, k = 0; i <= top_of_world; i++)
      for (j = 0; j < NUM_OF_DIRS; j++)
        if (world_vnum[i] != NULL)
          if (ROOM_FLAGGED(world_vnum[i], ROOM_ROLEPLAY))
          {
            snprintf(buf, sizeof(buf), "%2d: [%5d] %s\r\n", ++j, i,   world_vnum[i]->name);
            DYN_RESIZE(buf);
          }


    page_string(ch->desc, dynbuf, DYN_BUFFER);
    break;
  case 6:
    DYN_CREATE;
    *dynbuf = 0;
    strcpy(buf, "Death Traps\r\n-----------\r\n");
    DYN_RESIZE(buf);
    for (i = 0, j = 0; i <= top_of_world; i++)
      if (world_vnum[i] != NULL)
        if (ROOM_FLAGGED(world_vnum[i], ROOM_DEATH))
        {
          snprintf(buf, sizeof(buf), "%2d: [%5d] %s\r\n", ++j, GET_ROOM_VNUM(world_vnum[i]),  world_vnum[i]->name);
          DYN_RESIZE(buf);
        }
    page_string(ch->desc, dynbuf, DYN_BUFFER);
    break;
  case 7:
    DYN_CREATE;
    *dynbuf = 0;
    strcpy(buf, "Godrooms\r\n--------------------------\r\n");
    DYN_RESIZE(buf);
    for (i = 0, j = 0; i <= top_of_world; i++)
      if (world_vnum[i] != NULL)
        if (ROOM_FLAGGED(world_vnum[i], ROOM_GODROOM))
        {
          snprintf(buf, sizeof(buf), "%2d: [%5d] %s\r\n", ++j, GET_ROOM_VNUM(world_vnum[i]),   world_vnum[i]->name);
          DYN_RESIZE(buf);
        }
    page_string(ch->desc, dynbuf, DYN_BUFFER);
    break;
  case 8:
    show_shops(ch, value);
    break;
  case 9:
    hcontrol_list_houses(ch);
    break;
  case 10:
    if (*value && is_number(value))
    {
      tptr = value;
      do_connections(ch, tptr);
    }
    break;
  case 11:
    if (in_arena == ARENA_OFF)
    {
      new_send_to_char(ch, "The Arena is closed right now.\r\n");
    }
    else if (in_arena == ARENA_START)
    {
      new_send_to_char(ch, "Arena will start in %d hour(s)\r\n",
                       time_to_start);
      new_send_to_char(ch, "%sIt will last for %d hour(s)\r\n", buf,
                       game_length);
    }
    else if (in_arena == ARENA_RUNNING)
    {
      new_send_to_char(ch, "Arena will end in %d hour(s)\r\n",
                       time_left_in_game);
    }

    break;
  case 12:
    i = 0;
    new_send_to_char(ch,
                     "People currently snooping:\r\n--------------------------\r\n");
    for (d = descriptor_list; d; d = d->next)
    {
      if (d->snooping == NULL || d->character == NULL)
        continue;
      if (STATE(d) != CON_PLAYING
          || GET_LEVEL(ch) < GET_LEVEL(d->character))
        continue;
      if (!CAN_SEE(ch, d->character)
          || IN_ROOM(d->character) == NULL)
        continue;
      i++;
      new_send_to_char(ch, "%-10s - snooped by %s.\r\n",
                       GET_NAME(d->snooping->character),
                       GET_NAME(d->character));
    }
    if (i == 0)
      new_send_to_char(ch, "No one is currently snooping.\r\n");
    break;

  case 13:
    new_send_to_char(ch, "----Gold Statistics----\r\n");
    for (i = 1; i <= 10; i++)
      new_send_to_char(ch, "%18s: %lld\r\n", gold_types[i],
                       gold_data(i, 0));
    break;
  case 14:
    i = 0;
    if (!*value)
    {
      new_send_to_char(ch,"A name would help.\r\n");
      return;
    }
    for (len = zrn = 0; zrn <= top_of_zone_table; zrn++)
    {


      if (is_name(value, zone_table[zrn].builders))
      {
        i++;
        new_send_to_char(ch, "%d  ", zone_table[zrn].number);
        if (!(i%5) && i>0)
          new_send_to_char(ch, "\r\n");
      }

    }
    if (!i)
      new_send_to_char(ch, "%s has no zone to his/her name.", value);

    break;
  case 15:
    i = 0;
    strcpy(value, GET_NAME(ch));
    for (len = zrn = 0; zrn <= top_of_zone_table; zrn++)
    {


      if (is_name(value, zone_table[zrn].builders))
      {
        i++;
        new_send_to_char(ch, "%d  ", zone_table[zrn].number);
        if (!(i%5) && i>0)
          new_send_to_char(ch, "\r\n");
      }

    }
    if (!i)
      new_send_to_char(ch, "You have no zone to your name.");

    break;
  case 16:
    do_show_corpses(ch);
    break;
  case 17:
    do_show_errors(ch);
    show_door_errors(ch);
    break;
  /* show assembly */
  case 18:
    assemblyListToChar(ch);
    break;
    /* show what? */
  default:
    new_send_to_char(ch,"Sorry, I don't understand that.\r\n");
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
set_fields[] = {
                 {
                   "brief", LVL_GOD, PC, BINARY},	/* 0 */
                 {
                   "invstart", LVL_GOD, PC, BINARY},	/* 1 */
                 {
                   "title", LVL_GOD, PC, MISC}, {
                   "nosummon", LVL_GRGOD, PC, BINARY}, {
                   "maxhit", LVL_GRGOD, BOTH, NUMBER}, {
                   "maxmana", LVL_GRGOD, BOTH, NUMBER},	/* 5 */
                 {
                   "maxmove", LVL_GRGOD, BOTH, NUMBER}, {
                   "hit", LVL_GRGOD, BOTH, NUMBER}, {
                   "mana", LVL_GRGOD, BOTH, NUMBER}, {
                   "move", LVL_GRGOD, BOTH, NUMBER}, {
                   "align", LVL_GOD, BOTH, NUMBER},	/* 10 */
                 {
                   "str", LVL_GRGOD, BOTH, NUMBER}, {
                   "stradd", LVL_GRGOD, BOTH, NUMBER}, {
                   "int", LVL_GRGOD, BOTH, NUMBER}, {
                   "wis", LVL_GRGOD, BOTH, NUMBER}, {
                   "dex", LVL_GRGOD, BOTH, NUMBER},	/* 15 */
                 {
                   "con", LVL_GRGOD, BOTH, NUMBER}, {
                   "cha", LVL_GRGOD, BOTH, NUMBER}, {
                   "ac", LVL_GRGOD, BOTH, NUMBER}, {
                   "gold", LVL_GOD, BOTH, NUMBER}, {
                   "bank", LVL_GOD, PC, NUMBER},	/* 20 */
                 {
                   "exp", LVL_GRGOD, BOTH, NUMBER}, {
                   "hitroll", LVL_GRGOD, BOTH, NUMBER}, {
                   "damroll", LVL_GRGOD, BOTH, NUMBER}, {
                   "invis", LVL_IMPL, PC, NUMBER}, {
                   "nohassle", LVL_GRGOD, PC, BINARY},	/* 25 */
                 {
                   "frozen", LVL_FREEZE, PC, BINARY}, {
                   "practices", LVL_GRGOD, PC, NUMBER}, {
                   "lessons", LVL_GRGOD, PC, NUMBER}, {
                   "drunk", LVL_GRGOD, BOTH, MISC}, {
                   "hunger", LVL_GRGOD, BOTH, MISC},	/* 30 */
                 {
                   "thirst", LVL_GRGOD, BOTH, MISC}, {
                   "killer", LVL_GOD, PC, BINARY}, {
                   "thief", LVL_GOD, PC, BINARY}, {
                   "level", LVL_IMPL, BOTH, NUMBER}, {
                   "room", LVL_IMPL, BOTH, NUMBER},	/* 35 */
                 {
                   "roomflag", LVL_GRGOD, PC, BINARY}, {
                   "siteok", LVL_GRGOD, PC, BINARY}, {
                   "deleted", LVL_IMPL, PC, BINARY}, {
                   "class", LVL_GRGOD, BOTH, MISC}, {
                   "nowizlist", LVL_GOD, PC, BINARY},	/* 40 */
                 {
                   "quest", LVL_GOD, PC, BINARY}, {
                   "loadroom", LVL_GRGOD, PC, MISC}, {
                   "color", LVL_GOD, PC, BINARY}, {
                   "idnum", LVL_IMPL, PC, NUMBER}, {
                   "passwd", LVL_IMPL, PC, MISC},	/* 45 */
                 {
                   "nodelete", LVL_GOD, PC, BINARY}, {
                   "sex", LVL_GRGOD, BOTH, MISC}, {
                   "age", LVL_GRGOD, BOTH, NUMBER}, {
                   "remort", LVL_IMPL, PC, MISC}, {
                   "rtwo", LVL_IMPL, PC, MISC},	/* 50 */
                 {
                   "rthree", LVL_IMPL, PC, MISC}, {
                   "race", LVL_GRGOD, BOTH, MISC}, {
                   "pregnant", LVL_IMPL, PC, NUMBER}, {
                   "breakup", LVL_SEN, PC, MISC}, {
                   "rp", LVL_SEN, PC, BINARY},	/* 55 */
                 {
                   "pk", LVL_SEN, PC, BINARY}, {
                   "brasst", LVL_SEN, PC, NUMBER}, {
                   "bronzet", LVL_SEN, PC, NUMBER}, {
                   "silvert", LVL_SEN, PC, NUMBER}, {
                   "goldt", LVL_SEN, PC, NUMBER},	/* 60 */
                 {
                   "clan", LVL_SEN, PC, NUMBER}, {
                   "rank", LVL_SEN, PC, NUMBER}, {
                   "height", LVL_GOD, BOTH, NUMBER}, {
                   "weight", LVL_GOD, BOTH, NUMBER}, {
                   "helper", LVL_SEN, PC, BINARY},	/* 65 */
                 {
                   "spec01", LVL_SEN, PC, BINARY}, {
                   "spec02", LVL_SEN, PC, BINARY}, {
                   "spec03", LVL_SEN, PC, BINARY}, {
                   "spec04", LVL_SEN, PC, BINARY},	/* 69 */

                 {"speed", LVL_SEN, BOTH, NUMBER},
                 {"coventry", LVL_SEN, PC, BINARY},
                 {"pretitle", LVL_SEN, PC, MISC},
                 {"rpgroup", LVL_SEN, PC, MISC},
                 { "olc",		LVL_IMPL,	PC,	MISC },
                 { "hero",		LVL_SEN,	PC,	BINARY }, /* 75 */
                 {"immtitle", LVL_IMPL, PC, MISC},
                 {"mastery", LVL_IMPL, PC, MISC},	/* 77 */
                 {"rpl", LVL_GOD, PC, BINARY},
                 {"tradepoints",LVL_GOD,PC,NUMBER},
                 {   "\n", 0, BOTH, MISC}
               };


int perform_set(struct char_data *ch, struct char_data *vict, int mode,
                char *val_arg)
{
  int i, on = 0, off = 0, value = 0;
  room_rnum rnum;
  room_vnum rvnum;
  char output[MAX_STRING_LENGTH];
  int parse_race(char arg);
  void set_race(struct char_data *ch, int race);

  /* Check to make sure all the levels are correct */
  if (GET_LEVEL(ch) != LVL_IMPL)
  {
    if (!IS_NPC(vict) && GET_LEVEL(ch) <= GET_LEVEL(vict)
        && vict != ch)
    {
      new_send_to_char(ch,"Maybe that's not such a great idea...\r\n");
      return (0);
    }
  }
  if (GET_LEVEL(ch) < set_fields[mode].level)
  {
    new_send_to_char(ch,"You are not godly enough for that!\r\n");
    return (0);
  }

  /* Make sure the PC/NPC is correct */
  if (IS_NPC(vict) && !(set_fields[mode].pcnpc & NPC))
  {
    new_send_to_char(ch,"You can't do that to a beast!\r\n");
    return (0);
  }
  else if (!IS_NPC(vict) && !(set_fields[mode].pcnpc & PC))
  {
    new_send_to_char(ch,"That can only be done to a beast!\r\n");
    return (0);
  }

  /* Find the value of the argument */
  if (set_fields[mode].type == BINARY)
  {
    if (!strcmp(val_arg, "on") || !strcmp(val_arg, "yes"))
      on = 1;
    else if (!strcmp(val_arg, "off") || !strcmp(val_arg, "no"))
      off = 1;
    if (!(on || off))
    {
      new_send_to_char(ch,"Value must be 'on' or 'off'.\r\n");
      return (0);
    }
    snprintf(output, sizeof(output), "%s %s for %s.", set_fields[mode].cmd, ONOFF(on), GET_NAME(vict));
  }
  else if (set_fields[mode].type == NUMBER)
  {
    value = atoi(val_arg);
    snprintf(output, sizeof(output), "%s's %s set to %d.", GET_NAME(vict), set_fields[mode].cmd, value);
  }
  else
  {
    snprintf(output, sizeof(output), "Okay.");	/* can't use OK macro here 'cause of \r\n */
  }

  switch (mode)
  {
  case 0:
    SET_OR_REMOVE(PRF_FLAGS(vict), PRF_BRIEF);
    break;
  case 1:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_INVSTART);
    break;
  case 2:
    set_title(vict, val_arg);
    snprintf(output, sizeof(output), "%s's title is now: %s", GET_NAME(vict),
             GET_TITLE(vict));
    break;
  case 3:
    SET_OR_REMOVE(PRF_FLAGS(vict), PRF_SUMMONABLE);
    snprintf(output, sizeof(output), "Nosummon %s for %s.\r\n", ONOFF(!on),
             GET_NAME(vict));
    break;
  case 4:
    vict->points.max_hit = RANGE(1, 100000);
    affect_total(vict);	/* affect_total() handles regen updates */
    break;
  case 5:
    vict->points.max_mana = RANGE(1, 100000);
    affect_total(vict);	/* affect_total() handles regen updates */
    break;
  case 6:
    vict->points.max_move = RANGE(1, 100000);
    affect_total(vict);	/* affect_total() handles regen updates */
    break;
  case 7:
    vict->points.hit = RANGE(-9, vict->points.max_hit);
    affect_total(vict);	/* affect_total() handles regen updates */
    break;
  case 8:
    vict->points.mana = RANGE(0, vict->points.max_mana);
    affect_total(vict);	/* affect_total() handles regen updates */
    break;
  case 9:
    vict->points.move = RANGE(0, vict->points.max_move);
    affect_total(vict);	/* affect_total() handles regen updates */
    break;
  case 10:
    GET_ALIGNMENT(vict) = RANGE(-1000, 1000);
    affect_total(vict);
    break;
  case 11:
    if (IS_NPC(vict) || GET_LEVEL(vict) >= LVL_GRGOD)
      RANGE(3, 25);
    else
      RANGE(3, 18);
    vict->real_abils.str = value;
    vict->real_abils.str_add = 0;
    affect_total(vict);
    break;
  case 12:
    vict->real_abils.str_add = RANGE(0, 100);
    if (value > 0)
      vict->real_abils.str = 18;
    affect_total(vict);
    break;
  case 13:
    if (IS_NPC(vict) || GET_LEVEL(vict) >= LVL_GRGOD)
      RANGE(3, 25);
    else
      RANGE(3, 18);
    vict->real_abils.intel = value;
    affect_total(vict);
    break;
  case 14:
    if (IS_NPC(vict) || GET_LEVEL(vict) >= LVL_GRGOD)
      RANGE(3, 25);
    else
      RANGE(3, 18);
    vict->real_abils.wis = value;
    affect_total(vict);
    break;
  case 15:
    if (IS_NPC(vict) || GET_LEVEL(vict) >= LVL_GRGOD)
      RANGE(3, 25);
    else
      RANGE(3, 18);
    vict->real_abils.dex = value;
    affect_total(vict);
    break;
  case 16:
    if (IS_NPC(vict) || GET_LEVEL(vict) >= LVL_GRGOD)
      RANGE(3, 25);
    else
      RANGE(3, 18);
    vict->real_abils.con = value;
    affect_total(vict);
    break;
  case 17:
    if (IS_NPC(vict) || GET_LEVEL(vict) >= LVL_GRGOD)
      RANGE(3, 100);
    else
      RANGE(3, 100);
    vict->real_abils.cha = value;
    affect_total(vict);
    break;
  case 18:
    vict->points.armor = RANGE(-100, 100);
    affect_total(vict);
    break;
  case 19:
    char_gold(vict, RANGE(0, 1000000000) - char_gold(vict, 0, GOLD_HAND) , GOLD_HAND);
    break;
  case 20:
    char_gold(vict,RANGE(0, 1000000000) - char_gold(vict, 0, GOLD_BANK), GOLD_BANK);
    break;
  case 21:
    vict->points.exp = RANGE(0, 2000000000);
    break;
  case 22:
    vict->points.hitroll = RANGE(-20, 120);
    affect_total(vict);
    break;
  case 23:
    vict->points.damroll = RANGE(-20, 120);
    affect_total(vict);
    break;
  case 24:
    if (GET_LEVEL(ch) < LVL_IMPL && ch != vict)
    {
      new_send_to_char(ch,"You aren't godly enough for that!\r\n");
      return (0);
    }
    GET_INVIS_LEV(vict) = RANGE(0, GET_LEVEL(vict));
    break;
  case 25:
    if (GET_LEVEL(ch) < LVL_IMPL && ch != vict)
    {
      new_send_to_char(ch,"You aren't godly enough for that!\r\n");
      return (0);
    }
    SET_OR_REMOVE(PRF_FLAGS(vict), PRF_NOHASSLE);
    break;
  case 26:
    if (ch == vict && on)
    {
      new_send_to_char(ch,"Better not -- could be a long winter!\r\n");
      return (0);
    }
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_FROZEN);
    break;
  case 27:
  case 28:
    GET_PRACTICES(vict) = RANGE(0, 2000);
    break;
  case 29:
  case 30:
  case 31:
    if (!str_cmp(val_arg, "off"))
    {
      GET_COND(vict, (mode - 29)) = (char) -1;	/* warning: magic number here */
      snprintf(output, sizeof(output), "%s's %s now off.", GET_NAME(vict),
               set_fields[mode].cmd);
    }
    else if (is_number(val_arg))
    {
      value = atoi(val_arg);
      RANGE(0, 24);
      GET_COND(vict, (mode - 29)) = (char) value;	/* and here too */
      snprintf(output, sizeof(output), "%s's %s set to %d.", GET_NAME(vict),
               set_fields[mode].cmd, value);
    }
    else
    {
      new_send_to_char(ch,"Must be 'off' or a value from 0 to 24.\r\n");
      return (0);
    }
    check_regen_rates(vict);
    break;
  case 32:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_KILLER);
    break;
  case 33:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_THIEF);
    break;
  case 34:
    if (value > GET_LEVEL(ch) || value > LVL_IMPL)
    {
      new_send_to_char(ch,"You can't do that.\r\n");
      return (0);
    }
    RANGE(0, LVL_IMPL);
    GET_ORIG_LEV(vict) = 0;
    vict->player.level = (byte) value;
    break;
  case 35:
    if ((rnum = real_room(value)) != NULL)
    {
      new_send_to_char(ch,"No room exists with the number %d.\r\n", value);
      return (0);
    }
    move_char_to(vict, rnum);
    break;
  case 36:
    SET_OR_REMOVE(PRF_FLAGS(vict), PRF_ROOMFLAGS);
    break;
  case 37:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_SITEOK);
    break;
  case 38:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_DELETED);
    break;
  case 39:
    if ((i = parse_class(*val_arg)) == CLASS_UNDEFINED)
    {
      new_send_to_char(ch,"That is not a class.\r\n");
      return (0);
    }
    GET_CLASS(vict) = i;
    check_regen_rates(vict);
    break;
  case 40:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_NOWIZLIST);
    break;
  case 41:
    SET_OR_REMOVE(PRF_FLAGS(vict), PRF_QUEST);
    break;
  case 42:
    if (!str_cmp(val_arg, "off"))
    {
      REMOVE_BIT_AR(PLR_FLAGS(vict), PLR_LOADROOM);
    }
    else if (is_number(val_arg))
    {
      rvnum = atoi(val_arg);
      if (real_room(rvnum) != NULL)
      {
        SET_BIT_AR(PLR_FLAGS(vict), PLR_LOADROOM);
        GET_LOADROOM(vict) = rvnum;
        snprintf(output, sizeof(output), "%s will enter at room #%d.",
                 GET_NAME(vict), GET_LOADROOM(vict));
      }
      else
      {
        new_send_to_char(ch,"That room does not exist!\r\n");
        return (0);
      }
    }
    else
    {
      new_send_to_char(ch,"Must be 'off' or a room's virtual number.\r\n");
      return (0);
    }
    break;
  case 43:
    SET_OR_REMOVE(PRF_FLAGS(vict), PRF_COLOR_1);
    SET_OR_REMOVE(PRF_FLAGS(vict), PRF_COLOR_2);
    break;
  case 44:
    if (GET_IDNUM(ch) != 1 || !IS_NPC(vict))
      return (0);
    GET_IDNUM(vict) = value;
    break;
  case 45:
    if (GET_LEVEL(ch) < LVL_SEN)
    {
      new_send_to_char(ch,"You cannot change that.\r\n");
      return (0);
    }
    strncpy(GET_PASSWD(vict), CRYPT(val_arg, GET_NAME(vict)), MAX_PWD_LENGTH);
    *(GET_PASSWD(vict) + MAX_PWD_LENGTH) = '\0';
    snprintf(output, sizeof(output), "Password changed to '%s'.", val_arg);
    break;
  case 46:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_NODELETE);
    break;
  case 47:
    if ((i = search_block(val_arg, genders, FALSE)) < 0)
    {
      new_send_to_char(ch, "Must be 'male', 'female', or 'neutral'.\r\n");
      return (0);
    }
    GET_SEX(vict) = i;
    break;
  case 48:			/* set age */
    if (value < 2 || value > 200)
    {	/* Arbitrary limits. */
      new_send_to_char(ch, "Ages 2 to 200 accepted.\r\n");
      return (0);
    }
    /*
     * NOTE: May not display the exact age specified due to the integer
     * division used elsewhere in the code.  Seems to only happen for
     * some values below the starting age (17) anyway. -gg 5/27/98
     */
    vict->player.time.birth =
      time(0) - (value - 17 * SECS_PER_MUD_YEAR);
    check_regen_rates(vict);
    break;
  case 49:
    if ((i = parse_class(*val_arg)) == CLASS_UNDEFINED)
    {
      send_to_char("Removing remort: ", ch);
      if ( REMORTS(vict) > 0 )
        REMORTS(vict)--;
    }
    else if ( GET_REMORT(vict) == CLASS_UNDEFINED )
      REMORTS(vict)++;

    GET_REMORT(vict) = i;
    check_regen_rates(vict);
    break;
  case 50:
    if ((i = parse_class(*val_arg)) == CLASS_UNDEFINED)
    {
      send_to_char("Removing rtwo: ", ch);
      if ( REMORTS(vict) > 0 )
        REMORTS(vict)--;
    }
    else if ( GET_REMORT_TWO(vict) == CLASS_UNDEFINED )
      REMORTS(vict)++;

    GET_REMORT_TWO(vict) = i;
    check_regen_rates(vict);
    break;
  case 51:
    if ((i = parse_class(*val_arg)) == CLASS_UNDEFINED)
    {
      send_to_char("Removing rthree: ", ch);
      if ( REMORTS(vict) > 0 )
        REMORTS(vict)--;
    }
    else if ( GET_REMORT_THREE(vict) == CLASS_UNDEFINED )
      REMORTS(vict)++;

    GET_REMORT_THREE(vict) = i;
    check_regen_rates(vict);
    break;
  case 52:
    if ((i = parse_race(*val_arg)) == RACE_UNDEFINED)
    {
      send_to_char("That is not a race.\r\n", ch);
      return 0;
    }
    else
      set_race(vict, i);
    check_regen_rates(vict);
    break;

  case 53:
    if (GET_SEX(vict) == SEX_MALE)
    {
      send_to_char("Only women get pregnant!\r\n", ch);
      return 0;
    }

    if ((value < -1) || (value >= 6600))
    {	/* Limits */
      send_to_char("Preg must be between -1 and 6600.\r\n", ch);
      return 0;
    }
    if (value == 0)
      PREG(vict) = NOT_PREG;
    else
      if (value == PREG(vict))
      {
        send_to_char("But preg is already at that value!\r\n", ch);
        return 0;
      }

    if (value > PREG(vict))
    {
      act("You make an interesting, shamanistic gesture at $N's stomach.\r\nIt quickly shrinks down!\r\n", TRUE, ch, 0, vict, TO_CHAR);
      act("$n makes an interesting, shamanistic gesture at your stomach.\r\nYou feel a twinge of pain, and your stomach shrinks rapidly!\r\n", TRUE, ch, 0, vict, TO_VICT);
      act("$n makes an interesting, shamanistic gesture at $N's stomach.\r\nIt quickly shrinks down!\r\n", TRUE, ch, 0, vict, TO_NOTVICT);
      PREG(vict) = value;
    }
    else if (value < PREG(vict))
    {
      act("You make a mystical, shamanistic gesture at $N's stomach.\r\nIt bulges and rapidly grows!\r\n", TRUE, ch, 0, vict, TO_CHAR);
      act("$n makes a mystical, shamanistic gesture at your stomach.\r\nYou feel a twinge of pain, and your stomach expands!\r\n", TRUE, ch, 0, vict, TO_VICT);
      act("$n makes a mystical, shamanistic gesture at $N's stomach.\r\nIt quickly bulges and expands!\r\n", TRUE, ch, 0, vict, TO_NOTVICT);
      PREG(vict) = value;
    }
    check_regen_rates(vict);
    break;			/* End Mating Mod Debug Code */

  case 54:
    ROMANCE(vict) = 0;
    PARTNER(vict) = 0;
    send_to_char("Your victim is no longer romantically involved.\r\n",
                 ch);
    break;

  case 55:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_ROLEPLAYER);
    send_to_char("Done.\r\n", ch);
    break;

  case 56:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_PK);
    send_to_char("Done.\r\n", ch);
    break;

  case 57:
    GET_BRASS_TOKEN_COUNT(vict) += value;
    break;

  case 58:
    GET_BRONZE_TOKEN_COUNT(vict) += value;
    break;

  case 59:
    GET_SILVER_TOKEN_COUNT(vict) += value;
    break;

  case 60:
    GET_GOLD_TOKEN_COUNT(vict) += value;
    break;

  case 61:
    GET_CLAN(vict) = value;
    break;

  case 62:
    GET_CLAN_RANK(vict) = value;
    break;

  case 63:
    GET_HEIGHT(vict) = value;
    affect_total(vict);
    break;

  case 64:
    GET_WEIGHT(vict) = value;
    affect_total(vict);
    break;

  case 65:
    SET_OR_REMOVE(PLR_FLAGS(vict), PLR_NEWBIE_HLPR);
    send_to_char("Done.\r\n", ch);
    break;
  case 66:
    if (!str_cmp(val_arg, "on"))
    {
      send_to_char("Done.\r\n", ch);
      GET_CLASS_TIER(vict) = 1;
    }
    else if (!str_cmp(val_arg, "off"))
    {
      send_to_char("Done.\r\n", ch);
      GET_CLASS_TIER(vict) = 0;
    }
    else
      send_to_char("You Must Specify: spec or multi\r\n", ch);
    break;
  case 67:
    if (!str_cmp(val_arg, "on"))
    {
      send_to_char("Done.\r\n", ch);
      GET_REMORT_TIER(vict) = 1;
    }
    else if (!str_cmp(val_arg, "off"))
    {
      send_to_char("Done.\r\n", ch);
      GET_REMORT_TIER(vict) = 0;
    }
    else
      send_to_char("You Must Specify: spec or multi\r\n", ch);
    check_regen_rates(vict);
    break;
  case 68:
    if (!str_cmp(val_arg, "on"))
    {
      send_to_char("Done.\r\n", ch);
      GET_REMORT_TWO_TIER(vict) = 1;
    }
    else if (!str_cmp(val_arg, "off"))
    {
      send_to_char("Done.\r\n", ch);
      GET_REMORT_TWO_TIER(vict) = 0;
    }
    else
      send_to_char("You Must Specify: spec or multi\r\n", ch);
    check_regen_rates(vict);
    break;
  case 69:
    if (!str_cmp(val_arg, "on"))
    {
      send_to_char("Done.\r\n", ch);
      GET_REMORT_THREE_TIER(vict) = 1;
    }
    else if (!str_cmp(val_arg, "off"))
    {
      send_to_char("Done.\r\n", ch);
      GET_REMORT_THREE_TIER(vict) = 0;
    }
    else
      send_to_char("You Must Specify: spec or multi\r\n", ch);
    check_regen_rates(vict);
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
    else if (isname(val_arg, "assholes"))
      i = 4;
    else if (isname(val_arg, "orsinis"))
      i = 5;
    else if (isname(val_arg, "Lolthite"))
      i = 6;
    else if (!strcmp(val_arg, "none"))
      i = 0;
    GET_RP_GROUP(vict) = i;
    break;
  case 74:
    if (is_abbrev(val_arg, "socials") || is_abbrev(val_arg, "actions"))
      GET_OLC_ZONE(vict) = AEDIT_PERMISSION;
    else if (is_abbrev(val_arg, "off"))
      GET_OLC_ZONE(vict) = NOWHERE;
    else if (!is_number(val_arg))
    {
      new_send_to_char(ch, "Value must be either 'socials', 'actions', 'off' or a zone number.\r\n");
      return (0);
    }
    else
      GET_OLC_ZONE(vict) = atoi(val_arg);
    break;
  case 75:
    SET_OR_REMOVE_AR(PLR_FLAGS(vict), PLR_HERO);
    break;
  case 76:
    if (strlen(val_arg) > 20)
    {
      new_send_to_char(ch, "Sorry it must be 20 characters or less\r\n");
      return 0;
    }
    if (strstr(val_arg, "{"))
    {
      new_send_to_char(ch, "Sorry, no color codes please.\r\n");
      return 0;
    }
    free_string(&IMMTITLE(vict));
    IMMTITLE(vict) = NULL;
    if (val_arg && *val_arg)
      IMMTITLE(vict) = strdup(val_arg);
    snprintf(output, sizeof(output), "Imm Title of %s set to: %s",GET_NAME(vict), val_arg);
    break;
  case 77:
    if ((i = parse_class(*val_arg)) == CLASS_UNDEFINED)
    {
      send_to_char("That is not a class.\r\n", ch);
      return 0;
    }
    GET_MASTERY(vict, i) = !GET_MASTERY(vict, i);
    break;
  case 78:
    SET_OR_REMOVE_AR(PLR_FLAGS(vict), PLR_RP_LEADER);
    break;
  case 79:
    if(!isdigit(*val_arg)){
       if(*val_arg=='-')
         send_to_char("Please specify only positive numbers.\r\n",ch);
       else
         send_to_char("That is not a number.\r\n", ch);
       return 0;
    }
    TRADEPOINTS(vict)=atoi(val_arg);
    break;
  default:
    new_send_to_char(ch, "Can't set that!\r\n");
    return (0);
  }

  new_send_to_char(ch,"%s\r\n", CAP(output));
  return (1);
}

ACMD(do_saveall)
{
  if (GET_LEVEL(ch) < LVL_BUILDER)
    new_send_to_char (ch, "You are not holy enough to use this privelege.\r\n");
  else
  {
    save_all();
    new_send_to_char(ch, "World files saved.\r\n");
  }
}

ACMD(do_password)
{
  char arg[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];

  argument = two_arguments(argument, arg, buf2);

  if (!*arg || !*buf2)
  {
    new_send_to_char(ch, "password <old pass> <new pass>\r\n");
    return;
  }

  if (!*buf2 || strlen(buf2) > MAX_PWD_LENGTH || strlen(buf2) < 3 ||
      !str_cmp(buf2, GET_PC_NAME(ch)))
  {
    new_send_to_char(ch,"Illegal password.\r\n ");
    return;
  }

  if (strncmp(CRYPT(arg, GET_PASSWD(ch)), GET_PASSWD(ch), MAX_PWD_LENGTH))
  {
    new_send_to_char(ch, "Invalid password.\r\n");
    return;
  }

  strncpy(GET_PASSWD(ch), CRYPT(buf2, GET_NAME(ch)), MAX_PWD_LENGTH);
  *(GET_PASSWD(ch) + MAX_PWD_LENGTH) = '\0';
  new_send_to_char(ch, "Password changed to '%s'.", buf2);
  save_char(ch);

}


ACMD(do_set)
{
  struct char_data *vict = NULL, *cbuf = NULL;
  char field[MAX_INPUT_LENGTH], name[MAX_INPUT_LENGTH],
  val_arg[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH];
  int mode, len, player_i = 0, retval;
  char is_file = 0, is_player = 0;

  half_chop(argument, name, buf);

  if (!strcmp(name, "file"))
  {
    is_file = 1;
    half_chop(buf, name, buf);
  }
  else if (!str_cmp(name, "player"))
  {
    is_player = 1;
    half_chop(buf, name, buf);
  }
  else if (!str_cmp(name, "mob"))
    half_chop(buf, name, buf);

  half_chop(buf, field, buf);
  strcpy(val_arg, buf);

  if (!*name || !*field)
  {
    send_to_char("Usage: set <victim> <field> <value>\r\n", ch);
    return;
  }

  /* find the target */
  if (!is_file)
  {
    if (is_player)
    {
      if (!(vict = get_player_vis(ch, name, NULL, FIND_CHAR_WORLD)))
      {
        send_to_char("There is no such player.\r\n", ch);
        return;
      }
    }
    else
    {		/* is_mob */
      if (!(vict = get_char_vis(ch, name, NULL, FIND_CHAR_WORLD)))
      {
        send_to_char("There is no such creature.\r\n", ch);
        return;
      }
    }
  }
  else if (is_file)
  {
    //    new_send_to_char(ch, "This has been disabled as it is more trouble then it is worth.\r\n");
    //  return;
    /* try to load the player off disk */
    CREATE(cbuf, struct char_data, 1);
    clear_char(cbuf);
    if ((player_i = load_char(name, cbuf)) > -1)
    {
      if (GET_LEVEL(cbuf) >= GET_LEVEL(ch))
      {
        send_to_char("Sorry, you can't do that.\r\n", ch);
        return;
      }
      vict = cbuf;
    }
    else
    {
      free_char(cbuf);
      send_to_char("There is no such player.\r\n", ch);
      return;
    }
  }

  /* find the command in the list */
  len = strlen(field);
  for (mode = 0; *(set_fields[mode].cmd) != '\n'; mode++)
    if (!strncmp(field, set_fields[mode].cmd, len))
      break;

  /* perform the set */
  retval = perform_set(ch, vict, mode, val_arg);

  /* save the character if a change was made */
  if (retval)
  {
    if (!is_file && !IS_NPC(vict))
    {
      save_char(vict);
    }
    if (is_file)
    {
      send_to_char("Saved in file.\r\n", ch);
    }
  }


}

void out_rent(char *name)
{
  FILE *fl, *fp;
  char fname[MAX_INPUT_LENGTH], buf[MAX_STRING_LENGTH],
  fname2[MAX_INPUT_LENGTH];
  struct obj_file_elem object;
  struct obj_data *obj;
  struct rent_info rent;

  if (!get_filename(name, fname, CRASH_FILE))
    return;
  if (!(fl = fopen(fname, "rb")))
  {
    return;
  }

  snprintf(buf, sizeof(buf), "%s\r\n", fname);
  if (!feof(fl))
    fread(&rent, sizeof(struct rent_info), 1, fl);

  if (!get_filename(name, fname2, NEW_OBJ_FILES))
  {
    log("Unable to complete conversion - unable to get new object filename.\r\n");
    return;
  }
  if (!(fp = fopen(fname2, "w+")))
  {
    log("Unable to open new object file.\r\n");
    return;
  }

  fprintf(fp, "%d %d %d %d %d %d\r\n", rent.rentcode, rent.time,
          rent.net_cost_per_diem, rent.gold, rent.account, rent.nitems);
  /* for rent code */

  while (!feof(fl))
  {
    fread(&object, sizeof(struct obj_file_elem), 1, fl);

    if (ferror(fl) || ferror(fp))
    {
      fclose(fl);
      fclose(fp);
      log("Error in conversion of rent files.");
      return;
    }

    if (!feof(fl))
    {
      if (real_object(object.item_number) > 0)
      {
        /* none of these will be unique items. just can't happen */
        obj = read_object(object.item_number, VIRTUAL);
        my_obj_save_to_disk(fp, obj, 0);
        extract_obj(obj);
      }
    }
  }
  fclose(fl);

  /* write final line - this is never actually read.. but hey! */
  fprintf(fp, "$~\n");
  fclose(fp);
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

ACMD(do_objconv)
{
  int counter;
  float percent;
  struct char_data *victim;
  extern struct player_index_element *player_table;
  int flag25 = 0, flag50 = 0, flag75 = 0, flag100 = 0;
  int process_output(struct descriptor_data *t);
  struct descriptor_data *d;

  send_to_all("Please hold on - object conversion taking place.\r\n");

  for (d = descriptor_list; d; d = d->next)
  {
    process_output(d);
  }

  if (GET_LEVEL(ch) != LVL_IMPL && !IS_NPC(ch))
  {
    send_to_char("You may not.\r\n", ch);
    return;
  }
  /* okay, this is where we load every char, apparently.. but we'll
     do it one at a time, thank you very much */
  for (counter = 0; counter <= top_of_p_table; counter++)
  {
    CREATE(victim, struct char_data, 1);
    clear_char(victim);
    if (load_char((player_table + counter)->name, victim) > -1)
      out_rent(GET_NAME(victim));
    free_char(victim);
    percent = (float) ((float) counter / (float) top_of_p_table);
    if (percent > .25 && flag25 == 0)
    {
      send_to_char("25%...", ch);
      flag25 = 1;
      process_output(ch->desc);
    }
    if (percent > .50 && flag50 == 0)
    {
      send_to_char("50%...", ch);
      flag50 = 1;
      process_output(ch->desc);
    }
    if (percent > .75 && flag75 == 0)
    {
      send_to_char("75%...", ch);
      flag75 = 1;
      process_output(ch->desc);
    }
    if (percent > .90 && flag100 == 0)
    {
      send_to_char("90%...", ch);
      flag100 = 1;
      process_output(ch->desc);
    }
  }
  send_to_char("100%. Done.\r\n", ch);
}

ACMD(do_own)
{
  char what[MAX_INPUT_LENGTH];
  char owner[MAX_INPUT_LENGTH];
  long idnum;
  OBJ_DATA *obj;

  two_arguments(argument, what, owner);

  if (!*what || !*owner)
  {
    new_send_to_char(ch, "OWN <objectname in inventory> <idnum of player or 0 for none>\r\n");
    return;
  }

  if (!(obj = get_obj_in_list_vis(ch, what, NULL, ch->carrying)))
  {
    new_send_to_char(ch, "You don't seem to have any %ss.\r\n", what);
  }
  if (!is_number(owner))
  {
    new_send_to_char(ch, "%s is not an ID number.\r\n", owner);
    return;
  }
  idnum = atol(owner);
  if (idnum == 0)
  {
    new_send_to_char(ch, "Setting %s to NO owner.\r\n", obj->short_description);
    obj->owner = 0;
    return;
  }

  new_send_to_char(ch, "Setting %s's owner to %s.\r\n", obj->short_description, get_name_by_id(idnum));
  obj->owner = idnum;


}

ACMD(do_hackinvis)
{
  extern struct index_data *obj_index;
  extern struct obj_data *obj_proto;
  int nr, found = 0;
  int check_item_hack_invis(struct obj_data *obj, int fix);
  char buf[MAX_INPUT_LENGTH];
  DYN_DEFINE;
  *buf = 0;

  DYN_CREATE;
  *dynbuf = 0;

  for (nr = 0; nr <= top_of_objt; nr++)
  {
    if (check_item_hack_invis(&obj_proto[nr], FALSE))
    {
      snprintf(buf, sizeof(buf), "%5d. [%5d] %s\r\n", ++found,
               obj_index[nr].vnum, obj_proto[nr].short_description);
      DYN_RESIZE(buf);
    }
  }

  if (!found)
    send_to_char("No objects were found in those parameters.\r\n", ch);
  else
    page_string(ch->desc, dynbuf, DYN_BUFFER);

}


int check_item_hack_invis(struct obj_data *obj, int fix)
{
  char buf[MAX_INPUT_LENGTH];
  if (strlen(obj->description) < 4 && strchr(obj->description, '{'))
  {
    if (fix)
    {
      SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_NODISPLAY);
      free_string(&obj->description);
      snprintf(buf, sizeof(buf), "%s lies here", obj->short_description);
      obj->description = strdup(buf);
    }
    return 1;
  }
  else
    return 0;
}

ACMD(do_potionweight)
{

  int nr, found = 0;
  char buf[MAX_INPUT_LENGTH];
  DYN_DEFINE;
  *buf = 0;

  DYN_CREATE;
  *dynbuf = 0;

  for (nr = 0; nr <= top_of_objt; nr++)
  {
    if (GET_OBJ_TYPE(&obj_proto[nr]) == ITEM_POTION)
    {
      snprintf(buf, sizeof(buf), "%5d. [%5d][L:%2d][W: %2d][Cst: %6d] %s\r\n", ++found,
               obj_index[nr].vnum, GET_OBJ_VAL(&obj_proto[nr], 0),GET_OBJ_WEIGHT(&obj_proto[nr]),
               GET_OBJ_COST(&obj_proto[nr]), obj_proto[nr].short_description);
      DYN_RESIZE(buf);
    }
  }

  if (!found)
    send_to_char("No objects were found in those parameters.\r\n", ch);
  else
    page_string(ch->desc, dynbuf, DYN_BUFFER);

}

int check_potion_weight(struct obj_data *obj)
{
  int spell_weight(struct obj_data *obj, int val);
  int weight = 0;


  switch (GET_OBJ_TYPE(obj))
  {
  case ITEM_POTION:
  case ITEM_SCROLL:
    weight += spell_weight(obj, 1);
    weight += spell_weight(obj, 2);
    weight += spell_weight(obj, 3);
    weight_to_object(obj,weight);
    return 1;
    break;
  case ITEM_WAND:
  case ITEM_STAFF:
    weight += spell_weight(obj, 3) * GET_OBJ_VAL(obj, 2);
    weight_to_object(obj,weight);
    return 1;
    break;
  default:
    return 0;
    break;
  }



}
int check_potion_price(struct obj_data *obj)
{
  int weight = 0;


  switch (GET_OBJ_TYPE(obj))
  {
  case ITEM_POTION:
  case ITEM_SCROLL:
    weight += spell_price(obj, 1);
    weight += spell_price(obj, 2);
    weight += spell_price(obj, 3);
    weight *= 2000;
    GET_OBJ_COST(obj) = weight;
    return 1;
    break;
  case ITEM_WAND:
  case ITEM_STAFF:
    weight += spell_price(obj, 3) * GET_OBJ_VAL(obj, 2);
    weight *= 2000;
    GET_OBJ_COST(obj) = weight;
    return 1;
    break;
  default:
    return 0;
    break;
  }



}

int spell_weight(struct obj_data *obj, int val)
{
  if (GET_OBJ_VAL(obj, val) == -1)	/* i.e.: no spell */
    return 0;

  /*
   * Check for negative spells, spells beyond the top define, and any
   * spell which is actually a skill.
   */
  if (GET_OBJ_VAL(obj, val) <= 0)
    return 0;
  if (GET_OBJ_VAL(obj, val) > TOP_SPELL_DEFINE)
    return 1;
  if (GET_OBJ_VAL(obj, val) > MAX_SPELLS
      && GET_OBJ_VAL(obj, val) <= MAX_SKILLS)
    return 1;

  switch (GET_OBJ_VAL(obj, val))
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


int spell_price(struct obj_data *obj, int val)
{
  if (GET_OBJ_VAL(obj, val) == TYPE_UNDEFINED)	/* i.e.: no spell */
    return 0;

  /*
   * Check for negative spells, spells beyond the top define, and any
   * spell which is actually a skill.
   */
  if (GET_OBJ_VAL(obj, val) == 0)
    return 0;
  if (GET_OBJ_VAL(obj, val) > TOP_SPELL_DEFINE)
    return 1;
  if (GET_OBJ_VAL(obj, val) > MAX_SPELLS
      && GET_OBJ_VAL(obj, val) <= MAX_SKILLS)
    return 1;

  switch (GET_OBJ_VAL(obj, val))
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
  default:
    return 9;
    break;
  }



}

void do_connections(struct char_data *ch, char *arg)
{
  int zone_num;
  int j, i, k;
  int start_room;
  char tbuf[MAX_STRING_LENGTH];
  char buf[MAX_INPUT_LENGTH];
  DYN_DEFINE;
  *buf = 0;



  if (!*arg)
  {
    send_to_char("USAGE: show connections .\r\n", ch);
    send_to_char("USAGE: show connections <zone_num>\r\n", ch);
    return;
  }
  else if (*arg == '.')
    zone_num = IN_ROOM(ch)->zone;
  else
  {
    j = atoi(arg);
    for (zone_num = 0; zone_num <= top_of_zone_table; zone_num++)
      if (zone_table[zone_num].number == j)
        break;
  }
  DYN_CREATE;
  *dynbuf = 0;
  if (zone_num >= 0 && zone_num <= top_of_zone_table)
  {
    start_room = zone_table[zone_num].number * 100;
    snprintf(tbuf, sizeof(tbuf), "Connections from %-30.30s\r\n"
             "--------------------------------------------------------------------------------\r\n",
             zone_table[zone_num].name);
    DYN_RESIZE(tbuf);

    for (i = 0, k = 0; i <= top_of_world; i++)
    {
      if (!world_vnum[i])
        continue;
      for (j = 0; j < NUM_OF_DIRS; j++)
      {
        if (world_vnum[i]->zone == zone_num &&
            world_vnum[i]->dir_option[j] &&
            world_vnum[i]->dir_option[j]->to_room != NULL &&
            world_vnum[i]->dir_option[j]->to_room->zone != zone_num)
        {
          snprintf(tbuf, sizeof(tbuf),
                   "%3d: [%5d] %-23.23s -(%-5.5s)-> [%5d] %-23.23s\r\n",
                   ++k, world_vnum[i]->number, world_vnum[i]->name, dirs[j],
                   world_vnum[i]->dir_option[j]->to_room->number,
                   world_vnum[i]->dir_option[j]->to_room->name);
          DYN_RESIZE(tbuf);
        }
      }
    }

    snprintf(tbuf, sizeof(tbuf),  "\r\nConnections to %-30.30s\r\n"
             "--------------------------------------------------------------------------------\r\n",
             zone_table[zone_num].name);
    DYN_RESIZE(tbuf);
    for (i = 0, k = 0; i <= top_of_world; i++)
    {
      if (!world_vnum[i])
        continue;
      for (j = 0; j < NUM_OF_DIRS; j++)
      {
        if (world_vnum[i]->zone != zone_num &&
            world_vnum[i]->dir_option[j] &&
            world_vnum[i]->dir_option[j]->to_room != NULL &&
            world_vnum[i]->dir_option[j]->to_room->zone == zone_num)
        {
          snprintf(tbuf, sizeof(tbuf),
                   "%3d: [%5d] %-23.23s -(%-5.5s)-> [%5d] %-23.23s\r\n",
                   ++k, world_vnum[i]->number, world_vnum[i]->name, dirs[j],
                   world_vnum[i]->dir_option[j]->to_room->number,
                   world_vnum[i]->dir_option[j]->to_room->name);
          DYN_RESIZE(tbuf);
        }
      }
    }

    page_string(ch->desc, dynbuf, DYN_BUFFER);
  }
}

ACMD(do_innate)
{
  struct char_data *victim;
  struct affected_type *aff = NULL;
  int found = FALSE;
  char buf[MAX_INPUT_LENGTH];
  char arg[MAX_INPUT_LENGTH];

  half_chop(argument, arg, buf);

  if (!buf)
  {
    if (subcmd == SCMD_SINNATE)
      send_to_char
      ("You must specify WHICH spell you'd like to make innate.\r\n",
       ch);
    else
      send_to_char
      ("You must specify WHICH spell you'd like to remove innate from.\r\n",
       ch);
    return;
  }

  if (!(victim = get_player_vis(ch, arg, NULL, 0)))
  {
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    return;
  }

  /* lets make sure the spell exists. */
  if (victim->affected)
  {
    for (aff = victim->affected; aff && !found; aff = aff->next)
    {
      if (is_abbrev(buf, skill_name(aff->type)))
        found = TRUE;
    }
  }

  if (!found)
  {
    send_to_char
    ("That person is not affected by that spell.Sorry.\r\n", ch);
    return;
  }

  /* this searches  out all instances of named spell - after  all, spells
   * like bless cause  several affects, you'd  have to  make them all
   * innate
   */
  if (subcmd == SCMD_SINNATE)
    for (aff = victim->affected; aff; aff = aff->next)
    {
      if (is_abbrev(buf, skill_name(aff->type)))
        aff->expire = -2;	/* well, -1 is innate neh? never goes away */
    }
  else
    /* really, this is unaffect, with a specific target */
    for (aff = victim->affected; aff; aff = aff->next)
    {
      if (is_abbrev(buf, skill_name(aff->type)))
        affect_remove(victim, aff);
    }
  new_send_to_char(ch, "%s", CONFIG_OK);
}

void olc_list_flags(struct char_data *ch, const char *apply_stuff[])
{

  int counter = 0, columns = 0;

  // clear_screen(d);
  while (*apply_stuff[counter] != '\n')
  {
    new_send_to_char(ch, "{cg%2d{c0) %-20.20s %s", counter,
                     apply_stuff[counter],
                     !(++columns % 2) ? "\r\n" : "");
    counter++;
  }
}

ACMD(do_statinnate)
{
  int count = 0, object,  bot=0, top=0, s = -1;
  char buf[MAX_INPUT_LENGTH];
  char buf1[MAX_INPUT_LENGTH];
  DYN_DEFINE;
  skip_spaces(&argument);
  if (!*argument) {
    new_send_to_char(ch, "You must supply an argument.\r\nstatinnate <bottom vnum> <top vnum>\r\nstatinnate <name of innate>\r\n");
    return;
  }
  if (!is_number(argument)) {
    s = spell_num(argument);
  } else {
    two_arguments(argument, buf, buf1);
    if (is_number(buf) && is_number(buf1))
    {
      bot = atoi(buf);
      top = atoi(buf1);
    }
  }
  DYN_CREATE;
  *dynbuf = 0;
new_send_to_char(ch, "You list the innates:\r\n");
  for (object = 0; object <= top_of_objt; object++)
  {
    if (!obj_proto[object].obj_flags.obj_innate)
      continue;
    if (bot + top != 0) {
      if (obj_index[object].vnum >= bot && obj_index[object].vnum <= top)
      {
        count++;
        snprintf(buf, sizeof(buf), "[vnum: %7d] innate: %s - %s\r\n",
                 obj_index[object].vnum,
                 skill_name(obj_proto[object].obj_flags.obj_innate),
                 obj_proto[object].short_description);
        DYN_RESIZE(buf);
      }
    } else if (s != -1 && obj_proto[object].obj_flags.obj_innate == s) {
      count++;
      snprintf(buf, sizeof(buf), "[vnum: %7d] innate: %s - %s\r\n",
               obj_index[object].vnum,
               skill_name(obj_proto[object].obj_flags.obj_innate),
               obj_proto[object].short_description);
      DYN_RESIZE(buf);
    }
    
  }
  if (!count)
    new_send_to_char(ch, "No innates!");
  else
    page_string(ch->desc, dynbuf, DYN_BUFFER);
  
  
}

ACMD(do_statlist)
{
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];
  int aff = 0, pos = -1, nr, count = 0, object, size = 0;

  DYN_DEFINE;
  *buf = 0;

  *buf = '\0';

  two_arguments(argument, arg1, arg2);

  if (!*arg1)
  {
    send_to_char
    ("You must specify which affect you want. Options are:\r\n",
     ch);
    olc_list_flags(ch, apply_types);
    return;
  }

  for (aff = 0; *apply_types[aff] != '\n'; aff++)
    if (is_abbrev(arg1, apply_types[aff]))
      break;

  if (*apply_types[aff] == '\n')
  {
    send_to_char("Unrecognized apply type, options are:\r\n", ch);
    olc_list_flags(ch, apply_types);
    return;
  }

  if (*arg2)
  {
    for (pos = 0; *wear_bits[pos] != '\n'; pos++)
      if (is_abbrev(arg2, wear_bits[pos]))
        break;

    if (*wear_bits[pos] == '\n')
    {
      send_to_char("Unrecognized position, options are:", ch);
      olc_list_flags(ch, wear_bits);
      return;
    }
  }
  else
  {
    send_to_char("Unrecognized position, options are:", ch);
    olc_list_flags(ch, wear_bits);
    return;
  }
  DYN_CREATE;
  *dynbuf = 0;

  for (object = 0; object <= top_of_objt; object++)
  {

    if (pos != -1 && !CAN_WEAR(&obj_proto[object], pos))
      continue;

    for (nr = 0; nr < MAX_OBJ_AFFECT; nr++)
    {
      if (obj_proto[object].affected[nr].location == aff)
      {
        count++;
        snprintf(buf, sizeof(buf),  "[%7d] ", obj_index[object].vnum);
        DYN_RESIZE(buf);
        snprintf(buf, sizeof(buf),  "Modifys %s by [",
                 apply_types[aff]);
        DYN_RESIZE(buf);
        size = obj_proto[object].affected[nr].modifier;
        if (size < 0)
          snprintf(buf, sizeof(buf), "{cb%d{c0", size);
        else
        {
          switch (size)
          {
          case 0:
            snprintf(buf, sizeof(buf), "{cr0{c0");
            break;
          case 1:
            snprintf(buf, sizeof(buf), "{cy1{c0");
            break;
          case 2:
            snprintf(buf, sizeof(buf), "{cg2{c0");
            break;
          case 3:
            snprintf(buf, sizeof(buf), "{cG3{c0");
            break;
          case 4:
            snprintf(buf, sizeof(buf), "{cC4{c0");
            break;
          default:
            snprintf(buf, sizeof(buf), "{cW%d{c0", size);
            break;
          }

        }
        DYN_RESIZE(buf);

        snprintf(buf, sizeof(buf), "] %s - ",
                 obj_proto[object].short_description);
        DYN_RESIZE(buf);
        sprintbitarray(obj_proto[object].obj_flags.wear_flags,
                       wear_bits, TW_ARRAY_MAX, buf, sizeof(buf));
        DYN_RESIZE(buf);

        snprintf(buf, sizeof(buf), "\r\n");
        DYN_RESIZE(buf);
      }

    }
  }

  if (!count)
  {
    snprintf(buf, sizeof(buf), "No items that affect that stat.\r\n");
    DYN_RESIZE(buf);
  }

  page_string(ch->desc, dynbuf, DYN_BUFFER);
}

ACMD(do_osnoop)
{
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];
  int aff = 0, limit = 0, pos = ITEM_WEAR_TAKE, nr, count = 0, object, size = 0;
  char buf[MAX_STRING_LENGTH];
  two_arguments(argument, arg1, arg2);

  if (!*arg1)
  {
    send_to_char
    ("You must specify which affect you want. Options are:\r\n",
     ch);
    olc_list_flags(ch, apply_types);
    return;
  }

  for (aff = 0; *apply_types[aff] != '\n'; aff++)
    if (is_abbrev(arg1, apply_types[aff]))
      break;

  if (*apply_types[aff] == '\n')
  {
    send_to_char("Unrecognized apply type, options are:\r\n", ch);
    olc_list_flags(ch, apply_types);
    return;
  }
  if (*arg2)
    limit = atoi(arg2);
  if (!limit)
    limit = 5;



  for (object = 0; object <= top_of_objt; object++)
  {

    if (pos != -1 && !CAN_WEAR(&obj_proto[object], pos))
      continue;

    for (nr = 0; nr < MAX_OBJ_AFFECT; nr++)
    {
      if (obj_proto[object].affected[nr].location == aff)
      {
        size = obj_proto[object].affected[nr].modifier;
        if (size < limit)
          continue;
        count++;
        new_send_to_char(ch, "[%7d] ", obj_index[object].vnum);
        new_send_to_char(ch, "Modifys %s by [",
                         apply_types[aff]);

        if (size < 0)
          new_send_to_char(ch, "{cb%d{c0", size);
        else
        {
          switch (size)
          {
          case 0:
            new_send_to_char(ch,"{cr0{c0");
            break;
          case 1:
            new_send_to_char(ch, "{cy1{c0");
            break;
          case 2:
            new_send_to_char(ch,"{cg2{c0");
            break;
          case 3:
            new_send_to_char(ch,"{cG3{c0");
            break;
          case 4:
            new_send_to_char(ch, "{cC4{c0");
            break;
          default:
            new_send_to_char(ch,"{cW%d{c0", size);
            break;
          }

        }

        new_send_to_char(ch, "] %s - ",
                         obj_proto[object].short_description);
        sprintbitarray(obj_proto[object].obj_flags.wear_flags,
                       wear_bits, TW_ARRAY_MAX, buf, sizeof(buf));

        new_send_to_char(ch, "%s\r\n", buf);
      }

    }
  }

  if (!count)
    send_to_char("No items that affect that stat.\r\n", ch);
}


int update_award(struct char_data *ch)
{

  if ((PLR_FLAGGED(ch, PLR_RP_LEADER) || PLR_FLAGGED(ch, PLR_HERO) )&& (SPECIALS(ch)->last_reward) != (find_month()))
  {
    SPECIALS(ch)->last_reward = (find_month());
    GET_REWARD(ch) = 15;
    GET_AWARD(ch)  = 15;
  }
  if (GET_LEVEL(ch) > LVL_IMMORT)
  {
    SPECIALS(ch)->last_reward = (find_month());
    GET_REWARD(ch) = 30;
    GET_AWARD(ch)  = 30;
  }
  return GET_AWARD(ch);
}

int update_reward(struct char_data *ch)
{
  if (PLR_FLAGGED(ch, PLR_HERO) && (SPECIALS(ch)->last_reward) != (find_month()))
  {
    SPECIALS(ch)->last_reward = (find_month());
    GET_REWARD(ch) = 15;
    GET_AWARD(ch)  = 15;
  }
  if (GET_LEVEL(ch) > LVL_IMMORT)
  {
    SPECIALS(ch)->last_reward = (find_month());
    GET_REWARD(ch) = 30;
    GET_AWARD(ch)  = 30;
  }
  return GET_REWARD(ch);
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
ACMD(do_reward)
{
  char arg[MAX_INPUT_LENGTH]; // for name, for points
  char arg2[MAX_INPUT_LENGTH];
  struct char_data *vict;
  int pts;
  new_send_to_char(ch, "Use AWARD please.\r\n");
  return;
  if (IS_NPC(ch))
    return;

  if (PLR_FLAGGED(ch, PLR_HERO) ||  (GET_LEVEL(ch) > LVL_IMMORT))
  {

    if (update_reward(ch) == 0)
    {
      new_send_to_char(ch, "You don't have enough points! \r\nPoints are refreshed every 17 hours.\r\n");
      return;
    }
    argument = two_arguments(argument, arg, arg2);
    if (!*arg || !*arg2)
    {
      new_send_to_char(ch, "Usage: reward <amount> <playername>\r\n");
      return;
    }
    if (!(vict = get_player_vis(ch, arg2, NULL, 0)))
    {
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
    if (PLR_FLAGGED(vict, PLR_HERO))
    {
      new_send_to_char(ch, "You can only reward mortals.\r\n");
      return;
    }

    if ((pts = atoi(arg)) <= 0)
    {
      new_send_to_char(ch, "Invalid number. Try something more then 0.\r\n");
      return;
    }
    if (pts > update_reward(ch))
    {
      new_send_to_char(ch, "Invalid number. Try something less then %d.\r\n", GET_REWARD(ch));
      return;
    }

    new_send_to_char(vict, "%s showers you in praise and rewards you %d point%s!", GET_NAME(ch), pts, pts > 1 ? "s" : "");
    new_send_to_char(ch, "You shower %s in praise and reward %d point%s!", GET_NAME(vict), pts, pts > 1 ? "s" : "");
    if ( (GET_LEVEL(ch) < LVL_IMMORT))
      GET_REWARD(ch)   -= pts;
    GET_REWARD(vict) += pts;

  }
  else
  {
    new_send_to_char(ch, "You will be able to use your points soon.\r\n");
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
ACMD(do_award)
{
  char arg[MAX_INPUT_LENGTH]; // for name, for points
  char arg2[MAX_INPUT_LENGTH];
  struct char_data *vict;
  int pts;

  if (IS_NPC(ch))
    return;

  if (PLR_FLAGGED(ch, PLR_HERO) ||  (GET_LEVEL(ch) > LVL_IMMORT) || PLR_FLAGGED(ch, PLR_RP_LEADER))
  {

    if (update_award(ch) == 0)
    {
      new_send_to_char(ch, "You don't have enough points! \r\nPoints are refreshed every 17 hours.\r\n");
      return;
    }
    argument = two_arguments(argument, arg, arg2);
    if (!*arg || !*arg2)
    {
      new_send_to_char(ch, "Usage: award <amount> <playername>\r\n");
      return;
    }
    if (!(vict = get_player_vis(ch, arg2, NULL, 0)))
    {
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
    if (PLR_FLAGGED(vict, PLR_HERO))
    {
      new_send_to_char(ch, "You can only award mortals.\r\n");
      return;
    }
    if ((pts = atoi(arg)) <= 0)
    {
      new_send_to_char(ch, "Invalid number. Try something more then 0.\r\n");
      return;
    }
    if (pts > update_award(ch))
    {
      new_send_to_char(ch, "Invalid number. Try something less then %d.\r\n", GET_AWARD(ch));
      return;
    }

    new_send_to_char(vict, "%s showers you in praise and awards you %d point%s!\r\n", GET_NAME(ch), pts, pts > 1 ? "s" : "");
    new_send_to_char(ch, "You shower %s in praise and award %d point%s!\n\n", GET_NAME(vict), pts, pts > 1 ? "s" : "");

    if (GET_LEVEL(ch) < LVL_IMMORT)
      GET_AWARD(ch)   -= pts;
    GET_AWARD(vict) += pts;


  }
  else
  {
    new_send_to_char(ch, "You will be able to use your award points soon.\r\n");
  }
}


/*finish me*/
int mortal_player_info(struct char_data *ch, struct char_data *vict)
{
  char buf1[MAX_INPUT_LENGTH];

  if (GET_LEVEL(vict)<LVL_IMMORT)
  {
    strcpy(buf1, (char *) asctime(localtime(&(vict->player.time.logon))));
    buf1[10] = '\0';

    new_send_to_char(ch,
                     "\r\nLast Logon: [%s], Played [%dh %dm], Age [%d]\r\n",
                     buf1, vict->player.time.played / 3600,
                     ((vict->player.time.played % 3600) / 60), age(vict)->year);
  }

  return 1;
}
ACMD(do_ps_aux)
{
  char line[MAX_INPUT_LENGTH];
  FILE *fl;

  system("ps aux | grep circle > psaux");

  if ((fl = fopen("psaux", "r")) == NULL)
  {
    new_send_to_char(ch, "No file.\r\n");
    return;
  }

  while (get_line(fl, line))
  {
    new_send_to_char(ch, "%s\r\n", line);
  }

  fclose(fl);

}
ACMD(do_get_free_mem)
{
  char line[MAX_INPUT_LENGTH];
  FILE *fl;

  system("free -m > freemem");

  if ((fl = fopen("freemem", "r")) == NULL)
  {
    new_send_to_char(ch, "No file.\r\n");
    return;
  }

  while (get_line(fl, line))
  {
    new_send_to_char(ch, "%s\r\n", line);
  }

  fclose(fl);

}

void change_plrindex_name(long id, char *change)
{
  int tp;

  if (!change)
    return;
  for (tp = 0; tp <= top_of_p_table; tp++)
  {
    if (player_table[tp].id == id)
    {
      if (player_table[tp].name)
        free(player_table[tp].name);
      player_table[tp].name = strdup(change);
      save_player_index();
      return;
    }
  }
}

ACMD(do_namechange)
{
  DESCRIPTOR_DATA *d;
  struct char_data *tch = NULL;
  int loaded = 0;
  char newname[MAX_INPUT_LENGTH], oldname[MAX_INPUT_LENGTH], passw[MAX_INPUT_LENGTH];

  argument = two_arguments(argument, oldname, newname);
  one_argument(argument, passw);

  if (GET_LEVEL(ch) < LVL_SEN)
  {
    new_send_to_char(ch, "Que?\r\n");
    return;
  }

  if ((!*oldname || !*newname || !*passw) )
  {
    new_send_to_char(ch, "namechange <oldname> <newname> <newpassword>\r\n");
    return;
  }

  if ( (strlen(newname) >= MAX_NAME_LENGTH) || (strlen(newname) < 3))
  {
    new_send_to_char(ch, "Sorry but that name is the wrong length\r\n");
    return;
  }
  newname[0] = UPPER(newname[0]);



  for (d = descriptor_list; d; d = d->next)
  {
    if (!IS_PLAYING(d))
    {
      if (compares(GET_NAME(d->character), oldname))
      {
        new_send_to_char(ch, "This player is in a state that can't be changed just yet.\r\n");
        return;
      }
    }
    else if (compares(GET_NAME(d->character), oldname))
    {
      tch = d->character;
      if (GET_LEVEL(tch) >= GET_LEVEL(ch))
      {
        new_send_to_char(ch, "Ah, Bugger off!\r\n");
        return;
      }
      break;
    }
  }
  if (!tch)
  {
    struct kill_data *load_killlist(char *name);
    void load_locker(CHAR_DATA *ch);

    if (get_id_by_name(oldname) == -1)
    {
      new_send_to_char(ch, "A player by that name doesn't exist here.\r\n");
      return;
    }
    CREATE(tch, struct char_data, 1);
    if (load_char(oldname, tch) <= 0)
    {
      free(tch);
      log("load char error in namechange");
      return;
    }
    if (GET_LEVEL(tch) >= GET_LEVEL(ch))
    {
      new_send_to_char(ch, "Ah, Bugger off!\r\n");
      return;
    }
    reset_char(tch);
    read_aliases(tch);
    GET_ID(ch) = GET_IDNUM(ch);// = player_table[id].id;
    add_to_lookup_table(GET_IDNUM(ch), (void *)ch);

    char_to_room(tch, IN_ROOM(ch));
    GET_KILLS(tch) = load_killlist(GET_NAME(tch));
    load_locker(ch);
    add_char_to_list(ch);
    loaded = 1;
  }

  free_string(&tch->player.name);
  tch->player.name = str_dup(newname);
  newname[0] = LOWER(newname[0]);
  strlcpy(GET_PASSWD(tch), CRYPT(passw, GET_PC_NAME(tch)), MAX_PWD_LENGTH);
  change_plrindex_name(GET_IDNUM(tch), newname);
  save_char(tch);
  if (!loaded)
    Crash_crashsave(tch);
  write_aliases(tch);
  write_ignorelist(tch);

  new_send_to_char(ch, "%s's name changed to %s.\r\n", oldname, newname);
  if (!loaded)
    new_send_to_char(tch, "{cYYour name has been changed to %s. It's best if you quit and reenter to save.\r\n{c0", newname);

  if (loaded)
  {
    Crash_rentsave(tch, 0);
    extract_char(tch);
  }
}

ACMD(do_decrypt)
{
  char arg1[256], arg2[256];
  argument = one_arg(argument, arg1);
  argument = one_arg(argument, arg2);
  //argument = two_arguments(argument, arg1, arg2);
  new_send_to_char(ch, "Arg1: %s Arg2: %s\r\n", arg1, arg2);
  new_send_to_char(ch, "1: %s - %s - %d\r\n", CRYPT(arg1, arg2), CRYPT(arg1,CRYPT(arg1, arg2)),!strncmp(CRYPT(arg1, arg2), arg2, MAX_PWD_LENGTH));
  new_send_to_char(ch, "2: %s - %s - %d\r\n", CRYPT(arg2, arg1), CRYPT(arg1,CRYPT(arg2, arg1)),!strncmp(CRYPT(arg2, arg1), arg1, MAX_PWD_LENGTH));
}

void list_destinations(struct travel_point_data *travel_list, struct char_data *ch)
{
  size_t len = 0;
  struct travel_point_data *t;
  if (!travel_list)
    return;
  new_send_to_char(ch, "Destination points (@ was last point reached):\r\n");
  for (t = travel_list; t; t = t->next)
  {
    len += new_send_to_char(ch, "%c%d ", t->last_stop ? '@' : ' ', t->dest);
    if (len > 70)
    {
      len = 0;
      new_send_to_char(ch, "\r\n");
    }
  }
  new_send_to_char(ch, "\r\n");
}

ACMD(do_deleteplayer)
{

  char buf2[MAX_INPUT_LENGTH];
  one_argument(argument, buf2);
  ch->loader = -1;
  if (!*buf2)
  {
    new_send_to_char(ch,"Delete which player?\r\n");
    return;
  }
  else if (!(ch->loader = get_id_by_name(buf2)))
  {
    send_to_char("There is no such player.\r\n", ch);
    return;
  }
  new_send_to_char(ch, "{cYAre you ABSOLUTELY certain you want to delete {cR%s{cY?{c0\r\n\r\n{cgIf you are certain, type: 'yes I am' --:{c0", buf2);
  line_input(ch->desc, "", delete_player, NULL);

}

C_FUNC(delete_player)
{
  struct char_data *tch = d->character;
  char *charname;

  if (!tch)
    return;
  charname = get_name_by_id(tch->loader);

  if (arg && *arg && !strcmp(arg, "yes I am"))
  {
    write_to_output(d, "Deleting...\r\n");
    perform_delete_player(charname);
    write_to_output(d, "\r\n...Done\r\n");
  }
  else
  {
    write_to_output(d, "You cancel the delete on %s.\r\n", charname);
  }
}

void perform_delete_player(char *charname)
{
;
  int player_i;
  Crash_delete_file(charname);
  delete_pobj_file(charname);
  delete_aliases(charname);
  delete_variables(charname);
  if ((player_i = find_name(charname)) >= 0)
  {
    SET_BIT(player_table[player_i].flags, PINDEX_SELFDELETE);
    remove_player(player_i);
  }

}

void show_door_errors(struct char_data *ch)
{
  int i, door;
  int found = 0;
  obj_vnum vkey;
  obj_rnum rkey;
  char buf[MAX_STRING_LENGTH];
  DYN_DEFINE;
  DYN_CREATE;
  for (i = 0; i < top_of_world; i++)
  {
    if (world_vnum[i] == NULL)
      continue;

    for (door = 0; door < NUM_OF_DIRS; door++)
    {
      if (world_vnum[i]->dir_option[door])
      {
        vkey = world_vnum[i]->dir_option[door]->key;
        if (vkey > 0)
        {
          if ((rkey = real_object(vkey)) != NOTHING)
          {
            if (GET_OBJ_TYPE(obj_proto + rkey) != ITEM_KEY)
            {
              found++;
              snprintf(buf, sizeof(buf), "Room [%5d] Dir [%5s] Key [%5d] - {cCKEY is not of type key (%s).{c0\r\n", i, dirs[door], vkey, item_types[(int)GET_OBJ_TYPE(obj_proto + rkey)]);
              DYN_RESIZE(buf);
            }
          }
          else
          {
            found++;
            snprintf(buf, sizeof(buf), "Room [%5d] Dir [%5s] Key [%5d] - {cRKEY vnum does not exist.{c0\r\n", i, dirs[door], vkey);
            DYN_RESIZE(buf);
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
  if (!found)
  {
    sprintf(buf, "No door errors found!\r\n");
    DYN_RESIZE(buf);
  }
  page_string(ch->desc, dynbuf, DYN_BUFFER);
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
int hilite(const char *regex, const char *str, char *buf, size_t buflen)
{
  char *p, *s = (char *)str;
  size_t tmp = 0;
  size_t u = strlen(UND);
  bool found = FALSE;
  int cnt = strlen(regex);
  if ((p = strstr(str, regex)) == NULL) return 0;

  while ( *s  && tmp < buflen)
  {
    if ( s == p )
    {
      if (tmp + u < buflen)
      {
        snprintf(buf + tmp, buflen - tmp, "%s", UND);
        tmp += u;
        found = TRUE;
      }
    }
    if (found)
    {
      if (!cnt--)
      {
        if (tmp + u < buflen)
        {
          snprintf(buf + tmp, buflen - tmp, "%s", REM);
          tmp += u;
          found = FALSE;
        }

      }
    }

    buf[tmp++] = *(s++);
  }


  buf[tmp] = '\0';

  return 1;
}

#define T_ARG 0
#define T_BOD 1
#define T_NAM 2
#define T_ALL 3

int search_one_trig(int stype, struct trig_data *trig, char *buf, size_t len, int vnum, const char *phrase)
{
  
  size_t nlen = 0;
  int found = 0, count = 0;
  char strtmp2[MAX_STRING_LENGTH];
  count = snprintf(buf + nlen, len - nlen, "\r\n%sVnum: %-7d -- Name: %-50s%s\r\n",GRN, vnum, trig->name, REM);
  if (count > 0)
    nlen += count;

  if ((stype == T_NAM || stype == T_ALL) && trig->name)
  {
    if (hilite(phrase, (const char *)trig->name, strtmp2, sizeof(strtmp2)))
    {
      count = snprintf(buf + nlen, len - nlen, "      %s[NAME]%s %s%s\r\n",CYN, REM, strtmp2, REM);
      if (count >= 0)
        nlen += count;
      found++;
    }

  }
  if ((stype == T_ARG || stype == T_ALL) && trig->arglist)
  {
    if (hilite(phrase, (const char *)trig->arglist, strtmp2, sizeof(strtmp2)))
    {
      count = snprintf(buf + nlen, len - nlen, "      %s[ARGS]%s %s%s\r\n",GRN, REM, strtmp2, REM);
      if (count >= 0)
        nlen += count;
      found++;
    }

  }
  if ((stype == T_BOD || stype == T_ALL) && trig->cmdlist)
  {
    int n = 0;
    struct cmdlist_element *c;
    for (c = trig->cmdlist; c; c=c->next)
    {
      n++;
      if (hilite(phrase, c->cmd, strtmp2, sizeof(strtmp2)))
      {
        count = snprintf(buf + nlen, len - nlen, "      %s[BODY]%s Line %3d: %s%s\r\n",YEL, REM, n, strtmp2, REM);
        if (count >= 0)
          nlen += count;
        found++;
      }
    }

  }


  return found;
}
ACMD(do_search_triggers)
{
  int i;
  int found = 0;
  int stype;
  char buf[MAX_STRING_LENGTH];
  DYN_DEFINE;
  DYN_CREATE;

  argument = one_argument(argument, buf);
  skip_spaces(&argument);
  if (!*buf || !*argument)
  {
    new_send_to_char(ch, "TSEARCH <ALL | BODY | NAME | ARGS> <phrase to find>\r\n");
    return;
  }
  if (isname("all", buf))
  {
    stype = T_ALL;
  }
  else if (isname("body", buf))
  {
    stype = T_BOD;
  }
  else if (isname("name", buf))
  {
    stype = T_NAM;
  }
  else if (isname("args", buf))
  {
    stype = T_ARG;
  }
  else
  {
    new_send_to_char(ch, "TSEARCH <ALL | BODY | NAME | ARGS> <phrase to find>\r\n");
    return;
  }


  for (i = 0; i < top_of_trigt; i++)
  {
    if (trig_index[i]->proto)
    {
      if (search_one_trig(stype, trig_index[i]->proto, buf, sizeof(buf), trig_index[i]->vnum, (const char*)argument))
      {
        DYN_RESIZE(buf);
        found ++;
      }
    }

  }
  if (!found)
    sprintf(buf, "No triggers found!\r\n");
  else
    sprintf(buf, "%d triggers found!\r\n", found);
  DYN_RESIZE(buf);
  page_string(ch->desc, dynbuf, DYN_BUFFER);
}

ACMD(do_ctellsnoop){
   char arg[MAX_STRING_LENGTH];
   int num;
   if(GET_LEVEL(ch)!=LVL_IMPL) {
      new_send_to_char(ch,"Only imps are allowed to use this command!\r\n");
      return;
   }
   one_argument(argument,arg);
   if(isdigit(*arg) || *arg=='-'){
      if((num=atoi(arg))<0 || num>=num_of_clans){
         new_send_to_char(ch,"That is not a valid clannumber.\r\n");
         return;
      }
      GET_CSNP_LVL(ch)=num;
      new_send_to_char(ch,"Okay, you are now listening to the %s ctell.\r\n",clan[num].name);
   }
   else if(!strcmp(arg,"none")) {
      GET_CSNP_LVL(ch)=-2;
      new_send_to_char(ch,"Okay, you aren't snooping the ctells anymore.\r\n");
   }
   else if(!strcmp(arg,"all")){
      GET_CSNP_LVL(ch)=-1;
      new_send_to_char(ch,"Okay, you are now listening to all the ctells.\r\n");
   }
   else {
      char buf3[50];
      if(GET_CSNP_LVL(ch)==-2) snprintf(buf3,50,"None");
      else if(GET_CSNP_LVL(ch)==-1) snprintf(buf3,50,"All");
      else if(GET_CSNP_LVL(ch)>=0 && GET_CSNP_LVL(ch)<=num_of_clans) sprintf(buf3,clan[GET_CSNP_LVL(ch)].name);
      new_send_to_char(ch,"Usage: csnoop clannumber/none/all.\r\nCurrent setting: %s.\r\n",buf3);
   }
}

ACMD(do_addtp)
{
  char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
  char_data *vict;
  int amount, orig_tp;
  two_arguments(argument,arg1,arg2);
  vict = get_player_vis(ch, arg1, NULL, FIND_CHAR_WORLD);
  if(!vict)
  {
    new_send_to_char(ch,"There is no player around with that name.\r\n");
    return;
  }

  if(!isdigit(*arg2) && !(*arg2=='-' && isdigit(*(arg2+sizeof(char)))))
  {
    new_send_to_char(ch,"That is not a number.\r\n");
    return;
  }
  amount=atoi(arg2);
  orig_tp=TRADEPOINTS(vict);
  TRADEPOINTS(vict)+=amount;
  if(TRADEPOINTS(vict)<orig_tp && *arg2!='-')
  {
    TRADEPOINTS(vict)=orig_tp;
    new_send_to_char(ch,"The new amount of tradepoints would be too high, so it can't be done.\r\n");
    return;
  }
  if(TRADEPOINTS(vict)<0 || (TRADEPOINTS(vict)>orig_tp && *arg2=='-'))
  {
    TRADEPOINTS(vict)=orig_tp;
    new_send_to_char(ch,"%s doesn't have enough tradepoints for that!\r\n", GET_NAME(vict));
    return;
  }
  new_mudlog(CMP, MAX(LVL_SEN, GET_INVIS_LEV(ch)), TRUE, "%s gives %s %d tradepoints.",  GET_NAME(ch), GET_NAME(vict), amount);
  new_send_to_char(ch,"Amount of tradepoints is now %d (was: %d).\r\n",TRADEPOINTS(vict),orig_tp);
  
}
