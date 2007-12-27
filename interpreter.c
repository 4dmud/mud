/**************************************************************************
*   File: interpreter.c                                 Part of CircleMUD *
*  Usage: parse user commands, search for specials, call ACMD functions   *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#define __INTERPRETER_C__

#include "conf.h"
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

extern const char *MENU;


extern char *motd;
extern char *imotd;
extern char *background;
extern int top_of_p_table;
extern int circle_restrict;
extern int no_specials;
extern const int xap_objs;
extern int backup_wiped_pfiles;
extern int selfdelete_fastwipe;
extern int TEMP_LOAD_CHAR;
extern char last_command[];
extern const char compress_offer[];

/* external functions */
int delete_pobj_file(char *name);
void echo_on(struct descriptor_data *d);
void echo_off(struct descriptor_data *d);
void do_start(struct char_data *ch);
int special(struct char_data *ch, int cmd, char *arg);
int isbanned(char *hostname);
void read_saved_vars(struct char_data *ch);
int Valid_Name(char *newname);
void roll_real_abils(struct char_data *ch);
void oedit_parse(struct descriptor_data *d, char *arg);
void redit_parse(struct descriptor_data *d, char *arg);
//void hedit_parse(struct descriptor_data *d, char *arg);
void zedit_parse(struct descriptor_data *d, char *arg);
void medit_parse(struct descriptor_data *d, char *arg);
void sedit_parse(struct descriptor_data *d, char *arg);
void trigedit_parse(struct descriptor_data *d, char *arg);
void aedit_parse(struct descriptor_data *d, char *arg);
void read_aliases(struct char_data *ch);
void read_poofs(struct char_data *ch);
extern int Crash_load(struct char_data *ch);
void remove_player(int pfilepos);
void load_locker(CHAR_DATA *ch);
int count_locker(CHAR_DATA *ch);
long get_acc_by_name(char *name);
long get_acc_by_id(long id);
void default_char(struct char_data *ch);

void perform_wear(struct char_data *ch, struct obj_data *obj, int where);
/* local functions */
int perform_dupe_check(struct descriptor_data *d);
struct alias_data *find_alias(struct alias_data *alias_list, char *str);
void free_alias(struct alias_data *a);
void perform_complex_alias(struct txt_q *input_q, char *orig,
                           struct alias_data *a);
int perform_alias(struct descriptor_data *d, char *orig, size_t maxlen);
int reserved_word(char *argument);
int find_name(char *name);
int _parse_name(char *arg, char *name);
int get_account_num(int num, long acc);
int has_note(CHAR_DATA *ch, int type);
void con_character_creation(DESCRIPTOR_DATA *d, char *arg);

/* local global vars */

int race_val = 0;
int class_val = 0;
int total_commands_typed = 0;
int total_pcommands_typed = 0;
int total_trig_commands_typed = 0;

/* prototypes for all do_x functions. */
ACMD(do_action);
ACMD(do_advance);
ACMD(do_aedit);
ACMD(do_ahall);
ACMD(do_affects);
ACMD(do_alias);
ACMD(do_arena);
ACMD(do_assist);
ACMD(do_astat);
ACMD(do_at);
ACMD(do_atlvl);
ACMD(do_auction);
ACMD(do_autowiz);
ACMD(do_awho);
ACMD(do_ban);
ACMD(do_bet);
ACMD(do_bid);
ACMD(do_blowup);
ACMD(do_buck);
ACMD(do_bury);
ACMD(do_cast);
ACMD(do_chaos);
ACMD(do_clan);
ACMD(do_climb);
ACMD(do_color);
ACMD(do_copyover);
ACMD(do_commands);
ACMD(do_comm);
ACMD(do_compare);
ACMD(do_consider);
ACMD(do_convert);
ACMD(do_convey);
ACMD(do_credits);
ACMD(do_ctell);
ACMD(do_date);
ACMD(do_dc);
ACMD(do_deduct);
ACMD(do_descend);
ACMD(do_diagnose);
ACMD(do_dig);
ACMD(do_dig_ground);
ACMD(do_dismount);
ACMD(do_display);
ACMD(do_drink);
ACMD(do_drive);
ACMD(do_drop);
ACMD(do_eat);
ACMD(do_echo);
ACMD(do_edit);		/* Mainly intended as a test function. */
ACMD(do_enter);
ACMD(do_equipment);
ACMD(do_examine);
ACMD(do_exit);
ACMD(do_exits);
ACMD(do_feel);
ACMD(do_file);
ACMD(find_nugget);
ACMD(do_finger);
ACMD(do_flee);
ACMD(do_follow);
ACMD(do_force);
ACMD(do_gecho);
ACMD(do_gen_comm);
ACMD(do_gen_door);
ACMD(do_gen_ps);
ACMD(do_gen_tog);
ACMD(do_gen_write);
ACMD(do_get);
ACMD(do_give);
ACMD(do_gold);
ACMD(do_goto);
ACMD(do_grab);
ACMD(do_group);
ACMD(do_gsay);
ACMD(do_hcontrol);
ACMD(do_heal);
ACMD(do_help);
ACMD(do_hit);
ACMD(do_house);
ACMD(do_ignore);
ACMD(do_info);
ACMD(do_innate);
ACMD(do_insult);
ACMD(do_inventory);
ACMD(do_invis);
ACMD(do_jump);
ACMD(do_kick);
ACMD(do_last);
ACMD(do_leave);
ACMD(do_levels);
ACMD(do_linkload);
ACMD(do_listen);
ACMD(do_load);
ACMD(do_look);
ACMD(do_map);
ACMD(do_mine);
ACMD(do_not_here);
ACMD(do_objconv);
ACMD(do_objdump);
ACMD(do_offer);
ACMD(do_olc);
ACMD(do_oasis);
ACMD(do_olist);
ACMD(do_order);
ACMD(do_owners);
ACMD(do_page);
ACMD(do_peace);
ACMD(do_poofset);
ACMD(do_pour);
ACMD(do_powerplay);
ACMD(do_practice);
ACMD(do_prereq);
ACMD(do_purge);
ACMD(do_pull);
ACMD(do_put);
ACMD(do_qcomm);
ACMD(do_qicinfo);
ACMD(do_qicsave);
ACMD(do_qload);
ACMD(do_quit);
ACMD(do_race);
ACMD(do_rdig);
ACMD(do_reboot);
ACMD(do_recover);
ACMD(do_register);
ACMD(do_reload);
ACMD(do_remove);
ACMD(do_remort);
ACMD(do_reply);
ACMD(do_report);
ACMD(do_rest);
ACMD(do_restore);
ACMD(do_retreat);
ACMD(do_return);
ACMD(do_room_copy);
ACMD(do_rlist);
ACMD(do_sac);
ACMD(do_save);
ACMD(do_saveall);
ACMD(do_say);
ACMD(do_score);
ACMD(do_search);
ACMD(do_send);
ACMD(do_set);
ACMD(do_setqic);
ACMD(do_settime);
ACMD(do_show);
ACMD(do_shoot);
ACMD(do_shutdown);
ACMD(do_sit);
ACMD(do_skills);
ACMD(do_skillset);
ACMD(do_slay);
ACMD(do_sleep);
ACMD(do_slist);
ACMD(do_smell);
ACMD(do_smite);
ACMD(do_snoop);
ACMD(do_spec_comm);
ACMD(do_split);
ACMD(do_stand);
ACMD(do_stat);
ACMD(do_statlist);
ACMD(do_string);
ACMD(do_swap);
ACMD(do_switch);
ACMD(do_syslog);
ACMD(do_taste);
ACMD(do_tedit);
ACMD(do_teleport);
ACMD(do_throw);
ACMD(do_tell);
ACMD(do_time);
ACMD(do_title);
ACMD(do_toggle);
ACMD(do_trans);
ACMD(do_transfer);
ACMD(do_trust);
ACMD(do_unban);
ACMD(do_ungroup);
ACMD(do_use);
ACMD(do_users);
ACMD(do_visible);
ACMD(do_vnum);
ACMD(do_vstat);
ACMD(do_wake);
ACMD(do_wear);
ACMD(do_weather);
ACMD(do_where);
ACMD(do_who);
ACMD(do_wield);
ACMD(do_wimpy);
ACMD(do_wizlock);
ACMD(do_wiznet);
ACMD(do_wizutil);
ACMD(do_worth);
ACMD(do_write);
ACMD(do_zdelete);
ACMD(do_zlist);
ACMD(do_zreset);

/* Romance Module */
ACMD(do_askout);
ACMD(do_accept);
ACMD(do_reject);
ACMD(do_propose);
ACMD(do_breakup);
ACMD(do_marry);
ACMD(do_divorce);
/* End Romance Module (7 commands) */
/* MatingMod */
ACMD(do_seduce);
ACMD(do_consent);
ACMD(do_deny);
ACMD(do_abort);
ACMD(mate_toggle);
/* End Mating Module (5 commands) */

/* DG Script ACMD's */
ACMD(do_attach);
ACMD(do_detach);
ACMD(do_masound);
ACMD(do_mat);
ACMD(do_mdamage);
ACMD(do_mcollision);
ACMD(do_mdoor);
ACMD(do_mecho);
ACMD(do_mechoaround);
ACMD(do_mforce);
ACMD(do_mforget);
ACMD(do_mgoto);
ACMD(do_mhunt);
ACMD(do_mkill);
ACMD(do_mjunk);
ACMD(do_mload);
ACMD(do_mpdelayed);
ACMD(do_mpurge);
ACMD(do_mremember);
ACMD(do_msend);
ACMD(do_mslay);
ACMD(do_msteal);
ACMD(do_mteleport);
ACMD(do_mtransform);
ACMD(do_mzecho);
ACMD(do_mzrecho);
ACMD(do_mzreset);
ACMD(do_tlist);
ACMD(do_tstat);
ACMD(do_vdelete);
ACMD(do_mlag);
ACMD(do_mrecho);

ACMD(set_perc);
ACMD(forest_find);
ACMD(do_ipstat);
ACMD(do_dam_dice);
ACMD(do_tiername);
ACMD(do_leader);
ACMD(do_statinnate);
ACMD(do_saveall); //??
ACMD(do_speedwalk);
ACMD(do_subskill);
ACMD(do_osnoop);
ACMD(do_ignite);
ACMD(do_fell);
ACMD(do_subskillset);
ACMD(do_skilllist);
ACMD(do_subdisplay);
ACMD(do_spellinfo);
ACMD(do_pretitle);
ACMD(do_task);
ACMD(do_potionweight);
ACMD(do_professions);
ACMD(do_struggle);
ACMD(do_clear_buffer);
ACMD(do_reward);
ACMD(do_award);

ACMD(do_note);
ACMD(do_idea);
ACMD(do_news);
ACMD(do_changes);
ACMD(do_penalty);
ACMD(do_password);
ACMD(do_hackinvis);
ACMD(do_oasis_trigedit);
ACMD(do_prompt_new);
ACMD(do_topgold);
ACMD(do_fusion);
ACMD(do_fuse);
ACMD(do_get_free_mem);
ACMD(do_ps_aux);
ACMD(do_corpse);
ACMD(do_own);
ACMD(do_prac_spells);
ACMD(do_prac_skills);
ACMD(do_pageheight);
ACMD(do_pagewidth);
ACMD(do_hostfind);
ACMD(do_namechange);
ACMD(do_decrypt);
ACMD(do_locker);
ACMD(do_sayto);
ACMD(do_die);
ACMD(do_meld);
ACMD(do_account);
ACMD(do_fightmsg);
ACMD(do_killlist);
ACMD(do_recall);

void line_sep(DESCRIPTOR_DATA *d);
void con_disp_menu(DESCRIPTOR_DATA *d);


cpp_extern struct command_info *complete_cmd_info;

/* This is the Master Command List(tm).
 
 * You can put new commands in, take commands out, change the order
 * they appear in, etc.  You can adjust the "priority" of commands
 * simply by changing the order they appear in the command list.
 * (For example, if you want "as" to mean "assist" instead of "ask",
 * just put "assist" above "ask" in the Master Command List(tm).
 *
 * In general, utility commands such as "at" should have high priority;
 * infrequently used and dangerously destructive commands should have low
 * priority.
 */
cpp_extern const struct command_info cmd_info[] =
  {
    { "RESERVED" , "", 0, 0, 0, 0 , 0}
    ,	/* this must be first -- for specprocs */

    /* directions must come before other commands but after RESERVED */
    { "north"    , "n"    , POS_STANDING, do_move     , 0, SCMD_NORTH, 0 },
    { "east"     , "e"	, POS_STANDING, do_move     , 0, SCMD_EAST, 0 },
    { "south"    , "s"	, POS_STANDING, do_move     , 0, SCMD_SOUTH, 0 },
    { "west"     , "w"	, POS_STANDING, do_move     , 0, SCMD_WEST, 0 },
    { "up"       , "u"	, POS_STANDING, do_move     , 0, SCMD_UP, 0 },
    { "down"     , "d"	, POS_STANDING, do_move     , 0, SCMD_DOWN, 0 },

    /* now, the main list */
    { "--"      , "--"  , POS_DEAD    , do_clear_buffer  , 0, 0, 0 },
    { "afk"      , "afk"  , POS_DEAD    , do_gen_tog  , 0, SCMD_AFK, 0 },
    { "afktell" , "afkt"	, POS_DEAD    , do_gen_tog  , 0, SCMD_AFKTELL, 0 },
    { "at"       , "at"	, POS_DEAD    , do_at       , LVL_IMMORT, 0, WIZ_IMM1_GRP },
    { "atlev"    , "atlev", POS_DEAD    , do_atlvl    , LVL_IMMORT, 0, WIZ_IMM1_GRP },
    { "advance"  , "adv"	, POS_DEAD    , do_advance  , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "aedit"    , "aed"	, POS_DEAD    , do_oasis      , LVL_IMMORT, SCMD_OASIS_AEDIT, WIZ_EDIT_GRP },
    { "astat"    , "ast"	, POS_DEAD    , do_astat      , LVL_IMMORT, SCMD_OASIS_AEDIT, WIZ_EDIT_GRP },
    { "affects"  , "aff"  , POS_RESTING , do_affects  , 0, 0, 0 },
    { "aggro"      , "aggro"  , POS_DEAD    , do_gen_tog  , 0, SCMD_AGGRO, 0 },
    { "ahall"    , "ahall", POS_DEAD    , do_ahall    , 0, 0, 0 },
    { "alias"    , "ali"	, POS_DEAD    , do_alias    , 0, 0, 0 },
    { "answer"   , "answ" , POS_SLEEPING, do_gen_comm , 0, SCMD_NEWBIE1, 0 },
    { "arena"    , "arena", POS_STANDING, do_arena    , 0, 0, 0 },
    { "arinfo"   , "arinf", POS_SLEEPING, do_gen_tog  , 0, SCMD_ARENA, 0 },
    { "assist"   , "as"	, POS_FIGHTING, do_assist   , 1, 0, 0 },
    { "ask"      , "ask"	, POS_RESTING , do_spec_comm, 0, SCMD_ASK, 0 },
    { "auction"  , "auc"  , POS_RESTING , do_auction  , 1, 0, 0 },
    { "auctalk"  , "auct"	, POS_SLEEPING, do_gen_comm , 0, SCMD_AUCTION, 0 },
    { "autoassist","autoa", POS_DEAD    , do_gen_tog  , 0, SCMD_AUTOASSIST, 0 },
    { "autocompress","autoc", POS_DEAD    , do_gen_tog  , 0, SCMD_AUTOZLIB, 0 },
    { "autoexit" , "autoe", POS_DEAD    , do_gen_tog  , 0, SCMD_AUTOEXIT, 0 },
    { "autogold" , "autog", POS_DEAD    , do_gen_tog  , 0, SCMD_AUTOGOLD, 0 },
    { "autosplit", "autos", POS_DEAD    , do_gen_tog  , 0, SCMD_AUTOSPLIT, 0 },
    { "autoloot" , "autol", POS_DEAD    , do_gen_tog  , 0, SCMD_AUTOLOOT, 0 },
    { "autosac"  , "autosa", POS_DEAD    , do_gen_tog  , 0, SCMD_AUTOSAC, 0 },
    { "autowiz"  , "autow", POS_DEAD    , do_autowiz  , LVL_IMPL, 0, 0 },
    { "awho"     , "awho" , POS_DEAD    , do_awho     , 0, 0, 0 },
    { "award"     , "awa" , POS_DEAD    , do_award     , 0, 0, 0 },
    { "account"     , "account" , POS_DEAD    , do_account     , 0, 0, 0 },


    { "ban"      , "ban"	, POS_DEAD    , do_ban      , LVL_IMMORT, 0, WIZ_BAN_GRP },
    { "balance"  , "bal"	, POS_STANDING, do_not_here , 1, 0, 0 },
    { "bet"      , "bet"  , POS_DEAD    , do_not_here , 1, 0, 0 },
    { "bid"      , "bid"  , POS_RESTING , do_bid      , 1, 0, 0 },
    { "blowup"   , "blow" , POS_STANDING, do_blowup   , LVL_IMPL, 0, WIZ_IMPL_GRP},
    { "brief"    , "br"   , POS_DEAD    , do_gen_tog  , 0, SCMD_BRIEF, 0 },
    { "buildwalk", "buildwalk", POS_STANDING, do_gen_tog,   LVL_IMMORT, SCMD_BUILDWALK, WIZ_OLC_GRP },
    { "buck"     , "buck" , POS_STANDING, do_buck     , 0, 0, 0 },
    { "bury"     , "bury" , POS_STANDING, do_bury     , 0, 0, 0 },
    { "busy"     , "busy" , POS_DEAD    , do_gen_tog  , 0, SCMD_BUSY, 0 },
    { "buy"      , "b"    , POS_STANDING, do_not_here , 0, 0, 0 },
    { "bug"      , "bug"  , POS_DEAD    , do_gen_write, 0, SCMD_BUG, 0 },

    { "cast"     , "c"    , POS_SITTING , do_cast     , 1, 0, 0 },
    { "cedit"    , "cedit"   , POS_DEAD    , do_oasis    , LVL_IMPL, SCMD_OASIS_CEDIT, 0 },
    { "chaos"    , "chaos", POS_DEAD    , do_chaos    , LVL_IMMORT, 0, WIZ_IMM2_GRP },
    { "check"    , "che"   , POS_STANDING, do_not_here , 1, 0, 0 },
    { "changes"  , "cha"   , POS_DEAD, do_changes , 1, 0, 0 },
    { "clan"     , "clan" , POS_SLEEPING, do_clan     , 1, 0, 0 },
    { "clear"    , "cle"	, POS_DEAD    , do_gen_ps   , 0, SCMD_CLEAR, 0 },
    { "climb"    , "cli"  , POS_STANDING, do_climb    , 0, 0, 0 },
    { "close"    , "clo"	, POS_SITTING , do_gen_door , 0, SCMD_CLOSE, 0 },
    { "cls"      , "cls"	, POS_DEAD    , do_gen_ps   , 0, SCMD_CLEAR, 0 },
    { "clsolc"   , "clsolc"  , POS_DEAD    , do_gen_tog  , 0, SCMD_CLS, 0 },
    { "consider" , "con"	, POS_RESTING , do_consider , 0, 0, 0 },
    { "convert"  , "conv" , POS_RESTING , do_convert  , 0, 0, 0 },
    { "convey"   , "conve", POS_RESTING , do_convey   , 0, 0, 0 },
    { "corpse"   , "cor"	, POS_DEAD    , do_corpse    , 0, 0, 0 },
    { "color"    , "col"	, POS_DEAD    , do_color    , 0, 0, 0 },
    { "colour"   , "colour"	, POS_DEAD    , do_color    , 0, 0, 0 },
    { "\"" , "\""	, POS_DEAD    , do_comm , 0, 0, 0 },
    { "commands" , "comm"	, POS_DEAD    , do_commands , 0, SCMD_COMMANDS, 0 },
    { "compact"  , "comp"	, POS_DEAD    , do_gen_tog  , 0, SCMD_COMPACT, 0 },
    { "compress"  , "compress"	, POS_DEAD    , do_gen_tog  , 0, SCMD_COMPRESS, 0 },
    { "compare"  , "compa", POS_RESTING , do_compare  , 0, 0, 0 },
    { "copyover" , "copyo", POS_DEAD    , do_copyover , LVL_IMMORT, 0, WIZ_SEN_GRP },
    { "credits"  , "cre"	, POS_DEAD    , do_gen_ps   , 0, SCMD_CREDITS, 0 },
    { "ctell"    , "ct"   , POS_SLEEPING, do_ctell    , 0, 0, 0 },

    { "date"     , "da"	, POS_DEAD    , do_date     , 0, SCMD_DATE, 0 },
    { "damdice"  , "dam"	, POS_DEAD    , do_dam_dice , 0, 0, 0 },
    { "dc"       , "dc"	, POS_DEAD    , do_dc       , LVL_IMMORT, 0, WIZ_DSPLN_GRP },
    { "decrypt"       , "decrypt"	, POS_DEAD    , do_decrypt       , LVL_IMPL, 0, WIZ_DSPLN_GRP },
    { "deduct"   , "ded"  , POS_RESTING , do_deduct   , 1, 0, 0 },
    { "descend"  , "desc" , POS_STANDING, do_descend  , 1, 0, 0 },
    { "deposit"  , "dep"	, POS_STANDING, do_not_here , 1, 0, 0 },
    { "diagnose" , "dia"	, POS_RESTING , do_diagnose , 0, 0, 0 },
    { "dig"      , "dig"  , POS_STANDING, do_dig_ground      , 0, 0, 0 },
    { "dismount" , "dism" , POS_STANDING, do_dismount , 0, 0, 0 },
    { "display"  , "dis"	, POS_DEAD    , /*do_display */ do_prompt_new , 0, 0, 0 },
    { "donate"   , "don"	, POS_RESTING , do_drop     , 0, SCMD_DONATE, 0 },
    { "drink"    , "dr"	, POS_RESTING , do_drink    , 0, SCMD_DRINK, 0 },
    { "drive"    , "drive", POS_RESTING , do_drive    , 0, 0, 0 },
    { "drop"     , "dro"	, POS_RESTING , do_drop     , 0, SCMD_DROP, 0 },
    { "die"     , "die"	, POS_DEAD , do_die     , 0, 0, 0 },

    { "eat"      , "ea"	, POS_RESTING , do_eat      , 0, SCMD_EAT, 0 },
    { "echo"     , "echo"	, POS_SLEEPING, do_echo     , LVL_IMMORT, SCMD_ECHO, WIZ_QUEST_GRP },
    { "emote"    , "em"	, POS_RESTING , do_echo     , 1, SCMD_EMOTE, 0 },
    { ":"        , ":"	, POS_RESTING, do_echo      , 1, SCMD_EMOTE, 0 },
    { "enter"    , "en"	, POS_STANDING, do_enter    , 0, 0, 0 },
    { "equipment", "eq"	, POS_RESTING, do_equipment, 0, 0, 0 },
    { "exits"    , "exi"	, POS_RESTING , do_exits    , 0, 0, 0 },
    { "examine"  , "exa"	, POS_SITTING , do_examine  , 0, 0, 0 },

    { "force"    , "for"	, POS_SLEEPING, do_force    , LVL_IMMORT, 0, WIZ_SEN_GRP },
    { "feel"     , "feel" , POS_RESTING , do_feel     , 0, 0, 0 },
    { "file"     , "file" , POS_SLEEPING, do_file     , LVL_GRGOD, 0, 0 },
    { "find_nugget","find", POS_STANDING, find_nugget , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "fill"     , "fil"	, POS_STANDING, do_pour     , 0, SCMD_FILL, 0 },
    { "finger"   , "fing" , POS_SLEEPING, do_finger   , 0, 0, 0 },
    { "fire"     , "fire" , POS_STANDING, do_not_here , 0, 0, 0 },
    { "flee"     , "fl"	, POS_FIGHTING, do_flee     , 1, 0, 0 },
    { "fly"      , "fly"  , POS_STANDING, do_drive    , 0, 0, 0 },
    { "follow"   , "fol"	, POS_RESTING , do_follow   , 0, 0, 0 },
    { "fuse"     , "fuse"	, POS_RESTING , do_fuse   , 0, 0, 0 },
    { "fusion"   , "fusion"	, POS_RESTING , do_fusion   , 0, 0, 0 },
    { "freeze"   , "free" , POS_DEAD    , do_wizutil  , LVL_IMMORT, SCMD_FREEZE, WIZ_DSPLN_GRP },
    { "forest"   , "forest" , POS_DEAD    , forest_find  , LVL_IMMORT, 0, 0 },
    { "fightmsg"   , "fightmsg" , POS_DEAD    , do_fightmsg  , LVL_IMMORT, 0, 0 },


    { "get"      , "g"	, POS_RESTING , do_get      , 0, 0, 0 },
    { "gecho"    , "gech"	, POS_DEAD    , do_gecho    , LVL_IMMORT, 0, WIZ_QUEST_GRP },
    { "give"     , "giv"	, POS_RESTING , do_give     , 0, 0, 0 },
    { "goto"     , "got"	, POS_SLEEPING, do_goto     , LVL_IMMORT, 0, WIZ_IMM1_GRP },
    { "gold"     , "gol"	, POS_RESTING , do_gold     , 0, 0, 0 },
    { "gossip"   , "gos"  , POS_SLEEPING, do_gen_comm , 0, SCMD_GOSSIP, 0 },
    { "."        , "."    , POS_SLEEPING, do_gen_comm , 0, SCMD_GOSSIP, 0 },
    { "group"    , "gro"  , POS_RESTING , do_group    , 1, 0, 0 },
    { "grab"     , "gra"  , POS_RESTING , do_grab     , 0, 0, 0 },
    { "grats"    , "grat" , POS_SLEEPING, do_gen_comm , 0, SCMD_GRATZ, 0 },
    { "gsay"     , "gs"   , POS_SLEEPING, do_gsay     , 0, 0, 0 },
    { "gtell"    , "gt"   , POS_SLEEPING, do_gsay     , 0, 0, 0 },

    { "heal"     , "hea"  , POS_RESTING , do_not_here , 0, 0, 0 },
    { "hackinvis", "hackinvis"  , POS_RESTING , do_hackinvis , 55, 0, 0 },
    { "help"     , "he"	, POS_DEAD    , do_help     , 0, 0, 0 },
    { "handbook" , "hand"	, POS_DEAD    , do_gen_ps   , LVL_IMMORT, SCMD_HANDBOOK, WIZ_IMM1_GRP },
    { "hcontrol" , "hcon"	, POS_DEAD    , do_hcontrol , LVL_IMMORT, 0, WIZ_HOUSE_GRP },
    { "hostfind" , "host"	, POS_DEAD    , do_hostfind , LVL_IMMORT, 0, WIZ_BAN_GRP },
    { "hgossip"  , "hgos"  , POS_SLEEPING, do_gen_comm ,0, SCMD_HERO, 0 },
    { "hire"     , "hire" , POS_STANDING, do_not_here , LVL_IMPL, 0, 0 },
    { "hit"      , "h"	, POS_FIGHTING, do_hit      , 0, SCMD_HIT, 0 },
    { "hold"     , "ho"	, POS_RESTING , do_grab     , 1, 0, 0 },
    { "holler"   , "hol"	, POS_RESTING , do_gen_comm , 1, SCMD_HOLLER, 0 },
    { "holylight", "holy"	, POS_DEAD    , do_gen_tog  , LVL_HERO, SCMD_HOLYLIGHT, 0 },
    { "house"    , "hou"	, POS_RESTING , do_house    , 0, 0, 0 },
    //{ "hedit"    , "hedit"	, POS_DEAD    , do_oasis      , LVL_IMMORT, SCMD_OASIS_HEDIT, WIZ_EDIT_GRP },

    { "inventory", "i"	, POS_RESTING , do_inventory, 0, 0, 0 },
    { "identify", "ident"	, POS_RESTING    , do_not_here, 0, 0, 0 },
    { "ic"       , "ic"   , POS_DEAD    , do_gen_comm , 0, SCMD_IC, 0 },
    { "idea"     , "id"	, POS_DEAD    , do_idea, 0, SCMD_IDEA, 0 },
    { "ignite"   , "igni"  , POS_STANDING    , do_ignite   , 0, 0, 0 },
    { "ignore"   , "ign"  , POS_DEAD    , do_ignore   , 0, 0, 0 },
    { "imotd"    , "imo"	, POS_DEAD    , do_gen_ps   , LVL_IMMORT, SCMD_IMOTD, WIZ_IMM1_GRP },
    { "immlist"  , "imm"	, POS_DEAD    , do_gen_ps   , 0, SCMD_IMMLIST, 0 },
    { "info"     , "inf"	, POS_SLEEPING, do_gen_ps   , 0, SCMD_INFO, 0 },
    { "insult"   , "ins"	, POS_RESTING , do_insult   , 0, 0, 0 },
    { "invis"    , "inv"	, POS_DEAD    , do_invis    , LVL_IMMORT, 0, WIZ_IMM2_GRP },
    { "involve"   , "invo", POS_STANDING    , set_perc    , 0, 0, 0 },
    { "ipstat"   , "ip"   , POS_DEAD    , do_ipstat   , 0, 0, 0 },

    { "jump"     , "jump" , POS_STANDING, do_jump     , 0, 0, 0 },
    { "junk"     , "j"	, POS_RESTING , do_drop     , 0, SCMD_JUNK, 0 },

    { "kill"     , "k"	, POS_FIGHTING, do_hit      , 0, 0, 0 },
    { "killlist"     , "killist"	, POS_FIGHTING, do_killlist      , 0, 0, 0 },
    { "keeptitle", "keep" , POS_RESTING , do_gen_tog  , 1, SCMD_KEEPTITLE, 0 },

    { "look"     , "l"	, POS_RESTING , do_look     , 0, SCMD_LOOK, 0 },
    { "last"     , "la"	, POS_DEAD    , do_last     , LVL_IMMORT, 0, WIZ_IMM2_GRP },
    { "leader"   , "lead"	, POS_STANDING    , do_leader   , 0, 0, 0 },
    { "leave"    , "lea"	, POS_STANDING, do_leave    , 0, 0, 0 },
    { "levels"   , "lev"	, POS_DEAD    , do_levels   , 0, 0, 0 },
    { "linkload" , "link" , POS_DEAD    , do_linkload , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "links"    , "lin"     , POS_STANDING, do_oasis    , LVL_BUILDER, SCMD_OASIS_LINKS , WIZ_OLC_GRP},
    { "list"     , "li"	, POS_STANDING, do_not_here , 0, 0, 0 },
    { "listen"   , "lis"  , POS_RESTING , do_listen   , 0, 0, 0 },
    { "ls"       ,  "ls"  , POS_DEAD    , do_powerplay, LVL_IMMORT, 0, 0},
    { "lock"     , "loc"	, POS_SITTING , do_gen_door , 0, SCMD_LOCK, 0 },
    { "locker"   , "locker"  , POS_RESTING , do_locker   , 0, 0, 0 },
    { "load"     , "loa"	, POS_DEAD    , do_load     , LVL_IMMORT, 0, WIZ_LOAD_GRP },

    { "medit"    , "med"	, POS_DEAD    , do_oasis      , LVL_BUILDER, SCMD_OASIS_MEDIT, WIZ_OLC_GRP },
    { "movemsg" , "mov"	, POS_DEAD    , do_gen_tog  , 0, SCMD_MOVEMSG, 0 },
    { "motd"     , "motd" , POS_DEAD    , do_gen_ps   , 0, SCMD_MOTD, 0 },
    { "mail"     , "mail"	, POS_STANDING, do_not_here , 1, 0, 0 },
    { "map"      , "map"  , POS_DEAD    , do_map      , 0, 0, 0 },
    { "mine"     , "mine" , POS_STANDING, do_mine     , 0, 0, 0 },
    { "meld"     , "meld" , POS_STANDING, do_meld     , 0, 0, 0 },
    { "mlist"    , "mlist"   , POS_DEAD    , do_oasis    , LVL_BUILDER, SCMD_OASIS_MLIST , WIZ_OLC_GRP},
    { "mute"     , "mut"	, POS_DEAD    , do_wizutil  , LVL_IMMORT, SCMD_SQUELCH, WIZ_DSPLN_GRP },
    { "murder"   , "mur"	, POS_FIGHTING, do_hit      , 10, SCMD_MURDER, 0 },
    { "memory"    , "mem"   , POS_DEAD    , do_get_free_mem    , LVL_BUILDER, 0 , WIZ_OLC_GRP},

    { "news"     , "new"	, POS_DEAD, do_news , 0, SCMD_NEWS, 0 },
    { "newbie"   , "newb"  , POS_SLEEPING, do_gen_comm , 0, SCMD_NEWBIE, 0 },
    { "noauction", "noa"	, POS_DEAD    , do_gen_tog  , 0, SCMD_NOAUCTION, 0 },
    { "nobattlespam" , "nob"  , POS_DEAD    , do_gen_tog  , 0, SCMD_BATTLESPAM, 0 },
    { "noctalk" , "noct"	, POS_DEAD    , do_gen_tog  , 0, SCMD_NOCTALK, 0 },
    { "nogossip" , "nogo"	, POS_DEAD    , do_gen_tog  , 0, SCMD_NOGOSSIP, 0 },
    { "nograts"  , "nogr"	, POS_DEAD    , do_gen_tog  , 0, SCMD_NOGRATZ, 0 },
    { "nohassle" , "noh"	, POS_DEAD    , do_gen_tog  , LVL_IMMORT, SCMD_NOHASSLE, WIZ_IMM1_GRP },
    { "noic"     , "noi"  , POS_DEAD    , do_gen_tog  , 0, SCMD_NOIC, 0 },
    { "nomail"   , "nom"  , POS_DEAD    , do_gen_tog  , 0, SCMD_MAIL, 0 },
    { "noooc"   , "noooc"  , POS_DEAD    , do_gen_tog  , 0, SCMD_NOOOC, 0 },
    { "nomount"   , "nomou"  , POS_DEAD    , do_gen_tog  , 0, SCMD_MOUNTABLE, 0 },
    { "norepeat" , "nor"	, POS_DEAD    , do_gen_tog  , 0, SCMD_NOREPEAT, 0 },
    { "noshout"  , "nosh"	, POS_SLEEPING, do_gen_tog  , 1, SCMD_DEAF, 0 },
    { "nosummon" , "nosu"	, POS_DEAD    , do_gen_tog  , 1, SCMD_NOSUMMON, 0 },
    { "note"    , "nit"   , POS_DEAD, do_note , 1, 0, 0 },
    { "nohero"   , "nohe"	, POS_DEAD    , do_gen_tog  , 1, SCMD_NOHERO, 0 },
    { "nonewbie"   , "nonew"	, POS_DEAD    , do_gen_tog  , 1, SCMD_NONEWBIE, 0 },
    { "notitle"  , "noti"	, POS_DEAD    , do_wizutil  , LVL_IMMORT, SCMD_NOTITLE, WIZ_DSPLN_GRP },
    { "nowiz"    , "now"	, POS_DEAD    , do_gen_tog  , LVL_IMMORT, SCMD_NOWIZ, WIZ_IMM1_GRP },
    { "namechange"    , "namechange"	, POS_DEAD    , do_namechange  , LVL_SEN, 0, WIZ_SEN_GRP },

    { "objdump"  , "objd" , POS_DEAD    , do_objdump  , LVL_IMPL, 0, WIZ_IMPL_GRP },

    { "offtell"   , "offnote"	, POS_DEAD    , do_gen_tog  , 1, SCMD_NOTELL, 0 },
    { "order"    , "ord"	, POS_RESTING , do_order    , 1, 0, 0 },
    { "own"   , "own"  , POS_DEAD    , do_own   , LVL_SEN, 0, WIZ_SEN_GRP},
    { "owners"   , "owne"  , POS_DEAD    , do_owners   , LVL_IMPL, 0, WIZ_IMPL_GRP},
    { "offer"    , "off"	, POS_STANDING, do_not_here , 1, 0, 0 },
    { "open"     , "op"	, POS_SITTING , do_gen_door , 0, SCMD_OPEN, 0 },
    { "olc"      , "olc"     , POS_DEAD    , do_oasis    , LVL_BUILDER, SCMD_OLC_SAVEINFO , WIZ_OLC_GRP},
    { "olist"    , "olist"   , POS_DEAD    , do_oasis    , LVL_BUILDER, SCMD_OASIS_OLIST , WIZ_OLC_GRP},
    { "oedit"    , "oedit"   , POS_DEAD    , do_oasis    , LVL_BUILDER, SCMD_OASIS_OEDIT , WIZ_OLC_GRP},
    { "ocheck"    , "och"  , POS_DEAD    , do_osnoop    , LVL_IMPL, 0, WIZ_OLC_GRP },
    { "ooc"       , "ooc"   , POS_DEAD    , do_gen_comm , 0, SCMD_OOC, 0 },

    { "put"      , "p"	, POS_RESTING , do_put      , 0, 0, 0 },
    { "page"     , "pag"	, POS_SLEEPING    , do_page     , 0, 0, 0 },
    { "pardon"   , "par"	, POS_DEAD    , do_wizutil  , LVL_IMMORT, SCMD_PARDON, WIZ_DSPLN_GRP },
    { "password" , "pass"	, POS_DEAD    , do_password     , 0, 0, 0 },
    { "peace"    , "pea"  , POS_DEAD    , do_peace    , LVL_IMMORT, 0, WIZ_KILL_GRP },
    { "policy"   , "pol"	, POS_DEAD    , do_gen_ps   , 0, SCMD_POLICIES, 0 },
    { "poofin"   , "poofi", POS_DEAD    , do_poofset  , LVL_IMMORT, SCMD_POOFIN, WIZ_IMM1_GRP },
    { "poofout"  , "poofo", POS_DEAD    , do_poofset  , LVL_IMMORT, SCMD_POOFOUT, WIZ_IMM1_GRP },
    { "potionweight"    , "potionweight"	, POS_DEAD    , do_potionweight      , LVL_SEN, SCMD_OLC_OEDIT, WIZ_OLC_GRP},
    { "pour"     , "pour" , POS_STANDING, do_pour     , 0, SCMD_POUR, 0 },
    { "powerplay", "pow"  , POS_DEAD    , do_powerplay, LVL_IMMORT, 0, 0},
    { "pretitle" , "pre"  , POS_DEAD    , do_pretitle, 0, 0, 0},
    { "prompt"   , "promp", POS_DEAD    , /*do_display*/ do_prompt_new  , 0, 0, 0 },
    { "professions"   , "prof", POS_DEAD    , do_professions  , 0, 0, 0 },
    { "practice" , "pra"  , POS_SLEEPING, do_practice , 1, 0, 0 },
    { "prereq"   , "pre"  , POS_SLEEPING, do_prereq   , 0, 0, 0 },
    { "pull"     , "pull" , POS_STANDING, do_pull , 0, 0, 0 },
    { "pageheight"     , "pageheight" , POS_DEAD, do_pageheight , 0, 0, 0 },
    { "pagewidth"     , "pagewidth" , POS_DEAD, do_pagewidth , 0, 0, 0 },
    { "pagewrap"   , "pagewrap"  , POS_DEAD    , do_gen_tog  , 0, SCMD_PAGEWRAP, 0 },
    { "purge"    , "pur"	, POS_DEAD    , do_purge    , LVL_IMMORT, 0, WIZ_KILL_GRP },
    { "psaux"    , "psaux"   , POS_DEAD    , do_ps_aux    , LVL_BUILDER, 0 , WIZ_OLC_GRP},

    { "quaff"    , "q"	, POS_RESTING , do_use      , 0, SCMD_QUAFF, 0 },
    { "qecho"    , "qec"	, POS_DEAD    , do_qcomm    , LVL_IMMORT, SCMD_QECHO, WIZ_QUEST_GRP },
    { "quest"    , "que"	, POS_DEAD    , do_gen_tog  , 0, SCMD_QUEST, 0 },
    { "question" , "quest", POS_DEAD    , do_gen_comm , 0, SCMD_NEWBIE2, 0 },
    { "qui"      , "qui"	, POS_DEAD    , do_quit     , 0, 0, 0 },
    { "quit"     , "quit"	, POS_DEAD    , do_quit     , 0, SCMD_QUIT, 0 },
    { "qsay"     , "qsay"	, POS_RESTING , do_qcomm    , 0, SCMD_QSAY, 0 },
    { "qicinfo"  , "qicin", POS_DEAD    , do_qicinfo  , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "qload"    , "qlo"  , POS_DEAD    , do_qload    , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "qicsave"  , "qicsa", POS_DEAD    , do_qicsave  , LVL_IMPL, 0, WIZ_IMPL_GRP },

    { "race"     , "race" , POS_DEAD    , do_race     , LVL_IMMORT, 0, WIZ_SEN_GRP },
    { "radar"    , "radar", POS_STANDING, do_not_here , 0, 0, 0 },
    { "rdig"     , "rdig" , POS_DEAD    , do_rdig      , LVL_BUILDER, 0 , WIZ_OLC_GRP},
    { "reply"    , "r"	, POS_SLEEPING, do_reply    , 0, 0, 0 },
    { "rest"     , "res"	, POS_RESTING , do_rest     , 0, 0, 0 },
    { "read"     , "rea"	, POS_RESTING , do_look     , 0, SCMD_READ, 0 },
    { "recite"   , "rec"	, POS_RESTING , do_use      , 0, SCMD_RECITE, 0 },
    { "recall"   , "rec"	, POS_RESTING , do_recall      , 0, 0, 0 },
    { "receive"  , "rece"	, POS_SITTING , do_not_here , 1, 0, 0 },
    { "recover"  , "recov", POS_STANDING, do_recover  , 0, 0, 0 },
    { "register" , "reg"  , POS_STANDING, do_register , 0, 0, 0 },
    { "reload"   , "rel"  , POS_RESTING , do_reload   , 0, 0, 0 },
    { "remove"   , "rem"	, POS_RESTING , do_remove   , 0, 0, 0 },
    { "remort"   , "remor", POS_STANDING, do_remort   , 50,0, 0 },
    { "rent"     , "ren"	, POS_STANDING, do_not_here , 1, 0, 0 },
    { "report"   , "rep"	, POS_RESTING , do_report   , 0, 0, 0 },
    { "replylock"   , "replylock"  , POS_DEAD    , do_gen_tog  , 0, SCMD_REPLYLOCK, 0 },
    { "reroll"   , "rer"	, POS_DEAD    , do_wizutil  , LVL_IMMORT, SCMD_REROLL, WIZ_IMPL_GRP },
    { "restore"  , "resto", POS_DEAD    , do_restore  , LVL_IMMORT, 0, WIZ_HEAL_GRP },
    { "return"   , "ret"	, POS_DEAD    , do_return   , 0, 0, 0 },
    { "redit"    , "redit", POS_DEAD    ,  do_oasis    , LVL_BUILDER, SCMD_OASIS_REDIT, WIZ_OLC_GRP},
    { "reward"     , "rewa" , POS_DEAD    , do_reward     , 0, 0, 0 },
    { "rinnate"  , "rinn" , POS_DEAD    , do_innate   , LVL_IMPL, SCMD_RINNATE, WIZ_IMPL_GRP },
    { "rlist"    , "rli"  , POS_DEAD    , do_oasis    , LVL_BUILDER, SCMD_OASIS_RLIST, WIZ_OLC_GRP },
    { "rclone"   , "rclone"  , POS_DEAD    , do_room_copy, LVL_BUILDER, 0  , WIZ_OLC_GRP},
    { "rsay"      , "rsa"	, POS_RESTING , do_say      , 0, SCMD_RSAY, 0 },
    { "roomflags", "roomf", POS_DEAD    , do_gen_tog  , LVL_IMMORT, SCMD_ROOMFLAGS, WIZ_IMM2_GRP },
    { "rouse"     , "rou"	, POS_SLEEPING, do_wake     , 0, SCMD_ROUSE, 0 },

    { "say"      , "sa"	, POS_RESTING , do_say      , 0, SCMD_SAY, 0 },
    { "sacrifice", "sac"  , POS_RESTING , do_sac      , 0, 0, 0 },
    { "'"        , "'"	, POS_RESTING , do_say      , 0, 0, 0 },
    { "sayto"      , "sayto"	, POS_RESTING , do_sayto      , 0, 0, 0 },
    { ">"      , ">"	, POS_RESTING , do_sayto      , 0, 0, 0 },
    { "sail"     , "sail" , POS_STANDING, do_drive    , 0, 0, 0 },
    { "sample"   , "sam"	, POS_RESTING , do_eat      , 0, SCMD_TASTE, 0 },
    { "save"     , "save"	, POS_SLEEPING, do_save     , 0, 0, 0 },
    { "saveall"  , "savea", POS_SLEEPING, do_saveall  , LVL_IMMORT, 0, WIZ_OLC_GRP },
    { "score"    , "sco"	, POS_DEAD    , do_score    , 0, 0, 0 },
    { "search"   , "sea"  , POS_STANDING, do_search   , 0, 0, 0 },
    { "skills"   , "skills"  , POS_DEAD, do_prac_spells   , 0, 0, 0 },
    { "spells"   , "spells"  , POS_DEAD, do_prac_skills   , 0, 0, 0 },
    { "sell"     , "sel"	, POS_STANDING, do_not_here , 0, 0, 0 },
    { "send"     , "sen"	, POS_SLEEPING, do_send     , LVL_IMMORT, 0, WIZ_QUEST_GRP },
    { "set"      , "set"	, POS_DEAD    , do_set      , LVL_IMMORT, 0, WIZ_SEN_GRP },
    { "setqic"   , "setqi", POS_DEAD    , do_setqic   , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "settime"  , "settime", POS_STANDING, do_settime, LVL_IMMORT, 0, WIZ_OLC_GRP },
    { "sedit"    , "sedi"	, POS_DEAD    , do_oasis      , LVL_IMMORT, SCMD_OASIS_SEDIT, WIZ_OLC_GRP },
    { "shout"    , "sho"	, POS_RESTING , do_gen_comm , 0, SCMD_SHOUT, 0 },
    { "shake"    , "sha"	, POS_RESTING , do_action   , 0, 0, 0 },
    { "shiver"   , "shiv"	, POS_RESTING , do_action   , 0, 0, 0 },
    { "shoot"    , "shoot", POS_STANDING, do_shoot    , 0, 0, 0 },
    { "show"     , "sho"	, POS_DEAD    , do_show     , LVL_IMMORT, 0, WIZ_IMM2_GRP },
    { "shutdow"  , "shutdow", POS_DEAD    , do_shutdown  , LVL_IMMORT, 0, WIZ_IMPL_GRP },
    { "shutdown" , "shutdown", POS_DEAD    , do_shutdown , LVL_IMMORT, SCMD_SHUTDOWN, WIZ_IMPL_GRP },
    { "sinnate"  , "sinn" , POS_DEAD    , do_innate   , LVL_IMPL, SCMD_SINNATE, WIZ_IMPL_GRP },
    { "sip"      , "sip"	, POS_RESTING , do_drink    , 0, SCMD_SIP, 0 },
    { "sit"      , "sit"	, POS_RESTING , do_sit      , 0, 0, 0 },
    { "skillset" , "skillse" , POS_SLEEPING, do_skillset , LVL_IMMORT, 0, WIZ_SEN_GRP },
    { "skilllist" , "skillli" , POS_SLEEPING, do_skilllist , 0, 0, 0 },
    { "subskillset" , "subskillse" , POS_SLEEPING, do_subskillset , LVL_IMMORT, 0, WIZ_SEN_GRP },
    { "subdisplay" , "subdisplay" , POS_SLEEPING, do_subdisplay , 0, 0, 0 },
    { "slay"     , "slay" , POS_RESTING , do_slay     , LVL_IMMORT, 0, WIZ_KILL_GRP },
    { "sleep"    , "sl"	, POS_SLEEPING, do_sleep    , 0, 0, 0 },
    { "slist"    , "sli"  , POS_SITTING , do_oasis    , LVL_BUILDER, SCMD_OASIS_SLIST, WIZ_SEN_GRP },
    { "slowns"   , "slown", POS_DEAD    , do_gen_tog  , LVL_IMMORT, SCMD_SLOWNS, WIZ_SEN_GRP },
    { "smell"    , "smell", POS_SITTING , do_smell    , 0, 0, 0 },
    { "smite"    , "smite", POS_DEAD    , do_smite    , LVL_IMMORT, 0, WIZ_DSPLN_GRP },
    { "snoop"    , "sno"	, POS_DEAD    , do_snoop    , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "socials"  , "soc"	, POS_DEAD    , do_commands , 0, SCMD_SOCIALS, 0 },
    { "speedwalk", "sp"	, POS_STANDING, do_speedwalk    , 1, 0, 0 },
    { "spellinfo", "spell", POS_DEAD    , do_spellinfo    , 1, 0, 0 },
    { "split"    , "spl"	, POS_SITTING , do_split    , 1, 0, 0 },
    { "stand"    , "st"	, POS_RESTING , do_stand    , 0, 0, 0 },
    { "stat"     , "stat"	, POS_DEAD    , do_stat     , LVL_IMMORT, 0, WIZ_IMM2_GRP },
    { "statlist" , "statl", POS_DEAD    , do_statlist , LVL_IMPL, 0, WIZ_IMM2_GRP },
    { "statinnate"  , "statin" , POS_DEAD    , do_statinnate   , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "string"   , "str"  , POS_DEAD    , do_string   , LVL_IMMORT, 0, WIZ_SEN_GRP },
    { "struggle" , "stru" , POS_STANDING, do_struggle , 0, 0, 0 },
    { "swap"     , "swap" , POS_DEAD    , do_swap     , 0, 0, 0 },
    { "switch"   , "sw"	, POS_DEAD    , do_switch   , LVL_IMMORT, 0, WIZ_QUEST_GRP },
    { "syslog"   , "sys"	, POS_DEAD    , do_syslog   , LVL_IMMORT, 0, WIZ_IMM2_GRP },

    { "task"    , "ta"	, POS_DEAD    , do_task    , 0, 0, 0 },
    { "tedit"    , "ted"	, POS_DEAD    , do_tedit    , LVL_GRGOD, 0, WIZ_IMPL_GRP },
    { "tell"     , "te"	, POS_SLEEPING    , do_tell     , 0, 0, 0 },
    { "take"     , "ta"	, POS_RESTING , do_get      , 0, 0, 0 },
    { "taste"    , "tas"  , POS_RESTING , do_taste    , 0, 0, 0 },
    { "teleport" , "tel"	, POS_DEAD    , do_teleport , LVL_IMMORT, 0, WIZ_TELE_GRP },
    { "thaw"     , "thaw"	, POS_DEAD    , do_wizutil  , LVL_IMMORT, SCMD_THAW, WIZ_DSPLN_GRP },
    { "throw"    , "thr"  , POS_STANDING, do_throw    , 0, 0, 0 },
    { "title"    , "tit"	, POS_DEAD    , do_title    , 0, 0, 0 },
    { "time"     , "tim"	, POS_DEAD    , do_time     , 0, 0, 0 },
    { "tiername"     , "tierna"	, POS_DEAD    , do_tiername     , 0, 0, 0 },
    { "toggle"   , "tog"	, POS_DEAD    , do_toggle   , 0, 0, 0 },
    { "trackthru", "tra"  , POS_DEAD    , do_gen_tog  , LVL_IMPL, SCMD_TRACK, WIZ_IMPL_GRP },
    { "topgold"    , "top"	, POS_DEAD    , do_topgold   , LVL_IMMORT, 0, 0 },
    { "transfer" , "tran"	, POS_SLEEPING, do_trans    , LVL_IMMORT, 0, WIZ_TELE_GRP },
    { "trigedit" , "trig"	, POS_DEAD    , do_oasis      , LVL_BUILDER, SCMD_OASIS_TRIGEDIT, WIZ_TRIG_GRP },
    { "trust"    , "trus" , POS_DEAD    , do_trust    , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "typo"     , "typ"	, POS_DEAD    , do_gen_write, 0, SCMD_TYPO, 0 },

    { "unlock"   , "unl"	, POS_SITTING , do_gen_door , 0, SCMD_UNLOCK, 0 },
    { "ungroup"  , "ung"	, POS_DEAD    , do_ungroup  , 0, 0, 0 },
    { "unban"    , "unb"	, POS_DEAD    , do_unban    , LVL_IMMORT, 0, WIZ_BAN_GRP },
    { "unaffect" , "una"	, POS_DEAD    , do_wizutil  , LVL_IMMORT, SCMD_UNAFFECT, WIZ_HEAL_GRP },
    { "uptime"   , "upt"	, POS_DEAD    , do_date     , LVL_IMMORT, SCMD_UPTIME, WIZ_IMM2_GRP },
    { "use"      , "us"	, POS_SITTING , do_use      , 1, SCMD_USE, 0 },
    { "users"    , "user"	, POS_DEAD    , do_users    , LVL_IMMORT, 0, WIZ_IMM2_GRP },

    { "value"    , "val"	, POS_STANDING, do_not_here , 0, 0, 0 },
    { "version"  , "ver"	, POS_DEAD    , do_gen_ps   , 0, SCMD_VERSION, 0 },
    { "visible"  , "vis"	, POS_RESTING , do_visible  , 1, 0, 0 },
    { "vnum"     , "vnum"	, POS_DEAD    , do_vnum     , LVL_IMMORT, 0, WIZ_IMM2_GRP },
    { "vstat"    , "vsta"	, POS_DEAD    , do_vstat    , LVL_IMMORT, 0, WIZ_IMM2_GRP },
    { "vedit"    , "vedit"   , POS_DEAD    , do_oasis    , LVL_BUILDER, SCMD_OASIS_VEDIT , WIZ_OLC_GRP},
    { "vlist"    , "vlist"   , POS_DEAD    , do_oasis    , LVL_BUILDER, SCMD_OASIS_VLIST , WIZ_OLC_GRP},

    { "wake"     , "wak"	, POS_SLEEPING, do_wake     , 0, SCMD_WAKE, 0 },
    { "wager"    , "wager", POS_RESTING , do_bet      , 0, 0, 0 },
    { "wear"     , "wea"	, POS_RESTING , do_wear     , 0, 0, 0 },
    { "weather"  , "weat" , POS_RESTING , do_weather  , 0, 0, 0 },
    { "who"      , "who"	, POS_DEAD    , do_who      , 0, 0, 0 },
    { "whoami"   , "whoa"	, POS_DEAD    , do_gen_ps   , 0, SCMD_WHOAMI, 0 },
    { "where"    , "whe"	, POS_RESTING , do_where    , 1, 0, 0 },
    { "whisper"  , "whis"	, POS_RESTING , do_spec_comm, 0, SCMD_WHISPER, 0 },
    { "wield"    , "wie"	, POS_RESTING , do_wield    , 0, 0, 0 },
    { "wimpy"    , "wim"	, POS_DEAD    , do_wimpy    , 0, 0, 0 },
    { "withdraw" , "with"	, POS_STANDING, do_not_here , 1, 0, 0 },
    { "wiznet"   , "wiz"	, POS_DEAD    , do_wiznet   , LVL_HERO+1, 0, WIZ_IMM1_GRP },
    { ";"        , ";"	, POS_DEAD    , do_wiznet   , LVL_HERO+1, 0, WIZ_IMM1_GRP },
    { "wizhelp"  , "wizh"	, POS_SLEEPING, do_commands , LVL_IMMORT, SCMD_WIZHELP, WIZ_IMM1_GRP },
    { "wizlist"  , "wizl"	, POS_DEAD    , do_gen_ps   , 0, SCMD_WIZLIST, 0 },
    { "wizlock"  , "wizlo", POS_DEAD    , do_wizlock  , LVL_IMMORT, 0, WIZ_SEN_GRP },
    { "worth"    , "wor"  , POS_SLEEPING, do_worth    , 0, 0, 0 },
    { "write"    , "wr"	, POS_STANDING, do_write    , 1, 0, 0 },

    { "xreload"  , "xre"  , POS_DEAD    , do_reboot   , LVL_IMMORT, 0, WIZ_EDIT_GRP },

    { "zedit"    , "zed"	, POS_DEAD    , do_oasis    , LVL_BUILDER, SCMD_OASIS_ZEDIT, WIZ_OLC_GRP },
    { "zlist"    , "zli"  , POS_DEAD    , do_oasis    , LVL_BUILDER, SCMD_OASIS_ZLIST, WIZ_OLC_GRP },
    { "zreset"   , "zre"	, POS_DEAD    , do_zreset   , LVL_IMMORT, 0, WIZ_OLC_GRP },

    /*Skills */
    { "track"    , "tr"	, POS_STANDING, do_skills    , 0, SKILL_TRACK, 0 },
    { "backstab" , "bac"	, POS_STANDING, do_skills , 1, SKILL_BACKSTAB, 0 },
    { "bs"       , "bs"	, POS_STANDING, do_skills , 1, SKILL_BACKSTAB, 0 },
    { "bash"     , "bas"	, POS_FIGHTING, do_skills     , 1, SKILL_BASH, 0 },
    { "berserk"   , "bes" , POS_STANDING, do_skills   , 0, SKILL_BESERK, 0 },
    { "behead"    , "beh"	, POS_FIGHTING, do_skills    , 1, SKILL_BEHEAD, 0 },
    { "brace"    , "bra"	, POS_STANDING, do_skills    , 1, SKILL_BRACE, 0 },
    { "blackjack", "black", POS_STANDING, do_skills, 1, SKILL_BLACKJACK, 0 },
    { "bladedance"    , "blade"	, POS_STANDING, do_skills    , 1, SKILL_BLADE_DANCE, 0 },
    { "brew"     , "brew" , POS_STANDING, do_skills     , 1, SKILL_BREW, 0 },
    { "encircle"   , "encir"  , POS_FIGHTING, do_skills   , 1, SKILL_CIRCLE, 0 },
    { "charge"   , "char" , POS_FIGHTING, do_skills   , 0, SKILL_CHARGE, 0 },
    { "cleave"    , "cleav"	, POS_FIGHTING, do_skills    , 1, SKILL_CLEAVE, 0 },
    { "disarm"   , "disa" , POS_FIGHTING, do_skills   , 0, SKILL_DISARM, 0 },
    { "dodge"   , "dod" , POS_FIGHTING, do_skills   , 0, SKILL_DODGE, 0 },
    { "face"   , "face" , POS_FIGHTING, do_skills   , 0, SKILL_FACE, 0 },
    { "filet"    , "filet", POS_STANDING, do_skills    , 0, SKILL_FILET, 0 },
    { "flank"    , "fla", POS_STANDING, do_skills    , 0, SKILL_FLANK, 0 },
    { "focus"   , "focu" , POS_STANDING, do_skills   , 0, SKILL_FOCUS, 0 },
    { "forage"   , "fora" , POS_STANDING, do_skills   , 0, SKILL_FORAGE, 0 },
    { "fortify"   , "fort" , POS_STANDING, do_skills   , 0, SKILL_FORTIFY, 0 },
    { "grapple"   , "grapple" , POS_FIGHTING, do_skills   , 0, SKILL_GRAPPLE, 0 },
    { "grip"   , "grip" , POS_STANDING, do_skills   , 0, SKILL_GRIP, 0 },
    { "hide"     , "hid"  , POS_RESTING , do_skills     , 1, SKILL_HIDE, 0 },
    { "holystrength", "holys", POS_STANDING, do_skills     , 1, SKILL_HOLY_STRENGTH, 0 },
    { "hyperactivity", "hyp" , POS_STANDING, do_skills     , 1, SKILL_HYPERACTIVITY, 0 },
    { "joust"     , "jou"	, POS_FIGHTING, do_skills     , 1, SKILL_JOUST, 0 },
    { "kick"     , "kic"	, POS_FIGHTING, do_skills     , 1, SKILL_KICK, 0 },
    { "manifest"  , "man"  , POS_STANDING, do_skills  , 0, SKILL_MANIFEST, 0 },
    { "manipulate", "manip", POS_STANDING, do_skills    , 0, SKILL_MANIPULATE, 0 },
    { "martialarts" , "mart", POS_STANDING, do_skills    , 1, SKILL_MARTIAL_ARTS, 0 },
    { "meditate"   , "medi" , POS_STANDING, do_skills   , 0, SKILL_MEDITATE, 0 },
    { "mount"    , "mou"  , POS_STANDING, do_skills    , 0, SKILL_MOUNT, 0 },
    { "pick"     , "pi"	, POS_STANDING, do_skills , 1, SKILL_PICK_LOCK, 0 },
    { "phase"    , "ph"	, POS_STANDING, do_skills    , 1, SKILL_PHASE, 0 },
    { "poisonweapon", "poi",POS_STANDING,do_skills, 0, SKILL_POISON_WEAPON, 0 },
    { "push"     , "push" , POS_STANDING, do_skills     , 1, SKILL_PUSH, 0 },
    { "rescue"   , "resc"	, POS_FIGHTING, do_skills   , 1, SKILL_RESCUE, 0 },
    { "retreat"  , "retre", POS_FIGHTING, do_skills  , 0, SKILL_RETREAT, 0 },
    { "scribe"   , "scri" , POS_STANDING, do_skills   , 1, SKILL_SCRIBE, 0 },
    { "scalp"    , "scalp"	, POS_STANDING, do_skills    , 1, SKILL_SCALP, 0 },
    { "scan"     , "scan" , POS_STANDING, do_skills     , 1, SKILL_SCAN, 0 },
    { "slip"     , "sli" , POS_STANDING, do_skills     , 1, SKILL_SLIP, 0 },
    { "snare"    , "sna"  , POS_FIGHTING, do_skills    , 1, SKILL_SNARE, 0 },
    { "sneak"    , "snea"	, POS_STANDING, do_skills    , 1, SKILL_SNEAK, 0 },
    { "steal"    , "stea"	, POS_STANDING, do_skills    , 1, SKILL_STEAL, 0 },
    { "strangle"    , "stra", POS_STANDING, do_skills    , 1, SKILL_STRANGLE, 0 },
    { "tame"     , "tam"  , POS_RESTING , do_skills     , 0, SKILL_TAME, 0 },
    { "tinker"   , "tink" , POS_STANDING, do_skills   , 1, SKILL_TINKER, 0 },
    { "trample"  , "tram" , POS_FIGHTING    , do_skills  , 0, SKILL_TRAMPLE, 0 },
    { "truestrike"   , "true" , POS_STANDING, do_skills   , 0, SKILL_TRUE_STRIKE, 0 },
    { "woodsing" , "woo"  , POS_STANDING, do_skills    , 0, SKILL_SING_WOOD, 0 },

    /* subskills listed below */
    { "furyattack" , "fury"  , POS_FIGHTING, do_subskill    , 0, SUB_FURY_ATTACKS, 0 },
    { "drainblood" , "drain" , POS_STANDING, do_subskill    , 0, SUB_DRAIN_BLOOD, 0 },
    { "juggle" , "jug" , POS_STANDING, do_subskill    , 0, SUB_JUGGLE, 0 },
    { "tunnel" , "tun" , POS_STANDING, do_subskill    , 0, SUB_TUNNELING, 0 },
    { "fell" , "fell" , POS_STANDING, do_fell    , 0, 0, 0 }, // for skill LUMBERJACK
    /* DG trigger commands */
    { "attach"   , "att"	        , POS_DEAD    , do_attach   , LVL_IMMORT, 0, WIZ_TRIG_GRP },
    { "detach"   , "det"	        , POS_DEAD    , do_detach   , LVL_IMMORT, 0, WIZ_TRIG_GRP },
    { "masound"  , "masound"	, POS_DEAD    , do_masound  , -1, 0, 0 },
    { "mat"      , "mat"		, POS_DEAD    , do_mat      , -1, 0, 0 },
    { "mcollision","mcollision"   , POS_DEAD    , do_mcollision, -1, 0, 0 },
    { "mecho"    , "mecho"	, POS_DEAD    , do_mecho    , -1, 0, 0 },
    { "mechoaround", "mechoaround", POS_DEAD    , do_mechoaround    , -1, 0, 0 },
    { "mforce"   , "mforce"	, POS_DEAD    , do_mforce   , -1, 0, 0 },
    { "mforget"  , "mforget"	, POS_DEAD    , do_mforget  , -1, 0, 0 },
    { "mgoto"    , "mgoto"	, POS_DEAD    , do_mgoto    , -1, 0, 0 },
    { "mhunt"    , "mhunt"	, POS_DEAD    , do_mhunt    , -1, 0, 0 },
    { "mjunk"    , "mjunk"	, POS_SITTING , do_mjunk    , -1, 0, 0 },
    { "mdamage"  , "mdamage"      , POS_DEAD    , do_mdamage  , -1, 0, 0 },
    { "mdoor"    , "mdoor"        , POS_DEAD    , do_mdoor    , -1, 0, 0 },
    { "mkill"    , "mkill"	, POS_STANDING, do_mkill    , -1, 0, 0 },
    { "mload"    , "mload"	, POS_DEAD    , do_mload    , -1, 0, 0 },
    { "mlag"    , "mlag"	        , POS_DEAD    , do_mlag    , -1, 0, 0 },
    { "mpdelay"  , "mpdelay"	, POS_DEAD    , do_mpdelayed, LVL_IMPL, 0, 0 },
    { "mpurge"   , "mpurge"	, POS_DEAD    , do_mpurge    , -1, 0, 0 },
    { "mremember", "mremember"	, POS_DEAD    , do_mremember, -1, 0, 0 },
    { "msend"    , "msend"	, POS_DEAD    , do_msend    , -1, 0, 0 },
    { "mslay"    , "mslay"        , POS_DEAD    , do_mslay    , -1, 0, 0 },
    { "msteal"    , "msteal"      , POS_DEAD    , do_msteal    , -1, 0, 0 },
    { "mteleport", "mteleport"	, POS_DEAD    , do_mteleport, -1, 0, 0 },
    { "mtransform","mtransform"	, POS_DEAD    , do_mtransform, -1, 0, 0 },
    { "mzecho"   , "mzecho"	, POS_DEAD    , do_mzecho   , -1, 0, 0 },
    { "mzrecho"  , "mzrecho"	, POS_DEAD    , do_mzrecho  , -1, 0, 0 },
    { "mzreset"  , "mzreset"      , POS_DEAD    , do_mzreset  , 0, 0, 0 },
    { "tlist"    , "tli"	        , POS_DEAD    , do_oasis    , LVL_IMMORT, SCMD_OASIS_TLIST, WIZ_TRIG_GRP },
    { "tstat"    , "tst"	        , POS_DEAD    , do_tstat    , LVL_IMMORT, 0, WIZ_TRIG_GRP },
    { "vdelete"  , "vdel"         , POS_DEAD    , do_vdelete  , -1, 0, 0 },

    /* Romance Module Command List */
    { "askout"   , "asko"         , POS_RESTING , do_askout   , 1, 0 , 0 },
    { "accept"   , "acc"		, POS_RESTING , do_accept   , 1, 0 , 0 },
    { "reject"   , "rej"		, POS_RESTING , do_reject   , 1, 0 , 0 },
    { "propose"  , "prop"		, POS_STANDING, do_propose  , 1, 0 , 0 },
    { "breakup"  , "break"	, POS_STANDING, do_breakup  , 1, 0 , 0 },
    { "marry"    , "marry"	, POS_STANDING, do_marry    , LVL_IMMORT, 0, WIZ_MARRY_GRP },
    { "divorce"  , "divor"	, POS_STANDING, do_divorce  , LVL_IMMORT, 0, WIZ_MARRY_GRP },
    /* End Romance Module Command List */

    /* MatingMod Command List */
    { "seduce"   , "sedu"		, POS_STANDING, do_seduce   , 1, 0, 0 },
    { "consent"  , "consent"	, POS_STANDING, do_consent  , 1, 0, 0 },
    { "deny"     , "deny"		, POS_STANDING, do_deny     , 1, 0, 0 },
    { "abort"    , "abor"		, POS_RESTING , do_abort    , LVL_IMPL, 0, WIZ_IMPL_GRP },
    { "nomating" , "nomat"	, POS_SLEEPING, mate_toggle , 1, 0, 0 },
    /* End Mating Module Command List */

    { "\n", "zzzzzzz", 0, 0, 0, 0, 0 }
  }
  ;	/* this must be last */


const char *fill[] =
  {
    "in",
    "from",
    "with",
    "the",
    "on",
    "at",
    "to",
    "\n"
  };

const char *reserved[] =
  {
    "a",
    "an",
    "self",
    "me",
    "all",
    "room",
    "someone",
    "something",
    "\n"
  };

/*
 * This is the actual command interpreter called from game_loop() in comm.c
 * It makes sure you are the proper level and position to execute the command,
 * then calls the appropriate function.
 */
void command_interpreter(struct char_data *ch, char *argument)
{
  int cmd, length;
  char *line;
  char arg[MAX_INPUT_LENGTH];
  if (argument && *argument)
    strcpy(last_command, argument);
  else
    strcpy(last_command, "none");

  if (strlen(argument) > MAX_INPUT_LENGTH)
  {
    send_to_char("Huh?!?!\r\n", ch);
    new_mudlog( NRM, LVL_SEN, TRUE, "%s tried crashing the mud with too long input.",
                GET_NAME(ch));
    return;
  }


  if (!IS_NPC(ch) || ch->master)
    SET_BIT_AR(AFF_FLAGS(ch), AFF_GROUP);

  if (AFF_FLAGGED(ch, AFF_HIDE))
  {
    new_send_to_char(ch, "You come out from concealment.\r\n");
    REMOVE_BIT_AR(AFF_FLAGS(ch), AFF_HIDE);
  }

  /* just drop to next line for hitting CR */
  skip_spaces(&argument);
  if (!*argument)
    return;

  /*
   * special case to handle one-character, non-alphanumeric commands;
   * requested by many people so "'hi" or ";godnet test" is possible.
   * Patch sent by Eric Green and Stefan Wasilewski.
   */
  if (!isalpha(*argument))
  {
    arg[0] = argument[0];
    arg[1] = '\0';
    line = argument + 1;
  }
  else
    line = any_one_arg(argument, arg);
  //skip_spaces(&argument);

  /* Since all command triggers check for valid_dg_target before acting, the levelcheck
   * here has been removed.
   */
  {
    int cont;                                            /* continue the command checks */
    cont = command_wtrigger(ch, arg, line); /* any world triggers ? */
    if (!cont) cont = command_mtrigger(ch, arg, line);   /* any mobile triggers ? */
    if (!cont) cont = command_otrigger(ch, arg, line);   /* any object triggers ? */
    if (cont) return;                                    /* yes, command trigger took over */
  }


  for (length = strlen(arg), cmd = 0; *complete_cmd_info[cmd].command != '\n'; cmd++)
  {
    if (!strncmp(complete_cmd_info[cmd].command, arg, length))
      if (GET_LEVEL(ch) >= complete_cmd_info[cmd].minimum_level)
        break;
  }

  if (*complete_cmd_info[cmd].command == '\n')
    new_send_to_char(ch, "Huh?!?\r\n");
  else if (!IS_NPC(ch) && PLR_FLAGGED(ch, PLR_FROZEN) && GET_LEVEL(ch) < LVL_IMPL)
    new_send_to_char(ch, "You try, but the mind-numbing cold prevents you...\r\n");
  else if (complete_cmd_info[cmd].command_pointer == NULL)
    new_send_to_char(ch, "Sorry, that command hasn't been implemented yet.\r\n");
  else if ((complete_cmd_info[cmd].cmd_bits > 0)
           && (((!CMD_FLAGGED(ch, complete_cmd_info[cmd].cmd_bits))
                && (!CMD_FLAGGED2(ch, complete_cmd_info[cmd].cmd_bits))
                && (GET_LEVEL(ch) < LVL_IMPL)) || IS_NPC(ch)))
    send_to_char("You do not have that Immortal privilege.\r\n", ch);
  else if (IS_NPC(ch) && complete_cmd_info[cmd].minimum_level > LVL_IMMORT)
    new_send_to_char(ch, "You can't use immortal commands while switched.\r\n");
  else if (GET_POS(ch) < complete_cmd_info[cmd].minimum_position)
    switch (GET_POS(ch))
    {
    case POS_DEAD:
      new_send_to_char(ch, "Lie still; you are DEAD!!! :-(\r\n");
      break;
    case POS_INCAP:
    case POS_MORTALLYW:
      send_to_char
      ("You are in a pretty bad shape, unable to do anything!\r\n",
       ch);
      break;
    case POS_STUNNED:
      send_to_char
      ("All you can do right now is think about the stars!\r\n",
       ch);
      break;
    case POS_SLEEPING:
      send_to_char("In your dreams, or what?\r\n", ch);
      break;
    case POS_RESTING:
      send_to_char("Nah... You feel too relaxed to do that..\r\n",
                   ch);
      break;
    case POS_SITTING:
      send_to_char("Maybe you should get on your feet first?\r\n",
                   ch);
      break;
    case POS_FIGHTING:
      send_to_char("No way!  You're fighting for your life!\r\n",
                   ch);
      break;
    }
  else if (no_specials || !special(ch, cmd, line))
  {
    total_commands_typed++;
    if (!IS_NPC(ch))
      total_pcommands_typed++;
    ((*complete_cmd_info[cmd].command_pointer) (ch, line, cmd,
        complete_cmd_info[cmd].
        subcmd));
  }
}

/**************************************************************************
 * Routines to handle aliasing                                             *
  **************************************************************************/


struct alias_data *find_alias(struct alias_data *alias_list, char *str)
{
  while (alias_list != NULL)
  {
    if (*str == *alias_list->alias)	/* hey, every little bit counts :-) */
      if (!strcmp(str, alias_list->alias))
        return (alias_list);

    alias_list = alias_list->next;
  }

  return (NULL);
}


void free_alias(struct alias_data *a)
{
  if (a->alias)
    free(a->alias);
  if (a->replacement)
    free(a->replacement);
  free(a);
}


/* The interface to the outside world: do_alias */
ACMD(do_alias)
{
  char *repl;
  struct alias_data *a, *temp;
  char arg[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];
  DYN_DEFINE;
  *buf = 0;
  if (IS_NPC(ch))
    return;

  repl = any_one_arg(argument, arg);

  if (!*arg)
  {		/* no argument specified -- list currently defined aliases */
    send_to_char("Currently defined aliases:\r\n", ch);
    if ((a = GET_ALIASES(ch)) == NULL)
      send_to_char(" None.\r\n", ch);
    else
    {
      DYN_CREATE;
      *dynbuf = 0;
      while (a != NULL)
      {
        snprintf(buf, sizeof(buf), "%-15s %s\r\n", a->alias, a->replacement);
        DYN_RESIZE(buf);
        a = a->next;
      }
      page_string(ch->desc, dynbuf, DYN_BUFFER);
    }
  }
  else
  {			/* otherwise, add or remove aliases */
    /* is this an alias we've already defined? */
    if ((a = find_alias(GET_ALIASES(ch), arg)) != NULL)
    {
      REMOVE_FROM_LIST(a, GET_ALIASES(ch), next);
      free_alias(a);
    }
    /* if no replacement string is specified, assume we want to delete */
    if (!*repl)
    {
      if (a == NULL)
        send_to_char("No such alias.\r\n", ch);
      else
        send_to_char("Alias deleted.\r\n", ch);
    }
    else
    {		/* otherwise, either add or redefine an alias */
      if (!str_cmp(arg, "alias"))
      {
        send_to_char("You can't alias 'alias'.\r\n", ch);
        return;
      }
      CREATE(a, struct alias_data, 1);
      a->alias = str_dup(arg);
      delete_doubledollar(repl);
      a->replacement = str_dup(repl);
      if (strchr(repl, ALIAS_SEP_CHAR)
          || strchr(repl, ALIAS_VAR_CHAR))
        a->type = ALIAS_COMPLEX;
      else
        a->type = ALIAS_SIMPLE;
      a->next = GET_ALIASES(ch);
      GET_ALIASES(ch) = a;
      send_to_char("Alias added.\r\n", ch);
    }
  }
}

/*
 * Valid numeric replacements are only $1 .. $9 (makes parsing a little
 * easier, and it's not that much of a limitation anyway.)  Also valid
 * is "$*", which stands for the entire original line after the alias.
 * ";" is used to delimit commands.
 */
#define NUM_TOKENS       9

void perform_complex_alias(struct txt_q *input_q, char *orig,
                           struct alias_data *a)
{
  struct txt_q temp_queue;
  char *tokens[NUM_TOKENS], *temp, *write_point;
  char buf2[MAX_RAW_INPUT_LENGTH], buf[MAX_RAW_INPUT_LENGTH];	/* raw? */
  int num_of_tokens = 0, num;

  /* First, parse the original string */
  strcpy(buf2, orig);	/* strcpy: OK (orig:MAX_INPUT_LENGTH < buf2:MAX_RAW_INPUT_LENGTH) */
  temp = strtok(buf2, " ");
  while (temp != NULL && num_of_tokens < NUM_TOKENS)
  {
    tokens[num_of_tokens++] = temp;
    temp = strtok(NULL, " ");
  }

  /* initialize */
  write_point = buf;
  temp_queue.head = temp_queue.tail = NULL;

  /* now parse the alias */
  for (temp = a->replacement; *temp; temp++)
  {
    if (*temp == ALIAS_SEP_CHAR)
    {
      *write_point = '\0';
      buf[MAX_INPUT_LENGTH - 1] = '\0';
      write_to_q(buf, &temp_queue, 1);
      write_point = buf;
    }
    else if (*temp == ALIAS_VAR_CHAR)
    {
      temp++;
      if ((num = *temp - '1') < num_of_tokens && num >= 0)
      {
        strcpy(write_point, tokens[num]);
        write_point += strlen(tokens[num]);
      }
      else if (*temp == ALIAS_GLOB_CHAR)
      {
        strcpy(write_point, orig);
        write_point += strlen(orig);
      }
      else if ((*(write_point++) = *temp) == '$')	/* redouble $ for act safety */
        *(write_point++) = '$';
    }
    else
      *(write_point++) = *temp;
  }

  *write_point = '\0';
  buf[MAX_INPUT_LENGTH - 1] = '\0';
  write_to_q(buf, &temp_queue, 1);

  /* push our temp_queue on to the _front_ of the input queue */
  if (input_q->head == NULL)
    *input_q = temp_queue;
  else
  {
    temp_queue.tail->next = input_q->head;
    input_q->head = temp_queue.head;
  }
}


/*
 * Given a character and a string, perform alias replacement on it.
 *
 * Return values:
 *   0: String was modified in place; call command_interpreter immediately.
 *   1: String was _not_ modified in place; rather, the expanded aliases
 *      have been placed at the front of the character's input queue.
 */
int perform_alias(struct descriptor_data *d, char *orig, size_t maxlen)
{
  char first_arg[MAX_INPUT_LENGTH], *ptr;
  struct alias_data *a, *tmp;

  /* Mobs don't have alaises. */
  if (IS_NPC(d->character))
    return (0);

  /* bail out immediately if the guy doesn't have any aliases */
  if ((tmp = GET_ALIASES(d->character)) == NULL)
    return (0);

  /* find the alias we're supposed to match */
  ptr = any_one_arg(orig, first_arg);

  /* bail out if it's null */
  if (!*first_arg)
    return (0);

  /* if the first arg is not an alias, return without doing anything */
  if ((a = find_alias(tmp, first_arg)) == NULL)
    return (0);

  if (a->type == ALIAS_SIMPLE)
  {
    strlcpy(orig, a->replacement, maxlen);
    return (0);
  }
  else
  {
    perform_complex_alias(&d->input, ptr, a);
    return (1);
  }
}





/***************************************************************************
 * Various other parsing utilities                                         *
 **************************************************************************/

/*
 * searches an array of strings for a target string.  "exact" can be
 * 0 or non-0, depending on whether or not the match must be exact for
 * it to be returned.  Returns -1 if not found; 0..n otherwise.  Array
 * must be terminated with a '\n' so it knows to stop searching.
 */
int search_block(char *arg, const char **list, int exact)
{
  register int i, l;

  /* Make into lower case, and get length of string */
  for (l = 0; *(arg + l); l++)
    *(arg + l) = LOWER(*(arg + l));

  if (exact)
  {
    for (i = 0; **(list + i) != '\n'; i++)
      if (!strcmp(arg, *(list + i)))
        return (i);
  }
  else
  {
    if (!l)
      l = 1;		/* Avoid "" to match the first available
                        				 * string */
    for (i = 0; **(list + i) != '\n'; i++)
      if (!strncmp(arg, *(list + i), l))
        return (i);
  }

  return (-1);
}


int is_number(const char *str)
{
  while (*str)
    if (!isdigit(*(str++)))
      return (0);

  return (1);
}

/*
 * Function to skip over the leading spaces of a string.
 */
void skip_spaces(char **string)
{
  for (; **string && isspace(**string); (*string)++);
}


/*
 * Given a string, change all instances of double dollar signs ($$) to
 * single dollar signs ($).  When strings come in, all $'s are changed
 * to $$'s to avoid having users be able to crash the system if the
 * inputted string is eventually sent to act().  If you are using user
 * input to produce screen output AND YOU ARE SURE IT WILL NOT BE SENT
 * THROUGH THE act() FUNCTION (i.e., do_gecho, do_title, but NOT do_say),
 * you can call delete_doubledollar() to make the output look correct.
 *
 * Modifies the string in-place.
 */
char *delete_doubledollar(char *string)
{
  char *read, *write;

  /* If the string has no dollar signs, return immediately */
  if ((write = strchr(string, '$')) == NULL)
    return (string);

  /* Start from the location of the first dollar sign */
  read = write;


  while (*read)		/* Until we reach the end of the string... */
    if ((*(write++) = *(read++)) == '$')	/* copy one char */
      if (*read == '$')
        read++;		/* skip if we saw 2 $'s in a row */

  *write = '\0';

  return (string);
}
char *remove_percentage(char *string)
{
  char *read, *write;

  /* If the string has no dollar signs, return immediately */
  if ((write = strchr(string, '%')) == NULL)
    return (string);

  /* Start from the location of the first dollar sign */
  read = write;


  while (*read)		/* Until we reach the end of the string... */
    if ((*(write++) = *(read++)) == '%')	/* copy one char */
      write--;		/* skip if we saw 2 $'s in a row */

  *write = '\0';

  return (string);
}


int fill_word(char *argument)
{
  return (search_block(argument, fill, TRUE) >= 0);
}


int reserved_word(char *argument)
{
  return (search_block(argument, reserved, TRUE) >= 0);
}


/*
 * copy the first non-fill-word, space-delimited argument of 'argument'
 * to 'first_arg'; return a pointer to the remainder of the string.
 */
char *one_argument(char *argument, char *first_arg)
{
  char *begin = first_arg;

  if (!argument)
  {
    log("SYSERR: one_argument received a NULL pointer!");
    *first_arg = '\0';
    return (NULL);
  }

  do
  {
    skip_spaces(&argument);

    first_arg = begin;
    while (*argument && !isspace(*argument))
    {
      *(first_arg++) = LOWER(*argument);
      argument++;
    }

    *first_arg = '\0';
  }
  while (fill_word(begin));

  return (argument);
}


/*
 * one_word is like one_argument, except that words in quotes ("") are
 * considered one word.
 *
 * No longer ignores fill words.  -dak, 6 Jan 2003.
 */
char *one_word(char *argument, char *first_arg)
{
  skip_spaces(&argument);

  if (*argument == '\"')
  {
    argument++;
    while (*argument && *argument != '\"')
    {
      *(first_arg++) = LOWER(*argument);
      argument++;
    }
    argument++;
  }
  else
  {
    while (*argument && !isspace(*argument))
    {
      *(first_arg++) = LOWER(*argument);
      argument++;
    }
  }

  *first_arg = '\0';

  return (argument);
}


/* same as one_argument except that it doesn't ignore fill words */
char *any_one_arg(char *argument, char *first_arg)
{
  skip_spaces(&argument);

  while (*argument && !isspace(*argument))
  {
    *(first_arg++) = LOWER(*argument);
    argument++;
  }

  *first_arg = '\0';

  return (argument);
}

char *one_arg(char *arg, char *first_arg)
{
  skip_spaces(&arg);

  while (*arg && !isspace(*arg))
  {
    *(first_arg++) = *arg;
    arg++;
  }

  *first_arg = '\0';

  return (arg);
}


/*
 * Same as one_argument except that it takes two args and returns the rest;
 * ignores fill words
 */
char *two_arguments(char *argument, char *first_arg, char *second_arg)
{
  return (one_argument(one_argument(argument, first_arg), second_arg));	/* :-) */
}



/*
 * determine if a given string is an abbreviation of another
 * (now works symmetrically -- JE 7/25/94)
 *
 * that was dumb.  it shouldn't be symmetrical.  JE 5/1/95
 * 
 * returnss 1 if arg1 is an abbreviation of arg2
 */
int is_abbrev(const char *arg1, const char *arg2)
{
  register int c;
  if (!*arg1)
    return (0);

  if (!strcmp(arg1, arg2))
    return 1;

  for (c = 0; *arg1 && *arg2; arg1++, arg2++, c++)
    if (LOWER(*arg1) != LOWER(*arg2))
      return (0);

  if (!*arg1 && c > 1)
    return (1);
  else
    return (0);
}



/* return first space-delimited token in arg1; remainder of string in arg2 */
void half_chop(char *string, char *arg1, char *arg2)
{
  char *temp;
  int plen;

  temp = any_one_arg(string, arg1);
  skip_spaces(&temp);
  memmove(arg2, temp, (plen = strlen(temp)));
  *(arg2 + plen) = '\0';
  /*strcpy(arg2, temp);*/
}




/* Used in specprocs, mostly.  (Exactly) matches "command" to cmd number */
int find_command(const char *command)
{
  int cmd;

  for (cmd = 0; *complete_cmd_info[cmd].command != '\n'; cmd++)
    if (!strcmp(complete_cmd_info[cmd].command, command))
      return (cmd);

  return (-1);
}


int special(struct char_data *ch, int cmd, char *arg)
{
  register struct obj_data *i;
  register struct char_data *k;
  int j;

  /* special in room? */
  if (GET_ROOM_SPEC(IN_ROOM(ch)) != NULL)
    if (GET_ROOM_SPEC(IN_ROOM(ch)) (ch, IN_ROOM(ch), cmd, arg))
      return (1);

  /* special in equipment list? */
  for (j = 0; j < NUM_WEARS; j++)
    if (HAS_BODY(ch, j) && GET_EQ(ch, j)
        && GET_OBJ_SPEC(GET_EQ(ch, j)) != NULL)
      if (GET_OBJ_SPEC(GET_EQ(ch, j)) (ch, GET_EQ(ch, j), cmd, arg))
        return (1);

  /* special in inventory? */
  for (i = ch->carrying; i; i = i->next_content)
    if (GET_OBJ_SPEC(i) != NULL)
      if (GET_OBJ_SPEC(i) (ch, i, cmd, arg))
        return (1);

  /* special in mobile present? */
  for (k = IN_ROOM(ch)->people; k; k = k->next_in_room)
  {
    if (!MOB_FLAGGED(k, MOB_NOTDEADYET))
      if (GET_MOB_SPEC(k) != NULL)
        if (GET_MOB_SPEC(k) (ch, k, cmd, arg))
          return (1);
  }

  /* special in object present? */
  for (i = IN_ROOM(ch)->contents; i; i = i->next_content)
    if (GET_OBJ_SPEC(i) != NULL)
      if (GET_OBJ_SPEC(i) (ch, i, cmd, arg))
        return (1);

  return (0);
}



/* *************************************************************************
*  Stuff for controlling the non-playing sockets (get name, pwd etc)       *
************************************************************************* */


/* locate entry in p_table with entry->name == name. -1 mrks failed search */
int find_name(char *name)
{
  int i;

  for (i = 0; i <= top_of_p_table; i++)
  {
    if (!str_cmp((player_table + i)->name, name))
      return (i);
  }

  return (-1);
}


/* This function needs to die. */
int _parse_name(char *arg, char *name)
{
  int i;

  skip_spaces(&arg);
  for (i = 0; (*name = *arg); arg++, i++, name++)
  {
    if (!isalpha(*arg))
      return (1);
  }

  if (!i)
    return (1);

  return (0);
}


#define RECON		1
#define USURP		2
#define UNSWITCH	3

int perform_dupe_check(struct descriptor_data *d)
{
  struct descriptor_data *k, *next_k;
  struct char_data *target = NULL, *ch, *next_ch;
  int diff_acc = FALSE;
  int mode = 0;

  int id = GET_IDNUM(d->character);
  int same_account;

  //reset_char(d->character);
  /*
   * Now that this descriptor has successfully logged in, disconnect all
   * other descriptors controlling a character with the same ID number.
   */

  for (k = descriptor_list; k; k = next_k)
  {
    next_k = k->next;

    if (k == d)
      continue;
      
    if (d->character && k->character)
      same_account = (GET_ACC(d->character) == GET_ACC(k->character));
    else
      same_account = 0;
      
    if (k->original && (GET_IDNUM(k->original) == id))
    {
      /* Original descriptor was switched, booting it and restoring normal body control. */

      write_to_output(k, "\r\nMultiple login detected -- disconnecting.\r\n");
      STATE(k) = CON_CLOSE;
      if (!target)
      {
        target = k->original;
        mode = UNSWITCH;
      }
      if (k->character)
        k->character->desc = NULL;
      k->character = NULL;
      k->original = NULL;
    }
    else if (k->character && same_account && k->original)
    {
      /* Character taking over their own body, while an immortal was switched to it. */

      do_return(k->character, NULL, 0, 0);
    }
    else if (k->character && same_account)
    {
      /* Character taking over their own body. */

      if (!target && IS_PLAYING(k))
      {
        write_to_output(k, "\r\nThis body has been usurped!\r\n");
        target = k->character;
        mode = USURP;
      }
      k->character->desc = NULL;
      k->character = NULL;
      k->original = NULL;
      write_to_output(k, "\r\nMultiple login detected -- disconnecting.\r\n");
      STATE(k) = CON_CLOSE;
    }
  }

  /*
   * now, go through the character list, deleting all characters that
   * are not already marked for deletion from the above step (i.e., in the
   * CON_HANGUP state), and have not already been selected as a target for
   * switching into.  In addition, if we haven't already found a target,
   * choose one if one is available (while still deleting the other
   * duplicates, though theoretically none should be able to exist).
   */

  for (ch = character_list; ch; ch = next_ch)
  {
    next_ch = ch->next;

    if (IS_NPC(ch))
      continue;
    /* ignore chars with descriptors (already handled by above step) */
    if (ch->desc && target == ch)
      continue;
    if (!d->character)
      continue;
    if (GET_ACC(ch) != GET_ACC(d->character))
      continue;

    /* don't extract the target char we've found one already */
    if (ch == target)
      continue;

    /* we don't already have a target and found a candidate for switching */
    if (!target)
    {
      target = ch;
      mode = RECON;
      continue;
    }

    /* we've found a duplicate - blow him away, dumping his eq in limbo. */
    char_from_room(ch);
    char_to_room(ch, world_vnum[1]);
    extract_char(ch);
    log("%s's duplicate equipment being extracted.", GET_NAME(ch));
  }

  /* no target for switching into was found - allow login to continue */
  if (!target)
    return (0);

  /* Okay, we've found a target.  Connect d to target. */
  free_char(d->character); /* get rid of the old char */
  d->character = target;
  d->character->desc = d;
  d->original = NULL;
  d->character->char_specials.timer = 0;
  REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_MAILING);
  REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_WRITING);
  REMOVE_BIT_AR(AFF_FLAGS(d->character), AFF_GROUP);
  STATE(d) = CON_PLAYING;

  switch (mode)
  {
  case RECON:
    write_to_output(d, "Reconnecting.\r\n");
    act("$n has reconnected.", TRUE, d->character, 0, 0, TO_ROOM);
    new_mudlog( NRM, MAX(LVL_IMMORT, GET_INVIS_LEV(d->character)), TRUE, "%s [%s] has reconnected.", GET_NAME(d->character), d->host);
    break;
  case USURP:
    write_to_output(d,"You take over %s, already in use!\r\n", diff_acc ? "an account of yours" : "your body");
    act("$n suddenly keels over in pain, surrounded by a white aura...\r\n"
        "$n's body has been taken over by a new spirit!",
        TRUE, d->character, 0, 0, TO_ROOM);
    new_mudlog( NRM, MAX(LVL_GOD, GET_INVIS_LEV(d->character)), TRUE, "%s has re-logged in ... disconnecting old socket.", GET_NAME(d->character));
    break;
  case UNSWITCH:
    write_to_output(d,"Reconnecting to unswitched char.");
    new_mudlog( NRM, MAX(LVL_GOD, GET_INVIS_LEV(d->character)), TRUE, "%s [%s] has reconnected.", GET_NAME(d->character), d->host);
    break;
  }

  return (1);
}

/** 
//This is psudo code for something i would like to impliment eventually
 body human interfaces living {
       breathe {
          if (not breathable(environment.air)) {
	       choke();
          }
          call(1 seconds, breathe);
       }
       bool breathable(gas theGas) {
          if theGas = earthlike return true;
          else return false;
       }
    }
 
    environment room {
       theAir = smoky;
       gas air() {
          if (body.in_sealed_container())
             return body.container.environment.air();
          else
             return theAir;
       }
    }
 
    environment suit {
       gas air() {
          if (punctured)
             return (environment.air());
          else
             return earthlike;
       }
    }
*/

void list_current_accounts_menu(DESCRIPTOR_DATA *d)
{
  int i = 0, index, acc = get_acc_by_id(d->character->id);
  write_to_output(d, "\r\n{cC-=- Select a character to play (! denotes account owner) -=-{c0\r\n\r\n");
  for (index = 0; index <= top_of_p_table; index++)
  {
    if (player_table[index].account == acc)
    {
      write_to_output(d, "{cy%2d{cg)%s %-15s%s{c0",
                      i, player_table[index].account == player_table[index].id ? "!" : " " ,
                      player_table[index].name, ((i+1)%2) == 0 ? "\r\n" : "   ");
      i++;
    }
  }
  write_to_output(d, "\r\n"
                  " {cwChoose a number: {C0");
}

void account_manage_menu(DESCRIPTOR_DATA *d)
{
  write_to_output(d, "\r\n\r\n{cy-=- Manage Accounts -=-{c0\r\n\r\n"
                  "   {ccADD <name> <password>        -  To add another character to this account\r\n"
                  "   {cgREMOVE <name>                -  To remove a character from this account\r\n"
                  "   {ccJOIN <member> <members pass> -  To join %s to members account\r\n"
                  "   {cgMENU                         -  To return to main menu{c0\r\n\r\n"
                  "   {cwWhat do you want to do: {c0" ,
                  GET_NAME(d->character));
}

int parse_accounts(DESCRIPTOR_DATA *d, char *arg)
{
  char cmd[MAX_INPUT_LENGTH];
  char name[MAX_INPUT_LENGTH];
  char pass[MAX_INPUT_LENGTH] = "";
  char real_pass[MAX_INPUT_LENGTH];
  int pass_pass = 0;
  CHAR_DATA *member;
  int pos;
  arg = two_arguments(arg, cmd, name);
  skip_spaces(&arg);
  if (*arg)
    strcpy(pass, arg);

  if (!*name)
    return 1;

  if (!strcmp(name, GET_NAME(d->character)))
  {
    write_to_output(d, "\r\nYou do that to %s character\r\n"
                    " You are currently using it.\r\n", GET_NAME(d->character));
    account_manage_menu(d);
    return 0;
  }

  if ((pos = find_name(name)) == -1)
  {
    write_to_output(d, "\r\n%s is not a player here. Please write the name out completely.\r\n", name);
    account_manage_menu(d);
    return 0;
  }
  if (IS_SET(player_table[pos].flags, PINDEX_DELETED) ||
      IS_SET(player_table[pos].flags, PINDEX_SELFDELETE) ||
      player_table[pos].name[0] == '\0')
  {
    write_to_output(d, "\r\n%s is not a player here anymore.\r\n", name);
    account_manage_menu(d);
    return 0;
  }
  CREATE(member, CHAR_DATA, 1);
  clear_char(member);
  CREATE(member->player_specials, struct player_special_data, 1);

  TEMP_LOAD_CHAR = TRUE;
  if (store_to_char(name, member) == -1)
  {
    TEMP_LOAD_CHAR = FALSE;
    write_to_output(d, "\r\n%s can't be read at this time, contact an immortal.\r\n", name);
    account_manage_menu(d);
    return 0;
  }
  TEMP_LOAD_CHAR = FALSE;

  sprintf(real_pass, "%s", GET_PASSWD(member));
  free_char(member);

  pass_pass = !strncmp(CRYPT(pass, real_pass), real_pass, MAX_PWD_LENGTH);


  switch (LOWER(*cmd))
  {
  case 'a':
    if (pass_pass)
    {
      player_table[pos].account = get_acc_by_id(GET_IDNUM(d->character));
      save_player_index();
      write_to_output(d, "\r\n %s added to your account.\r\n", name);
    }
    else
    {
      write_to_output(d, "\r\n Incorrect Password: %s NOT added to your account.\r\n", name);
    }
    break;
  case 'r':
    if (player_table[pos].account == get_acc_by_id(GET_IDNUM(d->character)))
    {
      player_table[pos].account = player_table[pos].id;
      save_player_index();
      write_to_output(d, "\r\n %s removed from your account.\r\n", name);
    }
    else
    {
      write_to_output(d, "\r\n Not On Your Account: %s.\r\n", name);
    }
    break;
  case 'j':
    if (pass_pass)
    {
      int i = find_name(GET_NAME(d->character));
      if (i == -1)
      {
        write_to_output(d, "\r\nError.\r\n");
      }
      else
      {
        player_table[i].account = player_table[pos].account;
        save_player_index();
        write_to_output(d, "\r\n %s added to %s's account.\r\n", GET_NAME(d->character), name);
      }
    }
    else
    {
      write_to_output(d, "\r\n Incorrect Password: %s NOT added to your account.\r\n", name);
    }
    break;
  case 'm':
  default:
    con_disp_menu(d);
    return 1;
  }
  account_manage_menu(d);
  return 0;
}

/* load the player, put them in the right room - used by copyover_recover too */
int enter_player_game(struct descriptor_data *d)
{
  struct obj_data *obj = NULL;
  room_rnum load_room = NULL;
  struct char_data *ch = d->character;
  int load_result;
  void reset_default_status(struct char_data *ch);

  reset_char(ch);
  read_aliases(ch);
  ch->next = NULL;	// -- kalten


  GET_ID(ch) = GET_IDNUM(ch);// = player_table[id].id;
  if (find_char(GET_IDNUM(ch)) == NULL)
  add_to_lookup_table(GET_IDNUM(ch), (void *)ch);


  if (PLR_FLAGGED(ch, PLR_INVSTART))
    GET_INVIS_LEV(ch) = GET_LEVEL(ch);


  /* put character to their loadroom before autoequipping them */
  if (GET_LOADROOM(ch) != NOWHERE)
    load_room = real_room(GET_LOADROOM(ch));
  if (GET_LEVEL(ch) < 1)
    load_room = real_room(3081);

  /* If char was saved with NOWHERE, or real_room above failed... */
  if (load_room == NULL)
  {
    if (GET_LEVEL(ch) >= LVL_IMMORT)
      load_room = CONFIG_IMMORTAL_START;
    else
      load_room = CONFIG_MORTAL_START;
  }

  /*make sure all defaults are turned on for subskills */
  reset_default_status(ch);

  if (PLR_FLAGGED(ch, PLR_FROZEN))
    load_room = CONFIG_FROZEN_START;

  if (PROMPT(ch) == NULL)
    PROMPT(ch) = str_dup("{cg%h{cwH {cc%m{cwM {cy%v{cwV {cW(%S) {cC%E{cyTNL{c0>");

  new_send_to_char(ch, "%s", CONFIG_WELC_MESSG);
  //save_char(ch, NOWHERE);
  ch->next = character_list;
  character_list = ch;
  if (GET_LEVEL(ch) == 0)
    load_room = real_room(3081);
  char_to_room(ch, load_room);
  make_wholist();
  if (!AFF_FLAGGED(ch, AFF_POISON_1) && !AFF_FLAGGED(ch, AFF_POISON_2) &&
      !AFF_FLAGGED(ch, AFF_POISON_3) && !AFF_FLAGGED(ch, AFF_POISON_4) &&
      (((long) (time(0) - ch->player.time.logon)) >=
       SECS_PER_REAL_HOUR))
  {
    GET_HIT(ch) = GET_MAX_HIT(ch);
    GET_MOVE(ch) = GET_MAX_MOVE(ch);
    GET_MANA(ch) = GET_MAX_MANA(ch);
    GET_STAMINA(ch) = GET_MAX_STAMINA(ch);
  }

  new_mudlog( NRM, GET_LEVEL(ch), TRUE, "%s entering the game in room #%d, %s", GET_NAME(ch), load_room->number, load_room->name);
  act("$n has entered the game.", TRUE, ch, 0, 0, TO_ROOM);
  load_result = Crash_load(ch);

  load_locker(ch);
  if (LOCKER(ch))
    new_mudlog( NRM, GET_LEVEL(ch), TRUE, "  -- with %d items in locker", count_locker(ch));



  /* Clear their load room if it's not persistant. */
  if (!PLR_FLAGGED(ch, PLR_LOADROOM))
    GET_LOADROOM(ch) = NOWHERE;

  REMOVE_BIT_AR(PRF_FLAGS(ch), PRF_REPLYLOCK);

  STATE(d) = CON_PLAYING;
  if (GET_LEVEL(ch) == 0)
  {
    GET_CLAN(ch) = 12;
    GET_CLAN_RANK(ch) = 1;
    do_start(ch);
    write_to_output(d, "\r\n%s", CONFIG_START_MESSG);
    if (GET_CLASS(ch) == CLASS_MAGE || GET_CLASS(ch) == CLASS_PRIEST || GET_CLASS(ch) == CLASS_ESPER)
      write_to_output(d, "\r\n{cgRemember that as a magic capable class, you don't need to wield\r\n"
                      "any weapon at all, your own raw magic is enough in any battle.{c0\r\n\r\n");

    PAGEWIDTH(ch) = 80;
    PAGEHEIGHT(ch) = 40;
    GET_MAX_STAMINA(ch) = 100;
    SET_BIT_AR(AFF_FLAGS(ch), AFF_GROUP);

    LOOK(ch);

    //Backpack
    if ((obj = read_object(3081, VIRTUAL)) != NULL)
      obj_to_char(obj, ch);
    //Helmet
    if ((obj = read_object(3084, VIRTUAL)) != NULL)
      perform_wear(ch, obj, WEAR_HEAD);
    //Lantern
    if ((obj = read_object(3365, VIRTUAL)) != NULL)
      perform_wear(ch, obj, WEAR_LIGHT);
    //Seekers Feather
    if ((obj = read_object(12918, VIRTUAL)) != NULL)
      obj_to_char(obj, ch);
    //Note
    if ((obj = read_object(12922, VIRTUAL)) != NULL)
      obj_to_char(obj, ch);
    //Mini-quest leaflet
    if ((obj = read_object(21492, VIRTUAL)) != NULL)
      obj_to_char(obj, ch);

    switch (GET_CLASS(ch))
    {
    case CLASS_MAGE:
      //magic glasses
      if ((obj = read_object(3186, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_EYES);
      break;
    case CLASS_PRIEST:
      //ring of light
      if ((obj = read_object(3184, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_FINGER_L);
      break;
    case CLASS_ESPER:
      //intercom wristband
      if ((obj = read_object(3190, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_WRIST_L);
      break;
    case CLASS_THIEF:
      //dagger
      if ((obj = read_object(3020, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_WIELD);
      //cloak
      if ((obj = read_object(3185, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_ABOUT);
      break;
    case CLASS_GYPSY:
      //dagger
      if ((obj = read_object(3020, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_WIELD);
      //earring
      if ((obj = read_object(3187, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_EAR_L);
      break;
    case CLASS_RANGER:
      //dagger
      if ((obj = read_object(3020, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_WIELD);
      //sheath
      if ((obj = read_object(3189, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_ANKLE_L);
      break;
    case CLASS_WARRIOR:
      obj = read_object(3022, VIRTUAL);
      perform_wear(ch, obj, WEAR_WIELD);
      //cape
      if ((obj = read_object(3183, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_ABOUT);
      break;
    case CLASS_HUNTER:
      if ((obj = read_object(3021, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_WIELD);
      //amulet
      if ((obj = read_object(3188, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_NECK_1);
      break;
    default:
      if ((obj = read_object(3020, VIRTUAL)) != NULL)
        perform_wear(ch, obj, WEAR_WIELD);
    }

  }
  else
  {
    LOOK(ch);
  }

  GET_WAIT_STATE(ch) = 0;
  /*end new*/
  check_regen_rates(ch);
  read_saved_vars(ch);
  return load_result;
}

void string_append( CHAR_DATA *ch, char **pString )
{
  void string_write(struct descriptor_data *d, char **writeto, size_t len,
                    long mailto, void *data);

  send_to_char( "-=======- Entering APPEND Mode -========-\n\r", ch );
  send_to_char( "    Type /h on a new line for help\n\r", ch );
  send_to_char( " Terminate|save with a /s on a blank line.\n\r", ch );
  send_to_char( "-=======================================-\n\r", ch );

  if ( *pString == NULL )
  {
    *pString = str_dup( "" );
  }
  else
    new_send_to_char(ch, "%s", numlineas(*pString));


  free_string(ch->desc->backstr);
  /*if (pString != NULL)
  ch->desc->backstr = strdup(pString);
  else 
  ch->desc->backstr = NULL;*/
  string_write(ch->desc, pString, MAX_STRING_LENGTH, 0, NULL);

  act("$n begins editing a note.", TRUE, ch, 0, 0, TO_ROOM);
  /*STATE(ch->desc) = CON_TEXTED;*/

  return;
}



/* deal with newcomers and other non-playing sockets */
void nanny(struct descriptor_data *d, char *arg)
{
  char buf[128];
  int player_i, load_result;
  char tmp_name[MAX_INPUT_LENGTH];

  /* OasisOLC states */
  struct
  {
    int state;
    void (*func)(struct descriptor_data *, char *);
  }
  olc_functions[] = {
                      { CON_OEDIT, oedit_parse },
                      { CON_ZEDIT, zedit_parse },
                      { CON_SEDIT, sedit_parse },
                      { CON_MEDIT, medit_parse },
                      { CON_REDIT, redit_parse },
                      { CON_CEDIT, cedit_parse },
                      { CON_AEDIT, aedit_parse },
                      { CON_TRIGEDIT, trigedit_parse },
                      //{ CON_HEDIT, hedit_parse},
                      { CON_VEDIT, vedit_parse},
                      { -1, NULL }
                    };

  skip_spaces(&arg);
  /*
   * Quick check for the OLC states.
   */
  for (player_i = 0; olc_functions[player_i].state >= 0; player_i++)
    if (STATE(d) == olc_functions[player_i].state)
    {
      /* send context-sensitive help if need be */
      if (context_help(d, arg)) return;

      (*olc_functions[player_i].func)(d, arg);
      return;
    }

  /* Not in OLC. */
  switch (STATE(d))
  {

  case CON_GET_NAME:		/* wait for input of name */

    if (d->character == NULL)
    {
      CREATE(d->character, struct char_data, 1);
      clear_char(d->character);
      CREATE(d->character->player_specials, struct player_special_data, 1);

      d->character->desc = d;
    }
    if (!*arg)
      STATE(d) = CON_CLOSE;
    else
    {
      if ((_parse_name(arg, tmp_name)) || strlen(tmp_name) < 2 ||
          strlen(tmp_name) > MAX_NAME_LENGTH || !Valid_Name(tmp_name)
          || fill_word(strcpy(buf, tmp_name))
          || reserved_word(buf))
      {
        write_to_output(d,"Invalid name, please try another.\r\n" "Name: ");
        return;
      }
      log("Player name %s entered.", tmp_name);
      if ((player_i = load_char(tmp_name, d->character)) != -1)
      {
        if (player_i == -2)
        {
          write_to_output(d,
                          "I am sorry, there is a problem with your file.\r\n"
                          "To avoid destroying it further. \r\n"
                          "Please do not try and log in with this name.\r\n"
                          "Please email Mordecai@xtra.co.nz with the name of this character, \r\n"
                          "and the date and time you last remember it working.\r\n");
          log("Bad Jelly -- please check %s pfile.", tmp_name);
          STATE(d) = CON_CLOSE;
          return;
        }
        log("Player name %s generated.", GET_NAME(d->character));
        GET_PFILEPOS(d->character) = player_i;

        if (PLR_FLAGGED(d->character, PLR_DELETED))
        {
          if ((player_i = find_name(tmp_name)) >= 0)
            if (!Valid_Name(tmp_name))
            {
              write_to_output(d,"Invalid name, please try another.\r\nName: ");
              return;
            }
          remove_player(player_i);
          /* We get a false positive from the original deleted character. */
          free_char(d->character);
          /* Check for multiple creations... */

          CREATE(d->character, struct char_data, 1);
          clear_char(d->character);
          CREATE(d->character->player_specials,struct player_special_data, 1);

          d->character->desc = d;
          CREATE(d->character->player.name, char, strlen(tmp_name) + 1);
          strcpy(d->character->player.name, CAP(tmp_name));
          GET_PFILEPOS(d->character) = player_i;
          write_to_output(d,"Did I get that right, %s (Y/N)? ",
                          tmp_name);
          STATE(d) = CON_NAME_CNFRM;
        }
        else
        {
          /* undo it just in case they are set */
          REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_WRITING);
          REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_MAILING);
          REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_CRYO);

          write_to_output(d,"Password: ");
          echo_off(d);
          d->idle_tics = 0;
          STATE(d) = CON_PASSWORD;
        }
      }
      else
      {
        /* player unknown -- make new character */

        /* Check for multiple creations of a character. */
        if (!Valid_Name(tmp_name))
        {
          write_to_output(d,"Invalid name, please try another.\r\nName: ");
          return;
        }

        if (!d->character->player_specials)
          CREATE(d->character->player_specials, struct player_special_data, 1);

        CREATE(d->character->player.name, char, strlen(tmp_name) + 1);
        strcpy(d->character->player.name, CAP(tmp_name));
        write_to_output(d, "\r\n"
                        "This is a time travel mud, where there are 4 major dimensions\r\n"
                        "and many minor time zones. \r\n"
                        "%sNames chosen to follow this theme are greatly encouraged.%s\r\n"
                        "The major dimensions being:\r\n"
                        "%sFuture%s       -- Advanced technologies, space travel, implants, Planets\r\n"
                        "%sMedieval%s     -- Farming, Peasants, Swords and Dragons\r\n"
                        "%sPre Historic%s -- Sailing Ships, Ancient Gods, Ancient Terrors\r\n"
                        "%sOld West%s     -- Gunfights, Saloons, Ranching, Gold Mining%s\r\n\r\n",
                        "\x1B[1;32m", "\x1B[0m",
                        "\x1B[0;33m","\x1B[0;32m",
                        "\x1B[0;33m","\x1B[0;32m",
                        "\x1B[0;33m","\x1B[0;32m",
                        "\x1B[0;33m","\x1B[0;32m",
                        "\x1B[0m");
        write_to_output(d, "Did I get that right, %s (Y/N)? ", tmp_name);
        STATE(d) = CON_NAME_CNFRM;
      }
    }
    break;
  case CON_NAME_CNFRM:	/* wait for conf. of new name    */
    if (UPPER(*arg) == 'Y')
    {
      if (isbanned(d->host) >= BAN_NEW)
      {
        new_mudlog(NRM, LVL_GOD, TRUE,
                   "Request for new char %s denied from [%s] (siteban)",
                   GET_PC_NAME(d->character), d->host);
        write_to_output(d,"Sorry, new characters are not allowed at the moment!\r\n");
        STATE(d) = CON_CLOSE;
        return;
      }
      if (circle_restrict)
      {
        write_to_output(d,"Sorry, new players can't be created at the moment.\r\n");
        new_mudlog( NRM, LVL_GOD, TRUE,
                    "Request for new char %s denied from [%s] (wizlock)",
                    GET_PC_NAME(d->character), d->host);
        STATE(d) = CON_CLOSE;
        return;
      }
      write_to_output(d,"New character.\r\n");
      write_to_output(d, "Give me a password for %s: ",  GET_PC_NAME(d->character));
      echo_off(d);


      STATE(d) = CON_NEWPASSWD;
    }
    else if (*arg == 'n' || *arg == 'N')
    {
      write_to_output(d,"Okay, what IS it, then? ");
      free(d->character->player.name);
      d->character->player.name = NULL;
      STATE(d) = CON_GET_NAME;
    }
    else
    {
      write_to_output(d,"Please type Yes or No: ");
    }
    break;
  case CON_PASSWORD:		/* get pwd for known player      */
    /*
     * To really prevent duping correctly, the player's record should
     * be reloaded from disk at this point (after the password has been
     * typed).  However I'm afraid that trying to load a character over
     * an already loaded character is going to cause some problem down the
     * road that I can't see at the moment.  So to compensate, I'm going to
     * (1) add a 15 or 20-second time limit for entering a password, and (2)
     * re-add the code to cut off duplicates when a player quits.  JE 6 Feb 96
     */

    echo_on(d);		/* turn echo back on */
    /* New echo_on() eats the return on telnet. Extra space better than none. */
    write_to_output(d, "%s", "\r\n");

    if (!*arg)
      STATE(d) = CON_CLOSE;
    else
    {
      if (strncmp
          (CRYPT(arg, GET_PASSWD(d->character)),
           GET_PASSWD(d->character), MAX_PWD_LENGTH))
      {
        new_mudlog(BRF, LVL_GOD, TRUE, "Bad PW: %s [%s]", GET_NAME(d->character),
                   d->host);
        GET_BAD_PWS(d->character)++;
        save_char(d->character);
        // log("(nanny)Saved %s in room %d.", GET_NAME(d->character), NOWHERE);
        if (++(d->bad_pws) >= CONFIG_MAX_BAD_PWS)
        {	/* 3 strikes and you're out. */
          write_to_output(d,"Wrong password... disconnecting.\r\n");
          STATE(d) = CON_CLOSE;
        }
        else
        {
          write_to_output(d,"Wrong password.\r\nPassword: ");
          echo_off(d);
        }
        return;
      }

      /* Password was correct. */
      load_result = GET_BAD_PWS(d->character);
      GET_BAD_PWS(d->character) = 0;
      d->bad_pws = 0;

      if (isbanned(d->host) == BAN_SELECT &&
          !PLR_FLAGGED(d->character, PLR_SITEOK))
      {
        write_to_output(d,"Sorry, this char has not been cleared for login from your site!\r\n");
        STATE(d) = CON_CLOSE;
        new_mudlog(NRM, LVL_GOD, TRUE,"Connection attempt for %s denied from %s",
                   GET_NAME(d->character), d->host);
        return;
      }
      if (GET_LEVEL(d->character) < circle_restrict)
      {
        write_to_output(d,"The game is temporarily restricted.. try again later.\r\n");
        STATE(d) = CON_CLOSE;
        new_mudlog( NRM, LVL_GOD, TRUE,
                    "Request for login denied for %s [%s] (wizlock)",
                    GET_NAME(d->character), d->host);
        return;
      }
      /* check and make sure no other copies of this player are logged in */
      if (perform_dupe_check(d))
        return;

      line_sep(d);

      if (GET_LEVEL(d->character) >= LVL_HERO)
        write_to_output(d,"%s",imotd);
      else
        write_to_output(d,"%s",motd);
      if (GET_INVIS_LEV(d->character))
        new_mudlog(BRF, MAX(LVL_IMMORT, GET_INVIS_LEV(d->character)), TRUE,
                   "%s [%s] has connected. (invis %d)", GET_NAME(d->character), d->host,
                   GET_INVIS_LEV(d->character));
      else
        new_mudlog(BRF, LVL_IMMORT, TRUE,
                   "%s [%s] has connected.", GET_NAME(d->character), d->host);
      if (load_result)
      {
        write_to_output(d, "\r\n\r\n\007\007\007"
                        "%s%d LOGIN FAILURE%s SINCE LAST SUCCESSFUL LOGIN.%s\r\n",
                        CCRED(d->character, C_SPR), load_result,
                        (load_result > 1) ? "S" : "", CCNRM(d->character,
                                                            C_SPR));
        GET_BAD_PWS(d->character) = 0;
      }
      write_to_output(d,"\r\n*** PRESS RETURN: ");
      STATE(d) = CON_RMOTD;
    }
    break;

  case CON_NEWPASSWD:
  case CON_CHPWD_GETNEW:
    if (!*arg || strlen(arg) > MAX_PWD_LENGTH || strlen(arg) < 3 ||
        !str_cmp(arg, GET_PC_NAME(d->character)))
    {
      write_to_output(d,"\r\nIllegal password.\r\nPassword: ");
      return;
    }
    strncpy(GET_PASSWD(d->character),
            CRYPT(arg, GET_PC_NAME(d->character)), MAX_PWD_LENGTH);
    *(GET_PASSWD(d->character) + MAX_PWD_LENGTH) = '\0';

    write_to_output(d,"\r\nPlease retype password: ");
    if (STATE(d) == CON_NEWPASSWD)
      STATE(d) = CON_CNFPASSWD;
    else
      STATE(d) = CON_CHPWD_VRFY;

    break;

  case CON_CNFPASSWD:
  case CON_CHPWD_VRFY:
    if (strncmp
        (CRYPT(arg, GET_PASSWD(d->character)),
         GET_PASSWD(d->character), MAX_PWD_LENGTH))
    {
      write_to_output(d,"\r\nPasswords don't match... start over.\r\n");
      write_to_output(d,"Password: ");
      if (STATE(d) == CON_CNFPASSWD)
        STATE(d) = CON_NEWPASSWD;
      else
        STATE(d) = CON_CHPWD_GETNEW;
      return;
    }
    echo_on(d);

    if (STATE(d) == CON_CNFPASSWD)
    {

      write_to_output(d,
                      "\r\n\r\n\r\n"
                      "Welcome to 4Dimensions %s!\r\n"
                      "Time to customize your character.\r\n"
                      "--------------------------------------------\r\n",
                      GET_PC_NAME(d->character));
      STATE(d) = CON_CREATE_NEW;
      SUB_STATE(d) = STATE_ANSI;
      con_character_creation(d, arg);
    }
    else
    {
      save_char(d->character);
      // log("(nanny_chpwd_vrfy)Saved %s in room %d.", GET_NAME(d->character), NOWHERE);
      echo_on(d);
      write_to_output(d,"\r\nDone.\r\n");
      con_disp_menu(d);
    }

    break;
  case CON_CREATE_NEW:
    con_character_creation(d, arg);
    break;
  case CON_RMOTD:		/* read CR after printing motd   */
#ifdef HAVE_ZLIB_H
    if (CONFIG_ENABLE_COMPRESSION && !PRF_FLAGGED(d->character, PRF_NOCOMPRESS))
    {
      d->comp->state = 1;	/* waiting for response to offer */
      write_to_output(d, "%s", compress_offer);
    }
#endif /* HAVE_ZLIB_H */
    line_sep(d);
    con_disp_menu(d);
    break;

  case CON_MENU:		/* get selection from main menu  */
    switch (*arg)
    {
    case '0':
      write_to_output(d,"Goodbye.\r\n");
      STATE(d) = CON_CLOSE;
      break;

    case '1':

      line_sep(d);

      load_result = enter_player_game(d);

      greet_mtrigger(d->character, -1);
      greet_memory_mtrigger(d->character);


      if (has_mail(GET_IDNUM(d->character)))
        write_to_output(d, "You have unreceived mail at the post office.\r\n");
      if (has_note(d->character, NOTE_NOTE) )
        write_to_output(d, "You have unread notes, to view type NOTE\r\n");
      if (has_note(d->character, NOTE_NEWS) )
        write_to_output(d, "You have unread news, to view type NEWS.\r\n");
      if (has_note(d->character, NOTE_CHANGES) )
        write_to_output(d, "You have unread changes, to view type CHANGES.\r\n");
      if (has_note(d->character, NOTE_IDEA))
        write_to_output(d, "You have unread ideas, to view type IDEA.\r\n");
      d->has_prompt = 0;
      /* We've updated to 3.1 - some bits might be set wrongly: */
      REMOVE_BIT_AR(PRF_FLAGS(d->character), PRF_BUILDWALK);
      break;
    case '2':
      list_current_accounts_menu(d);
      STATE(d) = CON_ACCOUNT_CHOOSE;
      break;
    case '3':
      account_manage_menu(d);
      STATE(d) = CON_ACCOUNT_MANAGE;
      break;

    case '4':
      if (d->character->player.description)
      {
        write_to_output(d,"Current description:\r\n%s", d->character->player.description);
        /*
         * Don't free this now... so that the old description gets loaded
         * as the current buffer in the editor.  Do setup the ABORT buffer
         * here, however.
         *
         * free(d->character->player.description);
         * d->character->player.description = NULL;
         */
        d->backstr = str_dup(d->character->player.description);
      }
      write_to_output(d, "Enter the new text you'd like others to see when they look at you.\r\n");
      send_editor_help(d);
      d->str = &d->character->player.description;
      d->max_str = EXDSCR_LENGTH;
      STATE(d) = CON_EXDESC;
      break;

    case '5':
      page_string(d, background, 0);
      STATE(d) = CON_RMOTD;
      break;

    case '6':
      write_to_output(d,"\r\nEnter your old password: ");
      echo_off(d);
      STATE(d) = CON_CHPWD_GETOLD;
      break;

    case '7':
      write_to_output(d,"\r\nEnter your password for verification: ");
      echo_off(d);
      STATE(d) = CON_DELCNF1;
      break;

    default:
      line_sep(d);
      write_to_output(d,"\r\nThat's not a menu choice!\r\n");
      con_disp_menu(d);
      break;
    }

    break;
  case CON_ACCOUNT_CHOOSE:
    do
    {
      int i;
      if ((i = get_account_num(atoi(arg), get_acc_by_id(GET_IDNUM(d->character)))) == -1)
      {
        write_to_output(d, "Invalid number!\r\n");
        con_disp_menu(d);
        return;
      }

      if (player_table[i].id == GET_IDNUM(d->character))
      {
        con_disp_menu(d);
        return;
      }

      /* add the change character stuff here */
      free_char(d->character);
      d->character = NULL;
      CREATE(d->character, struct char_data, 1);
      clear_char(d->character);
      CREATE(d->character->player_specials, struct player_special_data, 1);


      d->character->desc = d;
      if ((player_i = load_char(player_table[i].name, d->character)) != -1)
      {
        if (player_i == -2)
        {
          write_to_output(d,"I am sorry, there is a problem with this character.\r\n"
                          "To avoid destroying it further. Please do not try and log in with this name.\r\n"
                          "Please email Mordecai@xtra.co.nz with the name of this char.\r\n");
          log("Bad Jelly -- please check %s pfile.", player_table[i].name);
          STATE(d) = CON_CLOSE;
          return;
        }
      }
      GET_PFILEPOS(d->character) = player_i;
      write_to_output(d, "You change to character %s.\r\n", player_table[i].name);
      con_disp_menu(d);
    }
    while (0);
    return;
  case CON_ACCOUNT_MANAGE:
    if (parse_accounts(d, arg))
    {
      con_disp_menu(d);
    }
    return;

  case CON_CHPWD_GETOLD:
    if (strncmp
        (CRYPT(arg, GET_PASSWD(d->character)),
         GET_PASSWD(d->character), MAX_PWD_LENGTH))
    {
      echo_on(d);
      write_to_output(d,"\r\nIncorrect password.\r\n");
      con_disp_menu(d);
    }
    else
    {
      write_to_output(d,"\r\nEnter a new password: ");
      STATE(d) = CON_CHPWD_GETNEW;
    }
    return;

  case CON_DELCNF1:
    echo_on(d);
    if (strncmp
        (CRYPT(arg, GET_PASSWD(d->character)),
         GET_PASSWD(d->character), MAX_PWD_LENGTH))
    {
      write_to_output(d,"\r\nIncorrect password.\r\n");
      con_disp_menu(d);
    }
    else
    {
      write_to_output(d,"\r\nYOU ARE ABOUT TO DELETE THIS CHARACTER PERMANENTLY.\r\n"
                      "ARE YOU ABSOLUTELY SURE?\r\n\r\n"
                      "Please type \"yes\" to confirm: ");
      STATE(d) = CON_DELCNF2;
    }
    break;

  case CON_DELCNF2:
    if (compares(arg, "yes") )
    {
      if (PLR_FLAGGED(d->character, PLR_FROZEN))
      {
        write_to_output(d,"You try to kill yourself, but the ice stops you.\r\n");
        write_to_output(d,"Character not deleted.\r\n\r\n");
        STATE(d) = CON_CLOSE;
        return;
      }
      if (GET_LEVEL(d->character) < LVL_GRGOD)
        SET_BIT_AR(PLR_FLAGS(d->character), PLR_DELETED);
      save_char(d->character);
      // log("(nanny_con_del)Saved %s in %d.", GET_NAME(d->character));
      Crash_delete_file(GET_NAME(d->character));
      delete_pobj_file(GET_NAME(d->character));
      delete_aliases(GET_NAME(d->character));
      delete_variables(GET_NAME(d->character));
      if (selfdelete_fastwipe)
        if ((player_i = find_name(GET_NAME(d->character))) >= 0)
        {
          SET_BIT(player_table[player_i].flags,
                  PINDEX_SELFDELETE);
          remove_player(player_i);
        }
      write_to_output(d, "Character '%s' deleted!\r\n"
                      "Goodbye.\r\n", GET_NAME(d->character));
      new_mudlog(NRM, LVL_GOD, TRUE, "%s (lev %d) has self-deleted.",
                 GET_NAME(d->character), GET_LEVEL(d->character));
      STATE(d) = CON_CLOSE;
      return;
    }
    else
    {
      write_to_output(d,"\r\nCharacter not deleted.\r\n");
      con_disp_menu(d);
    }
    break;

    /*
     * It's possible, if enough pulses are missed, to kick someone off
     * while they are at the password prompt. We'll just defer to let
     * the game_loop() axe them.
     */
  case CON_CLOSE:
    break;
  case CON_IDENT:
    /* do nothing */
    break;
  case CON_LINE_INPUT:
    {
      if (d->callback)
      {
        d->callback(d, arg, d->c_data);
        if (--(d->callback_depth) <= 0)
        {
          d->callback = NULL;	// if the function wasn't chained, clean up
          d->callback_depth = 0;	// AND wasn't recursive
          d->c_data = NULL;
          STATE(d) = CON_PLAYING;
        }
      }
      else
      {
        log("SYSERR: No callback function specified for state CON_LINE_INPUT");
        STATE(d) = CON_PLAYING;
      }
    }
    break;

  default:
    log("SYSERR: Nanny: illegal state of con'ness (%d) for '%s'; closing connection.", STATE(d), d->character ? GET_NAME(d->character) : "<unknown>");
    STATE(d) = CON_DISCONNECT;	/* Safest to do. */
    break;
  }
}

void con_disp_menu(DESCRIPTOR_DATA *d)
{
  if (d->character)
    write_to_output(d, "\r\nYou are signed in as %s.\r\n\r\n", GET_NAME(d->character));
  write_to_output(d,"%s",MENU);
  STATE(d) = CON_MENU;
}

