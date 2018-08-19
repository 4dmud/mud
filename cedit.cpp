/************************************************************************
 * OasisOLC - Game configuration / cedit.c                   v2.0     *
 * Copyright 2002-2003 Kip Potter   (kip_potter@hotmail.com)            *
 * A graphical in-game game configuration utility for OasisOLC.         *
 ************************************************************************/

#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "comm.h"
#include "interpreter.h"
#include "utils.h"
#include "db.h"
#include "constants.h"
#include "genolc.h"
#include "oasis.h"
#include "improved-edit.h"
#include "descriptor.h"


/******************************************************************************/
/** External Functions                                                       **/
/******************************************************************************/
void free_config(struct config_data *data);

/******************************************************************************/
/** Internal Macros                                                          **/
/******************************************************************************/
#define NO 0
#define YES 1

#define CHECK_VAR(var)  ((var == YES) ? "Yes" : "No")
#define TOGGLE_VAR(var)  if (var == YES) { var = NO; } else { var = YES; }


/******************************************************************************/
/** Internal Functions                                                       **/
/******************************************************************************/
void cedit_disp_menu(Descriptor *d);
void cedit_setup(Descriptor *d);
void cedit_save_to_disk( void );
int  save_config( int nowhere );
void reassign_rooms(void);

/******************************************************************************/
/** Routines                                                                 **/
/******************************************************************************/
ACMD(do_oasis_cedit)
{
  Descriptor *d;
  char buf1[MAX_STRING_LENGTH];

  /****************************************************************************/
  /** Parse any arguments.                                                   **/
  /****************************************************************************/
  one_argument(argument, buf1);
  //should be catched by the trust groups and such
//  if (GET_LEVEL(ch) < LVL_IMPL) {
//    ch->Send( "You can't modify the game configuration.\r\n");
//    return;
//  }

  d = ch->desc;

  if (!*buf1) {
    CREATE(d->olc, struct oasis_olc_data, 1);
    OLC_ZONE(d) = 0;
    cedit_setup(d);
    STATE(d) = CON_CEDIT;
    act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
    SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);

    new_mudlog(BRF, LVL_IMMORT, TRUE,
      "OLC: %s starts editing the game configuration.", GET_NAME(ch));
    return;
  } else if (str_cmp("save", buf1) != 0) {
    ch->Send( "Yikes!  Stop that, someone will get hurt!\r\n");
    return;
  }

  ch->Send( "Saving the game configuration.\r\n");
  new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(ch)), TRUE,
    "OLC: %s saves the game configuration.", GET_NAME(ch));

  cedit_save_to_disk();
}

/*-------------------------------------------------------------------*/

void cedit_setup(Descriptor *d)
{
  /****************************************************************************/
  /** Create the config_data struct.                                         **/
  /****************************************************************************/
  CREATE(OLC_CONFIG(d), struct config_data, 1);

  /****************************************************************************/
  /** Copy the current configuration from the config_info to this one.       **/
  /****************************************************************************/
  /****************************************************************************/
  /** Copy the game play options from the configuration info struct.         **/
  /****************************************************************************/
  OLC_CONFIG(d)->play.pk_allowed          = CONFIG_PK_ALLOWED;
  OLC_CONFIG(d)->play.pt_allowed          = CONFIG_PT_ALLOWED;
  OLC_CONFIG(d)->play.level_can_shout     = CONFIG_LEVEL_CAN_SHOUT;
  OLC_CONFIG(d)->play.holler_move_cost    = CONFIG_HOLLER_MOVE_COST;
  OLC_CONFIG(d)->play.tunnel_size         = CONFIG_TUNNEL_SIZE;
  OLC_CONFIG(d)->play.max_exp_gain        = CONFIG_MAX_EXP_GAIN;
  OLC_CONFIG(d)->play.max_exp_loss        = CONFIG_MAX_EXP_LOSS;
  OLC_CONFIG(d)->play.max_npc_corpse_time = CONFIG_MAX_NPC_CORPSE_TIME;
  OLC_CONFIG(d)->play.max_pc_corpse_time  = CONFIG_MAX_PC_CORPSE_TIME;
  OLC_CONFIG(d)->play.idle_void           = CONFIG_IDLE_VOID;
  OLC_CONFIG(d)->play.idle_rent_time      = CONFIG_IDLE_RENT_TIME;
  OLC_CONFIG(d)->play.idle_max_level      = CONFIG_IDLE_MAX_LEVEL;
  OLC_CONFIG(d)->play.dts_are_dumps       = CONFIG_DTS_ARE_DUMPS;
  OLC_CONFIG(d)->play.load_into_inventory = CONFIG_LOAD_INVENTORY;
  OLC_CONFIG(d)->play.track_through_doors = CONFIG_TRACK_T_DOORS;
  OLC_CONFIG(d)->play.immort_level_ok     = CONFIG_IMMORT_LEVEL_OK;
  OLC_CONFIG(d)->play.double_exp	  = CONFIG_DOUBLE_EXP;

  /****************************************************************************/
  /** Crash Saves                                                            **/
  /****************************************************************************/
  OLC_CONFIG(d)->csd.free_rent            = CONFIG_FREE_RENT;
  OLC_CONFIG(d)->csd.max_obj_save         = CONFIG_MAX_OBJ_SAVE;
  OLC_CONFIG(d)->csd.min_rent_cost        = CONFIG_MIN_RENT_COST;
  OLC_CONFIG(d)->csd.auto_save            = CONFIG_AUTO_SAVE;
  OLC_CONFIG(d)->csd.autosave_time        = CONFIG_AUTOSAVE_TIME;
  OLC_CONFIG(d)->csd.crash_file_timeout   = CONFIG_CRASH_TIMEOUT;
  OLC_CONFIG(d)->csd.rent_file_timeout    = CONFIG_RENT_TIMEOUT;

  /****************************************************************************/
  /** Room Numbers                                                           **/
  /****************************************************************************/
  OLC_CONFIG(d)->room_nums.mortal_start_room = CONFIG_MORTAL_START->number;
  OLC_CONFIG(d)->room_nums.immort_start_room = CONFIG_IMMORTAL_START->number;
  OLC_CONFIG(d)->room_nums.frozen_start_room = CONFIG_FROZEN_START->number;
  OLC_CONFIG(d)->room_nums.donation_room_1   = CONFIG_DON_ROOM_1->number;
  OLC_CONFIG(d)->room_nums.donation_room_2   = CONFIG_DON_ROOM_2->number;
  OLC_CONFIG(d)->room_nums.donation_room_3   = CONFIG_DON_ROOM_3->number;
  OLC_CONFIG(d)->room_nums.gladiator_death_room = CONFIG_GLA_DEATH_ROOM->number;

  /****************************************************************************/
  /** Game Operation                                                         **/
  /****************************************************************************/
  OLC_CONFIG(d)->operation.DFLT_PORT          = CONFIG_DFLT_PORT;
  OLC_CONFIG(d)->operation.max_playing        = CONFIG_MAX_PLAYING;
  OLC_CONFIG(d)->operation.max_filesize       = CONFIG_MAX_FILESIZE;
  OLC_CONFIG(d)->operation.max_bad_pws        = CONFIG_MAX_BAD_PWS;
  OLC_CONFIG(d)->operation.siteok_everyone    = CONFIG_SITEOK_ALL;
  OLC_CONFIG(d)->operation.use_new_socials    = CONFIG_NEW_SOCIALS;
  OLC_CONFIG(d)->operation.auto_save_olc      = CONFIG_OLC_SAVE;
  OLC_CONFIG(d)->operation.nameserver_is_slow = CONFIG_NS_IS_SLOW;

  /****************************************************************************/
  /** Autowiz                                                                **/
  /****************************************************************************/
  OLC_CONFIG(d)->autowiz.use_autowiz          = CONFIG_USE_AUTOWIZ;
  OLC_CONFIG(d)->autowiz.min_wizlist_lev      = CONFIG_MIN_WIZLIST_LEV;


  /****************************************************************************/
  /** Allocate space for the strings.                                        **/
  /****************************************************************************/
  OLC_CONFIG(d)->play.OK       = str_udup(CONFIG_OK);
  OLC_CONFIG(d)->play.NOPERSON = str_udup(CONFIG_NOPERSON);
  OLC_CONFIG(d)->play.NOEFFECT = str_udup(CONFIG_NOEFFECT);

  if (CONFIG_DFLT_IP)
    OLC_CONFIG(d)->operation.DFLT_IP     = strdup(CONFIG_DFLT_IP);
  else
    OLC_CONFIG(d)->operation.DFLT_IP     = NULL;

  if (CONFIG_DFLT_DIR) {
    if (OLC_CONFIG(d)->operation.DFLT_DIR)
      free(OLC_CONFIG(d)->operation.DFLT_DIR);
    OLC_CONFIG(d)->operation.DFLT_DIR    = strdup(CONFIG_DFLT_DIR);
  } else
    OLC_CONFIG(d)->operation.DFLT_DIR    = NULL;

  if (CONFIG_LOGNAME) {
    if (OLC_CONFIG(d)->operation.LOGNAME != NULL)
      free(OLC_CONFIG(d)->operation.LOGNAME);
    OLC_CONFIG(d)->operation.LOGNAME     = strdup(CONFIG_LOGNAME);
  } else
    OLC_CONFIG(d)->operation.LOGNAME     = NULL;

  if (CONFIG_MENU)
    OLC_CONFIG(d)->operation.MENU        = strdup(CONFIG_MENU);
  else
    OLC_CONFIG(d)->operation.MENU        = NULL;

  if (CONFIG_WELC_MESSG)
    OLC_CONFIG(d)->operation.WELC_MESSG  = strdup(CONFIG_WELC_MESSG);
  else
    OLC_CONFIG(d)->operation.WELC_MESSG  = NULL;

  if (CONFIG_START_MESSG)
    OLC_CONFIG(d)->operation.START_MESSG = strdup(CONFIG_START_MESSG);
  else
    OLC_CONFIG(d)->operation.START_MESSG = NULL;

  cedit_disp_menu(d);
}

/******************************************************************************/

void cedit_save_internally(Descriptor *d)
{
  /* see if we need to reassign spec procs on rooms */
  int reassign = (CONFIG_DTS_ARE_DUMPS != OLC_CONFIG(d)->play.dts_are_dumps);
  /****************************************************************************/
  /** Copy the data back from the descriptor to the config_info structure.   **/
  /****************************************************************************/
  CONFIG_PK_ALLOWED          = OLC_CONFIG(d)->play.pk_allowed;
  CONFIG_PT_ALLOWED          = OLC_CONFIG(d)->play.pt_allowed;
  CONFIG_LEVEL_CAN_SHOUT     = OLC_CONFIG(d)->play.level_can_shout;
  CONFIG_HOLLER_MOVE_COST    = OLC_CONFIG(d)->play.holler_move_cost;
  CONFIG_TUNNEL_SIZE         = OLC_CONFIG(d)->play.tunnel_size;
  CONFIG_MAX_EXP_GAIN        = OLC_CONFIG(d)->play.max_exp_gain;
  CONFIG_MAX_EXP_LOSS        = OLC_CONFIG(d)->play.max_exp_loss;
  CONFIG_MAX_NPC_CORPSE_TIME = OLC_CONFIG(d)->play.max_npc_corpse_time;
  CONFIG_MAX_PC_CORPSE_TIME  = OLC_CONFIG(d)->play.max_pc_corpse_time;
  CONFIG_IDLE_VOID           = OLC_CONFIG(d)->play.idle_void;
  CONFIG_IDLE_RENT_TIME      = OLC_CONFIG(d)->play.idle_rent_time;
  CONFIG_IDLE_MAX_LEVEL      = OLC_CONFIG(d)->play.idle_max_level;
  CONFIG_DTS_ARE_DUMPS       = OLC_CONFIG(d)->play.dts_are_dumps;
  CONFIG_LOAD_INVENTORY = OLC_CONFIG(d)->play.load_into_inventory;
  CONFIG_TRACK_T_DOORS = OLC_CONFIG(d)->play.track_through_doors;
  CONFIG_IMMORT_LEVEL_OK     = OLC_CONFIG(d)->play.immort_level_ok;
  CONFIG_DOUBLE_EXP 	     = OLC_CONFIG(d)->play.double_exp;

  /****************************************************************************/
  /** Crash Saves                                                            **/
  /****************************************************************************/
  CONFIG_FREE_RENT            = OLC_CONFIG(d)->csd.free_rent;
  CONFIG_MAX_OBJ_SAVE         = OLC_CONFIG(d)->csd.max_obj_save;
  CONFIG_MIN_RENT_COST        = OLC_CONFIG(d)->csd.min_rent_cost;
  CONFIG_AUTO_SAVE            = OLC_CONFIG(d)->csd.auto_save;
  CONFIG_AUTOSAVE_TIME        = OLC_CONFIG(d)->csd.autosave_time;
  CONFIG_CRASH_TIMEOUT   = OLC_CONFIG(d)->csd.crash_file_timeout;
  CONFIG_RENT_TIMEOUT    = OLC_CONFIG(d)->csd.rent_file_timeout;

  /****************************************************************************/
  /** Room Numbers                                                           **/
  /****************************************************************************/
  config_info.room_nums.mortal_start_room = (OLC_CONFIG(d)->room_nums.mortal_start_room);
  config_info.room_nums.immort_start_room = (OLC_CONFIG(d)->room_nums.immort_start_room);
  config_info.room_nums.frozen_start_room = (OLC_CONFIG(d)->room_nums.frozen_start_room);
  config_info.room_nums.donation_room_1   = OLC_CONFIG(d)->room_nums.donation_room_1;
  config_info.room_nums.donation_room_2   = OLC_CONFIG(d)->room_nums.donation_room_2;
  config_info.room_nums.donation_room_3   = OLC_CONFIG(d)->room_nums.donation_room_3;
  config_info.room_nums.gladiator_death_room = OLC_CONFIG(d)->room_nums.gladiator_death_room;

  /****************************************************************************/
  /** Game Operation                                                         **/
  /****************************************************************************/
  CONFIG_DFLT_PORT          = OLC_CONFIG(d)->operation.DFLT_PORT;
  CONFIG_MAX_PLAYING        = OLC_CONFIG(d)->operation.max_playing;
  CONFIG_MAX_FILESIZE       = OLC_CONFIG(d)->operation.max_filesize;
  CONFIG_MAX_BAD_PWS        = OLC_CONFIG(d)->operation.max_bad_pws;
  CONFIG_SITEOK_ALL         = OLC_CONFIG(d)->operation.siteok_everyone;
  CONFIG_NEW_SOCIALS        = OLC_CONFIG(d)->operation.use_new_socials;
  CONFIG_NS_IS_SLOW         = OLC_CONFIG(d)->operation.nameserver_is_slow;
  CONFIG_OLC_SAVE           = OLC_CONFIG(d)->operation.auto_save_olc;

  /****************************************************************************/
  /** Autowiz                                                                **/
  /****************************************************************************/
  CONFIG_USE_AUTOWIZ          = OLC_CONFIG(d)->autowiz.use_autowiz;
  CONFIG_MIN_WIZLIST_LEV      = OLC_CONFIG(d)->autowiz.min_wizlist_lev;

  /****************************************************************************/
  /** Allocate space for the strings.                                        **/
  /****************************************************************************/
  if (CONFIG_OK)
    free(CONFIG_OK);
  CONFIG_OK       = str_udup(OLC_CONFIG(d)->play.OK);

  if (CONFIG_NOPERSON)
    free(CONFIG_NOPERSON);
  CONFIG_NOPERSON = str_udup(OLC_CONFIG(d)->play.NOPERSON);

  if (CONFIG_NOEFFECT)
    free(CONFIG_NOEFFECT);
  CONFIG_NOEFFECT = str_udup(OLC_CONFIG(d)->play.NOEFFECT);

  if (CONFIG_DFLT_IP)
    free(CONFIG_DFLT_IP);
  if (OLC_CONFIG(d)->operation.DFLT_IP)
    CONFIG_DFLT_IP     = strdup(OLC_CONFIG(d)->operation.DFLT_IP);
  else
    CONFIG_DFLT_IP     = NULL;


  if (CONFIG_DFLT_DIR)
    free(CONFIG_DFLT_DIR);
  if (OLC_CONFIG(d)->operation.DFLT_DIR)
    CONFIG_DFLT_DIR    = strdup(OLC_CONFIG(d)->operation.DFLT_DIR);
  else
    CONFIG_DFLT_DIR    = NULL;

  if (CONFIG_LOGNAME)
    free(CONFIG_LOGNAME);
  if (OLC_CONFIG(d)->operation.LOGNAME)
    CONFIG_LOGNAME     = strdup(OLC_CONFIG(d)->operation.LOGNAME);
  else
    CONFIG_LOGNAME     = NULL;

  if (CONFIG_MENU)
    free(CONFIG_MENU);
  if (OLC_CONFIG(d)->operation.MENU)
    CONFIG_MENU        = strdup(OLC_CONFIG(d)->operation.MENU);
  else
    CONFIG_MENU        = NULL;

  if (CONFIG_WELC_MESSG)
    free(CONFIG_WELC_MESSG);
  if (OLC_CONFIG(d)->operation.WELC_MESSG)
    CONFIG_WELC_MESSG  = strdup(OLC_CONFIG(d)->operation.WELC_MESSG);
  else
    CONFIG_WELC_MESSG  = NULL;

  if (CONFIG_START_MESSG)
    free(CONFIG_START_MESSG);
  if (OLC_CONFIG(d)->operation.START_MESSG)
    CONFIG_START_MESSG = strdup(OLC_CONFIG(d)->operation.START_MESSG);
  else
    CONFIG_START_MESSG = NULL;

  /* if we changed the dts to/from dumps, reassign - Welcor */
  if (reassign)
    reassign_rooms();

  add_to_save_list(NOWHERE, SL_CFG);
}

/******************************************************************************/

void cedit_save_to_disk( void )
{
  /****************************************************************************/
  /** Just call save_config and get it over with.                            **/
  /****************************************************************************/
  save_config( NOWHERE );
}

/******************************************************************************/

int save_config( int nowhere )
{
  FILE *fl;
  char buf[MAX_STRING_LENGTH];

  if (!(fl = fopen(CONFIG_CONFFILE, "w"))) {
    perror("SYSERR: save_config");
    return (FALSE);
  }

  fprintf(fl,
    "* This file is autogenerated by OasisOLC (CEdit).\n"
    "* Please note the following information about this file's format.\n"
    "*\n"
    "* - If variable is a yes/no or true/false based variable, use 1's and 0's\n"
    "*   where YES or TRUE = 1 and NO or FALSE = 0.\n"
    "* - Variable names in this file are case-insensitive.  Variable values\n"
    "*   are not case-insensitive.\n"
    "* -----------------------------------------------------------------------\n"
    "* Lines starting with * are comments, and are not parsed.\n"
    "* -----------------------------------------------------------------------\n\n"
    "* [ Game Play Options ]\n"
  );

  fprintf(fl, "* Is player killing allowed on the mud?\n"
              "pk_allowed = %d\n\n", CONFIG_PK_ALLOWED);
  fprintf(fl, "* Is player thieving allowed on the mud?\n"
           "pt_allowed = %d\n\n", CONFIG_PT_ALLOWED);
  fprintf(fl, "* What is the minimum level a player can shout/gossip/etc?\n"
              "level_can_shout = %d\n\n", CONFIG_LEVEL_CAN_SHOUT);
  fprintf(fl, "* How many movement points does shouting cost the player?\n"
           "holler_move_cost = %d\n\n", CONFIG_HOLLER_MOVE_COST);
  fprintf(fl, "* How many players can fit in a tunnel?\n"
              "tunnel_size = %d\n\n", CONFIG_TUNNEL_SIZE);
  fprintf(fl, "* Maximum experience gainable per kill?\n"
              "max_exp_gain = %d\n\n", CONFIG_MAX_EXP_GAIN);
  fprintf(fl, "* Maximum experience loseable per death?\n"
              "max_exp_loss = %d\n\n", CONFIG_MAX_EXP_LOSS);
  fprintf(fl, "* Number of tics before NPC corpses decompose.\n"
              "max_npc_corpse_time = %d\n\n", CONFIG_MAX_NPC_CORPSE_TIME);
  fprintf(fl, "* Number of tics before PC corpses decompose.\n"
              "max_pc_corpse_time = %d\n\n", CONFIG_MAX_PC_CORPSE_TIME);
  fprintf(fl, "* Number of tics before a PC is sent to the void.\n"
              "idle_void = %d\n\n", CONFIG_IDLE_VOID);
  fprintf(fl, "* Number of tics before a PC is autorented.\n"
              "idle_rent_time = %d\n\n", CONFIG_IDLE_RENT_TIME);
  fprintf(fl, "* Level and above of players whom are immune to idle penalties.\n"
              "idle_max_level = %d\n\n", CONFIG_IDLE_MAX_LEVEL);
  fprintf(fl, "* Should the items in death traps be junked automatically?\n"
              "dts_are_dumps = %d\n\n", CONFIG_DTS_ARE_DUMPS);
  fprintf(fl, "* When an immortal loads an object, should it load into their inventory?\n"
              "load_into_inventory = %d\n\n", CONFIG_LOAD_INVENTORY);
  fprintf(fl, "* Should PC's be able to track through hidden or closed doors?\n"
              "track_through_doors = %d\n\n", CONFIG_TRACK_T_DOORS);
  fprintf(fl, "* Should players who reach enough exp automatically level to immortal?\n"
              "immort_level_ok = %d\n\n", CONFIG_IMMORT_LEVEL_OK);
  fprintf(fl, "* Should players get double experience?\n"
              "double_exp = %d\n\n", CONFIG_DOUBLE_EXP);

  strcpy(buf, CONFIG_OK);
  strip_cr(buf);

  fprintf(fl, "* Text sent to players when OK is all that is needed.\n"
              "ok = %s\n\n", buf);

  strcpy(buf, CONFIG_NOPERSON);
  strip_cr(buf);

  fprintf(fl, "* Text sent to players when noone is available.\n"
              "noperson = %s\n\n", buf);

  strcpy(buf, CONFIG_NOEFFECT);
  strip_cr(buf);

  fprintf(fl, "* Text sent to players when an effect fails.\n"
              "noeffect = %s\n", buf);



  /************************************************************************
   ** RENT / CRASHSAVE OPTIONS                                  **
   ************************************************************************/
  fprintf(fl, "\n\n\n* [ Rent/Crashsave Options ]\n");

  fprintf(fl, "* Should the MUD allow you to 'rent' for free?  (i.e. if you just quit,\n"
              "* your objects are saved at no cost, as in Merc-type MUDs.)\n"
              "free_rent = %d\n\n", CONFIG_FREE_RENT);

  fprintf(fl, "* Maximum number of items players are allowed to rent.\n"
           "max_obj_save = %d\n\n", CONFIG_MAX_OBJ_SAVE);

  fprintf(fl, "* Should the game automatically save people?\n"
              "auto_save = %d\n\n", CONFIG_AUTO_SAVE);

  fprintf(fl, "* If auto_save = 1, how often (in minutes) should the game save people's objects?\n"
              "autosave_time = %d\n\n", CONFIG_AUTOSAVE_TIME);

  fprintf(fl, "* Lifetime of crashfiles and force-rent (idlesave) files in days.\n"
              "crash_file_timeout = %d\n\n", CONFIG_CRASH_TIMEOUT);

  fprintf(fl, "* Lifetime of normal rent files in days.\n"
              "rent_file_timeout = %d\n\n", CONFIG_RENT_TIMEOUT);


  /************************************************************************
   ** ROOM NUMBERS                                 **
   ************************************************************************/
  fprintf(fl, "\n\n\n* [ Room Numbers ]\n");

  fprintf(fl, "* The virtual number of the room that mortals should enter at.\n"
           "mortal_start_room = %d\n\n", CONFIG_MORTAL_START->number);

  fprintf(fl, "* The virtual number of the room that immorts should enter at.\n"
              "immort_start_room = %d\n\n", CONFIG_IMMORTAL_START->number);

  fprintf(fl, "* The virtual number of the room that frozen people should enter at.\n"
           "frozen_start_room = %d\n\n", CONFIG_FROZEN_START->number);

  fprintf(fl, "* The virtual numbers of the donation rooms.  Note: Add donation rooms\n"
              "* sequentially (1 & 2 before 3). If you don't, you might not be able to\n"
              "* donate. Use -1 for 'no such room'.\n"
              "donation_room_1 = %d\n"
              "donation_room_2 = %d\n"
              "donation_room_3 = %d\n\n",
              CONFIG_DON_ROOM_1 != NULL ? CONFIG_DON_ROOM_1->number : -1,
              CONFIG_DON_ROOM_2 != NULL ? CONFIG_DON_ROOM_2->number : -1,
              CONFIG_DON_ROOM_3 != NULL ? CONFIG_DON_ROOM_3->number : -1);

  fprintf(fl, "* The room dead gladiators should go to.\n"
           "gladiator_death_room = %d\n\n", CONFIG_GLA_DEATH_ROOM->number);

  fprintf(fl, "\n\n\n* [ Game Operation Options ]\n");

  fprintf(fl, "* This is the default port on which the game should run if no port is\n"
              "* given on the command-line.  NOTE WELL: If you're using the\n"
              "* 'autorun' script, the port number there will override this setting.\n"
              "* Change the PORT= line in autorun instead of (or in addition to)\n"
              "* changing this.\n"
              "DFLT_PORT = %d\n\n",
              CONFIG_DFLT_PORT);

  if (CONFIG_DFLT_IP) {
    strcpy(buf, CONFIG_DFLT_IP);
    strip_cr(buf);

    fprintf(fl, "* IP address to which the MUD should bind.\nDFLT_IP = %s\n\n", buf);
  }

  if (CONFIG_DFLT_DIR) {
    strcpy(buf, CONFIG_DFLT_DIR);
    strip_cr(buf);

    fprintf(fl, "* default directory to use as data directory.\n"
                "DFLT_DIR = %s\n\n", buf);
  }

  if (CONFIG_LOGNAME) {
    strcpy(buf, CONFIG_LOGNAME);
    strip_cr(buf);

    fprintf(fl, "* What file to log messages to (ex: 'log/syslog').\n"
                "LOGNAME = %s\n\n", buf);
  }

  fprintf(fl, "* Maximum number of players allowed before game starts to turn people away.\n"
              "max_playing = %d\n\n",
              CONFIG_MAX_PLAYING);

  fprintf(fl, "* Maximum size of bug, typo, and idea files in bytes (to prevent bombing).\n"
              "max_filesize = %d\n\n",
              CONFIG_MAX_FILESIZE);

  fprintf(fl, "* Maximum number of password attempts before disconnection.\n"
              "max_bad_pws = %d\n\n",
              CONFIG_MAX_BAD_PWS);

  fprintf(fl, "* Is the site ok for everyone except those that are banned?\n"
              "siteok_everyone = %d\n\n",
              CONFIG_SITEOK_ALL);

  fprintf(fl, "* If you want to use the original social file format\n"
              "* and disable Aedit, set to 0, otherwise, 1.\n"
              "use_new_socials = %d\n\n",
              CONFIG_NEW_SOCIALS);

  fprintf(fl, "* If the nameserver is fast, set to 0, otherwise, 1.\n"
              "nameserver_is_slow = %d\n\n",
              CONFIG_NS_IS_SLOW);

  fprintf(fl, "* Should OLC autosave to disk (1) or save internally (0).\n"
              "auto_save_olc = %d\n\n",
              CONFIG_OLC_SAVE);

  if (CONFIG_MENU) {
    strcpy(buf, CONFIG_MENU);
    strip_cr(buf);

    fprintf(fl, "* The entrance/exit menu.\n"
                "MENU = \n%s~\n\n", buf);
  }

  if (CONFIG_WELC_MESSG) {
    strcpy(buf, CONFIG_WELC_MESSG);
    strip_cr(buf);

    fprintf(fl, "* The welcome message.\nWELC_MESSG = \n%s~\n\n", buf);
  }

  if (CONFIG_START_MESSG) {
    strcpy(buf, CONFIG_START_MESSG);
    strip_cr(buf);

    fprintf(fl, "* NEWBIE start message.\n"
                "START_MESSG = \n%s~\n\n", buf);
  }

  fprintf(fl, "\n\n\n* [ Autowiz Options ]\n");

  fprintf(fl, "* Should the game automatically create a new wizlist/immlist every time\n"
              "* someone immorts, or is promoted to a higher (or lower) god level?\n"
              "use_autowiz = %d\n\n",
              CONFIG_USE_AUTOWIZ);

  fprintf(fl, "* If yes, what is the lowest level which should be on the wizlist?\n"
              "min_wizlist_lev = %d\n\n",
              CONFIG_MIN_WIZLIST_LEV);


  fclose(fl);

  if (in_save_list(NOWHERE, SL_CFG))
    remove_from_save_list(NOWHERE, SL_CFG);

  return (TRUE);
}

/**************************************************************************
 Menu functions
 **************************************************************************/

/*
 * the main menu
 */
void cedit_disp_menu(Descriptor *d)
{
  get_char_colours(d->character);
  clear_screen(d);

  /*
   * Menu header
   */
  d->Output(
       "OasisOLC MUD Configuration Editor\r\n"
       "%sG%s) Game Play Options\r\n"
       "%sC%s) Crashsave/Rent Options\r\n"
       "%sR%s) Room Numbers\r\n"
          "%sO%s) Operation Options\r\n"
          "%sA%s) Autowiz Options\r\n"
          "%sQ%s) Quit\r\n"
          "Enter your choice : ",

          grn, nrm,
          grn, nrm,
          grn, nrm,
          grn, nrm,
          grn, nrm,
          grn, nrm
          );

  OLC_MODE(d) = CEDIT_MAIN_MENU;
}

/*-------------------------------------------------------------------*/

void cedit_disp_game_play_options(Descriptor *d)
{
  get_char_colours(d->character);
  clear_screen(d);

  d->Output( "\r\n\r\n"
        "%sA%s) Player Killing Allowed  : %s%s\r\n"
        "%sB%s) Player Thieving Allowed : %s%s\r\n"
        "%sC%s) Minimum Level To Shout  : %s%d\r\n"
        "%sD%s) Holler Move Cost        : %s%d\r\n"
        "%sE%s) Tunnel Size             : %s%d\r\n"
        "%sF%s) Maximum Experience Gain : %s%d\r\n"
        "%sG%s) Maximum Experience Loss : %s%d\r\n"
        "%sH%s) Max Time for NPC Corpse : %s%d\r\n"
        "%sI%s) Max Time for PC Corpse  : %s%d\r\n"
        "%sJ%s) Tics before PC sent to void : %s%d\r\n"
        "%sK%s) Tics before PC is autosaved : %s%d\r\n"
        "%sL%s) Level Immune To IDLE        : %s%d\r\n"
        "%sM%s) Death Traps Junk Items      : %s%s\r\n"
        "%sN%s) Objects Load Into Inventory : %s%s\r\n"
        "%sO%s) Track Through Doors         : %s%s\r\n"
        "%sP%s) Mortals Level To Immortal   : %s%s\r\n"
        "%sR%s) Double Experience Day	    : %s%s\r\n"
        "%s1%s) OK Message Text         : %s%s"
        "%s2%s) NOPERSON Message Text   : %s%s"
        "%s3%s) NOEFFECT Message Text   : %s%s"
        "%sQ%s) Exit To The Main Menu\r\n"
        "Enter your choice : ",
        grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->play.pk_allowed),
        grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->play.pt_allowed),
        grn, nrm, cyn, OLC_CONFIG(d)->play.level_can_shout,
        grn, nrm, cyn, OLC_CONFIG(d)->play.holler_move_cost,
        grn, nrm, cyn, OLC_CONFIG(d)->play.tunnel_size,
        grn, nrm, cyn, OLC_CONFIG(d)->play.max_exp_gain,
        grn, nrm, cyn, OLC_CONFIG(d)->play.max_exp_loss,
        grn, nrm, cyn, OLC_CONFIG(d)->play.max_npc_corpse_time,
        grn, nrm, cyn, OLC_CONFIG(d)->play.max_pc_corpse_time,

        grn, nrm, cyn, OLC_CONFIG(d)->play.idle_void,
        grn, nrm, cyn, OLC_CONFIG(d)->play.idle_rent_time,
        grn, nrm, cyn, OLC_CONFIG(d)->play.idle_max_level,
        grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->play.dts_are_dumps),
        grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->play.load_into_inventory),
        grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->play.track_through_doors),
        grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->play.immort_level_ok),
        grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->play.double_exp),

        grn, nrm, cyn, OLC_CONFIG(d)->play.OK,
        grn, nrm, cyn, OLC_CONFIG(d)->play.NOPERSON,
        grn, nrm, cyn, OLC_CONFIG(d)->play.NOEFFECT,

        grn, nrm
        );

  OLC_MODE(d) = CEDIT_GAME_OPTIONS_MENU;
}

/*-------------------------------------------------------------------*/

void cedit_disp_crash_save_options(Descriptor *d)
{
  get_char_colours(d->character);
  clear_screen(d);

  d->Output( "\r\n\r\n"
     "%sA%s) Free Rent          : %s%s\r\n"
     "%sB%s) Max Objects Saved  : %s%d\r\n"
     "%sC%s) Minimum Rent Cost  : %s%d\r\n"
     "%sD%s) Auto Save          : %s%s\r\n"
     "%sE%s) Auto Save Time     : %s%d minute(s)\r\n"
     "%sF%s) Crash File Timeout : %s%d day(s)\r\n"
     "%sG%s) Rent File Timeout  : %s%d day(s)\r\n"
     "%sQ%s) Exit To The Main Menu\r\n"
     "Enter your choice : ",
     grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->csd.free_rent),
     grn, nrm, cyn, OLC_CONFIG(d)->csd.max_obj_save,
     grn, nrm, cyn, OLC_CONFIG(d)->csd.min_rent_cost,
     grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->csd.auto_save),
     grn, nrm, cyn, OLC_CONFIG(d)->csd.autosave_time,
     grn, nrm, cyn, OLC_CONFIG(d)->csd.crash_file_timeout,
     grn, nrm, cyn, OLC_CONFIG(d)->csd.rent_file_timeout,
     grn, nrm
     );

  OLC_MODE(d) = CEDIT_CRASHSAVE_OPTIONS_MENU;
}

/*-------------------------------------------------------------------*/

void cedit_disp_room_numbers(Descriptor *d)
{
  get_char_colours(d->character);
  clear_screen(d);

  d->Output( "\r\n\r\n"
     "%sA%s) Mortal Start Room   : %s%d\r\n"
     "%sB%s) Immortal Start Room : %s%d\r\n"
     "%sC%s) Frozen Start Room   : %s%d\r\n"
     "%s1%s) Donation Room #1    : %s%d\r\n"
     "%s2%s) Donation Room #2    : %s%d\r\n"
     "%s3%s) Donation Room #3    : %s%d\r\n"
     "%s4%s) Gladiator Death Room: %s%d\r\n"
     "%sQ%s) Exit To The Main Menu\r\n"
     "Enter your choice : ",
     grn, nrm, cyn, OLC_CONFIG(d)->room_nums.mortal_start_room,
     grn, nrm, cyn, OLC_CONFIG(d)->room_nums.immort_start_room,
     grn, nrm, cyn, OLC_CONFIG(d)->room_nums.frozen_start_room,
     grn, nrm, cyn, OLC_CONFIG(d)->room_nums.donation_room_1,
     grn, nrm, cyn, OLC_CONFIG(d)->room_nums.donation_room_2,
     grn, nrm, cyn, OLC_CONFIG(d)->room_nums.donation_room_3,
     grn, nrm, cyn, OLC_CONFIG(d)->room_nums.gladiator_death_room,
     grn, nrm
     );

  OLC_MODE(d) = CEDIT_ROOM_NUMBERS_MENU;
}


/*-------------------------------------------------------------------*/

void cedit_disp_operation_options(Descriptor *d)
{
  get_char_colours(d->character);
  clear_screen(d);

  d->Output( "\r\n\r\n"
     "%sA%s) Default Port : %s%d\r\n"
     "%sB%s) Default IP   : %s%s\r\n"
     "%sC%s) Default Directory   : %s%s\r\n"
     "%sD%s) Logfile Name        : %s%s\r\n"
     "%sE%s) Max Players         : %s%d\r\n"
     "%sF%s) Max Filesize        : %s%d\r\n"
     "%sG%s) Max Bad Pws         : %s%d\r\n"
     "%sH%s) Site Ok Everyone    : %s%s\r\n"
     "%sI%s) Name Server Is Slow : %s%s\r\n"
        "%sJ%s) Use new socials file: %s%s\r\n"
        "%sK%s) OLC autosave to disk: %s%s\r\n"
     "%sL%s) Main Menu           : \r\n%s%s\r\n"
     "%sM%s) Welcome Message     : \r\n%s%s\r\n"
     "%sN%s) Start Message       : \r\n%s%s\r\n"
    "%sQ%s) Exit To The Main Menu\r\n"
    "Enter your choice : ",
    grn, nrm, cyn, OLC_CONFIG(d)->operation.DFLT_PORT,
    grn, nrm, cyn, OLC_CONFIG(d)->operation.DFLT_IP ? OLC_CONFIG(d)->operation.DFLT_IP : "<None>",
    grn, nrm, cyn, OLC_CONFIG(d)->operation.DFLT_DIR ? OLC_CONFIG(d)->operation.DFLT_DIR : "<None>",
    grn, nrm, cyn, OLC_CONFIG(d)->operation.LOGNAME ? OLC_CONFIG(d)->operation.LOGNAME : "<None>",
    grn, nrm, cyn, OLC_CONFIG(d)->operation.max_playing,
    grn, nrm, cyn, OLC_CONFIG(d)->operation.max_filesize,
    grn, nrm, cyn, OLC_CONFIG(d)->operation.max_bad_pws,
    grn, nrm, cyn, YESNO(OLC_CONFIG(d)->operation.siteok_everyone),
    grn, nrm, cyn, YESNO(OLC_CONFIG(d)->operation.nameserver_is_slow),
    grn, nrm, cyn, YESNO(OLC_CONFIG(d)->operation.use_new_socials),
    grn, nrm, cyn, YESNO(OLC_CONFIG(d)->operation.auto_save_olc),
    grn, nrm, cyn, OLC_CONFIG(d)->operation.MENU ? OLC_CONFIG(d)->operation.MENU : "<None>",
    grn, nrm, cyn, OLC_CONFIG(d)->operation.WELC_MESSG ? OLC_CONFIG(d)->operation.WELC_MESSG : "<None>",
    grn, nrm, cyn, OLC_CONFIG(d)->operation.START_MESSG ? OLC_CONFIG(d)->operation.START_MESSG : "<None>",
    grn, nrm
    );

  OLC_MODE(d) = CEDIT_OPERATION_OPTIONS_MENU;
}


/*-------------------------------------------------------------------*/

void cedit_disp_autowiz_options(Descriptor *d)
{
  get_char_colours(d->character);
  clear_screen(d);

  d->Output( "\r\n\r\n"
    "%sA%s) Use the autowiz        : %s%s\r\n"
    "%sB%s) Minimum wizlist level  : %s%d\r\n"
    "%sQ%s) Exit To The Main Menu\r\n"
    "Enter your choice : ",
    grn, nrm, cyn, CHECK_VAR(OLC_CONFIG(d)->autowiz.use_autowiz),
    grn, nrm, cyn, OLC_CONFIG(d)->autowiz.min_wizlist_lev,
    grn, nrm
    );

  OLC_MODE(d) = CEDIT_AUTOWIZ_OPTIONS_MENU;
}

/*-------------------------------------------------------------------*/

/**************************************************************************
  The GARGANTAUN event handler
 **************************************************************************/

void cedit_parse(Descriptor *d, char *arg)
{
  char *oldtext = NULL;

  switch (OLC_MODE(d)) {
    case CEDIT_CONFIRM_SAVESTRING:
      switch (*arg) {
        case 'y':
        case 'Y':
          cedit_save_internally(d);
          new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(d->character)), TRUE,
                 "OLC: %s modifies the game configuration.", GET_NAME(d->character));
          cleanup_olc(d, CLEANUP_CONFIG);
          if (CONFIG_AUTO_SAVE) {
            cedit_save_to_disk();
            d->Output( "Game configuration saved to disk.\r\n");
          }
          else
            d->Output( "Game configuration saved to memory.\r\n");
          return;
        case 'n':
        case 'N':
          d->Output( "Game configuration not saved to memory.\r\n");
          cleanup_olc(d, CLEANUP_CONFIG);
          return;
        default :
          d->Output( "\r\nThat is an invalid choice!\r\n");
          d->Output( "Do you wish to save the configuration? (y/n) : ");
          return;
      }

/*-------------------------------------------------------------------*/

    case CEDIT_MAIN_MENU:
      switch (*arg) {
        case 'g':
        case 'G':
          cedit_disp_game_play_options(d);
          OLC_MODE(d) = CEDIT_GAME_OPTIONS_MENU;
          break;

        case 'c':
        case 'C':
          cedit_disp_crash_save_options(d);
          OLC_MODE(d) = CEDIT_CRASHSAVE_OPTIONS_MENU;
          break;

        case 'r':
        case 'R':
          cedit_disp_room_numbers(d);
          OLC_MODE(d) = CEDIT_ROOM_NUMBERS_MENU;
          break;

        case 'o':
        case 'O':
          cedit_disp_operation_options(d);
          OLC_MODE(d) = CEDIT_OPERATION_OPTIONS_MENU;
          break;

        case 'a':
        case 'A':
          cedit_disp_autowiz_options(d);
          OLC_MODE(d) = CEDIT_AUTOWIZ_OPTIONS_MENU;
          break;

        case 'q':
        case 'Q':
          d->Output( "Do you wish to save the configuration? (y/n) : ");
          OLC_MODE(d) = CEDIT_CONFIRM_SAVESTRING;
          break;

        default:
          d->Output( "That is an invalid choice!\r\n");
          cedit_disp_menu(d);
          break;
      }
      break;


/*-------------------------------------------------------------------*/

    case CEDIT_GAME_OPTIONS_MENU:
      switch (*arg) {
        case 'a':
        case 'A':
          TOGGLE_VAR(OLC_CONFIG(d)->play.pk_allowed);
          break;

        case 'b':
        case 'B':
          TOGGLE_VAR(OLC_CONFIG(d)->play.pt_allowed);
          break;

        case 'c':
        case 'C':
          d->Output( "Enter the minimum level a player must be to shout, gossip, etc : ");
          OLC_MODE(d) = CEDIT_LEVEL_CAN_SHOUT;
          return;

        case 'd':
        case 'D':
          d->Output( "Enter the amount it costs (in move points) to holler : ");
          OLC_MODE(d) = CEDIT_HOLLER_MOVE_COST;
          return;

        case 'e':
        case 'E':
          d->Output( "Enter the maximum number of people allowed in a tunnel : ");
          OLC_MODE(d) = CEDIT_TUNNEL_SIZE;
          return;

        case 'f':
        case 'F':
          d->Output( "Enter the maximum gain of experience per kill for players : ");
          OLC_MODE(d) = CEDIT_MAX_EXP_GAIN;
          return;

        case 'g':
        case 'G':
          d->Output( "Enter the maximum loss of experience per death for players : ");
          OLC_MODE(d) = CEDIT_MAX_EXP_LOSS;
          return;

        case 'h':
        case 'H':
          d->Output( "Enter the number of tics before NPC corpses decompose : ");
          OLC_MODE(d) = CEDIT_MAX_NPC_CORPSE_TIME;
          return;

        case 'i':
        case 'I':
          d->Output( "Enter the number of tics before PC corpses decompose : ");
          OLC_MODE(d) = CEDIT_MAX_PC_CORPSE_TIME;
          return;

        case 'j':
        case 'J':
          d->Output( "Enter the number of tics before PC's are sent to the void (idle) : ");
          OLC_MODE(d) = CEDIT_IDLE_VOID;
          return;

        case 'k':
        case 'K':
          d->Output( "Enter the number of tics before PC's are automatically rented and forced to quit : ");
          OLC_MODE(d) = CEDIT_IDLE_RENT_TIME;
          return;

        case 'l':
        case 'L':
          d->Output( "Enter the level a player must be to become immune to IDLE : ");
          OLC_MODE(d) = CEDIT_IDLE_MAX_LEVEL;
          return;

        case 'm':
        case 'M':
          TOGGLE_VAR(OLC_CONFIG(d)->play.dts_are_dumps);
          break;

        case 'n':
        case 'N':
          TOGGLE_VAR(OLC_CONFIG(d)->play.load_into_inventory);
          break;

        case 'o':
        case 'O':
          TOGGLE_VAR(OLC_CONFIG(d)->play.track_through_doors);
          break;

        case 'p':
        case 'P':
          TOGGLE_VAR(OLC_CONFIG(d)->play.immort_level_ok);
          break;

    case 'r':
        case 'R':
          TOGGLE_VAR(OLC_CONFIG(d)->play.double_exp);
          break;

        case '1':
          d->Output( "Enter the OK message : ");
          OLC_MODE(d) = CEDIT_OK;
          return;

        case '2':
          d->Output( "Enter the NOPERSON message : ");
          OLC_MODE(d) = CEDIT_NOPERSON;
          return;

        case '3':
          d->Output( "Enter the NOEFFECT message : ");
          OLC_MODE(d) = CEDIT_NOEFFECT;
          return;

        case 'q':
        case 'Q':
          cedit_disp_menu(d);
          return;

        default:
          d->Output( "\r\nThat is an invalid choice!\r\n");
          cedit_disp_game_play_options(d);
      }

      cedit_disp_game_play_options(d);
      return;

 /*-------------------------------------------------------------------*/

    case CEDIT_CRASHSAVE_OPTIONS_MENU:
      switch (*arg) {
        case 'a':
        case 'A':
          TOGGLE_VAR(OLC_CONFIG(d)->csd.free_rent);
          break;

        case 'b':
        case 'B':
          d->Output( "Enter the maximum number of items players can rent : ");
          OLC_MODE(d) = CEDIT_MAX_OBJ_SAVE;
          return;

        case 'c':
        case 'C':
          d->Output( "Enter the surcharge on top of item costs : ");
          OLC_MODE(d) = CEDIT_MIN_RENT_COST;
          return;

        case 'd':
        case 'D':
          TOGGLE_VAR(OLC_CONFIG(d)->csd.auto_save);
          break;

        case 'e':
        case 'E':
          d->Output( "Enter how often (in minutes) should the MUD save players : ");
          OLC_MODE(d) = CEDIT_AUTOSAVE_TIME;
          return;

        case 'f':
        case 'F':
          d->Output( "Enter the lifetime of crash and idlesave files (days) : ");
          OLC_MODE(d) = CEDIT_CRASH_FILE_TIMEOUT;
          return;

        case 'g':
        case 'G':
          d->Output( "Enter the lifetime of normal rent files (days) : ");
          OLC_MODE(d) = CEDIT_RENT_FILE_TIMEOUT;
          return;

        case 'q':
        case 'Q':
          cedit_disp_menu(d);
          return;

        default:
          d->Output( "\r\nThat is an invalid choice!\r\n");
        }

        cedit_disp_crash_save_options(d);
        return;

 /*-------------------------------------------------------------------*/

    case CEDIT_ROOM_NUMBERS_MENU:
      switch (*arg) {
        case 'a':
        case 'A':
          d->Output( "Enter the room's vnum where mortals should load into : ");
          OLC_MODE(d) = CEDIT_MORTAL_START_ROOM;
          return;

        case 'b':
        case 'B':
          d->Output( "Enter the room's vnum where immortals should load into : ");
          OLC_MODE(d) = CEDIT_IMMORT_START_ROOM;
          return;

        case 'c':
        case 'C':
        d->Output( "Enter the room's vnum where frozen people should load into : ");
        OLC_MODE(d) = CEDIT_FROZEN_START_ROOM;
        return;

      case '1':
        d->Output( "Enter the vnum for donation room #1 : ");
        OLC_MODE(d) = CEDIT_DONATION_ROOM_1;
        return;

      case '2':
        d->Output( "Enter the vnum for donation room #2 : ");
        OLC_MODE(d) = CEDIT_DONATION_ROOM_2;
        return;

      case '3':
        d->Output( "Enter the vnum for donation room #3 : ");
        OLC_MODE(d) = CEDIT_DONATION_ROOM_3;
        return;

      case '4':
        d->Output( "Enter the vnum for the gladiator death room : ");
        OLC_MODE(d) = CEDIT_GLA_DEATH_ROOM;
        return;

      case 'q':
      case 'Q':
        cedit_disp_menu(d);
        return;

      default:
        d->Output( "\r\nThat is an invalid choice!\r\n");
    }

    cedit_disp_room_numbers(d);
    return;

 /*-------------------------------------------------------------------*/

     case CEDIT_OPERATION_OPTIONS_MENU:
       switch (*arg) {
         case 'a':
         case 'A':
           d->Output( "Enter the default port number : ");
           OLC_MODE(d) = CEDIT_DFLT_PORT;
           return;

         case 'b':
         case 'B':
           d->Output( "Enter the default IP Address : ");
           OLC_MODE(d) = CEDIT_DFLT_IP;
           return;

         case 'c':
         case 'C':
           d->Output( "Enter the default directory : ");
           OLC_MODE(d) = CEDIT_DFLT_DIR;
           return;

         case 'd':
         case 'D':
           d->Output( "Enter the name of the logfile : ");
           OLC_MODE(d) = CEDIT_LOGNAME;
           return;

         case 'e':
         case 'E':
           d->Output( "Enter the maximum number of players : ");
           OLC_MODE(d) = CEDIT_MAX_PLAYING;
           return;

         case 'f':
         case 'F':
           d->Output( "Enter the maximum size of the logs : ");
           OLC_MODE(d) = CEDIT_MAX_FILESIZE;
           return;

         case 'g':
         case 'G':
           d->Output( "Enter the maximum number of password attempts : ");
           OLC_MODE(d) = CEDIT_MAX_BAD_PWS;
           return;

         case 'h':
         case 'H':
           TOGGLE_VAR(OLC_CONFIG(d)->operation.siteok_everyone);
           break;

         case 'i':
         case 'I':
           TOGGLE_VAR(OLC_CONFIG(d)->operation.nameserver_is_slow);
           break;

         case 'j':
         case 'J':
           TOGGLE_VAR(OLC_CONFIG(d)->operation.use_new_socials);
           d->character->Send("Please note that using the stock social file will disable AEDIT.\r\n");
           break;

         case 'k':
         case 'K':
           TOGGLE_VAR(OLC_CONFIG(d)->operation.auto_save_olc);
           break;

         case 'l':
         case 'L':
           OLC_MODE(d) = CEDIT_MENU;
           clear_screen(d);
           send_editor_help(d);
           d->Output( "Enter the new MENU :\r\n\r\n");

           if (OLC_CONFIG(d)->operation.MENU) {
             d->Output( "%s", OLC_CONFIG(d)->operation.MENU);
             oldtext = strdup(OLC_CONFIG(d)->operation.MENU);
           }

           string_write(d, &OLC_CONFIG(d)->operation.MENU, MAX_INPUT_LENGTH, 0, oldtext);
           return;

         case 'm':
         case 'M':
           OLC_MODE(d) = CEDIT_WELC_MESSG;
           clear_screen(d);
           send_editor_help(d);
           d->Output( "Enter the new welcome message :\r\n\r\n");

           if (OLC_CONFIG(d)->operation.WELC_MESSG) {
             d->Output( "%s", OLC_CONFIG(d)->operation.WELC_MESSG);
             oldtext = str_udup(OLC_CONFIG(d)->operation.WELC_MESSG);
           }

           string_write(d, &OLC_CONFIG(d)->operation.WELC_MESSG, MAX_INPUT_LENGTH, 0, oldtext);
           return;

         case 'n':
         case 'N':
           OLC_MODE(d) = CEDIT_START_MESSG;
           clear_screen(d);
           send_editor_help(d);
           d->Output( "Enter the new newbie start message :\r\n\r\n");

           if (OLC_CONFIG(d)->operation.START_MESSG) {
             d->Output( "%s", OLC_CONFIG(d)->operation.START_MESSG);
             oldtext = strdup(OLC_CONFIG(d)->operation.START_MESSG);
           }

           string_write(d, &OLC_CONFIG(d)->operation.START_MESSG, MAX_INPUT_LENGTH, 0, oldtext);
           return;

         case 'q':
         case 'Q':
           cedit_disp_menu(d);
           return;

         default:
           d->Output( "\r\nThat is an invalid choice!\r\n");
      }

   cedit_disp_operation_options(d);
   return;

 /*-------------------------------------------------------------------*/

    case CEDIT_AUTOWIZ_OPTIONS_MENU:
      switch (*arg) {
        case 'a':
        case 'A':
          TOGGLE_VAR(OLC_CONFIG(d)->autowiz.use_autowiz);
          break;

        case 'b':
        case 'B':
          d->Output( "Enter the minimum level for players to appear on the wizlist : ");
          OLC_MODE(d) = CEDIT_MIN_WIZLIST_LEV;
          return;

        case 'q':
        case 'Q':
          cedit_disp_menu(d);
          return;

        default:
          d->Output( "\r\nThat is an invalid choice!\r\n");
      }

      cedit_disp_autowiz_options(d);
      return;

 /*-------------------------------------------------------------------*/

    case CEDIT_LEVEL_CAN_SHOUT:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the minimum level a player must be to shout, gossip, etc : ");
      } else {
        OLC_CONFIG(d)->play.level_can_shout = atoi(arg);
        cedit_disp_game_play_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_HOLLER_MOVE_COST:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the amount it costs (in move points) to holler : ");
      } else {
        OLC_CONFIG(d)->play.holler_move_cost = atoi(arg);
        cedit_disp_game_play_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_TUNNEL_SIZE:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the maximum number of people allowed in a tunnel : ");
      } else {
        OLC_CONFIG(d)->play.tunnel_size = atoi(arg);
        cedit_disp_game_play_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_MAX_EXP_GAIN:
      if (*arg)
        OLC_CONFIG(d)->play.max_exp_gain = atoi(arg);

      cedit_disp_game_play_options(d);
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_MAX_EXP_LOSS:
      if (*arg)
        OLC_CONFIG(d)->play.max_exp_loss = atoi(arg);

      cedit_disp_game_play_options(d);
      break;


/*-------------------------------------------------------------------*/

    case CEDIT_MAX_NPC_CORPSE_TIME:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the number of tics before NPC corpses decompose : ");
      } else {
        OLC_CONFIG(d)->play.max_npc_corpse_time = atoi(arg);
        cedit_disp_game_play_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_MAX_PC_CORPSE_TIME:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the number of tics before PC corpses decompose : ");
        } else {
          OLC_CONFIG(d)->play.max_pc_corpse_time = atoi(arg);
          cedit_disp_game_play_options(d);
        }
        break;

/*-------------------------------------------------------------------*/

    case CEDIT_IDLE_VOID:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the number of tics before PC's are sent to the void (idle) : ");
      } else {
        OLC_CONFIG(d)->play.idle_void = atoi(arg);
        cedit_disp_game_play_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_IDLE_RENT_TIME:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the number of tics before PC's are automatically rented and forced to quit : ");
      } else {
        OLC_CONFIG(d)->play.idle_rent_time = atoi(arg);
        cedit_disp_game_play_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_IDLE_MAX_LEVEL:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the level a player must be to become immune to IDLE : ");
      } else {
        OLC_CONFIG(d)->play.idle_max_level = atoi(arg);
        cedit_disp_game_play_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_OK:
      if (!genolc_checkstring(d, arg))
        break;

      if (OLC_CONFIG(d)->play.OK)
        free(OLC_CONFIG(d)->play.OK);

      OLC_CONFIG(d)->play.OK = str_udup(arg);
      strcat(OLC_CONFIG(d)->play.OK, "\r\n");

      cedit_disp_game_play_options(d);
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_NOPERSON:
      if (!genolc_checkstring(d, arg))
        break;

      if (OLC_CONFIG(d)->play.NOPERSON)
        free(OLC_CONFIG(d)->play.NOPERSON);

      OLC_CONFIG(d)->play.NOPERSON = str_udup(arg);
      strcat(OLC_CONFIG(d)->play.NOPERSON, "\r\n");

      cedit_disp_game_play_options(d);
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_NOEFFECT:
      if (!genolc_checkstring(d, arg))
        break;

      if (OLC_CONFIG(d)->play.NOEFFECT)
        free(OLC_CONFIG(d)->play.NOEFFECT);

      OLC_CONFIG(d)->play.NOEFFECT = str_udup(arg);
      strcat(OLC_CONFIG(d)->play.NOEFFECT, "\r\n");

      cedit_disp_game_play_options(d);
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_MAX_OBJ_SAVE:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the maximum objects a player can save : ");
        } else {
          OLC_CONFIG(d)->csd.max_obj_save = atoi(arg);
          cedit_disp_crash_save_options(d);
        }
        break;

/*-------------------------------------------------------------------*/

    case CEDIT_MIN_RENT_COST:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the minimum amount it costs to rent : ");
      } else {
        OLC_CONFIG(d)->csd.min_rent_cost = atoi(arg);
        cedit_disp_crash_save_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_AUTOSAVE_TIME:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the interval for player's being autosaved : ");
      } else {
        OLC_CONFIG(d)->csd.autosave_time = atoi(arg);
        cedit_disp_crash_save_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_CRASH_FILE_TIMEOUT:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the lifetime of crash and idlesave files (days) : ");
      } else {
        OLC_CONFIG(d)->csd.crash_file_timeout = atoi(arg);
        cedit_disp_crash_save_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_RENT_FILE_TIMEOUT:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the lifetime of rent files (days) : ");
      } else {
        OLC_CONFIG(d)->csd.rent_file_timeout = atoi(arg);
        cedit_disp_crash_save_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_MORTAL_START_ROOM:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the room's vnum where mortals should load into : ");
      } else if (real_room(atoi(arg)) == NULL) {
        d->Output(
          "That room doesn't exist!\r\n"
          "Enter the room's vnum where mortals should load into : ");
      } else {
        OLC_CONFIG(d)->room_nums.mortal_start_room = atoi(arg);
        cedit_disp_room_numbers(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_IMMORT_START_ROOM:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the room's vnum where immortals should load into : ");
      } else if (real_room(atoi(arg)) == NULL) {
        d->Output(
          "That room doesn't exist!\r\n"
          "Enter the room's vnum where immortals should load into : ");
      } else {
        OLC_CONFIG(d)->room_nums.immort_start_room = atoi(arg);
        cedit_disp_room_numbers(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_FROZEN_START_ROOM:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the room's vnum where frozen people should load into : ");
      } else if (real_room(atoi(arg)) == NULL) {
        d->Output(
          "That room doesn't exist!\r\n"
          "Enter the room's vnum where frozen people should load into : ");
      } else {
        OLC_CONFIG(d)->room_nums.frozen_start_room = atoi(arg);
        cedit_disp_room_numbers(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_DONATION_ROOM_1:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the vnum for donation room #1 : ");
      } else if (real_room(atoi(arg)) == NULL) {
        d->Output(
          "That room doesn't exist!\r\n"
          "Enter the vnum for donation room #1 : ");
      } else {
        OLC_CONFIG(d)->room_nums.donation_room_1 = atoi(arg);
        cedit_disp_room_numbers(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_DONATION_ROOM_2:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the vnum for donation room #2 : ");
      } else if (real_room(atoi(arg)) == NULL) {
        d->Output(
          "That room doesn't exist!\r\n"
          "Enter the vnum for donation room #2 : ");
      } else {
        OLC_CONFIG(d)->room_nums.donation_room_2 = atoi(arg);
        cedit_disp_room_numbers(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_DONATION_ROOM_3:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the vnum for donation room #3 : ");
      } else if (real_room(atoi(arg)) == NULL) {
        d->Output(
          "That room doesn't exist!\r\n"
          "Enter the vnum for donation room #3 : ");
      } else {
        OLC_CONFIG(d)->room_nums.donation_room_3 = atoi(arg);
        cedit_disp_room_numbers(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_GLA_DEATH_ROOM:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the vnum for the gladiator death room : ");
      } else if (real_room(atoi(arg)) == NULL) {
        d->Output(
          "That room doesn't exist!\r\n"
          "Enter the vnum for the gladiator death room : ");
      } else {
        OLC_CONFIG(d)->room_nums.gladiator_death_room = atoi(arg);
        cedit_disp_room_numbers(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_DFLT_PORT:
      OLC_CONFIG(d)->operation.DFLT_PORT = atoi(arg);
      cedit_disp_operation_options(d);
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_DFLT_IP:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the default ip address : ");
      } else {
        OLC_CONFIG(d)->operation.DFLT_IP = str_udup(arg);
        cedit_disp_operation_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_DFLT_DIR:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the default directory : ");
      } else {
        OLC_CONFIG(d)->operation.DFLT_DIR = str_udup(arg);
        cedit_disp_operation_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_LOGNAME:
      if (!*arg) {
        d->Output(
          "That is an invalid choice!\r\n"
          "Enter the name of the logfile : ");
      } else {
        OLC_CONFIG(d)->operation.LOGNAME = str_udup(arg);
        cedit_disp_operation_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_MAX_PLAYING:
      OLC_CONFIG(d)->operation.max_playing = atoi(arg);
      cedit_disp_operation_options(d);
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_MAX_FILESIZE:
      OLC_CONFIG(d)->operation.max_filesize = atoi(arg);
      cedit_disp_operation_options(d);
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_MAX_BAD_PWS:
      OLC_CONFIG(d)->operation.max_bad_pws = atoi(arg);
      cedit_disp_operation_options(d);
      break;

/*-------------------------------------------------------------------*/

    case CEDIT_MIN_WIZLIST_LEV:
      if (atoi(arg) > LVL_IMPL) {
        d->Output(
          "The minimum wizlist level can't be greater than %d.\r\n"
          "Enter the minimum level for players to appear on the wizlist : ", LVL_IMPL);
      } else {
        OLC_CONFIG(d)->autowiz.min_wizlist_lev = atoi(arg);
        cedit_disp_autowiz_options(d);
      }
      break;

/*-------------------------------------------------------------------*/

    default:
      /*
       * We should never get here, but just in case...
       */
      cleanup_olc(d, CLEANUP_CONFIG);
      new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: cedit_parse(): Reached default case!");
      d->Output( "Oops...\r\n");
      break;
  }
}

/*
 * End of parse_cedit()
 */
void reassign_rooms(void)
{
  void assign_rooms(void);
  int i;

  /* remove old funcs */
  for (i = 0; i < top_of_world; i++)
  if (world_vnum[i])
    world_vnum[i]->func = NULL;

  /* reassign spec_procs */
  assign_rooms();
}

void cedit_string_cleanup(Descriptor *d, int terminator)
{
  switch (OLC_MODE(d)) {
  case CEDIT_MENU:
  case CEDIT_WELC_MESSG:
  case CEDIT_START_MESSG:
    cedit_disp_operation_options(d);
    break;
  }
}
