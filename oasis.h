/************************************************************************
 * OasisOLC - General / oasis.h					v2.0	*
 * Original author: Levork						*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/
#include <memory>

#ifndef OASIS_H
#define OASIS_H

#define _OASISOLC	0x206   /* 2.0.6 */
/*
 * Used to determine what version of OasisOLC is installed.
 *
 * Ex: #if _OASISOLC >= OASIS_VERSION(2,0,0)
 */
#define OASIS_VERSION(x,y,z)	(((x) << 8 | (y) << 4 | (z))

#define AEDIT_PERMISSION  9999  /* arbitrary number higher than max zone vnum*/
#define HEDIT_PERMISSION  8888

/*
 * Set this to 1 to enable MobProg support.  MobProgs are available on
 * the CircleMUD FTP site in the "contrib/scripting/" directory.
 *
 * -- THIS WILL NOT WORK WITHOUT MobProgs INSTALLED. --
 * -- OasisOLC DOES NOT COME WITH THEM. -- Loud enough for you?
 *
 * It might work with DG Scripts (successor to MobProgs) but I haven't
 * tried, nor have I heard of anyone trying.
 */
#define CONFIG_OASIS_MPROG	0

/*
 * Macros, defines, structs and globals for the OLC suite.  You will need
 * to adjust these numbers if you ever add more.
 */
#define NUM_ZONE_FLAGS 		16
#define NUM_ROOM_FLAGS 		49
#define NUM_ROOM_SECTORS	26


#define NUM_MOB_FLAGS		48
#define NUM_AFF_FLAGS		88
#define NUM_ATTACK_TYPES	17

#define NUM_ITEM_TYPES		92
#define NUM_ITEM_FLAGS		80
#define NUM_ITEM_WEARS 		29  //44
#define NUM_APPLIES		32
#define NUM_LIQ_TYPES 		16
#define NUM_POSITIONS  		44
#define NUM_SPELLS		300

#define NUM_GENDERS		3
#define NUM_MOB_RACES		3
#define NUM_SHOP_FLAGS 		2
#define NUM_TRADERS 		8

#define MAX_PEOPLE_IN_CHAIR 	10	/* The maximum number of people you want \
                       to sit in a chair at the same time. */

#if CONFIG_OASIS_MPROG
/*
 * Define this to how many MobProg scripts you have.
 */
#define NUM_PROGS		12
#endif

/* -------------------------------------------------------------------------- */

/*
 * Limit information.
 */



const int MAX_ROOM_NAME	= 75;
const int MAX_MOB_NAME	= 50;
const int MAX_OBJ_NAME	= 50;
const int MAX_ROOM_DESC	= 2048;
const int MAX_EXIT_DESC	= 1024;
const int MAX_EXTRA_DESC  = 1024;
const int MAX_MOB_DESC	= 1024;
const int MAX_OBJ_DESC	= 1024;
const int MAX_DUPLICATES  = 2000;  /* when loading in zedit */

/* arbitrary limits - roll your own */
/* max weapon is 50d50 .. avg. 625 dam... */
const int MAX_WEAPON_SDICE     =    50;
const int MAX_WEAPON_NDICE     =    50;

const int MAX_OBJ_WEIGHT    =  1000000;
const int MAX_OBJ_COST      =  2000000;
const int MAX_OBJ_RENT      =  2000000;
const int MAX_CONTAINER_SIZE =   10000;

const int MAX_MOB_GOLD       = 1000000;
const int MAX_MOB_EXP        = 1500000;
/* this is one mud year.. */
const int MAX_OBJ_TIMER     =  1071000;

/* this defines how much memory is alloacted for 'bit strings' when
 * saving in OLC. Remember to change it if you go for longer bitvectors.
 */
const int BIT_STRING_LENGTH =128;
/*
 * The data types for miscellaneous functions.
 */
#define OASIS_WLD	0
#define OASIS_MOB	1
#define OASIS_OBJ	2
#define OASIS_ZON	3
#define OASIS_EXI	4
#define OASIS_CFG	5

/* -------------------------------------------------------------------------- */

extern int top_of_socialt;

/*
 * Utilities exported from olc.c.
 *   -- Umm, shouldn't this say 'from oasis.c' now???  * Mythran
 */
void cleanup_olc(Descriptor *d, sbyte cleanup_type);
void get_char_colours(Character *ch);
void split_argument(char *argument, char *tag);
int can_edit_zone(Character *ch, zone_rnum rnum);
/*
 * OLC structures.
 */
/* -------------------------------------------------------------------------- */

/*
 * The following defines used to be in config.c.
 */
#define NO	0
#define YES	1

struct oasis_olc_data {
  int mode;                      /* how to parse input       */
  zone_rnum zone_num;            /* current zone             */
  room_vnum number;              /* vnum of subject          */
  int value;                     /* mostly 'has changed' flag*/
  char *storage;                 /* used for 'tedit'         */
  Character *mob;         /* used for 'medit'         */
  Room *room;        /* used for 'redit'         */
  struct obj_data *obj;          /* used for 'oedit'         */
  Zone *zone;        /* used for 'zedit'         */
  struct shop_data *shop;        /* used for 'sedit'         */
  struct config_data *config;    /* used for 'cedit'         */
  struct extra_descr_data *desc; /* used in '[r|o|m]edit'    */
  struct extra_descr_data *above; /* used in '[r|o|m]edit'    */
  struct extra_descr_data *behind; /* used in '[r|o|m]edit'    */
  struct extra_descr_data *under; /* used in '[r|o|m]edit'    */
  struct q_descr_data *q_description;
  struct vehicle_attachment_data *attachment;
  struct help_index_element *help;
  unique_ptr<questcard> quest_card;
#if CONFIG_OASIS_MPROG           /*                          */
  int total_mprogs;              /*                          */
  struct mob_prog_data *mprog;   /*                          */
  struct mob_prog_data *mprogl;  /*                          */
#endif
  struct social_messg *action;   /* Aedit uses this one      */
  struct trig_data *trig;
  struct vehicle_data *vehicle;
  int script_mode;
  int trigger_position;
  int item_type;
  vector<int> *script; /* for assigning triggers in [r|o|m]edit*/

  struct assembly_data *OlcAssembly; /* used for 'assedit'         */
};

/*
 * Exported globals.
 */
extern const char *nrm, *grn, *cyn, *yel;

/*
 * Descriptor access macros.
 */
#define OLC(d)		((d)->olc)
#define OLC_MODE(d) 	(OLC(d)->mode)		/* Parse input mode.	*/
#define OLC_NUM(d) 	(OLC(d)->number)	/* Room/Obj VNUM.	*/
#define OLC_VAL(d) 	(OLC(d)->value)		/* Scratch variable.	*/
#define OLC_ZNUM(d) 	(OLC(d)->zone_num)	/* Real zone number.	*/
#define OLC_QC(d)       (OLC(d)->quest_card)
#define OLC_STORAGE(d)  (OLC(d)->storage)	/* char pointer.	*/
#define OLC_ROOM(d) 	(OLC(d)->room)		/* Room structure.	*/
#define OLC_OBJ(d) 	(OLC(d)->obj)		/* Object structure.	*/
#define OLC_ZONE(d)     (OLC(d)->zone)          /* Zone structure.	*/
#define OLC_MOB(d)	(OLC(d)->mob)		/* Mob structure.	*/
#define OLC_SHOP(d) 	(OLC(d)->shop)		/* Shop structure.	*/
#define OLC_DESC(d) 	(OLC(d)->desc)		/* Extra description.	*/
#define OLC_LUDESC(d) 	(OLC(d)->under)		/* Extra description.	*/
#define OLC_LADESC(d) 	(OLC(d)->above)		/* Extra description.	*/
#define OLC_LBDESC(d) 	(OLC(d)->behind)	/* Extra description.	*/
#define OLC_HELP(d)	(OLC(d)->help)		/* help structure */
#define OLC_QDESC(d)    (OLC(d)->q_description)
#define OLC_CONFIG(d)	(OLC(d)->config)	/* Config structure.	*/
#define OLC_TRIG(d)     (OLC(d)->trig)          /* Trigger structure.   */
#define OLC_VEHICLE(d)  (OLC(d)->vehicle)
#define OLC_ASSEDIT(d)  (OLC(d)->OlcAssembly)   /* assembly olc        */
#define OLC_ATTACHMENT(d)   (OLC(d)->attachment)    /* vehicle attachments */

#if CONFIG_OASIS_MPROG
#define OLC_MPROG(d)	(OLC(d)->mprog)		/* Temporary MobProg.	*/
#define OLC_MPROGL(d)	(OLC(d)->mprogl)	/* MobProg list.	*/
#define OLC_MTOTAL(d)	(OLC(d)->total_mprogs)	/* Total mprog number.	*/
#endif

#define OLC_ACTION(d)   (OLC(d)->action)        /* Action structure     */

/*
 * Other macros.
 */
#define OLC_EXIT(d)		(OLC_ROOM(d)->dir_option[OLC_VAL(d)])

/*
 * Cleanup types.
 */
#define CLEANUP_ALL		1	/* Free the whole lot.			*/
#define CLEANUP_STRUCTS 	2	/* Don't free strings.			*/
#define CLEANUP_CONFIG          3       /* Used just to send proper message. 	*/



/* Submodes of AEDIT connectedness     */
#define AEDIT_CONFIRM_SAVESTRING       0
#define AEDIT_CONFIRM_EDIT             1
#define AEDIT_CONFIRM_ADD              2
#define AEDIT_MAIN_MENU                3
#define AEDIT_ACTION_NAME              4
#define AEDIT_SORT_AS                  5
#define AEDIT_MIN_CHAR_POS             6
#define AEDIT_MIN_VICT_POS             7
#define AEDIT_HIDDEN_FLAG              8
#define AEDIT_MIN_CHAR_LEVEL           9
#define AEDIT_NOVICT_CHAR              10
#define AEDIT_NOVICT_OTHERS            11
#define AEDIT_VICT_CHAR_FOUND          12
#define AEDIT_VICT_OTHERS_FOUND        13
#define AEDIT_VICT_VICT_FOUND          14
#define AEDIT_VICT_NOT_FOUND           15
#define AEDIT_SELF_CHAR                16
#define AEDIT_SELF_OTHERS              17
#define AEDIT_VICT_CHAR_BODY_FOUND     18
#define AEDIT_VICT_OTHERS_BODY_FOUND   19
#define AEDIT_VICT_VICT_BODY_FOUND     20
#define AEDIT_OBJ_CHAR_FOUND           21
#define AEDIT_OBJ_OTHERS_FOUND         22
/*
 * Submodes of OEDIT connectedness.
 */
#define OEDIT_MAIN_MENU              	1
#define OEDIT_EDIT_NAMELIST          	2
#define OEDIT_SHORTDESC              	3
#define OEDIT_LONGDESC               	4
#define OEDIT_ACTDESC                	5
#define OEDIT_TYPE                   	6
#define OEDIT_EXTRAS                 	7
#define OEDIT_WEAR                  	8
#define OEDIT_WEIGHT                	9
#define OEDIT_COST                  	10
#define OEDIT_COSTPERDAY            	11
#define OEDIT_TIMER                 	12
#define OEDIT_VALUE_1               	13
#define OEDIT_VALUE_2               	14
#define OEDIT_VALUE_3               	15
#define OEDIT_VALUE_4               	16
#define OEDIT_VALUE_5               	17
#define OEDIT_VALUE_6               	18
#define OEDIT_VALUE_7               	19
#define OEDIT_VALUE_8               	20
#define OEDIT_VALUE_9               	21
#define OEDIT_VALUE_10               	22
#define OEDIT_APPLY                 	23
#define OEDIT_APPLYMOD              	24
#define OEDIT_CONFIRM_SAVEDB        	25
#define OEDIT_CONFIRM_SAVESTRING    	26
#define OEDIT_PROMPT_APPLY          	27
#define OEDIT_EXTRADESC_KEY         	28
#define OEDIT_EXTRADESC_KEYWORD       	29
#define OEDIT_EXTRADESC_DESCRIPTION 	30
#define OEDIT_EXTRADESC_MENU        	31
#define OEDIT_LEVEL                 	32
#define OEDIT_PERM			            33
#define OEDIT_INNATE                    34
#define OEDIT_SMELL_DESCRIPTION         35
#define OEDIT_TASTE_DESCRIPTION		    36
#define OEDIT_FEEL_DESCRIPTION		    37
#define OEDIT_ATTACHMENT_MENU           38
#define OEDIT_ATTACHMENT_VALUE          39
#define OEDIT_ATTACHMENT_MAX_VALUE      40
#define OEDIT_ATTACHMENT_TYPE           41
#define OEDIT_CRAFTING_COLOUR		    42
#define OEDIT_CRAFTING_QUALITY		    43
#define OEDIT_CRAFTING_MATERIAL		    44
#define OEDIT_CRAFTING_DYECOUNT		    45
#define OEDIT_CRAFTING_ORIGIN		    46
#define OEDIT_CRAFTING_STAGE		    47
#define OEDIT_CRAFTING_REPAIRS		    48
#define OEDIT_CRAFTING_MAX_QUALITY	    49

/*
 * Submodes of REDIT connectedness.
 */
#define REDIT_MAIN_MENU 		        1
#define REDIT_NAME 			            2
#define REDIT_DESC 			            3
#define REDIT_FLAGS 			        4
#define REDIT_SECTOR 			        5
#define REDIT_EXIT_MENU 		        6
#define REDIT_CONFIRM_SAVEDB 		    7
#define REDIT_CONFIRM_SAVESTRING 	    8
#define REDIT_EXIT_NUMBER 		        9
#define REDIT_EXIT_DESCRIPTION 		    10
#define REDIT_EXIT_KEYWORD 		        11
#define REDIT_EXIT_KEY 			        12
#define REDIT_EXIT_DOORFLAGS 		    13
#define REDIT_EXTRADESC_MENU 		    14
#define REDIT_EXTRADESC_KEY 		    15
#define REDIT_EXTRADESC_KEYWORD         16
#define REDIT_EXTRADESC_DESCRIPTION 	17
#define REDIT_SMELL			            18
#define REDIT_LISTEN			        19
#define REDIT_LOOK_ABOVE_MENU           20
#define REDIT_LOOK_ABOVE_KEY            21
#define REDIT_LOOK_ABOVE_KEYWORD        22
#define REDIT_LOOK_ABOVE_DESCRIPTION    23
#define REDIT_LOOK_BEHIND_MENU          24
#define REDIT_LOOK_BEHIND_KEY           25
#define REDIT_LOOK_BEHIND_KEYWORD       26
#define REDIT_LOOK_BEHIND_DESCRIPTION   27
#define REDIT_LOOK_UNDER_MENU           28
#define REDIT_LOOK_UNDER_KEY            29
#define REDIT_LOOK_UNDER_KEYWORD        30
#define REDIT_LOOK_UNDER_DESCRIPTION    31
#define REDIT_DELETE			        32
#define REDIT_MINE_MENU			        33
#define REDIT_MINE_NUMBER			    34
#define REDIT_MINE_DIFFICULTY			35
#define REDIT_MINE_TOOL			        36
#define REDIT_N_DESCRIPTION             37
#define REDIT_QUEST_MENU                38
#define REDIT_QUEST_TYPE                39
#define REDIT_QUEST_FLAG                40
#define REDIT_QUEST_DESCRIPTION         41

/*
 * Submodes of ZEDIT connectedness.
 */
#define ZEDIT_MAIN_MENU              	0
#define ZEDIT_DELETE_ENTRY		1
#define ZEDIT_NEW_ENTRY			2
#define ZEDIT_CHANGE_ENTRY		3
#define ZEDIT_COMMAND_TYPE		4
#define ZEDIT_IF_FLAG			5
#define ZEDIT_ARG1			6
#define ZEDIT_ARG2			7
#define ZEDIT_ARG3			8
#define ZEDIT_ARG4                      9
#define ZEDIT_ZONE_NAME			10
#define ZEDIT_ZONE_LIFE			11
#define ZEDIT_ZONE_BOT			12
#define ZEDIT_ZONE_TOP			13
#define ZEDIT_ZONE_RESET		14
#define ZEDIT_CONFIRM_SAVESTRING	15
#define ZEDIT_ZONE_BUILDERS		16
#define ZEDIT_PROB                      17
#define ZEDIT_PROB2                     18
#define ZEDIT_SARG1                     20
#define ZEDIT_SARG2                     21
#define ZEDIT_ZONE_FLAGS		22
#define ZEDIT_ZONE_BOTTOM		23
#define ZEDIT_DIMENSION     24

/*
 * Submodes of MEDIT connectedness.
 */
#define MEDIT_MAIN_MENU              	0
#define MEDIT_ALIAS			1
#define MEDIT_S_DESC			2
#define MEDIT_L_DESC			3
#define MEDIT_D_DESC			4
#define MEDIT_NPC_FLAGS			5
#define MEDIT_AFF_FLAGS			6
#define MEDIT_CONFIRM_SAVESTRING	7
#define MEDIT_ARRIVE                    8
#define MEDIT_LEAVE                     9
/*
 * Numerical responses.
 */
#define MEDIT_NUMERICAL_RESPONSE	10
#define MEDIT_SEX			11
#define MEDIT_HITROLL			12
#define MEDIT_DAMROLL			13
#define MEDIT_NDD			14
#define MEDIT_SDD			15
#define MEDIT_NUM_HP_DICE		16
#define MEDIT_SIZE_HP_DICE		17
#define MEDIT_ADD_HP			18
#define MEDIT_AC			19
#define MEDIT_EXP			20
#define MEDIT_GOLD			21
#define MEDIT_POS			22
#define MEDIT_DEFAULT_POS		23
#define MEDIT_ATTACK			24
#define MEDIT_LEVEL			25
#define MEDIT_ALIGNMENT			26
#if CONFIG_OASIS_MPROG
#define MEDIT_MPROG                     27
#define MEDIT_CHANGE_MPROG              28
#define MEDIT_MPROG_COMLIST             29
#define MEDIT_MPROG_ARGS                30
#define MEDIT_MPROG_TYPE                31
#define MEDIT_PURGE_MPROG               32
#endif
#define MEDIT_RACE			33
#define MEDIT_CLASS			34
#define MEDIT_TIER			35
#define MEDIT_SUBSKILL			36
#define MEDIT_SEGMENTS              	37
#define MEDIT_DELETE_SEGMENT        	38
#define MEDIT_CHA			39
#define MEDIT_SKIN			40
#define MEDIT_OWNER			41
#define MEDIT_TRAINING			42
#define MEDIT_DELETE_TRAINING		43

/*
 * Submodes of SEDIT connectedness.
 */
#define SEDIT_MAIN_MENU              	0
#define SEDIT_CONFIRM_SAVESTRING	1
#define SEDIT_NOITEM1			2
#define SEDIT_NOITEM2			3
#define SEDIT_NOCASH1			4
#define SEDIT_NOCASH2			5
#define SEDIT_NOBUY			6
#define SEDIT_BUY			7
#define SEDIT_SELL			8
#define SEDIT_PRODUCTS_MENU		11
#define SEDIT_ROOMS_MENU		12
#define SEDIT_NAMELIST_MENU		13
#define SEDIT_NAMELIST			14
/*
 * Numerical responses.
 */
#define SEDIT_NUMERICAL_RESPONSE	20
#define SEDIT_OPEN1			21
#define SEDIT_OPEN2			22
#define SEDIT_CLOSE1			23
#define SEDIT_CLOSE2			24
#define SEDIT_KEEPER			25
#define SEDIT_BUY_PROFIT		26
#define SEDIT_SELL_PROFIT		27
#define SEDIT_TYPE_MENU			29
#define SEDIT_DELETE_TYPE		30
#define SEDIT_DELETE_PRODUCT		31
#define SEDIT_NEW_PRODUCT		32
#define SEDIT_DELETE_ROOM		33
#define SEDIT_NEW_ROOM			34
#define SEDIT_SHOP_FLAGS		35
#define SEDIT_NOTRADE			36

/*
 * Submodes of CEDIT connectedness.
 */
#define CEDIT_MAIN_MENU			0
#define CEDIT_CONFIRM_SAVESTRING	1
#define CEDIT_GAME_OPTIONS_MENU		2
#define CEDIT_CRASHSAVE_OPTIONS_MENU	3
#define CEDIT_OPERATION_OPTIONS_MENU	4
#define CEDIT_DISP_EXPERIENCE_MENU	5
#define CEDIT_ROOM_NUMBERS_MENU		6
#define CEDIT_AUTOWIZ_OPTIONS_MENU	7
#define CEDIT_OK			8
#define CEDIT_NOPERSON			9
#define CEDIT_NOEFFECT			10
#define CEDIT_DFLT_IP			11
#define CEDIT_DFLT_DIR			12
#define CEDIT_LOGNAME			13
#define CEDIT_MENU			14
#define CEDIT_WELC_MESSG		15
#define CEDIT_START_MESSG		16

/*
 * Numerical responses.
 */
#define CEDIT_NUMERICAL_RESPONSE	20
#define CEDIT_LEVEL_CAN_SHOUT		21
#define CEDIT_HOLLER_MOVE_COST		22
#define CEDIT_TUNNEL_SIZE		23
#define CEDIT_MAX_EXP_GAIN		24
#define CEDIT_MAX_EXP_LOSS		25
#define CEDIT_MAX_NPC_CORPSE_TIME	26
#define CEDIT_MAX_PC_CORPSE_TIME	27
#define CEDIT_IDLE_VOID			28
#define CEDIT_IDLE_RENT_TIME		29
#define CEDIT_IDLE_MAX_LEVEL		30
#define CEDIT_DTS_ARE_DUMPS		31
#define CEDIT_LOAD_INTO_INVENTORY	32
#define CEDIT_TRACK_THROUGH_DOORS	33
#define CEDIT_IMMORT_LEVEL_OK		34
#define CEDIT_MAX_OBJ_SAVE		35
#define CEDIT_MIN_RENT_COST		36
#define CEDIT_AUTOSAVE_TIME		37
#define CEDIT_CRASH_FILE_TIMEOUT	38
#define CEDIT_RENT_FILE_TIMEOUT		39
#define CEDIT_MORTAL_START_ROOM		40
#define CEDIT_IMMORT_START_ROOM		41
#define CEDIT_FROZEN_START_ROOM		42
#define CEDIT_DONATION_ROOM_1		43
#define CEDIT_DONATION_ROOM_2		44
#define CEDIT_DONATION_ROOM_3		45
#define CEDIT_DFLT_PORT			46
#define CEDIT_MAX_PLAYING		47
#define CEDIT_MAX_FILESIZE		48
#define CEDIT_MAX_BAD_PWS		49
#define CEDIT_SITEOK_EVERYONE		50
#define CEDIT_NAMESERVER_IS_SLOW	51
#define CEDIT_USE_AUTOWIZ		52
#define CEDIT_MIN_WIZLIST_LEV		53
#define CEDIT_GLA_DEATH_ROOM		54

/*
 * Submodes of QEDIT connectedness.
 */
#define QEDIT_MAIN_MENU                0
#define QEDIT_NAME                     1
#define QEDIT_QUESTFLAG_MENU           2
#define QEDIT_QUESTFLAG_NAME           3
#define QEDIT_QUESTFLAG_VALUE          4
#define QEDIT_QUESTFLAG_RESETS         5
#define QEDIT_DESCRIPTION              6
#define QEDIT_FUNCTION_TRIGGER         7
#define QEDIT_ORDER                    8
#define QEDIT_COMMAND_MENU             9
#define QEDIT_COMMAND                  10
#define QEDIT_COMMAND_FUNCTION_TRIGGER 11
#define QEDIT_DIMENSION_MENU           12
#define QEDIT_DIMENSION_VALUE          13
#define QEDIT_DIFFICULTY_MENU          14
#define QEDIT_DIFFICULTY_VALUE         15
#define QEDIT_CONFIRM_SAVESTRING       16

/* -------------------------------------------------------------------------- */
#define MAX_HELP_KEYWORDS	75
#define MAX_HELP_ENTRY		10000

#define HEDIT_MAIN_MENU			0
#define HEDIT_ENTRY			1
#define HEDIT_MIN_LEVEL			2
#define HEDIT_KEYWORDS			3
#define HEDIT_CONFIRM_SAVESTRING 	4
#define HEDIT_CONFIRM_EDIT   5
#define HEDIT_CONFIRM_ADD    6

#define VEDIT_MAIN_MENU      0
#define VEDIT_VEHICLE    1
#define VEDIT_CONTROLS   2
#define VEDIT_HATCH	 3
#define VEDIT_WINDOW	 4

#define ASSEDIT_DO_NOT_USE              0
#define ASSEDIT_MAIN_MENU               1
#define ASSEDIT_ADD_COMPONENT           2
#define ASSEDIT_EDIT_COMPONENT          3
#define ASSEDIT_DELETE_COMPONENT        4
#define ASSEDIT_EDIT_EXTRACT            5
#define ASSEDIT_EDIT_INROOM             6
#define ASSEDIT_EDIT_TYPES              7
#define ASSEDIT_EDIT_TRIGGER            8

//#ifndef __GENOLC_C__

/*
 * Prototypes to keep.
 */
#ifndef ACMD
#define ACMD(name)  \
   void name(Character *ch, char *argument, int cmd, int subcmd)
#endif
void clear_screen(Descriptor *);
int can_edit_zone(Character *ch, zone_rnum rnum);
ACMD(do_oasis);
ACMD(do_oasis_list);
ACMD(do_oasis_links);

/*
 * Prototypes, to be moved later.
 */
 ACMD(do_oasis_list);
 ACMD(do_oasis_qedit);
 ACMD(do_oasis_hedit);
 void hedit_disp_extradesc_menu(Descriptor *d);
void hedit_disp_exit_menu(Descriptor *d);
void hedit_disp_exit_flag_menu(Descriptor *d);
void hedit_disp_flag_menu(Descriptor *d);
void hedit_disp_sector_menu(Descriptor *d);
void hedit_disp_menu(Descriptor *d);
void hedit_parse(Descriptor *d, char *arg);
void hedit_setup_new(Descriptor *d);
void hedit_setup_existing(Descriptor *d, int rnum);
void hedit_save_to_disk(Descriptor *d);
int  hedit_find_entry(char *keyword);
void hedit_save_internally(Descriptor *d);

void medit_free_mobile(Character *mob);
void medit_setup_new(Descriptor *d);
void medit_setup_existing(Descriptor *d, int rmob_num);
void init_mobile(Character *mob);
void medit_save_internally(Descriptor *d);
void medit_save_to_disk(zone_vnum zone_num);
void medit_disp_positions(Descriptor *d);
void medit_disp_mprog(Descriptor *d);
void medit_change_mprog(Descriptor *d);
void medit_disp_mprog_types(Descriptor *d);
void medit_disp_sex(Descriptor *d);
void medit_disp_attack_types(Descriptor *d);
void medit_disp_mob_flags(Descriptor *d);
void medit_disp_aff_flags(Descriptor *d);
void medit_disp_menu(Descriptor *d);
void medit_parse(Descriptor *d, char *arg);
void medit_string_cleanup(Descriptor *d, int terminator);
ACMD(do_oasis_medit);

void oedit_setup_new(Descriptor *d);
void oedit_setup_existing(Descriptor *d, int real_num);
void oedit_save_internally(Descriptor *d);
void oedit_save_to_disk(int zone_num);
void oedit_disp_container_flags_menu(Descriptor *d);
void oedit_disp_extradesc_menu(Descriptor *d);
void oedit_disp_prompt_apply_menu(Descriptor *d);
void oedit_liquid_type(Descriptor *d);
void oedit_disp_apply_menu(Descriptor *d);
void oedit_disp_weapon_menu(Descriptor *d);
void oedit_disp_spells_menu(Descriptor *d);
void oedit_disp_val1_menu(Descriptor *d);
void oedit_disp_val2_menu(Descriptor *d);
void oedit_disp_val3_menu(Descriptor *d);
void oedit_disp_val4_menu(Descriptor *d);
void oedit_disp_type_menu(Descriptor *d);
void oedit_disp_extra_menu(Descriptor *d);
void oedit_disp_wear_menu(Descriptor *d);
void oedit_disp_menu(Descriptor *d);
void oedit_parse(Descriptor *d, char *arg);
void oedit_disp_perm_menu(Descriptor *d);
void oedit_string_cleanup(Descriptor *d, int terminator);
ACMD(do_oasis_oedit);

void redit_string_cleanup(Descriptor *d, int terminator);
void redit_setup_new(Descriptor *d);
void redit_setup_existing(Descriptor *d, room_vnum v_num);
void redit_save_internally(Descriptor *d);
void redit_save_to_disk(zone_vnum zone_num);
void redit_disp_extradesc_menu(Descriptor *d);
void redit_disp_look_above_menu(Descriptor *d);
void redit_disp_look_under_menu(Descriptor *d);
void redit_disp_look_behind_menu(Descriptor *d);
void redit_disp_exit_menu(Descriptor *d);
void redit_disp_exit_flag_menu(Descriptor *d);
void redit_disp_flag_menu(Descriptor *d);
void redit_disp_sector_menu(Descriptor *d);
void redit_disp_menu(Descriptor *d);
void redit_parse(Descriptor *d, char *arg);
ACMD(do_oasis_redit);

void sedit_setup_new(Descriptor *d);
void sedit_setup_existing(Descriptor *d, int rshop_num);
void sedit_save_internally(Descriptor *d);
void sedit_save_to_disk(int zone_num);
void sedit_products_menu(Descriptor *d);
void sedit_compact_rooms_menu(Descriptor *d);
void sedit_rooms_menu(Descriptor *d);
void sedit_namelist_menu(Descriptor *d);
void sedit_shop_flags_menu(Descriptor *d);
void sedit_no_trade_menu(Descriptor *d);
void sedit_types_menu(Descriptor *d);
void sedit_disp_menu(Descriptor *d);
void sedit_parse(Descriptor *d, char *arg);
ACMD(do_oasis_sedit);

void zedit_setup(Descriptor *d, room_rnum room_num);
void zedit_new_zone(Character *ch, zone_vnum vzone_num, room_vnum bottom, room_vnum top);
void zedit_create_index(int znum, char *type);
void zedit_save_internally(Descriptor *d);
void zedit_save_to_disk(int zone_num);
void zedit_disp_menu(Descriptor *d);
void zedit_disp_comtype(Descriptor *d);
void zedit_disp_arg1(Descriptor *d);
void zedit_disp_arg2(Descriptor *d);
void zedit_disp_arg3(Descriptor *d);
void zedit_parse(Descriptor *d, char *arg);
ACMD(do_oasis_zedit);

void cedit_setup(Descriptor *d);
void cedit_parse(Descriptor *d, char *arg);
void cedit_save_to_disk( void );
void cedit_string_cleanup(Descriptor *d, int terminator);
ACMD(do_oasis_cedit);

void aedit_disp_menu(Descriptor * d);
void aedit_parse(Descriptor * d, char *arg);
void aedit_setup_new(Descriptor *d);
void aedit_setup_existing(Descriptor *d, int real_num);
void aedit_save_to_disk(Descriptor *d);
void aedit_save_internally(Descriptor *d);
void free_action(struct social_messg *mess);
ACMD(do_oasis_aedit);
/* oasis_delete.c */
int free_strings(void *data, int type);
/* oasis_list.c */
ACMD(do_oasis_list);
ACMD(do_oasis_links);
void list_triggers(Character *ch, zone_rnum rnum, trig_vnum vmin, trig_vnum vmax);
void list_rooms(Character *ch  , zone_rnum rnum, room_vnum vmin, room_vnum vmax);
void list_mobiles(Character *ch, zone_rnum rnum, mob_vnum vmin , mob_vnum vmax );
void list_objects(Character *ch, zone_rnum rnum, obj_vnum vmin , obj_vnum vmax );
void list_shops(Character *ch  , zone_rnum rnum, shop_vnum vmin, shop_vnum vmax);
void list_zones(Character *ch, zone_rnum rnum, zone_vnum vmin, zone_vnum vmax);
void print_zone(Character *ch, zone_rnum rnum);

ACMD(do_oasis_vedit);
void vedit_setup_new(Descriptor *d);
void vedit_setup_existing(Descriptor *d, int num);
void vehicle_menu(Descriptor *d);
void list_vehicles(Character *ch);
void vedit_parse(Descriptor *d,char *arg);

#define CONTEXT_HELP_STRING "help"

#define CONTEXT_OEDIT_MAIN_MENU              	1
#define CONTEXT_OEDIT_EDIT_NAMELIST          	2
#define CONTEXT_OEDIT_SHORTDESC              	3
#define CONTEXT_OEDIT_LONGDESC               	4
#define CONTEXT_OEDIT_ACTDESC                	5
#define CONTEXT_OEDIT_TYPE                   	6
#define CONTEXT_OEDIT_EXTRAS                 	7
#define CONTEXT_OEDIT_WEAR                  	8
#define CONTEXT_OEDIT_WEIGHT                	9
#define CONTEXT_OEDIT_COST                  	10
#define CONTEXT_OEDIT_COSTPERDAY            	11
#define CONTEXT_OEDIT_TIMER                 	12
#define CONTEXT_OEDIT_VALUE_1               	13
#define CONTEXT_OEDIT_VALUE_2               	14
#define CONTEXT_OEDIT_VALUE_3               	15
#define CONTEXT_OEDIT_VALUE_4               	16
#define CONTEXT_OEDIT_APPLY                 	17
#define CONTEXT_OEDIT_APPLYMOD              	18
#define CONTEXT_OEDIT_EXTRADESC_KEY         	19
#define CONTEXT_OEDIT_CONFIRM_SAVEDB        	20
#define CONTEXT_OEDIT_CONFIRM_SAVESTRING    	21
#define CONTEXT_OEDIT_PROMPT_APPLY          	22
#define CONTEXT_OEDIT_EXTRADESC_DESCRIPTION 	23
#define CONTEXT_OEDIT_EXTRADESC_MENU        	24
#define CONTEXT_OEDIT_LEVEL                 	25
#define CONTEXT_OEDIT_PERM			26
#define CONTEXT_REDIT_MAIN_MENU 		27
#define CONTEXT_REDIT_NAME 			28
#define CONTEXT_REDIT_DESC 			29
#define CONTEXT_REDIT_FLAGS 			30
#define CONTEXT_REDIT_SECTOR 			31
#define CONTEXT_REDIT_EXIT_MENU 		32
#define CONTEXT_REDIT_CONFIRM_SAVEDB 		33
#define CONTEXT_REDIT_CONFIRM_SAVESTRING 	34
#define CONTEXT_REDIT_EXIT_NUMBER 		35
#define CONTEXT_REDIT_EXIT_DESCRIPTION 		36
#define CONTEXT_REDIT_EXIT_KEYWORD 		37
#define CONTEXT_REDIT_EXIT_KEY 			38
#define CONTEXT_REDIT_EXIT_DOORFLAGS 		39
#define CONTEXT_REDIT_EXTRADESC_MENU 		40
#define CONTEXT_REDIT_EXTRADESC_KEY 		41
#define CONTEXT_REDIT_EXTRADESC_DESCRIPTION 	42
#define CONTEXT_ZEDIT_MAIN_MENU              	43
#define CONTEXT_ZEDIT_DELETE_ENTRY		44
#define CONTEXT_ZEDIT_NEW_ENTRY			45
#define CONTEXT_ZEDIT_CHANGE_ENTRY		46
#define CONTEXT_ZEDIT_COMMAND_TYPE		47
#define CONTEXT_ZEDIT_IF_FLAG			48
#define CONTEXT_ZEDIT_ARG1			49
#define CONTEXT_ZEDIT_ARG2			50
#define CONTEXT_ZEDIT_ARG3			51
#define CONTEXT_ZEDIT_ZONE_NAME			52
#define CONTEXT_ZEDIT_ZONE_LIFE			53
#define CONTEXT_ZEDIT_ZONE_BOT			54
#define CONTEXT_ZEDIT_ZONE_TOP			55
#define CONTEXT_ZEDIT_ZONE_RESET		56
#define CONTEXT_ZEDIT_CONFIRM_SAVESTRING	57
#define CONTEXT_ZEDIT_SARG1			58
#define CONTEXT_ZEDIT_SARG2			59
#define CONTEXT_MEDIT_MAIN_MENU              	60
#define CONTEXT_MEDIT_ALIAS			61
#define CONTEXT_MEDIT_S_DESC			62
#define CONTEXT_MEDIT_L_DESC			63
#define CONTEXT_MEDIT_D_DESC			64
#define CONTEXT_MEDIT_NPC_FLAGS			65
#define CONTEXT_MEDIT_AFF_FLAGS			66
#define CONTEXT_MEDIT_CONFIRM_SAVESTRING	67
#define CONTEXT_MEDIT_SEX			68
#define CONTEXT_MEDIT_HITROLL			69
#define CONTEXT_MEDIT_DAMROLL			70
#define CONTEXT_MEDIT_NDD			71
#define CONTEXT_MEDIT_SDD			72
#define CONTEXT_MEDIT_NUM_HP_DICE		73
#define CONTEXT_MEDIT_SIZE_HP_DICE		74
#define CONTEXT_MEDIT_ADD_HP			75
#define CONTEXT_MEDIT_AC			76
#define CONTEXT_MEDIT_EXP			77
#define CONTEXT_MEDIT_GOLD			78
#define CONTEXT_MEDIT_POS			79
#define CONTEXT_MEDIT_DEFAULT_POS		80
#define CONTEXT_MEDIT_ATTACK			81
#define CONTEXT_MEDIT_LEVEL			82
#define CONTEXT_MEDIT_ALIGNMENT			83
#define CONTEXT_SEDIT_MAIN_MENU              	84
#define CONTEXT_SEDIT_CONFIRM_SAVESTRING	85
#define CONTEXT_SEDIT_NOITEM1			86
#define CONTEXT_SEDIT_NOITEM2			87
#define CONTEXT_SEDIT_NOCASH1			88
#define CONTEXT_SEDIT_NOCASH2			89
#define CONTEXT_SEDIT_NOBUY			90
#define CONTEXT_SEDIT_BUY			91
#define CONTEXT_SEDIT_SELL			92
#define CONTEXT_SEDIT_PRODUCTS_MENU		93
#define CONTEXT_SEDIT_ROOMS_MENU		94
#define CONTEXT_SEDIT_NAMELIST_MENU		95
#define CONTEXT_SEDIT_NAMELIST			96
#define CONTEXT_SEDIT_OPEN1			97
#define CONTEXT_SEDIT_OPEN2			98
#define CONTEXT_SEDIT_CLOSE1			99
#define CONTEXT_SEDIT_CLOSE2			100
#define CONTEXT_SEDIT_KEEPER			101
#define CONTEXT_SEDIT_BUY_PROFIT		102
#define CONTEXT_SEDIT_SELL_PROFIT		103
#define CONTEXT_SEDIT_TYPE_MENU			104
#define CONTEXT_SEDIT_DELETE_TYPE		105
#define CONTEXT_SEDIT_DELETE_PRODUCT		106
#define CONTEXT_SEDIT_NEW_PRODUCT		107
#define CONTEXT_SEDIT_DELETE_ROOM		108
#define CONTEXT_SEDIT_NEW_ROOM			109
#define CONTEXT_SEDIT_SHOP_FLAGS		110
#define CONTEXT_SEDIT_NOTRADE			111
#define CONTEXT_TRIGEDIT_MAIN_MENU              112
#define CONTEXT_TRIGEDIT_TRIGTYPE               113
#define CONTEXT_TRIGEDIT_CONFIRM_SAVESTRING	114
#define CONTEXT_TRIGEDIT_NAME			115
#define CONTEXT_TRIGEDIT_INTENDED		116
#define CONTEXT_TRIGEDIT_TYPES			117
#define CONTEXT_TRIGEDIT_COMMANDS		118
#define CONTEXT_TRIGEDIT_NARG			119
#define CONTEXT_TRIGEDIT_ARGUMENT		120
#define CONTEXT_SCRIPT_MAIN_MENU		121
#define CONTEXT_SCRIPT_NEW_TRIGGER		122
#define CONTEXT_SCRIPT_DEL_TRIGGER		123

/* includes number 0 */
#define NUM_CONTEXTS 124

/* Prototypes for the context sensitive help system */
int find_context(Descriptor *d);
int find_context_oedit(Descriptor *d);
int find_context_redit(Descriptor *d);
int find_context_zedit(Descriptor *d);
int find_context_medit(Descriptor *d);
int find_context_sedit(Descriptor *d);
int find_context_trigedit(Descriptor *d);
int find_context_script_edit(Descriptor *d);
int context_help(Descriptor *d, char *arg);
void boot_context_help(void);
void free_context_help(void);

void trigedit_parse(Descriptor *d, char *arg);
void trigedit_setup_existing(Descriptor *d, int rtrg_num);
void trigedit_setup_new(Descriptor *d);
ACMD(do_oasis_trigedit);

#endif
