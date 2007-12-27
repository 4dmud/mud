/*
 * ************************************************************************
 * File: db.h                                          Part of CircleMUD *
 * Usage: header file for database handling                               * *
 * 
 * All rights reserved.  See license.doc for complete information.        * *
 * 
 * Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
 * CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               * 
 ***********************************************************************
 */

/* arbitrary constants used by index_boot() (must be unique) */
#define DB_BOOT_WLD	0
#define DB_BOOT_MOB	1
#define DB_BOOT_OBJ	2
#define DB_BOOT_ZON	3
#define DB_BOOT_SHP	4
#define DB_BOOT_HLP	5
#define DB_BOOT_TRG	6

#if defined(CIRCLE_MACINTOSH)
#define LIB_WORLD	":world:"
#define LIB_TEXT	":text:"
#define LIB_TEXT_HELP	":text:help:"
#define LIB_MISC	":misc:"
#define LIB_ETC		":etc:"
#define LIB_PLRTEXT	":plrtext:"
#define LIB_PLROBJS	":plrobjs:"
#define LIB_PLRALIAS	":plralias:"
#define LIB_HOUSE	":house:"
#define LIB_PLRVARS	":plrvars:"
#define SLASH		":"
#elif defined(CIRCLE_AMIGA) || defined(CIRCLE_UNIX) || defined(CIRCLE_WINDOWS) || defined(CIRCLE_ACORN)
#define LIB_WORLD	"world/"
#define LIB_TEXT	"text/"
#define LIB_TEXT_HELP	"text/help/"
#define LIB_MISC	"misc/"
#define LIB_ETC		"etc/"
#define LIB_PLRTEXT	"plrtext/"
#define LIB_PLROBJS	"plrobjs/"
#define LIB_HOUSE	"house/"
#define LIB_PLRVARS	"plrvars/"
#define SLASH		"/"
#define LIB_PLRALIAS    "plralias/"
#define LIB_PLRPOOFS    "plrpoofs/"
#else
#error "Unknown path components."
#endif

#define ZON_SUFFIX	".zon"
#define MOB_SUFFIX	".mob"
#define OBJ_SUFFIX	".obj"
#define WLD_SUFFIX	".wld"
#define SHP_SUFFIX	".shp"
#define TRG_SUFFIX	".trg"

#define SUF_OBJS	"objs"
#define SUF_TEXT	"text"
#define SUF_ALIAS       "alias"
#define SUF_POOFS       "poofs"
#define SUF_MEM		"mem"
#if defined(CIRCLE_AMIGA)
#define FASTBOOT_FILE   "/.fastboot"	/* autorun: boot without sleep  */
#define KILLSCRIPT_FILE "/.killscript"	/* autorun: shut mud down       */
#define PAUSE_FILE      "/pause"	/* autorun: don't restart mud   */
#elif defined(CIRCLE_MACINTOSH)
#define FASTBOOT_FILE	"::.fastboot"	/* autorun: boot without sleep   */
#define KILLSCRIPT_FILE	"::.killscript"	/* autorun: shut mud down        */
#define PAUSE_FILE	"::pause"	/* autorun: don't restart mud    */
#else
#define FASTBOOT_FILE   "../.fastboot"	/* autorun: boot without sleep  */
#define KILLSCRIPT_FILE "../.killscript"	/* autorun: shut mud down       */
#define PAUSE_FILE      "../pause"	/* autorun: don't restart mud   */
#endif

/* names of various files and directories */
#define INDEX_FILE	"index"	/* index of world files          */
#define MINDEX_FILE	"index.mini"	/* ... and for mini-mud-mode     */
#if 0
#define WLD_PREFIX	LIB_WORLD"wld"SLASH	/* room definitions      */
#define MOB_PREFIX	LIB_WORLD"mob"SLASH	/* monster prototypes    */
#define OBJ_PREFIX	LIB_WORLD"obj"SLASH	/* object prototypes     */
#define ZON_PREFIX	LIB_WORLD"zon"SLASH	/* zon defs & command tables */
#define SHP_PREFIX	LIB_WORLD"shp"SLASH	/* shop definitions      */
#define TRG_PREFIX	LIB_WORLD"trg"SLASH	/* shop definitions      */
#endif
#define HLP_PREFIX	LIB_TEXT"help"SLASH	/* for HELP <keyword>    */
#define HELP_FILE	"help.hlp"	/* help file             */
#define STARTUP_FILE	LIB_TEXT"startup"	/* startup screen        */
#define CREDITS_FILE	LIB_TEXT"credits"	/* for the 'credits' command     */
#define NEWS_FILE	LIB_TEXT"news"	/* for the 'news' command        */
#define MOTD_FILE	LIB_TEXT"motd"	/* messages of the day / mortal  */
#define IMOTD_FILE	LIB_TEXT"imotd"	/* messages of the day / immort  */
#define GREETINGS_FILE	LIB_TEXT"greetings"	/* The opening screen.   */
#define HELP_PAGE_FILE	LIB_TEXT_HELP"screen"	/* for HELP <CR>                 */
#define INFO_FILE	LIB_TEXT"info"	/* for INFO              */
#define WIZLIST_FILE	LIB_TEXT"wizlist"	/* for WIZLIST           */
#define IMMLIST_FILE	LIB_TEXT"immlist"	/* for IMMLIST           */
#define BACKGROUND_FILE	LIB_TEXT"background"	/* for the background story      */
#define POLICIES_FILE	LIB_TEXT"policies"	/* player policies/rules         */
#define HANDBOOK_FILE	LIB_TEXT"handbook"	/* handbook for new immorts      */
#define CONTEXT_HELP_FILE LIB_TEXT"contexthelp"	/* context help for olc	*/
#define VEHICLE_FILE   LIB_TEXT"vehicles"


//#define IDEA_FILE     LIB_MISC"ideas"
//#define TYPO_FILE       LIB_MISC"typos"
//#define BUG_FILE        LIB_MISC"bugs"
#define IDEA_FILE	"/home/httpd/vhosts/4dimensions.org/httpdocs/admin/ideas.html"
#define TYPO_FILE	"/home/httpd/vhosts/4dimensions.org/httpdocs/admin/typos.html"
#define BUG_FILE	"/home/httpd/vhosts/4dimensions.org/httpdocs/admin/bugs.html"
#define TIME_FILE       LIB_MISC"time"	/* time file for saving time */
#define HOST_LIST_FILE       LIB_MISC"host_list"	/* host lists file for meta server */
#define MESS_FILE	LIB_MISC"messages"	/* damage messages               */
#define SOCMESS_FILE_NEW	LIB_MISC"socials"	/* messgs for social acts        */
 #define SOCMESS_FILE	LIB_MISC"socials"  /* messages for social acts	*/
#define XNAME_FILE	LIB_MISC"xnames"	/* invalid name substrings       */
#define FOREST_FILE     LIB_MISC"forest"


#define CONFIG_FILE	LIB_ETC"config"    /* OasisOLC * GAME CONFIG FL */
#define PLAYER_FILE	LIB_ETC"players"	/* the player database           */
#define MAIL_FILE	LIB_ETC"plrmail"	/* for the mudmail system        */
#define NOTE_FILE       LIB_ETC"plrnotes.txt"
#define IDEAS_FILE      LIB_ETC"plrideas.txt"
#define NEWSNOTE_FILE       LIB_ETC"plrnews.txt"
#define PENALTY_FILE    LIB_ETC"plrpenalty.txt"
#define CHANGES_FILE    LIB_ETC"plrchanges.txt"
#define BAN_FILE	LIB_ETC"badsites"	/* for the siteban system        */
#define HCONTROL_FILE	LIB_ETC"hcontrol"	/* for the house system  */
#define CLAN_FILE	LIB_ETC"clans"	/* clan file             */
#define QIC_FILE	LIB_ETC"qicdb"	/* qic database          */
#define CLAN_DIR	LIB_ETC"/clan/"
#define CLAN_INDEX_FILE CLAN_DIR"clan_index"
#define ASSEMBLIES_FILE LIB_ETC"assemblies" /* Assemblies engine 	*/

#define PLR_PREFIX	"pfiles"
#define BACKUP_PREFIX	"pfiles/backup"
#define PLR_INDEX_FILE	"pfiles/plr_index"
#define SLASH "/"

#define PLR_SUFFIX	""
#define PINDEX_DELETED		(1 << 0)
#define PINDEX_NODELETE		(1 << 1)
#define PINDEX_SELFDELETE	(1 << 2)
#define PINDEX_FIXSKILLS		(1 << 3)

#define NOTE_NOTE 	0
#define NOTE_IDEA 	1
#define NOTE_PENALTY 	2
#define NOTE_NEWS 	3
#define NOTE_CHANGES 	4



//#define NO_EXTRANEOUS_TRIGGERS  	TRUE
#define DG_ALLOW_GODS TRUE
//#define R_EXIT(r, rdir)		 ((r)->dir_option[(rdir)])

#define USE_CREATE_CHAR 0

#define MAX_HELPENTRY_LENGTH 32384

extern NOTE_DATA *note_list;
extern NOTE_DATA *idea_list;
extern NOTE_DATA *penalty_list;
extern NOTE_DATA *news_list;
extern NOTE_DATA *changes_list;

extern int TEMP_LOAD_CHAR;
/* public procedures in db.c */
float bytesToSize(int by);
const char *bytesToUnit(int b);
void boot_db(void);
void destroy_db(void);
//mord ? ?
void zone_update(void);
room_rnum real_room(room_vnum vnum);
char *fread_string(FILE * fl, const char *error);
string *fread_string_s(FILE *fl, const char *error);
void save_mud_time(struct time_info_data *when);
//mord ? ?
void free_extra_descriptions(struct extra_descr_data *edesc);
//mord ? ?
void free_text_files(void);
//mord ? ?
zone_rnum real_zone(zone_vnum vnum);
//mord
room_rnum real_room(room_vnum vnum);
//mord
mob_rnum real_mobile(mob_vnum vnum);
//mord
obj_rnum real_object(obj_vnum vnum);
//mord

bitvector_t asciiflag_conv(char *flag);
void sprintbits(int data, char *dest);
void half_chop(char *string, char *arg1, char *arg2);
void char_to_store(Character *ch);
int store_to_char(const char *name, Character *ch);
#if USE_CREATE_CHAR
Character *create_char(void);
#endif
Character *read_mobile(mob_vnum nr);
int vnum_mobile(char *searchname, Character *ch);
struct obj_data *create_obj(void);
void clear_object(struct obj_data *obj);
void free_obj(struct obj_data *obj, int extracted);
void obj_data_to_pool(struct obj_data *obj);
void free_obj_q(struct obj_data *obj);
void free_obj_q_delayed(struct obj_data *obj);
void free_obj_forget(struct obj_data *obj);
struct obj_data *read_object(obj_vnum nr, int type);
int vnum_object(char *searchname, Character *ch);
int my_obj_save_to_disk(FILE * fp, struct obj_data *obj, int locate);
bool str_prefix( const char *astr, const char *bstr );
void fixskills(Character *ch);
/*
 * Read a letter from a file.
 */
char fread_letter( FILE *fp );
int interpolate( int level, int value_00, int value_32 );
long number_mm( void );
int number_door( void );
int number_percent( void );
char *fread_word( FILE *fp );
int fread_number( FILE *fp );


extern int sunlight;		/* What state the sun is at */

extern int OBJ_INNATE;
extern int OBJ_INNATE_MESSAGE;

#define REAL 0
#define VIRTUAL 1

/* structure for the reset commands */
struct reset_com {
    char command;		/* current command                      */

    bool if_flag;		/* if TRUE: exe only if preceding exe'd */
    int arg1;			/* */
    int arg2;			/* Arguments to the command             */
    int arg3;			/* */
    int arg4;
    int line;			/* line number this command appears on  */
    char *sarg1;		/* string argument                      */
    char *sarg2;		/* string argument                      */

    /*
     * Commands:              * 'M': Read a mobile     * 'O': Read an
     * object    * 'G': Give obj to mob   * 'P': Put obj in obj    * 'G':
     * Obj to char       * 'E': Obj to char equip * 'D': Set state of
     * door * 'T': Trigger command   *
     */
    reset_com() :
            command(0),
            if_flag(0),
            arg1(0),
            arg2(0),
            arg3(0),
            arg4(0),
            line(0),
            sarg1(NULL),
    sarg2(NULL) {}

    ~reset_com() {
        /* first see if any vars were defined in this zone */
        if (command == 'V') {
            if (sarg1)
                free(sarg1);
            if (sarg2)
                free(sarg2);
        }
    }
};



/* zone definition structure. for the 'zone-table'   */
class Zone {
public:
    char *name;			/* name of this zone                  */
    int lifespan;		/* how long between resets (minutes)  */
    int age;			/* current age of this zone (minutes) */
    room_vnum bot;		/* starting room number for this zone */
    room_vnum top;		/* upper limit for rooms in this zone */
    int dimension;
    int num_players;	/* how many players are in this zone currently? */
    int reset_mode;		/* conditions for reset (see below)   */
    zone_vnum number;		/* virtual number of this zone    */
    vector<reset_com> cmd;	/* command table for reset                */
    int pressure;		/* How is the pressure (Mb)               */
    int change;			/* How fast and what way does it change */
    int sky;			/* How is the sky                         */

    char *builders;		/* for OLC.  OBuild like extention,   *
                      				 * part of OLC+                       */

    long zone_flags;		/* Zone Flags                     */
    int idle_time;		/* How long has it been idle      */
    int write_zone();
    room_vnum Bot() {
        return bot;
    }
    room_vnum Top() {
        return top;
    }

    /*
     * Reset mode:                              * 0: Don't reset, and
     * don't update age.    * 1: Reset if no PC's are located in zone. *
     * 2: Just reset.                           *
     */
    Zone() {
        name =NULL;
        lifespan = 30;
        age = 0;
        bot = 0;
        top = 99;
        dimension = D_ALL;
        reset_mode = 2;
        number = NOWHERE;
        pressure = 1021;
        change = 1;
        sky = SKY_CLOUDLESS;
        builders = NULL;
        zone_flags = 0;
        idle_time = 0;
        num_players = 0;
    }

    Zone(const char * n, zone_vnum vn, room_vnum b, room_vnum t) {
        name =NULL;
        lifespan = 30;
        age = 0;
        bot = 0;
        top = 99;
        dimension = D_ALL;
        reset_mode = 2;
        number = NOWHERE;
        pressure = 1021;
        change = 1;
        sky = SKY_CLOUDLESS;
        builders = NULL;
        zone_flags = 0;
        idle_time = 0;
        /* now set these */
        bot=b;
        top = t;
        number = vn;
        if (n)
            name = strdup(n);
        reset_com rc = reset_com();
        rc.command = 'S';
        cmd.push_back(rc);
        num_players = 0;
    }

    void Destroy() {
        if (this->name != NULL)
            free(this->name);
        this->name = NULL;
        if (this->builders != NULL)
            free(this->builders);
        this->builders = NULL;
    }

~Zone() {}
}
;
/** genzon.c - at the bottom - Mord**/
/** zone vector compare **/
bool operator< (const vector<Zone>::iterator &a,const  vector<Zone>::iterator &b);
bool operator> (const vector<Zone>::iterator &a,const  vector<Zone>::iterator &b);
bool operator== (const vector<Zone>::iterator &a,const  vector<Zone>::iterator &b);
/** Zone compare **/
bool operator< (const Zone &a,const Zone &b);
bool operator> (const Zone &a,const Zone &b);
bool operator== (const Zone &a,const Zone &b);
/** zone data to int compare **/
bool operator< (const Zone &z,const zone_vnum b);
bool operator> (const Zone &z,const zone_vnum b);
bool operator== (const Zone &z,const zone_vnum b);


/* Bitvector for 'zone flags' */
#define Z_IDLE		(1 <<  0)	/* Idle zone flag */
#define Z_SYSTEM	(1 <<  1)	/* A zone that should not be idled */

/* for queueing zones for update   */
struct reset_q_element {
    zone_rnum zone_to_reset;	/* ref to Zone */
    struct reset_q_element *next;

    reset_q_element() {
        zone_to_reset=0;
        next=NULL;
    }
}
;



/* structure for the update queue     */
struct reset_q_type {
    struct reset_q_element *head;
    struct reset_q_element *tail;

    reset_q_type() {
        head=NULL;
        tail = NULL;
    }
}
;



struct help_index_element {
    char	*keywords;
    char *entry;
    int min_level;
    int duplicate;
    int id;
    int entries; /* How many key words there are */

    help_index_element() :
            keywords(NULL),
            entry(NULL),
            min_level(0),
            duplicate(0),
            id(0),
    entries(0) {}
}
;


void the_free_help(void);
void free_help(struct help_index_element *help);
extern unsigned int top_of_helpt;


/* don't change these */
#define BAN_NOT 	0
#define BAN_NEW 	1
#define BAN_SELECT	2
#define BAN_ALL		3
#define BAN_NAME	4

#define BANNED_SITE_LENGTH    50
struct ban_list_element {
    char site[BANNED_SITE_LENGTH + 1];
    int type;
    time_t date;
    char name[MAX_NAME_LENGTH + 1];
    struct ban_list_element *next;
};


/* global buffering system */

#ifndef __DB_C__
extern struct config_data config_info;
extern int top_of_world;
extern player_special_data dummy_mob;
extern struct social_messg *soc_mess_list;
extern int top_of_socialt;
extern obj_rnum top_of_objt;
//extern mob_rnum top_of_mobt;
extern zone_rnum top_of_zone_table;
extern map<mob_vnum, struct index_data*> mob_index;
extern struct index_data *obj_index;
extern struct shop_data *shop_index;
extern int top_shop;
extern index_data **trig_index;
extern struct trig_data *trigger_list;
extern unsigned int top_of_trigt;
extern long max_mob_id;
extern long max_obj_id;
extern int dg_owner_purged;
extern vector <Room *> world_vnum;
//extern map<mob_vnum, Character *> mob_proto;
extern struct obj_data *obj_proto;
//extern Zone *zone_table;
extern vector <Zone> zone_table;

extern map<long, obj_data *> object_list;
extern map<long, obj_data *> dead_obj;  /* delayed obj removal   */
#endif /* __DB_C__ */
extern Descriptor *descriptor_list;
extern Character *character_list;
typedef map<long, obj_data *> obj_list_type;
//extern struct htree_node *mob_htree;
//extern struct htree_node *obj_htree;
extern map<obj_vnum,obj_rnum> obj_vTor;

void strip_string(char *buffer);
int read_xap_objects(FILE * fl, Character *ch);
float mob_hitpoint_multi(int chclass);
void generate_weapon(OBJ_DATA *obj);
void add_char_to_list(Character *ch);
void clearAllZones();
#define CUR_WORLD_VERSION 1
#define CUR_ZONE_VERSION 3
#define HIGHEST_VNUM  99900
#define MAX_WORLD 99900

typedef list <Character*> CharacterList;
typedef CharacterList::iterator CharacterListIterator;

typedef list <Descriptor*> DescriptorList;
typedef DescriptorList::iterator DescriptorListIterator;
typedef vector <Room *>::iterator WorldIterator;

typedef map<mob_vnum, Character *>::iterator mp_iter;
typedef map<mob_vnum, struct index_data *>::iterator mi_iter;

bool MobProtoExists(mob_vnum nv);
Character * GetMobProto(mob_vnum vn);
void SetMobProto(mob_vnum vn, Character *c);
int GetMobProtoCount();
mob_vnum DeleteMobProto(mob_vnum vn);

bool MobIndexExists(mob_vnum nv);
struct index_data * GetMobIndex(mob_vnum vn);
void SetMobIndex(mob_vnum vn, struct index_data *c);
int GetMobIndexCount();
mob_vnum DeleteMobIndex(mob_vnum vn);

#include "playerindex.h"

