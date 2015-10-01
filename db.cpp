/*************************************************************************
*   File: db.c                                          Part of CircleMUD *
*  Usage: Loading/saving chars, booting/resetting world, internal funcs   *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */


#define __DB_C__

#include "config.h"
#include "sysdep.h"

#include <dirent.h>

// test 2

#include "structs.h"
#include "utils.h"
#include "db.h"
#include "comm.h"
#include "handler.h"
#include "spells.h"
#include "mail.h"
#include "interpreter.h"
#include "house.h"
#include "constants.h"
#include "oasis.h"
#include "dg_scripts.h"
#include "dg_event.h"
#include "clan.h"
#include "genobj.h"
#include "genmob.h"
#include "genwld.h"
#include "assemblies.h"
#include "trees.h"
#include "damage.h"
#include "descriptor.h"
#include "strutil.h"
#include "name.map.h"
#include "fight.h"

bool can_teach_skill ( Character *mob, int i );
int load_qic_check ( int rnum );
void qic_scan_rent ( void );
void purge_qic ( obj_rnum rnum );
void free_corpse_list ( struct corpse_list_data *cor );
void qic_load ( int rnum );
int allowed_pretitle ( Character *ch );
void free_clan_lists ( void );
void extract_all_in_list ( OBJ_DATA *obj );
void load_host_list ( void );
void add_room_to_mine ( room_rnum room );
const char *get_dirname ( char *filename, size_t len, char oname, int mode );
void free_hunter_list ( void );
void prune_crlf ( char *txt );
int generate_wep_type ( char *name );
int fuzzy_balance ( OBJ_DATA *wep );
int generate_wep_length ( OBJ_DATA *wep );
int OBJ_INNATE_MESSAGE = TRUE;
void ripple_detect(room_vnum location, int object);


int gen_wep_type_from_attack ( OBJ_DATA *obj );
void renumber_zones ( void );
extern int save_new_style;
extern struct corpse_list_data *corpse_list;
struct config_data config_info; /* Game configuration list.  */
void assign_mob_stats ( void );
void free_forests ( struct forest_data *this_forest );
int check_item_hack_invis ( struct obj_data *obj, int fix );
void free_join_list ( struct combine_data *list );
void sort_all_spell_data ( void );
void assign_skills ( void );
void assign_subskills ( void );
//void sprintbits(long vektor, char *outstring);
void tag_argument ( char *argument, char *tag );
int is_aggro ( Character *ch );
void generate_weapon ( OBJ_DATA *obj );

int create_vehicle_room(struct obj_data *obj);
void ASSIGNMOB ( mob_vnum mob, SPECIAL ( fname ) );
void ASSIGNOBJ ( obj_vnum obj, SPECIAL ( fname ) );
void ASSIGNROOM ( room_vnum room, SPECIAL ( fname ) );
SPECIAL ( postmaster );
SPECIAL ( cleric );
SPECIAL ( bank );
SPECIAL ( pet_shops );
void load_saved_artifacts ();


/**************************************************************************
*  declarations of most of the 'global' variables                         *
**************************************************************************/

/* help structures */

struct help_index_element *help_table = 0;   /* the help table    */
unsigned int top_of_helpt = 0;          /* top of help index table     */
unsigned int max_help_id = 0;
struct help_category_data *help_categories;

int TEMP_LOAD_CHAR = FALSE;

extern struct mob_stat_table mob_stats[];

vector <Room *> world_vnum; /* index table for room file   */
NameIndexer mobNames;
NameIndexer objNames;
room_vnum top_of_world = 0;   /* ref to top element of world   */
struct social_messg *soc_mess_list = NULL;      /* list of socials */
int top_of_socialt = -1;                        /* number of socials */

DescriptorList descriptorlist;
CharacterList characterlist;
Character *character_list = NULL;     /* global linked list of chars */
extern enum subskill_list subskill;

struct index_data **trig_index;    /* index table for triggers      */
//extern map<trig_rnum, index_data*> trig_index;
//extern map<trig_vnum, trig_rnum> trig_v_index;
struct trig_data *trigger_list = NULL;  /* all attached triggers */
unsigned int top_of_trigt = 0;         /* top of trigger index table    */
//struct htree_node *mob_htree = NULL;    /* hash tree for fast mob lookup */

long max_mob_id = MOB_ID_BASE;     /* for unique mob id's       */
long max_obj_id = OBJ_ID_BASE;     /* for unique obj id's       */
int dg_owner_purged;          /* For control of scripts */
//struct htree_node *obj_htree = NULL;    /* hash tree for fast obj lookup */

map<mob_vnum, struct index_data *> mob_index; /* index table for mobile file   */
map<mob_vnum, Character *> mob_proto;  /* prototypes for mobs           */
//mob_rnum top_of_mobt = 0;     /* top of mobile index table     */

map <obj_vnum,obj_rnum> obj_vTor;
obj_list_type  object_list;
obj_list_type  dead_obj;  /* delayed obj removal   */
struct index_data *obj_index; /* index table for object file   */
struct obj_data *obj_proto;   /* prototypes for objs           */
obj_rnum top_of_objt = 0;     /* top of object index table     */
struct zone_list_data *zone_list = NULL;

//Zone *zone_table; /* zone table                    */
vector <Zone> zone_table;
zone_rnum top_of_zone_table = 0;   /* top element of zone tab       */
struct message_list fight_messages[MAX_MESSAGES]; /* fighting messages     */

PlayerIndex pi;

int no_mail = 0;         /* mail disabled?                */
int mini_mud = 0;        /* mini-mud mode?                */
int no_rent_check = 0;        /* skip rent check on boot?      */
time_t boot_time = 0;         /* time of mud boot              */
int circle_restrict = 0; /* level of game restriction     */
// int xap_objs = 0;            /* Xap objs - defined in config.c*/
extern int no_specials;
extern int scheck;
int zone_count = 0;
int sunlight;

// Declare real-nums of battle-rooms
//room_vnum r_battle_start_room;   /* rnum of battle start room     */
//room_vnum r_battle_recall_room;  /* rnum of battle recall room    */
//room_vnum r_battle_min_room;     /* rnum of battle min room       */
//room_vnum r_battle_max_room;     /* rnum of battle max room       */

char *credits = NULL;         /* game credits                  */
char *news = NULL;       /* mud news                      */
char *motd = NULL;       /* message of the day - mortals */
char *imotd = NULL;      /* message of the day - immorts */
char *GREETINGS = NULL;       /* opening credits screen       */
char *help = NULL;       /* help screen                   */
char *info = NULL;       /* info page                     */
char *wizlist = NULL;         /* list of higher gods           */
char *immlist = NULL;         /* list of peon gods             */
char *background = NULL; /* background story              */
char *handbook = NULL;        /* handbook for new immortals    */
char *policies = NULL;        /* policies page                 */
char *startup = NULL;         /* startup screen                */


struct time_info_data time_info;   /* the infomation about the time    */
player_special_data dummy_mob = player_special_data();   /* dummy spec area for mobs     */
struct reset_q_type reset_q;  /* queue of zones to be reset    */

/* local functions */
void free_zone_list ( struct zone_list_data *z );
int check_bitvector_names ( bitvector_t bits, size_t namecount, const char *whatami, const char *whatbits );  //mord??
int check_object_spell_number ( struct obj_data *obj, int val, int nr );
int check_object_level ( struct obj_data *obj, int val, int nr );
void setup_dir ( FILE * fl, room_rnum room, int dir );
void index_boot ( int mode );
void discrete_load ( FILE * fl, int mode, char *filename, zone_vnum zon );
int check_object ( struct obj_data *obj, int nr );
void parse_trigger ( FILE * fl, int virtual_nr, zone_vnum zon );
void parse_room ( FILE * fl, int virtual_nr, zone_vnum zon );
void parse_mobile ( FILE * mob_f, int nr, zone_vnum zon );
char *parse_object ( FILE * obj_f, int nr, zone_vnum zon );
void load_zones ( FILE * fl, char *zonename );
void load_help ( FILE * fl );
void assign_mobiles ( void );
void assign_objects ( void );
void assign_rooms ( void );
void assign_the_shopkeepers ( void );
int zone_is_empty ( zone_rnum zone_nr );
void reset_zone ( zone_rnum zone );
int file_to_string ( const char *name, char *buf, size_t b_len );
int file_to_string_alloc ( const char *name, char **buf );
void reboot_wizlists ( void );
ACMD ( do_reboot );
void boot_world ( void );
int count_alias_records ( FILE * fl );
int count_hash_records ( FILE * fl );
void parse_simple_mob ( FILE * mob_f, Character *mob, int nr );
void interpret_espec ( const char *keyword, const char *value, int i,
                       int nr );
void parse_trainer_mob ( FILE * mob_f, Character *mob, int nr );
void parse_trainer_skills ( char *buf, Character *mob, int nr );
void parse_espec ( char *buf, Character *mob, int nr );
void parse_enhanced_mob ( FILE * mob_f, int i, int nr );
void get_one_line ( FILE * fl, char *buf );
void save_etext ( Character *ch );
//void check_start_rooms(void);
void renum_world ( void );
void renum_zone_table ( void );
void log_zone_error ( zone_rnum zone, int cmd_no, const char *message );
void reset_time ( void );
void give_mob_class ( Character *ch, int vnum );
void free_note ( NOTE_DATA *note, int type );
void free_social_messages ( void );
void set_mastery ( Character *ch, char buf );
void free_object_strings ( struct obj_data *obj );
void free_object_strings_proto ( struct obj_data *obj );
void boot_context_help ( void );
void free_context_help ( void );
char fread_letter ( FILE *fp );
void free_followers ( struct follow_type *k );
void load_default_config ( void );
void load_config ( void );

// kalten
//void assign_vehicles(void);
void load_vehicles ( void ); //mord
void set_race ( Character *ch, int race );



/* external functions */
void name_to_drinkcon ( struct obj_data *obj, int type );
void paginate_string ( const char *str, Descriptor *d ); //mord??
struct time_info_data *mud_time_passed ( time_t t2, time_t t1 );
void load_messages ( void );
void weather_and_time ( int mode );
void mag_assign_spells ( void );
void boot_social_messages ( void );
void create_command_list ( void );
void update_obj_file ( void );   /* In objsave.c */
void load_qic ( void );       /* In qic.c     */
void sort_commands ( void );
void sort_spells ( void );
void load_banned ( void );
void Read_Invalid_List ( void );
void boot_the_shops ( FILE * shop_f, char *filename, int rec_count );
void init_clans ( void );
int find_first_step ( room_rnum src, room_rnum target );
void load_corpses ( void );
void new_load_corpses ( void );
void save_char_vars ( Character *ch );
void destroy_shops ( void );  //mord??
void strip_cr ( char * );     //...
void load_notes ( void );
void free_identifier ( struct obj_data *obj );

#define READ_SIZE 256

/* make these TRUE if you want aliases and poofs saved in the pfiles
 * or FALSE to leave them out
 */
#define ASCII_SAVE_POOFS TRUE
#define ASCII_SAVE_ALIASES    TRUE


/*************************************************************************
*  routines for booting the system                                       *
*************************************************************************/


static int taone ( const struct dirent *a1 )
{
	if ( !a1 )
		return 0;
	else if ( !a1->d_name )
		return 0;
	else if ( !*a1->d_name ) //no blank filenames
		return 0;
	else if ( !isdigit ( *a1->d_name ) ) //all files start with a number
		return 0;
	else if ( strstr ( a1->d_name, "~" ) ) //don't read backups
		return 0;

	return 1;
}
int check_dir ( char *dirname )
{
	struct dirent **eps = NULL;
	int n;
	int zon, trg, mob, obj, shp, wld;
	zon = trg =  mob = obj = shp = wld = 0;
	n = scandir ( dirname, &eps, taone, alphasort );
	if ( n >= 0 )
	{
		int cnt;
		for ( cnt = 0; cnt < n; ++cnt )
		{
			if ( eps[cnt]->d_type == DT_REG && isdigit ( *eps[cnt]->d_name ) )
			{
				if ( eps[cnt]->d_name[strlen ( eps[cnt]->d_name )-1] == '~' )
				{
					free ( eps[cnt] );
					continue;
				}
				if ( strstr ( eps[cnt]->d_name, ".zon" ) )
					zon = TRUE;
				if ( strstr ( eps[cnt]->d_name, ".trg" ) )
					trg = TRUE;
				if ( strstr ( eps[cnt]->d_name, ".mob" ) )
					mob = TRUE;
				if ( strstr ( eps[cnt]->d_name, ".obj" ) )
					obj = TRUE;
				if ( strstr ( eps[cnt]->d_name, ".shp" ) )
					shp = TRUE;
				if ( strstr ( eps[cnt]->d_name, ".wld" ) )
					wld = TRUE;
			}
			free ( eps[cnt] );
		}
		if ( !zon )
			log ( "Directory %s is missing its .zon file", dirname );
		if ( !trg )
			log ( "Directory %s is missing its .trg file", dirname );
		if ( !mob )
			log ( "Directory %s is missing its .mob file", dirname );
		if ( !obj )
			log ( "Directory %s is missing its .obj file", dirname );
		if ( !shp )
			log ( "Directory %s is missing its .shp file", dirname );
		if ( !wld )
			log ( "Directory %s is missing its .wld file", dirname );
		if ( eps )
			free ( eps );
		if ( ( zon + trg + mob + obj + shp + wld ) == 6 )
			return 1;
		else
		{
			exit ( 1 );
			return 0;
		}
	}
	else
	{
		log ( "Couldn't open the directory: %s", dirname );
		exit ( 1 );
	}

	return 0;
}

int check_dir ( const string &dirname )
{
	int zon, trg, mob, obj, shp, wld;
	string ftag, errmsg;
	zon = trg =  mob = obj = shp = wld = 0;
	if ( dirname.size() == 0 )
		return 0;
	try
	{
		directory tdir ( dirname );
		queue_of_files files;
		set_of_files sorted_files;
		tdir.get_files ( files );
		sorted_files.load ( files );
		//isdigit(sorted_files.element().name()[0]) &&
		while ( sorted_files.move_next() )
		{
			if ( isdigit ( sorted_files.element().name() [0] ) && sorted_files.element().name().size() > 4 )
			{
				ftag = sorted_files.element().name().substr ( sorted_files.element().name().size()-4, sorted_files.element().name().size() );
				if ( ftag == ".zon" )
					zon = TRUE;
				else if ( ftag == ".trg" )
					trg = TRUE;
				else if ( ftag == ".mob" )
					mob = TRUE;
				else if ( ftag == ".obj" )
					obj = TRUE;
				else if ( ftag == ".shp" )
					shp = TRUE;
				else if ( ftag == ".wld" )
					wld = TRUE;
			}
		}

		if ( !zon )
			log ( "Directory %s is missing its .zon file", dirname.c_str() );
		if ( !trg )
			log ( "Directory %s is missing its .trg file", dirname.c_str() );
		if ( !mob )
			log ( "Directory %s is missing its .mob file", dirname.c_str() );
		if ( !obj )
			log ( "Directory %s is missing its .obj file", dirname.c_str() );
		if ( !shp )
			log ( "Directory %s is missing its .shp file", dirname.c_str() );
		if ( !wld )
			log ( "Directory %s is missing its .wld file", dirname.c_str() );

		if ( ( zon + trg + mob + obj + shp + wld ) == 6 )
			return 1;
		else
		{
			exit ( 1 );
			return 0;
		}
	}
	catch ( file::file_not_found e )
	{
		log ( "check_dir: file not found or accessable: %s", e.info.c_str() );
	}
	catch ( directory::dir_not_found e )
	{
		log ( "check_dir: dir not found or accessable: %s", e.info.c_str() );
	}
	catch ( directory::listing_error e )
	{
		log ( "check_dir: listing error: %s",  e.info.c_str() );
	}
	return 0;
}

int numsort ( const void *a, const void *b )
{
	const struct dirent *a1 = NULL, *b1 = NULL;

	if ( !a || !b )
		return -1;

	a1 = * ( const struct dirent ** ) a;
	b1 = * ( const struct dirent ** ) b;

	if ( !a1 || !b1 )
		return -1;
	else  if ( a1->d_type != DT_DIR || b1->d_type != DT_DIR )
		return -1;
	else if ( isdigit ( *a1->d_name ) && isdigit ( *b1->d_name ) )
	{
		return ( atoi ( a1->d_name ) - atoi ( b1->d_name ) );
	}
	else
	{
		log ( "ERROR in directory sort: a1 is %s, b1 is %s", a1->d_name, b1->d_name );
		return -1;
	}

}


/**

**/

static int zone_compare ( const void *a, const void *b )
{
	const struct zone_list_data *aa, *bb;

	aa = * ( const struct zone_list_data ** ) a;
	bb = * ( const struct zone_list_data ** ) b;

	return bb->num - aa->num;
}

/** This should be called every time a new zone is added/removed **/
void sort_zone_list ( int total )
{
	struct zone_list_data *temp_list[total + 1], *temp, *ztmp;
	int i;
	for ( i = 0, temp = zone_list; i<total && temp; temp = temp->next, i++ )
	{
		temp_list[i] = temp;
	}
#if 0
	for ( i=0;i<total;i++ )
	{
		log ( "Before - %d", temp_list[i]->num );
	}
#endif
	qsort ( temp_list,total,sizeof ( temp_list[0] ),zone_compare );
#if 1

	temp = zone_list;
	ztmp = zone_list;
	zone_list = NULL;
	for ( i=0;i<total;i++ )
	{
		//    log("After  - %d", temp_list[i]->num);
		CREATE ( temp, struct zone_list_data, 1 );
		temp->num = temp_list[i]->num;
		strcpy ( temp->zone, temp_list[i]->zone );
		temp->next = zone_list;
		zone_list = temp;

	}
	free_zone_list ( ztmp );
#endif

	/*for (i = 0, temp = zone_list; i<total && temp; temp = temp->next, i++)
	{
	  temp->num = temp_list[i]->num;
	  log("After2  - %d", temp_list[i]->num);
	  strcpy(temp->zone, temp_list[i]->zone);
	}*/
}

#if 0
int create_zone_index ( void )
{
	struct zone_list_data *temp;
	struct dirent **eps = NULL;
	char dname[256];
	int n, total = 0;

	n = scandir ( LIB_WORLD, &eps, taone, numsort );
	if ( n > 0 )
	{
		while ( n-- )
		{
			if ( eps[n]->d_type == DT_DIR && isdigit ( *eps[n]->d_name ) )
			{
				snprintf ( dname, sizeof ( dname ), "%s%s/", LIB_WORLD, eps[n]->d_name );
				if ( check_dir ( dname ) )
				{
					zone_count++;
					CREATE ( temp, struct zone_list_data, 1 );
					strcpy ( temp->zone, dname );
					temp->num = atoi ( eps[n]->d_name );
					temp->next = zone_list;
					zone_list = temp;
					total++;
					//log("zone: %d", temp->num);
				}
			}
			if ( eps[n] )
			{
				free ( eps[n] );
				eps[n] = NULL;
			}
		}
	}
	else
	{
		log ( "Couldn't open the directory: %s", LIB_WORLD );
		free ( eps );
		eps=NULL;
		exit ( 1 );
	}
	if ( eps )
		free ( eps );
	log ( " -- sorting zones" );
	sort_zone_list ( total );
	return 0;

}
#else
int create_zone_index ( void )
{
	struct zone_list_data *temp;
	int total = 0;
	string dname, errmsg;
	try
	{
		directory tdir ( LIB_WORLD );
		queue_of_dirs dirs;
		set_of_dirs sorted_dirs;

		// get all directories
		tdir.get_dirs ( dirs );
		// load the dirs and files into static_sets.  This
		// seems weird but a static_set can be enumerated in sorted order
		// so this way we can print everything in sorted order.  This
		// static_set also uses a median of three quick sort so the sorting
		// should be very fast.
		sorted_dirs.load ( dirs );

		while ( sorted_dirs.move_next() )
			if ( isdigit ( sorted_dirs.element().name() [0] ) && check_dir ( sorted_dirs.element().full_name() ) )
			{
				zone_count++;
				CREATE ( temp, struct zone_list_data, 1 );
				strcpy ( temp->zone, sorted_dirs.element().full_name().c_str() );
				temp->num = atoi ( sorted_dirs.element().name().c_str() );
				temp->next = zone_list;
				zone_list = temp;
				total++;
			}
		log ( " %d zones loaded to index", total );
		if ( total == 0 )
		{
			log ( "No zones found. Aborting Mud Boot." );
			exit ( 1 );
		}
		log ( " -- sorting zones" );
		sort_zone_list ( total );
		return 0;
	}
	catch ( file::file_not_found e )
	{
		log ( "Create_zone_index: file not found or accessable: %s", e.info.c_str() );
	}
	catch ( directory::dir_not_found e )
	{
		log ( "Create_zone_index: dir not found or accessable: %s",e.info.c_str() );
	}
	catch ( directory::listing_error e )
	{
		log ( "Create_zone_index: listing error: %s", e.info.c_str() );
	}
	exit ( 1 );
	return 1;
}
#endif
/* this is necessary for the autowiz system */
void reboot_wizlists ( void )
{
	file_to_string_alloc ( WIZLIST_FILE, &wizlist );
	file_to_string_alloc ( IMMLIST_FILE, &immlist );
}

/* Wipe out all the loaded text files, for shutting down. */
void free_text_files ( void )
{
	char **textfiles[] =
	{
		&wizlist, &immlist, &news, &credits, &motd, &imotd, &help, &info,
		&policies, &handbook, &background, &GREETINGS, NULL
	};
	int rf;

	for ( rf = 0; textfiles[rf]; rf++ )
		if ( *textfiles[rf] )
		{
			free ( *textfiles[rf] );
			*textfiles[rf] = NULL;
		}
}

/*
 * Too bad it doesn't check the return values to let the user
 * know about -1 values.  This will result in an 'Okay.' to a
 * 'reload' command even when the string was not replaced.
 * To fix later, if desired. -gg 6/24/99
 */
ACMD ( do_reboot )
{
	//int i;
	char arg[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );

	if ( !strcmp ( arg, "all" ) || *arg == '*' )
	{
		if ( file_to_string_alloc ( GREETINGS_FILE, &GREETINGS ) == 0 )
			prune_crlf ( GREETINGS );
		if ( file_to_string_alloc ( WIZLIST_FILE, &wizlist ) < 0 )
			ch->Send ( "Can not read wizlist\r\n" );
		if ( file_to_string_alloc ( IMMLIST_FILE, &immlist ) < 0 )
			ch->Send ( "Can not read immlist\r\n" );
		if ( file_to_string_alloc ( NEWS_FILE, &news ) < 0 )
			ch->Send ( "Can not read news\r\n" );
		if ( file_to_string_alloc ( CREDITS_FILE, &credits ) < 0 )
			ch->Send ( "Can not read credits\r\n" );
		if ( file_to_string_alloc ( MOTD_FILE, &motd ) < 0 )
			ch->Send ( "Can not read motd\r\n" );
		if ( file_to_string_alloc ( IMOTD_FILE, &imotd ) < 0 )
			ch->Send ( "Can not read imotd\r\n" );
		if ( file_to_string_alloc ( HELP_PAGE_FILE, &help ) < 0 )
			ch->Send ( "Can not read help front page\r\n" );
		if ( file_to_string_alloc ( INFO_FILE, &info ) < 0 )
			ch->Send ( "Can not read info file\r\n" );
		if ( file_to_string_alloc ( POLICIES_FILE, &policies ) < 0 )
			ch->Send ( "Can not read policies\r\n" );
		if ( file_to_string_alloc ( HANDBOOK_FILE, &handbook ) < 0 )
			ch->Send ( "Can not read handbook\r\n" );
		if ( file_to_string_alloc ( BACKGROUND_FILE, &background ) < 0 )
			ch->Send ( "Can not read background\r\n" );
		if ( help_table )
			the_free_help();
		index_boot ( DB_BOOT_HLP );
	}
	else if ( !strcmp ( arg, "wizlist" ) )
	{
		if ( file_to_string_alloc ( WIZLIST_FILE, &wizlist ) < 0 )
			ch->Send ( "Can not read wizlist\r\n" );
	}
	else if ( !strcmp ( arg, "immlist" ) )
	{
		if ( file_to_string_alloc ( IMMLIST_FILE, &immlist ) < 0 )
			ch->Send ( "Can not read immlist\r\n" );
	}
	else if ( !strcmp ( arg, "news" ) )
	{
		if ( file_to_string_alloc ( NEWS_FILE, &news ) < 0 )
			ch->Send ( "Can not read news\r\n" );
	}
	else if ( !strcmp ( arg, "credits" ) )
	{
		if ( file_to_string_alloc ( CREDITS_FILE, &credits ) < 0 )
			ch->Send ( "Can not read credits\r\n" );
	}
	else if ( !strcmp ( arg, "motd" ) )
	{
		if ( file_to_string_alloc ( MOTD_FILE, &motd ) < 0 )
			ch->Send ( "Can not read motd\r\n" );
	}
	else if ( !strcmp ( arg, "imotd" ) )
	{
		if ( file_to_string_alloc ( IMOTD_FILE, &imotd ) < 0 )
			ch->Send ( "Can not read imotd\r\n" );
	}
	else if ( !strcmp ( arg, "help" ) )
	{
		if ( file_to_string_alloc ( HELP_PAGE_FILE, &help ) < 0 )
			ch->Send ( "Can not read help front page\r\n" );
	}
	else if ( !strcmp ( arg, "info" ) )
	{
		if ( file_to_string_alloc ( INFO_FILE, &info ) < 0 )
			ch->Send ( "Can not read info\r\n" );
	}
	else if ( !strcmp ( arg, "policy" ) )
	{
		if ( file_to_string_alloc ( POLICIES_FILE, &policies ) < 0 )
			ch->Send ( "Can not read policy\r\n" );
	}
	else if ( !strcmp ( arg, "handbook" ) )
	{
		if ( file_to_string_alloc ( HANDBOOK_FILE, &handbook ) < 0 )
			ch->Send ( "Can not read handbook\r\n" );
	}
	else if ( !strcmp ( arg, "background" ) )
	{
		if ( file_to_string_alloc ( BACKGROUND_FILE, &background ) < 0 )
			ch->Send ( "Can not read background\r\n" );
	}
	else if ( !strcmp ( arg, "greetings" ) )
	{
		if ( file_to_string_alloc ( GREETINGS_FILE, &GREETINGS ) == 0 )
			prune_crlf ( GREETINGS );
		else
			ch->Send ( "Can not read greetings.\r\n" );
	}
	else if ( !strcmp ( arg, "xhelp" ) )
	{
		if ( help_table )
			the_free_help();   //mord??

		index_boot ( DB_BOOT_HLP );
	}
	else
	{
		ch->Send ( "Unknown reload option.\r\n" );
		return;
	}

	ch->Send ( "%s", CONFIG_OK );
}


void boot_world ( void )
{
	log ( "Creating zone index" );
	create_zone_index();
	log ( "Loading zone table." );
	index_boot ( DB_BOOT_ZON );

	log ( "Loading triggers and generating index." );
	index_boot ( DB_BOOT_TRG );

	log ( "Loading rooms." );
	index_boot ( DB_BOOT_WLD );

	log ( "Renumbering rooms." );
	renum_world();

	//log("Checking start rooms.");
	//check_start_rooms();
	log ( "Resetting PK Leader." );
        LAST_PK = strdup("Nobody");
        CHAMPION = 0;

	log ( "Seting mob stats..." );
	assign_mob_stats();

	log ( "Loading mobs and generating index." );
	index_boot ( DB_BOOT_MOB );

	log ( "Loading objs and generating index." );
	index_boot ( DB_BOOT_OBJ );



	log ( "Renumbering zone table." );
	renum_zone_table();
	log ( "Renumbering zones" );
	renumber_zones();

	log ( "Loading forest trees." );
	init_trees ( load_forest() );

	log ( "Loading Host List." );
	load_host_list();

#if !defined(WIN32)

	log ( "Loading Corpses" );
	if ( save_new_style )
		new_load_corpses();
	else
		load_corpses();
#endif

	log ( "Loading saved artifacts" );
	load_saved_artifacts();

	log ( "Loading Notes" );
	load_notes();

	if ( !no_specials )
	{
		log ( "Loading shops." );
		index_boot ( DB_BOOT_SHP );
	}

}

void free_objects()
{
	vector<long> ex_list;
	if ( object_list.empty() )
		return;
	/** Find all items that need extracting **/
	for ( olt_it ob = object_list.begin(); ob != object_list.end(); ob++ )
		ex_list.push_back ( GET_ID ( ( ob->second ) ) );
	/** extract them now **/
	for ( vector<long>::iterator v = ex_list.begin();v!= ex_list.end();v++ )
	{
		if ( object_list.find ( *v ) != object_list.end() )
			free_obj ( object_list[ ( *v ) ], TRUE );
		if ( dead_obj.find ( *v ) != dead_obj.end() )
			dead_obj.erase ( *v );
	}

	object_list.clear();
}
void free_pending_objects()
{
	vector<long> ex_list;
	if ( dead_obj.empty() )
		return;
	for ( olt_it o = dead_obj.begin();o != dead_obj.end();o++ )
		ex_list.push_back ( GET_ID ( ( o->second ) ) );
	for ( vector<long>::iterator v = ex_list.begin();v!= ex_list.end();v++ )
	{
		if ( dead_obj.find ( *v ) != dead_obj.end() )
			free_obj ( dead_obj[ ( *v ) ], TRUE );
		if ( object_list.find ( *v ) != object_list.end() )
			object_list.erase ( *v );
	}

	dead_obj.clear();
}

void free_characters ( Character *ch )
{
	if ( !ch )
		return;
	if ( ch->next )
		free_characters ( ch->next );

	delete ch;
}

void free_zone_list ( struct zone_list_data *z )
{
	if ( !z )
		return;
	if ( z->next )
		free_zone_list ( z->next );
	free ( z );
}

/* Free the world, in a memory allocation sense. */
void destroy_db ( void )
{

	unsigned int cnt;
	//    vector<Character>::iterator it;

	log ( "Free Hunter List" );
	free_hunter_list();
	log ( "Freeing the memory of the database." );
	/* Active Mobiles & Players */
	log ( "Freeing Characters and mobs" );
	free_characters ( character_list );
	log ( "Free corpse list" );
	free_corpse_list ( corpse_list );
log("Clearing Names Maps");
mobNames.Clear();
objNames.Clear();
	/* Active Objects */
	log ( "Freeing Objects." );
	free_objects();
	free_pending_objects();

	log ( "Freeing forests." );
	free_forests ( forest );
	log ( "Freeing Assemblies." );
	free_assemblies();

	log ( "Free Clan Lists" );
	free_clan_lists();
	/* Rooms */
	log ( "Freeing rooms." );
#if 0

	for ( cnt = 0; ( int ) cnt <= top_of_world; cnt++ )
	{
		if ( world_vnum[cnt] == NULL )
			continue;
		free_room_strings ( world_vnum[cnt] );
		/* free any assigned scripts */
		if ( SCRIPT ( world_vnum[cnt] ) )
			extract_script ( world_vnum[cnt], WLD_TRIGGER );
		/* free script proto list */
		free_proto_script ( world_vnum[cnt], WLD_TRIGGER );
		if ( world_vnum[cnt] )
			free ( world_vnum[cnt] );
		world_vnum[cnt] = NULL;
	}
#endif

	for_each ( world_vnum.begin(), world_vnum.end(), DeleteObject() );
	top_of_world = 0;
	/* Objects */
	for ( cnt = 0; cnt <= top_of_objt; cnt++ )
	{
		free_object_strings ( obj_proto + cnt );

		if ( obj_index[cnt].qic )
			delete obj_index[cnt].qic;

		/* free script proto list */
		free_proto_script ( &obj_proto[cnt], OBJ_TRIGGER );
	}


	free ( obj_proto );
	free ( obj_index );
	//    htree_free(obj_htree);

	/* Mobiles */

	for ( map<mob_vnum, Character *>::iterator it = mob_proto.begin(); it != mob_proto.end(); it++ )
	{
		if ( it->second != NULL )
		{
			( it->second )->free_proto_mob();
			delete ( it->second );
		}
	}

	for ( map<mob_vnum, struct index_data *>::iterator it = mob_index.begin(); it != mob_index.end(); it++ )
		if ( it->second != NULL )
			delete ( it->second );


	//delete mob_proto;
	//free(mob_index);
	//htree_free();

	//    htree_free(HTREE_NULL);
	//    free(HTREE_NULL);

	/* Shops */
	destroy_shops();

	/* Zones */

#define THIS_CMD zone_table[cnt].cmd[itr]
	/* zone table reset queue */
	if ( reset_q.head )
	{
		struct reset_q_element *ftemp=reset_q.head, *temp;
		while ( ftemp )
		{
			temp = ftemp->next;
			free ( ftemp );
			ftemp = temp;
		}
	}
#if 0
	for ( cnt = 0; ( int ) cnt <= top_of_zone_table; cnt++ )
	{
		if ( zone_table[cnt].name )
			free ( zone_table[cnt].name );
		if ( zone_table[cnt].builders )
			free ( zone_table[cnt].builders );
		if ( zone_table[cnt].cmd )
		{
			/* first see if any vars were defined in this zone */
			for ( ssize_t itr = 0;THIS_CMD.command != 'S';itr++ )
				if ( THIS_CMD.command == 'V' )
				{
					if ( THIS_CMD.sarg1 )
						free ( THIS_CMD.sarg1 );
					if ( THIS_CMD.sarg2 )
						free ( THIS_CMD.sarg2 );
				}
			/* then free the command list */
			delete[] zone_table[cnt].cmd;
		}
	}
#endif
	// delete zone_table;

	for ( vector<Zone>::iterator it = zone_table.begin();it != zone_table.end();it++ )
		( *it ).Destroy();

	free_zone_list ( zone_list );
	zone_list = NULL;

	/* zone table reset queue */
	/*
	if (reset_q.head) {
	  struct reset_q_element *ftemp=reset_q.head, *temp;
	  while (ftemp) {
	    temp = ftemp->next;
	    free(ftemp);
	    ftemp = temp;
	  }
	}
	*/

#undef THIS_CMD

	/* Triggers */
	for ( cnt=0; cnt < top_of_trigt; cnt++ )
	{
		if ( trig_index[cnt]->proto )
		{
			/* make sure to nuke the command list (memory leak) */
			/* free_trigger() doesn't free the command list */
			if ( trig_index[cnt]->proto->cmdlist )
			{
				struct cmdlist_element *i, *j;
				i = trig_index[cnt]->proto->cmdlist;
				while ( i )
				{
					j = i->next;
					if ( i->cmd )
						free ( i->cmd );
					free ( i );
					i = j;
				}
			}
			free_trigger ( trig_index[cnt]->proto );
		}
		free ( trig_index[cnt] );
	}
	free ( trig_index );

	/* Events */
	event_free_all();

	/* context sensitive help system */
	free_context_help();


}



/* body of the booting system */
void boot_db ( void )
{
	zone_rnum i;

	log ( "Boot db -- BEGIN." );

	log ( "Resetting the game time:" );
	reset_time();

	log ( "Reading news, credits, help, bground, info & motds." );
	file_to_string_alloc ( NEWS_FILE, &news );
	file_to_string_alloc ( CREDITS_FILE, &credits );
	file_to_string_alloc ( MOTD_FILE, &motd );
	file_to_string_alloc ( IMOTD_FILE, &imotd );
	file_to_string_alloc ( HELP_PAGE_FILE, &help );
	file_to_string_alloc ( INFO_FILE, &info );
	file_to_string_alloc ( WIZLIST_FILE, &wizlist );
	file_to_string_alloc ( IMMLIST_FILE, &immlist );
	file_to_string_alloc ( POLICIES_FILE, &policies );
	file_to_string_alloc ( HANDBOOK_FILE, &handbook );
	file_to_string_alloc ( BACKGROUND_FILE, &background );
	if ( file_to_string_alloc ( GREETINGS_FILE, &GREETINGS ) == 0 )
		prune_crlf ( GREETINGS );

	log ( "Loading spell definitions." );
	mag_assign_spells();

	log ( "Assigning spell and skill levels." );
	init_spell_levels();

	log ( "Loading skill definitions." );
	assign_skills();

	log ( "Loading SUB-skill definitions." );
	assign_subskills();

	log ( "Sorting skills and spells." );
	sort_spells();

	log ( "Booting World." );
	boot_world();
	//    htree_test();

	log ( "Loading help entries." );
	index_boot ( DB_BOOT_HLP );

	log ( "Setting up context sensitive help system for OLC" );
	boot_context_help();

	log ( "Generating player index." );
	pi.Build();

	log ( "Booting clans." );
	init_clans();

	//removed fight message loading to move them down

	log ( "Loading social messages." );
	boot_social_messages();
	log ( "loading command list." );
	create_command_list(); /* aedit patch -- M. Scott */

	log ( "Load QIC database." );
	load_qic();

	log ( "Scanning rent files for QIC items and timing out records." );
	qic_scan_rent();

	log ( "Assigning function pointers:" );

	if ( !no_specials )
	{
		log ( "   Mobiles." );
		assign_mobiles();
		log ( "   Shopkeepers." );
		assign_the_shopkeepers();
		log ( "   Objects." );
		assign_objects();
		log ( "   Rooms." );
		assign_rooms();
		log ( "   Vehicles." );
		load_vehicles();
		//assign_vehicles();
	}

	log ( "Booting assembled objects." );
	assemblyBootAssemblies();

	log ( "Sorting command list." );
	sort_commands();

	log ( "Booting mail system." );
	if ( !scan_file() )
	{
		log ( "    Mail boot failed -- Mail system disabled" );
		no_mail = 1;
	}
	log ( "Reading banned site and invalid-name list." );
	load_banned();
	Read_Invalid_List();

	if ( !no_rent_check )
	{
		log ( "Deleting timed-out crash and rent files:" );
		update_obj_file();
		pi.CleanPFiles();
		log ( "   Done." );
	}

	/* Moved here so the object limit code works. -gg 6/24/98 */
	if ( !mini_mud )
	{
		log ( "Booting houses." );
		House_boot();
	}
	//moved here so that they show up
	log ( "Loading fight messages." );
	load_messages();

	for ( i = 0; i <= top_of_zone_table; i++ )
	{
		log ( "Resetting #%d: %s (rooms %d-%d).", zone_table[i].number, zone_table[i].name,   //mord??
		      zone_table[i].bot /*(i ? (zone_table[i - 1].top + 1) : 0) */ ,
		      zone_table[i].top );
		reset_zone ( i );
	}

	reset_q.head = reset_q.tail = NULL;



	boot_time = time ( 0 );

	log ( "Boot db -- DONE." );
}


/* reset the time in the game from file */
void reset_time ( void )
{
	time_t beginning_of_time = 0;
	ifstream bgtime ( TIME_FILE );

	if ( !bgtime.is_open() )
		log ( "SYSERR: Can't read from '%s' time file.", TIME_FILE );
	else
	{
		stringstream ss;
		ss << bgtime.rdbuf();
		ss >> beginning_of_time;
		bgtime.close();
		//fscanf(bgtime, "%ld\n", &beginning_of_time);
		//fclose(bgtime);
	}

	if ( beginning_of_time == 0 )
		beginning_of_time = 650336715;


	time_info = *mud_time_passed ( time ( 0 ), beginning_of_time );

	if ( time_info.hours <= 4 )
		sunlight = SUN_DARK;
	else if ( time_info.hours == 5 )
		sunlight = SUN_RISE;
	else if ( time_info.hours <= 20 )
		sunlight = SUN_LIGHT;
	else if ( time_info.hours == 21 )
		sunlight = SUN_SET;
	else
		sunlight = SUN_DARK;

	log ( "   Current Gametime: %dH %dD %dM %dY.", time_info.hours,
	      time_info.day, time_info.month, time_info.year );
}

void free_extra_descriptions ( struct extra_descr_data *edesc )
{
	struct extra_descr_data *enext;

	for ( ; edesc; edesc = enext )
	{
		enext = edesc->next;

		free ( edesc->keyword );
		free ( edesc->description );
		free ( edesc );
	}
}

/* Write the time in 'when' to the MUD-time file. */
void save_mud_time ( struct time_info_data *when )
{
	ofstream bgtime ( TIME_FILE );

	if ( !bgtime.is_open() )
		log ( "SYSERR: Can't write to '%s' time file.", TIME_FILE );
	else
	{
		bgtime << mud_time_to_secs ( when ) << "\n";
		bgtime.close();
		//fprintf(bgtime, "%ld\n", mud_time_to_secs(when));
		//fclose(bgtime);
	}
}



ACMD ( do_pclean )
{
	int i, j;
	char dir_name[2048];
	for ( j = CRASH_FILE; j < LOCKER_FILES;j++ )
		for ( i = 'a';i < 'z';i++ )
		{
			if ( get_dirname ( dir_name, sizeof ( dir_name ), i, j ) != NULL )
			{
				pi.clean_dir ( dir_name );
			}
		}

	for ( i = 'a';i < 'z';i++ )
	{
		snprintf ( dir_name, sizeof ( dir_name ), "%s/%c/", PLR_PREFIX, i );
		pi.clean_dir ( dir_name );
	}
}


/*
 * Thanks to Andrey (andrey@alex-ua.com) for this bit of code, although I
 * did add the 'goto' and changed some "while()" into "do { } while()".
 *   -gg 6/24/98 (technically 6/25/98, but I care not.)
 */
int count_alias_records ( FILE * fl )
{
	char key[READ_SIZE], next_key[READ_SIZE];
	char line[READ_SIZE], *scan;
	int total_keywords = 0;

	/* get the first keyword line */
	get_one_line ( fl, key );

	while ( *key != '$' )
	{
		/* skip the text */
		do
		{
			get_one_line ( fl, line );
			if ( feof ( fl ) )
				goto ackeof;
		}
		while ( *line != '#' );

		/* now count keywords */
		scan = key;
		do
		{
			scan = one_word ( scan, next_key );
			if ( *next_key )
				++total_keywords;
		}
		while ( *next_key );

		/* get next keyword line (or $) */
		get_one_line ( fl, key );

		if ( feof ( fl ) )
			goto ackeof;
	}

	return ( total_keywords );

	/* No, they are not evil. -gg 6/24/98 */
ackeof:
	log ( "SYSERR: Unexpected end of  file." );
	ALERT_1;
	exit ( 1 );            /* Some day we hope to handle these things better... */
	return ( -1 );
}
float bytesToSize ( int by )
{
	float b = by;
	if ( b < 1024 )
		return b;
	b /= 1024;
	if ( b < 1024 )
		return b;
	b /= 1024;
	return b;

}
const char *bytesToUnit ( int b )
{
	if ( b < 1024 )
		return "bytes";
	b /= 1024;
	if ( b < 1024 )
		return "kilobytes";
	b /= 1024;
	return "megabytes";
}
/* function to count how many hash-mark delimited records exist in a file */
int count_hash_records ( FILE * fl )
{
	char buf[128];
	int count = 0;

	while ( fgets ( buf, 128, fl ) )
		if ( *buf == '#' )
			count++;

	return ( count );
}

void index_boot ( int mode )
{
	const char *index_filename, *prefix = NULL;
	FILE *findex = NULL, *db_file = NULL;
	int rec_count = 0, size[2];
	char buf2[MAX_INPUT_LENGTH];
	char buf1[MAX_INPUT_LENGTH];
	Zone *zd;

	switch ( mode )
	{
		case DB_BOOT_TRG:
			prefix = TRG_SUFFIX;
			break;
		case DB_BOOT_WLD:
			prefix = WLD_SUFFIX;
			break;
		case DB_BOOT_MOB:
			prefix = MOB_SUFFIX;
			break;
		case DB_BOOT_OBJ:
			prefix = OBJ_SUFFIX;
			break;
		case DB_BOOT_ZON:
			prefix = ZON_SUFFIX;
			break;
		case DB_BOOT_SHP:
			prefix = SHP_SUFFIX;
			break;
		case DB_BOOT_HLP:
			prefix = HLP_PREFIX;
			break;
		default:
			log ( "SYSERR: Unknown subcommand %d to index_boot!", mode );
			exit ( 1 );
	}
	if ( mini_mud )
		index_filename = MINDEX_FILE;
	else
		index_filename = INDEX_FILE;
	//TODO: fix dynamic loading for minimud mode
	if ( mode == DB_BOOT_HLP )
	{


		snprintf ( buf2, sizeof ( buf2 ), "%s%s", prefix, index_filename );

		if ( ( findex = fopen ( buf2, "ro" ) ) == NULL )
		{
			log ( "SYSERR: opening index file '%s': %s", buf2, strerror ( errno ) );
			return;
		}

		/* first, count the number of records in the file so we can malloc */
		if ( fscanf ( findex, "%s\n", buf1 ) == -1 )
		{
			//      load_xml_help(prefix);
			fclose ( findex );
			return;
		}
		while ( *buf1 != '$' )
		{
			snprintf ( buf2, sizeof ( buf2 ), "%s%s", prefix, buf1 );
			log ( "%s", buf2 );
			if ( ! ( db_file = fopen ( buf2, "rb" ) ) )
			{
				log ( "SYSERR: File '%s' listed in '%s/%s': %s", buf2, prefix,
				      index_filename, strerror ( errno ) );
				fscanf ( findex, "%s\n", buf1 );
				continue;
			}
			else
				rec_count += count_alias_records ( db_file );

			fclose ( db_file );
			fscanf ( findex, "%s\n", buf1 );
		}
		/* Exit if 0 records, unless this is shops */
		if ( !rec_count )
		{
			log ( "SYSERR: boot error - 0 records counted in %s/%s.", prefix,index_filename );
			exit ( 1 );
		}
	}
	else
	{
		struct zone_list_data *temp;
		for ( temp = zone_list;temp;temp = temp->next )
		{
			snprintf ( buf2, sizeof ( buf2 ), "%s/%d%s", temp->zone, temp->num, prefix );
			if ( ! ( db_file = fopen ( buf2, "r+" ) ) )
			{
				log ( "SYSERR: File '%s' listed in '%s/%s': %s", buf2, prefix,
				      index_filename, strerror ( errno ) );
				continue;
			}
			else
			{
				if ( mode == DB_BOOT_ZON )
					rec_count++;
				else
					rec_count += count_hash_records ( db_file );
			}
			fclose ( db_file );
		}
		/* Exit if 0 records, unless this is shops */
		if ( !rec_count )
		{
			if ( mode == DB_BOOT_SHP )
				return;
			log ( "SYSERR: boot error - 0 records counted in %s/%s.", prefix,index_filename );
			exit ( 1 );
		}
	}

	/* Any idea why you put this here Jeremy?
	   rec_count++; */

	/*
	 * NOTE: "bytes" does _not_ include strings or other later malloc'd things.
	 */
	switch ( mode )
	{
		case DB_BOOT_TRG:
			CREATE ( trig_index, struct index_data *, rec_count );
			size[0] = sizeof ( struct index_data ) * rec_count;
			log ( "   %d triggers, %.2f %s.", rec_count, bytesToSize ( size[0] ), bytesToUnit ( size[0] ) );
			break;
		case DB_BOOT_WLD:
			//CREATE(world, Room, rec_count);
			size[0] = sizeof ( Room ) * rec_count;
			log ( "   %d rooms, %.2f %s.", rec_count, bytesToSize ( size[0] ), bytesToUnit ( size[0] ) );
			break;
		case DB_BOOT_MOB:
			//mob_proto.assign(rec_count, *(new Character()));
			//CREATE(mob_proto, Character, rec_count);
			//        CREATE(mob_index, struct index_data, rec_count);
			size[0] = sizeof ( struct index_data ) * rec_count;
			size[1] = sizeof ( Character ) * rec_count;
			log ( "   %d mobs, %.2f %s in index, %.2f %s in prototypes.",
			      rec_count, bytesToSize ( size[0] ), bytesToUnit ( size[0] ), bytesToSize ( size[1] ), bytesToUnit ( size[1] ) );
			break;
		case DB_BOOT_OBJ:
			CREATE ( obj_proto, struct obj_data, rec_count );
			CREATE ( obj_index, struct index_data, rec_count );
			size[0] = sizeof ( struct index_data ) * rec_count;
			size[1] = sizeof ( struct obj_data ) * rec_count;
			log ( "   %d objs, %.2f %s in index, %.2f %s in prototypes.",
			      rec_count, bytesToSize ( size[0] ), bytesToUnit ( size[0] ), bytesToSize ( size[1] ), bytesToUnit ( size[1] ) );
			break;
		case DB_BOOT_ZON:
			//CREATE(zone_table, Zone, rec_count);
			zd = new Zone();
			zone_table.assign ( rec_count, *zd );
			delete zd;
			size[0] = sizeof ( Zone ) * rec_count;
			log ( "   %d zones,  %.2f %s.", rec_count, bytesToSize ( size[0] ), bytesToUnit ( size[0] ) );
			break;
		case DB_BOOT_HLP:
			CREATE ( help_table, struct help_index_element, rec_count );
			size[0] = sizeof ( struct help_index_element ) * rec_count;
			log ( "   %d entries, %.2f %s.", rec_count, bytesToSize ( size[0] ), bytesToUnit ( size[0] ) );
			break;
	}
	if ( mode == DB_BOOT_HLP )
	{
		rewind ( findex );
		fscanf ( findex, "%s\n", buf1 );
		while ( *buf1 != '$' )
		{
			snprintf ( buf2, sizeof ( buf2 ), "%s%s", prefix, buf1 );
			if ( ! ( db_file = fopen ( buf2, "r" ) ) )
			{
				log ( "SYSERR: %s: %s", buf2, strerror ( errno ) );
				exit ( 1 );
			}
			load_help ( db_file );
			fclose ( db_file );
			fscanf ( findex, "%s\n", buf1 );

		}
		fclose ( findex );

	}
	else
	{
		struct zone_list_data *temp;
		//start
		for ( temp=zone_list;temp;temp = temp->next )
		{
			snprintf ( buf2, sizeof ( buf2 ), "%s/%d%s", temp->zone, temp->num, prefix );
			if ( ! ( db_file = fopen ( buf2, "r" ) ) )
			{
				log ( "SYSERR: %s: %s", buf2, strerror ( errno ) );
				exit ( 1 );
			}
			switch ( mode )
			{
				case DB_BOOT_TRG:
				case DB_BOOT_WLD:
				case DB_BOOT_OBJ:
				case DB_BOOT_MOB:
					discrete_load ( db_file, mode, buf2, temp->num );
					break;
				case DB_BOOT_ZON:
					load_zones ( db_file, buf2 );
					break;
				case DB_BOOT_SHP:
					boot_the_shops ( db_file, buf2, rec_count );
					break;
			}

			fclose ( db_file );
		}
		//end
	}
}


void discrete_load ( FILE * fl, int mode, char *filename, zone_vnum zon )
{
	int nr = -1, last = 0, version = 1;
	char line[READ_SIZE];

	const char *modes[] = { "world", "mob", "obj", "ZON", "SHP", "HLP", "trg"
	                      };
	/* modes positions correspond to DB_BOOT_xxx in db.h */

	// log("INFO: Reading from file %s.", filename);

	for ( ;; )
	{
		/*
		 * we have to do special processing with the obj files because they have
		 * no end-of-record marker :(
		 */
		if ( mode != DB_BOOT_OBJ || nr < 0 )
			if ( !get_line ( fl, line ) )
			{
				if ( nr == -1 )
				{
					log ( "SYSERR: %s file %s is empty!", modes[mode],
					      filename );
				}
				else
				{
					log ( "SYSERR: Format error in %s after %s #%d\n"
					      "...expecting a new %s, but file ended!\n"
					      "(maybe the file is not terminated with '$'?)",
					      filename, modes[mode], nr, modes[mode] );
				}
				exit ( 1 );
			}
		if ( *line == '$' )
			return;

		else if ( *line == '@' )
		{
			if ( sscanf ( line, "@Version: %d", &version ) != 1 )
			{
				log ( "SYSERR: Format error after %s #%d", modes[mode],
				      last );
				log ( "SYSERR: ...Line: %s", line );
				exit ( 1 );
			}
		}
		if ( *line == '#' )
		{
			last = nr;
			if ( sscanf ( line, "#%d", &nr ) != 1 )
			{
				log ( "SYSERR: Format error after %s #%d", modes[mode],
				      last );
				exit ( 1 );
			}
			if ( nr >= HIGHEST_VNUM )
			{
				log ( "SYSERR: Vnum too high (max %d) in %s file %s near %s #%d",
				      HIGHEST_VNUM, modes[mode], filename, modes[mode], nr );
				return;
			}
			else
				switch ( mode )
				{
					case DB_BOOT_TRG:
						parse_trigger ( fl, nr, zon );
						break;
					case DB_BOOT_WLD:
						parse_room ( fl, nr, zon );
						break;
					case DB_BOOT_MOB:
						parse_mobile ( fl, nr, zon );
						break;
					case DB_BOOT_OBJ:
						strcpy ( line, parse_object ( fl, nr, zon ) );
						break;
				}
		}
		else
		{
			log ( "SYSERR: Format error in %s file %s near %s #%d",
			      modes[mode], filename, modes[mode], nr );
			log ( "SYSERR: ... offending line: '%s'", line );
			exit ( 1 );
		}
	}
}

bitvector_t asciiflag_conv ( char *flag )
{
	bitvector_t flags = 0;
	int num_true = TRUE;
	register char *p;

	for ( p = flag; *p; p++ )
	{
		if ( islower ( *p ) )
			flags |= 1 << ( *p - 'a' );
		else if ( isupper ( *p ) )
			flags |= 1 << ( 26 + ( *p - 'A' ) );

		if ( !isdigit ( *p ) )
			num_true = FALSE;
	}

	if ( num_true )
		flags = atol ( flag );

	return ( flags );
}

char fread_letter ( FILE * fp )
{
	char c;
	do
	{
		c = getc ( fp );
	}
	while ( isspace ( c ) );
	return c;
}

/* load the rooms */
void parse_room ( FILE * fl, int virtual_nr, zone_vnum zon )
{
	static Room * room_nr = NULL;
	zone_rnum zone = 0;
	int t[10], i;
	char line[256], flags[128], flags2[128], flags3[128], flags4[128];
	struct extra_descr_data *new_descr;
        struct q_descr_data *new_q_descr;
	struct forest_data *new_forest = NULL;
	char buf2[MAX_INPUT_LENGTH];
	char letter;

	if ( virtual_nr >= HIGHEST_VNUM )
	{
		log ( "Please increase HIGHEST_VNUM, or keep zone numbers below %d", HIGHEST_VNUM );
		exit ( 1 );
	}

	//log("Zone Load: %d - %d (%d)", zone, real_zone(zon), zon);
	if ( ( zone = real_zone ( zon ) ) == NOWHERE )
	{
		log ( "Real zone for zone %d - is nowhere", zon );
		exit ( 1 );
	}


	snprintf ( buf2, sizeof ( buf2 ), "#room %d", virtual_nr );//for fread_string

	if ( virtual_nr < zone_table[zone].bot )
	{
		log ( "SYSERR: Room #%d is below zone %d's bottom %d.", virtual_nr, zone_table[zone].number,zone_table[zone].bot );
		exit ( 1 );
	}                 //mord??
	while ( virtual_nr > zone_table[zone].top )
	{
		if ( ( ++zone ) > top_of_zone_table )
		{
			log ( "SYSERR: Room %d is outside of any zone.", virtual_nr );
			exit ( 1 );
		}
	}
	if ( world_vnum[virtual_nr] != NULL )
	{
		log ( "Room: %d somehow already has been made when it is loading now: %s", virtual_nr,world_vnum[virtual_nr]->name );
		delete world_vnum[virtual_nr];
		world_vnum[virtual_nr] = NULL;
	}
	room_nr=new Room();
	room_nr->zone = real_zone ( zon );
	room_nr->number = virtual_nr;
	if ( ( room_nr->name = fread_string ( fl, buf2 ) ) == NULL )
		room_nr->name = strdup ( "Undefined" );
	if ( ( room_nr->t_description = fread_string ( fl, buf2 ) ) == NULL )
		room_nr->SetDescription ( "Undefined" );
	else
		room_nr->AssignTempDesc();

        room_nr->t_description = NULL;
	if ( ( room_nr->smell = fread_string ( fl, buf2 ) ) == NULL )
		room_nr->smell  = strdup ( "Undefined" );
	if ( ( room_nr->listen = fread_string ( fl, buf2 ) ) == NULL )
		room_nr->listen  = strdup ( "Undefined" );
	room_nr->proto_script = NULL;
	room_nr->script = NULL;
	room_nr->affects = NULL;
	room_nr->mine.num = -1;
	room_nr->mine.dif = -1;
	world_vnum[virtual_nr]=room_nr;

	if ( !get_line ( fl, line ) )
	{
		log ( "SYSERR: Expecting roomflags/sector type of room #%d but file ended!", virtual_nr );
		exit ( 1 );
	}

	if ( sscanf ( line, " %d %s %s %s %s %d ", t, flags, flags2, flags3, flags4, t + 2 ) != 6 )
	{
		log ( "SYSERR: Format error in roomflags/sector type of room #%d", virtual_nr );
		exit ( 1 );
	}
	/* t[0] is the zone number; ignored with the zone-file system */
	room_nr->room_flags[0] = asciiflag_conv ( flags );
	room_nr->room_flags[1] = asciiflag_conv ( flags2 );
	room_nr->room_flags[2] = asciiflag_conv ( flags3 );
	room_nr->room_flags[3] = asciiflag_conv ( flags4 );
	room_nr->sector_type = t[2];

	REMOVE_BIT_AR ( ROOM_FLAGS ( room_nr ), ROOM_BFS_MARK );

	room_nr->func = NULL;
	room_nr->contents = NULL;
	room_nr->people = NULL;
	room_nr->light = 0;    /* Zero light sources */

	if ( ROOM_FLAGGED ( room_nr, ROOM_PETSHOP ) )
		ASSIGNROOM ( virtual_nr, pet_shops );

	for ( i = 0; i < NUM_OF_DIRS; i++ )
		room_nr->dir_option[i] = NULL;

	room_nr->ex_description = NULL;
	room_nr->look_above_description = NULL;
	room_nr->look_behind_description = NULL;
	room_nr->look_under_description = NULL;
        room_nr->n_description = NULL;
        room_nr->q_description = NULL;

	for ( ;; )
	{
		if ( !get_line ( fl, line ) )
		{
			log ( "SYSERR: Format error in room #%d (expecting D/E/S)", virtual_nr );
			exit ( 1 );
		}
		switch ( *line )
		{
                    case 'N':
                        if ((room_nr->n_description = fread_string(fl, buf2)) == NULL)
                        room_nr->n_description = NULL;
                    break;
		    case 'D':
		        setup_dir ( fl, room_nr, atoi ( line + 1 ) );
	    	    break;
                    case 'Q':
                        CREATE(new_q_descr, struct q_descr_data, 1);
                        new_q_descr->type = fread_number(fl);
                        if ((new_q_descr->flag = fread_string(fl, buf2)) == NULL)
                            new_q_descr->flag = strdup("Undefined");
                        if ((new_q_descr->description = fread_string(fl, buf2)) == NULL)
                            new_q_descr->description = strdup("Undefined");
                        if (room_nr->q_description) 
                            new_q_descr->next = room_nr->q_description;
                        room_nr->q_description = new_q_descr;
                    break;    
		    case 'E':
		        CREATE ( new_descr, struct extra_descr_data, 1 );
			if ( ( new_descr->keyword = fread_string ( fl, buf2 ) ) == NULL )
		    	    new_descr->keyword  = strdup ( "Undefined" );
			    if ( ( new_descr->description = fread_string ( fl, buf2 ) ) == NULL )
				new_descr->description = strdup ( "Undefined" );
				/* fix for crashes in the editor when formatting
				 * - e-descs are assumed to end with a \r\n
				 * -- Welcor 09/03
				 */
				if ( new_descr->description )
				{
					char *end = strchr ( new_descr->description, '\0' );
					if ( end > new_descr->description && * ( end-1 ) != '\n' )
					{
						CREATE ( end, char, strlen ( new_descr->description ) +3 );
						sprintf ( end, "%s\r\n", new_descr->description ); /* snprintf ok : size checked above*/
						free ( new_descr->description );
						new_descr->description = end;
					}
				}
				else
				{
					if ( new_descr->keyword )
						log ( "SYSERR: Format error in description '%s', room %d", new_descr->keyword, virtual_nr );
					else
						log ( "SYSERR: Format error in description room %d", virtual_nr );

					new_descr->description = strdup ( "Blank\r\n" );
				}
				new_descr->next = room_nr->ex_description;
				room_nr->ex_description = new_descr;
				break;
			case 'A':
				CREATE ( new_descr, struct extra_descr_data, 1 );
				if ( ( new_descr->keyword = fread_string ( fl, buf2 ) ) == NULL )
					new_descr->keyword  = strdup ( "Undefined" );
				if ( ( new_descr->description = fread_string ( fl, buf2 ) ) == NULL )
					new_descr->description = strdup ( "Undefined" );
				/* fix for crashes in the editor when formatting
				 * - e-descs are assumed to end with a \r\n
				 * -- Welcor 09/03
				 */
				{
					char *j = strchr ( new_descr->description, '\0' );
					if ( j > new_descr->description && * ( j-1 ) != '\n' )
					{
						CREATE ( j, char, strlen ( new_descr->description ) +3 );
						sprintf ( j, "%s\r\n", new_descr->description ); /* sprintf ok : size checked above*/
						free ( new_descr->description );
						new_descr->description = j;
					}
				}
				new_descr->next = room_nr->look_above_description;
				room_nr->look_above_description = new_descr;
				break;
			case 'B':
				CREATE ( new_descr, struct extra_descr_data, 1 );
				if ( ( new_descr->keyword = fread_string ( fl, buf2 ) ) == NULL )
					new_descr->keyword  = strdup ( "Undefined" );
				if ( ( new_descr->description = fread_string ( fl, buf2 ) ) == NULL )
					new_descr->description = strdup ( "Undefined" );
				/* fix for crashes in the editor when formatting
				 * - e-descs are assumed to end with a \r\n
				 * -- Welcor 09/03
				 */
				{
					char *j = strchr ( new_descr->description, '\0' );
					if ( j > new_descr->description && * ( j-1 ) != '\n' )
					{
						CREATE ( j, char, strlen ( new_descr->description ) +3 );
						sprintf ( j, "%s\r\n", new_descr->description ); /* sprintf ok : size checked above*/
						free ( new_descr->description );
						new_descr->description = j;
					}
				}
				new_descr->next = room_nr->look_behind_description;
				room_nr->look_behind_description = new_descr;
				break;
			case 'U':
				CREATE ( new_descr, struct extra_descr_data, 1 );
				if ( ( new_descr->keyword = fread_string ( fl, buf2 ) ) == NULL )
					new_descr->keyword  = strdup ( "Undefined" );
				if ( ( new_descr->description = fread_string ( fl, buf2 ) ) == NULL )
					new_descr->description = strdup ( "Undefined" );
				/* fix for crashes in the editor when formatting
				 * - e-descs are assumed to end with a \r\n
				 * -- Welcor 09/03
				 */
				{
					char *j = strchr ( new_descr->description, '\0' );
					if ( j > new_descr->description && * ( j-1 ) != '\n' )
					{
						CREATE ( j, char, strlen ( new_descr->description ) +3 );
						sprintf ( j, "%s\r\n", new_descr->description ); /* sprintf ok : size checked above*/
						free ( new_descr->description );
						new_descr->description = j;
					}
				}
				new_descr->next = room_nr->look_under_description;
				room_nr->look_under_description = new_descr;
				break;
			case 'M':
				room_nr->mine.num = fread_number ( fl );
				room_nr->mine.dif = fread_number ( fl );
				room_nr->mine.tool = fread_number ( fl );
				break;
			case 'S':       /* end of room */
				/* DG triggers -- script is defined after the end of the room */
				letter = fread_letter ( fl );
				ungetc ( letter, fl );
				while ( letter == 'T' )
				{
					dg_read_trigger ( fl, room_nr, WLD_TRIGGER );
					letter = fread_letter ( fl );
					ungetc ( letter, fl );
				}
				if ( virtual_nr > top_of_world )
					top_of_world = virtual_nr;
				if ( SECT ( room_nr ) == SECT_FOREST )
				{
					CREATE ( new_forest, struct forest_data, 1 );
					new_forest->room = room_nr;
					new_forest->next = forest;
					forest = new_forest;
					forest_room++;
				}
				add_room_to_mine ( room_nr );
				return;
			default:
				log ( "SYSERR: Format error in room #%d (expecting D/E/S/B/U/A)", virtual_nr );
				exit ( 1 );
		}
	}
}

void free_forests ( struct forest_data *this_forest )
{
	if ( !this_forest )
		return;

	if ( this_forest->next )
		free_forests ( this_forest->next );

	free ( this_forest );
}

/* read direction data */
void setup_dir ( FILE * fl, room_rnum room, int dir )
{
	int t[5];
	char line[256];
	char buf2[MAX_INPUT_LENGTH];

	sprintf ( buf2,"room #%d, direction D%d",room->number,dir );
	if ( dir < 0 || dir >= NUM_OF_DIRS )
	{
		char *di;
		log ( "Doors are only in the range of 0 to %d, this door isn't: %s\r\n(gulping door)", NUM_OF_DIRS, buf2 );
		di = fread_string ( fl, buf2 );
		if ( di )
			free_string ( &di );
		di = fread_string ( fl, buf2 );
		if ( di )
			free_string ( &di );
		if ( !get_line ( fl, line ) )
		{
			log ( "SYSERR: Format error, %s", buf2 );
			exit ( 1 );
		}
		return;
	}
	else
	{
		if ( room->dir_option[dir] == NULL )
		{
			CREATE ( room->dir_option[dir], struct room_direction_data, 1 );
			room->dir_option[dir]->general_description = NULL;
			room->dir_option[dir]->keyword = NULL;
		}
		else
		{
			free_string ( &room->dir_option[dir]->keyword );
			free_string ( &room->dir_option[dir]->general_description );
		}
		if ( ( room->dir_option[dir]->general_description = fread_string ( fl, buf2 ) ) == NULL )
		{
			/*room->dir_option[dir]->general_description = strdup("Undefined")*/
		}
		else if ( !strncmp ( room->dir_option[dir]->general_description , "Undefined", 9 ) )
		{
			free_string ( &room->dir_option[dir]->general_description );
		}
		if ( ( room->dir_option[dir]->keyword = fread_string ( fl, buf2 ) ) == NULL )
		{
			/*room->dir_option[dir]->keyword = strdup("Undefined");*/
		}
		else if ( !strncmp ( room->dir_option[dir]->keyword , "Undefined", 9 ) )
		{
			free_string ( &room->dir_option[dir]->keyword );
		}
		room->dir_option[dir]->nosave = 0;
	}

	// log("%s, %s.", world[room].dir_option[dir]->general_description, world[room].dir_option[dir]->keyword);

	if ( !get_line ( fl, line ) )
	{
		log ( "SYSERR: Format error, %s", buf2 );
		exit ( 1 );
	}
	if ( sscanf ( line, " %d %d %d ", t, t + 1, t + 2 ) != 3 )
	{
		log ( "SYSERR: Format error, %s", buf2 );
		exit ( 1 );
	}
	if ( t[0] == 1 )
		room->dir_option[dir]->exit_info = EX_ISDOOR;
	else if ( t[0] == 2 )
		room->dir_option[dir]->exit_info = EX_ISDOOR | EX_PICKPROOF;
	else
		room->dir_option[dir]->exit_info = 0;

	room->dir_option[dir]->key = ( ( t[1] == -1 || t[1] == 65535 ) ? NOTHING : t[1] );

	/* added check for rooms to 0 to point to nowhere - mord*/
	room->dir_option[dir]->to_room_tmp = ( ( t[2] == -1  || t[2] == 0 ) ? NOWHERE : t[2] );
}


/* make sure the start rooms exist & resolve their vnums to rnums */
/*void check_start_rooms(void)
{
  if ((CONFIG_MORTAL_START = real_room(CONFIG_MORTAL_START)) == NULL)
  {
    log("SYSERR:  Mortal start room does not exist.  Change in config.c.");
    exit(1);
  }
  if ((CONFIG_IMMORTAL_START = real_room(CONFIG_IMMORTAL_START)) == NOWHERE)
  {
    if (!mini_mud)
      log("SYSERR:  Warning: Immort start room does not exist.  Change in config.c.");
    CONFIG_IMMORTAL_START = CONFIG_MORTAL_START;
  }
  if ((CONFIG_FROZEN_START = real_room(CONFIG_FROZEN_START)) == NOWHERE)
  {
    if (!mini_mud)
      log("SYSERR:  Warning: Frozen start room does not exist.  Change in config.c.");
    CONFIG_FROZEN_START = CONFIG_MORTAL_START;
  }
}*/


/* resolve all vnums into rnums in the world */
void renum_world ( void )
{
	register int door;
	room_vnum room;

	for ( room = 0; room <= top_of_world; room++ )
		for ( door = 0; door < NUM_OF_DIRS; door++ )
			if ( world_vnum[room] != NULL )
				if ( world_vnum[room]->dir_option[door] )
					if ( world_vnum[room]->dir_option[door]->to_room_tmp != NOWHERE )
						world_vnum[room]->dir_option[door]->to_room =
						    real_room ( world_vnum[room]->dir_option[door]->to_room_tmp );
}


#define ZCMD zone_table[zone].cmd[cmd_no]

/*
 * "resulve vnums into rnums in the zone reset tables"
 *
 * Or in English: Once all of the zone reset tables have been loaded, we
 * resolve the virtual numbers into real numbers all at once so we don't have
 * to do it repeatedly while the game is running.  This does make adding any
 * room, mobile, or object a little more difficult while the game is running.
 *
 * NOTE 1: Assumes NOWHERE == NOBODY == NOTHING.
 * NOTE 2: Assumes sizeof(room_rnum) >= (sizeof(mob_rnum) and sizeof(obj_rnum))
 */
void renum_zone_table ( void )
{
	int cmd_no;
	int a, b, c, olda, oldb, oldc;
	zone_rnum zone;
	char buf[128];

	for ( zone = 0; zone <= top_of_zone_table; zone++ )
		for ( cmd_no = 0; ZCMD.command != 'S'; cmd_no++ )
		{
			a = b = c = 0;
			olda = ZCMD.arg1;
			oldb = ZCMD.arg2;
			oldc = ZCMD.arg3;
			switch ( ZCMD.command )
			{
				case 'M':
					a = ZCMD.arg1;
					c = ZCMD.arg3;
					break;
				case 'O':
					a = ZCMD.arg1 = real_object ( ZCMD.arg1 );
					if ( ZCMD.arg3 != NOWHERE )
						c = ZCMD.arg3;
					break;
				case 'G':
					a = ZCMD.arg1 = real_object ( ZCMD.arg1 );
					break;
				case 'E':
					a = ZCMD.arg1 = real_object ( ZCMD.arg1 );
					break;
				case 'P':
					a = ZCMD.arg1 = real_object ( ZCMD.arg1 );
					c = ZCMD.arg3 = real_object ( ZCMD.arg3 );
					break;
				case 'D':
					a = ZCMD.arg1;
					break;
				case 'R':          /* rem obj from room */
					a = ZCMD.arg1;
					b = ZCMD.arg2 = real_object ( ZCMD.arg2 );
					break;
				case 'T':          /* a trigger */
					b = ZCMD.arg2 = real_trigger ( ZCMD.arg2 );  // added by mord
					c = ZCMD.arg3;   // added by mord
					break;
				case 'V':          /* trigger variable assignment */
					b = ZCMD.arg3;   //added by mord
					break;
				case 'B':
					a = ZCMD.arg1 = real_object ( ZCMD.arg1 );
					if ( ZCMD.arg3 != NOWHERE )
						c = ZCMD.arg3 = real_object ( ZCMD.arg3 );
					break;
			}
			if ( a == NOWHERE || b == NOWHERE || c == NOWHERE )
			{
				if ( !mini_mud )
				{
					snprintf ( buf, sizeof ( buf ),
					           "Invalid vnum %d, cmd disabled",
					           a == NOWHERE ? olda : b ==
					           NOWHERE ? oldb : oldc );
					log_zone_error ( zone, cmd_no, buf );
				}
				ZCMD.command = '*';
			}
			else if ( ZCMD.arg3 < 0 )
				ZCMD.arg3 = 0;
			else if ( ZCMD.arg4 < 0 )
				ZCMD.arg4 = 0;
		}
}



void parse_simple_mob ( FILE * mob_f, Character *mob, int nr )
{
	int j, t[10];
	char line[256];
	int k = 1;
	//mob_proto[i].real_abils.str = (number(3, 20));
         mob->real_abils.intel = ( number ( 3, 20 ) );
 	 mob->real_abils.wis = ( number ( 3, 20 ) );
	 mob->real_abils.dex = ( number ( 3, 20 ) );
	 mob->real_abils.con = ( number ( 3, 20 ) );
	 mob->real_abils.cha = ( number ( 3, 20 ) );

	if ( !get_line ( mob_f, line ) )
	{
		log ( "SYSERR: Format error in mob #%d, file ended after S flag!",
		      nr );
		exit ( 1 );
	}

	if ( sscanf ( line, " %d %d %d %dd%d+%d %dd%d+%d ",
	              t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6, t + 7,
	              t + 8 ) != 9 )
	{
		log ( "SYSERR: Format error in mob #%d, first line after S flag\n"
		      "...expecting line of form '# # # #d#+# #d#+#'", nr );
		exit ( 1 );
	}

	 GET_LEVEL ( mob ) = k = IRANGE ( 1, t[0], MAX_MOB_LEVELS -1 ) ;
	 mob->points.hitroll = mob_stats[k].hitroll;
	 mob->points.armor = mob_stats[k].ac;

	// For when we are going to change this, this stuff is also done after a medit, in medit_save_internally.
	// if you change it here, change it there too.
	/* max hit = 0 is a flag that H, M, V is xdy+z */
	 mob->points.max_hit = ( dice ( mob_stats[k].hp_dice, mob_stats[k].hp_sides ) + mob_stats[k].hp_bonus );
	 mob->points.hit = mob->points.max_hit;
	 mob->points.mana = 10;
	 mob->points.move = 5000;
	 mob->points.stamina = 150 + ( k*2 );
	 mob->points.max_mana = 10;
	 mob->points.max_move = 5000;
	 mob->points.max_stamina = 100;
	
	mob->mob_specials.damnodice = mob_stats[k].dam_dice;
	 mob->mob_specials.damsizedice = mob_stats[k].dam_sides;
	mob->points.damroll = mob_stats[k].dam_bonus;
	mob->mob_specials.tier = 0;
	mob->mob_specials.subskill = TYPE_UNDEFINED;
	mob->mob_specials.join_list = NULL;
	mob->mob_specials.head_join = NULL;
	mob->mob_specials.damage_taken = 0;
	mob->mob_specials.dam_list = NULL;


	if ( !get_line ( mob_f, line ) )
	{
		log ( "SYSERR: Format error in mob #%d, second line after S flag\n"
		      "...expecting line of form '# #', but file ended!", nr );
		exit ( 1 );
	}

	if ( sscanf ( line, " %d %d ", t, t + 1 ) != 2 )
	{
		log ( "SYSERR: Format error in mob #%d, second line after S flag\n"
		      "...expecting line of form '# #'", nr );
		exit ( 1 );
	}


	GET_EXP ( mob ) = mob_stats[k].exp;
	GET_GROUP_EXP ( mob ) = 0;

	GET_PERC ( mob ) = 100;

	if ( !get_line ( mob_f, line ) )
	{
		log ( "SYSERR: Format error in last line of mob #%d\n"
		      "...expecting line of form '# # #', but file ended!", nr );
		exit ( 1 );
	}

	if ( sscanf ( line, " %d %d %d %d ", t, t + 1, t + 2, t + 3 ) != 4 )
	{
		log ( "SYSERR: Format error in last line of mob #%d\n"
		      "...expecting line of form '# # # #'", nr );
		exit ( 1 );
	}

	mob->char_specials.position = t[0];
	mob->mob_specials.default_pos = t[1];
	mob->player.sex = t[2];
	if ( ( mob->mob_specials.race = t[3] ) != MOB_RACE_ANIMAL || ( mob->mob_specials.race = t[3] ) != MOB_RACE_EXOTIC )
		GET_GOLD ( mob ) = ( t[0] ? mob_stats[k].gold : 0 );

	mob->player.chclass = 0;
	mob->player.weight = 200;
	mob->player.height = 198;
	mob->real_abils.str = 25;
	mob->real_abils.str_add = 100;

	/*
	 * these are now save applies; base save numbers for MOBs are now from
	 * the warrior save table.
	 */
	for ( j = 0; j < 5; j++ )
		GET_SAVE ( mob, j ) = 0;
}


/*
 * interpret_espec is the function that takes espec keywords and values
 * and assigns the correct value to the mob as appropriate.  Adding new
 * e-specs is absurdly easy -- just add a new CASE statement to this
 * function!  No other changes need to be made anywhere in the code.
 *
 * CASE        : Requires a parameter through 'value'.
 * BOOL_CASE   : Being specified at all is its value.
 */

#define CASE(test)  \
     if (value && !matched && !strcmp(keyword, test) && (matched = TRUE))

#define BOOL_CASE(test)  \
     if (!value && !matched && !strcmp(keyword, test) && (matched = TRUE))

#define RANGE(low, high) \
     (num_arg = MAX((low), MIN((high), (num_arg))))

void interpret_espec ( const char *keyword, const char *value, Character *mob, int nr )
{
	int num_arg = 0;
	bool matched = FALSE;
	/*
	 * If there isn't a colon, there is no value.  While Boolean options are
	 * possible, we don't actually have any.  Feel free to make some.
	 */
	if ( value )
		num_arg = atoi ( value );

	CASE ( "BareHandAttack" )
	{
		RANGE ( 0, 99 );
		mob->mob_specials.attack_type = num_arg;
	}

	CASE ( "Str" )
	{
		RANGE ( 3, 25 );
		mob->real_abils.str = num_arg;
	}

	CASE ( "StrAdd" )
	{
		RANGE ( 0, 100 );
		mob->real_abils.str_add = num_arg;
	}

	CASE ( "Int" )
	{
		RANGE ( 3, 25 );
		mob->real_abils.intel = num_arg;
	}

	CASE ( "Wis" )
	{
		RANGE ( 3, 25 );
		mob->real_abils.wis = num_arg;
	}

	CASE ( "Dex" )
	{
		RANGE ( 3, 25 );
		mob->real_abils.dex = num_arg;
	}

	CASE ( "Con" )
	{
		RANGE ( 3, 25 );
		mob->real_abils.con = num_arg;
	}

	CASE ( "Cha" )
	{
		RANGE ( 3, 25 );
		mob->real_abils.cha = num_arg;
	}

	CASE ( "Class" )
	{
		RANGE ( 0, NUM_MOB_CLASSES-1 );
		mob->player.chclass = num_arg;
	}

	CASE ( "Tier" )
	{
		RANGE ( 0, 4 );
		mob->mob_specials.tier = num_arg;
	}

	CASE ( "Subskill" )
	{
		RANGE ( -1, ( int ) TOP_SUB_DEFINE );
		mob->mob_specials.subskill = num_arg;
	}
	CASE ( "Skin" )
	{
		RANGE ( -1, 999999 );
		mob->mob_specials.skin = num_arg;
	}
	CASE ( "Owner" )
	{
		RANGE ( -1, ( int ) pi.TopIdNum );
		mob->mob_specials.skin = num_arg;
	}
	CASE ( "Leave" )
	{
	    GET_CUSTOM_LEAVE_MSG(mob) = strdup(value);
	}

	CASE ( "Arrive" )
	{
	    GET_CUSTOM_ARRIVE_MSG(mob) = strdup(value);
	}

	if ( !matched )
	{
		log ( "SYSERR: Warning: unrecognized espec keyword %s in mob #%d",
		      keyword, nr );
	}
}

#undef CASE
#undef BOOL_CASE
#undef RANGE

void parse_espec ( char *buf, Character *mob, int nr )
{
	char *ptr;

	if ( ( ptr = strchr ( buf, ':' ) ) != NULL )
	{
		* ( ptr++ ) = '\0';
		while ( isspace ( *ptr ) )
			ptr++;
	}
	interpret_espec ( buf, ptr, mob, nr );
}
void parse_trainer_skills ( char *buf, Character *mob, int nr )
{
	int v = spell_num ( buf );

	if ( v != TYPE_UNDEFINED && !can_teach_skill(mob, v)) 
		mob->mob_specials.teaches_skills.push_back ( v );
}

void parse_trainer_mob ( FILE * mob_f, Character *mob, int nr )
{
	char line[READ_SIZE];

	while ( get_line ( mob_f, line ) )
	{
		if ( !strcmp ( line, "H*END" ) )   /* end of the enhanced section */
			return;
		else if ( *line == '#' )     /* we've hit the next mob, maybe? */
		{
			log ( "SYSERR: Unterminated H (Training) section in mob #%d", nr );
			exit ( 1 );
		}
		else
			parse_trainer_skills ( line, mob, nr );
	}

	log ( "SYSERR: Unexpected end of file reached after mob #%d", nr );
	exit ( 1 );
}


void parse_enhanced_mob ( FILE * mob_f, Character *mob, int nr )
{
	char line[READ_SIZE];

	parse_simple_mob ( mob_f, mob, nr );

	while ( get_line ( mob_f, line ) )
	{
		if ( !strcmp ( line, "E" ) )   /* end of the enhanced section */
			return;
		else if ( *line == '#' )     /* we've hit the next mob, maybe? */
		{
			log ( "SYSERR: Unterminated E section in mob #%d", nr );
			exit ( 1 );
		}
		else
			parse_espec ( line, mob, nr );
	}

	log ( "SYSERR: Unexpected end of file reached after mob #%d", nr );
	exit ( 1 );
}

struct combine_data *add_base_link ( Character *mob, int vnum )
{
	struct combine_data *blink = NULL;
	CREATE ( blink, struct combine_data, 1 );
	blink->vnum = vnum;
	blink->joined = NULL;
	blink->next = mob->mob_specials.join_list;
	mob->mob_specials.join_list = blink;

	return blink;

}
struct combine_data *add_base_link_mob ( Character *mob, mob_vnum vnum )
{
	struct combine_data *blink = NULL;
	CREATE ( blink, struct combine_data, 1 );
	blink->vnum = vnum;
	blink->joined = NULL;
	blink->next = mob->mob_specials.join_list;
	mob->mob_specials.join_list = blink;
	return blink;

}
/* adds to current returns next
   Assumes all links pre created
   Gives new segment its head_join
*/
struct combine_data *add_full_link ( Character *mob, struct combine_data *current, Character *segment )
{

	if ( segment && mob )
	{
		if ( GET_MOB_VNUM ( segment ) == GET_MOB_VNUM ( mob ) )
		{
			log ( "ERROR: mob vnum %d is a segment of itself!", GET_MOB_VNUM ( mob ) );
			return current->next;
		}
		current->joined = segment;
		segment->mob_specials.head_join = mob;
		SET_BIT_AR ( MOB_FLAGS ( segment ), MOB_SENTINEL );
		GET_EXP ( mob ) += ( int ) ( GET_EXP ( segment ) * 0.3 );
	}
	return current->next;
}

struct combine_data *copy_proto_link ( struct combine_data *proto )
{
	struct combine_data *temp = NULL, *temp2 = NULL;

	if ( proto == NULL )
		return NULL;

	if ( proto->next != NULL )
		temp2 = copy_proto_link ( proto->next );

	CREATE ( temp, struct combine_data, 1 );

	temp->vnum = proto->vnum;
	temp->joined = proto->joined;
	temp->next = temp2;

	return temp;
}

void load_links ( Character *mob )
{
	struct combine_data *temp = NULL;
	if ( ( temp = mob->mob_specials.join_list ) == NULL )
		return;

	while ( temp )
		temp = add_full_link ( mob, temp, read_mobile ( temp->vnum ) );

}

int move_link_room ( Character *mob, room_rnum room )
{
	struct combine_data *temp = NULL;
	if ( mob->mob_specials.head_join != NULL )
		return 0;
	if ( ( temp = mob->mob_specials.join_list ) == NULL )
		return 1;

	while ( temp != NULL )
	{
		if ( temp->joined )
		{
			if ( IN_ROOM ( temp->joined ) != NULL )
				char_from_room ( temp->joined );

			char_to_room ( temp->joined, room );
		}

		temp = temp->next;
	}
	return 1;
}

void die_link ( Character *mob )
{
	struct combine_data *temp = NULL, *blink = NULL;
	Character *head;
	if ( mob == NULL )
		return;
	if ( ( head = mob->mob_specials.head_join ) == NULL )
		return;
	blink = head->mob_specials.join_list;
	while ( blink )
	{
		if ( blink->joined == mob )
			break;
		blink = blink->next;
	}

	if ( blink != NULL )
	{
		REMOVE_FROM_LIST ( blink, head->mob_specials.join_list, next );
		free ( ( void * ) blink );
		blink = NULL;
		mob->mob_specials.head_join = NULL;
		return;
	}
}

void delete_one_join ( Character *mob, int i )
{
	int j = 0;
	struct combine_data *temp, *prev = NULL;


	if ( mob == NULL )
		return;
	temp = mob->mob_specials.join_list;

	while ( temp )
	{
		if ( ( ++j == i ) && prev == NULL )
		{
			mob->mob_specials.join_list = temp->next;
			free ( temp );
			temp = NULL;
			return;
		}
		else if ( j == i )
		{
			prev->next = temp->next;
			free ( temp );
			temp = NULL;
			return;
		}
		prev = temp;
		temp = temp->next;
	}

}

struct combine_data *extract_all_links ( struct combine_data *proto )
{
	struct combine_data *temp = NULL;

	if ( proto == NULL )
		return NULL;

	if ( proto->next )
		extract_all_links ( proto->next );

	if ( proto->joined )
	{
		proto->joined->mob_specials.head_join = NULL;
		extract_char ( proto->joined );
	}
	free ( proto );
	proto = NULL;

	return temp;
}

int join_count ( Character *mob )
{
	struct combine_data *temp = mob->mob_specials.join_list;
	int i = 0;

	while ( temp )
	{
		++i;
		temp = temp->next;
	}
	return i;
}

void die_link_head ( Character *mob )
{
	if ( mob->mob_specials.join_list != NULL )
		extract_all_links ( mob->mob_specials.join_list );
	mob->mob_specials.join_list = NULL;
	return;
}

void extract_linked_mob ( Character *mob )
{
	die_link_head ( mob );
	//die_link(mob);
	return;
}

void free_join_list ( struct combine_data *blist )
{
	if ( blist == NULL )
		return;

	if ( blist->next )
		free_join_list ( blist->next );

	free ( blist );
	blist = NULL;
}

void parse_jspec ( char *buf, Character *mob, int nr )
{
	int vnum = atoi ( buf );
	if ( vnum > 0 ) // assume its valid, we have to
		add_base_link ( mob, vnum );
}


void parse_joined_mob ( FILE * mob_f,Character *mob, int nr )
{
	char line[256];

	parse_enhanced_mob ( mob_f,mob,nr );

	while ( get_line ( mob_f, line ) )
	{
		if ( !strcmp ( line, "J" ) )   /* end of the enhanced section */
			return;
		else if ( *line == '#' )     /* we've hit the next mob, maybe? */
		{
			log ( "SYSERR: Unterminated J section in mob #%d", nr );
			exit ( 1 );
		}
		else
			parse_jspec ( line, mob, nr );
	}

	log ( "SYSERR: Unexpected end of file reached after mob #%d", nr );
	exit ( 1 );
}


void parse_mobile ( FILE * mob_f, int nr, zone_vnum zon )
{
	void set_race ( Character *ch, int race );

	//static int i = 0;
	int j, t[10];
	int vn, count;
	char junk[8], *tmpptr;
	char line[READ_SIZE], letter;
	char f1[128], f2[128], f3[128], f4[128], f5[128], f6[128], f7[128],
	f8[128];
	char buf2[MAX_INPUT_LENGTH];
	Character *mob;
	struct index_data *mi;


	mi = new index_data ( nr );
	mob = new Character();
	mob->clear();
	mob->vnum = nr;

	SetMobProto ( nr, mob );
	SetMobIndex ( nr, mi );
	//mob = mob_proto[i];
	/*
	 * Mobiles should NEVER use anything in the 'player_specials' structure.
	 * The only reason we have every mob in the game share this copy of the
	 * structure is to save newbie coders from themselves. -gg 2/25/98
	 */
	if ( mob->player_specials && mob->player_specials != &dummy_mob )
	{
		delete mob->player_specials;
		mob->player_specials = &dummy_mob;
	}

	snprintf ( buf2, sizeof ( buf2 ), "mob vnum %d", nr );     /* sprintf: OK (for 'buf2 >= 19') */

	/***** String data *****/
	mob->player.name = fread_string ( mob_f, buf2 );
	if ( mob->player.name == NULL )
		mob->player.name = strdup ( "Undefined" );

	tmpptr = mob->player.short_descr = fread_string ( mob_f, buf2 );
	if ( mob->player.short_descr == NULL )
		mob->player.short_descr  = strdup ( "Undefined" );

	if ( tmpptr && *tmpptr )
		if ( !strcmp ( fname ( tmpptr ), "a" ) || !strcmp ( fname ( tmpptr ), "an" )
		        || !strcmp ( fname ( tmpptr ), "the" ) )
			*tmpptr = LOWER ( *tmpptr );
	mob->player.long_descr = fread_string ( mob_f, buf2 );
	if ( mob->player.long_descr == NULL )
		mob->player.long_descr = strdup ( "Undefined" );

	mob->player.description = fread_string ( mob_f, buf2 );
	if ( mob->player.description == NULL )
		mob->player.description = strdup ( "Undefined" );
	if ( mob->player.title != NULL )
		free ( mob->player.title );
	mob->player.title = NULL;
	mob->player.race = 5;
	//  set_race(mob_proto[i], mob_proto[i].player.race);

	/* *** Numeric data *** */
	if ( !get_line ( mob_f, line ) )
	{
		log ( "SYSERR: Format error after string section of mob #%d\n"
		      "...expecting line of form '# # # {S | E}', but file ended!",
		      nr )
		;
		exit ( 1 );
	}

	if ( sscanf
	        ( line, "%s %s %s %s %s %s %s %s %d %c", f1, f2, f3, f4, f5, f6, f7,
	          f8, t + 2, &letter ) != 10 )
	{
		log ( "SYSERR: Format error after string section of mob #%d\n"
		      "...expecting line of form '# # # {S | E}'", nr )
		;
		exit ( 1 );
	}
	MOB_FLAGS ( mob ) [0] = asciiflag_conv ( f1 );
	MOB_FLAGS ( mob ) [1] = asciiflag_conv ( f2 );
	MOB_FLAGS ( mob ) [2] = asciiflag_conv ( f3 );
	MOB_FLAGS ( mob ) [3] = asciiflag_conv ( f4 );
	SET_BIT_AR ( MOB_FLAGS ( mob ), MOB_ISNPC );
	REMOVE_BIT_AR ( MOB_FLAGS ( mob ), MOB_SPEC );
	if ( MOB_FLAGGED ( mob, MOB_NOTDEADYET ) )
	{
		/* Rather bad to load mobiles with this bit already set. */
		log ( "SYSERR: Mob #%d has reserved bit MOB_NOTDEADYET set.", nr );
		REMOVE_BIT_AR ( MOB_FLAGS ( mob ), MOB_NOTDEADYET );
	}


	//check_bitvector_names(MOB_FLAGS(mob_proto[i]), action_bits_count, buf2, "mobile");

	AFF_FLAGS ( mob ) [0] = asciiflag_conv ( f5 );
	AFF_FLAGS ( mob ) [1] = asciiflag_conv ( f6 );
	AFF_FLAGS ( mob ) [2] = asciiflag_conv ( f7 );
	AFF_FLAGS ( mob ) [3] = asciiflag_conv ( f8 );
        GET_ALIGNMENT ( mob ) = t[2];
	//check_bitvector_names(AFF_FLAGS(mob_proto[i]), affected_bits_count, buf2, "mobile affect");

	/* AGGR_TO_ALIGN is ignored if the mob is AGGRESSIVE. */
	if ( MOB_FLAGGED ( mob, MOB_AGGRESSIVE ) &&
	        ( MOB_FLAGGED ( mob, MOB_AGGR_GOOD ) ||
	          MOB_FLAGGED ( mob, MOB_AGGR_EVIL ) ||
	          MOB_FLAGGED ( mob, MOB_AGGR_NEUTRAL ) ) )
		log ( "SYSERR: Mob #%d both Aggressive and Aggressive_to_Alignment.", nr );
        
        // If mob is aggr to sex is ifnored if mob is Aggressive
        if ( MOB_FLAGGED ( mob, MOB_AGGRESSIVE ) &&
                ( MOB_FLAGGED ( mob, MOB_AGGR_FEMALE ) ||
                  MOB_FLAGGED ( mob, MOB_AGGR_MALE ) ||
                  MOB_FLAGGED ( mob, MOB_AGGR_SEX_NEUTRAL)))
		log ( "SYSERR: Mob #%d both Aggressive and Aggressive_to_sex.", nr );

	switch ( UPPER ( letter ) )
	{
		case 'S':              /* Simple monsters */
			parse_simple_mob ( mob_f, mob, nr )
			;
			break;
		case 'E':              /* Circle3 Enhanced monsters */
			parse_enhanced_mob ( mob_f, mob, nr );
			break;

			/* add new mob types here.. */
		default:
			log ( "SYSERR: Unsupported mob type '%c' in mob #%d", letter, nr );
			exit ( 1 );
	}
	letter = fread_letter ( mob_f );
	ungetc ( letter, mob_f );

	mob->mob_specials.join_list = NULL;
	mob->mob_specials.head_join = NULL;
	letter = fread_letter ( mob_f );
	ungetc ( letter, mob_f );
	while ( UPPER ( letter ) == 'J' )
	{
		// assume its valid, we have to
		get_line ( mob_f, line )
		;
		count = sscanf ( line,"%s %d",junk,&vn );
		if ( count != 2 )
		{
			log ( "SYSERR: Error assigning LINK! - Line was\n  %s", line );
			return;
		}
		add_base_link ( mob, vn );
		letter = fread_letter ( mob_f );
		ungetc ( letter, mob_f );
	}

	if ( UPPER ( letter ) == 'H' )
		parse_trainer_mob ( mob_f, mob, nr );

	/* DG triggers -- script info follows mob S/E section */
	letter = fread_letter ( mob_f );
	ungetc ( letter, mob_f );
	while ( UPPER ( letter ) == 'T' )
	{
		dg_read_trigger ( mob_f, mob, MOB_TRIGGER );
		letter = fread_letter ( mob_f );
		ungetc ( letter, mob_f );
	}


	mob->aff_abils = mob->real_abils;

	for ( j = 0; j < NUM_WEARS; j++ )
		mob->equipment[j] = NULL;

	//mob->nr = i;
	mob->desc = NULL;
	mob->proto = TRUE;

	if ( GET_CLASS ( mob ) == CLASS_NORMAL )
		give_mob_class ( mob, nr );

	if ( MOB_FLAGGED ( mob, MOB_HEALER ) )
		ASSIGNMOB ( nr, cleric );

	if ( MOB_FLAGGED ( mob, MOB_POSTMASTER ) )
		ASSIGNMOB ( nr, postmaster );
	/// if (! mob_htree)
	///     mob_htree = htree_init();
	/// htree_add(mob_htree, nr, i);
	/// top_of_mobt = mob_proto.size();
	///i++;

}

int is_aggro ( Character *ch )
{
	if ( MOB_FLAGGED ( ch, MOB_AGGRESSIVE ) ||
	        ( MOB_FLAGGED ( ch, MOB_AGGR_GOOD ) ||
	          MOB_FLAGGED ( ch, MOB_AGGR_EVIL ) ||
	          MOB_FLAGGED ( ch, MOB_AGGR_NEUTRAL ) ||
		  MOB_FLAGGED ( ch, MOB_AGGR_MALE ) ||
		  MOB_FLAGGED ( ch, MOB_AGGR_FEMALE ) ||
		  MOB_FLAGGED ( ch, MOB_AGGR_SEX_NEUTRAL ) ) )
		return 1;
	return 0;
}


/* read all objects from obj file; generate index and prototypes */
char *parse_object ( FILE * obj_f, int nr, zone_vnum zon )
{
	static int i = 0;
	static char line[READ_SIZE];
	int t[NUM_OBJ_VAL_POSITIONS], j = 0, retval;
	double tf[NUM_OBJ_FLOATING_VAL_POSITIONS];
	char *tmpptr;
	char f1[READ_SIZE], f2[READ_SIZE];
	char f3[READ_SIZE], f4[READ_SIZE];
	struct extra_descr_data *new_descr;
	char buf2[MAX_INPUT_LENGTH], *tmp;

	obj_index[i].vnum = nr;
	obj_index[i].number = 0;
	obj_index[i].func = NULL;
	obj_index[i].qic = NULL; // memory leak hopefully fixed - mord

	//    if (! obj_htree)
	//        obj_htree = htree_init();
	//    htree_add(obj_htree, nr, i);
	obj_vTor[nr]=i;

	clear_object ( obj_proto + i );
	obj_proto[i].in_room = NULL;
	obj_proto[i].item_number = i;

	snprintf ( buf2, sizeof ( buf2 ), "object #%d", nr );

	obj_proto[i].name = NULL;
	obj_proto[i].short_description = NULL;
	obj_proto[i].description = NULL;
	obj_proto[i].action_description = NULL;
	obj_proto[i].smell = NULL;
	obj_proto[i].feel = NULL;
	obj_proto[i].taste = NULL;

	/* *** string data *** */
	if ( ( tmp = fread_string ( obj_f, buf2 ) ) == NULL )
	{
		log ( "SYSERR: Null obj name or format error at or near %s", buf2 );
		exit ( 1 );
	}

	/** Convert it all to lower case **/
	obj_proto[i].name = ChToLower ( tmp );
	//    obj_proto[i].FillNames();

	if ( ( obj_proto[i].short_description = tmpptr = fread_string ( obj_f, buf2 ) ) == NULL )
		tmpptr = obj_proto[i].short_description = strdup ( "Undefined" );
	if ( tmpptr && *tmpptr )
		if ( !strcmp ( fname ( tmpptr ), "a" ) || !strcmp ( fname ( tmpptr ), "an" )
		        || !strcmp ( fname ( tmpptr ), "the" ) )
			*tmpptr = LOWER ( *tmpptr );


	/* this now checks to see if the item was made "invi" with {c */


	if ( ( obj_proto[i].description = fread_string ( obj_f, buf2 ) ) == NULL )
		obj_proto[i].description = strdup ( "Undefined" );


	if ( ( obj_proto[i].action_description = fread_string ( obj_f, buf2 ) ) == NULL )
		obj_proto[i].action_description = NULL;
	else if ( !strncmp ( obj_proto[i].action_description , "Undefined", 9 ) )
		free_string ( &obj_proto[i].action_description );

	if ( ( obj_proto[i].smell = fread_string ( obj_f, buf2 ) ) == NULL )
		obj_proto[i].smell = NULL;
	else if ( !strncmp ( obj_proto[i].smell , "Undefined", 9 ) )
		free_string ( &obj_proto[i].smell );

	if ( ( obj_proto[i].taste = fread_string ( obj_f, buf2 ) ) == NULL )
		obj_proto[i].taste = NULL;
	else if ( !strncmp ( obj_proto[i].taste , "Undefined", 9 ) )
		free_string ( &obj_proto[i].taste );

	if ( ( obj_proto[i].feel = fread_string ( obj_f, buf2 ) ) == NULL )
		obj_proto[i].feel = NULL;
	else if ( !strncmp ( obj_proto[i].feel , "Undefined", 9 ) )
		free_string ( &obj_proto[i].feel );

	/* *** numeric data *** */
	if ( !get_line ( obj_f, line ) )
	{
		log ( "SYSERR: Expecting first numeric line of %s, but file ended!", buf2 );
		exit ( 1 );
	}

	if ( ( retval =
	            sscanf ( line, " %d %d %d %d %d %d %d %d %d", t, t + 1, t + 2, t + 3, t + 4, t + 5,
	                     t + 6, t + 7, t + 8 ) ) != 9 )
	{
		log ( "SYSERR: Format error in first numeric line (expecting 9 args, got %d), %s", retval, buf2 );
		exit ( 1 );

	}

	obj_proto[i].obj_flags.type_flag = t[0];
	obj_proto[i].obj_flags.extra_flags[0] = t[1];
	obj_proto[i].obj_flags.extra_flags[1] = t[2];
	obj_proto[i].obj_flags.extra_flags[2] = t[3];
	obj_proto[i].obj_flags.extra_flags[3] = t[4];
	obj_proto[i].obj_flags.wear_flags[0] = t[5];
	obj_proto[i].obj_flags.wear_flags[1] = t[6];
	obj_proto[i].obj_flags.wear_flags[2] = t[7];
	obj_proto[i].obj_flags.wear_flags[3] = t[8];

	check_item_hack_invis ( &obj_proto[i], TRUE );
	tmpptr =obj_proto[i].description;
	if ( tmpptr && *tmpptr )
		CAP ( tmpptr );



	if ( !get_line ( obj_f, line ) )
	{
		log ( "SYSERR: Expecting second numeric line of %s, but file ended!", buf2 );
		exit ( 1 );
	}

	for ( j = 0; j < NUM_OBJ_VAL_POSITIONS; ++j )
		t[ j ] = 0;

	for ( j = 0; j < NUM_OBJ_FLOATING_VAL_POSITIONS; ++j )
		tf[ j ] = 0;

	sscanf ( line, "%d %d %d %d %d %d %d %d %lf %d %d %d %d %d %lf", t, t + 1, t + 2, t + 3, t + 4, t + 5,
			 t + 6, t + 7, tf, t + 8, t + 9, t + 10, t + 11, t + 12, tf + 1 );

	for ( j = 0; j < NUM_OBJ_VAL_POSITIONS; ++j )
		GET_OBJ_VAL ( &obj_proto[i], j ) = t[ j ];

	for ( j = 0; j < NUM_OBJ_FLOATING_VAL_POSITIONS; ++j )
		GET_OBJ_FLOATING_VAL ( &obj_proto[i], j ) = tf[ j ];

	if ( !get_line ( obj_f, line ) )
	{
		log ( "SYSERR: Expecting third numeric line of %s, but file ended!", buf2 );
		exit ( 1 );
	}

	if ( ( retval =
	       sscanf ( line, "%d %lld %d %d %d %d %s %s %s %s", t, &GET_OBJ_COST(obj_proto+i), t + 2, t + 3,
	                     t + 4, t + 5, f1, f2, f3, f4 ) ) != 10 )
	{
		if ( retval == 5 )
		{
			t[5] = 0;
			strcpy ( f1, "" );
			strcpy ( f2, "" );
			strcpy ( f3, "" );
			strcpy ( f4, "" );
		}
		else
		{
			log ( "SYSERR: Format error in third numeric line (expecting 6 args, got %d), %s", retval, buf2 );
			exit ( 1 );
		}
	}

	GET_OBJ_WEIGHT ( obj_proto + i ) = t[0];
	//	GET_OBJ_COST ( obj_proto + i ) = t[1];
	GET_OBJ_RENT ( obj_proto + i ) = t[2];
	obj_proto[i].obj_flags.obj_innate = t[3];
	obj_proto[i].obj_flags.timer = t[4];
	GET_OBJ_LEVEL ( obj_proto + i ) = t[5];
	GET_OBJ_PERM ( obj_proto + i ) [0] = asciiflag_conv ( f1 );
	GET_OBJ_PERM ( obj_proto + i ) [1] = asciiflag_conv ( f2 );
	GET_OBJ_PERM ( obj_proto + i ) [2] = asciiflag_conv ( f3 );
	GET_OBJ_PERM ( obj_proto + i ) [3] = asciiflag_conv ( f4 );





	/* check to make sure that weight of containers exceeds curr. quantity */
	if ( GET_OBJ_TYPE ( obj_proto + i ) == ITEM_DRINKCON || GET_OBJ_TYPE ( obj_proto + i ) == ITEM_FOUNTAIN )
	{
		if ( GET_OBJ_WEIGHT ( obj_proto + i ) < GET_OBJ_VAL ( obj_proto + i, 1 ) )
			GET_OBJ_WEIGHT ( obj_proto + i ) = GET_OBJ_VAL ( obj_proto + i, 1 ) + 5;
		GET_OBJ_VAL ( ( obj_proto + i ), 3 ) = 0; //unpoison it
	}

	if ( GET_OBJ_TYPE ( obj_proto + i ) == ITEM_DRINKCON )
		GET_OBJ_VAL ( ( obj_proto + i ), 3 ) = 0; //unpoison it

	/* *** extra descriptions and affect fields *** */

	for ( j = 0; j < MAX_OBJ_AFFECT; j++ )
	{
		obj_proto[i].affected[j].location = APPLY_NONE;
		obj_proto[i].affected[j].modifier = 0;
	}

	strlcat ( buf2, ", after numeric constants\n"
	          "...expecting 'E', 'A', '$', or next object number", sizeof ( buf2 ) );

	obj_proto[i].ex_description = NULL;
	obj_proto[i].proto_script = NULL;

	if ( GET_OBJ_TYPE ( obj_proto + i ) == ITEM_BANKBOOK )
		ASSIGNOBJ ( nr, bank );

	j = 0;
	for ( ;; )
	{
		if ( !get_line ( obj_f, line ) )
		{
			log ( "SYSERR: Format error in %s", buf2 );
			exit ( 1 );
		}
		switch ( *line )
		{
			/* New vehicle attachments */
			case 'V':
				struct vehicle_attachment_data *att;
				CREATE(att, struct vehicle_attachment_data, 1);
				if ( !get_line ( obj_f, line ) )
				{
					log ( "SYSERR: Format error in 'A' field, %s\n"
						  "...expecting 2 numeric constants but file ended!", buf2 );
					exit ( 1 );
				}

				if ( ( retval = sscanf ( line, " %d %d %d", t, t + 1, t+2 ) ) != 3 )
				{
					log ( "SYSERR: Format error in 'A' field, %s\n"
						  "...expecting 3 numeric arguments, got %d\n"
						  "...offending line: '%s'", buf2, retval, line );
					exit ( 1 );
				}
				att->type = t[0];
				att->value = t[1];
				att->max_value = t[2];
				if (obj_proto[i].attachment)
					att->next = obj_proto[i].attachment;
				obj_proto[i].attachment = att;
				break;
			case 'E':
				CREATE ( new_descr, struct extra_descr_data, 1 );
				if ( ( new_descr->keyword = fread_string ( obj_f, buf2 ) ) == NULL )
					new_descr->keyword = strdup ( "Undefined" );
				if ( ( new_descr->description = fread_string ( obj_f, buf2 ) ) == NULL )
					new_descr->description = strdup ( "Undefined" );
				new_descr->next = obj_proto[i].ex_description;
				obj_proto[i].ex_description = new_descr;
				break;
			case 'A':
				if ( j >= MAX_OBJ_AFFECT )
				{
					log ( "SYSERR: Too many A fields (%d max), %s", MAX_OBJ_AFFECT, buf2 );
					exit ( 1 );
				}
				if ( !get_line ( obj_f, line ) )
				{
					log ( "SYSERR: Format error in 'A' field, %s\n"
					      "...expecting 2 numeric constants but file ended!", buf2 );
					exit ( 1 );
				}

				if ( ( retval = sscanf ( line, " %d %d ", t, t + 1 ) ) != 2 )
				{
					log ( "SYSERR: Format error in 'A' field, %s\n"
					      "...expecting 2 numeric arguments, got %d\n"
					      "...offending line: '%s'", buf2, retval, line );
					exit ( 1 );
				}
				obj_proto[i].affected[j].location = t[0];
				obj_proto[i].affected[j].modifier = t[1];
				j++;
				break;
			case 'T':       /* DG triggers */
				dg_obj_trigger ( line, &obj_proto[i] );
				break;
			case '$':
			case '#':
				top_of_objt = i;
				check_object ( &obj_proto[i], nr );
				i++;
				return ( line );
			default:
				log ( "SYSERR: Format error in (%c): %s", *line, buf2 );
				exit ( 1 );
		}
	}
}


#define Z zone_table[zone]

/* load the zone table and command tables */
void load_zones ( FILE * fl, char *zonename )
{
	static zone_rnum zone = 0;
	int cmd_no, num_of_cmds = 0, line_num = 0, tmp, error = 0, arg_num;
	int version = 2;
	char *ptr = NULL, buf[READ_SIZE], zname[READ_SIZE], buf2[MAX_STRING_LENGTH];
	char t1[80], t2[80];
	long f[5];
	int retval, st_lines = 4;
	int zone_fix = FALSE;

	//log("loading zone: (%s)", zonename);
	strlcpy ( zname, zonename, sizeof ( zname ) );
	get_line ( fl, buf );
	if ( *buf == '@' )
	{
		if ( sscanf ( buf, "@Version: %d", &version ) != 1 )
		{
			log ( "SYSERR: Format error in %s (version)", zname );
			log ( "SYSERR: ... Line: %s", buf );
			exit ( 1 );
		}
		st_lines += 1;
	}
	rewind ( fl );

	for ( tmp = 0; tmp < st_lines; tmp++ )
		get_line ( fl, buf );   //mord??

	/*  More accurate count. Previous was always 4 or 5 too high. -gg 2001/1/17
	 *  Note that if a new zone command is added to reset_zone(), this string
	 *  will need to be updated to suit. - ae.
	 */
	while ( get_line ( fl, buf ) )
		if ( ( strchr ( "MOPGERDTVBZ", buf[0] ) && buf[1] == ' ' )
		        || ( buf[0] == 'S' ) )
			num_of_cmds++; 

	rewind ( fl );         //mord check strings in reset_zone()

	if ( num_of_cmds == 0 )
	{
		log ( "SYSERR: %s is empty!", zname );
		//exit ( 1 );
	}
	else
	{
		reset_com tmpzon = reset_com();
		Z.cmd.assign ( num_of_cmds + 1, tmpzon );
	}


	line_num += get_line ( fl, buf );

	if ( *buf == '@' )
	{
		if ( sscanf ( buf, "@Version: %d", &version ) != 1 )
		{
			log ( "SYSERR: Format error in %s (version)", zname );
			log ( "SYSERR: ... Line: %s", buf );
			exit ( 1 );
		}
		line_num += get_line ( fl, buf );
	}
	//  if (sscanf(buf, "#%hd", &Z.number) != 1) {
	if ( sscanf ( buf, "#%d", &Z.number ) != 1 )
	{
		log ( "SYSERR: Format error in %s, line %d", zname, line_num );
		exit ( 1 );
	}
	snprintf ( buf2, sizeof ( buf2 ), "beginning of zone #%d", Z.number );

	line_num += get_line ( fl, buf );
	if ( ( ptr = strchr ( buf, '~' ) ) != NULL ) /* take off the '~' if it's there */
		*ptr = '\0';
	Z.name = str_dup ( buf );

	line_num += get_line ( fl, buf );
	if ( ( ptr = strchr ( buf, '~' ) ) != NULL ) /* take off the '~' if it's there */
		*ptr = '\0';
	Z.builders = str_dup ( buf );

	line_num += get_line ( fl, buf );
	//&Z.bot, &Z.top, &Z.lifespan,  &Z.reset_mode, &Z.zone_flags

	retval = sscanf ( buf, "%ld %ld %ld %ld %ld %ld", f, f + 1, f + 2,  f + 3, f + 4, f+ 5 );
	switch ( retval )
	{
		case 6:
			Z.bot = ( int ) f[0];
			Z.top = ( int ) f[1];
			Z.lifespan = ( int ) f[2];
			Z.reset_mode = ( int ) f[3];
			Z.zone_flags = f[4];
			Z.dimension = f[5];
			break;
		case 5:
			Z.bot = ( int ) f[0];
			Z.top = ( int ) f[1];
			Z.lifespan = ( int ) f[2];
			Z.reset_mode = ( int ) f[3];
			Z.zone_flags = f[4];
			Z.dimension = D_ALL;
			break;
		case 4:
			Z.bot = ( Z.number * 100 );
			Z.top = ( int ) f[0];
			Z.lifespan = ( int ) f[1];
			Z.reset_mode = ( int ) f[2];
			Z.zone_flags = f[3];
			Z.dimension = D_ALL;
			break;
		case 3:
			Z.bot = ( Z.number * 100 );
			Z.top = ( int ) f[0];
			Z.lifespan = ( int ) f[1];
			Z.reset_mode = ( int ) f[2];
			Z.zone_flags = 0;
			Z.dimension = D_ALL;
			break;
		default:
			log ( "SYSERR: Format error in numeric constant line of %s.", zname );
			log ( "SYSERR: Could not fix previous error, aborting game." );
			exit ( 1 );
	}
	if ( Z.bot > Z.top )
	{
		log ( "SYSERR: Zone %d bottom (%d) > top (%d).", Z.number, Z.bot, Z.top );
		exit ( 1 );
	}


	Z.pressure = 960;
	if ( ( time_info.month >= 7 ) && ( time_info.month <= 12 ) )
		Z.pressure += dice ( 1, 50 );
	else
		Z.pressure += dice ( 1, 80 );
	Z.change = 0;
	if ( Z.pressure <= 980 )
		Z.sky = SKY_LIGHTNING;
	else if ( Z.pressure <= 1000 )
		Z.sky = SKY_RAINING;
	else if ( Z.pressure <= 1020 )
		Z.sky = SKY_CLOUDY;
	else
		Z.sky = SKY_CLOUDLESS;

	cmd_no = tmp = 0;

	for ( ; ; )
	{
		/* skip reading one line if we fixed above (line is correct already) */
		if ( zone_fix != TRUE )
		{
			if ( ( tmp = get_line ( fl, buf ) ) == 0 )
			{
				log ( "SYSERR: Format error in %s - premature end of file",
				      zname );
				exit ( 1 );
			}
		}
		else
			zone_fix = FALSE;

		line_num += tmp;
		ptr = buf;
		skip_spaces ( &ptr );

		if ( ZCMD.SetCommand( *ptr ) == '*' )
			continue;

		ptr++;

		if ( ZCMD.command == 'S' || ZCMD.command == '$' )
		{
			ZCMD.command = 'S';
			cmd_no ++;
			break;
		}
		error = 0;
		if ( strchr ( "D", ZCMD.command ) != NULL )  /* ### */
		{
			if ( sscanf ( ptr, " %d %d %d %d ", &tmp, &ZCMD.arg1, &ZCMD.arg2,
			              &ZCMD.arg3 ) != 4 )
				error = 1;
		}
		else if ( strchr ( "R", ZCMD.command ) != NULL )  /* ### */
		{
			if ( sscanf ( ptr, " %d %d %d ", &tmp, &ZCMD.arg1,
			              &ZCMD.arg2 ) != 3 )
				error = 2;
		}
		else if ( strchr ( "G", ZCMD.command ) != NULL )  /* ### */
		{
			if ( ( arg_num = sscanf ( ptr, " %d %d %d %d ", &tmp, &ZCMD.arg1,
			                          &ZCMD.arg2, &ZCMD.arg3 ) ) != 4 )
			{
				if ( arg_num != 3 )
					error = 3;
				else
					ZCMD.arg3 = 0;
			}
		}
		else if ( strchr ( "V", ZCMD.command ) != NULL )  //changed dg10
		{
			if ( sscanf ( ptr, " %d %d %d %d %79s %79[^\f\n\r\t\v]", &tmp, &ZCMD.arg1, &ZCMD.arg2,
			              &ZCMD.arg3, t1, t2 ) != 6 )
				error = 4;
			else
			{
				ZCMD.sarg1 = str_dup ( t1 );
				ZCMD.sarg2 = str_dup ( t2 );
			}
		}
		else            /* ### */
		{
			if ( ( arg_num =
			            sscanf ( ptr, " %d %d %d %d %d ", &tmp, &ZCMD.arg1,
			                     &ZCMD.arg2, &ZCMD.arg3, &ZCMD.arg4 ) ) != 5 )
			{
				if ( arg_num != 4 )
					error = 5;
				else
					ZCMD.arg4 = 0;
			}
		}

		ZCMD.if_flag = tmp;

		if ( error )
		{
			log ( "SYSERR: Format error in %s, line %d: '%s' (E:%d)", zname,
			      line_num, buf, error );
			exit ( 1 );
		}
		ZCMD.line = line_num;
		cmd_no++;
	}
	if ( num_of_cmds != cmd_no )
	{
		log ( "SYSERR: Zone command count mismatch for %s. Estimated: %d, Actual: %d", zname, num_of_cmds, cmd_no );
		//exit(1);
	}

	top_of_zone_table = zone++;
}

#undef Z
int int_compare ( const void *aa, const void* bb )
{
	int a = * ( int * ) aa;
	int b = * ( int * ) bb;

	return a - b;
}
void renumber_zones ( void )
{
	int i, j;
	for ( j = 0; j < top_of_zone_table; j++ )
	{
		for ( i = 0; i < top_of_zone_table; i++ )
		{
			if ( i==j )
				continue;
			if ( zone_table[j].number == zone_table[i].number )
			{
				log ( "ERROR: Virtual zone exists twice - [%s] and [%s]", zone_table[j].name, zone_table[i].name );
			}
			if ( zone_table[j].bot <= zone_table[i].bot && zone_table[j].top >= zone_table[i].bot )
			{
				log ( "ERROR: Zone [%d] (%d to %d) covers zone [%d] (%d to %d)",
				      zone_table[j].number, zone_table[j].bot, zone_table[j].top,
				      zone_table[i].number, zone_table[i].bot, zone_table[i].top );

			}
			if ( zone_table[j].bot <= zone_table[i].top && zone_table[j].top >= zone_table[i].top )
			{
				log ( "ERROR: Zone [%d] (%d to %d) covers zone [%d] (%d to %d)",
				      zone_table[j].number, zone_table[j].bot, zone_table[j].top,
				      zone_table[i].number, zone_table[i].bot, zone_table[i].top );

			}
		}
	}


}
void do_show_errors ( Character *ch )
{
	int i, j;
	int found = FALSE;
	for ( j = 0; j < top_of_zone_table; j++ )
	{
		for ( i = 0; i < top_of_zone_table; i++ )
		{
			if ( i==j )
				continue;
			if ( zone_table[j].number == zone_table[i].number )
			{
				found = TRUE;
				ch->Send ( "ERROR: Virtual zone exists twice - [%s] and [%s]\r\n", zone_table[j].name, zone_table[i].name );
			}
			if ( zone_table[j].bot <= zone_table[i].bot && zone_table[j].top >= zone_table[i].bot )
			{
				found = TRUE;
				ch->Send ( "ERROR: Zone [%d] (%d to %d) covers zone [%d] (%d to %d)\r\n",
				           zone_table[j].number, zone_table[j].bot, zone_table[j].top,
				           zone_table[i].number, zone_table[i].bot, zone_table[i].top );

			}
			if ( zone_table[j].bot <= zone_table[i].top && zone_table[j].top >= zone_table[i].top )
			{
				found = TRUE;
				ch->Send ( "ERROR: Zone [%d] (%d to %d) covers zone [%d] (%d to %d)\r\n",
				           zone_table[j].number, zone_table[j].bot, zone_table[j].top,
				           zone_table[i].number, zone_table[i].bot, zone_table[i].top );

			}
		}
	}
	if ( !found )
	{
		ch->Send ( "No zone overlaps found\r\n" );
	}
}


void get_one_line ( FILE * fl, char *buf )
{
	if ( fgets ( buf, READ_SIZE, fl ) == NULL )
	{
		log ( "SYSERR: error reading help file: not terminated with $?" );
		exit ( 1 );
	}

	buf[strlen ( buf ) - 1] = '\0';  /* take off the trailing \n */
}
void the_free_help ( void )
{
	unsigned int hp;

	if ( !help_table )
		return;

	for ( hp = 0; hp <= top_of_helpt; hp++ )
	{
		if ( help_table[hp].keywords )
			free ( help_table[hp].keywords );
		if ( help_table[hp].entry && !help_table[hp].duplicate )
			free ( help_table[hp].entry );
	}

	free ( help_table );
	help_table = NULL;
	top_of_helpt = 0;
}


#if 0

void load_help ( FILE * fl )
{
#if defined(CIRCLE_MACINTOSH)
	static char key[READ_SIZE + 1], next_key[READ_SIZE + 1], entry[32384];   /* too big for stack? */
#else

	char key[READ_SIZE + 2] = "", entry[32384] = "";
#endif

	size_t entrylen = 0;
	static char line[READ_SIZE + 1];
	struct help_index_element el;

	/* get the keyword line */
	get_one_line ( fl, key );
	while ( *key != '$' )
	{
		strcat ( key, "\r\n" ); /* strcat: OK (READ_SIZE - "\n" + "\r\n" == READ_SIZE + 1) */
		entrylen = strlcpy ( entry, key, sizeof ( entry ) );     //mord??

		/* read in the corresponding help entry */
		get_one_line ( fl, line );
		while ( *line != '#' && entrylen < sizeof ( entry ) - 1 )
		{
			entrylen +=   strlcpy ( entry + entrylen, line, sizeof ( entry ) - entrylen );

			if ( entrylen + 2 < sizeof ( entry ) - 1 )
			{
				strcpy ( entry + entrylen, "\r\n" ); /* strcpy: OK (size checked above) */
				entrylen += 2;
			}
			get_one_line ( fl, line );
		}

		if ( entrylen >= sizeof ( entry ) - 1 )
		{
			int keysize;
			const char *truncmsg = "\r\n*TRUNCATED*\r\n";

			strcpy ( entry + sizeof ( entry ) - strlen ( truncmsg ) - 1, truncmsg ); /* strcpy: OK (assuming sane 'entry' size) */

			keysize = strlen ( key ) - 2;
			log ( "SYSERR: Help entry exceeded buffer space: %.*s", keysize,
			      key );

			/* If we ran out of buffer space, eat the rest of the entry. */
			while ( *line != '#' )
				get_one_line ( fl, line );
		}



		el.min_level = 0;
		if ( ( *line == '#' ) && ( * ( line + 1 ) != 0 ) )
			el.min_level = atoi ( ( line + 1 ) );

		el.min_level = MAX ( 0, MIN ( el.min_level, LVL_IMPL ) );
		/* now, add the entry to the index with each keyword on the keyword line */

		el.duplicate = 0;
		el.entry = str_dup ( entry );

		add_to_help_index ( &el, key );

		/* get next keyword line (or $) */
		get_one_line ( fl, key );
	}
}
#endif
void load_help ( FILE *fl )
{
#if defined(CIRCLE_MACINTOSH)
	static char key[READ_SIZE + 1],  entry[MAX_HELPENTRY_LENGTH]; /* too big for stack? */
#else

	char key[READ_SIZE + 1], entry[MAX_HELPENTRY_LENGTH];
#endif

	size_t entrylen = 0;
	char line[READ_SIZE + 1];
	struct help_index_element el;

	/* get the first keyword line */
	get_one_line ( fl, key );
	while ( *key != '$' )
	{
		//strlcat(key, "\r\n", sizeof(key));     /* strcat: OK (READ_SIZE - "\n" + "\r\n" == READ_SIZE + 1) */
		//entrylen = strlcpy(entry, key, sizeof(entry));
		entrylen = 0;
		/* read in the corresponding help entry */
		get_one_line ( fl, line );
		while ( *line != '#' && entrylen < sizeof ( entry ) - 1 )
		{
			entrylen += strlcpy ( entry + entrylen, line, sizeof ( entry ) - entrylen );

			if ( entrylen + 2 < sizeof ( entry ) - 1 )
			{
				strcpy ( entry + entrylen, "\r\n" ); /* strcpy: OK (size checked above) */
				entrylen += 2;
			}

			get_one_line ( fl, line );
		}

		{
			int i = entrylen-1;
			while ( entry[i] == '\n' || entry[i] == '\r' ) /* Trim trailing whitespace */
				entry[i--] = 0;
			entrylen = i + 1;
		}
		if ( entrylen >= sizeof ( entry ) - 1 )
		{
			int keysize;
			const char *truncmsg = "\r\n*TRUNCATED*\r\n";

			strcpy ( entry + sizeof ( entry ) - strlen ( truncmsg ) - 1, truncmsg ); /* strcpy: OK (assuming sane 'entry' size) */

			keysize = strlen ( key ) - 2;
			log ( "SYSERR: Help entry exceeded buffer space: %.*s", keysize, key );

			/* If we ran out of buffer space, eat the rest of the entry. */
			while ( *line != '#' )
				get_one_line ( fl, line );
		}
		el.min_level = 0;
		if ( ( *line == '#' ) && ( * ( line + 1 ) != 0 ) )
			el.min_level = atoi ( ( line + 1 ) );
		/* now, add the entry to the index with each keyword on the keyword line */
		el.duplicate = 0;
		el.id = max_help_id++;
		el.entry = str_dup ( entry );
		el.entries = 0;
		prune_crlf ( key );
		el.keywords = str_dup ( key );
		el.duplicate = 0; //redundant call
		help_table[top_of_helpt++] = el;


		/* get next keyword line (or $) */
		get_one_line ( fl, key );
	}
}


#if 0
struct help_index_element *add_to_help_index ( struct help_index_element *parent,int id, char *header, char *body )
{
	struct help_index_element *el;

	CREATE ( el, struct help_index_element, 1 );
	if ( !parent )
	{
		//    create_help_core_parent(parent);
	}
	el->next = parent->items;
	parent->items = el;
	el->parent = parent;
	el->items = NULL;
	el->header = str_dup ( header );
	el->body = str_dup ( body );
	return el;

}
#endif



/*************************************************************************
*  procedures for resetting, both play-time and boot-time         *
*************************************************************************/
void do_show_trainers ( Character *ch )
{
	int  found = 0, i;
	char buf[MAX_INPUT_LENGTH];
	Character *mob;
	DYN_DEFINE;
	*buf = 0;
	DYN_CREATE;
	*dynbuf = 0;

	for ( mp_iter mit = mob_proto.begin(); mit != mob_proto.end(); mit++ )
	{
		mob = mit->second;

		if ( mob != NULL && !mob->mob_specials.teaches_skills.empty() )
		{
			snprintf ( buf, sizeof ( buf ), "%3d. [%5d] {cy%-40s{c0 %s\r\n", ++found,
			           mob->vnum, mob->player.short_descr,
			           mob->proto_script ? "[TRIG]" : "" );
			DYN_RESIZE ( buf );
			for ( i = 0; i < mob->mob_specials.teaches_skills.size();i++ )
			{
				snprintf ( buf, sizeof ( buf ), "%s[%5d] {cg%-15s{c0%s", 
				! ( i%2 ) ? "             " : " ",
				mob->mob_specials.teaches_skills[i], 
				skill_name ( mob->mob_specials.teaches_skills[i] ), 
				! ( i%2 ) ? " " : "\r\n" );
				DYN_RESIZE ( buf );
			}
			snprintf ( buf, sizeof ( buf ), "%s" , !( i%2 ) ? "" : "\r\n" );
			DYN_RESIZE ( buf );

		}
	}
	snprintf ( buf, sizeof ( buf ), "Found: %3d.\r\n" , found );
	DYN_RESIZE ( buf );
	page_string ( ch->desc, dynbuf, DYN_BUFFER );
	return;
}


int vnum_mobile ( char *searchname, Character *ch )
{
	int nr = 0, found = 0;
	char buf[MAX_INPUT_LENGTH];
	DYN_DEFINE;
	*buf = 0;
	DYN_CREATE;
	*dynbuf = 0;

	for ( mp_iter mit = mob_proto.begin(); mit != mob_proto.end(); mit++ )
	{

		if ( ( mit->second ) != NULL && isname_full ( searchname, ( mit->second )->player.name ) )
		{
			snprintf ( buf, sizeof ( buf ), "%3d. [%5d] %-40s %s\r\n", ++found,
			           ( mit->second )->vnum, ( mit->second )->player.short_descr,
			           ( mit->second )->proto_script ? "[TRIG]" : "" );
			DYN_RESIZE ( buf );
		}
		nr++;
	}
	page_string ( ch->desc, dynbuf, DYN_BUFFER );
	return ( found );
}



int vnum_object ( char *searchname, Character *ch , int type)
{
	int nr, found = 0;
	char buf[MAX_INPUT_LENGTH];
	DYN_DEFINE;
	*buf = 0;
	DYN_CREATE;
	*dynbuf = 0;

	for ( nr = 0; nr <= top_of_objt; nr++ )
	{
		if ( (!type &&  isname_full ( searchname, obj_proto[nr].name ))
      || (type && (IS_SET_AR(obj_proto[nr].obj_flags.bitvector, AFF_PROT_FIRE) 
      || IS_SET_AR(obj_proto[nr].obj_flags.bitvector, AFF_PROT_COLD))) )
		{
			snprintf ( buf, sizeof ( buf ), "%3d. [%5d] %-40s %s\r\n", ++found,
			           obj_index[nr].vnum,
			           obj_proto[nr].short_description,
			           obj_proto[nr].proto_script ? "[TRIG]" : "" );
			DYN_RESIZE ( buf );
		}
	}
	page_string ( ch->desc, dynbuf, DYN_BUFFER );
	return ( found );
}

#if USE_CREATE_CHAR
/* create a character, and add it to the char list */
Character *create_char ( void )
{
	Character *ch = new Character();

	add_char_to_list ( ch );
	//TODO: check this
	while ( !valid_id_num ( max_mob_id ) )
	{
		log ( "Error new id being assigned to mob already exists(%ld)!", max_mob_id );
		max_mob_id++;
	}
	GET_ID ( ch ) = max_mob_id++;
	/* find_char helper */
	add_to_lookup_table ( GET_ID ( ch ), ( void * ) ch );
	return ( ch );
}
#endif


/* create a new mobile from a prototype */
Character *read_mobile ( mob_vnum nr )
{
	Character *mob;

	if ( !MobProtoExists ( nr ) )
	{
		log ( "WARNING: Mobile vnum %d does not exist in database.", nr );
		return ( NULL );
	}

	mob = new Character();
	/** remove player data from mob **/
	*mob = *GetMobProto ( nr );
	mob->proto = FALSE;
	add_char_to_list ( mob );
	set_race ( mob, GetMobProto ( nr )->player.race );

	if ( MOB_TIER ( mob ) == 0 && ( !is_aggro ( mob ) ) )
	{
		if ( !number ( 0, 10 ) )
			MOB_TIER ( mob ) ++;
		if ( !number ( 0, 10 ) && MOB_TIER ( mob ) )
			MOB_TIER ( mob ) ++;
		if ( !number ( 0, 10 ) && MOB_TIER ( mob ) )
			MOB_TIER ( mob ) ++;
		if ( !number ( 0, 10 ) && MOB_TIER ( mob ) )
			MOB_TIER ( mob ) ++;
	}

	GET_EXP ( mob ) = ( GET_EXP ( mob ) * MAX ( 1, MOB_TIER ( mob ) + 1 ) );

	if ( !mob->points.max_hit )
	{
		mob->points.max_hit = dice ( mob->points.hit, mob->points.mana ) + mob->points.move;
	}
	mob->points.max_hit = FTOI ( mob->points.max_hit * ( mob_hitpoint_multi ( GET_CLASS ( mob ) ) * ( 1.0  + ( MOB_TIER ( mob ) * 0.75 ) ) ) ) ;
	mob->points.hit = mob->points.max_hit;
	mob->points.mana = mob->points.max_mana;
	mob->points.move = mob->points.max_move;
	mob->points.stamina = mob->points.max_stamina;

	mob->player.time.birth = time ( 0 );
	mob->player.time.played = 0;
	mob->player.time.logon = time ( 0 );

	if ( GET_MRACE ( mob ) == MOB_RACE_ANIMAL || GET_MRACE ( mob ) == MOB_RACE_EXOTIC)
		GET_GOLD ( mob ) = 0;

	GetMobIndex ( nr )->number++;
	while ( !valid_id_num ( max_mob_id ) )
	{
		log ( "Error new id being assigned to mob already exists(%ld)!", max_mob_id );
		max_mob_id++;
	}
	GET_ID ( mob ) = max_mob_id++;
	/* find_char helper */
	addChToLookupTable ( GET_ID ( mob ), mob );
	mobNames.addNamelist(mob->player.name, GET_ID(mob));

	//    copy_proto_script(GetMobProto(nr), mob, MOB_TRIGGER);
	assign_triggers ( mob, MOB_TRIGGER );
	mob->mob_specials.join_list = copy_proto_link ( GetMobProto ( nr )->mob_specials.join_list );
	load_links ( mob );

	return ( mob );
}


/* create an object, and add it to the object list */
struct obj_data *create_obj ( obj_rnum proto )
{
	struct obj_data *obj;

	CREATE ( obj, struct obj_data, 1 );
	clear_object ( obj );
	/** Testing that we don't need these - Mord **/
	//obj->next = object_list;
	//object_list = obj;

	obj->name = NULL;
	obj->action_description = NULL;
	obj->description = NULL;
	obj->short_description = NULL;
	obj->smell = NULL;
	obj->taste = NULL;
	obj->feel = NULL;
	obj->ex_description = NULL;
	obj->item_number = NOTHING;
	obj->in_room = NULL;
	if ( proto != NOTHING && proto <= top_of_objt )
		*obj = obj_proto[proto];
	GET_ID ( obj ) = max_obj_id++;
	/* find_obj helper */
	object_list[GET_ID ( obj ) ] = obj;
	addObjToLookupTable ( GET_ID ( obj ), obj );

	return ( obj );
}


/* create a new object from a prototype */
struct obj_data *read_object ( obj_vnum nr, int type )                  /* and obj_rnum */
{
	struct obj_data *obj = NULL;
	obj_rnum i;

	i = ( ( type == VIRTUAL ) ? real_object ( nr ) : nr );

	if ( i == NOTHING || i > top_of_objt )
	{
		log ( "Object (%c) %d does not exist in database.", type == VIRTUAL ? 'V' : 'R', nr );
		return ( NULL );
	}
	qic_load ( i ); //remove in free obj

	obj = create_obj ( i );

	if ( obj_index[i].qic )  {
 		log ( "%s created", obj->short_description );
  } 

	obj_index[i].number++;
	generate_weapon ( obj );
	copy_proto_script ( &obj_proto[i], obj, OBJ_TRIGGER );
	assign_triggers ( obj, OBJ_TRIGGER );
	objNames.addNamelist(obj->name, GET_ID(obj));

        /* New vehicle code by Horus                     *
         * If its vehicle, load the room for the vehicle */

	if ( GET_OBJ_TYPE ( obj ) == ITEM_VEHICLE2 )
		create_vehicle_room ( obj );

	return ( obj );
}

bool ZonePurgeObject ( olt_it ob, int zone )
{
	obj_data *obj = ob->second;
	if ( ! ( obj ) )
		return false;

	if ( ! ( obj )->carried_by && ! ( obj )->in_obj && ! ( obj )->worn_by & ! ( obj )->contains & ! ( obj )->in_locker
	        && ( ( obj )->in_room->zone == zone )
	        && ( !IS_SET_AR ( ( obj )->in_room->room_flags, ROOM_HOUSE ) )
	        && ( !IS_SET_AR ( ( obj )->in_room->room_flags, ROOM_HOUSE_CRASH ) )
	        && ( !IS_OBJ_STAT ( ( obj ), ITEM_PC_CORPSE ) ) )
		return true;
	else
		return false;
}
int purge_zone ( int zone )
{
	Character *ch, *next_ch;
	vector<long> ex_list;

	for ( ch = character_list; ch; ch = next_ch )
	{
		next_ch = ch->next;
		if ( IS_NPC ( ch ) && ( IN_ROOM ( ch )->zone == zone )
		        && !FIGHTING ( ch ) )
			extract_char ( ch );
	}

	/** Find all items that need extracting **/
	for ( olt_it ob = object_list.begin(); ob != object_list.end(); ob++ )
		if ( ZonePurgeObject ( ob, zone ) )
			ex_list.push_back ( GET_ID ( ( ob->second ) ) );
	/** extract them now **/
	for ( vector<long>::iterator v = ex_list.begin();v!= ex_list.end();v++ )
	{
		if ( object_list.find ( *v ) != object_list.end() )
			extract_obj ( object_list[ ( *v ) ] );
	}

	return 0;
}

#define ZO_DEAD  999

/* update zone ages, queue for reset if necessary, and dequeue when possible */
void zone_update ( void )
{
	int i, empty = 0;
	struct reset_q_element *update_u, *temp;
	static int timer = 0;

	/* jelson 10/22/92 */
	if ( ( ( ++timer * PULSE_ZONE ) / PASSES_PER_SEC ) >= 60 )
	{
		/* one minute has passed */
		/*
		 * NOT accurate unless PULSE_ZONE is a multiple of PASSES_PER_SEC or a
		 * factor of 60
		 */

		timer = 0;

		/* since one minute has passed, increment zone ages */
		for ( i = 0; i <= top_of_zone_table; i++ )
		{
			if ( zone_table[i].age < zone_table[i].lifespan &&
			        zone_table[i].reset_mode )
				( zone_table[i].age ) ++;

			if ( zone_table[i].age >= zone_table[i].lifespan &&
			        zone_table[i].age < ZO_DEAD && zone_table[i].reset_mode )
			{
				/* enqueue zone */

				CREATE ( update_u, struct reset_q_element, 1 );

				update_u->zone_to_reset = i;
				update_u->next = 0;

				if ( !reset_q.head )
					reset_q.head = reset_q.tail = update_u;
				else
				{
					reset_q.tail->next = update_u;
					reset_q.tail = update_u;
				}

				zone_table[i].age = ZO_DEAD;
			}
		}
	}



	/* end - one minute has passed */
	/* dequeue zones (if possible) and reset */
	/* this code is executed every 10 seconds (i.e. PULSE_ZONE) */
	for ( update_u = reset_q.head; update_u; update_u = update_u->next )
		if ( zone_table[update_u->zone_to_reset].reset_mode == 2 ||
		        ( empty = zone_is_empty ( update_u->zone_to_reset ) ) )
		{
			if ( empty && ZONE_FLAGGED ( update_u->zone_to_reset, ZONE_PURGE_EMPTY ) )
			{
				purge_zone ( update_u->zone_to_reset );
				new_mudlog ( CMP, LVL_GOD, FALSE, "Auto zone purge + reset (Zone %3d): %s ",
				             zone_table[update_u->zone_to_reset].number,
				             zone_table[update_u->zone_to_reset].name );
			}
			//else
			/* new_mudlog(CMP, LVL_GOD, FALSE, "Auto zone reset (Zone %3d): %s ",
			            zone_table[update_u->zone_to_reset].number,
			            zone_table[update_u->zone_to_reset].name);*/
			reset_zone ( update_u->zone_to_reset );
			/* dequeue */
			if ( update_u == reset_q.head )
				reset_q.head = reset_q.head->next;
			else
			{
				for ( temp = reset_q.head;temp && temp->next != update_u;
				        temp = temp->next )
					;

				if ( !update_u->next )
					reset_q.tail = temp;

				temp->next = update_u->next;
			}

			free ( update_u );
			break;
		}
}

void log_zone_error ( zone_rnum zone, int cmd_no, const char *message )
{
	new_mudlog ( NRM, LVL_GOD, TRUE, "SYSERR: zone file: %s", message );
	new_mudlog ( NRM, LVL_GOD, TRUE, "SYSERR: ...offending cmd: '%c' cmd in zone #%d, line %d",
	             ZCMD.command, zone_table[zone].number, ZCMD.line );
}

void make_maze ( int zone )
{
#if 0
	int card[400], temp, x, y, dir;
	room_rnum room, next_room = NULL;
	int num, test, r_back;
	int vnum = zone_table[zone].number;


	for ( test = 0; test < 400; test++ )
	{
		card[test] = test;
		temp = test;
		dir = temp / 100;
		temp = temp - ( dir * 100 );
		x = temp / 10;
		temp = temp - ( x * 10 );
		y = temp;
		room = world_vnum[ ( vnum * 100 ) + ( x * 10 ) + y];
		if ( ( x == 0 ) && ( dir == 0 ) )
			continue;
		if ( ( y == 9 ) && ( dir == 1 ) )
			continue;
		if ( ( x == 9 ) && ( dir == 2 ) )
			continue;
		if ( ( y == 0 ) && ( dir == 3 ) )
			continue;
		room->dir_option[dir]->to_room = -1;
		REMOVE_BIT_AR ( ROOM_FLAGS ( room ), ROOM_NOTRACK );
	}
	for ( x = 0; x < 399; x++ )
	{
		y = number ( 0, 399 );
		temp = card[y];
		card[y] = card[x];
		card[x] = temp;
	}

	for ( num = 0; num < 400; num++ )
	{
		temp = card[num];
		dir = temp / 100;
		temp = temp - ( dir * 100 );
		x = temp / 10;
		temp = temp - ( x * 10 );
		y = temp;
		room = world_vnum[ ( vnum * 100 ) + ( x * 10 ) + y];
		r_back = room;
		room = real_room ( room );
		if ( ( x == 0 ) && ( dir == 0 ) )
			continue;
		if ( ( y == 9 ) && ( dir == 1 ) )
			continue;
		if ( ( x == 9 ) && ( dir == 2 ) )
			continue;
		if ( ( y == 0 ) && ( dir == 3 ) )
			continue;
		if ( room->dir_option[dir]->to_room != -1 )
			continue;
		switch ( dir )
		{
			case 0:
				next_room = r_back - 10;
				break;
			case 1:
				next_room = r_back + 1;
				break;
			case 2:
				next_room = r_back + 10;
				break;
			case 3:
				next_room = r_back - 1;
				break;
		}
		next_room = real_room ( next_room );
		test = find_first_step ( room, next_room );
		switch ( test )
		{
			case BFS_ERROR:
				log ( "Maze making error." );
				break;
			case BFS_ALREADY_THERE:
				log ( "Maze making error." );
				break;
			case BFS_NO_PATH:

				room->dir_option[dir]->to_room = next_room;
				next_room->dir_option[ ( int ) rev_dir[dir]]->to_room = room;
				break;
		}
	}
	for ( num = 0; num < 100; num++ )
	{
		room = ( vnum * 100 ) + num;
		room = real_room ( room );
		/* Remove the next line if you want to be able to track your way through
		   the maze */
		SET_BIT_AR ( ROOM_FLAGS ( room ), ROOM_NOTRACK );

		REMOVE_BIT_AR ( ROOM_FLAGS ( room ), ROOM_BFS_MARK );
	}
#endif

}

#define ZONE_ERROR(message) \
     { log_zone_error(zone, cmd_no, message); last_cmd = 0; }
typedef  multimap<room_vnum, long> objs_in_room;
typedef  objs_in_room::iterator    obj_rit;
typedef  pair<room_vnum, long>     obj_rid;
typedef  pair<obj_rit, obj_rit>    oir_range;

obj_data *put_in_to ( room_vnum rvn, obj_vnum ovn, objs_in_room &mm )
{
	olt_it olit;
	oir_range oirr = mm.equal_range ( rvn );

	for ( obj_rit it = oirr.first;it!=oirr.second;it++ )
	{
		olit = object_list.find ( it->second );
		if ( olit != object_list.end() )
			if ( GET_OBJ_RNUM ( olit->second ) == ovn )
				return olit->second;
		//else log("Obj vnum wanted %d, in room %d, got %d",ovn, rvn, GET_OBJ_VNUM(olit->second));
	}
	return NULL;
}

/* execute the reset command table of a given zone */
void reset_zone ( zone_rnum zone )
{
	int cmd_no;
	bool last_cmd = false;
	Character *mob = NULL;
	struct obj_data *obj, *obj_to;
	Character *tmob = NULL;   /* for trigger assignment */
	struct obj_data *tobj = NULL;    /* for trigger assignment */
	objs_in_room oir; /* objects in rooms */
	room_rnum rm;
	room_vnum vrm, cmd_room = 0;
	char er_msg[MAX_INPUT_LENGTH];

	for ( cmd_no = 0; ZCMD.command != 'S'; cmd_no++ )
	{

		switch ( ZCMD.command )
		{
			case 'M':
			case 'O':
			case 'T':
			case 'V':
			case 'B':
				cmd_room = ZCMD.arg3;
				break;
			case 'D':
			case 'R':
				cmd_room = ZCMD.arg1;
				break;
			default:
				break;
		}

		if ( ZCMD.if_flag && !last_cmd )
			continue;

		switch ( ZCMD.command )
		{
			case '*':       /* ignore command */
				last_cmd = false;
				tobj = NULL;
				break;

			case 'M':       /* read a mobile */
				if ( real_room ( ZCMD.arg3 ) == NULL )
				{
					log ( "Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg3 );
					exit ( 1 );
				}
				if ( MobIndexExists ( ZCMD.arg1 ) && ( GetMobIndex ( ZCMD.arg1 )->number < ZCMD.arg2 ) )
				{
					mob = read_mobile ( ZCMD.arg1 );
					char_to_room ( mob, world_vnum[ZCMD.arg3] );
					if ( load_mtrigger ( mob ) != -1 )
					{
						tmob = mob;
						last_cmd = true;
					}
					else
					{
						mob = NULL;
						tmob = mob;
						last_cmd = false;
					}
				}
				else
					last_cmd = false;
				tobj = NULL;
				break;

			case 'O':       /* read an object */
				if ( real_room ( ZCMD.arg3 ) == NULL )
				{
					log ( "Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg3 );
					exit ( 1 );
				}

				if ( obj_index[ZCMD.arg1].number < ZCMD.arg2 )
				{
					if ( ZCMD.arg3 != NOWHERE )
					{
						if ( !get_obj_in_list_num ( ZCMD.arg1, world_vnum[ZCMD.arg3]->contents ) )
						{
							bool lqc = load_qic_check ( ZCMD.arg1 );
							/** we need to treat QIC items as if they loaded, so that the other items in the list with a QIC item can load still  - mord**/
							obj = read_object ( ZCMD.arg1, REAL );
							if ( obj && lqc )
							{
								obj_to_room ( obj, world_vnum[ZCMD.arg3] );
		                                                ripple_detect(cmd_room, ZCMD.arg1);
								if ( load_otrigger ( obj ) == -1 )
									obj = NULL;
								tobj = obj;
								last_cmd = obj ? true : false;

							}
							else
							{
								if ( obj )
								{
									purge_qic ( ZCMD.arg1 );
									extract_obj ( obj );
									tobj = obj = NULL;
									last_cmd = true;
								} else
								last_cmd = false; 
							}

						}
						else
							last_cmd = false;
					}
					else
					{
						obj = read_object ( ZCMD.arg1, REAL );
						if ( obj )
							IN_ROOM ( obj ) = NULL;
						tobj = obj;
						last_cmd = true;
					}
				}
				else
					last_cmd = false;
				tmob = NULL;
				if ( last_cmd && tobj )
					oir.insert ( obj_rid ( cmd_room,GET_ID ( tobj ) ) );

				break;

			case 'P':       /* object to object */
				if ( ( obj_index[ZCMD.arg1].number < ZCMD.arg2 ) && cmd_room )
				{
					if ( ! ( obj_to = put_in_to ( cmd_room, ZCMD.arg3, oir ) ) )   /* get_obj_in_list_num(ZCMD.arg3, cmd_room->contents) */
					{
						snprintf ( er_msg, sizeof ( er_msg ), "Put: target object vnum %d not found in room vnum %d", obj_index[ZCMD.arg3].vnum, cmd_room );
						ZONE_ERROR ( er_msg );
						//ZCMD.command = '*';
						break;
					}
					bool lqc = load_qic_check ( ZCMD.arg1 );
					obj = read_object ( ZCMD.arg1, REAL );
					if ( obj && lqc )
					{
						obj_to_obj ( obj, obj_to );
						ripple_detect(cmd_room, ZCMD.arg1);
						if ( load_otrigger ( obj ) == -1 )
							obj = NULL;
						tobj = obj;

						last_cmd = obj ? true : false;



					}
					else
					{
						if ( obj )
						{
							purge_qic ( ZCMD.arg1 );
							extract_obj ( obj );
							tobj = obj = NULL;
							last_cmd = true;
						}else 
						last_cmd = false;
					}
				}
				else
					last_cmd = false;
				tmob = NULL;
				if ( last_cmd && tobj )
					oir.insert ( obj_rid ( cmd_room,GET_ID ( tobj ) ) );
				break;

			case 'G':       /* obj_to_char ### */
				if ( !mob )
				{
					ZONE_ERROR
					( "attempt to give obj to non-existant mob, command disabled" );
					ZCMD.command = '*';
					break;
				}
				if ( obj_index[ZCMD.arg1].number < ZCMD.arg2 )
				{
					bool lqc = load_qic_check ( ZCMD.arg1 );
					obj = read_object ( ZCMD.arg1, REAL );
					if ( obj && lqc )
					{
						obj_to_char ( obj, mob );
                                                ripple_detect(cmd_room, ZCMD.arg1);
						if ( load_otrigger ( obj ) == -1 )
							obj = NULL;



						tobj = obj;

						last_cmd = obj ? true : false;



					}
					else
					{
						if ( obj )
						{
							purge_qic ( ZCMD.arg1 );
							extract_obj ( obj );
							tobj = obj = NULL;
							last_cmd = true;
						} else
						last_cmd = false;
					}
				}
				else
					last_cmd = false;
				tmob = NULL;
				if ( last_cmd && tobj )
					oir.insert ( obj_rid ( cmd_room,GET_ID ( tobj ) ) );
				break;
			case 'E':       /* object to equipment list ### */
				if ( !mob )
				{
					ZONE_ERROR
					( "trying to equip non-existant mob, command disabled" );
					ZCMD.command = '*';
					last_cmd = false;
					break;
				}
				if ( ( obj_index[ZCMD.arg1].number < ZCMD.arg2 ) )
				{
					if ( ZCMD.arg3 < 0 || ZCMD.arg3 >= NUM_WEARS )
					{
						ZONE_ERROR ( "invalid equipment pos number" );
					}
					else
					{
						int ret=-1;
						bool lqc = load_qic_check ( ZCMD.arg1 );
						obj = read_object ( ZCMD.arg1, REAL );
						if ( obj && lqc )
						{

                                                ripple_detect(cmd_room, ZCMD.arg1);
	

						IN_ROOM ( obj ) = IN_ROOM ( mob );
							if ( load_otrigger ( obj ) == -1 )
								obj = NULL;
							if ( obj && ( ret = wear_otrigger ( obj, mob, ZCMD.arg3 ) ) > 0 )
							{
								IN_ROOM ( obj ) = NULL;
								if ( equip_char ( mob, obj, ZCMD.arg3 ) )
								{

									last_cmd = true;
									tobj = obj;
								}
								else
								{
									if ( obj )
									{
										purge_qic ( ZCMD.arg1 );
										extract_obj ( obj );
										obj = NULL;
									}
								}
							}
							else if ( obj && ret == 0 )
							{
								obj_to_char ( obj, mob );
								tobj = obj;
								last_cmd = true;
							}
							else
							{
								obj = NULL;
								last_cmd = !lqc;
								tobj = NULL;
							}
						}
						else
						{
							if ( obj )
							{
								purge_qic ( ZCMD.arg1 );
								extract_obj ( obj );
								tobj = obj = NULL;
								last_cmd = true;
							} else
							last_cmd = false;
						}
					}
				}
				else
					last_cmd = false;
				tmob = NULL;
				if ( last_cmd && tobj )
					oir.insert ( obj_rid ( cmd_room,GET_ID ( tobj ) ) );
				break;
			case 'R':       /* rem obj from room */
				if ( !world_vnum[ZCMD.arg1] )
				{
					ZONE_ERROR ( "Zone room error" );
					log ( "room %d doesn't exist, and zedit needs it.",ZCMD.arg1 );
				}
				else
					if ( ( obj = get_obj_in_list_num ( ZCMD.arg2, world_vnum[ZCMD.arg1]->contents ) ) != NULL )
					{
						for ( multimap<room_vnum, long>::iterator it = oir.begin(); it!=oir.end();it++ )
						{
							if ( it->second == GET_ID ( obj ) )
							{
								oir.erase ( it );
								break;
							}
						}
						extract_obj ( obj );
						obj = NULL;
					}
				last_cmd = true;
				tmob = NULL;
				tobj = NULL;
				break;


			case 'D':       /* set state of door */
				if ( real_room ( ZCMD.arg1 ) == NULL )
				{
					log ( "Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg1 );
					exit ( 1 );
				}
				if ( ZCMD.arg2 < 0 || ZCMD.arg2 >= NUM_OF_DIRS ||
				        ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2] == NULL ) )
				{
					ZONE_ERROR
					( "WARNING: door does not exist, command disabled." );
					new_mudlog ( BRF, LVL_GOD, FALSE,"%s door in room %d doesn't exist, and zedit needs it.",
					             dirs[ZCMD.arg2], world_vnum[ZCMD.arg1]->number );
				}
				else
					switch ( ZCMD.arg3 )
					{
						case 0:
							REMOVE_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							             exit_info, EX_LOCKED );
							REMOVE_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							             exit_info, EX_CLOSED );
							REMOVE_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							             exit_info, EX_HIDDEN );
							break;
						case 1:
							SET_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							          exit_info, EX_CLOSED );
							REMOVE_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							             exit_info, EX_LOCKED );
							REMOVE_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							             exit_info, EX_HIDDEN );
							break;
						case 2:
							SET_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							          exit_info, EX_LOCKED );
							SET_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							          exit_info, EX_CLOSED );
							REMOVE_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							             exit_info, EX_HIDDEN );
							break;
						case 3:
							SET_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							          exit_info, EX_CLOSED );
							SET_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							          exit_info, EX_HIDDEN );
							REMOVE_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							             exit_info, EX_LOCKED );
							break;
						case 4:
							SET_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							          exit_info, EX_CLOSED );
							SET_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							          exit_info, EX_LOCKED );
							SET_BIT ( world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
							          exit_info, EX_HIDDEN );
							break;
					}
				last_cmd = true;
				tmob = NULL;
				tobj = NULL;
				break;

			case 'T': /* trigger command */
				if ( real_room ( ZCMD.arg1 ) == NULL )
				{
					log ( "Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg1 );
					exit ( 1 );
				}
				if ( ZCMD.arg1==MOB_TRIGGER && tmob )
				{
					if ( !SCRIPT ( tmob ) )
						CREATE ( SCRIPT ( tmob ), struct script_data, 1 );
					add_trigger ( SCRIPT ( tmob ), read_trigger ( ZCMD.arg2 ), -1 );
					last_cmd = true;
				}
				else if ( ZCMD.arg1==OBJ_TRIGGER && tobj )
				{
					if ( !SCRIPT ( tobj ) )
						CREATE ( SCRIPT ( tobj ), struct script_data, 1 );
					add_trigger ( SCRIPT ( tobj ), read_trigger ( ZCMD.arg2 ), -1 );
					last_cmd = true;
				}
				else if ( ZCMD.arg1==WLD_TRIGGER )
				{
					if ( ZCMD.arg3 == NOWHERE || ZCMD.arg3>top_of_world )
					{
						ZONE_ERROR ( "Invalid room number in trigger assignment" );
					}
					if ( !world_vnum[ZCMD.arg3]->script )
						CREATE ( world_vnum[ZCMD.arg3]->script, struct script_data, 1 );
					add_trigger ( world_vnum[ZCMD.arg3]->script, read_trigger ( ZCMD.arg2 ), -1 );
					last_cmd = true;
				}

				break;

			case 'V':
				if ( real_room ( ZCMD.arg3 ) == NULL )
				{
					log ( "Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg3 );
					exit ( 1 );
				}
				if ( ZCMD.arg1==MOB_TRIGGER && tmob )
				{
					if ( !SCRIPT ( tmob ) )
					{
						ZONE_ERROR ( "Attempt to give variable to scriptless mobile" );
					}
					else
						add_var ( & ( SCRIPT ( tmob )->global_vars ), ZCMD.sarg1, ZCMD.sarg2,
						          ZCMD.arg2 );
					last_cmd = true;
				}
				else if ( ZCMD.arg1==OBJ_TRIGGER && tobj )
				{
					if ( !SCRIPT ( tobj ) )
					{
						ZONE_ERROR ( "Attempt to give variable to scriptless object" );
					}
					else
						add_var ( & ( SCRIPT ( tobj )->global_vars ), ZCMD.sarg1, ZCMD.sarg2,
						          ZCMD.arg2 );
					last_cmd = true;
				}
				else if ( ZCMD.arg1==WLD_TRIGGER )
				{
					if ( ZCMD.arg3 == NOWHERE || ZCMD.arg3>top_of_world )
					{
						ZONE_ERROR ( "Invalid room number in variable assignment" );
					}
					else
					{
						if ( ! ( world_vnum[ZCMD.arg3]->script ) )
						{
							ZONE_ERROR ( "Attempt to give variable to scriptless object" );
						}
						else
							add_var ( & ( world_vnum[ZCMD.arg3]->script->global_vars ),
							          ZCMD.sarg1, ZCMD.sarg2, ZCMD.arg2 );
						last_cmd = true;
					}
				}
			case 'B':       /* read an object then bury it */
				if ( real_room ( ZCMD.arg3 ) == NULL )
				{
					log ( "Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg3 );
					exit ( 1 );
				}
				if ( ( obj_index[ZCMD.arg1].number < ZCMD.arg2 ) )
				{
					if ( ZCMD.arg3 >= 0 )
					{
						if ( !get_obj_in_list_num
						        ( ZCMD.arg1, world_vnum[ZCMD.arg3]->contents ) )
						{
							bool lqc = load_qic_check ( ZCMD.arg1 );
							obj = read_object ( ZCMD.arg1, REAL );
							if ( obj && lqc )
							{
								obj_to_room ( obj, world_vnum[ZCMD.arg3] );
		                                                ripple_detect(cmd_room, ZCMD.arg1);
								if ( load_otrigger ( obj ) == -1 )
									obj = NULL;
								tobj = obj;
								if ( obj )
									SET_BIT_AR ( GET_OBJ_EXTRA ( obj ), ITEM_BURIED );


								last_cmd = obj ? true : false;
							}
							else
							{
								if ( obj )
								{
									purge_qic(ZCMD.arg1);
									extract_obj ( obj );
									tobj = obj = NULL;
									last_cmd = false;
								} else
								last_cmd = false;
							}
						}
						else
							last_cmd = false;

					}
					else
					{
						obj = read_object ( ZCMD.arg1, REAL );
						if ( obj )
						{
							IN_ROOM ( obj ) = NULL;
							SET_BIT_AR ( GET_OBJ_EXTRA ( obj ), ITEM_BURIED );
						}
						tobj = obj;
						last_cmd = true;
					}
				}
				else
					last_cmd = false;
				tmob = NULL;
				if ( last_cmd && tobj )
					oir.insert ( obj_rid ( cmd_room,GET_ID ( tobj ) ) );
				break;

			case 'Z':
				make_maze ( zone );
				break;
			default:
				ZONE_ERROR ( "unknown cmd in reset table; cmd disabled" );
				ZCMD.command = '*';
				break;
		}
	}

	zone_table[zone].age = 0;

	/* handle reset_wtrigger's */
	for ( vrm = zone_table[zone].bot;vrm <= zone_table[zone].top;vrm++ )
	{
		rm = world_vnum[vrm];
		if ( rm != NULL )
			reset_wtrigger ( rm );
	}
}



/* for use in reset_zone; return TRUE if zone 'nr' is free of PC's  */
int zone_is_empty ( zone_rnum zone_nr )
{
	Descriptor *i;

	for ( i = descriptor_list; i; i = i->next )
	{

		if ( STATE ( i ) != CON_PLAYING )
			continue;
		if ( IN_ROOM ( i->character ) == NULL )
			continue;
		if ( i->character->in_room->zone != zone_nr )
			continue;

		/*
		 * if an immortal has nohassle off, he counts as present
		 * added for testing zone reset triggers - Welcor
		 */
		if ( ( GET_LEVEL ( i->character ) >= LVL_IMMORT )
		        && ( PRF_FLAGGED ( i->character, PRF_NOHASSLE ) ) )
			continue;


		return ( 0 );
	}

	return ( 1 );
}







#if defined(KEY)
#undef KEY
#endif

#define KEY( literal, field, value )                        \
                    if ( !strcmp( word, literal ) )    \
                    {                        \
                        field  = value;           \
                        fMatch = TRUE;            \
                        break;                    \
                    }

/* provided to free strings */
#if defined(KEYS)
#undef KEYS
#endif

#define KEYS( literal, field, value )                       \
                    if ( !strcmp( word, literal ) )    \
                    {                        \
                        free_string(field);            \
                        field  = value;           \
                        fMatch = TRUE;            \
                        break;                    \
                    }


int store_to_char ( const char *name, Character *ch )
{
	int id, num = 0, num2 = 0, num3 = 0, num4 = 0, num5 = 0, i;
	gold_int num6 = 0;
	long l1, l2, l3, l4;
	FILE *fl;
	char filename[40];
	char cn;
	char buf[MAX_INPUT_LENGTH];
	char  line[MAX_INPUT_LENGTH + 1], tag[6];
	struct affected_type tmp_aff[MAX_AFFECT];
	plrindex_it pvti;
	int rec_count = 0;

	if ( ch == NULL )
	{
		log ( "SYSERR: store_to_char received null ch!" );
		return -1;
	}
	if ( !name || !*name )
	{
		log ( "SYSERR: store_to_char received null name!" );
		return -1;
	}
	try
	{
		id = pi.TableIndexByName ( name );
		pvti = pi.Begin() + id;

		snprintf ( filename, sizeof ( filename ), "%s/%c/%s",
		           PLR_PREFIX, * ( *pvti ).name, ( *pvti ).name );
		if ( ! ( fl = fopen ( filename, "r" ) ) )
		{

			new_mudlog ( NRM, LVL_GOD, TRUE, "SYSERR: Player file not found %s", filename );
			return -1;
		}
		else
		{
			while ( get_line ( fl, line ) && rec_count < 6 )
				rec_count++;
			rewind ( fl );
			if ( rec_count < 5 )
			{
				fclose ( fl );
#ifdef __unix__
				/* decompress if .gz file exists */
				snprintf ( filename, sizeof ( filename ), "%s/%c/%s%s", PLR_PREFIX, * ( *pvti ).name, ( *pvti ).name, ".bak.gz" );
				if ( ( fl = fopen ( filename, "r" ) ) != NULL )
				{
					fclose ( fl );

					snprintf ( buf, sizeof ( buf ),"gzip -dfq %s", filename );
					system ( buf );
					snprintf ( buf, sizeof ( buf ), "cp -f %s/%c/%s.bak %s/%c/%s"
					           ,PLR_PREFIX, * ( *pvti ).name, ( *pvti ).name
					           ,PLR_PREFIX, * ( *pvti ).name, ( *pvti ).name );
					system ( buf );
				}
#endif
				new_mudlog ( NRM, LVL_GOD, TRUE, "SYSERR: Player file %s empty, testing backup", filename );
				snprintf ( filename, sizeof ( filename ), "%s/%c/%s",
				           PLR_PREFIX, * ( *pvti ).name, ( *pvti ).name );
				if ( ! ( fl = fopen ( filename, "r" ) ) )
				{
					new_mudlog ( NRM, LVL_GOD, TRUE, "SYSERR: Backup failed" );
					return -1;
				}
				rec_count = 0;
				while ( get_line ( fl, line ) && rec_count < 6 )
					rec_count++;
				rewind ( fl );
				if ( rec_count < 5 )
				{
					fclose ( fl );
					return -1;
				}
			}

		}

	}
	catch ( MudException e )
	{
		log ( "Name: '%s' unfound in player index", name );
		return -1;
	}

	while ( get_line ( fl, line ) )
	{
		tag_argument ( line, tag );
		num = atoi ( line );
		num6 = atoll ( line );


		switch ( *tag )
		{
			case 'A':
				if ( !strcmp ( tag, "Ac  " ) )
					GET_AC ( ch ) = num;
				else if ( !strcmp ( tag, "Act " ) )
				{
					sscanf ( line, "%lu %lu %lu %lu", &l1, &l2, &l3, &l4 );
					PLR_FLAGS ( ch ) [0] = l1;
					PLR_FLAGS ( ch ) [1] = l2;
					PLR_FLAGS ( ch ) [2] = l3;
					PLR_FLAGS ( ch ) [3] = l4;
				}
				else if ( !strcmp ( tag, "Affs" ) )
				{
					i = 0;
					do
					{
						get_line ( fl, line );
						if ( sscanf ( line, "%d %lld %d %d %d", &num, &num6, &num3, &num4, &num5 ) == 5 )
						{
							tmp_aff[i].type = IRANGE ( TYPE_UNDEFINED, num, MAX_SKILLS );
							tmp_aff[i].expire = ( sec_to_time ( ( long ) num6 ) );
							tmp_aff[i].modifier = IRANGE ( -1000, num3, 1000 );
							tmp_aff[i].location = IRANGE ( 0, num4, MAX_APPLY );
							tmp_aff[i].bitvector = IRANGE ( 0, num5, MAX_AFF_APPLY );
							i++;
						}
					}
					while ( num != 0 );
				}
				else if ( !strcmp ( tag, "Alin" ) )
					GET_ALIGNMENT ( ch ) = num;
				else if ( !strcmp ( tag, "Awrd" ) )
				{
					if ( num > 1000 )
						num = 0;
					GET_AWARD ( ch ) = num;
				}
				else if ( !strcmp ( tag, "Aff " ) )
					sscanf ( line, "%u %u %u %u",
					         & ( ch->char_specials.saved.affected_by[0] ),
					         & ( ch->char_specials.saved.affected_by[1] ),
					         & ( ch->char_specials.saved.affected_by[2] ),
					         & ( ch->char_specials.saved.affected_by[3] ) );
				else if ( !strcmp ( tag, "AfkM" ) )
					AFK_MSG ( ch ) = str_dup ( line );
				else if ( !strcmp ( tag, "Age " ) )
					SPECIALS(ch)->age = num;
		
				break;

			case 'B':
				if ( !strcmp ( tag, "Badp" ) )
					GET_BAD_PWS ( ch ) = num;
				else if ( !strcmp ( tag, "Bank" ) )
					GET_BANK_GOLD ( ch ) = num6;
				else if ( !strcmp ( tag, "Brth" ) )
					ch->player.time.birth = num6;
				else if ( !strcmp ( tag, "BraT" ) )
				{
					if ( num > 100 )
					{
						new_mudlog ( NRM, LVL_GOD, TRUE, "%s has more than 100 brass tokens. Correcting to 0.", GET_NAME ( ch ) );
						num = 0;
					}
					GET_BRASS_TOKEN_COUNT ( ch ) = num;
				}
				else if ( !strcmp ( tag, "BroT" ) )
				{
					if ( num > 100 )
					{
						new_mudlog ( NRM, LVL_GOD, TRUE, "%s has more than 100 bronze tokens. Correcting to 0.", GET_NAME ( ch ) );
						num = 0;
					}
					GET_BRONZE_TOKEN_COUNT ( ch ) = num;
				}
				else if ( !strcmp ( tag, "BetO" ) )
					GET_BETTED_ON ( ch ) = num;
				else if ( !strcmp ( tag, "Body" ) )
					EXTRA_BODY ( ch ) = num;
				else if ( !strcmp ( tag, "Bpmt" ) )
				{
					if ( BPROMPT ( ch ) )
						free ( BPROMPT ( ch ) );
					BPROMPT ( ch ) = str_dup ( line );
				}
				break;

			case 'C':
				if ( !strcmp ( tag, "Cha " ) )
					ch->real_abils.cha = num;
				else if ( !strcmp ( tag, "Clas" ) )
					GET_CLASS ( ch ) = num;
				else if ( !strcmp ( tag, "Con " ) )
					ch->real_abils.con = num;
				else if ( !strcmp ( tag, "Clan" ) )
				{
					if ( find_clan_by_id ( num ) == -1 )
						GET_CLAN ( ch ) = 0;
					else GET_CLAN ( ch ) = num;
				}
				else if ( !strcmp ( tag, "ClRk" ) )
				{
					//GET_CLAN_RANK ( ch ) = ( *pvti ).rank < 0 ? ( *pvti ).rank : num;
					if ( GET_CLAN ( ch ) == 0 || num < 1 )
						GET_CLAN_RANK ( ch ) = 0;
					else
					{
						if ( num > clan[ find_clan_by_id ( GET_CLAN ( ch ) )].ranks )
							GET_CLAN_RANK ( ch ) = 0;
						else GET_CLAN_RANK ( ch ) = num;
					}
				}
				else if ( !strcmp ( tag, "Clns" ) )
				{
					if ( num > 100 )
						num = 0;
					GET_COOLNESS ( ch ) = num;
				}
				else if ( !strcmp ( tag, "Conv" ) )
					GET_CONVERSIONS ( ch ) = num;
				else if ( !strcmp ( tag, "CrPt" ) )
				{
					if ( num > 100 )
						num = 0;
					CREATE_POINTS ( ch ) = num;
				}
				else if ( !strcmp ( tag, "Csnp" ) )
					GET_CSNP_LVL ( ch ) = num;
				break;

			case 'D':
				if ( !strcmp ( tag, "Desc" ) )
				{
					char buf2[MAX_INPUT_LENGTH];
					strcpy ( buf2, "desc: store_to_char" );
					if ( ( ch->player.description = fread_string ( fl, buf2 ) ) == NULL )
						ch->player.description = strdup ( "Undefined" );
				}
				else if ( !strcmp ( tag, "Dex " ) )
					ch->real_abils.dex = num;
				else if ( !strcmp ( tag, "Drip" ) )
					GET_DETECTOR(ch) = num; 
				else if ( !strcmp ( tag, "Drnk" ) )
					GET_COND ( ch, DRUNK ) = num;
				else if ( !strcmp ( tag, "Drol" ) )
					GET_DAMROLL ( ch ) = num;
				else if ( !strcmp ( tag, "Deed" ) )
					GET_DEED_COUNT ( ch) = num;
				else if ( !strcmp ( tag, "DTC " ) )
				{
					if ( num > 100 )
						num = 0;
					GET_DT_CNT ( ch ) = num;
				}
				break;

			case 'E':
				if ( !strcmp ( tag, "Exp " ) )
					GET_EXP ( ch ) = num6;
				else if ( !strcmp ( tag, "ExpG" ) )
					GET_GROUP_EXP ( ch ) = num6;
				else if ( !strcmp ( tag, "Emai" ) )
				{
					char buf2[MAX_INPUT_LENGTH];
					strcpy ( buf2, "email: store to char" );
					if ( ( GET_EMAIL ( ch ) = fread_string ( fl, buf2 ) ) == NULL )
						GET_EMAIL ( ch ) = strdup ( "Undefined" );
				} 
                                else if ( !strcmp ( tag, "Etho"))
                                        GET_ETHOS(ch) = num;
				break;

			case 'F':
				if ( !strcmp ( tag, "Flag" ) )
					CMD_FLAGS ( ch ) = num;
				else if ( !strcmp ( tag, "Frez" ) )
					GET_FREEZE_LEV ( ch ) = num;
				break;

			case 'G':
				if ( !strcmp ( tag, "Gold" ) )
					GET_GOLD ( ch ) = num6;
				if ( !strcmp ( tag, "GolT" ) )
				{
					GET_GOLD_TOKEN_COUNT ( ch ) = num;
				}
				break;

			case 'H':
				if ( !strcmp ( tag, "Hit " ) )
				{
					sscanf ( line, "%d/%d", &num, &num2 );
					GET_HIT ( ch ) = num;
					GET_MAX_HIT ( ch ) = num2;
				}
				else if ( !strcmp ( tag, "Hite" ) )
					GET_HEIGHT ( ch ) = num;
				else if ( !strcmp ( tag, "Host" ) )
				{
					if ( ! ( ch->desc ) )
						ch->player_specials->host = line;
					else if ( !ch->desc->host.empty() )
						ch->player_specials->host = ch->desc->host;
				}
				else if ( !strcmp ( tag, "Hrol" ) )
					GET_HITROLL ( ch ) = num;
				else if ( !strcmp ( tag, "Hung" ) )
					GET_COND ( ch, FULL ) = num;
				break;

			case 'I':
				if ( !strcmp ( tag, "Id  " ) )
				{

					GET_IDNUM ( ch ) = num6;
				}
				else if ( !strcmp ( tag, "Int " ) )
					ch->real_abils.intel = num;
				else if ( !strcmp ( tag, "Invs" ) )
					GET_INVIS_LEV ( ch ) = num;
				else if ( !strcmp ( tag, "ImTi" ) )
					IMMTITLE ( ch ) = str_dup ( line );
				break;

			case 'K':
				if ( !strcmp ( tag, "KilC" ) )
				{
					if ( num > 2000000 )
						num = 0;
					GET_KILL_CNT ( ch ) = num;
				}
				break;

			case 'L':
				if ( !strcmp ( tag, "Last" ) )
				{
					if ( ch->desc )
						ch->player.time.logon = time ( 0 );
					else
						ch->player.time.logon = num6;
				}
				else if ( !strcmp ( tag, "Lern" ) )
					GET_PRACTICES ( ch ) = num;
				else if ( !strcmp ( tag, "Levl" ) )
					GET_LEVEL ( ch ) = num;
				else if ( ! ( strcmp ( tag, "Lnot" ) ) )
					SPECIALS ( ch )->last_note = num;
				else if ( ! ( strcmp ( tag, "Lida" ) ) )
					SPECIALS ( ch )->last_idea = num;
				else if ( ! ( strcmp ( tag, "Lpen" ) ) )
					SPECIALS ( ch )->last_penalty = num;
				else if ( ! ( strcmp ( tag, "Lnew" ) ) )
					SPECIALS ( ch )->last_news = num;
				else if ( ! ( strcmp ( tag, "Lcha" ) ) )
					SPECIALS ( ch )->last_changes = num;
				else if ( !strcmp ( tag, "LdRm" ) )
					GET_LOADROOM ( ch ) = num;
				else if ( !strcmp ( tag, "LocE" ) )
				{
					LOCKER_EXPIRE ( ch ) = ( time_t ) num6;

				}
				else if ( !strcmp ( tag, "LocL" ) )
					LOCKER_LIMIT ( ch ) = num;
				/*A little something by Thotter */
				else if ( !strcmp ( tag, "Lgim" ) )
					GET_LOGINMSG ( ch )    = strdup ( line );
				else if ( !strcmp ( tag, "Lgom" ) )
					GET_LOGOUTMSG ( ch )   = strdup ( line );
				/*Ends here */
				break;

			case 'M':
				if ( !strcmp ( tag, "Mana" ) )
				{
					sscanf ( line, "%d/%d", &num, &num2 );
					GET_MANA ( ch ) = num;
					GET_MAX_MANA ( ch ) = num2;
				}
				else if ( !strcmp ( tag, "Move" ) )
				{
					sscanf ( line, "%d/%d", &num, &num2 );
					GET_MOVE ( ch ) = num;
					GET_MAX_MOVE ( ch ) = num2;
				}
				else if ( !strcmp ( tag, "Msty" ) )
				{
					do
					{
						get_line ( fl, line );
						sscanf ( line, "%d %c", &num, &cn );
						if ( num != 0 )
							set_mastery ( ch, cn );
					}
					while ( num != 0 );
				}
				break;

			case 'N':
				if ( !strcmp ( tag, "Name" ) )
					ch->player.name = strdup ( line );
				else if ( !strcmp ( tag, "Nail" ) )
					GET_NAILS ( ch ) = num;
				else if ( !strcmp ( tag, "NewL" ) )
					GET_NEWBIE_STATUS ( ch ) = num;
				break;
			case 'O':
				if ( !strcmp ( tag, "Olev" ) )
					GET_ORIG_LEV ( ch ) = num;
				break;
			case 'P':
				switch ( LOWER ( tag[2] ) )   /* third letter */
				{
					default:
						break;
					case 'e':
						if ( !strcmp ( tag, "Pdef" ) )
							GET_PERM_EVASION ( ch ) = num;
						else if ( !strcmp ( tag, "Preg" ) )
							PREG ( ch ) = num;
						else if ( !strcmp ( tag, "PreT" ) )
							PRETITLE ( ch ) = str_dup ( line );
						else if ( !strcmp ( tag, "Pref" ) )
						{
							sscanf ( line, "%u %u %u %u",
							         & ( PRF_FLAGS ( ch ) [0] ),
							         & ( PRF_FLAGS ( ch ) [1] ),
							         & ( PRF_FLAGS ( ch ) [2] ),
							         & ( PRF_FLAGS ( ch ) [3] ) );
						}
						break;
					case 'f':
						if ( !strcmp ( tag, "Poff" ) )
							GET_PERM_ACCURACY ( ch )    = num;
						break;
					case 'h':
						if ( !strcmp ( tag, "PgHi" ) )
							PAGEHEIGHT ( ch ) = num;
						break;
					case 'i':
						if ( !strcmp ( tag, "Prip" ) )
							GET_PK_RIP ( ch ) = num;
						else if ( !strcmp ( tag, "PfIn" ) )
							POOFIN ( ch ) = str_dup ( line );
						break;
					case 'm':
						if ( !strcmp ( tag, "Prmp" ) )
						{
							if ( PROMPT ( ch ) )
								free ( PROMPT ( ch ) );
							PROMPT ( ch ) = str_dup ( line );
						}
						break;
					case 'n':
						if ( !strcmp ( tag, "Ppnt" ) )
						{
							if ( num > 1000 )
								num = 0;
							GET_PK_POINTS ( ch ) = num;
						}
						else if ( !strcmp ( tag, "Pcnt" ) )
						{
							if ( num > 1000 )
								num = 0;
							GET_PK_CNT ( ch ) = num;
						}
						break;
					case 'o':
						if ( !strcmp ( tag, "PfOt" ) )
							POOFOUT ( ch ) = str_dup ( line );
						break;
					case 'r':
						if ( !strcmp ( tag, "Part" ) )
							PARTNER ( ch ) = pi.IdByName ( line );
						break;
					case 's':
						if ( !strcmp ( tag, "Pass" ) )
							strcpy ( GET_PASSWD ( ch ), line );
						else if ( !strcmp ( tag, "Post" ) )
							GET_POSTS ( ch ) = num;
						break;
					case 't':
						if ( !strcmp ( tag, "Prtn" ) )
							PARTNER ( ch ) = num;
						else if ( !strcmp ( tag, "PetM" ) )
							ch->pet = num;
						break;
					case 'w':
						if ( !strcmp ( tag, "PgWd" ) )
							PAGEWIDTH ( ch ) = num;
						break;
					case 'y':
						if ( !strcmp ( tag, "Plyd" ) )
							ch->player.time.played = num;
						break;
				} /* end of 3rd letter switch */
				break;

			case 'R':
				switch ( LOWER ( tag[3] ) )
				{
					case '1':
						if ( !strcmp ( tag, "Rem1" ) )
							GET_REMORT ( ch ) = num;
						break;
					case '2':
						if ( !strcmp ( tag, "Rem2" ) )
							GET_REMORT_TWO ( ch ) = num;
						break;
					case '3':
						if ( !strcmp ( tag, "Rem3" ) )
							GET_REMORT_THREE ( ch ) = num;
						break;
					case 'a':
						if ( !strcmp ( tag, "Roma" ) )
							ROMANCE ( ch ) = num;
						break;
					case 'c':
						if ( !strcmp ( tag, "RipC" ) )
						{
							if ( num > 10000 )
								num = 0;
							GET_RIP_CNT ( ch ) = num;
						}
						break;
					case 'd':
						if ( !strcmp ( tag, "Rwrd" ) )
							GET_REWARD ( ch ) = num;
						break;
					case 'e':
						if ( !strcmp ( tag, "Race" ) )
							set_race ( ch, num );
						break;
					case 'm':
						if ( !strcmp ( tag, "Room" ) )
							GET_LOADROOM ( ch ) =  num;
						else if ( !strcmp ( tag, "RwTm" ) )
							SPECIALS ( ch )->last_reward = num;
						else if ( !strcmp ( tag, "RStm" ) )
							GET_REGEN_STAMINA ( ch ) = num;
						break;
					case 'n':
						if ( !strcmp ( tag, "RMan" ) )
							GET_REGEN_MANA ( ch ) = num;
						break;
					case 'p':
						if ( !strcmp ( tag, "RPgp" ) )
							GET_RP_GROUP ( ch ) = num;
						break;
					case 's':
						if ( !strcmp ( tag, "Rems" ) )
							REMORTS ( ch ) = num;
						break;
					case 't':
						if ( !strcmp ( tag, "RHit" ) )
							GET_REGEN_HIT ( ch ) = num;
						break;
					case 'v':
						if ( !strcmp ( tag, "RMov" ) )
							GET_REGEN_MOVE ( ch ) = num;
						break;

				}
				break;

			case 'S':
				if ( !strcmp ( tag, "Sex " ) )
					GET_SEX ( ch ) = num;
				else if ( !strcmp ( tag, "Sped" ) )
					AFF_SPEED ( ch ) = num;
				else if ( !strcmp ( tag, "Stam" ) )
				{
					sscanf ( line, "%d/%d", &num, &num2 );
					GET_STAMINA ( ch ) = num;
					if ( num2 >= 100 )
						GET_MAX_STAMINA ( ch ) = num2;
				}
				else if ( !strcmp ( tag, "Skil" ) || !strcmp(tag, "Skills") )
				{
					do
					{
						get_line ( fl, line );
						sscanf ( line, "%d %d %d 0", &num, &num2, &num3 );
						//if (!str_cmp("hesara", ch->player.name))
						//	log("%s - %d, %d, %d",ch->player.name, num, num2, num3);
						if ( num2 != 0 )
						{
							set_skill ( ch, num, num2 );
							//   if (!str_cmp("hesara", ch->player.name))
							//	   log("HasSkill - %d", SAVED(ch).HasSkill(num));
							set_skill_wait ( ch, num, num3 );
						}
					}
					while ( num2 != 0 );
				}
				else if ( !strcmp ( tag, "Subs" ) )
				{
					sub_list *s;
					do
					{
						get_line ( fl, line );
						if ( sscanf ( line, "%d %d %d", &num, &num2, &num3 ) == 3 )
						{
							if ( num != 0 )
							{
								s = new sub_list();
								s->subskill = ( enum subskill_list ) ( num );
								s->status = ( enum sub_status_toggle ) num3;
								s->learn = num2;
								SAVED ( ch ).UpdateSub ( s );
							}
						}
					}
					while ( num != 0 );
				}
				else if ( !strcmp ( tag, "Str " ) )
				{
					sscanf ( line, "%d/%d", &num, &num2 );
					ch->real_abils.str = num;
					ch->real_abils.str_add = num2;
				}
				else if ( !strcmp ( tag, "SilT" ) )
				{
					if ( num > 100 )
					{
						new_mudlog ( NRM, LVL_GOD, TRUE, "%s has more than 100 silver tokens. Correcting to 0.", GET_NAME ( ch ) );
						num = 0;
					}
					GET_SILVER_TOKEN_COUNT ( ch ) = num;
				}
				break;

			case 'T':
				if ( !strcmp ( tag, "Thir" ) )
					GET_COND ( ch, THIRST ) = num;
				else if ( !strcmp ( tag, "Thr1" ) )
					GET_SAVE ( ch, 0 ) = num;
				else if ( !strcmp ( tag, "Thr2" ) )
					GET_SAVE ( ch, 1 ) = num;
				else if ( !strcmp ( tag, "Thr3" ) )
					GET_SAVE ( ch, 2 ) = num;
				else if ( !strcmp ( tag, "Thr4" ) )
					GET_SAVE ( ch, 3 ) = num;
				else if ( !strcmp ( tag, "Thr5" ) )
					GET_SAVE ( ch, 4 ) = num;
				else if ( !strcmp ( tag, "Titl" ) )
					ch->player.title = strdup ( line );
				else if ( !strcmp ( tag, "Tir1" ) )
					GET_CLASS_TIER ( ch ) = num;
				else if ( !strcmp ( tag, "Tir2" ) )
					GET_REMORT_TIER ( ch ) = num;
				else if ( !strcmp ( tag, "Tir3" ) )
					GET_REMORT_TWO_TIER ( ch ) = num;
				else if ( !strcmp ( tag, "Tir4" ) )
					GET_REMORT_THREE_TIER ( ch ) = num;
				else if ( !strcmp ( tag, "Trad" ) )
					TRADEPOINTS ( ch ) = num;
				break;

			case 'W':
				if ( !strcmp ( tag, "Wate" ) )
					GET_WEIGHT ( ch ) = num;
				else if ( !strcmp ( tag, "Wimp" ) )
					GET_WIMP_LEV ( ch ) = num;
				else if ( !strcmp ( tag, "Wis " ) )
					ch->real_abils.wis = num;
				else if ( !strcmp ( tag, "Wire" ) )
					GET_WIRE ( ch ) = num;
				break;

			default:
				log ( "SYSERR: Unknown tag %s in pfile %s", tag, name );
		}
	}

	fclose ( fl );


	if ( PRETITLE ( ch ) && !allowed_pretitle ( ch ) )
	{
		free ( PRETITLE ( ch ) );
		PRETITLE ( ch ) = NULL;
	}

	//ch->real_abils = ch->aff_abils;

	for ( i = 0; i < MAX_AFFECT && tmp_aff[i].type != 0; i++ )
	{
		if ( tmp_aff[i].type )
			affect_to_char ( ch, &tmp_aff[i] );
	}

	if ( !TEMP_LOAD_CHAR && pi.IsSet ( id, PINDEX_FIXSKILLS ) )
	{
		fixskills ( ch );
		pi.UnsetFlags ( id, PINDEX_FIXSKILLS );
		pi.Save();
	}

	if ( GET_LEVEL ( ch ) >= LVL_IMMORT )
	{
		for ( i = 1; i <= MAX_SKILLS; i++ )
			set_skill ( ch, i, 100 );
		GET_COND ( ch, FULL ) = -1;
		GET_COND ( ch, THIRST ) = -1;
		GET_COND ( ch, DRUNK ) = -1;
	}
	ch->affect_total();
	return 1;
}

void ripple_detect(room_vnum location, int object) 
{
Descriptor *r;
Descriptor *next;

				for (r = descriptor_list; r; r = next) {
                                  next = r->next;
                                  if (!r->connected && r->character &&
                                  !PLR_FLAGGED(r->character, PLR_WRITING) && obj_index[object].qic) {
                                  r->character->Send( "A new artifact sends ripples across the %s Era. ",  dimension_types[zone_table[GET_ROOM_ZONE(world_vnum[location])].dimension]);
            //                      if ((GET_DETECTOR(r->character) == 1) && (TRADEPOINTS(r->character) >= 100))  {
     	//Commenting out TP Cost to Arti Detector for Now	 TRADEPOINTS(r->character) -= 100;
                                  r->character->Send( "[%s]", zone_table[GET_ROOM_ZONE(world_vnum[location])].name);
//           }
				  r->character->Send( "\r\n");

      }
    } 
}
void Character::LoadKillList()
{
	char filename[MAX_INPUT_LENGTH], line[READ_SIZE];
	FILE *fl;
	int t[2];
	long tl[2];
	int id;

	return;//disabled for the moment!

	if ( ( id = pi.TableIndexById ( GET_IDNUM ( this ) ) ) <=0 )
	{
		log ( "bad index passed to load_killlist" );
		return;
	}
	if ( id < 0 || id >= pi.Size() )
		return;
	if ( SPECIALS ( this ) == NULL || SPECIALS ( this ) == &dummy_mob )
		return;


	snprintf ( filename, sizeof ( filename ), "%s/%c/%s%s",
	           PLR_PREFIX, *pi.NameByIndex ( id ), pi.NameByIndex ( id ), ".kills" );
	if ( ! ( fl = fopen ( filename, "r" ) ) )
		return;
	log ( "Loading Kill List %s", filename );
	while ( get_line ( fl, line ) )
	{
		if ( sscanf ( line, "%d %d %ld %ld",t, t+1, tl, tl+1 ) != 4 )
			continue;
		log ( "%d %d %ld %ld", t[0], t[1], tl[1], tl[0] );
		SPECIALS ( this )->SetKill ( t[0], t[1], tl[1], tl[0] );
	}
	fclose ( fl );
	return;
}
void Character::SaveKillList()
{
	int id;
	char filename[MAX_INPUT_LENGTH];
	return;
	FILE *fl;
	id = pi.TableIndexById ( GET_IDNUM ( this ) );
	if ( !SPECIALS ( this ) || SPECIALS ( this )->KillsCount() == 0 || id < 0 || id >= pi.Size() )
		return;
	snprintf ( filename, sizeof ( filename ), "%s/%c/%s%s",
	           PLR_PREFIX, *pi.NameByIndex ( id ), pi.NameByIndex ( id ), ".kills" );
	if ( ! ( fl = fopen ( filename, "w" ) ) )
	{
		log ( "Can't open file %s for writing", filename );
		return;
	}

	for ( kill_map::iterator it = SPECIALS ( this )->KillsBegin(); it != SPECIALS ( this )->KillsEnd(); it++ )
		fprintf ( fl, "%d %d %ld %ld\n", ( it->second )->vnum, ( it->second )->count, ( it->second )->first, ( it->second )->last );

	fclose ( fl );
	return;
}


/* remove ^M's from file output */
/* There may be a similar function in Oasis (and I'm sure
   it's part of obuild).  Remove this if you get a
   multiple definition error or if it you want to use a
   substitute
*/
void kill_ems ( char *str )
{
	char *ptr1, *ptr2;
	//char *tmp;
	//tmp = str;
	ptr1 = str;
	ptr2 = str;

	while ( *ptr1 )
	{
		if ( ( * ( ptr2++ ) = * ( ptr1++ ) ) == '\r' )
			if ( *ptr1 == '\r' )
				ptr1++;
	}
	*ptr2 = '\0';
}

void char_to_store ( Character *ch )
{
	FILE *fl;
	char outname[80], bits[127], buf[MAX_STRING_LENGTH];
	char tempname[MAX_INPUT_LENGTH];
	int i, id, save_index = FALSE, thing = 0;
	struct affected_type *aff, tmp_aff[MAX_AFFECT];

	if ( IS_NPC ( ch ) || GET_NAME ( ch ) == NULL )
	{
		log ( "SYSERR: Massive fuck up in char to store." );
		return;
	}
	//if (!ch->desc)
	//return;

	for ( i = 0; ( * ( bits + i ) = LOWER ( * ( GET_NAME ( ch ) + i ) ) ); i++ )
		;


	snprintf ( outname, sizeof ( outname ), "%s/%c/%s%s", PLR_PREFIX, *bits, bits, PLR_SUFFIX );

	snprintf ( tempname, sizeof ( tempname ), "%s%s", outname, ".tmp" );
	if ( !*tempname )
		return;
	else if ( ! ( fl = fopen ( tempname, "w" ) ) )
	{
		new_mudlog ( NRM, LVL_GOD, TRUE, "SYSERR: Couldn't open temp player file %s for write",
		             tempname );
		return;
	}
	/* remove affects from eq and spells (from char_to_store) */
	/* Unaffect everything a character can be affected by */
	ch->MakeNaked();

	for ( aff = ch->affected, i = 0; i < MAX_AFFECT; i++ )
	{
		if ( aff )
		{
			tmp_aff[i] = *aff;
			tmp_aff[i].next = 0;
			aff = aff->next;
		}
		else
		{
			tmp_aff[i].type = 0;
			tmp_aff[i].expire = 0;
			tmp_aff[i].modifier = 0;
			tmp_aff[i].location = 0;
			tmp_aff[i].bitvector = 0;
			tmp_aff[i].next = 0;
		}
	}

	while ( ch->affected )
		ch->affect_remove ( ch->affected );
	if ( ( i >= MAX_AFFECT ) && aff && aff->next )
		log ( "SYSERR: WARNING: OUT OF STORE ROOM FOR AFFECTED TYPES!!!" );

	ch->aff_abils = ch->real_abils;

	if ( GET_NAME ( ch ) )
		fprintf ( fl, "Name: %s\n", GET_NAME ( ch ) );
	if ( GET_PASSWD ( ch ) )
		fprintf ( fl, "Pass: %s\n", GET_PASSWD ( ch ) );
	if ( GET_TITLE ( ch ) != NULL )
		fprintf ( fl, "Titl: %s\n", GET_TITLE ( ch ) );
	if ( AFK_MSG ( ch ) )
		fprintf ( fl, "AfkM: %s\n", AFK_MSG ( ch ) );
	if ( ch->player.description != NULL )
	{
		strcpy ( buf, ch->player.description );
		strip_cr ( buf );
		fprintf ( fl, "Desc:\n%s~\n", buf );
	}
	if ( POOFIN ( ch ) )
		fprintf ( fl, "PfIn: %s\n", POOFIN ( ch ) );
	if ( POOFOUT ( ch ) )
		fprintf ( fl, "PfOt: %s\n", POOFOUT ( ch ) );
	if ( IMMTITLE ( ch ) )
		fprintf ( fl, "ImTi: %s\n", IMMTITLE ( ch ) );
	fprintf ( fl, "Sex : %d\n", GET_SEX ( ch ) );
	fprintf ( fl, "Clas: %d\n", GET_CLASS ( ch ) );
	fprintf ( fl, "Race: %d\n", GET_RACE ( ch ) );
	fprintf ( fl, "Levl: %d\n", GET_LEVEL ( ch ) );
	thing = ch->player.time.played;
	thing += time ( 0 ) - ch->player.time.logon;
	fprintf ( fl, "Brth: %ld\n", ch->player.time.birth );
	fprintf ( fl, "Plyd: %d\n", thing );  // + (time(0) - ch->player.time.logon)));
	if ( ch->desc )
		fprintf ( fl, "Last: %ld\n", time ( 0 ) );
	else
		fprintf ( fl, "Last: %ld\n", ch->player.time.logon );
	if ( ( ch->desc ) && ( !ch->desc->host.empty() ) )
		fprintf ( fl, "Host: %s\n", ch->desc->host.c_str() );
	else if ( !ch->player_specials->host.empty() )
		fprintf ( fl, "Host: %s\n", ch->player_specials->host.c_str() );
	fprintf ( fl, "Hite: %d\n", GET_HEIGHT ( ch ) );
	fprintf ( fl, "Wate: %d\n", GET_WEIGHT ( ch ) );
	fprintf ( fl, "Alin: %d\n", GET_ALIGNMENT ( ch ) );
	fprintf ( fl, "Id  : %ld\n", GET_IDNUM ( ch ) );
	fprintf ( fl, "Act : %u %u %u %u\n", PLR_FLAGS ( ch ) [0], PLR_FLAGS ( ch ) [1],
	          PLR_FLAGS ( ch ) [2], PLR_FLAGS ( ch ) [3] );

	fprintf ( fl, "Aff : %u %u %u %u\n", AFF_FLAGS ( ch ) [0], AFF_FLAGS ( ch ) [1],
	          AFF_FLAGS ( ch ) [2], AFF_FLAGS ( ch ) [3] );
	fprintf ( fl, "Thr1: %d\n", GET_SAVE ( ch, 0 ) );
	fprintf ( fl, "Thr2: %d\n", GET_SAVE ( ch, 1 ) );
	fprintf ( fl, "Thr3: %d\n", GET_SAVE ( ch, 2 ) );
	fprintf ( fl, "Thr4: %d\n", GET_SAVE ( ch, 3 ) );
	fprintf ( fl, "Thr5: %d\n", GET_SAVE ( ch, 4 ) );
	if ( GET_LEVEL ( ch ) < LVL_IMMORT )
	{
		if ( SAVED ( ch ).CountSkills() > 0 )
		{
		//	fprintf ( fl, "Skil:\n" );
			fprintf ( fl, "Skills:\n" );
			for ( skills_map::iterator it = SAVED ( ch ).SkillsBegin(); it != SAVED ( ch ).SkillsEnd();it++ )
			{
//				if ( knows_spell ( ch, ( it->second )->skill ) && ( it->second )->learn > 0 )
				if ( ( it->second )->learn > 0 )
					fprintf ( fl, "%d %d %d 0\n", ( it->second )->skill, ( it->second )->learn, ( it->second )->wait );
			}
			fprintf ( fl, "0 0 0 0\n" );
		}
		fprintf ( fl, "Subs:\n" );
		for ( subs_map::iterator it = SAVED ( ch ).SubsBegin();
		        it != SAVED ( ch ).SubsEnd(); it++ )
		{
			if ( ( it->second )->learn > 0 )
				fprintf ( fl, "%d %d %d\n", ( it->second )->subskill, ( it->second )->learn, ( it->second )->status );
		}
		fprintf ( fl, "0 0 0\n" );
	}
	if ( CMD_FLAGS ( ch ) )
		fprintf ( fl, "Flag: %ld\n", CMD_FLAGS ( ch ) );
	fprintf ( fl, "Wimp: %d\n", GET_WIMP_LEV ( ch ) );
	fprintf ( fl, "Frez: %d\n", GET_FREEZE_LEV ( ch ) );
	if ( GET_INVIS_LEV ( ch ) )
		fprintf ( fl, "Invs: %d\n", GET_INVIS_LEV ( ch ) );
	fprintf ( fl, "Room: %d\n", GET_LOADROOM ( ch ) );
	//   sprintbits(PRF_FLAGS(ch), bits);
	fprintf ( fl, "Pref: %u %u %u %u\n", PRF_FLAGS ( ch ) [0], PRF_FLAGS ( ch ) [1],
	          PRF_FLAGS ( ch ) [2], PRF_FLAGS ( ch ) [3] );
	fprintf ( fl, "Badp: %d\n", GET_BAD_PWS ( ch ) );
	fprintf ( fl, "Hung: %d\n", GET_COND ( ch, FULL ) );
	fprintf ( fl, "Thir: %d\n", GET_COND ( ch, THIRST ) );
	fprintf ( fl, "Drnk: %d\n", GET_COND ( ch, DRUNK ) );
	if ( GET_PRACTICES ( ch ) )
		fprintf ( fl, "Lern: %d\n", GET_PRACTICES ( ch ) );
	fprintf ( fl, "Str : %d/%d\n", GET_STR ( ch ), GET_ADD ( ch ) );
	fprintf ( fl, "Int : %d\n", GET_INT ( ch ) );
	fprintf ( fl, "Wis : %d\n", GET_WIS ( ch ) );
	fprintf ( fl, "Dex : %d\n", GET_DEX ( ch ) );
	fprintf ( fl, "Con : %d\n", GET_CON ( ch ) );
	fprintf ( fl, "Cha : %d\n", GET_CHA ( ch ) );
	fprintf ( fl, "Hit : %d/%d\n", GET_HIT ( ch ), GET_MAX_HIT ( ch ) );
	fprintf ( fl, "Mana: %d/%d\n", GET_MANA ( ch ), GET_MAX_MANA ( ch ) );
	fprintf ( fl, "Move: %d/%d\n", GET_MOVE ( ch ), GET_MAX_MOVE ( ch ) );
	fprintf ( fl, "Stam: %d/%d\n", GET_STAMINA ( ch ), GET_MAX_STAMINA ( ch ) );
	fprintf ( fl, "RHit: %d\n", GET_REGEN_HIT ( ch ) );
	fprintf ( fl, "RMan: %d\n", GET_REGEN_MANA ( ch ) );
	fprintf ( fl, "RMov: %d\n", GET_REGEN_MOVE ( ch ) );
	fprintf ( fl, "RStm: %d\n", GET_REGEN_STAMINA ( ch ) );
	fprintf ( fl, "Ac  : %d\n", 100 );
	if ( GET_GOLD ( ch ) )
		fprintf ( fl, "Gold: %lld\n", GET_GOLD ( ch ) );
	if ( GET_BANK_GOLD ( ch ) )
		fprintf ( fl, "Bank: %lld\n", GET_BANK_GOLD ( ch ) );
	fprintf ( fl, "Exp : %lld\n", GET_EXP ( ch ) );
	fprintf ( fl, "ExpG: %lld\n", GET_GROUP_EXP ( ch ) );
	fprintf ( fl, "Hrol: %d\n", 0 );
	fprintf ( fl, "Drol: %d\n", 0 );
	fprintf ( fl, "RPgp: %d\n", GET_RP_GROUP ( ch ) );
	fprintf ( fl, "Rems: %d\n", REMORTS ( ch ) );
	fprintf ( fl, "Rem1: %d\n", GET_REMORT ( ch ) );
	fprintf ( fl, "Rem2: %d\n", GET_REMORT_TWO ( ch ) );
	fprintf ( fl, "Rem3: %d\n", GET_REMORT_THREE ( ch ) );
	if ( PARTNER ( ch ) != NOBODY )
	{
		fprintf ( fl, "Roma: %d\n", ROMANCE ( ch ) );
		fprintf ( fl, "Prtn: %ld\n", PARTNER ( ch ) );
	}
	fprintf ( fl, "Preg: %d\n", PREG ( ch ) );
	if ( ch->pet != NOBODY )
		fprintf ( fl, "PetM: %d\n", ch->pet );
	if ( PRETITLE ( ch ) )
		fprintf ( fl, "PreT: %s\n", PRETITLE ( ch ) );
	if ( GET_CLAN ( ch ) )
	{
		fprintf ( fl, "Clan: %d\n", GET_CLAN ( ch ) );
		fprintf ( fl, "ClRk: %d\n", GET_CLAN_RANK ( ch ) );
	}
	if ( GET_BRASS_TOKEN_COUNT ( ch ) )
		fprintf ( fl, "BraT: %d\n", GET_BRASS_TOKEN_COUNT ( ch ) );
	if ( GET_BRONZE_TOKEN_COUNT ( ch ) )
		fprintf ( fl, "BroT: %d\n", GET_BRONZE_TOKEN_COUNT ( ch ) );
	if ( GET_SILVER_TOKEN_COUNT ( ch ) )
		fprintf ( fl, "SilT: %d\n", GET_SILVER_TOKEN_COUNT ( ch ) );
	if ( GET_GOLD_TOKEN_COUNT ( ch ) )
		fprintf ( fl, "GolT: %d\n", GET_GOLD_TOKEN_COUNT ( ch ) );
	if ( GET_RIP_CNT ( ch ) )
		fprintf ( fl, "RipC: %d\n", GET_RIP_CNT ( ch ) );
	if ( GET_KILL_CNT ( ch ) )
		fprintf ( fl, "KilC: %d\n", GET_KILL_CNT ( ch ) );
	if ( GET_DT_CNT ( ch ) )
		fprintf ( fl, "DTC : %d\n", GET_DT_CNT ( ch ) );
	if ( GET_BETTED_ON ( ch ) != NOBODY )
		fprintf ( fl, "BetO: %d\n", GET_BETTED_ON ( ch ) );
	if ( GET_CLASS_TIER ( ch ) )
		fprintf ( fl, "Tir1: %d\n", ( int ) GET_CLASS_TIER ( ch ) );
	if ( GET_REMORT_TIER ( ch ) )
		fprintf ( fl, "Tir2: %d\n", ( int ) GET_REMORT_TIER ( ch ) );
	if ( GET_REMORT_TWO_TIER ( ch ) )
		fprintf ( fl, "Tir3: %d\n", ( int ) GET_REMORT_TWO_TIER ( ch ) );
	if ( GET_REMORT_THREE_TIER ( ch ) )
		fprintf ( fl, "Tir4: %d\n", ( int ) GET_REMORT_THREE_TIER ( ch ) );
	if ( TRADEPOINTS ( ch ) )
		fprintf ( fl, "Trad: %d\n", ( int ) TRADEPOINTS ( ch ) );
	if ( AFF_SPEED ( ch ) )
		fprintf ( fl, "Sped: %d\n", AFF_SPEED ( ch ) );
	if ( GET_COOLNESS ( ch ) )
		fprintf ( fl, "Clns: %d\n", GET_COOLNESS ( ch ) );
	fprintf ( fl, "LdRm: %d\n", GET_LOADROOM ( ch ) );
	if ( GET_PK_CNT ( ch ) )
		fprintf ( fl, "Pcnt: %d\n",GET_PK_CNT ( ch ) );
	if ( GET_PK_RIP ( ch ) )
		fprintf ( fl, "Prip: %d\n",GET_PK_RIP ( ch ) );
	if ( GET_PK_POINTS ( ch ) )
		fprintf ( fl, "Ppnt: %d\n",GET_PK_POINTS ( ch ) );
	if ( GET_POSTS ( ch ) )
		fprintf ( fl, "Post: %d\n",GET_POSTS ( ch ) );
	if ( GET_NAILS ( ch ) )
		fprintf ( fl, "Nail: %d\n",GET_NAILS ( ch ) );
	if ( GET_WIRE ( ch ) )
		fprintf ( fl, "Wire: %d\n",GET_WIRE ( ch ) );
	if ( GET_PERM_ACCURACY ( ch ) )
		fprintf ( fl, "Poff: %d\n",GET_PERM_ACCURACY ( ch ) );
	if ( GET_PERM_EVASION ( ch ) )
		fprintf ( fl, "Pdef: %d\n",GET_PERM_EVASION ( ch ) );
	if ( GET_ORIG_LEV ( ch ) )
		fprintf ( fl, "Olev: %d\n", GET_ORIG_LEV ( ch ) );
	if ( GET_CONVERSIONS ( ch ) )
		fprintf ( fl, "Conv: %d\n", GET_CONVERSIONS ( ch ) );
	fprintf ( fl, "Lnot: %ld\n",SPECIALS ( ch )->last_note );
	fprintf ( fl, "Lida: %ld\n",SPECIALS ( ch )->last_idea );
	fprintf ( fl, "Lpen: %ld\n",SPECIALS ( ch )->last_penalty );
	fprintf ( fl, "Lnew: %ld\n",SPECIALS ( ch )->last_news );
	fprintf ( fl, "Lcha: %ld\n",SPECIALS ( ch )->last_changes );
	if ( GET_AWARD ( ch ) )
		fprintf ( fl, "Awrd: %d\n", GET_AWARD ( ch ) + GET_REWARD ( ch ) );
	if ( SPECIALS ( ch )->last_reward )
		fprintf ( fl, "RwTm: %d\n", SPECIALS ( ch )->last_reward );
	if ( PROMPT ( ch ) && *PROMPT ( ch ) )
		fprintf ( fl, "Prmp: %s\n", PROMPT ( ch ) );
	if ( BPROMPT ( ch ) && *BPROMPT ( ch ) )
		fprintf ( fl, "Bpmt: %s\n", BPROMPT ( ch ) );
	if ( EXTRA_BODY ( ch ) )
		fprintf ( fl, "Body: %d\n", EXTRA_BODY ( ch ) );
	fprintf ( fl, "PgWd: %d\n", PAGEWIDTH ( ch ) );
	fprintf ( fl, "PgHi: %d\n", PAGEHEIGHT ( ch ) );
	if ( LOCKER_EXPIRE ( ch ) )
		fprintf ( fl, "LocE: %lld\n", ( gold_int ) LOCKER_EXPIRE ( ch ) );
	if ( LOCKER_LIMIT ( ch ) )
		fprintf ( fl, "LocL: %d\n", LOCKER_LIMIT ( ch ) );
	if ( GET_NEWBIE_STATUS ( ch ) != -1 )
		fprintf ( fl, "NewL: %d\n", GET_NEWBIE_STATUS ( ch ) );
	if ( GET_EMAIL ( ch ) && *GET_EMAIL ( ch ) )
		fprintf ( fl, "Emai: \n%s~\n", GET_EMAIL ( ch ) );
	//Ethos and Arti Detection by Once 

	if ( GET_DEED_COUNT (ch))
		fprintf ( fl, "Deed: %d\n", GET_DEED_COUNT(ch));

        if ( GET_ETHOS (ch))
                fprintf ( fl, "Etho: %d\n", GET_ETHOS(ch));
	if (GET_DETECTOR(ch))
		fprintf ( fl, "Drip: %d\n", GET_DETECTOR(ch));

	/*Here follows a little something made by Thotter */
	if ( GET_LOGINMSG ( ch ) )
		fprintf ( fl, "Lgim: %s\n", GET_LOGINMSG ( ch ) );
	if ( GET_LOGOUTMSG ( ch ) )
		fprintf ( fl, "Lgom: %s\n", GET_LOGOUTMSG ( ch ) );
	/*end :P */
	/* here some more :P */
	if ( GET_CSNP_LVL ( ch ) )
		fprintf ( fl, "Csnp: %d\n", GET_CSNP_LVL ( ch ) );
	/* the end! *bows to the massive cheering from the audience* Read 1984! */
	/** hoho thots :-P - mord**/
	if ( SPECIALS(ch)->age != -1)
		fprintf ( fl, "Age : %d\n", SPECIALS(ch)->age);
	if ( CREATE_POINTS ( ch ) )
		fprintf ( fl, "CrPt: %d\n", CREATE_POINTS ( ch ) );
	{
		int ism = 0;
		for ( i = 0; i < NUM_CLASSES; i++ )
			if ( GET_MASTERY ( ch, i ) )
			{
				ism++;
				if ( ism == 1 )
					fprintf ( fl, "Msty:\n" );
				fprintf ( fl, "1 %c\n", *pc_class_types[i] );
			}
		if ( ism )
			fprintf ( fl, "0 0\n" );
	}

	fprintf ( fl, "Affs:\n" );
	for ( i = 0; i < MAX_AFFECT; i++ )
	{
		aff = &tmp_aff[i];
		if ( aff->type && aff->expire != -2 )
			fprintf ( fl, "%d %ld %d %d %d\n", aff->type, time_to_sec ( aff->expire ),
			          aff->modifier, aff->location, ( int ) aff->bitvector );
	}
	i = fprintf ( fl, "0 0 0 0 0\n" );
	fclose ( fl );

	/* now that we know there is enough space to put the file, save it
	   If it errors, bail - mord
	*/
	if ( i == -1 )
	{
		if ( remove
		        ( tempname ) == -1 )
		{
			new_mudlog ( NRM, LVL_GOD, TRUE, "unable to remove temp file: %s", tempname );
			log ( "unable to remove temp file: %s", tempname );
		}
	}
	else if ( rename ( tempname, outname ) == -1 )
		core_dump();
	/** put char back together **/

	for ( i = 0; i < MAX_AFFECT; i++ )
	{
		if ( tmp_aff[i].type )
			affect_to_char ( ch, &tmp_aff[i] );
	}

ch->MakeClothed();

	if ( GET_IDNUM ( ch ) <= 0 || GET_IDNUM ( ch ) > pi.TopIdNum )
	{
		GET_IDNUM ( ch ) = GET_ID ( ch ) =  pi.TopIdNum++;
		addChToLookupTable ( GET_ID ( ch ), ch );
	}

	try
	{
		id = pi.TableIndexByName ( GET_NAME ( ch ) );
	}
	catch ( MudException e )
	{
		return;
	}
	if ( pi.ClanByIndex ( id ) != GET_CLAN ( ch ) )
	{
		save_index = TRUE;
		pi.SetClan ( id, GET_CLAN ( ch ) );
	}
	if ( pi.RankByIndex ( id ) != GET_CLAN_RANK ( ch ) )
	{
		save_index = TRUE;
		pi.SetRank ( id, GET_CLAN_RANK ( ch ) );
	}
	if ( pi.LevelByIndex ( id ) != GET_LEVEL ( ch ) )
	{
		save_index = TRUE;
		pi.SetLevel ( id, GET_LEVEL ( ch ) );
	}
	if ( pi.LastByIndex ( id ) != ch->player.time.logon )
	{
		save_index = TRUE;
		pi.SetLast ( id, ch->player.time.logon );
	}
	if ( pi.IdByIndex ( id ) != GET_IDNUM ( ch ) )
	{
		save_index = TRUE;
		pi.SetId ( id, GET_IDNUM ( ch ) );
		pi.SetAcc ( id, GET_IDNUM ( ch ) );
	}
	if ( pi.GoldByIndex ( id ) != ch->Gold ( 0, GOLD_ALL ) )
	{
		save_index = TRUE;
		pi.SetGold ( id, ch->Gold ( 0, GOLD_ALL ) );
	}
	if ( pi.TokensByIndex ( id ) != GET_GOLD_TOKEN_COUNT ( ch ) )
	{
		save_index = TRUE;
		pi.SetTokens ( id, GET_GOLD_TOKEN_COUNT ( ch ) );
	}

	i = pi.FlagsByIndex ( id );
	if ( PLR_FLAGGED ( ch, PLR_DELETED ) )
		pi.SetFlags ( id, PINDEX_DELETED );
	else
		pi.UnsetFlags ( id, PINDEX_DELETED );
	if ( PLR_FLAGGED ( ch, PLR_NODELETE ) || PLR_FLAGGED ( ch, PLR_CRYO ) )
		pi.SetFlags ( id, PINDEX_NODELETE );
	else
		pi.UnsetFlags ( id, PINDEX_NODELETE );
	if ( pi.FlagsByIndex ( id ) != i || save_index )
		pi.Save();
}


void tag_argument ( char *argument, char *tag )
{
	char *tmp = argument, *ttag = tag, *wrt = argument;
	int i;

	for ( i = 0; i < 4; i++ )
		* ( ttag++ ) = * ( tmp++ );
	*ttag = '\0';

	while ( *tmp == ':' || *tmp == ' ' )
		tmp++;

	while ( *tmp )
		* ( wrt++ ) = * ( tmp++ );
	*wrt = '\0';
}
/*
void sprintbits(long vektor, char *outstring) {
    int i;
    char flags[53] =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

    strcpy(outstring, "");
    for (i = 0; i < 32; i++) {
        if (vektor & 1) {
            *outstring = flags[i];
            outstring++;
        }
        vektor >>= 1;
    }
    *outstring = 0;
}*/
/*
** print out the letter codes pertaining to the bits set in 'data'
*/
void sprintbits ( int data, char *dest )
{
	int i;
	char *p = dest;

	for ( i=0; i<32; i++ )
	{
		if ( data & ( 1<<i ) )
		{
			*p = ( ( i<=25 ) ? ( 'a'+i ) : ( 'A'+i ) );
			p++;
		}
	}
	*p = '\0';
}

void save_etext ( Character *ch )
{
	/* this will be really cool soon */

}





void clearAllZones()
{
	for ( vector<Zone>::iterator z = zone_table.begin();z != zone_table.end();z++ )
		( *z ).num_players = 0;
}
/************************************************************************
*  funcs of a (more or less) general utility nature              *
************************************************************************/
char *fread_string ( FILE *fl, const char *error )
{
	char tmp[READ_SIZE];
	int done = 0;
	size_t tlen = 0;
	string str, buf;
	int cr = 0, cr_found = FALSE;

	buf = "";
	do
	{
		if ( !fgets ( tmp, READ_SIZE, fl ) )
		{
			log ( "SYSERR: fread_string: format error at or near %s", error );
			exit ( 1 );
		}
		tlen = strlen ( tmp );
		/* If there is a '~', end the string; else put an "\r\n" over the '\n'. */
		/* now only removes trailing ~'s -- Welcor */

		if ( tlen > 0 )
		{
			int x;
			cr_found = FALSE;
			for ( x = tlen - 1; x >= 0 && ( tmp[x]=='\r' || tmp[x]=='\n' ); x-- )
			{
				tmp[x] = '\0';
				cr_found = TRUE;
			}
			if ( x < 0 )
				x = 0;

			if ( x == 0 )
				cr += cr_found;
			else
				cr = 0;

			str = tmp;

			if ( str[x] == '~' )
			{
				str = Trim ( str, "~" );
				done = 1;
			}
			else if ( cr <= 2 )
				str += "\r\n";
#if 0

			if ( tmp[x]=='~' )
			{
				tmp[x]='\0';
				done = 1;
			}
			else
			{
				strlcat ( tmp, "\r\n", sizeof ( tmp ) );
				x += 2;
				/*
				                if((x > 0 && x < tlen-3) || (tmp[x]!='\r' && tmp[x]!='\n'))
				                    ++x;
				                tmp[x] = '\r';
				                tmp[x++] = '\n';
				                tmp[x++] = '\0';
				                */
			}
#endif


			if ( buf.size() + str.size() >= MAX_STRING_LENGTH )
			{
				log ( "SYSERR: fread_string: string too large (db.c)" );
				log ( "%s", error );
				exit ( 1 );
			}
			else
			{
				buf += str; /* strcat: OK (size checked above) */
			}
		}
	}
	while ( !done );

	/* allocate space for the new string and copy it */
	return ( !buf.empty() ? strdup ( buf.c_str() ) : NULL );
}

string *fread_string_s ( FILE *fl, const char *error )
{
	string *s = new string ( "" );
	char tmp[516];
	int done = 0;
	size_t length = 0, tlen = 0, x;

	do
	{
		if ( !fgets ( tmp, 512, fl ) )
		{
			log ( "SYSERR: fread_string: format error at or near %s", error );
			exit ( 1 );
		}
		x = tlen = strlen ( tmp );

		/* If there is a '~', end the string; else put an "\r\n" over the '\n'. */
		/* now only removes trailing ~'s -- Welcor */
		if ( tlen > 0 )
		{
			for ( x = tlen - 1; x && ( tmp[x]=='\r' || tmp[x]=='\n' ); x-- )
				tmp[x] = '\0';

			if ( tmp[x]=='~' )
			{
				tmp[x]='\0';
				done = 1;
			}
			else
			{
				strlcat ( tmp, "\r\n", sizeof ( tmp ) );
			}

			if ( length + x >= MAX_STRING_LENGTH )
			{
				log ( "SYSERR: fread_string: string too large (db.c)" );
				log ( "%s", error );
				exit ( 1 );
			}
			else
			{
				s->append ( tmp ); /* strcat: OK (size checked above) */
				length += x;
			}
		}
	}
	while ( !done );

	/**clean up any existing ones**/
	return s;
}
/* Called to free all allocated follow_type structs */
void free_followers ( struct follow_type *k )
{
	if ( !k )
		return;

	if ( k->next )
		free_followers ( k->next );

	k->follower = NULL;
	free ( k );
}

void free_mob_memory ( memory_rec *k )
{
	if ( !k )
		return;

	if ( k->next )
		free_mob_memory ( k->next );

	free ( k );
}

// delayed version of free obj
void obj_data_to_pool ( struct obj_data *obj )
{
	if ( !obj )
		return;
	if ( dead_obj.find ( GET_ID ( obj ) ) != dead_obj.end() )
	{
		log ( "Object %s attempted to be added to dead list twice!", obj->short_description );
		return;
	}

	dead_obj[GET_ID ( obj ) ] = obj;
}

/* release memory allocated for an obj struct */
void free_obj ( struct obj_data *obj, int extracted )
{

	Character *next;
	//log("Freeing object: %d: %s", GET_OBJ_VNUM(obj), obj->short_description);
	if ( GET_OBJ_RNUM ( obj ) == NOWHERE )
	{
		free_object_strings ( obj );
		/* free script proto list */
		free_proto_script ( obj, OBJ_TRIGGER );
	}
	else
	{
		free_object_strings_proto ( obj );
		if ( obj->proto_script != obj_proto[GET_OBJ_RNUM ( obj ) ].proto_script )
			free_proto_script ( obj, OBJ_TRIGGER );
	}

	/* cancel message updates */
	if ( GET_TIMER_EVENT ( obj ) )
	{
		event_cancel ( GET_TIMER_EVENT ( obj ) );
		GET_TIMER_EVENT ( obj ) = NULL;
	}
	if ( TRAVEL_LIST ( obj ) != NULL )
	{
		free_travel_points ( TRAVEL_LIST ( obj ) );
		TRAVEL_LIST ( obj ) = NULL;
	}

	for ( Character *ch = OBJ_SAT_IN_BY ( obj ); ch; ch = next )
	{

		if ( ch )
		{
			next = NEXT_SITTING ( ch );
			SITTING ( ch ) = NULL;
			NEXT_SITTING ( ch ) = NULL;
		}
		else
			next = NULL;
	}
	OBJ_SAT_IN_BY ( obj ) = NULL;
	free_identifier ( obj );

	/* free any assigned scripts */
	if ( SCRIPT ( obj ) )
		extract_script ( obj, OBJ_TRIGGER );


	if ( !extracted && GET_OBJ_RNUM ( obj ) >= 0 )
	{
		purge_qic ( GET_OBJ_RNUM ( obj ) );
		obj_index[obj->item_number].number--;

	}
	objNames.remNamelist(GET_ID(obj));
	removeFromObjLookupTable ( GET_ID ( obj ) );
	object_list.erase ( GET_ID ( obj ) );
	free ( obj );
	obj = NULL;
}


/*
 * Steps:
 *   1: Make sure no one is using the pointer in paging.
 *   2: Read contents of a text file.
 *   3: Allocate space.
 *   4: Point 'buf' to it.
 *
 * We don't want to free() the string that someone may be
 * viewing in the pager.  page_string() keeps the internal
 * str_dup()'d copy on ->showstr_head and it won't care
 * if we delete the original.  Otherwise, strings are kept
 * on ->showstr_vector but we'll only match if the pointer
 * is to the string we're interested in and not a copy.
 */
int file_to_string_alloc ( const char *name, char **buf )
{
	int temppage;
	char temp[MAX_STRING_LENGTH];
	Descriptor *in_use;

	for ( in_use = descriptor_list; in_use; in_use = in_use->next )
		if ( in_use->showstr_vector && *in_use->showstr_vector == *buf )
			return ( -1 );

	/* Lets not free() what used to be there unless we succeeded. */
	if ( file_to_string ( name, temp, sizeof ( temp ) ) < 0 )
		return ( -1 );

	for ( in_use = descriptor_list; in_use; in_use = in_use->next )
	{
		if ( !in_use->showstr_count || *in_use->showstr_vector != *buf )
			continue;
		lock_desc ( in_use );
		/* Let's be nice and leave them at the page they were on. */
		temppage = in_use->showstr_page;
		paginate_string ( ( in_use->showstr_head =
		                        strdup ( *in_use->showstr_vector ) ), in_use );
		in_use->showstr_page = temppage;
		unlock_desc ( in_use );
	}

	if ( *buf )
		free ( *buf );

	*buf = str_dup ( temp );
	return ( 0 );
}


/* read contents of a text file, and place in buf */
int file_to_string ( const char *name, char *buf, size_t b_len )
{
	FILE *fl;
	char tmp[READ_SIZE + 3];
	int len;

	*buf = '\0';

	if ( ! ( fl = fopen ( name, "r" ) ) )
	{
		log ( "SYSERR: reading %s: %s", name, strerror ( errno ) );
		return ( -1 );
	}
	for ( ;; )
	{
		if ( !fgets ( tmp, READ_SIZE, fl ) )     /* EOF check */
			break;
		if ( ( len = strlen ( tmp ) ) > 0 )
			tmp[len - 1] = '\0';    /* take off the trailing \n */
		strlcat ( tmp, "\r\n", sizeof ( tmp ) );  /* strcat: OK (tmp:READ_SIZE+3) */

		if ( strlen ( buf ) + strlen ( tmp ) + 1 > MAX_STRING_LENGTH )
		{
			log ( "SYSERR: %s: string too big (%d max)", name,
			      MAX_STRING_LENGTH );
			*buf = '\0';
			fclose ( fl );
			return ( -1 );
		}
		strlcat ( buf, tmp, b_len ); /* strcat: OK (size checked above) */
	}

	fclose ( fl );


	return ( 0 );
}





void clear_object ( struct obj_data *obj )
{
	int i = 0;
	memset ( ( char * ) obj, 0, sizeof ( struct obj_data ) );

	GET_OBJ_WAS ( obj ) = NOTHING;
	TRAVEL_LIST ( obj ) = NULL;
	obj->item_number = NOTHING;
	obj->owner = 0;
	IN_ROOM ( obj ) = NULL;
	GET_OBJ_VROOM ( obj ) = NOWHERE;
	GET_OBJ_TIMER ( obj ) = -1;
	GET_OBJ_EXPIRE ( obj ) = 0;
	GET_TIMER_EVENT ( obj ) = NULL;
	obj->worn_on = NOWHERE;
	GET_OBJ_VROOM ( obj ) = NOWHERE;
	for ( i = 0; i < NUM_OBJ_VAL_POSITIONS; i++ )
		GET_OBJ_VAL ( obj, i ) = 0;
	for ( i = 0; i < NUM_OBJ_FLOATING_VAL_POSITIONS; i++ )
		GET_OBJ_FLOATING_VAL ( obj, i ) = 0;
	GET_OBJ_LEVEL ( obj ) = 0;
	OBJ_SAT_IN_BY ( obj ) = NULL;
	obj->idents = NULL;
	obj->obj_flags.obj_innate = 0;
	obj->skin = NOTHING;
	obj->hitched = NULL;
	obj->proto_script = NULL;
}

/* returns the real number of the room with given virtual number */
room_rnum real_room ( room_vnum vnum )
{
	if ( vnum >= 0 && vnum <= HIGHEST_VNUM )
		return world_vnum[vnum];
	else
		return NULL;
}

/*
 * Extend later to include more checks.
 *
 * TODO: Add checks for unknown bitvectors.
 */
int check_object ( struct obj_data *obj, int nr )
{
	//char objname[MAX_INPUT_LENGTH + 32];
	int error = FALSE;
	int check_potion_price ( struct obj_data *obj );
	int check_potion_weight ( struct obj_data *obj );
	char buf[MAX_STRING_LENGTH];

	if ( GET_OBJ_WEIGHT ( obj ) < 0 )
	{
		log ( "SYSERR: Object #%d (%s) has negative weight (%d) setting to 0.", nr, obj->short_description, GET_OBJ_WEIGHT ( obj ) );
		GET_OBJ_WEIGHT ( obj ) = 0;
	}

	if ( GET_OBJ_RENT ( obj ) < 0 )
	{
		log ( "SYSERR: Object #%d (%s) has negative cost/day (%d) fixing.",
		      nr, obj->short_description, GET_OBJ_RENT ( obj ) );
		GET_OBJ_RENT ( obj ) = 0;
	}

	sprintbitarray ( GET_OBJ_WEAR ( obj ), wear_bits, TW_ARRAY_MAX, buf, sizeof ( buf ) );
	if ( strstr ( buf, "UNDEFINED" ) && ( error = TRUE ) )
		log ( "SYSERR: Object #%d (%s) has unknown wear flags.",
		      nr, obj->short_description );

	sprintbitarray ( GET_OBJ_EXTRA ( obj ), extra_bits, EF_ARRAY_MAX, buf, sizeof ( buf ) );
	if ( strstr ( buf, "UNDEFINED" ) && ( error = TRUE ) )
		log ( "SYSERR: Object #%d (%s) has unknown extra flags.",
		      nr, obj->short_description );

	sprintbitarray ( obj->obj_flags.bitvector, affected_bits, AF_ARRAY_MAX,
	                 buf, sizeof ( buf ) );
	if ( strstr ( buf, "UNDEFINED" ) && ( error = TRUE ) )
		log ( "SYSERR: Object #%d (%s) has unknown affection flags.",
		      nr, obj->short_description );

	switch ( GET_OBJ_TYPE ( obj ) )
	{
		case ITEM_DRINKCON:
		{
			char onealias[MAX_INPUT_LENGTH], *space =
			    strchr ( obj->name, ' ' );


			strlcpy ( onealias, space ? space + 1 : obj->name,
			          sizeof ( onealias ) );
			if ( search_block ( onealias, drinknames, TRUE ) < 0 )
			{
				/*log("SYSERR: Object #%d (%s) doesn't have drink type as first alias. (%s) fixing",
				   GET_OBJ_VNUM(obj), obj->short_description, obj->name); */
				name_to_drinkcon ( obj, GET_OBJ_VAL ( obj, 2 ) );
				/*sprintf(obj->name, "%s %s",
				   drinks[GET_OBJ_VAL(obj, 2)], obj->name);*/
			}
		}
		/* Fall through. */
		case ITEM_FOUNTAIN:
			if ( GET_OBJ_VAL ( obj, 1 ) > GET_OBJ_VAL ( obj, 0 ) )
			{
				log ( "SYSERR: Object #%d (%s) contains (%d) more than maximum (%d) fixing.", nr, obj->short_description, GET_OBJ_VAL ( obj, 1 ), GET_OBJ_VAL ( obj, 0 ) );
				GET_OBJ_VAL ( obj, 1 ) = GET_OBJ_VAL ( obj, 0 );
			}
			break;
		case ITEM_SCROLL:
		case ITEM_POTION:

			error |= check_object_level ( obj, 0, nr );
			error |= check_object_spell_number ( obj, 1, nr );
			error |= check_object_spell_number ( obj, 2, nr );
			error |= check_object_spell_number ( obj, 3, nr );
			IN_ROOM ( obj ) = world_vnum[0];
			check_potion_price ( obj );
			check_potion_weight ( obj );
			IN_ROOM ( obj ) = NULL;
			break;
		case ITEM_WAND:
		case ITEM_STAFF:
			error |= check_object_level ( obj, 0, nr );
			error |= check_object_spell_number ( obj, 3, nr );
			if ( GET_OBJ_VAL ( obj, 2 ) > GET_OBJ_VAL ( obj, 1 ) && ( error = TRUE ) )
				log ( "SYSERR: Object #%d (%s) has more charges (%d) than maximum (%d).", nr, obj->short_description, GET_OBJ_VAL ( obj, 2 ), GET_OBJ_VAL ( obj, 1 ) );
			IN_ROOM ( obj ) = world_vnum[0];
			check_potion_price ( obj );
			check_potion_weight ( obj );
			IN_ROOM ( obj ) = NULL;
			break;
	}

	return ( error );
}

int check_object_spell_number ( struct obj_data *obj, int val, int nr )
{
	int error = FALSE;
	const char *spellname;

	if ( GET_OBJ_VAL ( obj, val ) == -1 ) /* i.e.: no spell */
		return ( error );

	/*
	 * Check for negative spells, spells beyond the top define, and any
	 * spell which is actually a skill.
	 */
	if ( GET_OBJ_VAL ( obj, val ) < 0 )
	{
		log ( "SYSERR: Object #%d (%s) has out of range spell (less than 0) (%s) #%d.",
		      nr, obj->short_description, skill_name ( GET_OBJ_VAL ( obj, val ) ), GET_OBJ_VAL ( obj, val ) );
		return ( error = TRUE );
	}
	if ( GET_OBJ_VAL ( obj, val ) > MAX_SKILLS )
	{
		log ( "SYSERR: Object #%d (%s) has out of range spell (bigger than %d) (%s) #%d.",
		      nr, obj->short_description, MAX_SKILLS, skill_name ( GET_OBJ_VAL ( obj, val ) ), GET_OBJ_VAL ( obj, val ) );
		return ( error = TRUE );
	}
	if ( GET_OBJ_VAL ( obj, val ) < 0 || GET_OBJ_VAL(obj, val) > MAX_SKILLS) 
	{
		log ( "SYSERR: Object #%d (%s) has out of range spell (assigning a skill!) (%s) #%d.",
		      nr, obj->short_description, skill_name ( GET_OBJ_VAL ( obj, val ) ), GET_OBJ_VAL ( obj, val ) );
		return ( error = TRUE );
	}

	/*
	 * This bug has been fixed, but if you don't like the special behavior...
	 */
#if 0

	if ( GET_OBJ_TYPE ( obj ) == ITEM_STAFF &&
	        HAS_SPELL_ROUTINE ( GET_OBJ_VAL ( obj, val ), MAG_AREAS | MAG_MASSES ) )
		log ( "... '%s' (#%d) uses %s spell '%s'.",
		      obj->short_description, GET_OBJ_VNUM ( obj ),
		      HAS_SPELL_ROUTINE ( GET_OBJ_VAL ( obj, val ),
		                          MAG_AREAS ) ? "area" : "mass",
		      skill_name ( GET_OBJ_VAL ( obj, val ) ) );
#endif

	if ( scheck )          /* Spell names don't exist in syntax check mode. */
		return ( error );

	/* Now check for unnamed spells. */
	if ( GET_OBJ_VAL ( obj, val ) )    //mord??
	{
		spellname = skill_name ( GET_OBJ_VAL ( obj, val ) );

		if ( ( spellname == unused_spellname
		        || !strcmp ( "UNDEFINED", spellname ) ) && ( error = TRUE ) )
			log ( "SYSERR: Object #%d (%s) uses '%s' spell #%d.",
			      nr, obj->short_description, spellname,
			      GET_OBJ_VAL ( obj, val ) );
	}

	return ( error );
}

int check_object_level ( struct obj_data *obj, int val, int nr )
{
	int error = FALSE;

	if ( ( GET_OBJ_VAL ( obj, val ) < 0 ) )
	{
		log ( "SYSERR: Object #%d (%s) has out of range level #%d changing to 1.", nr, obj->short_description, GET_OBJ_VAL ( obj, val ) );
		GET_OBJ_VAL ( obj, val ) = 1;
	}
	else if ( ( GET_OBJ_VAL ( obj, val ) > LVL_IMPL ) )
	{
		log ( "SYSERR: Object #%d (%s) has out of range level #%d changing to 56.", nr, obj->short_description, GET_OBJ_VAL ( obj, val ) );
		GET_OBJ_VAL ( obj, val ) = 56;
	}

	return ( error );
}

int my_obj_save_to_disk ( FILE * fp, struct obj_data *obj, int locate )
{
	int counter2;
	struct extra_descr_data *ex_desc;
	char buf1[MAX_STRING_LENGTH + 1];
	char smell[MAX_STRING_LENGTH + 1];
	char taste[MAX_STRING_LENGTH + 1];
	char feel[MAX_STRING_LENGTH + 1];

	if ( obj->action_description )
	{
		strcpy ( buf1, obj->action_description );
		strip_cr ( buf1 );
	}
	else
		*buf1 = 0;
	if ( obj->smell )
	{
		strcpy ( smell, obj->smell );
		strip_cr ( smell );
	}
	else
		*smell = 0;
	if ( obj->taste )
	{
		strcpy ( taste, obj->taste );
		strip_cr ( taste );
	}
	else
		*taste = 0;
	if ( obj->feel )
	{
		strcpy ( feel, obj->feel );
		strip_cr ( feel );
	}
	else
		*feel = 0;

	fprintf ( fp,
	          "#%d\n"
	          "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
	          GET_OBJ_VNUM ( obj ),
	          locate,
	          GET_OBJ_VAL ( obj, 0 ),
	          GET_OBJ_VAL ( obj, 1 ),
	          GET_OBJ_VAL ( obj, 2 ),
	          GET_OBJ_VAL ( obj, 3 ),
	          GET_OBJ_VAL ( obj, 4 ),
	          GET_OBJ_VAL ( obj, 5 ),
	          GET_OBJ_VAL ( obj, 6 ),
	          GET_OBJ_VAL ( obj, 7 ),
	          GET_OBJ_VAL ( obj, 8 ),
	          GET_OBJ_VAL ( obj, 9 ),
	          GET_OBJ_EXTRA ( obj ) [0],
	          GET_OBJ_EXTRA ( obj ) [1],
	          GET_OBJ_EXTRA ( obj ) [2],
	          GET_OBJ_EXTRA ( obj ) [3],
		  GET_OBJ_TIMER ( obj), // make sure the right timer is saved
		  GET_OBJ_INNATE ( obj ) );

	if ( ! ( IS_OBJ_STAT ( obj, ITEM_UNIQUE_SAVE ) ) )
	{
		return 1;
	}
	fprintf ( fp,
	          "XAP\n"
	          "%s~\n"
	          "%s~\n"
	          "%s~\n"
	          "%s~\n"
	          "%d %d %d %d %d %d %lld %d\n",
	          obj->name ? obj->name : "undefined",
	          obj->short_description ? obj->short_description : "undefined",
	          obj->description ? obj->description : "undefined",
	          buf1,
	          GET_OBJ_TYPE ( obj ),
	          GET_OBJ_WEAR ( obj ) [0],
	          GET_OBJ_WEAR ( obj ) [1],
	          GET_OBJ_WEAR ( obj ) [2],
	          GET_OBJ_WEAR ( obj ) [3],
	          GET_OBJ_WEIGHT ( obj ), GET_OBJ_COST ( obj ), GET_OBJ_RENT ( obj )
	        );
	/* Do we have affects? */
	for ( counter2 = 0; counter2 < MAX_OBJ_AFFECT; counter2++ )
		if ( obj->affected[counter2].modifier )
			fprintf ( fp, "A\n"
			          "%d %d\n",
			          obj->affected[counter2].location,
			          obj->affected[counter2].modifier );

	/* Do we have extra descriptions? */
	if ( obj->ex_description )  /*. Yep, save them too . */
	{
		for ( ex_desc = obj->ex_description; ex_desc;
		        ex_desc = ex_desc->next )
		{
			/*. Sanity check to prevent nasty protection faults . */
			if ( !*ex_desc->keyword || !*ex_desc->description )
			{
				continue;
			}
			strcpy ( buf1, ex_desc->description );
			strip_cr ( buf1 );
			fprintf ( fp, "E\n" "%s~\n" "%s~\n", ex_desc->keyword, buf1 );
		}
	}

	return 1;
}


#if 0
int read_xap_objects ( FILE * fl, Character *ch )
{
	char line[MAX_STRING_LENGTH];
	int t[20], nr, danger, j, k, zwei;
	struct obj_data *temp = NULL;
	struct extra_descr_data *new_descr;
	int num_of_objs = 0;
	char buf2[MAX_INPUT_LENGTH];
	int retval;

	if ( !feof ( fl ) )
		get_line ( fl, line );
	while ( !feof ( fl ) && line != NULL )
	{
		/* first, we get the number. Not too hard. */
		if ( *line == '#' )
		{
			num_of_objs++;
			if ( sscanf ( line, "#%d", &nr ) != 1 )
			{
				continue;
			}
			/* we have the number, check it, load obj. */
			if ( nr == -1 )   /* then its unique */
			{
				temp = create_obj();
				temp->item_number = NOTHING;
				SET_BIT_AR ( GET_OBJ_EXTRA ( temp ), ITEM_UNIQUE_SAVE );
			}
			else if ( nr < 0 )
			{
				continue;
			}
			else
			{
				temp = read_object ( nr, VIRTUAL );
				if ( nr >= 99999 || !temp )
				{
					continue;
				}
			}
			/* now we read locate. - this is for autoeq, will be 0 elsewise */
			/* only the rest of the vals, and extra flags will be read */
			get_line ( fl, line );
			retval = sscanf ( line, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
			                  t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6, t + 7,
			                  t + 8, t + 9, t + 10, t + 11, t + 12, t + 13, t + 14, t + 15, t + 16);

			if ( retval == 10 )
			{
				GET_OBJ_VAL ( temp, 0 ) = t[1];
				GET_OBJ_VAL ( temp, 1 ) = t[2];
				GET_OBJ_VAL ( temp, 2 ) = t[3];
				GET_OBJ_VAL ( temp, 3 ) = t[4];
				GET_OBJ_EXTRA ( temp ) [0] = t[5];
				GET_OBJ_EXTRA ( temp ) [1] = t[6];
				GET_OBJ_EXTRA ( temp ) [2] = t[7];
				GET_OBJ_EXTRA ( temp ) [3] = t[8];
				GET_OBJ_TIMER ( temp ) = t[9];
				/*---unknown----*/
				GET_OBJ_VAL ( temp, 4 ) = 0;
				GET_OBJ_VAL ( temp, 5 ) = 0;
				GET_OBJ_VAL ( temp, 6 ) = 0;
				GET_OBJ_VAL ( temp, 7 ) = 0;
				GET_OBJ_VAL ( temp, 8 ) = 0;
				GET_OBJ_VAL ( temp, 9 ) = 0;
				GET_OBJ_INNATE ( temp ) = 0;
			}
			else
			{
				GET_OBJ_VAL ( temp, 0 ) = t[1];
				GET_OBJ_VAL ( temp, 1 ) = t[2];
				GET_OBJ_VAL ( temp, 2 ) = t[3];
				GET_OBJ_VAL ( temp, 3 ) = t[4];
				GET_OBJ_VAL ( temp, 4 ) = t[5];
				GET_OBJ_VAL ( temp, 5 ) = t[6];
				GET_OBJ_VAL ( temp, 6 ) = t[7];
				GET_OBJ_VAL ( temp, 7 ) = t[8];
				GET_OBJ_VAL ( temp, 8 ) = t[9];
				GET_OBJ_VAL ( temp, 9 ) = t[10];
				GET_OBJ_EXTRA ( temp ) [0] = t[11];
				GET_OBJ_EXTRA ( temp ) [1] = t[12];
				GET_OBJ_EXTRA ( temp ) [2] = t[13];
				GET_OBJ_EXTRA ( temp ) [3] = t[14];
				GET_OBJ_TIMER ( temp ) = t[15];
				GET_OBJ_INNATE ( temp ) = t[16];
			}

			get_line ( fl, line );
			/* read line check for xap. */
			if ( !strcasecmp ( "XAP", line ) ) {   /* then this is a Xap Obj, requires
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       special care */
				if ( ( temp->name = fread_string ( fl, buf2 ) ) == NULL )
				{
					temp->name = "undefined";
				}

				if ( ( temp->short_description =
				            fread_string ( fl, buf2 ) ) == NULL )
				{
					temp->short_description = "undefined";
				}

				if ( ( temp->description = fread_string ( fl, buf2 ) ) == NULL )
				{
					temp->description = "undefined";
				}

				if ( ( temp->action_description =
				            fread_string ( fl, buf2 ) ) == NULL )
				{
					temp->action_description = 0;
				}

				if ( ( temp->smell = fread_string ( fl, buf2 ) ) == NULL )
					temp->smell = 0;

				if ( ( temp->taste = fread_string ( fl, buf2 ) ) == NULL )
					temp->taste = 0;

				if ( ( temp->feel = fread_string ( fl, buf2 ) ) == NULL )
					temp->feel = 0;

				if ( !get_line ( fl, line ) ||
				        ( sscanf ( line, "%d %d %d %d %d %d %d %d",
				                   t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6,
				                   t + 7 ) != 8 ) )
				{
					log (
					    "Format error in first numeric line (expecting _8_ args)" );
					return 0;
				}
				temp->obj_flags.type_flag = t[0];
				temp->obj_flags.wear_flags[0] = t[1];
				temp->obj_flags.wear_flags[1] = t[2];
				temp->obj_flags.wear_flags[2] = t[3];
				temp->obj_flags.wear_flags[3] = t[4];
				temp->obj_flags.weight = t[5];
				temp->obj_flags.cost = t[6];
				temp->obj_flags.cost_per_day = t[7];



				/* we're clearing these for good luck */

				for ( j = 0; j < MAX_OBJ_AFFECT; j++ )
				{
					temp->affected[j].location = APPLY_NONE;
					temp->affected[j].modifier = 0;
				}

				/* You have to null out the extradescs when you're parsing a xap_obj.
				   This is done right before the extradescs are read. */

				if ( temp->ex_description )
				{
					temp->ex_description = NULL;
				}

				for ( k = j = zwei = 0; !zwei && !feof ( fl ); )
				{
					switch ( *line )
					{
						case 'E':
							CREATE ( new_descr, struct extra_descr_data, 1 );
							if ( ( new_descr->keyword = fread_string ( fl, buf2 ) ) == NULL )
								new_descr->keyword = strdup ( "Undefined" );
							if ( ( new_descr->description = fread_string ( fl, buf2 ) ) == NULL )
								new_descr->description = strdup ( "Undefined" );
							new_descr->next = temp->ex_description;
							temp->ex_description = new_descr;
							get_line ( fl, line );
							break;
						case 'A':
							if ( j >= MAX_OBJ_AFFECT )
							{
								log ( "Too many object affectations in loading rent file" );
								danger = 1;
							}
							get_line ( fl, line );
							sscanf ( line, "%d %d", t, t + 1 );

							temp->affected[j].location = t[0];
							temp->affected[j].modifier = t[1];
							j++;
							get_line ( fl, line );
							break;

						case '$':
						case '#':
							zwei = 1;
							break;
						default:
							zwei = 1;
							break;
					}
				}      /* exit our for loop */
				get_line ( fl, line );
			}             /* exit our xap loop */
			generate_weapon ( temp );
			if ( temp != NULL )
			{
				obj_to_char ( temp, ch );
			}
		}
	}                 /* exit our while loop */
	return num_of_objs;
}
#endif


#if 0
/* returns the real number of the monster with given virtual number */
mob_rnum real_mobile ( mob_vnum vnum )
{
	mob_rnum bot, top, mid, i, last_top;

	i = htree_find ( mob_htree, vnum );
	if ( i != NOBODY && mob_index.at ( i ).vnum == vnum )
		return i;
	else
	{
		bot = 0;
		top = mob_index.size();

		/* perform binary search on mob-table */
		for ( ;; )
		{
			last_top = top;
			mid = ( bot + top ) / 2;
			if ( bot >= top )
				return ( NOBODY );
			if ( mob_index.at ( mid ).vnum == vnum )
			{
				log ( "mob_htree sync fix: %d: %d -> %d", vnum, i, mid );
				htree_add ( mob_htree, vnum, mid );
				return ( mid );
			}
			if ( mob_index.at ( mid ).vnum > vnum )
				top = mid - 1;
			else
				bot = mid + 1;

			if ( top > last_top )
				return NOWHERE;
		}
	}
}
#endif


/* returns the real number of the object with given virtual number */
obj_rnum real_object ( obj_vnum vnum )
{
#if 0
	obj_rnum bot, top, mid;

	if ( vnum == NOTHING )
		return NOTHING;

	bot = 0;
	top = top_of_objt;

	/* perform binary search on obj-table */
	for ( ;; )
	{
		mid = ( bot + top ) / 2;

		if ( ( obj_index + mid )->vnum == vnum )
			return ( mid );
		if ( bot >= top )
			return ( -1 );
		if ( ( obj_index + mid )->vnum > vnum )
			top = mid - 1;
		else
			bot = mid + 1;
	}
#elseif (0)
	int i;
	for ( i = 0; i <= top_of_objt; i++ )
	{
		if ( obj_index[i].vnum == vnum )
			return i;
	}
	return -1;
#else

	//    obj_rnum bot, top, mid, i, last_top;

	//    i = htree_find(obj_htree, vnum);
	if ( obj_vTor.find ( vnum ) !=obj_vTor.end() )
		return obj_vTor[vnum];
	else
		return NOTHING;
#if 0

	if ( i != NOWHERE && obj_index[i].vnum == vnum )
		return i;
	else
	{
		bot = 0;
		top = top_of_objt;

		/* perform binary search on obj-table */
		for ( ;; )
		{
			last_top = top;
			mid = ( bot + top ) / 2;

			//            if ((obj_index + mid)->vnum == vnum) {
			//                log("obj_htree sync fix: %d: %d -> %d", vnum, i, mid);
			//                htree_add(obj_htree, vnum, mid);
			//                return (mid);
			//            }
			if ( bot >= top )
				return ( NOTHING );
			if ( ( obj_index + mid )->vnum > vnum )
				top = mid - 1;
			else
				bot = mid + 1;

			if ( top > last_top )
				return NOWHERE;
		}
	}
#endif
#endif
}




int check_bitvector_names ( bitvector_t bits, size_t namecount,
                            const char *whatami, const char *whatbits )
{
	unsigned int flagnum;
	bool error = FALSE;

	/* See if any bits are set above the ones we know about. */
	if ( bits <=
	        ( ~ ( bitvector_t ) 0 >> ( sizeof ( bitvector_t ) * 8 - namecount ) ) )
		return ( FALSE );

	for ( flagnum = namecount; flagnum < sizeof ( bitvector_t ) * 8; flagnum++ )
		if ( ( 1 << flagnum ) & bits )
		{
			log ( "SYSERR: %s has unknown %s flag, bit %d (0 through %d known).", whatami, whatbits, flagnum, ( int ) ( namecount - 1 ) );
			error = TRUE;
		}

	return ( error );
}


void generate_weapon ( OBJ_DATA *obj )
{

	if ( GET_OBJ_TYPE ( obj ) != ITEM_WEAPON )
		return;
	else
	{
		char buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH], *point;


		strcpy ( buf, obj->short_description );
		strip_colour ( buf, sizeof ( buf ) );
		point = buf;

		do
		{
			point = any_one_arg ( point, buf2 );
			if ( ( GET_WEP_TYPE ( obj ) = generate_wep_type ( buf2 ) ) != 0 )
				break;
		}
		while ( point != NULL && *point );

	}

	if ( GET_WEP_TYPE ( obj ) == 0 )
		GET_WEP_TYPE ( obj ) = gen_wep_type_from_attack ( obj );

	if ( GET_WEP_LENGTH ( obj ) == 0 )
		GET_WEP_LENGTH ( obj ) = generate_wep_length ( obj );

	if ( GET_WEP_BALANCE ( obj ) == 0 )
		GET_WEP_BALANCE ( obj ) = fuzzy_balance ( obj );

	if ( GET_WEP_LENGTH ( obj ) >= 10 )
		GET_OBJ_WEIGHT ( obj ) = ( GET_WEP_LENGTH ( obj ) /10 );

}



void give_mob_class ( Character *ch, int vnum )
{
	char buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];
	char *point;
	int check_name_is_class_type ( const char *name );


	if ( GET_NAME ( ch ) != NULL )
		return;

	strcpy ( buf, GET_NAME ( ch ) );
	point = buf;

	do
	{
		point = any_one_arg ( point, buf2 );
		if ( ( GET_CLASS ( ch ) = check_name_is_class_type ( buf2 ) ) != CLASS_NORMAL )
			return;
	}
	while ( point != NULL && *point );


}

#define CHNAME(word)  (compares((word), name))
int check_name_is_class_type ( const char *name )
{
	if ( !name || !*name )
		return CLASS_NORMAL;

	/*CASTER */
	switch ( LOWER ( *name ) )
	{
		case 'c':
			if CHNAME ( "cleric" )
				return CLASS_CASTER;
			break;
		case 'd':
			if CHNAME ( "druid" )
				return CLASS_CASTER;
			break;
		case 'f':
			if CHNAME ( "fairy" )
				return CLASS_CASTER;
			break;
		case 'h':
			if CHNAME ( "healer" )
				return CLASS_CASTER;
			break;
		case 'm':
			if CHNAME ( "mage" )
				return CLASS_CASTER;
			if CHNAME ( "magician" )
				return CLASS_CASTER;
			if CHNAME ( "monk" )
				return CLASS_CASTER;
			break;
		case 'p':
			if CHNAME ( "pixie" )
				return CLASS_CASTER;
			break;
		case 's':
			if CHNAME ( "sorceror" )
				return CLASS_CASTER;
			if CHNAME ( "sorceress" )
				return CLASS_CASTER;
			if CHNAME ( "shaman" )
				return CLASS_CASTER;
			break;
		case 'w':
			if CHNAME ( "witch" )
				return CLASS_CASTER;
			if CHNAME ( "warlock" )
				return CLASS_CASTER;
			if CHNAME ( "wizard" )
				return CLASS_CASTER;
			break;
	}
	/*UNDEAD */
	switch ( *name )
	{
		case 'b':
			if CHNAME ( "body" )
				return CLASS_UNDEAD;
			break;
		case 'c':
			if CHNAME ( "corpse" )
				return CLASS_UNDEAD;
			break;
		case 'd':
			if CHNAME ( "dead" )
				return CLASS_UNDEAD;
			if CHNAME ( "demon" )
				return CLASS_UNDEAD;
			break;
		case 'g':
			if CHNAME ( "ghost" )
				return CLASS_UNDEAD;
			if CHNAME ( "ghoul" )
				return CLASS_UNDEAD;
			break;
		case 'm':
			if CHNAME ( "mummy" )
				return CLASS_UNDEAD;
			break;
		case 'n':
			if CHNAME ( "nightcrawler" )
				return CLASS_UNDEAD;
			break;
		case 's':
			if CHNAME ( "soul" )
				return CLASS_UNDEAD;
			if CHNAME ( "spectre" )
				return CLASS_UNDEAD;
			if CHNAME ( "specter" )
				return CLASS_UNDEAD;
			if CHNAME ( "skeletons" )
				return CLASS_UNDEAD;
			break;
		case 'u':
			if CHNAME ( "undead" )
				return CLASS_UNDEAD;
			break;
		case 'v':
			if CHNAME ( "vampire" )
				return CLASS_UNDEAD;
			break;
		case 'z':
			if CHNAME ( "zombie" )
				return CLASS_UNDEAD;
			break;
	}
	/*FIGHTER*/
	switch ( *name )
	{
		case 'f':
			if CHNAME ( "fighter" )
				return CLASS_FIGHTER;
			break;
		case 'g':
			if CHNAME ( "guard" )
				return CLASS_FIGHTER;
			break;
		case 'k':
			if CHNAME ( "knight" )
				return CLASS_FIGHTER;
			if CHNAME ( "king" )
				return CLASS_FIGHTER;
			break;
		case 'm':
			if CHNAME ( "mercanary" )
				return CLASS_FIGHTER;
			break;
		case 'p':
			if CHNAME ( "paladin" )
				return CLASS_FIGHTER;
			if CHNAME ( "page" )
				return CLASS_FIGHTER;
			if CHNAME ( "police" )
				return CLASS_FIGHTER;
			break;
		case 's':
			if CHNAME ( "soldier" )
				return CLASS_FIGHTER;
			break;
		case 'v':
			if CHNAME ( "viking" )
				return CLASS_FIGHTER;
			break;
		case 'w':
			if CHNAME ( "warrior" )
				return CLASS_FIGHTER;
			break;
	}
	/*ROGUE  */
	switch ( *name )
	{
		case 'a':
			if CHNAME ( "assassin" )
				return CLASS_ROGUE;
			break;
		case 'b':
			if CHNAME ( "bedouin" )
				return CLASS_ROGUE;
			break;
		case 'c':
			if CHNAME ( "crooked" )
				return CLASS_ROGUE;
			break;
		case 'g':
			if CHNAME ( "gypsy" )
				return CLASS_ROGUE;
			break;
		case 'p':
			if CHNAME ( "pickpocket" )
				return CLASS_ROGUE;
			break;
		case 'r':
			if CHNAME ( "robber" )
				return CLASS_ROGUE;
			if CHNAME ( "rogue" )
				return CLASS_ROGUE;
			break;
		case 's':
			if CHNAME ( "spy" )
				return CLASS_ROGUE;
			break;
		case 't':
			if CHNAME ( "thug" )
				return CLASS_ROGUE;
			if CHNAME ( "thief" )
				return CLASS_ROGUE;
			break;
	}

	/*ANIMAL */
	switch ( *name )
	{
		case 'a':
			if CHNAME ( "ass" )
				return CLASS_ANIMAL;
			break;
		case 'b':
			if CHNAME ( "bird" )
				return CLASS_ANIMAL;
			if CHNAME ( "bat" )
				return CLASS_ANIMAL;
			if CHNAME ( "bear" )
				return CLASS_ANIMAL;
			if CHNAME ( "bee" )
				return CLASS_ANIMAL;
			break;
		case 'c':
			if CHNAME ( "cockroach" )
				return CLASS_ANIMAL;
			if CHNAME ( "cat" )
				return CLASS_ANIMAL;
			if CHNAME ( "cow" )
				return CLASS_ANIMAL;
			if CHNAME ( "chicken" )
				return CLASS_ANIMAL;
			if CHNAME ( "crow" )
				return CLASS_ANIMAL;
			if CHNAME ( "coyote" )
				return CLASS_ANIMAL;
			break;
		case 'd':
			if CHNAME ( "dog" )
				return CLASS_ANIMAL;
			if CHNAME ( "donkey" )
				return CLASS_ANIMAL;
			if CHNAME ( "deer" )
				return CLASS_ANIMAL;
			if CHNAME ( "dolphin" )
				return CLASS_ANIMAL;
			if CHNAME ( "dove" )
				return CLASS_ANIMAL;
			if CHNAME ( "duck" )
				return CLASS_ANIMAL;
			break;
		case 'e':
			if CHNAME ( "eagle" )
				return CLASS_ANIMAL;
			break;
		case 'f':
			if CHNAME ( "fish" )
				return CLASS_ANIMAL;
			if CHNAME ( "frog" )
				return CLASS_ANIMAL;
			break;
		case 'g':
			if CHNAME ( "goat" )
				return CLASS_ANIMAL;
			break;
		case 'h':
			if CHNAME ( "horse" )
				return CLASS_ANIMAL;
			if CHNAME ( "hawk" )
				return CLASS_ANIMAL;
			if CHNAME ( "hen" )
				return CLASS_ANIMAL;
			break;
		case 'i':
			if CHNAME ( "iguana" )
				return CLASS_ANIMAL;
			break;
		case 'j':
			if CHNAME ( "jackrabit" )
				return CLASS_ANIMAL;
			break;
		case 'k':
			if CHNAME ( "kitten" )
				return CLASS_ANIMAL;
			if CHNAME ( "kangaroo" )
				return CLASS_ANIMAL;
			break;
		case 'l':
			if CHNAME ( "lizard" )
				return CLASS_ANIMAL;
			if CHNAME ( "lion" )
				return CLASS_ANIMAL;
			break;
		case 'm':
			if CHNAME ( "mule" )
				return CLASS_ANIMAL;
			if CHNAME ( "mouse" )
				return CLASS_ANIMAL;
			break;
		case 'o':
			if CHNAME ( "octopus" )
				return CLASS_ANIMAL;
			break;
		case 'p':
			if CHNAME ( "pet" )
				return CLASS_ANIMAL;
			if CHNAME ( "puppy" )
				return CLASS_ANIMAL;
			if CHNAME ( "pig" )
				return CLASS_ANIMAL;
			if CHNAME ( "piglet" )
				return CLASS_ANIMAL;
			if CHNAME ( "pinto" )
				return CLASS_ANIMAL;
			if CHNAME ( "pony" )
				return CLASS_ANIMAL;
			break;
		case 'q':
			if CHNAME ( "quail" )
				return CLASS_ANIMAL;
			break;
		case 'r':
			if CHNAME ( "rat" )
				return CLASS_ANIMAL;
			if CHNAME ( "rooster" )
				return CLASS_ANIMAL;
			break;
		case 's':
			if CHNAME ( "snake" )
				return CLASS_ANIMAL;
			if CHNAME ( "shark" )
				return CLASS_ANIMAL;
			if CHNAME ( "sheep" )
				return CLASS_ANIMAL;
			break;
		case 't':
			if CHNAME ( "toad" )
				return CLASS_ANIMAL;
			if CHNAME ( "turtle" )
				return CLASS_ANIMAL;
			if CHNAME ( "tiger" )
				return CLASS_ANIMAL;
			break;
		case 'v':
			if CHNAME ( "vulture" )
				return CLASS_ANIMAL;
			break;
		case 'w':
			if CHNAME ( "wolf" )
				return CLASS_ANIMAL;
			if CHNAME ( "worm" )
				return CLASS_ANIMAL;
			if CHNAME ( "whale" )
				return CLASS_ANIMAL;
			if CHNAME ( "wasp" )
				return CLASS_ANIMAL;
			break;
		case 'z':
			if CHNAME ( "zebra" )
				return CLASS_ANIMAL;
			break;
	}


	return CLASS_NORMAL;

}

/*
 * Read a number from a file.
 */
int fread_number ( FILE *fp )
{
	int num;
	bool sign;
	char c;

	do
	{
		c = getc ( fp );
	}
	while ( isspace ( c ) );

	num = 0;

	sign   = FALSE;
	if ( c == '+' )
	{
		c = getc ( fp );
	}
	else if ( c == '-' )
	{
		sign = TRUE;
		c = getc ( fp );
	}

	if ( !isdigit ( c ) )
	{
		log ( "BUG: Fread_number: bad format." );
		exit ( 1 );
	}

	while ( isdigit ( c ) )
	{
		num = num * 10 + c - '0';
		c      = getc ( fp );
	}

	if ( sign )
		num = 0 - num;

	if ( c == '|' )
		num += fread_number ( fp );
	else if ( c != ' ' )
		ungetc ( c, fp );

	return num;
}

/*
 * Read one word (into static buffer).
 */
char *fread_word ( FILE *fp )
{
	static char word[MAX_INPUT_LENGTH];
	char *pword;
	char cEnd;

	do
	{
		cEnd = getc ( fp );
	}
	while ( isspace ( cEnd ) );

	if ( cEnd == '\'' || cEnd == '"' )
	{
		pword   = word;
	}
	else
	{
		word[0] = cEnd;
		pword   = word+1;
		cEnd    = ' ';
	}

	for ( ; pword < word + MAX_INPUT_LENGTH; pword++ )
	{
		*pword = getc ( fp );
		if ( cEnd == ' ' ? isspace ( *pword ) : *pword == cEnd )
		{
			if ( cEnd == ' ' )
				ungetc ( *pword, fp );
			*pword = '\0';
			return word;
		}
	}

	log ( "BUG: Fread_word: word too long." );
	exit ( 1 );
	return NULL;
}

/*
 * Generate a percentile roll.
 */

int number_percent ( void )
{
	int percent;

	while ( ( percent = number_mm() & ( 128-1 ) ) > 99 )
		;

	return 1 + percent;
}

/*
 * Generate a random door.
 */

int number_door ( void )
{
	int door;

	while ( ( door = number_mm() & ( 8-1 ) ) > 5 )
		;

	return door;
}

long number_mm ( void )
{
	return random() >> 6;
}

/*
 * Simple linear interpolation.
 */
int interpolate ( int level, int value_00, int value_32 )
{
	return value_00 + level * ( value_32 - value_00 ) / 32;
}

/*
 * Compare strings, case insensitive, for prefix matching.
 * Return TRUE if astr not a prefix of bstr
 *   (compatibility with historical functions).
 */
bool str_prefix ( const char *astr, const char *bstr )
{
	if ( astr == NULL )
	{
		log ( "BUG:str_prefix: null astr." );
		return TRUE;
	}

	if ( bstr == NULL )
	{
		log ( "BUG:str_prefix: null bstr." );
		return TRUE;
	}

	for ( ; *astr; astr++, bstr++ )
	{
		if ( LOWER ( *astr ) != LOWER ( *bstr ) )
			return TRUE;
	}

	return FALSE;
}


/* External variables from config.c */
extern int pk_allowed;
extern int pt_allowed;
extern int level_can_shout;
extern int holler_move_cost;
extern int tunnel_size;
extern int max_exp_gain;
extern int max_exp_loss;
extern int max_npc_corpse_time;
extern int max_pc_corpse_time;
extern int idle_void;
extern int idle_rent_time;
extern int idle_max_level;
extern int dts_are_dumps;
extern int load_into_inventory;
extern int track_through_doors;
extern int immort_level_ok;
extern int double_exp;
extern int free_rent;
extern int max_obj_save;
extern int min_rent_cost;
extern int auto_save;
extern int autosave_time;
extern int crash_file_timeout;
extern int rent_file_timeout;
extern room_vnum mortal_start_room;
extern room_vnum immort_start_room;
extern room_vnum frozen_start_room;
extern room_vnum donation_room_1;
extern room_vnum donation_room_2;
extern room_vnum donation_room_3;
extern room_vnum gladiator_death_room;
extern ush_int DFLT_PORT;
extern const char *DFLT_IP;
extern const char *DFLT_DIR;
extern const char *LOGNAME;
extern int max_playing;
extern int max_filesize;
extern int max_bad_pws;
extern int siteok_everyone;
extern int nameserver_is_slow;
extern int use_new_socials;
extern int auto_save_olc;
extern const char *MENU;
extern const char *WELC_MESSG;
extern const char *START_MESSG;
extern int use_autowiz;
extern int min_wizlist_lev;
extern const char *OK;
extern const char *NOPERSON;
extern const char *NOEFFECT;

void load_default_config ( void )
{
	/****************************************************************************/
	/** This function is called only once, at boot-time.                       **/
	/** - We assume config_info is empty                          -- Welcor    **/
	/****************************************************************************/
	/********************************t********************************************/
	/** Game play options.                                                     **/
	/****************************************************************************/
	CONFIG_PK_ALLOWED              = pk_allowed;
	CONFIG_PT_ALLOWED             = pt_allowed;
	CONFIG_LEVEL_CAN_SHOUT      = level_can_shout;
	CONFIG_HOLLER_MOVE_COST     = holler_move_cost;
	CONFIG_TUNNEL_SIZE             = tunnel_size;
	CONFIG_MAX_EXP_GAIN            = max_exp_gain;
	CONFIG_MAX_EXP_LOSS            = max_exp_loss;
	CONFIG_MAX_NPC_CORPSE_TIME    = max_npc_corpse_time;
	CONFIG_MAX_PC_CORPSE_TIME   = max_pc_corpse_time;
	CONFIG_IDLE_VOID       = idle_void;
	CONFIG_IDLE_RENT_TIME          = idle_rent_time;
	CONFIG_IDLE_MAX_LEVEL          = idle_max_level;
	CONFIG_DTS_ARE_DUMPS           = dts_are_dumps;
	CONFIG_LOAD_INVENTORY         = load_into_inventory;
	CONFIG_OK              = strdup ( OK );
	CONFIG_NOPERSON        = strdup ( NOPERSON );
	CONFIG_NOEFFECT        = strdup ( NOEFFECT );
	CONFIG_TRACK_T_DOORS          = track_through_doors;
	CONFIG_IMMORT_LEVEL_OK = immort_level_ok;
	CONFIG_DOUBLE_EXP		= double_exp;

	/****************************************************************************/
	/** Rent / crashsave options.                                              **/
	/****************************************************************************/
	CONFIG_FREE_RENT              = free_rent;
	CONFIG_MAX_OBJ_SAVE           = max_obj_save;
	CONFIG_MIN_RENT_COST           = min_rent_cost;
	CONFIG_AUTO_SAVE       = auto_save;
	CONFIG_AUTOSAVE_TIME           = autosave_time;
	CONFIG_CRASH_TIMEOUT          = crash_file_timeout;
	CONFIG_RENT_TIMEOUT            = rent_file_timeout;

	/****************************************************************************/
	/** Room numbers.                                                          **/
	/****************************************************************************/
	CONFIG_MORTAL_START           = world_vnum[mortal_start_room];
	CONFIG_IMMORTAL_START         = world_vnum[immort_start_room];
	CONFIG_FROZEN_START           = world_vnum[frozen_start_room];
	CONFIG_DON_ROOM_1             = world_vnum[donation_room_1];
	CONFIG_DON_ROOM_2             = world_vnum[donation_room_2];
	CONFIG_DON_ROOM_3             = world_vnum[donation_room_3];
	CONFIG_GLA_DEATH_ROOM	  = world_vnum[gladiator_death_room];

	/****************************************************************************/
	/** Game operation options.                                                **/
	/****************************************************************************/
	CONFIG_DFLT_PORT              = DFLT_PORT;

	if ( DFLT_IP )
		CONFIG_DFLT_IP              = strdup ( DFLT_IP );
	else
		CONFIG_DFLT_IP              = NULL;

	CONFIG_DFLT_DIR               = strdup ( DFLT_DIR );

	if ( LOGNAME )
		CONFIG_LOGNAME              = strdup ( LOGNAME );
	else
		CONFIG_LOGNAME              = NULL;

	CONFIG_MAX_PLAYING            = max_playing;
	CONFIG_MAX_FILESIZE           = max_filesize;
	CONFIG_MAX_BAD_PWS            = max_bad_pws;
	CONFIG_SITEOK_ALL             = siteok_everyone;
	CONFIG_NS_IS_SLOW             = nameserver_is_slow;
	CONFIG_NEW_SOCIALS            = use_new_socials;
	CONFIG_OLC_SAVE               = auto_save_olc;
	CONFIG_MENU                   = strdup ( MENU );
	CONFIG_WELC_MESSG             = strdup ( WELC_MESSG );
	CONFIG_START_MESSG            = strdup ( START_MESSG );

	/****************************************************************************/
	/** Autowiz options.                                                       **/
	/****************************************************************************/
	CONFIG_USE_AUTOWIZ            = use_autowiz;
	CONFIG_MIN_WIZLIST_LEV        = min_wizlist_lev;
}

void load_config ( void )
{
	FILE *fl;
	char line[MAX_STRING_LENGTH];
	char tag[MAX_INPUT_LENGTH];
	int  num;
	char buf[MAX_INPUT_LENGTH];

	load_default_config();

	snprintf ( buf, sizeof ( buf ), "%s/%s", DFLT_DIR, CONFIG_CONFFILE );
	if ( ! ( fl = fopen ( CONFIG_CONFFILE, "r" ) ) && ! ( fl = fopen ( buf, "r" ) ) )
	{
		snprintf ( buf, sizeof ( buf ), "Game Config File: %s", CONFIG_CONFFILE );
		perror ( buf );
		return;
	}

	/****************************************************************************/
	/** Load the game configuration file.                                      **/
	/****************************************************************************/
	while ( get_line ( fl, line ) )
	{
		split_argument ( line, tag );
		num = atoi ( line );

		switch ( LOWER ( *tag ) )
		{
			case 'a':
				if ( !strcmp ( tag, "auto_save" ) )
					CONFIG_AUTO_SAVE = num;
				else if ( !strcmp ( tag, "autosave_time" ) )
					CONFIG_AUTOSAVE_TIME = num;
				else if ( !strcmp ( tag, "auto_save_olc" ) )
					CONFIG_OLC_SAVE = num;
				break;

			case 'c':
				if ( !strcmp ( tag, "crash_file_timeout" ) )
					CONFIG_CRASH_TIMEOUT = num;
				break;

			case 'd':
				if ( !strcmp ( tag, "dts_are_dumps" ) )
					CONFIG_DTS_ARE_DUMPS = num;
				else if ( !strcmp ( tag, "donation_room_1" ) )
					if ( num == -1 )
						config_info.room_nums.donation_room_1 = 0;
					else
						config_info.room_nums.donation_room_1 = num;
				else if ( !strcmp ( tag, "donation_room_2" ) )
					if ( num == -1 )
						config_info.room_nums.donation_room_2 = 0;
					else
						config_info.room_nums.donation_room_2 = num;
				else if ( !strcmp ( tag, "donation_room_3" ) )
					if ( num == -1 )
						config_info.room_nums.donation_room_3 = 0;
					else
						config_info.room_nums.donation_room_3 = num;
				else if ( !strcmp ( tag, "dflt_dir" ) )
				{
					if ( CONFIG_DFLT_DIR )
						free ( CONFIG_DFLT_DIR );
					if ( *line )
						CONFIG_DFLT_DIR = strdup ( line );
					else
						CONFIG_DFLT_DIR = strdup ( DFLT_DIR );
				}
				else if ( !strcmp ( tag, "double_exp" ) )
				{
					if ( num != 1 )
						config_info.play.double_exp = 0;
					else
						config_info.play.double_exp = 1;
				}
				else if ( !strcmp ( tag, "dflt_ip" ) )
				{
					if ( CONFIG_DFLT_IP )
						free ( CONFIG_DFLT_IP );
					if ( *line )
						CONFIG_DFLT_IP = strdup ( line );
					else
						CONFIG_DFLT_IP = NULL;
				}
				else if ( !strcmp ( tag, "dflt_port" ) )
					CONFIG_DFLT_PORT = num;
				break;

			case 'f':
				if ( !strcmp ( tag, "free_rent" ) )
					CONFIG_FREE_RENT = num;
				else if ( !strcmp ( tag, "frozen_start_room" ) )
					config_info.room_nums.frozen_start_room = num;
				break;

			case 'g':
				if ( !strcmp ( tag, "gladiator_death_room" ) )
					config_info.room_nums.gladiator_death_room = num;
				break;

			case 'h':
				if ( !strcmp ( tag, "holler_move_cost" ) )
					CONFIG_HOLLER_MOVE_COST = num;
				break;

			case 'i':
				if ( !strcmp ( tag, "idle_void" ) )
					CONFIG_IDLE_VOID = num;
				else if ( !strcmp ( tag, "idle_rent_time" ) )
					CONFIG_IDLE_RENT_TIME = num;
				else if ( !strcmp ( tag, "idle_max_level" ) )
					CONFIG_IDLE_MAX_LEVEL = num;
				else if ( !strcmp ( tag, "immort_level_ok" ) )
					CONFIG_IMMORT_LEVEL_OK = num;
				else if ( !strcmp ( tag, "immort_start_room" ) )
					config_info.room_nums.immort_start_room = num;
				break;

			case 'l':
				if ( !strcmp ( tag, "level_can_shout" ) )
					CONFIG_LEVEL_CAN_SHOUT = num;
				else if ( !strcmp ( tag, "load_into_inventory" ) )
					CONFIG_LOAD_INVENTORY = num;
				else if ( !strcmp ( tag, "logname" ) )
				{
					if ( CONFIG_LOGNAME )
						free ( CONFIG_LOGNAME );
					if ( *line )
						CONFIG_LOGNAME = strdup ( line );
					else
						CONFIG_LOGNAME = NULL;
				}
				break;

			case 'm':
				if ( !strcmp ( tag, "max_bad_pws" ) )
					CONFIG_MAX_BAD_PWS = num;
				else if ( !strcmp ( tag, "max_exp_gain" ) )
					CONFIG_MAX_EXP_GAIN = num;
				else if ( !strcmp ( tag, "max_exp_loss" ) )
					CONFIG_MAX_EXP_LOSS = num;
				else if ( !strcmp ( tag, "max_filesize" ) )
					CONFIG_MAX_FILESIZE = num;
				else if ( !strcmp ( tag, "max_npc_corpse_time" ) )
					CONFIG_MAX_NPC_CORPSE_TIME = num;
				else if ( !strcmp ( tag, "max_obj_save" ) )
					CONFIG_MAX_OBJ_SAVE = num;
				else if ( !strcmp ( tag, "max_pc_corpse_time" ) )
					CONFIG_MAX_PC_CORPSE_TIME = num;
				else if ( !strcmp ( tag, "max_playing" ) )
					CONFIG_MAX_PLAYING = num;
				else if ( !strcmp ( tag, "menu" ) )
				{
					if ( CONFIG_MENU )
						free ( CONFIG_MENU );
					strncpy ( buf, "Reading menu in load_config()", sizeof ( buf ) );
					CONFIG_MENU = fread_string ( fl, buf );
				}
				else if ( !strcmp ( tag, "min_rent_cost" ) )
					CONFIG_MIN_RENT_COST = num;
				else if ( !strcmp ( tag, "min_wizlist_lev" ) )
					CONFIG_MIN_WIZLIST_LEV = num;
				else if ( !strcmp ( tag, "mortal_start_room" ) )
					config_info.room_nums.mortal_start_room = num;
				break;

			case 'n':
				if ( !strcmp ( tag, "nameserver_is_slow" ) )
					CONFIG_NS_IS_SLOW = num;
				else if ( !strcmp ( tag, "noperson" ) )
				{
					char tmp[READ_SIZE];
					if ( CONFIG_NOPERSON )
						free ( CONFIG_NOPERSON );
					snprintf ( tmp, sizeof ( tmp ), "%s\r\n", line );
					CONFIG_NOPERSON = strdup ( tmp );
				}
				else if ( !strcmp ( tag, "noeffect" ) )
				{
					char tmp[READ_SIZE];
					if ( CONFIG_NOEFFECT )
						free ( CONFIG_NOEFFECT );
					snprintf ( tmp, sizeof ( tmp ), "%s\r\n", line );
					CONFIG_NOEFFECT = strdup ( tmp );
				}
				break;

			case 'o':
				if ( !strcmp ( tag, "ok" ) )
				{
					char tmp[READ_SIZE];
					if ( CONFIG_OK )
						free ( CONFIG_OK );
					snprintf ( tmp, sizeof ( tmp ), "%s\r\n", line );
					CONFIG_OK = strdup ( tmp );
				}
				break;

			case 'p':
				if ( !strcmp ( tag, "pk_allowed" ) )
					CONFIG_PK_ALLOWED = num;
				else if ( !strcmp ( tag, "pt_allowed" ) )
					CONFIG_PT_ALLOWED = num;
				break;

			case 'r':
				if ( !strcmp ( tag, "rent_file_timeout" ) )
					CONFIG_RENT_TIMEOUT = num;
				break;

			case 's':
				if ( !strcmp ( tag, "siteok_everyone" ) )
					CONFIG_SITEOK_ALL = num;
				else if ( !strcmp ( tag, "start_messg" ) )
				{
					strncpy ( buf, "Reading start message in load_config()", sizeof ( buf ) );
					if ( CONFIG_START_MESSG )
						free ( CONFIG_START_MESSG );
					CONFIG_START_MESSG = fread_string ( fl, buf );
				}
				break;

			case 't':
				if ( !strcmp ( tag, "tunnel_size" ) )
					CONFIG_TUNNEL_SIZE = num;
				else if ( !strcmp ( tag, "track_through_doors" ) )
					CONFIG_TRACK_T_DOORS = num;
				break;

			case 'u':
				if ( !strcmp ( tag, "use_autowiz" ) )
					CONFIG_USE_AUTOWIZ = num;
				else if ( !strcmp ( tag, "use_new_socials" ) )
					CONFIG_NEW_SOCIALS = num;
				break;

			case 'w':
				if ( !strcmp ( tag, "welc_messg" ) )
				{
					strncpy ( buf, "Reading welcome message in load_config()", sizeof ( buf ) );
					if ( CONFIG_WELC_MESSG )
						free ( CONFIG_WELC_MESSG );
					CONFIG_WELC_MESSG = fread_string ( fl, buf );
				}
				break;

			default:
				break;
		}
	}

	fclose ( fl );
}

/* returns the real number of the zone with given virtual number */
zone_rnum real_zone ( zone_vnum vnum )
{
#if 0
	zone_rnum bot, top, mid;

	bot = 0;
	top = top_of_zone_table;

	/* perform binary search on zone-table */
	for ( ;; )
	{
		mid = ( bot + top ) / 2;

		if ( ( zone_table + mid )->number == vnum )
			return ( mid );
		if ( bot >= top )
			return ( NOWHERE );
		if ( ( zone_table + mid )->number > vnum )
			top = mid - 1;
		else
			bot = mid + 1;
	}
#else
	zone_rnum t;
	for ( t = 0; t <= top_of_zone_table; t++ )
		if ( vnum == zone_table[t].number )
			return t;
	return NOWHERE;
#endif

}


int valid_to_save ( const char *name )
{
	for ( int tp = 0; tp < pi.Size(); tp++ )
	{
		if ( pi.DeletedByIndex ( tp ) )
			continue;
		if ( *pi.NameByIndex ( tp ) == LOWER ( *name ) && !str_cmp ( pi.NameByIndex ( tp ), name ) )
		{
			return 1;
		}
	}
	return 0;
}

void add_char_to_list ( Character *ch )
{
	//Character *tch;
	if ( ch != character_list )
	{
		ch->next = character_list;
		character_list = ch;
	}
}
/** Mob Proto Functions **/
bool MobProtoExists ( mob_vnum vn )
{
	if ( vn == NOBODY )
	{
		log ( "Passing NOTHING as vnum to mob proto exists!" );
		return FALSE;
	}
	return ( mob_proto.find ( vn ) != mob_proto.end() );
}

Character * GetMobProto ( mob_vnum vn )
{
	if ( !MobProtoExists ( vn ) )
		return NULL;
	if ( vn == NOBODY )
	{
		log ( "Passing NOTHING as vnum to get mob proto!" );
		return NULL;
	}

	return mob_proto[vn];
}
void SetMobProto ( mob_vnum vn, Character *c )
{
	if ( vn == NOBODY )
	{
		log ( "Passing NOTHING as vnum to set mob proto!" );
		return;
	}
	mob_proto[vn] = c;
}

mob_vnum DeleteMobProto ( mob_vnum vn )
{
	if ( MobProtoExists ( vn ) )
	{
		delete mob_proto[vn];
		mob_proto.erase ( vn );
		return vn;
	}
	return NOBODY;
}

int GetMobProtoCount()
{
	return mob_proto.size();
}
/** Mob Index Functions **/
bool MobIndexExists ( mob_vnum vn )
{
	if ( vn == NOBODY )
	{
		log ( "Passing NOTHING as vnum to mob index exists!" );
		return FALSE;
	}
	return ( mob_index.find ( vn ) != mob_index.end() );
}

struct index_data * GetMobIndex ( mob_vnum vn )
{
	if ( !MobIndexExists ( vn ) )
		return NULL;
	if ( vn == NOBODY )
	{
		log ( "Passing NOTHING as vnum to get mob proto!" );
		return NULL;
	}
	return mob_index[vn];
}
void SetMobIndex ( mob_vnum vn, struct index_data *c )
{
	if ( vn == NOBODY )
	{
		log ( "Passing NOTHING as vnum to set mob proto!" );
		return;
	}
	mob_index[vn] = c;
}

mob_vnum DeleteMobIndex ( mob_vnum vn )
{
	if ( MobIndexExists ( vn ) )
	{
		delete mob_index[vn];
		mob_index.erase ( vn );
		return vn;
	}
	return NOBODY;
}

int load_objects_to_room(room_rnum rnum, FILE *fl);
struct obj_data * read_one_item(FILE *fl, OBJ_DATA *temp, int *locate);
void load_artifact_file (char* name) {
  room_vnum vnum = atol(name);
  if (vnum <= 0) {
    log ("Illegal artifact file name: %s", name);
    return;
  }
  Room* room = world_vnum[vnum];
  log ("Loading artifacts for room %s", room->name);
  FILE* fd = fopen(name, "r");

  load_objects_to_room(room,fd);
  /*  
  struct obj_data *obj = NULL;
  int location;
  while ((obj = read_one_item(fd, obj, &location))) {
    obj_to_room(obj,room);
    obj = NULL;
  }
  */
  fclose(fd);
  log ("Done reading artifact file %s", name);
}

void load_saved_artifacts () {
  DIR *dp;
  struct dirent *ep;
  struct stat st_buf;

  dp = opendir(ARTI_DIR);
  if (!dp)
    mkdir(ARTI_DIR, 0777);
  else {
    char cwd[1024];
    getcwd(cwd, sizeof(cwd));
    chdir(ARTI_DIR);
    while ((ep = readdir (dp))) {
      int pos = strlen(ep->d_name) - 5;
      if (strcmp(ep->d_name+pos,".arti"))
	continue;

      if (stat(ep->d_name, &st_buf)) {
	log ( "Error reading artifact file %s: %s", ep->d_name, strerror(errno));
	continue;
      }
      if (S_ISREG(st_buf.st_mode))
	load_artifact_file(ep->d_name);
    }
    chdir(cwd);
  }
}

int save_one_item( OBJ_DATA *obj,FILE *fl, int locate);

void save_artifacts (FILE* fd,struct obj_data *container) {
  for (struct obj_data *obj = container->contains;obj;obj = obj->next_content)
    if (IS_OBJ_STAT(obj, ITEM_ARTIFACT))
      save_one_item(obj, fd, -1);
}


void save_artifacts (Room* room) {
  if (!ROOM_FLAGGED(room, ROOM_ARTISAVE))
    return;

  if(!boot_time)
    return;

  char cwd[1024];
  getcwd(cwd, sizeof(cwd));
  chdir(ARTI_DIR);

  char filename[20]; //hopefully no room will be created with an 15-digit zone num..
  snprintf(filename, sizeof(filename), "%d.arti", room->number);
  FILE* fd = fopen(filename, "w");
  for (struct obj_data *obj = room->contents;obj;obj = obj->next_content) {
    if (IS_OBJ_STAT(obj, ITEM_ARTIFACT)) {
      if (GET_OBJ_TYPE(obj) == ITEM_CONTAINER)
	save_artifacts(fd, obj);
      
      save_one_item(obj, fd, 0);
    }
  }
  
  fclose(fd);
  chdir(cwd);
}
