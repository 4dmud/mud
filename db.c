/*************************************************************************
*   File: db.c                                          Part of CircleMUD *
*  Usage: Loading/saving chars, booting/resetting world, internal funcs   *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

int sunlight;
//dont move this sunlight from up here.


#define __DB_C__

#include "conf.h"
#include "sysdep.h"

#include <dirent.h>


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
#include "xmlhelp.h"

#include "assemblies.h"
#include "trees.h" 
#include "htree.h"

int load_qic_check(int rnum);
void qic_scan_rent(void);
void purge_qic(obj_rnum rnum);
void free_corpse_list(struct corpse_list_data *cor);
void qic_load(int rnum);
int allowed_pretitle(CHAR_DATA *ch);
void free_clan_lists();
void extract_all_in_list(OBJ_DATA *obj);
void load_host_list(void);
struct kill_data *load_killlist(char *name);
void free_join_list(struct combine_data *list);
void add_room_to_mine(room_rnum room);

void free_hunter_list(void);
void prune_crlf(char *txt);
int generate_wep_type(char *name);
int fuzzy_balance(OBJ_DATA *wep);
int generate_wep_length(OBJ_DATA *wep);
int OBJ_INNATE_MESSAGE = TRUE;

int gen_wep_type_from_attack(OBJ_DATA *obj);
void renumber_zones();
extern int save_new_style;
extern struct corpse_list_data *corpse_list;
struct config_data config_info; /* Game configuration list.	 */
void assign_mob_stats(void);
void free_forests(struct forest_data *this_forest);
int check_item_hack_invis(struct obj_data *obj, int fix);

void assign_skills(void);
void assign_subskills(void);
void sprintbits(long vektor, char *outstring);
void tag_argument(char *argument, char *tag);
void clean_pfiles(void);
int is_aggro(CHAR_DATA *ch);
void generate_weapon(OBJ_DATA *obj);

void ASSIGNMOB(mob_vnum mob, SPECIAL(fname));
void ASSIGNOBJ(obj_vnum obj, SPECIAL(fname));
SPECIAL(postmaster);
SPECIAL(cleric);
SPECIAL(bank);

/**************************************************************************
*  declarations of most of the 'global' variables                         *
**************************************************************************/

/* help structures */

struct help_index_element *help_table = 0;	/* the help table	 */
unsigned int top_of_helpt = 0;		/* top of help index table	 */
unsigned int max_help_id = 0;
struct help_category_data *help_categories;

int TEMP_LOAD_CHAR = FALSE;

extern struct mob_stat_table mob_stats[];

struct room_data *world_vnum[HIGHEST_VNUM];	/* array of rooms                */
room_vnum top_of_world = 0;	/* ref to top element of world   */
struct social_messg *soc_mess_list = NULL;      /* list of socials */
int top_of_socialt = -1;                        /* number of socials */


struct char_data *character_list = NULL;	/* global linked list of chars */
extern enum subskill_list subskill;
void subs_remove(struct char_data *ch, struct sub_list *af);
void skills_remove(struct char_data *ch, struct skillspell_data *af);

struct index_data **trig_index;	/* index table for triggers      */
struct trig_data *trigger_list = NULL;	/* all attached triggers */
int top_of_trigt = 0;		/* top of trigger index table    */
struct htree_node *mob_htree = NULL;	/* hash tree for fast mob lookup */

long max_mob_id = MOB_ID_BASE;	/* for unique mob id's       */
long max_obj_id = OBJ_ID_BASE;	/* for unique obj id's       */
int dg_owner_purged;		/* For control of scripts */
struct htree_node *obj_htree = NULL;	/* hash tree for fast obj lookup */

struct index_data *mob_index;	/* index table for mobile file   */
struct char_data *mob_proto;	/* prototypes for mobs           */
mob_rnum top_of_mobt = 0;	/* top of mobile index table     */

struct obj_data *object_list = NULL;	/* global linked list of objs    */
struct obj_data *dead_obj = NULL;	/* delayed obj removal   */
struct index_data *obj_index;	/* index table for object file   */
struct obj_data *obj_proto;	/* prototypes for objs           */
obj_rnum top_of_objt = 0;	/* top of object index table     */
struct zone_list_data *zone_list = NULL;

struct zone_data *zone_table;	/* zone table                    */
zone_rnum top_of_zone_table = 0;	/* top element of zone tab       */
struct message_list fight_messages[MAX_MESSAGES];	/* fighting messages     */

struct player_index_element *player_table = NULL;	/* index to plr file     */
FILE *player_fl = NULL;		/* file desc of player file      */
int top_of_p_table = 0;		/* ref to top of table           */
int top_of_p_file = 0;		/* ref of size of p file         */
long top_idnum = 0;		/* highest idnum in use          */

int no_mail = 0;		/* mail disabled?                */
int mini_mud = 0;		/* mini-mud mode?                */
int no_rent_check = 0;		/* skip rent check on boot?      */
time_t boot_time = 0;		/* time of mud boot              */
int circle_restrict = 0;	/* level of game restriction     */
// int xap_objs = 0;            /* Xap objs - defined in config.c*/
extern int no_specials;
extern int scheck;
int zone_count = 0;

// Declare real-nums of battle-rooms
//room_vnum r_battle_start_room;   /* rnum of battle start room     */
//room_vnum r_battle_recall_room;  /* rnum of battle recall room    */
//room_vnum r_battle_min_room;     /* rnum of battle min room       */
//room_vnum r_battle_max_room;     /* rnum of battle max room       */

char *credits = NULL;		/* game credits                  */
char *news = NULL;		/* mud news                      */
char *motd = NULL;		/* message of the day - mortals */
char *imotd = NULL;		/* message of the day - immorts */
char *GREETINGS = NULL;		/* opening credits screen       */
char *help = NULL;		/* help screen                   */
char *info = NULL;		/* info page                     */
char *wizlist = NULL;		/* list of higher gods           */
char *immlist = NULL;		/* list of peon gods             */
char *background = NULL;	/* background story              */
char *handbook = NULL;		/* handbook for new immortals    */
char *policies = NULL;		/* policies page                 */
char *startup = NULL;		/* startup screen                */


struct time_info_data time_info;	/* the infomation about the time    */
struct player_special_data dummy_mob;	/* dummy spec area for mobs     */
struct reset_q_type reset_q;	/* queue of zones to be reset    */

/* local functions */
void free_zone_list(struct zone_list_data *z);
int check_bitvector_names(bitvector_t bits, size_t namecount, const char *whatami, const char *whatbits);	//mord??
int check_object_spell_number(struct obj_data *obj, int val, int nr);
int check_object_level(struct obj_data *obj, int val, int nr);
void setup_dir(FILE * fl, room_rnum room, int dir);
void index_boot(int mode);
void discrete_load(FILE * fl, int mode, char *filename, zone_vnum zon);
int check_object(struct obj_data *obj, int nr);
void parse_trigger(FILE * fl, int virtual_nr, zone_vnum zon);
void parse_room(FILE * fl, int virtual_nr, zone_vnum zon);
void parse_mobile(FILE * mob_f, int nr, zone_vnum zon);
char *parse_object(FILE * obj_f, int nr, zone_vnum zon);
void load_zones(FILE * fl, char *zonename);
void load_help(FILE * fl);
void assign_mobiles(void);
void assign_objects(void);
void assign_rooms(void);
void assign_the_shopkeepers(void);
void build_player_index(void);
int is_empty(zone_rnum zone_nr);
void reset_zone(zone_rnum zone);
int file_to_string(const char *name, char *buf, size_t b_len);
int file_to_string_alloc(const char *name, char **buf);
void reboot_wizlists(void);
ACMD(do_reboot);
void boot_world(void);
int count_alias_records(FILE * fl);
int count_hash_records(FILE * fl);
bitvector_t asciiflag_conv(char *flag);
void parse_simple_mob(FILE * mob_f, int i, int nr);
void interpret_espec(const char *keyword, const char *value, int i,
                     int nr);
void parse_espec(char *buf, int i, int nr);
void parse_enhanced_mob(FILE * mob_f, int i, int nr);
void get_one_line(FILE * fl, char *buf);
void save_etext(struct char_data *ch);
//void check_start_rooms(void);
void renum_world(void);
void renum_zone_table(void);
void log_zone_error(zone_rnum zone, int cmd_no, const char *message);
void reset_time(void);
long get_ptable_by_name(char *name);
long get_ptable_by_id(long id);
void give_mob_class(struct char_data *ch, int vnum);
void free_note(NOTE_DATA *note, int type);
void free_social_messages(void);
void set_mastery(CHAR_DATA *ch, char buf);
void free_object_strings(struct obj_data *obj);
void free_object_strings_proto(struct obj_data *obj);
void boot_context_help(void);
void free_context_help(void);
char fread_letter(FILE *fp);
void free_followers(struct follow_type *k);
void load_default_config( void );
void load_config( void );





// kalten
//void assign_vehicles(void);
void load_vehicles(void); //mord
void set_race(struct char_data *ch, int race);



/* external functions */
void name_to_drinkcon(struct obj_data *obj, int type);
void paginate_string(char *str, struct descriptor_data *d);	//mord??
struct time_info_data *mud_time_passed(time_t t2, time_t t1);
void free_alias(struct alias_data *a);
void load_messages(void);
void weather_and_time(int mode);
void mag_assign_spells(void);
void boot_social_messages(void);
void create_command_list(void);
void update_obj_file(void);	/* In objsave.c */
void load_qic(void);		/* In qic.c     */
void sort_commands(void);
void sort_spells(void);
void load_banned(void);
void Read_Invalid_List(void);
void boot_the_shops(FILE * shop_f, char *filename, int rec_count);
int find_name(char *name);
void init_clans(void);
int find_first_step(room_rnum src, room_rnum target);
void load_corpses(void);
void new_load_corpses(void);
void save_char_vars(struct char_data *ch);
void destroy_shops(void);	//mord??
void strip_cr(char *);		//...
void load_notes(void);

/* external vars */
extern struct descriptor_data *descriptor_list;
extern struct spell_info_type spell_info[];
extern const char *unused_spellname;
extern const char *unused_spellmessage;
extern int rev_dir[];

/* external ascii pfile vars */
extern int pfile_backup_minlevel;
extern int backup_wiped_pfiles;
extern struct pclean_criteria_data pclean_criteria[];
extern int selfdelete_fastwipe;
extern int auto_pwipe;


#define READ_SIZE 256

/* make these TRUE if you want aliases and poofs saved in the pfiles
 * or FALSE to leave them out
 */
#define ASCII_SAVE_POOFS	TRUE
#define ASCII_SAVE_ALIASES	TRUE


/*************************************************************************
*  routines for booting the system                                       *
*************************************************************************/
static int taone(const struct dirent *a1)
{
  if (!a1)
    return 0;
  else if (!a1->d_name)
    return 0;
  else if (!*a1->d_name) //no blank filenames
    return 0;
  else if (!isdigit(*a1->d_name))//all files start with a number
    return 0;
  else if (strstr(a1->d_name, "~")) //don't read backups
    return 0;

  return 1;
}
int check_dir(char *dirname)
{
  struct dirent **eps = NULL;
  int n;
  int zon, trg, mob, obj, shp, wld;
  zon = trg =  mob = obj = shp = wld = 0;
  n = scandir(dirname, &eps, taone, alphasort);
  if (n >= 0)
  {
    int cnt;
    for (cnt = 0; cnt < n; ++cnt)
    {
      if (eps[cnt]->d_type == DT_REG && isdigit(*eps[cnt]->d_name))
      {
        if (eps[cnt]->d_name[strlen(eps[cnt]->d_name)-1] == '~')
        {
          free(eps[cnt]);
          continue;
        }
        if (strstr(eps[cnt]->d_name, ".zon"))
          zon = TRUE;
        if (strstr(eps[cnt]->d_name, ".trg"))
          trg = TRUE;
        if (strstr(eps[cnt]->d_name, ".mob"))
          mob = TRUE;
        if (strstr(eps[cnt]->d_name, ".obj"))
          obj = TRUE;
        if (strstr(eps[cnt]->d_name, ".shp"))
          shp = TRUE;
        if (strstr(eps[cnt]->d_name, ".wld"))
          wld = TRUE;
      }
      free(eps[cnt]);
    }
    if (!zon)
      log("Directory %s is missing its .zon file", dirname);
    if (!trg)
      log("Directory %s is missing its .trg file", dirname);
    if (!mob)
      log("Directory %s is missing its .mob file", dirname);
    if (!obj)
      log("Directory %s is missing its .obj file", dirname);
    if (!shp)
      log("Directory %s is missing its .shp file", dirname);
    if (!wld)
      log("Directory %s is missing its .wld file", dirname);
    if (eps)
      free(eps);
    if ((zon + trg + mob + obj + shp + wld) == 6)
      return 1;
    else
    {
      exit(1);
      return 0;
    }
  }
  else
  {
    log("Couldn't open the directory: %s", dirname);
    exit(1);
  }

  return 0;
}

int numsort(const void *a, const void *b)
{
  const struct dirent *a1 = NULL, *b1 = NULL;



  if (!a || !b)
    return -1;

  a1 = *(const struct dirent **) a;
  b1 = *(const struct dirent **) b;

  if (!a1 || !b1)
    return -1;
  else  if (a1->d_type != DT_DIR || b1->d_type != DT_DIR)
    return -1;
  else if (isdigit(*a1->d_name) && isdigit(*b1->d_name))
  {
    return (atoi(a1->d_name) - atoi(b1->d_name));
  }
  else
  {
    log("ERROR in directory sort: a1 is %s, b1 is %s", a1->d_name, b1->d_name);
    return -1;
  }

}
static int zone_compare(const void *a, const void *b)
{
  const struct zone_list_data *aa, *bb;

  aa = *(const struct zone_list_data **)a;
  bb = *(const struct zone_list_data **)b;

  return bb->num - aa->num;
}
void sort_zone_list(int total)
{
  struct zone_list_data *temp_list[total + 1], *temp, *ztmp;
  int i;
  for (i = 0, temp = zone_list; i<total && temp; temp = temp->next, i++)
  {
    temp_list[i] = temp;
  }
#if 1
  for (i=0;i<total;i++)
  {
    log("Before - %d", temp_list[i]->num);
  }
#endif
  qsort(temp_list,total,sizeof(temp_list[0]),zone_compare);
#if 1
  temp = zone_list;
  ztmp = zone_list;
  zone_list = NULL;
  for (i=0;i<total;i++)
  {
    log("After  - %d", temp_list[i]->num);
    CREATE(temp, struct zone_list_data, 1);
    temp->num = temp_list[i]->num;
    strcpy(temp->zone, temp_list[i]->zone);
    temp->next = zone_list;
    zone_list = temp;

  }
  free_zone_list(ztmp);
#endif

  /*for (i = 0, temp = zone_list; i<total && temp; temp = temp->next, i++)
  {
    temp->num = temp_list[i]->num;
    log("After2  - %d", temp_list[i]->num);
    strcpy(temp->zone, temp_list[i]->zone);
  }*/
}


int create_zone_index(void)
{
  struct zone_list_data *temp;

  struct dirent **eps = NULL;
  char dname[256];
  int n;
  int total = 0;

  n = scandir(LIB_WORLD, &eps, taone, numsort);
  if (n > 0)
  {
    int cnt;
    for (cnt = n-1; cnt >= 0; cnt--)
    {
      if (eps[cnt]->d_type == DT_DIR && isdigit(*eps[cnt]->d_name))
      {
        snprintf(dname, sizeof(dname), "%s%s/", LIB_WORLD, eps[cnt]->d_name);
        if (check_dir(dname))
        {
          zone_count++;
          CREATE(temp, struct zone_list_data, 1);
          strcpy(temp->zone, dname);
          temp->num = atoi(eps[cnt]->d_name);
          temp->next = zone_list;
          zone_list = temp;
          total++;
          //log("zone: %d", temp->num);
        }
      }
      if (eps[cnt])
      {
        free(eps[cnt]);
        eps[cnt] = NULL;
      }
    }
  }
  else
  {
    log("Couldn't open the directory: %s", LIB_WORLD);
    free(eps);
    eps=NULL;
    exit(1);
  }
  if (eps)
    free(eps);
  log(" -- sorting zones");
  sort_zone_list(total);
  return 0;

}
/* this is necessary for the autowiz system */
void reboot_wizlists(void)
{
  file_to_string_alloc(WIZLIST_FILE, &wizlist);
  file_to_string_alloc(IMMLIST_FILE, &immlist);
}

/* Wipe out all the loaded text files, for shutting down. */
void free_text_files(void)
{
  char **textfiles[] = {
                         &wizlist, &immlist, &news, &credits, &motd, &imotd, &help, &info,
                         &policies, &handbook, &background, &GREETINGS, NULL
                       };
  int rf;

  for (rf = 0; textfiles[rf]; rf++)
    if (*textfiles[rf])
    {
      free(*textfiles[rf]);
      *textfiles[rf] = NULL;
    }
}

/*
 * Too bad it doesn't check the return values to let the user
 * know about -1 values.  This will result in an 'Okay.' to a
 * 'reload' command even when the string was not replaced.
 * To fix later, if desired. -gg 6/24/99
 */
ACMD(do_reboot)
{
  //int i;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!strcmp(arg, "all") || *arg == '*')
  {
    if (file_to_string_alloc(GREETINGS_FILE, &GREETINGS) == 0)
      prune_crlf(GREETINGS);
    if (file_to_string_alloc(WIZLIST_FILE, &wizlist) < 0)
      new_send_to_char(ch, "Can not read wizlist\r\n");
    if (file_to_string_alloc(IMMLIST_FILE, &immlist) < 0)
      new_send_to_char(ch, "Can not read immlist\r\n");
    if (file_to_string_alloc(NEWS_FILE, &news) < 0)
      new_send_to_char(ch, "Can not read news\r\n");
    if (file_to_string_alloc(CREDITS_FILE, &credits) < 0)
      new_send_to_char(ch, "Can not read credits\r\n");
    if (file_to_string_alloc(MOTD_FILE, &motd) < 0)
      new_send_to_char(ch, "Can not read motd\r\n");
    if (file_to_string_alloc(IMOTD_FILE, &imotd) < 0)
      new_send_to_char(ch, "Can not read imotd\r\n");
    if (file_to_string_alloc(HELP_PAGE_FILE, &help) < 0)
      new_send_to_char(ch, "Can not read help front page\r\n");
    if (file_to_string_alloc(INFO_FILE, &info) < 0)
      new_send_to_char(ch, "Can not read info file\r\n");
    if (file_to_string_alloc(POLICIES_FILE, &policies) < 0)
      new_send_to_char(ch, "Can not read policies\r\n");
    if (file_to_string_alloc(HANDBOOK_FILE, &handbook) < 0)
      new_send_to_char(ch, "Can not read handbook\r\n");
    if (file_to_string_alloc(BACKGROUND_FILE, &background) < 0)
      new_send_to_char(ch, "Can not read background\r\n");
    if (help_table)
      the_free_help();
    index_boot(DB_BOOT_HLP);
  }
  else if (!strcmp(arg, "wizlist"))
  {
    if (file_to_string_alloc(WIZLIST_FILE, &wizlist) < 0)
      new_send_to_char(ch, "Can not read wizlist\r\n");
  }
  else if (!strcmp(arg, "immlist"))
  {
    if (file_to_string_alloc(IMMLIST_FILE, &immlist) < 0)
      new_send_to_char(ch, "Can not read immlist\r\n");
  }
  else if (!strcmp(arg, "news"))
  {
    if (file_to_string_alloc(NEWS_FILE, &news) < 0)
      new_send_to_char(ch, "Can not read news\r\n");
  }
  else if (!strcmp(arg, "credits"))
  {
    if (file_to_string_alloc(CREDITS_FILE, &credits) < 0)
      new_send_to_char(ch, "Can not read credits\r\n");
  }
  else if (!strcmp(arg, "motd"))
  {
    if (file_to_string_alloc(MOTD_FILE, &motd) < 0)
      new_send_to_char(ch, "Can not read motd\r\n");
  }
  else if (!strcmp(arg, "imotd"))
  {
    if (file_to_string_alloc(IMOTD_FILE, &imotd) < 0)
      new_send_to_char(ch, "Can not read imotd\r\n");
  }
  else if (!strcmp(arg, "help"))
  {
    if (file_to_string_alloc(HELP_PAGE_FILE, &help) < 0)
      new_send_to_char(ch, "Can not read help front page\r\n");
  }
  else if (!strcmp(arg, "info"))
  {
    if (file_to_string_alloc(INFO_FILE, &info) < 0)
      new_send_to_char(ch, "Can not read info\r\n");
  }
  else if (!strcmp(arg, "policy"))
  {
    if (file_to_string_alloc(POLICIES_FILE, &policies) < 0)
      new_send_to_char(ch, "Can not read policy\r\n");
  }
  else if (!strcmp(arg, "handbook"))
  {
    if (file_to_string_alloc(HANDBOOK_FILE, &handbook) < 0)
      new_send_to_char(ch, "Can not read handbook\r\n");
  }
  else if (!strcmp(arg, "background"))
  {
    if (file_to_string_alloc(BACKGROUND_FILE, &background) < 0)
      new_send_to_char(ch, "Can not read background\r\n");
  }
  else if (!strcmp(arg, "greetings"))
  {
    if (file_to_string_alloc(GREETINGS_FILE, &GREETINGS) == 0)
      prune_crlf(GREETINGS);
    else
      new_send_to_char(ch, "Can not read greetings.\r\n");
  }
  else if (!strcmp(arg, "xhelp"))
  {
    if (help_table)
      the_free_help();	//mord??

    index_boot(DB_BOOT_HLP);
  }
  else
  {
    new_send_to_char(ch, "Unknown reload option.\r\n");
    return;
  }

  new_send_to_char(ch, "%s", CONFIG_OK);
}


void boot_world(void)
{
  log("Creating zone index");
  create_zone_index();
  log("Loading zone table.");
  index_boot(DB_BOOT_ZON);

  log("Loading triggers and generating index.");
  index_boot(DB_BOOT_TRG);

  log("Loading rooms.");
  index_boot(DB_BOOT_WLD);

  log("Renumbering rooms.");
  renum_world();

  //log("Checking start rooms.");
  //check_start_rooms();

  log("Seting mob stats...");
  assign_mob_stats();

  log("Loading mobs and generating index.");
  index_boot(DB_BOOT_MOB);

  log("Loading objs and generating index.");
  index_boot(DB_BOOT_OBJ);



  log("Renumbering zone table.");
  renum_zone_table();
  renumber_zones();

  log("Loading forest trees.");
  init_trees(load_forest());

  log("Loading Host List.");
  load_host_list();

#if !defined(WIN32)
  log("Loading Corpses");
  if (save_new_style)
    new_load_corpses();
  else
    load_corpses();
#endif

  log("Loading Notes");
  load_notes();

  if (!no_specials)
  {
    log("Loading shops.");
    index_boot(DB_BOOT_SHP);
  }

}

void free_objects(OBJ_DATA *obj)
{
  if (!obj)
    return;
  if (obj->next)
    free_objects(obj->next);
  free_obj(obj, FALSE);
  obj=NULL;
}
void free_pending_objects(OBJ_DATA *obj)
{
  if (!obj)
    return;
  if (obj->next)
    free_objects(obj->next);
  free_obj(obj, TRUE);
  obj=NULL;
}

void free_characters(CHAR_DATA *ch)
{
  if (!ch)
    return;
  if (ch->next)
    free_characters(ch->next);
  free_char(ch);
}

void free_zone_list(struct zone_list_data *z)
{
  if (!z)
    return;
  if (z->next)
    free_zone_list(z->next);
  free(z);
}

/* Free the world, in a memory allocation sense. */
void destroy_db(void)
{
  ssize_t cnt, itr;
  log("Free Hunter List");
  free_hunter_list();
  log("Freeing the memory of the database.");
  /* Active Mobiles & Players */
  log("Freeing Characters and mobs");
  free_characters(character_list);
  log("Free corpse list");
  free_corpse_list(corpse_list);

  /* Active Objects */
  log("Freeing Objects.");
  free_objects(object_list);
  free_pending_objects(dead_obj);

  log("Freeing forests.");
  free_forests(forest);
  log("Freeing Assemblies.");
  free_assemblies();

  log("Free Clan Lists");
  free_clan_lists();
  /* Rooms */
  log("Freeing rooms.");
  for (cnt = 0; cnt <= top_of_world; cnt++)
  {
    if (world_vnum[cnt] == NULL)
      continue;
    free_room_strings(world_vnum[cnt]);
    /* free any assigned scripts */
    if (SCRIPT(world_vnum[cnt]))
      extract_script(world_vnum[cnt], WLD_TRIGGER);
    /* free script proto list */
    free_proto_script(world_vnum[cnt], WLD_TRIGGER);
    if (world_vnum[cnt])
      free(world_vnum[cnt]);
    world_vnum[cnt] = NULL;
  }
  top_of_world = 0;
  /* Objects */
  for (cnt = 0; cnt <= top_of_objt; cnt++)
  {
    free_object_strings(obj_proto + cnt);

    if (obj_index[cnt].qic)
      free(obj_index[cnt].qic);

    /* free script proto list */
    free_proto_script(&obj_proto[cnt], OBJ_TRIGGER);
  }


  free(obj_proto);
  free(obj_index);
htree_free(obj_htree);

  /* Mobiles */
  for (cnt = 0; cnt <= top_of_mobt; cnt++)
  {
    free_mobile_strings(&mob_proto[cnt]);
    /* free script proto list */
    free_proto_script(&mob_proto[cnt], MOB_TRIGGER);
    free_join_list(mob_proto[cnt].mob_specials.join_list);

    while (mob_proto[cnt].affected)
      affect_remove(&mob_proto[cnt], mob_proto[cnt].affected);
  }
  free(mob_proto);
  free(mob_index);
  htree_free(mob_htree);

  htree_free(HTREE_NULL);
  free(HTREE_NULL);

  /* Shops */
  destroy_shops();

  /* Zones */

#define THIS_CMD zone_table[cnt].cmd[itr]
  /* zone table reset queue */
  if (reset_q.head)
  {
    struct reset_q_element *ftemp=reset_q.head, *temp;
    while (ftemp)
    {
      temp = ftemp->next;
      free(ftemp);
      ftemp = temp;
    }
  }
  for (cnt = 0; cnt <= top_of_zone_table; cnt++)
  {
    if (zone_table[cnt].name)
      free(zone_table[cnt].name);
    if (zone_table[cnt].builders)
      free(zone_table[cnt].builders);
    if (zone_table[cnt].cmd)
    {
      /* first see if any vars were defined in this zone */
      for (itr = 0;THIS_CMD.command != 'S';itr++)
        if (THIS_CMD.command == 'V')
        {
          if (THIS_CMD.sarg1)
            free(THIS_CMD.sarg1);
          if (THIS_CMD.sarg2)
            free(THIS_CMD.sarg2);
        }
      /* then free the command list */
      free(zone_table[cnt].cmd);
    }
  }
  free(zone_table);

  free_zone_list(zone_list);
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
  for (cnt=0; cnt < top_of_trigt; cnt++)
  {
    if (trig_index[cnt]->proto)
    {
      /* make sure to nuke the command list (memory leak) */
      /* free_trigger() doesn't free the command list */
      if (trig_index[cnt]->proto->cmdlist)
      {
        struct cmdlist_element *i, *j;
        i = trig_index[cnt]->proto->cmdlist;
        while (i)
        {
          j = i->next;
          if (i->cmd)
            free(i->cmd);
          free(i);
          i = j;
        }
      }
      free_trigger(trig_index[cnt]->proto);
    }
    free(trig_index[cnt]);
  }
  free(trig_index);

  /* Events */
  event_free_all();

  /* context sensitive help system */
  free_context_help();


}



/* body of the booting system */
void boot_db(void)
{
  zone_rnum i;

  log("Boot db -- BEGIN.");

  log("Resetting the game time:");
  reset_time();

  log("Reading news, credits, help, bground, info & motds.");
  file_to_string_alloc(NEWS_FILE, &news);
  file_to_string_alloc(CREDITS_FILE, &credits);
  file_to_string_alloc(MOTD_FILE, &motd);
  file_to_string_alloc(IMOTD_FILE, &imotd);
  file_to_string_alloc(HELP_PAGE_FILE, &help);
  file_to_string_alloc(INFO_FILE, &info);
  file_to_string_alloc(WIZLIST_FILE, &wizlist);
  file_to_string_alloc(IMMLIST_FILE, &immlist);
  file_to_string_alloc(POLICIES_FILE, &policies);
  file_to_string_alloc(HANDBOOK_FILE, &handbook);
  file_to_string_alloc(BACKGROUND_FILE, &background);
  if (file_to_string_alloc(GREETINGS_FILE, &GREETINGS) == 0)
    prune_crlf(GREETINGS);

  log("Loading spell definitions.");
  mag_assign_spells();


  log("Loading skill definitions.");
  assign_skills();

  log("Loading SUB-skill definitions.");
  assign_subskills();

  log("Booting World.");
  boot_world();
htree_test();

  log("Loading help entries.");
  index_boot(DB_BOOT_HLP);

  log("Setting up context sensitive help system for OLC");
  boot_context_help();

  log("Generating player index.");
  build_player_index();

  log("Booting clans.");
  init_clans();

  //removed fight message loading to move them down

  log("Loading social messages.");
  boot_social_messages();
  log("loading command list.");
  create_command_list();	/* aedit patch -- M. Scott */

  log("Load QIC database.");
  load_qic();

  log("Scanning rent files for QIC items and timing out records.");
  qic_scan_rent();

  log("Assigning function pointers:");

  if (!no_specials)
  {
    log("   Mobiles.");
    assign_mobiles();
    log("   Shopkeepers.");
    assign_the_shopkeepers();
    log("   Objects.");
    assign_objects();
    log("   Rooms.");
    assign_rooms();
    log("   Vehicles.");
    load_vehicles();
    //assign_vehicles();
  }

  log("Booting assembled objects.");
  assemblyBootAssemblies();

  log("Assigning spell and skill levels.");
  init_spell_levels();

  log("Sorting command list and spells.");
  sort_commands();
  sort_spells();

  log("Booting mail system.");
  if (!scan_file())
  {
    log("    Mail boot failed -- Mail system disabled");
    no_mail = 1;
  }
  log("Reading banned site and invalid-name list.");
  load_banned();
  Read_Invalid_List();

  if (!no_rent_check)
  {
    log("Deleting timed-out crash and rent files:");
    update_obj_file();
    clean_pfiles();
    log("   Done.");
  }

  /* Moved here so the object limit code works. -gg 6/24/98 */
  if (!mini_mud)
  {
    log("Booting houses.");
    House_boot();
  }
  //moved here so that they show up
  log("Loading fight messages.");
  load_messages();

  for (i = 0; i <= top_of_zone_table; i++)
  {
    log("Resetting #%d: %s (rooms %d-%d).", zone_table[i].number, zone_table[i].name,	//mord??
        zone_table[i].bot /*(i ? (zone_table[i - 1].top + 1) : 0) */ ,
        zone_table[i].top);
    reset_zone(i);
  }

  reset_q.head = reset_q.tail = NULL;



  boot_time = time(0);

  log("Boot db -- DONE.");
}


/* reset the time in the game from file */
void reset_time(void)
{
  time_t beginning_of_time = 0;
  FILE *bgtime;

  if ((bgtime = fopen(TIME_FILE, "r")) == NULL)
    log("SYSERR: Can't read from '%s' time file.", TIME_FILE);
  else
  {
    fscanf(bgtime, "%ld\n", &beginning_of_time);
    fclose(bgtime);
  }

  if (beginning_of_time == 0)
    beginning_of_time = 650336715;


  time_info = *mud_time_passed(time(0), beginning_of_time);

  if (time_info.hours <= 4)
    sunlight = SUN_DARK;
  else if (time_info.hours == 5)
    sunlight = SUN_RISE;
  else if (time_info.hours <= 20)
    sunlight = SUN_LIGHT;
  else if (time_info.hours == 21)
    sunlight = SUN_SET;
  else
    sunlight = SUN_DARK;

  log("   Current Gametime: %dH %dD %dM %dY.", time_info.hours,
      time_info.day, time_info.month, time_info.year);
}

void free_extra_descriptions(struct extra_descr_data *edesc)
{
  struct extra_descr_data *enext;

  for (; edesc; edesc = enext)
  {
    enext = edesc->next;

    free(edesc->keyword);
    free(edesc->description);
    free(edesc);
  }
}

/* Write the time in 'when' to the MUD-time file. */
void save_mud_time(struct time_info_data *when)
{
  FILE *bgtime;

  if ((bgtime = fopen(TIME_FILE, "w")) == NULL)
    log("SYSERR: Can't write to '%s' time file.", TIME_FILE);
  else
  {
    fprintf(bgtime, "%ld\n", mud_time_to_secs(when));
    fclose(bgtime);
  }
}

void free_player_index(void)
{
  int tp;

  if (!player_table)
    return;

  for (tp = 0; tp <= top_of_p_table; tp++)
    if (player_table[tp].name)
      free(player_table[tp].name);

  free(player_table);
  player_table = NULL;
  top_of_p_table = 0;
}


/* new version to build the player index for the ascii pfiles
 * generate index table for the player file
 */
void build_player_index(void)
{
  int rec_count = 0, i, retval;
  FILE *plr_index;
  char index_name[80], line[READ_SIZE], bits[64];
  char arg2[80];
  int save_index = FALSE;

  snprintf(index_name, sizeof(index_name), "%s", PLR_INDEX_FILE);
  if (!(plr_index = fopen(index_name, "r")))
  {
    top_of_p_table = -1;
    log("No player index file!  First new char will be IMP!");
    return;
  }

  /* count the number of players in the index */
  while (get_line(plr_index, line))
    if (*line != '~')
      rec_count++;
  rewind(plr_index);

  if (rec_count == 0)
  {
    player_table = NULL;
    top_of_p_file = top_of_p_table = -1;
    return;
  }
  log("   %d players in database.", rec_count);

  CREATE(player_table, struct player_index_element, rec_count);
  for (i = 0; i < rec_count; i++)
  {
    get_line(plr_index, line);
    if ((retval = sscanf(line, "%ld %s %d %s %ld %ld %hd %hd", &player_table[i].id, arg2,
                         &player_table[i].level, bits,  &player_table[i].last, &player_table[i].account,
                         &player_table[i].clan, &player_table[i].rank)) < 8)
    {
      if (retval == 5)
      {
        player_table[i].account = player_table[i].id;
        save_index = TRUE;
      }
      else if (retval == 6)
        save_index = TRUE;
      else
      {
        log("Player Index Error! Line %d.", i);
        exit(1);
      }
    }
    CREATE(player_table[i].name, char, strlen(arg2) + 1);
    strcpy(player_table[i].name, arg2);
    *player_table[i].name = LOWER(*player_table[i].name);
    player_table[i].flags = asciiflag_conv(bits);
    top_idnum = MAX(top_idnum, player_table[i].id);
  }
  fclose(plr_index);
  top_of_p_file = top_of_p_table = i - 1;




  if (save_index)
  {
    int j;
    struct char_data *victim;
    log("    fixing index fields: clans");
    for (j = 0; j <= top_of_p_table; j++)
    {
      CREATE(victim, struct char_data, 1);
      clear_char(victim);
      TEMP_LOAD_CHAR = TRUE;

      if (store_to_char((player_table + j)->name, victim) > -1)
      {
        player_table[j].clan = GET_CLAN(victim);
        player_table[j].rank = GET_CLAN_RANK(victim);
        free_char(victim);
      }
      else
        free(victim);

      TEMP_LOAD_CHAR = FALSE;
    }
    save_player_index();
  }
}

void save_player_index(void)
{
  int i, j, cnt = 0;
  char bits[64];
  FILE *index_file;
  char tempname[MAX_STRING_LENGTH];
  snprintf(tempname, sizeof(tempname), "%s%s", PLR_INDEX_FILE, ".tmp");
  if (!(index_file = fopen(tempname, "w")))
  {
    log("SYSERR:  Could not write player index file");
    ALERT_2;
    return;
  }

  for (i = 0; i <= top_of_p_table; i++)
  {
cnt = 0;
if (*player_table[i].name && 
         !IS_SET(player_table[i].flags, PINDEX_DELETED) && 
         !IS_SET(player_table[i].flags, PINDEX_SELFDELETE)) {
  for (j = 0; j <= top_of_p_table; j++)
  {
    if (cnt == 0 && *player_table[i].name && !strcmp(player_table[i].name, player_table[j].name) &&
         !IS_SET(player_table[i].flags, PINDEX_DELETED) && 
         !IS_SET(player_table[i].flags, PINDEX_SELFDELETE))
    {
cnt++;
      sprintbits(player_table[i].flags, bits);
      *player_table[i].name = LOWER(*player_table[i].name);
      fprintf(index_file, "%ld %s %d %s %ld %ld %hd %hd\n",
              player_table[i].id, player_table[i].name,
              player_table[i].level, *bits ? bits : "0",
              player_table[i].last, player_table[i].account,
              player_table[i].clan, player_table[i].rank);
    }
  }
}
}
  i = fprintf(index_file, "~\n");
  fclose(index_file);
  if (i == -1)
    remove(tempname);
  else
    rename(tempname, PLR_INDEX_FILE);
}

void remove_player(int pfilepos)
{
  char backup_name[128], pfile_name[128];
  FILE *backup_file;

  if (pfilepos < 0 || !*player_table[pfilepos].name)
    return;

  if (player_table[pfilepos].level >= pfile_backup_minlevel
      && backup_wiped_pfiles && (!selfdelete_fastwipe ||
                                 !IS_SET(player_table[pfilepos].flags,
                                         PINDEX_SELFDELETE)))
  {
    snprintf(backup_name, sizeof(backup_name), "%s/%s.%d", BACKUP_PREFIX,
             player_table[pfilepos].name, (int) time(0));
    if (!(backup_file = fopen(backup_name, "w")))
    {
      log( "PCLEAN: Unable to open backup file %s.",
           backup_name);
      return;
    }

    fprintf(backup_file, "**\n** PFILE: %s\n**\n",
            player_table[pfilepos].name);
    snprintf(pfile_name, sizeof(pfile_name), "%s/%c/%s%s", PLR_PREFIX,
             *player_table[pfilepos].name,
             player_table[pfilepos].name, PLR_SUFFIX);
    //      if(!fcat(pfile_name, backup_file))
    //              fprintf(backup_file, "** (NO FILE)\n");
    fclose(backup_file);

    remove(pfile_name);
  }
  log( "PCLEAN: %s Lev: %d Last: %s",
       player_table[pfilepos].name, player_table[pfilepos].level,
       asctime(localtime(&player_table[pfilepos].last)));
  player_table[pfilepos].name[0] = '\0';
  save_player_index();
}

void clean_pfiles(void)
{
  int i,  timeout = 0;
  time_t tm = time(0);

  for (i = 0; i <= top_of_p_table; i++)
  {
    if (IS_SET(player_table[i].flags, PINDEX_NODELETE))
      continue;
    timeout = -1;

    if ((IS_SET(player_table[i].flags, PINDEX_DELETED)) || (player_table[i].level < 40 &&player_table[i].clan == 12))
    {
      timeout = 90;

      timeout *= SECS_PER_REAL_DAY;
      if ((tm - player_table[i].last) > timeout)
        remove_player(i);
    }
  }
}


/*
 * Thanks to Andrey (andrey@alex-ua.com) for this bit of code, although I
 * did add the 'goto' and changed some "while()" into "do { } while()".
 *	-gg 6/24/98 (technically 6/25/98, but I care not.)
 */
int count_alias_records(FILE * fl)
{
  char key[READ_SIZE], next_key[READ_SIZE];
  char line[READ_SIZE], *scan;
  int total_keywords = 0;

  /* get the first keyword line */
  get_one_line(fl, key);

  while (*key != '$')
  {
    /* skip the text */
    do
    {
      get_one_line(fl, line);
      if (feof(fl))
        goto ackeof;
    }
    while (*line != '#');

    /* now count keywords */
    scan = key;
    do
    {
      scan = one_word(scan, next_key);
      if (*next_key)
        ++total_keywords;
    }
    while (*next_key);

    /* get next keyword line (or $) */
    get_one_line(fl, key);

    if (feof(fl))
      goto ackeof;
  }

  return (total_keywords);

  /* No, they are not evil. -gg 6/24/98 */
ackeof:
  log("SYSERR: Unexpected end of  file.");
  ALERT_1;
  exit(1);			/* Some day we hope to handle these things better... */
  return (-1);
}

/* function to count how many hash-mark delimited records exist in a file */
int count_hash_records(FILE * fl)
{
  char buf[128];
  int count = 0;

  while (fgets(buf, 128, fl))
    if (*buf == '#')
      count++;

  return (count);
}

void index_boot(int mode)
{
  const char *index_filename, *prefix = NULL;
  FILE *index = NULL, *db_file = NULL;
  int rec_count = 0, size[2];
  char buf2[MAX_INPUT_LENGTH];
  char buf1[MAX_INPUT_LENGTH];

  switch (mode)
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
    log("SYSERR: Unknown subcommand %d to index_boot!", mode);
    exit(1);
  }
  if (mini_mud)
    index_filename = MINDEX_FILE;
  else
    index_filename = INDEX_FILE;
  //TODO: fix dynamic loading for minimud mode
  if (mode == DB_BOOT_HLP)
  {


    snprintf(buf2, sizeof(buf2), "%s%s", prefix, index_filename);

    if ((index = fopen(buf2, "ro")) == NULL)
    {
      log("SYSERR: opening index file '%s': %s", buf2, strerror(errno));
      return;
    }

    /* first, count the number of records in the file so we can malloc */
    if (fscanf(index, "%s\n", buf1) == -1)
    {
      //      load_xml_help(prefix);
      fclose(index);
      return;
    }
    while (*buf1 != '$')
    {
      snprintf(buf2, sizeof(buf2), "%s%s", prefix, buf1);
      log(buf2);
      if (!(db_file = fopen(buf2, "rb")))
      {
        log("SYSERR: File '%s' listed in '%s/%s': %s", buf2, prefix,
            index_filename, strerror(errno));
        fscanf(index, "%s\n", buf1);
        continue;
      }
      else
        rec_count += count_alias_records(db_file);

      fclose(db_file);
      fscanf(index, "%s\n", buf1);
    }
    /* Exit if 0 records, unless this is shops */
    if (!rec_count)
    {
      log("SYSERR: boot error - 0 records counted in %s/%s.", prefix,index_filename);
      exit(1);
    }
  }
  else
  {
    struct zone_list_data *temp;
    for (temp = zone_list;temp;temp = temp->next)
    {
      snprintf(buf2, sizeof(buf2), "%s%d%s", temp->zone, temp->num, prefix);
      if (!(db_file = fopen(buf2, "r+")))
      {
        log("SYSERR: File '%s' listed in '%s/%s': %s", buf2, prefix,
            index_filename, strerror(errno));
        continue;
      }
      else
      {
        if (mode == DB_BOOT_ZON)
          rec_count++;
        else
          rec_count += count_hash_records(db_file);
      }
      fclose(db_file);
    }
    /* Exit if 0 records, unless this is shops */
    if (!rec_count)
    {
      if (mode == DB_BOOT_SHP)
        return;
      log("SYSERR: boot error - 0 records counted in %s/%s.", prefix,index_filename);
      exit(1);
    }
  }

  /* Any idea why you put this here Jeremy?
     rec_count++; */

  /*
   * NOTE: "bytes" does _not_ include strings or other later malloc'd things.
   */
  switch (mode)
  {
  case DB_BOOT_TRG:
    CREATE(trig_index, struct index_data *, rec_count);
    size[0] = sizeof(struct index_data) * rec_count;
    log("   %d triggers, %d bytes.", rec_count, size[0]);
    break;
  case DB_BOOT_WLD:
    //CREATE(world, struct room_data, rec_count);
    size[0] = sizeof(struct room_data) * rec_count;
    log("   %d rooms, %d bytes.", rec_count, size[0]);
    break;
  case DB_BOOT_MOB:
    CREATE(mob_proto, struct char_data, rec_count);
    CREATE(mob_index, struct index_data, rec_count);
    size[0] = sizeof(struct index_data) * rec_count;
    size[1] = sizeof(struct char_data) * rec_count;
    log("   %d mobs, %d bytes in index, %d bytes in prototypes.",
        rec_count, size[0], size[1]);
    break;
  case DB_BOOT_OBJ:
    CREATE(obj_proto, struct obj_data, rec_count);
    CREATE(obj_index, struct index_data, rec_count);
    size[0] = sizeof(struct index_data) * rec_count;
    size[1] = sizeof(struct obj_data) * rec_count;
    log("   %d objs, %d bytes in index, %d bytes in prototypes.",
        rec_count, size[0], size[1]);
    break;
  case DB_BOOT_ZON:
    CREATE(zone_table, struct zone_data, rec_count);
    size[0] = sizeof(struct zone_data) * rec_count;
    log("   %d zones, %d bytes.", rec_count, size[0]);
    break;
  case DB_BOOT_HLP:
    CREATE(help_table, struct help_index_element, rec_count);
    size[0] = sizeof(struct help_index_element) * rec_count;
    log("   %d entries, %d bytes.", rec_count, size[0]);
    break;
  }
  if (mode == DB_BOOT_HLP)
  {
    rewind(index);
    fscanf(index, "%s\n", buf1);
    while (*buf1 != '$')
    {
      snprintf(buf2, sizeof(buf2), "%s%s", prefix, buf1);
      if (!(db_file = fopen(buf2, "r")))
      {
        log("SYSERR: %s: %s", buf2, strerror(errno));
        exit(1);
      }
      load_help(db_file);
      fclose(db_file);
      fscanf(index, "%s\n", buf1);

    }
    fclose(index);

  }
  else
  {
    struct zone_list_data *temp;
    //start
    for (temp=zone_list;temp;temp = temp->next)
    {
      snprintf(buf2, sizeof(buf2), "%s%d%s", temp->zone, temp->num, prefix);
      if (!(db_file = fopen(buf2, "r")))
      {
        log("SYSERR: %s: %s", buf2, strerror(errno));
        exit(1);
      }
      switch (mode)
      {
      case DB_BOOT_TRG:
      case DB_BOOT_WLD:
      case DB_BOOT_OBJ:
      case DB_BOOT_MOB:
        discrete_load(db_file, mode, buf2, temp->num);
        break;
      case DB_BOOT_ZON:
        load_zones(db_file, buf2);
        break;
      case DB_BOOT_SHP:
        boot_the_shops(db_file, buf2, rec_count);
        break;
      }

      fclose(db_file);
    }
    //end
  }
}



void discrete_load(FILE * fl, int mode, char *filename, zone_vnum zon)
{
  int nr = -1, last = 0, version = 1;
  char line[READ_SIZE];

  const char *modes[] =
    { "world", "mob", "obj", "ZON", "SHP", "HLP", "trg" };
  /* modes positions correspond to DB_BOOT_xxx in db.h */

  // log("INFO: Reading from file %s.", filename);

  for (;;)
  {
    /*
     * we have to do special processing with the obj files because they have
     * no end-of-record marker :(
     */
    if (mode != DB_BOOT_OBJ || nr < 0)
      if (!get_line(fl, line))
      {
        if (nr == -1)
        {
          log("SYSERR: %s file %s is empty!", modes[mode],
              filename);
        }
        else
        {
          log("SYSERR: Format error in %s after %s #%d\n"
              "...expecting a new %s, but file ended!\n"
              "(maybe the file is not terminated with '$'?)",
              filename, modes[mode], nr, modes[mode]);
        }
        exit(1);
      }
    if (*line == '$')
      return;

    else if (*line == '@')
    {
      if (sscanf(line, "@Version: %d", &version) != 1)
      {
        log("SYSERR: Format error after %s #%d", modes[mode],
            last);
        log("SYSERR: ...Line: %s", line);
        exit(1);
      }
    }
    if (*line == '#')
    {
      last = nr;
      if (sscanf(line, "#%d", &nr) != 1)
      {
        log("SYSERR: Format error after %s #%d", modes[mode],
            last);
        exit(1);
      }
      if (nr >= 99999)
        return;
      else
        switch (mode)
        {
        case DB_BOOT_TRG:
          parse_trigger(fl, nr, zon);
          break;
        case DB_BOOT_WLD:
          parse_room(fl, nr, zon);
          break;
        case DB_BOOT_MOB:
          parse_mobile(fl, nr, zon);
          break;
        case DB_BOOT_OBJ:
          strcpy(line, parse_object(fl, nr, zon));
          break;
        }
    }
    else
    {
      log("SYSERR: Format error in %s file %s near %s #%d",
          modes[mode], filename, modes[mode], nr);
      log("SYSERR: ... offending line: '%s'", line);
      exit(1);
    }
  }
}

bitvector_t asciiflag_conv(char *flag)
{
  bitvector_t flags = 0;
  int is_number = TRUE;
  register char *p;

  for (p = flag; *p; p++)
  {
    if (islower(*p))
      flags |= 1 << (*p - 'a');
    else if (isupper(*p))
      flags |= 1 << (26 + (*p - 'A'));

    if (!isdigit(*p))
      is_number = FALSE;
  }

  if (is_number)
    flags = atol(flag);

  return (flags);
}

char fread_letter(FILE * fp)
{
  char c;
  do
  {
    c = getc(fp);
  }
  while (isspace(c));
  return c;
}

/* load the rooms */
void parse_room(FILE * fl, int virtual_nr, zone_vnum zon)
{
  static struct room_data * room_nr = NULL;
  zone_rnum zone = 0;
  int t[10], i;
  char line[256], flags[128], flags2[128], flags3[128], flags4[128];
  struct extra_descr_data *new_descr;
  struct forest_data *new_forest = NULL;
  char buf2[MAX_INPUT_LENGTH];
  char letter;

  if (virtual_nr >= HIGHEST_VNUM)
  {
    log("Please increase HIGHEST_VNUM, or keep zone numbers below %d", HIGHEST_VNUM);
    exit(1);
  }

  //log("Zone Load: %d - %d (%d)", zone, real_zone(zon), zon);
  if ((zone = real_zone(zon)) == NOWHERE)
  {
    log("Real zone for zone %d - is nowhere", zon);
    exit(1);
  }


  snprintf(buf2, sizeof(buf2), "#room %d", virtual_nr);//for fread_string

  if (virtual_nr < zone_table[zone].bot)
  {
    log("SYSERR: Room #%d is below zone %d's bottom %d.", virtual_nr, zone_table[zone].number,zone_table[zone].bot);
    exit(1);
  }				//mord??
  while (virtual_nr > zone_table[zone].top)
  {
    if ((++zone) > top_of_zone_table)
    {
      log("SYSERR: Room %d is outside of any zone.", virtual_nr);
      exit(1);
    }
  }
  if (world_vnum[virtual_nr] != NULL)
  {
    log("Room: %d somehow already has been made when it is loading now: %s", virtual_nr,world_vnum[virtual_nr]->name);
    free_room_strings(world_vnum[virtual_nr]);
if (SCRIPT(world_vnum[virtual_nr]))
      extract_script(world_vnum[virtual_nr], WLD_TRIGGER);
    /* free script proto list */
    free_proto_script(world_vnum[virtual_nr], WLD_TRIGGER);
    free(world_vnum[virtual_nr]);
    world_vnum[virtual_nr] = NULL;
  }
  room_nr=(struct room_data *)malloc(sizeof(struct room_data));
  room_nr->zone = real_zone(zon);
  room_nr->number = virtual_nr;
  room_nr->name = fread_string(fl, buf2);
  room_nr->description = fread_string(fl, buf2);
  room_nr->smell = fread_string(fl, buf2);
  room_nr->listen = fread_string(fl, buf2);
  room_nr->proto_script = NULL;
  room_nr->script = NULL;
  room_nr->affects = NULL;
  room_nr->mine.num = -1;
  room_nr->mine.dif = -1;
  world_vnum[virtual_nr]=room_nr;

  if (!get_line(fl, line))
  {
    log("SYSERR: Expecting roomflags/sector type of room #%d but file ended!", virtual_nr);
    exit(1);
  }

  if (sscanf(line, " %d %s %s %s %s %d ", t, flags, flags2, flags3, flags4, t + 2) != 6)
  {
    log("SYSERR: Format error in roomflags/sector type of room #%d", virtual_nr);
    exit(1);
  }
  /* t[0] is the zone number; ignored with the zone-file system */
  room_nr->room_flags[0] = asciiflag_conv(flags);
  room_nr->room_flags[1] = asciiflag_conv(flags2);
  room_nr->room_flags[2] = asciiflag_conv(flags3);
  room_nr->room_flags[3] = asciiflag_conv(flags4);
  room_nr->sector_type = t[2];




  room_nr->func = NULL;
  room_nr->contents = NULL;
  room_nr->people = NULL;
  room_nr->light = 0;	/* Zero light sources */

  for (i = 0; i < NUM_OF_DIRS; i++)
    room_nr->dir_option[i] = NULL;

  room_nr->ex_description = NULL;
  room_nr->look_above_description = NULL;
  room_nr->look_behind_description = NULL;
  room_nr->look_under_description = NULL;

  for (;;)
  {
    if (!get_line(fl, line))
    {
      log("SYSERR: Format error in room #%d (expecting D/E/S)", virtual_nr);
      exit(1);
    }
    switch (*line)
    {
    case 'D':

      setup_dir(fl, room_nr, atoi(line + 1));

      break;
    case 'E':
      CREATE(new_descr, struct extra_descr_data, 1);
      new_descr->keyword = fread_string(fl, buf2);
      new_descr->description = fread_string(fl, buf2);
      /* fix for crashes in the editor when formatting
       * - e-descs are assumed to end with a \r\n
       * -- Welcor 09/03 
       */
      {
        char *end = strchr(new_descr->description, '\0');
        if (end > new_descr->description && *(end-1) != '\n')
        {
          CREATE(end, char, strlen(new_descr->description)+3);
          sprintf(end, "%s\r\n", new_descr->description); /* snprintf ok : size checked above*/
          free(new_descr->description);
          new_descr->description = end;
        }
      }
      new_descr->next = room_nr->ex_description;
      room_nr->ex_description = new_descr;
      break;
    case 'A':
      CREATE(new_descr, struct extra_descr_data, 1);
      new_descr->keyword = fread_string(fl, buf2);
      new_descr->description = fread_string(fl, buf2);
      /* fix for crashes in the editor when formatting
       * - e-descs are assumed to end with a \r\n
       * -- Welcor 09/03 
       */
      {
        char *t = strchr(new_descr->description, '\0');
        if (t > new_descr->description && *(t-1) != '\n')
        {
          CREATE(t, char, strlen(new_descr->description)+3);
          sprintf(t, "%s\r\n", new_descr->description); /* sprintf ok : size checked above*/
          free(new_descr->description);
          new_descr->description = t;
        }
      }
      new_descr->next = room_nr->look_above_description;
      room_nr->look_above_description = new_descr;
      break;
    case 'B':
      CREATE(new_descr, struct extra_descr_data, 1);
      new_descr->keyword = fread_string(fl, buf2);
      new_descr->description = fread_string(fl, buf2);
      /* fix for crashes in the editor when formatting
       * - e-descs are assumed to end with a \r\n
       * -- Welcor 09/03 
       */
      {
        char *t = strchr(new_descr->description, '\0');
        if (t > new_descr->description && *(t-1) != '\n')
        {
          CREATE(t, char, strlen(new_descr->description)+3);
          sprintf(t, "%s\r\n", new_descr->description); /* sprintf ok : size checked above*/
          free(new_descr->description);
          new_descr->description = t;
        }
      }
      new_descr->next = room_nr->look_behind_description;
      room_nr->look_behind_description = new_descr;
      break;
    case 'U':
      CREATE(new_descr, struct extra_descr_data, 1);
      new_descr->keyword = fread_string(fl, buf2);
      new_descr->description = fread_string(fl, buf2);
      /* fix for crashes in the editor when formatting
       * - e-descs are assumed to end with a \r\n
       * -- Welcor 09/03 
       */
      {
        char *t = strchr(new_descr->description, '\0');
        if (t > new_descr->description && *(t-1) != '\n')
        {
          CREATE(t, char, strlen(new_descr->description)+3);
          sprintf(t, "%s\r\n", new_descr->description); /* sprintf ok : size checked above*/
          free(new_descr->description);
          new_descr->description = t;
        }
      }
      new_descr->next = room_nr->look_under_description;
      room_nr->look_under_description = new_descr;
      break;
    case 'M':
      room_nr->mine.num = fread_number(fl);
      room_nr->mine.dif = fread_number(fl);
      room_nr->mine.tool = fread_number(fl);
      break;
    case 'S':		/* end of room */
      /* DG triggers -- script is defined after the end of the room */
      letter = fread_letter(fl);
      ungetc(letter, fl);
      while (letter == 'T')
      {
        dg_read_trigger(fl, room_nr, WLD_TRIGGER);
        letter = fread_letter(fl);
        ungetc(letter, fl);
      }
      if (virtual_nr > top_of_world)
        top_of_world = virtual_nr;
      if (SECT(room_nr) == SECT_FOREST)
      {
        CREATE(new_forest, struct forest_data, 1);
        new_forest->room = room_nr;
        new_forest->next = forest;
        forest = new_forest;
        forest_room++;
      }
      add_room_to_mine(room_nr);
      return;
    default:
      log("SYSERR: Format error in room #%d (expecting D/E/S/B/U/A)", virtual_nr);
      exit(1);
    }
  }
}

void free_forests(struct forest_data *this_forest)
{
  if (!this_forest)
    return;

  if (this_forest->next)
    free_forests(this_forest->next);

  free(this_forest);
}

/* read direction data */
void setup_dir(FILE * fl, room_rnum room, int dir)
{
  int t[5];
  char line[256];
  char buf2[MAX_INPUT_LENGTH];

  sprintf(buf2,"room #%d, direction D%d",room->number,dir);
  if (dir < 0 || dir >= NUM_OF_DIRS)
  {
    char *di;
    log("Doors are only in the range of 0 to %d, this door isn't: %s\r\n(gulping door)", NUM_OF_DIRS, buf2);
    di = fread_string(fl, buf2);
    free_string(&di);
    di = fread_string(fl, buf2);
    free_string(&di);
    if (!get_line(fl, line))
    {
      log("SYSERR: Format error, %s", buf2);
      exit(1);
    }
    return;
  }
  else
  {
    if (room->dir_option[dir] == NULL)
    {
      CREATE(room->dir_option[dir], struct room_direction_data, 1);
      room->dir_option[dir]->general_description = NULL;
      room->dir_option[dir]->keyword = NULL;
    }
    else
    {
      free_string(&room->dir_option[dir]->keyword);
      free_string(&room->dir_option[dir]->general_description);
    }
    room->dir_option[dir]->general_description = fread_string(fl, buf2);
    room->dir_option[dir]->keyword = fread_string(fl, buf2);
    room->dir_option[dir]->nosave = 0;
  }

  // log("%s, %s.", world[room].dir_option[dir]->general_description, world[room].dir_option[dir]->keyword);

  if (!get_line(fl, line))
  {
    log("SYSERR: Format error, %s", buf2);
    exit(1);
  }
  if (sscanf(line, " %d %d %d ", t, t + 1, t + 2) != 3)
  {
    log("SYSERR: Format error, %s", buf2);
    exit(1);
  }
  if (t[0] == 1)
    room->dir_option[dir]->exit_info = EX_ISDOOR;
  else if (t[0] == 2)
    room->dir_option[dir]->exit_info = EX_ISDOOR | EX_PICKPROOF;
  else
    room->dir_option[dir]->exit_info = 0;

  room->dir_option[dir]->key = ((t[1] == -1 || t[1] == 65535) ? NOTHING : t[1]);

  /* added check for rooms to 0 to point to nowhere - mord*/
  room->dir_option[dir]->to_room_tmp = ((t[2] == -1  || t[2] == 0) ? NOWHERE : t[2]);
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
void renum_world(void)
{
  register int door;
  room_vnum room;

  for (room = 0; room <= top_of_world; room++)
    for (door = 0; door < NUM_OF_DIRS; door++)
      if (world_vnum[room] != NULL)
        if (world_vnum[room]->dir_option[door])
          if (world_vnum[room]->dir_option[door]->to_room_tmp != NOWHERE)
            world_vnum[room]->dir_option[door]->to_room =
              real_room(world_vnum[room]->dir_option[door]->to_room_tmp);
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
void renum_zone_table(void)
{
  int cmd_no;
  int a, b, c, olda, oldb, oldc;
  zone_rnum zone;
  char buf[128];

  for (zone = 0; zone <= top_of_zone_table; zone++)
    for (cmd_no = 0; ZCMD.command != 'S'; cmd_no++)
    {
      a = b = c = 0;
      olda = ZCMD.arg1;
      oldb = ZCMD.arg2;
      oldc = ZCMD.arg3;
      switch (ZCMD.command)
      {
      case 'M':
        a = ZCMD.arg1 = real_mobile(ZCMD.arg1);
        c = ZCMD.arg3;
        break;
      case 'O':
        a = ZCMD.arg1 = real_object(ZCMD.arg1);
        if (ZCMD.arg3 != NOWHERE)
          c = ZCMD.arg3;
        break;
      case 'G':
        a = ZCMD.arg1 = real_object(ZCMD.arg1);
        break;
      case 'E':
        a = ZCMD.arg1 = real_object(ZCMD.arg1);
        break;
      case 'P':
        a = ZCMD.arg1 = real_object(ZCMD.arg1);
        c = ZCMD.arg3 = real_object(ZCMD.arg3);
        break;
      case 'D':
        a = ZCMD.arg1;
        break;
      case 'R':		/* rem obj from room */
        a = ZCMD.arg1;
        b = ZCMD.arg2 = real_object(ZCMD.arg2);
        break;
      case 'T':		/* a trigger */
        b = ZCMD.arg2 = real_trigger(ZCMD.arg2);	// added by mord
        c = ZCMD.arg3;	// added by mord
        break;
      case 'V':		/* trigger variable assignment */
        b = ZCMD.arg3;	//added by mord
        break;
      case 'B':
        a = ZCMD.arg1 = real_object(ZCMD.arg1);
        if (ZCMD.arg3 != NOWHERE)
          c = ZCMD.arg3 = real_object(ZCMD.arg3);
        break;
      }
      if (a == NOWHERE || b == NOWHERE || c == NOWHERE)
      {
        if (!mini_mud)
        {
          snprintf(buf, sizeof(buf),
                   "Invalid vnum %d, cmd disabled",
                   a == NOWHERE ? olda : b ==
                   NOWHERE ? oldb : oldc);
          log_zone_error(zone, cmd_no, buf);
        }
        ZCMD.command = '*';
      }
      else if (ZCMD.arg3 < 0)
        ZCMD.arg3 = 0;
      else if (ZCMD.arg4 < 0)
        ZCMD.arg4 = 0;
    }
}



void parse_simple_mob(FILE * mob_f, int i, int nr)
{
  int j, t[10];
  char line[256];
  int k = 1;

  //mob_proto[i].real_abils.str = (number(3, 20));
  mob_proto[i].real_abils.intel = (number(3, 20));
  mob_proto[i].real_abils.wis = (number(3, 20));
  mob_proto[i].real_abils.dex = (number(3, 20));
  mob_proto[i].real_abils.con = (number(3, 20));
  mob_proto[i].real_abils.cha = (number(3, 20));

  if (!get_line(mob_f, line))
  {
    log("SYSERR: Format error in mob #%d, file ended after S flag!",
        nr);
    exit(1);
  }

  if (sscanf(line, " %d %d %d %dd%d+%d %dd%d+%d ",
             t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6, t + 7,
             t + 8) != 9)
  {
    log("SYSERR: Format error in mob #%d, first line after S flag\n"
        "...expecting line of form '# # # #d#+# #d#+#'", nr);
    exit(1);
  }

  GET_LEVEL(mob_proto + i) = k = IRANGE(1, t[0], MAX_MOB_LEVELS -1) ;
  mob_proto[i].points.hitroll = mob_stats[k].hitroll;
  mob_proto[i].points.armor = mob_stats[k].ac;

  /* max hit = 0 is a flag that H, M, V is xdy+z */
  mob_proto[i].points.max_hit = (dice(mob_stats[k].hp_dice, mob_stats[k].hp_sides) + mob_stats[k].hp_bonus);
  mob_proto[i].points.hit = mob_proto[i].points.max_hit;
  mob_proto[i].points.mana = 10;
  mob_proto[i].points.move = 5000;
  mob_proto[i].points.stamina = 150 + (k*2);

  mob_proto[i].points.max_mana = 10;
  mob_proto[i].points.max_move = 5000;
  mob_proto[i].points.max_stamina = 100;

  mob_proto[i].mob_specials.damnodice = mob_stats[k].dam_dice;
  mob_proto[i].mob_specials.damsizedice = mob_stats[k].dam_sides;
  mob_proto[i].points.damroll = mob_stats[k].dam_bonus;
  mob_proto[i].mob_specials.tier = 0;
  mob_proto[i].mob_specials.subskill = TYPE_UNDEFINED;

  if (!get_line(mob_f, line))
  {
    log("SYSERR: Format error in mob #%d, second line after S flag\n"
        "...expecting line of form '# #', but file ended!", nr);
    exit(1);
  }

  if (sscanf(line, " %d %d ", t, t + 1) != 2)
  {
    log("SYSERR: Format error in mob #%d, second line after S flag\n"
        "...expecting line of form '# #'", nr);
    exit(1);
  }


  GET_EXP(mob_proto + i) = mob_stats[k].exp;
  GET_GROUP_EXP(mob_proto + i) = 0;

  GET_PERC(mob_proto + i) = 100;

  if (!get_line(mob_f, line))
  {
    log("SYSERR: Format error in last line of mob #%d\n"
        "...expecting line of form '# # #', but file ended!", nr);
    exit(1);
  }

  if (sscanf(line, " %d %d %d %d ", t, t + 1, t + 2, t + 3) != 4)
  {
    log("SYSERR: Format error in last line of mob #%d\n"
        "...expecting line of form '# # # #'", nr);
    exit(1);
  }

  mob_proto[i].char_specials.position = t[0];
  mob_proto[i].mob_specials.default_pos = t[1];
  mob_proto[i].player.sex = t[2];
  if ((mob_proto[i].mob_specials.race = t[3] ) != MOB_RACE_ANIMAL)
    GET_GOLD(mob_proto + i) = (t[0] ? mob_stats[k].gold : 0);

  mob_proto[i].player.chclass = 0;
  mob_proto[i].player.weight = 200;
  mob_proto[i].player.height = 198;
  mob_proto[i].real_abils.str = 25;
  mob_proto[i].real_abils.str_add = 100;

  /*
   * these are now save applies; base save numbers for MOBs are now from
   * the warrior save table.
   */
  for (j = 0; j < 5; j++)
    GET_SAVE(mob_proto + i, j) = 0;
}


/*
 * interpret_espec is the function that takes espec keywords and values
 * and assigns the correct value to the mob as appropriate.  Adding new
 * e-specs is absurdly easy -- just add a new CASE statement to this
 * function!  No other changes need to be made anywhere in the code.
 *
 * CASE		: Requires a parameter through 'value'.
 * BOOL_CASE	: Being specified at all is its value.
 */

#define CASE(test)	\
	if (value && !matched && !strcmp(keyword, test) && (matched = TRUE))

#define BOOL_CASE(test)	\
	if (!value && !matched && !strcmp(keyword, test) && (matched = TRUE))

#define RANGE(low, high)	\
	(num_arg = MAX((low), MIN((high), (num_arg))))

void interpret_espec(const char *keyword, const char *value, int i, int nr)
{
  int num_arg = 0, matched = FALSE;

  /*
   * If there isn't a colon, there is no value.  While Boolean options are
   * possible, we don't actually have any.  Feel free to make some.
   */
  if (value)
    num_arg = atoi(value);

  CASE("BareHandAttack")
  {
    RANGE(0, 99);
    mob_proto[i].mob_specials.attack_type = num_arg;
  }

  CASE("Str")
  {
    RANGE(3, 25);
    mob_proto[i].real_abils.str = num_arg;
  }

  CASE("StrAdd")
  {
    RANGE(0, 100);
    mob_proto[i].real_abils.str_add = num_arg;
  }

  CASE("Int")
  {
    RANGE(3, 25);
    mob_proto[i].real_abils.intel = num_arg;
  }

  CASE("Wis")
  {
    RANGE(3, 25);
    mob_proto[i].real_abils.wis = num_arg;
  }

  CASE("Dex")
  {
    RANGE(3, 25);
    mob_proto[i].real_abils.dex = num_arg;
  }

  CASE("Con")
  {
    RANGE(3, 25);
    mob_proto[i].real_abils.con = num_arg;
  }

  CASE("Cha")
  {
    RANGE(3, 25);
    mob_proto[i].real_abils.cha = num_arg;
  }

  CASE("Class")
  {
    RANGE(0, NUM_MOB_CLASSES-1);
    mob_proto[i].player.chclass = num_arg;
  }

  CASE("Tier")
  {
    RANGE(0, 4);
    mob_proto[i].mob_specials.tier = num_arg;
  }

  CASE("Subskill")
  {
    RANGE(-1, TOP_SUB_DEFINE);
    mob_proto[i].mob_specials.subskill = num_arg;
  }
  CASE("Skin")
  {
    RANGE(-1, 999999);
    mob_proto[i].mob_specials.skin = num_arg;
  }

  if (!matched)
  {
    log("SYSERR: Warning: unrecognized espec keyword %s in mob #%d",
        keyword, nr);
  }
}

#undef CASE
#undef BOOL_CASE
#undef RANGE

void parse_espec(char *buf, int i, int nr)
{
  char *ptr;

  if ((ptr = strchr(buf, ':')) != NULL)
  {
    *(ptr++) = '\0';
    while (isspace(*ptr))
      ptr++;
  }
  interpret_espec(buf, ptr, i, nr);
}


void parse_enhanced_mob(FILE * mob_f, int i, int nr)
{
  char line[256];

  parse_simple_mob(mob_f, i, nr);

  while (get_line(mob_f, line))
  {
    if (!strcmp(line, "E"))	/* end of the enhanced section */
      return;
    else if (*line == '#')
    {	/* we've hit the next mob, maybe? */
      log("SYSERR: Unterminated E section in mob #%d", nr);
      exit(1);
    }
    else
      parse_espec(line, i, nr);
  }

  log("SYSERR: Unexpected end of file reached after mob #%d", nr);
  exit(1);
}

struct combine_data *add_base_link(int i, int vnum)
{
  struct combine_data *link = NULL;
  CREATE(link, struct combine_data, 1);
  link->vnum = vnum;
  link->joined = NULL;
  link->next = mob_proto[i].mob_specials.join_list;
  mob_proto[i].mob_specials.join_list = link;

  return link;

}
struct combine_data *add_base_link_mob(CHAR_DATA *mob, int vnum)
{
  struct combine_data *link = NULL;
  CREATE(link, struct combine_data, 1);
  link->vnum = vnum;
  link->joined = NULL;
  link->next = mob->mob_specials.join_list;
  mob->mob_specials.join_list = link;
  return link;

}
/* adds to current returns next
   Assumes all links pre created
   Gives new segment its head_join
*/
struct combine_data *add_full_link(CHAR_DATA *mob, struct combine_data *current, CHAR_DATA *segment)
{

  if (segment!=NULL)
  {
    current->joined = segment;
    segment->mob_specials.head_join = mob;
    SET_BIT_AR(MOB_FLAGS(segment), MOB_SENTINEL);
    GET_EXP(mob) += (int)(GET_EXP(segment) * 0.3);
  }
  return current->next;
}

struct combine_data *copy_proto_link(struct combine_data *proto)
{
  struct combine_data *temp = NULL, *temp2 = NULL;

  if (proto == NULL)
    return NULL;

  if (proto->next != NULL)
    temp2 = copy_proto_link(proto->next);

  CREATE(temp, struct combine_data, 1);

  temp->vnum = proto->vnum;
  temp->joined = proto->joined;
  temp->next = temp2;

  return temp;
}

void load_links(CHAR_DATA *mob)
{
  struct combine_data *temp = NULL;
  if ((temp = mob->mob_specials.join_list) == NULL)
    return;

  while (temp)
    temp = add_full_link(mob, temp, read_mobile(temp->vnum, VIRTUAL));

}

int move_link_room(CHAR_DATA *mob, room_rnum room)
{
  struct combine_data *temp = NULL;
  if (mob->mob_specials.head_join != NULL)
    return 0;
  if ((temp = mob->mob_specials.join_list) == NULL)
    return 1;

  while (temp != NULL)
  {
    if (temp->joined)
    {
      if (IN_ROOM(temp->joined) != NULL)
        char_from_room(temp->joined);

      char_to_room(temp->joined, room);
    }

    temp = temp->next;
  }
  return 1;
}

void die_link(CHAR_DATA *mob)
{
  struct combine_data *temp = NULL, *link = NULL;
  CHAR_DATA *head;
  if (mob == NULL)
    return;
  if ((head = mob->mob_specials.head_join) == NULL)
    return;
  link = head->mob_specials.join_list;
  while (link)
  {
    if (link->joined == mob)
      break;
    link = link->next;
  }

  if (link != NULL)
  {
    REMOVE_FROM_LIST(link, head->mob_specials.join_list, next);
    free(link);
    link = NULL;
    mob->mob_specials.head_join = NULL;
    return;
  }
}

void delete_one_join(CHAR_DATA *mob, int i)
{
  int j = 0;
  struct combine_data *temp, *prev = NULL;


  if (mob == NULL)
    return;
  temp = mob->mob_specials.join_list;

  while (temp)
  {
    if ((++j == i) && prev == NULL)
    {
      mob->mob_specials.join_list = temp->next;
      free(temp);
      temp = NULL;
      return;
    }
    else if (j == i)
    {
      prev->next = temp->next;
      free(temp);
      temp = NULL;
      return;
    }
    prev = temp;
    temp = temp->next;
  }

}

struct combine_data *extract_all_links(struct combine_data *proto)
{
  struct combine_data *temp = NULL;

  if (proto == NULL)
    return NULL;

  if (proto->next != NULL)
    extract_all_links(proto->next);

  if (proto->joined)
  {
    proto->joined->mob_specials.head_join = NULL;
    extract_char(proto->joined);
  }
  free(proto);
  proto = NULL;

  return temp;
}

int join_count(CHAR_DATA *mob)
{
  struct combine_data *temp = mob->mob_specials.join_list;
  int i = 0;

  while (temp)
  {
    ++i;
    temp = temp->next;
  }
  return i;
}

void die_link_head(CHAR_DATA *mob)
{
  if (mob->mob_specials.join_list != NULL)
    extract_all_links(mob->mob_specials.join_list);
  mob->mob_specials.join_list = NULL;
  return;
}

void extract_linked_mob(CHAR_DATA *mob)
{
  die_link_head(mob);
  //die_link(mob);
  return;
}

void free_join_list(struct combine_data *list)
{
  if (list == NULL)
    return;

  if (list->next != NULL)
    free_join_list(list->next);

  free(list);
  list = NULL;
}

void parse_jspec(char *buf, int i, int nr)
{
  int vnum = atoi(buf);
  if (vnum > 0) // assume its valid, we have to
    add_base_link(i, vnum);
}


void parse_joined_mob(FILE * mob_f, int i, int nr)
{
  char line[256];

  parse_enhanced_mob(mob_f,i,nr);

  while (get_line(mob_f, line))
  {
    if (!strcmp(line, "J"))	/* end of the enhanced section */
      return;
    else if (*line == '#')
    {	/* we've hit the next mob, maybe? */
      log("SYSERR: Unterminated J section in mob #%d", nr);
      exit(1);
    }
    else
      parse_jspec(line, i, nr);
  }

  log("SYSERR: Unexpected end of file reached after mob #%d", nr);
  exit(1);
}


void parse_mobile(FILE * mob_f, int nr, zone_vnum zon)
{
  void set_race(struct char_data *ch, int race);

  static int i = 0;
  int j, t[10];
  int vn, count;
  char junk[8];
  char line[READ_SIZE], *tmpptr = NULL, letter;
  char f1[128], f2[128], f3[128], f4[128], f5[128], f6[128], f7[128],
  f8[128];
  char buf2[MAX_INPUT_LENGTH];

  mob_index[i].vnum = nr;
  mob_index[i].number = 0;
  mob_index[i].func = NULL;

  clear_char(mob_proto + i);


  /*
   * Mobiles should NEVER use anything in the 'player_specials' structure.
   * The only reason we have every mob in the game share this copy of the
   * structure is to save newbie coders from themselves. -gg 2/25/98
   */
  mob_proto[i].player_specials = &dummy_mob;
  snprintf(buf2, sizeof(buf2), "mob vnum %d", nr);	/* sprintf: OK (for 'buf2 >= 19') */

  /***** String data *****/
  mob_proto[i].player.name = fread_string(mob_f, buf2);
  tmpptr = mob_proto[i].player.short_descr = fread_string(mob_f, buf2);
  if (tmpptr && *tmpptr)
    if (!strcmp(fname(tmpptr), "a") || !strcmp(fname(tmpptr), "an")
        || !strcmp(fname(tmpptr), "the"))
      *tmpptr = LOWER(*tmpptr);
  mob_proto[i].player.long_descr = fread_string(mob_f, buf2);
  mob_proto[i].player.description = fread_string(mob_f, buf2);
  GET_TITLE(mob_proto + i) = NULL;
  mob_proto[i].player.race = 5;
  //  set_race(mob_proto[i], mob_proto[i].player.race);

  /* *** Numeric data *** */
  if (!get_line(mob_f, line))
  {
    log("SYSERR: Format error after string section of mob #%d\n"
        "...expecting line of form '# # # {S | E}', but file ended!",
        nr);
    exit(1);
  }

  if (sscanf
      (line, "%s %s %s %s %s %s %s %s %d %c", f1, f2, f3, f4, f5, f6, f7,
       f8, t + 2, &letter) != 10)
  {
    log("SYSERR: Format error after string section of mob #%d\n"
        "...expecting line of form '# # # {S | E}'", nr);
    exit(1);
  }
  MOB_FLAGS(mob_proto + i)[0] = asciiflag_conv(f1);
  MOB_FLAGS(mob_proto + i)[1] = asciiflag_conv(f2);
  MOB_FLAGS(mob_proto + i)[2] = asciiflag_conv(f3);
  MOB_FLAGS(mob_proto + i)[3] = asciiflag_conv(f4);
  SET_BIT_AR(MOB_FLAGS(mob_proto + i), MOB_ISNPC);
  REMOVE_BIT_AR(MOB_FLAGS(mob_proto + i), MOB_SPEC);
  if (MOB_FLAGGED(mob_proto + i, MOB_NOTDEADYET))
  {
    /* Rather bad to load mobiles with this bit already set. */
    log("SYSERR: Mob #%d has reserved bit MOB_NOTDEADYET set.", nr);
    REMOVE_BIT_AR(MOB_FLAGS(mob_proto + i), MOB_NOTDEADYET);
  }


  //check_bitvector_names(MOB_FLAGS(mob_proto + i), action_bits_count, buf2, "mobile");

  AFF_FLAGS(mob_proto + i)[0] = asciiflag_conv(f5);
  AFF_FLAGS(mob_proto + i)[1] = asciiflag_conv(f6);
  AFF_FLAGS(mob_proto + i)[2] = asciiflag_conv(f7);
  AFF_FLAGS(mob_proto + i)[3] = asciiflag_conv(f8);
  GET_ALIGNMENT(mob_proto + i) = t[2];
  //check_bitvector_names(AFF_FLAGS(mob_proto + i), affected_bits_count, buf2, "mobile affect");

  /* AGGR_TO_ALIGN is ignored if the mob is AGGRESSIVE. */
  if (MOB_FLAGGED(mob_proto + i, MOB_AGGRESSIVE) &&
      (MOB_FLAGGED(mob_proto + i, MOB_AGGR_GOOD) ||
       MOB_FLAGGED(mob_proto + i, MOB_AGGR_EVIL) || MOB_FLAGGED(mob_proto + i,MOB_AGGR_NEUTRAL)))
    log("SYSERR: Mob #%d both Aggressive and Aggressive_to_Alignment.", nr);

  switch (UPPER(letter))
  {
  case 'S':			/* Simple monsters */
    parse_simple_mob(mob_f, i, nr);
    break;
  case 'E':			/* Circle3 Enhanced monsters */
    parse_enhanced_mob(mob_f, i, nr);
    break;

    /* add new mob types here.. */
  default:
    log("SYSERR: Unsupported mob type '%c' in mob #%d", letter, nr);
    exit(1);
  }
  mob_proto[i].mob_specials.join_list = NULL;
  mob_proto[i].mob_specials.head_join = NULL;
  letter = fread_letter(mob_f);
  ungetc(letter, mob_f);
  while (letter == 'J')
  {
    // assume its valid, we have to
    get_line(mob_f, line);
    count = sscanf(line,"%s %d",junk,&vn);
    if (count != 2)
    {
      log("SYSERR: Error assigning LINK! - Line was\n  %s", line);
      return;
    }
    add_base_link(i, vn);
    letter = fread_letter(mob_f);
    ungetc(letter, mob_f);
  }
  /* DG triggers -- script info follows mob S/E section */
  letter = fread_letter(mob_f);
  ungetc(letter, mob_f);
  while (letter == 'T')
  {
    dg_read_trigger(mob_f, &mob_proto[i], MOB_TRIGGER);
    letter = fread_letter(mob_f);
    ungetc(letter, mob_f);
  }

 
  mob_proto[i].aff_abils = mob_proto[i].real_abils;

  for (j = 0; j < NUM_WEARS; j++)
    mob_proto[i].equipment[j] = NULL;

  mob_proto[i].nr = i;
  mob_proto[i].desc = NULL;

  if (GET_CLASS(mob_proto + i) == CLASS_NORMAL)
    give_mob_class(mob_proto + i, nr);

  if (MOB_FLAGGED(mob_proto + i, MOB_HEALER))
    ASSIGNMOB(nr, cleric);

  if (MOB_FLAGGED(mob_proto + i, MOB_POSTMASTER))
    ASSIGNMOB(nr, postmaster);
    if (! mob_htree)
      mob_htree = htree_init();
    htree_add(mob_htree, nr, i);
  top_of_mobt = i++;
}

int is_aggro(CHAR_DATA *ch)
{
  if ( MOB_FLAGGED(ch, MOB_AGGRESSIVE) ||
       (MOB_FLAGGED(ch, MOB_AGGR_GOOD) ||
        MOB_FLAGGED(ch, MOB_AGGR_EVIL) ||
        MOB_FLAGGED(ch, MOB_AGGR_NEUTRAL)))
    return 1;
  return 0;
}


/* read all objects from obj file; generate index and prototypes */
char *parse_object(FILE * obj_f, int nr, zone_vnum zon)
{
  static int i = 0;
  static char line[READ_SIZE];
  int t[10], j = 0, retval;
  char *tmpptr;
  char f1[READ_SIZE], f2[READ_SIZE];
  char f3[READ_SIZE], f4[READ_SIZE];
  struct extra_descr_data *new_descr;
  char buf2[MAX_INPUT_LENGTH];

  obj_index[i].vnum = nr;
  obj_index[i].number = 0;
  obj_index[i].func = NULL;
  obj_index[i].qic = NULL; // memory leak hopefully fixed - mord

  if (! obj_htree)
    obj_htree = htree_init();
  htree_add(obj_htree, nr, i);

  clear_object(obj_proto + i);
  obj_proto[i].in_room = NULL;
  obj_proto[i].item_number = i;

  snprintf(buf2, sizeof(buf2), "object #%d", nr);

  obj_proto[i].name = NULL;
  obj_proto[i].short_description = NULL;
  obj_proto[i].description = NULL;
  obj_proto[i].action_description = NULL;
  obj_proto[i].smell = NULL;
  obj_proto[i].feel = NULL;
  obj_proto[i].taste = NULL;

  /* *** string data *** */
  if ((obj_proto[i].name = fread_string(obj_f, buf2)) == NULL)
  {
    log("SYSERR: Null obj name or format error at or near %s", buf2);
    exit(1);
  }

  obj_proto[i].short_description = tmpptr = fread_string(obj_f, buf2);
  if (tmpptr && *tmpptr)
    if (!strcmp(fname(tmpptr), "a") || !strcmp(fname(tmpptr), "an")
        || !strcmp(fname(tmpptr), "the"))
      *tmpptr = LOWER(*tmpptr);


  /* this now checks to see if the item was made "invi" with {c */

  obj_proto[i].description = fread_string(obj_f, buf2);


  obj_proto[i].action_description = fread_string(obj_f, buf2);
  obj_proto[i].smell = fread_string(obj_f, buf2);
  obj_proto[i].taste = fread_string(obj_f, buf2);
  obj_proto[i].feel = fread_string(obj_f, buf2);

  /* *** numeric data *** */
  if (!get_line(obj_f, line))
  {
    log("SYSERR: Expecting first numeric line of %s, but file ended!",
        buf2);
    exit(1);
  }

  if ((retval =
         sscanf(line, " %d %d %d %d %d %d %d %d %d", t, t + 1, t + 2, t + 3, t + 4, t + 5,
                t + 6, t + 7, t + 8)) != 9)
  {
    log("SYSERR: Format error in first numeric line (expecting 9 args, got %d), %s", retval, buf2);
    exit(1);

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

  check_item_hack_invis(&obj_proto[i], TRUE);
  tmpptr =obj_proto[i].description;
  if (tmpptr && *tmpptr)
    CAP(tmpptr);



  if (!get_line(obj_f, line))
  {
    log("SYSERR: Expecting second numeric line of %s, but file ended!",
        buf2);
    exit(1);
  }
  if ((retval =
         sscanf(line, "%d %d %d %d %d %d %d %d %d %d", t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6, t + 7, t + 8, t + 9)) != 10 )
  {
    if (retval == 4)
    {
      t[4] = 0;
      t[5] = 0;
      t[6] = 0;
      t[7] = 0;
      t[8] = 0;
      t[9] = 0;
    }
    else
    {
      log("SYSERR: Format error in second numeric line (expecting 10 args, got %d), %s", retval, buf2);
      exit(1);
    }
  }

  obj_proto[i].obj_flags.value[0] = t[0];
  obj_proto[i].obj_flags.value[1] = t[1];
  obj_proto[i].obj_flags.value[2] = t[2];
  obj_proto[i].obj_flags.value[3] = t[3];
  obj_proto[i].obj_flags.value[4] = t[4];
  obj_proto[i].obj_flags.value[5] = t[5];
  obj_proto[i].obj_flags.value[6] = t[6];
  obj_proto[i].obj_flags.value[7] = t[7];
  obj_proto[i].obj_flags.value[8] = t[8];
  obj_proto[i].obj_flags.value[9] = t[9];


  if (!get_line(obj_f, line))
  {
    log("SYSERR: Expecting third numeric line of %s, but file ended!",
        buf2);
    exit(1);
  }

  if ((retval =
         sscanf(line, "%d %d %d %d %d %d %s %s %s %s", t, t + 1, t + 2, t + 3,
                t + 4, t + 5, f1, f2, f3, f4)) != 10)
  {
    if (retval == 5)
    {
      t[5] = 0;
      strcpy(f1, "");
      strcpy(f2, "");
      strcpy(f3, "");
      strcpy(f4, "");
    }
    else
    {
      log("SYSERR: Format error in third numeric line (expecting 6 args, got %d), %s", retval, buf2);
      exit(1);
    }
  }

  GET_OBJ_WEIGHT(obj_proto + i) = t[0];
  GET_OBJ_COST(obj_proto + i) = t[1];
  GET_OBJ_RENT(obj_proto + i) = t[2];
  obj_proto[i].obj_flags.obj_innate = t[3];
  obj_proto[i].obj_flags.timer = t[4];
  GET_OBJ_LEVEL(obj_proto + i) = t[5];
  GET_OBJ_PERM(obj_proto + i)[0] = asciiflag_conv(f1);
  GET_OBJ_PERM(obj_proto + i)[1] = asciiflag_conv(f2);
  GET_OBJ_PERM(obj_proto + i)[2] = asciiflag_conv(f3);
  GET_OBJ_PERM(obj_proto + i)[3] = asciiflag_conv(f4);





  /* check to make sure that weight of containers exceeds curr. quantity */
  if (GET_OBJ_TYPE(obj_proto + i) == ITEM_DRINKCON || GET_OBJ_TYPE(obj_proto + i) == ITEM_FOUNTAIN)
  {
    if (GET_OBJ_WEIGHT(obj_proto + i) < GET_OBJ_VAL(obj_proto + i, 1))
      GET_OBJ_WEIGHT(obj_proto + i) = GET_OBJ_VAL(obj_proto + i, 1) + 5;
    GET_OBJ_VAL((obj_proto + i), 3) = 0; //unpoison it
  }

  if (GET_OBJ_TYPE(obj_proto + i) == ITEM_DRINKCON)
    GET_OBJ_VAL((obj_proto + i), 3) = 0; //unpoison it

  /* *** extra descriptions and affect fields *** */

  for (j = 0; j < MAX_OBJ_AFFECT; j++)
  {
    obj_proto[i].affected[j].location = APPLY_NONE;
    obj_proto[i].affected[j].modifier = 0;
  }

  strlcat(buf2, ", after numeric constants\n"
          "...expecting 'E', 'A', '$', or next object number", sizeof(buf2));
  j = 0;

  obj_proto[i].ex_description = NULL;

  for (;;)
  {
    if (!get_line(obj_f, line))
    {
      log("SYSERR: Format error in %s", buf2);
      exit(1);
    }
    switch (*line)
    {
    case 'E':
      CREATE(new_descr, struct extra_descr_data, 1);
      new_descr->keyword = fread_string(obj_f, buf2);
      new_descr->description = fread_string(obj_f, buf2);
      new_descr->next = obj_proto[i].ex_description;
      obj_proto[i].ex_description = new_descr;
      break;
    case 'A':
      if (j >= MAX_OBJ_AFFECT)
      {
        log("SYSERR: Too many A fields (%d max), %s",
            MAX_OBJ_AFFECT, buf2);
        exit(1);
      }
      if (!get_line(obj_f, line))
      {
        log("SYSERR: Format error in 'A' field, %s\n"
            "...expecting 2 numeric constants but file ended!",
            buf2);
        exit(1);
      }

      if ((retval = sscanf(line, " %d %d ", t, t + 1)) != 2)
      {
        log("SYSERR: Format error in 'A' field, %s\n"
            "...expecting 2 numeric arguments, got %d\n"
            "...offending line: '%s'", buf2, retval, line);
        exit(1);
      }
      obj_proto[i].affected[j].location = t[0];
      obj_proto[i].affected[j].modifier = t[1];
      j++;
      break;
    case 'T':		/* DG triggers */
      dg_obj_trigger(line, &obj_proto[i]);
      break;
    case '$':
    case '#':
      top_of_objt = i;
      check_object(&obj_proto[i], nr);
      i++;
      return (line);
    default:
      log("SYSERR: Format error in (%c): %s", *line, buf2);
      exit(1);
    }
  }

  if (GET_OBJ_TYPE(obj_proto + i) == ITEM_BANKBOOK)
    ASSIGNOBJ(nr, bank);
}


#define Z	zone_table[zone]

/* load the zone table and command tables */
void load_zones(FILE * fl, char *zonename)
{
  static zone_rnum zone = 0;
  int cmd_no, num_of_cmds = 0, line_num = 0, tmp, error = 0, arg_num;
  int version = 2;
  char *ptr = NULL, buf[READ_SIZE], zname[READ_SIZE], buf2[MAX_STRING_LENGTH];
  char t1[80], t2[80];
  long f[5];
  int retval;
  int zone_fix = FALSE;

  //log("loading zone: (%s)", zonename);
  strlcpy(zname, zonename, sizeof(zname));
  for (tmp = 0; tmp < 4; tmp++)
    get_line(fl, buf);	//mord??

  /*  More accurate count. Previous was always 4 or 5 too high. -gg 2001/1/17
   *  Note that if a new zone command is added to reset_zone(), this string
   *  will need to be updated to suit. - ae.
   */
  while (get_line(fl, buf))
    if ((strchr("MOPGERDTVBZ", buf[0]) && buf[1] == ' ')
        || (buf[0] == 'S' && buf[1] == '\0'))
      num_of_cmds++;

  rewind(fl);			//mord check strings in reset_zone()

  if (num_of_cmds == 0)
  {
    log("SYSERR: %s is empty!", zname);
    exit(1);
  }
  else
    CREATE(Z.cmd, struct reset_com, num_of_cmds);

  line_num += get_line(fl, buf);

  if (*buf == '@')
  {
    if (sscanf(buf, "@Version: %d", &version) != 1)
    {
      log("SYSERR: Format error in %s (version)", zname);
      log("SYSERR: ... Line: %s", buf);
      exit(1);
    }
    line_num += get_line(fl, buf);
  }
  //  if (sscanf(buf, "#%hd", &Z.number) != 1) {
  if (sscanf(buf, "#%d", &Z.number) != 1)
  {
    log("SYSERR: Format error in %s, line %d", zname, line_num);
    exit(1);
  }
  snprintf(buf2, sizeof(buf2), "beginning of zone #%d", Z.number);

  line_num += get_line(fl, buf);
  if ((ptr = strchr(buf, '~')) != NULL)	/* take off the '~' if it's there */
    *ptr = '\0';
  Z.name = str_dup(buf);

  line_num += get_line(fl, buf);
  if ((ptr = strchr(buf, '~')) != NULL)	/* take off the '~' if it's there */
    *ptr = '\0';
  Z.builders = str_dup(buf);

  line_num += get_line(fl, buf);
  //&Z.bot, &Z.top, &Z.lifespan,  &Z.reset_mode, &Z.zone_flags

  retval = sscanf(buf, "%ld %ld %ld %ld %ld %ld", f, f + 1, f + 2,  f + 3, f + 4, f+ 5);
  switch (retval)
  {
  case 6:
    Z.bot = (int)f[0];
    Z.top = (int)f[1];
    Z.lifespan = (int)f[2];
    Z.reset_mode = (int)f[3];
    Z.zone_flags = f[4];
    Z.dimension = f[5];
    break;
  case 5:
    Z.bot = (int)f[0];
    Z.top = (int)f[1];
    Z.lifespan = (int)f[2];
    Z.reset_mode = (int)f[3];
    Z.zone_flags = f[4];
    Z.dimension = D_ALL;
    break;
  case 4:
    Z.bot = (Z.number * 100);
    Z.top = (int)f[0];
    Z.lifespan = (int)f[1];
    Z.reset_mode = (int)f[2];
    Z.zone_flags = f[3];
    Z.dimension = D_ALL;
    break;
  case 3:
    Z.bot = (Z.number * 100);
    Z.top = (int)f[0];
    Z.lifespan = (int)f[1];
    Z.reset_mode = (int)f[2];
    Z.zone_flags = 0;
    Z.dimension = D_ALL;
    break;
  default:
    log("SYSERR: Format error in numeric constant line of %s.", zname);
    log("SYSERR: Could not fix previous error, aborting game.");
    exit(1);
  }
  if (Z.bot > Z.top)
  {
    log("SYSERR: Zone %d bottom (%d) > top (%d).", Z.number, Z.bot, Z.top);
    exit(1);
  }


  Z.pressure = 960;
  if ((time_info.month >= 7) && (time_info.month <= 12))
    Z.pressure += dice(1, 50);
  else
    Z.pressure += dice(1, 80);
  Z.change = 0;
  if (Z.pressure <= 980)
    Z.sky = SKY_LIGHTNING;
  else if (Z.pressure <= 1000)
    Z.sky = SKY_RAINING;
  else if (Z.pressure <= 1020)
    Z.sky = SKY_CLOUDY;
  else
    Z.sky = SKY_CLOUDLESS;

  cmd_no = 0;

  for (;;)
  {
    /* skip reading one line if we fixed above (line is correct already) */
    if (zone_fix != TRUE)
    {
      if ((tmp = get_line(fl, buf)) == 0)
      {
        log("SYSERR: Format error in %s - premature end of file",
            zname);
        exit(1);
      }
    }
    else
      zone_fix = FALSE;

    line_num += tmp;
    ptr = buf;
    skip_spaces(&ptr);

    if ((ZCMD.command = *ptr) == '*')
      continue;

    ptr++;

    if (ZCMD.command == 'S' || ZCMD.command == '$')
    {
      ZCMD.command = 'S';
      break;
    }
    error = 0;
    if (strchr("D", ZCMD.command) != NULL)
    {	/* ### */
      if (sscanf(ptr, " %d %d %d %d ", &tmp, &ZCMD.arg1, &ZCMD.arg2,
                 &ZCMD.arg3) != 4)
        error = 1;
    }
    else if (strchr("R", ZCMD.command) != NULL)
    {	/* ### */
      if (sscanf(ptr, " %d %d %d ", &tmp, &ZCMD.arg1,
                 &ZCMD.arg2) != 3)
        error = 2;
    }
    else if (strchr("G", ZCMD.command) != NULL)
    {	/* ### */
      if ((arg_num = sscanf(ptr, " %d %d %d %d ", &tmp, &ZCMD.arg1,
                            &ZCMD.arg2, &ZCMD.arg3)) != 4)
      {
        if (arg_num != 3)
          error = 3;
        else
          ZCMD.arg3 = 0;
      }
    }
    else if (strchr("V", ZCMD.command) != NULL)
    {//changed dg10
      if (sscanf(ptr, " %d %d %d %d %79s %79[^\f\n\r\t\v]", &tmp, &ZCMD.arg1, &ZCMD.arg2,
                 &ZCMD.arg3, t1, t2) != 6)
        error = 4;
      else
      {
        ZCMD.sarg1 = str_dup(t1);
        ZCMD.sarg2 = str_dup(t2);
      }
    }
    else
    {		/* ### */
      if ((arg_num =
             sscanf(ptr, " %d %d %d %d %d ", &tmp, &ZCMD.arg1,
                    &ZCMD.arg2, &ZCMD.arg3, &ZCMD.arg4)) != 5)
      {
        if (arg_num != 4)
          error = 5;
        else
          ZCMD.arg4 = 0;
      }
    }

    ZCMD.if_flag = tmp;

    if (error)
    {
      log("SYSERR: Format error in %s, line %d: '%s' (E:%d)", zname,
          line_num, buf, error);
      exit(1);
    }
    ZCMD.line = line_num;
    cmd_no++;
  }
  if (num_of_cmds != cmd_no + 1)
  {
    log("SYSERR: Zone command count mismatch for %s. Estimated: %d, Actual: %d", zname, num_of_cmds, cmd_no + 1);
    //exit(1);
  }

  top_of_zone_table = zone++;
}

#undef Z
int int_compare(const void *aa, const void* bb)
{
  int a = * (int *) aa;
  int b = * (int *) bb;

  return a - b;
}
void renumber_zones()
{
  int i, j;
  for (j = 0; j < top_of_zone_table; j++)
  {
    for (i = 0; i < top_of_zone_table; i++)
    {
      if (i==j)
        continue;
      if (zone_table[j].number == zone_table[i].number)
      {
        log("ERROR: Virtual zone exists twice - [%s] and [%s]", zone_table[j].name, zone_table[i].name);
      }
      if (zone_table[j].bot <= zone_table[i].bot && zone_table[j].top >= zone_table[i].bot)
      {
        log("ERROR: Zone [%d] (%d to %d) covers zone [%d] (%d to %d)",
            zone_table[j].number, zone_table[j].bot, zone_table[j].top,
            zone_table[i].number, zone_table[i].bot, zone_table[i].top);

      }
      if (zone_table[j].bot <= zone_table[i].top && zone_table[j].top >= zone_table[i].top)
      {
        log("ERROR: Zone [%d] (%d to %d) covers zone [%d] (%d to %d)",
            zone_table[j].number, zone_table[j].bot, zone_table[j].top,
            zone_table[i].number, zone_table[i].bot, zone_table[i].top);

      }
    }
  }


}
void do_show_errors(CHAR_DATA *ch)
{
  int i, j;
  int found = FALSE;
  for (j = 0; j < top_of_zone_table; j++)
  {
    for (i = 0; i < top_of_zone_table; i++)
    {
      if (i==j)
        continue;
      if (zone_table[j].number == zone_table[i].number)
      {
        found = TRUE;
        new_send_to_char(ch,"ERROR: Virtual zone exists twice - [%s] and [%s]\r\n", zone_table[j].name, zone_table[i].name);
      }
      if (zone_table[j].bot <= zone_table[i].bot && zone_table[j].top >= zone_table[i].bot)
      {
        found = TRUE;
        new_send_to_char(ch,"ERROR: Zone [%d] (%d to %d) covers zone [%d] (%d to %d)\r\n",
                         zone_table[j].number, zone_table[j].bot, zone_table[j].top,
                         zone_table[i].number, zone_table[i].bot, zone_table[i].top);

      }
      if (zone_table[j].bot <= zone_table[i].top && zone_table[j].top >= zone_table[i].top)
      {
        found = TRUE;
        new_send_to_char(ch,"ERROR: Zone [%d] (%d to %d) covers zone [%d] (%d to %d)\r\n",
                         zone_table[j].number, zone_table[j].bot, zone_table[j].top,
                         zone_table[i].number, zone_table[i].bot, zone_table[i].top);

      }
    }
  }
  if (!found)
  {
    new_send_to_char(ch, "No zone overlaps found\r\n");
  }
}


void get_one_line(FILE * fl, char *buf)
{
  if (fgets(buf, READ_SIZE, fl) == NULL)
  {
    log("SYSERR: error reading help file: not terminated with $?");
    exit(1);
  }

  buf[strlen(buf) - 1] = '\0';	/* take off the trailing \n */
}
void the_free_help(void)
{
  int hp;

  if (!help_table)
    return;

  for (hp = 0; hp <= top_of_helpt; hp++)
  {
    if (help_table[hp].keywords)
      free(help_table[hp].keywords);
    if (help_table[hp].entry && !help_table[hp].duplicate)
      free(help_table[hp].entry);
  }

  free(help_table);
  help_table = NULL;
  top_of_helpt = 0;
}


#if 0

void load_help(FILE * fl)
{
#if defined(CIRCLE_MACINTOSH)
  static char key[READ_SIZE + 1], next_key[READ_SIZE + 1], entry[32384];	/* too big for stack? */
#else
  char key[READ_SIZE + 2] = "", entry[32384] = "";
#endif
  size_t entrylen = 0;
  static char line[READ_SIZE + 1];
  struct help_index_element el;

  /* get the keyword line */
  get_one_line(fl, key);
  while (*key != '$')
  {
    strcat(key, "\r\n");	/* strcat: OK (READ_SIZE - "\n" + "\r\n" == READ_SIZE + 1) */
    entrylen = strlcpy(entry, key, sizeof(entry));	//mord??

    /* read in the corresponding help entry */
    get_one_line(fl, line);
    while (*line != '#' && entrylen < sizeof(entry) - 1)
    {
      entrylen +=	strlcpy(entry + entrylen, line, sizeof(entry) - entrylen);

      if (entrylen + 2 < sizeof(entry) - 1)
      {
        strcpy(entry + entrylen, "\r\n");	/* strcpy: OK (size checked above) */
        entrylen += 2;
      }
      get_one_line(fl, line);
    }

    if (entrylen >= sizeof(entry) - 1)
    {
      int keysize;
      const char *truncmsg = "\r\n*TRUNCATED*\r\n";

      strcpy(entry + sizeof(entry) - strlen(truncmsg) - 1, truncmsg);	/* strcpy: OK (assuming sane 'entry' size) */

      keysize = strlen(key) - 2;
      log("SYSERR: Help entry exceeded buffer space: %.*s", keysize,
          key);

      /* If we ran out of buffer space, eat the rest of the entry. */
      while (*line != '#')
        get_one_line(fl, line);
    }



    el.min_level = 0;
    if ((*line == '#') && (*(line + 1) != 0))
      el.min_level = atoi((line + 1));

    el.min_level = MAX(0, MIN(el.min_level, LVL_IMPL));
    /* now, add the entry to the index with each keyword on the keyword line */

    el.duplicate = 0;
    el.entry = str_dup(entry);

    add_to_help_index(&el, key);

    /* get next keyword line (or $) */
    get_one_line(fl, key);
  }
}
#endif
void load_help(FILE *fl)
{
#if defined(CIRCLE_MACINTOSH)
  static char key[READ_SIZE + 1],  entry[32384]; /* too big for stack? */
#else
  char key[READ_SIZE + 1], entry[32384];
#endif
  size_t entrylen = 0;
  char line[READ_SIZE + 1];
  struct help_index_element el;

  /* get the first keyword line */
  get_one_line(fl, key);
  while (*key != '$')
  {
    //strlcat(key, "\r\n", sizeof(key));	/* strcat: OK (READ_SIZE - "\n" + "\r\n" == READ_SIZE + 1) */
    //entrylen = strlcpy(entry, key, sizeof(entry));
    entrylen = 0;
    /* read in the corresponding help entry */
    get_one_line(fl, line);
    while (*line != '#' && entrylen < sizeof(entry) - 1)
    {
      entrylen += strlcpy(entry + entrylen, line, sizeof(entry) - entrylen);

      if (entrylen + 2 < sizeof(entry) - 1)
      {
       strcpy(entry + entrylen, "\r\n");	/* strcpy: OK (size checked above) */
      entrylen += 2;
      }

      get_one_line(fl, line);
    }

{
int i = entrylen-1;
	while (entry[i] == '\n' || entry[i] == '\r') /* Trim trailing whitespace */
	  entry[i--] = 0;
entrylen = i + 1;
}
    if (entrylen >= sizeof(entry) - 1)
    {
      int keysize;
      const char *truncmsg = "\r\n*TRUNCATED*\r\n";

      strcpy(entry + sizeof(entry) - strlen(truncmsg) - 1, truncmsg);	/* strcpy: OK (assuming sane 'entry' size) */

      keysize = strlen(key) - 2;
      log("SYSERR: Help entry exceeded buffer space: %.*s", keysize, key);

      /* If we ran out of buffer space, eat the rest of the entry. */
      while (*line != '#')
        get_one_line(fl, line);
    }
    el.min_level = 0;
    if ((*line == '#') && (*(line + 1) != 0))
      el.min_level = atoi((line + 1));
    /* now, add the entry to the index with each keyword on the keyword line */
    el.duplicate = 0;
    el.id = max_help_id++;
    el.entry = str_dup(entry);
    prune_crlf(key);
    el.keywords = str_dup(key);
    el.duplicate = 0; //redundant call
    help_table[top_of_helpt++] = el;


    /* get next keyword line (or $) */
    get_one_line(fl, key);
  }
}


#if 0
struct help_index_element *add_to_help_index(struct help_index_element *perent,int id, char *header, char *body)
{
  struct help_index_element *el;

  CREATE(el, struct help_index_element, 1);
  if (!perent)
  {
    //    create_help_core_perent(perent);
  }
  el->next = perent->items;
  perent->items = el;
  el->perent = perent;
  el->items = NULL;
  el->header = str_dup(header);
  el->body = str_dup(body);
  return el;

}
#endif



/*************************************************************************
*  procedures for resetting, both play-time and boot-time	 	 *
*************************************************************************/



int vnum_mobile(char *searchname, struct char_data *ch)
{
  int nr, found = 0;
  char buf[MAX_INPUT_LENGTH];
  DYN_DEFINE;
  *buf = 0;
  DYN_CREATE;
  *dynbuf = 0;

  for (nr = 0; nr <= top_of_mobt; nr++)
  {
    if (isname_full(searchname, mob_proto[nr].player.name))
    {
      snprintf(buf, sizeof(buf), "%3d. [%5d] %-40s %s\r\n", ++found,
               mob_index[nr].vnum, mob_proto[nr].player.short_descr,
               mob_proto[nr].proto_script ? "[TRIG]" : "" );
      DYN_RESIZE(buf);
    }
  }
  if (found)
    page_string(ch->desc, dynbuf, DYN_BUFFER);
  return (found);
}



int vnum_object(char *searchname, struct char_data *ch)
{
  int nr, found = 0;
  char buf[MAX_INPUT_LENGTH];
  DYN_DEFINE;
  *buf = 0;
  DYN_CREATE;
  *dynbuf = 0;

  for (nr = 0; nr <= top_of_objt; nr++)
  {
    if (isname_full(searchname, obj_proto[nr].name))
    {
      snprintf(buf, sizeof(buf), "%3d. [%5d] %-40s %s\r\n", ++found,
               obj_index[nr].vnum,
               obj_proto[nr].short_description,
               obj_proto[nr].proto_script ? "[TRIG]" : "");
      DYN_RESIZE(buf);
    }
  }
  page_string(ch->desc, dynbuf, DYN_BUFFER);
  return (found);
}

#if USE_CREATE_CHAR
/* create a character, and add it to the char list */
struct char_data *create_char(void)
{
  struct char_data *ch;

  CREATE(ch, struct char_data, 1);
  clear_char(ch);
  ch->next = character_list;
  character_list = ch;
  //TODO: check this
  while (!valid_id_num(max_mob_id))
  {
    log("Error new id being assigned to mob already exists(%ld)!", max_mob_id);
    max_mob_id++;
  }
  GET_ID(ch) = max_mob_id++;
  /* find_char helper */
  add_to_lookup_table(GET_ID(ch), (void *)ch);
  return (ch);
}
#endif


/* create a new mobile from a prototype */
struct char_data *read_mobile(mob_vnum nr, int type)
{				/* and mob_rnum */
  mob_rnum i;
  struct char_data *mob;

  if (type == VIRTUAL)
  {
    if ((i = real_mobile(nr)) == NOBODY)
    {
      log("WARNING: Mobile vnum %d does not exist in database.", nr);
      return (NULL);
    }
  }
  else
    i = nr;

  CREATE(mob, struct char_data, 1);
  clear_char(mob);
  *mob = mob_proto[i];
  mob->next = character_list;
  character_list = mob;
  set_race(mob, mob_proto[i].player.race);

  if (MOB_TIER(mob) == 0 && (!is_aggro(mob)))
  {
    if (!number(0, 10)) MOB_TIER(mob)++;
    if (!number(0, 10) && MOB_TIER(mob)) MOB_TIER(mob)++;
    if (!number(0, 10) && MOB_TIER(mob)) MOB_TIER(mob)++;
    if (!number(0, 10) && MOB_TIER(mob)) MOB_TIER(mob)++;
  }

  if (!mob->points.max_hit)
  {
    mob->points.max_hit = dice(mob->points.hit, mob->points.mana) + mob->points.move;
  }
  mob->points.max_hit *= (mob_hitpoint_multi(GET_CLASS(mob)) * (1.0  + (MOB_TIER(mob) * 0.5))) ;
  mob->points.hit = mob->points.max_hit;
  mob->points.mana = mob->points.max_mana;
  mob->points.move = mob->points.max_move;
  mob->points.stamina = mob->points.max_stamina;

  mob->player.time.birth = time(0);
  mob->player.time.played = 0;
  mob->player.time.logon = time(0);

  if (GET_MRACE(mob) == MOB_RACE_ANIMAL)
    GET_GOLD(mob) = 0;

  mob_index[i].number++;
  while (!valid_id_num(max_mob_id))
  {
    log("Error new id being assigned to mob already exists(%ld)!", max_mob_id);
    max_mob_id++;
  }
  GET_ID(mob) = max_mob_id++;
  /* find_char helper */
  add_to_lookup_table(GET_ID(mob), (void *)mob);

  copy_proto_script(&mob_proto[i], mob, MOB_TRIGGER);
  assign_triggers(mob, MOB_TRIGGER);
  mob->mob_specials.join_list = copy_proto_link(mob_proto[i].mob_specials.join_list);
  load_links(mob);

  return (mob);
}


/* create an object, and add it to the object list */
struct obj_data *create_obj(void)
{
  struct obj_data *obj;

  CREATE(obj, struct obj_data, 1);
  clear_object(obj);
  obj->next = object_list;
  object_list = obj;

  obj->name = NULL;
  obj->action_description = NULL;
  obj->description = NULL;
  obj->short_description = NULL;
  obj->smell = NULL;
  obj->taste = NULL;
  obj->feel = NULL;
  obj->ex_description = NULL;

  GET_ID(obj) = max_obj_id++;
  /* find_obj helper */
  add_to_lookup_table(GET_ID(obj), (void *)obj);


  return (obj);
}


/* create a new object from a prototype */
struct obj_data *read_object(obj_vnum nr, int type)
{				/* and obj_rnum */
  struct obj_data *obj = NULL;
  obj_rnum i;
  i = ((type == VIRTUAL) ? real_object(nr) : nr);

  if (i == NOTHING || i > top_of_objt)
  {
    log("Object (%c) %d does not exist in database.", type == VIRTUAL ? 'V' : 'R', nr);
    return (NULL);
  }
  qic_load(i); //remove in free obj

  CREATE(obj, struct obj_data, 1);
  clear_object(obj);
  *obj = obj_proto[i];
  obj->next = object_list;
  object_list = obj;

  if (obj_index[i].qic)
    log("%s created", obj->short_description);

  obj_index[i].number++;

  GET_ID(obj) = max_obj_id++;
  /* find_obj helper */
  add_to_lookup_table(GET_ID(obj), (void *)obj);
  generate_weapon(obj);
  copy_proto_script(&obj_proto[i], obj, OBJ_TRIGGER);
  assign_triggers(obj, OBJ_TRIGGER);

  return (obj);
}

int purge_zone(int zone)
{
  struct char_data *ch, *next_ch;
  struct obj_data *ob, *next_ob;

  for (ch = character_list; ch; ch = next_ch)
  {
    next_ch = ch->next;
    if (IS_NPC(ch) && (IN_ROOM(ch)->zone == zone)
        && !FIGHTING(ch))
      extract_char(ch);
  }

  for (ob = object_list; ob; ob = next_ob)
  {
    next_ob = ob->next;
    if (!ob->carried_by && !ob->in_obj && !ob->worn_by & !ob->contains & !ob->in_locker
        && (ob->in_room->zone == zone)
        //    && (!IS_SET_AR(world[ob->in_room].room_flags, ROOM_NODECAY))
        && (!IS_SET_AR(ob->in_room->room_flags, ROOM_HOUSE))
        &&
        (!IS_SET_AR(ob->in_room->room_flags, ROOM_HOUSE_CRASH))
        && (!IS_OBJ_STAT(ob, ITEM_PC_CORPSE)))
      extract_obj(ob);
  }
  return 0;
}

#define ZO_DEAD  999

/* update zone ages, queue for reset if necessary, and dequeue when possible */
void zone_update(void)
{
  int i, empty = 0;
  struct reset_q_element *update_u, *temp;
  static int timer = 0;

  /* jelson 10/22/92 */
  if (((++timer * PULSE_ZONE) / PASSES_PER_SEC) >= 60)
  {
    /* one minute has passed */
    /*
     * NOT accurate unless PULSE_ZONE is a multiple of PASSES_PER_SEC or a
     * factor of 60
     */

    timer = 0;

    /* since one minute has passed, increment zone ages */
    for (i = 0; i <= top_of_zone_table; i++)
    {
      if (zone_table[i].age < zone_table[i].lifespan &&
          zone_table[i].reset_mode)
        (zone_table[i].age)++;

      if (zone_table[i].age >= zone_table[i].lifespan &&
          zone_table[i].age < ZO_DEAD && zone_table[i].reset_mode)
      {
        /* enqueue zone */

        CREATE(update_u, struct reset_q_element, 1);

        update_u->zone_to_reset = i;
        update_u->next = 0;

        if (!reset_q.head)
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
  for (update_u = reset_q.head; update_u; update_u = update_u->next)
    if (zone_table[update_u->zone_to_reset].reset_mode == 2 ||
        (empty = is_empty(update_u->zone_to_reset)))
    {
      if (empty && ZONE_FLAGGED(update_u->zone_to_reset, ZONE_PURGE_EMPTY))
      {
        purge_zone(update_u->zone_to_reset);
        new_mudlog(CMP, LVL_GOD, FALSE, "Auto zone purge + reset (Zone %3d): %s ",
                   zone_table[update_u->zone_to_reset].number,
                   zone_table[update_u->zone_to_reset].name);
      }
      //else
      /* new_mudlog(CMP, LVL_GOD, FALSE, "Auto zone reset (Zone %3d): %s ",
                  zone_table[update_u->zone_to_reset].number,
                  zone_table[update_u->zone_to_reset].name);*/
      reset_zone(update_u->zone_to_reset);
      /* dequeue */
      if (update_u == reset_q.head)
        reset_q.head = reset_q.head->next;
      else
      {
        for (temp = reset_q.head; temp->next != update_u;
             temp = temp->next);

        if (!update_u->next)
          reset_q.tail = temp;

        temp->next = update_u->next;
      }

      free(update_u);
      break;
    }
}

void log_zone_error(zone_rnum zone, int cmd_no, const char *message)
{
  new_mudlog(NRM, LVL_GOD, TRUE, "SYSERR: zone file: %s", message);
  new_mudlog(NRM, LVL_GOD, TRUE, "SYSERR: ...offending cmd: '%c' cmd in zone #%d, line %d",
             ZCMD.command, zone_table[zone].number, ZCMD.line);
}

void make_maze(int zone)
{
#if 0
  int card[400], temp, x, y, dir;
  room_rnum room, next_room = NULL;
  int num, test, r_back;
  int vnum = zone_table[zone].number;


  for (test = 0; test < 400; test++)
  {
    card[test] = test;
    temp = test;
    dir = temp / 100;
    temp = temp - (dir * 100);
    x = temp / 10;
    temp = temp - (x * 10);
    y = temp;
    room = world_vnum[(vnum * 100) + (x * 10) + y];
    if ((x == 0) && (dir == 0))
      continue;
    if ((y == 9) && (dir == 1))
      continue;
    if ((x == 9) && (dir == 2))
      continue;
    if ((y == 0) && (dir == 3))
      continue;
    room->dir_option[dir]->to_room = -1;
    REMOVE_BIT_AR(ROOM_FLAGS(room), ROOM_NOTRACK);
  }
  for (x = 0; x < 399; x++)
  {
    y = number(0, 399);
    temp = card[y];
    card[y] = card[x];
    card[x] = temp;
  }

  for (num = 0; num < 400; num++)
  {
    temp = card[num];
    dir = temp / 100;
    temp = temp - (dir * 100);
    x = temp / 10;
    temp = temp - (x * 10);
    y = temp;
    room = world_vnum[(vnum * 100) + (x * 10) + y];
    r_back = room;
    room = real_room(room);
    if ((x == 0) && (dir == 0))
      continue;
    if ((y == 9) && (dir == 1))
      continue;
    if ((x == 9) && (dir == 2))
      continue;
    if ((y == 0) && (dir == 3))
      continue;
    if (room->dir_option[dir]->to_room != -1)
      continue;
    switch (dir)
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
    next_room = real_room(next_room);
    test = find_first_step(room, next_room);
    switch (test)
    {
    case BFS_ERROR:
      log("Maze making error.");
      break;
    case BFS_ALREADY_THERE:
      log("Maze making error.");
      break;
    case BFS_NO_PATH:

      room->dir_option[dir]->to_room = next_room;
      next_room->dir_option[(int) rev_dir[dir]]->to_room = room;
      break;
    }
  }
  for (num = 0; num < 100; num++)
  {
    room = (vnum * 100) + num;
    room = real_room(room);
    /* Remove the next line if you want to be able to track your way through
       the maze */
    SET_BIT_AR(ROOM_FLAGS(room), ROOM_NOTRACK);

    REMOVE_BIT_AR(ROOM_FLAGS(room), ROOM_BFS_MARK);
  }
#endif

}

#define ZONE_ERROR(message) \
	{ log_zone_error(zone, cmd_no, message); last_cmd = 0; }

/* execute the reset command table of a given zone */
void reset_zone(zone_rnum zone)
{
  int cmd_no, last_cmd = 0;
  struct char_data *mob = NULL;
  struct obj_data *obj, *obj_to;
  struct char_data *tmob = NULL;	/* for trigger assignment */
  struct obj_data *tobj = NULL; 	/* for trigger assignment */
  room_rnum rm;
  room_vnum vrm;

  for (cmd_no = 0; ZCMD.command != 'S'; cmd_no++)
  {

    if (ZCMD.if_flag && !last_cmd)
      continue;


    switch (ZCMD.command)
    {
    case '*':		/* ignore command */
      last_cmd = 0;
      tobj = NULL;
      break;

    case 'M':		/* read a mobile */
      if (real_room(ZCMD.arg3) == NULL)
      {
        log("Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg3);
        exit(1);
      }
      if ((mob_index[ZCMD.arg1].number < ZCMD.arg2))
      {
        mob = read_mobile(ZCMD.arg1, REAL);
        char_to_room(mob, world_vnum[ZCMD.arg3]);
        if (load_mtrigger(mob) != -1)
        {
          tmob = mob;
          last_cmd = 1;
        }
        else
        {
          mob = NULL;
          tmob = mob;
          last_cmd = 0;
        }
      }
      else
        last_cmd = 0;
      tobj = NULL;
      break;

    case 'O':		/* read an object */
      if (real_room(ZCMD.arg3) == NULL)
      {
        log("Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg3);
        exit(1);
      }
      if (obj_index[ZCMD.arg1].number < ZCMD.arg2)
      {
        if (ZCMD.arg3 != NOWHERE)
        {
          if (!get_obj_in_list_num(ZCMD.arg1, world_vnum[ZCMD.arg3]->contents))
          {
            obj = read_object(ZCMD.arg1, REAL);
            if (obj && load_qic_check(ZCMD.arg1))
            {
              obj_to_room(obj, world_vnum[ZCMD.arg3]);
              if (load_otrigger(obj) == -1)
                obj = NULL;
              tobj = obj;
              last_cmd = obj ? 1 : 0;
            }
            else
            {
              if (obj)
              {
                purge_qic(ZCMD.arg1);
                extract_obj(obj);
                obj = NULL;
              }

              tobj = obj;
              last_cmd = 0;
            }

          }
          else
            last_cmd = 0;
        }
        else
        {
          obj = read_object(ZCMD.arg1, REAL);
          IN_ROOM(obj) = NULL;
          tobj = obj;
          last_cmd = 1;
        }
      }
      else
        last_cmd = 0;
      tmob = NULL;

      break;

    case 'P':		/* object to object */
      if ((obj_index[ZCMD.arg1].number < ZCMD.arg2) )
      {
        obj = read_object(ZCMD.arg1, REAL);
        if (!(obj_to = get_obj_num(ZCMD.arg3)))
        {
          ZONE_ERROR("target obj not found, command disabled");
          ZCMD.command = '*';
          if (obj)
            extract_obj(obj);
          break;
        }
        if (obj && load_qic_check(ZCMD.arg1))
        {
          obj_to_obj(obj, obj_to);
          if (load_otrigger(obj) == -1)
            obj = NULL;
          tobj = obj;

          last_cmd = obj ? 1 : 0;
        }
        else
        {
          if (obj)
          {
            purge_qic(ZCMD.arg1);
            extract_obj(obj);
            obj = NULL;
          }

          tobj = obj;
          last_cmd = 0;
        }
      }
      else
        last_cmd = 0;
      tmob = NULL;
      break;

    case 'G':		/* obj_to_char ### */
      if (!mob)
      {
        ZONE_ERROR
        ("attempt to give obj to non-existant mob, command disabled");
        ZCMD.command = '*';
        break;
      }
      if (obj_index[ZCMD.arg1].number < ZCMD.arg2)
      {
        obj = read_object(ZCMD.arg1, REAL);
        if (obj && load_qic_check(ZCMD.arg1))
        {
          obj_to_char(obj, mob);
          if (load_otrigger(obj) == -1)
            obj = NULL;

          tobj = obj;

          last_cmd = obj ? 1 : 0;
        }
        else
        {
          if (obj)
          {
            purge_qic(ZCMD.arg1);
            extract_obj(obj);
            obj = NULL;
          }
          tobj = obj;
          last_cmd = 0;
        }
      }
      else
        last_cmd = 0;
      tmob = NULL;
      break;

    case 'E':		/* object to equipment list ### */
      if (!mob)
      {
        ZONE_ERROR
        ("trying to equip non-existant mob, command disabled");
        ZCMD.command = '*';
        break;
      }
      if ((obj_index[ZCMD.arg1].number < ZCMD.arg2))
      {
        if (ZCMD.arg3 < 0 || ZCMD.arg3 >= NUM_WEARS)
        {
          ZONE_ERROR("invalid equipment pos number");
        }
        else
        {
          int ret;
          obj = read_object(ZCMD.arg1, REAL);
          if (obj && load_qic_check(ZCMD.arg1))
          {
            IN_ROOM(obj) = IN_ROOM(mob);
            if (load_otrigger(obj) == -1)
              obj = NULL;
            if (obj && (ret = wear_otrigger(obj, mob, ZCMD.arg3)) > 0)
            {
              IN_ROOM(obj) = NULL;
              equip_char(mob, obj, ZCMD.arg3);
            }
            else if (obj && ret == 0)
            {
              obj_to_char(obj, mob);
              tobj = obj;
              last_cmd = 1;
            }
            else
            {
              obj = NULL;
              last_cmd = 0;
              tobj = NULL;
            }
          }
          else
          {
            if (obj)
            {
              purge_qic(ZCMD.arg1);
              extract_obj(obj);
              obj = NULL;
            }
            last_cmd = 0;
          }
        }
      }
      else
        last_cmd = 0;
      tmob = NULL;
      break;

    case 'R':		/* rem obj from room */
      if (!world_vnum[ZCMD.arg1])
      {
        ZONE_ERROR("Zone room error");
        log("room %d doesn't exist, and zedit needs it.",ZCMD.arg1);
      }
      else
        if ( (obj = get_obj_in_list_num(ZCMD.arg2, world_vnum[ZCMD.arg1]->contents)) != NULL)
        {
          extract_obj(obj);
          obj = NULL;
        }
      last_cmd = 1;
      tmob = NULL;
      tobj = NULL;
      break;


    case 'D':		/* set state of door */
      if (real_room(ZCMD.arg1) == NULL)
      {
        log("Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg1);
        exit(1);
      }
      if (ZCMD.arg2 < 0 || ZCMD.arg2 >= NUM_OF_DIRS ||
          (world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2] == NULL))
      {
        ZONE_ERROR
        ("WARNING: door does not exist, command disabled.");
        new_mudlog(BRF, LVL_GOD, FALSE,"%s door in room %d doesn't exist, and zedit needs it.",
                   dirs[ZCMD.arg2], world_vnum[ZCMD.arg1]->number);
      }
      else
        switch (ZCMD.arg3)
        {
        case 0:
          REMOVE_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                     exit_info, EX_LOCKED);
          REMOVE_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                     exit_info, EX_CLOSED);
          REMOVE_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                     exit_info, EX_HIDDEN);
          break;
        case 1:
          SET_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                  exit_info, EX_CLOSED);
          REMOVE_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                     exit_info, EX_LOCKED);
          REMOVE_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                     exit_info, EX_HIDDEN);
          break;
        case 2:
          SET_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                  exit_info, EX_LOCKED);
          SET_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                  exit_info, EX_CLOSED);
          REMOVE_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                     exit_info, EX_HIDDEN);
          break;
        case 3:
          SET_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                  exit_info, EX_CLOSED);
          SET_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                  exit_info, EX_HIDDEN);
          REMOVE_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                     exit_info, EX_LOCKED);
          break;
        case 4:
          SET_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                  exit_info, EX_CLOSED);
          SET_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                  exit_info, EX_LOCKED);
          SET_BIT(world_vnum[ZCMD.arg1]->dir_option[ZCMD.arg2]->
                  exit_info, EX_HIDDEN);
          break;
        }
      last_cmd = 1;
      tmob = NULL;
      tobj = NULL;
      break;

    case 'T': /* trigger command */
      if (real_room(ZCMD.arg1) == NULL)
      {
        log("Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg1);
        exit(1);
      }
      if (ZCMD.arg1==MOB_TRIGGER && tmob)
      {
        if (!SCRIPT(tmob))
          CREATE(SCRIPT(tmob), struct script_data, 1);
        add_trigger(SCRIPT(tmob), read_trigger(ZCMD.arg2), -1);
        last_cmd = 1;
      }
      else if (ZCMD.arg1==OBJ_TRIGGER && tobj)
      {
        if (!SCRIPT(tobj))
          CREATE(SCRIPT(tobj), struct script_data, 1);
        add_trigger(SCRIPT(tobj), read_trigger(ZCMD.arg2), -1);
        last_cmd = 1;
      }
      else if (ZCMD.arg1==WLD_TRIGGER)
      {
        if (ZCMD.arg3 == NOWHERE || ZCMD.arg3>top_of_world)
        {
          ZONE_ERROR("Invalid room number in trigger assignment");
        }
        if (!world_vnum[ZCMD.arg3]->script)
          CREATE(world_vnum[ZCMD.arg3]->script, struct script_data, 1);
        add_trigger(world_vnum[ZCMD.arg3]->script, read_trigger(ZCMD.arg2), -1);
        last_cmd = 1;
      }

      break;

    case 'V':
      if (real_room(ZCMD.arg3) == NULL)
      {
        log("Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg3);
        exit(1);
      }
      if (ZCMD.arg1==MOB_TRIGGER && tmob)
      {
        if (!SCRIPT(tmob))
        {
          ZONE_ERROR("Attempt to give variable to scriptless mobile");
        }
        else
          add_var(&(SCRIPT(tmob)->global_vars), ZCMD.sarg1, ZCMD.sarg2,
                  ZCMD.arg3);
        last_cmd = 1;
      }
      else if (ZCMD.arg1==OBJ_TRIGGER && tobj)
      {
        if (!SCRIPT(tobj))
        {
          ZONE_ERROR("Attempt to give variable to scriptless object");
        }
        else
          add_var(&(SCRIPT(tobj)->global_vars), ZCMD.sarg1, ZCMD.sarg2,
                  ZCMD.arg3);
        last_cmd = 1;
      }
      else if (ZCMD.arg1==WLD_TRIGGER)
      {
        if (ZCMD.arg3 == NOWHERE || ZCMD.arg3>top_of_world)
        {
          ZONE_ERROR("Invalid room number in variable assignment");
        }
        else
        {
          if (!(world_vnum[ZCMD.arg3]->script))
          {
            ZONE_ERROR("Attempt to give variable to scriptless object");
          }
          else
            add_var(&(world_vnum[ZCMD.arg3]->script->global_vars),
                    ZCMD.sarg1, ZCMD.sarg2, ZCMD.arg2);
          last_cmd = 1;
        }
      }
    case 'B':		/* read an object then bury it */
      if (real_room(ZCMD.arg3) == NULL)
      {
        log("Zone: %d - '%c' zone command at command %d invalid number for room vnum %d!", zone_table[zone].number,ZCMD.command, cmd_no, ZCMD.arg3);
        exit(1);
      }
      if ((obj_index[ZCMD.arg1].number < ZCMD.arg2))
      {
        if (ZCMD.arg3 >= 0)
        {
          if (!get_obj_in_list_num
              (ZCMD.arg1, world_vnum[ZCMD.arg3]->contents))
          {
            obj = read_object(ZCMD.arg1, REAL);
            if (obj && load_qic_check(ZCMD.arg1))
            {
              obj_to_room(obj, world_vnum[ZCMD.arg3]);
              if (load_otrigger(obj) == -1)
                obj = NULL;
              tobj = obj;
              if (obj)
                SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_BURIED);

              last_cmd = obj ? 1 : 0;
            }
            else
            {
              if (obj)
              {
                // 	      purge_qic(ZCMD.arg1);
                extract_obj(obj);
                obj = NULL;
                tobj = obj;
              }
              last_cmd = 0;
            }
          }
          else
            last_cmd = 0;

        }
        else
        {
          obj = read_object(ZCMD.arg1, REAL);
          IN_ROOM(obj) = NULL;
          tobj = obj;
          last_cmd = 1;
          SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_BURIED);
        }
      }
      else
        last_cmd = 0;
      tmob = NULL;
      break;

    case 'Z':
      make_maze(zone);
      break;
    default:
      ZONE_ERROR("unknown cmd in reset table; cmd disabled");
      ZCMD.command = '*';
      break;
    }
  }

  zone_table[zone].age = 0;

  /* handle reset_wtrigger's */
  vrm = zone_table[zone].bot;
  while (vrm <= zone_table[zone].top)
  {
    rm = world_vnum[vrm];
    if (rm != NULL) reset_wtrigger(rm);
    vrm++;
  }
}



/* for use in reset_zone; return TRUE if zone 'nr' is free of PC's  */
int is_empty(zone_rnum zone_nr)
{
  struct descriptor_data *i;

  for (i = descriptor_list; i; i = i->next)
  {
    if (STATE(i) != CON_PLAYING)
      continue;
    if (IN_ROOM(i->character) == NULL)
      continue;
    if (i->character->in_room->zone != zone_nr)
      continue;

    /*
     * if an immortal has nohassle off, he counts as present
     * added for testing zone reset triggers - Welcor
     */
    if ((GET_LEVEL(i->character) >= LVL_IMMORT)
        && (PRF_FLAGGED(i->character, PRF_NOHASSLE)))
      continue;


    return (0);
  }

  return (1);
}





/*************************************************************************
*  stuff related to the save/load player system				 *
*************************************************************************/

long get_ptable_by_name(char *name)
{
  int i;
  char arg[MAX_INPUT_LENGTH];
  one_argument(name, arg);
  for (i = 0; i <= top_of_p_table; i++)
    if (!strcmp(player_table[i].name, arg))
      return (i);

  return (-1);
}


long get_ptable_by_id(long id)
{
  int i;
  for (i = 0; i <= top_of_p_table; i++)
    if (player_table[i].id == id)
      return (i);

  return (-1);
}

long get_id_by_name(char *name)
{
  int i;
  char arg[MAX_INPUT_LENGTH];

  one_argument(name, arg);
  for (i = 0; i <= top_of_p_table; i++)
    if (!strcmp(player_table[i].name, arg))
      return (player_table[i].id);

  return (-1);
}

long get_acc_by_name(char *name)
{
  int i;
  char arg[MAX_INPUT_LENGTH];

  one_argument(name, arg);
  for (i = 0; i <= top_of_p_table; i++)
    if (!strcmp(player_table[i].name, arg))
      return (player_table[i].account);

  return (-1);
}
long get_acc_by_id(long id)
{
  int i;
  for (i = 0; i <= top_of_p_table; i++)
    if (player_table[i].id == id)
      return (player_table[i].account);

  return (-1);
}

//returns index of player num for account acc
int get_account_num(int num, long acc)
{
  int i, n = 0;
  for (i = 0; i <= top_of_p_table; i++)
    if (!IS_SET(player_table[i].flags, PINDEX_DELETED) &&
        !IS_SET(player_table[i].flags, PINDEX_SELFDELETE) &&
        player_table[i].name[0] != '\0' && player_table[i].account == acc && n++ == num)
      return (i);

  return (-1);
}


char *get_name_by_id(long id)
{
  int i;

  for (i = 0; i <= top_of_p_table; i++)
    if (player_table[i].id == id)
      return (player_table[i].name);

  return (NULL);
}


int load_char(char *name, struct char_data *ch)
{
  CHAR_DATA *tch;
  int ret_val = 0;
  int tp, k;

  if (ch == NULL)
  {
    new_mudlog(NRM, LVL_GOD, TRUE,"SYSERR: load_char recieved null ch!");
    return -1;
  }

  if (player_table == NULL)
    return -1;

  for (tch = character_list;tch;tch = tch->next)
  {
    if (!IS_NPC(tch) && !strcmp(GET_NAME(tch), name))
    {
      log("load_char loading a character (%s) that is already in the game!", name);
    }
  }
  for (k = 0; (*(name + k) = LOWER(*(name + k))); k++);

  for (tp = 0; tp <= top_of_p_table; tp++)
  {
    if (!IS_SET(player_table[tp].flags, PINDEX_DELETED) &&
        !IS_SET(player_table[tp].flags, PINDEX_SELFDELETE) &&
        (!player_table[tp].name || !*player_table[tp].name))
      continue;
    if (*player_table[tp].name == *name && !strcmp(player_table[tp].name, name))
    {
      ret_val = store_to_char(name, ch);
      if (ret_val != -1)
      {

        return (player_table[tp].id);
      }
      else
        return -2;
    }
  }
  log("NAME: '%s' not found in player index. Create new...", name);
  return -1;
}

#if defined(KEY)
#undef KEY
#endif

#define KEY( literal, field, value )					\
				if ( !strcmp( word, literal ) )	\
				{					\
				    field  = value;			\
				    fMatch = TRUE;			\
				    break;				\
				}

/* provided to free strings */
#if defined(KEYS)
#undef KEYS
#endif

#define KEYS( literal, field, value )					\
				if ( !strcmp( word, literal ) )	\
				{					\
				    free_string(field);			\
				    field  = value;			\
				    fMatch = TRUE;			\
				    break;				\
				}

void default_char(struct char_data *ch)
{
  int i;
  if (!ch)
    return;

  ch->affected = NULL;
  ch->subs = NULL; //yep subskills are a linked list. - mord
  ch->skills = NULL; // and now skills and spells are a linked list
  GET_SEX(ch) = SEX_MALE;
  GET_CLASS(ch) = CLASS_WARRIOR;
  GET_LEVEL(ch) = 1;
  GET_HEIGHT(ch) = 100;
  GET_WEIGHT(ch) = 100;
  GET_ALIGNMENT(ch) = 0;
  for (i = 0; i < 4; i++)
  {
    AFF_FLAGS(ch)[i] = 0;
    PRF_FLAGS(ch)[i] = 0;
    PLR_FLAGS(ch)[i] = 0;
  }
  for (i = 0; i < NUM_CLASSES; i++)
    GET_MASTERY(ch, i) = 0;
  for (i = 0; i < 5; i++)
    GET_SAVE(ch, i) = 0;
  GET_LOADROOM(ch) = NOWHERE;
  GET_INVIS_LEV(ch) = 0;
  GET_FREEZE_LEV(ch) = 0;
  GET_WIMP_LEV(ch) = 5;
  GET_COND(ch, FULL) = 0;
  GET_COND(ch, THIRST) = 0;
  GET_COND(ch, DRUNK) = 0;
  GET_BAD_PWS(ch) = 0;
  GET_PRACTICES(ch) = 0;
  GET_GOLD(ch) = 0;
  GET_BANK_GOLD(ch) = 0;
  GET_EXP(ch) = 0;
  GET_GROUP_EXP(ch) = 0;
  GET_HITROLL(ch) = 0;
  GET_DAMROLL(ch) = 0;
  GET_AC(ch) = 100;
  ch->real_abils.str = 0;
  ch->real_abils.str_add = 0;
  ch->real_abils.dex = 0;
  ch->real_abils.intel = 0;
  ch->real_abils.wis = 0;
  ch->real_abils.con = 0;
  ch->real_abils.cha = 0;
  //GET_SPEED(ch) = 0;
  GET_HIT(ch) = 25;
  GET_MAX_HIT(ch) = 25;
  GET_MANA(ch) = 100;
  GET_MAX_MANA(ch) = 100;
  GET_MOVE(ch) = 50;
  GET_MAX_MOVE(ch) = 50;
  GET_STAMINA(ch) = 100;
  GET_MAX_STAMINA(ch) = 100;
  ch->player_specials->host = NULL;
  AFF_SPEED(ch) = 0;
  ch->player.time.last_logon = time(0);
  GET_PERC(ch) = 100;
  AFK_MSG(ch) = NULL;
  BUSY_MSG(ch) = NULL;
  GET_PK_CNT(ch) = 0;
  GET_PK_RIP(ch) = 0;
  GET_PK_POINTS(ch) = 0;
  GET_POSTS(ch) = 0;
  GET_NAILS(ch)  = 0;
  GET_WIRE(ch)   = 0;
  GET_PERM_OFFENCE(ch)	= 0;
  GET_PERM_DEFENCE(ch)	= 0;
  GET_ORIG_LEV(ch) = 0;
  PRETITLE(ch) = NULL;
  IMMTITLE(ch) = NULL;
  REMORTS(ch) = 0;

  GET_REGEN_HIT(ch) = 0;
  GET_REGEN_MANA(ch) = 0;
  GET_REGEN_MOVE(ch) = 0;
  GET_REGEN_STAMINA(ch) = 0;
  GET_RP_GROUP(ch)  = 0;
  GET_CONVERSIONS(ch) = 1;
  SPECIALS(ch)->last_note = time(0);
  SPECIALS(ch)->last_idea = time(0);
  SPECIALS(ch)->last_penalty = time(0);
  SPECIALS(ch)->last_news = time(0);
  SPECIALS(ch)->last_changes = time(0);
  GET_LAST_DAM_D(ch) = 0;
  GET_LAST_DAM_T(ch) = 0;
  EXTRA_BODY(ch) = 0;

  ch->pnote = NULL;
  SPECIALS(ch)->last_reward = 0;
  GET_REWARD(ch) = 0;
  GET_AWARD(ch) = 0;
  CONCEALMENT(ch) = 0;
  PROMPT(ch)  = NULL;
  BPROMPT(ch) = NULL;
  PAGEWIDTH(ch) = 80;
  PAGEHEIGHT(ch) = 25;
  LOCKER_EXPIRE(ch) = 0;
  LOCKER_LIMIT(ch) = 0;
  GET_KILLS(ch) = NULL;
  GET_LOGOUTMSG(ch) = NULL;
  GET_LOGINMSG(ch) = NULL;

}

int store_to_char(char *name, struct char_data *ch)
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
  struct sub_list *temp = NULL;
  int rec_count = 0;

  if (ch == NULL)
  {
    log("SYSERR: store_to_char recieved null ch!");
    return -1;
  }
  if (!name || !*name)
  {
    log("SYSERR: store_to_char recieved null name!");
    return -1;
  }

  if ((id = find_name(name)) < 0)
  {
    log("Name: '%s' unfound in player index", name);
    return -1;
  }
  else
  {

    snprintf(filename, sizeof(filename), "%s/%c/%s",
             PLR_PREFIX, *player_table[id].name, player_table[id].name);
    if (!(fl = fopen(filename, "r")))
    {

      new_mudlog(NRM, LVL_GOD, TRUE, "SYSERR: Player file not found %s", filename);
      return -1;
    }
    else
    {
      while (get_line(fl, line)&& rec_count < 6)
        rec_count++;
      rewind(fl);
      if (rec_count < 5 )
      {
        fclose(fl);
#if defined(unix)
        /* decompress if .gz file exists */
        snprintf(filename, sizeof(filename), "%s/%c/%s%s", PLR_PREFIX, *player_table[id].name, player_table[id].name, ".bak.gz");
        if ( ( fl = fopen( filename, "r" ) ) != NULL )
        {
          fclose(fl);

          snprintf(buf, sizeof(buf),"gzip -dfq %s", filename);
          system(buf);
          snprintf(buf, sizeof(buf), "cp -f %s/%c/%s.bak %s/%c/%s"
                   ,PLR_PREFIX, *player_table[id].name, player_table[id].name
                   ,PLR_PREFIX, *player_table[id].name, player_table[id].name);
          system(buf);
        }
#endif
        new_mudlog(NRM, LVL_GOD, TRUE, "SYSERR: Player file %s empty, testing backup", filename);
        snprintf(filename, sizeof(filename), "%s/%c/%s",
                 PLR_PREFIX, *player_table[id].name, player_table[id].name);
        if (!(fl = fopen(filename, "r")))
        {
          new_mudlog(NRM, LVL_GOD, TRUE, "SYSERR: Backup failed");
          return -1;
        }
        rec_count = 0;
        while (get_line(fl, line) && rec_count < 6)
          rec_count++;
        rewind(fl);
        if (rec_count < 5)
        {
          fclose(fl);
          return -1;
        }
      }

    }
    if (ch->player_specials == NULL)
      CREATE(ch->player_specials, struct player_special_data, 1);
  }

  /*for (i = 0; i < MAX_AFFECT; i++)
  {
    tmp_aff[i].type = TYPE_UNDEFINED;
    tmp_aff[i].expire = 0;
    tmp_aff[i].modifier = 0;
    tmp_aff[i].location = 0;
    tmp_aff[i].bitvector = 0;
  }*/

  default_char(ch);

  while (get_line(fl, line))
  {
    tag_argument(line, tag);
    num = atoi(line);
    num6 = atoll(line);


    switch (*tag)
    {
    case 'A':
      if (!strcmp(tag, "Ac  "))
        GET_AC(ch) = num;
      else if (!strcmp(tag, "Act "))
      {
        sscanf(line, "%lu %lu %lu %lu", &l1, &l2, &l3, &l4);
        PLR_FLAGS(ch)[0] = l1;
        PLR_FLAGS(ch)[1] = l2;
        PLR_FLAGS(ch)[2] = l3;
        PLR_FLAGS(ch)[3] = l4;
      }
      else if (!strcmp(tag, "Affs"))
      {
        i = 0;
        do
        {
          get_line(fl, line);
          if (sscanf(line, "%d %lld %d %d %d", &num, &num6, &num3, &num4, &num5) == 5)
          {
            tmp_aff[i].type = IRANGE(TYPE_UNDEFINED, num, TOP_SPELL_DEFINE);
            tmp_aff[i].expire = (sec_to_time((long)num6));
            tmp_aff[i].modifier = IRANGE(-1000, num3, 1000);
            tmp_aff[i].location = IRANGE(0, num4, MAX_APPLY);
            tmp_aff[i].bitvector = IRANGE(0, num5, MAX_AFF_APPLY);
            i++;
          }
        }
        while (num != 0);
      }
      else if (!strcmp(tag, "Alin"))
        GET_ALIGNMENT(ch) = num;
      else if (!strcmp(tag, "Awrd"))
        GET_AWARD(ch) = num;
      else if (!strcmp(tag, "Aff "))
        sscanf(line, "%u %u %u %u",
               &(ch->char_specials.saved.affected_by[0]),
               &(ch->char_specials.saved.affected_by[1]),
               &(ch->char_specials.saved.affected_by[2]),
               &(ch->char_specials.saved.affected_by[3]));
      else if (!strcmp(tag, "AfkM"))
        AFK_MSG(ch) = str_dup(line);
      break;

    case 'B':
      if (!strcmp(tag, "Badp"))
        GET_BAD_PWS(ch) = num;
      else if (!strcmp(tag, "Bank"))
        GET_BANK_GOLD(ch) = num6;
      else if (!strcmp(tag, "Brth"))
        ch->player.time.birth = num6;
      else if (!strcmp(tag, "BraT"))
        GET_BRASS_TOKEN_COUNT(ch) = num;
      else if (!strcmp(tag, "BroT"))
        GET_BRONZE_TOKEN_COUNT(ch) = num;
      else if (!strcmp(tag, "BetO"))
        GET_BETTED_ON(ch) = num;
      else if (!strcmp(tag, "Body"))
        EXTRA_BODY(ch) = num;
      else if (!strcmp(tag, "Bpmt"))
        {
          if (BPROMPT(ch))
            free(BPROMPT(ch));
          BPROMPT(ch) = str_dup(line);
        }
      break;

    case 'C':
      if (!strcmp(tag, "Cha "))
        ch->real_abils.cha = num;
      else if (!strcmp(tag, "Clas"))
        GET_CLASS(ch) = num;
      else if (!strcmp(tag, "Con "))
        ch->real_abils.con = num;
      else if (!strcmp(tag, "Clan"))
        GET_CLAN(ch) = num;
      else if (!strcmp(tag, "ClRk"))
        GET_CLAN_RANK(ch) = num;
      else if (!strcmp(tag, "Clns"))
        GET_COOLNESS(ch) = num;
      else if (!strcmp(tag, "Conv"))
        GET_CONVERSIONS(ch) = num;
      else if (!strcmp(tag, "CrPt"))
        CREATE_POINTS(ch) = num;
      break;

    case 'D':
      if (!strcmp(tag, "Desc"))
      {
        char buf2[MAX_STRING_LENGTH];
        strcpy(buf2, "d: store_to_char");
        ch->player.description = fread_string(fl, buf2);
      }
      else if (!strcmp(tag, "Dex "))
        ch->real_abils.dex = num;
      else if (!strcmp(tag, "Drnk"))
        GET_COND(ch, DRUNK) = num;
      else if (!strcmp(tag, "Drol"))
        GET_DAMROLL(ch) = num;
      else if (!strcmp(tag, "DTC "))
        GET_DT_CNT(ch) = num;
      break;

    case 'E':
      if (!strcmp(tag, "Exp "))
        GET_EXP(ch) = num6;
      else if (!strcmp(tag, "ExpG"))
        GET_GROUP_EXP(ch) = num6;
      else if (!strcmp(tag, "Emai"))
      {
        char buf2[MAX_STRING_LENGTH];
        strcpy(buf2, "e: store to char");
        GET_EMAIL(ch) = fread_string(fl, buf2);
      }
      break;

    case 'F':
      if (!strcmp(tag, "Flag"))
        CMD_FLAGS(ch) = num;
      else if (!strcmp(tag, "Frez"))
        GET_FREEZE_LEV(ch) = num;
      break;

    case 'G':
      if (!strcmp(tag, "Gold"))
        GET_GOLD(ch) = num6;
      if (!strcmp(tag, "GolT"))
        GET_GOLD_TOKEN_COUNT(ch) = num;
      break;

    case 'H':
      if (!strcmp(tag, "Hit "))
      {
        sscanf(line, "%d/%d", &num, &num2);
        GET_HIT(ch) = num;
        GET_MAX_HIT(ch) = num2;
      }
      else if (!strcmp(tag, "Hite"))
        GET_HEIGHT(ch) = num;
      else if (!strcmp(tag, "Host"))
      {
        if (!(ch->desc))
          ch->player_specials->host = str_dup(line);
        else
          ch->player_specials->host = str_dup(ch->desc->host);
      }
      else if (!strcmp(tag, "Hrol"))
        GET_HITROLL(ch) = num;
      else if (!strcmp(tag, "Hung"))
        GET_COND(ch, FULL) = num;
      break;

    case 'I':
      if (!strcmp(tag, "Id  "))
      {

        GET_IDNUM(ch) = num6;
      }
      else if (!strcmp(tag, "Int "))
        ch->real_abils.intel = num;
      else if (!strcmp(tag, "Invs"))
        GET_INVIS_LEV(ch) = num;
      else if (!strcmp(tag, "ImTi"))
        IMMTITLE(ch) = str_dup(line);
      break;

    case 'K':
      if (!strcmp(tag, "KilC"))
        GET_KILL_CNT(ch) = num;
      break;

    case 'L':
      if (!strcmp(tag, "Last"))
      {
        if (ch->desc)
          ch->player.time.logon = time(0);
        else
          ch->player.time.logon = num6;
      }
      else if (!strcmp(tag, "Lern"))
        GET_PRACTICES(ch) = num;
      else if (!strcmp(tag, "Levl"))
        GET_LEVEL(ch) = num;
      else if (!(strcmp(tag, "Lnot")))
        SPECIALS(ch)->last_note = num;
      else if (!(strcmp(tag, "Lida")))
        SPECIALS(ch)->last_idea = num;
      else if (!(strcmp(tag, "Lpen")))
        SPECIALS(ch)->last_penalty = num;
      else if (!(strcmp(tag, "Lnew")))
        SPECIALS(ch)->last_news = num;
      else if (!(strcmp(tag, "Lcha")))
        SPECIALS(ch)->last_changes = num;
      else if (!strcmp(tag, "LdRm"))
        GET_LOADROOM(ch) = num;
      else if (!strcmp(tag, "LocE"))
      {
        LOCKER_EXPIRE(ch) = (time_t) num6;

      }
      else if (!strcmp(tag, "LocL"))
        LOCKER_LIMIT(ch) = num;
       /*A little something by Thotter */
      else if (!strcmp(tag, "Lgim")) GET_LOGINMSG(ch)    = strdup(line);
      else if (!strcmp(tag, "Lgom")) GET_LOGOUTMSG(ch)   = strdup(line);
       /*Ends here */
      break;

    case 'M':
      if (!strcmp(tag, "Mana"))
      {
        sscanf(line, "%d/%d", &num, &num2);
        GET_MANA(ch) = num;
        GET_MAX_MANA(ch) = num2;
      }
      else if (!strcmp(tag, "Move"))
      {
        sscanf(line, "%d/%d", &num, &num2);
        GET_MOVE(ch) = num;
        GET_MAX_MOVE(ch) = num2;
      }
      else if (!strcmp(tag, "Msty"))
      {
        do
        {
          get_line(fl, line);
          sscanf(line, "%d %c", &num, &cn);
          if (num != 0)
            set_mastery(ch, cn);
        }
        while (num != 0);
      }
      break;

    case 'N':
      if (!strcmp(tag, "Name"))
        ch->player.name = str_dup(line);
      else if (!strcmp(tag, "Nail"))
        GET_NAILS(ch) = num;
      else if (!strcmp(tag, "NewL"))
        GET_NEWBIE_STATUS(ch) = num;
      break;
    case 'O':
      if (!strcmp(tag, "Olev"))
        GET_ORIG_LEV(ch) = num;
      break;
    case 'P':
      switch (LOWER(tag[2]))
      { /* third letter */
      default:
        break;
      case 'e':
        if (!strcmp(tag, "Pdef"))
          GET_PERM_DEFENCE(ch) = num;
        else if (!strcmp(tag, "Preg"))
          PREG(ch) = num;
        else if (!strcmp(tag, "PreT"))
          PRETITLE(ch) = str_dup(line);
        else if (!strcmp(tag, "Pref"))
        {
          sscanf(line, "%u %u %u %u",
                 &(PRF_FLAGS(ch)[0]),
                 &(PRF_FLAGS(ch)[1]),
                 &(PRF_FLAGS(ch)[2]),
                 &(PRF_FLAGS(ch)[3]));
        }
        break;
      case 'f':
        if (!strcmp(tag, "Poff"))
          GET_PERM_OFFENCE(ch)	= num;
        break;
      case 'h':
        if (!strcmp(tag, "PgHi"))
          PAGEHEIGHT(ch) = num;
        break;
      case 'i':
        if (!strcmp(tag, "Prip"))
          GET_PK_RIP(ch) = num;
        else if (!strcmp(tag, "PfIn"))
          POOFIN(ch) = str_dup(line);
        break;
      case 'm':
        if (!strcmp(tag, "Prmp"))
        {
          if (PROMPT(ch))
            free(PROMPT(ch));
          PROMPT(ch) = str_dup(line);
        }
        break;
      case 'n':
        if (!strcmp(tag, "Ppnt"))
          GET_PK_POINTS(ch) = num;
        else if (!strcmp(tag, "Pcnt"))
          GET_PK_CNT(ch) = num;
        break;
      case 'o':
        if (!strcmp(tag, "PfOt"))
          POOFOUT(ch) = str_dup(line);
        break;
      case 'r':
        if (!strcmp(tag, "Part"))
          PARTNER(ch) = get_id_by_name(line);
        break;
      case 's':
        if (!strcmp(tag, "Pass"))
          strcpy(GET_PASSWD(ch), line);
        else if (!strcmp(tag, "Post"))
          GET_POSTS(ch) = num;
        break;
      case 't':
        if (!strcmp(tag, "Prtn"))
          PARTNER(ch) = num;
        break;
      case 'w':
        if (!strcmp(tag, "PgWd"))
          PAGEWIDTH(ch) = num;
        break;
      case 'y':
        if (!strcmp(tag, "Plyd"))
          ch->player.time.played = num;
        break;
      } /* end of 3rd letter switch */
      break;

    case 'R':
      switch (LOWER(tag[3]))
      {
      case '1':
        if (!strcmp(tag, "Rem1"))
          GET_REMORT(ch) = num;
        break;
      case '2':
        if (!strcmp(tag, "Rem2"))
          GET_REMORT_TWO(ch) = num;
        break;
      case '3':
        if (!strcmp(tag, "Rem3"))
          GET_REMORT_THREE(ch) = num;
        break;
      case 'a':
        if (!strcmp(tag, "Roma"))
          ROMANCE(ch) = num;
        break;
      case 'c':
        if (!strcmp(tag, "RipC"))
          GET_RIP_CNT(ch) = num;
        break;
      case 'd':
        if (!strcmp(tag, "Rwrd"))
          GET_REWARD(ch) = num;
        break;
      case 'e':
        if (!strcmp(tag, "Race"))
          set_race(ch, num);
        break;
      case 'm':
        if (!strcmp(tag, "Room"))
          GET_LOADROOM(ch) =  num;
        else if (!strcmp(tag, "RwTm"))
          SPECIALS(ch)->last_reward = num;
        else if (!strcmp(tag, "RStm"))
          GET_REGEN_STAMINA(ch) = num;
        break;
      case 'n':
        if (!strcmp(tag, "RMan"))
          GET_REGEN_MANA(ch) = num;
        break;
      case 'p':
        if (!strcmp(tag, "RPgp"))
          GET_RP_GROUP(ch) = num;
        break;
      case 's':
        if (!strcmp(tag, "Rems"))
          REMORTS(ch) = num;
        break;
      case 't':
        if (!strcmp(tag, "RHit"))
          GET_REGEN_HIT(ch) = num;
        break;
      case 'v':
        if (!strcmp(tag, "RMov"))
          GET_REGEN_MOVE(ch) = num;
        break;

      }
      break;

    case 'S':
      if (!strcmp(tag, "Sex "))
        GET_SEX(ch) = num;
      else if (!strcmp(tag, "Sped"))
        AFF_SPEED(ch) = num;
      else if (!strcmp(tag, "Stam"))
      {
        sscanf(line, "%d/%d", &num, &num2);
        GET_STAMINA(ch) = num;
        if (num2 >= 100)
          GET_MAX_STAMINA(ch) = num2;
      }
      else if (!strcmp(tag, "Skil"))
      {
        do
        {
          get_line(fl, line);
          sscanf(line, "%d %d %d 0", &num, &num2, &num3);
          if (num != 0)
          {
            set_skill(ch, num, num2);
            set_skill_wait(ch, num, num3);
          }
        }
        while (num != 0);
      }
      else if (!strcmp(tag, "Subs"))
      {
        do
        {
          get_line(fl, line);
          if (sscanf(line, "%d %d %d", &num, &num2, &num3) == 3)
          {
            if (num != 0)
            {
              CREATE(temp, struct sub_list, 1);
              temp->next = ch->subs;
              temp->subskill = (enum subskill_list)(num);
              temp->status = (enum sub_status_types)num3;
              temp->learn = num2;
              ch->subs = temp;
            }
          }


        }
        while (num != 0);
      }

      else if (!strcmp(tag, "Str "))
      {
        sscanf(line, "%d/%d", &num, &num2);
        ch->real_abils.str = num;
        ch->real_abils.str_add = num2;
      }
      else if (!strcmp(tag, "SilT"))
        GET_SILVER_TOKEN_COUNT(ch) = num;
      break;

    case 'T':
      if (!strcmp(tag, "Thir"))
        GET_COND(ch, THIRST) = num;
      else if (!strcmp(tag, "Thr1"))
        GET_SAVE(ch, 0) = num;
      else if (!strcmp(tag, "Thr2"))
        GET_SAVE(ch, 1) = num;
      else if (!strcmp(tag, "Thr3"))
        GET_SAVE(ch, 2) = num;
      else if (!strcmp(tag, "Thr4"))
        GET_SAVE(ch, 3) = num;
      else if (!strcmp(tag, "Thr5"))
        GET_SAVE(ch, 4) = num;
      else if (!strcmp(tag, "Titl"))
        GET_TITLE(ch) = str_dup(line);
      else if (!strcmp(tag, "Tir1"))
        GET_CLASS_TIER(ch) = num;
      else if (!strcmp(tag, "Tir2"))
        GET_REMORT_TIER(ch) = num;
      else if (!strcmp(tag, "Tir3"))
        GET_REMORT_TWO_TIER(ch) = num;
      else if (!strcmp(tag, "Tir4"))
        GET_REMORT_THREE_TIER(ch) = num;
      break;

    case 'W':
      if (!strcmp(tag, "Wate"))
        GET_WEIGHT(ch) = num;
      else if (!strcmp(tag, "Wimp"))
        GET_WIMP_LEV(ch) = num;
      else if (!strcmp(tag, "Wis "))
        ch->real_abils.wis = num;
      else if (!strcmp(tag, "Wire"))
        GET_WIRE(ch) = num;
      break;

    default:
      log( "SYSERR: Unknown tag %s in pfile %s", tag, name);
    }
  }

  fclose(fl);


  if (PRETITLE(ch) && !allowed_pretitle(ch))
  {
    free(PRETITLE(ch));
    PRETITLE(ch) = NULL;
  }

  //ch->real_abils = ch->aff_abils;

  for (i = 0; i < MAX_AFFECT && tmp_aff[i].type != 0; i++)
  {
    if (tmp_aff[i].type)
      affect_to_char(ch, &tmp_aff[i]);
  }

  if (GET_LEVEL(ch) >= LVL_IMMORT)
  {
    for (i = 1; i <= MAX_SKILLS; i++)
      set_skill(ch, i, 100);
    GET_COND(ch, FULL) = -1;
    GET_COND(ch, THIRST) = -1;
    GET_COND(ch, DRUNK) = -1;
  }
  affect_total(ch);
  return 1;
}

struct kill_data *load_killlist(char *name)
{
  char filename[MAX_INPUT_LENGTH], line[READ_SIZE];
  FILE *fl;
  int t[2];
  long tl[2];
  struct kill_data *temp = NULL, *top = NULL;
  if (!get_id_by_name(name))
  {
    log("bad index passed to load_killlist");
    return top;
  }
  snprintf(filename, sizeof(filename), "%s/%c/%s%s",
           PLR_PREFIX, LOWER(*name), name, ".kills");
  if (!(fl = fopen(filename, "r")))
    return top;
  while (get_line(fl, line))
  {
    if (sscanf(line, "%d %d %ld %ld",t, t+1, tl, tl+1) != 4)
      continue;
    CREATE(temp, struct kill_data, 1);
    temp->vnum  = t[0];
    temp->count = t[1];
    temp->first = tl[0];
    temp->last  = tl[1];
    temp->next = top;
    top = temp;
  }
  fclose(fl);
  return top;
}

int save_killlist(int id, struct kill_data *kills)
{
  char filename[MAX_INPUT_LENGTH];
  FILE *fl;
  struct kill_data *temp = NULL;
  if (!kills || id < 0 || id > top_of_p_table)
    return 0;
  snprintf(filename, sizeof(filename), "%s/%c/%s%s",
           PLR_PREFIX, *player_table[id].name, player_table[id].name, ".kills");
  if (!(fl = fopen(filename, "w")))
  {
    log("Can't open file %s for writing", filename);
    return 0;
  }

  for (temp = kills; temp; temp=temp->next)
    fprintf(fl, "%d %d %ld %ld\n",temp->vnum,  temp->count, temp->first, temp->last);

  fclose(fl);
  return 1;
}
void free_killlist_all(struct kill_data *kills)
{
  if (!kills)
    return;
  if (kills->next)
    free_killlist_all(kills->next);
  free(kills);
}
void free_killlist(struct char_data *ch)
{
  free_killlist_all(GET_KILLS(ch));
  GET_KILLS(ch) = NULL;
}

/* remove ^M's from file output */
/* There may be a similar function in Oasis (and I'm sure
   it's part of obuild).  Remove this if you get a
   multiple definition error or if it you want to use a
   substitute
*/
void kill_ems(char *str)
{
  char *ptr1, *ptr2, *tmp;
  tmp = str;
  ptr1 = str;
  ptr2 = str;

  while (*ptr1)
  {
    if ((*(ptr2++) = *(ptr1++)) == '\r')
      if (*ptr1 == '\r')
        ptr1++;
  }
  *ptr2 = '\0';
}

void save_char(struct char_data *ch)
{

  if (ch == NULL)
  {
    log("SYSERR: save_char recieved null ch!");
    return;
  }
  if (IS_NPC(ch))
    return;
  OBJ_INNATE_MESSAGE = FALSE;
  char_to_store(ch);
  save_killlist(get_ptable_by_id(GET_IDNUM(ch)), GET_KILLS(ch));
  OBJ_INNATE_MESSAGE = TRUE;
  save_char_vars(ch);
}


void char_to_store(struct char_data *ch)
{
  FILE *fl;
  char outname[80], bits[127], buf[MAX_STRING_LENGTH];
  char tempname[MAX_STRING_LENGTH];
  int i, id, save_index = FALSE, thing = 0;
  struct affected_type *aff, tmp_aff[MAX_AFFECT];
  struct obj_data *char_eq[NUM_WEARS];
  struct sub_list *temp;
  struct skillspell_data *skill;

  if (IS_NPC(ch) || GET_NAME(ch) == NULL)
  {
    log("SYSERR: Massive fuck up in char to store.");
    return;
  }
  //if (!ch->desc)
  //return;

  for (i = 0; (*(bits + i) = LOWER(*(GET_NAME(ch) + i))); i++);


  snprintf(outname, sizeof(outname), "%s/%c/%s%s", PLR_PREFIX, *bits, bits, PLR_SUFFIX);

  snprintf(tempname, sizeof(tempname), "%s%s", outname, ".tmp");
  if (!tempname)
    return;
  else if (!(fl = fopen(tempname, "w")))
  {
    new_mudlog(NRM, LVL_GOD, TRUE, "SYSERR: Couldn't open temp player file %s for write",
               tempname);
    return;
  }
  /* remove affects from eq and spells (from char_to_store) */
  /* Unaffect everything a character can be affected by */
  for (i = 0; i < NUM_WEARS; i++)
  {
    if (GET_EQ(ch, i))
    {
      char_eq[i] = unequip_char(ch, i);
#ifndef NO_EXTRANEOUS_TRIGGERS
      remove_otrigger(char_eq[i], ch);
#endif

    }
    else
      char_eq[i] = NULL;
  }

  for (aff = ch->affected, i = 0; i < MAX_AFFECT; i++)
  {
    if (aff)
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

  while (ch->affected)
    affect_remove(ch, ch->affected);
  if ((i >= MAX_AFFECT) && aff && aff->next)
    log("SYSERR: WARNING: OUT OF STORE ROOM FOR AFFECTED TYPES!!!");

  ch->aff_abils = ch->real_abils;

  if (GET_NAME(ch))
    fprintf(fl, "Name: %s\n", GET_NAME(ch));
  if (GET_PASSWD(ch))
    fprintf(fl, "Pass: %s\n", GET_PASSWD(ch));
  if (GET_TITLE(ch))
    fprintf(fl, "Titl: %s\n", GET_TITLE(ch));
  if (AFK_MSG(ch))
    fprintf(fl, "AfkM: %s\n", AFK_MSG(ch));
  if (ch->player.description && *ch->player.description)
  {
    strcpy(buf, ch->player.description);
    strip_cr(buf);
    fprintf(fl, "Desc:\n%s~\n", buf);
  }
  if (POOFIN(ch))
    fprintf(fl, "PfIn: %s\n", POOFIN(ch));
  if (POOFOUT(ch))
    fprintf(fl, "PfOt: %s\n", POOFOUT(ch));
  if (IMMTITLE(ch))
    fprintf(fl, "ImTi: %s\n", IMMTITLE(ch));
  fprintf(fl, "Sex : %d\n", GET_SEX(ch));
  fprintf(fl, "Clas: %d\n", GET_CLASS(ch));
  fprintf(fl, "Race: %d\n", GET_RACE(ch));
  fprintf(fl, "Levl: %d\n", GET_LEVEL(ch));
  thing = ch->player.time.played;
  thing += time(0) - ch->player.time.logon;
  fprintf(fl, "Brth: %ld\n", ch->player.time.birth);
  fprintf(fl, "Plyd: %d\n", thing);	// + (time(0) - ch->player.time.logon)));
  if (ch->desc)
    fprintf(fl, "Last: %ld\n", time(0));
  else
    fprintf(fl, "Last: %ld\n", ch->player.time.logon);
  if ((ch->desc) && (ch->desc->host))
    fprintf(fl, "Host: %s\n", ch->desc->host);
  else if (ch->player_specials->host)
    fprintf(fl, "Host: %s\n", ch->player_specials->host);
  fprintf(fl, "Hite: %d\n", GET_HEIGHT(ch));
  fprintf(fl, "Wate: %d\n", GET_WEIGHT(ch));
  fprintf(fl, "Alin: %d\n", GET_ALIGNMENT(ch));
  fprintf(fl, "Id  : %ld\n", GET_IDNUM(ch));
  fprintf(fl, "Act : %u %u %u %u\n", PLR_FLAGS(ch)[0], PLR_FLAGS(ch)[1],
          PLR_FLAGS(ch)[2], PLR_FLAGS(ch)[3]);

  fprintf(fl, "Aff : %u %u %u %u\n", AFF_FLAGS(ch)[0], AFF_FLAGS(ch)[1],
          AFF_FLAGS(ch)[2], AFF_FLAGS(ch)[3]);
  fprintf(fl, "Thr1: %d\n", GET_SAVE(ch, 0));
  fprintf(fl, "Thr2: %d\n", GET_SAVE(ch, 1));
  fprintf(fl, "Thr3: %d\n", GET_SAVE(ch, 2));
  fprintf(fl, "Thr4: %d\n", GET_SAVE(ch, 3));
  fprintf(fl, "Thr5: %d\n", GET_SAVE(ch, 4));
  if (GET_LEVEL(ch) < LVL_IMMORT)
  {
    fprintf(fl, "Skil:\n");
    skill = ch->skills;
    while (skill != NULL)
    {
      if (knows_spell(ch, skill->skill) && skill->learn > 0)
        fprintf(fl, "%d %d %d 0\n", skill->skill, skill->learn, skill->wait);
      skill = skill->next;
    }
    fprintf(fl, "0 0 0 0\n");
    fprintf(fl, "Subs:\n");
    for (temp = ch->subs; temp != NULL; temp = temp->next)
    {
      if (temp->learn > 0)
        fprintf(fl, "%d %d %d\n", temp->subskill, temp->learn, temp->status);
    }
    fprintf(fl, "0 0 0\n");
  }
  if (CMD_FLAGS(ch))
    fprintf(fl, "Flag: %ld\n", CMD_FLAGS(ch));
  fprintf(fl, "Wimp: %d\n", GET_WIMP_LEV(ch));
  fprintf(fl, "Frez: %d\n", GET_FREEZE_LEV(ch));
  if (GET_INVIS_LEV(ch))
    fprintf(fl, "Invs: %d\n", GET_INVIS_LEV(ch));
  fprintf(fl, "Room: %d\n", GET_LOADROOM(ch));
  //   sprintbits(PRF_FLAGS(ch), bits);
  fprintf(fl, "Pref: %u %u %u %u\n", PRF_FLAGS(ch)[0], PRF_FLAGS(ch)[1],
          PRF_FLAGS(ch)[2], PRF_FLAGS(ch)[3]);
  fprintf(fl, "Badp: %d\n", GET_BAD_PWS(ch));
  fprintf(fl, "Hung: %d\n", GET_COND(ch, FULL));
  fprintf(fl, "Thir: %d\n", GET_COND(ch, THIRST));
  fprintf(fl, "Drnk: %d\n", GET_COND(ch, DRUNK));
  if (GET_PRACTICES(ch))
    fprintf(fl, "Lern: %d\n", GET_PRACTICES(ch));
  fprintf(fl, "Str : %d/%d\n", GET_STR(ch), GET_ADD(ch));
  fprintf(fl, "Int : %d\n", GET_INT(ch));
  fprintf(fl, "Wis : %d\n", GET_WIS(ch));
  fprintf(fl, "Dex : %d\n", GET_DEX(ch));
  fprintf(fl, "Con : %d\n", GET_CON(ch));
  fprintf(fl, "Cha : %d\n", GET_CHA(ch));
  fprintf(fl, "Hit : %d/%d\n", GET_HIT(ch), GET_MAX_HIT(ch));
  fprintf(fl, "Mana: %d/%d\n", GET_MANA(ch), GET_MAX_MANA(ch));
  fprintf(fl, "Move: %d/%d\n", GET_MOVE(ch), GET_MAX_MOVE(ch));
  fprintf(fl, "Stam: %d/%d\n", GET_STAMINA(ch), GET_MAX_STAMINA(ch));
  fprintf(fl, "RHit: %d\n", GET_REGEN_HIT(ch));
  fprintf(fl, "RMan: %d\n", GET_REGEN_MANA(ch));
  fprintf(fl, "RMov: %d\n", GET_REGEN_MOVE(ch));
  fprintf(fl, "RStm: %d\n", GET_REGEN_STAMINA(ch));
  fprintf(fl, "Ac  : %d\n", 100);
  if (GET_GOLD(ch))
    fprintf(fl, "Gold: %lld\n", GET_GOLD(ch));
  if (GET_BANK_GOLD(ch))
    fprintf(fl, "Bank: %lld\n", GET_BANK_GOLD(ch));
  fprintf(fl, "Exp : %lld\n", GET_EXP(ch));
  fprintf(fl, "ExpG: %lld\n", GET_GROUP_EXP(ch));
  fprintf(fl, "Hrol: %d\n", 0);
  fprintf(fl, "Drol: %d\n", 0);
  fprintf(fl, "RPgp: %d\n", GET_RP_GROUP(ch));
  fprintf(fl, "Rems: %d\n", REMORTS(ch));
  fprintf(fl, "Rem1: %d\n", GET_REMORT(ch));
  fprintf(fl, "Rem2: %d\n", GET_REMORT_TWO(ch));
  fprintf(fl, "Rem3: %d\n", GET_REMORT_THREE(ch));
  if (PARTNER(ch) != NOBODY)
  {
    fprintf(fl, "Roma: %d\n", ROMANCE(ch));
    fprintf(fl, "Prtn: %ld\n", PARTNER(ch));
  }
  fprintf(fl, "Preg: %d\n", PREG(ch));
  if (PRETITLE(ch))
    fprintf(fl, "PreT: %s\n", PRETITLE(ch));
  if (GET_CLAN(ch))
  {
    fprintf(fl, "Clan: %d\n", GET_CLAN(ch));
    fprintf(fl, "ClRk: %d\n", GET_CLAN_RANK(ch));
  }
  if (GET_BRASS_TOKEN_COUNT(ch))
    fprintf(fl, "BraT: %d\n", GET_BRASS_TOKEN_COUNT(ch));
  if (GET_BRONZE_TOKEN_COUNT(ch))
    fprintf(fl, "BroT: %d\n", GET_BRONZE_TOKEN_COUNT(ch));
  if (GET_SILVER_TOKEN_COUNT(ch))
    fprintf(fl, "SilT: %d\n", GET_SILVER_TOKEN_COUNT(ch));
  if (GET_GOLD_TOKEN_COUNT(ch))
    fprintf(fl, "GolT: %d\n", GET_GOLD_TOKEN_COUNT(ch));
  if (GET_RIP_CNT(ch))
    fprintf(fl, "RipC: %d\n", GET_RIP_CNT(ch));
  if (GET_KILL_CNT(ch))
    fprintf(fl, "KilC: %d\n", GET_KILL_CNT(ch));
  if (GET_DT_CNT(ch))
    fprintf(fl, "DTC : %d\n", GET_DT_CNT(ch));
  if (GET_BETTED_ON(ch) != NOBODY)
    fprintf(fl, "BetO: %d\n", GET_BETTED_ON(ch));
  if (GET_CLASS_TIER(ch))
    fprintf(fl, "Tir1: %d\n", (int) GET_CLASS_TIER(ch));
  if (GET_REMORT_TIER(ch))
    fprintf(fl, "Tir2: %d\n", (int) GET_REMORT_TIER(ch));
  if (GET_REMORT_TWO_TIER(ch))
    fprintf(fl, "Tir3: %d\n", (int) GET_REMORT_TWO_TIER(ch));
  if (GET_REMORT_THREE_TIER(ch))
    fprintf(fl, "Tir4: %d\n", (int) GET_REMORT_THREE_TIER(ch));
  if (AFF_SPEED(ch))
    fprintf(fl, "Sped: %d\n", AFF_SPEED(ch));
  if (GET_COOLNESS(ch))
    fprintf(fl, "Clns: %d\n", GET_COOLNESS(ch));
  fprintf(fl, "LdRm: %d\n", GET_LOADROOM(ch));
  if (GET_PK_CNT(ch))
    fprintf(fl, "Pcnt: %d\n",GET_PK_CNT(ch));
  if (GET_PK_RIP(ch))
    fprintf(fl, "Prip: %d\n",GET_PK_RIP(ch));
  if (GET_PK_POINTS(ch))
    fprintf(fl, "Ppnt: %d\n",GET_PK_POINTS(ch));
  if (GET_POSTS(ch))
    fprintf(fl, "Post: %d\n",GET_POSTS(ch));
  if (GET_NAILS(ch))
    fprintf(fl, "Nail: %d\n",GET_NAILS(ch));
  if (GET_WIRE(ch))
    fprintf(fl, "Wire: %d\n",GET_WIRE(ch));
  if (GET_PERM_OFFENCE(ch))
    fprintf(fl, "Poff: %d\n",GET_PERM_OFFENCE(ch));
  if (GET_PERM_DEFENCE(ch))
    fprintf(fl, "Pdef: %d\n",GET_PERM_DEFENCE(ch));
  if (GET_ORIG_LEV(ch))
    fprintf(fl, "Olev: %d\n", GET_ORIG_LEV(ch));
  if (GET_CONVERSIONS(ch))
    fprintf(fl, "Conv: %d\n", GET_CONVERSIONS(ch));
  fprintf(fl, "Lnot: %ld\n",SPECIALS(ch)->last_note);
  fprintf(fl, "Lida: %ld\n",SPECIALS(ch)->last_idea);
  fprintf(fl, "Lpen: %ld\n",SPECIALS(ch)->last_penalty);
  fprintf(fl, "Lnew: %ld\n",SPECIALS(ch)->last_news);
  fprintf(fl, "Lcha: %ld\n",SPECIALS(ch)->last_changes);
  if (GET_AWARD(ch))
    fprintf(fl, "Awrd: %d\n", GET_AWARD(ch) + GET_REWARD(ch));
  if (SPECIALS(ch)->last_reward)
    fprintf(fl, "RwTm: %d\n", SPECIALS(ch)->last_reward);
  if (PROMPT(ch) && *PROMPT(ch))
    fprintf(fl, "Prmp: %s\n", PROMPT(ch));
  if (BPROMPT(ch) && *BPROMPT(ch))
    fprintf(fl, "Bpmt: %s\n", BPROMPT(ch));
  if (EXTRA_BODY(ch))
    fprintf(fl, "Body: %d\n", EXTRA_BODY(ch));
  fprintf(fl, "PgWd: %d\n", PAGEWIDTH(ch));
  fprintf(fl, "PgHi: %d\n", PAGEHEIGHT(ch));
  if (LOCKER_EXPIRE(ch))
    fprintf(fl, "LocE: %lld\n", (gold_int)LOCKER_EXPIRE(ch));
  if (LOCKER_LIMIT(ch))
    fprintf(fl, "LocL: %d\n", LOCKER_LIMIT(ch));
  if (GET_NEWBIE_STATUS(ch) != -1)
    fprintf(fl, "NewL: %d\n", GET_NEWBIE_STATUS(ch));
  if (GET_EMAIL(ch) && *GET_EMAIL(ch))
    fprintf(fl, "Emai: \n%s~\n", GET_EMAIL(ch));
  /*Here follows a little something made by Thotter */
     if (GET_LOGINMSG(ch))
       fprintf(fl, "Lgim: %s\n", GET_LOGINMSG(ch));
     if(GET_LOGOUTMSG(ch))
       fprintf(fl, "Lgom: %s\n", GET_LOGOUTMSG(ch));
  /*end :P */
  if (CREATE_POINTS(ch))
    fprintf(fl, "CrPt: %d\n", CREATE_POINTS(ch));
  {
    int ism = 0;
    for (i = 0; i < NUM_CLASSES; i++)
      if (GET_MASTERY(ch, i))
      {
        ism++;
        if (ism == 1)  fprintf(fl, "Msty:\n");
        fprintf(fl, "1 %c\n", *pc_class_types[i]);
      }
    if (ism)
      fprintf(fl, "0 0\n");
  }

  fprintf(fl, "Affs:\n");
  for (i = 0; i < MAX_AFFECT; i++)
  {
    aff = &tmp_aff[i];
    if (aff->type && aff->expire != -2)
      fprintf(fl, "%d %ld %d %d %d\n", aff->type, time_to_sec(aff->expire),
              aff->modifier, aff->location, (int) aff->bitvector);
  }
  i = fprintf(fl, "0 0 0 0 0\n");
  fclose(fl);

  /* now that we know there is enough space to put the file, save it
     If it errors, bail - mord
  */
  if (i == -1)
  {
    if (remove(tempname) == -1)
    {
      new_mudlog(NRM, LVL_GOD, TRUE, "unable to remove temp file: %s", tempname);
      log("unable to remove temp file: %s", tempname);
    }
  }
  else if (rename(tempname, outname) == -1)
    core_dump();
  /** put char back together **/

  for (i = 0; i < MAX_AFFECT; i++)
  {
    if (tmp_aff[i].type)
      affect_to_char(ch, &tmp_aff[i]);
  }

  for (i = 0; i < NUM_WEARS; i++)
  {
    if (char_eq[i])
    {
#ifndef NO_EXTRANEOUS_TRIGGERS
      if ((thing = wear_otrigger(char_eq[i], ch, i)) > 0)
      {
#endif
        if (!equip_char(ch, char_eq[i], i))
          new_mudlog(NRM, LVL_GOD, TRUE, "%s : error reequiping %s", GET_NAME(ch), char_eq[i]->short_description);
#ifndef NO_EXTRANEOUS_TRIGGERS

      }
      else if (thing == 0)
        obj_to_char(char_eq[i], ch);
#endif

    }
  }

  if ((id = find_name(GET_NAME(ch))) < 0)
    return;
  if (player_table[id].clan != GET_CLAN(ch))
  {
    save_index = TRUE;
    player_table[id].clan = GET_CLAN(ch);
  }
  if (player_table[id].rank != GET_CLAN_RANK(ch))
  {
    save_index = TRUE;
    player_table[id].rank = GET_CLAN_RANK(ch);
  }
  if (player_table[id].level != GET_LEVEL(ch))
  {
    save_index = TRUE;
    player_table[id].level = GET_LEVEL(ch);
  }
  if (player_table[id].last != ch->player.time.logon)
  {
    save_index = TRUE;
    player_table[id].last = ch->player.time.logon;
  }
if (player_table[id].id != GET_IDNUM(ch))
  {
    save_index = TRUE;
    player_table[id].id = GET_IDNUM(ch);
  }

  i = player_table[id].flags;
  if (PLR_FLAGGED(ch, PLR_DELETED))
    SET_BIT(player_table[id].flags, PINDEX_DELETED);
  else
    REMOVE_BIT(player_table[id].flags, PINDEX_DELETED);
  if (PLR_FLAGGED(ch, PLR_NODELETE) || PLR_FLAGGED(ch, PLR_CRYO))
    SET_BIT(player_table[id].flags, PINDEX_NODELETE);
  else
    REMOVE_BIT(player_table[id].flags, PINDEX_NODELETE);
  if (player_table[id].flags != i || save_index)
    save_player_index();
}


void tag_argument(char *argument, char *tag)
{
  char *tmp = argument, *ttag = tag, *wrt = argument;
  int i;

  for (i = 0; i < 4; i++)
    *(ttag++) = *(tmp++);
  *ttag = '\0';

  while (*tmp == ':' || *tmp == ' ')
    tmp++;

  while (*tmp)
    *(wrt++) = *(tmp++);
  *wrt = '\0';
}

void sprintbits(long vektor, char *outstring)
{
  int i;
  char flags[53] =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

  strcpy(outstring, "");
  for (i = 0; i < 32; i++)
  {
    if (vektor & 1)
    {
      *outstring = flags[i];
      outstring++;
    }
    vektor >>= 1;
  }
  *outstring = 0;
}

void save_etext(struct char_data *ch)
{
  /* this will be really cool soon */

}


/*
 * Create a new entry in the in-memory index table for the player file.
 * If the name already exists, by overwriting a deleted character, then
 * we re-use the old position.
 */
int create_entry(char *name)
{
  int i, pos;

  if (top_of_p_table == -1)
  {	/* no table */
    CREATE(player_table, struct player_index_element, 1);
    pos = top_of_p_table = 0;
  }
  else if ((pos = get_ptable_by_name(name)) == -1)
  {	/* new name */
    i = ++top_of_p_table + 1;

    RECREATE(player_table, struct player_index_element, i);
    pos = top_of_p_table;
  }

  CREATE(player_table[pos].name, char, strlen(name) + 1);

  player_table[pos].id = 0;
  player_table[pos].flags = 0;
  player_table[pos].level = 0;
  player_table[pos].last = 0;
  player_table[pos].account = 0;
  player_table[pos].clan = 0;
  player_table[pos].rank = 0;

  /* copy lowercase equivalent of name to table field */
  for (i = 0; (player_table[pos].name[i] = LOWER(name[i])); i++)
    /* Nothing */ ;

  return (pos);
}



/************************************************************************
*  funcs of a (more or less) general utility nature			*
************************************************************************/



/* file read string */
char *fread_string(FILE *fl, const char *error)
{
  char buf[MAX_STRING_LENGTH], tmp[513];
  char *point;
  int done = 0;
  size_t length = 0, templength, tlen;

  *buf = '\0';

  do
  {
    if (!fgets(tmp, 512, fl))
    {
      log("SYSERR: fread_string: format error at or near %s", error);
      exit(1);
    }
    /* If there is a '~', end the string; else put an "\r\n" over the '\n'. */
    /* now only removes trailing ~'s -- Welcor */
    //point = strchr(tmp, '\0');
    if ((tlen = strlen(tmp)) > 0)
    {
      point = (tmp + tlen - 1);
      for (; (*point=='\r' || *point=='\n'); point--);
      if (*point=='~')
      {
        *point='\0';
        done = 1;
      }
      else
      {
        /** but what if this ends up making it bigger then the buffer?**/
        *(++point) = '\r';
        *(++point) = '\n';
        *(++point) = '\0';
      }

      templength = point - tmp;

      if (length + templength >= MAX_STRING_LENGTH)
      {
        log("SYSERR: fread_string: string too large (db.c)");
        log("%s", error);
        exit(1);
      }
      else
      {
        strcat(buf + length, tmp);	/* strcat: OK (size checked above) */
        length += templength;
      }
    }
  }
  while (!done);

  /* allocate space for the new string and copy it */
  return (strlen(buf) ? strdup(buf) : NULL);
}
/* Called to free all allocated follow_type structs */
void free_followers(struct follow_type *k)
{
  if (!k)
    return;

  if (k->next)
    free_followers(k->next);

  k->follower = NULL;
  free(k);
}

void free_mob_memory(memory_rec *k)
{
  if (!k)
    return;

  if (k->next)
    free_mob_memory(k->next);

  free(k);
}

/* release memory allocated for a char struct */
void free_char(struct char_data *ch)
{
  void free_ignorelist(struct char_data *ch);
  int i;
  struct alias_data *a;


  if (ch == NULL)
    return;

  while (ch->affected)
    affect_remove(ch, ch->affected);

  for (i = 0; i < 4; i++)
    if (GET_POINTS_EVENT(ch, i))
    {
      event_cancel(GET_POINTS_EVENT(ch, i));
      GET_POINTS_EVENT(ch, i) = NULL;
    }
  /* cancel message updates */
  if (GET_MESSAGE_EVENT(ch))
  {
    event_cancel(GET_MESSAGE_EVENT(ch));
    GET_MESSAGE_EVENT(ch) = NULL;
  }
  if (GET_FIGHT_EVENT(ch))
  {
    event_cancel(GET_FIGHT_EVENT(ch));
    GET_FIGHT_EVENT(ch) = NULL;
  }

  free_travel_points(TRAVEL_LIST(ch));
  TRAVEL_LIST(ch) = NULL;
  free_mob_memory(MEMORY(ch));
  MEMORY(ch) = NULL;
  free_followers(ch->followers);
  ch->followers = NULL;
  free_note(ch->pnote, -1);

  while (ch->subs)
    subs_remove(ch, ch->subs);
  ch->subs = NULL;
  while (ch->skills)
    skills_remove(ch, ch->skills);
  ch->skills = NULL;

  free_ignorelist(ch);

  if (ch->player_specials != NULL && ch->player_specials != &dummy_mob)
  {
    while ((a = GET_ALIASES(ch)) != NULL)
    {
      GET_ALIASES(ch) = (GET_ALIASES(ch))->next;
      free_alias(a);
    }
    free_string(&BPROMPT(ch));
    if (PROMPT(ch));
    free(PROMPT(ch));
    if (ch->player_specials->poofin)
      free(ch->player_specials->poofin);
    if (ch->player_specials->poofout)
      free(ch->player_specials->poofout);
    if (ch->player_specials->afk_msg)
      free(ch->player_specials->afk_msg);
    if (ch->player_specials->busy_msg)
      free(ch->player_specials->busy_msg);
    if (ch->player_specials->host)
      free(ch->player_specials->host);
    if (ch->player_specials->pretitle)
      free(ch->player_specials->pretitle);

    if (GET_EMAIL(ch))
      free(GET_EMAIL(ch));
    if (IMMTITLE(ch))
      free(IMMTITLE(ch));

    extract_all_in_list(LOCKER(ch));
    LOCKER(ch) = NULL;
    free_killlist(ch);
    if (ch->player_specials)
      free(ch->player_specials);

    if (IS_NPC(ch))
      log("SYSERR: Mob %s (#%d) had player_specials allocated!", GET_NAME(ch), GET_MOB_VNUM(ch));
  }
  if (!IS_NPC(ch) || (IS_NPC(ch) && GET_MOB_RNUM(ch) == NOBODY))
  {
    /* if this is a player, or a non-prototyped non-player, free all */
    if (GET_NAME(ch))
      free(GET_NAME(ch));
    free_string(&ch->player.title);
    free_string(&ch->player.short_descr);
    free_string(&ch->player.long_descr);
    free_string(&ch->player.description);

    /* free script proto list */
    free_proto_script(ch, MOB_TRIGGER);

    free_join_list(ch->mob_specials.join_list);
    ch->mob_specials.join_list = NULL;

  }
  else if ((i = GET_MOB_RNUM(ch)) != NOBODY)
  {
    /* otherwise, free strings only if the string is not pointing at proto */
    if (ch->player.name && ch->player.name != mob_proto[i].player.name)
      free(ch->player.name);
    if (ch->player.title
        && ch->player.title != mob_proto[i].player.title)
      free(ch->player.title);
    if (ch->player.short_descr
        && ch->player.short_descr != mob_proto[i].player.short_descr)
      free(ch->player.short_descr);
    if (ch->player.long_descr
        && ch->player.long_descr != mob_proto[i].player.long_descr)
      free(ch->player.long_descr);
    if (ch->player.description
        && ch->player.description != mob_proto[i].player.description)
      free(ch->player.description);

    /* free script proto list if it's not the prototype */
    if (ch->proto_script && ch->proto_script != mob_proto[i].proto_script)
      free_proto_script(ch, MOB_TRIGGER);

    if (ch->mob_specials.join_list && ch->mob_specials.join_list != mob_proto[i].mob_specials.join_list)
      free_join_list(ch->mob_specials.join_list);
    ch->mob_specials.join_list = NULL;

  }

  /* free any assigned scripts */
  if (SCRIPT(ch))
    extract_script(ch, MOB_TRIGGER);


  if (ch->desc)
    ch->desc->character = NULL;
  /* find_char helper */
  if (GET_ID(ch) > 0)
    remove_from_lookup_table(GET_ID(ch));
  free(ch);
}

// delayed version of free obj
void obj_data_to_pool(struct obj_data *obj)
{
  struct obj_data *temp;
  for (temp = dead_obj; temp; temp = temp->next)
    if (temp == obj)
    {
      log("Object %s attempted to be added to dead list twice!", obj->short_description);
      return;
    }
  obj->next = dead_obj;
  dead_obj = obj;
}

/* release memory allocated for an obj struct */
void free_obj(struct obj_data *obj, int extracted)
{
void free_identifier(struct obj_data *obj);
  //log("Freeing object: %d: %s", GET_OBJ_VNUM(obj), obj->short_description);
  if (GET_OBJ_RNUM(obj) == NOWHERE)
  {
    free_object_strings(obj);
    /* free script proto list */
    free_proto_script(obj, OBJ_TRIGGER);
  }
  else
  {
    free_object_strings_proto(obj);
    if (obj->proto_script != obj_proto[GET_OBJ_RNUM(obj)].proto_script)
      free_proto_script(obj, OBJ_TRIGGER);
  }

  free_travel_points(TRAVEL_LIST(obj));
  TRAVEL_LIST(obj) = NULL;
  free_identifier(obj);

  /* free any assigned scripts */
  if (SCRIPT(obj))
    extract_script(obj, OBJ_TRIGGER);

  remove_from_lookup_table(GET_ID(obj));
  if (!extracted && GET_OBJ_RNUM(obj) >= 0)
  {
    purge_qic(GET_OBJ_RNUM(obj));
    obj_index[obj->item_number].number--;

  }

  free(obj);
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
int file_to_string_alloc(const char *name, char **buf)
{
  int temppage;
  char temp[MAX_STRING_LENGTH];
  struct descriptor_data *in_use;

  for (in_use = descriptor_list; in_use; in_use = in_use->next)
    if (in_use->showstr_vector && *in_use->showstr_vector == *buf)
      return (-1);

  /* Lets not free() what used to be there unless we succeeded. */
  if (file_to_string(name, temp, sizeof(temp)) < 0)
    return (-1);

  for (in_use = descriptor_list; in_use; in_use = in_use->next)
  {
    if (!in_use->showstr_count || *in_use->showstr_vector != *buf)
      continue;

    /* Let's be nice and leave them at the page they were on. */
    temppage = in_use->showstr_page;
    paginate_string((in_use->showstr_head =
                       strdup(*in_use->showstr_vector)), in_use);
    in_use->showstr_page = temppage;
  }

  if (*buf)
    free(*buf);

  *buf = str_dup(temp);
  return (0);
}


/* read contents of a text file, and place in buf */
int file_to_string(const char *name, char *buf, size_t b_len)
{
  FILE *fl;
  char tmp[READ_SIZE + 3];
  int len;

  *buf = '\0';

  if (!(fl = fopen(name, "r")))
  {
    log("SYSERR: reading %s: %s", name, strerror(errno));
    return (-1);
  }
  for (;;)
  {
    if (!fgets(tmp, READ_SIZE, fl))	/* EOF check */
      break;
    if ((len = strlen(tmp)) > 0)
      tmp[len - 1] = '\0';	/* take off the trailing \n */
    strlcat(tmp, "\r\n", sizeof(tmp));	/* strcat: OK (tmp:READ_SIZE+3) */

    if (strlen(buf) + strlen(tmp) + 1 > MAX_STRING_LENGTH)
    {
      log("SYSERR: %s: string too big (%d max)", name,
          MAX_STRING_LENGTH);
      *buf = '\0';
      fclose(fl);
      return (-1);
    }
    strlcat(buf, tmp, b_len);	/* strcat: OK (size checked above) */
  }

  fclose(fl);


  return (0);
}



/* clear some of the the working variables of a char */
void reset_char(struct char_data *ch)
{
  int i;

  for (i = 0; i < NUM_WEARS; i++)
    GET_EQ(ch, i) = NULL;
  for (i = 0; i < TOP_FUSE_LOCATION; i++)
    FUSE_LOC(ch, i) = NULL;
  DIE_TIME(ch) = 0;
  REMOVE_BIT_AR(PLR_FLAGS(ch), PLR_DYING);
  ATK_CHANCE(ch) = 3;
  FUSED_TO(ch) = NULL;
  GET_SKILLMULTI(ch) = 0.0;
  ch->followers = NULL;
  ch->master = NULL;
  IN_ROOM(ch) = NULL;
  ch->carrying = NULL;
  ch->next = NULL;
  ch->next_fighting = NULL;
  ch->next_in_room = NULL;
  SITTING(ch) = NULL;
  NEXT_SITTING(ch) = NULL;
  FIGHTING(ch) = NULL;
  GET_POINTS_EVENT(ch,0) = NULL;
  GET_POINTS_EVENT(ch, 1) = NULL;
  GET_POINTS_EVENT(ch,2) = NULL;
  GET_POINTS_EVENT(ch,3) = NULL;
  GET_FIGHT_EVENT(ch)  = NULL;
  GET_MESSAGE_EVENT(ch) = NULL;
  GET_TASK(ch)  = NULL;
  GET_MSG_RUN(ch) = 0;
  ch->char_specials.position = POS_STANDING;
  ch->mob_specials.default_pos = POS_STANDING;
  ch->mob_specials.join_list = NULL;
  ch->mob_specials.head_join = NULL;
  ch->char_specials.carry_weight = 0;
  ch->char_specials.carry_items = 0;
  GET_SPELL_DIR(ch) = NOWHERE;
  GET_PERC(ch) = 100.0;
  ch->pnote = NULL;
  LOCKER(ch) = NULL;
  MINE_DIR(ch) = NOWHERE;
  MINE_SPEED(ch) = 0;
  MINE_STEALTH(ch) = 0;
  MINE_BONUS(ch) = 0;
  MINE_DAMAGE(ch) = 0;
  HAS_MAIL(ch) = -1;
  IS_SAVING(ch) = FALSE;
  GET_IGNORELIST(ch) = NULL;

  if (GET_HIT(ch) <= 0)
    GET_HIT(ch) = 1;
  if (GET_MOVE(ch) <= 0)
    GET_MOVE(ch) = 1;
  if (GET_MANA(ch) <= 0)
    GET_MANA(ch) = 1;
  if (GET_MAX_HIT(ch) <= 0)
    GET_MAX_HIT(ch) = 1;
  if (GET_MAX_MOVE(ch) <= 0)
    GET_MAX_MOVE(ch) = 1;
  if (GET_MAX_MANA(ch) <= 0)
    GET_MAX_MANA(ch) = 1;
  if (GET_MAX_STAMINA(ch) <= 0)
    GET_MAX_STAMINA(ch) = 1;
  check_regen_rates(ch);	/* start regening points */
  GET_LAST_TELL(ch) = NOBODY;


}



/* clear ALL the working variables of a char; do NOT free any space alloc'ed */
void clear_char(struct char_data *ch)
{
  memset((char *) ch, 0, sizeof(struct char_data));

  IN_ROOM(ch) = NULL;
  TRAVEL_LIST(ch) = NULL;
  GET_PFILEPOS(ch) = -1;
  GET_IDNUM(ch) = 0;
  GET_NEXT_SKILL(ch) = TYPE_UNDEFINED;
  GET_NEXT_VICTIM(ch) = -1;
  GET_MOB_RNUM(ch) = NOBODY;
  GET_WAS_IN(ch) = NULL;
  GET_POS(ch) = POS_STANDING;
  ch->mob_specials.default_pos = POS_STANDING;
  ch->loader = NOBODY;
  GET_SPELL_DIR(ch) = NOWHERE;
  ch->followers = NULL;
  RIDING(ch) = NULL;
  RIDDEN_BY(ch) = NULL;
  HUNTING(ch) = NULL;
  HUNT_COUNT(ch) = 0;
  //ch->attack_type = NOTHING;
  GET_ATTACK_POS(ch) = TYPE_UNDEFINED;
  //ch->owner_id = -1;
  //ch->familiar_name[0] = '\0';
  ch->has_note[0] = -1;
  ch->has_note[1] = -1;
  ch->has_note[2] = -1;
  ch->has_note[3] = -1;
  ch->has_note[4] = -1;
  GET_POINTS_EVENT(ch,0) = NULL;
  GET_POINTS_EVENT(ch, 1) = NULL;
  GET_POINTS_EVENT(ch,2) = NULL;
  GET_POINTS_EVENT(ch,3) = NULL;
  GET_FIGHT_EVENT(ch)  = NULL;
  GET_MESSAGE_EVENT(ch) = NULL;
  GET_TASK(ch)  = NULL;
  GET_MSG_RUN(ch) = 0;
  GET_AC(ch) = 100;
  ch->points.max_mana = 100;

  CMD_FLAGS2(ch) = 0;

}


void clear_object(struct obj_data *obj)
{
  int i = 0;
  memset((char *) obj, 0, sizeof(struct obj_data));

  GET_OBJ_WAS(obj) = NOTHING;
  TRAVEL_LIST(obj) = NULL;
  obj->item_number = NOTHING;
  obj->owner = 0;
  IN_ROOM(obj) = NULL;
  GET_OBJ_VROOM(obj) = NOWHERE;
  GET_OBJ_TIMER(obj) = -1;
  obj->worn_on = NOWHERE;
  GET_OBJ_VROOM(obj) = NOWHERE;
  for  (i = 0; i < NUM_OBJ_VAL_POSITIONS; i ++)
    GET_OBJ_VAL(obj, i) = 0;
  GET_OBJ_LEVEL(obj) = 0;
  OBJ_SAT_IN_BY(obj) = NULL;
obj->idents = NULL;
  obj->obj_flags.obj_innate = 0;
  obj->skin = NOTHING;
}




/*
 * Called during character creation after picking character class
 * (and then never again for that character).
 */
/* initialize a new character only if class is set */
void init_char(struct char_data *ch)
{
  int i, taeller;

  /* create a player_special structure */
  if (ch->player_specials == NULL)
    CREATE(ch->player_specials, struct player_special_data, 1);

  /* *** if this is our first player --- he be God *** */

  if (top_of_p_table == 0)
  {

    GET_LEVEL(ch) = LVL_IMPL;
    GET_EXP(ch) = exp_needed(ch);



    /* The implementor never goes through do_start(). */
    GET_MAX_HIT(ch) = 1500;
    GET_MAX_MANA(ch) = 1100;
    GET_MAX_MOVE(ch) = 1100;
    GET_MAX_STAMINA(ch) = 5000;
    GET_HIT(ch) = GET_MAX_HIT(ch);
    GET_MANA(ch) = GET_MAX_MANA(ch);
    GET_MOVE(ch) = GET_MAX_MOVE(ch);
    GET_STAMINA(ch) = GET_MAX_STAMINA(ch);
  }
  set_title(ch, NULL);

  /* Romance Initialization, initialize to no partner and single */
  ch->player.romance = 0;
  ch->player.partner = 0;
  GET_KILLS(ch) = NULL;
  ch->player.short_descr = NULL;
  ch->player.long_descr = NULL;
  ch->player.description = NULL;


  ch->player.time.birth = time(0);
  ch->player.time.played = 0;
  ch->player.time.logon = time(0);
  REMORTS(ch) = 0;

  LOCKER_EXPIRE(ch) = 0;
  LOCKER_LIMIT(ch) = 0;


  /* make favors for sex, including MatingMod additions */
  if (ch->player.sex == SEX_MALE)
  {
    ch->player.weight = number(120, 180);
    ch->player.height = number(160, 200);
    PREG(ch) = MALE;
  }
  else
  {
    ch->player.weight = number(100, 160);
    ch->player.height = number(150, 180);
    PREG(ch) = NOT_PREG;	// End of MatingMod Additions
  }

  ch->player.was_class = -1;
  ch->player.was_class1 = -1;
  ch->player.was_class2 = -1;

  /*mordecai */
  ch->player_specials->saved.tier = 1; // done in nanny
  ch->player_specials->saved.tier1 = 0;
  ch->player_specials->saved.tier2 = 0;
  ch->player_specials->saved.tier3 = 0;



  GET_MAX_MANA(ch) = 100;
  GET_MANA(ch) = GET_MAX_MANA(ch);
  GET_HIT(ch) = GET_MAX_HIT(ch);
  GET_MAX_MOVE(ch) = 82;
  GET_MOVE(ch) = GET_MAX_MOVE(ch);
  ch->points.armor = 100;
  ch->points.gold = 15000;
  GET_STAMINA(ch) = 100;
  GET_GROUP_EXP(ch) = 0;
  GET_LOGOUTMSG(ch) = NULL;
  GET_LOGINMSG(ch) = NULL;


  //TODO: check this
  if ((i = get_ptable_by_name(GET_NAME(ch))) != -1)
  {
    while (!valid_id_num(++top_idnum))
      log("Error new id %ld being assigned to %s already exists!",top_idnum, GET_NAME(ch));
    player_table[i].id = GET_IDNUM(ch) = GET_ID(ch) =  top_idnum;

    player_table[i].account = GET_IDNUM(ch);
    add_to_lookup_table(GET_ID(ch), (void *)ch);
  }
  else
    log("SYSERR: init_char: Character '%s' not found in player table.",
        GET_NAME(ch));



  for (taeller = 0; taeller < AF_ARRAY_MAX; taeller++)
    ch->char_specials.saved.affected_by[taeller] = 0;


  for (i = 0; i < 5; i++)
    GET_SAVE(ch, i) = 0;

  if (GET_LEVEL(ch) > LVL_SEN)
  {
    ch->real_abils.intel = 25;
    ch->real_abils.wis = 25;
    ch->real_abils.dex = 25;
    ch->real_abils.str = 25;
    ch->real_abils.str_add = 100;
    ch->real_abils.con = 25;
    ch->real_abils.cha = 25;
  }

  for (i = 0; i < 2; i++)
    GET_COND(ch, i) = (GET_LEVEL(ch) == LVL_IMPL ? -1 : 48);
  GET_COND(ch, 2) = 0;

  GET_LOADROOM(ch) = NOWHERE;
  check_regen_rates(ch);


}



/* returns the real number of the room with given virtual number */
room_rnum real_room(room_vnum vnum)
{
#if 0
  room_rnum bot, top, mid;

  bot = 0;
  top = top_of_world;

  /* perform binary search on world-table */
  for (;;)
  {
    mid = (bot + top) / 2;

    if ((world + mid)->number == vnum)
      return (mid);
    if (bot >= top)
      return (NOWHERE);
    if ((world + mid)->number > vnum)
      top = mid - 1;
    else
      bot = mid + 1;
  }
#endif
  if (vnum >= 0 && vnum <= HIGHEST_VNUM)
    return world_vnum[vnum];
  else
    return NULL;
}

/*
 * Extend later to include more checks.
 *
 * TODO: Add checks for unknown bitvectors.
 */
int check_object(struct obj_data *obj, int nr)
{
  //char objname[MAX_INPUT_LENGTH + 32];
  int error = FALSE;
  int check_potion_price(struct obj_data *obj);
  int check_potion_weight(struct obj_data *obj);
  char buf[MAX_STRING_LENGTH];

  if (GET_OBJ_WEIGHT(obj) < 0)
  {
    log("SYSERR: Object #%d (%s) has negative weight (%d) setting to 0.", nr, obj->short_description, GET_OBJ_WEIGHT(obj));
    GET_OBJ_WEIGHT(obj) = 0;
  }

  if (GET_OBJ_RENT(obj) < 0)
  {
    log("SYSERR: Object #%d (%s) has negative cost/day (%d) fixing.",
        nr, obj->short_description, GET_OBJ_RENT(obj));
    GET_OBJ_RENT(obj) = 0;
  }

  sprintbitarray(GET_OBJ_WEAR(obj), wear_bits, TW_ARRAY_MAX, buf, sizeof(buf));
  if (strstr(buf, "UNDEFINED") && (error = TRUE))
    log("SYSERR: Object #%d (%s) has unknown wear flags.",
        nr, obj->short_description);

  sprintbitarray(GET_OBJ_EXTRA(obj), extra_bits, EF_ARRAY_MAX, buf, sizeof(buf));
  if (strstr(buf, "UNDEFINED") && (error = TRUE))
    log("SYSERR: Object #%d (%s) has unknown extra flags.",
        nr, obj->short_description);

  sprintbitarray(obj->obj_flags.bitvector, affected_bits, AF_ARRAY_MAX,
                 buf, sizeof(buf));
  if (strstr(buf, "UNDEFINED") && (error = TRUE))
    log("SYSERR: Object #%d (%s) has unknown affection flags.",
        nr, obj->short_description);

  switch (GET_OBJ_TYPE(obj))
  {
  case ITEM_DRINKCON:
    {
      char onealias[MAX_INPUT_LENGTH], *space =
        strchr(obj->name, ' ');


      strlcpy(onealias, space ? space + 1 : obj->name,
              sizeof(onealias));
      if (search_block(onealias, drinknames, TRUE) < 0)
      {
        /*log("SYSERR: Object #%d (%s) doesn't have drink type as first alias. (%s) fixing",
           GET_OBJ_VNUM(obj), obj->short_description, obj->name); */
        name_to_drinkcon(obj, GET_OBJ_VAL(obj, 2));
        /*sprintf(obj->name, "%s %s",
        	 drinks[GET_OBJ_VAL(obj, 2)], obj->name);*/
      }
    }
    /* Fall through. */
  case ITEM_FOUNTAIN:
    if (GET_OBJ_VAL(obj, 1) > GET_OBJ_VAL(obj, 0))
    {
      log("SYSERR: Object #%d (%s) contains (%d) more than maximum (%d) fixing.", nr, obj->short_description, GET_OBJ_VAL(obj, 1), GET_OBJ_VAL(obj, 0));
      GET_OBJ_VAL(obj, 1) = GET_OBJ_VAL(obj, 0);
    }
    break;
  case ITEM_SCROLL:
  case ITEM_POTION:

    error |= check_object_level(obj, 0, nr);
    error |= check_object_spell_number(obj, 1, nr);
    error |= check_object_spell_number(obj, 2, nr);
    error |= check_object_spell_number(obj, 3, nr);
    IN_ROOM(obj) = world_vnum[0];
    check_potion_price(obj);
    check_potion_weight(obj);
    IN_ROOM(obj) = NULL;
    break;
  case ITEM_WAND:
  case ITEM_STAFF:
    error |= check_object_level(obj, 0, nr);
    error |= check_object_spell_number(obj, 3, nr);
    if (GET_OBJ_VAL(obj, 2) > GET_OBJ_VAL(obj, 1) && (error = TRUE))
      log("SYSERR: Object #%d (%s) has more charges (%d) than maximum (%d).", nr, obj->short_description, GET_OBJ_VAL(obj, 2), GET_OBJ_VAL(obj, 1));
    IN_ROOM(obj) = world_vnum[0];
    check_potion_price(obj);
    check_potion_weight(obj);
    IN_ROOM(obj) = NULL;
    break;
  }

  return (error);
}

int check_object_spell_number(struct obj_data *obj, int val, int nr)
{
  int error = FALSE;
  const char *spellname;

  if (GET_OBJ_VAL(obj, val) == -1)	/* i.e.: no spell */
    return (error);

  /*
   * Check for negative spells, spells beyond the top define, and any
   * spell which is actually a skill.
   */
  if (GET_OBJ_VAL(obj, val) < 0)
    error = TRUE;
  if (GET_OBJ_VAL(obj, val) > TOP_SPELL_DEFINE)
    error = TRUE;
  if (GET_OBJ_VAL(obj, val) > MAX_SPELLS
      && GET_OBJ_VAL(obj, val) <= MAX_SKILLS)
    error = TRUE;
  if (error)
    log("SYSERR: Object #%d (%s) has out of range spell #%d.",
        nr, obj->short_description, GET_OBJ_VAL(obj,
                                                val));

  /*
   * This bug has been fixed, but if you don't like the special behavior...
   */
#if 0
  if (GET_OBJ_TYPE(obj) == ITEM_STAFF &&
      HAS_SPELL_ROUTINE(GET_OBJ_VAL(obj, val), MAG_AREAS | MAG_MASSES))
    log("... '%s' (#%d) uses %s spell '%s'.",
        obj->short_description, GET_OBJ_VNUM(obj),
        HAS_SPELL_ROUTINE(GET_OBJ_VAL(obj, val),
                          MAG_AREAS) ? "area" : "mass",
        skill_name(GET_OBJ_VAL(obj, val)));
#endif

  if (scheck)			/* Spell names don't exist in syntax check mode. */
    return (error);

  /* Now check for unnamed spells. */
  if (GET_OBJ_VAL(obj, val))
  {	//mord??
    spellname = skill_name(GET_OBJ_VAL(obj, val));

    if ((spellname == unused_spellname
         || !strcmp("UNDEFINED", spellname)) && (error = TRUE))
      log("SYSERR: Object #%d (%s) uses '%s' spell #%d.",
          nr, obj->short_description, spellname,
          GET_OBJ_VAL(obj, val));
  }

  return (error);
}

int check_object_level(struct obj_data *obj, int val, int nr)
{
  int error = FALSE;

  if ((GET_OBJ_VAL(obj, val) < 0))
  {
    log("SYSERR: Object #%d (%s) has out of range level #%d changing to 1.", nr, obj->short_description, GET_OBJ_VAL(obj, val));
    GET_OBJ_VAL(obj, val) = 1;
  }
  else if ((GET_OBJ_VAL(obj, val) > LVL_IMPL))
  {
    log("SYSERR: Object #%d (%s) has out of range level #%d changing to 56.", nr, obj->short_description, GET_OBJ_VAL(obj, val));
    GET_OBJ_VAL(obj, val) = 56;
  }

  return (error);
}

int my_obj_save_to_disk(FILE * fp, struct obj_data *obj, int locate)
{
  int counter2;
  struct extra_descr_data *ex_desc;
  char buf1[MAX_STRING_LENGTH + 1];
  char smell[MAX_STRING_LENGTH + 1];
  char taste[MAX_STRING_LENGTH + 1];
  char feel[MAX_STRING_LENGTH + 1];

  if (obj->action_description)
  {
    strcpy(buf1, obj->action_description);
    strip_cr(buf1);
  }
  else
    *buf1 = 0;
  if (obj->smell)
  {
    strcpy(smell, obj->smell);
    strip_cr(smell);
  }
  else
    *smell = 0;
  if (obj->taste)
  {
    strcpy(taste, obj->taste);
    strip_cr(taste);
  }
  else
    *taste = 0;
  if (obj->feel)
  {
    strcpy(feel, obj->feel);
    strip_cr(feel);
  }
  else
    *feel = 0;

  fprintf(fp,
          "#%d\n"
          "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
          GET_OBJ_VNUM(obj),
          locate,
          GET_OBJ_VAL(obj, 0),
          GET_OBJ_VAL(obj, 1),
          GET_OBJ_VAL(obj, 2),
          GET_OBJ_VAL(obj, 3),
          GET_OBJ_VAL(obj, 4),
          GET_OBJ_VAL(obj, 5),
          GET_OBJ_VAL(obj, 6),
          GET_OBJ_VAL(obj, 7),
          GET_OBJ_VAL(obj, 8),
          GET_OBJ_VAL(obj, 9),
          GET_OBJ_EXTRA(obj)[0],
          GET_OBJ_EXTRA(obj)[1],
          GET_OBJ_EXTRA(obj)[2],
          GET_OBJ_EXTRA(obj)[3], GET_OBJ_TIMER(obj), GET_OBJ_INNATE(obj));

  if (!(IS_OBJ_STAT(obj, ITEM_UNIQUE_SAVE)))
  {
    return 1;
  }
  fprintf(fp,
          "XAP\n"
          "%s~\n"
          "%s~\n"
          "%s~\n"
          "%s~\n"
          "%d %d %d %d %d %d %d %d\n",
          obj->name ? obj->name : "undefined",
          obj->short_description ? obj->short_description : "undefined",
          obj->description ? obj->description : "undefined",
          buf1,
          GET_OBJ_TYPE(obj),
          GET_OBJ_WEAR(obj)[0],
          GET_OBJ_WEAR(obj)[1],
          GET_OBJ_WEAR(obj)[2],
          GET_OBJ_WEAR(obj)[3],
          GET_OBJ_WEIGHT(obj), GET_OBJ_COST(obj), GET_OBJ_RENT(obj)
         );
  /* Do we have affects? */
  for (counter2 = 0; counter2 < MAX_OBJ_AFFECT; counter2++)
    if (obj->affected[counter2].modifier)
      fprintf(fp, "A\n"
              "%d %d\n",
              obj->affected[counter2].location,
              obj->affected[counter2].modifier);

  /* Do we have extra descriptions? */
  if (obj->ex_description)
  {	/*. Yep, save them too . */
    for (ex_desc = obj->ex_description; ex_desc;
         ex_desc = ex_desc->next)
    {
      /*. Sanity check to prevent nasty protection faults . */
      if (!*ex_desc->keyword || !*ex_desc->description)
      {
        continue;
      }
      strcpy(buf1, ex_desc->description);
      strip_cr(buf1);
      fprintf(fp, "E\n" "%s~\n" "%s~\n", ex_desc->keyword, buf1);
    }
  }

  return 1;
}



int read_xap_objects(FILE * fl, struct char_data *ch)
{
  char line[MAX_STRING_LENGTH];
  int t[20], nr, danger, j, k, zwei;
  struct obj_data *temp = NULL;
  struct extra_descr_data *new_descr;
  int num_of_objs = 0;
  char buf2[MAX_INPUT_LENGTH];
  int retval;

  if (!feof(fl))
    get_line(fl, line);
  while (!feof(fl) && line != NULL)
  {
    /* first, we get the number. Not too hard. */
    if (*line == '#')
    {
      num_of_objs++;
      if (sscanf(line, "#%d", &nr) != 1)
      {
        continue;
      }
      /* we have the number, check it, load obj. */
      if (nr == -1)
      {	/* then its unique */
        temp = create_obj();
        temp->item_number = NOTHING;
        SET_BIT_AR(GET_OBJ_EXTRA(temp), ITEM_UNIQUE_SAVE);
      }
      else if (nr < 0)
      {
        continue;
      }
      else
      {
        temp = read_object(nr, VIRTUAL);
        if (nr >= 99999 || !temp)
        {
          continue;
        }
      }
      /* now we read locate. - this is for autoeq, will be 0 elsewise */
      /* only the rest of the vals, and extra flags will be read */
      get_line(fl, line);
      retval = sscanf(line, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                      t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6, t + 7,
                      t + 8, t + 9, t + 10, t + 11, t + 12, t + 13, t + 14, t + 15, t + 16);

      if (retval == 10)
      {
        GET_OBJ_VAL(temp, 0) = t[1];
        GET_OBJ_VAL(temp, 1) = t[2];
        GET_OBJ_VAL(temp, 2) = t[3];
        GET_OBJ_VAL(temp, 3) = t[4];
        GET_OBJ_EXTRA(temp)[0] = t[5];
        GET_OBJ_EXTRA(temp)[1] = t[6];
        GET_OBJ_EXTRA(temp)[2] = t[7];
        GET_OBJ_EXTRA(temp)[3] = t[8];
        GET_OBJ_TIMER(temp) = t[9];
        /*---unknown----*/
        GET_OBJ_VAL(temp, 4) = 0;
        GET_OBJ_VAL(temp, 5) = 0;
        GET_OBJ_VAL(temp, 6) = 0;
        GET_OBJ_VAL(temp, 7) = 0;
        GET_OBJ_VAL(temp, 8) = 0;
        GET_OBJ_VAL(temp, 9) = 0;
        GET_OBJ_INNATE(temp) = 0;
      }
      else
      {
        GET_OBJ_VAL(temp, 0) = t[1];
        GET_OBJ_VAL(temp, 1) = t[2];
        GET_OBJ_VAL(temp, 2) = t[3];
        GET_OBJ_VAL(temp, 3) = t[4];
        GET_OBJ_VAL(temp, 4) = t[5];
        GET_OBJ_VAL(temp, 5) = t[6];
        GET_OBJ_VAL(temp, 6) = t[7];
        GET_OBJ_VAL(temp, 7) = t[8];
        GET_OBJ_VAL(temp, 8) = t[9];
        GET_OBJ_VAL(temp, 9) = t[10];
        GET_OBJ_EXTRA(temp)[0] = t[11];
        GET_OBJ_EXTRA(temp)[1] = t[12];
        GET_OBJ_EXTRA(temp)[2] = t[13];
        GET_OBJ_EXTRA(temp)[3] = t[14];
        GET_OBJ_TIMER(temp) = t[15];
        GET_OBJ_INNATE(temp) = t[16];
      }

      get_line(fl, line);
      /* read line check for xap. */
      if (!strcasecmp("XAP", line))
      {	/* then this is a Xap Obj, requires
                                                                                                                                                						   special care */
        if ((temp->name = fread_string(fl, buf2)) == NULL)
        {
          temp->name = "undefined";
        }

        if ((temp->short_description =
               fread_string(fl, buf2)) == NULL)
        {
          temp->short_description = "undefined";
        }

        if ((temp->description = fread_string(fl, buf2)) == NULL)
        {
          temp->description = "undefined";
        }

        if ((temp->action_description =
               fread_string(fl, buf2)) == NULL)
        {
          temp->action_description = 0;
        }

        if ((temp->smell = fread_string(fl, buf2)) == NULL)
          temp->smell = 0;

        if ((temp->taste = fread_string(fl, buf2)) == NULL)
          temp->taste = 0;

        if ((temp->feel = fread_string(fl, buf2)) == NULL)
          temp->feel = 0;

        if (!get_line(fl, line) ||
            (sscanf(line, "%d %d %d %d %d %d %d %d",
                    t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6,
                    t + 7) != 8))
        {
          log(
            "Format error in first numeric line (expecting _8_ args)");
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

        for (j = 0; j < MAX_OBJ_AFFECT; j++)
        {
          temp->affected[j].location = APPLY_NONE;
          temp->affected[j].modifier = 0;
        }

        /* You have to null out the extradescs when you're parsing a xap_obj.
           This is done right before the extradescs are read. */

        if (temp->ex_description)
        {
          temp->ex_description = NULL;
        }

        for (k = j = zwei = 0; !zwei && !feof(fl);)
        {
          switch (*line)
          {
          case 'E':
            CREATE(new_descr, struct extra_descr_data, 1);
            new_descr->keyword = fread_string(fl, buf2);
            new_descr->description = fread_string(fl, buf2);
            new_descr->next = temp->ex_description;
            temp->ex_description = new_descr;
            get_line(fl, line);
            break;
          case 'A':
            if (j >= MAX_OBJ_AFFECT)
            {
              log("Too many object affectations in loading rent file");
              danger = 1;
            }
            get_line(fl, line);
            sscanf(line, "%d %d", t, t + 1);

            temp->affected[j].location = t[0];
            temp->affected[j].modifier = t[1];
            j++;
            get_line(fl, line);
            break;

          case '$':
          case '#':
            zwei = 1;
            break;
          default:
            zwei = 1;
            break;
          }
        }		/* exit our for loop */
        get_line(fl, line);
      }			/* exit our xap loop */
      generate_weapon(temp);
      if (temp != NULL)
      {
        obj_to_char(temp, ch);
      }
    }
  }				/* exit our while loop */
  return num_of_objs;
}




/* returns the real number of the monster with given virtual number */
mob_rnum real_mobile(mob_vnum vnum)
{
#if 0
  mob_rnum bot, top, mid;

  if (vnum == NOTHING)
    return NOTHING;

  bot = 0;
  top = top_of_mobt;

  /* perform binary search on mob-table */
  for (;;)
  {
    mid = (bot + top) / 2;

    if ((mob_index + mid)->vnum == vnum)
      return (mid);
    if (bot >= top)
      return (-1);
    if ((mob_index + mid)->vnum > vnum)
      top = mid - 1;
    else
      bot = mid + 1;
  }
#elseif (0)
  int i;
  for (i = 0; i <= top_of_mobt; i++)
  {
    if (mob_index[i].vnum == vnum)
      return i;
  }
  return -1;
#else
 mob_rnum bot, top, mid, i, last_top;

  i = htree_find(mob_htree, vnum);

  if (i != NOBODY && mob_index[i].vnum == vnum)
    return i;
  else {
  bot = 0;
  top = top_of_mobt;

  /* perform binary search on mob-table */
  for (;;) {
    last_top = top;
    mid = (bot + top) / 2;

      if ((mob_index + mid)->vnum == vnum) {
        log("mob_htree sync fix: %d: %d -> %d", vnum, i, mid);
        htree_add(mob_htree, vnum, mid);
      return (mid);
      }
    if (bot >= top)
      return (NOBODY);
    if ((mob_index + mid)->vnum > vnum)
      top = mid - 1;
    else
      bot = mid + 1;

    if (top > last_top)
      return NOWHERE;
  }
  }

#endif
}



/* returns the real number of the object with given virtual number */
obj_rnum real_object(obj_vnum vnum)
{
#if 0
  obj_rnum bot, top, mid;

  if (vnum == NOTHING)
    return NOTHING;

  bot = 0;
  top = top_of_objt;

  /* perform binary search on obj-table */
  for (;;)
  {
    mid = (bot + top) / 2;

    if ((obj_index + mid)->vnum == vnum)
      return (mid);
    if (bot >= top)
      return (-1);
    if ((obj_index + mid)->vnum > vnum)
      top = mid - 1;
    else
      bot = mid + 1;
  }
#elseif (0)
  int i;
  for (i = 0; i <= top_of_objt; i++)
  {
    if (obj_index[i].vnum == vnum)
      return i;
  }
  return -1;
#else
 obj_rnum bot, top, mid, i, last_top;

  i = htree_find(obj_htree, vnum);

  if (i != NOWHERE && obj_index[i].vnum == vnum)
    return i;
  else {
  bot = 0;
  top = top_of_objt;

  /* perform binary search on obj-table */
  for (;;) {
    last_top = top; 
    mid = (bot + top) / 2;

      if ((obj_index + mid)->vnum == vnum) {
        log("obj_htree sync fix: %d: %d -> %d", vnum, i, mid);
        htree_add(obj_htree, vnum, mid);
      return (mid);
      }
    if (bot >= top)
      return (NOTHING);
    if ((obj_index + mid)->vnum > vnum)
      top = mid - 1;
    else
      bot = mid + 1;

    if (top > last_top)
      return NOWHERE;
  }
  }
#endif
}




int check_bitvector_names(bitvector_t bits, size_t namecount,
                          const char *whatami, const char *whatbits)
{
  unsigned int flagnum;
  bool error = FALSE;

  /* See if any bits are set above the ones we know about. */
  if (bits <=
      (~(bitvector_t) 0 >> (sizeof(bitvector_t) * 8 - namecount)))
    return (FALSE);

  for (flagnum = namecount; flagnum < sizeof(bitvector_t) * 8; flagnum++)
    if ((1 << flagnum) & bits)
    {
      log("SYSERR: %s has unknown %s flag, bit %d (0 through %d known).", whatami, whatbits, flagnum, namecount - 1);
      error = TRUE;
    }

  return (error);
}


void generate_weapon(OBJ_DATA *obj)
{

  if (GET_OBJ_TYPE(obj) != ITEM_WEAPON)
    return;
  else
  {
    char buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH], *point;


    strcpy(buf, obj->short_description);
    strip_color(buf, sizeof(buf));
    point = buf;

    do
    {
      point = any_one_arg(point, buf2);
      if ((GET_WEP_TYPE(obj) = generate_wep_type(buf2)) != 0)
        break;
    }
    while (point != NULL && *point);

  }

  if (GET_WEP_TYPE(obj) == 0)
    GET_WEP_TYPE(obj) = gen_wep_type_from_attack(obj);

  if (GET_WEP_LENGTH(obj) == 0)
    GET_WEP_LENGTH(obj) = generate_wep_length(obj);

  if (GET_WEP_BALANCE(obj) == 0)
    GET_WEP_BALANCE(obj) = fuzzy_balance(obj);

  if (GET_WEP_LENGTH(obj) >= 10)
    GET_OBJ_WEIGHT(obj) = (GET_WEP_LENGTH(obj)/10);

}



void give_mob_class(struct char_data *ch, int vnum)
{
  char buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];
  char *point;
  int check_name_is_class_type(char *name);


  if (GET_NAME(ch) == NULL)
    return;

  strcpy(buf, ch->player.name);
  point = buf;

  do
  {
    point = any_one_arg(point, buf2);
    if ((GET_CLASS(ch) = check_name_is_class_type(buf2)) != CLASS_NORMAL)
      return;
  }
  while (point != NULL && *point);


}

#define CHNAME(word)  (compares((word), name))
int check_name_is_class_type(char *name)
{
  if (!name || !*name)
    return CLASS_NORMAL;

  /*CASTER */
  switch (LOWER(*name))
  {
  case 'c':
    if CHNAME("cleric") return CLASS_CASTER;
    break;
  case 'd':
    if CHNAME("druid") return CLASS_CASTER;
    break;
  case 'f':
    if CHNAME("fairy") return CLASS_CASTER;
    break;
  case 'h':
    if CHNAME("healer") return CLASS_CASTER;
    break;
  case 'm':
    if CHNAME("mage") return CLASS_CASTER;
    if CHNAME("magician") return CLASS_CASTER;
    if CHNAME("monk") return CLASS_CASTER;
    break;
  case 'p':
    if CHNAME("pixie") return CLASS_CASTER;
    break;
  case 's':
    if CHNAME("sorceror") return CLASS_CASTER;
    if CHNAME("sorceress") return CLASS_CASTER;
    if CHNAME("shaman") return CLASS_CASTER;
    break;
  case 'w':
    if CHNAME("witch") return CLASS_CASTER;
    if CHNAME("warlock") return CLASS_CASTER;
    if CHNAME("wizard") return CLASS_CASTER;
    break;
  }
  /*UNDEAD */
  switch (*name)
  {
  case 'b':
    if CHNAME("body") return CLASS_UNDEAD;
    break;
  case 'c':
    if CHNAME("corpse") return CLASS_UNDEAD;
    break;
  case 'd':
    if CHNAME("dead") return CLASS_UNDEAD;
    if CHNAME("demon") return CLASS_UNDEAD;
    break;
  case 'g':
    if CHNAME("ghost") return CLASS_UNDEAD;
    if CHNAME("ghoul") return CLASS_UNDEAD;
    break;
  case 'm':
    if CHNAME("mummy") return CLASS_UNDEAD;
    break;
  case 'n':
    if CHNAME("nightcrawler") return CLASS_UNDEAD;
    break;
  case 's':
    if CHNAME("soul") return CLASS_UNDEAD;
    if CHNAME("spectre") return CLASS_UNDEAD;
    if CHNAME("specter") return CLASS_UNDEAD;
    if CHNAME("skeletons") return CLASS_UNDEAD;
    break;
  case 'u':
    if CHNAME("undead") return CLASS_UNDEAD;
    break;
  case 'v':
    if CHNAME("vampire") return CLASS_UNDEAD;
    break;
  case 'z':
    if CHNAME("zombie") return CLASS_UNDEAD;
    break;
  }
  /*FIGHTER*/
  switch (*name)
  {
  case 'f':
    if CHNAME("fighter") return CLASS_FIGHTER;
    break;
  case 'g':
    if CHNAME("guard") return CLASS_FIGHTER;
    break;
  case 'k':
    if CHNAME("knight") return CLASS_FIGHTER;
    if CHNAME("king") return CLASS_FIGHTER;
    break;
  case 'm':
    if CHNAME("mercanary") return CLASS_FIGHTER;
    break;
  case 'p':
    if CHNAME("paladin") return CLASS_FIGHTER;
    if CHNAME("page") return CLASS_FIGHTER;
    if CHNAME("police") return CLASS_FIGHTER;
    break;
  case 's':
    if CHNAME("soldier") return CLASS_FIGHTER;
    break;
  case 'v':
    if CHNAME("viking") return CLASS_FIGHTER;
    break;
  case 'w':
    if CHNAME("warrior") return CLASS_FIGHTER;
    break;
  }
  /*ROGUE  */
  switch (*name)
  {
  case 'a':
    if CHNAME("assassin") return CLASS_ROGUE;
    break;
  case 'b':
    if CHNAME("bedouin") return CLASS_ROGUE;
    break;
  case 'c':
    if CHNAME("crooked") return CLASS_ROGUE;
    break;
  case 'g':
    if CHNAME("gypsy") return CLASS_ROGUE;
    break;
  case 'p':
    if CHNAME("pickpocket") return CLASS_ROGUE;
    break;
  case 'r':
    if CHNAME("robber") return CLASS_ROGUE;
    if CHNAME("rogue") return CLASS_ROGUE;
    break;
  case 's':
    if CHNAME("spy") return CLASS_ROGUE;
    break;
  case 't':
    if CHNAME("thug") return CLASS_ROGUE;
    if CHNAME("thief") return CLASS_ROGUE;
    break;
  }

  /*ANIMAL */
  switch (*name)
  {
  case 'a':
    if CHNAME("ass") return CLASS_ANIMAL;
    break;
  case 'b':
    if CHNAME("bird") return CLASS_ANIMAL;
    if CHNAME("bat") return CLASS_ANIMAL;
    if CHNAME("bear") return CLASS_ANIMAL;
    if CHNAME("bee") return CLASS_ANIMAL;
    break;
  case 'c':
    if CHNAME("cockroach") return CLASS_ANIMAL;
    if CHNAME("cat") return CLASS_ANIMAL;
    if CHNAME("cow") return CLASS_ANIMAL;
    if CHNAME("chicken") return CLASS_ANIMAL;
    if CHNAME("crow") return CLASS_ANIMAL;
    if CHNAME("coyote") return CLASS_ANIMAL;
    break;
  case 'd':
    if CHNAME("dog") return CLASS_ANIMAL;
    if CHNAME("donkey") return CLASS_ANIMAL;
    if CHNAME("deer") return CLASS_ANIMAL;
    if CHNAME("dolphin") return CLASS_ANIMAL;
    if CHNAME("dove") return CLASS_ANIMAL;
    if CHNAME("duck") return CLASS_ANIMAL;
    break;
  case 'e':
    if CHNAME("eagle") return CLASS_ANIMAL;
    break;
  case 'f':
    if CHNAME("fish") return CLASS_ANIMAL;
    if CHNAME("frog") return CLASS_ANIMAL;
    break;
  case 'g':
    if CHNAME("goat") return CLASS_ANIMAL;
    break;
  case 'h':
    if CHNAME("horse") return CLASS_ANIMAL;
    if CHNAME("hawk") return CLASS_ANIMAL;
    if CHNAME("hen") return CLASS_ANIMAL;
    break;
  case 'i':
    if CHNAME("iguana") return CLASS_ANIMAL;
    break;
  case 'j':
    if CHNAME("jackrabit") return CLASS_ANIMAL;
    break;
  case 'k':
    if CHNAME("kitten") return CLASS_ANIMAL;
    if CHNAME("kangaroo") return CLASS_ANIMAL;
    break;
  case 'l':
    if CHNAME("lizard") return CLASS_ANIMAL;
    if CHNAME("lion") return CLASS_ANIMAL;
    break;
  case 'm':
    if CHNAME("mule") return CLASS_ANIMAL;
    if CHNAME("mouse") return CLASS_ANIMAL;
    break;
  case 'o':
    if CHNAME("octopus") return CLASS_ANIMAL;
    break;
  case 'p':
    if CHNAME("pet") return CLASS_ANIMAL;
    if CHNAME("puppy") return CLASS_ANIMAL;
    if CHNAME("pig") return CLASS_ANIMAL;
    if CHNAME("piglet") return CLASS_ANIMAL;
    if CHNAME("pinto") return CLASS_ANIMAL;
    if CHNAME("pony") return CLASS_ANIMAL;
    break;
  case 'q':
    if CHNAME("quail") return CLASS_ANIMAL;
    break;
  case 'r':
    if CHNAME("rat") return CLASS_ANIMAL;
    if CHNAME("rooster") return CLASS_ANIMAL;
    break;
  case 's':
    if CHNAME("snake") return CLASS_ANIMAL;
    if CHNAME("shark") return CLASS_ANIMAL;
    if CHNAME("sheep") return CLASS_ANIMAL;
    break;
  case 't':
    if CHNAME("toad") return CLASS_ANIMAL;
    if CHNAME("turtle") return CLASS_ANIMAL;
    if CHNAME("tiger") return CLASS_ANIMAL;
    break;
  case 'v':
    if CHNAME("vulture") return CLASS_ANIMAL;
    break;
  case 'w':
    if CHNAME("wolf") return CLASS_ANIMAL;
    if CHNAME("worm") return CLASS_ANIMAL;
    if CHNAME("whale") return CLASS_ANIMAL;
    if CHNAME("wasp") return CLASS_ANIMAL;
    break;
  case 'z':
    if CHNAME("zebra") return CLASS_ANIMAL;
    break;
  }


  return CLASS_NORMAL;

}

/*
 * Read a number from a file.
 */
int fread_number( FILE *fp )
{
  int number;
  bool sign;
  char c;

  do
  {
    c = getc( fp );
  }
  while ( isspace(c) );

  number = 0;

  sign   = FALSE;
  if ( c == '+' )
  {
    c = getc( fp );
  }
  else if ( c == '-' )
  {
    sign = TRUE;
    c = getc( fp );
  }

  if ( !isdigit(c) )
  {
    log( "BUG: Fread_number: bad format." );
    exit( 1 );
  }

  while ( isdigit(c) )
  {
    number = number * 10 + c - '0';
    c      = getc( fp );
  }

  if ( sign )
    number = 0 - number;

  if ( c == '|' )
    number += fread_number( fp );
  else if ( c != ' ' )
    ungetc( c, fp );

  return number;
}

/*
 * Read one word (into static buffer).
 */
char *fread_word( FILE *fp )
{
  static char word[MAX_INPUT_LENGTH];
  char *pword;
  char cEnd;

  do
  {
    cEnd = getc( fp );
  }
  while ( isspace( cEnd ) );

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
    *pword = getc( fp );
    if ( cEnd == ' ' ? isspace(*pword) : *pword == cEnd )
    {
      if ( cEnd == ' ' )
        ungetc( *pword, fp );
      *pword = '\0';
      return word;
    }
  }

  log( "BUG: Fread_word: word too long." );
  exit( 1 );
  return NULL;
}

/*
 * Generate a percentile roll.
 */

int number_percent( void )
{
  int percent;

  while ( (percent = number_mm() & (128-1) ) > 99 )
    ;

  return 1 + percent;
}

/*
 * Generate a random door.
 */

int number_door( void )
{
  int door;

  while ( ( door = number_mm() & (8-1) ) > 5)
    ;

  return door;
}

long number_mm( void )
{
  return random() >> 6;
}

/*
 * Simple linear interpolation.
 */
int interpolate( int level, int value_00, int value_32 )
{
  return value_00 + level * (value_32 - value_00) / 32;
}

/*
 * Compare strings, case insensitive, for prefix matching.
 * Return TRUE if astr not a prefix of bstr
 *   (compatibility with historical functions).
 */
bool str_prefix( const char *astr, const char *bstr )
{
  if ( astr == NULL )
  {
    log( "BUG:str_prefix: null astr." );
    return TRUE;
  }

  if ( bstr == NULL )
  {
    log( "BUG:str_prefix: null bstr." );
    return TRUE;
  }

  for ( ; *astr; astr++, bstr++ )
  {
    if ( LOWER(*astr) != LOWER(*bstr) )
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

void load_default_config( void )
{
  /****************************************************************************/
  /** This function is called only once, at boot-time.                       **/
  /** - We assume config_info is empty                          -- Welcor    **/
  /****************************************************************************/
  /********************************t********************************************/
  /** Game play options.                                                     **/
  /****************************************************************************/
  CONFIG_PK_ALLOWED 	        = pk_allowed;
  CONFIG_PT_ALLOWED             = pt_allowed;
  CONFIG_LEVEL_CAN_SHOUT 	= level_can_shout;
  CONFIG_HOLLER_MOVE_COST 	= holler_move_cost;
  CONFIG_TUNNEL_SIZE 	        = tunnel_size;
  CONFIG_MAX_EXP_GAIN	        = max_exp_gain;
  CONFIG_MAX_EXP_LOSS 	        = max_exp_loss;
  CONFIG_MAX_NPC_CORPSE_TIME    = max_npc_corpse_time;
  CONFIG_MAX_PC_CORPSE_TIME	= max_pc_corpse_time;
  CONFIG_IDLE_VOID		= idle_void;
  CONFIG_IDLE_RENT_TIME	        = idle_rent_time;
  CONFIG_IDLE_MAX_LEVEL	        = idle_max_level;
  CONFIG_DTS_ARE_DUMPS	        = dts_are_dumps;
  CONFIG_LOAD_INVENTORY         = load_into_inventory;
  CONFIG_OK			= strdup(OK);
  CONFIG_NOPERSON		= strdup(NOPERSON);
  CONFIG_NOEFFECT		= strdup(NOEFFECT);
  CONFIG_TRACK_T_DOORS          = track_through_doors;
  CONFIG_IMMORT_LEVEL_OK	= immort_level_ok;

  /****************************************************************************/
  /** Rent / crashsave options.                                              **/
  /****************************************************************************/
  CONFIG_FREE_RENT              = free_rent;
  CONFIG_MAX_OBJ_SAVE           = max_obj_save;
  CONFIG_MIN_RENT_COST	        = min_rent_cost;
  CONFIG_AUTO_SAVE		= auto_save;
  CONFIG_AUTOSAVE_TIME	        = autosave_time;
  CONFIG_CRASH_TIMEOUT          = crash_file_timeout;
  CONFIG_RENT_TIMEOUT	        = rent_file_timeout;

  /****************************************************************************/
  /** Room numbers.                                                          **/
  /****************************************************************************/
  CONFIG_MORTAL_START           = world_vnum[mortal_start_room];
  CONFIG_IMMORTAL_START         = world_vnum[immort_start_room];
  CONFIG_FROZEN_START           = world_vnum[frozen_start_room];
  CONFIG_DON_ROOM_1             = world_vnum[donation_room_1];
  CONFIG_DON_ROOM_2             = world_vnum[donation_room_2];
  CONFIG_DON_ROOM_3             = world_vnum[donation_room_3];

  /****************************************************************************/
  /** Game operation options.                                                **/
  /****************************************************************************/
  CONFIG_DFLT_PORT              = DFLT_PORT;

  if (DFLT_IP)
    CONFIG_DFLT_IP              = strdup(DFLT_IP);
  else
    CONFIG_DFLT_IP              = NULL;

  CONFIG_DFLT_DIR               = strdup(DFLT_DIR);

  if (LOGNAME)
    CONFIG_LOGNAME              = strdup(LOGNAME);
  else
    CONFIG_LOGNAME              = NULL;

  CONFIG_MAX_PLAYING            = max_playing;
  CONFIG_MAX_FILESIZE           = max_filesize;
  CONFIG_MAX_BAD_PWS            = max_bad_pws;
  CONFIG_SITEOK_ALL             = siteok_everyone;
  CONFIG_NS_IS_SLOW             = nameserver_is_slow;
  CONFIG_NEW_SOCIALS            = use_new_socials;
  CONFIG_OLC_SAVE               = auto_save_olc;
  CONFIG_MENU                   = strdup(MENU);
  CONFIG_WELC_MESSG             = strdup(WELC_MESSG);
  CONFIG_START_MESSG            = strdup(START_MESSG);

  /****************************************************************************/
  /** Autowiz options.                                                       **/
  /****************************************************************************/
  CONFIG_USE_AUTOWIZ            = use_autowiz;
  CONFIG_MIN_WIZLIST_LEV        = min_wizlist_lev;
}

void load_config( void )
{
  FILE *fl;
  char line[MAX_STRING_LENGTH];
  char tag[MAX_INPUT_LENGTH];
  int  num;
  char buf[MAX_INPUT_LENGTH];

  load_default_config();

  snprintf(buf, sizeof(buf), "%s/%s", DFLT_DIR, CONFIG_CONFFILE);
  if ( !(fl = fopen(CONFIG_CONFFILE, "r")) && !(fl = fopen(buf, "r")) )
  {
    snprintf(buf, sizeof(buf), "Game Config File: %s", CONFIG_CONFFILE);
    perror(buf);
    return;
  }

  /****************************************************************************/
  /** Load the game configuration file.                                      **/
  /****************************************************************************/
  while (get_line(fl, line))
  {
    split_argument(line, tag);
    num = atoi(line);

    switch (LOWER(*tag))
    {
    case 'a':
      if (!strcmp(tag, "auto_save"))
        CONFIG_AUTO_SAVE = num;
      else if (!strcmp(tag, "autosave_time"))
        CONFIG_AUTOSAVE_TIME = num;
      else if (!strcmp(tag, "auto_save_olc"))
        CONFIG_OLC_SAVE = num;
      break;

    case 'c':
      if (!strcmp(tag, "crash_file_timeout"))
        CONFIG_CRASH_TIMEOUT = num;
      break;

    case 'd':
      if (!strcmp(tag, "dts_are_dumps"))
        CONFIG_DTS_ARE_DUMPS = num;
      else if (!strcmp(tag, "donation_room_1"))
        if (num == -1)
          config_info.room_nums.donation_room_1 = 0;
        else
          config_info.room_nums.donation_room_1 = num;
      else if (!strcmp(tag, "donation_room_2"))
        if (num == -1)
          config_info.room_nums.donation_room_2 = 0;
        else
          config_info.room_nums.donation_room_2 = num;
      else if (!strcmp(tag, "donation_room_3"))
        if (num == -1)
          config_info.room_nums.donation_room_3 = 0;
        else
          config_info.room_nums.donation_room_3 = num;
      else if (!strcmp(tag, "dflt_dir"))
      {
        if (CONFIG_DFLT_DIR)
          free(CONFIG_DFLT_DIR);
        if (line && *line)
          CONFIG_DFLT_DIR = strdup(line);
        else
          CONFIG_DFLT_DIR = strdup(DFLT_DIR);
      }
      else if (!strcmp(tag, "dflt_ip"))
      {
        if (CONFIG_DFLT_IP)
          free(CONFIG_DFLT_IP);
        if (line && *line)
          CONFIG_DFLT_IP = strdup(line);
        else
          CONFIG_DFLT_IP = NULL;
      }
      else if (!strcmp(tag, "dflt_port"))
        CONFIG_DFLT_PORT = num;
      break;

    case 'f':
      if (!strcmp(tag, "free_rent"))
        CONFIG_FREE_RENT = num;
      else if (!strcmp(tag, "frozen_start_room"))
        config_info.room_nums.frozen_start_room = num;
      break;

    case 'h':
      if (!strcmp(tag, "holler_move_cost"))
        CONFIG_HOLLER_MOVE_COST = num;
      break;

    case 'i':
      if (!strcmp(tag, "idle_void"))
        CONFIG_IDLE_VOID = num;
      else if (!strcmp(tag, "idle_rent_time"))
        CONFIG_IDLE_RENT_TIME = num;
      else if (!strcmp(tag, "idle_max_level"))
        CONFIG_IDLE_MAX_LEVEL = num;
      else if (!strcmp(tag, "immort_level_ok"))
        CONFIG_IMMORT_LEVEL_OK = num;
      else if (!strcmp(tag, "immort_start_room"))
        config_info.room_nums.immort_start_room = num;
      break;

    case 'l':
      if (!strcmp(tag, "level_can_shout"))
        CONFIG_LEVEL_CAN_SHOUT = num;
      else if (!strcmp(tag, "load_into_inventory"))
        CONFIG_LOAD_INVENTORY = num;
      else if (!strcmp(tag, "logname"))
      {
        if (CONFIG_LOGNAME)
          free(CONFIG_LOGNAME);
        if (line && *line)
          CONFIG_LOGNAME = strdup(line);
        else
          CONFIG_LOGNAME = NULL;
      }
      break;

    case 'm':
      if (!strcmp(tag, "max_bad_pws"))
        CONFIG_MAX_BAD_PWS = num;
      else if (!strcmp(tag, "max_exp_gain"))
        CONFIG_MAX_EXP_GAIN = num;
      else if (!strcmp(tag, "max_exp_loss"))
        CONFIG_MAX_EXP_LOSS = num;
      else if (!strcmp(tag, "max_filesize"))
        CONFIG_MAX_FILESIZE = num;
      else if (!strcmp(tag, "max_npc_corpse_time"))
        CONFIG_MAX_NPC_CORPSE_TIME = num;
      else if (!strcmp(tag, "max_obj_save"))
        CONFIG_MAX_OBJ_SAVE = num;
      else if (!strcmp(tag, "max_pc_corpse_time"))
        CONFIG_MAX_PC_CORPSE_TIME = num;
      else if (!strcmp(tag, "max_playing"))
        CONFIG_MAX_PLAYING = num;
      else if (!strcmp(tag, "menu"))
      {
        if (CONFIG_MENU)
          free(CONFIG_MENU);
        strncpy(buf, "Reading menu in load_config()", sizeof(buf));
        CONFIG_MENU = fread_string(fl, buf);
      }
      else if (!strcmp(tag, "min_rent_cost"))
        CONFIG_MIN_RENT_COST = num;
      else if (!strcmp(tag, "min_wizlist_lev"))
        CONFIG_MIN_WIZLIST_LEV = num;
      else if (!strcmp(tag, "mortal_start_room"))
        config_info.room_nums.mortal_start_room = num;
      break;

    case 'n':
      if (!strcmp(tag, "nameserver_is_slow"))
        CONFIG_NS_IS_SLOW = num;
      else if (!strcmp(tag, "noperson"))
      {
        char tmp[READ_SIZE];
        if (CONFIG_NOPERSON)
          free(CONFIG_NOPERSON);
        snprintf(tmp, sizeof(tmp), "%s\r\n", line);
        CONFIG_NOPERSON = strdup(tmp);
      }
      else if (!strcmp(tag, "noeffect"))
      {
        char tmp[READ_SIZE];
        if (CONFIG_NOEFFECT)
          free(CONFIG_NOEFFECT);
        snprintf(tmp, sizeof(tmp), "%s\r\n", line);
        CONFIG_NOEFFECT = strdup(tmp);
      }
      break;

    case 'o':
      if (!strcmp(tag, "ok"))
      {
        char tmp[READ_SIZE];
        if (CONFIG_OK)
          free(CONFIG_OK);
        snprintf(tmp, sizeof(tmp), "%s\r\n", line);
        CONFIG_OK = strdup(tmp);
      }
      break;

    case 'p':
      if (!strcmp(tag, "pk_allowed"))
        CONFIG_PK_ALLOWED = num;
      else if (!strcmp(tag, "pt_allowed"))
        CONFIG_PT_ALLOWED = num;
      break;

    case 'r':
      if (!strcmp(tag, "rent_file_timeout"))
        CONFIG_RENT_TIMEOUT = num;
      break;

    case 's':
      if (!strcmp(tag, "siteok_everyone"))
        CONFIG_SITEOK_ALL = num;
      else if (!strcmp(tag, "start_messg"))
      {
        strncpy(buf, "Reading start message in load_config()", sizeof(buf));
        if (CONFIG_START_MESSG)
          free(CONFIG_START_MESSG);
        CONFIG_START_MESSG = fread_string(fl, buf);
      }
      break;

    case 't':
      if (!strcmp(tag, "tunnel_size"))
        CONFIG_TUNNEL_SIZE = num;
      else if (!strcmp(tag, "track_through_doors"))
        CONFIG_TRACK_T_DOORS = num;
      break;

    case 'u':
      if (!strcmp(tag, "use_autowiz"))
        CONFIG_USE_AUTOWIZ = num;
      else if (!strcmp(tag, "use_new_socials"))
        CONFIG_NEW_SOCIALS = num;
      break;

    case 'w':
      if (!strcmp(tag, "welc_messg"))
      {
        strncpy(buf, "Reading welcome message in load_config()", sizeof(buf));
        if (CONFIG_WELC_MESSG)
          free(CONFIG_WELC_MESSG);
        CONFIG_WELC_MESSG = fread_string(fl, buf);
      }
      break;

    default:
      break;
    }
  }

  fclose(fl);
}

/* returns the real number of the zone with given virtual number */
zone_rnum real_zone(zone_vnum vnum)
{
#if 0
  zone_rnum bot, top, mid;

  bot = 0;
  top = top_of_zone_table;

  /* perform binary search on zone-table */
  for (;;)
  {
    mid = (bot + top) / 2;

    if ((zone_table + mid)->number == vnum)
      return (mid);
    if (bot >= top)
      return (NOWHERE);
    if ((zone_table + mid)->number > vnum)
      top = mid - 1;
    else
      bot = mid + 1;
  }
#else
  zone_rnum t;
  for (t = 0; t <= top_of_zone_table; t++)
    if (vnum == zone_table[t].number)
      return t;
  return NOWHERE;
#endif
}

int valid_id_num(long id)
{
  CHAR_DATA *tch;
  DESCRIPTOR_DATA *d;
  for (tch = character_list; tch; tch = tch->next)
    if (GET_ID(tch) == id)
      return 0;
  for (d = descriptor_list; d; d = d->next)
    if (d->character && GET_ID(d->character) == id)
      return 0;
  return 1;
}
int valid_to_save(char *name) {
int tp;
for (tp = 0; tp <= top_of_p_table; tp++)
  {
    if (!IS_SET(player_table[tp].flags, PINDEX_DELETED) &&
        !IS_SET(player_table[tp].flags, PINDEX_SELFDELETE) &&
        (!player_table[tp].name || !*player_table[tp].name))
      continue;
    if (*player_table[tp].name == *name && !strcmp(player_table[tp].name, name))
    {
      return 1;
    }
  }
return 0;
}


