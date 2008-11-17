
/* ************************************************************************
*   File: utils.h                                       Part of CircleMUD *
*  Usage: header file: utility macros and prototypes of utility funcs     *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hoins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */


/* external declarations and prototypes **********************************/

/* new define for quick check */
#define DEAD(ch) (PLR_FLAGGED((ch), PLR_NOTDEADYET) || MOB_FLAGGED((ch), MOB_NOTDEADYET))

#define LANA(string) (strchr("aeiouyAEIOUY", string[0]) ? "an" : "a")
#define CANA(string) (strchr("aeiouyAEIOUY", string[0]) ? "An" : "A")
// make a string on-the-fly

extern FILE *logfile;
extern FILE *comfile;
extern int syslogfd;
#define MAX_FDS 64

#define log			basic_mud_log

#define READ_SIZE	256

#define PAGE_LENGTH     30
#define PAGE_WIDTH      80

#define GET_POINTS_EVENT(ch, i) ((ch)->pts_event[i])
#define GET_FIGHT_EVENT(ch)   ((ch)->fight_event)
#define GET_TASK(ch)    ((ch)->task)
#define GET_TASK_NUM(ch)	((ch)->on_task)
#define GET_ACC(ch)  (pi.GetAccById(GET_IDNUM((ch))))

#define IS_ROGUE(chclass) (chclass == CLASS_THIEF || chclass == CLASS_GYPSY || chclass == CLASS_RANGER)
#define IS_FIGHTER(chclass) (chclass == CLASS_WARRIOR || chclass == CLASS_HUNTER)
#define IS_CASTER(chclass) (chclass == CLASS_MAGE || chclass == CLASS_ESPER || chclass == CLASS_PRIEST)

#define IS_VALID(data)          ((data) != NULL && (data)->valid)
#define VALIDATE(data)          ((data)->valid = TRUE)
#define INVALIDATE(data) 	((data)->valid = FALSE)
#define UMAX(a, b)               ((a) > (b) ? (a) : (b))



void free_string ( char **pt );
char *numlineas ( char *string );
void string_append ( Character *ch, char **pString );

struct time_info_data *mud_time_passed ( time_t t2, time_t t1 );
const char *race_name ( Character *ch );
int has_weapon ( Character *ch );
float has_staff ( Character *ch );
int num_dice_wep ( Character *ch, short dual );
int size_dice_wep ( Character *ch, short dual );
int highest_tier ( Character *ch );
float race_dam_mod ( int race, int magic );

void strip_colour ( char *inbuf, size_t i_buf );
size_t proc_colour ( char *inbuf, int color_lvl, size_t len );
extern int sub_success;
int total_chance ( Character *ch, int skill );
gold_int exp_needed ( Character *ch );
gold_int group_exp_needed ( Character *ch );
gold_int level_exp ( int chclass, int level, int tier, int remorts );
int use_stamina ( Character *ch, int amount );

#define divide_2(a)  (a>>1)
#define mult_2(a)     (a<<1)

#define IS_UNIQUE(obj) (IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))

#define IS_ELEMENTAL(ch) ((MOB_FLAGGED(ch, MOB_ELEM_EARTH)|| (MOB_FLAGGED(ch, MOB_ELEM_AIR))||(MOB_FLAGGED(ch, MOB_ELEM_WATER))||(MOB_FLAGGED(ch, MOB_ELEM_FIRE))))

/* strings */

#define MXP_BEG "\x03"    /* becomes < */
#define MXP_END "\x04"    /* becomes > */
#define MXP_AMP "\x05"    /* becomes & */

/* characters */

#define MXP_BEGc '\x03'    /* becomes < */
#define MXP_ENDc '\x04'    /* becomes > */
#define MXP_AMPc '\x05'    /* becomes & */

/* constructs an MXP tag with < and > around it */

#define MXPTAG(arg) MXP_BEG arg MXP_END

#define ESC "\x1B"  /* esc character */

#define MXPMODE(arg) ESC "[" #arg "z"

/* flags for show_list_to_char */

enum
{
	eItemNothing,   /* item is not readily accessible */
	eItemGet,     /* item on ground */
	eItemDrop,    /* item in inventory */
	eItemBid     /* auction item */
};

/*replace parts of strings*/
char *strrepl ( char *Str, size_t BufSiz, char *OldStr, char *NewStr );
/*string concat for multiple strings*/
char *xstrcat ( char *des, char *src, ... );
/* th, st, nd, rd - for numbers */
const char *ordinal_text ( int number );
char *str_str ( char *cs, char *ct );
char *str_str ( char *cs, const char *ct );

#if 0
/*quicksort a linked list*/
void    *sortl ( void *list, void * ( *getnext ) ( void * ),
                 void ( *setnext ) ( void *, void * ),
                 int ( *compare ) ( void *, void * ) );    /* Ll_Qsort.C     */

typedef struct list_struct
{
	struct list_struct *next;
	char *key;
	/* other stuff */
}
list;

list *lsort ( list *p );
#endif

/*comma format a long*/
size_t commafmt ( char   *buf,          /* Buffer for formatted string  */
                  size_t     bufsize,        /* Size of buffer               */
                  gold_int    N );             /* Number to convert            */

/* public functions in utils.c */
bool is_prefix ( const char *aStr, const char *bStr );
bool compares ( const char *aStr, const char *bStr );
char *str_dup ( const char *source );
/*
 * Only provide our versions if one isn't in the C library. These macro names
 * will be defined by sysdep.h if a strcasecmp or stricmp exists.
 */
#ifndef str_cmp
int	str_cmp ( const char *arg1, const char *arg2 );
#endif
#ifndef strn_cmp
int	strn_cmp ( const char *arg1, const char *arg2, int n );
#endif
void comlog ( const char *format, ... )   __attribute__ ( ( format ( printf, 1, 2 ) ) );
void basic_mud_log ( const char *format, ... )   __attribute__ ( ( format ( printf, 1, 2 ) ) );
int touch ( const char *path );
void mudlog ( const char *str, int type, int level, int file );
void    new_mudlog ( int type, int level, int file, const char *str, ... ) __attribute__ ( ( format ( printf, 4, 5 ) ) );
void log_death_trap ( Character *ch );
int number ( int from, int to );
float number ( float from, float to );
int dice ( int number, int size );
size_t new_sprintbit ( bitvector_t vektor, const char *names[], char *result,
                       size_t reslen );
size_t new_sprinttype ( int type, const char *names[], char *result,
                        size_t reslen );
void sprintbit ( bitvector_t vektor, const char *names[], char *result, size_t r_len );
void sprinttype ( int type, const char *names[], char *result, size_t r_len );
int get_line ( FILE * fl, char *buf );
int get_line ( ifstream &fl, char *buf );
int get_filename ( const char *orig_name, char *filename, int mode );
void sprintbitarray ( int bitvector[], const char *name[], int maxar,
                      char *result, size_t r_len );
time_t mud_time_to_secs ( struct time_info_data *now );
struct time_info_data *age ( Character *ch );
int num_pc_in_room ( Room *room );
void line_input ( Descriptor *d, const char *prompt,
                  C_FUNC ( *callback ), void *info );
void core_dump_real ( const char *, int );
char *stristr ( const char *String, const char *Pattern );

char *stripcr ( char *dest, const char *src );
int room_is_dark ( room_rnum room );
time_t sec_to_time ( int sec );
long time_to_sec ( time_t timeCheck );
char *center_align ( char *str, size_t width );
#define HOURS_TO_EXPIRE(num) ((time_t)(time(0) + ((num) * SECS_PER_MUD_HOUR)))

void basic_mud_vlog ( const char *format, va_list args );



#define core_dump()		core_dump_real(__FILE__, __LINE__)


// m0rd
char* print_gold ( char* result, gold_int gold );
int alter_gold ( Character *ch, gold_int amount );
void alter_stamina ( Character *ch, int amount );
int speed_update ( Character *ch );

//attack functions
int accuracy_tot ( Character *attacker );
int evasion_tot ( Character *vict );

/* random functions in random.c */
void circle_srandom ( unsigned long initial_seed );
unsigned long circle_random ( void );

/* undefine MAX and MIN so that our functions are used instead */
#ifdef MAX
#undef MAX
#endif

#ifdef MIN
#undef MIN
#endif

inline gold_int MIN ( gold_int a, gold_int b )
{
	return ( a < b ? a : b );
}
inline gold_int MAX ( gold_int a, gold_int b )
{
	return ( a > b ? a : b );
}
inline int MIN ( int a, int b )
{
	return ( a < b ? a : b );
}
inline int MAX ( int a, int b )
{
	return ( a > b ? a : b );
}
inline float MIN ( float a, float b )
{
	return ( a < b ? a : b );
}
inline float MAX ( float a, float b )
{
	return ( a > b ? a : b );
}
inline double MIN ( double a, double b )
{
	return ( a < b ? a : b );
}
inline double MAX ( double a, double b )
{
	return ( a > b ? a : b );
}
inline unsigned int MIN ( unsigned int a, unsigned int b )
{
	return ( a < b ? a : b );
}
inline unsigned int MAX ( unsigned int a, unsigned int b )
{
	return ( a > b ? a : b );
}
inline long MIN ( long a, long b )
{
	return ( a < b ? a : b );
}
inline long MAX ( long a, long b )
{
	return ( a > b ? a : b );
}



/* in class.c */
int class_elem_weakness ( int chcl );
int class_elem_strength ( int chcl );

/* in magic.c */
bool circle_follow ( Character *ch, Character *victim );

/* in act.informative.c */
void look_at_room ( Character *ch, int mode );
Character *rand_group ( Character *ch );

/* in act.movmement.c */
int do_simple_obj_move ( struct obj_data *obj, int dir, Character *ch );
int do_simple_move ( Character *ch, int dir, int following );
int perform_move ( Character *ch, int dir, int following );

/* in limits.c */
int mana_gain ( Character *ch );
int hit_gain ( Character *ch );
int move_gain ( Character *ch );
int stamina_gain ( Character *ch );
void advance_level ( Character *ch, bool silent = false );
void set_title ( Character *ch, char *title );
void set_pretitle ( Character *ch, char *title );
void gain_exp ( Character *ch, gold_int gain );
void gain_exp_regardless ( Character *ch, gold_int gain, bool silent = false );
void gain_condition ( Character *ch, int condition, int value );
void check_idling ( Character *ch );
void regen_update ( void );
void point_update ( void );
void update_pos ( Character *victim );
void total_perc ( Character *ch );
int group_size ( Character *ch );
void set_loginmsg ( Character *ch, char *loginmsg );   /* EDIT BY THOTTER!!!*/
void set_logoutmsg ( Character *ch, char *logoutmsg ); /* ^^^^^^^^^^^^*/

/* in comm.c */
void    circle_exit ( int r );
#define exit(r)  circle_exit(r)

extern const struct race_data races[NUM_RACES];

#define IS_PLAYING(d)   (STATE(d) == CON_TEDIT || STATE(d) == CON_REDIT ||      \
                        STATE(d) == CON_MEDIT || STATE(d) == CON_OEDIT ||       \
                        STATE(d) == CON_ZEDIT || STATE(d) == CON_SEDIT ||       \
                        STATE(d) == CON_CEDIT || STATE(d) == CON_PLAYING ||     \
                        STATE(d) == CON_AEDIT || STATE(d) == CON_TRIGEDIT ||    \
			STATE(d) == CON_LINE_INPUT || STATE(d) == CON_HEDIT)


/* various constants *****************************************************/
#define IRANGE(a, b, c)          ((b) < (a) ? (a) : ((b) > (c) ? (c) : (b)))

/* defines for mudlog() */
#define OFF	0
#define BRF	1
#define NRM	2
#define CMP	3

/* get_filename() */
#define CRASH_FILE		0
#define ETEXT_FILE		1
#define ALIAS_FILE      	2
#define POOF_FILE		3
#define NEW_OBJ_FILES   	4
#define SCRIPT_VARS_FILE	5
#define IGNORE_FILE		6
#define ASCII_OBJ_FILES         7
#define LOCKER_FILES             8

/* breadth-first searching */
#define BFS_ERROR		(-1)
#define BFS_ALREADY_THERE	(-2)
#define BFS_NO_PATH		(-3)

/*
 * XXX: These constants should be configurable. See act.informative.c
 *	and utils.c for other places to change.
 */
/* mud-life time */
#define SECS_PER_MUD_HOUR	72
#define SECS_PER_MUD_DAY	(24*SECS_PER_MUD_HOUR)
#define SECS_PER_MUD_MONTH	(35*SECS_PER_MUD_DAY)
#define SECS_PER_MUD_YEAR	(17*SECS_PER_MUD_MONTH)

/* real-life time (remember Real Life?) */
#define SECS_PER_REAL_MIN	60
#define SECS_PER_REAL_HOUR	(60*SECS_PER_REAL_MIN)
#define SECS_PER_REAL_DAY	(24*SECS_PER_REAL_HOUR)
#define SECS_PER_REAL_YEAR	(365*SECS_PER_REAL_DAY)


/* string utils **********************************************************/


#define YESNO(a) ((a) ? "YES" : "NO")
#define ONOFF(a) ((a) ? "ON" : "OFF")
#if !defined(LOWER)
#define LOWER(c)   (((c)>='A'  && (c) <= 'Z') ? ((c)+('a'-'A')) : (c))
#endif
#define UPPER(c)   (((c)>='a'  && (c) <= 'z') ? ((c)+('A'-'a')) : (c) )

#define ISNEWL(ch) ((ch) == '\n' || (ch) == '\r')
#define IF_STR(st) ((st) ? (st) : "\0")

#define AN(string) (strchr("aeiouAEIOU", *string) ? "an" : "a")

inline char *CAP ( char *txt )
{
	*txt = UPPER ( *txt );
	return ( txt );
}

/* memory utils **********************************************************/
#define CREATE(result, type, number)  do {\
	if ((number) * sizeof(type) <= 0)	\
		log("SYSERR: Zero bytes or less requested at %s:%d.", __FILE__, __LINE__);	\
	if (!((result) = (type *) calloc ((number), sizeof(type))))	\
		{ perror("SYSERR: calloc failure"); abort(); } } while(0)

#define RECREATE(result,type,number) do {\
  if (!((result) = (type *) realloc ((result), sizeof(type) * (number))))\
		{ perror("SYSERR: realloc failure"); abort(); } } while(0)



/* Create a duplicate of a string */
inline char *str_dup ( const char *source )
{
	char *new_z = NULL;

	CREATE ( new_z, char, strlen ( source ) + 1 );
	return ( strcpy ( new_z, source ) );
}

/* Dynamic string buffer macros. */


#define DYN_DEFINE \
     char* dynbuf = 0; \
     size_t dynbuf_size = 0;


#define DYN_CREATE \
     dynbuf_size = MAX_STRING_LENGTH; \
     CREATE(dynbuf, char, dynbuf_size); \
 

#define DYN_RESIZE(sbuf) \
     if (strlen(dynbuf) + strlen((sbuf)) >= dynbuf_size) { \
       dynbuf_size += MAX_STRING_LENGTH > strlen((sbuf)) ? MAX_STRING_LENGTH : strlen((sbuf)) + 1; \
       RECREATE(dynbuf, char, dynbuf_size); \
     } \
     strcat(dynbuf, (sbuf));

/*
 * the source previously used the same code in many places to remove an item
 * from a list: if it's the list head, change the head, else traverse the
 * list looking for the item before the one to be removed.  Now, we have a
 * macro to do this.  To use, just make sure that there is a variable 'temp'
 * declared as the same type as the list to be manipulated.  BTW, this is
 * a great application for C++ templates but, alas, this is not C++.  Maybe
 * CircleMUD 4.0 will be...
 */
#define REMOVE_FROM_LIST(item, head, next)	\
   if ((item) == (head))		\
      head = (item)->next;		\
   else {				\
      temp = head;			\
      while (temp && (temp->next != (item))) \
	 temp = temp->next;		\
      if (temp)				\
         temp->next = (item)->next;	\
   }					\
 
int get_sub(Character *ch, int i);
void improve_sub ( Character *ch, enum subskill_list sub, int amount );

#define GET_SUB(ch, i)	get_sub(ch, i)
void set_skill ( Character *ch, int skill, int amount );
/* basic bitvector utils *************************************************/

#define Q_FIELD(x)  ((int) (x) / 32)
#define Q_BIT(x)    (1 << ((x) % 32))

#define IS_SET_AR(var, bit)       ((var)[Q_FIELD(bit)] & Q_BIT(bit))
#define SET_BIT_AR(var, bit)      ((var)[Q_FIELD(bit)] |= Q_BIT(bit))
#define REMOVE_BIT_AR(var, bit)   ((var)[Q_FIELD(bit)] &= ~Q_BIT(bit))
#define TOGGLE_BIT_AR(var, bit)   ((var)[Q_FIELD(bit)] ^= Q_BIT(bit))


#if !defined(IS_SET)
#define IS_SET(flag,bit)  ((flag) & (bit))
#endif
#if !defined(SET_BIT)
#define SET_BIT(var,bit)  ((var) |= (bit))
#endif
#if !defined(REMOVE_BIT)
#define REMOVE_BIT(var,bit)  ((var) &= ~(bit))
#endif
#if !defined(TOGGLE_BIT)
#define TOGGLE_BIT(var,bit) ((var) ^= (bit))
#endif

/*
 * Accessing player specific data structures on a mobile is a very bad thing
 * to do.  Consider that changing these variables for a single mob will change
 * it for every other single mob in the game.  If we didn't specifically check
 * for it, 'wimpy' would be an extremely bad thing for a mob to do, as an
 * example.  If you really couldn't care less, change this to a '#if 0'.
 */
#if 0
/* Subtle bug in the '#var', but works well for now. */
#define CHECK_PLAYER_SPECIAL(ch, var) \
	(*(((ch)->player_specials == &dummy_mob) ? (log("SYSERR: Mob using '"#var"' at %s:%d.", __FILE__, __LINE__), &(var)) : &(var)))
#else
#define CHECK_PLAYER_SPECIAL(ch, var)	(var)
#endif

#define ATK_CHANCE(ch) ((ch)->atk)
#define IMMUNE(ch) ((ch)->char_specials.saved.immune)
#define RESIST_VAL(ch, i) ((ch)->char_specials.saved.resist_val)
#define RESIST_FLAGS(ch) ((ch)->char_specials.saved.resist)
#define MOB_FLAGS(ch) ((ch)->char_specials.saved.act)
#define PLR_FLAGS(ch) ((ch)->char_specials.saved.act)
#define CMD_FLAGS(ch) ((ch)->player_specials->saved.cmd)
#define CMD_FLAGS2(ch) ((ch)->cmd2)
#define PRF_FLAGS(ch) CHECK_PLAYER_SPECIAL((ch), ((ch)->player_specials->saved.pref))
#define AFF_FLAGS(ch) ((ch)->char_specials.saved.affected_by)
#define ROOM_FLAGS(loc) ((loc)->room_flags)
#define ZONE_FLAGS(loc) (zone_table[(loc)].zone_flags)
#define DESC_FLAGS(d)	((d)->options)
#define SPELL_ROUTINES(spl)   (spell_info[spl].routines)

#define IS_NPC(ch)  (IS_SET_AR(MOB_FLAGS(ch), MOB_ISNPC))
#define IS_MOB(ch)  (IS_NPC(ch) && !(ch)->proto)

#define IS_IMP(ch)	(GET_LEVEL(ch) == LVL_IMPL)
#define IS_IMM(ch)	(GET_LEVEL(ch) >= LVL_IMMORT)

#define MOB_FLAGGED(ch, flag) (IS_NPC(ch) && IS_SET_AR(MOB_FLAGS(ch), (flag)))
#define PLR_FLAGGED(ch, flag) (!IS_NPC(ch) && IS_SET_AR(PLR_FLAGS(ch), (flag)))
#define AFF_FLAGGED(ch, flag) (IS_SET_AR(AFF_FLAGS(ch), (flag)))
#define PRF_FLAGGED(ch, flag) (IS_SET_AR(PRF_FLAGS(ch), (flag)))
#define CMD_FLAGGED(ch, flag) (IS_SET(CMD_FLAGS(ch), (flag)))
#define CMD_FLAGGED2(ch, flag) (IS_SET(CMD_FLAGS2(ch), (flag)))
#define ROOM_FLAGGED(loc, flag) (IS_SET_AR(ROOM_FLAGS(loc), (flag)))
#define ZONE_FLAGGED(loc, flag) (IS_SET(ZONE_FLAGS(loc), (flag)))
#define SECTOR(loc) ((loc)->sector_type)
int has_body ( Character *ch, int flag );
#define HAS_BODY(ch, flag) (has_body(ch, flag))
#define EXIT_FLAGGED(exit, flag) (IS_SET((exit)->exit_info, (flag)))
#define OBJAFF_FLAGGED(obj, flag) (IS_SET(GET_OBJ_AFFECT(obj), (flag)))
#define OBJVAL_FLAGGED(obj, flag) (IS_SET(GET_OBJ_VAL((obj), 1), (flag)))
#define OBJWEAR_FLAGGED(obj, flag) (IS_SET_AR((obj)->obj_flags.wear_flags, (flag)))
#define OBJ_FLAGGED(obj, flag) (IS_SET_AR(GET_OBJ_EXTRA(obj), (flag)))
#define DESC_FLAGGED(d, flag) (IS_SET(DESC_FLAGS(d), (flag)))
#define HAS_SPELL_ROUTINE(spl, flag) (IS_SET(SPELL_ROUTINES(spl), (flag)))

#define IS_BURIED(obj)	(IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_BURIED))
#define IS_HIDDEN(obj)	(IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_HIDDEN))

/* IS_AFFECTED for backwards compatibility */
#define IS_AFFECTED(ch, skill) (AFF_FLAGGED((ch), (skill)))

#define PLR_TOG_CHK(ch,flag) ((TOGGLE_BIT_AR(PLR_FLAGS(ch), (flag))) && \
				  (IS_SET_AR(PLR_FLAGS(ch), (flag))))
#define PRF_TOG_CHK(ch,flag) ((TOGGLE_BIT_AR(PRF_FLAGS(ch), (flag))) && \
				  (IS_SET_AR(PRF_FLAGS(ch), (flag))))

#define INTERNAL(ch)	((ch)->internal_flags)

/* room utils ************************************************************/
#define GET_ROOM_ZONE(room)	((room)->zone)
#define ROOM_OWNER(room)	(zone_table[GET_ROOM_ZONE(room)].owner)

#define VALID_ROOM_RNUM(rnum)   ((rnum) != NULL)


#define SECT(room)	(VALID_ROOM_RNUM(room) ? \
				(room)->sector_type : SECT_INSIDE)

#define IS_DARK(room) room_is_dark((room))

#define IS_DARKer(room)  ( !room->light && \
                         (ROOM_FLAGGED(room, ROOM_DARK) || \
                          ( ( SECT(room) != SECT_INSIDE && \
                              SECT(room) != SECT_CITY ) && \
                            (sunlight == SUN_SET || \
			     sunlight == SUN_DARK)) ) )

#define IS_LIGHT(room)  (!IS_DARK(room))

#define VALID_RNUM(rnum)      VALID_ROOM_RNUM((rnum))
#define GET_ROOM_VNUM(rnum) \
      ((room_vnum)(VALID_RNUM(rnum) ? (rnum)->number : NOWHERE))
#define GET_ROOM_SPEC(room) (VALID_RNUM(room) ? (room)->func : NULL)

/* char utils ************************************************************/

#define GET_NEXT_SKILL(ch) ((ch)->combatskill.w_type)
#define GET_NEXT_VICTIM(ch) ((ch)->combatskill.vict_id)
#define GET_SWEEP_DAM(ch) ((ch)->sweep_damage)
#define IN_ROOM(ch)	((ch)->in_room)
#define GET_WAS_IN(ch)	((ch)->was_in_room)
#define GET_AGE(ch)     (age(ch)->year)


#define GET_PC_NAME(ch)	((ch)->player.name)
#define GET_NAME(ch)    (IS_NPC(ch) ? \
			 (ch)->player.short_descr : GET_PC_NAME(ch))
#define GET_TITLE(ch)   ((ch)->player.title)

#define TOROOM(room, dir) (room->dir_option[dir] ? \
			    room->dir_option[dir]->to_room : NULL)
#if 0
#define GET_PC_NAME_S(ch)	(*(ch)->player.name)
#define GET_PC_NAME_SP(ch)	((ch)->player.name)

#define GET_NAME(ch)    (IS_NPC(ch) ? \
			 *(ch)->player.short_descr : GET_PC_NAME_S(ch))
#define GET_NAME_SP(ch)    (IS_NPC(ch) ? \
			 (ch)->player.short_descr : GET_PC_NAME_SP(ch))

#define GET_TITLE_S(ch)   (*(ch)->player.title)
#define GET_TITLE_SP(ch)   ((ch)->player.title)
#endif

#define GET_LEVEL(ch)   ((ch)->player.level)
#define GET_PASSWD(ch)	((ch)->player.passwd)

/*
 * I wonder if this definition of GET_REAL_LEVEL should be the definition
 * of GET_LEVEL?  JE
 */
#define GET_REAL_LEVEL(ch) \
   (ch->desc && ch->desc->original ? GET_LEVEL(ch->desc->original) : \
    GET_LEVEL(ch))

#define GET_CLASS(ch)   ((ch)->player.chclass)
#define GET_RACE(ch)	((ch)->player.race)
/* get body is questionable, need to check this -- mord*/
#define GET_BODY(ch)	(races[(int)GET_RACE((ch))].body_bits)
#define EXTRA_BODY(ch)  ((ch)->body)
#define GET_HOME(ch)	((ch)->player.hometown)
#define GET_HEIGHT(ch)	((ch)->player.height)
#define GET_WEIGHT(ch)	((ch)->player.weight)
#define GET_SEX(ch)	((ch)->player.sex)
#define LAST_MOVE(ch)   ((ch)->last_move)


#define MAX_MORTAL_BASE 22
#define MAX_IMM_BASE    25
#define GET_STR(ch)     ((ch)->aff_abils.str)
#define GET_ADD(ch)     ((ch)->aff_abils.str_add)
#define GET_DEX(ch)     ((ch)->aff_abils.dex)
#define GET_INT(ch)     ((ch)->aff_abils.intel)
#define GET_WIS(ch)     ((ch)->aff_abils.wis)
#define GET_CON(ch)     ((ch)->aff_abils.con)
#define GET_CHA(ch)     ((ch)->aff_abils.cha)

#define CREATE_POINTS(ch) ((ch)->real_abils.points)

#define MAX_PLAYER_DAMROLL 150
#define MAX_MOB_DAMROLL    10000

#define GET_GROUP_EXP(ch) ((ch)->points.group_exp)
#define GET_EXP(ch)	  ((ch)->points.exp)
#define GET_AC(ch)        ((ch)->points.armor)
#define GET_HIT(ch)	  ((ch)->points.hit)
#define GET_MAX_HIT(ch)	  ((ch)->points.max_hit)
#define GET_MOVE(ch)	  ((ch)->points.move)
#define GET_MAX_MOVE(ch)  ((ch)->points.max_move)
#define GET_MANA(ch)	  ((ch)->points.mana)
#define GET_MAX_MANA(ch)  ((ch)->points.max_mana)
#define GET_GOLD(ch)	  ((ch)->points.gold)
#define GET_BANK_GOLD(ch) ((ch)->points.bank_gold)
#define GET_HITROLL(ch)	  ((ch)->points.hitroll)
#define GET_DAMROLL(ch)   ((ch)->points.damroll)
#define GET_STAMINA(ch)	  ((ch)->points.stamina)
#define GET_MAX_STAMINA(ch)   ((ch)->points.max_stamina)

#define GET_AMT_BET(ch) CHECK_PLAYER_SPECIAL((ch),((ch)->player_specials->saved.bet_amt))
#define GET_BETTED_ON(ch) CHECK_PLAYER_SPECIAL((ch),((ch)->player_specials->saved.betted_on))

#define GET_POS(ch)	  ((ch)->char_specials.position)
#define GET_IDNUM(ch)	  ((ch)->char_specials.saved.idnum)
#define GET_ID(x)         ((x)->id)
#define IS_CARRYING_W(ch) ((ch)->char_specials.carry_weight)
#define IS_CARRYING_N(ch) ((ch)->char_specials.carry_items)
#define FIGHTING(ch)	  ((ch)->char_specials.fighting)
#define MASTER(ch)	  ((ch)->master)
#define HUNTING(ch)	  ((ch)->char_specials.hunting)
#define HUNT_COUNT(ch)	  ((ch)->char_specials.hunt_count)
#define MAX_HUNTSTEPS(ch) FTOI((GET_LEVEL(ch) * 0.2) + 2)
#define RIDING(ch)	  ((ch)->char_specials.riding)	// (DAK)
#define RIDDEN_BY(ch)	  ((ch)->char_specials.ridden_by)	// (DAK)
#define GET_SAVE(ch, i)	  ((ch)->char_specials.saved.apply_saving_throw[i])
#define GET_ALIGNMENT(ch) ((ch)->char_specials.saved.alignment)
#define TALLY_FISH(ch)	  ((ch)->char_specials.tally[0])
#define TALLY_FOBJ(ch)	  ((ch)->char_specials.tally[1])
#define TALLY_DEBRIS(ch)  ((ch)->char_specials.tally[2])

#define SPECIALS(ch)          ((ch)->player_specials)
#define SAVED(ch)            	(SPECIALS(ch)->saved)
#define GET_COND(ch, i)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.conditions[(i)]))
#define GET_LOADROOM(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.load_room))
#define GET_PRACTICES(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.spells_to_learn))
#define GET_INVIS_LEV(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.invis_level))
#define GET_WIMP_LEV(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.wimp_level))
#define GET_FREEZE_LEV(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.freeze_level))
#define GET_BAD_PWS(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.bad_pws))
#define GET_SKILLS(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.skills))
#define GET_SUBS(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.subs))

#define IS_SAVING(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.saving))
#define GET_TALK(ch, i)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.talks[i]))
#define GET_KILLS(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->kills))
#define GET_NEWBIE_STATUS(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->newbie_status))
#define GET_EMAIL(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->email))
#define POOFIN(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->poofin))
#define IMMTITLE(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->immtitle))
#define POOFOUT(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->poofout))
#define GET_LOGINMSG(ch)      CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->loginmsg) )/*EDITED BY THOTTER!!! */
#define GET_LOGOUTMSG(ch)     CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->logoutmsg))/*EDITED BY THOTTER!!! */
#define PROMPT(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->prompt))
#define BPROMPT(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->battle_prompt))
#define GET_OLC_ZONE(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.olc_zone))
#define GET_LAST_OLC_TARG(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->last_olc_targ))
#define GET_LAST_OLC_MODE(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->last_olc_mode))
#define GET_ALIASES(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->aliases))
#define GET_LAST_TELL(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->last_tell))
#define GET_RIP_CNT(ch)         CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.rip_cnt))
#define GET_KILL_CNT(ch)        CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.kill_cnt))
#define GET_DT_CNT(ch)          CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.dt_cnt))
#define GET_REGEN_HIT(ch)       CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.reg_hit))
#define GET_REGEN_MANA(ch)      CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.reg_mana))
#define GET_REGEN_MOVE(ch)      CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.reg_move))
#define GET_REGEN_STAMINA(ch)      CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.reg_stamina))
#define GET_RP_GROUP(ch)        CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.rp_group))
#define TRADEPOINTS(ch)        CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.tradepoints))
#define MINE_DIR(ch)        CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.mine_dir))
#define MINE_STEALTH(ch)        CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.mine_stealth))
#define MINE_SPEED(ch)        CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.mine_speed))
#define MINE_BONUS(ch)        CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.mine_bonus))
#define MINE_DAMAGE(ch)        CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.mine_damage))
int check_mail ( Character *ch );
#define HAS_MAIL(ch)        CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.has_mail))
#define AFK_MSG(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->afk_msg))
#define BUSY_MSG(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->busy_msg))
#define REMORTS(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->remorts))
#define PRETITLE(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->pretitle))
#define GET_CONVERSIONS(ch)     CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->conversions))
#define GET_AWARD(ch)           CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->awardpoints))
#define GET_REWARD(ch)          CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->rewardpoints))
#define PAGEHEIGHT(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->pageheight))
#define PAGEWIDTH(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->pagewidth))
#define LOCKER(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->locker))
#define LOCKER_EXPIRE(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->expire))
#define LOCKER_LIMIT(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->limit))
#define DIE_TIME(ch)		CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->dying))
#define GET_SKILLMULTI(ch) CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->skillmulti))

//mord
#define GET_PK_CNT(ch)          CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.pk_kills))
#define GET_PK_RIP(ch)          CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.pk_deaths))
#define GET_PK_POINTS(ch)       CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.pk_points))
#define GET_POSTS(ch)           CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.fence_posts))
#define GET_NAILS(ch)           CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.fence_nails))
#define GET_WIRE(ch)            CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.fence_wire))
#define GET_PERM_ACCURACY(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.perm_accuracy))
#define GET_PERM_EVASION(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.perm_evasion))
#define GET_LAST_DAM_D(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.last_dam_done))
#define GET_LAST_DAM_T(ch)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.last_dam_taken))
#define GET_MASTERY(ch, i)	CHECK_PLAYER_SPECIAL((ch), (SPECIALS(ch)->saved.master[i]))
#define GET_ATTACK_POS(ch)      ((ch)->attack_location)
#define CONCEALMENT(ch) 	((ch)->concealment)
#define FUSE_LOC(ch, loc)       ((ch)->fuses[(loc)])
#define FUSED_TO(ch)            ((ch)->fused_to)
//Thotts
#define GET_CSNP_LVL(ch)	(SPECIALS(ch)->saved.ctellsnoop)

/** mob specials **/
#define MOB_DAM_TAKEN(mob)    ((mob)->mob_specials.damage_taken)
#define MOB_DAM_LIST(mob)     ((mob)->mob_specials.dam_list)

/* Token machine defines */
#define GET_BRASS_TOKEN_COUNT(ch)  (SPECIALS(ch)->saved.brass_tokens)
#define GET_BRONZE_TOKEN_COUNT(ch) (SPECIALS(ch)->saved.bronze_tokens)
#define GET_SILVER_TOKEN_COUNT(ch) (SPECIALS(ch)->saved.silver_tokens)
#define GET_GOLD_TOKEN_COUNT(ch)   (SPECIALS(ch)->saved.gold_tokens)
//mord
#define GET_SPEED(ch)  		   (SPECIALS(ch)->saved.speed)
#define AFF_SPEED(ch)		   (SPECIALS(ch)->saved.aff_speed)
#define GET_COOLNESS(ch)	   (SPECIALS(ch)->saved.coolness)

#define GET_SPELL_DIR(ch)	   ((ch)->spell_dir)
#define GET_PERC(ch)		   ((ch)->interact)


#define SHOP_IN 	1
#define SHOP_OUT 	2
#define DROPPED 	3
#define TAKEN 		4
#define AUCTION_IN 	5
#define AUCTION_OUT 	6
#define GOLD_GIVEN 	7
#define GOLD_RECEIVED 	8
#define DG_GOLD_IN 	9
#define DG_GOLD_OUT 	10

int get_skill_wait ( Character *ch, int skill );
void set_skill_wait ( Character *ch, int skill, int wait );
void make_wholist ( void );

#define GET_SKILL(ch, i)	ch->get_skill(i)
#define SET_SKILL(ch, i, pct)	set_skill(ch, i, pct)
#define GET_SPELL_WAIT(ch, i)   get_skill_wait(ch, i)
#define SET_SPELL_WAIT(ch, i, skill) set_skill_wait(ch, i, skill)

#define GET_EQ(ch, i)		((ch)->equipment[i])

#define GET_MOB_SPEC(ch)	(IS_MOB(ch) ? mob_index[(ch)->vnum]->func : NULL)
//#define GET_MOB_RNUM(mob)	((mob)->nr)
#define GET_MOB_VNUM(mob)	((mob)->vnum)


#define GET_MRACE(ch)		((ch)->mob_specials.race)
#define IS_ANIMAL(ch)		(IS_NPC(ch) && (GET_MRACE(ch) == MOB_RACE_ANIMAL))
#define IS_EXOTIC(ch)		(IS_NPC(ch) && (GET_MRACE(ch) == MOB_RACE_EXOTIC))
#define GET_MTYPE(ch)		((ch)->mob_specials.type)
#define IS_MOB_PREG(ch)		((ch)->mob_specials.pregnant)
#define DUE_DATE(ch)		((ch)->mob_specials.due_date)
#define MOB_TIER(ch)		((ch)->mob_specials.tier)
#define MOB_SUBSKILL(ch)	((ch)->mob_specials.subskill)
#define MOB_SKIN(ch)		((ch)->mob_specials.skin)
#define MOB_OWNER(ch)		((ch)->mob_specials.owner)

#define GET_DEFAULT_POS(ch)	((ch)->mob_specials.default_pos)
#define MEMORY(ch)		((ch)->mob_specials.memory)

#define STRENGTH_APPLY_INDEX(ch) \
        ( ((GET_ADD(ch)==0) || (GET_STR(ch) != (GET_LEVEL(ch) >= LVL_IMMORT ? MAX_IMM_BASE : MAX_MORTAL_BASE))) ?\
           GET_STR(ch) :\
          (GET_ADD(ch) <= 50) ? 26 :( \
          (GET_ADD(ch) <= 75) ? 27 :( \
          (GET_ADD(ch) <= 90) ? 28 :( \
          (GET_ADD(ch) <= 99) ? 29 :  30 ) ) )                   \
        )
int current_class_is_tier_num ( Character *ch );
#define CAN_CARRY_W(ch) (str_app[STRENGTH_APPLY_INDEX(ch)].carry_w + (400 * ((current_class_is_tier_num(ch)/2))))
#define CAN_CARRY_N(ch) (5 + (GET_DEX(ch) >> 1) + (GET_LEVEL(ch) >> 1))
#define AWAKE(ch) (GET_POS(ch) > POS_SLEEPING)
#define CAN_SEE_IN_DARK(ch) \
(AFF_FLAGGED(ch, AFF_INFRAVISION) || (!IS_NPC(ch) && (PRF_FLAGGED(ch, PRF_HOLYLIGHT) || GET_RACE(ch) == RACE_DWARF)))
#define CAN_HUNT(ch) (!MOB_FLAGGED(ch, MOB_NOPUSH) && !MOB_FLAGGED(ch, MOB_SENTINEL))

#define IS_GOOD(ch)    	(GET_ALIGNMENT(ch) >= 350)
#define IS_EVIL(ch)    	(GET_ALIGNMENT(ch) <= -350)
#define IS_NEUTRAL(ch) 	(!IS_GOOD(ch) && !IS_EVIL(ch))
#define IS_PK(ch)	(PLR_FLAGGED(ch, PLR_PK))
#define IS_MALE(ch)	(GET_SEX(ch) == SEX_MALE)
#define IS_FEMALE(ch)	(GET_SEX(ch) == SEX_FEMALE)
#define HAS_SCRIPT(ch)  (ch->script)

/* These three deprecated. */
#define WAIT_STATE(ch, cycle) do { GET_WAIT_STATE(ch) = (cycle); } while(0)
#define CHECK_WAIT(ch)                ((ch)->wait > 0)
#define GET_MOB_WAIT(ch)      GET_WAIT_STATE(ch)
/* New, preferred macro. */
#define GET_WAIT_STATE(ch)    ((ch)->wait)


/* descriptor-based utils ************************************************/

/* Hrm, not many.  We should make more. -gg 3/4/99 */
#define STATE(d)	((d)->connected)
#define ORIG_STATE(d)   ((d)->orig_connected)
#define SUB_STATE(d)    ((d)->sub_state)

/* object utils **********************************************************/
/*
 * Check for NOWHERE or the top array index?
 * If using unsigned types, the top array index will catch everything.
 * If using signed types, NOTHING will catch the majority of bad accesses.
 */
#define VALID_OBJ_RNUM(obj)	(GET_OBJ_RNUM(obj) <= top_of_objt && \
				 GET_OBJ_RNUM(obj) != NOTHING)

#define GET_OBJ_WAS(obj)  	((obj)->was_vnum)
#define GET_OBJ_VROOM(obj)      ((obj)->vroom)
#define GET_OBJ_NAME(obj)	((obj)->name)
#define GET_OBJ_LEVEL(obj)      ((obj)->obj_flags.level)
#define GET_OBJ_PERM(obj)       ((obj)->obj_flags.bitvector)
#define GET_OBJ_TYPE(obj)	  ((obj)->obj_flags.type_flag)
#define GET_OBJ_COST(obj)	  ((obj)->obj_flags.cost)
#define GET_OBJ_RENT(obj)	  ((obj)->obj_flags.cost_per_day)
#define GET_OBJ_EXTRA(obj)	  ((obj)->obj_flags.extra_flags)
#define GET_OBJ_EXTRA_AR(obj, i)   ((obj)->obj_flags.extra_flags[(i)])
#define GET_OBJ_WEAR(obj)	  ((obj)->obj_flags.wear_flags)
#define GET_OBJ_VAL(obj, val)	  ((obj)->obj_flags.value[(val)])
#define GET_FUEL(obj)           GET_OBJ_VAL(obj, 2)
#define GET_MAX_FUEL(obj)       GET_OBJ_VAL(obj, 3)
#define GET_FUEL_PERCENTAGE(obj)  (GET_MAX_FUEL(obj) != 0 ? 0 : GET_FUEL(obj)*100/GET_MAX_FUEL(obj))
#define GET_GEM_FUEL(obj)	  GET_OBJ_VAL(obj, 0)
#define GET_OBJ_MATERIAL(obj)   GET_OBJ_VAL(obj, 9)
#define GET_WEP_BALANCE(obj)    GET_OBJ_VAL(obj, 5)
#define GET_WEP_TYPE(obj)       GET_OBJ_VAL(obj, 6)
#define GET_WEP_LENGTH(obj)     GET_OBJ_VAL(obj, 4)
#define TRAP_IS_SET(obj)        GET_OBJ_VAL(obj, 3)
#define GET_OBJ_WEIGHT(obj)	  ((obj)->obj_flags.weight)
#define GET_OBJ_TIMER(obj)	  ((obj)->obj_flags.timer)
#define GET_OBJ_EXPIRE(obj)	  ((obj)->obj_flags.expire)
#define GET_TIMER_EVENT(obj)    ((obj)->obj_flags.timer_event)
#define GET_OBJ_RNUM(obj)	  ((obj)->item_number)
#define GET_OBJ_VNUM(obj)	  (VALID_OBJ_RNUM(obj) ? \
    obj_index[GET_OBJ_RNUM(obj)].vnum : NOTHING)
#define GET_OBJ_INNATE(obj)     ((obj)->obj_flags.obj_innate)

#define IS_OBJ_STAT(obj,stat)	(IS_SET_AR((obj)->obj_flags.extra_flags,stat))
#define IS_ROOM_STAT(room, stat) (IS_SET_AR(room->room_flags, stat))
#define IS_CORPSE(obj)		(GET_OBJ_TYPE(obj) == ITEM_CONTAINER && \
					GET_OBJ_VAL((obj), 3) == 1 && \
				(IS_OBJ_STAT(obj, ITEM_PC_CORPSE) || \
				 IS_OBJ_STAT(obj, ITEM_NPC_CORPSE)))

#define GET_OBJ_SPEC(obj) (VALID_OBJ_RNUM(obj) ? \
	(obj_index[(obj)->item_number].func) : NULL)

#define CAN_WEAR(obj, part) (IS_SET_AR((obj)->obj_flags.wear_flags, (part)))

/* compound utilities and other macros **********************************/

/*
 * Used to compute CircleMUD version. To see if the code running is newer
 * than 3.0pl13, you would use: #if _CIRCLEMUD > CIRCLEMUD_VERSION(3,0,13)
 */
#define CIRCLEMUD_VERSION(major, minor, patchlevel) \
	(((major) << 16) + ((minor) << 8) + (patchlevel))

#define HSHR(ch) (GET_SEX(ch) ? (GET_SEX(ch)==SEX_MALE ? "his":"her") :"its")
#define HSSH(ch) (GET_SEX(ch) ? (GET_SEX(ch)==SEX_MALE ? "he" :"she") : "it")
#define HMHR(ch) (GET_SEX(ch) ? (GET_SEX(ch)==SEX_MALE ? "him":"her") : "it")

#define ANA(obj) (strchr("aeiouyAEIOUY", *(obj)->name) ? "An" : "A")
#define SANA(obj) (strchr("aeiouyAEIOUY", *(obj)->name) ? "an" : "a")


/* Various macros building up to CAN_SEE */

#define LIGHT_OK(sub)	(!AFF_FLAGGED(sub, AFF_BLIND) && \
   (IS_LIGHT((sub)->in_room) || ((AFF_FLAGGED((sub), AFF_INFRAVISION)) || GET_RACE(sub) == RACE_DWARF)))

#define INVIS_OK(sub, obj) \
 ((!AFF_FLAGGED((obj),AFF_INVISIBLE) || AFF_FLAGGED(sub,AFF_DETECT_INVIS)) && \
 (!AFF_FLAGGED((obj), AFF_HIDE) || AFF_FLAGGED(sub, AFF_SENSE_LIFE)))

#define MORT_CAN_SEE(sub, obj) (LIGHT_OK(sub) && INVIS_OK(sub, obj))

#define IMM_CAN_SEE(sub, obj) \
   (MORT_CAN_SEE(sub, obj) || (!IS_NPC(sub) && PRF_FLAGGED(sub, PRF_HOLYLIGHT)))

#define SELF(sub, obj)  ((sub) == (obj))
#define HERE(sub, obj)  (IN_ROOM((sub))==IN_ROOM((obj)))
#define LOOK(ch)        look_at_room(ch, 0);
#define SEE_HELPER(sub, obj) (!IS_NPC(sub) && !IS_NPC(obj) && REMORTS(sub) == 0 && PLR_FLAGGED(obj, PLR_NEWBIE_HLPR))
/* Can subject see character "obj"? */
#define CAN_SEE(sub, obj) (SELF(sub, obj) || SEE_HELPER(sub, obj) || \
   (((GET_REAL_LEVEL(sub) >= (IS_NPC(obj) ? 0 : GET_INVIS_LEV(obj))) || (!IS_NPC(sub) && PLR_FLAGGED(sub, PLR_HERO) && (IS_NPC(obj) || GET_INVIS_LEV(obj) == LVL_HERO))) && \
   IMM_CAN_SEE(sub, obj)))

/* End of CAN_SEE */


#define INVIS_OK_OBJ(sub, obj) \
  (!IS_OBJ_STAT((obj), ITEM_INVISIBLE) || AFF_FLAGGED((sub), AFF_DETECT_INVIS))

/* Is anyone carrying this object and if so, are they visible? */
#define CAN_SEE_OBJ_CARRIER(sub, obj) \
  ((!obj->carried_by || CAN_SEE(sub, obj->carried_by)) &&	\
   (!obj->worn_by || CAN_SEE(sub, obj->worn_by)))

#define MORT_CAN_SEE_OBJ(sub, obj) \
  (LIGHT_OK(sub) && INVIS_OK_OBJ(sub, obj) && CAN_SEE_OBJ_CARRIER(sub, obj))

#define CAN_SEE_OBJ(sub, obj) \
   (((MORT_CAN_SEE_OBJ(sub, obj) || GET_OBJ_TYPE(obj) == ITEM_LIGHT)&& !IS_BURIED(obj)) || (!IS_NPC(sub) && PRF_FLAGGED((sub), PRF_HOLYLIGHT)))

#define CAN_CARRY_OBJ(ch,obj)  \
   (((IS_CARRYING_W(ch) + GET_OBJ_WEIGHT(obj)) <= CAN_CARRY_W(ch)) &&   \
    ((IS_CARRYING_N(ch) + 1) <= CAN_CARRY_N(ch)))

#define CAN_GET_OBJ(ch, obj)   \
   (CAN_WEAR((obj), ITEM_WEAR_TAKE) && CAN_CARRY_OBJ((ch),(obj)) && \
    CAN_SEE_OBJ((ch),(obj)))
const char * hidden_name ( Character *ch );
#define PERS(ch, vict)   (!CAN_SEE(vict, ch) ?  "someone" :  hidden_name(ch))
#define PERS_S(ch, vict)   (!CAN_SEE(vict, ch) ?  string("someone") :  hidden_name(ch))


#define OBJS(obj, vict) (CAN_SEE_OBJ((vict), (obj)) ? \
	(obj)->short_description  : "something")

#define OBJN(obj, vict) (CAN_SEE_OBJ((vict), (obj)) ? \
	fname((obj)->name) : "something")


#define EXIT(ch, door)  (IN_ROOM(ch)->dir_option[door])
#define W_EXIT(room, num)     ((room)->dir_option[(num)])
#define R_EXIT(room, num)     ((room)->dir_option[(num)])
extern room_rnum VEHICLE_ROOM;

#define EXIT2(roomnum, door) ((roomnum)->dir_option[door])
#define CAN_GO2(roomnum, door) (EXIT2(roomnum, door) && \
                       (EXIT2(roomnum, door)->to_room != NULL) && \
                       !IS_SET(EXIT2(roomnum,door)->exit_info, EX_CLOSED))

#define CAN_GO(ch, door) (EXIT(ch,door) && \
			 (EXIT(ch,door)->to_room != NULL) && \
			 !IS_SET(EXIT(ch, door)->exit_info, EX_CLOSED))

const char *get_class_abbrev ( Character *ch );
#define CLASS_ABBR(ch) (IS_NPC(ch) ? "---" : get_class_abbrev(ch))
#define RACE_ABBR(ch) (IS_NPC(ch) ? "--" : race_abbrevs[(int)GET_RACE(ch)])

#define IS_MAGE(ch)             (!IS_NPC(ch) && \
                                (GET_CLASS(ch) == CLASS_MAGE))
#define IS_PRIEST(ch)           (!IS_NPC(ch) && \
                                (GET_CLASS(ch) == CLASS_PRIEST))
#define IS_THIEF(ch)            (!IS_NPC(ch) && \
                                (GET_CLASS(ch) == CLASS_THIEF))
#define IS_WARRIOR(ch)          (!IS_NPC(ch) && \
                                (GET_CLASS(ch) == CLASS_WARRIOR))
#define IS_HUNTER(ch)           (!IS_NPC(ch) && \
                                (GET_CLASS(ch) == CLASS_HUNTER))
#define IS_GYPSY(ch)            (!IS_NPC(ch) && \
                                (GET_CLASS(ch) == CLASS_GYPSY))
#define IS_RANGER(ch)           (!IS_NPC(ch) && \
                                (GET_CLASS(ch) == CLASS_RANGER))
#define IS_ESPER(ch)            (!IS_NPC(ch) && \
                                (GET_CLASS(ch) == CLASS_ESPER))

#define IS_FAUN(ch)		(!IS_NPC(ch) && \
				(GET_RACE(ch) == RACE_FAUN))
#define IS_CENTAUR(ch)		(!IS_NPC(ch) && \
				(GET_RACE(ch) == RACE_CENTAUR))
#define IS_DWARF(ch)		(!IS_NPC(ch) && \
				(GET_RACE(ch) == RACE_DWARF))
#define IS_ELF(ch)		(!IS_NPC(ch) && \
				(GET_RACE(ch) == RACE_ELF))
#define IS_INDIAN(ch)		(!IS_NPC(ch) && \
				(GET_RACE(ch) == RACE_INDIAN))
#define IS_GRINGO(ch)           (!IS_NPC(ch) && \
                                (GET_RACE(ch) == RACE_GRINGO))
#define IS_MARTIAN(ch)           (!IS_NPC(ch) && \
                                (GET_RACE(ch) == RACE_MARTIAN))
#define IS_SPACE_WOLF(ch)       (!IS_NPC(ch) && \
                                (GET_RACE(ch) == RACE_SPACE_WOLF))
#define IS_GLADIATOR(ch)	(!IS_NPC(ch) && \
				(GET_RACE(ch) == RACE_GLADIATOR))
char *print_weather ( room_rnum room, char *buf, size_t len );
#define OUTSIDE(ch) (!(ROOM_FLAGGED((ch)->in_room, ROOM_INDOORS)||SECT((ch)->in_room)==SECT_INSIDE) )
#define IS_DAY (sunlight == SUN_LIGHT)
#define IS_NIGHT  (sunlight == SUN_DARK)
#define IS_HOT(room) ((IS_DAY && SECT(room) == SECT_DESERT) || (SECT(room) == SECT_SUN) || (SECT(room) == SECT_BADLANDS))
#define IS_COLD(room) ((IS_NIGHT && SECT(room) == SECT_DESERT) || SECT(room) == SECT_ICE || SECT(room) == SECT_SNOW || SECT(room) == SECT_TUNDRA)
#define IS_IN_WATER(room) (SECT(room) == SECT_UNDERWATER || SECT(room) == SECT_WATER_SWIM || SECT(room) == SECT_WATER_NOSWIM)
/* OS compatibility ******************************************************/


/* there could be some strange OS which doesn't have NULL... */
#ifndef NULL
#define NULL (void *)0
#endif

#if !defined(FALSE)
#define FALSE 0
#endif

#if !defined(TRUE)
#define TRUE  (!FALSE)
#endif

#if !defined(YES)
#define YES 1
#endif

#if !defined(NO)
#define NO 0
#endif


/* defines for fseek */
#ifndef SEEK_SET
#define SEEK_SET	0
#define SEEK_CUR	1
#define SEEK_END	2
#endif

/*
 * NOCRYPT can be defined by an implementor manually in sysdep.h.
 * CIRCLE_CRYPT is a variable that the 'configure' script
 * automatically sets when it determines whether or not the system is
 * capable of encrypting.
 */
#if defined(NOCRYPT) || !defined(CIRCLE_CRYPT)
#define CRYPT(a,b) (a)
#else
#define CRYPT(a,b) ((char *) crypt((a),(b)))
#endif

#define SENDOK(ch)	(((ch)->desc || SCRIPT_CHECK((ch), MTRIG_ACT)) && \
			(to_sleeping || AWAKE(ch)) && \
			!PLR_FLAGGED((ch), PLR_WRITING))

#define GET_REMORT(ch) 		    ((ch)->player.was_class)
#define GET_REMORT_TWO(ch) 	    ((ch)->player.was_class1)
#define GET_REMORT_THREE(ch)	    ((ch)->player.was_class2)

#define GET_ORIG_LEV(ch)   	    (SPECIALS(ch)->saved.orig_lev)
#define GET_CLASS_TIER(ch)          (SPECIALS(ch)->saved.tier)
#define GET_REMORT_TIER(ch)         (SPECIALS(ch)->saved.tier1)
#define GET_REMORT_TWO_TIER(ch)     (SPECIALS(ch)->saved.tier2)
#define GET_REMORT_THREE_TIER(ch)   (SPECIALS(ch)->saved.tier3)

#define IS_STUCK(ch) (AFF_FLAGGED((ch), AFF_STUCK))
/* Romance Macros */
#define ROMANCE(ch) ((ch)->player.romance)
#define PARTNER(ch) ((ch)->player.partner)
/* MatingMod Macros */
#define PREG(ch) ((ch)->player.ticks_left)
#define IS_PREG(ch)	(PREG(ch) != NOT_PREG)

#define GET_IGNORELIST(ch) (SPECIALS(ch)->ignorelist)

void char_from_chair ( Character *ch );
#define SITTING(ch)          ((ch)->char_specials.chair)
#define NEXT_SITTING(ch)     ((ch)->char_specials.next_in_chair)
#define OBJ_SAT_IN_BY(obj)   ((obj)->sitting_here)

#define URANGE(a, b, c)          ((b) < (a) ? (a) : ((b) > (c) ? (c) : (b)))

typedef unsigned char BYTE;	/* 8bit unsigned */
typedef signed char SBYTE;	/* 8bit signed */
typedef signed short int WORD;	/* 16bit signed */
typedef unsigned short int UWORD;	/* 16bit unsigned */
typedef signed long int LWORD;	/* 32bit signed */
typedef unsigned long int ULWORD;	/* 32bit unsigned */
typedef unsigned long int FLAG;	/* 32bit unsigned */

#define GET_OBJ_AFFECT(obj)     ((obj)->obj_flags.bitvector)
#define OBJAFF_FLAGGED(obj, flag) (IS_SET(GET_OBJ_AFFECT(obj), (flag)))


#define FOCUS_STAFF 1
#define FOCUS_ORB   2
#define FOCUS_ORBSTAFF 3

#define CAN_ENTER_ZONE(ch, room) \
((!ZONE_FLAGGED(room->zone, ZONE_CLOSED)) || ((ZONE_FLAGGED(room->zone, ZONE_RP_ONLY))&&(PLR_FLAGGED(ch, PLR_ROLEPLAYER))) || \
((ZONE_FLAGGED(room->zone, ZONE_PK_ONLY)) && IS_PK(ch)) || ((ZONE_FLAGGED(room->zone, ZONE_REMORT_ONLY)) && REMORTS(ch)) || \
((ZONE_FLAGGED(room->zone, ZONE_NOCHEATER)) && PLR_FLAGGED(ch, PLR_CHEATER)))



/* prototypes from regen.c */
void alter_hit ( Character *ch, int amount );
void alter_mana ( Character *ch, int amount );
void alter_move ( Character *ch, int amount );
int sub_number ( char *name );
const char * colour_option_name ( int num );
struct obj_data *revert_object ( struct obj_data *obj );
#define GOLD_BANK   0
#define GOLD_HAND   1
#define GOLD_ALL    2
float square_root ( float num );
int get_sub_status ( Character *ch, int i );
int toggle_sub_status ( Character *ch, int i, int onoff );
int stop_task ( Character *ch );
int first_word_is_name ( Character *ch, char * argument );
int choose_real_abils ( Character *ch, char select, int amount );

#define TRAVEL_LIST(thing) ((thing)->travel_list)
int hunt_location ( long id, int type );
void add_travel_point_by_pointer ( struct travel_point_data **tlist, room_vnum dest );
void add_travel_point_by_thing ( void *thing, int type, room_vnum dest );
void remove_travel_point_by_dest ( struct travel_point_data **tlist, room_vnum dest );
void remove_travel_point_by_num ( struct travel_point_data **tlist, int num );
void remove_travel_point_by_pointer ( struct travel_point_data **tlist, struct travel_point_data *dead );
void free_travel_points ( struct travel_point_data *t );

int do_simple_obj_move ( struct obj_data *obj, int dir, Character *ch );
int perform_move_obj ( struct obj_data *obj, int dir, Character *ch );
struct obj_data *has_vehicle ( Character *ch );


/*******************  Config macros *********************/

#define CONFIG_CONFFILE         config_info.CONFFILE

#define CONFIG_PK_ALLOWED       config_info.play.pk_allowed
#define CONFIG_PT_ALLOWED       config_info.play.pt_allowed
#define CONFIG_LEVEL_CAN_SHOUT  config_info.play.level_can_shout
#define CONFIG_HOLLER_MOVE_COST config_info.play.holler_move_cost
#define CONFIG_TUNNEL_SIZE      config_info.play.tunnel_size
#define CONFIG_MAX_EXP_GAIN     config_info.play.max_exp_gain
#define CONFIG_MAX_EXP_LOSS     config_info.play.max_exp_loss
#define CONFIG_MAX_NPC_CORPSE_TIME config_info.play.max_npc_corpse_time
#define CONFIG_MAX_PC_CORPSE_TIME config_info.play.max_pc_corpse_time
#define CONFIG_IDLE_VOID        config_info.play.idle_void
#define CONFIG_IDLE_RENT_TIME   config_info.play.idle_rent_time
#define CONFIG_IDLE_MAX_LEVEL   config_info.play.idle_max_level
#define CONFIG_DTS_ARE_DUMPS    config_info.play.dts_are_dumps
#define CONFIG_LOAD_INVENTORY   config_info.play.load_into_inventory
#define CONFIG_TRACK_T_DOORS    config_info.play.track_through_doors
#define CONFIG_IMMORT_LEVEL_OK  config_info.play.immort_level_ok
#define CONFIG_DOUBLE_EXP	config_info.play.double_exp
#define CONFIG_OK               config_info.play.OK
#define CONFIG_NOPERSON         config_info.play.NOPERSON
#define CONFIG_NOEFFECT         config_info.play.NOEFFECT

/** Crash Saves **/
#define CONFIG_FREE_RENT        config_info.csd.free_rent
#define CONFIG_MAX_OBJ_SAVE     config_info.csd.max_obj_save
#define CONFIG_MIN_RENT_COST    config_info.csd.min_rent_cost
#define CONFIG_AUTO_SAVE        config_info.csd.auto_save
#define CONFIG_AUTOSAVE_TIME    config_info.csd.autosave_time
#define CONFIG_CRASH_TIMEOUT    config_info.csd.crash_file_timeout
#define CONFIG_RENT_TIMEOUT     config_info.csd.rent_file_timeout

/** Room Numbers **/
#define CONFIG_MORTAL_START     world_vnum[config_info.room_nums.mortal_start_room]
#define CONFIG_IMMORTAL_START   world_vnum[config_info.room_nums.immort_start_room]
#define CONFIG_FROZEN_START     world_vnum[config_info.room_nums.frozen_start_room]
#define CONFIG_DON_ROOM_1       world_vnum[config_info.room_nums.donation_room_1]
#define CONFIG_DON_ROOM_2       world_vnum[config_info.room_nums.donation_room_2]
#define CONFIG_DON_ROOM_3       world_vnum[config_info.room_nums.donation_room_3]
#define CONFIG_GLA_DEATH_ROOM	world_vnum[config_info.room_nums.gladiator_death_room]

/** Game Operation **/
#define CONFIG_DFLT_PORT        config_info.operation.DFLT_PORT
#define CONFIG_DFLT_IP          config_info.operation.DFLT_IP
#define CONFIG_MAX_PLAYING      config_info.operation.max_playing
#define CONFIG_MAX_FILESIZE     config_info.operation.max_filesize
#define CONFIG_MAX_BAD_PWS      config_info.operation.max_bad_pws
#define CONFIG_SITEOK_ALL       config_info.operation.siteok_everyone
#define CONFIG_OLC_SAVE         config_info.operation.auto_save_olc
#define CONFIG_NEW_SOCIALS      config_info.operation.use_new_socials
#define CONFIG_NS_IS_SLOW       config_info.operation.nameserver_is_slow
#define CONFIG_DFLT_DIR         config_info.operation.DFLT_DIR
#define CONFIG_LOGNAME          config_info.operation.LOGNAME
#define CONFIG_MENU             config_info.operation.MENU
#define CONFIG_WELC_MESSG       config_info.operation.WELC_MESSG
#define CONFIG_START_MESSG      config_info.operation.START_MESSG

/** Autowiz **/
#define CONFIG_USE_AUTOWIZ      config_info.autowiz.use_autowiz
#define CONFIG_MIN_WIZLIST_LEV  config_info.autowiz.min_wizlist_lev


bool valid_id_num ( long id );
int fileExists ( char * fileName );

#define FTOI(f) ((int)((f)))
