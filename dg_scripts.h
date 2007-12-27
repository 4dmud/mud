/**************************************************************************
*  File: scripts.h                                                        *
*  Usage: header file for script structures and contstants, and           *
*         function prototypes for scripts.c                               *
*                                                                         *
*                                                                         *
*  $Author: w4dimenscor $
*  $Date: 2007/05/19 18:06:12 $
*  $Revision: 1.15 $
**************************************************************************/

#define DG_SCRIPT_VERSION "DG Scripts 1.0.14"

#define SCRIPT_ERROR_CODE     -9999999   /* this shouldn't happen too often */

#define    MOB_TRIGGER   0
#define    OBJ_TRIGGER   1
#define    WLD_TRIGGER   2
#define 	 ASS_TRIGGER	3


/* unless you change this, Puff casts all your dg spells */
#define DG_CASTER_PROXY 1
/* spells cast by objects and rooms use this level */
#define DG_SPELL_LEVEL  45


/*
 * define this if you don't want wear/remove triggers to fire when
 * a player is saved.
 */
#define NO_EXTRANEOUS_TRIGGERS
/*
 * %actor.room% behaviour :
 * Until pl 7 %actor.room% returned a room vnum.
 * Working with this number in scripts was unnecessarily hard,
 * especially in those situations one needed the id of the room,
 * the items in it, etc. As a result of this, the output
 * has been changed (as of pl 8) to a room variable.
 * This means old scripts will need a minor adjustment;
 *
 * Before:
 * if %actor.room%==3001
 *   %echo% You are at the main temple.
 *
 * After:
 * eval room %actor.room%
 * if %room.vnum%==3001
 *   %echo% You are at the main temple.
 *
 * If you wish to continue using the old style, comment out the line below.
 *
 * Welcor
 */

//#define ACTOR_ROOM_IS_UID 1


/* mob trigger types */
#define MTRIG_GLOBAL           (1 << 0)	/* check even if zone empty   */
#define MTRIG_RANDOM           (1 << 1)	/* checked randomly           */
#define MTRIG_COMMAND          (1 << 2)	/* character types a command  */
#define MTRIG_SPEECH           (1 << 3)	/* a char says a word/phrase  */
#define MTRIG_ACT              (1 << 4)	/* word or phrase sent to act */
#define MTRIG_DEATH            (1 << 5)	/* character dies             */
#define MTRIG_GREET            (1 << 6)	/* something enters room seen */
#define MTRIG_GREET_ALL        (1 << 7)	/* anything enters room       */
#define MTRIG_ENTRY            (1 << 8)	/* the mob enters a room      */
#define MTRIG_RECEIVE          (1 << 9)	/* character is given obj     */
#define MTRIG_FIGHT            (1 << 10)	/* each pulse while fighting  */
#define MTRIG_HITPRCNT         (1 << 11)	/* fighting and below some hp */
#define MTRIG_BRIBE	           (1 << 12)	/* coins are given to mob     */
#define MTRIG_LOAD             (1 << 13)	/* the mob is loaded          */
#define MTRIG_MEMORY           (1 << 14)	/* mob see's someone remembered */
#define MTRIG_TIME	           (1 << 15)	/* mob is on a time prog      */
#define MTRIG_CAST             (1 << 16)	/* mob targetted by spell     */
#define MTRIG_LEAVE            (1 << 17)	/* someone leaves room seen   */
#define MTRIG_DOOR             (1 << 18)	/* door manipulated in room   */
#define MTRIG_FUNCTION         (1 << 19)

/* obj trigger types */
#define OTRIG_GLOBAL           (1 << 0)	/* unused                     */
#define OTRIG_RANDOM           (1 << 1)	/* checked randomly           */
#define OTRIG_COMMAND          (1 << 2)	/* character types a command  */
#define OTRIG_SPEECH	      (1 << 3)	/* character speaks          */
#define OTRIG_ENTER	       (1 << 4) /* object enters the room     */
#define OTRIG_TIMER            (1 << 5)	/* item's timer expires       */
#define OTRIG_GET              (1 << 6)	/* item is picked up          */
#define OTRIG_DROP             (1 << 7)	/* character trys to drop obj */
#define OTRIG_GIVE             (1 << 8)	/* character trys to give obj */
#define OTRIG_WEAR             (1 << 9)	/* character trys to wear obj */
#define OTRIG_ASSEMBLE         (1 << 10)/* attached to the assembly command group */
#define OTRIG_REMOVE           (1 << 11)/* character trys to remove obj */
#define OTRIG_PUT_IN	       (1 << 12)/* character tries to put something into a container */

#define OTRIG_LOAD             (1 << 13)/* the object is loaded       */
#define OTRIG_GET_OUT          (1 << 14)/* character tries to get something out of a container */
#define OTRIG_TIME             (1 << 15)/* time prog */
#define OTRIG_CAST             (1 << 16)/* object targetted by spell */
#define OTRIG_LEAVE            (1 << 17)     /* someone leaves room seen    */
#define OTRIG_CONSUME          (1 << 18)    /* char tries to eat/drink obj */
#define OTRIG_FUNCTION         (1 << 19)

/* wld trigger types */
#define WTRIG_GLOBAL           (1 << 0)	/* check even if zone empty   */
#define WTRIG_RANDOM           (1 << 1)	/* checked randomly           */
#define WTRIG_COMMAND          (1 << 2)	/* character types a command  */
#define WTRIG_SPEECH           (1 << 3)	/* a char says word/phrase    */

#define WTRIG_RESET            (1 << 5)	/* zone has been reset        */
#define WTRIG_ENTER            (1 << 6)	/* character enters room      */
#define WTRIG_DROP             (1 << 7)	/* something dropped in room  */

#define WTRIG_TIME             (1 << 15)	/* room has a time trigger    */
#define WTRIG_CAST             (1 << 16)	/* spell cast in room */
#define WTRIG_LEAVE            (1 << 17)	/* character leaves the room */
#define WTRIG_DOOR             (1 << 18)	/* door manipulated in room  */
#define WTRIG_FUNCTION         (1 << 19)

/* obj command trigger types */
#define OCMD_EQUIP             (1 << 0)	/* obj must be in char's equip */
#define OCMD_INVEN             (1 << 1)	/* obj must be in char's inven */
#define OCMD_ROOM              (1 << 2)	/* obj must be in char's room  */

/* obj consume trigger commands */
#define OCMD_EAT    1
#define OCMD_DRINK  2
#define OCMD_QUAFF  3


#define TRIG_NEW                0	/* trigger starts from top  */
#define TRIG_RESTART            1	/* trigger restarting       */


/*
 * These are slightly off of PULSE_MOBILE so
 * everything isnt happening at the same time 
 */
#define PULSE_DG_SCRIPT         (13 RL_SEC)


#define MAX_SCRIPT_DEPTH      10	/* maximum depth triggers can \
					   recurse into each other */


/* one line of the trigger */
struct cmdlist_element {
    char *cmd;			/* one line of a trigger */
    struct cmdlist_element *original;
    struct cmdlist_element *next;
};

struct trig_var_data {
    //char *name;			/* name of variable  */
    //char *value;		/* value of variable */
    string name;
    string value;
    long context;		/* 0: global context */

    struct trig_var_data *next;
    trig_var_data() {
        //name = NULL;
        //value = NULL;
        context = 0;
    }
    ~trig_var_data() {
        //if (name)
        //    delete[] name;
        //if (value)
        //   delete[] value;
    }
};

/* structure for triggers */
struct trig_data {
    int nr;			/* trigger's rnum                  */
    byte attach_type;		/* mob/obj/wld intentions          */
    byte data_type;		/* type of game_data for trig      */
    char *name;			/* name of trigger                 */
    long trigger_type;		/* type of trigger (for bitvector) */
    struct cmdlist_element *cmdlist;	/* top of command list             */
    struct cmdlist_element *curr_state;	/* ptr to current line of trigger  */
    int narg;			/* numerical argument              */
    char *arglist;		/* argument list                   */
    int depth;			/* depth into nest ifs/whiles/etc  */
    int loops;			/* loop iteration counter          */
    struct event *wait_event;	/* event to pause the trigger      */
    ubyte purged;		/* trigger is set to be purged     */
    struct trig_var_data *var_list;	/* list of local vars for trigger  */
    /** for script functions **/
    struct trig_data *parent; /* this is the trigger that is running this script */
    /** end for script functions **/
    struct trig_data *next;
    struct trig_data *next_in_world;	/* next in the global trigger list */
};


/* a complete script (composed of several triggers) */
struct script_data {
    long types;			/* bitvector of trigger types */
    struct trig_data *trig_list;	/* list of triggers           */
    struct trig_var_data *global_vars;	/* list of global variables   */
    ubyte purged;		/* script is set to be purged */
    long context;		/* current context for statics */

    struct script_data *next;	/* used for purged_scripts    */
};

/* The event data for the wait command */
struct wait_event_data {
    struct trig_data *trigger;
    void *go;
    int type;
    wait_event_data(struct trig_data *tr, void *v, int ty) {
        trigger = tr;
        go = v;
        type = ty;
    }
    ~wait_event_data() {}
}
;

/* typedefs that the dg functions rely on */
typedef struct index_data index_data;
typedef struct obj_data obj_data;
typedef struct trig_data trig_data;

/* used for actor memory triggers */
struct script_memory {
    long id;			/* id of who to remember */
    char *cmd;			/* command, or NULL for generic */
    struct script_memory *next;
};
/* from dg_objcmd.c */
room_rnum obj_room(obj_data *obj);

/* function prototypes from dg_triggers.c */
char *one_phrase(char *arg, char *first_arg);
int is_substring(char *sub, char *string);
int word_check(char *str, char *wordlist);

void check_time_triggers(void);
void find_uid_name(char *uid, char *name, size_t nlen);
void find_uid_name(string &uid, char *name, size_t nlen);
void do_sstat_room(Character * ch);
void do_sstat_object(Character *ch, obj_data *j);
void do_sstat_character(Character *ch, Character *k);
void time_mtrigger(Character *ch);
void time_otrigger(obj_data *obj);
void time_wtrigger(Room *room);
void act_mtrigger(const Character *ch, char *str,
                  Character *actor, Character *victim, obj_data *object, obj_data *target, char *arg);
void speech_mtrigger(Character *actor, char *str);
void speech_wtrigger(Character *actor, char *str);
int speech_otrigger(Character * actor, char *str);
void greet_memory_mtrigger(Character *ch);
int greet_mtrigger(Character *actor, int dir);
int entry_mtrigger(Character *ch);
void entry_memory_mtrigger(Character *ch);
int enter_wtrigger(Room *room, Character *actor, int dir);
int drop_otrigger(obj_data *obj, Character *actor);
int timer_otrigger(obj_data *obj);
int get_otrigger(obj_data *obj, Character *actor);
int drop_wtrigger(obj_data *obj, Character *actor);
int give_otrigger(obj_data *obj, Character *actor,
                  Character *victim);
int receive_mtrigger(Character *ch, Character *actor,
                     obj_data *obj);
void bribe_mtrigger(Character *ch, Character *actor,
                    int amount);


int wear_otrigger(obj_data *obj, Character *actor, int where);
int remove_otrigger(obj_data *obj, Character *actor);

int cmd_otrig(obj_data *obj, Character *actor, char *cmd,
              char *argument, int type);
int command_mtrigger(Character *actor, char *cmd, char *argument);
int command_otrigger(Character *actor, char *cmd, char *argument);
int command_wtrigger(Character *actor, char *cmd, char *argument);
int death_mtrigger(Character *ch, Character *actor);
void fight_mtrigger(Character *ch);
void hitprcnt_mtrigger(Character *ch);

void random_mtrigger(Character *ch);
void random_otrigger(obj_data *obj);
void random_wtrigger(Room *ch);
void reset_wtrigger(Room *ch);

int load_mtrigger(Character *ch);
int load_otrigger(obj_data *obj);

int cast_mtrigger(Character *actor, Character *ch, int spellnum);
int cast_otrigger(Character *actor, obj_data *obj, int spellnum);
int cast_wtrigger(Character *actor, Character *vict, obj_data *obj, int spellnum);

int leave_mtrigger(Character *actor, int dir);
int leave_wtrigger(Room *room, Character *actor, int dir);
int leave_otrigger(Room *room, Character *actor, int dir);

int door_mtrigger(Character *actor, int subcmd, int dir);
int door_wtrigger(Character *actor, int subcmd, int dir);

int consume_otrigger(obj_data *obj, Character *actor, int cmd);
int put_in_otrigger(obj_data *cont, obj_data *obj, Character *actor);
int get_out_otrigger(obj_data *cont, obj_data *obj, Character *actor);
int enter_otrigger(obj_data *object, const char* direction);

/* function prototypes from dg_scripts.c */
void script_damage(Character *vict, int dam);
obj_data *find_obj(long n);
Room *find_room(long n);
Character *find_char(long n);
char *str_str(char *cs, char *ct);
int find_eq_pos_script(char *arg);
int can_wear_on_pos(struct obj_data *obj, int pos);
Character *get_char(char *name);
Character *get_char(const char *name);
Character *get_char_near_obj(obj_data *obj, char *name);
Character *get_char_in_room(Room *room, char *name);
obj_data *get_obj_near_obj(obj_data *obj, char *name);
obj_data *get_obj(const char *name);
Room *get_room(const char *name);
Character *get_char_by_obj(obj_data *obj, const char *name);
Character *get_char_by_room(Room *room,const char *name);
obj_data *get_obj_by_obj(obj_data *obj,const char *name);
obj_data *get_obj_in_room(Room *room, char *name);
obj_data *get_obj_by_room(Room *room,const char *name);
int trgvar_in_room(room_vnum vnum);
obj_data *get_obj_in_list(char *name, struct obj_data *list);
obj_data *get_obj_in_list(const char *name, struct obj_data *list);
obj_data *get_object_in_equip(Character * ch, char *name);
obj_data *get_object_in_equip(Character * ch, const char *name);
void script_trigger_check(void);
void add_trigger(struct script_data *sc, trig_data *t, int loc);


void script_vlog(const char *format, va_list args);
void script_log(const char *format, ...) __attribute__ ((format (printf, 1, 2)));
char *matching_quote(char *p);
Room *dg_room_of_obj(struct obj_data *obj);

/* To maintain strict-aliasing we'll have to do this trick with a union */
/* Thanks to Chris Gilbert for reminding me that there are other options. */
int script_driver(void *go_adress, trig_data *trig, int type, int mode);
trig_rnum real_trigger(trig_vnum vnum);
void process_eval(void *go, struct script_data *sc, trig_data *trig,
                  int type, char *cmd);
void read_saved_vars(Character *ch);
void save_char_vars(Character *ch);
//void init_lookup_table(void);
//void add_to_lookup_table(long uid, void *c);
//void remove_from_lookup_table(long uid);
void addObjToLookupTable(long uid, obj_data * o);
void addChToLookupTable(long uid, Character * c);
void removeFromObjLookupTable(long uid);
void removeFromChLookupTable(long uid);

/* from dg_db_scripts.c */
void parse_trigger(FILE * trig_f, int nr, zone_vnum zon);
trig_data *read_trigger(unsigned int nr);
void trig_data_init(trig_data *this_data);
void trig_data_copy(trig_data *this_data, const trig_data *trg);
void dg_read_trigger(FILE *fp, void *proto, int type);
void dg_obj_trigger(char *line, struct obj_data *obj);
void assign_triggers(void *i, int type);


/* From dg_variables.c */
void add_var(struct trig_var_data **var_list,const char *name,const char *value, long id);
void add_var(struct trig_var_data **var_list, string name, string value, long id);
int item_in_list(char *item, obj_data *list);
char *skill_percent(Character *ch, char *skill);
int char_has_item(char *item, Character *ch);
void var_subst(void *go, struct script_data *sc, trig_data *trig,
               int type, char *line, char *buf, size_t b_len);
int text_processed(char *field, char *subfield, struct trig_var_data *vd,
                   char *str, size_t slen);
void find_replacement(void *go, struct script_data *sc, trig_data *trig,
                      int type, char *var, char *field, char *subfield, char *str, size_t slen);


/* From dg_handler.c */
void free_var_el(struct trig_var_data *var);
void free_varlist(struct trig_var_data *vd);
int remove_var(struct trig_var_data **var_list,const char *name);
void free_trigger(trig_data *trig);
void extract_trigger(struct trig_data *trig);
void extract_script(void *thing, int type);
void extract_script_mem(struct script_memory *sc);
void free_proto_script(void *thing, int type);
void copy_proto_script(void *source, void *dest, int type);
void extract_value(struct script_data *sc, struct trig_data *trig, char *cmd);
int trgvar_in_room(room_vnum vnum);
int valid_dg_target(Character *ch, int allow_gods);
void delete_variables(const char *charname);
void update_wait_events(Room *to, Room *from);

/* from dg_comm.c */
char *any_one_name(char *argument, char *first_arg);
void sub_write_to_char(Character *ch, char *tokens[],
                       void *otokens[], char type[]);
void sub_write(char *arg, Character *ch, byte find_invis, int targets);
void send_to_zone(char *messg, zone_rnum zone);
void send_to_zone_range(char *messg, int zone_rnum, int lower_vnum,
                        int upper_vnum);

/* from dg_misc.c */
void do_dg_cast(void *go, struct script_data *sc, trig_data *trig,
                int type, char *cmd);
void do_dg_affect(void *go, struct script_data *sc, trig_data *trig,
                  int type, char *cmd);
void do_dg_destination(void *go, struct script_data *sc, trig_data *trig,
                       int type, char *cmd);

char *sub_percent(Character *ch, char *sub);
void function_script(void *go, struct script_data *sc, trig_data *perent, int type, char *cmd);

/* Macros for scripts */

// #define UID_CHAR   '\x1b'
#define UID_CHAR   '}' /* easier to debug this way - } can be read on screen */

#define GET_TRIG_NAME(t)          ((t)->name)
#define GET_TRIG_RNUM(t)          ((t)->nr)
#define GET_TRIG_VNUM(t)	  (trig_index[(t)->nr]->vnum)
#define GET_TRIG_TYPE(t)          ((t)->trigger_type)
#define GET_TRIG_DATA_TYPE(t)	  ((t)->data_type)
#define GET_TRIG_NARG(t)          ((t)->narg)
#define GET_TRIG_ARG(t)           ((t)->arglist)
#define GET_TRIG_VARS(t)	  ((t)->var_list)
#define GET_TRIG_WAIT(t)	  ((t)->wait_event)
#define GET_TRIG_DEPTH(t)         ((t)->depth)
#define GET_TRIG_LOOPS(t)         ((t)->loops)

/* player id's: 0 to MOB_ID_BASE - 1            */
/* mob id's: MOB_ID_BASE to ROOM_ID_BASE - 1      */
/* room id's: ROOM_ID_BASE to OBJ_ID_BASE - 1    */
#define MOB_ID_BASE	  50000  /* 50000 player IDNUMS should suffice */
#define ROOM_ID_BASE    1050000 /* 1000000 Mobs */
#define OBJ_ID_BASE     1300000 /* 250000 Rooms */

#define SCRIPT(o)		  ((o)->script)
#define SCRIPT_MEM(c)             ((c)->memory)

#define SCRIPT_TYPES(s)		  ((s)->types)
#define TRIGGERS(s)		  ((s)->trig_list)

#define GET_SHORT(ch)    ((ch)->player.short_descr)


#define SCRIPT_CHECK(go, type)   (SCRIPT(go) && \
				  IS_SET(SCRIPT_TYPES(SCRIPT(go)), type))
#define TRIGGER_CHECK(t, type)   (IS_SET(GET_TRIG_TYPE(t), type) && \
				  !GET_TRIG_DEPTH(t))

#define ADD_UID_VAR(buf, trig, go, name, context) do { \
		         sprintf(buf, "%c%ld", UID_CHAR, GET_ID(go)); \
                         add_var(&GET_TRIG_VARS(trig), name, buf, context); }   while (0)

#define DRIVER_USES_UNION 0

/* dg_scripts.c*/

obj_data *get_obj_in_room(Room * room, char *name);
Character *get_char_by_obj(obj_data * obj, char *name);
obj_data *get_obj_near_obj(obj_data * obj, char *name);
Character *get_char_in_room(Room * room, char *name);
Character *get_char_near_obj(obj_data * obj, char *name);

