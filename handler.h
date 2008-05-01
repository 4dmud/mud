/* ************************************************************************
*   File: handler.h                                     Part of CircleMUD *
*  Usage: header file: prototypes of handling and utility functions       *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

/* handling the affected-structures */
void affect_modify(Character *ch, byte loc, int mod,
                   bitvector_t bitv, bool add);
void affect_to_char(Character *ch, struct affected_type *af);
void affect_from_char(Character *ch, int type);
bool affected_by_spell(Character *ch, int type);
void affect_join(Character *ch, struct affected_type *af,
                 bool add_dur, bool avg_dur, bool add_mod, bool avg_mod);

void remove_all_normal_affects(Character *ch);
/* utility */
const char *money_desc(gold_int amount);
struct obj_data *create_money(gold_int amount);
int isname(const char *str, const char *namelist);
int is_name(const char *str, const char *namelist);
int isname_hard ( const char *str, const char *namelist );
int isname_full(const char *strlist, const char *namelist);
char *fname(const char *namelist);
int get_number(char **name);
int get_number(const char **name);
char * str_until(char *strlist, const char *key, char *newstr, size_t len);
int begins_with_number(char * str);

/* ******** objects *********** */

void obj_to_char(struct obj_data *object, Character *ch);
void obj_to_char_no_weight(struct obj_data *object, Character *ch);
int obj_from_char(struct obj_data *object);

int equip_char(Character *ch, struct obj_data *obj, int pos);
struct obj_data *unequip_char(Character *ch, int pos);
int invalid_align(Character *ch, struct obj_data *obj);


struct obj_data *get_obj_in_list_num(int num, struct obj_data *list);
struct obj_data *get_obj_num(obj_rnum nr);

void obj_to_room(struct obj_data *object, room_rnum room);
void obj_from_room(struct obj_data *object);
void obj_to_obj(struct obj_data *obj, struct obj_data *obj_to);
int obj_from_obj(struct obj_data *obj);
void object_list_new_owner(struct obj_data *list, Character *ch);

void extract_obj(struct obj_data *obj);
void extract_obj_q(struct obj_data *obj);

/* ******* characters ********* */

Character *get_char_room(const char *name, int *number, room_rnum room);
Character *get_char_num(mob_vnum nr);
//Character *get_char(char *name);
Character *get_char(const char *name);

Character *get_player_room(room_rnum room, char *name, int *number,
                                        int inroom);
Character *get_room_vis(room_rnum room, char *name, int *number, Character *ch = NULL);
Character *find_in_dir(room_rnum room, char *name, int dir, Character *ch = NULL);   //in magic.c
int move_char_to(Character *ch, room_rnum room);
void char_from_room(Character *ch);
void char_to_room(Character *ch, room_rnum room);
void extract_char ( Character *ch, int e_now = 0 );
void extract_char_final(Character *ch);
void extract_pending_chars(void);
void remove_hunter(Character *ch);
void add_hunter(Character *ch);

/* find if character can see */
Character *get_char_room_vis(Character *ch, char *name,
                                          int *number);
Character *get_player_vis(Character *ch, char *name,
                                       int *number, int inroom);

Character *get_char_vis(Character *ch, char *name,
                                     int *number, int where);
struct obj_data *get_obj_in_list_vis(Character *ch, char *name,
                                           int *number, struct obj_data *list);
struct obj_data *get_obj_vis(Character *ch, char *name, int *num);
struct obj_data *get_obj_in_equip_vis(Character *ch, char *arg,
                                            int *number,
                                            struct obj_data *equipment[]);
int get_obj_pos_in_equip_vis(Character *ch, char *arg, int *num,
                             struct obj_data *equipment[]);


/* find all dots */

int find_all_dots(char *arg);

#define FIND_INDIV  0
#define FIND_ALL    1
#define FIND_ALLDOT 2


const char *simple_class_name(Character *ch);
/* Generic Find */

int generic_find(char *arg, bitvector_t bitvector, Character *ch,
                 Character **tar_ch, struct obj_data **tar_obj);

#define FIND_CHAR_ROOM     (1 << 0)
#define FIND_CHAR_WORLD    (1 << 1)
#define FIND_OBJ_INV       (1 << 2)
#define FIND_OBJ_ROOM      (1 << 3)
#define FIND_OBJ_WORLD     (1 << 4)
#define FIND_OBJ_EQUIP     (1 << 5)
#define FIND_CHAR_NOTINROOM (1 << 6)

//Max number of items alloud in houses
#define MAX_HOUSE_ITEMS  500

Character *check_ch(Character *ch);

/* prototypes from crash save system */
int delete_pobj_file(const char *name);
void delete_aliases(const char *charname);
void delete_variables(const char *charname);
int Crash_get_filename(const char *orig_name, char *filename);
int Crash_delete_file(const char *name);
int Crash_delete_crashfile(Character *ch);
int Crash_clean_file(const char *name);
void Crash_listrent(Character *ch,const char *name);
int Crash_load(Character *ch);
void Crash_crashsave(Character *ch);
void Crash_idlesave(Character *ch);
void Crash_save_all(void);
int Crash_load_xapobjs(Character *ch);


void stop_follower(Character *ch);

int lock_desc(Descriptor *d);
int unlock_desc(Descriptor *d);
int is_locked(Descriptor *d);

