//
// C++ Interface: character
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
int number(int from, int to);
float number(float from, float to);
#define MAKE_STRING(msg) \
   (((ostringstream&) (ostringstream() << boolalpha << msg)).str())
   
class Character {
public:
    int pfilepos;        /* playerfile pos                */
    //mob_rnum nr;         /* Mob's rnum                    */
    mob_vnum vnum;
    bool proto;
    Room * in_room;         /* Location (real room number)   */
    Room * was_in_room;     /* location for linkdead people  */
    int wait;            /* wait for how many loops       */

    struct char_player_data player;     /* Normal data                   */
    struct char_ability_data real_abils;     /* Abilities without modifiers   */
    struct char_ability_data aff_abils; /* Abils with spells/stones/etc  */
    struct char_point_data points; /* Points                        */
    struct char_special_data char_specials;  /* PC/NPC specials        */
    player_special_data *player_specials;  /* PC specials            */
    struct mob_special_data mob_specials;    /* NPC specials           */

    struct combat_skill_data combatskill;

    struct affected_type *affected;     /* affected by what spells       */
    struct obj_data *equipment[NUM_WEARS];   /* Equipment array               */

    struct obj_data *carrying;     /* Head of list                  */
    Descriptor *desc;  /* NULL for mobiles              */

    long id;             /* used by DG triggers             */
    vector<int> *proto_script;    /* list of default triggers      */
    struct script_data *script;    /* script info for the object      */
    struct script_memory *memory;  /* for mob memory triggers         */

    Character *next_in_room;  /* For room->people - list         */
    Character *next;     /* For either monster or ppl-list  */
    Character *next_fighting; /* For fighting list               */

    struct follow_type *followers; /* List of chars followers       */
    Character *master;   /* Who is char following?        */
    long cmd2;           /* These wizcmds aren't saved     */
    byte internal_flags; /* Flags used internally - not saved */
    struct event *pts_event[4]; /* events for regening H/M/V/S     */
    struct event *fight_event;     /*events used for fighting/defending */
    struct event *message_event; /* events used in skill/spell messages*/
    struct sub_task_obj *task;   /* working on a task? This will look after you!*/
    int spell_dir;       /*used for casting directional spells */
    float interact;      /*used for the percentage they land hits and get hit and gain exp in battle */
    int attack_location;
    long loader;         /*id of player who linkloaded them */
    int msg_run;
    int on_task;
    struct note_data *pnote;
    sh_int concealment;
    int has_note[NUM_NOTE_TYPES];
    Character *fuses[TOP_FUSE_LOCATION];
    Character *fused_to;
    struct obj_data *hitched;
    time_t last_move;
    int sweep_damage;
    int body;                   /* body positions aquired */
    byte atk;
    long pulling;
    mob_vnum pet;
    struct travel_point_data *travel_list;
    size_t Send(const char *messg, ...) __attribute__ ((format(printf, 2, 3)));
    size_t Send(string &i);
    size_t Send(string *i);
    size_t Send(stringstream &i);
    gold_int Gold(gold_int amount, int type);
    void send_char_pos(int dam);
    int get_skill(int i);
    void affect_remove(struct affected_type *af);
    void remove_all_affects();
    void free_proto_mob();
    void appear();
    void clear();
    void init();
    void reset();
    void freeself();
    void check_regen_rates();
    void save();
    void init_char_strings();
    void free_char_strings();
    void free_non_proto_strings();

    void affect_total();
    void default_char();
    #if (1)
    template<typename T>
    Character & operator<< (const T & i)  {
        if (this != NULL && desc != NULL) {
            if (send_string == NULL)
                send_string = new stringstream();
            send_string->str("");
            *send_string << i;
            Send(*send_string);
        }
        return *this;
    };
    #else
    Character & operator<< (const stringstream & i)  {
        if (this != NULL && desc != NULL) {
            Send(*send_string);
        }
        return *this;
    };
    #endif

    int skill_roll(int skill_num) {
        return (get_skill(skill_num) > number(1, 101));
    };

    int compute_armor_class() {
        return points.armor < -100 ? -100 : points.armor > 100 ? 100 : points.armor;  /* -100 is lowest */
    };

    Character(bool is_mob = 1);
    ~Character();

    Character * assign (Character *b);
    bool zone_empty();
    void LoadKillList();
    void SaveKillList();
    /* movement functions */
    bool CanMove();
    bool HasBoat();
    bool Flying();
    bool SpaceProtected();
    bool SunProtected();
    bool WaterBreathing();
    Character *NextFightingMe();
	inline bool MountHere() {return (char_specials.riding && char_specials.riding->in_room == in_room);}
	inline bool RiderHere() {return (char_specials.ridden_by && char_specials.ridden_by->in_room == in_room);}
	inline bool MasterHere() {return (master && master->in_room == in_room);}
private:
    stringstream *send_string;

};
