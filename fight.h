

int apply_ac(Character *ch, int eq_pos);
int modify_dam(int dam, Character *ch, Character *vict , int w_type);
int arena_ok(Character *ch, Character *victim);
int find_fe_type(Character *ch);
int has_weapon(Character *ch);
int can_fight(Character *ch, Character *vict, int silent);
int both_pk(Character *a, Character *b);
void kill_list(Character *ch, Character *vict);
int chance_hit_part(Character *ch, int part);

void diag_char_to_char ( Character *i, Character *ch );
void hunt_victim ( Character *ch );

/* prototypes from fight.c */
void set_fighting(Character *ch, Character *victim);
void stop_fighting(Character *ch);
void forget(Character *ch, Character *victim);
void remember(Character *ch, Character *victim);
int skill_message(int dam, Character *ch, Character *vict,
                  int attacktype);
                  
void start_fighting_delay(Character *vict, Character *ch);

#define IS_WEAPON(type) (((type) >= TYPE_HIT) && ((type) <= TYPE_GORE))
#define IS_SPELL_ATK(type) (((type) >= TYPE_ATK_ORB) && ((type) <= TYPE_ATK_TORPEDO))
#define IS_SPELL_CAST(tt) (((tt) > 0) && ((tt) < MAX_SKILLS) && (spell_info[tt].type == 1))
#define IS_SKILL(tt) (((tt) >= 0) && ((tt) < MAX_SKILLS) && (spell_info[tt].type == 2))
#define IS_SUBSKILL(type) (((type) > TYPE_SUFFERING) && ((type) < (TOP_SUB_DEFINE + TYPE_SUFFERING)))
#define IS_OTHERDAM(type) (((type) == TYPE_SUFFERING) || (((type) >= SPELL_BURN) && ((type) <= SPELL_ACID)) \
			|| (((type)>= TYPE_UNDERWATER) && ((type) <= TYPE_DESERT)))



#define IS_TOP_CENTER(pos)  ((pos) == WEAR_NECK_1 ||(pos) == WEAR_NECK_2 ||(pos) == WEAR_HEAD || (pos) == WEAR_FACE \
                          || (pos) == WEAR_EYES || (pos) == WEAR_EAR_R ||(pos) == WEAR_EAR_L || (pos) == WEAR_HORNS || \
			  (pos) == WEAR_ANTENNA )
			  
#define IS_TOP_LEFT(pos)    ((pos) == WEAR_FINGER_L  || (pos) == WEAR_HANDS ||(pos) == WEAR_ARMS \
                          || (pos) == WEAR_SHIELD  ||(pos) == WEAR_WRIST_L ||(pos) == WEAR_ABOUT)
			  
#define IS_TOP_RIGHT(pos)   ((pos) == WEAR_FINGER_R || (pos) == WEAR_HANDS ||(pos) == WEAR_ARMS \
                          || (pos) == WEAR_WRIST_R || (pos) == WEAR_HOLD || (pos) == WEAR_ABOUT)
			  
#define IS_CENTER(pos)      ((pos) == WEAR_NECK_1 ||(pos) == WEAR_NECK_2 ||(pos) == WEAR_BODY || (pos) == WEAR_SHIELD \
                          || (pos) == WEAR_ABOUT || (pos) == WEAR_WAIST ||(pos) == WEAR_TAIL )
			  
#define IS_LOWER_LEFT(pos)  ((pos) == WEAR_LEGS ||(pos) == WEAR_FEET ||(pos) == WEAR_LEGS_2 || (pos) == WEAR_FEET_2 \
                          || (pos) == WEAR_ABOUT || (pos) == WEAR_ANKLE_L ||(pos) == WEAR_TAIL )
			  
#define IS_LOWER_RIGHT(pos) ((pos) == WEAR_LEGS ||(pos) == WEAR_FEET ||(pos) == WEAR_LEGS_2 || (pos) == WEAR_FEET_2 \
                          || (pos) == WEAR_ABOUT || (pos) == WEAR_ANKLE_R ||(pos) == WEAR_TAIL )

#define FE_TYPE_MELEE -1
#define FE_TYPE_SKILL -2
#define FE_TYPE_SPELL -3
#define FE_TYPE_UNDEAD -4
#define FE_TYPE_ANIMAL -5
#define PULSES_PER_FIGHT (6.5*PASSES_PER_SEC)

#define SHIELD_BLOCK     0
#define SHIELD_REFLECT   1
#define SHIELD_EVADE     2

#define TOP_SPEED_VALUE 850.0f
#define TOP_MOB_SPEED_VALUE 850.0f

#define MAX_MOB_DAM 15000
#define MAX_PLAYER_DAM 3000

#define MAX_STAFF_MULTI 2.5f

#define LONG_WEP_MULTI  0.2f
#define SHORT_WEP_MULTI_ROGUE 0.2f
#define SINGLE_WEP_BONUS 1.1f

#define NO_WEP     0
#define SINGLE_WEP 1
#define DUAL_WEP   2

#define DAM_SPEED_MULTI(ch)    (1.0+(GET_SPEED(ch) > TOP_SPEED_VALUE ? (((float)GET_SPEED(ch)-TOP_SPEED_VALUE)/100.0f) : 0))

int is_short_wep(struct obj_data *obj);
#define IS_SHORT_WEP(obj) (is_short_wep(obj))
#define IS_LONG_WEP(obj) (!is_short_wep(obj))

enum body_parts_list {
PART_HEAD,
PART_FACE,
PART_THROAT,
PART_LEFT_ARM,
PART_LEFT_SHOULDER,
PART_RIGHT_ARM,
PART_RIGHT_SHOULDER,
PART_TORSO,
PART_ABDOMEN,
PART_LEFT_LEG,
PART_RIGHT_LEG,
PART_MAX
};

enum body_areas_list {
PART_TOP_CENTER,
PART_TOP_LEFT,
PART_TOP_RIGHT,
PART_CENTER,
PART_LOWER_LEFT,
PART_LOWER_RIGHT,
PART_AREA_MAX
};

#define WEP_STANDARD 	0
#define WEP_KNIFE	1
#define WEP_DAGGER	2
#define WEP_SHORTSWORD  3
#define WEP_LONGSWORD   4
#define WEP_KATANA      5
#define WEP_RAPIER      6
#define WEP_CUTLASS     7
#define WEP_BROADSWORD  8
#define WEP_HALFAXE     9
#define WEP_AXE         10
#define WEP_WARHAMMER   11
#define WEP_MACE        12
#define WEP_SHORTSTAFF  13
#define WEP_STAFF       14
#define WEP_WHIP        15
#define WEP_CLUB        16
#define WEP_TEETH       17
#define WEP_CLAWS       18
#define WEP_PROJECTILE  19
//Adding greatsword as a weapon type --> Prom
#define WEP_GREATSWORD  20
// Increasing this by 1 just in case (21 to 22)--> Prom
#define MAX_WEAPON_TYPES 22

#define ONE_HANDED 1
#define TWO_HANDED 2

#define WEP_SPEED 0
#define WEP_DEFENCE 1
#define WEP_ATTACK 2

struct weapon_type_data {
int hands;
int speedtop;
int speedbot;
int evasiontop;
int evasionbot;
int accuracytop;
int accuracybot;
int balance;
const char *name;
};

extern struct weapon_type_data weapon_type_info[MAX_WEAPON_TYPES];


/*
"Standard" "Knife" "Dagger" "Shortsword" "Longsword" "Katana" "Rapier" "Cutlass" "Broadsword" "Half-Axe" "Double-Axe" "War-Hammer" "Mace" "Shortstaff" "Staff" "Whip" 
*/
/* fight.c protos -mord */
#define ATTACK_MAGIC 1
#define ATTACK_SKILL 2
#define ATTACK_MELEE 3
int attack_type(char chclass);
void start_fighting(Character* ch, Character* vict);

int fe_solo_damage(Character* ch, Character* vict, int damage, int w_type);
int fe_melee_hit(Character* ch, Character* vict, int type, int melee);
int fe_spell_hit(Character* ch, Character* vict, int type);
int fe_deal_damage(Character* ch, Character* vict, int dam, int w_type);
void stop_fighting(Character* ch);
int attack_type(char chclass);

void group_gain(Character *ch, Character *victim);
void solo_gain(Character *ch, Character *victim,
               bool missile);
void die(Character *ch, Character *killer);
float backstab_mult(int level, int tier);
float cleave_mult(int level, int tier);
float slit_mult(int level, int tier);
float skill_type_multi ( Character *ch, Character *vict, int type );

/* Weapon attack texts */
/* Attacktypes with grammar */

struct attack_hit_type
{
  const char *singular;
  const char *plural;
};


extern struct attack_hit_type attack_hit_text[];
