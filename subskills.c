/*subskills by mordecai@xtra.co.nz */

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "interpreter.h"
#include "spells.h"
#include "handler.h"
#include "comm.h"
#include "db.h"
#include "dg_scripts.h"
#include "constants.h"
#include "descriptor.h"



/* external variables */
extern struct spell_info_type spell_info[];
extern int pt_allowed;
SPECIAL(shop_keeper);
/* extern variables */
extern Descriptor *descriptor_list;
extern int pk_allowed;

/* external functions */
int arena_ok(Character *ch, Character *victim);
void improve_skill(Character *ch, int skill);
int compute_armor_class(Character *ch);
void spello(int spl, const char *name, int max_mana, int min_mana,
            int mana_change, int minpos, int targets, int violent,
            int routines, int wait, const char *message, int pulse);
int find_first_step(room_rnum src, room_rnum target);
ACMD(do_gen_door);
void send_not_to_spam(char *buf, Character *ch,
                      Character *victim, struct obj_data *weap,
                      int spam);
void check_killer(Character *ch, Character *vict);
int has_class(Character *ch, int chclass);
int tier_level(Character *ch, int chclass);
int perform_push(Character *ch, int dir, int need_specials_check,
                 Character *attacker);
void log_push_to_death(Character *ch, Character *attacker);
ACMD(do_assist);
int has_weapon(Character *ch);
//-----

/*local variables*/
enum subskill_list subskill;

struct sub_skill_info_type sub_info[TOP_SUB_DEFINE];

/* local functions */
int toggle_sub_status(Character *ch, int i, int onoff);
int get_sub_status(Character *ch, int i);
int subkillset(Character *ch, enum subskill_list subcmd, int amount);
void improve_sub(Character *ch, enum subskill_list  sub, int amount);
int total_sub_chance(Character *ch, enum subskill_list  sub);
void unused_sub(enum subskill_list subcmd);

void subo(enum subskill_list subcmd, const char *name, int stat, int cost,
          int percent, int minpos, int targets, byte violent, int routines,
          int stat_type, int flags, int cl_type, int perent);

const char *sub_name(int num);
int sub_number(char *name);
int init_subskills(void);
void assign_subskills(void);

ACMD(do_subskill);
int sub_success = TRUE;
/*all the subskills*/
ASUB(sub_fury_attacks);
ASUB(sub_drain_blood);
ASUB(sub_juggle);
ASUB(sub_throttle);
ASUB(sub_tunneling);
ASUB(sub_sweep_attack);

const char *unused_subname = "<UNUSED SUB>";	/* So we can get &unused_subname */

#define TIER (current_class_is_tier_num(ch))
#define CALL_SUB(sname) sub_success = ( sname(ch, vict, obj, arg)); //arg was argument it is a single argument check this

#define SK_NONE             0
#define SK_VIOLENT          (1 << 0)
#define SK_NEED_WEAPON      (1 << 1)

#define CL_TYPE_NONE       0 /* all/any */
#define CL_TYPE_CASTER    (1 << 0)
#define CL_TYPE_FIGHTER   (1 << 1)
#define CL_TYPE_ROGUE     (1 << 2)
#define CL_TYPE_UNDEAD    (1 << 3)
#define CL_TYPE_ANIMAL    (1 << 4)
#define CL_TYPE_CLAN      (1 << 5)
#define CL_TYPE_RACE      (1 << 6)


const char *sub_name(int num)
{
  if (num > 0 && num < TOP_SUB_DEFINE)
    return (sub_info[num].name);
  else if (num == -1)
    return ("UNUSED");
  else
    return ("UNDEFINED");
}

int sub_number(char *name)
{
  int skindex, ok;
  char *temp, *temp2;
  char first[256], first2[256], tempbuf[256];

  for (skindex = 1; skindex <= TOP_SUB_DEFINE; skindex++)
  {
    if (is_abbrev(name, sub_name(skindex)))
      return (skindex);

    ok = TRUE;
    strlcpy(tempbuf, sub_name(skindex), sizeof(tempbuf));	/* strlcpy: OK */
    temp = any_one_arg(tempbuf, first);
    temp2 = any_one_arg(name, first2);
    while (*first && *first2 && ok)
    {
      if (!is_abbrev(first2, first))
        ok = FALSE;
      temp = any_one_arg(temp, first);
      temp2 = any_one_arg(temp2, first2);
    }

    if (ok && !*first2)
      return (skindex);
  }

  return (-1);
}

/*
#define STAT_SUB_HP  (1 << 0)
#define STAT_SUB_MA  (1 << 1)
#define STAT_SUB_MV  (1 << 2)
*/
/*
number the define of the subskill
the name of the subskill
what stats the subskill will deduct cost from
the cost in points
the cost in percentage
the minimum position
the targets
bool violent (1 = yes it is violent, 0 = no)
routines
pulse, this is a modifyer for speed, reducing or increasing the time till next attack after this one is called
val 1 - the dice size bonus
val 2 - the dice num bonus
cl_type - the class type, includes races and
*/
#define PEAC 0 // peaceful - rather.. not violent
#define VIOL 1 // violent

#define PASS_UNITS (PASSES_PER_SEC/4)

void assign_subskills(void)
{
  int i;
  for (i = (int)SUB_UNDEFINED; ((enum subskill_list) i) < TOP_SUB_DEFINE; i++)
    unused_sub((enum subskill_list)i);

  /* gives a chance of avoiding sanctuary */
  subo(SUB_SWEEP_ATTACK, "SweepAttack", 0, 0,
       0, POS_FIGHTING, TAR_IGNORE , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO | SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);

  /* gives a chance of avoiding sanctuary */
  subo(SUB_REPEL_SANC, "RepelSanctuary", 0, 0,
       0, POS_FIGHTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO | SUB_TYPE_DEFAULT_ON, CL_TYPE_FIGHTER, PROF_COMBATANT);

  /* gives a chance of not dying when killed - allowing healing */
  subo(SUB_UNDYING, "Undying", 0, 0,
       0, POS_FIGHTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO | SUB_TYPE_DEFAULT_ON, CL_TYPE_FIGHTER, PROF_COMBATANT);

  /* your ability to control living animals */
  subo(SUB_BRICKMAKING, "BrickMaking", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_BRICKMAKER);
  /*
  3. TRIP could be a nice thief skill too, a bit like bash, but not as effective. */
  subo(SUB_TRIP, "Trip", 0, 0,
       0, POS_FIGHTING, 0 , VIOL,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
  /* an enhancement to strangle */
  subo(SUB_THROTTLE, "Throttle", 0, 0,
       0, POS_STANDING, 0 , VIOL,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_CMD, CL_TYPE_NONE, SKILL_STRANGLE);
  /*
   MASK 
  - This hides the name of the user in the long desc, making the room message: 
   'A masked figure is standing here' instead of the usual 'Playername is standing 
   here'.
   In any action the masked person would show as 'The masked figure' for instance 
   'The masked figure says,'...  or 'The mask figures coughs conspicioualy.'
  - In the WHO list the player shows up only as 'A masked figure'.
  - The player has to actually WEAR a mask for the skill to work.
  - While mask is toggled, this person cannot be attacked unless someone guesses who 
   it is, and types in the right, FULL name. 'Murder Once' would work, but not 
   'murder o'
  - Attacking or being attacked immediately breaks MASK. If possible with the message 
   'You rip the mask from Playername's face.' or 'Playername sheds the mask with a 
   diabolic laugh.'
   */
  subo(SUB_MASK, "Mask", 0, 0,
       0, POS_STANDING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, SKILL_HIDE);
  /*
  CLOAK 
  - This adds HIDING to MASK, so the cloaked person only can be seen sense_life on.
   The long desc (to these with sense_life) would say 'A cloaked figure is hiding here.'
   (Otherwise it works the same as MASK).
  - In the WHO list the player shows up as 'A cloaked figure'.
  */
  subo(SUB_CLOAK, "Cloak", 0, 0,
       0, POS_STANDING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, SKILL_HIDE);
  /*
  SHADOWCLOAK
  - This adds invisibility, to MASK and CLOAK. People with both sense_life and 
   detect_invis on would see: 'The shadows loom deep in one corner of the room'. 
  - In the WHO list the player shows up as 'An invisible lurcher'.
  */
  subo(SUB_SHADOWCLOAK, "ShadowCloak", 0, 0,
       0, POS_STANDING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, SKILL_HIDE);

  /* your abilty to get the beast to follow you */
  subo(SUB_BEASTTAME, "BeastTame", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD | SUB_TYPE_PROF, CL_TYPE_NONE, PROF_BEASTMASTER);
  /* you ability to order an animal around */
  subo(SUB_BEASTTONGUE, "BeastTongue", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_BEASTMASTER);
  /* your ability to control living animals */
  subo(SUB_BEASTMASTER, "BeastMaster", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_BEASTMASTER);
  /* give tasks to NPC's */
  subo(SUB_TASKMASTER, "TaskMaster", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MANAGEMENT);
  /*abiliy to control this being */
  subo(SUB_UNDEAD_OVERLORD, "UnDeadOverlord", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_NECROMANCER);
  /*your ability to control this being */
  subo(SUB_UNDEAD_DEMON, "UnDeadDemon", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_NECROMANCER);
  /* your control over minor demons */
  subo(SUB_UNDEAD_MINORDEMON, "UnDeadMinorDemon", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_NECROMANCER);
  /*can you control one of these if you summon it? */
  subo(SUB_UNDEAD_DRAGON, "UnDeadDragon", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_NECROMANCER);
  /* your control over undead */
  subo(SUB_UNDEAD_MASTER, "UnDeadMaster", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_NECROMANCER);
  /*summons an undead being (random being depending on your ability, start of small with rats and birds etc) */
  subo(SUB_UNDEAD_SUMMON, "UnDeadSummon", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_NECROMANCER);
  /* worship the undead, increase your undead powers */
  subo(SUB_UNDEAD_WORSHIP, "UnDeadWorship", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_NECROMANCER);

  /*will the dragon let others be taken along while you master it? */
  subo(SUB_DRAGON_PASSENGERS, "DragonPassengers", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_DRAGONMASTER);
  /*will your dragon accept big items? */
  subo(SUB_DRAGON_CARGO, "DragonCargo", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_DRAGONMASTER);
  /*if your dragon can fly, you need this skill to be able to liftoff! */
  subo(SUB_DRAGON_FLYING, "DragonFly", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_DRAGONMASTER);
  /*does the dragon stay in your control? */
  subo(SUB_DRAGON_MASTERY, "DragonMastery", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_DRAGONMASTER);
  /*your ability to put one under your control */
  subo(SUB_DRAGON_ENCHANTING, "DragonEnchant", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_DRAGONMASTER);
  /*your chance at mounting one of these motherfuckers */
  subo(SUB_DRAGON_MOUNTING, "DragonMounting", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_DRAGONMASTER);
  /*summons a dragon */
  subo(SUB_DRAGON_SUMMONING, "DragonSummon", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_DRAGONMASTER);
  /* will it obey you? */
  subo(SUB_DRAGON_TAMING, "DragonTame", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_DRAGONMASTER);
  /*lets you strangle using an item*/
  subo(SUB_GAROTTE, "Garotte", STAT_SUB_MV, 100 ,
       10, POS_STANDING, TAR_FIGHT_SELF, PEAC,
       SK_VIOLENT,STATUS_TYPE_ONOFF, SUB_TYPE_CMD , CL_TYPE_NONE, SKILL_STRANGLE);
  /*

  SUB_TUMBLE,
  SUB_CLOWN,
  SUB_TRAPEZE,
  SUB_CHARCOALBURNER,
  */

  subo(SUB_CHARCOALBURNER, "CharcoalBurn", STAT_SUB_MV, 100 ,
       10, POS_STANDING, TAR_CHAR_ROOM | TAR_IGNORE, PEAC,
       SK_NONE,STATUS_TYPE_ONOFF, SUB_TYPE_CMD | SUB_TYPE_PROF, CL_TYPE_NONE, PROF_WOODSMAN);

  /*swing ur way into the hearts of your audience as they hold their breath in amazement as you swing from great heights!
    for gold and exp of course */
  subo(SUB_TRAPEZE, "Trapese", STAT_SUB_MV, 100 ,
       10, POS_STANDING, TAR_CHAR_ROOM | TAR_IGNORE, PEAC,
       SK_NONE,STATUS_TYPE_ONOFF, SUB_TYPE_CMD | SUB_TYPE_PROF, CL_TYPE_NONE, PROF_PERFORMER);

  /* Physical humor in the form of a stuid player, make your audience laugh for exp and gold */
  subo(SUB_CLOWN, "Clown", STAT_SUB_MV, 100 ,
       10, POS_STANDING, TAR_CHAR_ROOM | TAR_IGNORE, PEAC,
       SK_NONE,STATUS_TYPE_ONOFF, SUB_TYPE_CMD | SUB_TYPE_PROF, CL_TYPE_NONE, PROF_PERFORMER);

  /* fall about for an audiance to be amazed for exp and gold */
  subo(SUB_TUMBLE, "Tumble", STAT_SUB_MV, 100 ,
       10, POS_STANDING, TAR_CHAR_ROOM | TAR_IGNORE, PEAC,
       SK_NONE,STATUS_TYPE_ONOFF, SUB_TYPE_CMD | SUB_TYPE_PROF, CL_TYPE_NONE, PROF_PERFORMER);

  /* perform feats of balance to an audience, the more advanced you become at this the more you can get wow bonuses from your audience, for gold, tokens and exp */
  subo(SUB_BALANCE, "Balance", STAT_SUB_MV, 100 ,
       10, POS_STANDING, TAR_CHAR_ROOM | TAR_IGNORE, PEAC,
       SK_NONE,STATUS_TYPE_ONOFF, SUB_TYPE_CMD | SUB_TYPE_PROF, CL_TYPE_NONE, PROF_PERFORMER);

  /*lets you juggle for exp or money*/
  subo(SUB_JUGGLE, "Juggle", STAT_SUB_MV, 100 ,
       10, POS_STANDING, TAR_CHAR_ROOM | TAR_IGNORE, PEAC,
       SK_NONE,STATUS_TYPE_ONOFF, SUB_TYPE_CMD | SUB_TYPE_PROF, CL_TYPE_NONE, PROF_PERFORMER);
  /*enables use of stun weapons -- wolfsbane*/
  subo(SUB_STUN_PHASER, "StunPhaser",STAT_SUB_HP | STAT_SUB_MV, 100 ,
       10, POS_FIGHTING, TAR_FIGHT_SELF, PEAC,
       SK_VIOLENT,STATUS_TYPE_ONOFF, SUB_TYPE_AUTO | SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MISC);
  /*causes an affect on self when full moon*/
  subo(SUB_LYCAN, "Lycanthropy", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_VAMPIRE);

  /*causes an affect on self when fighting*/
  subo(SUB_FURY_ATTACKS, "FuryAttack", STAT_SUB_HP | STAT_SUB_MV, 100 ,
       10, POS_FIGHTING, TAR_FIGHT_SELF, PEAC,
       SK_VIOLENT,STATUS_TYPE_ONOFF, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_VAMPIRE);

  /*increases regen rate at night */
  subo(SUB_NIGHT_REGEN, "NightRegen", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_VAMPIRE);

  /*causes an affect on self when fighting*/
  subo(SUB_DRAIN_BLOOD, "DrainBlood", STAT_SUB_HP | STAT_SUB_MV | STAT_SUB_MA, 100 ,
       10, POS_FIGHTING, TAR_FIGHT_SELF, PEAC,
       SK_VIOLENT, STATUS_TYPE_ONOFF, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_VAMPIRE);

  /*chance of getting extra gold when killing someone */
  subo(SUB_PILLAGE, "Pillage", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);
  /*refine crude gems into more pure gems */
  subo(SUB_TUNNELING, "Tunneling", 0, 0,
       0, POS_STANDING, TAR_IGNORE , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MINER);
  /*refine base ore into a more pure state */
  subo(SUB_REFINE_ORE, "RefineOre", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MINER);
  /*refine crude gems into more pure gems */
  subo(SUB_REFINE_GEMS, "RefineGems", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MINER);
  /*reprocess the ore into its elemental makeup*/
  subo(SUB_REPROCESS_ORE, "ReprocessOre", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MINER);
  /*reprocess gems into their elemental makeup*/
  subo(SUB_REPROCESS_GEMS, "ReprocessGems", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MINER);
  /* ability to work a factory, reprocessing machines*/
  subo(SUB_INDUSTRY, "Industry", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_BUSINESS);
  /* ability to buy and sell on the market*/
  subo(SUB_MARKETING, "Marketing", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE,STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMMERCE);
  /* ability to get better deals when buys and selling*/
  subo(SUB_BARTER, "Barter", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_BUSINESS);
  /* ability to mine in soft areas (packed earth/mudstone/sand/sandstone)*/
  subo(SUB_SOFT_MINING, "SoftMining", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MINER);
  /* ability to mine in hard areas (basalt/Slate/Phyllite/Schist/Gneiss)*/
  subo(SUB_HARD_MINING, "HardMining", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MINER);
  /* skill to shapen weapons, increase their damage*/
  subo(SUB_SHARPENING, "Sharpening", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, SKILL_TINKER);
  /* ability to refine skins to leather, manipulate leather */
  subo(SUB_LEATHERWORK, "LeatherWork", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_TANNER);
  /* ability to make clothing from materials*/
  subo(SUB_TAILORING, "Tailoring", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_TAILOR);
  /* ability to survey a room to check what types of minerals are abundant,
     also counts as special bonus in new mine code */
  subo(SUB_SURVEYING, "Surveying", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MINER);
  /* ability to create/work with electronic equipment */
  subo(SUB_ELECTRONICS, "Electronics", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_TECH);
  /* understanding of the use of small shields, lowering hinderance of using a shield*/
  subo(SUB_SMALL_SHIELDS, "SmallShields", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);
  /* the same as small shields, for large shields */
  subo(SUB_LARGE_SHIELDS, "LargeShields", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);
  /* ability to understand forign text's */
  subo(SUB_DECYPHERING, "Decyphering", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_TECH);
  /* ability to use heavy weapons with greater speed*/
  subo(SUB_HEAVY_WEAPONS, "HeavyWeapons", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);
  /* ability to use light weapons with greater damage */
  subo(SUB_LIGHT_WEAPONS, "LightWeapons", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);
  /* ability to make beer/wine from raw materials */
  subo(SUB_BREWING, "Brewing", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_BREWER);
  /* ability to watch a group, letting them perform a task with greater efficency */
  subo(SUB_SUPERVISING, "Supervising", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MANAGEMENT);
  /* ability to manage a group, letting them perform a task with greater results */
  subo(SUB_MANAGEMENT, "Management", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MANAGEMENT);
  /* ability to lead a group, letting them perform a task at less cost */
  subo(SUB_LEADERSHIP, "Leadership", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MANAGEMENT);
  /* ability to co-ordinate a group, letting them perform multiple tasks */
  subo(SUB_GROUP_COORDINATION, "GroupCoordination", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MANAGEMENT);
  /* ability to use (high weight/ac) armor for most protection and speed */
  subo(SUB_HEAVY_ARMOR, "HeavyArmor", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);
  /* ability to use (medium weight/ac) armor for most protection and speed */
  subo(SUB_MEDIUM_ARMOR, "MediumArmor", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);
  /* ability to use (light weight/ac) armor for most protection and speed */
  subo(SUB_LIGHT_ARMOR, "LightArmor", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);
  /*ability to ignite a lightsaber*/
  subo(SUB_LIGHTSABER_PROF, "Lightsaber proficency", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MISC);
  /*ability to ignite both ends of a lightsaber*/
  subo(SUB_LIGHTSABER_DOUBLE, "Lightsaber Double-ended", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MISC);
  /* reduction in speed when fighting in ships */
  subo(SUB_DOGFIGHTING, "Dogfighting", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_PILOT);
  /* ability to gamble in the streets with other mobs and players to earn money, tokens and exp*/
  subo(SUB_COCKFIGHTING, "Cockfighting", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MISC);
  /* Dirty fighting moves learnt on the streets - increased damage */
  subo(SUB_STREETFIGHTING, "Streetfighting", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_COMBATANT);
  /* find where a undergound spring is */
  subo(SUB_DEVINE_WATER, "DevineWater", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_WOODSMAN);
  /* create a well to tap into an underground spring */
  subo(SUB_CREATE_WELL, "CreateWell", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_WOODSMAN);
  /* affect sight and distance */
  subo(SUB_OPTICS, "Optics", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_GLAZIER);
  /* Mill lumber into planks/posts/pulp */
  subo(SUB_MILLING, "Milling", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MILLER);
  /* create writing paper from pulp*/
  subo(SUB_PAPERMAKING, "Papermaking", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_SCRIBE);
  /* create scrolls from paper */
  subo(SUB_SCROLLMAKING, "Scrollmaking", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_SCRIBE);
  /* control the heat of a furnace */
  subo(SUB_HEAT_MANAGEMENT, "HeatManagement", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_AUTO| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_MISC);
  /* create armor and items from glass */
  subo(SUB_GLASSWORK, "Glasswork", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_GLAZIER);
  /* create liquid containers from glass */
  subo(SUB_BOTTLEMAKING, "BottleMaking", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_GLAZIER);
  /* Create vials for potions from glass bottles */
  subo(SUB_VIALMAKING, "VialMaking", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_GLAZIER);
  /* chop down magic trees to make lumber */
  subo(SUB_LUMBERJACK, "Lumberjack", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD| SUB_TYPE_PROF, CL_TYPE_NONE, PROF_WOODSMAN);
  /* guard magic trees so that no one can alter them */
  subo(SUB_TREEGUARD, "TreeGuard", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, SKILL_SING_WOOD);
  /*reward - gives +50 speed */
  subo(SUB_LOYALSPEED, "BonusSpeed", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_AUTO | SUB_TYPE_DEFAULT_ON, CL_TYPE_NONE, TYPE_UNDEFINED);
  /*reward - gives +10 attack rating */
  subo(SUB_LOYALATTACK, "BonusAttack", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_AUTO | SUB_TYPE_DEFAULT_ON, CL_TYPE_NONE, TYPE_UNDEFINED);
  /*reward - gives +10 defence rating */
  subo(SUB_LOYALDEFEND, "BonusEvasion", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_AUTO | SUB_TYPE_DEFAULT_ON, CL_TYPE_NONE, TYPE_UNDEFINED);
  /*reward - gives +5% damage */
  subo(SUB_LOYALDAMAGE, "BonusDamage", 0, 0,
       0, POS_SLEEPING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_ONOFF, SUB_TYPE_AUTO | SUB_TYPE_DEFAULT_ON, CL_TYPE_NONE, TYPE_UNDEFINED);

  subo(SUB_ASSEMBLE, "Assemble", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_BAKE, "Bake", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_BREW, "Brew", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_CRAFT, "Craft", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_FLETCH, "Fletch", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_KNIT, "Knit", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_MAKE, "Make", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_MIX, "Mix", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_THATCH, "Thatch", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_WEAVE, "Weave", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);
subo(SUB_FORGE, "Forge", 0, 0,
       0, POS_SITTING, 0 , PEAC,
       SK_NONE, STATUS_TYPE_TRAINABLE, SUB_TYPE_CMD, CL_TYPE_NONE, TYPE_UNDEFINED);

}
#undef SINFO
#define SINFO sub_info[(int)subcmd]
ACMD(do_subskill)
{

  Character *vict = NULL, *orig = ch;
  struct obj_data *obj = NULL;
  int pass = 1;
  int chclass;
  bool immort = 0;
  sub_success = FALSE;
  char arg[MAX_STRING_LENGTH];

  /* If switched, orig should point to original char */
  if (ch->desc && ch->desc->original)
    orig = ch->desc->original;

  chclass = GET_CLASS(orig);

  if (SINFO.cl_type && !(IS_SET(SINFO.cl_type, CL_TYPE_RACE)||IS_SET(SINFO.cl_type, CL_TYPE_CLAN)))
  {
    pass = 0;
    if (IS_NPC(orig))
    {
      if (chclass == CLASS_UNDEAD)
        pass += IS_SET(SINFO.cl_type, CL_TYPE_UNDEAD);
      if ( chclass == CLASS_ANIMAL)
        pass += IS_SET(SINFO.cl_type, CL_TYPE_ANIMAL);
      if ( chclass == CLASS_CASTER)
        pass += IS_SET(SINFO.cl_type, CL_TYPE_CASTER);
      if ( chclass == CLASS_FIGHTER)
        pass += IS_SET(SINFO.cl_type, CL_TYPE_FIGHTER);
      if ( chclass == CLASS_ROGUE)
        pass += IS_SET(SINFO.cl_type, CL_TYPE_ROGUE);
    }
    else
    {
      if ( chclass == CLASS_MAGE || chclass == CLASS_PRIEST || chclass == CLASS_ESPER )
        pass += IS_SET(SINFO.cl_type, CL_TYPE_CASTER);
      if ( chclass == CLASS_WARRIOR || chclass == CLASS_HUNTER)
        pass += IS_SET(SINFO.cl_type, CL_TYPE_FIGHTER);
      if ( chclass == CLASS_THIEF || chclass == CLASS_GYPSY || chclass == CLASS_RANGER)
        pass += IS_SET(SINFO.cl_type, CL_TYPE_ROGUE);
    }
  }

  if (!IS_NPC(orig) && GET_LEVEL(orig) > LVL_IMMORT)
    immort = 1;

  if (!(pass || total_sub_chance(orig, (enum subskill_list)subcmd)!=-1) && !immort)
    send_to_char("You have no idea how to do that.\r\n", ch);
  else if (IS_SET(SINFO.routines, SK_VIOLENT) && ROOM_FLAGGED(IN_ROOM(ch), ROOM_PEACEFUL))
    send_to_char("This room just has such a peaceful, easy feeling...\r\n", ch);
  else
  {

    /* Yup, we can. Start the command line parsing */

    one_argument(argument, arg);


    /* Determine Target */
    if (!*arg)
    {
      if (FIGHTING(ch) && IS_SET(SINFO.targets, TAR_FIGHT_VICT) &&
          IN_ROOM(ch) == IN_ROOM(FIGHTING(ch)))
      {
        vict = FIGHTING(ch);
      }
      else if (IS_SET(SINFO.targets, TAR_FIGHT_SELF) ||
               IS_SET(SINFO.targets, TAR_SELF_ONLY))
      {
        vict = ch;
      }
    }
    else
    {
      if (IS_SET(SINFO.targets, TAR_CHAR_WORLD)
          && !(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD)))
      {
        send_to_char("Nobody is around by that name!\r\n", ch);
        return;
      }
      else if (IS_SET(SINFO.targets, TAR_CHAR_ROOM)
               && !(vict =
                      get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)))
      {
        send_to_char("Nobody is here by that name!\r\n", ch);
        return;
      }
    }

    if (!IS_SET(SINFO.targets, TAR_IGNORE) && !vict)
      ch->Send(
                       "%s... aren't we missing something here?\r\n",
                       SINFO.name);
    else if (IS_SET(SINFO.targets, TAR_NOT_SELF) && vict == ch)
      ch->Send( "You can't %s yourself!\r\n", SINFO.name);
    else if (IS_SET(SINFO.targets, TAR_SELF_ONLY) && vict != ch)
      ch->Send( "That can only be done to yourself.\r\n");
    else if (IS_SET(SINFO.routines, SK_NEED_WEAPON)
             && !has_weapon(ch))
      ch->Send(
                       "You need to wield a weapon to make it a success.\r\n");
    else
    {
      if (IS_SET(SINFO.routines, SK_VIOLENT))
      {

        send_not_to_spam("{cg$n engages in combat with $N.{c0", ch,
                         vict, NULL, 0);
      }
      switch (subcmd)
      {
      case SUB_FURY_ATTACKS:
        CALL_SUB(sub_fury_attacks);
        break;
      case SUB_DRAIN_BLOOD:
        CALL_SUB(sub_drain_blood);
        break;
      case SUB_JUGGLE:
        CALL_SUB(sub_juggle);
        break;
      case SUB_TUNNELING:
        CALL_SUB(sub_tunneling);
        break;
      case SUB_SWEEP_ATTACK:
       CALL_SUB(sub_sweep_attack);
       break;
      default:
        ch->Send( "I don't know that subskill.\r\n");
        return;

      }
    }
  }
}


ASUB(sub_fury_attacks)
{
  ch->Send( "This subskill is unfinished!\r\n");
  return FALSE;



}

ASUB(sub_drain_blood)
{
  /** make this a starting attack, the victims hp and adds it to yours **/
  /** You do as much damage as you are damaged **/
 if (GET_SUB(ch, SUB_DRAIN_BLOOD) <= 0)
  {
    ch->Send( "You haven't got that ability!\r\n");
    return SUB_UNDEFINED;
  }
  
  if (get_sub_status(ch, SUB_DRAIN_BLOOD) == STATUS_OFF ) {
  if (GET_ALIGNMENT(ch) > -600) {
  ch->Send( "You focus on draining blood.\r\n");
  toggle_sub_status(ch, SUB_DRAIN_BLOOD, STATUS_ON);
  } else {
  ch->Send( "You aren't evil enough drain blood.\r\n");
  }
  } else {
  ch->Send( "You focus on normal attacks.\r\n");
  toggle_sub_status(ch, SUB_DRAIN_BLOOD, STATUS_OFF);
  }
  return TRUE;
  


}

ASUB(sub_sweep_attack)
{
/** carry the leftover damage from the attack onto the next
    available mob in the room 
    Your skill level in this defines how much new damage you
    add to the attack.
**/

 if (GET_SUB(ch, SUB_SWEEP_ATTACK) <= 0)
  {
    ch->Send( "You haven't got that ability!\r\n");
    return SUB_UNDEFINED;
  }
  
  if (get_sub_status(ch, SUB_SWEEP_ATTACK) == STATUS_OFF ) {
  if (speed_update(ch) > 500) {
  ch->Send( "You focus on sweeping attacks.\r\n");
  toggle_sub_status(ch, SUB_SWEEP_ATTACK, STATUS_ON);
  } else {
  ch->Send( "You aren't speedy enough to focus on sweeping attacks.\r\n");
  }
  } else {
  ch->Send( "You focus on normal attacks.\r\n");
  toggle_sub_status(ch, SUB_SWEEP_ATTACK, STATUS_OFF);
  }
  return TRUE;
  


}

ACMD(do_ignite)
{
  struct obj_data *hilt = NULL, tmpobj;
  struct obj_data *sabre = NULL;
  char buf2[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];
  int counter = 0, pos = NOWHERE;
  int pass = 0;

  if ((hilt = GET_EQ(ch, WEAR_WIELD)) != NULL && ( GET_OBJ_TYPE(hilt) == ITEM_LIGHTSABRE_HILT))
    pass = 1;
  if (pass == 0 && ((hilt = GET_EQ(ch, WEAR_WIELD_2)) != NULL && GET_OBJ_TYPE(hilt) == ITEM_LIGHTSABRE_HILT))
    pass = 1;

  if (pass == 0)
  {
    ch->Send( "You don't have the right equipment to do that.\r\n");
    return;
  }

  if (GET_SUB(ch, SUB_LIGHTSABER_PROF) > 0)
  {
    ch->Send(
                     "With a snap-hiss your glowing blade extends to it's full length, filling the area with a subtle hum.\r\n");
    act("You hear a loud hiss, followed by an everpresent hum, as $n's lightsaber extends to full length.", FALSE, ch, 0, 0, TO_ROOM);
  }
  else
  {
    ch->Send( "You can't figure out where the on button is!\r\n");
    return;
  }
  if (GET_OBJ_VAL(hilt, 0) < 1 || GET_OBJ_VAL(hilt, 0) > 2)
  {
    ch->Send( "Your sabre seems to be broken!\r\n");
    return;
  }

  sabre = create_obj();

  pos = hilt->worn_on;
  if ((hilt = unequip_char(ch, pos)) == NULL)
    return;

  if (sabre->name)
    free(sabre->name);
  if (sabre->description)
    free(sabre->description);
  if (sabre->short_description)
    free(sabre->short_description);

  snprintf(buf, sizeof(buf), "%s %s lightsaber", hilt->name, color_option_name((int) GET_OBJ_VAL(hilt, 3)));
  sabre->name = str_dup(buf);

  snprintf(buf2,sizeof(buf2), "a %s bladed %s lightsaber", GET_OBJ_VAL(hilt, 0)==1 ? "single" : "double", color_option_name((int) GET_OBJ_VAL(hilt, 3)));
  sabre->short_description = str_dup(buf2);

  snprintf(buf2,sizeof(buf2), "a %s bladed %s lightsaber lies here", GET_OBJ_VAL(hilt, 0)==1 ? "single" : "double", color_option_name((int) GET_OBJ_VAL(hilt, 3)));
  sabre->description = str_dup(buf2);
  sabre->item_number = hilt->item_number;
  GET_OBJ_WAS(sabre) = GET_OBJ_VNUM(hilt);
  GET_OBJ_TYPE(sabre) = ITEM_WEAPON;
  SET_BIT_AR(GET_OBJ_EXTRA(sabre), ITEM_HUM);
  SET_BIT_AR(GET_OBJ_EXTRA(sabre), ITEM_UNIQUE_SAVE);
  SET_BIT_AR(GET_OBJ_EXTRA(sabre), ITEM_LIGHTSABRE);
  GET_OBJ_VAL(sabre, 0) = GET_OBJ_VNUM(hilt);
  GET_OBJ_VAL(sabre, 1) = GET_OBJ_VAL(hilt, 1);
  GET_OBJ_VAL(sabre, 2) = GET_OBJ_VAL(hilt, 2);
  GET_OBJ_VAL(sabre, 3) = 3; //slash
  GET_OBJ_COST(sabre) = GET_OBJ_COST(hilt);
  GET_OBJ_WEIGHT(sabre) = GET_OBJ_WEIGHT(hilt);
  GET_OBJ_RENT(sabre) = 0;
  GET_OBJ_TIMER(sabre) = -1;

  for (counter = 0; counter < TW_ARRAY_MAX; counter++)
    GET_OBJ_WEAR(sabre)[counter] = GET_OBJ_WEAR(hilt)[counter];

  for (counter = 0; counter < MAX_OBJ_AFFECT; counter++)
    if (hilt->affected[counter].modifier)
    {
      sabre->affected[counter].location = hilt->affected[counter].location;
      sabre->affected[counter].modifier = hilt->affected[counter].modifier;
    }
  
  /*transfer script data? Could it be this easy?*/
  /*
  remove_from_lookup_table(GET_ID(sabre));
  remove_from_lookup_table(GET_ID(hilt));
  sabre->id = hilt->id;			// used by DG triggers              
  add_to_lookup_table(GET_ID(sabre), (void *) sabre);
  hilt->id = 0;
  sabre->proto_script = hilt->proto_script;
  sabre->script = hilt->script;
  hilt->proto_script = NULL;
  hilt->script = NULL;



  extract_obj(hilt);
  */
  	/* move new obj info over to old object and delete new obj */
	memcpy(&tmpobj, sabre, sizeof(*sabre));
	tmpobj.in_room = IN_ROOM(hilt);
	tmpobj.carried_by = hilt->carried_by;
	tmpobj.worn_by = hilt->worn_by;
	tmpobj.in_locker = hilt->in_locker;
	tmpobj.worn_on = hilt->worn_on;
	tmpobj.in_obj = hilt->in_obj;
	tmpobj.contains = hilt->contains;
	tmpobj.id = hilt->id;
	tmpobj.proto_script = hilt->proto_script;
	tmpobj.script = hilt->script;
	tmpobj.next_content = hilt->next_content;
	tmpobj.next = hilt->next;
	memcpy(hilt, &tmpobj, sizeof(*hilt));
	hilt->description = sabre->description;
	hilt->short_description = sabre->short_description;
	hilt->name = sabre->name;
	sabre->description=NULL;
	sabre->short_description=NULL;
	sabre->name=NULL;
	

  equip_char(ch, hilt, pos);
  extract_obj(sabre);
  return;

}

struct obj_data * revert_object(struct obj_data *hilt)
{
  int i;
  struct obj_data *sabre, tmpobj;

  if (!IS_SET_AR(GET_OBJ_EXTRA(hilt), ITEM_LIGHTSABRE) )
    return hilt;

  i = (GET_OBJ_WAS(hilt) == NOTHING ? GET_OBJ_VAL(hilt, 0) : GET_OBJ_WAS(hilt));
  if ((sabre = read_object(i, VIRTUAL)) == NULL)
  {
    log("SYSERR: %s couldn't revert to %d because that vnum doesn't exist!", hilt->short_description, i);
    return hilt;
  }
  if (GET_OBJ_TYPE(sabre) != ITEM_LIGHTSABRE_HILT)
  {
    extract_obj(sabre);
    return hilt;
  }
    for (i = 0; i < MAX_OBJ_AFFECT; i++)
    if (hilt->affected[i].modifier)
    {
      sabre->affected[i].location = hilt->affected[i].location;
      sabre->affected[i].modifier = hilt->affected[i].modifier;
    }
  //success!
  /* move new obj info over to old object and delete new obj */
	memcpy(&tmpobj, sabre, sizeof(*sabre));
	tmpobj.in_room = IN_ROOM(hilt);
	tmpobj.carried_by = hilt->carried_by;
	tmpobj.in_locker = hilt->in_locker;
	tmpobj.worn_by = hilt->worn_by;
	tmpobj.worn_on = hilt->worn_on;
	tmpobj.in_obj = hilt->in_obj;
	tmpobj.contains = hilt->contains;
	tmpobj.id = hilt->id;
	tmpobj.proto_script = hilt->proto_script;
	tmpobj.script = hilt->script;
	tmpobj.next_content = hilt->next_content;
	tmpobj.next = hilt->next;
	memcpy(hilt, &tmpobj, sizeof(*hilt));


  extract_obj(sabre);
  return hilt;

}


void subo(enum subskill_list subcmd, const char *name, int sta, int cost,
          int percent, int minpos, int targets, byte violent, int routines,
          int stat_type, int flags, int cl_type, int perent)
{
  SINFO.min_position = minpos;		/* Position for caster   */
  SINFO.stat = sta;		/* uses hitp, mana, or move points */
  SINFO.cost = cost;		        /* the cost in units */
  SINFO.percentage = percent;		/* the cost in percentage */
  /*
     where actual cost is the lower of the two, percentage or cost 
     for example if the cost is 200 hitpoints, and the percentage is 15%
     and the player has 1000 hp, the percentage hp is 150 points
     then the actual cost will be 150, because the percentage 
     is lower then the cost value -- mordipie
  */
  SINFO.routines = routines;
  SINFO.violent = violent;
  SINFO.targets = targets;		/* See below for use with TAR_XXX  */
  SINFO.name = name; /*the name of the subskill*/
  SINFO.perent = perent;  /* is this a true subskill? if so which skill is it the child of, if not -1 */
  SINFO.stat_type = stat_type;
  SINFO.flags = flags;
  SINFO.cl_type = cl_type; /*who can use this sub?*/
}


void unused_sub(enum subskill_list subcmd)
{
  SINFO.min_position = 0;		/* Position for caster   */
  SINFO.stat = 0;		/* uses hitp, mana, or move points */
  SINFO.cost = 0;		        /* the cost in units */
  SINFO.percentage = 0;		/* the cost in percentage */
  /*
     where actual cost is the lower of the two, percentage or cost 
     for example if the cost is 200 hitpoints, and the percentage is 15%
     and the player has 1000 hp, the percentage hp is 150 points
     then the actual cost will be 150, because the percentage 
     is lower then the cost value -- mordipie
  */
  SINFO.routines = 0;
  SINFO.violent = 0;
  SINFO.targets = 0;		/* See below for use with TAR_XXX  */
  SINFO.name = unused_subname; /*the name of the subskill*/
  SINFO.perent = TYPE_UNDEFINED;  /* is this a true subskill? if so which skill is it the child of, if not -1 */
  SINFO.stat_type = STATUS_TYPE_ONOFF;
  SINFO.flags = SUB_TYPE_AUTO;
  SINFO.cl_type = 0; /*who can use this sub?*/
}


int total_sub_chance(Character *ch, enum subskill_list  subcmd)
{
  int count = 0, total = 0, check = TRUE;


  if (!IS_SET(SINFO.flags, SUB_TYPE_PROF) && SINFO.perent != TYPE_UNDEFINED)
  {
    if (GET_SKILL(ch, SINFO.perent) > 1)
    {
      count++;
      total += GET_SKILL(ch, SINFO.perent);
    }
    else
    {
      check = FALSE;
    }
  }


  if (check)
  {
    count++;
    total += GET_SUB(ch, subcmd);
    return (total / count);
  }
  else
    return 0;
}
void improve_sub(Character *ch, enum subskill_list  sub, int amount)
{
  struct sub_list *temp 	= (IS_NPC(ch) ? NULL : ch->subs);
  struct sub_list *temp2 	= NULL;

  if (IS_NPC(ch))
  {
    MOB_SUBSKILL(ch) = sub;
    return;
  }

  while (temp)
  {
    if (temp->subskill == (sub))
    {
      temp->learn += amount;
      if (temp->learn < 0)
        temp->learn = 0;
      if (temp->learn > 100)
        temp->learn = 100;
      return;
    }

    if (temp->next == NULL)
      temp2 = temp;

    temp = temp->next;
  }

  /*couldn't find it! - lets give t to em!*/
  if (ch->subs == NULL)
  {

    CREATE(ch->subs, struct sub_list, 1);
    ch->subs->subskill = (enum subskill_list)(sub);
    ch->subs->learn = amount;
    ch->subs->status = (enum sub_status_toggle)0;
    ch->subs->next = NULL;
  }
  else
  {
    /* temp should equal null here */
    CREATE(temp2->next, struct sub_list, 1);
    temp2->next->subskill = (enum subskill_list)(sub);
    temp2->next->learn = amount;
    temp2->next->status = (enum sub_status_toggle)0;
    temp2->next->next = NULL;

  }
  log("SUBSKILL: %s achieves %d percent in a new skill %s.", GET_NAME(ch), amount, sub_name(sub));

  save_char(ch);
}


const char * color_option_name(int num)
{
  extern const char *color_option_list[];

  if (num < 0 || num >= MAX_COLOR_OPTIONS)
    return "UNDEFINED";

  return color_option_list[num];

}

int get_sub(Character *ch, int i)
{
  struct sub_list *temp = (IS_NPC(ch) ? NULL : ch->subs);

  if (IS_NPC(ch) && MOB_SUBSKILL(ch) == i)
    return 100;

  while (temp)
  {
    if (temp->subskill == (i))
      return temp->learn;
    temp = temp->next;
  }
  return 0;

}

int get_sub_status(Character *ch, int i)
{
  struct sub_list *temp = (IS_NPC(ch) ? NULL : ch->subs);

  while (temp)
  {
    if (temp->subskill == (i))
      return temp->status;
    temp = temp->next;
  }
  return STATUS_OFF;

}

int toggle_sub_status(Character *ch, int i, int onoff)
{
  struct sub_list *temp = (IS_NPC(ch) ? NULL : ch->subs);

  while (temp)
  {
    if (temp->subskill == (i))
    {
      return (temp->status = (enum sub_status_toggle)onoff);
    }
    temp = temp->next;
  }
  return STATUS_OFF;

}


/* display your skill/subskill based toggles */
ACMD(do_subdisplay)
{
  struct sub_list *temp = NULL;
  char buf[MAX_INPUT_LENGTH];
  int i = 0;
  DYN_DEFINE;
  *buf = '\0';

  if (IS_NPC(ch))
  {
    ch->Send( "Nah, sorry.. yours are set up differently\r\n");
    return;
  }
  if ((temp = ch->subs) == NULL)
  {
    ch->Send( "You don't even KNOW any subskills!\r\n");
    return;
  }

  DYN_CREATE;
  *dynbuf = 0;
  while (temp != NULL)
  {

    if (temp->subskill > 0 &&  (sub_info[temp->subskill].stat_type == STATUS_TYPE_ONOFF))
    {
      i++;
      sprintf(buf, "{cc%22s: [{cy%3s{cc] %s\r\n",
              sub_name(temp->subskill), ONOFF(temp->status), IS_SET(sub_info[temp->subskill].flags,SUB_TYPE_AUTO) ? "(Uncontroled)" : "(  Controled)");

      DYN_RESIZE(buf);
    }
    temp = temp->next;
  }
  sprintf(buf, "{c0");
  DYN_RESIZE(buf);


  page_string(ch->desc, dynbuf, DYN_BUFFER);


}

int default_on(enum subskill_list sub)
{
  if (IS_SET(sub_info[sub].flags, SUB_TYPE_DEFAULT_ON))
    return 1;
  else
    return 0;
}

void reset_default_status(Character *ch)
{
  struct sub_list *temp = NULL;

  if ((temp = ch->subs) == NULL)
    return;

  while (temp != NULL)
  {
    temp->status = (enum sub_status_toggle)default_on(temp->subskill);
    temp = temp->next;
  }

}



