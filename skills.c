#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "db.h"
#include "interpreter.h"
#include "handler.h"
#include "spells.h"
#include "screen.h"
#include "constants.h"
#include "dg_scripts.h"
#include "dg_event.h"


/* external variables */
extern struct spell_info_type spell_info[];
SPECIAL(shop_keeper);
extern int slipping;
/* extern variables */
extern int corpse_mod;

/* external functions */
ACMD(do_give);
ACMD(do_put);
ACMD(do_drop);
ACMD(do_get);
int class_damroll(struct char_data *ch);
void start_fighting_delay(struct char_data *ch, struct char_data *vict);
void halt_fighting(struct char_data *ch);
void dismount_char(struct char_data *ch);
int can_fight(CHAR_DATA *ch, CHAR_DATA *vict);
void perform_give(CHAR_DATA *ch, CHAR_DATA *vict,
                  OBJ_DATA *obj);
extern int arena_ok(CHAR_DATA *ch, CHAR_DATA *victim);
void improve_skill(CHAR_DATA *ch, int skill);
int compute_armor_class(CHAR_DATA *ch);
void spello(int spl, const char *name, int max_mana, int min_mana,
            int mana_change, int minpos, int targets, int violent,
            int routines, int wait, int first_prereq, int second_prereq, int tier, int level);
int find_first_step(room_rnum src, room_rnum target);
void skill_attack(struct char_data *ch, struct char_data *vict, int skill, int pass);
ACMD(do_gen_door);
void send_not_to_spam(char *buf, CHAR_DATA *ch,
                      CHAR_DATA *victim, OBJ_DATA *weap,
                      int spam);
void check_killer(CHAR_DATA *ch, CHAR_DATA *vict);
int has_class(CHAR_DATA *ch, int chclass);
int tier_level(CHAR_DATA *ch, int chclass);
int perform_push(CHAR_DATA *ch, int dir, int need_specials_check,
                 CHAR_DATA *attacker);
void log_push_to_death(CHAR_DATA *ch, CHAR_DATA *attacker);
ACMD(do_assist);
int has_weapon(CHAR_DATA *ch);
int perform_grapple(CHAR_DATA *ch, int dir, int need_specials_check,
                    CHAR_DATA *attacker);
int perform_charge(CHAR_DATA *ch, CHAR_DATA *vict);
int skill_cost(int h, int m, int v, CHAR_DATA *ch);


#define TIER (ch ? current_class_is_tier_num(ch) : 1)

/* local variables */
#define OBJ_VNUM_BLACK_POWDER 	21
/* Defines for the skill system */
//#define skillo(skill, name, tar, violent) spello(skill, name, 0, 0, 0, 0, tar, violent, 0, NULL);
#define skillo(skill, name, tar, violent, first, second, tier, level) spello(skill, name, 0, 0, 0, 0, tar, violent, 0, 0, first, second, tier, level);
#define skillo_static(skill, name, first, second, tier, level) spello(skill, name, 0, 0, 0, 0, 0, 0, 0, 0, first, second, tier, level);
#define CALL_SKILL(sname) sk_success = sname(ch, vict, obj, argument); //arg was argument it is a single argument check this


#define SK_NONE             0
#define SK_VIOLENT          (1 << 0)
#define SK_NEED_WEAPON      (1 << 1)
/*for assisting*/
struct follow_type *k;


typedef struct
{
  int dirNum;
  char *dirCmd;
}
dirParseStruct;

/* Local Functions */
int total_chance(CHAR_DATA *ch, int skill);
ASKILL(skill_backstab);
ASKILL(skill_bash);
ASKILL(skill_hide);
ASKILL(skill_kick);
/* pick is handled by do_gen_door (act.movement.c) */
ASKILL(skill_rescue);
ASKILL(skill_sneak);
ASKILL(skill_steal);
ASKILL(skill_strangle);
ASKILL(skill_track);
ASKILL(skill_trample);
ASKILL(skill_disarm);
ASKILL(skill_joust);
ASKILL(skill_dodge);
ASKILL(skill_charge);
ASKILL(skill_grip);
ASKILL(skill_face);
ASKILL(skill_focus);
ASKILL(skill_flank);
ASKILL(skill_grapple);
ASKILL(skill_holy_strength);
ASKILL(skill_beserk);
ASKILL(skill_meditate);
ASKILL(skill_hyperactivity);
ASKILL(skill_true_strike);

ASKILL(skill_fortify);
ASKILL(skill_scalp);
ASKILL(skill_blade_dance);
ASKILL(skill_cleave);
ASKILL(skill_behead);
ASKILL(skill_brace);
ASKILL(skill_tinker);
ASKILL(skill_poison_weapon);
ASKILL(skill_retreat);
ASKILL(skill_filet);
ASKILL(skill_forage);
ASKILL(skill_push);
ASKILL(skill_scan);
ASKILL(skill_brew);
ASKILL(skill_scribe);

ASKILL(skill_mount);
ASKILL(skill_tame);
ASKILL(skill_snare);
ASKILL(skill_circle);
ASKILL(skill_blackjack);

ASKILL(skill_sing_wood);
ASKILL(skill_manifest);
ASKILL(skill_manipulate);
ASKILL(skill_phase);
ASKILL(skill_martial_arts);
ASKILL(skill_slip);


ACMD(do_skills);

#define NO_TIER 1
#define NO_LEVEL 0
#define NO_FIRST TYPE_UNDEFINED
#define NO_SECOND TYPE_UNDEFINED



void assign_skills(void)
{

  skillo(SKILL_MOUNT, "mount", TAR_CHAR_ROOM | TAR_NOT_SELF,
         SK_NONE, NO_FIRST, NO_SECOND, NO_TIER, 2);

  skillo(SKILL_TAME, "tame", TAR_CHAR_ROOM | TAR_NOT_SELF,
         SK_NONE, NO_FIRST, NO_SECOND, NO_TIER, 24);

  skillo(SKILL_SNARE, "snare",
         TAR_CHAR_ROOM | TAR_FIGHT_VICT | TAR_NOT_SELF, SK_VIOLENT,
         NO_FIRST, NO_SECOND, NO_TIER, 47);

  skillo(SKILL_CIRCLE, "encircle",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT | SK_NEED_WEAPON, SKILL_BACKSTAB, SKILL_MELEE, 2, 32);

  skillo(SKILL_BLACKJACK, "blackjack",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT | SK_NEED_WEAPON, SKILL_HANDTOHAND, NO_SECOND, NO_TIER,NO_LEVEL);

  skillo(SKILL_STRANGLE, "strangle",
         TAR_CHAR_ROOM | TAR_NOT_SELF,
         SK_VIOLENT, NO_FIRST, NO_SECOND, 3, 29);

  skillo(SKILL_PUSH, "push", TAR_CHAR_ROOM | TAR_NOT_SELF,   SK_NONE, SKILL_HANDTOHAND, NO_SECOND, NO_TIER, 12);

  skillo(SKILL_SCAN, "scan", TAR_IGNORE, SK_NONE, NO_FIRST, NO_SECOND, NO_TIER, 31);

  skillo(SKILL_BREW, "brew", TAR_IGNORE, SK_NONE, NO_FIRST, NO_SECOND, 0, 29);

  skillo(SKILL_SING_WOOD, "woodsing", TAR_IGNORE, SK_NONE, NO_FIRST,NO_SECOND, 4, 25);

  skillo(SKILL_MANIFEST, "manifest", TAR_IGNORE, SK_NONE, NO_FIRST,NO_SECOND, 0, 1);

  skillo(SKILL_MANIPULATE, "manipulate", TAR_IGNORE, SK_NONE,
         NO_FIRST, NO_SECOND, 3, 49);

  skillo(SKILL_SCRIBE, "scribe", TAR_IGNORE, SK_NONE, NO_FIRST,
         NO_SECOND, NO_TIER, 39);

  skillo(SKILL_TINKER, "tinker", TAR_IGNORE, SK_NONE, NO_FIRST,NO_SECOND, 1, 30);

  skillo(SKILL_POISON_WEAPON, "poison weapon", TAR_IGNORE,
         SK_NONE, NO_FIRST, NO_SECOND, 3, 5);

  skillo(SKILL_RETREAT, "retreat", TAR_IGNORE, SK_NONE, NO_FIRST,NO_SECOND, 2, 12);

  skillo(SKILL_FILET, "filet", TAR_IGNORE, SK_NONE, NO_FIRST,NO_SECOND, NO_TIER, 14);

  skillo(SKILL_SLIP, "slip", TAR_IGNORE, SK_NONE, NO_FIRST, NO_SECOND,3, 9);

  skillo(SKILL_FORAGE, "forage", TAR_IGNORE, SK_NONE, NO_FIRST,
         NO_SECOND, NO_TIER, 28);

  skillo(SKILL_BACKSTAB, "backstab", TAR_CHAR_ROOM | TAR_NOT_SELF,
         SK_VIOLENT | SK_NEED_WEAPON, NO_FIRST, NO_SECOND, NO_TIER,16);

  skillo(SKILL_DISARM, "disarm", TAR_CHAR_ROOM | TAR_NOT_SELF |
         TAR_FIGHT_VICT,  SK_VIOLENT | SK_NEED_WEAPON, SKILL_MELEE, NO_SECOND, 2,9);


  skillo(SKILL_BASH, "bash",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT | SK_NEED_WEAPON, SKILL_HANDTOHAND, NO_SECOND, NO_TIER, 45);

  skillo(SKILL_HIDE, "hide", TAR_CHAR_ROOM | TAR_SELF_ONLY,
         SK_NONE, NO_FIRST, NO_SECOND, NO_TIER, 8);

  skillo(SKILL_KICK, "kick",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT, NO_FIRST, NO_SECOND, NO_TIER, 2);

  skillo(SKILL_TRAMPLE, "trample",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT, NO_FIRST, NO_SECOND, 3, 18);

  skillo(SKILL_JOUST, "joust",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT, NO_FIRST, NO_SECOND, 3, 29);

  skillo(SKILL_PICK_LOCK, "pick lock", TAR_IGNORE, SK_NONE, NO_FIRST,NO_SECOND, 1, 49);

  skillo(SKILL_RESCUE, "rescue", TAR_CHAR_ROOM | TAR_NOT_SELF,
         SK_NONE, NO_FIRST, NO_SECOND, NO_TIER, 27);

  skillo(SKILL_SNEAK, "sneak", TAR_CHAR_ROOM | TAR_SELF_ONLY,
         SK_NONE, NO_FIRST, NO_SECOND, NO_TIER, 31);

  skillo(SKILL_STEAL, "steal", TAR_IGNORE, SK_VIOLENT, NO_FIRST,NO_SECOND, NO_TIER, 4);

  skillo(SKILL_TRACK, "track", TAR_CHAR_WORLD | TAR_NOT_SELF,
         SK_NONE, NO_FIRST, NO_SECOND, 0, 27);

  skillo(SKILL_GRAPPLE, "grapple",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT, NO_FIRST, NO_SECOND, 4, 17);

  skillo(SKILL_DODGE, "dodge", TAR_IGNORE,
         SK_NONE, NO_FIRST, NO_SECOND, 3, 4);
  skillo(SKILL_PHASE, "phase", TAR_IGNORE,
         SK_NONE, NO_FIRST, NO_SECOND, 3, 4);

  skillo(SKILL_BLACKJACK, "blackjack",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT | SK_NEED_WEAPON, NO_FIRST, NO_SECOND, 0, 7);

  skillo(SKILL_CHARGE, "charge",
         TAR_IGNORE, SK_VIOLENT, NO_FIRST, NO_SECOND, 3, 26);

  skillo(SKILL_GRIP, "grip", TAR_CHAR_ROOM | TAR_SELF_ONLY,
         SK_NONE, NO_FIRST, NO_SECOND, 1, 49);

  skillo(SKILL_FACE, "face", TAR_CHAR_ROOM | TAR_NOT_SELF,
         SK_NONE, NO_FIRST, NO_SECOND, 3, 20);

  skillo(SKILL_FOCUS, "focus", TAR_CHAR_ROOM | TAR_SELF_ONLY,
         SK_NONE, NO_FIRST, NO_SECOND, 3, 24);

  skillo(SKILL_HOLY_STRENGTH, "holy strength",   TAR_CHAR_ROOM | TAR_SELF_ONLY,
         SK_NONE, NO_FIRST, NO_SECOND,2, 5);

  skillo(SKILL_BESERK, "berserk", TAR_CHAR_ROOM | TAR_SELF_ONLY,
         SK_NONE, NO_FIRST, NO_SECOND, 3, 8);

  skillo(SKILL_MEDITATE, "meditate", TAR_CHAR_ROOM | TAR_SELF_ONLY,
         SK_NONE, NO_FIRST, NO_SECOND, 2, 30);

  skillo(SKILL_HYPERACTIVITY, "hyperactivity",  TAR_CHAR_ROOM | TAR_SELF_ONLY,
         SK_NONE, NO_FIRST, NO_SECOND, 4, 36);

  skillo(SKILL_MARTIAL_ARTS, "martial arts", TAR_CHAR_ROOM |
         TAR_SELF_ONLY,   SK_NONE, NO_FIRST, NO_SECOND, 4, 26);

  skillo(SKILL_TRUE_STRIKE, "true strike", TAR_CHAR_ROOM |
         TAR_SELF_ONLY, SK_NONE, NO_FIRST, NO_SECOND, 3, 48);



  skillo(SKILL_FORTIFY, "fortify", TAR_CHAR_ROOM | TAR_SELF_ONLY,
         SK_NONE, NO_FIRST, NO_SECOND, 3, 11);

  skillo(SKILL_SCALP, "scalp",   TAR_OBJ_INV | TAR_OBJ_ROOM,
         SK_NONE, NO_FIRST, NO_SECOND, 2, 40);

  skillo(SKILL_BLADE_DANCE, "bladedance",TAR_CHAR_ROOM |
         TAR_SELF_ONLY,	   SK_NONE, NO_FIRST, NO_SECOND, 4, 29);

  skillo(SKILL_CLEAVE, "cleave",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT | SK_NEED_WEAPON, NO_FIRST, NO_SECOND, 3, 38);

  skillo(SKILL_BEHEAD, "behead",
         TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT | SK_NEED_WEAPON, NO_FIRST, NO_SECOND, 3,38);

  skillo(SKILL_BRACE, "brace", TAR_IGNORE,
         SK_NONE, NO_FIRST, NO_SECOND, 1, 15);

  skillo(SKILL_FLANK, "flank" ,TAR_CHAR_ROOM | TAR_NOT_SELF | TAR_FIGHT_VICT,
         SK_VIOLENT | SK_NEED_WEAPON, NO_FIRST, NO_SECOND, 3, 48);

  skillo_static(SKILL_RIDING, "riding", NO_FIRST, NO_SECOND, NO_TIER,3);
  skillo_static(SKILL_BOW, "bow", NO_FIRST, NO_SECOND, NO_TIER,8);
  skillo_static(SKILL_SLING, "sling", NO_FIRST, NO_SECOND, NO_TIER,12);
  skillo_static(SKILL_CROSSBOW, "crossbow", NO_FIRST, NO_SECOND,NO_TIER, 20);
  skillo_static(SKILL_THROW, "throw", NO_FIRST, NO_SECOND, NO_TIER, 22);
  skillo_static(SKILL_DUAL, "dual", NO_FIRST, NO_SECOND, 1, 27);
  skillo_static(SKILL_FIREARM, "firearm", NO_FIRST, NO_SECOND, 0, 25);
  skillo_static(SKILL_TRAP_AWARE, "trap aware", NO_FIRST, NO_SECOND,3, 5);
  skillo_static(SKILL_PARRY, "parry", NO_FIRST, NO_SECOND, 2,30);
  /*mord */
  skillo_static(SKILL_DRUNK, "drunk", NO_FIRST, NO_SECOND, NO_TIER,NO_LEVEL);
  skillo_static(SKILL_MOUNTED_COMBAT, "mounted combat", SKILL_RIDING, NO_SECOND, 2, 14);
  skillo_static(SKILL_HANDTOHAND, "hand-to-hand", NO_FIRST, NO_SECOND,0, 25);

  skillo_static(SKILL_MELEE, "melee", NO_FIRST, NO_SECOND, 0, 10);

  skillo_static(SKILL_SECOND_ATTACK, "second attack",     SKILL_MELEE, NO_SECOND, 1, 10);

  skillo_static(SKILL_THIRD_ATTACK, "third attack",SKILL_SECOND_ATTACK,SKILL_MELEE, 2, 10);

  skillo_static(SKILL_FOURTH_ATTACK, "fourth attack",SKILL_THIRD_ATTACK,SKILL_MELEE, 3, 10);

  skillo_static(SKILL_FIFTH_ATTACK, "fifth attack",SKILL_FOURTH_ATTACK,SKILL_MELEE, 4, 10);

  skillo_static(SKILL_LONGARM, "longarm", NO_FIRST, NO_SECOND, 2, 1);


}


#define spellnum subcmd		/* lame define so SINFO macro works */
#define flags violent
ACMD(do_skills)
{

  CHAR_DATA *vict = NULL, *orig = ch;
  OBJ_DATA *obj = NULL;
  int target = 0;
  int i = 0;
  int sk_success = 0;

  /* If switched, orig should point to original char */
  if (ch->desc && ch->desc->original)
    orig = ch->desc->original;

  /** Can we even use this skill right here and right now? 
   ** I know there is redundancy in the messages of these checks*/
  if (IS_NPC(orig) && orig->desc)  
    new_send_to_char(ch, "You have no idea how to do that.\r\n");
  else if (!IS_NPC(orig) && ((!knows_spell(orig, subcmd) || !GET_SKILL(ch, subcmd)) && GET_LEVEL(orig) < LVL_GOD))  
    new_send_to_char(ch, "You have no idea how to do that.\r\n");
  else if (IS_SET(SINFO.flags, SK_VIOLENT) && ROOM_FLAGGED(IN_ROOM(ch), ROOM_PEACEFUL))
    new_send_to_char(ch,"This room just has such a peaceful, easy feeling...\r\n");
  else
  {
    char arg[MAX_STRING_LENGTH];

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
      if (IS_SET(SINFO.targets, TAR_OBJ_INV))
        if ((obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)) != NULL)
          target = TRUE;

      if (!target && IS_SET(SINFO.targets, TAR_OBJ_EQUIP))
      {
        for (i = 0; !target && i < NUM_WEARS; i++)
          if (HAS_BODY(ch, i) && GET_EQ(ch, i)
              && isname(arg, GET_EQ(ch, i)->name))
          {
            obj = GET_EQ(ch, i);
            target = TRUE;
          }
      }

      if (!target && IS_SET(SINFO.targets, TAR_OBJ_ROOM))
        if ((obj =
               get_obj_in_list_vis(ch, arg, NULL,
                                   IN_ROOM(ch)->contents)) != NULL)
          target = TRUE;

      if (!target && IS_SET(SINFO.targets, TAR_OBJ_WORLD))
        if ((obj = get_obj_vis(ch, arg, NULL)) != NULL)
          target = TRUE;
    }

    if (SINFO.targets != TAR_IGNORE && !vict && !target)
      new_send_to_char(ch,
                       "%s needs an argument to work.\r\n",
                       SINFO.name);
    else if (IS_SET(SINFO.targets, TAR_NOT_SELF) && vict == ch)
      new_send_to_char(ch, "You can't %s yourself!\r\n", SINFO.name);
    else if (IS_SET(SINFO.targets, TAR_SELF_ONLY) && vict != ch)
      new_send_to_char(ch, "That can only be done to yourself.\r\n");
    else if (IS_SET(SINFO.flags, SK_NEED_WEAPON)
             && !has_weapon(ch))
      new_send_to_char(ch,
                       "You need to wield a weapon to make it a success.\r\n");
    else
    {
      if (vict && IS_SET(SINFO.flags, SK_VIOLENT) )
      {
        if (!can_fight(ch, vict))
        {
          new_send_to_char(ch, "You can't do that to them!\r\n");
          return;
        }
      }
      if (!skill_cost(0, 0, 4, ch))
      {
        new_send_to_char(ch, "You are exausted!");
        return;
      }
      switch (subcmd)
      {
      case SKILL_BACKSTAB:
        CALL_SKILL(skill_backstab);
        break;
      case SKILL_BASH:
        CALL_SKILL(skill_bash);
        break;
      case SKILL_HIDE:
        CALL_SKILL(skill_hide);
        break;
      case SKILL_KICK:
        CALL_SKILL(skill_kick);
        break;
      case SKILL_CIRCLE:
        CALL_SKILL(skill_circle);
        break;
      case SKILL_BLACKJACK:
        CALL_SKILL(skill_blackjack);
        break;
      case SKILL_MOUNT:
        CALL_SKILL(skill_mount);
        break;
      case SKILL_TAME:
        CALL_SKILL(skill_tame);
        break;
      case SKILL_RESCUE:
        CALL_SKILL(skill_rescue);
        break;
      case SKILL_SNEAK:
        CALL_SKILL(skill_sneak);
        break;
      case SKILL_STEAL:
        CALL_SKILL(skill_steal);
        break;
      case SKILL_TRACK:
        CALL_SKILL(skill_track);
        break;
      case SKILL_JOUST:
        CALL_SKILL(skill_joust);
        break;
      case SKILL_DODGE:
        CALL_SKILL(skill_dodge);
        break;
      case SKILL_PHASE:
        CALL_SKILL(skill_phase);
        break;
      case SKILL_CHARGE:
        CALL_SKILL(skill_charge);
        break;
      case SKILL_GRIP:
        CALL_SKILL(skill_grip);
        break;
      case SKILL_FACE:
        CALL_SKILL(skill_face);
        break;
      case SKILL_FLANK:
        CALL_SKILL(skill_flank);
        break;
      case SKILL_GRAPPLE:
        CALL_SKILL(skill_grapple);
        break;
      case SKILL_HOLY_STRENGTH:
        CALL_SKILL(skill_holy_strength);
        break;
      case SKILL_BESERK:
        CALL_SKILL(skill_beserk);
        break;
      case SKILL_MEDITATE:
        CALL_SKILL(skill_meditate);
        break;
      case SKILL_HYPERACTIVITY:
        CALL_SKILL(skill_hyperactivity);
        break;
      case SKILL_TRUE_STRIKE:
        CALL_SKILL(skill_true_strike);
        break;
      case SKILL_SNARE:
        CALL_SKILL(skill_snare);
        break;
      case SKILL_MARTIAL_ARTS:
        CALL_SKILL(skill_martial_arts);
        break;
      case SKILL_FORTIFY:
        CALL_SKILL(skill_fortify);
        break;
      case SKILL_SCALP:
        CALL_SKILL(skill_scalp);
        break;
      case SKILL_BLADE_DANCE:
        CALL_SKILL(skill_blade_dance);
        break;
      case SKILL_CLEAVE:
        CALL_SKILL(skill_cleave);
        break;
      case SKILL_BEHEAD:
        CALL_SKILL(skill_behead);
        break;
      case SKILL_BRACE:
        CALL_SKILL(skill_brace);
        break;
      case SKILL_TRAMPLE:
        CALL_SKILL(skill_trample);
        break;
      case SKILL_DISARM:
        CALL_SKILL(skill_disarm);
        break;
      case SKILL_FOCUS:
        CALL_SKILL(skill_focus);
        break;
      case SKILL_TINKER:
        CALL_SKILL(skill_tinker);
        break;
      case SKILL_POISON_WEAPON:
        CALL_SKILL(skill_poison_weapon);
        break;
      case SKILL_RETREAT:
        CALL_SKILL(skill_retreat);
        break;
      case SKILL_FORAGE:
        CALL_SKILL(skill_forage);
        break;
      case SKILL_FILET:
        CALL_SKILL(skill_filet);
        break;
      case SKILL_PUSH:
        CALL_SKILL(skill_push);
        break;
      case SKILL_SCAN:
        CALL_SKILL(skill_scan);
        break;
      case SKILL_BREW:
        CALL_SKILL(skill_brew);
        break;
      case SKILL_SCRIBE:
        CALL_SKILL(skill_scribe);
        break;
      case SKILL_SING_WOOD:
        CALL_SKILL(skill_sing_wood);
        break;
      case SKILL_MANIFEST:
        CALL_SKILL(skill_manifest);
        break;
      case SKILL_MANIPULATE:
        CALL_SKILL(skill_manipulate);
        break;
      case SKILL_SLIP:
        CALL_SKILL(skill_slip);
        break;
      case SKILL_STRANGLE:
        CALL_SKILL(skill_strangle);
        break;
        /* Special case, pick is still handled by do_gen_door */
      case SKILL_PICK_LOCK:
        do_gen_door(ch, argument, cmd, SCMD_PICK);
        break;
      default:
        log("SYSERR: Skill #%d is unknown to do_skills", subcmd);
      }
      if (sk_success)
        improve_skill(ch, sk_success);


    }
  }
}

#undef flags
#undef spellnum


ASKILL(skill_backstab)
{

  int percent, prob;
  OBJ_DATA *wep = GET_EQ(ch, WEAR_WIELD);
  int size_dice_wep(CHAR_DATA *ch, short dual);
  int num_dice_wep(CHAR_DATA *ch, short dual);
  int class_damroll(CHAR_DATA *ch);


  if (use_stamina( ch, 12) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  if (!skill_cost(0, 0, 40, ch))
  {
    new_send_to_char(ch, "You are exausted!");
    return 0;
  }

  if (!IS_NPC(ch) && (!wep ||GET_OBJ_VAL(wep, 3) != TYPE_PIERCE - TYPE_HIT))
  {
    new_send_to_char(ch,
                     "Only piercing weapons can be used for backstabbing.\r\n");
    return 0;
  }
  if (FIGHTING(vict))
  {
    new_send_to_char(ch,
                     "You can't backstab a fighting person -- they're too alert!\r\n");
    return 0;
  }
  if ((MOB_FLAGGED(vict, MOB_AWARE) || ((GET_HIT(vict)*2) < GET_MAX_HIT(vict)) ) && AWAKE(vict))
  {
    act("You notice $N lunging at you!", FALSE, vict, 0, ch, TO_CHAR);
    act("$e notices you lunging at $m!", FALSE, vict, 0, ch, TO_VICT);
    act("$n notices $N lunging at $m!", FALSE, vict, 0, ch,  TO_NOTVICT);
    start_fighting(vict, ch);

    WAIT_STATE(ch, (PULSE_VIOLENCE * 3)/(TIERNUM+2));
    return 0;
  }

  percent = number(1, 101);	/* 101% is a complete failure */
  prob = IS_NPC(ch) ? GET_LEVEL(ch) : total_chance(ch, SKILL_BACKSTAB);

  if (AWAKE(vict) && (percent > prob))
    skill_attack(ch, vict, SKILL_BACKSTAB, FALSE);
  else
    skill_attack(ch, vict, SKILL_BACKSTAB, TRUE);

  WAIT_STATE(ch, (PULSE_VIOLENCE * 3)/(TIERNUM+2));
  return SKILL_BACKSTAB;
}

ASKILL(skill_charge)
{
  CHAR_DATA *temp = NULL, *temp_next = NULL;
  int ret = 0;
  if (use_stamina( ch, 5) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }

  for (temp = IN_ROOM(ch)->people;temp; temp = temp_next)
  {
    temp_next = temp->next_in_room;
    if (ch == temp)
      continue;
    if (RIDING(ch) && temp==RIDING(ch))
      continue;
    if (ch == temp->master)
      continue;
    ret = perform_charge(ch, temp);

    if (ret == 0)
    {
      act("$N dodges to the side and you charge straight past!", FALSE, ch, 0, temp, TO_CHAR);
      act("you dodge $n's charge and $e rushes right past you!", FALSE, ch, 0, temp, TO_VICT);
    }
    else if (ret == -1)
      break;


  }


  return SKILL_CHARGE;
}

int perform_charge(CHAR_DATA *ch, CHAR_DATA *vict)
{
  int prob, percent = number(0, 100);
  if (MOB_FLAGGED(vict, MOB_NOBASH))
    return 0;
  if (GET_POS(vict) < POS_FIGHTING)
    return 0;
  if (!can_fight(ch, vict))
    return 0;

  prob = total_chance(ch, SKILL_CHARGE);


  if (percent > prob)
  {
    skill_attack(ch, vict, SKILL_BASH, FALSE);
    GET_POS(ch) = POS_SITTING;
    GET_WAIT_STATE(ch) += 2 RL_SEC;
    return -1;
  }
  else
  {
    skill_attack(ch, vict, SKILL_BASH, TRUE);
    return 1;

  }
  return 0;
}

ASKILL(skill_bash)
{

  int percent, prob;

  if (!CONFIG_PK_ALLOWED && !IS_NPC(vict) && !arena_ok(ch, vict))
  {
    send_to_char("You and your victim must both be in the arena.\r\n",
                 ch);
    return 0;
  }
  if (GET_POS(vict) == POS_SITTING)
  {
    new_send_to_char(ch, "They are already down and your bash goes right over them!!\r\n");
    fe_deal_damage(ch, vict, 0, SKILL_BASH);
    GET_POS(ch) = POS_SITTING;
    GET_WAIT_STATE(ch) += 2 RL_SEC;
    return 0;
  }
  if (use_stamina( ch, 10) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  if (!skill_cost(0, 0, 20, ch))
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }

  percent = number(1, 110);	/* 101% is a complete failure */
  prob = total_chance(ch, SKILL_BASH);

  if (MOB_FLAGGED(vict, MOB_NOBASH))
    percent = 101;

  if (percent > prob)
  {
    skill_attack(ch, vict, SKILL_BASH, FALSE);
    GET_POS(ch) = POS_SITTING;
    GET_WAIT_STATE(ch) += 1 RL_SEC;
    return 0;
  }
  else
  {
    /*
     * If we bash a player and they wimp out, they will move to the previous
     * room before we set them sitting.  If we try to set the victim sitting
     * first to make sure they don't flee, then we can't bash them!  So now
     * we only set them sitting if they didn't flee. -gg 9/21/98
     */

    skill_attack(ch, vict, SKILL_BASH, TRUE);

    return SKILL_BASH;
  }
}


ASKILL(skill_rescue)
{

  CHAR_DATA *tmp_ch;
  int percent, prob;

  if (!skill_cost(0, 10, 40, ch))
  {
    new_send_to_char(ch, "You are exausted!");
    return 0;
  }

  if (FIGHTING(ch) == vict)
  {
    send_to_char
    ("How can you rescue someone you are trying to kill?\r\n", ch);
    return 0;
  }
  for (tmp_ch = IN_ROOM(ch)->people; tmp_ch &&
       (FIGHTING(tmp_ch) != vict); tmp_ch = tmp_ch->next_in_room);

  if (!tmp_ch)
  {
    act("But nobody is fighting $M!", FALSE, ch, 0, vict, TO_CHAR);
    return 0;
  }
  percent = number(1, 101);	/* 101% is a complete failure */
  prob = total_chance(ch, SKILL_RESCUE);

  if (percent > prob)
  {
    send_to_char("You fail the rescue!\r\n", ch);
    return 0;
  }
  send_to_char("Banzai!  To the rescue...\r\n", ch);
  act("You are rescued by $N, you are confused!", FALSE, vict, 0, ch,
      TO_CHAR);
  act("$n heroically rescues $N!", FALSE, ch, 0, vict, TO_NOTVICT);
  if (FIGHTING(vict) == tmp_ch)
    stop_fighting(vict);
  if (FIGHTING(tmp_ch))
    stop_fighting(tmp_ch);
  if (FIGHTING(ch))
    stop_fighting(ch);

  start_fighting(ch, tmp_ch);
  start_fighting(tmp_ch, ch);

  WAIT_STATE(vict, PULSE_VIOLENCE);
  return SKILL_RESCUE;
}


ASKILL(skill_kick)
{

  int percent, prob;
  if (use_stamina( ch, 5) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  /* 101% is a complete failure */
  percent = ((10 - (compute_armor_class(vict) / 10)) * 2) + number(1, 70);
  prob = total_chance(ch, SKILL_KICK);
  WAIT_STATE(ch, 1.5 RL_SEC);

  skill_attack(ch, vict, SKILL_KICK, (percent < prob));

  return SKILL_KICK;
}


ASKILL(skill_sneak)
{

  struct affected_type af;
  byte percent;

  if (AFF_FLAGGED(ch, AFF_SNEAK))
  {
    affect_from_char(ch, SKILL_SNEAK);
    send_to_char("Okay, you stop sneaking.\r\n", ch);
  }
  else
  {
    if (use_stamina( ch, 10) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
    send_to_char("Okay, you'll try to move silently for a while.\r\n", ch);

    percent = number(1, 101);	/* 101% is a complete failure */

    if (percent >
        GET_SKILL(ch, SKILL_SNEAK) + dex_app_skill[GET_DEX(ch)].sneak)
    {
      new_send_to_char(ch, "You stumble over your own feet\r\n");
      return 0;
    }

    af.type = SKILL_SNEAK;
    af.expire = HOURS_TO_EXPIRE(GET_LEVEL(ch));
    af.modifier = 0;
    af.location = APPLY_NONE;
    af.bitvector = AFF_SNEAK;
    affect_to_char(ch, &af);
  }
  return SKILL_SNEAK;
}


ASKILL(skill_hide)
{

  byte percent;

  if (AFF_FLAGGED(ch, AFF_HIDE))
  {
    REMOVE_BIT_AR(AFF_FLAGS(ch), AFF_HIDE);
    new_send_to_char(ch, "You stop hiding.");
  }
  else
  {

    if (use_stamina( ch, 10) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
    send_to_char("You attempt to hide yourself.\r\n", ch);
    percent = number(1, 101);	/* 101% is a complete failure */

    if (percent >
        total_chance(ch, SKILL_HIDE) + dex_app_skill[GET_DEX(ch)].hide)
      return 0;

    SET_BIT_AR(AFF_FLAGS(ch), AFF_HIDE);
  }
  return SKILL_HIDE;
}


ASKILL(skill_steal)
{


  char vict_name[MAX_INPUT_LENGTH], obj_name[MAX_INPUT_LENGTH];
  int percent, gold, eq_pos, pcsteal = 0, ohoh = 0;

  if (use_stamina( ch, 5) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }

  two_arguments(argument, obj_name, vict_name);

  if ((vict = get_char_vis(ch, vict_name, NULL, FIND_CHAR_ROOM))==NULL)
  {
    send_to_char("Steal what from who?\r\n", ch);
    return 0;
  }
  else if (vict == ch)
  {
    send_to_char("Come on now, that's rather stupid!\r\n", ch);
    return 0;
  }



  /* 101% is a complete failure */
  percent = number(1, 101) - dex_app_skill[GET_DEX(ch)].p_pocket;

  if (GET_POS(vict) < POS_SLEEPING)
    percent = -1;		/* ALWAYS SUCCESS, unless heavy object. */

  if (!IS_NPC(vict) && IS_PK(vict) && !IS_NPC(ch) && IS_PK(ch))
    pcsteal = 1;

  if (!AWAKE(vict))		/* Easier to steal from sleeping people. */
    percent -= 50;

  /* NO NO With Imp's and Shopkeepers, and if player thieving is not allowed */
  if ((GET_LEVEL(vict) >= LVL_IMMORT && !IS_NPC(vict)) ||
      GET_MOB_SPEC(vict) == shop_keeper || MOB_FLAGGED(vict, MOB_AWARE))
    percent = 101;		/* Failure */

  if (str_cmp(obj_name, "coins") && str_cmp(obj_name, "gold"))
  {

    if (!
        (obj =
           get_obj_in_list_vis(ch, obj_name, NULL, vict->carrying)))
    {

      for (eq_pos = 0; eq_pos < NUM_WEARS; eq_pos++)
        if (GET_EQ(vict, eq_pos) &&
            (isname(obj_name, GET_EQ(vict, eq_pos)->name)) &&
            CAN_SEE_OBJ(ch, GET_EQ(vict, eq_pos)))
        {
          obj = GET_EQ(vict, eq_pos);
          break;
        }
      if (!obj)
      {
        act("$E hasn't got that item.", FALSE, ch, 0, vict,
            TO_CHAR);
        return 0;
      }
      else
      {		/* It is equipment */
        if ((GET_POS(vict) > POS_STUNNED))
        {
          send_to_char
          ("Steal the equipment now?  Impossible!\r\n", ch);
          return 0;
        }
        else
        {
          if (!give_otrigger(obj, vict, ch) ||
              !receive_mtrigger(ch, vict, obj))
          {
            new_send_to_char(ch, "Impossible!\r\n");
            return 0;
          }
          {
            new_send_to_char(ch, "Its stuck!\r\n");
            return 0;
          }
          act("You unequip $p and steal it.", FALSE, ch, obj, 0,
              TO_CHAR);
          act("$n steals $p from $N.", FALSE, ch, obj, vict,
              TO_NOTVICT);
          obj_to_char(unequip_char(vict, eq_pos), ch);
        }
      }
    }
    else
    {		/* obj found in inventory */

      percent += GET_OBJ_WEIGHT(obj);	/* Make heavy harder */

      if (percent > total_chance(ch, SKILL_STEAL))
      {
        ohoh = TRUE;
        send_to_char("Oops..\r\n", ch);
        act("$n tried to steal something from you!", FALSE, ch, 0,
            vict, TO_VICT);
        act("$n tries to steal something from $N.", TRUE, ch, 0,
            vict, TO_NOTVICT);
      }
      else
      {		/* Steal the item */
        if (IS_CARRYING_N(ch) + 1 < CAN_CARRY_N(ch))
        {
          if (!give_otrigger(obj, vict, ch) ||
              !receive_mtrigger(ch, vict, obj))
          {
            new_send_to_char(ch, "Impossible!\r\n");
            return 0;
          }
          if (IS_CARRYING_W(ch) + GET_OBJ_WEIGHT(obj) <
              CAN_CARRY_W(ch))
          {
            obj_from_char(obj);
            obj_to_char(obj, ch);
            send_to_char("Got it!\r\n", ch);
          }
        }
        else
          send_to_char("You cannot carry that much.\r\n", ch);
      }
    }
  }
  else
  {			/* Steal some coins */
    if (AWAKE(vict) && (percent > total_chance(ch, SKILL_STEAL)))
    {
      ohoh = TRUE;
      send_to_char("Oops..\r\n", ch);
      act("You discover that $n has $s hands in your wallet.", FALSE,
          ch, 0, vict, TO_VICT);
      act("$n tries to steal gold from $N.", TRUE, ch, 0, vict,
          TO_NOTVICT);
    }
    else
    {
      /* Steal some gold coins */
      gold = (int) ((GET_GOLD(vict) * number(1, 10)) / 100);
      gold = MIN(1782, gold);
      if (gold > 0)
      {
        char_gold(ch, gold, GOLD_HAND);
        char_gold(vict, -gold, GOLD_HAND);
        if (gold > 1)
        {
          new_send_to_char(ch, "Bingo!  You got %d gold coins.\r\n",
                           gold);
        }
        else
        {
          send_to_char
          ("You manage to swipe a solitary gold coin.\r\n",
           ch);
        }
      }
      else
      {
        send_to_char("You couldn't get any gold...\r\n", ch);
      }
    }
  }

  if (ohoh && IS_NPC(vict) && AWAKE(vict))
    start_fighting(vict, ch);
  return (!ohoh ? SKILL_STEAL : 0);
}

ASKILL(skill_track)
{

  int dir;

  /* We can't track the victim. */
  if (AFF_FLAGGED(vict, AFF_NOTRACK))
  {
    send_to_char("You sense no trail.\r\n", ch);
    return 0;
  }
  if (use_stamina( ch, 10) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  /* 101 is a complete failure, no matter what the proficiency. */
  if (number(0, 101) >= total_chance(ch, SKILL_TRACK))
  {
    int tries = 10;
    /* Find a random direction. :) */
    do
    {
      dir = number(0, NUM_OF_DIRS - 1);
    }
    while (!CAN_GO(ch, dir) && --tries);
    new_send_to_char(ch,
                     "{cWYou notice some tracks leading %s from here!{c0\r\n",
                     dirs[dir]);
    return 0;
  }

  /* They passed the skill check. */
  dir = find_first_step(IN_ROOM(ch), vict->in_room);

  switch (dir)
  {
  case BFS_ERROR:
    send_to_char("Hmm.. something seems to be wrong.\r\n", ch);
    break;
  case BFS_ALREADY_THERE:
    send_to_char("You're already in the same room!!\r\n", ch);
    break;
  case BFS_NO_PATH:
    new_send_to_char(ch,
                     "There are confusing tracks all around you.\r\n");
    break;
  default:			/* Success! */
    new_send_to_char(ch,
                     "{cWYou notice some tracks leading %s from here!{c0\r\n",
                     dirs[dir]);
    return SKILL_TRACK;
    break;
  }
  return 0;
}

/* NOTE: MOB_NOBASH prevents from disarming */
ASKILL(skill_disarm)
{
  bool yesno = 0;
  if (vict == ch)
  {
    send_to_char("Try removing your weapon instead.\r\n", ch);
    return 0;
  }
  if (use_stamina( ch, 15) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }

  else if (AFF_FLAGGED(ch, AFF_CHARM) && (ch->master == vict))
  {
    send_to_char
    ("The thought of disarming your master seems revolting to you.\r\n",
     ch);
    return 0;
  }
  else if (!(obj = GET_EQ(vict, WEAR_WIELD)))
    act("$N is unarmed!", FALSE, ch, 0, vict, TO_CHAR);
  else if (IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_NO_DISARM) ||
           MOB_FLAGGED(vict, MOB_NOBASH) ||
           (number(1, 101) > (!IS_NPC(ch) ?
                              total_chance(ch, SKILL_DISARM) : number(0,
                                                                      100))))
  {
    act("You failed to disarm $N!", FALSE, ch, 0, vict, TO_CHAR);
    damage(vict, ch, number(1, GET_LEVEL(vict)), TYPE_HIT);
  }
  else if (dice(2, GET_STR(ch)) + GET_LEVEL(ch) <=
           dice(2, GET_STR(vict)) + GET_LEVEL(vict))
  {
    act("You almost succeed in disarming $N", FALSE, ch, 0, vict,
        TO_CHAR);
    act("You were almost disarmed by $N!", FALSE, vict, 0, ch,
        TO_CHAR);
    damage(vict, ch, number(1, GET_LEVEL(vict) / 2), TYPE_HIT);
  }
  else
  {

    obj_to_char(unequip_char(vict, WEAR_WIELD), vict);
    if (GET_EQ(vict, WEAR_WIELD_2))
      equip_char(vict, unequip_char(vict, WEAR_WIELD_2), WEAR_WIELD);
    act("You succeed in disarming your enemy!", FALSE, ch, 0, 0,
        TO_CHAR);
    act("Your $p flies from your hands!", FALSE, vict, obj, 0,
        TO_CHAR);
    act("$n disarms $N, $p is fumbled from being wielded.", FALSE, ch, obj, vict,
        TO_NOTVICT);
    yesno = 1;
  }
  start_fighting(ch, vict);
  WAIT_STATE(ch, PULSE_VIOLENCE);
  return (yesno ? SKILL_DISARM : 0);
}


ASKILL(skill_trample)
{
  int chance = 0, dam = 0;
  extern int average_damage(struct char_data *ch);
  char buf[MAX_STRING_LENGTH];
  if (!(RIDING(ch) || GET_RACE(ch) == RACE_CENTAUR))
  {
    send_to_char
    ("You aren't a centaur and you arent riding anything.\n\r",
     ch);
    return 0;
  }
  if (use_stamina( ch, 20) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }

  if (RIDING(ch))
    dam = GET_LEVEL(RIDING(ch));
  else
    dam = GET_LEVEL(ch);

  chance = total_chance(ch, SKILL_TRAMPLE);

  chance += (GET_LEVEL(ch) - GET_LEVEL(vict));

  if (number(1, 101) < chance)
  {
    snprintf(buf, sizeof(buf), "{cyYou use %s to trample $N.{c0",
             (RIDING(ch) ? PERS(RIDING(ch), vict) : "your hooves"));
    act(buf, FALSE, ch, 0, vict, TO_CHAR);
    snprintf(buf, sizeof(buf), "$n uses %s to trample $N.",
             (RIDING(ch) ? PERS(RIDING(ch), vict) : "$s hooves"));
    act(buf, FALSE, ch, 0, vict, TO_VICT);
    snprintf(buf, sizeof(buf), "{cy$n uses %s to trample you!{c0",
             (RIDING(ch) ? PERS(RIDING(ch), vict) : "$s hooves"));
    act(buf, TRUE, ch, 0, vict, TO_NOTVICT);


    GET_POS(vict) = POS_SITTING;

    skill_attack(ch, vict, SKILL_TRAMPLE, TRUE);
    GET_WAIT_STATE(ch) += (3 RL_SEC);
    return SKILL_TRAMPLE;
  }
  else
  {

    act("{cyYou attempt to trample $N but $E dodges out of the way!.{c0", FALSE, ch, 0, vict, TO_CHAR);
    act("$n attempts to trample $n but $E dodges out of the way!",
        FALSE, ch, 0, vict, TO_NOTVICT);
    act("{cy$n attempts to trample you but you dodge out of the way!{c0", FALSE, ch, 0, vict, TO_VICT);
    skill_attack(ch, vict, SKILL_TRAMPLE, FALSE);
    WAIT_STATE(ch, PULSE_VIOLENCE * 3);
    return 0;
  }

}

ASKILL(skill_joust)
{

  if (!vict)
    return 0;

  if (!(RIDING(ch) || GET_RACE(ch) == RACE_CENTAUR))
  {
    new_send_to_char(ch, "You aren't a centaur and you arent riding anything.\n\r");
    return 0;
  }
  if (use_stamina( ch, 10) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  /* knocks vict off horse and onto ground, small damage.
  if vict isnt riding, then big damage. */
  GET_WAIT_STATE(ch) += 1 RL_SEC;
  if (number(1, 101) >
      total_chance(ch, SKILL_JOUST))
  {
    new_send_to_char(ch, "You fail!\r\n");
    GET_WAIT_STATE(ch) += 2 RL_SEC;
    return 0;
  }

  if (RIDING(vict))
  {
    act("$N is knocked from $S mount.", FALSE, ch, 0, vict, TO_CHAR);
    act("You are knocked from your mount by $n's joust.", FALSE, ch, 0, vict, TO_VICT);
    GET_WAIT_STATE(ch) += 1 RL_SEC;
    dismount_char(vict);
  }
  else
  {
    new_send_to_char(ch, "But they aren't riding anything!\r\n");

  }
  return 0;


}

ASKILL(skill_dodge)
{
  struct affected_type af;


  if (use_stamina( ch, 15) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  if (AFF_FLAGGED(ch, AFF_DODGE))
  {
    affect_from_char(ch, SKILL_DODGE);
    send_to_char("Okay, you stop hopping from foot to foot.\r\n", ch);
    return 0;
  }

  /* 101% is a complete failure */
  if (number(1, 101) >
      total_chance(ch, SKILL_DODGE) + dex_app_skill[GET_DEX(ch)].hide)
  {
    send_to_char("Your leg cramps and you fail.\r\n", ch);
    return 0;
  }

  send_to_char("You rise up on the balls of your feet.\r\n", ch);


  af.type = SKILL_DODGE;
  af.expire = -2;
  af.modifier = 0;
  af.location = APPLY_NONE;
  af.bitvector = AFF_DODGE;
  affect_to_char(ch, &af);
  return SKILL_DODGE;

}

ASKILL(skill_phase)
{
  struct affected_type af;


  if (use_stamina( ch, 15) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  if (AFF_FLAGGED(ch, AFF_PHASE))
  {
    affect_from_char(ch, SKILL_PHASE);
    send_to_char("Okay, you move in a single direction only.\r\n", ch);
    return 0;
  }
  /* 101% is a complete failure */
  if (number(1, 101) >
      total_chance(ch, SKILL_PHASE) + dex_app_skill[GET_DEX(ch)].hide)
  {
    send_to_char("You couldn't manage to move fast enough that time and you fail.\r\n", ch);
    return 0;
  }

  send_to_char("You move in multiple directions at once.\r\n", ch);

  af.type = SKILL_PHASE;
  af.expire = -2;
  af.modifier = 0;
  af.location = APPLY_NONE;
  af.bitvector = AFF_PHASE;
  affect_to_char(ch, &af);
  return SKILL_PHASE;

}



ASKILL(skill_grip)
{
  struct affected_type af;
  byte percent;


  if (AFF_FLAGGED(ch, AFF_GRIP))
  {
    affect_from_char(ch, SKILL_GRIP);
    send_to_char("Okay, you loosen your grip.\r\n", ch);
    return 0;
  }
  else
  {
    if (use_stamina( ch, 25) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
    send_to_char("You clench your hand on your weapon.\r\n", ch);

    act("$n clenches $s hand on $s weapon.", FALSE, ch, obj, vict, TO_ROOM);



    percent = number(1, 101);	/* 101% is a complete failure */

    if (percent >
        GET_SKILL(ch, SKILL_GRIP) + dex_app_skill[GET_DEX(ch)].sneak)
    {
      new_send_to_char(ch, "Your hands cramp.\r\n");
      return 0;
    }

    af.type = SKILL_GRIP;
    af.expire = HOURS_TO_EXPIRE(GET_LEVEL(ch)/10+TIERNUM);
    af.modifier = 0;
    af.location = APPLY_NONE;
    af.bitvector = AFF_GRIP;
    affect_to_char(ch, &af);
    return SKILL_GRIP;
  }
}

ASKILL(skill_face)
{

  if (!FIGHTING(ch))
  {
    new_send_to_char(ch, "Type 'hit <victim>'\r\n");
    return 0;
  }

  if (FIGHTING(ch) == vict)
  {
    new_send_to_char(ch, "You redouble your efforts!\r\n");
    return 0;
  }
  if (use_stamina( ch, 8) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  WAIT_STATE(ch, 1 RL_SEC);
  act("$n turns and faces $N!", FALSE, ch, obj, vict, TO_ROOM | TO_NOTVICT);


  act("$n turns to face you!", FALSE, ch, obj, vict, TO_VICT);

  act("You turn to face $N!", FALSE, ch, obj, vict, TO_CHAR);

  stop_fighting(ch);
  start_fighting(ch, vict);
  return SKILL_FACE;
}

ASKILL(skill_focus)
{
  struct affected_type af;
  byte percent;
  if (use_stamina( ch, 5) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }

  if (AFF_FLAGGED(ch, AFF_FOCUS))
  {
    affect_from_char(ch, SKILL_FOCUS);
    send_to_char("Okay, stop focusing your energy.\r\n", ch);
    return 0;
  }
  else
  {
    send_to_char("You block out the sounds around you and focus.\r\n", ch);
    act("$n gets a focused look on $s face.", FALSE, ch, 0, NULL, TO_ROOM);

    percent = number(1, 101);	/* 101% is a complete failure */

    if (percent >
        GET_SKILL(ch, SKILL_FOCUS) + dex_app_skill[GET_DEX(ch)].sneak)
    {
      new_send_to_char(ch, "You can't seem to focus properly.\r\n");
      return 0;
    }

    af.type = SKILL_FOCUS;
    af.expire = HOURS_TO_EXPIRE(GET_LEVEL(ch)/10+TIERNUM);
    af.modifier = 0;
    af.location = APPLY_NONE;
    af.bitvector = AFF_FOCUS;
    affect_to_char(ch, &af);
    return SKILL_FOCUS;
  }
}

ASKILL(skill_grapple)
{
  //char name[100], todir[256];
  int to, fail = FALSE;
  //CHAR_DATA *victim = NULL;
  if (use_stamina( ch, 10) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  to = number(0, 5);

  if (!CAN_GO(vict, to))
    fail = TRUE;
  else if (GET_LEVEL(vict) >= LVL_GOD && GET_LEVEL(ch) != LVL_IMPL)
    fail = TRUE;
  else if (MOB_FLAGGED(vict, MOB_NOBASH))
    fail = TRUE;
  else if (!((dice(1, 20) + 6) - (GET_STR(ch) - GET_STR(vict)) < GET_STR(ch)))
    fail = TRUE;


  if (fail)
  {
    act("You grapple with $N, but slip and $E falls on you!", FALSE, ch, 0, vict, TO_CHAR);
    act("$n grapples with $N, but slips and $E falls on $m!", FALSE, ch, 0, vict, TO_NOTVICT);
    act("$n trys to grapple you, but slips and you fall on $m!", FALSE, ch, 0, vict, TO_VICT);
    damage(ch, ch, GET_WEIGHT(vict), TYPE_UNDEFINED);
    return 0;
  }
  else
  {

    act("You grapple with $N and pick $M up. With a mighty heave you throw $M!", FALSE, ch, 0, vict, TO_CHAR);
    act("$n grapples with you picking you up. With a mighty heave $n throws you!", FALSE, ch, 0, vict, TO_VICT);
    act("$n grapples with $N picking $M up. With a mighty heave $n throws $N!", FALSE, ch, 0, vict, TO_NOTVICT);
    if (damage(vict, vict, GET_WEIGHT(vict), TYPE_UNDEFINED) >= 0)
      perform_grapple(vict, to, TRUE, ch);
    return SKILL_GRAPPLE;
  }

}

ASKILL(skill_beserk)
{
  struct affected_type af;
  byte percent;

  if (use_stamina( ch, 20) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }

  if (AFF_FLAGGED(ch, AFF_BESERK))
  {
    affect_from_char(ch, SKILL_BESERK);
    send_to_char("The red hase lifts from your vision.\r\n", ch);
    return 0;
  }
  else
  {
    send_to_char("Your eyes grow wide and a red hase blurs your vision.\r\n", ch);
    act("$n's eyes grow wide, and $e look out of control!", FALSE, ch, 0, NULL, TO_ROOM);

    percent = number(1, 101);	/* 101% is a complete failure */

    if (percent >
        GET_SKILL(ch, SKILL_BESERK) + dex_app_skill[GET_DEX(ch)].sneak)
    {
      new_send_to_char(ch, "You stumble over your own feet\r\n");
      return 0;
    }

    af.type = SKILL_BESERK;
    af.expire = HOURS_TO_EXPIRE(GET_LEVEL(ch)/10+TIERNUM);
    af.modifier = 0;
    af.location = APPLY_NONE;
    af.bitvector = AFF_BESERK;
    affect_to_char(ch, &af);
    return SKILL_BESERK;
  }
}

ASKILL(skill_meditate)
{
  struct affected_type af;
  byte percent;


  if (AFF_FLAGGED(ch, AFF_MEDITATE))
  {
    affect_from_char(ch, SKILL_MEDITATE);
    send_to_char("Okay, bring your mind back from meditation.\r\n", ch);
    return 0;
  }
  else
  {
    send_to_char("You clear your mind..\r\n", ch);

    percent = number(1, 101);	/* 101% is a complete failure */
    if (percent >
        GET_SKILL(ch, SKILL_MEDITATE) + dex_app_skill[GET_DEX(ch)].sneak)
    {
      new_send_to_char(ch, "You can not seem to clear your thoughts.\r\n");
      return 0;
    }
    act("$n closes $s eyes and starts meditating.", FALSE, ch, 0, NULL, TO_ROOM);

    GET_POS(ch) = POS_RESTING;
    update_pos(ch);

    af.type = SKILL_MEDITATE;
    af.expire = HOURS_TO_EXPIRE(GET_LEVEL(ch)/10+TIERNUM);
    af.modifier = 0;
    af.location = APPLY_NONE;
    af.bitvector = AFF_MEDITATE;
    affect_to_char(ch, &af);
    return SKILL_MEDITATE;
  }
}

ASKILL(skill_true_strike)
{
  struct affected_type af;
  byte percent;

  if (use_stamina( ch, 25) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  if (AFF_FLAGGED(ch, AFF_TRUE_STRIKING))
  {
    affect_from_char(ch, SKILL_TRUE_STRIKE);
    send_to_char("Okay, your strike chance returns to normal.\r\n", ch);
    return 0;
  }
  else
  {
    send_to_char("You chant battlesong to your weapons.\r\n", ch);

    percent = number(1, 101);	/* 101% is a complete failure */

    if (percent >
        GET_SKILL(ch, SKILL_TRUE_STRIKE) + dex_app_skill[GET_DEX(ch)].sneak)
    {
      new_send_to_char(ch, "You stutter and forget the battlesong.\r\n");
      return 0;
    }


    act("$n chants battlesong to $s weapons.", FALSE, ch, 0, NULL, TO_ROOM);
    af.type = SKILL_TRUE_STRIKE;
    af.expire = HOURS_TO_EXPIRE(GET_LEVEL(ch)/10+TIERNUM);
    af.modifier = 5;
    af.location = APPLY_DAMROLL;
    af.bitvector = AFF_TRUE_STRIKING;
    affect_to_char(ch, &af);
    return SKILL_TRUE_STRIKE;
  }
}
//TODO
ASKILL(skill_flank)
{
  new_send_to_char(ch, "This Skill Is Unfinished.\r\n");
  return 0;
}

ASKILL(skill_fortify)
{
  struct affected_type af;
  byte percent;

  if (AFF_FLAGGED(ch, AFF_FORTIFY_BODY))
  {
    affect_from_char(ch, SKILL_FORTIFY);
    send_to_char("Okay, you stop fortifying your body.\r\n", ch);
    return 0;
  }
  else
  {

    if (use_stamina( ch, 25) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
    send_to_char("You clench your muscles and focus your energy.\r\n", ch);

    percent = number(1, 101);	/* 101% is a complete failure */

    if (percent >
        GET_SKILL(ch, SKILL_FORTIFY) + dex_app_skill[GET_DEX(ch)].sneak)
    {
      new_send_to_char(ch, "You can't seem to concentrate.\r\n");
      return 0;
    }
    act("$n clenches $s muscles and fortifies $s energy.", FALSE, ch, 0, NULL, TO_ROOM);

    af.type = SKILL_FORTIFY;
    af.expire = HOURS_TO_EXPIRE(GET_LEVEL(ch)/10+TIERNUM);
    af.modifier = GET_STR(ch)*3;
    af.location = APPLY_HIT;
    af.bitvector = AFF_FORTIFY_BODY;
    affect_to_char(ch, &af);
    return SKILL_FORTIFY;
  }
}

ASKILL(skill_scalp)
{
  extern void crumble_obj(CHAR_DATA *ch, OBJ_DATA *obj);
  OBJ_DATA *scalp = NULL;
  struct extra_descr_data *new_descr = NULL;
  char /*buf1[MAX_STRING_LENGTH],*/ buf2[MAX_STRING_LENGTH];
  char scalpb[MAX_STRING_LENGTH];
  char *scalpa = scalpb;
  int pc = 0;

  if (obj)
  {
    if (!IS_CORPSE(obj))
    {
      new_send_to_char(ch, "You rip off the scalp of... nothing really! Fear your stupidity!\r\n");
      return 0;
    }
    else if (IS_OBJ_STAT(obj, ITEM_PC_CORPSE) && GET_OBJ_VAL(obj, 6) == 0)
    {
      new_send_to_char(ch, "They aren't a PK'er!\r\n");
      return 0;
    }
    if (use_stamina( ch, 80) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
    pc = IS_OBJ_STAT(obj, ITEM_PC_CORPSE);

    act("You slice the scalp from $p.", FALSE, ch, obj, NULL, TO_CHAR);
    act("$n slices the scalp from $p.", FALSE, ch, obj, NULL, TO_ROOM);

    strcpy(scalpa, obj->short_description);
    crumble_obj(ch, obj);
    scalpa = scalpa+14;
    scalp = create_obj();

    scalp->item_number = NOTHING;
    scalp->in_room = NULL;
    snprintf(buf2, sizeof(buf2), "scalp %s", scalpa);
    scalp->name = str_dup(buf2);

    if (pc)
      snprintf(buf2, sizeof(buf2), "The scalp of %s lies here.", scalpa);
    else
      snprintf(buf2, sizeof(buf2), "The tattered scalp of %s lies here.", scalpa);

    scalp->description = str_dup(buf2);

    if (pc)
      sprintf(buf2, "the scalp of %s", scalpa);
    else
      sprintf(buf2, "the tattered scalp of %s", scalpa);

    scalp->short_description = str_dup(buf2);

    /* extra description coolness! */
    CREATE(new_descr, struct extra_descr_data, 1);
    new_descr->keyword = str_dup(buf2);
    sprintf(buf2, "It appears to be the scalp of %s.",scalpa);
    new_descr->description = str_dup(buf2);
    new_descr->next = NULL;
    scalp->ex_description = new_descr;
    GET_OBJ_TYPE(scalp) = ITEM_TREASURE;

    SET_BIT_AR(GET_OBJ_WEAR(scalp), ITEM_WEAR_HOLD);
    SET_BIT_AR(GET_OBJ_WEAR(scalp), ITEM_WEAR_TAKE);
    SET_BIT_AR(GET_OBJ_EXTRA(scalp), ITEM_UNIQUE_SAVE);
    GET_OBJ_WEIGHT(scalp) = 3;
    GET_OBJ_RENT(scalp) = 0;
    obj_to_char(scalp, ch);
    return SKILL_SCALP;

  }

  /*short_desc is +14, then all the way to 0
  IS_OBJ_STAT(obj, ITEM_NPC_CORPSE)
  PC_CORPSE
  */
  return 0;

}

ASKILL(skill_blade_dance)
{
  struct affected_type af;
  byte percent;


  if (AFF_FLAGGED(ch, AFF_BLADEDANCE))
  {
    affect_from_char(ch, SKILL_BLADE_DANCE);
    send_to_char("Okay, you stop dancing the blades.\r\n", ch);
    return 0;
  }
  else
  {
    if (use_stamina( ch, 30) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
    send_to_char("You spin your weapons around in your fingers.\r\n", ch);

    percent = number(1, 101);	/* 101% is a complete failure */

    if (percent >
        GET_SKILL(ch, SKILL_BLADE_DANCE) + dex_app_skill[GET_DEX(ch)].sneak)
    {
      new_send_to_char(ch, "You stumble over your own feet\r\n");
      return 0;
    }

    act("$n spins $s weapons around between $s fingers.", FALSE, ch, 0, NULL, TO_ROOM);

    af.type = SKILL_BLADE_DANCE;
    af.expire = HOURS_TO_EXPIRE((int)((GET_LEVEL(ch)/10)+TIERNUM));
    af.modifier = 5;
    af.location = APPLY_HITROLL;
    af.bitvector = AFF_BLADEDANCE;
    affect_to_char(ch, &af);
    return SKILL_BLADE_DANCE;
  }
}

ASKILL(skill_cleave)
{
  OBJ_DATA *wep = GET_EQ(ch, WEAR_WIELD);


  if (GET_OBJ_VAL(wep, 3) != TYPE_SLASH - TYPE_HIT)
  {
    send_to_char("You need to wield a slashing weapon to cleave.\r\n",  ch);
    return 0;
  }
  if (use_stamina( ch, 5) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  /*
      act("You swing your $p like a cleaver at $N!", FALSE, ch, wep, vict, TO_CHAR);
      act("$n swings $s $p like a cleaver at you!", FALSE, ch, wep, vict, TO_VICT);
      act("$n swings $s $p like a cleaver at $N!", FALSE, ch, wep, vict, TO_ROOM);
   
      WAIT_STATE(ch, (PULSE_VIOLENCE * 3)/(TIERNUM+2));
  */

  /* Only appropriately skilled PCs and uncharmed mobs */
  corpse_mod = 2;
  skill_attack(ch, vict, SKILL_CLEAVE, (IS_NPC(ch) ? GET_LEVEL(ch) : GET_SKILL(ch, SKILL_CLEAVE)) > number(0, 100));
  corpse_mod = 0;
  return SKILL_CLEAVE;
}

ASKILL(skill_behead)
{
  OBJ_DATA *wep = GET_EQ(ch, WEAR_WIELD);


  if (GET_OBJ_VAL(wep, 3) != TYPE_SLASH - TYPE_HIT)
  {
    send_to_char("You need to wield a slashing weapon to behead.\r\n",  ch);
    return 0;
  }
  if (use_stamina( ch, 5) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  /*
      act("You swing your $p at $N's neck!", FALSE, ch, wep, vict, TO_CHAR);
      act("$n swings $s $p at your neck!", FALSE, ch, wep, vict, TO_VICT);
      act("$n swings $s $p at $N's neck!!", FALSE, ch, wep, vict, TO_ROOM);
   
      WAIT_STATE(ch, (PULSE_VIOLENCE * 3)/(TIERNUM+2));
  */
  /* Only appropriately skilled PCs and uncharmed mobs */
  corpse_mod = 1;
  skill_attack(ch, vict, SKILL_BEHEAD, (IS_NPC(ch) ? GET_LEVEL(ch) : GET_SKILL(ch, SKILL_BEHEAD)) > number(0, 100));
  corpse_mod = 0;
  return SKILL_BEHEAD;
}

ASKILL(skill_martial_arts)
{

  struct affected_type af[2];
  byte percent;
  int i;

  WAIT_STATE(ch, 3 RL_SEC);
  if (AFF_FLAGGED(ch, AFF_MARTIAL_ARTS))
  {
    affect_from_char(ch, SKILL_MARTIAL_ARTS);
    send_to_char("Okay, you stop using your martial arts skills.\r\n", ch);
    return 0;
  }
  else
  {
    send_to_char("You stetch your arms and legs limbering up for a fight.\r\n", ch);
    percent = number(1, 101);	/* 101% is a complete failure */
    if (use_stamina( ch, 15) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
    if (percent >
        GET_SKILL(ch, SKILL_MARTIAL_ARTS) + dex_app_skill[GET_DEX(ch)].sneak)
    {
      new_send_to_char(ch, "You stumble over your own feet\r\n");
      return 0;
    }
    for (i = 0; i < 2; i++)
    {
      af[i].type = SKILL_MARTIAL_ARTS;
      af[i].bitvector = 0;
      af[i].modifier = 0;
      af[i].location = APPLY_NONE;
    }
    act("$n stretches $s arms and legs, ready for a fight.", FALSE, ch, 0, NULL, TO_ROOM);

    af[0].expire = HOURS_TO_EXPIRE(((GET_LEVEL(ch)/5)+TIERNUM));
    af[0].modifier = 0;
    af[0].location = APPLY_NONE;
    af[0].bitvector = AFF_MARTIAL_ARTS;
    af[0].type = SKILL_MARTIAL_ARTS;
    affect_to_char(ch, &af[0]);

    if (GET_COND(ch, DRUNK) > 18)
    {
      af[1].expire = HOURS_TO_EXPIRE(((GET_LEVEL(ch)/5)+TIERNUM));
      af[1].modifier = 0;
      af[1].location = APPLY_NONE;
      af[1].bitvector = AFF_DRUNKEN_MASTER;
      af[1].type = SKILL_MARTIAL_ARTS;
      affect_to_char(ch, &af[1]);
    }

    return SKILL_MARTIAL_ARTS;
  }
}

ASKILL(skill_punch)
{
  new_send_to_char(ch, "This Skill Is Unfinished.\r\n");
  return 0;
}


ASKILL(skill_slip)
{
  char buf[MAX_INPUT_LENGTH];
  int fail = FALSE;
  argument = one_argument(argument, buf);
  WAIT_STATE(ch, 2 RL_SEC);
  slipping = TRUE;
  if (isname("give", buf))
    do_give(ch, argument, 0, 0);
  else if (isname("get", buf))
    do_get(ch, argument, 0, 0);
  else if (isname("drop", buf))
    do_drop(ch, argument, 0, SCMD_DROP);
  else if (isname("put", buf))
    do_put(ch, argument, 0, 0);
  else
  {
    new_send_to_char(ch, "Sorry, but only the commands: give, get, drop and put\r\n");
    fail = TRUE;
  }
  slipping = FALSE;
  return ((fail || !number(0, 60))? TYPE_UNDEFINED :SKILL_SLIP );
}

ASKILL(skill_hyperactivity)
{
  struct affected_type af;
  byte percent;
  WAIT_STATE(ch, 3 RL_SEC);
  if (AFF_FLAGGED(ch, AFF_HYPERACTIVITY))
  {
    send_to_char("You take your ritilen.\r\n", ch);
    affect_from_char(ch, SKILL_HYPERACTIVITY);
    return 0;
  }
  else
    if (use_stamina( ch, 5) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
  send_to_char("Okay, you start to spin out!!\r\n", ch);


  percent = number(1, 101);	/* 101% is a complete failure */

  if (percent > GET_SKILL(ch, SKILL_HYPERACTIVITY))
  {
    new_send_to_char(ch, "You feel calm.\r\n");
    return 0;
  }

  act("$n spins out, bouncing off the walls!", FALSE, ch, 0, NULL, TO_ROOM);

  af.type = SKILL_HYPERACTIVITY;
  af.expire = HOURS_TO_EXPIRE(3);
  af.modifier = 100;
  af.location = APPLY_SPEED;
  af.bitvector = AFF_HYPERACTIVITY;
  affect_to_char(ch, &af);
  return SKILL_HYPERACTIVITY;
}

ASKILL(skill_holy_strength)
{
  struct affected_type af;
  byte percent;
  WAIT_STATE(ch, 3 RL_SEC);
  if (AFF_FLAGGED(ch, AFF_HOLY_STRENGTH))
  {
    send_to_char("Your muscles return to normal size.\r\n", ch);
    affect_from_char(ch, SKILL_HOLY_STRENGTH);
    return 0;
  }
  else
    if (use_stamina( ch, 15) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
  send_to_char("Your muscles bulge!!\r\n", ch);


  percent = number(1, 101);	/* 101% is a complete failure */

  if (percent > GET_SKILL(ch, SKILL_HOLY_STRENGTH))
  {
    new_send_to_char(ch, "You still feel weak.\r\n");
    return 0;
  }

  act("$n's muscles bulge with strength!", FALSE, ch, 0, NULL, TO_ROOM);

  af.type = SKILL_HOLY_STRENGTH;
  af.expire = HOURS_TO_EXPIRE(3);
  af.modifier = 30;
  af.location = APPLY_STR;
  af.bitvector = AFF_HOLY_STRENGTH;
  affect_to_char(ch, &af);
  return (SKILL_HOLY_STRENGTH);
}

ASKILL(skill_brace)
{
  byte percent;
  struct affected_type af;

  send_to_char("You attempt to brace yourself.\r\n", ch);
  WAIT_STATE(ch, 3 RL_SEC);
  if (AFF_FLAGGED(ch, AFF_BRACE))
  {
    affect_from_char(ch, SKILL_BRACE);
    send_to_char("You stop bracing yourself.\r\n", ch);
    return 0;
  }
  if (use_stamina( ch, 5) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  percent = number(1, 101);	/* 101% is a complete failure */

  if (percent > GET_SKILL(ch, SKILL_BRACE))
  {
    send_to_char("You didn't manage to brace yourself.\r\n", ch);
    return 0;
  }
  af.type = SKILL_BRACE;
  af.expire = -2;
  af.modifier = 0;
  af.location = APPLY_NONE;
  af.bitvector = AFF_BRACE;
  affect_to_char(ch, &af);

  return (SKILL_BRACE);
}



ASKILL(skill_push)
{
  char name[100], todir[256];
  int to;
  CHAR_DATA *victim = NULL;
  char buf[MAX_STRING_LENGTH];

  half_chop(argument, name, todir);
  if (!*name || !*todir)
    send_to_char("Push whom where?\r\n", ch);
  else if (!(victim = get_char_room_vis(ch, name, NULL)))
    send_to_char("Noone by that name here.\r\n", ch);
  else if (ch == victim)
    send_to_char("But... can you walk?\r\n", ch);
  else if (IS_AFFECTED(ch, AFF_CHARM))
    send_to_char("No, no....\r\n", ch);
  else if (MOB_FLAGGED(victim, MOB_NOPUSH))
    send_to_char("Your victim pushes you back.\r\n", ch);
  else if ((to = search_block(todir, dirs, FALSE)) < 0)
  {
    send_to_char("That is not a direction.\r\n", ch);
    return 0;

  }
  else
  {
    int stren = (GET_STR(ch) * GET_SKILL(ch, SKILL_PUSH))/100;
    GET_WAIT_STATE(ch) += 3 RL_SEC;
    if (use_stamina( ch, 20) < 0)
    {
      new_send_to_char(ch, "You are far too exausted!");
      return 0;
    }
    strcpy(todir, dirs[to]);
    if (GET_POS(victim) <= POS_SITTING)
    {
      send_to_char
      ("You can't push anybody while they are lying on the ground",
       ch);
      return 0;
    }
    if (GET_POS(victim) == POS_FIGHTING)
    {
      sprintf(buf, "No! You can't push %s while fighting!\r\n",
              HSSH(ch));
      send_to_char(buf, ch);
      return 0;
    }
    sprintf(buf, "$n is trying to push you %s!", todir);
    act(buf, FALSE, ch, 0, victim, TO_VICT);
    act("$n is trying to push $N", FALSE, ch, 0, victim, TO_NOTVICT);
    if (!CAN_GO(victim, to))
    {
      new_send_to_char(ch, "Can't push %s there\r\n", HMHR(ch));
    }
    else if (GET_LEVEL(victim) >= LVL_GOD
             && GET_LEVEL(ch) != LVL_IMPL)
    {
      send_to_char("Oh, no, no, no.\r\n", ch);
      send_to_char("Is trying to push you... what a mistake!\r\n",
                   victim);
    }
    else if (GET_LEVEL(victim) - GET_LEVEL(ch) > 4)
    {
      new_send_to_char(ch, "You can't push %s.\r\n", HMHR(victim));

      new_send_to_char(ch, "%s can't push you.\r\n", GET_NAME(ch));
    }
    else if (MOB_FLAGGED(victim, MOB_NOBASH))
    {
      send_to_char("Ouch! Is too big for you!\r\n", ch);
    }
    else if ((dice(1, 20) + 3) - (stren - GET_STR(victim)) <
             stren)
    {
      /* You can balance the check above, this works fine for me */
      if (perform_push(victim, to, TRUE, ch))
      {
        send_to_char("Ciao, ciao!\r\n", ch);
        new_send_to_char(victim, "%s has pushed you!", GET_NAME(ch));
        return SKILL_PUSH;
      }
    }
    else
    {
      send_to_char("Oops... you fail.", ch);
      new_send_to_char(victim, "%s fails.\r\n", GET_NAME(ch));

    }
  }
  return 0;

}

ASKILL(skill_scan)
{

  CHAR_DATA *i;
  int  dir = 0, dis, maxdis, x, found = 0;
  room_rnum location = NULL, original_loc, is_in;
  char arg[MAX_STRING_LENGTH];

  const dirParseStruct dirParse[6] =
    {
      {SCMD_NORTH - 1, "north"},
      {SCMD_SOUTH - 1, "south"},
      {SCMD_EAST - 1, "east"},
      {SCMD_WEST - 1, "west"},
      {SCMD_UP - 1, "up"},
      {SCMD_DOWN - 1, "down"}
    };


  const char *distance[] =
    {
      "right here",
      "immediately ",
      "nearby ",
      "a ways ",
      "far ",
      "very far ",
      "extremely far ",
      "impossibly far ",
    };

  if (IS_AFFECTED(ch, AFF_BLIND))
  {
    act("You can't see anything, you're blind!", TRUE, ch, 0, 0,
        TO_CHAR);
    return 0;
  }

  if (use_stamina( ch, 3) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  WAIT_STATE(ch, 1 RL_SEC);

  maxdis = (1 + ((GET_SKILL(ch, SKILL_SCAN) * 5) / 100));
  if (GET_LEVEL(ch) >= LVL_GOD)
    maxdis = 7;

  one_argument(argument, arg);

  act("You quickly scan the area and see:", TRUE, ch, 0, 0, TO_CHAR);
  act("$n quickly scans the area.", FALSE, ch, 0, 0, TO_ROOM);

  if (number(1, 100) > GET_SKILL(ch, SKILL_SCAN))
  {
    send_to_char("You squint, but your eyesight fails you.\r\n", ch);
    return 0;
  }

  for (x = 0; x < NUM_OF_DIRS; x++)
  {
    if (is_abbrev(arg, dirParse[x].dirCmd))
    {
      dir = dirParse[x].dirNum;
      break;
    }
    else
      dir = (-1);
  }
  if (dir != -1)
  {
    original_loc = IN_ROOM(ch);

    if (ch == NULL)
      /* But something is invalid */
      return 0;
    else if (dir >= NUM_OF_DIRS)
    {
      send_to_char("That is not a valid direction\r\n", ch);
      return 0;
    }
    else if (!original_loc || !original_loc->dir_option[dir]
             || original_loc->dir_option[dir]->to_room == NULL)
    {
      /* But there is no exit that way */
      send_to_char
      ("Alas, you cannot see through that particular wall...\r\n",
       ch);
      return 0;
    }
    else
      if (IS_SET
          (original_loc->dir_option[dir]->exit_info,
           EX_CLOSED))
      {
        /* But the door is closed */
        if (original_loc->dir_option[dir]->keyword)
        {
          new_send_to_char(ch, "The %s seems to be closed.\r\n",
                           fname(original_loc->dir_option[dir]->keyword));
        }
        else
          send_to_char("It seems to be closed.\r\n", ch);
      }



    /* a location has been found. */
    original_loc = IN_ROOM(ch);
    location = original_loc->dir_option[dir]->to_room;
    char_from_room(ch);
    char_to_room(ch, location);
    LOOK(ch);

    /* check if the char is still there */
    if (IN_ROOM(ch) == location)
    {
      char_from_room(ch);
      char_to_room(ch, original_loc);
    }

    return SKILL_SCAN;

  }





  is_in = IN_ROOM(ch);
  for (dir = 0; dir < NUM_OF_DIRS; dir++)
  {
    IN_ROOM(ch) = is_in;
    for (dis = 0; dis <= maxdis; dis++)
    {
      if (((dis == 0) && (dir == 0)) || (dis > 0))
      {
        for (i = IN_ROOM(ch)->people; i; i = i->next_in_room)
        {
          if ((!((ch == i) && (dis == 0))) && CAN_SEE(ch, i))
          {
            new_send_to_char(ch, "%33s: %s%s%s%s\r\n", GET_NAME(i),
                             distance[dis],
                             ((dis > 0) && (dir < (NUM_OF_DIRS - 2))) ? "to the " : "",
                             (dis > 0) ? dirs[dir] : "",
                             ((dis > 0) && (dir > (NUM_OF_DIRS - 3))) ? "wards" : "");
            found++;
          }
        }
      }
      if (!CAN_GO(ch, dir) || (IN_ROOM(ch)->dir_option[dir]->to_room == is_in))
        break;
      else
        IN_ROOM(ch) = IN_ROOM(ch)->dir_option[dir]->to_room;
    }
  }
  if (found == 0)
    act("Nobody anywhere near you.", TRUE, ch, 0, 0, TO_CHAR);
  IN_ROOM(ch) = is_in;
  return (SKILL_SCAN);

}

/* These Skills kept in act.movement.c*/
/*
ASKILL(skill_mount){ }
ASKILL(skill_tame){ }
ASKILL(skill_snare){ }
ASKILL(skill_blackjack){ }
*/



/* Poison weapon by Thelonius for EnvyMud */
ASKILL(skill_poison_weapon)
{
  OBJ_DATA *pobj, *wobj;
  char arg[MAX_STRING_LENGTH];


  one_argument(argument, arg);

  if (arg[0] == '\0')
  {
    send_to_char("What are you trying to poison?\n\r", ch);
    return 0;
  }

  if (!(obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
  {
    send_to_char("You do not have that weapon.\n\r", ch);
    return 0;
  }
  if (GET_OBJ_TYPE(obj) != ITEM_WEAPON)
  {
    send_to_char("That item is not a weapon.\n\r", ch);
    return 0;
  }
  if (IS_OBJ_STAT(obj, ITEM_POISONED_1) ||
      IS_OBJ_STAT(obj, ITEM_POISONED_2) ||
      IS_OBJ_STAT(obj, ITEM_POISONED_3) ||
      IS_OBJ_STAT(obj, ITEM_POISONED_4))
  {
    send_to_char("That weapon is already poisoned.\n\r", ch);
    return 0;
  }
  if (!skill_cost(0, 10, 150, ch))
  {
    new_send_to_char(ch, "You are exausted!");
    return 0;
  }
  /* Now we have a valid weapon...check to see if we have the powder. */
  for (pobj = ch->carrying; pobj; pobj = pobj->next_content)
  {
    if ((GET_OBJ_TYPE(pobj) == ITEM_POISON_1) ||
        (GET_OBJ_TYPE(pobj) == ITEM_POISON_2) ||
        (GET_OBJ_TYPE(pobj) == ITEM_POISON_3) ||
        (GET_OBJ_TYPE(pobj) == ITEM_POISON_4))
      break;
  }
  if (!pobj)
  {
    send_to_char("You do not have the poison.\n\r", ch);
    return 0;
  }

  /* Okay, we have the powder...do we have water? */
  for (wobj = ch->carrying; wobj; wobj = wobj->next_content)
  {
    if (GET_OBJ_TYPE(wobj) == ITEM_DRINKCON
        && GET_OBJ_VAL(wobj, 1) > 0 && GET_OBJ_VAL(wobj, 2) == 0)
      break;
  }
  if (!wobj)
  {
    send_to_char("You have no water to mix with the powder.\n\r", ch);
    return 0;
  }

  /* Great, we have the ingredients...but is the thief smart enough? */
  if (!IS_NPC(ch) && GET_WIS(ch) < 16)
  {
    send_to_char("You can't quite remember what to do...\n\r", ch);
    return 0;
  }
  /* And does the thief have steady enough hands? */
  if (!IS_NPC(ch)
      && (GET_DEX(ch) < 17 || GET_COND(ch, DRUNK) > 0))
  {
    send_to_char
    ("Your hands aren't steady enough to properly mix the poison.\n\r",
     ch);
    return 0;
  }

  WAIT_STATE(ch, 4 * PULSE_VIOLENCE);

  /* Check the skill percentage */
  if (!IS_NPC(ch)
      && number(1, 101) > GET_SKILL(ch, SKILL_POISON_WEAPON))
  {
    send_to_char("You failed and spill some on yourself.  Ouch!\n\r",
                 ch);
    damage(ch, ch, GET_LEVEL(ch), -1);
    act("$n spills the poison all over!", FALSE, ch, NULL, NULL,
        TO_ROOM);
    extract_obj(pobj);
    extract_obj(wobj);
    return 0;
  }

  /* Well, I'm tired of waiting.  Are you? */
  act("You mix $p in $P, creating a deadly poison!",
      FALSE, ch, pobj, wobj, TO_CHAR);
  act("$n mixes $p in $P, creating a deadly poison!",
      FALSE, ch, pobj, wobj, TO_ROOM);
  act("You pour the poison over $p, which glistens wickedly!",
      FALSE, ch, obj, NULL, TO_CHAR);
  act("$n pours the poison over $p, which glistens wickedly!",
      FALSE, ch, obj, NULL, TO_ROOM);

  if (GET_OBJ_TYPE(pobj) == ITEM_POISON_1)
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_POISONED_1);
  else if (GET_OBJ_TYPE(pobj) == ITEM_POISON_2)
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_POISONED_2);
  else if (GET_OBJ_TYPE(pobj) == ITEM_POISON_3)
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_POISONED_3);
  else if (GET_OBJ_TYPE(pobj) == ITEM_POISON_4)
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_POISONED_4);

  /* Set an object timer.  Dont want proliferation of poisoned weapons */
  GET_OBJ_TIMER(obj) = 10 + GET_LEVEL(ch);

  if (IS_OBJ_STAT(obj, ITEM_BLESS))
    GET_OBJ_TIMER(obj) *= 2;

  if (IS_OBJ_STAT(obj, ITEM_MAGIC))
    GET_OBJ_TIMER(obj) *= 2;

  /* WHAT?  All of that, just for that one bit?  How lame. ;) */
  act("The remainder of the poison eats through $p.",
      FALSE, ch, wobj, NULL, TO_CHAR);
  act("The remainder of the poison eats through $p.",
      FALSE, ch, wobj, NULL, TO_ROOM);
  extract_obj(pobj);
  extract_obj(wobj);
  return ( SKILL_POISON_WEAPON);

}

ASKILL(skill_retreat)
{
  int prob, percent, dir = 0;
  int retreat_type;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!FIGHTING(ch))
  {
    send_to_char("You are not fighting!\r\n", ch);
    return 0;
  }
  if (!*arg)
  {
    send_to_char("Retreat where?!?\r\n", ch);
    return 0;
  }

  retreat_type = search_block(argument + 1, dirs, FALSE);
  dir = retreat_type;

  if (retreat_type < 0 || !EXIT(ch, retreat_type) ||
      EXIT(ch, retreat_type)->to_room == NULL)
  {
    send_to_char("Retreat where?\r\n", ch);
    return 0;
  }

  percent = GET_SKILL(ch, SKILL_RETREAT);
  prob = number(0, 101);

  if (prob <= percent)
  {
    if (CAN_GO(ch, dir) &&
        !IS_SET_AR(ROOM_FLAGS(EXIT(ch, dir)->to_room), ROOM_DEATH))
    {
      act("$n skillfully retreats from combat.", TRUE, ch, 0, 0,
          TO_ROOM);
      send_to_char("You skillfully retreat from combat.\r\n", ch);
      WAIT_STATE(ch, 2 RL_SEC);
      halt_fighting(ch);
      do_simple_move(ch, dir, TRUE);
      return SKILL_RETREAT;
    }
    else
    {
      act("$n tries to retreat from combat but has no where to go!",
          TRUE, ch, 0, 0, TO_ROOM);
      send_to_char("You cannot retreat in that direction!\r\n", ch);
      return 0;
    }
  }
  else
  {
    send_to_char("You fail your attempt to retreat!\r\n", ch);
    WAIT_STATE(ch, PULSE_VIOLENCE);
    return 0;
  }
}

ASKILL(skill_filet)
{
  OBJ_DATA *food = NULL;
  char arg[MAX_INPUT_LENGTH];
  int prob = 0;

  one_argument(argument, arg);


  if (!*arg)
  {
    send_to_char("What do you want to filet?\r\n", ch);
    return 0;
  }

  if (!
      (obj =
         get_obj_in_list_vis(ch, arg, NULL,
                             IN_ROOM(ch)->contents)))
  {
    send_to_char("There is nothing like that here. Try again.\r\n",
                 ch);
    return 0;
  }

  if (!IS_OBJ_STAT(obj, ITEM_NPC_CORPSE))
  {
    if (!IS_OBJ_STAT(obj, ITEM_EDIBLE))
    {
      send_to_char("You can't make a filet out of that.\r\n", ch);
      return 0;
    }
  }
  if (use_stamina( ch, 15) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  prob = number(1, 101);

  if (prob > total_chance(ch, SKILL_FILET))
  {
    send_to_char
    ("As you begin to filet the corpse, you make a mistake and destroy it.\r\n",
     ch);
    extract_obj(obj);
    return 0;
  }

  /* we've got an obj that is a mob corpse and can be food */
  if ((food = read_object(30, VIRTUAL)) != NULL)
  {
    send_to_char
    ("You skillfully cut a hunk of meat from the corpse and pick it up.\r\n",
     ch);
    act("$n skillfully cuts a hunk of meat from the corpse and picks it up.\r\n", FALSE, ch, 0, 0, TO_ROOM);
    obj_to_char(food, ch);
    extract_obj(obj);
    return SKILL_FILET;
  }
  else
    new_send_to_char(ch, "Your hands turn numb and you fumble.\r\n");
  return 0;
}



ASKILL(skill_forage)
{
  OBJ_DATA *item_found = NULL;
  int item_no = 51;		/* Initialize with first item poss. */




  if (SECT(IN_ROOM(ch)) != SECT_FIELD && SECT(IN_ROOM(ch)) != SECT_FOREST
      && SECT(IN_ROOM(ch)) != SECT_HILLS
      && SECT(IN_ROOM(ch)) != SECT_MOUNTAIN
      && SECT(IN_ROOM(ch)) != SECT_DESERT)
  {
    send_to_char("You cannot forage on this type of terrain!\r\n", ch);
    return 0;
  }
  if (use_stamina( ch, 5) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }
  send_to_char("You start searching the area.\r\n",ch);
  act("$n starts foraging the area.", FALSE, ch, 0, 0,
      TO_ROOM);
  if (IS_NPC(ch) || (number(1, 125) > GET_SKILL(ch, SKILL_FORAGE)))
  {
    WAIT_STATE(ch, PULSE_VIOLENCE * 2);
    new_send_to_char(ch, "You have no luck finding anything.\r\n");
    return 0;
  }
  else
  {
    item_no = 50 + number(1, 11);
    if ((item_found = read_object(item_no, VIRTUAL)) == NULL)
    {
      new_send_to_char(ch, "You have no luck finding anything to eat\r\n");
      return 0;
    }
    obj_to_char(item_found, ch);

    act("You have found $p!", FALSE, ch, item_found, 0, TO_CHAR);

    if ((number(1, 125) > GET_SKILL(ch, SKILL_FORAGE)) && ((GET_OBJ_TYPE(item_found) == ITEM_DRINKCON) ||
        (GET_OBJ_TYPE(item_found) == ITEM_FOUNTAIN) ||
        (GET_OBJ_TYPE(item_found) == ITEM_FOOD)) && GET_OBJ_VAL(item_found, 3))
    {
      GET_OBJ_VAL(item_found, 3) = 0;
      act("$p steams briefly as you sterilize it.", FALSE, ch, item_found, 0, TO_CHAR);
    }

    act("$n has found something in $S forage attempt.\r\n", FALSE, ch, 0, 0, TO_ROOM);
    return SKILL_FORAGE;
  }
}

ASKILL(skill_circle)
{
  OBJ_DATA *wep = GET_EQ(ch, WEAR_WIELD);


  if (!FIGHTING(vict))
  {
    act("Why?  $E isn't bothering anyone.", FALSE, ch, NULL, vict,  TO_CHAR);
    return 0;
  }

  if (GET_OBJ_VAL(wep, 3) != TYPE_PIERCE - TYPE_HIT)
  {
    send_to_char("You need to wield a piercing weapon to circle.\r\n",
                 ch);
    return 0;
  }
  if (use_stamina( ch, 20) < 0)
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }

  if (!skill_cost(0, 0, 25, ch))
  {
    new_send_to_char(ch, "You are far too exausted!");
    return 0;
  }

  act("You circle around behind $N...", FALSE, ch, NULL, vict, TO_CHAR);
  act("$n circles around behind $N...", FALSE, ch, NULL, vict, TO_NOTVICT);
  if (number(0, 120) < total_chance(ch, SKILL_CIRCLE))
    skill_attack(ch, vict, SKILL_BACKSTAB, TRUE);
  else
    skill_attack(ch, vict, SKILL_BACKSTAB, FALSE);


  WAIT_STATE(ch, 3 RL_SEC);


  return SKILL_CIRCLE;

}

void improve_skill(CHAR_DATA *ch, int skill)
{
  extern struct spell_info_type spell_info[];

  int percent = GET_SKILL(ch, skill);
  int splevel = (spell_info[skill].min_level);
  gold_int share;

  if (skill == TYPE_UNDEFINED)
    return;

  if (number(1, 1000) > GET_WIS(ch) + GET_INT(ch))
    return;
  if (percent >= 97 || percent <= 0)
    return;

  if (FIRST_PRE(skill) != TYPE_UNDEFINED)
  {
    if (GET_SKILL(ch, FIRST_PRE(skill)))
    {
      if (!number(0, 10))
        improve_skill(ch, FIRST_PRE(skill));
    }
    else
      return;
  }
  if (SECOND_PRE(skill) != TYPE_UNDEFINED)
  {
    if (GET_SKILL(ch, SECOND_PRE(skill)))
    {
      if (!number(0, 10))
        improve_skill(ch, SECOND_PRE(skill));
    }
    else
      return;
  }

  percent++;

  if (percent >= 97)
    share = exp_needed(ch) * 0.2;
  else
    share = number(50, 120) * (percent) * splevel;

  SET_SKILL(ch, skill, percent);
  new_send_to_char(ch, "You feel your ability in %s %s.\r\n",  skill_name(skill), (percent >= 97) ? "reach full strength" : "improve");
  gain_exp(ch, share);

  return;
}

int total_chance(CHAR_DATA *ch, int skill)
{
  int count = 0, total = 0, check = TRUE;
  int skch = 0;
  //    int cls = (IS_NPC(ch) ? 0 : GET_CLASS(ch));
  if (!knows_spell(ch, skill))
    return 0;

  if (FIRST_PRE(skill) != TYPE_UNDEFINED)
  {
    if ((skch = GET_SKILL(ch, FIRST_PRE(skill))) > 1)
    {
      count++;
      total += skch;
    }
    else
    {
      check = FALSE;
    }
  }

  if ((SECOND_PRE(skill) != TYPE_UNDEFINED))
  {
    if ((skch = GET_SKILL(ch, SECOND_PRE(skill))) > 1)
    {
      count++;
      total += skch;
    }
    else
    {
      check = FALSE;
    }
  }

  if (check)
  {
    if ((skch = GET_SKILL(ch, skill)) > 1)
    {
      count++;
      total +=  skch;
      return (total / count);
    }
    else
      return 0;
  }
  else
    return 0;
}

int perform_grapple(CHAR_DATA *ch, int dir, int need_specials_check,
                    CHAR_DATA *attacker)
{
  room_rnum was_in;
  int House_can_enter(CHAR_DATA *ch, room_vnum house);
  void death_cry(CHAR_DATA *ch);
  int special(CHAR_DATA *ch, int cmd, char *arg);
  int fail = FALSE;


  if (need_specials_check && special(ch, dir + 1, ""))
    fail = TRUE;

  if (IS_SET_AR(ROOM_FLAGS(IN_ROOM(ch)), ROOM_ATRIUM))
  {
    if (!House_can_enter(ch, EXIT(ch, dir)->to_room->number))
    {
      fail = TRUE;
    }
  }
  if (IS_SET_AR(ROOM_FLAGS(EXIT(ch, dir)->to_room), ROOM_TUNNEL) &&
      EXIT(ch, dir)->to_room->people != NULL)
  {
    fail = TRUE;
  }
  if (!enter_wtrigger(EXIT(ch, dir)->to_room, ch, dir))
    fail = TRUE;

  if (fail)
  {
    act("You hit the wall, and slide down to the ground.", FALSE, ch, 0, NULL, TO_CHAR);
    act("$n hits the wall, and slides down to the ground.", FALSE, ch, 0, NULL, TO_ROOM);
    start_fighting(ch, attacker);
    return 0;
  }
  else
  {
    act("You sail through the air out of the room.", FALSE, ch, 0, NULL, TO_CHAR);
    act("$n sails through the air, out of the room.", FALSE, ch, 0, NULL, TO_ROOM);
  }

  was_in = IN_ROOM(ch);
  char_from_room(ch);
  char_to_room(ch, was_in->dir_option[dir]->to_room);
  act("$n tumbles into the room.", TRUE, ch, 0, 0, TO_ROOM);

  GET_POS(ch) = POS_SITTING;
  update_pos(ch);
  if (ch->desc != NULL)
    LOOK(ch);

  if (IS_SET_AR(ROOM_FLAGS(IN_ROOM(ch)), ROOM_DEATH) &&
      GET_LEVEL(ch) <= LVL_HERO)
  {
    log_push_to_death(ch, attacker);
    death_cry(ch);
    extract_char(ch);
    return 0;
  }
  return 1;
}

int skill_cost(int h, int m, int v, CHAR_DATA *ch)
{
  int mv;
  if (GET_LEVEL(ch) > LVL_IMMORT)
    return 1;

  mv = (v/(4 - (GET_LEVEL(ch)/15)));
  if (((GET_HIT(ch)-h)<0)||((GET_MANA(ch)-m)<0)||((GET_MOVE(ch)-mv)<0))
    return 0;

  if (h)
    damage(ch, ch, h, TYPE_UNDEFINED);
  if (m)
    alter_mana(ch, m);
  if (v)
    alter_move(ch, mv);

  return 1;

}


