/* ************************************************************************
*   File: magic.c                                       Part of CircleMUD *
*  Usage: low-level functions for magic; spell template code              *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */


#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "spells.h"
#include "handler.h"
#include "db.h"
#include "interpreter.h"
#include "dg_scripts.h"
#include "dg_event.h"
#include "constants.h"
#include "fight.h"


int spell_size_dice(Character *ch);
int spell_num_dice(Character *ch);
#define S_SDICE (spell_size_dice(ch))
#define S_NDICE (spell_num_dice(ch))
extern int mini_mud;

void skill_attack(Character *ch, Character *vict, int skill, int pass);
void start_fighting_delay(Character *vict, Character *ch);
byte saving_throws(int class_num, int type, int level);     /* class.c */
void clearMemory(Character *ch);
void weight_change_object(struct obj_data *obj, int weight);
void add_follower(Character *ch, Character *leader);
int arena_ok(Character *ch, Character *victim);
void change_alignment(Character *ch, Character *victim);
void zap_char(Character *victim);
int find_first_step(room_rnum src, room_rnum target);


/* local functions */
int mag_materials(Character *ch, int item0, int item1, int item2,
                  int extract, int verbose);
void perform_mag_groups(int level, Character *ch,
                        Character *tch, int spellnum, int savetype);
int mag_savingthrow(Character *ch, int type, int modifier);
int do_magic_direction(int level, int dir, int dist, Character *ch, Character *vict, int spellnum);
int perform_mag_direction(int level, room_rnum room, Character *ch, Character *vict, int spellnum);
void affect_update(void);
bool can_have_follower(Character *ch, mob_vnum mob_num);
bool can_have_follower(Character *ch, Character *vict);

/*
 * Saving throws are now in class.c as of bpl13.
 */


/*
 * Negative apply_saving_throw[] values make saving throws better!
 * Then, so do negative modifiers.  Though people may be used to
 * the reverse of that. It's due to the code modifying the target
 * saving throw instead of the random number of the character as
 * in some other systems.
 */
int mag_savingthrow(Character *ch, int type, int modifier) {
    /* NPCs use warrior tables according to some book */
    int class_sav = CLASS_WARRIOR;
    int save;
    type = IRANGE(0, type, 4);
    if (!ch)
        return TRUE;
    if (!IS_NPC(ch))
        class_sav = GET_CLASS(ch);

    save = saving_throws(class_sav, type, GET_LEVEL(ch));
    save += GET_SAVE(ch, type);
    save += modifier;

    /* Throwing a 0 is always a failure. */
    if (MAX(1, save) < number(0, 99))
        return (TRUE);

    /* Oops, failed. Sorry. */
    return (FALSE);
}


/* affect_update: called from comm.c (causes spells to wear off) */
void affect_update(void) {
    struct affected_type *af, *next;
    Character *i;
    time_t t = time(0);

    for (i = character_list; i; i = i->next)
        for (af = i->affected; af; af = next) {
            next = af->next;
            if (af->expire > t) /* hasnt reached "time" yet.*/
                continue;
            if (af->expire == -2)   /* No action */
                continue;
            else {
                if ((af->type > 0) && (af->type <= MAX_SPELLS))
                    if (!af->next || (af->next->type != af->type) ||
                            (af->next->expire > t))
                        if (*spell_wear_off_msg[af->type])
                            new_send_to_char(i, "%s\r\n",spell_wear_off_msg[af->type]);
                i->affect_remove(af);
            }
        }
}


/*
 *  mag_materials:
 *  Checks for up to 3 vnums (spell reagents) in the player's inventory.
 *
 * No spells implemented in Circle 3.0 use mag_materials, but you can use
 * it to implement your own spells which require ingredients (i.e., some
 * heal spell which requires a rare herb or some such.)
 */
int mag_materials(Character *ch, int item0, int item1, int item2,
                  int extract, int verbose) {
    struct obj_data *tobj;
    struct obj_data *obj0 = NULL, *obj1 = NULL, *obj2 = NULL;

    for (tobj = ch->carrying; tobj; tobj = tobj->next_content) {
        if ((item0 > 0) && (GET_OBJ_VNUM(tobj) == item0)) {
            obj0 = tobj;
            item0 = -1;
        } else if ((item1 > 0) && (GET_OBJ_VNUM(tobj) == item1)) {
            obj1 = tobj;
            item1 = -1;
        } else if ((item2 > 0) && (GET_OBJ_VNUM(tobj) == item2)) {
            obj2 = tobj;
            item2 = -1;
        }
    }
    if ((item0 > 0) || (item1 > 0) || (item2 > 0)) {
        if (verbose) {
            switch (number(0, 2)) {
            case 0:
                send_to_char("A wart sprouts on your nose.\r\n", ch);
                break;
            case 1:
                send_to_char("Your hair falls out in clumps.\r\n", ch);
                break;
            case 2:
                send_to_char("A huge corn develops on your big toe.\r\n",
                             ch);
                break;
            }
        }
        return (FALSE);
    }
    if (extract) {
        if (item0 < 0) {
            obj_from_char(obj0);
            extract_obj(obj0);
        }
        if (item1 < 0) {
            obj_from_char(obj1);
            extract_obj(obj1);
        }
        if (item2 < 0) {
            obj_from_char(obj2);
            extract_obj(obj2);
        }
    }
    if (verbose) {
        send_to_char("A puff of smoke rises from your pack.\r\n", ch);
        act("A puff of smoke rises from $n's pack.", TRUE, ch, NULL, NULL,
            TO_ROOM);
    }
    return (TRUE);
}

/*
caster, spell number, direction or -1 for direction unknown, victim or null
 
will return
 -1 if failed (no victim found).
  0 if target or max distance is ch room.
 >0 the distance of the target if target, or the distance of the
    furthest reachable room. 
*/
int magic_distance(Character *ch, int spellnum, int dir,
                   Character *victim) {
    int maxdis = TIERNUM+1;
    int i;
    room_rnum nextroom = IN_ROOM(ch);
    room_rnum room = nextroom, vroom = NULL;

    if (victim)
        vroom = IN_ROOM(victim);

    if ((!CAN_GO2(room, dir)) || (dir == NOWHERE)) {
        ch->Send( "You can not cast magic that direction.\r\n");
        return NOWHERE;      //cant send magic that way.
    }
    if (ROOM_FLAGGED(room, ROOM_PEACEFUL)
            || ((vroom != NULL) && ROOM_FLAGGED(vroom, ROOM_PEACEFUL))) {
        if (SINFO.violent)
            return NOWHERE;    //cant send magic there
    }

    if (room == vroom || (dir == NOWHERE))
        return NOWHERE;       //victim is in same room.


    for (i = 0; i < maxdis && (nextroom != NULL); i++) {

        if (vroom != NULL && vroom == nextroom)
            return i + 1;

        if (CAN_GO2(nextroom, dir))
            nextroom = EXIT2(nextroom, dir)->to_room;
        else
            nextroom = NULL;
    }

    if (vroom != NULL) {
        ch->Send( "Nothing by that name in range to the %s.\r\n", dirs[dir]);
        return NOTHING;
    }
    return i;
}


//Default value of ch is NULL, as defined in handler.h
Character *find_in_dir(room_rnum room, char *name, int dir, Character *ch) {
    Character *tch;
    int i, num = get_number(&name);
    room_rnum nextroom = NULL;

    if (room == NULL)
        return NULL;

    if (dir == NOWHERE)
        return NULL;

    if (!name || !*name)
        return NULL;

    nextroom = room;
    for (i = 0; i < 10 && (nextroom != NULL); i++) {

        if ((tch = get_room_vis(nextroom, name, &num,ch)) != NULL)
            return tch;

        if (CAN_GO2(nextroom, dir))
            nextroom = EXIT2(nextroom, dir)->to_room;
        else
            nextroom = NULL;
    }

    return NULL;
}




/*
 * Every spell that does damage comes through here.  This calculates the
 * amount of damage, adds in any modifiers, determines what the saves are,
 * tests for save and calls damage().
 *
 * -1 = dead, otherwise the amount of damage done.
 */
int mag_damage(int level, Character *ch, Character *victim,
               int spellnum, int savetype) {

    int spell_lvl = 0;
    int good = 1;
    int evil = 1;
    int pass = TRUE;
    long pvict = -1;


    if (ch) {
        good = IS_GOOD(ch);
        evil = IS_EVIL(ch);
    }



    if (level <= 0)
        spell_lvl = GET_LEVEL(ch);
    else
        spell_lvl = level;

    if (victim == NULL || ch == NULL)
        return (0);

    switch (spellnum) {
    case  SPELL_SOULSMASH:
    case  SPELL_DEMONSHREAK:
    case  SPELL_LIFESUCK:
    case  SPELL_BURNINGSKULL:
    case  SPELL_HEARTSQUEEZE:
    case  SPELL_FACEMELT:
        if (!evil) {
            act("You aren't evil enough to cast this spell!", FALSE, ch, 0, victim, TO_CHAR);
            return (0);
        }
        break;

    case SPELL_DISPEL_EVIL:
        if (evil) {
            act("Your body convulses in torment!", FALSE, ch, 0, victim, TO_CHAR);
            return 0;
        } else if (IS_GOOD(victim)) {
            act("The gods protect $N.", FALSE, ch, 0, victim, TO_CHAR);
            return (0);
        }
        break;
    case SPELL_DISPEL_GOOD:
        if (good) {
            act("Your body convulses in torment!", FALSE, ch, 0, victim, TO_CHAR);
            return 0;
        } else if (IS_EVIL(victim)) {
            act("The gods protect $N.", FALSE, ch, 0, victim, TO_CHAR);
            return (0);
        }
        break;
    case SPELL_HARM:
        if ((MOB_FLAGGED(victim, MOB_UNDEAD)|| (IS_NPC(ch) && GET_CLASS(ch) == CLASS_UNDEAD))) {
            int dam = -dice(spell_lvl / 4, 2 * (spell_lvl / 3));

            send_to_char("They seem to be healing!\r\n", ch);
            alter_hit(victim, dam);
            return 0;
        }
        break;

    }                 /* switch(spellnum) */

    if (AFF_FLAGGED(victim, AFF_SLEEP))
        affect_from_char(victim, SPELL_SLEEP);

    if (AFF_FLAGGED(victim, AFF_SWEET_DREAMS))
        affect_from_char(victim, SPELL_SWEET_DREAMS);

    if (!victim->canHuntChar(ch) && GET_SPELL_DIR(ch) != NOWHERE)
        pass = FALSE;
    if (FIGHTING(ch) != NULL) {
        /** if the player is fighting already, savethe person they are fighting with **/
        if (FIGHTING(ch) != victim)
            pvict = GET_ID(FIGHTING(ch));
        /** Temporarily make this other person the FIGHTEE **/
        FIGHTING(ch) = victim;
    }
    /** attack them **/
    skill_attack(ch, victim, spellnum, pass);
    /** then switch back if nessercery **/
    if (pvict != -1)
        FIGHTING(ch) = find_char(pvict);

    if (DEAD(victim) || GET_POS(victim) == POS_DEAD)
        return -1;
    else
        return 1;

}

/*
 * Every spell that does an affect comes through here.  This determines
 * the effect, whether it is added or replacement, whether it is legal or
 * not, etc.
 *
 * affect_join(vict, aff, add_dur, avg_dur, add_mod, avg_mod)
*/


void mag_affects(int level, Character *ch, Character *victim,
                 int spellnum, int savetype) {
    struct affected_type af[MAX_SPELL_AFFECTS];
    struct affected_type *aff;
    bool accum_affect = FALSE, accum_duration =FALSE, is_innate = FALSE;

    const char *to_vict = NULL, *to_room = NULL;
    int i;//, dir = -1;
    //char format[256];
    int chcha = 11;
    int chlevel = level;
    int tier = 4;
    int suc_val = 0;
    float staff = (has_staff(ch) ? has_staff(ch) : 1);
    int m_user = (ch ? GET_CLASS(ch) == CLASS_MAGE || GET_CLASS(ch) == CLASS_PRIEST || GET_CLASS(ch) == CLASS_ESPER : 0);


    if (victim == NULL)
        return;

    if (ch) {
        chcha = GET_CHA(ch);
        chlevel = GET_LEVEL(ch);
        tier = TIERNUM;
    }

    for (i = 0; i < MAX_SPELL_AFFECTS; i++) {
        af[i].type = spellnum;
        af[i].bitvector = 0;
        af[i].modifier = 0;
        af[i].location = APPLY_NONE;
    }

    switch (spellnum) {
    case  SPELL_INFERNO:
    case SPELL_BURNING_HANDS:
        if (number(1, 50) <= level && !AFF_FLAGGED(victim, AFF_PROT_FIRE)
                && !mag_savingthrow(victim, savetype, 0)) {
            af[0].expire = HOURS_TO_EXPIRE(3);
            af[0].bitvector = AFF_BURNING;
            accum_duration =TRUE;
            to_vict = "You start burning.";
            to_room = "$n starts burning.";
        }
        break;

    case SPELL_CHILL_TOUCH:
        af[0].location = APPLY_STR;
        if (mag_savingthrow(victim, savetype, 0))
            af[0].expire = HOURS_TO_EXPIRE(1);
        else
            af[0].expire = HOURS_TO_EXPIRE(4);

        af[0].modifier = FTOI(-4 * staff);
        af[1].bitvector = AFF_PROCRASTINATE;
        af[1].location = APPLY_SPEED;
        af[1].modifier = FTOI(-75 * staff);
        af[1].expire = af[0].expire;


        accum_duration = TRUE;
        to_vict = "You feel your strength wither!";
        break;

    case SPELL_ABSOLVE:
        if (!OBJ_INNATE)
            GET_ALIGNMENT(victim) = IRANGE(-1000, GET_ALIGNMENT(ch)+(200), 1000);

        to_vict = "All of your sins have been forgiven.";
        to_room = "A halo appears above $n's head for a moment.";
        break;

    case SPELL_ARMOR:
        af[0].location = APPLY_AC;
        af[0].modifier = -5;
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(18*staff);
        accum_duration = FALSE;
        to_vict = "You feel someone protecting you.";
        break;
    case SPELL_ENERGY_DRAIN:
        af[0].location = APPLY_HITROLL;
        af[0].modifier = -5;
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24*staff);


        af[1].location = APPLY_REGEN_MOVE;
        af[1].modifier = -35;
        if (OBJ_INNATE)
            af[1].expire = -2;
        else
            af[1].expire = HOURS_TO_EXPIRE(24);


        accum_duration =FALSE;
        to_vict = "You feel drained.";

        break;
    case SPELL_BLESS:
        af[0].location = APPLY_HITROLL;
        af[0].modifier = 5;
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24*staff);


        af[1].location = APPLY_REGEN_MOVE;
        af[1].modifier = 35;
        if (OBJ_INNATE)
            af[1].expire = -2;
        else
            af[1].expire = HOURS_TO_EXPIRE(24);


        accum_duration =FALSE;
        to_vict = "You feel righteous.";
        break;

    case SPELL_BLINDNESS:
    case SPELL_COLOUR_SPRAY:
        if (MOB_FLAGGED(victim, MOB_NOBLIND)
                || mag_savingthrow(victim, savetype, 0)) {
            if (ch)
                ch->Send("You fail.\r\n");
            return;
        }

        af[0].location = APPLY_HITROLL;
        af[0].modifier = -10;
        af[0].expire = HOURS_TO_EXPIRE(2);
        af[0].bitvector = AFF_BLIND;

        af[1].location = APPLY_AC;
        af[1].modifier = 40;
        af[1].expire = HOURS_TO_EXPIRE(2);
        af[1].bitvector = AFF_BLIND;

        af[2].location = APPLY_REGEN_HIT;
        af[2].modifier = -40;
        af[2].expire = HOURS_TO_EXPIRE(3);
        af[2].bitvector = AFF_BLIND;

        to_room = "$n seems to be blinded!";
        to_vict = "You have been blinded!";
        break;

    case SPELL_CURSE:
        if (ch && mag_savingthrow(victim, savetype, 0)) {
            ch->Send( "%s", CONFIG_NOEFFECT);
            return;
        }

        af[0].location = APPLY_HITROLL;
        af[0].expire = HOURS_TO_EXPIRE(1 + (chlevel / 2));
        af[0].modifier = -chcha;
        af[0].bitvector = AFF_CURSE;

        af[1].location = APPLY_DAMROLL;
        af[1].expire = HOURS_TO_EXPIRE(1 + (chlevel / 2));
        af[1].modifier = -chcha;
        af[1].bitvector = AFF_CURSE;

        af[2].location = APPLY_REGEN_HIT;
        af[2].modifier = -150;
        af[2].expire = HOURS_TO_EXPIRE(3);
        af[2].bitvector = AFF_CURSE;

        accum_duration =TRUE;
        accum_affect = TRUE;
        to_room = "$n briefly glows red!";
        to_vict = "You feel very uncomfortable.";
        break;

    case SPELL_DETECT_ALIGN:
        if (OBJ_INNATE)
            af[0].expire =-2;
        else
            af[0].expire = HOURS_TO_EXPIRE(12 + level);
        af[0].bitvector = AFF_DETECT_ALIGN;
        accum_duration =FALSE;
        to_vict =
            "Your eyes tingle and people move into a different focus.";
        break;

    case SPELL_DETECT_INVIS:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(12 + level);
        af[0].bitvector = AFF_DETECT_INVIS;
        accum_duration =FALSE;
        to_vict = "You start to see things that never use to be there.";
        break;

    case SPELL_DETECT_MAGIC:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(12 + level);
        af[0].bitvector = AFF_DETECT_MAGIC;
        accum_duration =FALSE;
        to_vict = "Things start to take on a hazy purple hue.";
        break;

    case SPELL_EVIL_EYE:
        GET_ALIGNMENT(victim) = -(1000);
        to_vict = "Evil thoughts begin to creep into your mind!";
        to_room = "$n grins evilly for no apparent reason.";
        // zap_char(victim);
        break;

    case SPELL_HOLD_PERSON:
        if (ch && mag_savingthrow(victim, savetype, 0)) {
            ch->Send( "%s", CONFIG_NOEFFECT);
            return;
        }
        af[0].expire = HOURS_TO_EXPIRE(6);
        af[0].bitvector = AFF_HOLD;

        to_vict = "A magical force holds you firm.";
        to_room = "A magical force hold $n firm.";
        break;

    case SPELL_PARALYZE:
        if (ch && mag_savingthrow(victim, savetype, 0)) {
            ch->Send( "%s", CONFIG_NOEFFECT);
            return;
        }
        af[0].location = APPLY_HITROLL;
        af[0].modifier = -40;
        af[0].expire = HOURS_TO_EXPIRE(4);
        af[0].bitvector = AFF_HOLD;
        to_vict = "You are paralyzed.";
        to_room = "$n is paralyzed!";
        break;

    case SPELL_INFRAVISION:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(12 + level);
        af[0].bitvector = AFF_INFRAVISION;
        accum_duration =FALSE;
        to_vict = "Your eyes glow red.";
        to_room = "$n's eyes glow red.";
        break;

    case SPELL_INVISIBLE:
        if (!victim)
            victim = ch;

        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(12 + (chlevel / 4));
        af[0].modifier = -40;
        af[0].location = APPLY_AC;
        af[0].bitvector = AFF_INVISIBLE;
        accum_duration =FALSE;
        to_vict = "You vanish.";
        to_room = "$n slowly fades out of existence.";
        break;

    case SPELL_POISON:
        if (!IS_NPC(victim) && mag_savingthrow(victim, savetype, 0)) {
            if (ch)
                ch->Send( "%s", CONFIG_NOEFFECT);
            return;
        }

        af[0].location = APPLY_STR;
        af[0].expire = HOURS_TO_EXPIRE(chlevel);
        af[0].modifier = -2;
        af[0].bitvector = AFF_POISON_1;
        to_vict = "You feel very sick.";
        to_room = "$n gets violently ill!";
        break;

    case SPELL_POISON_2:
        if (ch && mag_savingthrow(victim, savetype, 0)) {
            ch->Send( "%s", CONFIG_NOEFFECT);
            return;
        }

        af[0].location = APPLY_STR;
        af[0].expire = -2;
        af[0].modifier = -4;
        af[0].bitvector = AFF_POISON_2;
        to_vict = "You feel very sick.";
        to_room = "$n gets violently ill!";
        break;

    case SPELL_POISON_3:
        if (ch && mag_savingthrow(victim, savetype, 0)) {
            ch->Send( "%s", CONFIG_NOEFFECT);
            return;
        }

        af[0].location = APPLY_STR;
        af[0].expire = -2;
        af[0].modifier = -6;
        af[0].bitvector = AFF_POISON_3;
        to_vict = "You feel very sick.";
        to_room = "$n gets violently ill!";
        break;

    case SPELL_POISON_4:
        if (ch && mag_savingthrow(victim, savetype, 0)) {
            ch->Send( "%s", CONFIG_NOEFFECT);
            return;
        }

        af[0].location = APPLY_STR;
        af[0].expire = -2;
        af[0].modifier = -8;
        af[0].bitvector = AFF_POISON_4;
        to_vict = "You feel very sick.";
        to_room = "$n gets violently ill!";
        break;

    case SPELL_PROT_FROM_EVIL:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24 + chcha);
        af[0].bitvector = AFF_PROTECT_EVIL;
        accum_duration =FALSE;
        to_vict = "A glowing aura of pure good surrounds you!";
        to_room = "A glowing aura of pure good surrounds $n!";
        break;

    case SPELL_SANCTUARY:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE((chcha / 3)+3);
        af[0].bitvector = AFF_SANCTUARY;

        af[1].location  = APPLY_REGEN_HIT;
        af[1].modifier  = 40;
        af[1].expire    = af[0].expire;
        af[1].bitvector = AFF_SANCTUARY;

        accum_duration =FALSE;
        to_vict = "A white aura momentarily surrounds you.";
        to_room = "$n is surrounded by a white aura.";
        break;

    case SPELL_SLEEP:
        if (!can_fight(ch, victim, FALSE))
            return;
        if (MOB_FLAGGED(victim, MOB_NOSLEEP))
            return;
        if (mag_savingthrow(victim, savetype, 0))
            return;

        af[0].expire = HOURS_TO_EXPIRE(4 + (chlevel / 4));
        af[0].bitvector = AFF_SLEEP;

        if (GET_POS(victim) > POS_SLEEPING) {
            victim->Send( "You feel very sleepy...  Zzzz......");
            act("$n goes to sleep.", TRUE, victim, 0, 0, TO_ROOM);
            GET_POS(victim) = POS_SLEEPING;
        }
        break;

    case SPELL_STEELSKIN:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].location = APPLY_AC;
        af[0].modifier = -chcha;

        if (OBJ_INNATE)
            af[1].expire = -2;
        else
            af[1].expire = HOURS_TO_EXPIRE(24 + chcha);
        af[1].location = APPLY_SAVING_SPELL;
        af[1].modifier = -8;

        accum_duration =FALSE;
        to_vict = "You feel your skin as tough as steel.";
        break;

    case SPELL_STONESKIN:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24 + chcha);
        af[0].location = APPLY_AC;
        af[0].modifier = -5;
        af[0].bitvector = AFF_STONESKIN;
        accum_duration =FALSE;
        to_vict = "You feel your skin turn into granite.";
        break;

    case SPELL_STRENGTH:
        if (GET_ADD(victim) == 100)
            return;

        af[0].type = SPELL_STRENGTH;
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE((chlevel / 2) + 4);

        af[0].modifier = ((1 + (level > 18) + (level > 40)));
        ;
        af[0].location = APPLY_STR;
        af[0].bitvector = AFF_HOLY_STRENGTH;



        accum_duration =FALSE;
        accum_affect = TRUE;
        to_vict = "You feel stronger!";
        break;

    case SPELL_SENSE_LIFE:
        to_vict = "Your feel your awareness improve.";
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(chlevel + chcha);
        af[0].bitvector = AFF_SENSE_LIFE;
        accum_duration =FALSE;
        break;

    case SPELL_WATERWALK:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24 + chcha);
        af[0].bitvector = AFF_WATERWALK;
        accum_duration =FALSE;
        to_vict = "You feel webbing between your toes.";
        break;

    case SPELL_HASTE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(((10 + (chcha > 20) + (chcha > 30))*staff));
        af[0].bitvector = AFF_HASTE;
        af[0].modifier = 90 + chcha;
        af[0].location = APPLY_SPEED;

        accum_duration =FALSE;
        to_vict = "You feel yourself moving with unnatural speed.";
        break;

    case SPELL_SHIELD:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(chcha);
        af[0].bitvector = AFF_SHIELD;
        if (speed_update(victim) > 400 && m_user) {
            af[1].bitvector = AFF_BLUR;
            af[1].modifier = 1;
            af[1].expire = af[0].expire;
            af[1].location = APPLY_DEX;
        }
        accum_duration =FALSE;
        to_vict = "A pulsing blue shield appears around you.";
        to_room = "A pulsing blue shield suddenly surrounds $n";
        break;

    case SPELL_PROT_FIRE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(10 + level);
        af[0].bitvector = AFF_PROT_FIRE;
        accum_duration =FALSE;
        to_vict = "You feel a shell of insulation form around your body.";
        break;

    case SPELL_PROT_COLD:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE( 10 + level);
        af[0].bitvector = AFF_PROT_COLD;
        accum_duration =FALSE;
        to_vict = "You feel a shell of warmth form around your body.";
        break;

    case SPELL_CONE_OF_COLD:
        if (number(1, 30) <= level && !AFF_FLAGGED(victim, AFF_PROT_FIRE)
                && !MOB_FLAGGED(victim, MOB_NOFREEZE)
                && !mag_savingthrow(victim, savetype, 0)) {
            af[0].expire = HOURS_TO_EXPIRE( 1);
            af[0].bitvector = AFF_FREEZING;
            accum_duration =TRUE;
            to_vict = "You are consumed with coldness.";
            to_room = "$n starts shivering.";
        }
        break;

    case SPELL_ACID_ARROW:
        if (number(1, 30) <= level
                && !mag_savingthrow(victim, savetype, 0)) {
            af[0].expire = HOURS_TO_EXPIRE(3);
            af[0].bitvector = AFF_ACIDED;
            accum_duration =TRUE;
            to_vict = "The acid arrow drenches you.";
            to_room = "$n is drenched by an acid covered arrow.";
        }
        break;

    case SPELL_FLAME_ARROW:
        if (number(1, 30) <= level && !AFF_FLAGGED(victim, AFF_PROT_FIRE)
                && !mag_savingthrow(victim, savetype, 0)) {
            af[0].expire = HOURS_TO_EXPIRE(3);
            af[0].bitvector = AFF_BURNING;
            accum_duration =TRUE;
            to_vict = "The flame arrow sets you on fire.";
            to_room = "$n is hit by a flame arrow and starts burning.";
        }
        break;

    case SPELL_FIRE_SHIELD:
        if (OBJ_INNATE)
            af[0].expire  = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(tier * 11);
        af[0].bitvector = AFF_FIRE_SHIELD;
        to_vict = "You are surrounded by a flaming red aura.";
        to_room = "$n is suddenly surrounded by flaming red aura.";
        break;

    case SPELL_FIRE_BREATH:
        if (number(1, 50) <= level && !AFF_FLAGGED(victim, AFF_PROT_FIRE)
                && !mag_savingthrow(victim, savetype, 0)) {
            af[0].expire = HOURS_TO_EXPIRE( 1);
            af[0].bitvector = AFF_BURNING;
            accum_duration =TRUE;
            to_vict = "You are engulfed in flames.";
            to_room = "$n is engulfed in flames.";
        }
        break;

    case SPELL_BURN:
        af[0].expire = HOURS_TO_EXPIRE(1);
        af[0].bitvector = AFF_BURNING;
        accum_duration =TRUE;
        to_vict = "You are engulfed in flames.";
        to_room = "$n is engulfed in flames.";
        break;

    case SPELL_FREEZE:
        af[0].expire = HOURS_TO_EXPIRE( 1);
        af[0].bitvector = AFF_FREEZING;
        accum_duration =TRUE;
        to_vict = "You are consumed with coldness.";
        to_room = "$n starts shivering.";
        break;

    case SPELL_ACID:
        af[0].expire = HOURS_TO_EXPIRE(1);
        af[0].bitvector = AFF_ACIDED;
        accum_duration =TRUE;
        to_vict = "You are drenched in acid.";
        to_room = "$n is dripping with acid.";
        break;

    case SPELL_MIND_FIRE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(tier * 11);
        af[0].bitvector = AFF_MIND_FIRE;
        accum_duration =TRUE;
        to_vict = "Your mind becomes alive with thoughts of fire!";
        break;
    case SPELL_MIND_ELEC:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(tier * 11);
        af[0].bitvector = AFF_MIND_ELEC;
        accum_duration =TRUE;
        to_vict = "Your mind becomes alive with thoughts of electricity!";
        break;
    case SPELL_MIND_WATER:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(tier * 11);
        af[0].bitvector = AFF_MIND_WATER;
        accum_duration =TRUE;
        to_vict = "Your mind becomes alive with thoughts of water!";
        break;
    case SPELL_MIND_ICE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(tier * 11);
        af[0].bitvector = AFF_MIND_ICE;
        accum_duration =TRUE;
        to_vict = "Your mind becomes alive with thoughts of ice!";
        break;

    case SPELL_PROT_FROM_GOOD:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_PROTECT_GOOD;

        accum_duration =FALSE;
        to_vict = "A dark aura flickers into existence around you.";
        to_room = "A dark aura flickers into existence around $n.";
        break;
    case SPELL_SHIELD_ICE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_SHIELD_ICE;

        accum_duration =FALSE;
        to_vict = "A cold shield of ice forms around you.";
        to_room = "A cold shield of ice forms around $n.";
        break;
    case SPELL_SHIELD_THORN:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_SHIELD_THORNS;

        accum_duration =FALSE;
        to_vict = "A spinning haze of sharp thorns circles you.";
        to_room = "A spinning haze of sharp thorns circles around $n.";
        break;
    case SPELL_SHIELD_MANA:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_SHIELD_MANA;

        accum_duration =FALSE;
        to_vict = "A humming shield of magical energy covers you.";
        to_room = "A humming shield of magical energy covers $n.";
        break;


    case SPELL_SHIELD_MIRROR:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_SHIELD_MIRROR;

        accum_duration =FALSE;
        to_vict = "A metallic, glassy orb expands around you.";
        to_room = "A metallic, glassy orb expands around $n.";
        break;
    case SPELL_SHIELD_HOLY:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_SHIELD_HOLY;




        accum_duration =FALSE;
        to_vict = "You are touched by a holy light.";
        to_room = "A shaft of light beams down on $n.";
        break;

    case SPELL_SHIELD_STATIC:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_SHIELD_STATIC;

        accum_duration =FALSE;
        to_vict = "You tingle as a wave of static energy washes over you.";
        break;
    case SPELL_FORTIFY_MIND:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_FORTIFY_MIND;
        af[0].location = APPLY_INT;
        af[0].modifier = 3;

        accum_duration =FALSE;
        to_vict = "You feel more sure of your thoughts.";
        break;

    case SPELL_FORTIFY_BODY:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_FORTIFY_BODY;
        af[0].location = APPLY_HIT;
        af[0].modifier = FTOI(30*staff);

        accum_duration =FALSE;
        to_vict = "You feel more sure of your actions.";
        break;

    case SPELL_SWEET_DREAMS:
        if (ch && number(1, 25) > GET_INT(ch)) {
            ch->Send( "You slip and draw the wrong rune!\r\n");
            victim = ch;
        }
        af[0].expire = HOURS_TO_EXPIRE(2);
        af[0].bitvector = AFF_SWEET_DREAMS;

        accum_duration =FALSE;
        to_vict = "Happy thoughts fill your mind as you drift off to sleep.";
        to_room = "$n smiles and collapses.";
        break;
    case SPELL_DEVINE_MIND:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_DEVINE_MIND;
        af[0].location = APPLY_CHA;
        af[0].modifier = FTOI(5*staff);

        accum_duration =FALSE;
        to_vict = "Focus and determination fills your mind.";
        break;
    case SPELL_NUMB_MIND:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_NUMB_MIND;

        accum_duration =FALSE;
        to_vict = "Your thoughts slow.";
        to_room = "$n staggers.";
        break;
    case SPELL_SLOW:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].location = APPLY_SPEED;
        af[0].modifier = FTOI(-(100*staff));
        af[0].bitvector = AFF_SLOW;

        accum_duration =FALSE;
        to_vict = "Your body moves as if in jelly.";
        to_room = "$n moves as if in jelly.";
        break;
    case SPELL_FLIGHT:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_FLY;

        accum_duration =FALSE;
        to_vict = "With a rustle enormous wings sprout from your back and unfurl.";
        to_room = "With a rustle enormous wings sprout from $n's back and unfurl.";
        break;
    case SPELL_BATTLE_RAGE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_BATTLE_RAGE;
        af[0].location = APPLY_AC;
        af[0].modifier = FTOI(10*staff);

        accum_duration =FALSE;
        to_vict = "You bare your teeth as the fire of anger runs through your veins.";
        break;
    case SPELL_MAGIC_BUBBLE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_MAGIC_BUBBLE;

        accum_duration =FALSE;
        to_vict = "A bubble forms around you.";
        to_room = "A bubble forms around $n.";
        break;
    case SPELL_FORSEE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_FORSEE;

        accum_duration =FALSE;
        to_vict = "You start anticipating everyones movements.";
        break;
    case SPELL_CONFUSE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(24);
        af[0].bitvector = AFF_CONFUSED;

        accum_duration =FALSE;
        to_room = "$n frowns. Not sure what just happened.";
        to_vict = "You frown. Not sure what just happened.";
        break;

    case SPELL_SUFFOCATE:
        if (OBJ_INNATE)
            af[0].expire = -2;
        else
            af[0].expire = HOURS_TO_EXPIRE(3);
        af[0].bitvector = AFF_SUFFOCATING;

        accum_duration =FALSE;
        to_vict = "You gag and start clawing at the air around your neck.";
        to_room = "$n gags and starts clawing at the air around $s neck.";
        break;
    case SPELL_CORRUPT_ARMOR:
        af[0].expire = HOURS_TO_EXPIRE(24 + chcha);
        af[0].location = APPLY_AC;
        af[0].modifier = 25;
        af[0].bitvector = AFF_CORRUPTED;
        accum_duration =FALSE;
        to_vict = "Your armor starts rotting.";
        to_room = "$n's armor starts rotting.";
        break;
    case SPELL_WEAKEN:
        af[0].expire = HOURS_TO_EXPIRE(24 + chcha);
        af[0].location = APPLY_STR;
        af[0].modifier = -6;
        af[0].bitvector = AFF_WEAKENED;

        accum_duration =FALSE;
        to_vict = "Your muscles atrophy.";
        to_room = "$n sags under the weight.";
        break;



    }

    if (spellnum == SPELL_MIND_FIRE ||spellnum == SPELL_MIND_WATER ||spellnum == SPELL_MIND_ICE ||spellnum == SPELL_MIND_ELEC ) {
        if (affected_by_spell(victim, SPELL_MIND_FIRE)) {
            affect_from_char(victim, SPELL_MIND_FIRE);
            if (*spell_wear_off_msg[SPELL_MIND_FIRE])
                victim->Send( "%s\r\n",spell_wear_off_msg[SPELL_MIND_FIRE]);
        }
        if (affected_by_spell(victim, SPELL_MIND_ICE)) {
            affect_from_char(victim, SPELL_MIND_ICE);
            if (*spell_wear_off_msg[SPELL_MIND_ICE])
                victim->Send( "%s\r\n",spell_wear_off_msg[SPELL_MIND_ICE]);
        }
        if (affected_by_spell(victim, SPELL_MIND_WATER)) {
            affect_from_char(victim, SPELL_MIND_WATER);
            if (*spell_wear_off_msg[SPELL_MIND_WATER])
                victim->Send( "%s\r\n",spell_wear_off_msg[SPELL_MIND_WATER]);
        }
        if (affected_by_spell(victim, SPELL_MIND_ELEC)) {
            affect_from_char(victim, SPELL_MIND_ELEC);
            if (*spell_wear_off_msg[SPELL_MIND_ELEC])
                victim->Send( "%s\r\n",spell_wear_off_msg[SPELL_MIND_ELEC]);
        }
    }

    for (aff = victim->affected; aff && !is_innate; aff = aff->next) {
        if (spellnum == aff->type && aff->expire == -2)
            is_innate = TRUE;
    }

    /*
     * If this is a mob that has this affect set in its mob file, do not
     * perform the affect.  This prevents people from un-sancting mobs
     * by sancting them and waiting for it to fade, for example.
     */
    if (IS_NPC(victim) && !affected_by_spell(victim, spellnum))
        for (i = 0; i < MAX_SPELL_AFFECTS; i++)
            if (AFF_FLAGGED(victim, af[i].bitvector) && ch) {
                ch->Send( "%s", CONFIG_NOEFFECT);
                return;
            }

    /*
     * If the victim is already affected by this spell, and the spell does
     * not have an accumulative effect, then fail the spell.
     * if not innate, and already has the spell, then remove the spell and refresh it. - mord
     */
    if (affected_by_spell(victim, spellnum)
            && !(accum_duration || accum_affect) && ch) {
        if (is_innate && ch) {
            ch->Send( "%s", CONFIG_NOEFFECT);
            return;
        } else
            affect_from_char(victim, spellnum);
    }

    /*
     *  If the victim has that spell affect set innate, then make sure that the
     *  spell is treated like the above, which fails the spell.
     */


    if (affected_by_spell(victim, spellnum) && is_innate && ch) {
        ch->Send( "%s", CONFIG_NOEFFECT);
        return;
    }

    if (spellnum == SPELL_SWEET_DREAMS)
        GET_POS(victim) = POS_SLEEPING;

    for (i = 0; i < MAX_SPELL_AFFECTS; i++)
        if (af[i].bitvector || (af[i].location != APPLY_NONE)) {
            affect_join(victim, af + i, accum_duration, FALSE, accum_affect, FALSE);
            suc_val = 1;
        }
    if (suc_val) {
        if (to_vict != NULL && OBJ_INNATE_MESSAGE)
            act(to_vict, FALSE, victim, 0, ch, TO_CHAR);
        if (to_room != NULL && OBJ_INNATE_MESSAGE)
            act(to_room, TRUE, victim, 0, ch, TO_ROOM);

        zap_char(victim);
    }
}

int do_magic_direction(int level, int dir, int dist, Character *ch, Character *vict, int spellnum) {
    int i;
    room_rnum nextroom = IN_ROOM(ch);
    int ret = 0;

    for (i = 0; i < dist && (nextroom != NULL); i++) {

        ret += perform_mag_direction( level, nextroom , ch , vict , spellnum);

        if (CAN_GO2(nextroom, dir))
            nextroom = EXIT2(nextroom, dir)->to_room;
        else
            nextroom = NULL;
    }
    return ret;
}

int perform_mag_direction(int level, room_rnum room, Character *ch, Character *vict, int spellnum) {
    Character *tch, *tch_next;
    char format[MAX_INPUT_LENGTH];
    char format2[MAX_INPUT_LENGTH];
    const char *dirname;
    int ret = 0;

    if (room == NULL|| GET_SPELL_DIR(ch) == NOWHERE)
        return 0;

    switch (rev_dir[GET_SPELL_DIR(ch)]) {
    case NORTH:
        dirname = "the north";
        break;
    case SOUTH:
        dirname = "the south";
        break;
    case EAST:
        dirname = "the east";
        break;
    case WEST:
        dirname = "the west";
        break;
    case UP:
        dirname = "above";
        break;
    case DOWN:
        dirname = "below";
        break;
    default:
        dirname = "somewhere";
        break;
    }

    #define CANA(string) (strchr("aeiouyAEIOUY", string[0]) ? "An" : "A")

    if (room == IN_ROOM(ch)) {
        sprintf(format, "%s %s emerges from $N and strikes you!", CANA(skill_name(spellnum)),skill_name(spellnum));

        sprintf(format2, "%s %s emerges from $N and strikes $n!" ,CANA(skill_name(spellnum)),skill_name(spellnum));
    } else {

        sprintf(format, "%s %s arrives from %s and strikes you!", CANA(skill_name(spellnum)),skill_name(spellnum),dirname);

        sprintf(format2, "%s %s arrives from %s and strikes $n!" ,CANA(skill_name(spellnum)),skill_name(spellnum),dirname);
    }



    for (tch = room->people; tch; tch = tch_next) {
        tch_next = tch->next_in_room;
        if (vict && tch != vict)
            continue;
        if (SELF(tch,ch))
            continue;
        if (!IS_NPC(tch) && GET_LEVEL(tch) >= LVL_GOD)
            continue;
        if (tch->master == ch)
            continue;
        if ((!IS_NPC(tch) && !IS_NPC(ch))  && (!ROOM_FLAGGED(IN_ROOM(tch), ROOM_ARENA) || !both_pk(ch,tch)))
            continue;
        act(format, FALSE, tch, 0, ch, TO_CHAR);
        act(format2, TRUE, tch, 0, ch, TO_ROOM);
        switch (spellnum) {
        case SPELL_INFERNO:
        case SPELL_MANA_BLAST:
        case SPELL_ELECTRIC_BLAST:
        case SPELL_ACID_ARROW:
        case SPELL_FLAME_ARROW:
        case SPELL_MAGIC_MISSILE:
        case SPELL_CONE_OF_COLD:
        case SPELL_FIREBALL:

            if (mag_damage(level, ch, tch, spellnum, 1))
                ret += 1;
            break;
        case SPELL_MANA_TRANSFER:
        case SPELL_LIFE_TRANSFER:
        case SPELL_VITALIZE:
            mag_points(level, ch, tch, spellnum, 1);
            ret += 1;
            break;
        }
    }
    return ret;
}


/*
 * This function is used to provide services to mag_groups.  This function
 * is the one you should change to add new group spells.
 */

void perform_mag_groups(int level, Character *ch,
                        Character *tch, int spellnum, int savetype) {
    switch (spellnum) {
    case SPELL_GROUP_HEAL:
        mag_points(level, ch, tch, SPELL_HEAL, savetype);
        break;
    case SPELL_GROUP_ARMOR:
        mag_affects(level, ch, tch, SPELL_ARMOR, savetype);
        break;
    case SPELL_GROUP_SHIELD:
        mag_affects(level, ch, tch, SPELL_SHIELD, savetype);
        break;
    case SPELL_GROUP_RECALL:
        spell_recall(level, ch, tch, NULL, 0);
        break;
    }
}


/*
 * Every spell that affects the group should run through here
 * perform_mag_groups contains the switch statement to send us to the right
 * magic.
 *
 * group spells affect everyone grouped with the caster who is in the room,
 * caster last.
 *
 * To add new group spells, you shouldn't have to change anything in
 * mag_groups -- just add a new case to perform_mag_groups.
 */

void mag_groups(int level, Character *ch, int spellnum,
                int savetype) {
    Character *tch, *k;
    struct follow_type *f, *f_next;

    if (ch == NULL)
        return;

    if (!ch->master && !ch->followers)
    return;
    
        k = ch->master ? ch->master : ch;
    
    for (f = k->followers; f; f = f_next) {
        f_next = f->next;
        tch = f->follower;
        if (!HERE(ch, tch))
            continue;
        if (GET_PERC(tch) == 0)
            continue;
        if (SELF(ch,tch))
            continue;
        perform_mag_groups(level, ch, tch, spellnum, savetype);
    }

    if ((k != ch) && AFF_FLAGGED(k, AFF_GROUP))
        perform_mag_groups(level, ch, k, spellnum, savetype);
    perform_mag_groups(level, ch, ch, spellnum, savetype);
}


/*
 * mass spells affect every creature in the room except the caster.
 *
 * No spells of this class currently implemented as of Circle 3.0.
 */

void mag_masses(int level, Character *ch, int spellnum,
                int savetype) {
    Character *tch, *tch_next;

    for (tch = ch->in_room->people; tch; tch = tch_next) {
        tch_next = tch->next_in_room;
        if (tch == ch)
            continue;

    switch (spellnum) {}
    }
}


/*
 * Every spell that affects an area (room) runs through here.  These are
 * generally offensive spells.  This calls mag_damage to do the actual
 * damage -- all spells listed here must also have a case in mag_damage()
 * in order for them to work.
 *
 *  area spells have limited targets within the room.
*/

void mag_areas(int level, Character *ch, int spellnum, int savetype) {
    Character *tch, *next_tch;
    const char *to_char = NULL, *to_room = NULL;
    int count = 0, rounds;


    if (ch == NULL)
        return;

    rounds = level/3;
    /*
     * to add spells to this fn, just add the message here plus an entry
     * in mag_damage for the damaging part of the spell.
     */
    switch (spellnum) {
    case SPELL_EARTHQUAKE:
        to_char =
            "You gesture and the earth begins to shake all around you!";
        to_room =
            "$n gracefully gestures and the earth begins to shake violently!";
        break;

    case  SPELL_INFERNO:
        to_char =
            "You gesture and the world blazes into an inferno!";
        to_room =
            "$n gestures and the world blazes into an inferno!";
        break;
    case  SPELL_MANA_BLAST:
        to_char =
            "You concentrate and summon all your magical energy!";
        to_room =
            "$n concentrates and summons all $s magical energy!";
        break;
    case SPELL_HOLY_SHOUT:
        to_char = "The world shook as you issue holy shout!";
        to_room = "$n opens the holy bible and said the holy words!";
        break;

    case SPELL_METEOR_SHOWER:
        to_char = "You call the iron meteors with your magic!";
        to_room = "$n glows and a rain of iron meteors comes suddenly!";
        break;

    case SPELL_FIRE_BREATH:
        to_char = "You snort and fire shoots out of your nostrils!";
        to_room = "$n snorts and a gout of fire shoots out of $s nostrils at you!";
        break;

    case SPELL_GAS_BREATH:
        to_char = "You burp and a noxious gas rolls rapidly out of your nostrils!";
        to_room = "$n rumbles and a noxious gas rolls out of $s nostrils!";
        break;

    case SPELL_FROST_BREATH:
        to_char = "You shiver as a shaft of frost leaps from your mouth!";
        to_room = "$n shivers as a shaft of frost leaps from $s mouth!";
        break;

    case SPELL_ACID_BREATH:
        to_char = "Your indigestion acts up and a wash of acid leaps from your mouth!";
        to_room = "$n looks pained as a wash of acid leaps from $s mouth!";
        break;

    case SPELL_LIGHTNING_BREATH:
        to_char = "You open your mouth and bolts of lightning shoot out!";
        to_room = "$n opens $s mouth and bolts of lightning shoot out!";
        break;

    case SPELL_CONE_OF_COLD:
        to_char = "You aim your cone of cold.";
        to_room = "$n launches a deadly cone of super-cooled air!";
        break;
    }

    if (to_char != NULL)
        act(to_char, FALSE, ch, 0, 0, TO_CHAR);
    if (to_room != NULL)
        act(to_room, FALSE, ch, 0, 0, TO_ROOM);


    for (tch = ch->in_room->people; tch; tch = next_tch) {
        next_tch = tch->next_in_room;

        /*
         * The skips: 1: the caster
         *            2: immortals
         *            3: if no pk on this mud, skips over all players
         *            4: pets (charmed NPCs)
         */
        if (count++ >= rounds)
            break;
        if (SELF(tch,ch))
            continue;
        if (!IS_NPC(tch) && GET_LEVEL(tch) >= LVL_GOD)
            continue;
        if (!can_fight(ch, tch, FALSE))
            continue;
        if (!IS_NPC(ch) && IS_NPC(tch) && AFF_FLAGGED(tch, AFF_CHARM))
            continue;

        /* Doesn't matter if they die here so we don't check. -gg 6/24/98 */
        if (number(0,170) < (level + GET_SKILL(ch, spellnum))) {
            GET_WAIT_STATE(ch) += 1 RL_SEC;
            mag_damage(level, ch, tch, spellnum, 1);
        } else {
            act("$N is missed by your spell and attacks!", FALSE, ch, 0, tch, TO_CHAR);
            act("Angered by $n's missed spell you attack!", FALSE, ch, 0, tch, TO_VICT);
            start_fighting_delay(tch, ch);
        }
    }
}


/*
 *  Every spell which summons/gates/conjours a mob comes through here.
 *
 *  None of these spells are currently implemented in Circle 3.0; these
 *  were taken as examples from the JediMUD code.  Summons can be used
 *  for spells like clone, ariel servant, etc.
 *
 * 10/15/97 (gg) - Implemented Animate Dead and Clone.
 */

/*
 * These use act(), don't put the \r\n.
 */
const char *mag_summon_msgs[] = {
                                    "\r\n",
                                    "$n makes a strange magical gesture; you feel a strong breeze!",  //1
                                    "$n animates a corpse!",  // 2
                                    "$N appears from a cloud of thick blue smoke!",    //3
                                    "$N appears from a cloud of thick green smoke!",   //4
                                    "$N appears from a cloud of thick red smoke!",     //5
                                    "$N disappears in a thick black cloud!", //6
                                    "As $n makes a strange magical gesture, you feel a searing heat.",     //7
                                    "As $n makes a strange magical gesture, you feel a strong breeze.",    //8
                                    "As $n makes a strange magical gesture, you feel a sudden chill.",     //9
                                    "As $n makes a strange magical gesture, you feel the ground tremble.", //10
                                    "$n magically divides!",  //11
                                    "$n animates a corpse!"   //12
                                };

/*
 * Keep the \r\n because these use send_to_char.
 */
const char *mag_summon_fail_msgs[] = {
                                         "\r\n",
                                         "There are no such creatures.\r\n",
                                         "Uh oh...\r\n",
                                         "Oh dear.\r\n",
                                         "Oh shit!\r\n",
                                         "The elements resist!\r\n",
                                         "You failed.\r\n",
                                         "There is no corpse!\r\n"
                                     };

/* These mobiles do not exist. */
#define MOB_MONSUM_I          130
#define MOB_MONSUM_II         140
#define MOB_MONSUM_III        150
#define MOB_GATE_I       160
#define MOB_GATE_II      170
#define MOB_GATE_III          180

/* Defined mobiles. */
#define MOB_ELEMENTAL_BASE    20   /* Only one for now. */
#define MOB_CLONE        10
#define MOB_ZOMBIE       11
#define MOB_AERIALSERVANT     19
#define MOB_EARTH_ELEM        22
#define MOB_WATER_ELEM        23
#define MOB_AIR_ELEM          24
#define MOB_FIRE_ELEM         25


void mag_summons(int level, Character *ch, struct obj_data *obj,
                 int spellnum, int savetype) {
    Character *mob = NULL;
    struct obj_data *tobj, *next_obj;
    int pfail = 0, msg = 0, fmsg = 0, num = 1, handle_corpse = FALSE, i;
    mob_vnum mob_num;

    if (ch == NULL)
        return;

    switch (spellnum) {
    case SPELL_CLONE:
        msg = 10;
        fmsg = number(2, 6); /* Random fail message. */
        mob_num = MOB_CLONE;
        pfail = 50 - GET_CHA(ch);
        break;

    case SPELL_ANIMATE_DEAD:
        if (obj == NULL || !IS_CORPSE(obj) || IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_NPC_CORPSE)) {
            act(mag_summon_fail_msgs[7], FALSE, ch, 0, 0, TO_CHAR);
            return;
        }
        handle_corpse = TRUE;
        msg = 12;
        fmsg = number(2, 6); /* Random fail message. */
        mob_num = MOB_ZOMBIE;
        pfail = 30 - GET_CHA(ch);
        break;

    case SPELL_EARTH_ELEMENTAL:
        handle_corpse = FALSE;
        msg = 10;
        fmsg = number(2, 6); /* Random fail message. */
        mob_num = MOB_EARTH_ELEM;
        pfail = 45 - GET_CHA(ch);
        num += GET_CHA(ch)/60;
        break;

    case SPELL_WATER_ELEMENTAL:
        handle_corpse = FALSE;
        msg = 9;
        fmsg = number(2, 6); /* Random fail message. */
        mob_num = MOB_WATER_ELEM;
        pfail = 25;
        num += GET_CHA(ch)/60;
        break;

    case SPELL_AIR_ELEMENTAL:
        handle_corpse = FALSE;
        msg = 8;
        fmsg = number(2, 6); /* Random fail message. */
        mob_num = MOB_AIR_ELEM;
        pfail = 25;
        num += GET_CHA(ch)/60;
        break;

    case SPELL_FIRE_ELEMENTAL:
        handle_corpse = FALSE;
        msg = 7;
        fmsg = number(2, 6); /* Random fail message. */
        mob_num = MOB_FIRE_ELEM;
        pfail = 25;
        num += GET_CHA(ch)/60;
        break;

    default:
        return;
    }

   

    if (AFF_FLAGGED(ch, AFF_CHARM)) {
        ch->Send("You are too giddy to have any followers!\r\n");
        return;
    }
    if (!can_have_follower(ch, mob_num)) {
  ch->Send("Sorry, but you can't have any more then a total of \r\n"
           "160 levels between all of your charmed followers\r\n");
  return;
    }
    if (number(0, 101) < pfail) {
        ch->Send("%s", mag_summon_fail_msgs[fmsg]);
        return;
    }
    for (i = 0; i < num; i++) {
        if (!(mob = read_mobile(mob_num))) {
            ch->Send("You don't quite remember how to make that creature.\r\n");
            return;
        }

        char_to_room(mob, ch->in_room);
        
        IS_CARRYING_W(mob) = 0;
        IS_CARRYING_N(mob) = 0;
        SET_BIT_AR(AFF_FLAGS(mob), AFF_CHARM);
        SET_BIT_AR(AFF_FLAGS(mob), AFF_GROUP);
        if (spellnum == SPELL_CLONE) {     /* Don't mess up the proto with strcpy. */
            mob->player.name = strdup(GET_NAME(ch));
            mob->player.short_descr = strdup(GET_NAME(ch));
        }
        act(mag_summon_msgs[msg], FALSE, ch, 0, mob, TO_ROOM);
        load_mtrigger(mob);
        add_follower(mob, ch);
        GET_GOLD(mob) = 0;
        GET_EXP(mob) = 0;
        switch (spellnum) {
        case SPELL_EARTH_ELEMENTAL:
            SET_BIT_AR(MOB_FLAGS(mob), MOB_ELEM_EARTH);
            break;

        case SPELL_WATER_ELEMENTAL:
            SET_BIT_AR(MOB_FLAGS(mob), MOB_ELEM_WATER);
            break;

        case SPELL_AIR_ELEMENTAL:
            SET_BIT_AR(MOB_FLAGS(mob), MOB_ELEM_AIR);
            break;

        case SPELL_FIRE_ELEMENTAL:
            SET_BIT_AR(MOB_FLAGS(mob), MOB_ELEM_FIRE);
            break;
        }
    }
    if (handle_corpse) {
        for (tobj = obj->contains; tobj; tobj = next_obj) {
            next_obj = tobj->next_content;
            obj_from_obj(tobj);
            obj_to_char(tobj, mob);
        }
        extract_obj(obj);
    }
}


void mag_points(int level, Character *ch, Character *victim,
                int spellnum, int savetype) {
    int hit = 0, move = 0, mana = 0, stam = 0, spell_lvl = 0;
    int good = 1, evil = 1;
    int multi = 1;

    if (ch && level == GET_LEVEL(ch)) {
        /*now thats a nasty hack*/
        good = IS_GOOD(ch);
        evil = IS_EVIL(ch);
        if (ch != victim)
            multi = 2;
    }

    if (level <= 0)
        spell_lvl = GET_LEVEL(ch);
    else
        spell_lvl = level;

    if (victim == NULL)
        return;

    switch (spellnum) {
    case SPELL_CURE_LIGHT:
        hit = dice(5, spell_lvl / 2) + (spell_lvl / 3);
        send_to_char("You feel better.\r\n", victim);
        break;
    case SPELL_CURE_CRITIC:
        hit = 50 + dice(4, spell_lvl / 2) + (spell_lvl / 2);
        send_to_char("You feel a lot better!\r\n", victim);
        break;
    case SPELL_HEAL:
        hit = 100 + dice(spell_lvl / 2, spell_lvl) + number(spell_lvl, spell_lvl*2);
        send_to_char("A warm feeling floods your body.\r\n", victim);
        break;
    case SPELL_LIFE_TRANSFER:
        hit = MIN(dice(1, 6) + GET_LEVEL(ch), GET_HIT(ch) - 1);
        alter_hit(ch, hit);
        hit = MAX(0, hit - dice(1, 3));
        move = hit;
        send_to_char
        ("You feel woozy as your life essence is drained away.\r\n",
         ch);
        send_to_char("Your life essence returns.\r\n", victim);
        break;
    case SPELL_MANA_TRANSFER:
        mana = MIN(dice(1, 6) + GET_LEVEL(ch), GET_MANA(ch) - 1);
        alter_mana(ch, mana);
        mana = MAX(0, mana - dice(1, 3));
        send_to_char("Your head aches as your magic flows away.\r\n", ch);
        send_to_char("You feel a strong flow of magical energy.\r\n", victim);
        break;
    case SPELL_VITALIZE:
        move = dice(FTOI(spell_lvl * 0.5), FTOI(spell_lvl * 0.5));
        stam = spell_lvl;
        act("Your mind scans $N's body, revitalizing them.", FALSE, ch, 0, victim, TO_CHAR);
        act("$n's mind scans your body, revitalizing you.", FALSE, ch, 0, victim, TO_VICT);
        break;
    }

    alter_hit(victim, -hit * multi);
    alter_move(victim, -move * multi);
    alter_mana(victim, -mana * multi);
    alter_stamina(victim, -stam * multi);
    update_pos(victim);
}


void mag_unaffects(int level, Character *ch,
                   Character *victim, int spellnum, int type) {
    int spell = 0, poisoned = 0;
    const char *to_vict = NULL, *to_room = NULL;

    if (victim == NULL)
        return;

    switch (spellnum) {
    case SPELL_CURE_BLIND:
    case SPELL_HEAL:
        spell = SPELL_BLINDNESS;
        to_vict = "Your vision returns!";
        to_room = "There's a momentary gleam in $n's eyes.";
        affect_from_char(victim, SPELL_COLOUR_SPRAY);
        break;
    case SPELL_ANTIDOTE_1:
        spell = SPELL_POISON;
        to_vict = "A warm feeling runs through your body!";
        to_room = "$n looks better.";
        poisoned = 1;
        break;
    case SPELL_REMOVE_CURSE:
        spell = SPELL_CURSE;
        to_vict = "You don't feel so unlucky.";
        break;
    case SPELL_ANTIDOTE_2:
        spell = SPELL_POISON_2;
        to_vict =
            "A warm feeling runs through your body as the poison disipates!";
        to_room = "$n looks better.";
        poisoned = 1;
        break;
    case SPELL_ANTIDOTE_3:
        spell = SPELL_POISON_3;
        to_vict =
            "A warm feeling runs through your body as the poison disipates!";
        to_room = "$n looks better.";
        poisoned = 1;
        break;
    case SPELL_DISPELL_SANCTURY:
        spell = SPELL_SANCTUARY;
        to_vict = "Your sanctury flickers and fades.";
        to_room = "$n's white aura flickers and fades.";
        break;
    default:
        log("SYSERR: unknown spellnum %d passed to mag_unaffects.",
            spellnum);
        log("SYSERR: mag_unaffects: name: %s, vict: %s, type: %d.",
            GET_NAME(ch), GET_NAME(victim), type);
        break;
    }

    if (!spell)
        return;


    if (!affected_by_spell(victim, spell)) {
        if (spellnum != SPELL_HEAL)    /* 'cure blindness' message. */
            ch->Send( "%s", CONFIG_NOEFFECT);
        return;
    }

    affect_from_char(victim, spell);
    if (to_vict != NULL)
        act(to_vict, FALSE, victim, 0, ch, TO_CHAR);
    if (to_room != NULL)
        act(to_room, TRUE, victim, 0, ch, TO_ROOM);

    if (poisoned)
        victim->check_regen_rates();     /* speed up regen rate immediately */

}


void mag_alter_objs(int level, Character *ch, struct obj_data *obj,
                    int spellnum, int savetype) {
    const char *to_char = NULL, *to_room = NULL;

    if (obj == NULL)
        return;

    switch (spellnum) {
    case SPELL_BLESS:
        if (!IS_OBJ_STAT(obj, ITEM_BLESS) &&
                (GET_OBJ_WEIGHT(obj) <= 5 * GET_LEVEL(ch))) {
            SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_BLESS);
            to_char = "$p glows briefly.";
        }
        break;
    case SPELL_CURSE:
        if (!IS_OBJ_STAT(obj, ITEM_NODROP)) {
            SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_NODROP);
            if (GET_OBJ_TYPE(obj) == ITEM_WEAPON)
                GET_OBJ_VAL(obj, 2)--;
            to_char = "$p briefly glows red.";
        }
        break;
    case SPELL_INVISIBLE:
        if (!IS_OBJ_STAT(obj, ITEM_NOINVIS | ITEM_INVISIBLE)) {
            SET_BIT_AR(obj->obj_flags.extra_flags, ITEM_INVISIBLE);
            to_char = "$p vanishes.";
        }
        break;
    case SPELL_POISON:
        if (((GET_OBJ_TYPE(obj) == ITEM_DRINKCON) ||
                (GET_OBJ_TYPE(obj) == ITEM_FOUNTAIN) ||
                (GET_OBJ_TYPE(obj) == ITEM_FOOD)) && !GET_OBJ_VAL(obj, 3)) {
            GET_OBJ_VAL(obj, 3) = 1;
            to_char = "$p steams briefly.";
        } else {
            to_char = "$p cannot be poisoned, only food or drink can be.";
        }
        break;
    case SPELL_REMOVE_CURSE:
        if (IS_OBJ_STAT(obj, ITEM_NODROP)) {
            REMOVE_BIT_AR(obj->obj_flags.extra_flags, ITEM_NODROP);
            if (GET_OBJ_TYPE(obj) == ITEM_WEAPON)
                GET_OBJ_VAL(obj, 2)++;
            to_char = "$p briefly glows blue.";
        }
        break;
    case SPELL_ANTIDOTE_1:
        if (((GET_OBJ_TYPE(obj) == ITEM_DRINKCON) ||
                (GET_OBJ_TYPE(obj) == ITEM_FOUNTAIN) ||
                (GET_OBJ_TYPE(obj) == ITEM_FOOD)) && GET_OBJ_VAL(obj, 3)) {
            GET_OBJ_VAL(obj, 3) = 0;
            to_char = "$p steams briefly.";
        }
        break;
    }

    if (to_char == NULL)
        ch->Send( "%s", CONFIG_NOEFFECT);
    else
        act(to_char, TRUE, ch, obj, 0, TO_CHAR);

    if (to_room != NULL)
        act(to_room, TRUE, ch, obj, 0, TO_ROOM);
    else if (to_char != NULL)
        act(to_char, TRUE, ch, obj, 0, TO_ROOM);

}



void mag_creations(int level, Character *ch, int spellnum) {
    struct obj_data *tobj;
    obj_vnum z;

    if (ch == NULL)
        return;
    /* level = MAX(MIN(level, LVL_IMPL), 1); - Hm, not used. */

    switch (spellnum) {
    case SPELL_CREATE_FOOD:
        z = 10;
        break;
    default:
        send_to_char("Spell unimplemented, it would seem.\r\n", ch);
        return;
    }

    if (!(tobj = read_object(z, VIRTUAL))) {
        send_to_char("I seem to have goofed.\r\n", ch);
        log("SYSERR: spell_creations, spell %d, obj %d: obj not found",
            spellnum, z);
        return;
    }
    obj_to_char(tobj, ch);
    act("$n creates $p.", FALSE, ch, tobj, 0, TO_ROOM);
    act("You create $p.", FALSE, ch, tobj, 0, TO_CHAR);
    load_otrigger(tobj);
}


