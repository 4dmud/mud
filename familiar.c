//http://uo.stratics.com/content/reputation/pets.shtml
#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "house.h"
#include "screen.h"
#include "constants.h"
#include "oasis.h"
#include "dg_scripts.h"
#include "spells.h"
#include "fight.h"

int can_take_obj(Character *ch, struct obj_data *obj);
void make_manifest(Character *ch,struct obj_data *obj);
int perform_get_from_room(Character *ch, struct obj_data *obj);
void perform_wear(Character *ch, struct obj_data *obj, int where);
Character *load_familiar(Character *owner);
void save_familiar(Character *owner);
Character *find_owner(Character *familiar);
long familiar_exp(Character *ch, long exp);
void set_familiar_type(Character *familiar, int type);
int show_familiar_type(Character *familiar);
float split_percent(Character *ch);
int call_magic(Character *caster, Character *cvict,
               struct obj_data *ovict, char *tar_str, int spellnum,
               int level, int casttype);

void find_usable_weapon(Character *ch);
void dismount_char(Character *ch);
ACMD(do_wear);
ASKILL(skill_snare);
ASKILL(skill_backstab);
ASKILL(skill_grapple);
ASKILL(skill_encircle);
ASKILL(skill_disarm);

/* NPC -- AI stuff -- mord*/
void parse_mob_commands(Character *ch); // this gets called for all mobs.
void parse_class_actions(Character *ch);
void parse_elemental_actions(Character *ch);

void parse_rogue_commands(Character *ch);
void parse_caster_commands(Character *ch);
void parse_fighter_commands(Character *ch);
void parse_animal_commands(Character *ch);
void parse_undead_commands(Character *ch);
Character * parse_aggressive(Character *ch);
void parse_tasks(Character *ch);

#define CAST_THE_SPELL(ch, vict, i) call_magic((ch), (vict), NULL, NULL, (i), GET_LEVEL((ch)), CAST_BREATH)

void parse_mob_commands(Character *ch) {
    int health = ((GET_HIT(ch)*100)/GET_MAX_HIT(ch));


    if (!IS_ELEMENTAL(ch)) {
        if (IS_AFFECTED(ch, AFF_CHARM) && ch->master) {
            if ((health < 40)) {
                if (number(1, 40) > health) {
                    act("$n realises that you don't care for $m any more and decides to leave!", FALSE, ch, 0, ch->master, TO_VICT);
                    act("$n realises that $N doesn't care for $m any more and decides to leave!", FALSE, ch, 0, ch->master, TO_NOTVICT);
                    extract_char(ch);
                    return;
                }
            }
            /*
            if (GET_MOB_WAIT(ch) == 0 && FIGHTING(ch) && !FIGHTING(ch->master)) {
            GET_MOB_WAIT(ch) += 3 RL_SEC;
            GET_POS(ch) = POS_SITTING;     
            }*/
        }
    } else
        parse_elemental_actions(ch);

    parse_class_actions(ch);

    parse_tasks(ch);
}




void parse_class_actions(Character *ch) {

    switch (GET_CLASS(ch)) {
    case CLASS_ROGUE:
        parse_rogue_commands(ch);
        break;
    case CLASS_CASTER:
        parse_caster_commands(ch);
        break;
    case CLASS_FIGHTER:
        parse_fighter_commands(ch);
        break;
    case CLASS_ANIMAL:
        parse_animal_commands(ch);
        break;
    case CLASS_UNDEAD:
        parse_undead_commands(ch);
        break;


    }

}



void parse_caster_commands(Character *ch) {
    void make_manifest(Character *ch,struct obj_data *obj);
    Character *master = ((!ch->master || (!HERE(ch->master, ch))) ? ch : (number(0, 1) ? ch->master : ch));
    Character *vict;
    int spl = TYPE_UNDEFINED;
    find_usable_weapon(ch);
    if (!GET_EQ(ch, WEAR_FOCUS) && GET_EQ(ch, WEAR_WIELD)) {
        if (GET_OBJ_TYPE(GET_EQ(ch, WEAR_WIELD)) == ITEM_WEAPON) {
            make_manifest(ch, GET_EQ(ch, WEAR_WIELD));
            do_wear(ch, (char *)"orb", 0, 0);
        }
    }
    vict = parse_aggressive(ch);
    if ((FIGHTING(ch) && HERE(FIGHTING(ch), ch)) || vict) {
        if (GET_POS(ch) < POS_FIGHTING)
            return;
        if (!vict) {
            if (FIGHTING(master))
                master = FIGHTING(master);
            else
                master = FIGHTING(ch);
        } else
            master = vict;

        if (number(0, 300) <= GET_LEVEL(ch)) {
            switch (number(0, GET_LEVEL(ch)/12)) {
            case 0:
                spl = (SPELL_MAGIC_MISSILE);
                break;
            case 1:
                spl = (SPELL_BURNING_HANDS);
                break;
            case 2:
                spl = (SPELL_ACID_ARROW);
                break;
            case 3:
                spl = (SPELL_FLAME_ARROW);
                break;
            case 4:
                spl = (SPELL_WEAKEN);
                break;
            case 5:
                spl = (SPELL_CORRUPT_ARMOR);
                break;
            case 6:
                spl = (SPELL_SOULSMASH);
                break;
            case 7:
                spl = (SPELL_DISPELL_SANCTURY);
                break;
            case 8:
                spl = (SPELL_SWEET_DREAMS);
                break;
            case 9:
                if (!ch->master)
                    spl = (SPELL_COLOUR_SPRAY);
                break;
            case 10:
                spl = (SPELL_FIREBALL);
                break;
            case 11:
                spl = (SPELL_MAGIC_BUBBLE);
                break;
            case 12:
                spl = (SPELL_TELEPORT);
                break;
            case 13:
                spl = (SPELL_LIGHTNING_BOLT);
                break;
            case 14:
                spl = (SPELL_ENERGY_DRAIN);
                break;
            case 15:
                spl = (SPELL_POISON);
                break;
            case 16:
                spl = (SPELL_FACEMELT);
                break;
            case 17:
                spl = (SPELL_SUFFOCATE);
                break;

            }
            CAST_THE_SPELL(ch, master, spl);
        }

    } else {
        if (number(0, 1000) <= GET_LEVEL(ch)) {
            if (GET_POS(ch) < POS_FIGHTING)
                return;
            switch (number(0, GET_LEVEL(ch)/12)) {
            case 0:
                spl = (SPELL_SANCTUARY);
                break;
            case 1:
                spl = (SPELL_FLIGHT);
                break;
            case 2:
                spl = (SPELL_ARMOR);
                break;
            case 3:
                spl = (SPELL_SHIELD);
                break;
            case 4:
                spl = (SPELL_BATTLE_RAGE);
                break;
            case 5:
                spl = (SPELL_SHIELD_THORN);
                break;
            case 6:
                spl = (SPELL_SHIELD_MANA);
                break;
            case 7:
                spl = (SPELL_SHIELD_HOLY);
                break;
            case 8:
                spl = (SPELL_FORTIFY_BODY);
                break;
            case 9:
                spl = (SPELL_INFRAVISION);
                break;
            case 10:
                spl = (SPELL_SHIELD_ICE);
                break;
            case 11:
                spl = (SPELL_SHIELD_MIRROR);
                break;
            case 12:
                spl = (SPELL_FORTIFY_MIND);
                break;
            case 13:
                spl = (SPELL_FIRE_SHIELD);
                break;
            case 14:
                spl = (SPELL_MIND_ICE);
                break;
            case 15:
                spl = (SPELL_MIND_FIRE);
                break;
            case 16:
                spl = (SPELL_MIND_WATER);
                break;
            case 17:
                spl = (SPELL_FORSEE);
                break;

            }

            if (!affected_by_spell(master, spl))
                CAST_THE_SPELL(ch, master, spl);
        }

    }
}
void parse_rogue_commands(Character *ch) {
    find_usable_weapon(ch);
    Character *vict,  *master = ((!ch->master || (!HERE(ch->master, ch))) ? ch : (number(0, 1) ? ch->master : ch));
    find_usable_weapon(ch);


    if (FIGHTING(ch) && HERE(FIGHTING(ch), ch)) {
        if (GET_POS(ch) < POS_FIGHTING)
            return;
        if (FIGHTING(master))
            master = FIGHTING(master);
        else
            master = FIGHTING(ch);

        if (number(0, 200) <= GET_LEVEL(ch)) {
            switch (number(0, GET_LEVEL(ch)/20)) {
            case 0:
                act("$n knocks your feet out from under you and you hit the ground hard!!", FALSE, ch, 0, master, TO_VICT);
                act("$n knocks $N's feet out from under $M and $E hits the ground hard!!", FALSE, ch, 0, master, TO_NOTVICT);
                GET_POS(master) = POS_SITTING;
                return;
            case 1:
                act("$n ducks, sidesteps and stabs you in the back!!", FALSE, ch, 0, master, TO_VICT);
                act("$n ducks, sidesteps and stabs $M in the back!!", FALSE, ch, 0, master, TO_NOTVICT);
                fe_solo_damage(ch, master, FTOI(dice(ch->mob_specials.damnodice,ch->mob_specials.damsizedice) * (GET_LEVEL(ch)*0.1)), TYPE_UNDEFINED);
                return;
            case 2:
            case 3:
            case 4:
                skill_disarm(ch, master, NULL, NULL);
                return;
            case 5:
                skill_grapple(ch, master, NULL, NULL);
                return;
            case 6:
                skill_snare(ch, master, NULL, NULL);
                return;

            }
        }
    }

    if ((vict = parse_aggressive(ch)) != NULL) {
        if (GET_POS(ch) < POS_FIGHTING)
            return;
        switch (number(0, GET_LEVEL(ch)/20)) {
        case 0:
            act("$n knocks your feet out from under you and you hit the ground hard!!", FALSE, ch, 0, master, TO_VICT);
            act("$n knocks $N's feet out from under $M and $E hits the ground hard!!", FALSE, ch, 0, master, TO_NOTVICT);
            GET_POS(vict) = POS_SITTING;
            set_fighting(ch, vict);
            return;
        case 1:
            if (GET_EQ(ch, WEAR_WIELD))
                skill_backstab(ch, vict, NULL, NULL);
            return;
        case 2:
        case 3:
        case 4:
            skill_disarm(ch, vict, NULL, NULL);
            return;
        case 5:
            skill_grapple(ch, vict, NULL, NULL);
            return;

        }
    }

}
void parse_fighter_commands(Character *ch) {
    parse_rogue_commands(ch);
    if (GET_POS(ch) < POS_FIGHTING)
        return;
    return;
    Character *master = ((!ch->master || (!HERE(ch->master, ch))) ? ch : (number(0, 1) ? ch->master : ch));
    int spl = TYPE_UNDEFINED;
    find_usable_weapon(ch);
    if (!GET_EQ(ch, WEAR_FOCUS) && GET_EQ(ch, WEAR_WIELD)) {
        if (GET_OBJ_TYPE(GET_EQ(ch, WEAR_WIELD)) == ITEM_WEAPON) {
            make_manifest(ch, GET_EQ(ch, WEAR_WIELD));
            do_wear(ch, ( char *)"orb", 0, 0);
        }
    }

    if (FIGHTING(ch) && HERE(FIGHTING(ch), ch)) {
        if (FIGHTING(master))
            master = FIGHTING(master);
        else
            master = FIGHTING(ch);

        if (number(0, 300) <= GET_LEVEL(ch)) {
            switch (number(0, GET_LEVEL(ch)/25)) {
            case 0:
                spl = (SPELL_MAGIC_MISSILE);
                break;
            case 1:
                spl = (SPELL_BURNING_HANDS);
                break;
            case 2:
                spl = (SPELL_ACID_ARROW);
                break;
            case 3:
                spl = (SPELL_FLAME_ARROW);
                break;
            case 4:
                spl = (SPELL_WEAKEN);
                break;
            case 5:
                spl = (SPELL_CORRUPT_ARMOR);
                break;
            case 6:
                spl = (SPELL_SOULSMASH);
                break;
            case 7:
                spl = (SPELL_DISPELL_SANCTURY);
                break;
            case 8:
                spl = (SPELL_SWEET_DREAMS);
                break;
            case 9:
                if (!ch->master)
                    spl = (SPELL_COLOUR_SPRAY);
                break;
            case 10:
                spl = (SPELL_FIREBALL);
                break;
            case 11:
                spl = (SPELL_HARM);
                break;
            case 12:
                spl = (SPELL_TELEPORT);
                break;
            case 13:
                spl = (SPELL_LIGHTNING_BOLT);
                break;
            case 14:
                spl = (SPELL_ENERGY_DRAIN);
                break;
            case 15:
                spl = (SPELL_POISON);
                break;
            case 16:
                spl = (SPELL_FACEMELT);
                break;
            case 17:
                spl = (SPELL_SUFFOCATE);
                break;

            }
            CAST_THE_SPELL(ch, master, spl);
        }

    } else {
        if (number(0, 1000) <= GET_LEVEL(ch)) {
            switch (number(0, GET_LEVEL(ch)/4)) {
            case 0:
                spl = (SPELL_SANCTUARY);
                break;
            case 1:
                spl = (SPELL_FLIGHT);
                break;
            case 2:
                spl = (SPELL_ARMOR);
                break;
            case 3:
                spl = (SPELL_SHIELD);
                break;
            case 4:
                spl = (SPELL_BATTLE_RAGE);
                break;
            case 5:
                spl = (SPELL_SHIELD_THORN);
                break;
            case 6:
                spl = (SPELL_SHIELD_MANA);
                break;
            case 7:
                spl = (SPELL_SHIELD_HOLY);
                break;
            case 8:
                spl = (SPELL_FORTIFY_BODY);
                break;
            case 9:
                spl = (SPELL_INFRAVISION);
                break;
            case 10:
                spl = (SPELL_SHIELD_ICE);
                break;
            case 11:
                spl = (SPELL_SHIELD_MIRROR);
                break;
            case 12:
                spl = (SPELL_FORTIFY_MIND);
                break;
            case 13:
                spl = (SPELL_FIRE_SHIELD);
                break;
            case 14:
                spl = (SPELL_MIND_ICE);
                break;
            case 15:
                spl = (SPELL_MIND_FIRE);
                break;
            case 16:
                spl = (SPELL_MIND_WATER);
                break;
            case 17:
                spl = (SPELL_FORSEE);
                break;

            }

            if (!affected_by_spell(master, spl))
                CAST_THE_SPELL(ch, master, spl);
        }

    }


}

void parse_animal_commands(Character *ch) {
    Character *master = ((!ch->master || (!HERE(ch->master, ch))) ? ch : (number(0, 1) ? ch->master : ch));
    int spl = TYPE_UNDEFINED;
    return;
    find_usable_weapon(ch);
    if (!GET_EQ(ch, WEAR_FOCUS) && GET_EQ(ch, WEAR_WIELD)) {
        if (GET_OBJ_TYPE(GET_EQ(ch, WEAR_WIELD)) == ITEM_WEAPON) {
            make_manifest(ch, GET_EQ(ch, WEAR_WIELD));
            do_wear(ch, (char *)"orb", 0, 0);
        }
    }

    if (FIGHTING(ch) && HERE(FIGHTING(ch), ch)) {
        if (FIGHTING(master))
            master = FIGHTING(master);
        else
            master = FIGHTING(ch);

        if (number(0, 300) <= GET_LEVEL(ch)) {
            switch (number(0, GET_LEVEL(ch)/4)) {
            case 0:
                spl = (SPELL_MAGIC_MISSILE);
                break;
            case 1:
                spl = (SPELL_BURNING_HANDS);
                break;
            case 2:
                spl = (SPELL_ACID_ARROW);
                break;
            case 3:
                spl = (SPELL_FLAME_ARROW);
                break;
            case 4:
                spl = (SPELL_WEAKEN);
                break;
            case 5:
                spl = (SPELL_CORRUPT_ARMOR);
                break;
            case 6:
                spl = (SPELL_SOULSMASH);
                break;
            case 7:
                spl = (SPELL_DISPELL_SANCTURY);
                break;
            case 8:
                spl = (SPELL_SWEET_DREAMS);
                break;
            case 9:
                if (!ch->master)
                    spl = (SPELL_COLOUR_SPRAY);
                break;
            case 10:
                spl = (SPELL_FIREBALL);
                break;
            case 11:
                spl = (SPELL_HARM);
                break;
            case 12:
                spl = (SPELL_TELEPORT);
                break;
            case 13:
                spl = (SPELL_LIGHTNING_BOLT);
                break;
            case 14:
                spl = (SPELL_ENERGY_DRAIN);
                break;
            case 15:
                spl = (SPELL_POISON);
                break;
            case 16:
                spl = (SPELL_FACEMELT);
                break;
            case 17:
                spl = (SPELL_SUFFOCATE);
                break;

            }
            CAST_THE_SPELL(ch, master, spl);
        }

    } else {
        if (number(0, 1000) <= GET_LEVEL(ch)) {
            switch (number(0, GET_LEVEL(ch)/4)) {
            case 0:
                spl = (SPELL_SANCTUARY);
                break;
            case 1:
                spl = (SPELL_FLIGHT);
                break;
            case 2:
                spl = (SPELL_ARMOR);
                break;
            case 3:
                spl = (SPELL_SHIELD);
                break;
            case 4:
                spl = (SPELL_BATTLE_RAGE);
                break;
            case 5:
                spl = (SPELL_SHIELD_THORN);
                break;
            case 6:
                spl = (SPELL_SHIELD_MANA);
                break;
            case 7:
                spl = (SPELL_SHIELD_HOLY);
                break;
            case 8:
                spl = (SPELL_FORTIFY_BODY);
                break;
            case 9:
                spl = (SPELL_INFRAVISION);
                break;
            case 10:
                spl = (SPELL_SHIELD_ICE);
                break;
            case 11:
                spl = (SPELL_SHIELD_MIRROR);
                break;
            case 12:
                spl = (SPELL_FORTIFY_MIND);
                break;
            case 13:
                spl = (SPELL_FIRE_SHIELD);
                break;
            case 14:
                spl = (SPELL_MIND_ICE);
                break;
            case 15:
                spl = (SPELL_MIND_FIRE);
                break;
            case 16:
                spl = (SPELL_MIND_WATER);
                break;
            case 17:
                spl = (SPELL_FORSEE);
                break;

            }

            if (!affected_by_spell(master, spl))
                CAST_THE_SPELL(ch, master, spl);
        }

    }

}

void parse_undead_commands(Character *ch) {
    Character *master = ((!ch->master || (!HERE(ch->master, ch))) ? ch : (number(0, 1) ? ch->master : ch));
    int spl = TYPE_UNDEFINED;
    return;
    find_usable_weapon(ch);
    if (!GET_EQ(ch, WEAR_FOCUS) && GET_EQ(ch, WEAR_WIELD)) {
        if (GET_OBJ_TYPE(GET_EQ(ch, WEAR_WIELD)) == ITEM_WEAPON) {
            make_manifest(ch, GET_EQ(ch, WEAR_WIELD));
            do_wear(ch, (char *)"orb", 0, 0);
        }
    }

    if (FIGHTING(ch) && HERE(FIGHTING(ch), ch)) {
        if (FIGHTING(master))
            master = FIGHTING(master);
        else
            master = FIGHTING(ch);

        if (number(0, 300) <= GET_LEVEL(ch)) {
            switch (number(0, GET_LEVEL(ch)/4)) {
            case 0:
                spl = (SPELL_MAGIC_MISSILE);
                break;
            case 1:
                spl = (SPELL_BURNING_HANDS);
                break;
            case 2:
                spl = (SPELL_ACID_ARROW);
                break;
            case 3:
                spl = (SPELL_FLAME_ARROW);
                break;
            case 4:
                spl = (SPELL_WEAKEN);
                break;
            case 5:
                spl = (SPELL_CORRUPT_ARMOR);
                break;
            case 6:
                spl = (SPELL_SOULSMASH);
                break;
            case 7:
                spl = (SPELL_DISPELL_SANCTURY);
                break;
            case 8:
                spl = (SPELL_SWEET_DREAMS);
                break;
            case 9:
                if (!ch->master)
                    spl = (SPELL_COLOUR_SPRAY);
                break;
            case 10:
                spl = (SPELL_FIREBALL);
                break;
            case 11:
                spl = (SPELL_HARM);
                break;
            case 12:
                spl = (SPELL_TELEPORT);
                break;
            case 13:
                spl = (SPELL_LIGHTNING_BOLT);
                break;
            case 14:
                spl = (SPELL_ENERGY_DRAIN);
                break;
            case 15:
                spl = (SPELL_POISON);
                break;
            case 16:
                spl = (SPELL_FACEMELT);
                break;
            case 17:
                spl = (SPELL_SUFFOCATE);
                break;

            }
            CAST_THE_SPELL(ch, master, spl);
        }

    } else {
        if (number(0, 1000) <= GET_LEVEL(ch)) {
            switch (number(0, GET_LEVEL(ch)/4)) {
            case 0:
                spl = (SPELL_SANCTUARY);
                break;
            case 1:
                spl = (SPELL_FLIGHT);
                break;
            case 2:
                spl = (SPELL_ARMOR);
                break;
            case 3:
                spl = (SPELL_SHIELD);
                break;
            case 4:
                spl = (SPELL_BATTLE_RAGE);
                break;
            case 5:
                spl = (SPELL_SHIELD_THORN);
                break;
            case 6:
                spl = (SPELL_SHIELD_MANA);
                break;
            case 7:
                spl = (SPELL_SHIELD_HOLY);
                break;
            case 8:
                spl = (SPELL_FORTIFY_BODY);
                break;
            case 9:
                spl = (SPELL_INFRAVISION);
                break;
            case 10:
                spl = (SPELL_SHIELD_ICE);
                break;
            case 11:
                spl = (SPELL_SHIELD_MIRROR);
                break;
            case 12:
                spl = (SPELL_FORTIFY_MIND);
                break;
            case 13:
                spl = (SPELL_FIRE_SHIELD);
                break;
            case 14:
                spl = (SPELL_MIND_ICE);
                break;
            case 15:
                spl = (SPELL_MIND_FIRE);
                break;
            case 16:
                spl = (SPELL_MIND_WATER);
                break;
            case 17:
                spl = (SPELL_FORSEE);
                break;

            }

            if (!affected_by_spell(master, spl))
                CAST_THE_SPELL(ch, master, spl);
        }

    }

}

void parse_elemental_actions(Character *ch) {

    if (MOB_FLAGGED(ch, MOB_ELEM_EARTH)) {}
    if (MOB_FLAGGED(ch, MOB_ELEM_FIRE)) {}
    if (MOB_FLAGGED(ch, MOB_ELEM_AIR)) {}
    if (MOB_FLAGGED(ch, MOB_ELEM_WATER)) {}
}

void parse_tasks(Character *ch) {}

#define LEVELS_PER_CHA 4
#define TCH f->follower
void check_group_control(Character *ch) {
    int cha = 0;
    struct follow_type *f, *f_next;
    int can_control_follower(Character *ch, Character *vict, int amount);

    if (ch) {
        if (ch->followers == NULL)
            return;

        cha = GET_CHA(ch);

        for (f = ch->followers; f != NULL ; f = f_next) {
            f_next = f->next;
            if (DEAD(TCH) || GET_POS(TCH)== POS_DEAD)
                continue;
            if (!AFF_FLAGGED(TCH, AFF_CHARM))
                continue;
            cha = can_control_follower(ch, TCH, cha);
            if (cha < 0) {
                act("$N blinks and struggles from your control!", FALSE, ch, 0, TCH, TO_CHAR);
                act("$N blinks and struggles from $n's control!", FALSE, ch, 0, TCH, TO_ROOM);
                if (!number(0, 5)) {}
            }

        }

    }

}

int can_control_follower(Character *ch, Character *vict, int amount) {
    int ret_val = 1;

    if (ch->desc)
        return amount;

    ret_val = (amount - (amount*LEVELS_PER_CHA));

    return ret_val;

}

void find_usable_weapon(Character *ch) {
    struct obj_data *obj = GET_EQ(ch, WEAR_WIELD), *onext;
    struct obj_data *shield = GET_EQ(ch, WEAR_SHIELD);
    int found = 0;
    if (obj != NULL || IN_ROOM(ch) == NULL)
        return;
    /*search inventory*/
    for (obj = ch->carrying; obj != NULL ; obj = obj->next_content) {
        if (GET_OBJ_TYPE(obj) == ITEM_WEAPON) {
            found = 1;
            break;
        } else if (!shield && GET_OBJ_TYPE(obj) == ITEM_ARMOR && CAN_WEAR(obj, ITEM_WEAR_SHIELD)) {
            shield = obj;
        }
    }

    /*search room*/
    if (found == 0) {
        for (obj = IN_ROOM(ch)->contents; obj != NULL; obj = onext) {
            onext = obj->next_content;
            if (GET_OBJ_TYPE(obj) == ITEM_WEAPON && can_take_obj(ch, obj)) {
                perform_get_from_room(ch, obj);
                found = 1;
                break;
            } else if (!shield && GET_OBJ_TYPE(obj) == ITEM_ARMOR && CAN_WEAR(obj, ITEM_WEAR_SHIELD)) {
                perform_get_from_room(ch, obj);
                shield = obj;
            }

        }
    }

    if (found == 1 && obj) {
        if (GET_OBJ_WEIGHT(obj) <= str_app[STRENGTH_APPLY_INDEX(ch)].wield_w)
            perform_wear(ch, obj, WEAR_WIELD);
    }
    if (GET_CLASS(ch) != CLASS_CASTER && shield != GET_EQ(ch, WEAR_SHIELD)) {
        perform_wear(ch, shield, WEAR_SHIELD);
    }


}



int sum_charmie_levels(Character *ch) {
    Character *k;
    struct follow_type *f;
    int cnt = 0;
    k = ch->master ? ch->master : ch;
    if (IS_NPC(k) && AFF_FLAGGED(k, AFF_CHARM))
        cnt += GET_LEVEL(k);

    for (f = k->followers;f;f = f->next) {
        if (IS_NPC(f->follower) && AFF_FLAGGED(f->follower, AFF_CHARM))
            cnt += GET_LEVEL(f->follower);
    }
    return cnt;

}
bool can_have_follower(Character *ch, mob_vnum mob_num) {
int tot = sum_charmie_levels(ch);
if (!MobProtoExists(mob_num))
return FALSE;
if (tot > 160)
return FALSE;
if ((tot + GET_LEVEL(GetMobProto(mob_num))) > 160)
return FALSE;

return TRUE;
}
bool can_have_follower(Character *ch, Character *vict) {
int tot = sum_charmie_levels(ch);
if (!vict)
return FALSE;
if (tot > 160)
return FALSE;
if ((tot + GET_LEVEL(vict)) > 160)
return FALSE;

return TRUE;}
