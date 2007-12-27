/***************************************************************************
 *  Original Diku Mud copyright (C) 1990, 1991 by Sebastian Hammer,        *
 *  Michael Seifert, Hans Henrik St{rfeldt, Tom Madsen, and Katja Nyboe.   *
 *                                                                         *
 *  Merc Diku Mud improvments copyright (C) 1992, 1993 by Michael          *
 *  Chastain, Michael Quan, and Mitchell Tse.                              *
 *                                                                         *
 *  In order to use any part of this Merc Diku Mud, you must comply with   *
 *  both the original Diku license in 'license.doc' as well the Merc       *
 *  license in 'license.txt'.  In particular, you may not remove either of *
 *  these copyright notices.                                               *
 *                                                                         *
 *  Much time and thought has gone into this software and you are          *
 *  benefitting.  We hope that you share your changes too.  What goes      *
 *  around, comes around.                                                  *
 ***************************************************************************/

/***************************************************************************
 *  The MOBprograms have been contributed by N'Atas-ha.  Any support for   *
 *  these routines should not be expected from Merc Industries.  However,  *
 *  under no circumstances should the blame for bugs, etc be placed on     *
 *  Merc Industries.  They are not guaranteed to work on all systems due   *
 *  to their frequent use of strxxx functions.  They are also not the most *
 *  efficient way to perform their tasks, but hopefully should be in the   *
 *  easiest possible way to install and begin using. Documentation for     *
 *  such installation can be found in INSTALL.  Enjoy........    N'Atas-Ha *
 ***************************************************************************/
/*
 * $Log: dg_mobcmd.c,v $
 * Revision 1.25  2007/06/10 02:18:39  w4dimenscor
 * changed all entries in the code of 'color' to 'colour', but i now regret it.
 *
 * Revision 1.24  2006/08/31 10:39:16  w4dimenscor
 * Fixe dthe crash bug in medit. and also changed the mob proto list. there is still a memory leak in medit, which is being fixed now
 *
 * Revision 1.23  2006/08/13 06:26:51  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.22  2006/07/15 12:53:12  w4dimenscor
 * Tweaked mtransform further and it should work fine now.
 *
 * Revision 1.21  2006/07/14 19:06:09  w4dimenscor
 * Fixed mtransform!
 *
 * Revision 1.20  2006/06/18 12:42:21  w4dimenscor
 * %damage% %actor% works again on mobs. The problem was that the damage function was never called if only %actor% was
 * used (in stead of %actor.name%).
 *
 * Revision 1.19  2006/05/30 09:14:19  w4dimenscor
 * rewrote the colour code, process_output, and vwrite_to_output so that they use strings and have better buffer checks
 *
 * Revision 1.18  2006/05/22 10:50:48  w4dimenscor
 * Created 3 new files, mxp.cpp, mxp.h and descriptor.cpp
 * struct descriptor_data has been converted to class Descriptor
 *
 * Revision 1.17  2006/05/21 11:02:26  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.16  2006/05/20 09:33:12  w4dimenscor
 * fixed the bug where if a mob was purged while fighting the people who were fighting it would stay fighting nothing
 *
 * Revision 1.15  2006/05/08 20:55:12  w4dimenscor
 * Whoops, made a little mistake in the trigger firing. All set now.
 *
 * Revision 1.14  2006/05/08 19:36:27  w4dimenscor
 * Commiting some files that were out of the cvs because of the last backup fiasco, and there is also a bugfix for teleport in
 * scripts.
 *
 * Revision 1.13  2006/05/01 11:29:26  w4dimenscor
 * I wrote a typo checker that automaticly corrects typos in the comm channels. I have also been fixing shadowed variables. There may be residual issues with it.
 *
 * Revision 1.12  2006/04/09 05:15:44  w4dimenscor
 *
 * added the ability to script teleports to move followers:
 * %teleport% %actor% 1234 followers
 *
 * Revision 1.11  2006/02/17 22:19:54  w4dimenscor
 * Fixed error for ubuntu that doesnt like empty array declarations, moved ice shield to a better place and fixed its messages, added auto auction fixes, allowed mounts to gain exp properly
 *
 * Revision 1.10  2005/11/19 06:18:38  w4dimenscor
 * Fixed many bugs, and added features
 *
 * Revision 1.9  2005/08/19 08:51:14  w4dimenscor
 * fixed the variables not working
 *
 * Revision 1.8  2005/06/18 12:20:53  w4dimenscor
 * changed a bunch of send_to_char's to new_send_to_chars, adjusted some mxp code
 *
 * Revision 1.7  2005/04/06 07:16:28  w4dimenscor
 * added dg variables: is_roleplay, is_peaceful. Added the GATE, and RP to constants.c
 *
 * Revision 1.6  2005/03/24 09:23:05  w4dimenscor
 * Added info about where a spell can be cast to spellinfo
 *
 * Revision 1.5  2005/03/16 18:47:03  w4dimenscor
 * updated some spacing and formatting
 *
 * Revision 1.4  2005/03/15 09:55:49  w4dimenscor
 * fixed error with mtransform and linked mobs
 *
 * Revision 1.3  2004/12/05 09:46:52  w4dimenscor
 * fixed mtransform, fixed format in clan tell, and added limit on magic items carried, and lowered weight of magic items, and increased cost
 *
 * Revision 1.2  2004/11/20 02:33:25  w4dimenscor
 * updated and cleaned up the script system
 *
 * Revision 1.1.1.1  2004/11/12 02:16:48  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.23  2004/08/15 01:12:26  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */
#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "screen.h"
#include "dg_scripts.h"
#include "db.h"
#include "utils.h"
#include "handler.h"
#include "interpreter.h"
#include "comm.h"
#include "spells.h"
#include "constants.h"
#include "fight.h"
#include "descriptor.h"

void raw_kill(Character *ch, Character *killer);
void send_to_zone_range(char *messg, int zone_rnum, int lower_vnum,
                        int upper_vnum);
void reset_zone(zone_rnum zone);
bitvector_t asciiflag_conv(char *flag);
int real_zone(int number);
void die(Character *ch, Character *killer);
int valid_dg_target(Character *ch, int allow_gods);
room_rnum find_target_room(Character *ch, char *rawroomstr);
void stop_fusion(Character *ch);
void die_link(Character *mob);
extern struct hunter_data *hunter_list;
int followers_to_master(Character *ch, room_rnum was_in);
/*
 * Local functions.
 */
void mob_log(Character *mob, const char *format, ...);
ACMD(do_masound);
ACMD(do_mkill);
ACMD(do_mjunk);
ACMD(do_mechoaround);
ACMD(do_msend);
ACMD(do_mecho);
ACMD(do_mzoneecho);
ACMD(do_mload);
ACMD(do_mpurge);
ACMD(do_mgoto);
ACMD(do_mat);
ACMD(do_mteleport);
ACMD(do_mdamage);
ACMD(do_mforce);
ACMD(do_mhunt);
ACMD(do_mremember);
ACMD(do_mforget);
ACMD(do_mtransform);
ACMD(do_mdoor);
ACMD(do_mfollow);
ACMD(do_mrecho);

/* attaches mob's name and vnum to msg and sends it to script_log */
void mob_log(Character *mob, const char *format, ...) {
    va_list args;
    char buf[MAX_STRING_LENGTH];

    snprintf(buf, sizeof(buf), "Mob (%s, VNum %d):: %s",
             GET_SHORT(mob), GET_MOB_VNUM(mob), format);

    va_start(args, format);
    script_vlog(buf, args);
    va_end(args);
}


/*
** macro to determine if a mob is permitted to use these commands
*/
#define MOB_OR_IMPL(ch) \
  (IS_NPC(ch) && (!(ch)->desc || GET_LEVEL((ch)->desc->original)>=LVL_IMPL))



/* mob commands */

ACMD(do_msteal) {
    Character *vict;
    struct obj_data *obj;
    char vict_name[MAX_INPUT_LENGTH], obj_name[MAX_INPUT_LENGTH];

    vict = get_char_room_vis(ch, vict_name, NULL);
    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    two_arguments(argument, obj_name, vict_name);


    if (!(vict)) {
        mob_log(ch, "msteal: victim not found");
        return;
    } else if (vict == ch) {
        mob_log(ch, "msteal: victim is self");
        return;
    }

    if (!(obj = get_obj_in_list_vis(vict, obj_name, NULL, vict->carrying))) {
        mob_log(ch, "msteal called with no object argument");
        return;
    } else {
        if ((IS_CARRYING_N(ch) + 1 < CAN_CARRY_N(ch))) {
            if ((IS_CARRYING_W(ch) + GET_OBJ_WEIGHT(obj)) <
                    CAN_CARRY_W(ch)) {
                obj_from_char(obj);
                obj_to_char(obj, ch);
                mob_log(ch, "msteal: Successful steal");
                return;
            }
        } else {
            mob_log(ch, "msteal: Cannot carry weight");
            return;
        }
    }
}

/* prints the argument to all the rooms aroud the mobile */
ACMD(do_masound) {
    room_rnum was_in_room;
    int door;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    if (!*argument) {
        mob_log(ch, "masound called with no argument");
        return;
    }

    skip_spaces(&argument);

    was_in_room = IN_ROOM(ch);
    for (door = 0; door < NUM_OF_DIRS; door++) {
        struct room_direction_data *newexit;

        if (((newexit = was_in_room->dir_option[door]) != NULL) &&
                newexit->to_room != NULL && newexit->to_room != was_in_room) {
            IN_ROOM(ch) = newexit->to_room;
            sub_write(argument, ch, TRUE, TO_ROOM);
        }
    }

    IN_ROOM(ch) = was_in_room;
}


/* lets the mobile kill any player or mobile without murder*/
ACMD(do_mkill) {
    char arg[MAX_INPUT_LENGTH];
    Character *victim;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    one_argument(argument, arg);

    if (!*arg) {
        mob_log(ch, "mkill called with no argument");
        return;
    }

    if (*arg == UID_CHAR) {
        if (!(victim = get_char(arg))) {
            mob_log(ch, "mkill: victim (%s) not found",arg);
            return;
        }
    } else if (!(victim = get_char_room_vis(ch, arg, NULL))) {
        mob_log(ch, "mkill: victim (%s) not found",arg);
        return;
    }

    if (SELF(victim, ch)) {
        mob_log(ch, "mkill: victim is self");
        return;
    }

    if (!IS_NPC(victim) && PRF_FLAGGED(victim, PRF_NOHASSLE)) {
        mob_log(ch, "mkill: target has nohassle on");
        return;
    }

    if (FIGHTING(ch)) {
        mob_log(ch, "mkill: already fighting");
        return;
    }

    start_fighting(ch, victim);
    return;
}


ACMD(do_mlag) {
    char arg[MAX_INPUT_LENGTH];
    char arg2[MAX_INPUT_LENGTH];
    Character *victim;
    int w = 0;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    two_arguments(argument, arg, arg2);

    if (!*arg) {
        mob_log(ch, "mkill called with no argument");
        return;
    }

    if (*arg == UID_CHAR) {
        if (!(victim = get_char(arg))) {
            mob_log(ch, "mlag: victim (%s) not found", arg);
            return;
        }
    } else if (!(victim = get_char_room_vis(ch, arg, NULL))) {
        mob_log(ch, "mlag: victim (%s) not found", arg);
        return;
    }

    if (victim == ch) {
        mob_log(ch, "mlag: victim is self");
        return;
    }

    if (!IS_NPC(victim) && PRF_FLAGGED(victim, PRF_NOHASSLE)) {
        mob_log(ch, "mlag: target has nohassle on");
        return;
    }

    if ((w = atoi(arg2)) < 1)
        return;

    if (w > 300) {
        mob_log(ch, "mlag: duration longer then 30 seconds outside range.");
        return;
    }
    if (w <= 0)
        return;


    w = (w RL_SEC)/10;

    WAIT_STATE(ch, w);
    return;
}

/*
 * lets the mobile destroy an object in its inventory
 * it can also destroy a worn object and it can destroy 
 * items using all.xxxxx or just plain all of them
 */
ACMD(do_mjunk) {
    char arg[MAX_INPUT_LENGTH];
    int pos, junk_all = 0;
    obj_data *obj = NULL;
    obj_data *obj_next = NULL;
    int find_eq_pos(Character *ch, struct obj_data *obj, char *arg);


    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    one_argument(argument, arg);

    if (!*arg) {
        mob_log(ch, "mjunk called with no argument");
        return;
    }

    if (!str_cmp(arg, "all"))
        junk_all = 1;

    if (*arg == UID_CHAR)
        obj = get_obj(arg);

    if (obj != NULL) {
        if (obj->worn_by!= NULL)
            extract_obj(unequip_char(ch, find_eq_pos(ch, obj, 0)));
        else
            extract_obj(obj);
        return;
    }


    if ((find_all_dots(arg) == FIND_INDIV) && !junk_all) {
        /* Thanks to Carlos Myers for fixing the line below */
        if ((pos = get_obj_pos_in_equip_vis(ch, arg, NULL, ch->equipment)) >= 0) {
            extract_obj(unequip_char(ch, pos));
            return;
        }
        if ((obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)) != NULL )
            extract_obj(obj);
        return;
    } else {
        for (obj = ch->carrying; obj != NULL; obj = obj_next) {
            obj_next = obj->next_content;
            if (arg[3] == '\0' || isname(arg+4, obj->name)) {
                extract_obj(obj);
            }
        }
        /* Thanks to Carlos Myers for fixing the line below */
        while ((pos = get_obj_pos_in_equip_vis(ch, arg, NULL, ch->equipment)) >= 0)
            extract_obj(unequip_char(ch, pos));
    }
    return;
}



/* prints the message to everyone in the room other than the mob and victim */
ACMD(do_mechoaround) {
    char arg[MAX_INPUT_LENGTH];
    Character *victim;
    char *p;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    p = one_argument(argument, arg);
    skip_spaces(&p);

    if (!*arg) {
        mob_log(ch, "mechoaround called with no argument");
        return;
    }

    if (*arg == UID_CHAR) {
        if (!(victim = get_char(arg))) {
            // sprintf(buf, "mechoaround: victim (%s) does not exist",arg);
            // mob_log(ch, buf);
            return;
        }
    } else if (!(victim = get_char_room_vis(ch, arg, NULL))) {
        // sprintf(buf, "mechoaround: victim (%s) does not exist",arg);
        // mob_log(ch, buf);
        return;
    }
    if (IN_ROOM(victim) != NULL) {
        sub_write(p, victim, TRUE, TO_ROOM);
    } else {
        mob_log(ch, "calling mechoaround when %s is in nowhere", GET_NAME(victim));
    }
}


/* sends the message to only the victim */
ACMD(do_msend) {
    char arg[MAX_INPUT_LENGTH];
    Character *victim;
    char *p;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    p = one_argument(argument, arg);
    skip_spaces(&p);

    if (!*arg) {
        mob_log(ch, "msend called with no argument");
        return;
    }

    if (*arg == UID_CHAR) {
        if (!(victim = get_char(arg))) {
            // sprintf(buf, "msend: victim (%s) does not exist",arg);
            // mob_log(ch, buf);
            return;
        }
    } else if (!(victim = get_char_room_vis(ch, arg, NULL))) {
        // sprintf(buf, "msend: victim (%s) does not exist",arg);
        // mob_log(ch, buf);
        return;
    }

    sub_write(p, victim, TRUE, TO_CHAR);
}

ACMD(do_mzoneecho) {
    int zone;
    char room_number[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;

    msg = any_one_arg(argument, room_number);
    skip_spaces(&msg);

    if (!*room_number || !*msg)
        mob_log(ch, "mzoneecho called with too few args");

    else if ((zone = real_zone(atoi(room_number))) == NOWHERE)
        mob_log(ch, "mzoneecho called for nonexistant zone");

    else {
        sprintf(buf, "%s\r\n", msg);
        send_to_zone(buf, zone);
    }
}


/* prints the message to the room at large */
ACMD(do_mecho) {
    char *p;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    if (!*argument) {
        mob_log(ch, "mecho called with no arguments");
        return;
    }
    p = argument;
    skip_spaces(&p);

    sub_write(p, ch, TRUE, TO_ROOM);
}

/* prints the message to everyone in the zone */
ACMD(do_mzecho) {
    int zone;
    char zone_name[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    msg = any_one_arg(argument, zone_name);
    skip_spaces(&msg);

    if (!*zone_name || !*msg)
        mob_log(ch, "mzoneecho called with too few args");

    else if ((zone = real_zone(atoi(zone_name))) < 0)
        mob_log(ch, "mzoneecho called for nonexistant zone");

    else {
        log("zone: %d.", zone);
        sprintf(buf, "%s\r\n", msg);
        send_to_zone(buf, zone);
    }
}

/* prints the message to everyone in the zone within a range of numbers */
ACMD(do_mzrecho) {
    int zone, lower_vnum, upper_vnum;
    char zone_name[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;
    char strlower[MAX_INPUT_LENGTH], strupper[MAX_INPUT_LENGTH];

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    msg = any_one_arg(argument, zone_name);
    msg = two_arguments(msg, strlower, strupper);

    skip_spaces(&msg);

    if (!*zone_name || !*msg || !*strlower || !*strupper)
        mob_log(ch, "mzrecho called with too few args");


    else if ((zone = real_zone(atoi(zone_name))) < 0)
        mob_log(ch, "mzrecho called for nonexistant zone");

    else {
        lower_vnum = atoi(strlower);
        upper_vnum = atoi(strupper);

        sprintf(buf, "%s\r\n", msg);
        send_to_zone_range(buf, zone, lower_vnum, upper_vnum);
    }
}

/* prints the message to everyone in the range of numbers */
ACMD(do_mrecho) {
    char start[MAX_INPUT_LENGTH], finish[MAX_INPUT_LENGTH], *msg;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }
    msg = two_arguments(argument, start, finish);

    skip_spaces(&msg);

    if (!*msg || !*start || !*finish)
        mob_log(ch, "mrecho called with too few args");
    else
        send_to_range(atoi(start), atoi(finish), "%s\r\n", msg);

}

ACMD(do_mdamage) {
    char name[MAX_INPUT_LENGTH], amount[MAX_INPUT_LENGTH];
    int dam = 0;
    Character *vict;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    two_arguments(argument, name, amount);

    if (!*name || !*amount) {
        mob_log(ch, "mdamage: bad syntax");
        return;
    }

    dam = atoi(amount);
    if (*name == UID_CHAR) {
        if (!(vict = get_char(name))) {
            mob_log(ch, "mdamage: victim (%s) does not exist", name);
            return;
        }
        script_damage(vict,dam);
    } else if (!str_cmp("all", name)) {
        Character *tvict;
        if (!IN_ROOM(ch))
            return;
        /**TODO: I hate this loop, because it is possable that on the extraction
                 of a mob or player after damage, it could wipe the next char.
          **/
        for (vict = IN_ROOM(ch)->people;vict;vict = tvict) {

            if (ch != vict) {
                if (!IS_NPC(vict)) {
                    script_damage(vict, dam);
                }
            }
            if (!DEAD(vict))
                tvict = vict->next_in_room;
            else
                tvict = NULL;
        }
        return;
    } else if (!(vict = get_char_room_vis(ch, name, NULL))) {
        mob_log(ch, "mdamage: victim (%s) does not exist (in this room)", name);
        return;
    } else
        script_damage(vict, dam);
}


/*
 * lets the mobile load an item or mobile.  All items
 * are loaded into inventory, unless it is NO-TAKE. 
 */
ACMD(do_mload) {
    char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
    int num = 0;
    Character *mob;
    obj_data *object;
    char *target;
    Character *tch;
    obj_data *cnt;
    int pos;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    if( ch->desc && GET_LEVEL(ch->desc->original) < LVL_IMPL)
        return;

    target = two_arguments(argument, arg1, arg2);

    skip_spaces(&target);

    if (!*arg1 || !*arg2 || !is_number(arg2) || ((num = atoi(arg2)) < 0)) {
        mob_log(ch, "mload: bad syntax");
        return;
    }

    /* load mob to target room - Jamie Nelson, April 13 2004 */
    if (is_abbrev(arg1, "mob")) {
        room_rnum rnum;
        if (!target || !*target)
            rnum = IN_ROOM(ch);
        else {
            if (!isdigit(*target) || (rnum = real_room(atoi(target))) == NULL) {
                mob_log(ch, "mload: room target vnum doesn't exist (loading mob vnum %d to room %s)", num, target);
                return;
            }
        }
        if ((mob = read_mobile(num)) == NULL) {
            mob_log(ch, "mload: bad mob vnum");
            return;

        }

        char_to_room(mob, rnum);
        if (load_mtrigger(mob)!= -1) {
            if (SCRIPT(ch)) { // it _should_ have, but it might be detached.
                char buf[MAX_INPUT_LENGTH];
                sprintf(buf, "%c%ld", UID_CHAR, GET_ID(mob));
                add_var(&(SCRIPT(ch)->global_vars), "loaded", buf, 0);
            }
        }
    } else if (is_abbrev(arg1, "obj")) {
        if ((object = read_object(num, VIRTUAL)) == NULL) {
            mob_log(ch, "mload: bad object vnum");
            return;
        }

        if (SCRIPT(ch)) { // it _should_ have, but it might be detached.
            char buf[MAX_INPUT_LENGTH];
            sprintf(buf, "%c%ld", UID_CHAR, GET_ID(object));
            add_var(&(SCRIPT(ch)->global_vars), "loaded", buf, 0);
        }

        /* special handling to make objects able to load on a person/in a container/worn etc. */
        if (!target || !*target) {
            if (CAN_WEAR(object, ITEM_WEAR_TAKE)) {
                if (GET_OBJ_VNUM(object) >= 3300 && GET_OBJ_VNUM(object) <= 3312) {
                    if (IN_ROOM(ch))
                        mob_log(ch, "[TOKEN] %s loads %s in %d",  GET_NAME(ch), object->short_description, GET_ROOM_VNUM(IN_ROOM(ch)));
                }
                obj_to_char(object, ch);
            } else {
                obj_to_room(object, IN_ROOM(ch));
            }
            load_otrigger(object);
            return;
        }
        two_arguments(target, arg1, arg2); /* recycling ... */
        tch = (arg1 && *arg1 == UID_CHAR) ? get_char(arg1) : get_char_room_vis(ch, arg1, NULL);
        if (tch) {
            if (arg2 && *arg2 &&
                    (pos = find_eq_pos_script(arg2)) >= 0 &&
                    !GET_EQ(tch, pos) &&
                    can_wear_on_pos(object, pos)) {
                equip_char(tch, object, pos);
                load_otrigger(object);
                return;
            }
            if (GET_OBJ_VNUM(object) >= 3300 && GET_OBJ_VNUM(object) <= 3312) {
                if (IN_ROOM(ch))
                    mob_log(ch, "[TOKEN] %s loads %s to %s in %d",  GET_NAME(ch), object->short_description,GET_NAME(tch), GET_ROOM_VNUM(IN_ROOM(ch)));
            }
            obj_to_char(object, tch);
            load_otrigger(object);
            return;
        }
        cnt = (arg1 && *arg1 == UID_CHAR) ? get_obj(arg1) : get_obj_vis(ch, arg1, NULL);
        if (cnt && GET_OBJ_TYPE(cnt) == ITEM_CONTAINER) {
            if (GET_OBJ_VNUM(object) >= 3300 && GET_OBJ_VNUM(object) <= 3312) {
                if (IN_ROOM(ch))
                    mob_log(ch, "[TOKEN] %s loads %s to %s in %d",  GET_NAME(ch), object->short_description, cnt->short_description, GET_ROOM_VNUM(IN_ROOM(ch)));
            }
            obj_to_obj(object, cnt);
            load_otrigger(object);
            return;
        }
        /* neither char nor container found - just dump it in room */
        if (GET_OBJ_VNUM(object) >= 3300 && GET_OBJ_VNUM(object) <= 3312) {
            if (IN_ROOM(ch))
                mob_log(ch, "[TOKEN] %s loads %s to room %d",  GET_NAME(ch), object->short_description,GET_ROOM_VNUM(IN_ROOM(ch)));
        }
        obj_to_room(object, IN_ROOM(ch));
        load_otrigger(object);
        return;
    } else
        mob_log(ch, "mload: bad type");
}
/*
 * lets the mobile purge all objects and other npcs in the room,
 * or purge a specified object or mob in the room.  It can purge
 *  itself, but this will be the last command it does.
 */
ACMD(do_mpurge) {
    char arg[MAX_INPUT_LENGTH];
    Character *victim;
    obj_data  *obj;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
        return;

    one_argument(argument, arg);

    if (!*arg) {
        /* 'purge' */
        Character *vnext;
        obj_data  *obj_next;

        for (victim = IN_ROOM(ch)->people; victim; victim = vnext) {
            vnext = victim->next_in_room;
            if (IS_NPC(victim) && victim != ch)
                extract_char(victim);
        }

        for (obj = IN_ROOM(ch)->contents; obj; obj = obj_next) {
            if (GET_OBJ_VNUM(obj) >= 3300 && GET_OBJ_VNUM(obj) <= 3312) {
                if (IN_ROOM(ch))
                    mob_log(ch, "[TOKEN] %s purges %s in room %d",  GET_NAME(ch), obj->short_description,GET_ROOM_VNUM(IN_ROOM(ch)));
            }
            obj_next = obj->next_content;
            extract_obj(obj);
        }

        return;
    }

    if (*arg == UID_CHAR)
        victim = get_char(arg);
    else
        victim = get_char_room_vis(ch, arg, NULL);

    if (victim == NULL) {
        if (*arg == UID_CHAR)
            obj = get_obj(arg);
        else
            obj = get_obj_vis(ch, arg, NULL);

        if (obj) {
            if (GET_OBJ_VNUM(obj) >= 3300 && GET_OBJ_VNUM(obj) <= 3312) {
                if (IN_ROOM(ch))
                    mob_log(ch, "[TOKEN] %s purges %s",  GET_NAME(ch), obj->short_description);
            }
            extract_obj(obj);
            obj = NULL;
        } else
            mob_log(ch, "mpurge: bad argument");

        return;
    }

    if (!IS_NPC(victim)) {
        mob_log(ch, "mpurge: purging a PC (%s)", GET_NAME(victim));
        return;
    }

    if (victim==ch)
        dg_owner_purged = 1;

    extract_char(victim);
}


/* lets the mobile goto any location it wishes that is not private */
ACMD(do_mgoto) {
    char arg[MAX_INPUT_LENGTH];
    room_rnum location;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    one_argument(argument, arg);

    if (!*arg) {
        mob_log(ch, "mgoto called with no argument");
        return;
    }

    if ((location = find_target_room(ch, arg)) == NULL) {
        mob_log(ch, "mgoto: invalid location");
        return;
    }

    if (FIGHTING(ch))
        stop_fighting(ch);

    char_from_room(ch);
    char_to_room(ch, location);
    enter_wtrigger(IN_ROOM(ch), ch, -1);
}


/* lets the mobile do a command at another location. Very useful */
ACMD(do_mat) {
    char arg[MAX_INPUT_LENGTH];
    room_rnum location, original;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    argument = one_argument(argument, arg);

    if (!*arg || !*argument) {
        mob_log(ch, "mat: bad argument");
        return;
    }

    if ((location = find_target_room(ch, arg)) == NULL) {
        mob_log(ch, "mat: invalid location");
        return;
    }

    original = IN_ROOM(ch);
    char_from_room(ch);
    char_to_room(ch, location);
    command_interpreter(ch, argument);

    /*
     * See if 'ch' still exists before continuing!
     * Handles 'at XXXX quit' case.
     */
    if (IN_ROOM(ch) == location) {
        char_from_room(ch);
        char_to_room(ch, original);
    }
}


/*
 * lets the mobile transfer people.  the all argument transfers
 * everyone in the current room to the specified location
 */
ACMD(do_mteleport) {
    char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
    room_rnum target, was_in;
    Character *vict, *next_ch;
    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    argument = two_arguments(argument, arg1, arg2);
    skip_spaces(&argument);

    if (!*arg1 || !*arg2) {
        mob_log(ch, "mteleport: bad syntax");
        return;
    }

    target = find_target_room(ch, arg2);

    if (target == NULL) {
        mob_log(ch, "mteleport target is an invalid room");
        return;
    }
    if (!str_cmp(arg1, "all")) {
        if (target == IN_ROOM(ch)) {
            //mob_log(ch, "mteleport all is teleporting people to the same room as the mob teleporting them is in");
            return;
        }

        for (vict = IN_ROOM(ch)->people; vict; vict = next_ch) {
            next_ch = vict->next_in_room;
            if (vict == ch)
                continue;
            if (valid_dg_target(vict, TRUE)) {
                char_from_room(vict);
                char_to_room(vict, target);
                entry_memory_mtrigger(ch);
                greet_mtrigger(ch, -1);
                greet_memory_mtrigger(ch);
                enter_wtrigger(IN_ROOM(ch), ch, -1);
            }
        }
    } else {
        if (*arg1 == UID_CHAR) {
            if (!(vict = get_char(arg1))) {
                // sprintf(buf, "mteleport: victim (%s) does not exist",arg1);
                // mob_log(ch, buf);
                return;
            }
        } else if (!(vict = get_char_vis(ch, arg1, NULL, FIND_CHAR_ROOM))) {
            // sprintf(buf, "mteleport: victim (%s) does not exist",arg1);
            // mob_log(ch, buf);
            return;
        }
        if (valid_dg_target(vict, TRUE)) {
            was_in = IN_ROOM(vict);
            char_from_room(vict);
            char_to_room(vict, target);
            entry_memory_mtrigger(vict);
            greet_mtrigger(vict, -1);
            greet_memory_mtrigger(vict);
            enter_wtrigger(IN_ROOM(vict), vict, -1);
            if (isname(argument, "followers"))
                followers_to_master(vict, was_in);
        }
    }
}


/*
 * lets the mobile force someone to do something.  must be mortal level
 * and the all argument only affects those in the room with the mobile
 */
ACMD(do_mforce) {
    char arg[MAX_INPUT_LENGTH];

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
        return;

    argument = one_argument(argument, arg);

    if (!*arg || !*argument) {
        mob_log(ch, "mforce: bad syntax");
        return;
    }

    if (!str_cmp(arg, "all")) {
        Descriptor *i;
        Character *vch;

        for (i = descriptor_list; i; i = i->next) {
            lock_desc(i);
            if ((i->character != ch) && !i->connected &&
                    (IN_ROOM(i->character) == IN_ROOM(ch))) {
                vch = i->character;
                if (GET_LEVEL(vch) < GET_LEVEL(ch) && CAN_SEE(ch, vch) &&
                        valid_dg_target(vch, FALSE)) {
                    command_interpreter(vch, argument);
                }
            }
            unlock_desc(i);
        }
    } else {
        Character *victim;

        if (*arg == UID_CHAR) {
            if (!(victim = get_char(arg))) {
                // sprintf(buf, "mforce: victim (%s) does not exist",arg);
                // mob_log(ch, buf);
                return;
            }
        } else if ((victim = get_char_room_vis(ch, arg, NULL)) == NULL) {
            // mob_log(ch, "mforce: no such victim");
            return;
        }

        if (victim == ch) {
            mob_log(ch, "mforce: forcing self");
            return;
        }

        if (valid_dg_target(victim, FALSE)) {
            command_interpreter(victim, argument);
        }
    }
}


/* hunt for someone */
ACMD(do_mhunt) {
    Character *victim;
    char arg[MAX_INPUT_LENGTH];

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
        return;

    one_argument(argument, arg);

    if (!*arg) {
        mob_log(ch, "mhunt called with no argument");
        return;
    }


    if (FIGHTING(ch))
        return;

    if (*arg == UID_CHAR) {
        if (!(victim = get_char(arg))) {
            // sprintf(buf, "mhunt: victim (%s) does not exist", arg);
            // mob_log(ch, buf);
            return;
        }
    } else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM))) {
        // sprintf(buf, "mhunt: victim (%s) does not exist", arg);
        // mob_log(ch, buf);
        return;
    }
    HUNTING(ch) = victim;
    remove_hunter(ch);
    add_hunter(ch);



}


/* place someone into the mob's memory list */
ACMD(do_mremember) {
    Character *victim;
    struct script_memory *mem;
    char arg[MAX_INPUT_LENGTH];

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
        return;

    argument = one_argument(argument, arg);

    if (!*arg) {
        mob_log(ch, "mremember: bad syntax");
        return;
    }

    if (*arg == UID_CHAR) {
        if (!(victim = get_char(arg))) {
            // sprintf(buf, "mremember: victim (%s) does not exist", arg);
            // mob_log(ch, buf);
            return;
        }
    } else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM))) {
        // sprintf(buf, "mremember: victim (%s) does not exist", arg);
        // mob_log(ch, buf);
        return;
    }

    /* create a structure and add it to the list */
    CREATE(mem, struct script_memory, 1);
    if (!SCRIPT_MEM(ch))
        SCRIPT_MEM(ch) = mem;
    else {
        struct script_memory *tmpmem = SCRIPT_MEM(ch);
        while (tmpmem->next)
            tmpmem = tmpmem->next;
        tmpmem->next = mem;
    }

    /* fill in the structure */
    mem->id = GET_ID(victim);
    if (argument && *argument) {
        log("strdup from mremember");
        mem->cmd = strdup(argument);
    }
}


/* remove someone from the list */
ACMD(do_mforget) {
    Character *victim;
    struct script_memory *mem, *prev;
    char arg[MAX_INPUT_LENGTH];

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    if (ch->desc && (GET_LEVEL(ch->desc->original) < LVL_IMPL))
        return;

    one_argument(argument, arg);

    if (!*arg) {
        mob_log(ch, "mforget: bad syntax");
        return;
    }

    if (*arg == UID_CHAR) {
        if (!(victim = get_char(arg))) {
            // sprintf(buf, "mforget: victim (%s) does not exist", arg);
            // mob_log(ch, buf);
            return;
        }
    } else if (!(victim = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM))) {
        // sprintf(buf, "mforget: victim (%s) does not exist", arg);
        // mob_log(ch, buf);
        return;
    }

    mem = SCRIPT_MEM(ch);
    prev = NULL;
    while (mem) {
        if (mem->id == GET_ID(victim)) {
            if (mem->cmd)
                free(mem->cmd);
            if (prev == NULL) {
                SCRIPT_MEM(ch) = mem->next;
                free(mem);
                mem = SCRIPT_MEM(ch);
            } else {
                prev->next = mem->next;
                free(mem);
                mem = prev->next;
            }
        } else {
            prev = mem;
            mem = mem->next;
        }
    }
}


/* transform into a different mobile */
ACMD(do_mtransform) {
    char arg[MAX_INPUT_LENGTH];
    Character *m;//, tmpmob, *tm;
    obj_data *obj[NUM_WEARS];
    struct hunter_data *hunt = NULL, *hnext;
//    mob_rnum new_rnum;//this_rnum = GET_MOB_RNUM(ch), new_rnum;
    int keep_hp = 1;       /* new mob keeps the old mob's hp/max hp/exp */
    int pos;
    mob_vnum mvnum = 0;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    if (ch->desc) {
        ch->Send("You've got no VNUM to return to, dummy! try 'switch'\r\n");
        return;
    }

    one_argument(argument, arg);

    if (!*arg)
        mob_log(ch, "mtransform: missing argument");
    else if (!isdigit(*arg) && *arg != '-')
        mob_log(ch, "mtransform: bad argument");
    else {

        if (isdigit(*arg))
            mvnum = atoi(arg);
        else {
            keep_hp = 0;
            mvnum = atoi(arg + 1);
        }

        if (mvnum != GET_MOB_VNUM(ch)) {
            m = read_mobile(mvnum);
            if (m == NULL) {
                mob_log(ch, "mtransform: bad mobile vnum");
                return;
            }

            /* move new obj info over to old object and delete new obj */

            for (pos = 0; pos < NUM_WEARS; pos++) {
                if (GET_EQ(ch, pos))
                    obj[pos] = unequip_char(ch, pos);
                else
                    obj[pos] = NULL;
            }

            if (!ch->proto && ch->vnum != NOBODY)
                GetMobIndex(ch->vnum)->number--;

            /* put the mob in the same room as ch so extract will work */
            char_to_room(m, IN_ROOM(ch));

            char_from_chair(ch);
            stop_fusion(ch);

            remove_hunter(ch);

            /* we can't forget the hunters either... */
            for (hunt = hunter_list; hunt; hunt = hnext) {
                hnext = hunt->next;

                if (!hunt->hunter)
                    continue;
                if (HUNTING(hunt->hunter) != ch)
                    continue;

                forget(hunt->hunter, ch);
                forget(ch, hunt->hunter);
                remove_hunter(hunt->hunter);

            }

            if (keep_hp) {
                GET_HIT(m) = GET_HIT(m);
                GET_MAX_HIT(m) = GET_MAX_HIT(ch);
                GET_EXP(m) = GET_EXP(ch);
            }
#if 0
            //tmpmob = *m;
            //tm = &tmpmob;
            /*memcpy(&tmpmob, m, sizeof(*m));
            //rryan: we need to copy the strings so we don't end up free'ing the prototypes later
            if(m->player.name.size() > 0)
              tmpmob.player.name = m->player.name;
            if(m->player.title->size() > 0)
              tmpmob.player.title = m->player.title;
            if(m->player.short_descr.size() > 0)
              tmpmob.player.short_descr = m->player.short_descr;
            if(m->player.long_descr.size() > 0)
              tmpmob.player.long_descr = m->player.long_descr;
            if(m->player.description.size() > 0)
              tmpmob.player.description = m->player.description; */
            tm->id = ch->id;
            /** affected gets brought over automaticly in the assign above */
            tm->affected = ch->affected;
            tm->carrying = ch->carrying;
            SITTING(tm) = SITTING(ch);
            tm->hitched = ch->hitched;
            tm->proto_script = ch->proto_script;
            tm->script = ch->script;
            tm->memory = ch->memory;
            tm->next_in_room = ch->next_in_room;
            tm->next = ch->next;
            tm->next_fighting = ch->next_fighting;
            tm->followers = ch->followers;
            tm->master = ch->master;
            tm->mob_specials = ch->mob_specials;
            GET_FIGHT_EVENT(tm) = GET_FIGHT_EVENT(ch);
            GET_MESSAGE_EVENT(tm) = GET_MESSAGE_EVENT(ch);

            for (int i = 0; i < 4; i++)
                GET_POINTS_EVENT(tm, i) = GET_POINTS_EVENT(ch, i);

            GET_WAS_IN(tm) = GET_WAS_IN(ch);
            if (keep_hp) {
                GET_HIT(tm) = GET_HIT(ch);
                GET_MAX_HIT(tm) = GET_MAX_HIT(ch);
                GET_EXP(tm) = GET_EXP(ch);
            }
            GET_GOLD(tm) = GET_GOLD(ch);
            GET_POS(tm) = GET_POS(ch);
            IS_CARRYING_W(tm) = IS_CARRYING_W(ch);
            IS_CARRYING_N(tm) = IS_CARRYING_N(ch);
            FIGHTING(tm) = FIGHTING(ch);
            HUNTING(tm) = HUNTING(ch);
            RIDING(tm) = RIDING(ch);
            RIDDEN_BY(tm) = RIDDEN_BY(ch);
            MOB_TIER(tm) = MOB_TIER(ch);
#endif

            ch->assign(m);
            //memcpy(ch, tm, sizeof(*ch));

            //    *ch = tmpmob;

            for (pos = 0; pos < NUM_WEARS; pos++) {
                if (obj[pos])
                    equip_char(ch, obj[pos], pos);
            }
            //ch->nr = new_rnum;
            GET_FIGHT_EVENT(m) = NULL;
            GET_MESSAGE_EVENT(m) = NULL;
            for (int i = 0; i < 4; i++)
                GET_POINTS_EVENT(m, i) = NULL;
            extract_char(m);
        } else {
            /* mtransforming into self, so lets just */
            if (keep_hp)
                GET_HIT(ch) = GET_MAX_HIT(ch);

        }
    }
}


ACMD(do_mdoor) {
    char target[MAX_INPUT_LENGTH], direction[MAX_INPUT_LENGTH];
    char field[MAX_INPUT_LENGTH], *value;
    Room *rm;
    struct room_direction_data *newexit;
    int dir, fd;
    room_rnum to_room;

    const char *door_field[] = {
                                   "purge",
                                   "description",
                                   "flags",
                                   "key",
                                   "name",
                                   "room",
                                   "\n"
                               };


    if (!MOB_OR_IMPL(ch)) {
        ch->Send("Huh?!?\r\n");
        return;
    }

    if (AFF_FLAGGED(ch, AFF_CHARM))
        return;

    argument = two_arguments(argument, target, direction);
    value = one_argument(argument, field);
    skip_spaces(&value);

    if (!*target || !*direction || !*field) {
        mob_log(ch, "mdoor called with too few args");
        return;
    }

    if ((rm = get_room(target)) == NULL) {
        mob_log(ch, "mdoor: invalid target");
        return;
    }

    if ((dir = search_block(direction, dirs, FALSE)) == -1) {
        mob_log(ch, "mdoor: invalid direction");
        return;
    }

    if ((fd = search_block(field, door_field, FALSE)) == -1) {
        mob_log(ch, "odoor: invalid field");
        return;
    }

    newexit = rm->dir_option[dir];

    /* purge exit */
    if (fd == 0) {
        if (newexit) {
            if (newexit->general_description)
                free(newexit->general_description);
            if (newexit->keyword)
                free(newexit->keyword);
            free(newexit);
            rm->dir_option[dir] = NULL;
        }
    } else {
        if (!newexit) {
            CREATE(newexit, struct room_direction_data, 1);
            rm->dir_option[dir] = newexit;
        }

        switch (fd) {
        case 1:         /* description */
            if (newexit->general_description)
                free(newexit->general_description);
            CREATE(newexit->general_description, char, strlen(value) + 3);
            strcpy(newexit->general_description, value);
            strcat(newexit->general_description, "\r\n");
            break;
        case 2:         /* flags       */
            newexit->exit_info = (sh_int) asciiflag_conv(value);
            break;
        case 3:         /* key         */
            newexit->key = atoi(value);
            break;
        case 4:         /* name        */
            if (newexit->keyword)
                free(newexit->keyword);
            CREATE(newexit->keyword, char, strlen(value) + 1);
            strcpy(newexit->keyword, value);
            break;
        case 5:         /* room        */
            if ((to_room = real_room(atoi(value))) != NULL)
                newexit->to_room = to_room;
            else
                mob_log(ch, "mdoor: invalid door target");
            break;
        }
    }
}

ACMD(do_mslay) {
    Character *vict;
    char arg[MAX_INPUT_LENGTH];

    if (!MOB_OR_IMPL(ch)) {
        ch->Send("Huh?!?\r\n");
        return;
    }

    one_argument(argument, arg);

    if (!*arg) {
        ch->Send("Kill who?\r\n");
        return;
    }

    if (!(vict = get_char_room_vis(ch, arg, NULL))) {
        ch->Send("They aren't here.\r\n");
        return;
    }

    if (ch == vict) {
        ch->Send("Your mother would be so sad.. :(\r\n");
        return;
    }

    act("You chop $M to pieces!  Ah!  The blood!", FALSE, ch, 0, vict,
        TO_CHAR);
    act("$N chops you to pieces!", FALSE, vict, 0, ch, TO_CHAR);
    act("$n brutally slays $N!", FALSE, ch, 0, vict, TO_NOTVICT);

    if (vict)
        raw_kill(vict, ch);

}

ACMD(do_mcollision) {
    struct obj_data *obj = NULL, *obj_next = NULL;
    int damage = 0;
    room_rnum was_in;
    char arg[MAX_INPUT_LENGTH];

    if (!MOB_OR_IMPL(ch)) {
        ch->Send( "Huh?!?\r\n");
        return;
    }

    one_argument(argument, arg);

    if (!arg) {
        mob_log(ch, "mcollision: called without amount of damage.");
        return;
    }

    damage = atoi(arg);

    if (damage < 0 || damage > 3) {
        mob_log(ch, "mcollision: damage value is out of range, %d.",
                damage);
        return;
    }

    was_in = IN_ROOM(ch);

    for (obj = IN_ROOM(ch)->contents; obj; obj = obj_next) {
        obj_next = obj->next_content;
        if (GET_OBJ_TYPE(obj) == ITEM_VEHICLE) {
            IN_ROOM(ch) = real_room(GET_OBJ_VAL(obj, 0));
            GET_OBJ_VAL(obj, 2) -= damage;
            if (GET_OBJ_VAL(obj, 2) <= 0) {   // blow it up
                //act("As the $p collides with $n, it explodes.",
                //    FALSE, ch, obj, NULL, TO_ROOM);
                IN_ROOM(ch) = was_in;
                act("As $p collides with $n, it explodes.",
                    FALSE, ch, obj, NULL, TO_ROOM);

                extract_obj(obj);
                return;
            } else {
                act("$n collides with the ship, causing damage to the hull.", FALSE, ch, obj, NULL, TO_ROOM);
                IN_ROOM(ch) = was_in;
                act("$n collides with $p, causing some damage to $p.",
                    FALSE, ch, obj, NULL, TO_ROOM);
                return;
            }
        }
    }
}

ACMD(do_mzreset) {
    zone_rnum i;

    if (!MOB_OR_IMPL(ch)) {
        ch->Send("Huh?!?\r\n");
        return;
    }

    i = ch->in_room->zone;

    if (i >= 0 && i <= top_of_zone_table) {
        reset_zone(i);
        new_mudlog(NRM, MAX(LVL_GRGOD, GET_INVIS_LEV(ch)), TRUE, "(GC) %s reset zone %d (%s)", GET_NAME(ch), i,
                   zone_table[i].name);
    } else
        ch->Send("Invalid zone number.\r\n");
}
