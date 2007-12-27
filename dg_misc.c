/**************************************************************************
*  File: dg_misc.c                                                        *
*  Usage: contains general functions for script usage.                    *
*                                                                         *
*                                                                         *
**************************************************************************/

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "dg_scripts.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "dg_event.h"
#include "db.h"
#include "screen.h"
#include "spells.h"
#include "constants.h"

/* copied from spell_parser.c: */
//#define SINFO spell_info[spellnum]

/* external vars */
void die(Character *ch, Character * killer);


/* cast a spell; can be called by mobiles, objects and rooms, and no   */
/* level check is required. Note that mobs should generally use the    */
/* normal 'cast' command (which must be patched to allow mobs to cast  */
/* spells) as the spell system is designed to have a character caster, */
/* and this cast routine may overlook certain issues.                  */
/* LIMITATION: a target MUST exist for the spell unless the spell is   */
/* set to TAR_IGNORE. Also, group spells are not permitted             */
/* code borrowed from do_cast() */
void do_dg_cast(void *go, struct script_data *sc, trig_data *trig,
                int type, char *cmd) {
    Character *caster = NULL;
    Character *tch = NULL;
    struct obj_data *tobj = NULL;
    Room *caster_room = NULL;
    char *s, *t;
    int spellnum, target = 0;
    char buf2[MAX_STRING_LENGTH], orig_cmd[MAX_INPUT_LENGTH];

    /* need to get the caster or the room of the temporary caster */
    switch (type) {
    case MOB_TRIGGER:
        caster = (Character *)go;
        break;
    case WLD_TRIGGER:
        caster_room = (Room *)go;
        break;
    case OBJ_TRIGGER:
        caster_room = dg_room_of_obj((struct obj_data *)go);
        if (!caster_room) {
            script_log("dg_do_cast: unknown room for object-caster!");
            return;
        }
        break;
    default:
        script_log("dg_do_cast: unknown trigger type!");
        return;
    }
    strcpy(orig_cmd, cmd);
    /* get: blank, spell name, target name */
    s = strtok(cmd, "'");
    if (s == NULL) {
        script_log("Trigger: %s, VNum %d. dg_cast needs spell name.",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig));
        return;
    }
    s = strtok(NULL, "'");
    if (s == NULL) {
        script_log("Trigger: %s, VNum %d. dg_cast needs spell name in `'s.",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig));
        return;
    }
    t = strtok(NULL, "\0");

    /* spellnum = search_block(s, spells, 0); */
    spellnum = find_skill_num(s);
    if ((spellnum < 1) || (spellnum > MAX_SPELLS)) {
        script_log("Trigger: %s, VNum %d. dg_cast: invalid spell name (%s)",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig), orig_cmd);
        return;
    }

    /* Find the target */
    if (t != NULL) {
        one_argument(strcpy(buf2, t), t);
        skip_spaces(&t);
    }
    if (IS_SET(SINFO.targets, TAR_IGNORE)) {
        target = TRUE;
    } else if (t != NULL && *t) {
        if (!target &&
                (IS_SET(SINFO.targets, TAR_CHAR_ROOM) ||
                 IS_SET(SINFO.targets, TAR_CHAR_WORLD))) {
            if ((tch = get_char(t)) != NULL)
                target = TRUE;
        }

        if (!target &&
                (IS_SET(SINFO.targets, TAR_OBJ_INV) ||
                 IS_SET(SINFO.targets, TAR_OBJ_EQUIP) ||
                 IS_SET(SINFO.targets, TAR_OBJ_ROOM) ||
                 IS_SET(SINFO.targets, TAR_OBJ_WORLD))) {
            if ((tobj = get_obj(t)) != NULL)
                target = TRUE;
        }

        if (!target) {
            script_log("Trigger: %s, VNum %d. dg_cast: target not found (%s)",
                       GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig), orig_cmd);
            return;
        }
    }

    if (IS_SET(SINFO.routines, MAG_GROUPS)) {
        script_log("Trigger: %s, VNum %d. dg_cast: group spells not permitted (%s)",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig), orig_cmd);
        return;
    }
    if (IS_SET(SINFO.targets, TAR_AREA_DIR)) {
        script_log("Trigger: %s, VNum %d. dg_cast: directional spells not permitted (%s)",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig), orig_cmd);
        return;
    }

    if (!caster) {
        caster = read_mobile(DG_CASTER_PROXY, VIRTUAL);
        if (!caster) {
            script_log("dg_cast: Cannot load the caster mob!");
            return;
        }
        /* set the caster's name to that of the object, or the gods.... */
        if (type==OBJ_TRIGGER)
            caster->player.short_descr = strdup(((struct obj_data *)go)->short_description);
        else if (type==WLD_TRIGGER)
            caster->player.short_descr = strdup("The gods");
        char_to_room(caster, caster_room);
        /*
        caster->next_in_room = caster_room->people;
        caster_room->people = caster;
        caster->in_room = real_room(caster_room->number);
        */
        call_magic(caster, tch, tobj,"", spellnum, DG_SPELL_LEVEL, CAST_SPELL);
        extract_char(caster);
    } else
        call_magic(caster, tch, tobj,"", spellnum, GET_LEVEL(caster), CAST_SPELL);
}


/* modify an affection on the target. affections can be of the AFF_x  */
/* variety or APPLY_x type. APPLY_x's have an integer value for them  */
/* while AFF_x's have boolean values. In any case, the duration MUST  */
/* be non-zero.                                                       */
/* usage:  apply <target> <property> <value> <duration>               */
#define APPLY_TYPE	1
#define AFFECT_TYPE	2
void do_dg_affect(void *go, struct script_data *sc, trig_data *trig,
                  int script_type, char *cmd) {
    Character *ch = NULL;
    int value=0, duration=0;
    char junk[MAX_INPUT_LENGTH]; /* will be set to "dg_affect" */
    char charname[MAX_INPUT_LENGTH], property[MAX_INPUT_LENGTH];
    char value_p[MAX_INPUT_LENGTH], duration_p[MAX_INPUT_LENGTH];
    int i=0, type=0;
    struct affected_type af;


    half_chop(cmd, junk, cmd);
    half_chop(cmd, charname, cmd);
    half_chop(cmd, property, cmd);
    half_chop(cmd, value_p, duration_p);

    /* make sure all parameters are present */
    if (!charname || !*charname || !property || !*property ||
            !value_p || !*value_p || !duration_p || !*duration_p) {
        script_log("Trigger: %s, VNum %d. dg_affect usage: <target> <property> <value> <duration>",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig));
        return;
    }

    value = atoi(value_p);
    duration = atoi(duration_p);
    if (duration <= 0) {
        script_log("Trigger: %s, VNum %d. dg_affect: need positive duration!",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig));
        return;
    }

    /* find the property -- first search apply_types */
    i = 0;
    while (str_cmp(apply_types[i], "\n")) {
        if (!str_cmp(apply_types[i], property)) {
            type=APPLY_TYPE;
            break;
        }
        i++;
    }

    if (!type) { /* search affect_types now */
        i = 0;
        while (str_cmp(affected_bits[i], "\n")) {
            if (!str_cmp(affected_bits[i], property)) {
                type=AFFECT_TYPE;
                break;
            }
            i++;
        }
    }

    if (!type) { /* property not found */
        script_log("Trigger: %s, VNum %d. dg_affect: unknown property '%s'!",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig), property);
        return;
    }


    /* locate the target */
    ch = get_char(charname);
    if (!ch) {
        script_log("Trigger: %s, VNum %d. dg_affect: cannot locate target!",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig));
        return;
    }
    if (!str_cmp(value_p, "off")) {
        affect_from_char(ch, SPELL_DG_AFFECT);
        return;
    }

    /* add the affect */
    af.type = SPELL_DG_AFFECT;
    af.expire = sec_to_time(duration);
    af.modifier = value;
    if (type == APPLY_TYPE) {
        af.location = (i);
        af.bitvector = 0;
    } else {
        af.location = 0;
        af.bitvector = (1<<(i-1));
    }

    affect_to_char(ch, &af);
}



/* Used throughout the xxxcmds.c files for checking if a char
 * can be targetted
 * - allow_gods is false when called by %force%, for instance,
 * while true for %teleport%.  -- Welcor
 */
int valid_dg_target(Character *ch, int allow_gods) {
    if (!ch)
        return FALSE;
    else if (IS_NPC(ch))
        return TRUE;		/* all npcs are allowed as targets */
    else if (GET_LEVEL(ch) < LVL_IMMORT)
        return TRUE;		/* as well as all mortals */
    else if (!allow_gods && GET_LEVEL(ch) >= LVL_IMMORT)
        return FALSE;		/* but not always the highest gods */
    else if (!PRF_FLAGGED(ch, PRF_NOHASSLE)
             && GET_INVIS_LEV(ch) < LVL_IMMORT)
        return TRUE;		/* the ones in between as allowed as long as they're visible,
    				 * and have no-hassle off.   */
    else
        return FALSE;		/* The rest are gods with nohassle on... */
}

/* Add or remove a destination for a mob or an object              *
 * to the end of their destination list.                           *
 * usage:  dg_dest (add/remove) <room vnum/all> <target>           *
 * Jamie Nelson - mordecai@xtra.co.nz      (3rd mar 04)            */

void do_dg_destination(void *go, struct script_data *sc, trig_data *trig,
                       int script_type, char *cmd) {
    Character *ch = NULL, *c = NULL;
    obj_data *obj = NULL, *o = NULL;
    Room *room = NULL;
    char junk[MAX_INPUT_LENGTH]; /* will be set to "dg_affect" */
    char addrem[MAX_INPUT_LENGTH], dest[MAX_INPUT_LENGTH];
    char name[MAX_INPUT_LENGTH];
    room_rnum loc = NULL;
    int all = FALSE;
    int add
        = 0;

    half_chop(cmd, junk, cmd);
    half_chop(cmd, addrem, cmd);
    half_chop(cmd, dest, cmd);
    half_chop(cmd, name, cmd);

    /* make sure all parameters are present */
    if (!addrem || !*addrem || !dest || !*dest || !name || !*name) {
        script_log("Trigger: %s, VNum %d. dg_dest (add/remove) <room vnum/ALL> <target>",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig));
        return;
    }
    if (!strcasecmp(dest, "all"))
        all = TRUE;
    else if (((loc = world_vnum[atoi(dest)])) == NULL) {
        script_log("Trigger: %s, VNum %d. dg_dest: invalid target room!",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig));
        return;
    }

    if (!strcmp(addrem, "add"))
        add
            = TRUE;
    else if (!strcmp(addrem, "remove"))
        add
            = FALSE;
    else {
        script_log("Trigger: %s, VNum %d. dg_dest: need to specify add or remove for second argument!",
                   GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig));
        return;
    }

    if (all && add
           ) {
            script_log("Trigger: %s, VNum %d. dg_dest: can only use ALL with remove!",
                       GET_TRIG_NAME(trig), GET_TRIG_VNUM(trig));
            return;
        }
    /* locate target */
    switch (script_type) {
    case MOB_TRIGGER:
        ch = (Character *) go;
        if (!strcmp(name, "self"))
            c = ch;
        else if ((o = get_object_in_equip(ch, name)))
            ;
        else if ((o = get_obj_in_list(name, ch->carrying)))
            ;
        else if ((c = get_char_room(name, NULL, IN_ROOM(ch))))
            ;
        else if ((o = get_obj_in_list(name,IN_ROOM(ch)->contents)))
            ;
        else if ((c = get_char(name)))
            ;
    else if ((o = get_obj(name))) {}

        break;
    case OBJ_TRIGGER:
        obj = (obj_data *) go;
        if (!strcmp(name, "self"))
            o = obj;
        else if ((c = get_char_by_obj(obj, name)))
            ;
    else if ((o = get_obj_by_obj(obj, name))) {}
        break;
    case WLD_TRIGGER:
        room = (Room *) go;
        if ((c = get_char_by_room(room, name)))
            ;
    else if ((o = get_obj_by_room(room, name))) {}
        break;
    }

    if (c) {
        if (add
           )
            add_travel_point_by_pointer(&TRAVEL_LIST(c), loc->number);
        else
            remove_travel_point_by_dest(&TRAVEL_LIST(c), loc->number);
    } else if (o) {
        if (add
           )
            add_travel_point_by_pointer(&TRAVEL_LIST(o), loc->number);
        else
            remove_travel_point_by_dest(&TRAVEL_LIST(o), loc->number);
    }

    return;
}

