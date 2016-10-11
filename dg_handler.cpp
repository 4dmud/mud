#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "dg_scripts.h"
#include "utils.h"
#include "comm.h"
#include "db.h"
#include "handler.h"
#include "spells.h"
#include "dg_event.h"
#include "constants.h"

void free_cmdlist ( trig_data *trig );

/* release memory allocated for a variable list */
void free_varlist(struct trig_var_data *vd) {
    struct trig_var_data *i, *j;

    for (i = vd; i;) {
        j = i;
        i = i->next;
        delete j;
    }
}

/*
 * remove var name from var_list
 * returns 1 if found, else 0
 */
int remove_var(struct trig_var_data **var_list,const char *name) {
    struct trig_var_data *i, *j;

    for (j = NULL, i = *var_list; i && i->name.compare(name);
            j = i, i = i->next)
        ;

    if (i) {
        if (j) {
            j->next = i->next;
            delete i;
        } else {
            *var_list = i->next;
            delete i;
        }

        return 1;
    }

    return 0;
}

/*
 * Return memory used by a trigger
 * The command list is free'd when changed and when
 * shutting down, unless it's flagged to be updated.
 */
void free_trigger(struct trig_data *trig) {
    if (trig->name) {
        free(trig->name);
        trig->name = NULL;
    }
    if (trig->arglist) {
        free(trig->arglist);
        trig->arglist = NULL;
    }
    if (trig->var_list) {
        free_varlist(trig->var_list);
        trig->var_list = NULL;
    }
    if (GET_TRIG_WAIT(trig))
        event_cancel(GET_TRIG_WAIT(trig));
    GET_TRIG_WAIT(trig) = NULL;

    if (trig->update_me) // cmdlist is different from proto
        free_cmdlist (trig);

    free(trig);
}


/* remove a single trigger from a mob/obj/room */
void extract_trigger(struct trig_data *trig) {
    struct trig_data *temp;

    trig_index[trig->nr]->number--;

    /* walk the trigger list and remove this one */
    REMOVE_FROM_LIST(trig, trigger_list, next_in_world);

    free_trigger(trig);
}

/* remove all triggers from a mob/obj/room */
void extract_script(void *thing, int type) {
    struct script_data *sc = NULL;
    struct trig_data *trig, *next_trig;
    Character *mob;
    obj_data *obj;
    Room *room;

    switch (type) {
    case MOB_TRIGGER:
        mob = (Character *)thing;
        sc = SCRIPT(mob);
        SCRIPT(mob) = NULL;
        break;
    case OBJ_TRIGGER:
        obj = (struct obj_data *)thing;
        sc = SCRIPT(obj);
        SCRIPT(obj) = NULL;
        break;
    case WLD_TRIGGER:
        room = (Room *)thing;
        sc = SCRIPT(room);
        SCRIPT(room) = NULL;
        break;
    }

#if 0 /* debugging */
    {
        Character *i = character_list;
        struct obj_data *j = object_list;
        room_rnum k;
        if (sc) {
            for ( ; i ; i = i->next)
                assert(sc != SCRIPT(i));

            for ( ; j ; j = j->next)
                assert(sc != SCRIPT(j));

            for (k = 0; k < top_of_world; k++)
                assert(sc != SCRIPT(&world[k]));
        }
    }
#endif
    if (sc == NULL)
        return;

    for (trig = TRIGGERS(sc); trig; trig = next_trig) {
        next_trig = trig->next;
        extract_trigger(trig);
    }
    TRIGGERS(sc) = NULL;
    /* Thanks to James Long for tracking down this memory leak */
    free_varlist(sc->global_vars);

    free(sc);
}

/* erase the script memory of a mob */
void extract_script_mem(struct script_memory *sc) {
    struct script_memory *next;
    while (sc) {
        next = sc->next;
        if (sc->cmd)
            free(sc->cmd);
        free(sc);
        sc = next;
    }
}

void free_proto_script(void *thing, int type) {
    vector<int> *proto = NULL;
    Character *mob;
    obj_data *obj;
    Room *room;

    switch (type) {
    case MOB_TRIGGER:
        mob = (Character *) thing;
        if (mob->proto_script == NULL)
            return;
        proto = mob->proto_script;
        mob->proto_script = NULL;
        break;
    case OBJ_TRIGGER:
        obj = (struct obj_data *) thing;
        if (obj->proto_script == NULL)
            return;
        if (!obj->proto_script)
            return;
        proto = obj->proto_script;
        obj->proto_script = NULL;
        break;
    case WLD_TRIGGER:
        room = (Room *) thing;
        if (room->proto_script == NULL)
            return;
        proto = room->proto_script;
        room->proto_script = NULL;
        break;
    default:
        return;
    }

    if (proto != NULL)
        delete proto;
    proto=NULL;
}

/* perhaps not the best place for this, but I didn't want a new file */
char *sub_percent(Character *ch, char *sub) {
    static char retval[16];
    int subnum;

    subnum = sub_number(sub);
    if (subnum <= 0)
        return (char *)"unknown subskill";

    snprintf(retval, sizeof(retval), "%d", GET_SUB(ch, subnum));
    return retval;
}
/* perhaps not the best place for this, but I didn't want a new file */
char *skill_percent(Character *ch, char *skill) {
    static char retval[16];
    int skillnum;

    skillnum = find_skill_num(skill);
    if (skillnum <= 0)
        return ((char *)"unknown skill");

    if (!knows_spell(ch, skillnum))
        return (char *)"0";


    snprintf(retval, sizeof(retval), "%d", total_chance(ch, skillnum));
    return retval;
}


void copy_proto_script(void *source, void *dest, int type) {
    vector<int> *tp_src = NULL;

    switch (type) {
    case MOB_TRIGGER:
        tp_src = ((Character *)source)->proto_script;
        if (((Character *)dest)->proto_script &&
                MobProtoExists(((Character *)dest)->vnum) &&
                GetMobProto(((Character *)dest)->vnum)->proto_script != ((Character *)dest)->proto_script) {
            delete ((Character *)dest)->proto_script;
            ((Character *)dest)->proto_script = NULL;
        }
        break;
    case OBJ_TRIGGER:
        tp_src = ((obj_data *)source)->proto_script;
        if (((obj_data *)dest)->proto_script &&
                obj_proto[GET_OBJ_RNUM(((obj_data *)dest))].proto_script != ((obj_data *)dest)->proto_script) {
            delete ((obj_data *)dest)->proto_script;
            ((obj_data *)dest)->proto_script = NULL;
        }
        break;
    case WLD_TRIGGER:
        tp_src = ((Room *)source)->proto_script;
        if (((Room *)dest)->proto_script &&
                GET_ROOM_VNUM(((Room *)dest)) != NOWHERE &&
                world_vnum[GET_ROOM_VNUM((Room *)dest)] &&
                world_vnum[GET_ROOM_VNUM((Room *)dest)]->proto_script != ((Room *)dest)->proto_script) {
            delete ((Room *)dest)->proto_script;
            ((Room *)dest)->proto_script = NULL;
        }
        break;
    }

    if (tp_src) {
        switch (type) {
        case MOB_TRIGGER:
            ((Character *)dest)->proto_script = new vector<int>(tp_src->begin(), tp_src->end());
            break;
        case OBJ_TRIGGER:
            ((obj_data *)dest)->proto_script = new vector<int>(tp_src->begin(), tp_src->end());
            break;
        case WLD_TRIGGER:
            ((Room *)dest)->proto_script = new vector<int>(tp_src->begin(), tp_src->end());
            break;
        }
    }
}


void delete_variables(const char *charname) {
    char filename[PATH_MAX];

    if (!get_filename(charname, filename , SCRIPT_VARS_FILE))
        return;

    if (remove
            (filename) < 0 && errno != ENOENT)
        log("SYSERR: deleting variable file %s: %s", filename, strerror(errno));
}

void update_wait_events(Room *to, Room *from) {
    struct trig_data *trig;

    if (!SCRIPT(from))
        return;

    for (trig = TRIGGERS(SCRIPT(from)); trig; trig = trig->next) {
        if (!GET_TRIG_WAIT(trig))
            continue;

        ((struct wait_event_data *)GET_TRIG_WAIT(trig)->event_obj)->go = to;
    }
}


