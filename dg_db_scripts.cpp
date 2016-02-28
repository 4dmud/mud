/* ************************************************************************
*  File: db.script.c                             Part of Death's Gate MUD *
*                                                                         *
*  Usage: Contains routines to handle db functions for scripts and trigs  *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Death's Gate MUD is based on CircleMUD, Copyright (C) 1993, 94.        *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
*                                                                         *
*  $Author: w4dimenscor $
*  $Date: 2007/08/19 01:06:10 $
*  $Revision: 1.10 $
************************************************************************ */

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "dg_scripts.h"
#include "utils.h"
#include "db.h"
#include "handler.h"
#include "dg_event.h"
#include "comm.h"
#include "constants.h"
#include "oasis.h"




void parse_trigger(FILE *trig_f, int nr, zone_vnum zon) {
    int t[2], k, attach_type;
    char line[256], *cmds, *s, flags[256], errors[MAX_INPUT_LENGTH];
    struct cmdlist_element *cle;
    struct index_data *t_index;
    struct trig_data *trig;

    CREATE(trig, trig_data, 1);
    CREATE(t_index, index_data, 1);

    t_index->vnum = nr;
    t_index->number = 0;
    t_index->func = NULL;
    t_index->proto = trig;

    snprintf(errors, sizeof(errors), "trig vnum %d", nr);

    trig->nr = top_of_trigt;
    if ((trig->name = fread_string(trig_f, errors)) == NULL) {
        log("SYSERR: %s has no name in file!", errors);
        exit(1);
    }

    get_line(trig_f, line);
    k = sscanf(line, "%d %s %d", &attach_type, flags, t);
    trig->attach_type = (sbyte)attach_type;
    trig->trigger_type = (long)asciiflag_conv(flags);
    trig->narg = (k == 3) ? t[0] : 0;

    if ((trig->arglist = fread_string(trig_f, errors)) == NULL) {
        /* no args is fine */
    }

    if ((cmds = s = fread_string(trig_f, errors)) == NULL) {
        log("SYSERR: %s has no commands in file!", errors);
        exit(1);
    }

    CREATE(trig->cmdlist, struct cmdlist_element, 1);
    trig->cmdlist->cmd = strdup(strtok(s, "\n\r"));
    cle = trig->cmdlist;

    while ((s = strtok(NULL, "\n\r"))) {
        CREATE(cle->next, struct cmdlist_element, 1);
        cle = cle->next;
        cle->cmd = strdup(s);
    }

    free(cmds);

    trig_index[top_of_trigt++] = t_index;
}



/*
 * create a new trigger from a prototype.
 * nr is the real number of the trigger.
 */
trig_data *read_trigger(unsigned int nr) {
    index_data *t_index;
    trig_data *trig;

    if (nr >= top_of_trigt)
        return NULL;
    if ((t_index = trig_index[nr]) == NULL)
        return NULL;

    CREATE(trig, trig_data, 1);
    trig_data_copy(trig, t_index->proto);

    t_index->number++;

    return trig;
}



void trig_data_init(trig_data * this_data) {
    this_data->nr = NOTHING;
    this_data->data_type = 0;
    this_data->name = NULL;
    this_data->trigger_type = 0;
    this_data->cmdlist = NULL;
    this_data->curr_state = NULL;
    this_data->narg = 0;
    this_data->arglist = NULL;
    this_data->depth = 0;
    this_data->wait_event = NULL;
    this_data->remove_me = FALSE;
    this_data->var_list = NULL;

    this_data->next = NULL;
}


void trig_data_copy(trig_data * this_data, const trig_data * trg) {
    if (this_data == trg) {
        log("SYSERR: trig_data_copy passed two of the same pointers!");
        return;
    }
    trig_data_init(this_data);

    this_data->nr = trg->nr;
    this_data->attach_type = trg->attach_type;
    this_data->data_type = trg->data_type;
    if (trg->name)
        this_data->name = str_dup(trg->name);
    else {
        this_data->name = strdup("unnamed trigger");
        log("Trigger with no name! (%d)", trg->nr);
    }
    this_data->trigger_type = trg->trigger_type;
    this_data->cmdlist = trg->cmdlist;
    this_data->narg = trg->narg;
    if (trg->arglist)
        this_data->arglist = str_dup(trg->arglist);
}



/* for mobs and rooms: */
void dg_read_trigger(FILE *fp, void *proto, int type) {
    char line[READ_SIZE];
    char junk[8];
    int vnum, rnum, count;
    Character *mob;
    Room *room;
    //  struct trig_proto_list *trg_proto, *new_trg;

    get_line(fp, line);
    count = sscanf(line,"%7s %d",junk,&vnum);

    if (count != 2) {
        new_mudlog(BRF, LVL_BUILDER, TRUE,
                   "SYSERR: Error assigning trigger! - Line was\n  %s", line);
        return;
    }

    rnum = real_trigger(vnum);
    if (rnum == NOTHING) {
        switch(type) {
        case MOB_TRIGGER:
            new_mudlog(BRF, LVL_BUILDER, TRUE,
                       "SYSERR: dg_read_trigger: Trigger vnum #%d asked for but non-existant! (mob: %s - %d)",
                       vnum, GET_NAME((Character *)proto), GET_MOB_VNUM((Character *)proto));
            break;
        case WLD_TRIGGER:
            new_mudlog(BRF, LVL_BUILDER, TRUE,
                       "SYSERR: dg_read_trigger: Trigger vnum #%d asked for but non-existant! (room:%d)",
                       vnum, GET_ROOM_VNUM( ((Room *)proto) ));
            break;
        default:
            new_mudlog(BRF, LVL_BUILDER, TRUE,
                       "SYSERR: dg_read_trigger: Trigger vnum #%d asked for but non-existant! (?)", vnum);
            break;
        }
        return;
    }

    switch(type) {
    case MOB_TRIGGER:
        mob = (Character *)proto;
        if (mob->proto_script == NULL)
            mob->proto_script = new vector<int>(1, vnum);
        else
            mob->proto_script->push_back(vnum);
        break;
    case WLD_TRIGGER:
        room = (Room *)proto;
        if (room->proto_script == NULL)
            room->proto_script = new vector<int>(1, vnum);
        else
            room->proto_script->push_back(vnum);

        if (room != NULL && rnum != NOTHING) {
            if (!(room->script))
			{
                CREATE(room->script, struct script_data, 1);
                room->script->function_trig = -1;
			}
            add_trigger(SCRIPT(room), read_trigger(rnum), -1);
        } else {
            new_mudlog(BRF, LVL_BUILDER, TRUE,
                       "SYSERR: non-existant trigger #%d assigned to room #%d",
                       vnum, room ? room->number : NOWHERE);
        }
        break;
    default:
        new_mudlog(BRF, LVL_BUILDER, TRUE,
                   "SYSERR: Trigger vnum #%d assigned to non-mob/obj/room", vnum);
    }
}

void dg_obj_trigger(char *line, struct obj_data *obj) {
    char junk[8];
    int vnum, rnum, count;
//    struct trig_proto_list *trg_proto, *new_trg;

    count = sscanf(line,"%s %d",junk,&vnum);

    if (count != 2) {
        new_mudlog(BRF, LVL_BUILDER, TRUE,
                   "SYSERR: dg_obj_trigger() : Error assigning trigger! - Line was:\n  %s", line);
        return;
    }

    rnum = real_trigger(vnum);
    if (rnum==NOTHING) {
        new_mudlog(BRF, LVL_BUILDER, TRUE,
                   "SYSERR: Trigger vnum #%d asked for but non-existant! (Object: %s - %d)",
                   vnum, obj->short_description, GET_OBJ_VNUM(obj));
        return;
    }
    if (obj->proto_script == NULL)
        obj->proto_script = new vector<int>(1, vnum);
    else
        obj->proto_script->push_back(vnum);
}

void assign_triggers(void *i, int type) {
    Character *mob = NULL;
    struct obj_data *obj = NULL;
    Room *room = NULL;
    int rnum;
    int tr = 0;

    switch (type) {
    case MOB_TRIGGER:
        mob = (Character *)i;
        if (!mob->proto_script)
        break;
        for (tr = 0; tr < mob->proto_script->size(); tr++) {
            rnum = real_trigger((*mob->proto_script)[tr]);
            if (rnum==NOTHING) {
                new_mudlog(BRF, LVL_BUILDER, TRUE,
                           "SYSERR: trigger #%d non-existant, for mob #%d",
                           (*mob->proto_script)[tr], mob->vnum);
            } else {
                if (!SCRIPT(mob))
				{
                    CREATE(SCRIPT(mob), struct script_data, 1);
                    SCRIPT(mob)->function_trig = -1;
				}
                add_trigger(SCRIPT(mob), read_trigger(rnum), -1);
            }
        }
        break;
    case OBJ_TRIGGER:
        obj = (obj_data *)i;
        if (!obj->proto_script)
        break;
        for (tr = 0; tr < obj->proto_script->size(); tr++) {
            rnum = real_trigger((*obj->proto_script)[tr]);
            if (rnum==NOTHING) {
                log("SYSERR: trigger #%d non-existant, for obj #%d",
                    (*obj->proto_script)[tr], obj_index[obj->item_number].vnum);
            } else {
                if (!SCRIPT(obj))
				{
                    CREATE(SCRIPT(obj), struct script_data, 1);
                    SCRIPT(obj)->function_trig = -1;
				}
                add_trigger(SCRIPT(obj), read_trigger(rnum), -1);
            }
        }
        break;
    case WLD_TRIGGER:
        room = (Room *)i;
        if (!room->proto_script)
        break;
        for (tr = 0; tr < room->proto_script->size(); tr++) {
            rnum = real_trigger((*room->proto_script)[tr]);
            if (rnum==NOTHING) {
                new_mudlog(BRF, LVL_BUILDER, TRUE,
                           "SYSERR: trigger #%d non-existant, for room #%d",
                           (*room->proto_script)[tr], room->number);
            } else {
                if (!SCRIPT(room)) {
                    CREATE(SCRIPT(room), struct script_data, 1);
                    SCRIPT(room)->trig_list = NULL;
                    SCRIPT(room)->global_vars = NULL;
                    SCRIPT(room)->purged = 0;
                    SCRIPT(room)->types = 0;
                    SCRIPT(room)->context = 0;
                    SCRIPT(room)->next = NULL;
                    SCRIPT(room)->function_trig = -1;
                }
                add_trigger(SCRIPT(room), read_trigger(rnum), -1);
            }
        }
        break;
    default:
        new_mudlog(BRF, LVL_BUILDER, TRUE,
                   "SYSERR: unknown type for assign_triggers()");
        break;
    }
}
