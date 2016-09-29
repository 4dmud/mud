/*
** dg_olc.c: this source file is used in extending Oasis style OLC for
** dg-scripts onto a CircleMUD that already has dg-scripts (as released
** by Mark Heilpern on 1/1/98) implemented.
**
** Parts of this file by Chris Jacobson of _Aliens vs Predator: The MUD_
*/


#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "db.h"
#include "genolc.h"
#include "interpreter.h"
#include "oasis.h"
#include "dg_olc.h"
#include "dg_event.h"
#include "constants.h"
#include "descriptor.h"

/* prototype externally defined functions */
//extern const char *trig_types[], *otrig_types[], *wtrig_types[];
zone_rnum real_zone_by_thing(room_vnum vznum);
/*locals*/
void trigedit_disp_menu(Descriptor *d);
void trigedit_disp_types(Descriptor *d);
void trigedit_save(Descriptor *d);
void trigedit_create_index(int znum, char *type);
void trigedit_string_cleanup(Descriptor *d, int terminator);
int format_script(Descriptor *d);

/* ***********************************************************************
 * trigedit
 * ***********************************************************************/

ACMD(do_oasis_trigedit) {
    int number, real_num;
    Descriptor *d;

    /*
     * Parse any arguments.
     */
    skip_spaces(&argument);
    if (!*argument || !isdigit(*argument)) {
        ch->Send( "Specify a trigger VNUM to edit.\r\n");
        return;
    }

    number = atoi(argument);

    /*
     * Check that it isn't already being edited.
     */
    for (d = descriptor_list; d; d = d->next) {
        if (STATE(d) == CON_TRIGEDIT) {
            if (d->olc && OLC_NUM(d) == number) {
                ch->Send( "That trigger is currently being edited by %s.\r\n",
                          GET_NAME(d->character));
                return;
            }
        }
    }
    d = ch->desc;
    /*
     * Give descriptor an OLC structure.
     */
    if (d->olc) {
        new_mudlog(BRF, LVL_IMMORT, TRUE,
                   "SYSERR: do_oasis_trigedit: Player already had olc structure.");
        free(d->olc);
    }
    CREATE(d->olc, struct oasis_olc_data, 1);

    /*
     * Find the zone.
     */
    if ((OLC_ZNUM(d) = real_zone_by_thing(number)) == NOWHERE) {
        ch->Send( "Sorry, there is no zone for that number!\r\n");
        free(d->olc);
        d->olc = NULL;
        return;
    }

    /*
     * Everyone but IMPLs can only edit zones they have been assigned.
     */
    if (!can_edit_zone(ch, OLC_ZNUM(d))) {
        ch->Send( "You do not have permission to edit this zone.\r\n");
        new_mudlog(BRF, LVL_IMPL, TRUE, "OLC: %s tried to edit zone %d (allowed zone %d)",
                   GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));
        free(d->olc);
        d->olc = NULL;
        return;
    }
    OLC_NUM(d) = number;

    /*
     *  If this is a new trigger, setup a new one,
     *  otherwise, setup the a copy of the existing trigger
     */
    if ((real_num = real_trigger(number)) == NOTHING)
        trigedit_setup_new(d);
    else
        trigedit_setup_existing(d, real_num);

    STATE(d) = CON_TRIGEDIT;

    act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
    SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);

    new_mudlog(BRF, LVL_IMMORT, TRUE,"OLC: %s starts editing zone %d [trigger](allowed zone %d)",
               GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));
}

/* called when a mob or object is being saved to disk, so its script can */
/* be saved */
void script_save_to_disk(FILE *fp, void *item, int type) {
    vector<int> *it;
    int i;

    if (type==MOB_TRIGGER) {
        it = ((Character *)item)->proto_script;
    } else if (type==OBJ_TRIGGER) {
        it = ((struct obj_data *)item)->proto_script;
    } else if (type==WLD_TRIGGER) {
        it = ((Room *)item)->proto_script;
    } else {
        log("SYSERR: Invalid type passed to script_save_mobobj_to_disk()");
        return;
    }
    if (it) {
        for (i = 0;i < it->size();i++) {
            fprintf(fp,"T %d\n", (*it)[i]);
        }
    }
}


void trigedit_setup_new(Descriptor *d) {
    struct trig_data *trig;

    /*
     * Allocate a scratch trigger structure
     */
    CREATE(trig, struct trig_data, 1);

    trig->nr = -1;

    /*
     * Set up some defaults
     */
    trig->name = strdup("new trigger");
    trig->trigger_type = MTRIG_GREET;

    /* cmdlist will be a large char string until the trigger is saved */
    CREATE(OLC_STORAGE(d), char, MAX_CMD_LENGTH);
    strncpy(OLC_STORAGE(d),
            "*My trigger commandlist is not complete!\r\n", MAX_CMD_LENGTH-1);
    trig->narg = 100;

    OLC_TRIG(d) = trig;
    OLC_VAL(d) = 0;  /* Has changed flag. (It hasn't so far, we just made it.) */

    trigedit_disp_menu(d);
}

void trigedit_setup_existing(Descriptor *d, int rtrg_num) {
    struct trig_data *trig;
    struct cmdlist_element *c;
    /*
     * Allocate a scratch trigger structure
     */
    CREATE(trig, struct trig_data, 1);

    trig_data_copy(trig, trig_index[rtrg_num]->proto);

    /* convert cmdlist to a char string */
    c = trig->cmdlist;
    CREATE(OLC_STORAGE(d), char, MAX_CMD_LENGTH);
    strcpy(OLC_STORAGE(d), "");

    while (c) {
        strcat(OLC_STORAGE(d), c->cmd);
        strcat(OLC_STORAGE(d), "\r\n");
        c = c->next;
    }
    /* now trig->cmdlist is something to pass to the text editor */
    /* it will be converted back to a real cmdlist_element list later */

    OLC_TRIG(d) = trig;
    OLC_VAL(d) = 0;  /* Has changed flag. (It hasn't so far, we just made it.) */

    trigedit_disp_menu(d);
}


void trigedit_disp_menu(Descriptor *d) {
    struct trig_data *trig = OLC_TRIG(d);
    string attach_type;
    char trgtypes[256];

    get_char_colours(d->character);

    if (trig->attach_type==OBJ_TRIGGER) {
        attach_type = "Objects";
        new_sprintbit(GET_TRIG_TYPE(trig), otrig_types, trgtypes, sizeof(trgtypes));
    } else if (trig->attach_type==WLD_TRIGGER) {
        attach_type = "Rooms";
        new_sprintbit(GET_TRIG_TYPE(trig), wtrig_types, trgtypes, sizeof(trgtypes));
    } else {
        attach_type = "Mobiles";
        new_sprintbit(GET_TRIG_TYPE(trig), trig_types, trgtypes, sizeof(trgtypes));
    }

    clear_screen(d);

    d->Output ( "{cyDon't forget to turn colourcode on when you're going to copy-paste.{c0\r\n\r\n" );
    d->Output(
        "Trigger Editor [%s%d%s]\r\n\r\n"
        "%s1)%s Name         : %s%s\r\n"
        "%s2)%s Intended for : %s%s\r\n"
        "%s3)%s Trigger types: %s%s\r\n"
        "%s4)%s Numeric Arg  : %s%d\r\n"
        "%s5)%s Arguments    : %s%s\r\n"
        "%s6)%s Commands:\r\n%s%s\r\n"
        "%sQ)%s Quit\r\n"
        "Enter Choice :",

        grn, OLC_NUM(d), nrm, 			/* vnum on the title line */
        grn, nrm, yel, GET_TRIG_NAME(trig),		/* name                   */
        grn, nrm, yel, attach_type.c_str(),			/* attach type            */
        grn, nrm, yel, trgtypes,			/* greet/drop/etc         */
        grn, nrm, yel, trig->narg,			/* numeric arg            */
        grn, nrm, yel, trig->arglist?trig->arglist:"",/* strict arg             */
        grn, nrm, cyn, OLC_STORAGE(d),		/* the command list       */
        grn, nrm);                                    /* quit colours            */

    OLC_MODE(d) = TRIGEDIT_MAIN_MENU;
}

void trigedit_disp_types(Descriptor *d) {
    int i, columns = 0;
    const char **types;
    char bitbuf[MAX_STRING_LENGTH];

    switch(OLC_TRIG(d)->attach_type) {
    case WLD_TRIGGER:
        types = wtrig_types;
        break;
    case OBJ_TRIGGER:
        types = otrig_types;
        break;
    case MOB_TRIGGER:
    default:
        types = trig_types;
        break;
    }

    get_char_colours(d->character);
    clear_screen(d);

    for (i = 0; i < NUM_TRIG_TYPE_FLAGS; i++) {
        d->Output( "%s%2d%s) %-20.20s  %s", grn, i + 1, nrm, types[i],
                   !(++columns % 2) ? "\r\n" : "");
    }
    new_sprintbit(GET_TRIG_TYPE(OLC_TRIG(d)), types, bitbuf, sizeof(bitbuf));
    d->Output( "\r\nCurrent types : %s%s%s\r\nEnter type (0 to quit) : ",
               cyn, bitbuf, nrm);

}

void trigedit_parse(Descriptor *d, char *arg) {
    int i = 0;

    switch (OLC_MODE(d)) {
    case TRIGEDIT_MAIN_MENU:
        switch (tolower(*arg)) {
        case 'q':
            if (OLC_VAL(d)) { /* Anything been changed? */
                if (!GET_TRIG_TYPE(OLC_TRIG(d))) {
                    d->Output( "Invalid Trigger Type! Answer a to abort quit!\r\n");
                }
                d->Output( "Do you wish to save the changes to the trigger? (y/n): ");
                OLC_MODE(d) = TRIGEDIT_CONFIRM_SAVESTRING;
            } else
                cleanup_olc(d, CLEANUP_ALL);
            return;
        case '1':
            OLC_MODE(d) = TRIGEDIT_NAME;
            d->Output( "Name: ");
            break;
        case '2':
            OLC_MODE(d) = TRIGEDIT_INTENDED;
            d->Output( "0: Mobiles, 1: Objects, 2: Rooms: ");
            break;
        case '3':
            OLC_MODE(d) = TRIGEDIT_TYPES;
            trigedit_disp_types(d);
            break;
        case '4':
            OLC_MODE(d) = TRIGEDIT_NARG;
            d->Output( "Numeric argument: ");
            break;
        case '5':
            OLC_MODE(d) = TRIGEDIT_ARGUMENT;
            d->Output( "Argument: ");
            break;
        case '6':
            OLC_MODE(d) = TRIGEDIT_COMMANDS;
            d->Output( "Enter trigger commands: (/s saves /h for help)\r\n\r\n");
            if (d->backstr) {
                free(d->backstr);
                d->backstr = NULL;
            }
            //d->backstr.erase();
            if (OLC_STORAGE(d)) {
                d->Output( "%s", OLC_STORAGE(d));
                d->backstr = strdup(OLC_STORAGE(d));
            }
            d->str = &OLC_STORAGE(d);
            d->max_str = MAX_CMD_LENGTH;
            d->mail_to = 0;
            OLC_VAL(d) = 1;

            break;
        default:
            trigedit_disp_menu(d);
            return;
        }
        return;

    case TRIGEDIT_CONFIRM_SAVESTRING:
        switch(tolower(*arg)) {
        case 'y':
            trigedit_save(d);
            new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(d->character)), TRUE,
                       "OLC: %s edits trigger %d", GET_NAME(d->character),
                       OLC_NUM(d));
            /* fall through */
        case 'n':
            cleanup_olc(d, CLEANUP_ALL);
            return;
        case 'a': /* abort quitting */
            break;
        default:
            d->Output( "Invalid choice!\r\n");
            d->Output( "Do you wish to save the trigger? : ");
            return;
        }
        break;

    case TRIGEDIT_NAME:
        smash_tilde(arg);
        if (OLC_TRIG(d)->name)
            free(OLC_TRIG(d)->name);
        OLC_TRIG(d)->name = strdup((arg && *arg) ? arg : "undefined");
        OLC_VAL(d)++;
        break;

    case TRIGEDIT_INTENDED:
        if ((atoi(arg)>=MOB_TRIGGER) || (atoi(arg)<=WLD_TRIGGER))
            OLC_TRIG(d)->attach_type = atoi(arg);
        OLC_VAL(d)++;
        break;

    case TRIGEDIT_NARG:
        OLC_TRIG(d)->narg = atoi(arg);
        OLC_VAL(d)++;
        break;

    case TRIGEDIT_ARGUMENT:
        smash_tilde(arg);
        OLC_TRIG(d)->arglist = (*arg?strdup(arg):NULL);
        OLC_VAL(d)++;
        break;

    case TRIGEDIT_TYPES:
        if ((i = atoi(arg)) == 0)
            break;
        else if (!((i < 0) || (i > NUM_TRIG_TYPE_FLAGS)))
            TOGGLE_BIT((GET_TRIG_TYPE(OLC_TRIG(d))), 1 << (i - 1));
        OLC_VAL(d)++;
        trigedit_disp_types(d);
        return;

    case TRIGEDIT_COMMANDS:
        break;

    }

    OLC_MODE(d) = TRIGEDIT_MAIN_MENU;
    trigedit_disp_menu(d);
}




/* save the zone's triggers to internal memory and to disk */
void trigedit_save(Descriptor *d) {
    unsigned int i, top;
    trig_rnum rnum;
    int found = 0;
    char *s;
    trig_data *proto;
    trig_data *trig = OLC_TRIG(d);
    trig_data *live_trig;
    struct cmdlist_element *cmd, *next_cmd;
    struct index_data **new_index;

    Descriptor *dsc;
    FILE *trig_file;
    int zone;
    char buf[MAX_CMD_LENGTH];
    char bitBuf[MAX_INPUT_LENGTH];
    char fname[MAX_INPUT_LENGTH];

    if ((rnum = real_trigger(OLC_NUM(d))) != NOTHING) {
        proto = trig_index[rnum]->proto;
        for (cmd = proto->cmdlist; cmd; cmd = next_cmd) {
            next_cmd = cmd->next;
            if (cmd->cmd)
                free(cmd->cmd);
            free(cmd);
        }


        free(proto->arglist);
        free(proto->name);
        proto->arglist = NULL;
        proto->name = NULL;

        /* Recompile the command list from the new script */
        s = OLC_STORAGE(d);

        CREATE(trig->cmdlist, struct cmdlist_element, 1);
        trig->cmdlist->line_nr = 1;
        if (s) {
            char *t = strtok(s, "\r\n"); /* strtok returns NULL if s is "\r\n" */
            if (t)
                trig->cmdlist->cmd = strdup(t);
            else
                trig->cmdlist->cmd = strdup("* No script");
            cmd = trig->cmdlist;
            while ((s = strtok(NULL, "\r\n"))) {
                CREATE(cmd->next, struct cmdlist_element, 1);
                cmd->next->line_nr = cmd->line_nr + 1;
                cmd = cmd->next;
                cmd->cmd = strdup(s);
            }
        } else
            trig->cmdlist->cmd = strdup("* No Script");

        /* make the prototype look like what we have */
        trig_data_copy(proto, trig);

        /* go through the mud and replace existing triggers         */
        live_trig = trigger_list;
        while (live_trig) {
            if (GET_TRIG_RNUM(live_trig) == rnum) {
                if (live_trig->arglist) {
                    free(live_trig->arglist);
                    live_trig->arglist = NULL;
                }
                if (live_trig->name) {
                    free(live_trig->name);
                    live_trig->name = NULL;
                }

                if (proto->arglist)
                    live_trig->arglist = strdup(proto->arglist);
                if (proto->name)
                    live_trig->name = strdup(proto->name);

                /* anything could have happened so we don't want to keep these */
                if (GET_TRIG_WAIT(live_trig)) {
                    event_cancel(GET_TRIG_WAIT(live_trig));
                    GET_TRIG_WAIT(live_trig)=NULL;
                }
                if (live_trig->var_list) {
                    free_varlist(live_trig->var_list);
                    live_trig->var_list=NULL;
                }

                live_trig->cmdlist = proto->cmdlist;
                live_trig->curr_state = live_trig->cmdlist;
                live_trig->curr_state->line_nr = 1;
                live_trig->trigger_type = proto->trigger_type;
                live_trig->attach_type = proto->attach_type;
                live_trig->narg = proto->narg;
                live_trig->data_type = proto->data_type;
                live_trig->depth = 0;
            }

            live_trig = live_trig->next_in_world;
        }
    } else {
        /* this is a new trigger */
        CREATE(new_index, struct index_data *, top_of_trigt + 2);

        /* Recompile the command list from the new script */

        s = OLC_STORAGE(d);

        CREATE(trig->cmdlist, struct cmdlist_element, 1);
        trig->cmdlist->line_nr = 1;
        if (s) {
            /* strtok returns NULL if s is "\r\n" */
            char *t = strtok(s, "\r\n");
            trig->cmdlist->cmd = strdup(t ? t : "* No script");
            cmd = trig->cmdlist;

            while ((s = strtok(NULL, "\r\n"))) {
                CREATE(cmd->next, struct cmdlist_element, 1);
                cmd->next->line_nr = cmd->line_nr + 1;
                cmd = cmd->next;
                cmd->cmd = strdup(s);
            }
        } else
            trig->cmdlist->cmd = strdup("* No Script");

        for (i = 0; i < top_of_trigt; i++) {
            if (!found) {
                if (trig_index[i]->vnum > OLC_NUM(d)) {
                    found = TRUE;
                    rnum = i;

                    CREATE(new_index[rnum], struct index_data, 1);
                    GET_TRIG_RNUM(OLC_TRIG(d)) = rnum;
                    new_index[rnum]->vnum = OLC_NUM(d);
                    new_index[rnum]->number = 0;
                    new_index[rnum]->func = NULL;
                    CREATE(proto, struct trig_data, 1);
                    new_index[rnum]->proto = proto;
                    trig_data_copy(proto, trig);

                    if (trig->name)
                        proto->name = strdup(trig->name);
                    if (trig->arglist)
                        proto->arglist = strdup(trig->arglist);

                    new_index[rnum + 1] = trig_index[rnum];

                    proto = trig_index[rnum]->proto;
                    proto->nr = rnum + 1;
                } else {
                    new_index[i] = trig_index[i];
                }
            } else {
                new_index[i + 1] = trig_index[i];
                proto = trig_index[i]->proto;
                proto->nr = i + 1;
            }
        }

        if (!found) {
            rnum = i;
            CREATE(new_index[rnum], struct index_data, 1);
            GET_TRIG_RNUM(OLC_TRIG(d)) = rnum;
            new_index[rnum]->vnum = OLC_NUM(d);
            new_index[rnum]->number = 0;
            new_index[rnum]->func = NULL;

            CREATE(proto, struct trig_data, 1);
            new_index[rnum]->proto = proto;
            trig_data_copy(proto, trig);

            if (trig->name)
                proto->name = strdup(trig->name);
            if (trig->arglist)
                proto->arglist = strdup(trig->arglist);
        }

        free(trig_index);

        trig_index = new_index;
        top_of_trigt++;

        /* HERE IT HAS TO GO THROUGH AND FIX ALL SCRIPTS/TRIGS OF HIGHER RNUM */
        for (live_trig = trigger_list; live_trig; live_trig = live_trig->next_in_world)
            GET_TRIG_RNUM(live_trig) += (GET_TRIG_RNUM(live_trig) > rnum);

        /*
         * Update other trigs being edited.
         */
        for (dsc = descriptor_list; dsc; dsc = dsc->next)
            if (STATE(dsc) == CON_TRIGEDIT)
                if (GET_TRIG_RNUM(OLC_TRIG(dsc)) >= rnum)
                    GET_TRIG_RNUM(OLC_TRIG(dsc))++;

    }

    /* now write the trigger out to disk, along with the rest of the  */
    /* triggers for this zone, of course                              */
    /* note: we write this to disk NOW instead of letting the builder */
    /* have control because if we lose this after having assigned a   */
    /* new trigger to an item, we will get SYSERR's upton reboot that */
    /* could make things hard to debug.                               */

    zone = zone_table[OLC_ZNUM(d)].number;
    top = zone_table[OLC_ZNUM(d)].top;

#ifdef CIRCLE_MAC

    snprintf(fname, sizeof(fname), "%s:%i:%i.new", LIB_WORLD,zone, zone);
#else

    snprintf(fname, sizeof(fname), "%s%i/%i.new", LIB_WORLD,zone, zone);
#endif

    if (!(trig_file = fopen(fname, "w"))) {
        new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(d->character)), TRUE,
                   "SYSERR: OLC: Can't open trig file \"%s\"", fname);
        return;
    }

    for (i = zone_table[OLC_ZNUM(d)].bot; i <= top; i++) {
        if ((rnum = real_trigger(i)) != NOTHING) {
            trig = trig_index[rnum]->proto;

            if (fprintf(trig_file, "#%d\n", i) < 0) {
                new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(d->character)), TRUE,
                           "SYSERR: OLC: Can't write trig file!");
                fclose(trig_file);
                return;
            }
            sprintbits(GET_TRIG_TYPE(trig), bitBuf);
            fprintf(trig_file,      "%s%c\n"
                    "%d %s %d\n"
                    "%s%c\n",
                    (GET_TRIG_NAME(trig)) ? (GET_TRIG_NAME(trig)) : "unknown trigger", STRING_TERMINATOR,
                    trig->attach_type,
                    *bitBuf ? bitBuf : "0", GET_TRIG_NARG(trig),
                    GET_TRIG_ARG(trig) ? GET_TRIG_ARG(trig) : "", STRING_TERMINATOR);

            /* Build the text for the script */
            strcpy(buf,""); /* strcpy OK for MAX_CMD_LENGTH > 0*/
            for (cmd = trig->cmdlist; cmd; cmd = cmd->next) {
                strcat(buf, cmd->cmd);
                strcat(buf, "\n");
            }

            if (!buf[0])
                strcpy(buf, "* Empty script");

            fprintf(trig_file, "%s%c\n", buf, STRING_TERMINATOR);
            *buf = '\0';
        }
    }

    fprintf(trig_file, "$%c\n", STRING_TERMINATOR);
    fclose(trig_file);

#ifdef CIRCLE_MAC

    snprintf(buf, sizeof(buf), "%s:%i:%i.trg", LIB_WORLD,zone, zone);
#else

    snprintf(buf, sizeof(buf), "%s%i/%i.trg", LIB_WORLD,zone, zone);
#endif

    remove
        (buf);
    rename(fname, buf);

}

void dg_olc_script_copy(Descriptor *d) {

    if (OLC_SCRIPT(d))
       delete OLC_SCRIPT(d);
       OLC_SCRIPT(d) = NULL;

    if (OLC_ITEM_TYPE(d)==MOB_TRIGGER) {
        if (OLC_MOB(d)->proto_script)
            OLC_SCRIPT(d) = new vector<int>(OLC_MOB(d)->proto_script->begin(), OLC_MOB(d)->proto_script->end());
    } else if (OLC_ITEM_TYPE(d)==OBJ_TRIGGER) {
        if (OLC_OBJ(d)->proto_script)
            OLC_SCRIPT(d) = new vector<int>(OLC_OBJ(d)->proto_script->begin(), OLC_OBJ(d)->proto_script->end());
    } else {
        if (OLC_ROOM(d)->proto_script)
            OLC_SCRIPT(d) = new vector<int>(OLC_ROOM(d)->proto_script->begin(), OLC_ROOM(d)->proto_script->end());
    }
}

void dg_script_menu(Descriptor *d) {
    int i = 0;

    /* make sure our input parser gets used */
    OLC_MODE(d) = OLC_SCRIPT_EDIT;
    OLC_SCRIPT_EDIT_MODE(d) = SCRIPT_MAIN_MENU;

    clear_screen(d);
    d->Output( "     Script Editor\r\n\r\n     Trigger List:\r\n");

    if (OLC_SCRIPT(d)) {
        for (i = 0; i < OLC_SCRIPT(d)->size();i++) {
            d->Output( "     %2d) [%s%d%s] %s%s%s", i, cyn,
                       (*OLC_SCRIPT(d))[i], nrm, cyn,
                       trig_index[real_trigger((*OLC_SCRIPT(d))[i])]->proto->name, nrm);
            if (trig_index[real_trigger((*OLC_SCRIPT(d))[i])]->proto->attach_type !=
                    OLC_ITEM_TYPE(d))
                d->Output( "   %s** Mis-matched Trigger Type **%s\r\n",grn,nrm);
            else
                d->Output( "\r\n");

        }
    }
    if (i==0)
        d->Output( "     <none>\r\n");

    d->Output(  "\r\n"
                " %sN%s)  New trigger for this script\r\n"
                " %sD%s)  Delete a trigger in this script\r\n"
                " %sX%s)  Exit Script Editor\r\n\r\n"
                "     Enter choice :",
                grn, nrm, grn, nrm, grn, nrm);
}

int dg_script_edit_parse(Descriptor *d, char *arg) {
    //  struct trig_proto_list *trig, *currtrig;
    int count, pos, vnum;

    switch(OLC_SCRIPT_EDIT_MODE(d)) {
    case SCRIPT_MAIN_MENU:
        switch(tolower(*arg)) {
        case 'x':
            /* this was buggy.
               First we created a copy of a thing, but maintained pointers to scripts,
               then if we altered the scripts, we freed the pointers and added new ones
               to the OLC_THING. If we then chose _NOT_ to save the changes, the
               pointers in the original thing pointed to garbage. If we saved changes
               the pointers were updated correctly.

               Solution:
               Here we just point the working copies to the new proto_scripts
               We only update the original when choosing to save internally,
               then free the unused memory there.

               Welcor

               Thanks to
               Jeremy Stanley - fungi@yuggoth.org and
               Torgny Bjers - artovil@arcanerealms.org
               for the bug report.

               After updating to OasisOLC 2.0.3 I discovered some malfunctions
               in this code, so I restructured it a bit. Now things work like this:
               OLC_SCRIPT(d) is assigned a copy of the edited things' proto_script.
               OLC_OBJ(d), etc.. are initalized with proto_script = NULL;
               On save, the saved copy is updated with OLC_SCRIPT(d) as new proto_script (freeing the old one).
               On quit/nosave, OLC_SCRIPT is free()'d, and the prototype not touched.

            */
            return 0;
        case 'n':
            d->Output( "\r\nPlease enter position, vnum   (ex: 1, 200):");
            OLC_SCRIPT_EDIT_MODE(d) = SCRIPT_NEW_TRIGGER;
            break;
        case 'd':
            d->Output( "     Which entry should be deleted?  -1 to abort :");
            OLC_SCRIPT_EDIT_MODE(d) = SCRIPT_DEL_TRIGGER;
            break;
        default:
            dg_script_menu(d);
            break;
        }
        return 1;

    case SCRIPT_NEW_TRIGGER:
        vnum = -1;
        pos  = -1;
        count = sscanf(arg,"%d, %d",&pos,&vnum);
        if (count==1) {
            vnum = pos;
            pos = 999;
        }

        if (pos<0)
            break; /* this aborts a new trigger entry */
        if (!OLC_SCRIPT(d))
            pos = 0;
        else if (pos > OLC_SCRIPT(d)->size())
            pos = OLC_SCRIPT(d)->size();

        if (vnum==0)
            break; /* this aborts a new trigger entry */

        if (real_trigger(vnum) == NOTHING) {
            d->Output( "Invalid Trigger VNUM!\r\n"
                       "Please enter position, vnum   (ex: 1, 200):");
            return 1;
        }

        /* add the new info in position */
        if (!OLC_SCRIPT(d))
            OLC_SCRIPT(d) = new vector<int>(1, vnum);
        else if (pos == OLC_SCRIPT(d)->size())
            OLC_SCRIPT(d)->push_back(vnum);
        else
            OLC_SCRIPT(d)->insert(OLC_SCRIPT(d)->begin() + pos, vnum);
        OLC_VAL(d)++;
        break;

    case SCRIPT_DEL_TRIGGER:
        if (!is_number(arg))
            break;
        pos = atoi(arg);
        if (pos<0)
            break;
        if (!OLC_SCRIPT(d))
            break;
        if (pos>=OLC_SCRIPT(d)->size())
            break;
        OLC_SCRIPT(d)->erase(OLC_SCRIPT(d)->begin() + pos);
        if (OLC_SCRIPT(d)->size() == 0) {
            delete OLC_SCRIPT(d);
            OLC_SCRIPT(d) = NULL;
        }
        OLC_VAL(d)++;
        break;
    }

    dg_script_menu(d);
    return 1;
}

void trigedit_string_cleanup(Descriptor *d, int terminator) {
    switch (OLC_MODE(d)) {
    case TRIGEDIT_COMMANDS:
        trigedit_disp_menu(d);
        break;
    }
}


#if 0 /* change to 1 if you get messages telling you you don't have strncasecmp() */
int strncasecmp (const char *s1, const char *s2, int n) {
    unsigned char c1, c2;
    while(*s1 && *s2 && n--) {
        c1 = ((*s1 >= 'A') && (*s1 <= 'Z')) ? (*s1++) + ('a' - 'A') : (*s1++);
        c2 = ((*s2 >= 'A') && (*s2 <= 'Z')) ? (*s2++) + ('a' - 'A') : (*s2++);
        if (c1 != c2)
            return (c1 > c2) ? 1 : -1;
    }
    if (*s1 && !*s2)
        return 1;
    if (!*s1 && *s2)
        return -1;
    return 0;
}
#endif
/*TODO: donate this bracket code to welcor */
/**
returns negitive value if there are too many right brackets.
a positive number if too many left brackets.
and 0 if balanced.
Mordecai - 4dimensions.org:6000
Sun Feb 6 2005
**/
int check_braces(char *str) {
    int parens = 0;
    char *p;
    for (p = str;*p;p++) {
        if ('(' == *p)
            parens++;
        else if (')' == *p)
            parens--;
    }
    return parens;
}

int format_script(Descriptor *d) {
    char nsc[MAX_CMD_LENGTH], *t, line[READ_SIZE];
    char *sc;
    size_t len = 0, nlen = 0, llen = 0, brac = 0;
    int indent = 0, i, line_num = 0;
    bool indent_next = FALSE, found_case = FALSE, found_while = FALSE, first_case = FALSE, in_switch = FALSE;

    if (!d->str || !*d->str)
        return FALSE;

    sc = strdup(*d->str); /* we work on a copy, because of strtok() */
    t = strtok(sc, "\r\n");
    *nsc = '\0';

    while (t) {
        line_num++;
        skip_spaces(&t);
        if (( (brac = check_braces(t)) != 0) &&(!strn_cmp("elseif ", t, 7) || !strn_cmp("else", t, 4) || !strn_cmp("else if ", t, 8) || !strn_cmp(t, "if ", 3)
                                                || !strn_cmp("while ", t, 6) || !strn_cmp("switch ", t, 7) || !strn_cmp( "extract ",t, 8)
                                                || !strn_cmp("case ", t, 5) || !strn_cmp( "eval ",t, 5) || !strn_cmp( "nop ",t, 4) || !strn_cmp( "set ",t, 4))) {
            d->Output( "Unmatched %s bracket (line %d)!\r\n", brac < 0 ? "right" : "left", line_num);
            free(sc);
            return FALSE;
        }
        if (!strncasecmp(t, "if ", 3))
            indent_next = TRUE;
        else if (!strncasecmp(t, "switch ", 7)) {
            indent_next = TRUE;
            first_case = TRUE;
            in_switch = TRUE;
        } else if (!strncasecmp(t, "while ", 6)) {
            found_while = TRUE;
            indent_next = TRUE;
        } else if (!strncasecmp(t, "end", 3) ||
                   !strncasecmp(t, "done", 4)) {
            if (!indent) {
                d->Output( "Unmatched 'end' or 'done' (line %d)!\r\n", line_num);
                free(sc);
                return FALSE;
            }
            indent--;
            indent_next = FALSE;
            if (in_switch) {
                in_switch = FALSE;
                indent--;
            }
        } else if (!strncasecmp(t, "else", 4)) {
            if (!indent) {
                d->Output( "Unmatched 'else' (line %d)!\r\n", line_num);
                free(sc);
                return FALSE;
            }
            indent--;
            indent_next = TRUE;
        } else if (!strncasecmp(t, "case", 4) ||
                   !strncasecmp(t, "default", 7)) {
            if (!indent) {
                d->Output( "Case/default outside switch (line %d)!\r\n", line_num);
                free(sc);
                return FALSE;
            }
            indent_next = TRUE;
            found_case = TRUE;
            if (first_case)
                first_case = FALSE;
            else
                indent--;
        } else if (!strncasecmp(t, "break", 5)) {
            if ((!found_case && !found_while) || !indent ) {
                d->Output( "Break not in case or while (line %d)!\r\n", line_num);
                free(sc);
                return FALSE;
            }
            found_case = FALSE;
            found_while = FALSE;
        }

        *line = '\0';
        for (nlen = 0, i = 0;i<indent;i++) {
            strncat(line, "  ", sizeof(line)-1);
            nlen += 2;
        }
        llen = snprintf(line + nlen, sizeof(line) - nlen, "%s\r\n", t);
        if (llen < 0 || llen + nlen + len > d->max_str - 1 ) {
            d->Output( "String too long, formatting aborted\r\n");
            free(sc);
            return FALSE;
        }
        len = len + nlen + llen;
        strcat(nsc, line);  /* strcat OK, size checked above */

        if (indent_next) {
            indent++;
            indent_next = FALSE;
        }
        t = strtok(NULL, "\r\n");
    }

    if (indent)
        d->Output( "Unmatched if, while or switch ignored.\r\n");

    free(*d->str);
    *d->str = strdup(nsc);
    free(sc);

    return TRUE;
}
