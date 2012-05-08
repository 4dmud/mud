/***********************************************************************
*  File: corpse.c Version 1.1                                          *
*  Usage: Corpse Saving over reboots/crashes/copyovers                 *
*                                                                      *
*  By: Michael Cunningham (Romulus) IMP of Legends of The Phoenix MUD  *
*  Permission is granted to use and modify this software as long as    *
*  credits are given in the credits command in the game.               *
*  Built for circle30bpl15                                             *
*  Bunch of code reused from the XAP object patch by Patrick Dughi     *
*                                                  <dughi@IMAXX.NET>   *
*  Thankyou Patrick!                                                   *
*  Some functions have been renamed to protect the innocent            *
*  All Rights Reserved, Copyright (C) 1999                             *
***********************************************************************/

/* The standard includes */
#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "comm.h"
#include "handler.h"
#include "db.h"
#include "interpreter.h"
#include "utils.h"
#include "spells.h"
#include "graph.h"
#include "descriptor.h"

/* Set this define to wherever you want to save your corpses */
#define CORPSE_FILE LIB_MISC"corpse.save"
#define NEW_CORPSE_FILE LIB_MISC"new_corpse.save"
struct corpse_list_data *corpse_list = NULL;
extern const char *dirs[];

/* External Structures / Variables */
extern room_vnum frozen_start_room;
extern int save_new_style;

/* External Functions */
int find_first_step(room_rnum src, room_rnum target,bool honour_notrack=false);
int get_pidx_from_name(Character *ch);
void obj_from_room(struct obj_data *obj);
void obj_to_room(struct obj_data *object, room_rnum room);
void obj_to_obj(struct obj_data *obj, struct obj_data *obj_to);
int save_one_item( OBJ_DATA *obj,FILE *fl, int locate);
int new_write_corpse(FILE * fp, struct obj_data *obj, int locate);
void new_load_corpses(void);
void check_timer(obj_data *obj);
int automeld(obj_data *corpse);
/* Local Function Declerations */
void do_show_corpses(Character *ch);
void remove_corpse_from_list(OBJ_DATA *corpse);
int old_write_corpse(FILE * fp, struct obj_data *obj, int locate);
void save_corpses(void);
void load_corpses(void);
int corpse_save(struct obj_data *obj, FILE * fp, int location,
                bool recurse_this_tree);
int write_corpse_to_disk(FILE * fp, struct obj_data *obj, int locate);
void clean_string(char *buffer);

/* Tada! THE FUNCTIONS ! Yaaa! */

void clean_string(char *buffer) {
    register char *ptr, *str;

    ptr = buffer;
    str = ptr;

    while ((*str = *ptr)) {
        str++;
        ptr++;
        if (*ptr == '\r')
            ptr++;
    }
}

void add_corpse_to_list(OBJ_DATA *corpse);
void remove_corpse_from_list(OBJ_DATA *corpse);

obj_data* find_corpse(Character* ch) {
	for (struct corpse_list_data* ce = corpse_list;ce != NULL;ce = ce->next) 
		if (GET_OBJ_VAL(ce->corpse, 0) == get_pidx_from_name(ch))
			return ce->corpse;

	return NULL;
}

int corpse_save(struct obj_data *obj, FILE * fp, int location,
                bool recurse_this_tree) {
    /* This function basically is responsible for taking the    */
    /* supplied obj and figuring out if it has any contents. If */
    /* it does then we write those to disk.. Ad Nasum.          */

    //struct obj_data *tmp;
    int result;

    if (obj) {			/* a little recursion (can be a dangerous thing:) */

        /* recurse_this_tree causes the recursion to branch only
           down the corpses content's tree and not the contents of the
           room. obj->next_content points to the rooms contents
           the first time this function is called from save_corpses
           hence we avoid going down there otherwise we will save
           the rooms contents as well as the corpses contents in the 
           corpse.save file. 
         */

        if (recurse_this_tree != FALSE) {
            corpse_save(obj->next_content, fp, location,
                        recurse_this_tree);
        }
        recurse_this_tree = TRUE;
        corpse_save(obj->contains, fp, MIN(0, location) - 1,
                    recurse_this_tree);
        result = write_corpse_to_disk(fp, obj, location);

        /* readjust the wieght while we do this */
        /*for (tmp = obj->in_obj; tmp; tmp = tmp->in_obj)
            GET_OBJ_WEIGHT(tmp) -= GET_OBJ_WEIGHT(obj);*/

        if (!result)
            return (0);
    }
    return (TRUE);
}


int write_corpse_to_disk(FILE * fp, struct obj_data *obj, int locate) {
    if (save_new_style)
        return save_one_item(obj, fp, locate);
    else
        return old_write_corpse(fp, obj, locate);
}



int old_write_corpse(FILE * fp, struct obj_data *obj, int locate) {
    /* This is basically Patrick's my_obj_save_to_disk function with    */
    /* a few minor tweaks to make it work for corpses. Basically it     */
    /* writes one object out to the corpse file every time it is called. */
    /* It can handle regular obj's and XAP objects.                     */

    int counter;
    struct extra_descr_data *ex_desc;
    char buf1[MAX_STRING_LENGTH + 1];

    if (obj->action_description) {
        strcpy(buf1, obj->action_description);
        clean_string(buf1);
    } else
        *buf1 = 0;
    fprintf(fp,
            "#%d\n"
            "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
            GET_OBJ_VNUM(obj),
            locate,
            GET_OBJ_VAL(obj, 0),
            GET_OBJ_VAL(obj, 1),
            GET_OBJ_VAL(obj, 2),
            GET_OBJ_VAL(obj, 3),
            GET_OBJ_VAL(obj, 4),
            GET_OBJ_VAL(obj, 5),
            GET_OBJ_VAL(obj, 6),
            GET_OBJ_VAL(obj, 7),
            GET_OBJ_VAL(obj, 8),
            GET_OBJ_VAL(obj, 9),
            GET_OBJ_EXTRA(obj)[0],
            GET_OBJ_EXTRA(obj)[1],
            GET_OBJ_EXTRA(obj)[2],
            GET_OBJ_EXTRA(obj)[3], GET_OBJ_TIMER(obj), GET_OBJ_INNATE(obj), GET_OBJ_VROOM(obj));



    if (!(IS_OBJ_STAT(obj, ITEM_UNIQUE_SAVE))) {
        return 1;
    }
    fprintf(fp,
            "XAP\n"
            "%s~\n"
            "%s~\n"
            "%s~\n"
            "%s~\n"
            "%d %d %d %d %d %d %d %d\n",
            obj->name ? obj->name : "undefined",
            obj->short_description ? obj->short_description : "undefined",
            obj->description ? obj->description : "undefined",
            buf1,
            GET_OBJ_TYPE(obj),
            GET_OBJ_WEAR(obj)[0],
            GET_OBJ_WEAR(obj)[1],
            GET_OBJ_WEAR(obj)[2],
            GET_OBJ_WEAR(obj)[3],
            GET_OBJ_WEIGHT(obj), GET_OBJ_COST(obj), GET_OBJ_RENT(obj)
           );
    /* Do we have affects? */
    for (counter = 0; counter < MAX_OBJ_AFFECT; counter++)
        if (obj->affected[counter].modifier)
            fprintf(fp, "A\n"
                    "%d %d\n",
                    obj->affected[counter].location,
                    obj->affected[counter].modifier);

    /* Do we have extra descriptions? */
    if (obj->ex_description) {	/*. Yep, save them too . */
        for (ex_desc = obj->ex_description; ex_desc;
                ex_desc = ex_desc->next) {
            /*. Sanity check to prevent nasty protection faults . */
            if (!*ex_desc->keyword || !*ex_desc->description) {
                continue;
            }
            strcpy(buf1, ex_desc->description);
            clean_string(buf1);
            fprintf(fp, "E\n" "%s~\n" "%s~\n", ex_desc->keyword, buf1);
        }
    }
    return 1;
}

void save_corpses(void) {
    /* This is basically the mother of all the save corpse functions */
    /* You can call it from anywhere in the game with no arguments */
    /* Basically any time a corpse is manipulated in any way..either */
    /* directly or indirectly you need to call this function */

    FILE *fp;
    struct obj_data *i;
    struct corpse_list_data *temp = NULL, *next;
    int location = 0;

    /* Open corpse file */

    if (!(fp = fopen(((save_new_style) ? NEW_CORPSE_FILE : CORPSE_FILE), "wb"))) {
        if (errno != ENOENT)	/* if it fails, NOT because of no file */
            log("SYSERR: checking for corpse file %s : %s", CORPSE_FILE,
                strerror(errno));
        return;
    }

    /* Scan the object list */
    for (temp = corpse_list; temp; temp = next) {
        next = temp->next;
        i = temp->corpse;
        if (!i->contains)
            continue;
        /* It is, so save it to a file */
        if (!corpse_save(i, fp, location, FALSE)) {
            log("SYSERR: A corpse didnt save for some reason");
            fclose(fp);
            return;
        }
    }
    /* Close the corpse file */
    fclose(fp);
}

void load_corpses(void) {
    /* Ahh load corpses.. it was cake to write them out to a file      */
    /* it was a pain to load them back up through without a character. */
    /* Because I don't have a character I couldnt figure out how to     */
    /* put objects back into the corpse the exact way they came out..  */
    /* like objects back in their container, etc. So I just decided to */
    /* Dump it all in the corpse and let the character sort it out.    */
    /* If they don't like it, screwum. They are lucky I coded this:)    */
    /* Oh, and a bunch of this code is from Patricks XAP obj's code    */

    FILE *fp;
    char line[256];
    int t[20], danger, zwei = 0;
    int j, k, nr, num_objs = 0;
    struct obj_data *temp = NULL, *obj = NULL, *next_obj = NULL;
    struct extra_descr_data *new_descr;
    char buf2[MAX_INPUT_LENGTH];
    char buf1[MAX_INPUT_LENGTH];
    int retval = 0;

    for (j = 0; j < 15; j++)
        t[j] = 0;

    if (!(fp = fopen(CORPSE_FILE, "r+b"))) {
        if (errno != ENOENT) {
            sprintf(buf1, "SYSERR: READING CORPSE FILE %s in load_corpses",
                    CORPSE_FILE);
            perror(buf1);
        }
    }

    if (!feof(fp))
        get_line(fp, line);
    while (!feof(fp)) {
        temp = NULL;
        /* first, we get the number. Not too hard. */
        if (*line == '#') {
            if (sscanf(line, "#%d", &nr) != 1) {
                continue;
            }
            /* we have the number, check it, load obj. */
            if (nr == NOTHING) {	/* then it is unique */
                temp = create_obj(NOTHING);
            } else if (nr < 0) {
                continue;
            } else {
                if (nr >= 999999)
                    continue;
                temp = read_object(nr, VIRTUAL);
                if (!temp) {
                    continue;
                }
            }



            /* now we read locate. - this is for autoeq, will be 0 elsewise */
            /* only the rest of the vals, and extra flags will be read */
            get_line(fp, line);
            retval = sscanf(line, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                            t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6, t + 7,
                            t + 8, t + 9, t + 10, t + 11, t + 12, t + 13, t + 14, t + 15, t + 16, t + 17);

            if (retval == 11) {
                GET_OBJ_VAL(temp, 0) = t[1];
                GET_OBJ_VAL(temp, 1) = t[2];
                GET_OBJ_VAL(temp, 2) = t[3];
                GET_OBJ_VAL(temp, 3) = t[4];
                GET_OBJ_EXTRA(temp)[0] = t[5];
                GET_OBJ_EXTRA(temp)[1] = t[6];
                GET_OBJ_EXTRA(temp)[2] = t[7];
                GET_OBJ_EXTRA(temp)[3] = t[8];
                GET_OBJ_VROOM(temp)    = t[9];
                GET_OBJ_TIMER(temp) = t[10];
                /*---unknown----*/
                GET_OBJ_VAL(temp, 4) = 0;
                GET_OBJ_VAL(temp, 5) = 0;
                GET_OBJ_VAL(temp, 6) = 0;
                GET_OBJ_VAL(temp, 7) = 0;
                GET_OBJ_VAL(temp, 8) = 0;
                GET_OBJ_VAL(temp, 9) = 0;
                GET_OBJ_INNATE(temp) = 0;
            } else {
                GET_OBJ_VAL(temp, 0) = t[1];
                GET_OBJ_VAL(temp, 1) = t[2];
                GET_OBJ_VAL(temp, 2) = t[3];
                GET_OBJ_VAL(temp, 3) = t[4];
                GET_OBJ_VAL(temp, 4) = t[5];
                GET_OBJ_VAL(temp, 5) = t[6];
                GET_OBJ_VAL(temp, 6) = t[7];
                GET_OBJ_VAL(temp, 7) = t[8];
                GET_OBJ_VAL(temp, 8) = t[9];
                GET_OBJ_VAL(temp, 9) = t[10];
                GET_OBJ_EXTRA(temp)[0] = t[11];
                GET_OBJ_EXTRA(temp)[1] = t[12];
                GET_OBJ_EXTRA(temp)[2] = t[13];
                GET_OBJ_EXTRA(temp)[3] = t[14];
                GET_OBJ_TIMER(temp) = t[15];
                GET_OBJ_INNATE(temp) = t[16];
                GET_OBJ_VROOM(temp)    = t[17];

            }



            get_line(fp, line);
            /* read line check for xap. */
            if (!strcasecmp("XAP", line)) {	/* then this is a Xap Obj, requires
                						   special care */
                if ((temp->name = fread_string(fp, buf2)) == NULL) {
                    temp->name = strdup("undefined");
                }

                if ((temp->short_description = fread_string(fp, buf2)) == NULL) {
                    temp->short_description = strdup("undefined");
                }

                if ((temp->description = fread_string(fp, buf2)) == NULL) {
                    temp->description = strdup("undefined");
                }

                if ((temp->action_description =
                            fread_string(fp, buf2)) == NULL) {
                    temp->action_description = 0;
                }
                if (!get_line(fp, line) ||
                        (sscanf
                         (line, "%d %d %d %d %d %d %d %d", t, t + 1, t + 2,
                          t + 3, t + 4, t + 5, t + 6, t + 7) != 8)) {
                    log("Format error in first numeric line (expecting 8 args)");
                }
                temp->obj_flags.type_flag = t[0];
                temp->obj_flags.wear_flags[0] = t[1];
                temp->obj_flags.wear_flags[1] = t[2];
                temp->obj_flags.wear_flags[2] = t[3];
                temp->obj_flags.wear_flags[3] = t[4];
                temp->obj_flags.weight = t[5];
                temp->obj_flags.cost = t[6];
                temp->obj_flags.cost_per_day = t[7];



                /* we're clearing these for good luck */

                for (j = 0; j < MAX_OBJ_AFFECT; j++) {
                    temp->affected[j].location = APPLY_NONE;
                    temp->affected[j].modifier = 0;
                }

                /* You have to null out the extradescs when you're parsing a xap_obj.
                   This is done right before the extradescs are read. */

                if (temp->ex_description) {
                    temp->ex_description = NULL;
                }

                get_line(fp, line);
                for (k = j = zwei = 0; !zwei && !feof(fp);) {
                    switch (*line) {
                    case 'E':
                        CREATE(new_descr, struct extra_descr_data, 1);
                        if ((new_descr->keyword = fread_string(fp, buf2)) == NULL)
                            new_descr->keyword = strdup("Undefined");
                        if ((new_descr->description = fread_string(fp, buf2)) == NULL)
                            new_descr->description = strdup("Undefined");
                        new_descr->next = temp->ex_description;
                        temp->ex_description = new_descr;
                        get_line(fp, line);
                        break;
                    case 'A':
                        if (j >= MAX_OBJ_AFFECT) {
                            log("SYSERR: Too many object affectations in loading rent file");
                            danger = 1;
                        }
                        get_line(fp, line);
                        sscanf(line, "%d %d", t, t + 1);

                        temp->affected[j].location = t[0];
                        temp->affected[j].modifier = t[1];
                        j++;
                        get_line(fp, line);
                        break;

                    case '$':
                    case '#':
                        zwei = 1;
                        break;
                    default:
                        zwei = 1;
                        break;
                    }
                }		/* exit our for loop */
            }			/* exit our xap loop */
            if (temp != NULL) {
                num_objs++;
                /* Check if our object is a corpse */
                /* scan our temp room for objects */
                if (IS_OBJ_STAT(temp, ITEM_PC_CORPSE)) {
                    add_corpse_to_list(temp);
                    for (obj = world_vnum[1202]->contents; obj;
                            obj = next_obj) {
                        next_obj = obj->next_content;
                        if (obj) {
                            obj_from_room(obj);	/* get those objs from that room */
                            obj_to_obj(obj, temp);	/* and put them in the corpse */
                        }
                    }		/* exit the room scanning loop */
                    if (temp) {	/* put the corpse in the right room */
                        log("CORPSE: Corpse '%s' to room %d.",
                            temp->short_description, GET_OBJ_VROOM(temp));
                        obj_to_room(temp, IN_ROOM(temp));
                    }
                } else {
                    /* just a plain obj..send it to a temp room until we load a corpse */
                    if (temp)
                        obj_to_room(temp, (real_room(1202)));
                }
            }
        }
    }
    fclose(fp);
}

void add_corpse_to_list(OBJ_DATA *corpse) {
    struct corpse_list_data *temp = NULL;
    if (!corpse || !IS_OBJ_STAT(corpse, ITEM_PC_CORPSE))
        return;
    log("Adding %s to save list", corpse->short_description);
    check_timer(corpse);
    CREATE(temp, struct corpse_list_data, 1);
    temp->corpse = corpse;
    temp->next = corpse_list;
    corpse_list = temp;
}

void remove_corpse_from_list(OBJ_DATA *corpse) {
    struct corpse_list_data *tcorpse = corpse_list, *temp;
    if (!corpse_list || !corpse)
        return;
    log("Removing %s from save list", corpse->short_description);
    while (tcorpse) {
        if (tcorpse->corpse == corpse) {
            REMOVE_FROM_LIST(tcorpse, corpse_list, next);
            free(tcorpse);
            return;
        }
        tcorpse = tcorpse->next;
    }
}

void free_corpse_list(struct corpse_list_data *cor) {
    if (!cor)
        return;

    if (cor->next)
        free_corpse_list(cor->next);

    free(cor);
}

void do_show_corpses(Character *ch) {
    struct corpse_list_data *temp = NULL;
    time_t diff;
    time_t tm = time(0);
    if (!corpse_list) {
        ch->Send( "No player corpses around.\r\n");
        return;
    }
    temp = corpse_list;
    while (temp) {
    check_timer(temp->corpse);
    if (GET_OBJ_EXPIRE(temp->corpse) > tm) {
        diff = (GET_OBJ_EXPIRE(temp->corpse) - tm);
        ch->Send( "In room %d is %s - {cC%ld{cy min and {cY%ld{cy seconds  until Automeld{c0\r\n", GET_OBJ_VROOM(temp->corpse), temp->corpse->short_description,  diff/60, diff%60);
        } else
        ch->Send( "In room %d is %s - (Timer not set)\r\n", GET_OBJ_VROOM(temp->corpse), temp->corpse->short_description);
        temp = temp->next;
    }
}

ACMD(do_corpse) {
    int found = 0;
    struct corpse_list_data *temp = NULL;
    char arg[MAX_INPUT_LENGTH];
    room_rnum rnum = NULL;
    int track;
    if (!corpse_list) {
        ch->Send( "No player corpses around.\r\n");
        return;
    }
    one_argument(argument, arg);

    track = strcmp(arg, "track");

    temp = corpse_list;
    while (temp) {
        if (IN_ROOM(temp->corpse) != NULL) {
            if (GET_OBJ_VAL(temp->corpse, 0) == get_pidx_from_name(ch)) {
                if (track) {
                time_t diff = GET_OBJ_EXPIRE(temp->corpse) - time(0);
                    ch->Send( "In room %s is %s - (Auto-melding in {cC%ld{c0 min and {cY%ld{c0 seconds)\r\n", IN_ROOM(temp->corpse)->name, temp->corpse->short_description,
                              diff/60, diff%60);
                } else
                    rnum = IN_ROOM(temp->corpse);
                found = 1;
            }
        }
        temp = temp->next;
    }
    if (!found) {
        ch->Send( "No unrecovered corpse of yours found.\r\n");
        return;
    }
    if (track)
        return;

    /* track corpse */
    if (rnum == NULL) {
        ch->Send( "Can't find corpse to track.\r\n");
        return;
    }

    {
        int dir;

        /* They passed the skill check. */
        dir = graph.find_first_step(IN_ROOM(ch), rnum);

        switch (dir) {
        case BFS_ERROR:
            send_to_char("Hmm.. something seems to be wrong.\r\n", ch);
            break;
        case BFS_ALREADY_THERE:
            send_to_char("You're already in the same room!!\r\n", ch);
            break;
        case BFS_NO_PATH:
            ch->Send(
                "You can't sense the track to your corpse.\r\n");
            break;
        default:			/* Success! */
            ch->Send(
                "{cWYou sense the track to your corpse leading %s from here!{c0\r\n",
                dirs[dir]);
            WAIT_STATE(ch, PULSE_VIOLENCE);
            break;
        }
        return;
    }

}

Character *find_char_by_uid_in_lookup_table ( long uid ); //this is from dg_scripts.cpp
void restore_all_corpses() {
	corpse_list_data* next = NULL;
	for (struct corpse_list_data* ce = corpse_list;ce;ce=next) {
		next = ce->next;
		if (ce->corpse) {
			int id = GET_OBJ_VAL(ce->corpse,0);
			automeld(ce->corpse);
			Character* ch = find_char_by_uid_in_lookup_table(id);
			if (ch && ch->desc)
				write_to_descriptor(ch->desc->descriptor, "You meld in a sudden flash of light!\r\n", ch->desc->comp);
		}
	}
}
