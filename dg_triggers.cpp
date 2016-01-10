/**************************************************************************
*  File: triggers.c                                                       *
*                                                                         *
*  Usage: contains all the trigger functions for scripts.                 *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Death's Gate MUD is based on CircleMUD, Copyright (C) 1993, 94.        *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
*                                                                         *
*  $Author: w4dimenscor $
*  $Date: 2007/06/09 06:09:08 $
*  $Revision: 1.17 $
**************************************************************************/

#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "dg_scripts.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "constants.h"
#include "oasis.h"
#include "descriptor.h"
#include "assemblies.h"

extern struct index_data **trig_index;
extern const char *cmd_door[];

#ifndef LVL_BUILDER
#define LVL_BUILDER LVL_GOD
#endif


/* external functions */
const char *skill_name(int num);
char *matching_quote(char *p);
char *str_str(char *cs, char *ct);
extern struct time_info_data time_info;
int zone_is_empty ( zone_rnum zone_nr );

/*
 *  General functions used by several triggers
 */


/*
 * Copy first phrase into first_arg, returns rest of string
 */
char *one_phrase(char *arg, char *first_arg) {
    skip_spaces(&arg);

    if (!*arg)
        *first_arg = '\0';

    else if (*arg == '"') {
        char *p, c;

        p = matching_quote(arg);
        c = *p;
        *p = '\0';
        strcpy(first_arg, arg + 1);
        if (c == '\0')
            return p;
        else
            return p + 1;
    }
    else {
        char *s, *p;

        s = first_arg;
        p = arg;

        while (*p && !isspace(*p) && *p != '"')
            *s++ = *p++;

        *s = '\0';
        return p;
    }

    return arg;
}


int is_substring(char *sub, char *string) {
    char *s;

    if ((s = str_str(string, sub))) {
        int len = strlen(string);
        int sublen = strlen(sub);

        /* check front */
        if ((s == string || isspace(*(s - 1)) || ispunct(*(s - 1))) &&
                /* check end */
                ((s + sublen == string + len) || isspace(s[sublen]) ||
                 ispunct(s[sublen])))
            return 1;
    }

    return 0;
}


/*
 * return 1 if str contains a word or phrase from wordlist.
 * phrases are in double quotes (").
 * if wrdlist is NULL, then return 1, if str is NULL, return 0.
 */
int word_check(char *str, char *wordlist) {
    char words[MAX_INPUT_LENGTH], phrase[MAX_INPUT_LENGTH], *s;

    if (*wordlist == '*')
        return 1;

    strcpy(words, wordlist);

    for (s = one_phrase(words, phrase); *phrase; s = one_phrase(s, phrase))
        if (is_substring(phrase, str))
            return 1;

    return 0;
}



/*
 *  mob triggers
 */

void random_mtrigger(Character * ch) {
    trig_data *t;

    /*
     * A GlobalRandom trigger always fires.
     * A Random trigger is only called if a char is in the zone without nohassle.
     */

    if ( AFF_FLAGGED ( ch, AFF_CHARM ) )
        return;

    for ( t = TRIGGERS ( SCRIPT ( ch ) ); t; t = t->next ) {
        if ( TRIGGER_CHECK ( t, MTRIG_RANDOM ) && number ( 1, 100 ) <= GET_TRIG_NARG ( t ) &&
	   ( TRIGGER_CHECK ( t, MTRIG_GLOBAL ) || !zone_is_empty ( GET_ROOM_ZONE ( IN_ROOM ( ch ) ) ) ) )
                script_driver ( &ch, t, MOB_TRIGGER, TRIG_NEW );
    }
}

void bribe_mtrigger(Character * ch, Character * actor, int amount) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (!SCRIPT_CHECK(ch, MTRIG_BRIBE) || AFF_FLAGGED(ch, AFF_CHARM))
        return;

    for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
        if (TRIGGER_CHECK(t, MTRIG_BRIBE) && (amount >= GET_TRIG_NARG(t))) {

            snprintf(buf, sizeof(buf), "%d", amount);
            add_var(&GET_TRIG_VARS(t), "amount", buf, 0);
            ADD_UID_VAR(buf, t, actor, "actor", 0);
            script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);

            break;
        }
    }
}

void greet_memory_mtrigger(Character * actor) {
    trig_data *t;
    Character *ch;
    struct script_memory *mem;
    char buf[MAX_INPUT_LENGTH];
    int command_performed = 0;

    if ( IN_ROOM ( actor ) == NULL )
        return;

    for (ch = IN_ROOM(actor)->people; ch; ch = ch->next_in_room) {
        if ((!SCRIPT_MEM(ch) || !AWAKE(ch) || FIGHTING(ch) || (ch == actor)
                || AFF_FLAGGED(ch, AFF_CHARM)) && ch != NULL)
            continue;

        /* find memory line with command only */
        for (mem = SCRIPT_MEM(ch); mem && SCRIPT_MEM(ch); mem = mem->next) {
            if (GET_ID(actor) != mem->id)
                continue;
            if (mem->cmd) {
                command_interpreter(ch, mem->cmd);   /* no script */
                command_performed = 1;
                break;
            }
        }

        /* if a command was not performed execute the memory script */
        if (mem && !command_performed) {
            for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
                if (IS_SET(GET_TRIG_TYPE(t), MTRIG_MEMORY) &&
                        CAN_SEE(ch, actor) &&
                        !GET_TRIG_DEPTH(t) &&
                        number(1, 100) <= GET_TRIG_NARG(t)) {
                    ADD_UID_VAR(buf, t, actor, "actor", 0);
                    script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
                    break;
                }
            }
        }

        /* delete the memory */
        if (mem) {
            if (SCRIPT_MEM(ch) == mem) {
                SCRIPT_MEM(ch) = mem->next;
            } else {
                struct script_memory *prev;
                prev = SCRIPT_MEM(ch);
                while (prev->next != mem)
                    prev = prev->next;
                prev->next = mem->next;
            }
            if (mem->cmd)
                free(mem->cmd);
            free(mem);
        }
    }
}


int greet_mtrigger(Character * actor, int dir) {
    trig_data *t = NULL;
    Character *ch = NULL;
    char buf[MAX_INPUT_LENGTH];
    int intermediate, final = TRUE;
    if (IN_ROOM(actor) == NULL)
        return FALSE;
    for (ch = IN_ROOM(actor)->people; ch; ch = ch->next_in_room) {
        if (ch == NULL)
            break;
        if (!SCRIPT_CHECK(ch, MTRIG_GREET | MTRIG_GREET_ALL) ||
                !AWAKE(ch) || FIGHTING(ch) || (ch == actor) ||
                AFF_FLAGGED(ch, AFF_CHARM))
            continue;

        for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
            if (((IS_SET(GET_TRIG_TYPE(t), MTRIG_GREET)
                    && CAN_SEE(ch, actor))
                    || IS_SET(GET_TRIG_TYPE(t), MTRIG_GREET_ALL))
                    && !GET_TRIG_DEPTH(t)
                    && (number(1, 100) <= GET_TRIG_NARG(t))) {

                if (dir>=0 && dir < NUM_OF_DIRS)
                    add_var(&GET_TRIG_VARS(t), "direction",
                            (char *) dirs[rev_dir[dir]], 0);
                else
                    add_var(&GET_TRIG_VARS(t), "direction", "none", 0);
                ADD_UID_VAR(buf, t, actor, "actor", 0);

                intermediate = script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
                if (!intermediate)
                    final = FALSE;
                continue;
            }
        }
    }
    return final;
}
/**
 
**/

void entry_memory_mtrigger(Character * ch) {
    trig_data *t;
    Character *actor;
    struct script_memory *mem;
    char buf[MAX_INPUT_LENGTH];

    if ( !SCRIPT_MEM(ch) || AFF_FLAGGED(ch, AFF_CHARM) || IN_ROOM ( ch ) == NULL )
        return;


    for (actor = IN_ROOM(ch)->people; actor && SCRIPT_MEM(ch);
            actor = actor->next_in_room) {
        if (actor != ch && SCRIPT_MEM(ch)) {
            for (mem = SCRIPT_MEM(ch); mem && SCRIPT_MEM(ch);
                    mem = mem->next) {
                if (GET_ID(actor) == mem->id) {
                    struct script_memory *prev;
                    if (mem->cmd)
                        command_interpreter(ch, mem->cmd);
                    else {
                        for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
                            if (TRIGGER_CHECK(t, MTRIG_MEMORY)
                                    && (number(1, 100) <= GET_TRIG_NARG(t))) {

                                ADD_UID_VAR(buf, t, actor, "actor", 0);

                                script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
                                break;
                            }
                        }
                    }
                    /* delete the memory */
                    if (SCRIPT_MEM(ch) == mem) {
                        SCRIPT_MEM(ch) = mem->next;
                    } else {
                        prev = SCRIPT_MEM(ch);
                        while (prev->next != mem)
                            prev = prev->next;
                        prev->next = mem->next;
                    }
                    if (mem->cmd)
                        free(mem->cmd);
                    free(mem);
                }
            }             /* for (mem =..... */
        }
    }
}

int entry_mtrigger(Character * ch) {
    trig_data *t;

    if (!SCRIPT_CHECK(ch, MTRIG_ENTRY) || AFF_FLAGGED(ch, AFF_CHARM))
        return 1;

    for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
        if (TRIGGER_CHECK(t, MTRIG_ENTRY)
                && (number(1, 100) <= GET_TRIG_NARG(t))) {


            return script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
        }
    }

    return 1;
}

int command_mtrigger(Character * actor, char *cmd, char *argument) {
    Character *ch, *ch_next;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    char *remove_percentage(char *string);
    string argument_copy;

    /* prevent people we like from becoming trapped :P */
    if (!valid_dg_target(actor, FALSE))
        return 0;

    if ( IN_ROOM ( actor ) == NULL )
        return 0;

    for (ch = IN_ROOM(actor)->people; ch; ch = ch_next) {
        ch_next = ch->next_in_room;

        if (SCRIPT_CHECK(ch, MTRIG_COMMAND) && !AFF_FLAGGED(ch, AFF_CHARM)
                && (actor != ch)) {
            for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
                if (!TRIGGER_CHECK(t, MTRIG_COMMAND))
                    continue;

                if (!GET_TRIG_ARG(t) || !*GET_TRIG_ARG(t)) {
                    sprintf(buf,
                            "SYSERR: Command Trigger #%d has no text argument!",
                            GET_TRIG_VNUM(t));
                    mudlog(buf, NRM, LVL_BUILDER, TRUE);
                    continue;
                }

                if (*GET_TRIG_ARG(t) == '*' ||
                        !strn_cmp(GET_TRIG_ARG(t), cmd,
                                  strlen(GET_TRIG_ARG(t)))) {

                    argument_copy = string ( argument );
                    ADD_UID_VAR(buf, t, actor, "actor", 0);
                    while ( argument_copy != "" && argument_copy[0] == ' ' )
			argument_copy.erase ( argument_copy.begin() );
                    add_var(&GET_TRIG_VARS(t), "arg", remove_percentage ( (char*) argument_copy.c_str() ), 0);
                    skip_spaces(&cmd);
                    add_var(&GET_TRIG_VARS(t), "cmd", cmd, 0);

                    if (script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW))
                        return 1;

                }
            }
        }
    }

    return 0;
}


void speech_mtrigger(Character * actor, char *str) {
    Character *ch, *ch_next;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    char *remove_percentage(char *string);

    if ( IN_ROOM ( actor ) == NULL )
        return;

    for (ch = IN_ROOM(actor)->people; ch; ch = ch_next) {
        ch_next = ch->next_in_room;

        if (SCRIPT_CHECK(ch, MTRIG_SPEECH) && AWAKE(ch) &&
                !AFF_FLAGGED(ch, AFF_CHARM) && (actor != ch))
            for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
                if (!TRIGGER_CHECK(t, MTRIG_SPEECH))
                    continue;

                if (!GET_TRIG_ARG(t) || !*GET_TRIG_ARG(t)) {
                    sprintf(buf,
                            "SYSERR: Speech Trigger #%d has no text argument!",
                            GET_TRIG_VNUM(t));
                    mudlog(buf, NRM, LVL_BUILDER, TRUE);
                    continue;
                }

                if (((GET_TRIG_NARG(t) && word_check(str, GET_TRIG_ARG(t)))
                        || (!GET_TRIG_NARG(t)
                            && is_substring(GET_TRIG_ARG(t), str)))) {

                    ADD_UID_VAR(buf, t, actor, "actor", 0);
                    add_var(&GET_TRIG_VARS(t), "speech", remove_percentage(str), 0);

                    script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
                    break;
                }
            }
    }
}

void act_mtrigger(const Character * ch, char *str, Character * actor,
                  Character * victim, obj_data * object,
                  obj_data * target, char *arg) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (SCRIPT_CHECK(ch, MTRIG_ACT) && !AFF_FLAGGED(ch, AFF_CHARM) &&
            (actor != ch))
        for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
            if (!TRIGGER_CHECK(t, MTRIG_ACT))
                continue;

            if (!GET_TRIG_ARG(t) || !*GET_TRIG_ARG(t)) {
                sprintf(buf,
                        "SYSERR: Act Trigger #%d has no text argument!",
                        GET_TRIG_VNUM(t));
                mudlog(buf, NRM, LVL_BUILDER, TRUE);
                continue;
            }

            if (((GET_TRIG_NARG(t) && word_check(str, GET_TRIG_ARG(t))) ||
                    (!GET_TRIG_NARG(t)
                     && is_substring(GET_TRIG_ARG(t), str)))) {

                if (actor)
                    ADD_UID_VAR(buf, t, actor, "actor", 0);
                if (victim)
                    ADD_UID_VAR(buf, t, victim, "victim", 0);
                if (object)
                    ADD_UID_VAR(buf, t, object, "object", 0);
                if (target)
                    ADD_UID_VAR(buf, t, target, "target", 0);
                if (str) {
                    /* we're guaranteed to have a string ending with \r\n\0 */
                    char *nstr = strdup(str), *fstr = nstr, *p = strchr(nstr, '\r');
                    skip_spaces(&nstr);
                    *p = '\0';
                    add_var(&GET_TRIG_VARS(t), "arg", nstr, 0);
                    free(fstr);
                }
                script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
                break;
            }
        }
}

void fight_mtrigger(Character * ch) {
    Character *actor;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (!SCRIPT_CHECK(ch, MTRIG_FIGHT) || !FIGHTING(ch) ||
            AFF_FLAGGED(ch, AFF_CHARM))
        return;

    for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
        if (TRIGGER_CHECK(t, MTRIG_FIGHT) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {
            actor = FIGHTING(ch);
            if (actor)
                ADD_UID_VAR(buf, t, actor, "actor", 0);
            else
                add_var(&GET_TRIG_VARS(t), "actor", "nobody", 0);


            script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
            break;
        }
    }
}

void time_mtrigger(Character *ch) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    /*
     * This trigger is called if the hour is the same as specified in Narg.
     */

    if ( AFF_FLAGGED ( ch, AFF_CHARM ) )
        return;

    for ( t = TRIGGERS ( SCRIPT ( ch ) ); t; t = t->next ) {
        if ( TRIGGER_CHECK ( t, MTRIG_TIME ) && time_info.hours == GET_TRIG_NARG ( t ) &&
	   ( TRIGGER_CHECK ( t, MTRIG_GLOBAL ) || !zone_is_empty ( GET_ROOM_ZONE ( IN_ROOM ( ch ) ) ) ) ) {
		snprintf ( buf, sizeof ( buf ), "%d", time_info.hours );
		add_var ( &GET_TRIG_VARS ( t ), "time", buf, 0 );
		script_driver ( &ch, t, MOB_TRIGGER, TRIG_NEW );
        }
    }
}



void hitprcnt_mtrigger(Character * ch) {
    Character *actor;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (ch != NULL) {
        if (!SCRIPT_CHECK(ch, MTRIG_HITPRCNT) || !FIGHTING(ch) ||
                AFF_FLAGGED(ch, AFF_CHARM))
            return;

        for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
            if (TRIGGER_CHECK(t, MTRIG_HITPRCNT) && GET_MAX_HIT(ch) &&
                    (((GET_HIT(ch) * 100) / GET_MAX_HIT(ch)) <=
                     GET_TRIG_NARG(t))) {


                actor = FIGHTING(ch);
                ADD_UID_VAR(buf, t, actor, "actor", 0);

                script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
                break;
            }
        }
    }
}


int receive_mtrigger(Character * ch, Character * actor, obj_data * obj) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    int ret_val;

    if (!SCRIPT_CHECK(ch, MTRIG_RECEIVE) || AFF_FLAGGED(ch, AFF_CHARM))
        return 1;

    for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
        //    if (TRIGGER_CHECK(t, MTRIG_RECEIVE) &&
        //      (number(1, 100) <= GET_TRIG_NARG(t))){
        if (TRIGGER_CHECK(t, MTRIG_RECEIVE)) {

            ADD_UID_VAR(buf, t, actor, "actor", 0);
            ADD_UID_VAR(buf, t, obj, "object", 0);

            ret_val = script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);

            if (DEAD(actor) || DEAD(ch) || obj->carried_by != actor)
                return 0;
            else
                return ret_val;
        }
    }

    return 1;
}


int death_mtrigger(Character * ch, Character * actor) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (!SCRIPT_CHECK(ch, MTRIG_DEATH) || AFF_FLAGGED(ch, AFF_CHARM))
        return 1;

    for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
        if (TRIGGER_CHECK(t, MTRIG_DEATH) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {


            if (actor)
                ADD_UID_VAR(buf, t, actor, "actor", 0);

            return script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
        }
    }

    return 1;
}

int load_mtrigger(Character * ch) {
    trig_data *t;

    if (!SCRIPT_CHECK(ch, MTRIG_LOAD))
        return 1;

    for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
        if (TRIGGER_CHECK(t, MTRIG_LOAD) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {


            script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
            return ch ? 1 : -1;
        }
    }
    return 1;
}

int cast_mtrigger(Character * actor, Character * ch, int spellnum) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (ch == NULL)
        return 1;

    if (!SCRIPT_CHECK(ch, MTRIG_CAST) || AFF_FLAGGED(ch, AFF_CHARM))
        return 1;

    for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
        if (TRIGGER_CHECK(t, MTRIG_CAST) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {


            ADD_UID_VAR(buf, t, actor, "actor", 0);
            sprintf(buf, "%d", spellnum);
            add_var(&GET_TRIG_VARS(t), "spell", buf, 0);
            add_var(&GET_TRIG_VARS(t), "spellname",
                    (char *) skill_name(spellnum), 0);

            return script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
        }
    }

    return 1;
}

int leave_mtrigger(Character * actor, int dir) {
    trig_data *t;
    Character *ch;
    char buf[MAX_INPUT_LENGTH];

    if ( IN_ROOM ( actor ) == NULL )
        return 1;

    for (ch = IN_ROOM(actor)->people; ch; ch = ch->next_in_room) {
        if (!SCRIPT_CHECK(ch, MTRIG_LEAVE) ||
                !AWAKE(ch) || FIGHTING(ch) || (ch == actor) ||
                AFF_FLAGGED(ch, AFF_CHARM))
            continue;

        for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
            if ((IS_SET(GET_TRIG_TYPE(t), MTRIG_LEAVE)
                    && CAN_SEE(ch, actor)) && !GET_TRIG_DEPTH(t)
                    && (number(1, 100) <= GET_TRIG_NARG(t))) {

                if (dir>=0 && dir < NUM_OF_DIRS)
                    add_var(&GET_TRIG_VARS(t), "direction", (char *) dirs[dir], 0);
                else
                    add_var(&GET_TRIG_VARS(t), "direction", "none", 0);
                ADD_UID_VAR(buf, t, actor, "actor", 0);

                return script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
            }
        }
    }
    return 1;
}

int door_mtrigger(Character * actor, int subcmd, int dir) {
    trig_data *t;
    Character *ch;
    char buf[MAX_INPUT_LENGTH];

    if ( IN_ROOM ( actor ) == NULL )
        return 1;

    for (ch = IN_ROOM(actor)->people; ch; ch = ch->next_in_room) {
        if (!SCRIPT_CHECK(ch, MTRIG_DOOR) ||
                !AWAKE(ch) || FIGHTING(ch) || (ch == actor) ||
                AFF_FLAGGED(ch, AFF_CHARM))
            continue;

        for (t = TRIGGERS(SCRIPT(ch)); t; t = t->next) {
            if (IS_SET(GET_TRIG_TYPE(t), MTRIG_DOOR) && CAN_SEE(ch, actor)
                    && !GET_TRIG_DEPTH(t)
                    && (number(1, 100) <= GET_TRIG_NARG(t))) {

                add_var(&GET_TRIG_VARS(t), "cmd", (char *) cmd_door[subcmd], 0);
                if (dir>=0 && dir < NUM_OF_DIRS)
                    add_var(&GET_TRIG_VARS(t), "direction", (char *) dirs[dir],0);
                else
                    add_var(&GET_TRIG_VARS(t), "direction", "none", 0);
                ADD_UID_VAR(buf, t, actor, "actor", 0);

                return script_driver(&ch, t, MOB_TRIGGER, TRIG_NEW);
            }
        }
    }
    return 1;
}


/*
 *  object triggers
 */

void random_otrigger(obj_data * obj) {
    trig_data *t;
    zone_rnum zone;
    obj_data *ob = obj;
	Room *r = NULL;

    for ( t = TRIGGERS ( SCRIPT ( obj ) ); t; t = t->next ) {
	if ( TRIGGER_CHECK ( t, OTRIG_RANDOM ) && number ( 1, 100 ) <= GET_TRIG_NARG ( t ) ) {
		if ( !TRIGGER_CHECK ( t, OTRIG_GLOBAL ) ) {
			if ( obj->in_obj )
				ob = obj->in_obj;

			r = obj_room ( ob );
			if ( !r )
				continue;
			zone = GET_ROOM_ZONE ( r );

			if ( zone_is_empty ( zone ) )
				continue;
		}
                script_driver ( &obj, t, OBJ_TRIGGER, TRIG_NEW );
	}
    }
}


int timer_otrigger(struct obj_data *obj) {
    trig_data *t;

    if (!SCRIPT_CHECK(obj, OTRIG_TIMER))
        return 1;

    for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
        if (TRIGGER_CHECK(t, OTRIG_TIMER)) {
            script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);
            if (!obj)
                return -1;
        }
    }

    return 1;
}


int get_otrigger(obj_data * obj, Character * actor) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    int ret_val = 0;

    if (!SCRIPT_CHECK(obj, OTRIG_GET))
        return 1;

    for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
        if (TRIGGER_CHECK(t, OTRIG_GET)
                && (number(1, 100) <= GET_TRIG_NARG(t))) {

            ADD_UID_VAR(buf, t, actor, "actor", 0);

            ret_val = script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);

            /* don't allow a get to take place, if
             * a) the actor is killed (the mud would choke on obj_to_char).
             * b) the object is purged.
             */
            if (!obj)
                return -1;
            else if (DEAD(actor))
                return 0;
            else
                return ret_val;
        }
    }

    return 1;
}

/* checks for command trigger on specific object. assumes obj has cmd trig */
int cmd_otrig(obj_data * obj, Character * actor, char *cmd,
              char *argument, int type) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    char *remove_percentage(char *string);
    string argument_copy;

    if (obj && SCRIPT_CHECK(obj, OTRIG_COMMAND))
        for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
            if (!TRIGGER_CHECK(t, OTRIG_COMMAND))
                continue;

            if (IS_SET(GET_TRIG_NARG(t), type) && (!GET_TRIG_ARG(t) || !*GET_TRIG_ARG(t))) {
                new_mudlog(NRM, LVL_BUILDER, TRUE, "SYSERR: O-Command Trigger #%d has no text argument!",
                           +          GET_TRIG_VNUM(t));
                continue;
            }

            if (IS_SET(GET_TRIG_NARG(t), type) &&
                    (*GET_TRIG_ARG(t) == '*' ||
                     !strn_cmp(GET_TRIG_ARG(t), cmd, strlen(GET_TRIG_ARG(t))))) {

                argument_copy = string ( argument );
                ADD_UID_VAR(buf, t, actor, "actor", 0);
                while ( argument_copy != "" && argument_copy[0] == ' ' )
			argument_copy.erase ( argument_copy.begin() );
                add_var(&GET_TRIG_VARS(t), "arg", remove_percentage ( (char*) argument_copy.c_str() ), 0);
                skip_spaces(&cmd);
                add_var(&GET_TRIG_VARS(t), "cmd", cmd, 0);

                if (script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW))
                    return 1;

            }
        }

    return 0;
}


int command_otrigger(Character * actor, char *cmd, char *argument) {
    obj_data *obj, *next;
    int i;

    if (!(actor)) {
        log("SYSERR:Invalid pointer passed to command_otrigger.");
        return 0;
    }

    /* prevent people we like from becoming trapped :P */
    if (!valid_dg_target(actor, FALSE))
        return 0;

    for (i = 0; i < NUM_WEARS; i++)
        if (GET_EQ(actor, i))
            if (cmd_otrig(GET_EQ(actor, i), actor, cmd, argument, OCMD_EQUIP))
                return 1;

    for (obj = actor->carrying; obj; obj = next) {
        next = obj->next_content;
        if (obj->carried_by == actor && cmd_otrig(obj, actor, cmd, argument, OCMD_INVEN))
            return 1;
    }

    if ( IN_ROOM ( actor ) == NULL )
        return 0;

    for (obj = IN_ROOM(actor)->contents; obj; obj = next) {
        next = obj->next_content;
        if (cmd_otrig(obj, actor, cmd, argument, OCMD_ROOM))
            return 1;
    }

    return 0;
}


int wear_otrigger(obj_data * obj, Character * actor, int wh) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    int ret_val;

    if (!(obj || actor)) {
        log("SYSERR:Invalid pointer passed to wear_otrigger.");
        return 0;
    }

    if (!SCRIPT_CHECK(obj, OTRIG_WEAR))
        return 1;

    for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
        if (TRIGGER_CHECK(t, OTRIG_WEAR)) {

            ADD_UID_VAR(buf, t, actor, "actor", 0);

            ret_val = script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);

            /* don't allow a wear to take place, if
             * the object is purged.
             */
            if (!obj)
                return -1;
            else
                return ret_val;
        }
    }

    return 1;
}


int remove_otrigger(obj_data * obj, Character * actor) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    int ret_val;

    if (!(obj && actor)) {
        log("SYSERR:Invalid pointer passed to remove_otrigger.");
        return 0;
    }

    if (!SCRIPT_CHECK(obj, OTRIG_REMOVE))
        return 1;

    for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
        if (TRIGGER_CHECK(t, OTRIG_REMOVE)) {

            ADD_UID_VAR(buf, t, actor, "actor", 0);

            ret_val = script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);

            /* don't allow a remove to take place, if
               the object is purged.
             */
            if (!obj)
                return -1;
            else
                return ret_val;
        }
    }

    return 1;
}


int drop_otrigger(obj_data * obj, Character * actor) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    int ret_val;

    if (!(obj && actor )) {
        log("SYSERR:Invalid pointer passed to drop_otrigger.");
        return 0;
    }

    if (!SCRIPT_CHECK(obj, OTRIG_DROP))
        return 1;

    for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
        if (TRIGGER_CHECK(t, OTRIG_DROP)
                && (number(1, 100) <= GET_TRIG_NARG(t))) {

            ADD_UID_VAR(buf, t, actor, "actor", 0);

            ret_val = script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);

            /* don't allow a drop to take place, if
             * the object is purged (nothing to drop).
             */
            if (!obj)
                return -1;
            else
                return ret_val;
        }
    }

    return 1;
}


int give_otrigger(obj_data * obj, Character * actor, Character * victim) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    int ret_val;

    if (!(obj && actor && victim)) {
        log("SYSERR:Invalid pointer passed to give_otrigger.");
        return 0;
    }


    if (!SCRIPT_CHECK(obj, OTRIG_GIVE))
        return 1;

    for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
        if (TRIGGER_CHECK(t, OTRIG_GIVE)
                && (number(1, 100) <= GET_TRIG_NARG(t))) {
            ADD_UID_VAR(buf, t, victim, "victim", 0);
            ADD_UID_VAR(buf, t, actor, "actor", 0);

            ret_val = script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);


            /* don't allow a give to take place, if
             * a) the object is purged.
             * b) the object is not carried by the giver.
             */
            if (!obj )
                return -1;
            else if (obj->carried_by != actor)
                return 0;
            else
                return ret_val;
        }
    }

    return 1;
}

int load_otrigger(obj_data * obj) {
    trig_data *t;

    if (!SCRIPT_CHECK(obj, OTRIG_LOAD))
        return 1;

    for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
        if (TRIGGER_CHECK(t, OTRIG_LOAD) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {


            script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);

            return obj ? 1 : -1;
        }
    }
    return 1;
}

int cast_otrigger(Character * actor, obj_data * obj, int spellnum) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (obj == NULL)
        return 1;

    if (!SCRIPT_CHECK(obj, OTRIG_CAST))
        return 1;

    for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
        if (TRIGGER_CHECK(t, OTRIG_CAST) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {


            ADD_UID_VAR(buf, t, actor, "actor", 0);
            snprintf(buf, sizeof(buf), "%d", spellnum);
            add_var(&GET_TRIG_VARS(t), "spell", buf, 0);
            add_var(&GET_TRIG_VARS(t), "spellname",(char *) skill_name(spellnum), 0);

            return script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);
        }
    }

    return 1;
}


/* checks for speech trigger on specific object. assumes obj has speech trig */
int speech_otrig(obj_data * obj, Character * actor, char *str, int type) {
    //    struct obj_data *obj, *obj_next;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    char *remove_percentage(char *string);
    int temp;

    if (obj && SCRIPT_CHECK(obj, OTRIG_SPEECH))
        for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
            if (!TRIGGER_CHECK(t, OTRIG_SPEECH))
                continue;
            /*IS_SET(GET_TRIG_NARG(t), type) &&*/
            if ( (!GET_TRIG_ARG(t) || !*GET_TRIG_ARG(t))) {
                new_mudlog(NRM, LVL_BUILDER, TRUE, "SYSERR: O-SPEECH Trigger #%d has no text argument!", GET_TRIG_VNUM(t));
                continue;
            }

            if (((GET_TRIG_NARG(t) && word_check(str, GET_TRIG_ARG(t))) ||
                    (!GET_TRIG_NARG(t)
                     && is_substring(GET_TRIG_ARG(t), str)))) {

                ADD_UID_VAR(buf, t, actor, "actor", 0);
                add_var(&GET_TRIG_VARS(t), "speech", remove_percentage(str), 0);

                temp =script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);

                return temp;
            }
        }

    return 0;
}


int speech_otrigger(Character * actor, char *str) {
    obj_data *obj;
    int i;

    for (i = 0; i < NUM_WEARS; i++)
        if (speech_otrig(GET_EQ(actor, i), actor, str, OCMD_EQUIP))
            return (1);

    for (obj = actor->carrying; obj; obj = obj->next_content)
        if (speech_otrig(obj, actor, str, OCMD_INVEN))
            return (1);

    if ( IN_ROOM ( actor ) == NULL )
        return 0;

    for (obj = IN_ROOM(actor)->contents; obj;
            obj = obj->next_content)
        if (speech_otrig(obj, actor, str, OCMD_ROOM))
            return (1);

    return (0);
}

int leave_otrigger(Room *room, Character *actor, int dir) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    int temp, final = 1;
    obj_data *obj, *obj_next;

    for (obj = room->contents; obj; obj = obj_next) {
        obj_next = obj->next_content;
        if (!SCRIPT_CHECK(obj, OTRIG_LEAVE))
            continue;

        for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
            if (TRIGGER_CHECK(t, OTRIG_LEAVE) &&
                    (number(1, 100) <= GET_TRIG_NARG(t))) {

                if (dir>=0 && dir < NUM_OF_DIRS)
                    add_var(&GET_TRIG_VARS(t), "direction", (char *)dirs[dir], 0);
                else
                    add_var(&GET_TRIG_VARS(t), "direction", "none", 0);
                ADD_UID_VAR(buf, t, actor, "actor", 0);

                temp = script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);

                if (temp == 0)
                    final = 0;
            }
        }
    }

    return final;
}
/* this needs to start a script running, and at the end of the script, call the assemble function - mord*/
int assemble_otrigger(obj_vnum lVnum, Character *ch, int subcmd, int cmd) {
    //  trig_data *t;
    //char buf[MAX_INPUT_LENGTH];
    ASSEMBLY     *pAssembly = NULL;
    int ret_val = 0;

    if( (pAssembly = assemblyGetAssemblyPtr( lVnum )) == NULL ) {
        log( "SYSERR: NULL assemble_otrigger(): Invalid 'lVnum' #%d.", lVnum );
        return (FALSE);
    }
   


    //ADD_UID_VAR(buf, t, ch, "actor", 0);
    /*switch (cmd)
    {
    case OCMD_EAT:
      add_var(&GET_TRIG_VARS(t), "command", "eat", 0);
      break;
    case OCMD_DRINK:
      add_var(&GET_TRIG_VARS(t), "command", "drink", 0);
      break;
    }*/

    //ret_val = script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);

    /* don't allow a wear to take place, if
     * the object is purged.
     */
    //if (!obj)
    //return -1;
    //else
    return ret_val;
}

int consume_otrigger(obj_data *obj, Character *actor, int cmd) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    int ret_val;

    if (!SCRIPT_CHECK(obj, OTRIG_CONSUME))
        return 1;

    for (t = TRIGGERS(SCRIPT(obj)); t; t = t->next) {
        if (TRIGGER_CHECK(t, OTRIG_CONSUME)) {

            ADD_UID_VAR(buf, t, actor, "actor", 0);
            switch (cmd) {
            case OCMD_EAT:
                add_var(&GET_TRIG_VARS(t), "command", "eat", 0);
                break;
            case OCMD_DRINK:
                add_var(&GET_TRIG_VARS(t), "command", "drink", 0);
                break;
            }

            ret_val = script_driver(&obj, t, OBJ_TRIGGER, TRIG_NEW);

            /* don't allow a wear to take place, if
             * the object is purged.
             */
            if (!obj)
                return -1;
            else
                return ret_val;
        }
    }

    return 1;
}

void time_otrigger(obj_data *obj) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    zone_rnum zone;
    obj_data *ob = obj;
	Room *r = NULL;

    for ( t = TRIGGERS ( SCRIPT ( obj ) ); t; t = t->next ) {
        if ( TRIGGER_CHECK ( t, OTRIG_TIME ) && time_info.hours == GET_TRIG_NARG ( t ) ) {
		if ( !TRIGGER_CHECK ( t, OTRIG_GLOBAL ) ) {
			if ( obj->in_obj )
				ob = obj->in_obj;

			r = obj_room ( ob );
			if ( !r )
				continue;
			zone = GET_ROOM_ZONE ( r );

			if ( zone_is_empty ( zone ) )
				continue;
		}
		snprintf ( buf, sizeof ( buf ), "%d", time_info.hours );
		add_var ( &GET_TRIG_VARS ( t ), "time", buf, 0 );
                script_driver ( &obj, t, OBJ_TRIGGER, TRIG_NEW );
	}
    }
}

int put_in_otrigger(obj_data *container, obj_data *obj, Character *actor) {
    trig_data *t;
    int ret_val;
    char buf[MAX_INPUT_LENGTH];

    if(!SCRIPT_CHECK(container, OTRIG_PUT_IN))
        return 1;

    for (t = TRIGGERS(SCRIPT(container)); t; t = t->next) {
        if(TRIGGER_CHECK(t, OTRIG_PUT_IN)) {
            ADD_UID_VAR(buf, t, obj, "object", 0);
            ADD_UID_VAR(buf, t, actor, "actor", 0);
            ret_val = script_driver(&container, t, OBJ_TRIGGER, TRIG_NEW);

            /* don't allow a put to take place, if
             * the object is purged (nothing to put).
             */
            if (!obj)
                return -1;
            else
                return ret_val;
        }
    }
    return 1;
}

int get_out_otrigger(obj_data *container, obj_data *obj, Character *actor) {
    trig_data *t;
    int ret_val;
    char buf[MAX_INPUT_LENGTH];

    if(!SCRIPT_CHECK(container, OTRIG_GET_OUT))
        return 1;

    for (t = TRIGGERS(SCRIPT(container)); t; t = t->next) {
        if(TRIGGER_CHECK(t, OTRIG_GET_OUT)) {
            ADD_UID_VAR(buf, t, obj, "object", 0);
            ADD_UID_VAR(buf, t, actor, "actor", 0);
            ret_val = script_driver(&container, t, OBJ_TRIGGER, TRIG_NEW);

            /* don't allow a put to take place, if
             * the object is purged (nothing to put).
             */
            if (!obj)
                return -1;
            else
                return ret_val;
        }
    }
    return 1;
}

//for dg_dest on objects, and maybe for animated objects and object teleport in the future.
int enter_otrigger(obj_data *object, const char* dir) {
    trig_data *t;
    int ret_val;

    if(!SCRIPT_CHECK(object, OTRIG_ENTER))
        return 1;

    for (t = TRIGGERS(SCRIPT(object)); t; t = t->next) {
        if(TRIGGER_CHECK(t, OTRIG_ENTER)) {
            if (number(1, 100) > GET_TRIG_NARG(t))
                return 1;

            add_var(&GET_TRIG_VARS(t), "direction", dir, 0);
            ret_val = script_driver(&object, t, OBJ_TRIGGER, TRIG_NEW);

            // don't move back if the object purged.
            if (!object)
                return 1;
            else
                return ret_val;
        }
    }
    return 1;
}

/*
 *  world triggers
 */

void reset_wtrigger(Room * room) {
    trig_data *t;

    if (!SCRIPT_CHECK(room, WTRIG_RESET))
        return;

    for (t = TRIGGERS(SCRIPT(room)); t; t = t->next) {
        if (TRIGGER_CHECK(t, WTRIG_RESET) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {



            script_driver(&room, t, WLD_TRIGGER, TRIG_NEW);
            break;
        }
    }
}

void random_wtrigger(Room *room) {
    trig_data *t;

    for ( t = TRIGGERS ( SCRIPT ( room ) ); t; t = t->next )
        if ( TRIGGER_CHECK ( t, WTRIG_RANDOM ) && number ( 1, 100 ) <= GET_TRIG_NARG ( t ) &&
	   ( TRIGGER_CHECK ( t, WTRIG_GLOBAL ) || !zone_is_empty ( GET_ROOM_ZONE ( room ) ) ) )
                script_driver ( &room, t, WLD_TRIGGER, TRIG_NEW );
}


int enter_wtrigger(Room *room, Character * actor, int dir) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (!SCRIPT_CHECK(room, WTRIG_ENTER))
        return 1;

    for (t = TRIGGERS(SCRIPT(room)); t; t = t->next) {
        if (TRIGGER_CHECK(t, WTRIG_ENTER) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {

            if (dir>=0 && dir < NUM_OF_DIRS)
                add_var(&GET_TRIG_VARS(t), "direction",
                        (char *) dirs[rev_dir[dir]], 0);
            else
                add_var(&GET_TRIG_VARS(t), "direction", "none", 0);
            ADD_UID_VAR(buf, t, actor, "actor", 0);


            return script_driver(&room, t, WLD_TRIGGER, TRIG_NEW);
        }
    }

    return 1;
}


int command_wtrigger(Character * actor, char *cmd, char *argument) {
    Room *room;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    char *remove_percentage(char *string);
    string argument_copy;

    /* prevent people we like from becoming trapped :P */
    if (!valid_dg_target(actor, FALSE))
        return 0;

    if (!actor || !IN_ROOM(actor) || !SCRIPT_CHECK(IN_ROOM(actor), WTRIG_COMMAND))
        return 0;

    room = IN_ROOM(actor);
    for (t = TRIGGERS(SCRIPT(room)); t; t = t->next) {
        if (!TRIGGER_CHECK(t, WTRIG_COMMAND))
            continue;

        if (!GET_TRIG_ARG(t) || !*GET_TRIG_ARG(t)) {
            sprintf(buf,
                    "SYSERR: W-Command Trigger #%d has no text argument!",
                    GET_TRIG_VNUM(t));
            mudlog(buf, NRM, LVL_BUILDER, TRUE);
            continue;
        }

        if (*GET_TRIG_ARG(t) == '*' ||
                !strn_cmp(GET_TRIG_ARG(t), cmd, strlen(GET_TRIG_ARG(t)))) {

            argument_copy = string ( argument );
            ADD_UID_VAR(buf, t, actor, "actor", 0);
            while ( argument_copy != "" && argument_copy[0] == ' ' )
		argument_copy.erase ( argument_copy.begin() );
            add_var(&GET_TRIG_VARS(t), "arg", remove_percentage ( (char*) argument_copy.c_str() ), 0);
            skip_spaces(&cmd);
            add_var(&GET_TRIG_VARS(t), "cmd", cmd, 0);

            if ( script_driver(&room, t, WLD_TRIGGER, TRIG_NEW) )
                return 1;

        }
    }

    return 0;
}


void speech_wtrigger(Character * actor, char *str) {
    Room *room;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    char *remove_percentage(char *string);

    if (!actor || !SCRIPT_CHECK(IN_ROOM(actor), WTRIG_SPEECH))
        return;

    room = IN_ROOM(actor);
    for (t = TRIGGERS(SCRIPT(room)); t; t = t->next) {
        if (!TRIGGER_CHECK(t, WTRIG_SPEECH))
            continue;

        if (!GET_TRIG_ARG(t) || !*GET_TRIG_ARG(t)) {
            sprintf(buf,
                    "SYSERR: W-Speech Trigger #%d has no text argument!",
                    GET_TRIG_VNUM(t));
            mudlog(buf, NRM, LVL_BUILDER, TRUE);
            continue;
        }

        if (*GET_TRIG_ARG(t) == '*' ||
                (GET_TRIG_NARG(t) && word_check(str, GET_TRIG_ARG(t))) ||
                (!GET_TRIG_NARG(t) && is_substring(GET_TRIG_ARG(t), str))) {

            ADD_UID_VAR(buf, t, actor, "actor", 0);
            add_var(&GET_TRIG_VARS(t), "speech", remove_percentage(str), 0);

            script_driver(&room, t, WLD_TRIGGER, TRIG_NEW);
            break;
        }
    }
}

int drop_wtrigger(obj_data * obj, Character * actor) {
    Room *room;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    int ret_val;

    if (!actor || !SCRIPT_CHECK(IN_ROOM(actor), WTRIG_DROP))
        return 1;

    room = IN_ROOM(actor);
    for (t = TRIGGERS(SCRIPT(room)); t; t = t->next)
        if (TRIGGER_CHECK(t, WTRIG_DROP) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {


            ADD_UID_VAR(buf, t, actor, "actor", 0);
            ADD_UID_VAR(buf, t, obj, "object", 0);

            ret_val = script_driver(&room, t, WLD_TRIGGER, TRIG_NEW);
            if (!obj)
                return -1;
            if (obj->carried_by != actor)
                return 0;
            else
                return ret_val;
            break;
        }

    return 1;
}


int cast_wtrigger(Character * actor, Character * vict, obj_data * obj,
                  int spellnum) {
    Room *room;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (!actor || !SCRIPT_CHECK(IN_ROOM(actor), WTRIG_CAST))
        return 1;

    room = IN_ROOM(actor);
    for (t = TRIGGERS(SCRIPT(room)); t; t = t->next) {
        if (TRIGGER_CHECK(t, WTRIG_CAST) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {


            ADD_UID_VAR(buf, t, actor, "actor", 0);
            if (vict)
                ADD_UID_VAR(buf, t, vict, "victim", 0);
            if (obj)
                ADD_UID_VAR(buf, t, obj, "object", 0);
            sprintf(buf, "%d", spellnum);
            add_var(&GET_TRIG_VARS(t), "spell", buf, 0);
            add_var(&GET_TRIG_VARS(t), "spellname",
                    (char *) skill_name(spellnum), 0);

            return script_driver(&room, t, WLD_TRIGGER, TRIG_NEW);
        }
    }

    return 1;
}


int leave_wtrigger(Room *room, Character * actor, int dir) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (!SCRIPT_CHECK(room, WTRIG_LEAVE))
        return 1;

    for (t = TRIGGERS(SCRIPT(room)); t; t = t->next) {
        if (TRIGGER_CHECK(t, WTRIG_LEAVE) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {

            if (dir>=0 && dir < NUM_OF_DIRS)
                add_var(&GET_TRIG_VARS(t), "direction", (char *) dirs[dir], 0);
            else
                add_var(&GET_TRIG_VARS(t), "direction", "none", 0);
            ADD_UID_VAR(buf, t, actor, "actor", 0);

            return script_driver(&room, t, WLD_TRIGGER, TRIG_NEW);
        }
    }

    return 1;
}

int door_wtrigger(Character * actor, int subcmd, int dir) {
    Room *room;
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    if (!actor || !SCRIPT_CHECK(IN_ROOM(actor), WTRIG_DOOR))
        return 1;

    room = IN_ROOM(actor);
    for (t = TRIGGERS(SCRIPT(room)); t; t = t->next) {
        if (TRIGGER_CHECK(t, WTRIG_DOOR) &&
                (number(1, 100) <= GET_TRIG_NARG(t))) {

            add_var(&GET_TRIG_VARS(t), "cmd", (char *) cmd_door[subcmd],
                    0);
            if (dir>=0 && dir < NUM_OF_DIRS)
                add_var(&GET_TRIG_VARS(t), "direction", (char *) dirs[dir], 0);
            else
                add_var(&GET_TRIG_VARS(t), "direction", "none", 0);
            ADD_UID_VAR(buf, t, actor, "actor", 0);

            return script_driver(&room, t, WLD_TRIGGER, TRIG_NEW);
        }
    }

    return 1;
}


void time_wtrigger(Room *room) {
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];

    for ( t = TRIGGERS ( SCRIPT ( room ) ); t; t = t->next ) {
        if ( TRIGGER_CHECK ( t, WTRIG_TIME ) && time_info.hours == GET_TRIG_NARG ( t ) &&
	   ( TRIGGER_CHECK ( t, WTRIG_GLOBAL ) || !zone_is_empty ( GET_ROOM_ZONE ( room ) ) ) ) {
		snprintf ( buf, sizeof ( buf ), "%d", time_info.hours );
		add_var ( &GET_TRIG_VARS ( t ), "time", buf, 0 );
		script_driver ( &room, t, WLD_TRIGGER, TRIG_NEW );
        }
    }
}

