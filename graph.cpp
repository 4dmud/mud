/* ************************************************************************
*   File: graph.c                                       Part of CircleMUD *
*  Usage: various graph algorithms                                        *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "constants.h"
#include "graph.h"


/* Externals */
ACMD(do_say);
/* External functions */
void improve_skill(Character *ch, int skill);

/* Local functions */
ACMD(do_track);
void hunt_victim(Character *ch);

Graph graph;



int Graph::VALID_EDGE(room_rnum x, int y,bool honour_notrack) {
    if (x->dir_option[y] == NULL || TOROOM(x, y) == NULL)
        return (0);
    if (CONFIG_TRACK_T_DOORS == FALSE && IS_CLOSED(x, y))
        return (0);
    if ((ROOM_FLAGGED(TOROOM(x, y), ROOM_NOTRACK) && honour_notrack)
            || IS_MARKED(TOROOM(x, y)))
        return (0);

    return (1);
}

void Graph::bfs_enqueue(room_rnum room, int dir) {
	struct bfs_queue_struct *curr = new bfs_queue_struct(room, dir);

    if (queue_tail) {
        queue_tail->next = curr;
        queue_tail = curr;
    } else
        queue_head = queue_tail = curr;
    
}


void Graph::bfs_dequeue(void) {
    struct bfs_queue_struct *curr;

    curr = queue_head;

    if (!(queue_head = queue_head->next))
        queue_tail = 0;
    delete curr;
}


void Graph::bfs_clear_queue(void) {
	/*std::list <bfs_queue_struct *>::iterator it;
	for (it = bfs_queue.begin();it != bfs_queue.end();it++) {
		delete (*it);
		*it = NULL;
	}
	bfs_queue.clear();*/
    while (queue_head)
       bfs_dequeue();
}


/*
 * find_first_step: given a source room and a target room, find the first
 * step on the shortest path from the source to the target.
 *
 * Intended usage: in mobile_activity, give a mob a dir to go if they're
 * tracking another mob or a PC.  Or, a 'track' skill for PCs.
 */
int Graph::find_first_step(room_rnum src, room_rnum target,bool honour_notrack) {
    int curr_dir;
    if (!src || !target) {	    
	    return (BFS_ERROR);
    }

    if (src->number < 0 || src->number > top_of_world || target->number < 0
            || target->number > top_of_world) {
        log("SYSERR: Illegal value %d or %d passed to find_first_step. (%s)", src->number, target->number, __FILE__);
        return (BFS_ERROR);
    }
    if (src == target)
        return (BFS_ALREADY_THERE);

    /* clear marks first, some OLC systems will save the mark. */ 
    for (int curr_room = 0; curr_room <= top_of_world; curr_room++)
        if (world_vnum[curr_room])
    UNMARK(world_vnum[curr_room]);

    MARK(src);

    /* first, enqueue the first steps, saving which direction we're going. */
    for (curr_dir = 0; curr_dir < NUM_OF_DIRS; curr_dir++)
        if (VALID_EDGE(src, curr_dir,honour_notrack)) {
            MARK(TOROOM(src, curr_dir));
            bfs_enqueue(TOROOM(src, curr_dir), curr_dir);
        }

    /* now, do the classic BFS. */
    while (queue_head) {
        if (queue_head->room == target) {
            curr_dir = queue_head->dir;
            bfs_clear_queue();
            return (curr_dir);
        } else {
            for (curr_dir = 0; curr_dir < NUM_OF_DIRS; curr_dir++)
                if (VALID_EDGE(queue_head->room, curr_dir,honour_notrack)) {
                    MARK(TOROOM(queue_head->room, curr_dir));
                    bfs_enqueue(TOROOM(queue_head->room, curr_dir),
                                queue_head->dir);
                }
            bfs_dequeue();
        }
    }

    return (BFS_NO_PATH);
}


/********************************************************
* Functions and Commands which use the above functions. *
********************************************************/

ACMD(do_track) {
    Character *vict;
    int dir;
    char arg[MAX_INPUT_LENGTH];

    /* The character must have the track skill. */
    if (IS_NPC(ch) || !total_chance(ch, SKILL_TRACK)) {
        send_to_char("You have no idea how.\r\n", ch);
        return;
    }
    one_argument(argument, arg);
    if (!*arg) {
        send_to_char("Whom are you trying to track?\r\n", ch);
        return;
    }
    /* The person can't see the victim. */
    if (!(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_WORLD))) {
        send_to_char("No one is around by that name.\r\n", ch);
        return;
    }
    /* We can't track the victim. */
    if (AFF_FLAGGED(vict, AFF_NOTRACK)) {
        send_to_char("You sense no trail.\r\n", ch);
        return;
    }

    /* 101 is a complete failure, no matter what the proficiency. */
    if (number(0, 101) >= total_chance(ch, SKILL_TRACK)) {
        int tries = 10;
        /* Find a random direction. :) */
        do {
            dir = number(0, NUM_OF_DIRS - 1);
        } while (!CAN_GO(ch, dir) && --tries);
        ch->Send( "You sense a trail %s from here!\r\n", dirs[dir]);
        improve_skill(ch, SKILL_TRACK);
        return;
    }

    /* They passed the skill check. */
    dir = graph.find_first_step(ch->in_room, vict->in_room);

    switch (dir) {
    case BFS_ERROR:
        send_to_char("Hmm.. something seems to be wrong.\r\n", ch);
        break;
    case BFS_ALREADY_THERE:
        send_to_char("You're already in the same room!!\r\n", ch);
        break;
    case BFS_NO_PATH:
        ch->Send( "You can't sense a trail to %s from here.\r\n",HMHR(vict));
        break;
    default:			/* Success! */
        ch->Send( "You sense a trail %s from here!\r\n", dirs[dir]);
        break;
    }
}


void hunt_victim(Character *ch) {
    int dir;
    sbyte found;
    Character *tmp;
    char tbuf[MAX_STRING_LENGTH];

    if (!ch || !HUNTING(ch) || FIGHTING(ch) || ch == HUNTING(ch))
        return;

    if (HUNT_COUNT(ch) > 0)
        HUNT_COUNT(ch)--;
    else {
        HUNTING(ch) = NULL;
        remove_hunter(ch);
    }

    /* make sure the char still exists */
    for (found = FALSE, tmp = character_list; tmp && !found; tmp = tmp->next)
        if (HUNTING(ch) == tmp)
            found = TRUE;

    if (!found) {
        do_say(ch, (char *)"Damn! My prey is gone!!", 0, 0);
        HUNTING(ch) = NULL;
        remove_hunter(ch);
        return;
    }
    if ( IN_ROOM ( ch ) && ((dir = graph.find_first_step(IN_ROOM(ch), IN_ROOM(HUNTING(ch)),false)) < 0 || dir > 5 ||
	ROOM_FLAGGED(IN_ROOM(ch)->dir_option[dir]->to_room, ROOM_NOMOB))) {
        if (dir != BFS_ALREADY_THERE) {
	    if (!IS_SET(INTERNAL_FLAGS(ch), INT_LOSTPREY)) {
                snprintf(tbuf, sizeof(tbuf), "Damn! I lost %s!", HMHR(HUNTING(ch)));
                do_say(ch, tbuf, 0, 0);
	        //now that the world knows that, we don't have to repeat it all the time.
		//Continue hunting though.
	        SET_BIT(INTERNAL_FLAGS(ch), INT_LOSTPREY);
	    }
        } else {
            HUNTING(ch) = NULL;
            remove_hunter(ch);
        }
    } else {
        if (IN_ROOM(ch) && HUNTING(ch) && IN_ROOM(HUNTING(ch)) && IN_ROOM(HUNTING(ch))->zone == IN_ROOM(ch)->zone) {
	    REMOVE_BIT(INTERNAL_FLAGS(ch), INT_LOSTPREY);
            switch (number(0, 20)) {
            case 0:
                HUNTING(ch)->Send("You sense something nearby, coming for you.\r\n");
                break;
            case 1:
                HUNTING(ch)->Send("You see an unfriendly face in the distance.\r\n");
                break;
            case 2:
                HUNTING(ch)->Send("You have a nasty feeling of danger lurking about.\r\n");
                break;
            }


        }
        perform_move(ch, dir, 1);
    }
    return;
}
