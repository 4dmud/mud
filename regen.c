/* ************************************************************************
*  File: regen.c                                                          *
*                                                                         *
*  Usage: Contains routines to handle event based point regeneration      *
*                                                                         *
*  Written by Eric Green (ejg3@cornell.edu)                               *
************************************************************************ */
/*
Circle 3.0 Event Based Regeneration Patch 1.0
=============================================

First release, 9 March 1998

This patch implements event based regeneration for CircleMUD.  This makes it
so each point type (hit, mana, or move) is regenerated one point at a time,
rather than having one big jump once per tick.  The rate of regeneration is
still the same with this patch, it just does it gradually in order to make
the game run "smoother" and to defeat people with tick counters who sleep
right before the tick.

This patch does not add as much processor overhead as one might think at
first glance.  Stock CircleMUD recalculates the regeneration of every
character in the game (mobs and players) once per tick.  This patch changes
regeneration so only the characters whose points are less than their maximum
are checked.  Even with multiple checks per tick, if most characters are at
their full point level, the over head could be lower than with stock Circle.


Installation
------------
   1.  Install DG Events (version 1.1 or later), following the instructions
       provided with that patch.
   2.  Copy the regen.c file to your circle/src directory.
   3.  In your circle directory (not circle/src), run
       "patch -p1 < regen.patch".
   4.  Rerun configure (to update your Makefile).
   5.  If your mud has been modified, see the "Using" section below and make
       any necessary changes.
   5.  Recompile your MUD.

  The patch is based on Circle 3.0 pl12, so you might have to do some patching
  by hand if you have modified your source or are using it on a different
  version.  This has only been testing on Linux, but hopefully will work on
  all systems unmodified.  Please let me know if you have to make any changes
  for it to work on your system.


Using
-----
  Stock CircleMUD allows a character's points to be altered by directly
  manipulating the points variable, such as by setting GET_HIT(ch) or
  ch->points.hit.  When you use this patch, any changes must be done using
  the functions:

      void alter_hit(struct char_data *ch, int amount);
      void alter_mana(struct char_data *ch, int amount);
      void alter_move(struct char_data *ch, int amount);

  The first argument is the character to alter.  The second argument is
  the number of points to subtract from the points.  To add points, use
  a negative amount.  If the points variable must be manipulated directly,
  call the appropriate alter function with amount of 0 to make sure there
  are the proper regeneration events.  Remember to do this when increasing
  the maximum value also.

  Another function is provided to update regeneration on all three points
  types:

      void check_regen_rates(struct char_data *ch);

  This will make sure that any point category below maximum has an event
  to regenerate it.  It also will recalculate the regeneration rate if
  the rate has been increased.  It should be called whenever there is a
  significant change in regeneration rates, or if you want to check that all
  three points types have the proper regen events.


Possible Problems
-----------------
  If the mud fails to compile with this patch due to errors like:

    fight.o(.text+0xaf8): undefined reference to `alter_hit'
    handler.o(.text+0x0bc): undefined reference to `check_regen_rates'

  then you did not properly update your Makefile, either by rerunning
  configure, or copying over the Makefile.win to Makefile.  regen.o
  must be added to OBJFILES in order for it to compile properly.

  If you get an error compiling about undefined references to the functions
  event_create, event_time, and event_cancel, you did not properly install
  the dgevents patch.  Refer to the documentation with that package to
  check your installation.


  If a player stops regenerating properly, you probably forgot to use an
  alter_xxx() call and instead manipulated the variable directly.  You
  need to track down where this occurred and use the alter_xxx() or
  check_regen_rates().


  Warning:  Once you get used to event based regen, it is hard playing on
  a mud using ticks.


Location
--------
  The latest version of this regen patch should be available from the
  CircleMUD ftp site (ftp://ftp.circlemud.org/pub/CircleMUD/contrib/code).
  The DG Events patch can be found in the same location, named dgevents.


If you find a problem with this package, please email me.


Eric Green
ejg3@cornell.edu
http://www.imaxx.net/~thrytis

*/

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "interpreter.h"
#include "utils.h"
#include "spells.h"
#include "handler.h"
#include "comm.h"
#include "dg_event.h"		/* For modern (pl8+) DG_SCRIPTS packages */


/* Player point types for events */
#define REGEN_HIT      0
#define REGEN_MANA     1
#define REGEN_MOVE     2
#define REGEN_STAMINA  3

#define PULSES_PER_MUD_HOUR     (SECS_PER_MUD_HOUR*PASSES_PER_SEC)

extern struct char_data *find_char(long n);

/* event object structure for point regen */
struct regen_event_obj {
    struct char_data *ch;	/* character regening */
    int type;			/* HIT, MOVE, or MANA */
};


EVENTFUNC(points_event)
{
    struct regen_event_obj *regen = (struct regen_event_obj *) event_obj;
    struct char_data *ch = regen->ch;
    int type, gain;
    

    type = regen->type;


	if (ch != NULL && GET_POS(ch) >= POS_STUNNED && IN_ROOM(ch) != NULL) {
	    /* no help for the dying */

	    /*
	     * Increment type of points by one.
	     * If not at max, reenqueue the event.
	     */

	    switch (type) {
	    case REGEN_HIT:
		GET_HIT(ch) = MIN(GET_HIT(ch) + 3, GET_MAX_HIT(ch));

		if (GET_POS(ch) <= POS_STUNNED)
		    update_pos(ch);

		if (GET_HIT(ch) < GET_MAX_HIT(ch)) {
		    /* reenqueue the event */
		    gain = (hit_gain(ch) * 0.333);
		    return IRANGE(1, (PULSES_PER_MUD_HOUR / (gain ? gain : 1)), PULSES_PER_MUD_HOUR);
		}
		break;

	    case REGEN_MANA:
		GET_MANA(ch) = MIN(GET_MANA(ch) + 3, GET_MAX_MANA(ch));

		if (GET_MANA(ch) < GET_MAX_MANA(ch)) {
		    /* reenqueue the event */
		    gain = (mana_gain(ch) * 0.333);
		    return IRANGE(1,(PULSES_PER_MUD_HOUR / (gain ? gain : 1)), PULSES_PER_MUD_HOUR);
		}
		break;

	    case REGEN_MOVE:
		GET_MOVE(ch) = MIN(GET_MOVE(ch) + 3, GET_MAX_MOVE(ch));

		if (GET_MOVE(ch) < GET_MAX_MOVE(ch)) {
		    /* reenqueue the event */
		    gain = (move_gain(ch) * 0.333);
		    return IRANGE(1,(PULSES_PER_MUD_HOUR / (gain ? gain : 1)), PULSES_PER_MUD_HOUR);
		}
		break;
           case REGEN_STAMINA:
		GET_STAMINA(ch) = MIN(GET_STAMINA(ch) + 1, GET_MAX_STAMINA(ch));
		
		if (GET_STAMINA(ch) < GET_MAX_STAMINA(ch)) {
		if (ch->desc && GET_STAMINA(ch) * 20 < GET_MAX_STAMINA(ch))
		new_send_to_char(ch, "You pant heavily, deep and fast.\r\n");
		
		    /* reenqueue the event */
		    gain = stamina_gain(ch);
		    return IRANGE(1, (PULSES_PER_MUD_HOUR / (gain ? gain : 1)), PULSES_PER_MUD_HOUR);
		} else {
		if (IS_NPC(ch) && GET_POS(ch) == POS_RESTING) {
		GET_POS(ch) = POS_STANDING;
		act("$n climbs happily to $s feet.", FALSE, ch, 0, 0, TO_ROOM);
		}
		}
		break;
	    default:
		log("SYSERR:  Unknown points event type %d",type);
	    }
	}
    
    /* kill this event */
    if (ch != NULL) 
    GET_POINTS_EVENT(ch, type) = NULL;
    
    if (event_obj) 
    free(event_obj);
    
    return 0;
}


/*
 * subtracts amount of hitpoints from ch's current and starts points event
 */
void alter_hit(struct char_data *ch, int amount)
{

    GET_HIT(ch) = MIN(GET_HIT(ch) - amount, GET_MAX_HIT(ch));
    
    /* take off old event, create updated event */
	    if (GET_POINTS_EVENT(ch, REGEN_HIT) != NULL)
		event_cancel(GET_POINTS_EVENT(ch, REGEN_HIT));
		GET_POINTS_EVENT(ch, REGEN_HIT) = NULL;

    if (GET_HIT(ch) <= HIT_INCAP)
	return;

    if (GET_HIT(ch) < GET_MAX_HIT(ch) && GET_POINTS_EVENT(ch, REGEN_HIT) == NULL) {
        struct regen_event_obj *regen = NULL;
    long time;
    int gain;
	CREATE(regen, struct regen_event_obj, 1);
	regen->ch = ch;
	regen->type = REGEN_HIT;
	gain = (hit_gain(ch));
	time = PULSES_PER_MUD_HOUR / (gain ? gain : 1);
	GET_POINTS_EVENT(ch, REGEN_HIT) = event_create(points_event, regen, time);
	if (amount >= 0) {

	    /*
	     * if the character gained hp, update position and
	     * restart mana and move regeneration if needed.
	     */
	    update_pos(ch);
	    alter_mana(ch, 0);
	    alter_move(ch, 0);
	}
    }
}


/*
 * subtracts amount of mana from ch's current and starts points event
 */
void alter_mana(struct char_data *ch, int amount)
{


    GET_MANA(ch) = MIN(GET_MANA(ch) - amount, GET_MAX_MANA(ch));
    /* take off old event, create updated event */
	    if (GET_POINTS_EVENT(ch, REGEN_STAMINA) != NULL)
		event_cancel(GET_POINTS_EVENT(ch, REGEN_STAMINA));
		GET_POINTS_EVENT(ch, REGEN_STAMINA) = NULL;

    if (!GET_POINTS_EVENT(ch, REGEN_MANA)
	&& (GET_MANA(ch) < GET_MAX_MANA(ch))) {

	/* make sure the character isn't dying */
	if (GET_POS(ch) >= POS_STUNNED) {
	    struct regen_event_obj *regen = NULL;
    long time;
    int gain;
	    CREATE(regen, struct regen_event_obj, 1);
	    regen->ch = ch;
	    regen->type = REGEN_MANA;
	    gain = (mana_gain(ch));
	    time = PULSES_PER_MUD_HOUR / (gain ? gain : 1);
	    GET_POINTS_EVENT(ch, REGEN_MANA) = event_create(points_event, regen, time);
	}
    }
}


/*
 * subtracts amount of moves from ch's current and starts points event
 */
void alter_move(struct char_data *ch, int amount)
{


    GET_MOVE(ch) = MIN(GET_MOVE(ch) - amount, GET_MAX_MOVE(ch));
    
    /* take off old event, create updated event */
	    if (GET_POINTS_EVENT(ch, REGEN_MOVE) != NULL)
		event_cancel(GET_POINTS_EVENT(ch, REGEN_MOVE));
		GET_POINTS_EVENT(ch, REGEN_MOVE) = NULL;

    if (!GET_POINTS_EVENT(ch, REGEN_MOVE)
	&& (GET_MOVE(ch) < GET_MAX_MOVE(ch))) {

	/* make sure the character isn't dying */
	if (GET_POS(ch) >= POS_STUNNED) {
	    struct regen_event_obj *regen = NULL;
    long time;
    int gain;
	    CREATE(regen, struct regen_event_obj, 1);
	    regen->ch = ch;
	    regen->type = REGEN_MOVE;
	    gain = (move_gain(ch));

	    time = PULSES_PER_MUD_HOUR / (gain ? gain : 1);
	    GET_POINTS_EVENT(ch, REGEN_MOVE) =
		event_create(points_event, regen, time);
	}
    }
}

/*
 * subtracts amount of moves from ch's current and starts points event
 */
void alter_stamina(struct char_data *ch, int amount)
{
;

    GET_STAMINA(ch) = MIN(GET_STAMINA(ch) - amount, GET_MAX_STAMINA(ch));
    
        /* take off old event, create updated event */
	    if (GET_POINTS_EVENT(ch, REGEN_STAMINA) != NULL)
		event_cancel(GET_POINTS_EVENT(ch, REGEN_STAMINA));
		GET_POINTS_EVENT(ch, REGEN_STAMINA) = NULL;

    if (!GET_POINTS_EVENT(ch, REGEN_STAMINA)
	&& (GET_STAMINA(ch) < GET_MAX_STAMINA(ch))&& IN_ROOM(ch) != NULL) {

	/* make sure the character isn't dying */
	if (GET_POS(ch) >= POS_STUNNED) {
	    struct regen_event_obj *regen = NULL;
    long time;
    int gain;
	    CREATE(regen, struct regen_event_obj, 1);
	    regen->ch = ch;
	    regen->type = REGEN_STAMINA;
	    gain = (stamina_gain(ch));

	    time = PULSES_PER_MUD_HOUR / (gain ? gain : 1);
	    GET_POINTS_EVENT(ch, REGEN_STAMINA) =
		event_create(points_event, regen, time);
	}
    } 
    if (GET_STAMINA(ch) > (10 - GET_MAX_STAMINA(ch))) {
    if (IS_NPC(ch) && GET_POS(ch) == POS_RESTING) {
		GET_POS(ch) = POS_STANDING;
		act("$n climbs happily to $s feet.", FALSE, ch, 0, 0, TO_ROOM);
		}
    }
}

/* updates regen rates.  Use when big regen rate changes are made */
void check_regen_rates(struct char_data *ch)
{
    struct regen_event_obj *regen;
    int type, gain = 0;
    long time;

    if (ch == NULL || GET_HIT(ch) <= HIT_INCAP)
	return;

    for (type = REGEN_HIT; type <= REGEN_STAMINA; type++) {

	switch (type) {
	case REGEN_HIT:
	    if (GET_HIT(ch) >= GET_MAX_HIT(ch))
		continue;
	    gain = hit_gain(ch);
	    break;

	case REGEN_MANA:
	    if (GET_MANA(ch) >= GET_MAX_MANA(ch))
		continue;
	    gain = mana_gain(ch);
	    break;

	case REGEN_MOVE:
	    if (GET_MOVE(ch) >= GET_MAX_MOVE(ch))
		continue;
	    gain = move_gain(ch);
	    break;
	case REGEN_STAMINA:
	    if (GET_STAMINA(ch) >= GET_MAX_STAMINA(ch))
		continue;
	    gain = stamina_gain(ch);
	    break;
	}

	time = PULSES_PER_MUD_HOUR / (gain > 0 ? gain : 1);

	if (GET_POINTS_EVENT(ch, type) == NULL ||
	    (time < event_time(GET_POINTS_EVENT(ch, type)))) {

	    /* take off old event, create updated event */
	    if (GET_POINTS_EVENT(ch, type) != NULL)
		event_cancel(GET_POINTS_EVENT(ch, type));
		GET_POINTS_EVENT(ch, type) = NULL;
		
	    CREATE(regen, struct regen_event_obj, 1);
	    regen->ch = ch;
	    regen->type = type;
	    GET_POINTS_EVENT(ch, type) =
		event_create(points_event, regen, time);
	}
    }
}
