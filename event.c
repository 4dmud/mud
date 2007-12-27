#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "event.h"
#include "utils.h"
#include "comm.h"
#include "handler.h"
#include "db.h"
#include "dg_scripts.h"

int in_event_handler = 0;
struct event_info2 *pending_events = NULL;
struct event_info2 *prev = NULL;

void run_events2(void)
{
    struct event_info2 *temp, *prox;

    in_event_handler = 1;

    prev = NULL;
    for (temp = pending_events; temp; temp = prox) {
	prox = temp->next;
	temp->ticks_to_go--;
	if (temp->ticks_to_go == 0) {

	    /* run the event */
	    if (!temp->func)
		log("SYSERR: Attempting to run a NULL event. Ignoring");
	    else
		(temp->func) (temp->causer, temp->victim,
			      (long) temp->info);

	    /* remove the event from the list. */
	    if (!prev)
		pending_events = prox;
	    else
		prev->next = prox;
	    free(temp);
	} else if (temp->ticks_to_go == -1) {
	    if (!prev)
		pending_events = prox;
	    else
		prev->next = prox;
	    free(temp);
	} else
	    prev = temp;
    }

    in_event_handler = 0;
};

void add_event2(int delay, EVENT2(*func), void *causer, void *victim,
		void *info)
{
    struct event_info2 *current;

    CREATE(current, struct event_info2, 1);

    current->ticks_to_go = delay;
    current->func = func;
    current->causer = causer;
    current->victim = victim;
    current->info = (long *) info;

    current->next = pending_events;
    pending_events = current;
    if (in_event_handler && !prev)
	prev = pending_events;
};

void clean_events2(void *pointer)
{
    struct event_info2 *temp, *prox;
    struct event_info2 *previous;

    if (in_event_handler)
	log("SYSERR: Trying to remove events inside the handler. Attempting to continue.");
    previous = NULL;
    for (temp = pending_events; temp; temp = prox) {
	prox = temp->next;
	if (temp->causer == pointer || temp->victim == pointer ||
	    (void *) (temp->func) == pointer)
	    temp->ticks_to_go = 0;
    }
};
