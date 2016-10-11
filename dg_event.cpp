/*
* dg_event.c: This file contains a simplified event system to allow
 * DG Script triggers to use the "wait" command, causing a delay in the
 * middle of a script.
 *
 * By: Mark A. Heilpern (Sammy @ eQuoria MUD   equoria.com:4000)
 *
 * As of dg scripts pl 8 this includes the 'FULL' DG event package.
 * This file includes the file queue.c, which handles the priority queues.
 * Thomas Arp - Welcor - 2002
 *
 */


#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "db.h"
#include "utils.h"
#include "dg_event.h"
#include "dg_scripts.h"
#include "constants.h"
#include "comm.h"
#include "regen.h"

struct dgqueue *event_q = NULL;      /* the event queue */
EVENTFUNC ( message_event );

extern unsigned long pulse;


/* initializes the event queue */
void event_init(void) {
    event_q = new dgqueue();
}

/*
** Add an event to the current list
*/
/* creates an event and returns it */
struct event *event_create(EVENTFUNC(*func), void *event_obj, long when, int type) {
    if (when < 1)          /* make sure its in the future */
        when = 1;
    struct event *new_event = new event(func, event_obj, type);
    new_event->q_el = queue_enq(event_q, new_event, when + pulse);
    return new_event;
}


/* removes the event from the system */
void event_cancel(struct event *event) {
    if (!event) {
        log("SYSERR:  Attempted to cancel a NULL event");
        ALERT_2;
        core_dump();
        abort();
        return;
    }

    if (!event->q_el) {
        /** this happens when the event is being canceled within the event function **/
        log("SYSERR: (dg_events) Attempting to cancel a non-NULL unqueued event!!");
        //ALERT_2;
        return;
    } else
        queue_deq(event_q, event->q_el);

    if (event->event_obj) {
        switch (event->type) {
        case EVENT_TYPE_MESSAGE:
            delete (struct message_event_obj *)event->event_obj;
            break;
        case EVENT_TYPE_FIGHT:
            delete (struct fight_event_obj *)event->event_obj;
            break;
        case EVENT_TYPE_REGEN:
            delete (struct regen_event_obj *)event->event_obj;
            break;
        case EVENT_TYPE_DAMAGE:
            delete (struct aff_dam_event_obj *)event->event_obj;
            break;
        case EVENT_TYPE_TRIG:
            delete (struct wait_event_data *)event->event_obj;
            break;
        case EVENT_TYPE_TIMER:
            delete (struct timer_event_data *)event->event_obj;
            break;
        default:
            free(event->event_obj);
            break;
        }
        delete event;
    }
}


/* Process any events whose time has come. */
void event_process(void) {
    struct event *the_event = NULL;
    long new_time;

    while (pulse >= queue_key(event_q)) {
        if (!(the_event = (struct event *) queue_head(event_q))) {
            log("SYSERR: Attempt to get a NULL event");
            ALERT_2;
            return;
        }

        /*
         ** Set the_event->q_el to NULL so that any functions called beneath
         ** event_process can tell if they're being called beneath the actual
         ** event function.
         */
        the_event->q_el = NULL;

        /* call event func, reenqueue event if retval > 0 */
        if ( the_event->func == message_event )
            new_time = the_event->func ( the_event );
        else
            new_time = the_event->func ( the_event->event_obj );

        if (new_time > 0)
            the_event->q_el = queue_enq(event_q, the_event, new_time + pulse);
        else
            delete the_event;
    }
}

/* returns the time remaining before the event */
unsigned long event_time(struct event *event) {
    long when;
    if (event == NULL || event->q_el == NULL) {
        ALERT_2;
        return 0;
    }

    when = queue_elmt_key(event->q_el);

    return (when - pulse);
}


/* frees all events in the queue */
void event_free_all(void) {
    struct event *the_event;

    while ((the_event = (struct event *) queue_head(event_q))) {
        if (the_event->event_obj) {
            switch (the_event->type) {
            case EVENT_TYPE_MESSAGE:
                delete (struct message_event_obj *)the_event->event_obj;
                break;
            case EVENT_TYPE_FIGHT:
                delete (struct fight_event_obj *)the_event->event_obj;
                break;
            case EVENT_TYPE_REGEN:
                delete (struct regen_event_obj *)the_event->event_obj;
                break;
            case EVENT_TYPE_DAMAGE:
                delete (struct aff_dam_event_obj *)the_event->event_obj;
                break;
            case EVENT_TYPE_TRIG:
                delete (struct wait_event_data *)the_event->event_obj;
                break;
            case EVENT_TYPE_TIMER:
                delete (struct timer_event_data *)the_event->event_obj;
                break;
            default:
                free(the_event->event_obj);
                break;
            }
        }
        delete the_event;
    }
    queue_free(event_q);
}

/* boolean function to tell whether an event is queued or not */
bool event_is_queued(struct event *event) {
    if (event->q_el)
        return TRUE;
    else
        return FALSE;
}

/* ************************************************************************
*  File: queue.c                                                          *
*                                                                         *
*  Usage: generic queue functions for building and using a priority queue *
*                                                                         *
************************************************************************ */

/* add data into the priority queue q with key */
struct q_element *queue_enq(struct dgqueue *q, void *data, long key) {
    struct q_element *qe, *i;
    int bucket;

    qe = new q_element(data, key);

    bucket = key % NUM_EVENT_QUEUES; /* which queue does this go in */

    if (!q->head[bucket]) {  /* queue is empty */
        q->head[bucket] = qe;
        q->tail[bucket] = qe;
    } else {
        for (i = q->tail[bucket]; i; i = i->prev) {

            if (i->key < key) {   /* found insertion point */
                if (i == q->tail[bucket])
                    q->tail[bucket] = qe;
                else {
                    qe->next = i->next;
                    i->next->prev = qe;
                }

                qe->prev = i;
                i->next = qe;
                break;
            }
        }

        if (i == NULL) {     /* insertion point is front of list */
            qe->next = q->head[bucket];
            q->head[bucket] = qe;
            qe->next->prev = qe;
        }

    }

    return qe;
}


/* remove queue element qe from the priority queue q */
void queue_deq(struct dgqueue *q, struct q_element *qe) {
    int i;

    assert(qe);

    i = (qe->key % (NUM_EVENT_QUEUES));

    if (qe->prev == NULL)
        q->head[i] = qe->next;
    else
        qe->prev->next = qe->next;

    if (qe->next == NULL)
        q->tail[i] = qe->prev;
    else if (qe->next)
        qe->next->prev = qe->prev;
    else
        log("ERROR: qe->next non existant!");

    delete qe;
}





/*
 * removes and returns the data of the
 * first element of the priority queue q
 */
void *queue_head(struct dgqueue *q) {
    void *data;
    int i;

    i = pulse % NUM_EVENT_QUEUES;

    if (!q->head[i])
        return NULL;

    data = q->head[i]->data;
    queue_deq(q, q->head[i]);
    return data;
}


/*
 * returns the key of the head element of the priority queue
 * if q is NULL, then return the largest unsigned number
 */
long queue_key(struct dgqueue *q) {
    int i;

    i = pulse % NUM_EVENT_QUEUES;

    if (q->head[i])
        return q->head[i]->key;
    else
        return LONG_MAX;
}


/* returns the key of queue element qe */
long queue_elmt_key(struct q_element *qe) {
    return qe ? qe->key : 0;
}


/* free q and contents */
void queue_free(struct dgqueue *q) {
    int i;
    struct q_element *qe, *next_qe;

    for (i = 0; i < NUM_EVENT_QUEUES; i++)
        for (qe = q->head[i]; qe; qe = next_qe) {
            next_qe = qe->next;
            delete qe;
        }

    delete q;
}
