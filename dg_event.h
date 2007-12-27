/* ************************************************************************
*  File: events.h                                                         *
*                                                                         *
*  Usage: structures and prototypes for events                            *
*                                                                         *
*  Written by Eric Green (ejg3@cornell.edu)                               *
*                                                                         *
*  Changes:                                                               *
*      3/6/98 ejg:  Changed return type of EVENTFUNC from void to long.   *
*                   Moved struct event definition to events.c.            *
************************************************************************ */


/*
** how often will heartbeat() call the 'wait' event function?
*/
#define PULSE_DG_EVENT 1

#define EVENT_TYPE_MESSAGE 0
#define EVENT_TYPE_REGEN 1
#define EVENT_TYPE_FIGHT 2
#define EVENT_TYPE_DAMAGE 3
#define EVENT_TYPE_TRIG 4
#define EVENT_TYPE_TIMER 5


/********** Event related section *********/

#define EVENTFUNC(name) long (name)(void *event_obj)


/*
** define event related structures
*/
struct event {
    EVENTFUNC(*func);
    void *event_obj;
    struct q_element *q_el;
    int type;

    event(EVENTFUNC(*f), void * eo, int t) {
        func = f;
        event_obj = eo;
        type = t;
    }
    ~event() {}
    ;
};
/****** End of Event related info ********/

/***** Queue related info ******/

/* number of queues to use (reduces enqueue cost) */
#define NUM_EVENT_QUEUES    10

struct dgqueue {
    struct q_element *head[NUM_EVENT_QUEUES], *tail[NUM_EVENT_QUEUES];

    dgqueue() {
        for (unsigned int i = 0; i < NUM_EVENT_QUEUES;i++)
            head[i] = tail[i] = NULL;
    };
    ~dgqueue() {
        //this gets cleaned up in another function - mord
    };

};

struct q_element {
    void *data;
    long key;
    struct q_element *prev, *next;
    q_element() {
        data = NULL;
        key = -1;
        prev = NULL;
        next = NULL;
    }
    q_element(void *d, long k) {
        data = d;
        key = k;
        prev = NULL;
        next = NULL;
    }
    ~q_element() {}
};

/*move this to constants */
struct fight_event_obj {
    long id;
    fight_event_obj(int i) {
id = i;
    }
    fight_event_obj() { id = -1; }
    ~fight_event_obj() {}
};
struct aff_dam_event_obj
{
  Character* ch;
  int damage;
  int interval;
  int recurse;
  int (*event_fun)(int, Character*);
  int id;
  aff_dam_event_obj() {}
  ~aff_dam_event_obj() {}
};
/****** End of Queue related info ********/

/* - events - function protos need by other modules */
void event_init(void);
struct event *event_create(EVENTFUNC(*func), void *event_obj, long when, int type);
void event_cancel(struct event *event);
void event_process(void);
unsigned long event_time(struct event *event);
void event_free_all(void);

/* - queues - function protos need by other modules */
struct q_element *queue_enq(struct dgqueue *q, void *data, long key);
void queue_deq(struct dgqueue *q, struct q_element *qe);
void *queue_head(struct dgqueue *q);
long queue_key(struct dgqueue *q);
long queue_elmt_key(struct q_element *qe);
void queue_free(struct dgqueue *q);
bool event_is_queued(struct event *event);
