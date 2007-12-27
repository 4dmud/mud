
/* Player point types for events */
#define REGEN_HIT      0
#define REGEN_MANA     1
#define REGEN_MOVE     2
#define REGEN_STAMINA  3

#define PULSES_PER_MUD_HOUR     (SECS_PER_MUD_HOUR*PASSES_PER_SEC)
EVENTFUNC(points_event);

/* event object structure for point regen */
struct regen_event_obj
{
  Character *ch;	/* character regening */
  int type;			/* HIT, MOVE, or MANA */

  regen_event_obj() {
ch = NULL;
type = REGEN_HIT;
  }
  regen_event_obj(Character *c, int t) {
ch = c;
type = t;
  }

  ~regen_event_obj() {}
};
