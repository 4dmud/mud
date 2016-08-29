#ifndef ACTION_H
#define ACTION_H
/* struct for syls */
struct syllable
{
  const char *org;
  const char *news;
};
extern struct syllable syls[];

/** Message event ID numbers, so that we can have multiple message events running at once **/

#define ME_CALL_LIGHTNING 1
#define ME_SINGWOOD 2
#define ME_LUMBERJACK 3
#define ME_JUGGLE 4
#define ME_THROTTLE 5
#define ME_TUNNELING 6
#define ME_PICK_LOCK 7
#define ME_CONTROL_WEATHER 8
#define ME_MANIFEST 9
#define ME_COPYOVER 10
#endif
