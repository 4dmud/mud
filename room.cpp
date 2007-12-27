#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "db.h"
#include "handler.h"
#include "comm.h"
#include "genolc.h"
#include "genwld.h"
#include "genzon.h"
#include "dg_olc.h"
#include "shop.h"
// functor to help finding a room by name
struct findRoomName
{
  const char *name;
  // ctor
  findRoomName (const char *n) : name ( n ) {}
  // check for player with correct name, and actually playing
  bool operator() (const Room * r) const
    {
    return !strcmp(r->name, name);
    } // end of operator()  
};  // end of findRoomName

struct findRoomVnum
{
  const room_vnum rvnum;
  // ctor
  findRoomVnum (const room_vnum rv) : rvnum(rv) {}
  // check for player with correct name, and actually playing
  bool operator() (const Room * r) const
    {
    return (r->number==rvnum);
    } // end of operator()  
};  // end of findRoomVnum

Room::Room() {
number = NOWHERE;
zone = NOWHERE;
sector_type = 0; 
name = NULL;  
description = NULL;  
smell = NULL;       
listen = NULL;    
ex_description = NULL; 
look_above_description = NULL; 
look_behind_description = NULL;  
look_under_description = NULL; 
for (int i = 0; i < NUM_OF_DIRS;i++)
    dir_option[i] = NULL; 
    for (int i = 0; i < RF_ARRAY_MAX;i++)
    room_flags[i]=0;  
    
    mine = room_mine_data();
    light = 0;               /* Number of lightsources in room       */
     func = NULL;
    proto_script = NULL;    /* list of default triggers    */
    script = NULL;    /* script info for the object           */
    contents = NULL;     /* List of items in room                */
    people = NULL;   /* List of NPC / PC in room             */
    affects = NULL;
    ores = NULL;

}
Room::~Room() {
        free_room_strings();
        /* free any assigned scripts */
        if (SCRIPT(this))
            extract_script(this, WLD_TRIGGER);
        /* free script proto list */
        free_proto_script(this, WLD_TRIGGER);
}

int Room::free_room_strings()
{
  int i;

  /* Free descriptions. */

    free_string(&name);
    free_string(&description);
    free_string(&smell);
    free_string(&listen);
  if (ex_description)
    free_ex_descriptions(ex_description);
  ex_description = NULL;
  
  if (look_under_description)
    free_ex_descriptions(look_under_description);
  look_under_description = NULL;
  
  if (look_above_description)
    free_ex_descriptions(look_above_description);
  look_above_description = NULL;
  
  if (look_behind_description)
    free_ex_descriptions(look_behind_description);
  look_behind_description = NULL;

  /* Free exits. */
  for (i = 0; i < NUM_OF_DIRS; i++)
  {
    if (R_EXIT(this, i))
    {
      if (R_EXIT(this, i)->general_description)
        free_string(&R_EXIT(this, i)->general_description);
      if (R_EXIT(this, i)->keyword)
        free_string(&R_EXIT(this, i)->keyword);
      free(R_EXIT(this, i));
      R_EXIT(this, i) = NULL;
    }
  }

  return TRUE;
}

int Room::copy_room(Room *from)
{
  free_room_strings();
  *this = *from;
  copy_room_strings(from);

  /* Don't put people and objects in two locations.
     Am thinking this shouldn't be done here... */
  //from->people = NULL;
  //from->contents = NULL;

  return TRUE;
}

/*
 * Copy strings over so bad things don't happen.  We do not free the
 * existing strings here because copy_room() did a shallow copy previously
 * and we'd be freeing the very strings we're copying.  If this function
 * is used elsewhere, be sure to free_room_strings() the 'dest' room first.
 */
int Room::copy_room_strings(Room *source)
{
  int i;

  if (this == NULL || source == NULL)
  {
    log("SYSERR: GenOLC: copy_room_strings: NULL values passed.");
    return FALSE;
  }

  description = str_udup(source->description);
  name = str_udup(source->name);

  smell = str_udup(source->smell);
  listen = str_udup(source->listen);

  for (i = 0; i < NUM_OF_DIRS; i++)
  {
    if (!R_EXIT(source, i))
      continue;

    CREATE(R_EXIT(this, i), struct room_direction_data, 1);
    *R_EXIT(this, i) = *R_EXIT(source, i);
    if (R_EXIT(source, i)->general_description)
      R_EXIT(this, i)->general_description = strdup(R_EXIT(source, i)->general_description);
    if (R_EXIT(source, i)->keyword)
      R_EXIT(this, i)->keyword = strdup(R_EXIT(source, i)->keyword);
  }

  if (source->ex_description)
    copy_ex_descriptions(&ex_description, source->ex_description);
  if (source->look_under_description)
    copy_ex_descriptions(&look_under_description, source->look_under_description);
  if (source->look_above_description)
    copy_ex_descriptions(&look_above_description, source->look_above_description);
  if (source->look_behind_description)
    copy_ex_descriptions(&look_behind_description, source->look_behind_description);
  return TRUE;
}



