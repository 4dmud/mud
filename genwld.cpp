/************************************************************************
 * Generic OLC Library - Rooms / genwld.c			v1.0	*
 * Original author: Levork						*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#include "config.h"
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

void update_wait_events(Room *to, Room *from);
/*
 * This function will copy the strings so be sure you free your own
 * copies of the description, title, and such.
 */
room_rnum add_room(Room *room)
{
  //  Character *tch;
  //  struct obj_data *tobj;
  int i; 
  unsigned long found = FALSE;
  //  room_rnum i;

  if (room == NULL)
  {
    log("Add_room: null room given.");
    return NULL;
  }

  if ((i = room->number) != NOWHERE && world_vnum[i])
  {
	 // log("Room Desc 1: %s", room->GetDescription());
    if (SCRIPT(world_vnum[i]))
      extract_script(world_vnum[i], WLD_TRIGGER);
    /** save the new string **/
    //world_vnum[i]->t_description = strdup(room->GetDescription());
    
    //log("Room Desc 2: %s", room->GetDescription());
    room->contents = world_vnum[i]->contents;
    room->people = world_vnum[i]->people;
    world_vnum[i]->free_room_strings();
    *world_vnum[i] = *room;
    world_vnum[i]->SetDesc(-1);
    world_vnum[i]->mine = room->mine;
    world_vnum[i]->copy_room_strings(room);
    //log("Room Desc 4: %s", room->GetDescription());
    //log("Room Desc 5: %s", world_vnum[i]->GetDescription());
    add_to_save_list(zone_table[room->zone].number, SL_WLD);
    log("GenOLC: add_room: Updated existing room #%d.", i);
    return world_vnum[i];
  }

  //  RECREATE(world, Room, top_of_world + 2);
  top_of_world = MAX(room->number, top_of_world);
#if 0

  for (i = top_of_world; i > 0; i--)
  {
    if (room->number > world[i - 1].number)
    {
      world[i] = *room;
      copy_room_strings(&world[i], room);
      found = i;
      break;
    }
    else
    {
      /* Copy the room over now. */
      world[i] = world[i - 1];
      update_wait_events(&world[i], &world[i-1]);

      /* People in this room must have their in_rooms moved up one. */
      for (tch = world[i].people; tch; tch = tch->next_in_room)
        IN_ROOM(tch) += (IN_ROOM(tch) != NOWHERE);

      /* Move objects too. */
      for (tobj = world[i].contents; tobj; tobj = tobj->next_content)
        IN_ROOM(tobj) += (IN_ROOM(tobj) != NOWHERE);
    }
  }
  if (!found)
  {
    world[0] = *room;	/* Last place, in front. */
    copy_room_strings(&world[0], room);
  }

  log("GenOLC: add_room: Added room %d at index #%d.", room->number, found);
#endif
  world_vnum[room->number]=room;
  found=(unsigned long)(room);
  log("GenOLC: add_room: Added room %d at address %lu.", room->number, found);


  /* found is equal to the array index where we added the room. */

  /*
   * Find what zone that room was in so we can update the loading table.
   */
#if 0
  for (i = 0;/*room->zone;*/ i <= top_of_zone_table; i++)
    for (j = 0; ZCMD(i, j).command != 'S'; j++)
      switch (ZCMD(i, j).command)
      {
      case 'M':
      case 'O':
      case 'T':
      case 'V':
        ZCMD(i, j).arg3 += (ZCMD(i, j).arg3 != NOWHERE && ZCMD(i, j).arg3 >= found);
        break;
      case 'D':
      case 'R':
        ZCMD(i, j).arg1 += (ZCMD(i, j).arg1 != NOWHERE && ZCMD(i, j).arg1 >= found);
      case 'G':
      case 'P':
      case 'E':
      case '*':
        /* Known zone entries we don't care about. */
        break;
      default:
        new_mudlog(BRF, LVL_GOD, TRUE, "SYSERR: GenOLC: add_room: Unknown zone entry found (%c) !", (ZCMD(i, j).command));
      }
  /*
   * Update the loadroom table. Adds 1 or 0.
   */
  CONFIG_MORTAL_START += (CONFIG_MORTAL_START >= found);
  CONFIG_IMMORTAL_START += (CONFIG_IMMORTAL_START >= found);
  CONFIG_FROZEN_START += (CONFIG_FROZEN_START >= found);

  /*
   * Update world exits.
   */
  /* changed to do-while to work with unsigned index w/o warnings */
  i = top_of_world + 1;
  do
  {
    i--;
    for (j = 0; j < NUM_OF_DIRS; j++)
      if (W_EXIT(i, j) && W_EXIT(i, j)->to_room != NOWHERE)
        W_EXIT(i, j)->to_room += (W_EXIT(i, j)->to_room >= found);
  }
  while (i > 0);
#endif

  add_to_save_list(zone_table[room->zone].number, SL_WLD);

  /*
   * Return what array entry we placed the new room in.
   */
  return world_vnum[room->number];
}

/* -------------------------------------------------------------------------- */

int delete_room(room_rnum rnum)
{
  int i;
  int j;
  Character *ppl, *next_ppl;
  struct obj_data *obj, *next_obj;
  Room *room;

  if (rnum->number <= 0 || rnum->number > top_of_world)	/* Can't delete void yet. */
    return FALSE;

  room = world_vnum[rnum->number];

  add_to_save_list(zone_table[room->zone].number, SL_WLD);

  /* This is something you might want to read about in the logs. */
  log("GenOLC: delete_room: Deleting room #%d (%s).", room->number, room->name);

  if (CONFIG_MORTAL_START == rnum)
  {
    log("WARNING: GenOLC: delete_room: Deleting mortal start room!");
    CONFIG_MORTAL_START = 0;	/* The Void */
  }
  if (CONFIG_IMMORTAL_START == rnum)
  {
    log("WARNING: GenOLC: delete_room: Deleting immortal start room!");
    CONFIG_IMMORTAL_START = 0;	/* The Void */
  }
  if (CONFIG_FROZEN_START == rnum)
  {
    log("WARNING: GenOLC: delete_room: Deleting frozen start room!");
    CONFIG_FROZEN_START = 0;	/* The Void */
  }

  /*
   * Dump the contents of this room into the Void.  We could also just
   * extract the people, mobs, and objects here.
   */
  for (obj = rnum->contents; obj; obj = next_obj)
  {
    next_obj = obj->next_content;
    obj_from_room(obj);
    obj_to_room(obj, 0);
  }
  for (ppl = rnum->people; ppl; ppl = next_ppl)
  {
    next_ppl = ppl->next_in_room;
    char_from_room(ppl);
    char_to_room(ppl, 0);
  }

  room->free_room_strings();
  if (SCRIPT(room))
    extract_script(room, WLD_TRIGGER);
  free_proto_script(room, WLD_TRIGGER);

  /*
   * Change any exit going to this room to go the void.
   * Also fix all the exits pointing to rooms above this.
   */
  i = top_of_world + 1;
  do
  {
    i--;
    if (world_vnum[i] == NULL)
      continue;
    for (j = 0; j < NUM_OF_DIRS; j++)
      if (W_EXIT(world_vnum[i], j) == NULL)
        continue;
      else if (W_EXIT(world_vnum[i], j)->to_room == rnum)
      {
        if ((!W_EXIT(world_vnum[i], j)->keyword || !*W_EXIT(world_vnum[i], j)->keyword) &&
            (!W_EXIT(world_vnum[i], j)->general_description || !*W_EXIT(world_vnum[i], j)->general_description))
        {
          /* no description, remove exit completely */
          if (W_EXIT(world_vnum[i], j)->keyword)
            free(W_EXIT(world_vnum[i], j)->keyword);
          if (W_EXIT(world_vnum[i], j)->general_description)
            free(W_EXIT(world_vnum[i], j)->general_description);
          free(W_EXIT(world_vnum[i], j));
          W_EXIT(world_vnum[i], j) = NULL;
        }
        else
        {
          /* description is set, just point to nowhere */
          W_EXIT(world_vnum[i], j)->to_room = NULL;
        }
      }
  }
  while (i > 0);

  /*
   * Find what zone that room was in so we can update the loading table.
   */
  for (i = 0; i <= top_of_zone_table; i++)
    for (j = 0; ZCMD(i , j).command != 'S'; j++)
      switch (ZCMD(i, j).command)
      {
      case 'M':
      case 'O':
      case 'T':
      case 'V':
        if (ZCMD(i, j).arg3 == rnum->number)
          ZCMD(i, j).command = '*';	/* Cancel command. */
        else if (ZCMD(i, j).arg3 > rnum->number)
          ZCMD(i, j).arg3 -= (ZCMD(i, j).arg3 != NOWHERE); /* with unsigned NOWHERE > any rnum */
        break;
      case 'D':
      case 'R':
        if (ZCMD(i, j).arg1 == rnum->number)
          ZCMD(i, j).command = '*';	/* Cancel command. */
        else if (ZCMD(i, j).arg1 > rnum->number)
          ZCMD(i, j).arg1 -= (ZCMD(i, j).arg1 != NOWHERE); /* with unsigned NOWHERE > any rnum */
      case 'G':
      case 'P':
      case 'E':
      case '*':
        /* Known zone entries we don't care about. */
        break;
      default:
        new_mudlog(BRF, LVL_GOD, TRUE, "SYSERR: GenOLC: delete_room: Unknown zone entry found!");
      }

  /*
   * Remove this room from all shop lists.
   */
  do
  {
    extern int top_shop;
    for (i = 0;i < top_shop;i++)
    {
      for (j = 0;SHOP_ROOM(i, j) != NULL;j++)
      {
        if (SHOP_ROOM(i, j) == rnum)
          SHOP_ROOM(i, j) = world_vnum[0]; /* set to the void */
      }
    }
  }
  while (0);

  if (room)
  {
    world_vnum[room->number] = NULL;
    free(room);
  }
  /*
   * Now we actually move the rooms down.
   */
#if 0
  for (i = rnum; i < top_of_world; i++)
  {
    world[i] = world[i + 1];
    update_wait_events(&world[i], &world[i+1]);

    for (ppl = world[i].people; ppl; ppl = ppl->next_in_room)
      IN_ROOM(ppl) -= (IN_ROOM(ppl) != NOWHERE);	/* Redundant check? */

    for (obj = world[i].contents; obj; obj = obj->next_content)
      IN_ROOM(obj) -= (IN_ROOM(obj) != NOWHERE);	/* Redundant check? */
  }

  top_of_world--;
  RECREATE(world, Room, top_of_world + 1);
#endif

  return TRUE;
}


int save_rooms(zone_rnum rzone)
{
  int i;
  Room *room;
  struct extra_descr_data *ex_desc = NULL;;
  FILE *sf;
  char filename[128];
  char buf[MAX_STRING_LENGTH];
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];
  char buf3[MAX_STRING_LENGTH];

#if CIRCLE_UNSIGNED_INDEX
  if (rzone == NOWHERE || rzone > top_of_zone_table)
  {
#else
  if (rzone < 0 || rzone > top_of_zone_table)
  {
#endif
    log("SYSERR: GenOLC: save_rooms: Invalid zone number %d passed! (0-%d)", rzone, top_of_zone_table);
    return FALSE;
  }

  log("GenOLC: save_rooms: Saving rooms in zone #%d (%d-%d).",
      zone_table[rzone].number, zone_table[rzone].Bot(), zone_table[rzone].top);

  snprintf(filename, sizeof(filename), "%s/%d/%d.new", LIB_WORLD, zone_table[rzone].number, zone_table[rzone].number);
  if (!(sf = fopen(filename, "w")))
  {
    perror("SYSERR: save_rooms");
    return FALSE;
  }

  for (i = zone_table[rzone].Bot(); i <= zone_table[rzone].top; i++)
  {
    if ((room = real_room(i)) != NULL)
    {
      int j;
      /*
       * Copy the description and strip off trailing newlines.
       */
      strlcpy(buf, room->HasDesc() ? room->GetDescription() : "Empty room.", sizeof(buf)-1 );
      strip_cr(buf);

      /*
      * Remove the '\r\n' sequences from smell.
      */
      strlcpy(buf2,
             room->smell ? room->
			     smell : "You smell nothing special.\n", sizeof(buf2)-1);
      strip_cr(buf2);

      /*
       * Remove the '\r\n' sequences from description.
       */
      strlcpy(buf3,
             room->listen ? room->
			     listen : "You hear nothing special.\n", sizeof(buf3)-1);
      strip_cr(buf3);

      /*
       * Save the numeric and string section of the file.
       */

      fprintf(sf,
              "#%d\n"
              "%s%c\n"
              "%s%c\n"
              "%s%c\n"
              "%s%c\n"
              "%d %d %d %d %d %d\n",
              room->number,
              room->name ? room->name : "Untitled", STRING_TERMINATOR,
              buf, STRING_TERMINATOR,
              buf2, STRING_TERMINATOR,
              buf3, STRING_TERMINATOR,
              zone_table[room->zone].number, room->room_flags[0], room->room_flags[1],
              room->room_flags[2], room->room_flags[3], room->sector_type
             );

      /*
       * Now you write out the exits for the room.
       */
      for (j = 0; j < NUM_OF_DIRS; j++)
      {
        if (R_EXIT(room, j) && !R_EXIT(room, j)->nosave)
        {
          int dflag;
          if (R_EXIT(room, j)->general_description)
          {
            strlcpy(buf, R_EXIT(room, j)->general_description, sizeof(buf)-1);
            strip_cr(buf);
          }
          else
            *buf = '\0';

          /*
           * Figure out door flag.
           */
          if (IS_SET(R_EXIT(room, j)->exit_info, EX_ISDOOR))
          {
            if (IS_SET(R_EXIT(room, j)->exit_info, EX_PICKPROOF))
              dflag = 2;
            else
              dflag = 1;
          }
          else
            dflag = 0;

          if (R_EXIT(room, j)->keyword)
            strlcpy(buf1, R_EXIT(room, j)->keyword, sizeof(buf1)-1 );
          else
            *buf1 = '\0';

          /*
           * Now write the exit to the file.
           */
          fprintf(sf,
                  "D%d\n"
                  "%s~\n"
                  "%s~\n"
                  "%d %d %d\n", j, buf, buf1, dflag,
                  R_EXIT(room, j)->key != NOTHING ? R_EXIT(room, j)->key : -1,
                  R_EXIT(room, j)->to_room != NULL ? R_EXIT(room, j)->to_room->number : -1);

        }
      }
    
      if (room->n_description)
      {
          strlcpy(buf, room->n_description, sizeof(buf));
          strip_cr(buf);
          fprintf(sf, "N\n%s~\n", room->n_description);
      }

      if (room->ex_description)
      {
        for (ex_desc = room->ex_description; ex_desc; ex_desc = ex_desc->next)
        {
          strlcpy(buf, ex_desc->description, sizeof(buf));
          strip_cr(buf);
          fprintf(sf,	"E\n"
                  "%s~\n"
                  "%s~\n", ex_desc->keyword, buf);
        }
      }
      if (room->look_above_description)
      {
        for (ex_desc = room->look_above_description; ex_desc; ex_desc = ex_desc->next)
        {
          strlcpy(buf, ex_desc->description, sizeof(buf));
          strip_cr(buf);
          fprintf(sf,	"A\n"
                  "%s~\n"
                  "%s~\n", ex_desc->keyword, buf);
        }
      }
      if (room->look_under_description)
      {
        for (ex_desc = room->look_under_description; ex_desc; ex_desc = ex_desc->next)
        {
          strncpy(buf, ex_desc->description, sizeof(buf));
          strip_cr(buf);
          fprintf(sf,	"U\n"
                  "%s~\n"
                  "%s~\n", ex_desc->keyword, buf);
        }
      }
      if (room->look_behind_description)
      {
        for (ex_desc = room->look_behind_description; ex_desc; ex_desc = ex_desc->next)
        {
          strlcpy(buf, ex_desc->description, sizeof(buf));
          strip_cr(buf);
          fprintf(sf,	"B\n"
                  "%s~\n"
                  "%s~\n", ex_desc->keyword, buf);
        }
      }
      if (room->mine.num != -1)
      {
        fprintf(sf,	"M\n"
                "%d\n"
                "%d\n"
                "%d\n", room->mine.num, room->mine.dif,room->mine.tool);
      }

      fprintf(sf, "S\n");
      script_save_to_disk(sf, room, WLD_TRIGGER);
    }
  }

  /*
   * Write the final line and close it.
   */
  fprintf(sf, "$~\n");
  fclose(sf);

  /* Old file we're replacing. */
  snprintf(buf, sizeof(buf), "%s/%d/%d.wld", LIB_WORLD, zone_table[rzone].number, zone_table[rzone].number);

  remove(buf);
  rename(filename, buf);

  if (in_save_list(zone_table[rzone].number, SL_WLD))
    remove_from_save_list(zone_table[rzone].number, SL_WLD);
  return TRUE;
}


/*
 * Idea by: Cris Jacobin <jacobin@bellatlantic.net>
 */
room_rnum duplicate_room(room_vnum dest_vnum, room_rnum orig)
{
  int znum;
  room_rnum new_rnum;
  Room nroom;

#if CIRCLE_UNSIGNED_INDEX
  if (orig->number == NOWHERE || orig->number > top_of_world)
  {
#else
  if (orig->number < 0 || orig->number > top_of_world)
  {
#endif
    log("SYSERR: GenOLC: copy_room: Given bad original real room.");
    return NULL;
  }
  else if ((znum = real_zone_by_thing(dest_vnum)) == NOWHERE)
  {
    log("SYSERR: GenOLC: copy_room: No such destination zone.");
    return NULL;
  }

  /*
   * add_room will make its own copies of strings.
   */
  if ((new_rnum = add_room(&nroom)) == NULL)
  {
    log("SYSERR: GenOLC: copy_room: Problem adding room.");
    return NULL;
  }

  //nroom = world[new_rnum];
  nroom.number = dest_vnum;
  nroom.zone = znum;

  /* So the people and objects aren't in two places at once... */
  nroom.contents = NULL;
  nroom.people = NULL;

  return new_rnum;
}

/* -------------------------------------------------------------------------- */
/** room vector compare **/
    bool operator< (const vector<Room *>::iterator &a,const vector<Room *>::iterator &b) {
        return ((*a)->number < (*b)->number);
    }
    bool operator> (const vector<Room *>::iterator &a,const vector<Room *>::iterator &b) {
        return ((*a)->number > (*b)->number);
    }
    bool operator== (const vector<Room *>::iterator &a,const vector<Room *>::iterator &b) {
        return ((*a)->number == (*b)->number);
    }
/** Room compare **/
    bool operator< (const Room &a,const Room &b) {
        return (a.number < b.number);
    }
    bool operator> (const Room &a,const Room &b) {
        return (a.number > b.number);
    }
    bool operator== (const Room &a,const Room &b) {
        return (a.number == b.number);
    }
/** Room to int compare **/
    bool operator< (const Room &r,const room_vnum b) {
        return (r.number < b);
    }
    bool operator> (const Room &r,const room_vnum b) {
        return (r.number > b);
    }
    bool operator== (const Room &r,const room_vnum b) {
        return (r.number == b);
    }
