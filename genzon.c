/************************************************************************
 * Generic OLC Library - Zones / genzon.c			v1.0	*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/
#include <sys/stat.h>
#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "db.h"
#include "genolc.h"
#include "genzon.h"
#include "dg_scripts.h"

extern zone_rnum top_of_zone_table;
extern struct room_data *world_vnum[];
extern struct zone_data *zone_table;
extern struct char_data *mob_proto;
extern struct obj_data *obj_proto;
extern struct index_data *mob_index;
extern struct index_data *obj_index;
extern struct index_data **trig_index;
extern struct trig_data *trigger_list;

/* real zone of room/mobile/object/shop given */
zone_rnum real_zone_by_thing(room_vnum vznum)
{
#if 0
  zone_rnum bot, top, mid;
  int low, high;

  bot = 0;
  top = top_of_zone_table;

  /* perform binary search on zone-table */
  for (;;) {
    mid = (bot + top) / 2;

    /* Upper/lower bounds of the zone. */
    low = genolc_zone_bottom(mid);
    high = zone_table[mid].top;

    if (low <= vznum && vznum <= high)
      return mid;
    if (bot >= top)
      return NOWHERE;
    if (low > vznum)
      top = mid - 1;
    else
      bot = mid + 1;
  }
#else
  zone_rnum t;
  for (t = 0; t <= top_of_zone_table; t++)
    if (vznum >= zone_table[t].bot && vznum <= zone_table[t].top)
      return t;
  return NOWHERE;
#endif
}

zone_rnum create_new_zone(zone_vnum vzone_num, room_vnum bottom, room_vnum top, const char **error)
{
  FILE *fp;
  struct zone_data *zone;
  int i;
  zone_rnum rznum;
  char buf[MAX_STRING_LENGTH];

#if CIRCLE_UNSIGNED_INDEX
  if (vzone_num == NOWHERE) {
#else
  if (vzone_num < 0) {
#endif
    *error = "You can't make negative zones.\r\n";
    return NOWHERE;
  } else if (bottom > top) {
    *error = "Bottom room cannot be greater than top room.\r\n";
    return NOWHERE;
  }

#if _CIRCLEMUD < CIRCLEMUD_VERSION(3,0,21)
  /*
   * New with bpl19, the OLC interface should decide whether
   * to allow overlap before calling this function. There
   * are more complicated rules for that but it's not covered
   * here.
   */
  if (vzone_num > 999) {
    *error = "999 is the highest zone allowed.\r\n";
    return NOWHERE;
  }

  /*
   * Make sure the zone does not exist.
   */
  room = vzone_num * 100; /* Old CircleMUD 100-zones. */
  for (i = 0; i <= top_of_zone_table; i++)
    if (genolc_zone_bottom(i) <= room && zone_table[i].top >= room) {
      *error = "A zone already covers that area.\r\n";
      return NOWHERE;
    }
#else
  for (i = 0; i < top_of_zone_table; i++)
    if (zone_table[i].number == vzone_num) {
      *error = "That virtual zone already exists.\r\n";
      return NOWHERE;
     }
#endif

/* create directory */

  snprintf(buf, sizeof(buf), "%s%d", LIB_WORLD, vzone_num);
mkdir(buf, S_IRWXO | S_IRWXU | S_IRWXG);

  /*
   * Create the zone file.
   */
  snprintf(buf, sizeof(buf), "%s%d/%d.zon", LIB_WORLD, vzone_num, vzone_num);
  if (!(fp = fopen(buf, "w"))) {
    new_mudlog(BRF, LVL_IMPL, TRUE, "SYSERR: OLC: Can't write new zone file.");
    *error = "Could not write zone file.\r\n";
    return NOWHERE;
  }
fprintf(fp, "#%d\nNew Zone~\n~\n%d 30 2 0\nS\n$\n", vzone_num, top);
  fclose(fp);

  /*
   * Create the room file.
   */
  snprintf(buf, sizeof(buf), "%s%d/%d.wld", LIB_WORLD, vzone_num, vzone_num);
  if (!(fp = fopen(buf, "w"))) {
    new_mudlog(BRF, LVL_IMPL, TRUE, "SYSERR: OLC: Can't write new world file.");
    *error = "Could not write world file.\r\n";
    return NOWHERE;
  }
      fprintf(fp,
	"#%d\nThe Beginning~\nNot much here.\n~\nYou smell nothing of interest.\n~\nYou hear nothing out of the ordinary.\n~\n%d 0 0 0 0 0\nS\n$\n",
	    vzone_num * 100, vzone_num);
  fclose(fp);

  /*
   * Create the mobile file.
   */
  snprintf(buf, sizeof(buf), "%s%d/%d.mob", LIB_WORLD, vzone_num, vzone_num);
  if (!(fp = fopen(buf, "w"))) {
    new_mudlog(BRF, LVL_IMPL, TRUE, "SYSERR: OLC: Can't write new mob file.");
    *error = "Could not write mobile file.\r\n";
    return NOWHERE;
  }
  fprintf(fp, "$\n");
  fclose(fp);

  /*
   * Create the object file.
   */
  snprintf(buf, sizeof(buf), "%s%d/%d.obj", LIB_WORLD, vzone_num, vzone_num);
  if (!(fp = fopen(buf, "w"))) {
    new_mudlog(BRF, LVL_IMPL, TRUE, "SYSERR: OLC: Can't write new obj file.");
    *error = "Could not write object file.\r\n";
    return NOWHERE;
  }
  fprintf(fp, "$\n");
  fclose(fp);

  /*
   * Create the shop file.
   */
  snprintf(buf, sizeof(buf), "%s%d/%d.shp", LIB_WORLD, vzone_num, vzone_num);
  if (!(fp = fopen(buf, "w"))) {
    new_mudlog(BRF, LVL_IMPL, TRUE, "SYSERR: OLC: Can't write new shop file.");
    *error = "Could not write shop file.\r\n";
    return NOWHERE;
  }
  fprintf(fp, "$~\n");
  fclose(fp);

  /*
   * Create the trigger file.
   */
  snprintf(buf, sizeof(buf), "%s%d/%d.trg", LIB_WORLD, vzone_num, vzone_num);
  if (!(fp = fopen(buf, "w"))) {
    new_mudlog(BRF, LVL_IMPL, TRUE, "SYSERR: OLC: Can't write new trigger file");
    *error = "Could not write trigger file.\r\n";
    return NOWHERE;
  }
  fprintf(fp, "$~\n");
  fclose(fp);


  /*
   * Make a new zone in memory. This was the source of all the zedit new
   * crashes reported to the CircleMUD list. It was happily overwriting
   * the stack.  This new loop by Andrew Helm fixes that problem and is
   * more understandable at the same time.
   *
   * The variable is 'top_of_zone_table_table + 2' because we need record 0
   * through top_of_zone (top_of_zone_table + 1 items) and a new one which
   * makes it top_of_zone_table + 2 elements large.
   */
  RECREATE(zone_table, struct zone_data, top_of_zone_table + 2);
  zone_table[top_of_zone_table + 1].number = 99900;

  if (vzone_num > zone_table[top_of_zone_table].number)
    rznum = top_of_zone_table + 1;
  else {
    for (i = top_of_zone_table + 1; i > 0 && vzone_num < zone_table[i - 1].number; i--)
      zone_table[i] = zone_table[i - 1];
    rznum = i;
  }
  zone = &zone_table[rznum];

  /*
   * Ok, insert the new zone here.
   */  
  zone->name = strdup("New Zone");
  zone->number = vzone_num;
  zone->builders = strdup("None");
  zone->dimension = D_ALL;
  zone->bot = (vzone_num * 100);
  zone->top = top;
  zone->lifespan = 30;
  zone->age = 0;
  zone->reset_mode = 2;
  zone->zone_flags = 0;
  /*
   * No zone commands, just terminate it with an 'S'
   */
  CREATE(zone->cmd, struct reset_com, 1);
  zone->cmd[0].command = 'S';

  top_of_zone_table++;

  add_to_save_list(zone->number, SL_ZON);
  return rznum;
}

/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/

void remove_room_zone_commands(zone_rnum zone, room_rnum room_num)
{
  int subcmd = 0, cmd_room = -2;

  /*
   * Delete all entries in zone_table that relate to this room so we
   * can add all the ones we have in their place.
   */
  while (zone_table[zone].cmd[subcmd].command != 'S') {
    switch (zone_table[zone].cmd[subcmd].command) {
    case 'M':
    case 'O':
    case 'T':
    case 'V':
      cmd_room = zone_table[zone].cmd[subcmd].arg3;
      break;
    case 'D':
    case 'R':
      cmd_room = zone_table[zone].cmd[subcmd].arg1;
      break;
    default:
      break;
    }
    if (cmd_room == room_num->number)
      remove_cmd_from_list(&zone_table[zone].cmd, subcmd);
    else
      subcmd++;
  }
}

/*-------------------------------------------------------------------*/

/*
 * Save all the zone_table for this zone to disk.  This function now
 * writes simple comments in the form of (<name>) to each record.  A
 * header for each field is also there.
 */
int save_zone(zone_rnum zone_num)
{
  int subcmd, arg1 = -1, arg2 = -1, arg3 = -1;
  char fname[128], oldname[128];
  const char *comment = NULL;
  FILE *zfile;
  
#if CIRCLE_UNSIGNED_INDEX
  if (zone_num == NOWHERE || zone_num > top_of_zone_table) {
#else
  if (zone_num < 0 || zone_num > top_of_zone_table) {
#endif
    log("SYSERR: GenOLC: save_zone: Invalid real zone number %d. (0-%d)", zone_num, top_of_zone_table);
    return FALSE;
  }

  snprintf(fname, sizeof(fname), "%s/%d/%d.new", LIB_WORLD, zone_table[zone_num].number, zone_table[zone_num].number);
  if (!(zfile = fopen(fname, "w"))) {
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: save_zones:  Can't write zone %d.", zone_table[zone_num].number);
    return FALSE;
  }
fprintf(zfile, "@Version: %d\n", 2);	//version is now 2 - mord
  /*
   * Print zone header to file	
   */
  fprintf(zfile, "#%d\n"
                 "%s~\n"
                 "%s~\n"
                 "%d %d %d %d %ld %d\n",
	  zone_table[zone_num].number,
	  (zone_table[zone_num].name && *zone_table[zone_num].name)
		? zone_table[zone_num].name : "undefined",		
	  (zone_table[zone_num].builders && *zone_table[zone_num].builders)
		? zone_table[zone_num].builders : "None.",
          genolc_zone_bottom(zone_num),
	  zone_table[zone_num].top,
	  zone_table[zone_num].lifespan,
	  zone_table[zone_num].reset_mode,
	  zone_table[zone_num].zone_flags,
    zone_table[zone_num].dimension
	  );

	/*
	 * Handy Quick Reference Chart for Zone Values.
	 *
	 * Field #1    Field #3   Field #4  Field #5
	 * -------------------------------------------------
	 * M (Mobile)  Mob-Vnum   Wld-Max   Room-Vnum
	 * O (Object)  Obj-Vnum   Wld-Max   Room-Vnum
	 * G (Give)    Obj-Vnum   Wld-Max   Unused
	 * E (Equip)   Obj-Vnum   Wld-Max   EQ-Position
	 * P (Put)     Obj-Vnum   Wld-Max   Target-Obj-Vnum
	 * D (Door)    Room-Vnum  Door-Dir  Door-State
	 * R (Remove)  Room-Vnum  Obj-Vnum  Unused
         * T (Trigger) Trig-type  Trig-Vnum Room-Vnum
         * V (var)     Trig-type  Context   Room-Vnum Varname Value
	 * -------------------------------------------------
	 */

  for (subcmd = 0; ZCMD(zone_num, subcmd).command != 'S'; subcmd++) {
    switch (ZCMD(zone_num, subcmd).command) {
    case 'M':
      arg1 = mob_index[ZCMD(zone_num, subcmd).arg1].vnum;
      arg2 = ZCMD(zone_num, subcmd).arg2;
      arg3 = world_vnum[ZCMD(zone_num, subcmd).arg3]->number;
      comment = mob_proto[ZCMD(zone_num, subcmd).arg1].player.short_descr;
      break;
    case 'O':
      arg1 = obj_index[ZCMD(zone_num, subcmd).arg1].vnum;
      arg2 = ZCMD(zone_num, subcmd).arg2;
      arg3 = world_vnum[ZCMD(zone_num, subcmd).arg3]->number;
      comment = obj_proto[ZCMD(zone_num, subcmd).arg1].short_description;
      break;
    case 'G':
      arg1 = obj_index[ZCMD(zone_num, subcmd).arg1].vnum;
      arg2 = ZCMD(zone_num, subcmd).arg2;
      arg3 = -1;
      comment = obj_proto[ZCMD(zone_num, subcmd).arg1].short_description;
      break;
    case 'E':
      arg1 = obj_index[ZCMD(zone_num, subcmd).arg1].vnum;
      arg2 = ZCMD(zone_num, subcmd).arg2;
      arg3 = ZCMD(zone_num, subcmd).arg3;
      comment = obj_proto[ZCMD(zone_num, subcmd).arg1].short_description;
      break;
    case 'P':
      arg1 = obj_index[ZCMD(zone_num, subcmd).arg1].vnum;
      arg2 = ZCMD(zone_num, subcmd).arg2;
      arg3 = obj_index[ZCMD(zone_num, subcmd).arg3].vnum;
      comment = obj_proto[ZCMD(zone_num, subcmd).arg1].short_description;
      break;
    case 'D':
      arg1 = world_vnum[ZCMD(zone_num, subcmd).arg1]->number;
      arg2 = ZCMD(zone_num, subcmd).arg2;
      arg3 = ZCMD(zone_num, subcmd).arg3;
      comment = world_vnum[ZCMD(zone_num, subcmd).arg1]->name;
      break;
    case 'R':
      arg1 = world_vnum[ZCMD(zone_num, subcmd).arg1]->number;
      arg2 = obj_index[ZCMD(zone_num, subcmd).arg2].vnum;
      comment = obj_proto[ZCMD(zone_num, subcmd).arg2].short_description;
      arg3 = -1;
      break;
    case 'T':
      arg1 = ZCMD(zone_num, subcmd).arg1; /* trigger type */
      arg2 = trig_index[ZCMD(zone_num, subcmd).arg2]->vnum; /* trigger vnum */
      arg3 = world_vnum[ZCMD(zone_num, subcmd).arg3]->number; /* room num */
      comment = GET_TRIG_NAME(trig_index[real_trigger(arg2)]->proto); 
      break;
    case 'V':
      arg1 = ZCMD(zone_num, subcmd).arg1; /* trigger type */
      arg2 = ZCMD(zone_num, subcmd).arg2; /* context */
      arg3 = world_vnum[ZCMD(zone_num, subcmd).arg3]->number;
      break;
    case 'B':
      arg1 = obj_index[ZCMD(zone_num, subcmd).arg1].vnum;
      arg2 = ZCMD(zone_num, subcmd).arg2;
      arg3 = world_vnum[ZCMD(zone_num, subcmd).arg3]->number; 
      comment = obj_proto[ZCMD(zone_num, subcmd).arg1].short_description;
      break;
    case 'Z':
      arg1 = 0;
      arg2 = 0;
      arg3 = 0;
      break;
    case '*':
      /*
       * Invalid commands are replaced with '*' - Ignore them.
       */
      continue;
    default:
      new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: z_save_to_disk(): Unknown cmd '%c' - NOT saving", ZCMD(zone_num, subcmd).command);
      continue;
    }
    if (ZCMD(zone_num, subcmd).command != 'V')
    fprintf(zfile, "%c %d %d %d %d\t(%s)\n",
		ZCMD(zone_num, subcmd).command, ZCMD(zone_num, subcmd).if_flag, arg1, arg2, arg3, comment);
    else
      fprintf(zfile, "%c %d %d %d %d %s %s\n",
              ZCMD(zone_num, subcmd).command, ZCMD(zone_num, subcmd).if_flag, arg1, arg2, arg3,
              ZCMD(zone_num, subcmd).sarg1, ZCMD(zone_num, subcmd).sarg2);
  }
  fputs("S\n$\n", zfile);
  fclose(zfile);
  snprintf(oldname, sizeof(oldname), "%s/%d/%d.zon", LIB_WORLD, zone_table[zone_num].number, zone_table[zone_num].number);
  remove(oldname);
  rename(fname, oldname);
  
  if (in_save_list(zone_table[zone_num].number, SL_ZON))
    remove_from_save_list(zone_table[zone_num].number, SL_ZON);
  return TRUE;
}

/*-------------------------------------------------------------------*/

/*
 * Some common code to count the number of comands in the list.
 */
int count_commands(struct reset_com *list)
{
  int count = 0;

  while (list[count].command != 'S')
    count++;

  return count;
}

/*-------------------------------------------------------------------*/

/*
 * Adds a new reset command into a list.  Takes a pointer to the list
 * so that it may play with the memory locations.
 */
void add_cmd_to_list(struct reset_com **list, struct reset_com *newcmd, int pos)
{
  int count, i, l;
  struct reset_com *newlist;

  /*
   * Count number of commands (not including terminator).
   */
  count = count_commands(*list);

  /*
   * Value is +2 for the terminator and new field to add.
   */
  CREATE(newlist, struct reset_com, count + 2);

  /*
   * Even tighter loop to copy the old list and insert a new command.
   */
  for (i = 0, l = 0; i <= count; i++) {
    newlist[i] = ((i == pos) ? *newcmd : (*list)[l++]);
  }

  /*
   * Add terminator, then insert new list.
   */
  newlist[count + 1].command = 'S';
  free(*list);
  *list = newlist;
}

/*-------------------------------------------------------------------*/

/*
 * Remove a reset command from a list.	Takes a pointer to the list
 * so that it may play with the memory locations.
 */
void remove_cmd_from_list(struct reset_com **list, int pos)
{
  int count, i, l;
  struct reset_com *newlist;

  /*
   * Count number of commands (not including terminator)  
   */
  count = count_commands(*list);

  /*
   * Value is 'count' because we didn't include the terminator above
   * but since we're deleting one thing anyway we want one less.
   */
  CREATE(newlist, struct reset_com, count);

  /*
   * Even tighter loop to copy old list and skip unwanted command.
   */
  for (i = 0, l = 0; i < count; i++) {
    if (i != pos) {
      newlist[l++] = (*list)[i];
    }
  }
  /*
   * Add the terminator, then insert the new list.
   */
  newlist[count - 1].command = 'S';
  free(*list);
  *list = newlist;
}

/*-------------------------------------------------------------------*/

/*
 * Error check user input and then add new (blank) command  
 */
int new_command(struct zone_data *zone, int pos)
{
  int subcmd = 0;
  struct reset_com *new_com;

  /*
   * Error check to ensure users hasn't given too large an index  
   */
  while (zone->cmd[subcmd].command != 'S')
    subcmd++;

  if (pos < 0 || pos > subcmd)
    return 0;

  /*
   * Ok, let's add a new (blank) command 
   */
  CREATE(new_com, struct reset_com, 1);
  new_com->command = 'N';
  add_cmd_to_list(&zone->cmd, new_com, pos);
  return 1;
}

/*-------------------------------------------------------------------*/

/*
 * Error check user input and then remove command  
 */
void delete_command(struct zone_data *zone, int pos)
{
  int subcmd = 0;

  /*
   * Error check to ensure users hasn't given too large an index  
   */
  while (zone->cmd[subcmd].command != 'S')
    subcmd++;

  if (pos < 0 || pos >= subcmd)
    return;

  /*
   * Ok, let's zap it  
   */
  remove_cmd_from_list(&zone->cmd, pos);
}

/*-------------------------------------------------------------------*/
