/************************************************************************
 *  OasisOLC - Rooms / redit.c					v2.0	*
 *  Original author: Levork						*
 *  Copyright 1996 Harvey Gilpin					*
 *  Copyright 1997-2001 George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "db.h"
#include "boards.h"
#include "genolc.h"
#include "genwld.h"
#include "genzon.h"
#include "oasis.h"
#include "improved-edit.h"
#include "dg_olc.h"
#include "constants.h"

/*------------------------------------------------------------------------*/

/*
 * External data structures.
 */
extern struct room_data *world_vnum[];
extern struct obj_data *obj_proto;
extern struct char_data *mob_proto;
extern const char *room_bits[];
extern const char *sector_types[];
extern const char *exit_bits[];
extern struct zone_data *zone_table;
extern struct descriptor_data *descriptor_list;

/*------------------------------------------------------------------------*/


/**-----------------------------------------------------------------------*\
  Utils and exported functions.
\*------------------------------------------------------------------------*/

ACMD(do_oasis_redit)
{
  char *buf3;
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];
  int number = NOWHERE, save = 0;
  struct descriptor_data *d;

  /* Parse any arguments. */
  buf3 = two_arguments(argument, buf1, buf2);

  if (!*buf1)
    number = GET_ROOM_VNUM(IN_ROOM(ch));
  else if (!isdigit(*buf1))
  {
    if (str_cmp("save", buf1) != 0)
    {
      new_send_to_char(ch, "Yikes!  Stop that, someone will get hurt!\r\n");
      return;
    }

    save = TRUE;

    if (is_number(buf2))
      number = atoi(buf2);
    else if (GET_OLC_ZONE(ch) >= 0)
    {
      zone_rnum zlok;

      if ((zlok = real_zone(GET_OLC_ZONE(ch))) == NOWHERE)
        number = NOWHERE;
      else
        number = genolc_zone_bottom(zlok);
    }

    if (number == NOWHERE)
    {
      new_send_to_char(ch, "Save which zone?\r\n");
      return;
    }
  }

  /*
   * If a numeric argument was given (like a room number), get it.
   */
  if (number == NOWHERE)
    number = atoi(buf1);

  /* Check to make sure the room isn't already being edited. */
  for (d = descriptor_list; d; d = d->next)
  {
    if (STATE(d) == CON_REDIT)
    {
      if (d->olc && OLC_NUM(d) == number)
      {
        new_send_to_char(ch, "That room is currently being edited by %s.\r\n",
                         PERS(d->character, ch));
        return;
      }
    }
  }

  /* Retrieve the player's descriptor. */
  d = ch->desc;

  /* Give the descriptor an OLC structure. */
  if (d->olc)
  {
    new_mudlog(BRF, LVL_IMMORT, TRUE, "SYSERR: do_oasis_redit: Player already had olc structure.");
    free(d->olc);
  }

  /* Create the OLC structure. */
  CREATE(d->olc, struct oasis_olc_data, 1);

  /* Find the zone. */
  OLC_ZNUM(d) = save ? real_zone(number) : real_zone_by_thing(number);
  if (OLC_ZNUM(d) == NOWHERE)
  {
    new_send_to_char(ch, "Sorry, there is no zone for that number!\r\n");
    free(d->olc);
    d->olc = NULL;
    return;
  }

  /* Make sure the builder is allowed to modify this zone. */
  if (!can_edit_zone(ch, OLC_ZNUM(d)))
  {
    new_send_to_char(ch, "You do not have permission to edit this zone.\r\n");
    new_mudlog(BRF, LVL_IMPL, TRUE, "OLC: %s tried to edit zone %d allowed zone %d",
               GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));

    free(d->olc);
    d->olc = NULL;
    return;
  }

  if (save)
  {
    new_send_to_char(ch, "Saving all rooms in zone %d.\r\n", zone_table[OLC_ZNUM(d)].number);
    new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(ch)), TRUE, "OLC: %s saves room info for zone %d.", GET_NAME(ch), zone_table[OLC_ZNUM(d)].number);

    /* Save the rooms. */
    save_rooms(OLC_ZNUM(d));

    /* Free the olc data from the descriptor. */
    free(d->olc);
    d->olc = NULL;
    return;
  }

  OLC_NUM(d) = number;

  if (world_vnum[number])
    redit_setup_existing(d, number);
  else
    redit_setup_new(d);

  STATE(d) = CON_REDIT;
  act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);

  new_mudlog(BRF, LVL_IMMORT, TRUE, "OLC: %s starts editing zone %d allowed zone %d",
             GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));
}

void redit_setup_new(struct descriptor_data *d)
{
  CREATE(OLC_ROOM(d), struct room_data, 1);

  OLC_ROOM(d)->name = strdup("An unfinished room");
  OLC_ROOM(d)->description = strdup("You are in an unfinished room.\r\n");
  OLC_ROOM(d)->smell = strdup("You smell nothing of interest.\r\n");
  OLC_ROOM(d)->listen = strdup("You hear nothing interesting.\r\n");
  OLC_ROOM(d)->number = NOWHERE;
  OLC_ROOM(d)->mine.num = -1;
  OLC_ROOM(d)->mine.dif = -1;
  OLC_ROOM(d)->mine.tool = 0;
  OLC_ITEM_TYPE(d) = WLD_TRIGGER;
  OLC_ROOM(d)->proto_script = OLC_SCRIPT(d) = NULL;
  redit_disp_menu(d);
  OLC_VAL(d) = 0;
}

/*------------------------------------------------------------------------*/

void redit_setup_existing(struct descriptor_data *d, room_vnum v_num)
{
  /*
   * Build a copy of the room for editing.
   */
  CREATE(OLC_ROOM(d), struct room_data, 1);

  *OLC_ROOM(d) = *world_vnum[v_num];

  copy_room_strings(OLC_ROOM(d), world_vnum[v_num]);
  /*
   * Attach copy of room to player's descriptor.
   */
  OLC_VAL(d) = 0;
  OLC_ITEM_TYPE(d) = WLD_TRIGGER;
  OLC_ROOM(d)->script = NULL;
  OLC_ROOM(d)->proto_script = NULL;
  OLC_ROOM(d)->contents = NULL;
  OLC_ROOM(d)->people = NULL;
  dg_olc_script_copy(d);
  redit_disp_menu(d);
}

/*------------------------------------------------------------------------*/

void redit_save_internally(struct descriptor_data *d)
{
  int j,  new_room = FALSE;
  room_rnum room_num;
  struct descriptor_data *dsc;

  if (OLC_ROOM(d)->number == NOWHERE)
  {
    new_room = TRUE;
    OLC_ROOM(d)->number = OLC_NUM(d);
    CREATE(world_vnum[OLC_ROOM(d)->number], struct room_data, 1);
  }
  /* FIXME: Why is this not set elsewhere? */
  OLC_ROOM(d)->zone = OLC_ZNUM(d);

  if ((room_num = add_room(OLC_ROOM(d))) == NULL)
  {
    write_to_output(d, "Something went wrong...\r\n");
    log("SYSERR: redit_save_internally: Something failed! (%d)", room_num->number);
    return;
  }
  /* Update triggers */
  /* Free old proto list */
  if (room_num->proto_script && room_num->proto_script != OLC_SCRIPT(d))
    free_proto_script(room_num, WLD_TRIGGER);

  room_num->proto_script = OLC_SCRIPT(d);
  assign_triggers(room_num, WLD_TRIGGER);
  /* end trigger update */


  /* Don't adjust numbers on a room update. */
  if (!new_room)
    return;

  /* Idea contributed by C.Raehl 4/27/99 */
  for (dsc = descriptor_list; dsc; dsc = dsc->next)
  {
    if (dsc == d)
      continue;

    if (STATE(dsc) == CON_ZEDIT)
    {
      for (j = 0; OLC_ZONE(dsc)->cmd[j].command != 'S'; j++)
        switch (OLC_ZONE(dsc)->cmd[j].command)
        {
        case 'O':
        case 'M':
        case 'T':
        case 'V':
          OLC_ZONE(dsc)->cmd[j].arg3 += (OLC_ZONE(dsc)->cmd[j].arg3 >= room_num->number);
          break;
        case 'D':
          OLC_ZONE(dsc)->cmd[j].arg2 += (OLC_ZONE(dsc)->cmd[j].arg2 >= room_num->number);
          /* Fall through */
        case 'R':
          OLC_ZONE(dsc)->cmd[j].arg1 += (OLC_ZONE(dsc)->cmd[j].arg1 >= room_num->number);
          break;
        }
    }/* else if (STATE(dsc) == CON_REDIT) {
                  for (j = 0; j < NUM_OF_DIRS; j++)
                    if (OLC_ROOM(dsc)->dir_option[j])
                      if (OLC_ROOM(dsc)->dir_option[j]->to_room >= room_num)
                        OLC_ROOM(dsc)->dir_option[j]->to_room++;
                }*/
  }
}

/*------------------------------------------------------------------------*/

void redit_save_to_disk(zone_vnum zone_num)
{
  save_rooms(zone_num);		/* :) */
}

/*------------------------------------------------------------------------*/

void free_room(struct room_data *room)
{
  /* Free the strings (Mythran). */
  free_strings(room, OASIS_WLD);

  if (SCRIPT(room))
    extract_script(room, WLD_TRIGGER);
  free_proto_script(room, WLD_TRIGGER);
  /* Free the room. */
  free(room);	/* XXX ? */
  room = NULL;
}

/**************************************************************************
 Menu functions 
 **************************************************************************/
void redit_disp_mine_menu(struct descriptor_data *d)
{
  write_to_output(d,
                  "%s1%s) Mine Number     : %s%d\r\n"
                  "%s2%s) Mine Level      : %s%d\r\n"
                  "%s3%s) Mine Tool Needed: %s%s%s\r\n",
                  grn, nrm, yel, OLC_ROOM(d)->mine.num,
                  grn, nrm, yel, OLC_ROOM(d)->mine.dif,
                  grn, nrm, yel, OLC_ROOM(d)->mine.tool == TOOL_SHOVEL ? "Shovel" : "Pickaxe",
                  nrm
                 );
  write_to_output(d, "(Setting Mine Number as -1 will remove room from a mine) \r\n");
  write_to_output(d, "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_MINE_MENU;
}
/*
 * For extra descriptions.
 */
void redit_disp_extradesc_menu(struct descriptor_data *d)
{
  struct extra_descr_data *extra_desc = OLC_DESC(d);

  clear_screen(d);
  write_to_output(d,
                  "%s1%s) Keyword: %s%s\r\n"
                  "%s2%s) Description:\r\n%s%s\r\n"
                  "%s3%s) Goto next description: ",

                  grn, nrm, yel, extra_desc->keyword ? extra_desc->keyword : "<NONE>",
                  grn, nrm, yel, extra_desc->description ? extra_desc->description : "<NONE>",
                  grn, nrm
                 );

  write_to_output(d, !extra_desc->next ? "<NOT SET>\r\n" : "Set.\r\n");
  write_to_output(d, "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_EXTRADESC_MENU;
}
void redit_disp_look_above_menu(struct descriptor_data *d)
{
  struct extra_descr_data *extra_desc = OLC_LADESC(d);

  clear_screen(d);
  write_to_output(d,
                  "%s1%s) Keyword: %s%s\r\n"
                  "%s2%s) Description:\r\n%s%s\r\n"
                  "%s3%s) Goto next look above description: ",

                  grn, nrm, yel, extra_desc->keyword ? extra_desc->keyword : "<NONE>",
                  grn, nrm, yel, extra_desc->description ? extra_desc->description : "<NONE>",
                  grn, nrm
                 );

  write_to_output(d, !extra_desc->next ? "<NOT SET>\r\n" : "Set.\r\n");
  write_to_output(d, "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_LOOK_ABOVE_MENU;
}
void redit_disp_look_under_menu(struct descriptor_data *d)
{
  struct extra_descr_data *extra_desc = OLC_LUDESC(d);

  clear_screen(d);
  write_to_output(d,
                  "%s1%s) Keyword: %s%s\r\n"
                  "%s2%s) Description:\r\n%s%s\r\n"
                  "%s3%s) Goto next look under description: ",

                  grn, nrm, yel, extra_desc->keyword ? extra_desc->keyword : "<NONE>",
                  grn, nrm, yel, extra_desc->description ? extra_desc->description : "<NONE>",
                  grn, nrm
                 );

  write_to_output(d, !extra_desc->next ? "<NOT SET>\r\n" : "Set.\r\n");
  write_to_output(d, "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_LOOK_UNDER_MENU;
}
void redit_disp_look_behind_menu(struct descriptor_data *d)
{
  struct extra_descr_data *extra_desc = OLC_LBDESC(d);

  clear_screen(d);
  write_to_output(d,
                  "%s1%s) Keyword: %s%s\r\n"
                  "%s2%s) Description:\r\n%s%s\r\n"
                  "%s3%s) Goto next look behind description: ",

                  grn, nrm, yel, extra_desc->keyword ? extra_desc->keyword : "<NONE>",
                  grn, nrm, yel, extra_desc->description ? extra_desc->description : "<NONE>",
                  grn, nrm
                 );

  write_to_output(d, !extra_desc->next ? "<NOT SET>\r\n" : "Set.\r\n");
  write_to_output(d, "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_LOOK_BEHIND_MENU;
}
/*
 * For exits.
 */
void redit_disp_exit_menu(struct descriptor_data *d)
{
  char door_buf[24];
  /*
   * if exit doesn't exist, alloc/create it 
   */
  if (OLC_EXIT(d) == NULL)
    CREATE(OLC_EXIT(d), struct room_direction_data, 1);

  /*
   * Weird door handling! 
   */
  if (IS_SET(OLC_EXIT(d)->exit_info, EX_ISDOOR))
  {
    if (IS_SET(OLC_EXIT(d)->exit_info, EX_PICKPROOF))
      strncpy(door_buf, "Pickproof", sizeof(door_buf)-1);
    else
      strncpy(door_buf, "Is a door", sizeof(door_buf)-1);
  }
  else
    strncpy(door_buf, "No door", sizeof(door_buf)-1);

  get_char_colors(d->character);
  clear_screen(d);
  write_to_output(d,
                  "%s1%s) Exit to     : %s%d - %s\r\n"
                  "%s2%s) Description :-\r\n%s%s\r\n"
                  "%s3%s) Door name   : %s%s\r\n"
                  "%s4%s) Key         : %s%d\r\n"
                  "%s5%s) Door flags  : %s%s\r\n"
                  "%s6%s) Purge exit.\r\n"
                  "Enter choice, 0 to quit : ",

                  grn, nrm, cyn, OLC_EXIT(d)->to_room != NULL ? OLC_EXIT(d)->to_room->number : -1,
                  OLC_EXIT(d)->to_room != NULL ? OLC_EXIT(d)->to_room->name : "Nowhere",
                  grn, nrm, yel, OLC_EXIT(d)->general_description ? OLC_EXIT(d)->general_description : "<NONE>",
                  grn, nrm, yel, OLC_EXIT(d)->keyword ? OLC_EXIT(d)->keyword : "<NONE>",
                  grn, nrm, cyn, OLC_EXIT(d)->key != NOTHING ? OLC_EXIT(d)->key : -1,
                  grn, nrm, cyn, door_buf, grn, nrm
                 );

  OLC_MODE(d) = REDIT_EXIT_MENU;
}

/*
 * For exit flags.
 */
void redit_disp_exit_flag_menu(struct descriptor_data *d)
{
  get_char_colors(d->character);
  write_to_output(d, "%s0%s) No door\r\n"
                  "%s1%s) Closeable door\r\n"
                  "%s2%s) Pickproof\r\n"
                  "Enter choice : ", grn, nrm, grn, nrm, grn, nrm);
}

/*
 * For room flags.
 */
void redit_disp_flag_menu(struct descriptor_data *d)
{
  char bits[MAX_STRING_LENGTH];
  int counter, columns = 0;

  get_char_colors(d->character);
  clear_screen(d);
  for (counter = 0; counter < NUM_ROOM_FLAGS; counter++)
  {
    write_to_output(d, "%s%2d%s) %-20.20s %s", grn, counter + 1, nrm,
                    room_bits[counter], !(++columns % 2) ? "\r\n" : "");
  }

  sprintbitarray(OLC_ROOM(d)->room_flags, room_bits, RF_ARRAY_MAX, bits, sizeof(bits));
  write_to_output(d, "\r\nRoom flags: %s%s%s\r\n"
                  "Enter room flags, 0 to quit : ", cyn, bits, nrm);
  OLC_MODE(d) = REDIT_FLAGS;
}

/*
 * For sector type.
 */
void redit_disp_sector_menu(struct descriptor_data *d)
{
  int counter, columns = 0;

  clear_screen(d);
  for (counter = 0; counter < NUM_ROOM_SECTORS; counter++)
  {
    write_to_output(d, "%s%2d%s) %-20.20s %s", grn, counter, nrm,
                    sector_types[counter], !(++columns % 2) ? "\r\n" : "");
  }
  write_to_output(d, "\r\nEnter sector type : ");
  OLC_MODE(d) = REDIT_SECTOR;
}

/*
 * The main menu.
 */
void redit_disp_menu(struct descriptor_data *d)
{
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];
  struct room_data *room;

  get_char_colors(d->character);
  clear_screen(d);
  room = OLC_ROOM(d);
  sprintbitarray(room->room_flags, room_bits, RF_ARRAY_MAX, buf1, sizeof(buf1));
  sprinttype(room->sector_type, sector_types, buf2, sizeof(buf2));
  write_to_output(d,
                  "-- Room number : [%s%d%s]  	Room zone: [%s%d%s]\r\n"
                  "%s1%s) Name         : %s%s\r\n"
                  "%s2%s) Description  :\r\n%s%s"
                  "%s3%s) Room flags   : %s%s\r\n"
                  "%s4%s) Sector type  : %s%s\r\n"
                  "%s5%s) Exit north   : %s%-6d - %s\r\n"
                  "%s6%s) Exit east    : %s%-6d - %s\r\n"
                  "%s7%s) Exit south   : %s%-6d - %s\r\n"
                  "%s8%s) Exit west    : %s%-6d - %s\r\n"
                  "%s9%s) Exit up      : %s%-6d - %s\r\n"
                  "%sA%s) Exit down    : %s%-6d - %s\r\n"
                  "%sB%s) Descriptions :%s Extra\r\n"
                  /*---4d--------------------------------------*/
                  "%sE%s) Descriptions :%s Look under\r\n"
                  "%sF%s) Descriptions :%s Look behind\r\n"
                  "%sG%s) Descriptions :%s Look above\r\n"
                  "%sH%s) Smell        :\r\n%s%s"
                  "%sI%s) Listen       :\r\n%s%s"
                  "%sJ%s) Mine         : %sNum: %d Level: %d Tool: %s\r\n"

                  /*---end-------------------------------------*/
                  "%sS%s) Script       : %s%s\r\n"
                  "%sD%s) Delete Room\r\n"
                  "%sQ%s) Quit\r\n"
                  "Enter choice : ",

                  cyn, OLC_NUM(d), nrm,
                  cyn, zone_table[OLC_ZNUM(d)].number, nrm,
                  grn, nrm, yel, room->name,
                  grn, nrm, yel, room->description,
                  grn, nrm, cyn, buf1,
                  grn, nrm, cyn, buf2,
                  grn, nrm, cyn,
                  room->dir_option[NORTH] && room->dir_option[NORTH]->to_room != NULL ?
                  room->dir_option[NORTH]->to_room->number : -1,
                  room->dir_option[NORTH] && room->dir_option[NORTH]->to_room != NULL ?
                  room->dir_option[NORTH]->to_room->name : "Nowhere",
                  grn, nrm, cyn,
                  room->dir_option[EAST] && room->dir_option[EAST]->to_room != NULL ?
                  room->dir_option[EAST]->to_room->number : -1,
                  room->dir_option[EAST] && room->dir_option[EAST]->to_room != NULL ?
                  room->dir_option[EAST]->to_room->name : "Nowhere",
                  grn, nrm, cyn,
                  room->dir_option[SOUTH] && room->dir_option[SOUTH]->to_room != NULL ?
                  room->dir_option[SOUTH]->to_room->number : -1,
                  room->dir_option[SOUTH] && room->dir_option[SOUTH]->to_room != NULL ?
                  room->dir_option[SOUTH]->to_room->name : "Nowhere",
                  grn, nrm, cyn,
                  room->dir_option[WEST] && room->dir_option[WEST]->to_room != NULL ?
                  room->dir_option[WEST]->to_room->number : -1,
                  room->dir_option[WEST] && room->dir_option[WEST]->to_room != NULL ?
                  room->dir_option[WEST]->to_room->name : "Nowhere",
                  grn, nrm, cyn,
                  room->dir_option[UP] && room->dir_option[UP]->to_room != NULL ?
                  room->dir_option[UP]->to_room->number : -1,
                  room->dir_option[UP] && room->dir_option[UP]->to_room != NULL ?
                  room->dir_option[UP]->to_room->name : "Nowhere",
                  grn, nrm, cyn,
                  room->dir_option[DOWN] && room->dir_option[DOWN]->to_room != NULL ?
                  room->dir_option[DOWN]->to_room->number : -1,
                  room->dir_option[DOWN] && room->dir_option[DOWN]->to_room != NULL ?
                  room->dir_option[DOWN]->to_room->name : "Nowhere",
                  grn, nrm, cyn,/*extra desc */
                  grn, nrm, cyn,/* under */
                  grn, nrm, cyn,/* behind */
                  grn, nrm, cyn,/* above */
                  grn, nrm, cyn, room->smell,/* smell */
                  grn, nrm, cyn, room->listen,/* listen */
                  grn, nrm, cyn, room->mine.num, room->mine.dif, room->mine.num == -1 ? "None" : room->mine.tool == TOOL_SHOVEL ? "Shovel" : "Pickaxe",
                  grn, nrm, cyn, OLC_SCRIPT(d) ? "Set." : "Not Set.",
                  grn, nrm,/*delete*/
                  grn, nrm /*quit*/


                 );

  OLC_MODE(d) = REDIT_MAIN_MENU;
}

/**************************************************************************
  The main loop
 **************************************************************************/

void redit_parse(struct descriptor_data *d, char *arg)
{
  int number;
  char *oldtext = NULL;

  switch (OLC_MODE(d))
  {
  case REDIT_CONFIRM_SAVESTRING:
    switch (*arg)
    {
    case 'y':
    case 'Y':
      redit_save_internally(d);
      new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(d->character)), TRUE,
                 "OLC: %s edits room %d.", GET_NAME(d->character), OLC_NUM(d));
      if (CONFIG_OLC_SAVE)
      {
        redit_save_to_disk(real_zone_by_thing(OLC_NUM(d)));
        write_to_output(d, "Room saved to disk.\r\n");
      }
      else
        write_to_output(d, "Room saved to memory.\r\n");
      /*
       * Do NOT free strings! Just the room structure. 
       */
      /* okay free strings now - mord */
      free_room_strings(OLC_ROOM(d));
      cleanup_olc(d, CLEANUP_STRUCTS);
      break;
    case 'n':
    case 'N':
      /*
       * Free everything up, including strings, etc.
       */
      cleanup_olc(d, CLEANUP_ALL);
      break;
    default:
      write_to_output(d, "Invalid choice!\r\nDo you wish to save this room ? : ");
      break;
    }
    return;

  case REDIT_MAIN_MENU:
    switch (*arg)
    {
    case 'q':
    case 'Q':
      if (OLC_VAL(d))
      { /* Something has been modified. */
        write_to_output(d, "Do you wish to save this room ? : ");
        OLC_MODE(d) = REDIT_CONFIRM_SAVESTRING;
      }
      else
        cleanup_olc(d, CLEANUP_ALL);
      return;
    case '1':
      write_to_output(d, "Enter room name:-\r\n] ");
      OLC_MODE(d) = REDIT_NAME;
      break;
    case '2':
      OLC_MODE(d) = REDIT_DESC;
      clear_screen(d);
      send_editor_help(d);
      write_to_output(d, "Enter room description:\r\n\r\n");

      if (OLC_ROOM(d)->description)
      {
        write_to_output(d, "%s", OLC_ROOM(d)->description);
        oldtext = strdup(OLC_ROOM(d)->description);
      }
      string_write(d, &OLC_ROOM(d)->description, MAX_ROOM_DESC, 0, oldtext);
      OLC_VAL(d) = 1;
      break;
    case '3':
      redit_disp_flag_menu(d);
      break;
    case '4':
      redit_disp_sector_menu(d);
      break;
    case '5':
      OLC_VAL(d) = NORTH;
      redit_disp_exit_menu(d);
      break;
    case '6':
      OLC_VAL(d) = EAST;
      redit_disp_exit_menu(d);
      break;
    case '7':
      OLC_VAL(d) = SOUTH;
      redit_disp_exit_menu(d);
      break;
    case '8':
      OLC_VAL(d) = WEST;
      redit_disp_exit_menu(d);
      break;
    case '9':
      OLC_VAL(d) = UP;
      redit_disp_exit_menu(d);
      break;
    case 'a':
    case 'A':
      OLC_VAL(d) = DOWN;
      redit_disp_exit_menu(d);
      break;
    case 'b':
    case 'B':
      /*
       * If the extra description doesn't exist.
       */
      if (!OLC_ROOM(d)->ex_description)
      {
        CREATE(OLC_ROOM(d)->ex_description, struct extra_descr_data, 1);
        OLC_ROOM(d)->ex_description->next = NULL;
      }
      OLC_DESC(d) = OLC_ROOM(d)->ex_description;
      redit_disp_extradesc_menu(d);
      break;
    case 'e':
    case 'E':
      /*
       * If the look under description doesn't exist.
       */
      if (!OLC_ROOM(d)->look_under_description)
      {
        CREATE(OLC_ROOM(d)->look_under_description,
               struct extra_descr_data, 1);
        OLC_ROOM(d)->look_under_description->next = NULL;
      }
      OLC_LUDESC(d) = OLC_ROOM(d)->look_under_description;
      redit_disp_look_under_menu(d);
      break;
    case 'f':
    case 'F':
      /*
       * If the look behind description doesn't exist.
       */
      if (!OLC_ROOM(d)->look_behind_description)
      {
        CREATE(OLC_ROOM(d)->look_behind_description, struct extra_descr_data, 1);
        OLC_ROOM(d)->look_behind_description->next = NULL;
      }
      OLC_LBDESC(d) = OLC_ROOM(d)->look_behind_description;
      redit_disp_look_behind_menu(d);
      break;
    case 'g':
    case 'G':
      /*
        * If the look above description doesn't exist.
        */
      if (!OLC_ROOM(d)->look_above_description)
      {
        CREATE(OLC_ROOM(d)->look_above_description, struct extra_descr_data, 1);
        OLC_ROOM(d)->look_above_description->next = NULL;
      }
      OLC_LADESC(d) = OLC_ROOM(d)->look_above_description;
      redit_disp_look_above_menu(d);
      break;
    case 'h':
    case 'H':
      OLC_MODE(d) = REDIT_SMELL;
      clear_screen(d);
      send_editor_help(d);
      write_to_output(d, "Enter Smell description:\r\n\r\n");

      if (OLC_ROOM(d)->smell)
      {
        write_to_output(d, "%s", OLC_ROOM(d)->smell);
        oldtext = strdup(OLC_ROOM(d)->smell);
      }
      string_write(d, &OLC_ROOM(d)->smell, MAX_ROOM_DESC, 0, oldtext);
      OLC_VAL(d) = 1;
      break;
    case 'i':
    case 'I':
      OLC_MODE(d) = REDIT_LISTEN;
      clear_screen(d);
      send_editor_help(d);
      write_to_output(d, "Enter Listen description:\r\n\r\n");

      if (OLC_ROOM(d)->listen)
      {
        write_to_output(d, "%s", OLC_ROOM(d)->listen);
        oldtext = strdup(OLC_ROOM(d)->listen);
      }
      string_write(d, &OLC_ROOM(d)->listen, MAX_ROOM_DESC, 0, oldtext);
      OLC_VAL(d) = 1;
      break;
    case 'd':
    case 'D':
      /* Delete the room, prompt first. */
      write_to_output(d, "Are you sure you want to delete this room? ");
      OLC_MODE(d) = REDIT_DELETE;
      break;
    case 's':
    case 'S':
      OLC_SCRIPT_EDIT_MODE(d) = SCRIPT_MAIN_MENU;
      dg_script_menu(d);
      return;
    case 'j':
    case 'J':
      redit_disp_mine_menu(d);
      return;
    default:
      write_to_output(d, "Invalid choice!");
      redit_disp_menu(d);
      break;
    }
    return;

  case REDIT_MINE_MENU:
    switch (*arg)
    {
    case '0':
      break;
    case '1':
      OLC_MODE(d) = REDIT_MINE_NUMBER;
      write_to_output(d, "Enter Mine Number : ");
      return;
    case '2':
      OLC_MODE(d) = REDIT_MINE_DIFFICULTY;
      write_to_output(d, "Enter Mine Difficulty :");
      return;
    case '3':
      OLC_MODE(d) = REDIT_MINE_TOOL;
      write_to_output(d, "0) Shovel\r\n1) Pickaxe\r\nEnter a number:");
      return;
    }
    break;
  case REDIT_MINE_NUMBER:
    number = atoi(arg);
    if (number < -1)
    {
      write_to_output(d, "You can't choose a number below -1\r\n");
      return;
    }
    else if (number > 100)
    {
      write_to_output(d, "You can't choose a number above 100\r\n");
      return;
    }
    OLC_ROOM(d)->mine.num = number;
    break;
  case REDIT_MINE_DIFFICULTY:
    number = atoi(arg);
    if (number < 0)
    {
      write_to_output(d, "You can't choose a number below 0\r\n");
      return;
    }
    else if (number > 5)
    {
      write_to_output(d, "You can't choose a number above 5\r\n");
      return;
    }
    OLC_ROOM(d)->mine.dif = number;
    break;
  case REDIT_MINE_TOOL:
    number = atoi(arg);
    if (number < 0)
    {
      write_to_output(d, "You can't choose a number below 0\r\n");
      return;
    }
    else if (number > 1)
    {
      write_to_output(d, "You can't choose a number above 1\r\n");
      return;
    }
    OLC_ROOM(d)->mine.tool = number;
    break;

  case OLC_SCRIPT_EDIT:
    if (dg_script_edit_parse(d, arg)) return;
    break;

  case REDIT_NAME:
    if (!genolc_checkstring(d, arg))
      break;
    if (OLC_ROOM(d)->name)
      free(OLC_ROOM(d)->name);
    arg[MAX_ROOM_NAME - 1] = '\0';
    OLC_ROOM(d)->name = str_udup(arg);
    break;

  case REDIT_DESC:
    /*
     * We will NEVER get here, we hope.
     */
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: Reached REDIT_DESC case in parse_redit().");
    write_to_output(d, "Oops, in REDIT_DESC.\r\n");
    break;

  case REDIT_SMELL:
    /*
     * We will NEVER get here, we hope.
     */
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: Reached SMELL_DESC case in parse_redit().");
    write_to_output(d, "Oops, in SMELL_DESC.\r\n");
    break;

  case REDIT_LISTEN:
    /*
     * We will NEVER get here, we hope.
     */
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: Reached LISTEN_DESC case in parse_redit().");
    write_to_output(d, "Oops, in REDIT_DESC.\r\n");
    break;
  case REDIT_FLAGS:
    number = atoi(arg);
    if (number < 0 || number > NUM_ROOM_FLAGS)
    {
      write_to_output(d, "That is not a valid choice!\r\n");
      redit_disp_flag_menu(d);
    }
    else if (number == 0)
      break;
    else
    {
      /*
       * Toggle the bit.
       */
      TOGGLE_BIT_AR(OLC_ROOM(d)->room_flags, (number - 1));
      redit_disp_flag_menu(d);
    }
    return;

  case REDIT_SECTOR:
    number = atoi(arg);
    if (number < 0 || number >= NUM_ROOM_SECTORS)
    {
      write_to_output(d, "Invalid choice!");
      redit_disp_sector_menu(d);
      return;
    }
    OLC_ROOM(d)->sector_type = number;
    break;

  case REDIT_EXIT_MENU:
    switch (*arg)
    {
    case '0':
      break;
    case '1':
      OLC_MODE(d) = REDIT_EXIT_NUMBER;
      write_to_output(d, "Exit to room number : ");
      return;
    case '2':
      OLC_MODE(d) = REDIT_EXIT_DESCRIPTION;
      send_editor_help(d);
      write_to_output(d, "Enter exit description:\r\n\r\n");
      if (OLC_EXIT(d)->general_description)
      {
        write_to_output(d, "%s", OLC_EXIT(d)->general_description);
        oldtext = strdup(OLC_EXIT(d)->general_description);
      }
      string_write(d, &OLC_EXIT(d)->general_description, MAX_EXIT_DESC, 0, oldtext);
      return;
    case '3':
      OLC_MODE(d) = REDIT_EXIT_KEYWORD;
      write_to_output(d, "Enter keywords : ");
      return;
    case '4':
      OLC_MODE(d) = REDIT_EXIT_KEY;
      write_to_output(d, "Enter key number : ");
      return;
    case '5':
      OLC_MODE(d) = REDIT_EXIT_DOORFLAGS;
      redit_disp_exit_flag_menu(d);
      return;
    case '6':
      /*
       * Delete an exit.
       */
      if (OLC_EXIT(d)->keyword)
        free(OLC_EXIT(d)->keyword);
      if (OLC_EXIT(d)->general_description)
        free(OLC_EXIT(d)->general_description);
      if (OLC_EXIT(d))
        free(OLC_EXIT(d));
      OLC_EXIT(d) = NULL;
      break;
    default:
      write_to_output(d, "Try again : ");
      return;
    }
    break;

  case REDIT_EXIT_NUMBER:
    {
      room_rnum rm = NULL;
      if ((number = atoi(arg)) != -1)
        if ((rm = real_room(number)) == NULL)
        {
          write_to_output(d, "That room does not exist, try again : ");
          return;
        }
      OLC_EXIT(d)->to_room = rm;
      redit_disp_exit_menu(d);
      return;
    }
    break;
  case REDIT_EXIT_DESCRIPTION:
    /*
     * We should NEVER get here, hopefully.
     */
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: Reached REDIT_EXIT_DESC case in parse_redit");
    write_to_output(d, "Oops, in REDIT_EXIT_DESCRIPTION.\r\n");
    break;

  case REDIT_EXIT_KEYWORD:
    if (OLC_EXIT(d)->keyword)
      free(OLC_EXIT(d)->keyword);
    OLC_EXIT(d)->keyword = str_udup(arg);
    redit_disp_exit_menu(d);
    return;

  case REDIT_EXIT_KEY:
    number = atoi(arg);
    if (number < 0)
      OLC_EXIT(d)->key = NOTHING;
    else
      OLC_EXIT(d)->key = number;
    redit_disp_exit_menu(d);
    return;

  case REDIT_EXIT_DOORFLAGS:
    number = atoi(arg);
    if (number < 0 || number > 2)
    {
      write_to_output(d, "That's not a valid choice!\r\n");
      redit_disp_exit_flag_menu(d);
    }
    else
    {
      /*
       * Doors are a bit idiotic, don't you think? :) -- I agree. -gg
       */
      OLC_EXIT(d)->exit_info = (number == 0 ? 0 :
                                (number == 1 ? EX_ISDOOR :
                                 (number == 2 ? EX_ISDOOR | EX_PICKPROOF : 0)));
      /*
       * Jump back to the menu system.
       */
      redit_disp_exit_menu(d);
    }
    return;

  case REDIT_EXTRADESC_KEY:
    if (genolc_checkstring(d, arg))
    {
      if (OLC_DESC(d)->keyword)
        free(OLC_DESC(d)->keyword);
      OLC_DESC(d)->keyword = str_udup(arg);
    }
    redit_disp_extradesc_menu(d);
    return;

  case REDIT_EXTRADESC_MENU:
    switch ((number = atoi(arg)))
    {
    case 0:
      /*
       * If something got left out, delete the extra description
       * when backing out to the menu.
       */
      if (OLC_DESC(d)->keyword == NULL || OLC_DESC(d)->description == NULL)
      {
        struct extra_descr_data *temp;
        if (OLC_DESC(d)->keyword)
          free(OLC_DESC(d)->keyword);
        if (OLC_DESC(d)->description)
          free(OLC_DESC(d)->description);

        /*
         * Clean up pointers.
         */
        REMOVE_FROM_LIST(OLC_DESC(d), OLC_ROOM(d)->ex_description, next);
        free(OLC_DESC(d));
      }
      break;
    case 1:
      OLC_MODE(d) = REDIT_EXTRADESC_KEY;
      write_to_output(d, "Enter keywords, separated by spaces : ");
      return;
    case 2:
      OLC_MODE(d) = REDIT_EXTRADESC_DESCRIPTION;
      send_editor_help(d);
      write_to_output(d, "Enter extra description:\r\n\r\n");
      if (OLC_DESC(d)->description)
      {
        write_to_output(d, "%s", OLC_DESC(d)->description);
        oldtext = strdup(OLC_DESC(d)->description);
      }
      string_write(d, &OLC_DESC(d)->description, MAX_MESSAGE_LENGTH, 0, oldtext);
      return;
    case 3:
      if (OLC_DESC(d)->keyword == NULL || OLC_DESC(d)->description == NULL)
      {
        write_to_output(d, "You can't edit the next extra description without completing this one.\r\n");
        redit_disp_extradesc_menu(d);
      }
      else
      {
        struct extra_descr_data *new_extra;

        if (OLC_DESC(d)->next)
          OLC_DESC(d) = OLC_DESC(d)->next;
        else
        {
          /*
           * Make new extra description and attach at end.
           */
          CREATE(new_extra, struct extra_descr_data, 1);
          OLC_DESC(d)->next = new_extra;
          OLC_DESC(d) = new_extra;
        }
        redit_disp_extradesc_menu(d);
      }
      return;
    }
    break;

    /*-----4d------*/
  case REDIT_LOOK_UNDER_KEY:
    if (genolc_checkstring(d, arg))
    {
      if (OLC_LUDESC(d)->keyword)
        free(OLC_LUDESC(d)->keyword);
      OLC_LUDESC(d)->keyword = str_udup(arg);
    }
    redit_disp_look_under_menu(d);
    return;
  case REDIT_LOOK_UNDER_MENU:
    switch ((number = atoi(arg)))
    {
    case 0:
      /*
       * If something got left out, delete the extra description
       * when backing out to the menu.
       */
      if (OLC_LUDESC(d)->keyword == NULL || OLC_LUDESC(d)->description == NULL)
      {
        struct extra_descr_data *temp;
        if (OLC_LUDESC(d)->keyword)
          free(OLC_LUDESC(d)->keyword);
        if (OLC_LUDESC(d)->description)
          free(OLC_LUDESC(d)->description);

        /*
         * Clean up pointers.
         */
        REMOVE_FROM_LIST(OLC_LUDESC(d), OLC_ROOM(d)->look_under_description, next);
        free(OLC_LUDESC(d));
      }
      break;
    case 1:
      OLC_MODE(d) = REDIT_LOOK_UNDER_KEY;
      write_to_output(d, "Enter keywords, separated by spaces : ");
      return;
    case 2:
      OLC_MODE(d) = REDIT_LOOK_UNDER_DESCRIPTION;
      send_editor_help(d);
      write_to_output(d, "Enter look under description:\r\n\r\n");
      if (OLC_LUDESC(d)->description)
      {
        write_to_output(d, "%s", OLC_LUDESC(d)->description);
        oldtext = strdup(OLC_LUDESC(d)->description);
      }
      string_write(d, &OLC_LUDESC(d)->description, MAX_MESSAGE_LENGTH, 0, oldtext);
      return;
    case 3:
      if (OLC_LUDESC(d)->keyword == NULL || OLC_LUDESC(d)->description == NULL)
      {
        write_to_output(d, "You can't edit the next under description without completing this one.\r\n");
        redit_disp_look_under_menu(d);
      }
      else
      {
        struct extra_descr_data *new_extra;

        if (OLC_LUDESC(d)->next)
          OLC_LUDESC(d) = OLC_LUDESC(d)->next;
        else
        {
          /*
           * Make new extra description and attach at end.
           */
          CREATE(new_extra, struct extra_descr_data, 1);
          OLC_LUDESC(d)->next = new_extra;
          OLC_LUDESC(d) = new_extra;
        }
        redit_disp_look_under_menu(d);
      }
      return;
    }
    break;
  case REDIT_LOOK_BEHIND_KEY:
    if (genolc_checkstring(d, arg))
    {
      if (OLC_LBDESC(d)->keyword)
        free(OLC_LBDESC(d)->keyword);
      OLC_LBDESC(d)->keyword = str_udup(arg);
    }
    redit_disp_look_behind_menu(d);
    return;
  case REDIT_LOOK_BEHIND_MENU:
    switch ((number = atoi(arg)))
    {
    case 0:
      /*
       * If something got left out, delete the extra description
       * when backing out to the menu.
       */
      if (OLC_LBDESC(d)->keyword == NULL || OLC_LBDESC(d)->description == NULL)
      {
        struct extra_descr_data *temp;
        if (OLC_LBDESC(d)->keyword)
          free(OLC_LBDESC(d)->keyword);
        OLC_LBDESC(d)->keyword = NULL;
        if (OLC_LBDESC(d)->description)
          free(OLC_LBDESC(d)->description);
        OLC_LBDESC(d)->description = NULL;

        /*
         * Clean up pointers.
         */
        REMOVE_FROM_LIST(OLC_LBDESC(d), OLC_ROOM(d)->look_behind_description, next);
        free(OLC_LBDESC(d));
        OLC_LBDESC(d) = NULL;
      }
      break;
    case 1:
      OLC_MODE(d) = REDIT_LOOK_BEHIND_KEY;
      write_to_output(d, "Enter keywords, separated by spaces : ");
      return;
    case 2:
      OLC_MODE(d) = REDIT_LOOK_BEHIND_DESCRIPTION;
      send_editor_help(d);
      write_to_output(d, "Enter look behind description:\r\n\r\n");
      if (OLC_LBDESC(d)->description)
      {
        write_to_output(d, "%s", OLC_LBDESC(d)->description);
        oldtext = strdup(OLC_LBDESC(d)->description);
      }
      string_write(d, &OLC_LBDESC(d)->description, MAX_MESSAGE_LENGTH, 0, oldtext);
      return;
    case 3:
      if (OLC_LBDESC(d)->keyword == NULL || OLC_LBDESC(d)->description == NULL)
      {
        write_to_output(d, "You can't edit the next below description without completing this one.\r\n");
        redit_disp_look_behind_menu(d);
      }
      else
      {
        struct extra_descr_data *new_extra;

        if (OLC_LBDESC(d)->next)
          OLC_LBDESC(d) = OLC_LBDESC(d)->next;
        else
        {
          /*
           * Make new extra description and attach at end.
           */
          CREATE(new_extra, struct extra_descr_data, 1);
          OLC_LBDESC(d)->next = new_extra;
          OLC_LBDESC(d) = new_extra;
        }
        redit_disp_look_behind_menu(d);
      }
      return;
    }
    break;
  case REDIT_LOOK_ABOVE_KEY:
    if (genolc_checkstring(d, arg))
    {
      if (OLC_LADESC(d)->keyword)
        free(OLC_LADESC(d)->keyword);
      OLC_LADESC(d)->keyword = str_udup(arg);
    }
    redit_disp_look_above_menu(d);
    return;
  case REDIT_LOOK_ABOVE_MENU:
    switch ((number = atoi(arg)))
    {
    case 0:
      /*
       * If something got left out, delete the extra description
       * when backing out to the menu.
       */
      if (OLC_LADESC(d)->keyword == NULL || OLC_LADESC(d)->description == NULL)
      {
        struct extra_descr_data *temp;
        if (OLC_LADESC(d)->keyword)
          free(OLC_LADESC(d)->keyword);
        if (OLC_LADESC(d)->description)
          free(OLC_LADESC(d)->description);

        /*
         * Clean up pointers.
         */
        REMOVE_FROM_LIST(OLC_LADESC(d), OLC_ROOM(d)->look_above_description, next);
        free(OLC_LADESC(d));
      }
      break;
    case 1:
      OLC_MODE(d) = REDIT_LOOK_ABOVE_KEY;
      write_to_output(d, "Enter keywords, separated by spaces : ");
      return;
    case 2:
      OLC_MODE(d) = REDIT_LOOK_ABOVE_DESCRIPTION;
      send_editor_help(d);
      write_to_output(d, "Enter look above description:\r\n\r\n");
      if (OLC_LADESC(d)->description)
      {
        write_to_output(d, "%s", OLC_LADESC(d)->description);
        oldtext = strdup(OLC_LADESC(d)->description);
      }
      string_write(d, &OLC_LADESC(d)->description, MAX_MESSAGE_LENGTH, 0, oldtext);
      return;
    case 3:
      if (OLC_LADESC(d)->keyword == NULL || OLC_LADESC(d)->description == NULL)
      {
        write_to_output(d, "You can't edit the next above description without completing this one.\r\n");
        redit_disp_look_above_menu(d);
      }
      else
      {
        struct extra_descr_data *new_extra;

        if (OLC_LADESC(d)->next)
          OLC_LADESC(d) = OLC_LADESC(d)->next;
        else
        {
          /*
           * Make new extra description and attach at end.
           */
          CREATE(new_extra, struct extra_descr_data, 1);
          OLC_LADESC(d)->next = new_extra;
          OLC_LADESC(d) = new_extra;
        }
        redit_disp_look_above_menu(d);
      }
      return;
    }
    break;
    /*-----end-----*/

  case REDIT_DELETE:
    if (*arg == 'y' || *arg == 'Y')
    {
      if (delete_room(OLC_ROOM(d)))
        write_to_output(d, "Room deleted.\r\n");
      else
        write_to_output(d, "Couldn't delete the room!.\r\n");

      cleanup_olc(d, CLEANUP_ALL);
      return;
    }
    else if (*arg == 'n' || *arg == 'N')
    {
      redit_disp_menu(d);
      OLC_MODE(d) = REDIT_MAIN_MENU;
      return;
    }
    else
      write_to_output(d, "Please answer 'Y' or 'N': ");

    break;

  default:
    /*
     * We should never get here.
     */
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: Reached default case in parse_redit");
    break;
  }
  /*
   * If we get this far, something has been changed.
   */
  OLC_VAL(d) = 1;
  redit_disp_menu(d);
}

void redit_string_cleanup(struct descriptor_data *d, int terminator)
{
  switch (OLC_MODE(d))
  {
  case REDIT_SMELL:
  case REDIT_LISTEN:
  case REDIT_DESC:
    redit_disp_menu(d);
    break;
  case REDIT_EXIT_DESCRIPTION:
    redit_disp_exit_menu(d);
    break;
  case REDIT_EXTRADESC_DESCRIPTION:
    redit_disp_extradesc_menu(d);
    break;
  case REDIT_LOOK_ABOVE_DESCRIPTION:
    redit_disp_look_above_menu(d);
    break;
  case REDIT_LOOK_BEHIND_DESCRIPTION:
    redit_disp_look_behind_menu(d);
    break;
  case REDIT_LOOK_UNDER_DESCRIPTION:
    redit_disp_look_under_menu(d);
    break;
  }
}
