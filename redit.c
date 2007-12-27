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
#include "descriptor.h"

/*------------------------------------------------------------------------*/

/*
 * External data structures.
 */
extern const char *room_bits[];
extern const char *sector_types[];
extern const char *exit_bits[];
extern Descriptor *descriptor_list;

/*------------------------------------------------------------------------*/


/**-----------------------------------------------------------------------*\
  Utils and exported functions.
\*------------------------------------------------------------------------*/

ACMD(do_oasis_redit)
{
  char *buf3;
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];
  int num = NOWHERE, save = 0;
  Descriptor *d;

  /* Parse any arguments. */
  buf3 = two_arguments(argument, buf1, buf2);

  if (!*buf1)
    num = GET_ROOM_VNUM(IN_ROOM(ch));
  else if (!isdigit(*buf1))
  {
    if (str_cmp("save", buf1) != 0)
    {
      ch->Send( "Yikes!  Stop that, someone will get hurt!\r\n");
      return;
    }

    save = TRUE;

    if (is_number(buf2))
      num = atoi(buf2);
    else if (GET_OLC_ZONE(ch) >= 0)
    {
      zone_rnum zlok;

      if ((zlok = real_zone(GET_OLC_ZONE(ch))) == NOWHERE)
        num = NOWHERE;
      else
        num = zone_table[zlok].Bot();
    }

    if (num == NOWHERE)
    {
      ch->Send( "Save which zone?\r\n");
      return;
    }
  }

  /*
   * If a numeric argument was given (like a room number), get it.
   */
  if (num == NOWHERE)
    num = atoi(buf1);

  /* Check to make sure the room isn't already being edited. */
  for (d = descriptor_list; d; d = d->next)
  {
    if (STATE(d) == CON_REDIT)
    {
      if (d->olc && OLC_NUM(d) == num)
      {
        ch->Send( "That room is currently being edited by %s.\r\n",
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
  OLC_ZNUM(d) = save ? real_zone(num) : real_zone_by_thing(num);
  if (OLC_ZNUM(d) == NOWHERE)
  {
    ch->Send( "Sorry, there is no zone for that number!\r\n");
    free(d->olc);
    d->olc = NULL;
    return;
  }

  /* Make sure the builder is allowed to modify this zone. */
  if (!can_edit_zone(ch, OLC_ZNUM(d)))
  {
    ch->Send( "You do not have permission to edit this zone.\r\n");
    new_mudlog(BRF, LVL_IMPL, TRUE, "OLC: %s tried to edit zone %d allowed zone %d",
               GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));

    free(d->olc);
    d->olc = NULL;
    return;
  }

  if (save)
  {
    ch->Send( "Saving all rooms in zone %d.\r\n", zone_table[OLC_ZNUM(d)].number);
    new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(ch)), TRUE, "OLC: %s saves room info for zone %d.", GET_NAME(ch), zone_table[OLC_ZNUM(d)].number);

    /* Save the rooms. */
    save_rooms(OLC_ZNUM(d));

    /* Free the olc data from the descriptor. */
    free(d->olc);
    d->olc = NULL;
    return;
  }

  OLC_NUM(d) = num;

  if (world_vnum[num])
    redit_setup_existing(d, num);
  else
    redit_setup_new(d);

  STATE(d) = CON_REDIT;
  act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);

  new_mudlog(BRF, LVL_IMMORT, TRUE, "OLC: %s starts editing zone %d allowed zone %d",
             GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));
}

void redit_setup_new(Descriptor *d)
{
  OLC_ROOM(d) = new Room();

  OLC_ROOM(d)->name = strdup("An unfinished room");
  OLC_ROOM(d)->SetDescription("You are in an unfinished room.\r\n");
  OLC_ROOM(d)->smell = strdup("You smell nothing interesting.\r\n");
  OLC_ROOM(d)->listen = strdup("You hear nothing interesting.\r\n");
  OLC_ROOM(d)->number = NOWHERE;
  OLC_ROOM(d)->mine.num = -1;
  OLC_ROOM(d)->mine.dif = -1;
  OLC_ROOM(d)->mine.tool = 0;
  OLC_ITEM_TYPE(d) = WLD_TRIGGER;
  OLC_ROOM(d)->proto_script = NULL;
  OLC_SCRIPT(d) = NULL;
  redit_disp_menu(d);
  OLC_VAL(d) = 0;
}

/*------------------------------------------------------------------------*/

void redit_setup_existing(Descriptor *d, room_vnum v_num)
{
  /*
   * Build a copy of the room for editing.
   */
  OLC_ROOM(d) = new Room();

  *OLC_ROOM(d) = *world_vnum[v_num];
  OLC_ROOM(d)->SetDesc(-1);
  OLC_ROOM(d)->copy_room_strings(world_vnum[v_num]);
  /*
   * Attach copy of room to player's descriptor.
   */
  OLC_VAL(d) = 0;
  OLC_ITEM_TYPE(d) = WLD_TRIGGER;
  OLC_ROOM(d)->script = NULL;
  OLC_ROOM(d)->contents = NULL;
  OLC_ROOM(d)->people = NULL;

  dg_olc_script_copy(d);
  redit_disp_menu(d);
}

/*------------------------------------------------------------------------*/

void redit_save_internally(Descriptor *d)
{
  int j,  new_room = FALSE;
  room_rnum room_num;
  Descriptor *dsc;

  if (OLC_ROOM(d)->number == NOWHERE)
  {
    new_room = TRUE;
    OLC_ROOM(d)->number = OLC_NUM(d);
    world_vnum[OLC_ROOM(d)->number] = new Room();
  }
  /* FIXME: Why is this not set elsewhere? */
  OLC_ROOM(d)->zone = OLC_ZNUM(d);

  if ((room_num = add_room(OLC_ROOM(d))) == NULL)
  {
    d->Output( "Something went wrong...\r\n");
    log("SYSERR: redit_save_internally: Something failed! (%d)", room_num->number);
    return;
  }
  /* Update triggers */
  /* Free old proto list */
  if (GET_ROOM_VNUM(room_num) != NOWHERE && world_vnum[GET_ROOM_VNUM(room_num)] &&
  world_vnum[GET_ROOM_VNUM(room_num)]->proto_script != room_num->proto_script)
  if (room_num->proto_script && room_num->proto_script != OLC_SCRIPT(d))
  free_proto_script(room_num, WLD_TRIGGER);
  room_num->proto_script = OLC_SCRIPT(d);
  assign_triggers(room_num, WLD_TRIGGER);
  OLC_SCRIPT(d) = NULL;
  /* end trigger update */

  REMOVE_BIT_AR(ROOM_FLAGS(room_num), ROOM_BFS_MARK);
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


/**************************************************************************
 Menu functions 
 **************************************************************************/
void redit_disp_mine_menu(Descriptor *d)
{
  d->Output(
                  "%s1%s) Mine Number     : %s%d\r\n"
                  "%s2%s) Mine Level      : %s%d\r\n"
                  "%s3%s) Mine Tool Needed: %s%s%s\r\n",
                  grn, nrm, yel, OLC_ROOM(d)->mine.num,
                  grn, nrm, yel, OLC_ROOM(d)->mine.dif,
                  grn, nrm, yel, OLC_ROOM(d)->mine.tool == TOOL_SHOVEL ? "Shovel" : "Pickaxe",
                  nrm
                 );
  d->Output( "(Setting Mine Number as -1 will remove room from a mine) \r\n");
  d->Output( "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_MINE_MENU;
}
/*
 * For extra descriptions.
 */
void redit_disp_extradesc_menu(Descriptor *d)
{
  struct extra_descr_data *extra_desc = OLC_DESC(d);

  clear_screen(d);
  d->Output(
                  "%s1%s) Keyword: %s%s\r\n"
                  "%s2%s) Description:\r\n%s%s\r\n"
                  "%s3%s) Goto next description: ",

                  grn, nrm, yel, extra_desc->keyword ? extra_desc->keyword : "<NONE>",
                  grn, nrm, yel, extra_desc->description ? extra_desc->description : "<NONE>",
                  grn, nrm
                 );

  d->Output( !extra_desc->next ? "<NOT SET>\r\n" : "Set.\r\n");
  d->Output( "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_EXTRADESC_MENU;
}
void redit_disp_look_above_menu(Descriptor *d)
{
  struct extra_descr_data *extra_desc = OLC_LADESC(d);

  clear_screen(d);
  d->Output(
                  "%s1%s) Keyword: %s%s\r\n"
                  "%s2%s) Description:\r\n%s%s\r\n"
                  "%s3%s) Goto next look above description: ",

                  grn, nrm, yel, extra_desc->keyword ? extra_desc->keyword : "<NONE>",
                  grn, nrm, yel, extra_desc->description ? extra_desc->description : "<NONE>",
                  grn, nrm
                 );

  d->Output( !extra_desc->next ? "<NOT SET>\r\n" : "Set.\r\n");
  d->Output( "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_LOOK_ABOVE_MENU;
}
void redit_disp_look_under_menu(Descriptor *d)
{
  struct extra_descr_data *extra_desc = OLC_LUDESC(d);

  clear_screen(d);
  d->Output(
                  "%s1%s) Keyword: %s%s\r\n"
                  "%s2%s) Description:\r\n%s%s\r\n"
                  "%s3%s) Goto next look under description: ",

                  grn, nrm, yel, extra_desc->keyword ? extra_desc->keyword : "<NONE>",
                  grn, nrm, yel, extra_desc->description ? extra_desc->description : "<NONE>",
                  grn, nrm
                 );

  d->Output( !extra_desc->next ? "<NOT SET>\r\n" : "Set.\r\n");
  d->Output( "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_LOOK_UNDER_MENU;
}
void redit_disp_look_behind_menu(Descriptor *d)
{
  struct extra_descr_data *extra_desc = OLC_LBDESC(d);

  clear_screen(d);
  d->Output(
                  "%s1%s) Keyword: %s%s\r\n"
                  "%s2%s) Description:\r\n%s%s\r\n"
                  "%s3%s) Goto next look behind description: ",

                  grn, nrm, yel, extra_desc->keyword ? extra_desc->keyword : "<NONE>",
                  grn, nrm, yel, extra_desc->description ? extra_desc->description : "<NONE>",
                  grn, nrm
                 );

  d->Output( !extra_desc->next ? "<NOT SET>\r\n" : "Set.\r\n");
  d->Output( "Enter choice (0 to quit) : ");
  OLC_MODE(d) = REDIT_LOOK_BEHIND_MENU;
}
/*
 * For exits.
 */
void redit_disp_exit_menu(Descriptor *d)
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

  get_char_colours(d->character);
  clear_screen(d);
  d->Output(
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
void redit_disp_exit_flag_menu(Descriptor *d)
{
  get_char_colours(d->character);
  d->Output( "%s0%s) No door\r\n"
                  "%s1%s) Closeable door\r\n"
                  "%s2%s) Pickproof\r\n"
                  "Enter choice : ", grn, nrm, grn, nrm, grn, nrm);
}

/*
 * For room flags.
 */
void redit_disp_flag_menu(Descriptor *d)
{
  char bits[MAX_STRING_LENGTH];
  int counter, columns = 0;

  get_char_colours(d->character);
  clear_screen(d);
  for (counter = 0; counter < NUM_ROOM_FLAGS; counter++)
  {
    d->Output( "%s%2d%s) %-20.20s %s", grn, counter + 1, nrm,
                    room_bits[counter], !(++columns % 2) ? "\r\n" : "");
  }

  sprintbitarray(OLC_ROOM(d)->room_flags, room_bits, RF_ARRAY_MAX, bits, sizeof(bits));
  d->Output( "\r\nRoom flags: %s%s%s\r\n"
                  "Enter room flags, 0 to quit : ", cyn, bits, nrm);
  OLC_MODE(d) = REDIT_FLAGS;
}

/*
 * For sector type.
 */
void redit_disp_sector_menu(Descriptor *d)
{
  int counter, columns = 0;

  clear_screen(d);
  for (counter = 0; counter < NUM_ROOM_SECTORS; counter++)
  {
    d->Output( "%s%2d%s) %-20.20s %s", grn, counter, nrm,
                    sector_types[counter], !(++columns % 2) ? "\r\n" : "");
  }
  d->Output( "\r\nEnter sector type : ");
  OLC_MODE(d) = REDIT_SECTOR;
}

/*
 * The main menu.
 */
void redit_disp_menu(Descriptor *d)
{
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];
  Room *room;

  get_char_colours(d->character);
  clear_screen(d);
  room = OLC_ROOM(d);
  sprintbitarray(room->room_flags, room_bits, RF_ARRAY_MAX, buf1, sizeof(buf1));
  sprinttype(room->sector_type, sector_types, buf2, sizeof(buf2));
  d->Output(
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
//                "%sD%s) Delete Room\r\n"
                  "%sQ%s) Quit\r\n"
                  "Enter choice : ",

                  cyn, OLC_NUM(d), nrm,
                  cyn, zone_table[OLC_ZNUM(d)].number, nrm,
                  grn, nrm, yel, room->name,
		  grn, nrm, yel, room->GetDescription(),
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
//                  grn, nrm,/*delete*/
                  grn, nrm /*quit*/


                 );

  OLC_MODE(d) = REDIT_MAIN_MENU;
}

/**************************************************************************
  The main loop
 **************************************************************************/

void redit_parse(Descriptor *d, char *arg)
{
  int num;
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
        d->Output( "Room saved to disk.\r\n");
      }
      else
        d->Output( "Room saved to memory.\r\n");
      /*
       * Do NOT free strings! Just the room structure. 
       */
      /* okay free strings now - mord */
      OLC_ROOM(d)->free_room_strings();
      cleanup_olc(d, CLEANUP_STRUCTS);
      break;
    case 'n':
    case 'N':
      /*
       * Free everything up, including strings, etc.
       */
	    OLC_ROOM(d)->free_room_strings();
      cleanup_olc(d, CLEANUP_ALL);
      break;
    default:
      d->Output( "Invalid choice!\r\nDo you wish to save this room ? : ");
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
        d->Output( "Do you wish to save this room ? : ");
        OLC_MODE(d) = REDIT_CONFIRM_SAVESTRING;
      }
      else
        cleanup_olc(d, CLEANUP_ALL);
      return;
    case '1':
      d->Output( "Enter room name:-\r\n] ");
      OLC_MODE(d) = REDIT_NAME;
      break;
    case '2':
      OLC_MODE(d) = REDIT_DESC;
      clear_screen(d);
      send_editor_help(d);
      d->Output( "Enter room description:\r\n\r\n");

      if (OLC_ROOM(d)->HasDesc())
      {
	      d->Output( "%s", OLC_ROOM(d)->GetDescription());
	      oldtext = strdup(OLC_ROOM(d)->GetDescription());
      }
      if (OLC_ROOM(d)->t_description)
	      free(OLC_ROOM(d)->t_description);
      OLC_ROOM(d)->t_description = strdup(OLC_ROOM(d)->GetDescription());
      string_write(d, &OLC_ROOM(d)->t_description, MAX_ROOM_DESC, 0, oldtext);
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
      d->Output( "Enter Smell description:\r\n\r\n");

      if (OLC_ROOM(d)->smell)
      {
        d->Output( "%s", OLC_ROOM(d)->smell);
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
      d->Output( "Enter Listen description:\r\n\r\n");

      if (OLC_ROOM(d)->listen)
      {
        d->Output( "%s", OLC_ROOM(d)->listen);
        oldtext = strdup(OLC_ROOM(d)->listen);
      }
      string_write(d, &OLC_ROOM(d)->listen, MAX_ROOM_DESC, 0, oldtext);
      OLC_VAL(d) = 1;
      break;
    case 'd':
    case 'D':
      /* Delete the room, prompt first. */
//      d->Output( "Are you sure you want to delete this room? ");
//      OLC_MODE(d) = REDIT_DELETE;
      d->Output( "Room deleting is disabled!\r\n");
      redit_disp_menu(d);
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
      d->Output( "Invalid choice!");
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
      d->Output( "Enter Mine Number : ");
      return;
    case '2':
      OLC_MODE(d) = REDIT_MINE_DIFFICULTY;
      d->Output( "Enter Mine Difficulty :");
      return;
    case '3':
      OLC_MODE(d) = REDIT_MINE_TOOL;
      d->Output( "0) Shovel\r\n1) Pickaxe\r\nEnter a number:");
      return;
    }
    break;
  case REDIT_MINE_NUMBER:
    num = atoi(arg);
    if (num < -1)
    {
      d->Output( "You can't choose a number below -1\r\n");
      return;
    }
    else if (num > 100)
    {
      d->Output( "You can't choose a number above 100\r\n");
      return;
    }
    OLC_ROOM(d)->mine.num = num;
    break;
  case REDIT_MINE_DIFFICULTY:
    num = atoi(arg);
    if (num < 0)
    {
      d->Output( "You can't choose a number below 0\r\n");
      return;
    }
    else if (num > 5)
    {
      d->Output( "You can't choose a number above 5\r\n");
      return;
    }
    OLC_ROOM(d)->mine.dif = num;
    break;
  case REDIT_MINE_TOOL:
    num = atoi(arg);
    if (num < 0)
    {
      d->Output( "You can't choose a number below 0\r\n");
      return;
    }
    else if (num > 1)
    {
      d->Output( "You can't choose a number above 1\r\n");
      return;
    }
    OLC_ROOM(d)->mine.tool = num;
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
    d->Output( "Oops, in REDIT_DESC.\r\n");
    break;

  case REDIT_SMELL:
    /*
     * We will NEVER get here, we hope.
     */
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: Reached SMELL_DESC case in parse_redit().");
    d->Output( "Oops, in SMELL_DESC.\r\n");
    break;

  case REDIT_LISTEN:
    /*
     * We will NEVER get here, we hope.
     */
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: Reached LISTEN_DESC case in parse_redit().");
    d->Output( "Oops, in REDIT_DESC.\r\n");
    break;
  case REDIT_FLAGS:
    num = atoi(arg);
    if (num < 0 || num > NUM_ROOM_FLAGS)
    {
      d->Output( "That is not a valid choice!\r\n");
      redit_disp_flag_menu(d);
    }
    else if (num == 0)
      break;
    else
    {
      /*
       * Toggle the bit.
       */
      TOGGLE_BIT_AR(OLC_ROOM(d)->room_flags, (num - 1));
      redit_disp_flag_menu(d);
    }
    return;

  case REDIT_SECTOR:
    num = atoi(arg);
    if (num < 0 || num >= NUM_ROOM_SECTORS)
    {
      d->Output( "Invalid choice!");
      redit_disp_sector_menu(d);
      return;
    }
    OLC_ROOM(d)->sector_type = num;
    break;

  case REDIT_EXIT_MENU:
    switch (*arg)
    {
    case '0':
      break;
    case '1':
      OLC_MODE(d) = REDIT_EXIT_NUMBER;
      d->Output( "Exit to room number : ");
      return;
    case '2':
      OLC_MODE(d) = REDIT_EXIT_DESCRIPTION;
      send_editor_help(d);
      d->Output( "Enter exit description:\r\n\r\n");
      if (OLC_EXIT(d)->general_description)
      {
        d->Output( "%s", OLC_EXIT(d)->general_description);
        oldtext = strdup(OLC_EXIT(d)->general_description);
      }
      string_write(d, &OLC_EXIT(d)->general_description, MAX_EXIT_DESC, 0, oldtext);
      return;
    case '3':
      OLC_MODE(d) = REDIT_EXIT_KEYWORD;
      d->Output( "Enter keywords : ");
      return;
    case '4':
      OLC_MODE(d) = REDIT_EXIT_KEY;
      d->Output( "Enter key number : ");
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
      d->Output( "Try again : ");
      return;
    }
    break;

  case REDIT_EXIT_NUMBER:
    {
      room_rnum rm = NULL;
      if ((num = atoi(arg)) != -1)
        if ((rm = real_room(num)) == NULL)
        {
          d->Output( "That room does not exist, try again : ");
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
    d->Output( "Oops, in REDIT_EXIT_DESCRIPTION.\r\n");
    break;

  case REDIT_EXIT_KEYWORD:
    if (OLC_EXIT(d)->keyword)
      free(OLC_EXIT(d)->keyword);
    OLC_EXIT(d)->keyword = str_udup(arg);
    redit_disp_exit_menu(d);
    return;

  case REDIT_EXIT_KEY:
    num = atoi(arg);
    if (num < 0)
      OLC_EXIT(d)->key = NOTHING;
    else
      OLC_EXIT(d)->key = num;
    redit_disp_exit_menu(d);
    return;

  case REDIT_EXIT_DOORFLAGS:
    num = atoi(arg);
    if (num < 0 || num > 2)
    {
      d->Output( "That's not a valid choice!\r\n");
      redit_disp_exit_flag_menu(d);
    }
    else
    {
      /*
       * Doors are a bit idiotic, don't you think? :) -- I agree. -gg
       */
      OLC_EXIT(d)->exit_info = (num == 0 ? 0 :
                                (num == 1 ? EX_ISDOOR :
                                 (num == 2 ? EX_ISDOOR | EX_PICKPROOF : 0)));
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
    switch ((num = atoi(arg)))
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
      d->Output( "Enter keywords, separated by spaces : ");
      return;
    case 2:
      OLC_MODE(d) = REDIT_EXTRADESC_DESCRIPTION;
      send_editor_help(d);
      d->Output( "Enter extra description:\r\n\r\n");
      if (OLC_DESC(d)->description)
      {
        d->Output( "%s", OLC_DESC(d)->description);
        oldtext = strdup(OLC_DESC(d)->description);
      }
      string_write(d, &OLC_DESC(d)->description, MAX_MESSAGE_LENGTH, 0, oldtext);
      return;
    case 3:
      if (OLC_DESC(d)->keyword == NULL || OLC_DESC(d)->description == NULL)
      {
        d->Output( "You can't edit the next extra description without completing this one.\r\n");
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
    switch ((num = atoi(arg)))
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
      d->Output( "Enter keywords, separated by spaces : ");
      return;
    case 2:
      OLC_MODE(d) = REDIT_LOOK_UNDER_DESCRIPTION;
      send_editor_help(d);
      d->Output( "Enter look under description:\r\n\r\n");
      if (OLC_LUDESC(d)->description)
      {
        d->Output( "%s", OLC_LUDESC(d)->description);
        oldtext = strdup(OLC_LUDESC(d)->description);
      }
      string_write(d, &OLC_LUDESC(d)->description, MAX_MESSAGE_LENGTH, 0, oldtext);
      return;
    case 3:
      if (OLC_LUDESC(d)->keyword == NULL || OLC_LUDESC(d)->description == NULL)
      {
        d->Output( "You can't edit the next under description without completing this one.\r\n");
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
    switch ((num = atoi(arg)))
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
      d->Output( "Enter keywords, separated by spaces : ");
      return;
    case 2:
      OLC_MODE(d) = REDIT_LOOK_BEHIND_DESCRIPTION;
      send_editor_help(d);
      d->Output( "Enter look behind description:\r\n\r\n");
      if (OLC_LBDESC(d)->description)
      {
        d->Output( "%s", OLC_LBDESC(d)->description);
        oldtext = strdup(OLC_LBDESC(d)->description);
      }
      string_write(d, &OLC_LBDESC(d)->description, MAX_MESSAGE_LENGTH, 0, oldtext);
      return;
    case 3:
      if (OLC_LBDESC(d)->keyword == NULL || OLC_LBDESC(d)->description == NULL)
      {
        d->Output( "You can't edit the next below description without completing this one.\r\n");
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
    switch ((num = atoi(arg)))
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
      d->Output( "Enter keywords, separated by spaces : ");
      return;
    case 2:
      OLC_MODE(d) = REDIT_LOOK_ABOVE_DESCRIPTION;
      send_editor_help(d);
      d->Output( "Enter look above description:\r\n\r\n");
      if (OLC_LADESC(d)->description)
      {
        d->Output( "%s", OLC_LADESC(d)->description);
        oldtext = strdup(OLC_LADESC(d)->description);
      }
      string_write(d, &OLC_LADESC(d)->description, MAX_MESSAGE_LENGTH, 0, oldtext);
      return;
    case 3:
      if (OLC_LADESC(d)->keyword == NULL || OLC_LADESC(d)->description == NULL)
      {
        d->Output( "You can't edit the next above description without completing this one.\r\n");
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
    d->Output("Room deleting is disabled!\r\n");
    return;
    if (*arg == 'y' || *arg == 'Y')
    {
      if (delete_room(OLC_ROOM(d)))
        d->Output( "Room deleted.\r\nPlease confirm delete by saving zone\r\n");
      else
        d->Output( "Couldn't delete the room!.\r\n");

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
      d->Output( "Please answer 'Y' or 'N': ");

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

void redit_string_cleanup(Descriptor *d, int terminator)
{
  switch (OLC_MODE(d))
  {
  case REDIT_SMELL:
  case REDIT_LISTEN:
	  redit_disp_menu(d);
	  break;
  case REDIT_DESC:
	  if (STRINGADD_SAVE == terminator) 
		  OLC_ROOM(d)->AssignTempDesc();		  
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
