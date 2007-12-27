/************************************************************************
 * OasisOLC - Zones / zedit.c					v2.0	*
 * Copyright 1996 Harvey Gilpin						*
 * Copyright 1997-2001 George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "comm.h"
#include "interpreter.h"
#include "utils.h"
#include "db.h"
#include "constants.h"
#include "genolc.h"
#include "genzon.h"
#include "oasis.h"
#include "dg_scripts.h"
#include "descriptor.h"

/*-------------------------------------------------------------------*/
extern struct index_data **trig_index;
extern const char *zone_bits[];
/*-------------------------------------------------------------------*/

/*
 * Nasty internal macros to clean up the code.
 */
#define MYCMD		(OLC_ZONE(d)->cmd[subcmd])
#define OLC_CMD(d)	(OLC_ZONE(d)->cmd[OLC_VAL(d)])


/* Prototypes. */
int start_change_command(Descriptor *d, int pos);
void zedit_disp_flag_menu(Descriptor *d);

const char *door_stat[] = {
    "open",
    "closed",
    "closed,locked",
    "closed,hidden",
    "closed,hidden,locked"
};

/*-------------------------------------------------------------------*/

ACMD(do_oasis_zedit)
{
  int number = NOWHERE, save = 0;
  room_rnum real_num;
  Descriptor *d;
  char *buf3;
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];
  
  /****************************************************************************/
  /** Parse any arguments.                                                   **/
  /****************************************************************************/
  buf3 = two_arguments(argument, buf1, buf2);
  
  /****************************************************************************/
  /** If no argument was given, use the zone the builder is standing in.     **/
  /****************************************************************************/
  if (!*buf1)
    number = GET_ROOM_VNUM(IN_ROOM(ch));
  else if (!isdigit(*buf1)) {
    if (str_cmp("save", buf1) == 0) {
      save = TRUE;
      
      if (is_number(buf2))
        number = atoi(buf2);
      else if (GET_OLC_ZONE(ch) > 0) {
        zone_rnum zlok;
        
        if ((zlok = real_zone(GET_OLC_ZONE(ch))) == NOWHERE)
          number = NOWHERE;
        else
          number = zone_table[zlok].Bot();
      }
      
      if (number == NOWHERE) {
        ch->Send( "Save which zone?\r\n");
        return;
      }
    } else if (GET_LEVEL(ch) >= LVL_IMPL) {
      if (str_cmp("new", buf1) || !buf3 || !*buf3)
        ch->Send( "Format: zedit new <zone number> <bottom-room> <top-room>\r\n");
      else {
        char sbot[MAX_INPUT_LENGTH], stop[MAX_INPUT_LENGTH];
        room_vnum bottom, top;
        
        two_arguments(buf3, sbot, stop);
        
        number = atoi(buf2);
        if (number < 0)
          number = NOWHERE;
        bottom = atoi(sbot);
        top = atoi(stop);
        
        /**********************************************************************/
        /** Setup the new zone (displays the menu to the builder).           **/
        /**********************************************************************/
        zedit_new_zone(ch, number, bottom, top);
      }
      
      /************************************************************************/
      /** Done now, exit the function.                                       **/
      /************************************************************************/
      return;
      
    } else {
      ch->Send( "Yikes!  Stop that, someone will get hurt!\r\n");
      return;
    }
  }
  
  /****************************************************************************/
  /** If a numeric argumentwas given, retrieve it.                           **/
  /****************************************************************************/
  if (number == NOWHERE)
    number = atoi(buf1);
  
  /****************************************************************************/
  /** Check that nobody is currently editing this zone.                      **/
  /****************************************************************************/
  for (d = descriptor_list; d; d = d->next) {
    if (STATE(d) == CON_ZEDIT) {
      if (d->olc && OLC_NUM(d) == number) {
        ch->Send( "That zone is currently being edited by %s.\r\n",
          PERS(d->character, ch));
        return;
      }
    }
  }
  
  /****************************************************************************/
  /** Store the builder's descriptor in d.                                   **/
  /****************************************************************************/
  d = ch->desc;
  
  /****************************************************************************/
  /** Give the builder's descriptor an OLC structure.                        **/
  /****************************************************************************/
  if (d->olc) {
    new_mudlog(BRF, LVL_IMMORT, TRUE, "SYSERR: do_oasis_zedit: Player already "
      "had olc structure.");
    free(d->olc);
  }
  
  CREATE(d->olc, struct oasis_olc_data, 1);
  
  /****************************************************************************/
  /** Find the zone.                                                         **/
  /****************************************************************************/
  OLC_ZNUM(d) = save ? real_zone(number) : real_zone_by_thing(number);
  if (OLC_ZNUM(d) == NOWHERE) {
    ch->Send( "Sorry, there is no zone for that number!\r\n");
    
    /**************************************************************************/
    /** Free the descriptor's OLC structure.                                 **/
    /**************************************************************************/
    free(d->olc);
    d->olc = NULL;
    return;
  }
  
  /****************************************************************************/
  /** Everyone but IMPLs can only edit zones they have been assigned.        **/
  /****************************************************************************/
  if (!can_edit_zone(ch, OLC_ZNUM(d))) {
    ch->Send( "You do not have permission to edit this zone.\r\n");
    new_mudlog(BRF, LVL_IMPL, TRUE,
      "OLC: %s tried to edit zone %d allowed zone %d", GET_NAME(ch),
      zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));
    free(d->olc);
    d->olc = NULL;
    return;
  }
  
  /****************************************************************************/
  /** If we need to save, then save the zone.                                **/
  /****************************************************************************/
  if (save) {
    ch->Send( "Saving all zone information for zone %d.\r\n",
      zone_table[OLC_ZNUM(d)].number);
    new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(ch)), TRUE,
      "OLC: %s saves zone information for zone %d.", GET_NAME(ch),
      zone_table[OLC_ZNUM(d)].number);
    
    /**************************************************************************/
    /** Save the zone information to the zone file.                          **/
    /**************************************************************************/
    save_zone(OLC_ZNUM(d));
    
    /**************************************************************************/
    /** Free the descriptor's OLC structure.                                 **/
    /**************************************************************************/
    free(d->olc);
    d->olc = NULL;
    return;
  }
  
  OLC_NUM(d) = number;
  
  if ((real_num = real_room(number)) == NULL) {
    d->Output( "That room does not exist.\r\n");
    
    /**************************************************************************/
    /** Free the descriptor's OLC structure.                                 **/
    /**************************************************************************/
    free(d->olc);
    d->olc = NULL;
    return;
  }

  zedit_setup(d, real_num);
  STATE(d) = CON_ZEDIT;
  
  act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);
  
  new_mudlog(BRF, LVL_IMMORT, TRUE, "OLC: %s starts editing zone %d allowed zone %d",
    GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));
}

void zedit_setup(Descriptor *d, room_rnum room_num)
{
  Zone *zone;
  int subcmd = 0, count = 0;
  room_rnum cmd_room = NULL;

  /*
   * Allocate one scratch zone structure.  
   */
  zone = new Zone();

  /*
   * Copy all the zone header information over.
   */
  zone->name = strdup(zone_table[OLC_ZNUM(d)].name);
  if (zone_table[OLC_ZNUM(d)].builders)
    zone->builders = strdup(zone_table[OLC_ZNUM(d)].builders);
  zone->lifespan = zone_table[OLC_ZNUM(d)].lifespan;
  zone->bot = zone_table[OLC_ZNUM(d)].bot;
  zone->top = zone_table[OLC_ZNUM(d)].top;
  zone->reset_mode = zone_table[OLC_ZNUM(d)].reset_mode;  
  zone->zone_flags = zone_table[OLC_ZNUM(d)].zone_flags;
  zone->dimension  = zone_table[OLC_ZNUM(d)].dimension;
  /*
   * The remaining fields are used as a 'has been modified' flag  
   */
  zone->number = 0;	/* Header information has changed.	*/
  zone->age = 0;	/* The commands have changed.		*/

  /*
   * Start the reset command list with a terminator.
   */
  reset_com rc = reset_com();
  rc.command = 'S';
  zone->cmd.push_back(rc);

  /*
   * Add all entries in zone_table that relate to this room.
   */
  while (ZCMD(OLC_ZNUM(d), subcmd).command != 'S') {
    switch (ZCMD(OLC_ZNUM(d), subcmd).command) {
    case 'M':
    case 'O':
    case 'T':
    case 'V':
      cmd_room = world_vnum[ZCMD(OLC_ZNUM(d), subcmd).arg3];
      break;
    case 'D':
    case 'R':
      cmd_room = world_vnum[ZCMD(OLC_ZNUM(d), subcmd).arg1];
      break;
    default:
      break;
    }
    if (cmd_room == room_num) {
      add_cmd_to_list(zone->cmd, ZCMD(OLC_ZNUM(d), subcmd), count);
      count++;
    }
    subcmd++;
  }

  OLC_ZONE(d) = zone;
  /*
   * Display main menu.
   */
  zedit_disp_menu(d);
}

/*-------------------------------------------------------------------*/

/*
 * Create a new zone.
 */
void zedit_new_zone(Character *ch, zone_vnum vzone_num, room_vnum bottom, room_vnum top)
{
  int result;
  const char *error;
  Descriptor *dsc;

  if ((result = create_new_zone(vzone_num, bottom, top, &error)) == NOWHERE) {
    ch->Send("%s", error);
    return;
  }

  for (dsc = descriptor_list; dsc; dsc = dsc->next) {
    switch (STATE(dsc)) {
      case CON_REDIT:
        OLC_ROOM(dsc)->zone += (OLC_ZNUM(dsc) >= result);
        /* Fall through. */
      case CON_ZEDIT:
      case CON_MEDIT:
      case CON_SEDIT:
      case CON_OEDIT:
      case CON_TRIGEDIT:
        OLC_ZNUM(dsc) += (OLC_ZNUM(dsc) >= result);
        break;
      default:
        break;
    }
  }

  zedit_save_to_disk(result); /* save to disk .. */

  new_mudlog(BRF, MAX(LVL_BUILDER, GET_INVIS_LEV(ch)), TRUE,
    "OLC: %s creates new zone #%d", GET_NAME(ch), vzone_num);
  ch->Send( "Zone created successfully.\r\n");
}

/*-------------------------------------------------------------------*/

/*
 * Save all the information in the player's temporary buffer back into
 * the current zone table.
 */
void zedit_save_internally(Descriptor *d)
{
  int	mobloaded = FALSE,
	objloaded = FALSE,
	subcmd;
  room_rnum room_num = real_room(OLC_NUM(d));

  if (room_num == NULL) {
    log("SYSERR: zedit_save_internally: OLC_NUM(d) room %d not found.", OLC_NUM(d));
    return;
  }

  remove_room_zone_commands(OLC_ZNUM(d), room_num);

  /*
   * Now add all the entries in the players descriptor list  
   */
  for (subcmd = 0; MYCMD.command != 'S'; subcmd++) {
    /*
     * Since Circle does not keep track of what rooms the 'G', 'E', and
     * 'P' commands are exitted in, but OasisOLC groups zone commands
     * by rooms, this creates interesting problems when builders use these
     * commands without loading a mob or object first.  This fix prevents such
     * commands from being saved and 'wandering' through the zone command
     * list looking for mobs/objects to latch onto.
     * C.Raehl 4/27/99
     */
    switch (MYCMD.command) {
      /* Possible fail cases. */
      case 'G':
      case 'E':        
        if (mobloaded) {
	objloaded = TRUE;
          break;
	  }
        d->Output( "Equip/Give command not saved since no mob was loaded first.\r\n");
        continue;
      case 'P':
        if (objloaded)
          break;
        d->Output( "Put command not saved since another object was not loaded first.\r\n");
        continue;
      /* Pass cases. */
      case 'M':
        mobloaded = TRUE;
        break;
      case 'O':
        objloaded = TRUE;
        break;
      default:
        mobloaded = objloaded = FALSE;
        break;
    }
    add_cmd_to_list(zone_table[OLC_ZNUM(d)].cmd, MYCMD, subcmd);
  }

  /*
   * Finally, if zone headers have been changed, copy over  
   */
  if (OLC_ZONE(d)->number) {
    free(zone_table[OLC_ZNUM(d)].name);
    free(zone_table[OLC_ZNUM(d)].builders);
    
    zone_table[OLC_ZNUM(d)].name = strdup(OLC_ZONE(d)->name);
    zone_table[OLC_ZNUM(d)].builders = strdup(OLC_ZONE(d)->builders);
    zone_table[OLC_ZNUM(d)].bot = OLC_ZONE(d)->bot;
    zone_table[OLC_ZNUM(d)].top = OLC_ZONE(d)->top;
    zone_table[OLC_ZNUM(d)].reset_mode = OLC_ZONE(d)->reset_mode;
    zone_table[OLC_ZNUM(d)].lifespan = OLC_ZONE(d)->lifespan;
    zone_table[OLC_ZNUM(d)].zone_flags = OLC_ZONE(d)->zone_flags;
    zone_table[OLC_ZNUM(d)].dimension = OLC_ZONE(d)->dimension;
  }
  add_to_save_list(zone_table[OLC_ZNUM(d)].number, SL_ZON);
}

/*-------------------------------------------------------------------*/

void zedit_save_to_disk(int zone)
{
  save_zone(zone);
}

/*-------------------------------------------------------------------*/

/*
 * Error check user input and then setup change  
 */
int start_change_command(Descriptor *d, int pos)
{
  if (pos < 0 || pos >= OLC_ZONE(d)->cmd.size())
    return 0;

  /*
   * Ok, let's get editing.
   */
  OLC_VAL(d) = pos;
  return 1;
}

/**************************************************************************
 Menu functions 
 **************************************************************************/
void zedit_disp_flag_menu(Descriptor *d)
{
    int counter, columns = 0;
    char buf1[MAX_INPUT_LENGTH];

    get_char_colors(d->character);
    // clear_screen(d);
    for (counter = 0; counter < NUM_ZONE_FLAGS; counter++) {
	d->Output( "%s%2d%s) %-20.20s %s", grn, counter + 1, nrm,
		zone_bits[counter], !(++columns % 2) ? "\r\n" : "");
    }
    sprintbit(OLC_ZONE(d)->zone_flags, zone_bits, buf1, sizeof(buf1));
    d->Output( "\r\nZone flags: %s%s%s\r\n"
	    "Enter zone flags, 0 to quit : ", cyn, buf1, nrm);
    OLC_MODE(d) = ZEDIT_ZONE_FLAGS;
}
void zedit_disp_dimension_menu(Descriptor *d) {

       int counter, columns = 0;
    get_char_colors(d->character);
    for (counter = 0; counter <= D_PRE_HIST; counter++) {
	d->Output( "%s%2d%s) %-20.20s %s", grn, counter, nrm,
		dimension_types[counter], !(++columns % 2) ? "\r\n" : "");
    }
    d->Output( "Enter Dimension: ");
    OLC_MODE(d) = ZEDIT_DIMENSION;
  }

/*
 * the main menu 
 */
void zedit_disp_menu(Descriptor *d)
{
  int subcmd = 0,  counter = 0;
  char buf1[MAX_STRING_LENGTH];
  room_rnum room;

  get_char_colors(d->character);
  clear_screen(d);
  room = real_room(OLC_NUM(d));
  
sprintbit((long) OLC_ZONE(d)->zone_flags, zone_bits, buf1, sizeof(buf1));
  /*
   * Menu header  
   */
  d->Output(
	  "Room number: %s%d%s		Room zone: %s%d\r\n"
	  "%s1%s) Builders       : %s%s\r\n"
	  "%sZ%s) Zone name      : %s%s\r\n"
    "%sX%s) Dimension      : %s%s\r\n"
	  "%sL%s) Lifespan       : %s%d minutes\r\n"
	  "%sB%s) Bottom of zone : %s%d\r\n"
	  "%sT%s) Top of zone    : %s%d\r\n"
	  "%sR%s) Reset Mode     : %s%s%s\r\n"
	  "%sF%s) Zone Flags     : %s%s%s\r\n"
	  "[Command list]\r\n",

	  cyn, OLC_NUM(d), nrm,
	  cyn, zone_table[OLC_ZNUM(d)].number,
	  grn, nrm, yel, OLC_ZONE(d)->builders ? OLC_ZONE(d)->builders : "None.",
	  grn, nrm, yel, OLC_ZONE(d)->name ? OLC_ZONE(d)->name : "<NONE!>",
    grn, nrm, yel, dimension_types[OLC_ZONE(d)->dimension],
	  grn, nrm, yel, OLC_ZONE(d)->lifespan,
	  grn, nrm, yel, OLC_ZONE(d)->bot,
	  grn, nrm, yel, OLC_ZONE(d)->top,
	  grn, nrm,
          yel,
          OLC_ZONE(d)->reset_mode ? ((OLC_ZONE(d)->reset_mode == 1) ? "Reset when no players are in zone." : "Normal reset.") : "Never reset",
          nrm,
	   grn, nrm, yel, buf1,
	   nrm
	  );

  /*
   * Print the commands for this room into display buffer.
   */
  while (MYCMD.command != 'S') {
    /*
     * Translate what the command means.
     */
    d->Output( "%s%d - %s", nrm, counter++, yel);
    switch (MYCMD.command) {
    case 'M':
      d->Output( "%sLoad %s [%s%d%s], Max : %d",
              MYCMD.if_flag ? " then " : "",
              mob_proto[MYCMD.arg1]->player.short_descr, cyn,
              mob_index[MYCMD.arg1].vnum, yel, MYCMD.arg2
              );
      break;
    case 'G':
      d->Output( "%sGive it %s [%s%d%s], Max : %d",
	      MYCMD.if_flag ? " then " : "",
	      obj_proto[MYCMD.arg1].short_description,
	      cyn, obj_index[MYCMD.arg1].vnum, yel,
	      MYCMD.arg2
	      );
      break;
    case 'O':
      d->Output( "%sLoad %s [%s%d%s], Max : %d",
	      MYCMD.if_flag ? " then " : "",
	      obj_proto[MYCMD.arg1].short_description,
	      cyn, obj_index[MYCMD.arg1].vnum, yel,
	      MYCMD.arg2
	      );
      break;
    case 'E':
      d->Output( "%sEquip with %s [%s%d%s], %s, Max : %d",
	      MYCMD.if_flag ? " then " : "",
	      obj_proto[MYCMD.arg1].short_description,
	      cyn, obj_index[MYCMD.arg1].vnum, yel,
	      equipment_types[MYCMD.arg3],
	      MYCMD.arg2
	      );
      break;
    case 'P':
      d->Output( "%sPut %s [%s%d%s] in %s [%s%d%s], Max : %d",
	      MYCMD.if_flag ? " then " : "",
	      obj_proto[MYCMD.arg1].short_description,
	      cyn, obj_index[MYCMD.arg1].vnum, yel,
	      obj_proto[MYCMD.arg3].short_description,
	      cyn, obj_index[MYCMD.arg3].vnum, yel,
	      MYCMD.arg2
	      );
      break;
    case 'R':
      d->Output( "%sRemove %s [%s%d%s] from room.",
	      MYCMD.if_flag ? " then " : "",
	      obj_proto[MYCMD.arg2].short_description,
	      cyn, obj_index[MYCMD.arg2].vnum, yel
	      );
      break;
    case 'D':
      d->Output( "%sSet door %s as %s.",
	      MYCMD.if_flag ? " then " : "",
	      dirs[MYCMD.arg2],
	      //MYCMD.arg3 ? ((MYCMD.arg3 == 1) ? "closed" : "locked") : "open"
	      door_stat[MYCMD.arg3]
	      );
      break;
    case 'T':
      d->Output( "%sAttach trigger %s%s%s [%s%d%s] to %s",
        MYCMD.if_flag ? " then " : "",
        cyn, trig_index[MYCMD.arg2]->proto->name, yel, 
        cyn, trig_index[MYCMD.arg2]->vnum, yel,
        ((MYCMD.arg1 == MOB_TRIGGER) ? "mobile" :
          ((MYCMD.arg1 == OBJ_TRIGGER) ? "object" :
            ((MYCMD.arg1 == WLD_TRIGGER)? "room" : "????"))));
      break;
    case 'V':
      d->Output( "%sAssign global %s:%d to %s = %s",
        MYCMD.if_flag ? " then " : "",
        MYCMD.sarg1, MYCMD.arg2,
        ((MYCMD.arg1 == MOB_TRIGGER) ? "mobile" :
          ((MYCMD.arg1 == OBJ_TRIGGER) ? "object" :
            ((MYCMD.arg1 == WLD_TRIGGER)? "room" : "????"))),
        MYCMD.sarg2);
      break;
 case 'B':
	    d->Output( "%sBury %s [%s%d%s],  Max : %d",
		    MYCMD.if_flag ? " then " : "",
		    obj_proto[MYCMD.arg1].short_description,
		    cyn, obj_index[MYCMD.arg1].vnum, yel, MYCMD.arg2);
	    break;
    default:
      d->Output( "<Unknown Command>");
      break;
    }
    d->Output( "\r\n");
    subcmd++;
  }
  /*
   * Finish off menu  
   */
   d->Output(
	  "%s%d - <END OF LIST>\r\n"
	  "%sN%s) Insert new command.\r\n"
	  "%sE%s) Edit a command.\r\n"
	  "%sD%s) Delete a command.\r\n"
	  "%sQ%s) Quit\r\nEnter your choice : ",
	  nrm, counter, grn, nrm, grn, nrm, grn, nrm, grn, nrm
	  );

  OLC_MODE(d) = ZEDIT_MAIN_MENU;
}

/*-------------------------------------------------------------------*/

/*
 * Print the command type menu and setup response catch. 
 */
void zedit_disp_comtype(Descriptor *d)
{
  get_char_colors(d->character);
  clear_screen(d);
  d->Output(
	"\r\n"
	"%sM%s) Load Mobile to room             %sO%s) Load Object to room\r\n"
	"%sE%s) Equip mobile with object        %sG%s) Give an object to a mobile\r\n"
	"%sP%s) Put object in another object    %sD%s) Open/Close/Lock a Door\r\n"
	"%sR%s) Remove an object from the room\r\n"
	"%sT%s) Assign a trigger                %sV%s) Set a global variable\r\n"
	"\r\n"
	"What sort of command will this be? : ",
	grn, nrm, grn, nrm, grn, nrm, grn, nrm, grn, nrm,
	grn, nrm, grn, nrm, grn, nrm, grn, nrm
	);
  OLC_MODE(d) = ZEDIT_COMMAND_TYPE;
}

/*-------------------------------------------------------------------*/

/*
 * Print the appropriate message for the command type for arg1 and set
 * up the input catch clause  
 */
void zedit_disp_arg1(Descriptor *d)
{
  d->Output( "\r\n");

  switch (OLC_CMD(d).command) {
  case 'M':
    d->Output( "Input mob's vnum : ");
    OLC_MODE(d) = ZEDIT_ARG1;
    break;
  case 'O':
  case 'E':
  case 'P':
  case 'G':
    d->Output( "Input object vnum : ");
    OLC_MODE(d) = ZEDIT_ARG1;
    break;
  case 'D':
  case 'R':
    /*
     * Arg1 for these is the room number, skip to arg2  
     */
    OLC_CMD(d).arg1 = world_vnum[OLC_NUM(d)]->number;
    zedit_disp_arg2(d);
    break;
  case 'T':
  case 'V':
    d->Output( "Input trigger type (0:mob, 1:obj, 2:room) :");
    OLC_MODE(d) = ZEDIT_ARG1;
    break;
  default:
    /*
     * We should never get here.
     */
    cleanup_olc(d, CLEANUP_ALL);
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: zedit_disp_arg1(): Help!");
    d->Output( "Oops...\r\n");
    return;
  }
}

/*-------------------------------------------------------------------*/

/*
 * Print the appropriate message for the command type for arg2 and set
 * up the input catch clause.
 */
void zedit_disp_arg2(Descriptor *d)
{
  int i;

  d->Output( "\r\n");

  switch (OLC_CMD(d).command) {
  case 'M':
  case 'O':
  case 'E':
  case 'P':
  case 'G':
    d->Output( "Input the maximum number that can exist on the mud : ");
    break;
  case 'D':
    for (i = 0; *dirs[i] != '\n'; i++) {
      d->Output( "%d) Exit %s.\r\n", i, dirs[i]);
    }
    d->Output( "Enter exit number for door : ");
    break;
  case 'R':
    d->Output( "Input object's vnum : ");
    break;
  case 'T':
    d->Output( "Enter the trigger VNum : ");
    break;
  case 'V':
    d->Output( "Global's context (0 for none) : ");
    break;
  default:
    /*
     * We should never get here, but just in case...
     */
    cleanup_olc(d, CLEANUP_ALL);
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: zedit_disp_arg2(): Help!");
    d->Output( "Oops...\r\n");
    return;
  }
  OLC_MODE(d) = ZEDIT_ARG2;
}

/*-------------------------------------------------------------------*/

/*
 * Print the appropriate message for the command type for arg3 and set
 * up the input catch clause.
 */
void zedit_disp_arg3(Descriptor *d)
{
  int i = 0;

  d->Output( "\r\n");

  switch (OLC_CMD(d).command) {
  case 'E':
    while (*equipment_types[i] != '\n') {
      d->Output( "%2d) %26.26s %2d) %26.26s\r\n", i,
	   equipment_types[i], i + 1, (*equipment_types[i + 1] != '\n') ?
	      equipment_types[i + 1] : "");
      if (*equipment_types[i + 1] != '\n')
	i += 2;
      else
	break;
    }
    d->Output( "Location to equip : ");
    break;
  case 'P':
    d->Output( "Virtual number of the container : ");
    break;
  case 'D':
	d->Output(
	             "0)  Door open\r\n"
		     "1)  Door closed\r\n"
		     "2)  Door locked\r\n"
		     "3)  Door closed, hidden\r\n"
		     "4)  Door closed, hidden, locked\r\n"
		     "Enter state of the door : ");
	break;
  case 'V':
  case 'T':
  case 'M':
  case 'O':
  case 'R':
  case 'G':
  case 'B':
  default:
    /*
     * We should never get here, just in case.
     */
    cleanup_olc(d, CLEANUP_ALL);
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: zedit_disp_arg3(): Help!");
    d->Output( "Oops...\r\n");
    return;
  }
  OLC_MODE(d) = ZEDIT_ARG3;
}

/**************************************************************************
  The GARGANTAUN event handler
 **************************************************************************/

void zedit_parse(Descriptor *d, char *arg)
{
  int pos, i = 0;
  int num = 0;

  switch (OLC_MODE(d)) {
/*-------------------------------------------------------------------*/
  case ZEDIT_CONFIRM_SAVESTRING:
    switch (*arg) {
    case 'y':
    case 'Y':
      /*
       * Save the zone in memory, hiding invisible people.
       */
      zedit_save_internally(d);
      if (CONFIG_OLC_SAVE) {
	d->Output( "Saving zone info to disk.\r\n");
	zedit_save_to_disk(OLC_ZNUM(d));
      } else
        d->Output( "Saving zone info in memory.\r\n");

      new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(d->character)), TRUE,
	"OLC: %s edits zone info for room %d.", GET_NAME(d->character), OLC_NUM(d));
      /* FALL THROUGH */
    case 'n':
    case 'N':
      cleanup_olc(d, CLEANUP_ALL);
      break;
    default:
      d->Output( "Invalid choice!\r\n");
      d->Output( "Do you wish to save the zone info? : ");
      break;
    }
    break;
   /* End of ZEDIT_CONFIRM_SAVESTRING */

/*-------------------------------------------------------------------*/
  case ZEDIT_MAIN_MENU:
    switch (*arg) {
    case 'q':
    case 'Q':
      if (OLC_ZONE(d)->age || OLC_ZONE(d)->number) {
	d->Output( "Do you wish to save the changes to the zone info? (y/n) : ");
	OLC_MODE(d) = ZEDIT_CONFIRM_SAVESTRING;
      } else {
	d->Output( "No changes made.\r\n");
	cleanup_olc(d, CLEANUP_ALL);
      }
      break;
    case 'n':
    case 'N':
      /*
       * New entry.
       */
      if (OLC_ZONE(d)->cmd[0].command == 'S') {
        /* first command */
        if (new_command(OLC_ZONE(d), 0) && start_change_command(d, 0)) {
	  zedit_disp_comtype(d);
	  OLC_ZONE(d)->age = 1;
          break;
	}
      }
      d->Output( "What number in the list should the new command be? : ");
      OLC_MODE(d) = ZEDIT_NEW_ENTRY;
      break;
    case 'e':
    case 'E':
      /*
       * Change an entry.
       */
      d->Output( "Which command do you wish to change? : ");
      OLC_MODE(d) = ZEDIT_CHANGE_ENTRY;
      break;
    case 'd':
    case 'D':
      /*
       * Delete an entry.
       */
      d->Output( "Which command do you wish to delete? : ");
      OLC_MODE(d) = ZEDIT_DELETE_ENTRY;
      break;
    case 'z':
    case 'Z':
      /*
       * Edit zone name.
       */
      d->Output( "Enter new zone name : ");
      OLC_MODE(d) = ZEDIT_ZONE_NAME;
      break;
    case 'x':
    case 'X':
       zedit_disp_dimension_menu(d);
       break;
    case '1':
      /*
       * Edit zone builders.
       */
      d->Output( "Enter new builders list : ");
      OLC_MODE(d) = ZEDIT_ZONE_BUILDERS;
      break;
#if _CIRCLEMUD >= CIRCLEMUD_VERSION(3,0,21)
    case 'b':
    case 'B':
      /*
       * Edit bottom of zone.
       */
      if (GET_LEVEL(d->character) < LVL_IMPL)
	zedit_disp_menu(d);
      else {
	d->Output( "Enter new bottom of zone : ");
	OLC_MODE(d) = ZEDIT_ZONE_BOT;
      }
      break;
#endif
    case 't':
    case 'T':
      /*
       * Edit top of zone.
       */
      if (GET_LEVEL(d->character) < LVL_IMPL)
	zedit_disp_menu(d);
      else {
	d->Output( "Enter new top of zone : ");
	OLC_MODE(d) = ZEDIT_ZONE_TOP;
      }
      break;
    case 'l':
    case 'L':
      /*
       * Edit zone lifespan.
       */
      d->Output( "Enter new zone lifespan : ");
      OLC_MODE(d) = ZEDIT_ZONE_LIFE;
      break;
    case 'r':
    case 'R':
      /*
       * Edit zone reset mode.
       */
      d->Output( "\r\n"
		"0) Never reset\r\n"
		"1) Reset only when no players in zone\r\n"
		"2) Normal reset\r\n"
		"Enter new zone reset type : ");
      OLC_MODE(d) = ZEDIT_ZONE_RESET;
      break;
      	case 'f':
	case 'F':
	    zedit_disp_flag_menu(d);
	    break;
    default:
      zedit_disp_menu(d);
      break;
    }
    break;
    /* End of ZEDIT_MAIN_MENU */

/*-------------------------------------------------------------------*/
  case ZEDIT_NEW_ENTRY:
    /*
     * Get the line number and insert the new line.
     */
    pos = atoi(arg);
    if (isdigit(*arg) && new_command(OLC_ZONE(d), pos)) {
      if (start_change_command(d, pos)) {
	zedit_disp_comtype(d);
	OLC_ZONE(d)->age = 1;
      }
    } else
      zedit_disp_menu(d);
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_DELETE_ENTRY:
    /*
     * Get the line number and delete the line.
     */
    pos = atoi(arg);
    if (isdigit(*arg)) {
      delete_command(OLC_ZONE(d), pos);
      OLC_ZONE(d)->age = 1;
    }
    zedit_disp_menu(d);
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_CHANGE_ENTRY:
    /*
     * Parse the input for which line to edit, and goto next quiz.
     */
    /*
     *  Abort edit, and return to main menu 
     * - idea from Mark Garringer zizazat@hotmail.com
     */
    if (toupper(*arg) == 'A') { 
      if (OLC_CMD(d).command == 'N') { 
        OLC_CMD(d).command = '*';
      } 
      zedit_disp_menu(d);
      break;
    }

    pos = atoi(arg);
    if (isdigit(*arg) && start_change_command(d, pos)) {
      zedit_disp_comtype(d);
      OLC_ZONE(d)->age = 1;
    } else
      zedit_disp_menu(d);
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_COMMAND_TYPE:
    /*
     * Parse the input for which type of command this is, and goto next
     * quiz.
     */
    OLC_CMD(d).command = toupper(*arg);
    if (!OLC_CMD(d).command || (strchr("MOPEDGRTV", OLC_CMD(d).command) == NULL)) {
      d->Output( "Invalid choice, try again : ");
    } else {
      if (OLC_VAL(d)) {	/* If there was a previous command. */
        if (OLC_CMD(d).command == 'T' || OLC_CMD(d).command == 'V') {
          OLC_CMD(d).if_flag = 1;
          zedit_disp_arg1(d);
        } else {
	d->Output( "Is this command dependent on the success of the previous one? (y/n)\r\n");
	OLC_MODE(d) = ZEDIT_IF_FLAG;
	}
      } else {	/* 'if-flag' not appropriate. */
	OLC_CMD(d).if_flag = 0;
	zedit_disp_arg1(d);
      }
    }
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_IF_FLAG:
    /*
     * Parse the input for the if flag, and goto next quiz.
     */
    switch (*arg) {
    case 'y':
    case 'Y':
      OLC_CMD(d).if_flag = 1;
      break;
    case 'n':
    case 'N':
      OLC_CMD(d).if_flag = 0;
      break;
    default:
      d->Output( "Try again : ");
      return;
    }
    zedit_disp_arg1(d);
    break;
/*-------------------------------------------------------------------*/
    case ZEDIT_PROB:
	/*
	 *       Which command to change loadchance for? :/
	 */

	pos = atoi(arg);
	if (isdigit(*arg) && start_change_command(d, pos)) {
	    OLC_MODE(d) = ZEDIT_PROB2;
	    OLC_ZONE(d)->age = 1;
	    d->Output("Chance of loading (0-100) : ");
	    break;
	} else
	    d->Output("Invalid choice.\r\n");
	zedit_disp_menu(d);
	break;
/*-------------------------------------------------------------------*/
    case ZEDIT_PROB2:
	/*
	 *       Change the loadchance
	 */
	OLC_CMD(d).arg4 = MAX(0, MIN(100, atoi(arg)));
	OLC_MODE(d) = ZEDIT_MAIN_MENU;
	zedit_disp_menu(d);
	break;

/*-------------------------------------------------------------------*/
  case ZEDIT_ARG1:
    /*
     * Parse the input for arg1, and goto next quiz.
     */
    if (!isdigit(*arg)) {
      d->Output( "Must be a numeric value, try again : ");
      return;
    }
    switch (OLC_CMD(d).command) {
    case 'M':
      if ((pos = real_mobile(atoi(arg))) != NOBODY) {
	OLC_CMD(d).arg1 = pos;
	zedit_disp_arg2(d);
      } else
	d->Output( "That mobile does not exist, try again : ");
      break;
    case 'O':
    case 'P':
    case 'E':
    case 'G':
      if ((pos = real_object(atoi(arg))) != NOTHING) {
	OLC_CMD(d).arg1 = pos;
	zedit_disp_arg2(d);
      } else
	d->Output( "That object does not exist, try again : ");
      break;
    case 'T':
    case 'V':
      if (atoi(arg)<MOB_TRIGGER || atoi(arg)>WLD_TRIGGER)
        d->Output( "Invalid input.");
      else {
       OLC_CMD(d).arg1 = atoi(arg);
        zedit_disp_arg2(d);
      }
      break;
    case 'D':
    case 'R':
    default:
      /*
       * We should never get here.
       */
      cleanup_olc(d, CLEANUP_ALL);
      new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: zedit_parse(): case ARG1: Ack!");
      d->Output( "Oops...\r\n");
      break;
    }
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_ARG2:
    /*
     * Parse the input for arg2, and goto next quiz.
     */
    if (!isdigit(*arg)) {
      d->Output( "Must be a numeric value, try again : ");
      return;
    }
    switch (OLC_CMD(d).command) {
    case 'M':
    case 'O':
      OLC_CMD(d).arg2 = MIN(MAX_DUPLICATES, atoi(arg));
      OLC_CMD(d).arg3 = world_vnum[OLC_NUM(d)]->number;
      zedit_disp_menu(d);
      break;
    case 'G':
      OLC_CMD(d).arg2 = MIN(MAX_DUPLICATES, atoi(arg));
      zedit_disp_menu(d);
      break;
    case 'P':
    case 'E':
      OLC_CMD(d).arg2 = MIN(MAX_DUPLICATES, atoi(arg));
      zedit_disp_arg3(d);
      break;
    case 'V':
      OLC_CMD(d).arg2 = atoi(arg); /* context */
      OLC_CMD(d).arg3 = world_vnum[OLC_NUM(d)]->number;
      d->Output( "Enter the global name : ");
      OLC_MODE(d) = ZEDIT_SARG1;
      break;
    case 'T':
      if (real_trigger(atoi(arg)) != NOTHING) {
        OLC_CMD(d).arg2 = real_trigger(atoi(arg)); /* trigger */
        OLC_CMD(d).arg3 = world_vnum[OLC_NUM(d)]->number;   
        zedit_disp_menu(d);
      } else
        d->Output( "That trigger does not exist, try again : ");
      break;
    case 'D':
      pos = atoi(arg);
      /*
       * Count directions.
       */
      if (pos < 0 || pos > NUM_OF_DIRS)
	d->Output( "Try again : ");
      else {
	OLC_CMD(d).arg2 = pos;
	zedit_disp_arg3(d);
      }
      break;
    case 'R':
      if ((pos = real_object(atoi(arg))) != NOTHING) {
	OLC_CMD(d).arg2 = pos;
	zedit_disp_menu(d);
      } else
	d->Output( "That object does not exist, try again : ");
      break;
    default:
      /*
       * We should never get here, but just in case...
       */
      cleanup_olc(d, CLEANUP_ALL);
      new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: zedit_parse(): case ARG2: Ack!");
      d->Output( "Oops...\r\n");
      break;
    }
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_ARG3:
    /*
     * Parse the input for arg3, and go back to main menu.
     */
    if (!isdigit(*arg)) {
      d->Output( "Must be a numeric value, try again : ");
      return;
    }
    switch (OLC_CMD(d).command) {
    case 'E':
      pos = atoi(arg);
      /*
       * Count number of wear positions.  We could use NUM_WEARS, this is
       * more reliable.
       */
      while (*equipment_types[i] != '\n')
	i++;
      if (pos < 0 || pos > i)
	d->Output( "Try again : ");
      else {
	OLC_CMD(d).arg3 = pos;
	zedit_disp_menu(d);
      }
      break;
    case 'P':
      if ((pos = real_object(atoi(arg))) != NOTHING) {
	OLC_CMD(d).arg3 = pos;
	zedit_disp_menu(d);
      } else
	d->Output( "That object does not exist, try again : ");
      break;
    case 'D':
      pos = atoi(arg);
      if (pos < 0 || pos > 4)
	d->Output( "Try again : ");
      else {
	OLC_CMD(d).arg3 = pos;
	zedit_disp_menu(d);
      }
      break;
    case 'M':
    case 'O':
    case 'G':
    case 'R':
    case 'T':
    case 'V':
    default:
      /*
       * We should never get here, but just in case...
       */
      cleanup_olc(d, CLEANUP_ALL);
      new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: zedit_parse(): case ARG3: Ack!");
      d->Output( "Oops...\r\n");
      break;
    }
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_SARG1:
    if (strlen(arg)) {
      OLC_CMD(d).sarg1 = strdup(arg);
      OLC_MODE(d) = ZEDIT_SARG2;
      d->Output( "Enter the global value : ");
    } else
      d->Output( "Must have some name to assign : ");
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_SARG2:
    if (strlen(arg)) {
      OLC_CMD(d).sarg2 = strdup(arg);
      zedit_disp_menu(d);
    } else
      d->Output( "Must have some value to set it to :");
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_ZONE_NAME:
    /*
     * Add new name and return to main menu.
     */
    if (genolc_checkstring(d, arg)) {
      if (OLC_ZONE(d)->name)
        free(OLC_ZONE(d)->name);
      else
        log("SYSERR: OLC: ZEDIT_ZONE_NAME: no name to free!");
      OLC_ZONE(d)->name = strdup(arg);
      OLC_ZONE(d)->number = 1;
    }
    zedit_disp_menu(d);
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_ZONE_BUILDERS:
    /*
     * Add new builders list and return to main menu.
     */
    if (genolc_checkstring(d, arg)) {
      if (OLC_ZONE(d)->builders)
        free(OLC_ZONE(d)->builders);
      else
        log("SYSERR: OLC: ZEDIT_ZONE_BUILDERS: no builders list to free!");
      OLC_ZONE(d)->builders = strdup(arg);
      OLC_ZONE(d)->number = 1;
    }
    zedit_disp_menu(d);
    break;
  
/*-------------------------------------------------------------------*/
  case ZEDIT_ZONE_RESET:
    /*
     * Parse and add new reset_mode and return to main menu.
     */
    pos = atoi(arg);
    if (!isdigit(*arg) || pos < 0 || pos > 2)
      d->Output( "Try again (0-2) : ");
    else {
      OLC_ZONE(d)->reset_mode = pos;
      OLC_ZONE(d)->number = 1;
      zedit_disp_menu(d);
    }
    break;

    case ZEDIT_DIMENSION:
    /*
     * Parse and add new reset_mode and return to main menu.
     */
    pos = atoi(arg);
    if (!isdigit(*arg) || pos < 0 || pos > 4) {
      d->Output( "Try again (0-4) : ");
      zedit_disp_dimension_menu(d);
    } else {
      OLC_ZONE(d)->dimension = pos;
      OLC_ZONE(d)->number = 1;
      zedit_disp_menu(d);
    }
    break;

/*-------------------------------------------------------------------*/
  case ZEDIT_ZONE_LIFE:
    /*
     * Parse and add new lifespan and return to main menu.
     */
    pos = atoi(arg);
    if (!isdigit(*arg) || pos < 0 || pos > 240)
      d->Output( "Try again (0-240) : ");
    else {
      OLC_ZONE(d)->lifespan = pos;
      OLC_ZONE(d)->number = 1;
      zedit_disp_menu(d);
    }
    break;

/*-------------------------------------------------------------------*/
#if _CIRCLEMUD >= CIRCLEMUD_VERSION(3,0,21)
  case ZEDIT_ZONE_BOT:
    /*
     * Parse and add new bottom room in zone and return to main menu.
     */
    if (OLC_ZNUM(d) == 0)
      OLC_ZONE(d)->bot = LIMIT(atoi(arg), 0, OLC_ZONE(d)->top);
    else
      OLC_ZONE(d)->bot = LIMIT(atoi(arg), zone_table[OLC_ZNUM(d) - 1].top + 1, OLC_ZONE(d)->top);
    OLC_ZONE(d)->number = 1;
    zedit_disp_menu(d);
    break;
#endif

/*-------------------------------------------------------------------*/
  case ZEDIT_ZONE_TOP:
    /*
     * Parse and add new top room in zone and return to main menu.
     */
    if (OLC_ZNUM(d) == top_of_zone_table)
      OLC_ZONE(d)->top = LIMIT(atoi(arg), OLC_ZONE(d)->Bot(), HIGHEST_VNUM);
    else
      OLC_ZONE(d)->top = LIMIT(atoi(arg), OLC_ZONE(d)->Bot(), zone_table[OLC_ZNUM(d) + 1].Bot() - 1);
    OLC_ZONE(d)->number = 1;
    zedit_disp_menu(d);
    break;
case ZEDIT_ZONE_FLAGS:

	num = atoi(arg);
	if ((num > NUM_ZONE_FLAGS)) {
	    d->Output("That is not a valid choice!\r\n");
	    zedit_disp_flag_menu(d);
	} else if (num == 0) {
	    zedit_disp_menu(d);
	    break;
	} else {
	    TOGGLE_BIT(OLC_ZONE(d)->zone_flags, 1 << (num - 1));
	    OLC_ZONE(d)->number = 1;
	    zedit_disp_flag_menu(d);
	}
	return;

	break;
/*-------------------------------------------------------------------*/
  default:
    /*
     * We should never get here, but just in case...
     */
    cleanup_olc(d, CLEANUP_ALL);
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: zedit_parse(): Reached default case!");
    d->Output( "Oops...\r\n");
    break;
  }
}

/******************************************************************************/
/** End of parse_zedit()                                                     **/
/******************************************************************************/
