//
// C Implementation: vehicle_edit
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "comm.h"
#include "interpreter.h"
#include "db.h"
#include "utils.h"
#include "spells.h"
#include "handler.h"
#include "mail.h"
#include "screen.h"
#include "dg_scripts.h"
#include "clan.h"
#include "constants.h"
#include "genolc.h"
#include "oasis.h"
#include "tedit.h"
#include "improved-edit.h"
#include "descriptor.h"



struct vehicle_data *vehicles = NULL;

void ASSIGNOBJ(obj_vnum obj, SPECIAL(fname));

int vehicle_count(void)
{
  int count = 0;
  struct vehicle_data *temp;
  for (temp = vehicles;temp; temp = temp->next) count++;
  return count;
}

int valid_vehicle_num(int num)
{
  return num > 0 && num <= vehicle_count();
}

struct vehicle_data *find_vehicle_by_num(int num)
{
  int count = 1;
  struct vehicle_data *temp = NULL;
  for (temp = vehicles;!(!temp || count == num); temp = temp->next) count++;
  return temp;
}

/* flips it over so vehicles always are the right order */
int write_vehicles(FILE *fl, struct vehicle_data *v)
{

int cnt = 0;
#if 0
  if (!v)
    return 0;
  else if (v->next)
    cnt += write_vehicles(fl, v->next);
#else
struct vehicle_data *temp;
#endif
for (temp = v;temp;temp = temp->next) {
cnt++;

  fprintf(fl, "@vehicle %d\n", cnt);
  fprintf(fl, "%d :Vehicle\n",    temp->vehicle);
  fprintf(fl, "%d :Controls\n",   temp->controls);
  fprintf(fl, "%d :Hatch\n",      temp->hatch);
  fprintf(fl, "%d :Window\n",     temp->window);
  }
  return cnt;
}

void save_vehicles(void)
{
  char tempfile[MAX_INPUT_LENGTH + 4];
  FILE *fl;
  int echeck = 1;

  snprintf(tempfile, sizeof(tempfile), "%s.tmp", VEHICLE_FILE);
  if ((fl = fopen(tempfile, "w")) == NULL)
  {
    log("Couldn't open vehicle file - no vehicles saved");
    return;
  }
  fprintf(fl, "*Vehicle controls hatch window - vnums in that order\n");
  write_vehicles(fl, vehicles);
  echeck = fprintf(fl, "$\n");
  fclose(fl);
  if (echeck > 0)
    rename(tempfile, VEHICLE_FILE);
}


void load_vehicles(void)
{
  FILE *fl;
  struct vehicle_data *temp = NULL;
  char line[READ_SIZE];
  char tag[READ_SIZE];
  int v[5];
  int num = 0, part = 0;
  obj_rnum rnum;
  SPECIAL(vehicle_controls);
  SPECIAL(vehicle_window);
  SPECIAL(vehicle_hatch);
  SPECIAL(vehicle);

  if ((fl = fopen(VEHICLE_FILE, "r")) == NULL)
  {
    log("Couldn't open vehicle file - no vehicles assigned");
    return;
  }
  while (get_line(fl, line))
  {
    if (*line == '*')
      continue;
    if (*line == '$')
      break;
    if (*line == '@')
    {

      num++;
      part = 0;
      while (part < 4 && get_line(fl, line))
      {
        if (sscanf(line, "%d :%s", v, tag) != 2)
          break;
        part++;
        switch (*tag)
        {
        case 'V':
          v[1] = v[0];
          break;
        case 'C':
          v[2] = v[0];
          break;
        case 'H':
          v[3] = v[0];
          break;
        case 'W':
          v[4] = v[0];
          break;
        default:
          log("Error in vehicle file (%s): tag: %d - vehicle %d", VEHICLE_FILE, part, num);
      fclose(fl);
          exit(1);
          return;
        }
      }

      if (part != 4)
      {
        log("Error in vehicle file (%s): Vehicle number %d", VEHICLE_FILE, num);
    fclose(fl);
        exit(1);
        return;
      }


      CREATE(temp, struct vehicle_data, 1);

      temp->vehicle = v[1];
      temp->controls = v[2];
      temp->hatch = v[3];
      temp->window = v[4];
      temp->next = vehicles;
      vehicles = temp;

      if ((rnum = real_object(temp->vehicle)) != NOTHING)
        if (GET_OBJ_TYPE(obj_proto + rnum) == ITEM_VEHICLE)
          ASSIGNOBJ(temp->vehicle, vehicle);

      if ((rnum = real_object(temp->controls)) != NOTHING)
        if (GET_OBJ_TYPE(obj_proto + rnum) == ITEM_V_CONTROLS)
          ASSIGNOBJ(temp->controls, vehicle_controls);

      if ((rnum = real_object(temp->hatch)) != NOTHING)
        if (GET_OBJ_TYPE(obj_proto + rnum) == ITEM_V_HATCH)
          ASSIGNOBJ(temp->hatch, vehicle_hatch);

      if ((rnum = real_object(temp->window)) != NOTHING)
        if (GET_OBJ_TYPE(obj_proto + rnum) == ITEM_V_WINDOW)
          ASSIGNOBJ(temp->window, vehicle_window);
    }
  }
  log("     %d vehicles.", num);
  fclose(fl);
}

void remove_vehicle_num(int num)
{
  struct vehicle_data *v, *temp;
  int count = 0;

  for (v = vehicles;v; v = v->next)
    if (num == ++count)
    {
      REMOVE_FROM_LIST(v, vehicles, next);
      free(v);
      save_vehicles();
      return;
    }

}

void free_vehicle(struct vehicle_data *v)
{
  if (!v)
    return;
  if (v->next)
    free_vehicle(v->next);

  free(v);
}
void free_vehicles(void)
{
  free_vehicle(vehicles);
  vehicles = NULL;
}

void list_vehicles(Character *ch)
{
  struct vehicle_data *v;
  int count = 0;
  char buf[MAX_INPUT_LENGTH];
  DYN_DEFINE;

  DYN_CREATE;
  *dynbuf = '\0';

  ch->Send(
                   "Vehicle list ------------------------------------------------\r\n");
  for (v = vehicles;v;v = v->next)
  {
    count++;
    snprintf(buf, sizeof(buf),
             "{cR%-3d){cy Vehicle: {cg%-5d{cy Controls: {cg%-5d{cy Hatch: {cg%-5d{cy Window: {cg%-5d{c0\r\n",
             count, v->vehicle, v->controls, v->hatch, v->window);

    DYN_RESIZE(buf);
  }


  page_string(ch->desc, dynbuf, DYN_BUFFER);

}

ACMD(do_oasis_vedit)
{
  //char *buf3;
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];
  int number = NOWHERE, add = FALSE, rem = FALSE;
  Descriptor *d;

  /* Parse any arguments. */
  //buf3 =
  two_arguments(argument, buf1, buf2);

  if (!*buf1)
  {
    ch->Send(
                     "vedit new                -- to add a new vehicle\r\n"
                     "vedit <number>           -- to edit an existing vehicle\r\n"
                     "vedit remove <number>    -- to remove an existing vehicle\r\n"
                     "vlist                    -- to list current vehicles\r\n"
                     "vedit help               -- display this menu\r\n");
    return;
  }
  else if (!isdigit(*buf1))
  {
    if (!str_cmp("new", buf1))
      add = TRUE;
    else if (!str_cmp("remove", buf1))
      rem = TRUE;
    else
    {
      ch->Send(
                       "vedit new                -- to add a new vehicle\r\n"
                       "vedit <number>           -- to edit an existing vehicle\r\n"
                       "vedit remove <number>    -- to remove an existing vehicle\r\n"
                       "vlist                    -- to list current vehicles\r\n"
                       "vedit help               -- display this menu\r\n");
      return;
    }

    if (is_number(buf2))
    {
      number = atoi(buf2);
      if (rem)
      {
        if (!valid_vehicle_num(number))
        {
          ch->Send( "That is not a valid vehicle number (use vlist to view numbers).\r\n");
          return;
        }
      }
    }
    else
    {
      ch->Send( "That is not a number!\r\n");
      return;
    }
  }

  /*
   * If a numeric argument was given (like a room number), get it.
   */
  if (number == NOWHERE)
  {
    number = atoi(buf1);

    if (!valid_vehicle_num(number))
    {
      ch->Send( "That is not a valid vehicle number (use vlist to view numbers).\r\n");
      return;
    }
  }

  /* Check to make sure the room isn't already being edited. */
  for (d = descriptor_list; d; d = d->next)
  {
    if (STATE(d) == CON_VEDIT)
    {
      ch->Send( "The vehicles are currently being edited by %s.\r\n",
                       PERS(d->character, ch));
      return;
    }
  }

  /* Retrieve the player's descriptor. */
  d = ch->desc;

  /* Give the descriptor an OLC structure. */
  if (d->olc)
  {
    new_mudlog(BRF, LVL_IMMORT, TRUE, "SYSERR: do_oasis_vedit: Player already had olc structure.");
    free(d->olc);
    d->olc = NULL;
  }

  if (rem)
  {
    remove_vehicle_num(number);
    ch->Send( "Vehicle entry removed.\r\n");
    /* Free the olc data from the descriptor. */
    return;
  }

  /* Create the OLC structure. */
  CREATE(d->olc, struct oasis_olc_data, 1);

  if (!add) {
    OLC_NUM(d) = number;
    vedit_setup_existing(d, number);
  }
  else {
    OLC_NUM(d) = vehicle_count() + 1;
    vedit_setup_new(d);
  }

  STATE(d) = CON_VEDIT;
  act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);

  new_mudlog(BRF, LVL_IMMORT, TRUE, "OLC: %s starts editing vehicles", GET_NAME(ch));
}

void vedit_setup_new(Descriptor *d)
{
  CREATE(OLC_VEHICLE(d), struct vehicle_data, 1);
  OLC_VEHICLE(d)->vehicle = -1;
  OLC_VEHICLE(d)->controls = -1;
  OLC_VEHICLE(d)->hatch = -1;
  OLC_VEHICLE(d)->window = -1;
  OLC_VEHICLE(d)->next = NULL;
  vehicle_menu(d);
  OLC_VAL(d) = 0;
}
void vedit_setup_existing(Descriptor *d, int num)
{
  struct vehicle_data *vehicle, *temp;


  if ((temp = find_vehicle_by_num(num)) == NULL)
  {
    log("vehicle doesnt exist! %d", num);
    cleanup_olc(d, CLEANUP_ALL);
    d->Output( "That vehicle doesnt exist!\r\n");
    return;
  }

  CREATE(vehicle, struct vehicle_data, 1);


  vehicle->vehicle = temp->vehicle;
  vehicle->controls = temp->controls;
  vehicle->hatch = temp->hatch;
  vehicle->window = temp->window;

  /*
   * Attach copy of vehicle to player's descriptor.
   */
  OLC_VEHICLE(d) = vehicle;
  OLC_VAL(d) = 0;

  vehicle_menu(d);
}

void vehicle_menu(Descriptor *d)
{
  obj_rnum rnum[4];
  rnum[0] = real_object(OLC_VEHICLE(d)->vehicle);
  rnum[1] = real_object(OLC_VEHICLE(d)->controls);
  rnum[2] = real_object(OLC_VEHICLE(d)->hatch);
  rnum[3] = real_object(OLC_VEHICLE(d)->window);

  d->Output(
                  "{Cy---Vehicle number: {cg%d{c0\r\n"
                  "{cRA){cy Vehicle  : {cc%-5d {c0-- %s\r\n"
                  "{cRB){cy Controls : {cc%-5d {c0-- %s\r\n"
                  "{cRC){cy Hatch    : {cc%-5d {c0-- %s\r\n"
                  "{cRD){cy Window   : {cc%-5d {c0-- %s\r\n"
                  "\r\n"
                  "{cRQ){cy Save and Quit the editor\r\n"
                  "{cRE){cy Exit without saving\r\n"
                  "\r\n"
                  "{cgEdit what letter: {c0",
                  OLC_NUM(d),
                  ((rnum[0]==NOTHING) || OLC_VEHICLE(d)->vehicle == 0 ) ? -1 : OLC_VEHICLE(d)->vehicle,
                  ((rnum[0]==NOTHING) || OLC_VEHICLE(d)->vehicle == 0 ) ? "NONE": obj_proto[rnum[0]].short_description,
                  ((rnum[1]==NOTHING) || OLC_VEHICLE(d)->controls == 0 ) ? -1 : OLC_VEHICLE(d)->controls,
                  ((rnum[1]==NOTHING) || OLC_VEHICLE(d)->controls == 0 ) ? "NONE": obj_proto[rnum[1]].short_description,
                  ((rnum[2]==NOTHING) || OLC_VEHICLE(d)->hatch == 0) ? -1 : OLC_VEHICLE(d)->hatch,
                  ((rnum[2]==NOTHING) || OLC_VEHICLE(d)->hatch == 0) ? "NONE": obj_proto[rnum[2]].short_description,
                  ((rnum[3]==NOTHING) || OLC_VEHICLE(d)->window == 0) ? -1 : OLC_VEHICLE(d)->window,
                  ((rnum[3]==NOTHING) || OLC_VEHICLE(d)->window == 0) ? "NONE": obj_proto[rnum[3]].short_description
                 );
  OLC_MODE(d) = VEDIT_MAIN_MENU;

}

void vedit_parse(Descriptor *d,char *arg)
{
  obj_vnum vnum;
  obj_rnum rnum;

  if (!OLC_VEHICLE(d))
  {
    log("problem in vehicle editor - no vehicle structure");
    cleanup_olc(d, CLEANUP_ALL);
    return;
  }
  switch (OLC_MODE(d))
  {
  case VEDIT_MAIN_MENU:
    switch (LOWER(*arg))
    {
    case 'a':
      d->Output( "Vnum of vehicle: ");
      OLC_MODE(d) = VEDIT_VEHICLE;
      break;
    case 'b':
      d->Output( "Vnum of controls: ");
      OLC_MODE(d) = VEDIT_CONTROLS;
      break;
    case 'c':
      d->Output( "Vnum of hatch: ");
      OLC_MODE(d) = VEDIT_HATCH;
      break;
    case 'd':
      d->Output( "Vnum of window: ");
      OLC_MODE(d) = VEDIT_WINDOW;
      break;
    case 'q':
      {
        struct vehicle_data *temp;
        int num;
        if (OLC_NUM(d) <= (num = vehicle_count()))
        {
          if ((temp = find_vehicle_by_num(OLC_NUM(d))) != NULL)
          {
            temp->vehicle   = OLC_VEHICLE(d)->vehicle;
            temp->controls  = OLC_VEHICLE(d)->controls;
            temp->hatch     = OLC_VEHICLE(d)->hatch;
            temp->window    = OLC_VEHICLE(d)->window;
          }
          cleanup_olc(d, CLEANUP_ALL);
        }
        else
        {
          temp = find_vehicle_by_num(num);
          if (temp)
            temp->next = OLC_VEHICLE(d);
          else
            vehicles = OLC_VEHICLE(d);
          OLC_VEHICLE(d) = NULL;
          cleanup_olc(d, CLEANUP_ALL);
        }
        save_vehicles();
        d->Output( "Vehicle Saved!\r\n");
        return;
      }
      break;
    case 'e':
      cleanup_olc(d, CLEANUP_ALL);
      d->Output( "Vehicle Aborted!\r\n");
      return;
      break;
    default:
      d->Output( "Ah.. not a valid option.\r\n");
      vehicle_menu(d);
      break;
    }
    break;
  case VEDIT_VEHICLE:
    if (!arg || !*arg || !isdigit(*arg))
    {
      d->Output( "\r\nThat is not a number!\r\n");
      vehicle_menu(d);
      return;
    }
    else if ((vnum = atoi(arg)) <= 0)
    {
      d->Output( "\r\nPositive numbers only!\r\n");
      vehicle_menu(d);
      return;
    }
    else if ((rnum = real_object(vnum)) == NOTHING)
    {
      d->Output( "That is not even an object!\r\n");
      vehicle_menu(d);
      return;
    }
    else if (GET_OBJ_TYPE(obj_proto + rnum) != ITEM_VEHICLE)
    {
      d->Output( "\r\nThat is not a vehicle object!\r\n");
      vehicle_menu(d);
      return;
    }
    OLC_VEHICLE(d)->vehicle = vnum;
    vehicle_menu(d);
    break;
  case VEDIT_CONTROLS:
    if (!arg || !*arg || !isdigit(*arg))
    {
      d->Output( "\r\nThat is not a number!\r\n");
      vehicle_menu(d);
      return;
    }
    else if ((vnum = atoi(arg)) <= 0)
    {
      d->Output( "\r\nPositive numbers only!\r\n");
      vehicle_menu(d);
      return;
    }
    else if ((rnum = real_object(vnum)) == NOTHING)
    {
      d->Output( "That is not even an object!\r\n");
      vehicle_menu(d);
      return;
    }
    else if (GET_OBJ_TYPE(obj_proto + rnum) != ITEM_V_CONTROLS)
    {
      d->Output( "\r\nThat is not a vehicle controls object!\r\n");
      vehicle_menu(d);
      return;
    }
    OLC_VEHICLE(d)->controls = vnum;
    vehicle_menu(d);
    break;
  case VEDIT_HATCH:
    if (!arg || !*arg || !isdigit(*arg))
    {
      d->Output( "\r\nThat is not a number!\r\n");
      vehicle_menu(d);
      return;
    }
    else if ((vnum = atoi(arg)) <= 0)
    {
      d->Output( "\r\nPositive numbers only!\r\n");
      vehicle_menu(d);
      return;
    }
    else if ((rnum = real_object(vnum)) == NOTHING)
    {
      d->Output( "That is not even an object!\r\n");
      vehicle_menu(d);
      return;
    }
    else if (GET_OBJ_TYPE(obj_proto + rnum) != ITEM_V_HATCH)
    {
      d->Output( "\r\nThat is not a vehicle hatch object!\r\n");
      vehicle_menu(d);
      return;
    }
    OLC_VEHICLE(d)->hatch = vnum;
    vehicle_menu(d);
    break;
  case VEDIT_WINDOW:
    if (!arg || !*arg || !isdigit(*arg))
    {
      d->Output( "\r\nThat is not a number!\r\n");
      vehicle_menu(d);
      return;
    }
    else if ((vnum = atoi(arg)) <= 0)
    {
      d->Output( "\r\nPositive numbers only!\r\n");
      vehicle_menu(d);
      return;
    }
    else if ((rnum = real_object(vnum)) == NOTHING)
    {
      d->Output( "That is not even an object!\r\n");
      vehicle_menu(d);
      return;
    }
    else if (GET_OBJ_TYPE(obj_proto + rnum) != ITEM_V_WINDOW)
    {
      d->Output( "\r\nThat is not a vehicle object!\r\n");
      vehicle_menu(d);
      return;
    }
    OLC_VEHICLE(d)->window = vnum;
    vehicle_menu(d);
    break;
  }

}
