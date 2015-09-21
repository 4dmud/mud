/* ************************************************************************
*   File: objsave.c                                     Part of CircleMUD *
*  Usage: loading/saving player objects for rent and crash-save           *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *this is 4d's
************************************************************************ */

/* now with auto-equip - BK */

/*
 * AutoEQ by Burkhard Knopf <burkhard.knopf@informatik.tu-clausthal.de>
 */

#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "comm.h"
#include "handler.h"
#include "db.h"
#include "interpreter.h"
#include "utils.h"
#include "spells.h"
#include "genolc.h"
#include "descriptor.h"

/* these factors should be unique integers */
#define RENT_FACTOR      1
#define CRYO_FACTOR      4

#define LOC_INVENTORY    0
#define MAX_BAG_ROWS     5

extern int qic_items;              /* Number of items in database */
extern int qic_vnums[];
void add_owner(int nr, long id);

extern const int xap_objs;
int save_new_style = 1;
/* Extern functions */
//ACMD(do_action);
ACMD(do_tell);
SPECIAL(receptionist);
SPECIAL(cryogenicist);
void write_aliases(Character *ch);
void check_timer(obj_data *obj);

/* local functions */
struct obj_data *Obj_from_store(struct obj_file_elem object,
                                      int *location);
void Crash_extract_norent_eq(Character *ch);
void Crash_report_rent(Character *ch, Character *recep,
                       struct obj_data *obj, gold_int *cost,
                       long *nitems, int display, int factor);
void update_obj_file(void);
void Crash_rent_deadline(Character *ch, Character *recep,
                         long cost);
void Crash_restore_weight(struct obj_data *obj);
void Crash_extract_objs(struct obj_data *obj, Character *ch);
void Crash_extract_norents(struct obj_data *obj, Character *ch);
void Crash_extract_expensive(struct obj_data *obj);
void Crash_calculate_rent(struct obj_data *obj, gold_int *cost);
void Crash_rentsave(Character *ch, int cost);
int Crash_offer_rent(Character *ch, Character *receptionist,
                     int display, int factor);
int Crash_report_unrentables(Character *ch, Character *recep,
                             struct obj_data *obj);
int Obj_to_store(struct obj_data *obj, FILE * fl, int location);
int Crash_write_rentcode(Character *ch, FILE * fl,
                         struct rent_info *rent);
int gen_receptionist(Character *ch, Character *recep,
                     int cmd, char *arg, int mode);
int Crash_save(struct obj_data *obj, FILE * fp, int locate);
int Crash_is_unrentable(struct obj_data *obj);
int Crash_clean_qic(FILE * fl, char *name);

char *remove_cr(char *str, char *buf, size_t len);
int write_extra_desc(FILE *fl, struct extra_descr_data *ex_desc);
int write_extra_descs(FILE *fl, OBJ_DATA *obj);
int write_object_affects(FILE *fl, OBJ_DATA *obj);
int load_char_objects_to_char_old(Character *ch, FILE *fl);
int load_char_objects_to_char(Character *ch, FILE *fl);
struct obj_data *read_one_item(FILE *fl, OBJ_DATA *temp, int *locate);
int relocate_obj(room_rnum rnum, Character *ch, OBJ_DATA *temp, int locate, OBJ_DATA **cont_row);
int save_char_objs(Character *ch);
int save_one_item( OBJ_DATA *obj, FILE *fl, int locate);
int load_qic_check(int rnum);
void new_load_corpses(void);
void add_corpse_to_list(OBJ_DATA *corpse);
void remove_corpse_from_list(OBJ_DATA *corpse);
void add_identifier(struct obj_data *obj, long id);
void free_identifier(struct obj_data *obj);
int has_identifier(struct obj_data *obj, long id);
void pause_timer(struct obj_data* object);

#define NEW_CORPSE_FILE LIB_MISC"new_corpse.save"

extern struct corpse_list_data *corpse_list;
#if 0
/* skipped with xap objs */
struct obj_data *Obj_from_store(struct obj_file_elem object, int *location)
{
  struct obj_data *obj;

  int j, taeller;

  *location = 0;
  if (object.item_number != NOTHING || real_object(object.item_number) >= 0)
  {
    obj = read_object(object.item_number, VIRTUAL);
    *location = (int) object.locate;
    GET_OBJ_VAL(obj, 0) = object.value[0];
    GET_OBJ_VAL(obj, 1) = object.value[1];
    GET_OBJ_VAL(obj, 2) = object.value[2];
    GET_OBJ_VAL(obj, 3) = object.value[3];
    for (taeller = 0; taeller < EF_ARRAY_MAX; taeller++)
      obj->obj_flags.extra_flags[taeller] =
        object.extra_flags[taeller];
    GET_OBJ_WEIGHT(obj) = object.weight;
    GET_OBJ_TIMER(obj) = object.timer;
    for (taeller = 0; taeller < AF_ARRAY_MAX; taeller++)
      obj->obj_flags.bitvector[taeller] = object.bitvector[taeller];

    for (j = 0; j < MAX_OBJ_AFFECT; j++)
      obj->affected[j] = object.affected[j];

    return (obj);
  }
  else
    return (NULL);
}
#endif


int Obj_to_store_from(struct obj_data *obj, FILE * fl, int locate)
{
  return my_obj_save_to_disk(fl, obj, locate);
}


int Obj_to_store(struct obj_data *obj, FILE * fl, int location)
{
  return Obj_to_store_from(obj, fl, location);
}

int delete_pobj_file(const char *name)
{
  char filename[MAX_INPUT_LENGTH];
  FILE *fl;

  if (!get_filename(name, filename, ASCII_OBJ_FILES))
    return (0);

  if (!(fl = fopen(filename, "rb")))
  {
    if (errno != ENOENT) /* if it fails but NOT because of no file */
      log("SYSERR: deleting crash file %s (1): %s", filename, strerror(errno));
    return (0);
  }
  fclose(fl);

  /* if it fails, NOT because of no file */
  if (remove(filename) < 0 && errno != ENOENT)
    log("SYSERR: deleting crash file %s (2): %s", filename, strerror(errno));

  return (1);
}

int Crash_delete_file(const char *name)
{
  char filename[MAX_INPUT_LENGTH];
  FILE *fl;

  if (!get_filename(name, filename, NEW_OBJ_FILES))
    return (0);

  if (!(fl = fopen(filename, "rb")))
  {
    if (errno != ENOENT) /* if it fails but NOT because of no file */
      log("SYSERR: deleting crash file %s (1): %s", filename, strerror(errno));
    return (0);
  }
  fclose(fl);

  /* if it fails, NOT because of no file */
  if (remove(filename) < 0 && errno != ENOENT)
    log("SYSERR: deleting crash file %s (2): %s", filename, strerror(errno));

  return (1);
}


int Crash_delete_crashfile(Character *ch)
{
  char fname[MAX_INPUT_LENGTH];
  FILE *fl;
  struct rent_info rent;
  char line[MAX_INPUT_LENGTH];

  /* lets not delete em for now */
  if (!ch)
    return 1;

  if (!get_filename(GET_NAME(ch), fname, NEW_OBJ_FILES))
    return 0;

  if (!(fl = fopen(fname, "rb")))
  {
    if (errno != ENOENT) /* if it fails, NOT because of no file */
      log("SYSERR: checking for crash file %s (3): %s", fname, strerror(errno));
    return (0);
  }
  if (!feof(fl))
    get_line(fl, line);
  sscanf(line, "%d %d %d %d %d %d", &rent.rentcode, &rent.time, &rent.net_cost_per_diem, &rent.gold, &rent.account, &rent.nitems);
  fclose(fl);

  if (rent.rentcode == RENT_CRASH)
    Crash_delete_file(GET_NAME(ch));

  return (1);
}


int Crash_clean_file(const char *name)
{
  char fname[MAX_STRING_LENGTH], filetype[20];
  FILE *fl;
  int rentcode, timed, netcost, gold, account, nitems;
  char line[MAX_STRING_LENGTH];


  /* this isnt REALLY nessercery */
  //return 1;

  if (!get_filename(name, fname, NEW_OBJ_FILES))
    return 0;
  /*
   * open for write so that permission problems will be flagged now, at boot
   * time.
   */
  if (!(fl = fopen(fname, "r+b")))
  {
    if (errno != ENOENT) /* if it fails, NOT because of no file */
      log("SYSERR: OPENING OBJECT FILE %s (4): %s", fname, strerror(errno));
    return (0);
  }

  if (!feof(fl))
    get_line(fl, line);
  sscanf(line, "%d %d %d %d %d %d", &rentcode, &timed, &netcost,
         &gold, &account, &nitems);
  fclose(fl);


  if ((rentcode == RENT_CRASH) ||
      (rentcode == RENT_FORCED) || (rentcode == RENT_TIMEDOUT))
  {
    if (timed < (time(0) - (CONFIG_CRASH_TIMEOUT * SECS_PER_REAL_DAY)))
    {
      Crash_delete_file(name);
      switch (rentcode)
      {
      case RENT_CRASH:
        strcpy(filetype, "crash");
        break;
      case RENT_FORCED:
        strcpy(filetype, "forced rent");
        break;
      case RENT_TIMEDOUT:
        strcpy(filetype, "idlesave");
        break;
      default:
        strcpy(filetype, "UNKNOWN!");
        break;
      }
      log("    Deleting %s's %s file.", name, filetype);
      return 1;
    }
    /* Must retrieve rented items w/in 30 days */
  }
  else if (rentcode == RENT_RENTED)
    if (timed < time(0) - (CONFIG_RENT_TIMEOUT * SECS_PER_REAL_DAY))
    {
      Crash_delete_file(name);
      log("    Deleting %s's rent file.", name);
      return 1;
    }
  return (0);
}

/* Special version that is called from the QIC scanner, saves us some time since we don't
 * have to scan twice!
 */
int Crash_clean_qic(FILE * fl, char *name)
{
  char filetype[MAX_INPUT_LENGTH];
  char line[MAX_INPUT_LENGTH];
  int rentcode, timed, netcost, gold, account, nitems;

  rewind(fl);



  if (!feof(fl))
    get_line(fl, line);
  sscanf(line, "%d %d %d %d %d %d", &rentcode, &timed, &netcost,
         &gold, &account, &nitems);
  fclose(fl);

  if ((rentcode == RENT_CRASH) ||
      (rentcode == RENT_FORCED)
      || (rentcode == RENT_TIMEDOUT))
  {
    if (timed < time(0) - (CONFIG_CRASH_TIMEOUT * SECS_PER_REAL_DAY))
    {
      Crash_delete_file(name);
      switch (rentcode)
      {
      case RENT_CRASH:
        strcpy(filetype, "crash");
        break;
      case RENT_FORCED:
        strcpy(filetype, "forced rent");
        break;
      case RENT_TIMEDOUT:
        strcpy(filetype, "idlesave");
        break;
      default:
        strcpy(filetype, "UNKNOWN!");
        break;
      }
      log( "    Deleting %s's %s file.", name, filetype);
      return 1;
    }
    /* Must retrieve rented items w/in 180 days */
  }
  else if (rentcode == RENT_RENTED)
    if (timed < time(0) - (CONFIG_RENT_TIMEOUT * SECS_PER_REAL_DAY))
    {
      Crash_delete_file(name);
      log( "    Deleting %s's rent file.", name);
      return 1;
    }
  return (0);
}


void update_obj_file(void)
{
  for (int i = 0; i < pi.Size(); i++)
  {
    if (*pi.NameByIndex(i))
      Crash_clean_file(pi.NameByIndex(i));
  }
  return;
}



void Crash_listrent(Character *ch,const char *name)
{
  FILE *fl;
  char fname[MAX_INPUT_LENGTH], buf[MAX_STRING_LENGTH], buf2[MAX_STRING_LENGTH];
  struct obj_data *obj;
  int rentcode, timed, netcost, gold, account, nitems;
  int t[10], nr;
  char line[MAX_STRING_LENGTH];
  char *sdesc;


  if (!get_filename(name, fname, NEW_OBJ_FILES))
    return;


  if (!(fl = fopen(fname, "rb")))
  {
    ch->Send( "%s has no rent file.\r\n", name);
    return;
  }
  sprintf(buf, "%s\r\n", fname);

  if (!feof(fl))
  {

    get_line(fl, line);
    sscanf(line, "%d %d %d %d %d %d", &rentcode, &timed, &netcost,
           &gold, &account, &nitems);

  }


  switch (rentcode)
  {
  case RENT_RENTED:
    strcat(buf, "Rent\r\n");
    break;
  case RENT_CRASH:
    strcat(buf, "Crash\r\n");
    break;
  case RENT_CRYO:
    strcat(buf, "Cryo\r\n");
    break;
  case RENT_TIMEDOUT:
  case RENT_FORCED:
    strcat(buf, "TimedOut\r\n");
    break;
  default:
    strcat(buf, "Undef\r\n");
    break;
  }

  /* else we have xap objs */
  while (!feof(fl))
  {
    get_line(fl, line);
    if (*line == '#')
    {     /* swell - its an item */
      sscanf(line, "#%d", &nr);
      if (nr != NOTHING)
      {   /* then we can dispense with it easily */
      
        obj = read_object(nr, VIRTUAL);
        if (obj) {
        sprintf(buf, "%s[%5d] (%5dau) %-20s\r\n", buf,
                nr, GET_OBJ_RENT(obj), obj->short_description);
        free_obj(obj, FALSE);
        }
        
      }
      else
      { char *di;
   /* its nothing, and a unique item. bleh. partial parse. */
        get_line(fl, line);   /* this is obj+val */
        get_line(fl, line);   /* this is XAP */
        sprintf(buf2, "xap objects in listrent");
        di = fread_string(fl, buf2);    /* screw the name */
        if (di)
          free_string( &di);
        sdesc = fread_string(fl, buf2);
        di = fread_string(fl, buf2);    /* screw the long desc */
        if (di)
          free_string( &di);
        di = fread_string(fl, buf2);    /* screw the action desc. */
        if (di)
          free_string( &di);
        get_line(fl, line);   /* this is an important line.rent.. */
        sscanf(line, "%d %d %d %d %d", t, t + 1, t + 2, t + 3,
               t + 4);
        /* great we got it all, make the buf */
        sprintf(buf, "%s[%5d] (%5dau) %-20s\r\n", buf,
                nr, t[4], sdesc);
        /* best of all, we don't care if there's descs, or stuff.. */
        /* since we're only doing operations on lines beginning in # */
        /* i suppose you don't want to make exdescs start with # .:) */
      }
    }
  }

  // --HERE--
  /* why would you have send_to_char here anyway?!?! */
  page_string(ch->desc, buf, 0);
  fclose(fl);
}



int Crash_write_rentcode(Character *ch, FILE * fl,
                         struct rent_info *rent)
{

  if (fprintf(fl, "%d %d %d %d %d %d\r\n", rent->rentcode, rent->time,
              rent->net_cost_per_diem, rent->gold, rent->account,
              rent->nitems) < 1)
  {
    perror("Syserr: Writing rent code");
    return 0;
  }

  return 1;
}


/* so this is gonna be the auto equip (hopefully) */
void auto_equip(Character *ch, struct obj_data *obj, int locate)
{
  int j;

  if (locate > 0)
  {       /* was worn */
    switch (j = locate - 1)
    {
    case WEAR_LIGHT:
      break;
    case WEAR_FINGER_R:
    case WEAR_FINGER_L:
    case WEAR_THUMB_R:
    case WEAR_THUMB_L:
      if (!CAN_WEAR(obj, ITEM_WEAR_FINGER))  /* not fitting :( */
        locate = 0;
      break;
    case WEAR_NECK_1:
    case WEAR_NECK_2:
      if (!CAN_WEAR(obj, ITEM_WEAR_NECK))
        locate = 0;
      break;
    case WEAR_BODY:
      if (!CAN_WEAR(obj, ITEM_WEAR_BODY))
        locate = 0;
      break;
    case WEAR_HEAD:
      if (!CAN_WEAR(obj, ITEM_WEAR_HEAD))
        locate = 0;
      break;
    case WEAR_LEGS:
      if (!CAN_WEAR(obj, ITEM_WEAR_LEGS))
        locate = 0;
      break;
    case WEAR_LEGS_2:
      if (!CAN_WEAR(obj, ITEM_WEAR_LEGS))
        locate = 0;
      break;
    case WEAR_FEET:
      if (!CAN_WEAR(obj, ITEM_WEAR_FEET))
        locate = 0;
      break;
    case WEAR_FEET_2:
      if (!CAN_WEAR(obj, ITEM_WEAR_FEET))
        locate = 0;
      break;
    case WEAR_HANDS:
      if (!CAN_WEAR(obj, ITEM_WEAR_HANDS))
        locate = 0;
      break;
    case WEAR_ARMS:
      if (!CAN_WEAR(obj, ITEM_WEAR_ARMS))
        locate = 0;
      break;
    case WEAR_SHIELD:
      if (!CAN_WEAR(obj, ITEM_WEAR_SHIELD))
        locate = 0;
      break;
    case WEAR_ABOUT:
      if (!CAN_WEAR(obj, ITEM_WEAR_ABOUT))
        locate = 0;
      break;
    case WEAR_WAIST:
      if (!CAN_WEAR(obj, ITEM_WEAR_WAIST))
        locate = 0;
      break;
    case WEAR_WRIST_R:
    case WEAR_WRIST_L:
      if (!CAN_WEAR(obj, ITEM_WEAR_WRIST))
        locate = 0;
      break;
    case WEAR_WIELD:
      if (!CAN_WEAR(obj, ITEM_WEAR_WIELD))
        locate = 0;
      break;
    case WEAR_WIELD_2:
      if (!CAN_WEAR(obj, ITEM_WEAR_WIELD))
        locate = 0;
      break;
    case WEAR_HOLD:
      if (!CAN_WEAR(obj, ITEM_WEAR_HOLD))
        locate = 0;
      break;
    case WEAR_FACE:
      if (!CAN_WEAR(obj, ITEM_WEAR_FACE))
        locate = 0;
      break;
    case WEAR_EYES:
      if (!CAN_WEAR(obj, ITEM_WEAR_EYES))
        locate = 0;
      break;
    case WEAR_HIPS:
      if (!CAN_WEAR(obj, ITEM_WEAR_HIPS))
        locate = 0;
      break;
    case WEAR_EAR_R:
    case WEAR_EAR_L:
    case WEAR_EAR_TIP:
      if (!CAN_WEAR(obj, ITEM_WEAR_EAR))
        locate = 0;
      break;
    case WEAR_ANKLE_R:
    case WEAR_ANKLE_L:
      if (!CAN_WEAR(obj, ITEM_WEAR_ANKLE))
        locate = 0;
      break;
    case WEAR_HORNS:
      if (!CAN_WEAR(obj, ITEM_WEAR_HORNS))
        locate = 0;
      break;
    case WEAR_ANTENNA:
      if (!CAN_WEAR(obj, ITEM_WEAR_ANTENNA))
        locate = 0;
      break;
    case WEAR_TAIL:
      if (!CAN_WEAR(obj, ITEM_WEAR_TAIL))
        locate = 0;
      break;
    case WEAR_FOCUS:
      if (!CAN_WEAR(obj, ITEM_WEAR_FOCUS))
        locate = 0;
      break;
    case WEAR_SADDLE:
      if (!CAN_WEAR(obj, ITEM_WEAR_ABOUT))
        locate = 0;
      break;
    case WEAR_CREST:
      if (!CAN_WEAR(obj, ITEM_WEAR_CREST))
        locate = 0;
      break;
    case WEAR_THIGH_L:
    case WEAR_THIGH_R:
      if (!CAN_WEAR(obj, ITEM_WEAR_THIGH))
        locate = 0;
      break;
    case WEAR_KNEE_L:
    case WEAR_KNEE_R:
      if (!CAN_WEAR(obj, ITEM_WEAR_KNEE))
        locate = 0;
      break;
    case WEAR_FLOATING:
      if (!CAN_WEAR(obj, ITEM_WEAR_FLOATING))
        locate = 0;
      break;
    default:
      locate = 0;
    }
    if (locate > 0)
    {
      if (!GET_EQ(ch, j))
      {
        /* check ch's alignment to prevent $M from being zapped
           through auto-equip */
        if ((IS_OBJ_STAT(obj, ITEM_ANTI_EVIL) && IS_EVIL(ch)) ||
            (IS_OBJ_STAT(obj, ITEM_ANTI_GOOD) && IS_GOOD(ch)) ||
            (IS_OBJ_STAT(obj, ITEM_ANTI_NEUTRAL)
             && IS_NEUTRAL(ch)))
          locate = 0;
        else
          equip_char(ch, obj, j);
      }
      else          /* oops - saved player with double equipment[j]? */
        locate = 0;
    }
  }
  if (locate <= 0)
    obj_to_char(obj, ch);
}



#define MAX_BAG_ROW 5
/* should be enough - who would carry a bag in a bag in a bag in a
   bag in a bag in a bag ?!? */

/* return values:
        0 - successful load, keep char in rent room.
        1 - load failure or load of crash items -- put char in temple.
        2 - rented equipment lost (no $)
*/
int Crash_load(Character *ch)
{
  return (Crash_load_xapobjs(ch));
}



int Crash_save(struct obj_data *obj, FILE * fp, int locate)
{
  struct obj_data *tmp;
  int result;

  if (obj)
  {
    Crash_save(obj->next_content, fp, locate);
    Crash_save(obj->contains, fp, MIN(0, locate) - 1);

    for (tmp = obj->in_obj; tmp; tmp = tmp->in_obj)
      GET_OBJ_WEIGHT(tmp) -= GET_OBJ_WEIGHT(obj);

    if (save_new_style)
      result = save_one_item(obj, fp, locate);
    else
      result = Obj_to_store_from(obj, fp, locate);


    if (result <= 0)
      return (FALSE);
  }
  return (TRUE);
}


void Crash_restore_weight(struct obj_data *obj)
{
  if (obj)
  {
    Crash_restore_weight(obj->contains);
    Crash_restore_weight(obj->next_content);
    if (obj->in_obj)
      GET_OBJ_WEIGHT(obj->in_obj) += GET_OBJ_WEIGHT(obj);
  }
}

void Crash_extract_objs(struct obj_data *obj, Character *ch)
{
  obj_rnum nrr;
  if (obj)
  {
    Crash_extract_objs(obj->contains, ch);
    Crash_extract_objs(obj->next_content, ch);
    if (obj->worn_by)
      obj = unequip_char(obj->worn_by, obj->worn_on);
    else if (obj->carried_by)
      obj_from_char(obj);
    else if (obj->in_obj)
      obj_from_obj(obj);
    nrr = real_object(GET_OBJ_VNUM(obj));
    if (nrr != NOTHING && obj_index[nrr].qic != NULL)
    {
      /*already added from scan rent*/
      obj_index[nrr].qic->items++;
    }
    extract_obj(obj);
  }
}


int Crash_is_unrentable(struct obj_data *obj)
{
  if (!obj)
    return 0;
  if (GET_OBJ_TYPE(obj) == ITEM_KEY && OBJ_FLAGGED(obj, ITEM_KEYSTAY))
    return 0;

  if (IS_OBJ_STAT(obj, ITEM_NORENT) || GET_OBJ_TYPE(obj) == ITEM_KEY)
    return 1;

  return 0;
}


void Crash_extract_norents(struct obj_data *obj, Character *ch)
{
  if (obj)
  {
    Crash_extract_norents(obj->contains, ch);
    Crash_extract_norents(obj->next_content, ch);

    if (obj->owner != 0)
    {
      if (obj->owner != GET_IDNUM(ch) && obj->worn_by == NULL)
      {
        while (obj->in_obj)
          obj_from_obj(obj);
        if (obj->carried_by)
          obj_from_char(obj);
        obj_to_room(obj, IN_ROOM(ch));
        ch->Send( "%s drops to the ground.\r\n", obj->short_description);
        act("$p drops to the ground.", FALSE, ch, obj, NULL, TO_ROOM);
        return;
      }
    }
    if (Crash_is_unrentable(obj))
    {
      obj_rnum nrr = real_object(GET_OBJ_VNUM(obj));
      if (nrr != NOTHING && obj_index[nrr].qic != NULL)
      {
        /*already added from scan rent*/
        obj_index[nrr].qic->items++;
      }
      extract_obj(obj);
      return;
    }
  }
}

/*
 * Get !RENT items from equipment to inventory and
 * extract !RENT out of worn containers.
 */
void Crash_extract_norent_eq(Character *ch)
{
  int j;

  for (j = 0; j < NUM_WEARS; j++)
  {
    if (GET_EQ(ch, j) == NULL)
      continue;

    if (Crash_is_unrentable(GET_EQ(ch, j)))
      obj_to_char(unequip_char(ch, j), ch);
    else
      Crash_extract_norents(GET_EQ(ch, j), ch);
  }
}

void Crash_extract_expensive(struct obj_data *obj)
{
  struct obj_data *tobj, *max;

  max = obj;
  for (tobj = obj; tobj; tobj = tobj->next_content)
    if (GET_OBJ_RENT(tobj) > GET_OBJ_RENT(max))
      max = tobj;
  extract_obj(max);
}



void Crash_calculate_rent(struct obj_data *obj, gold_int *cost)
{
  return;
}


void Crash_crashsave(Character *ch)
{
  char filename[MAX_INPUT_LENGTH];
  char tempname[MAX_INPUT_LENGTH + 4];
  struct rent_info rent;
  int j;
  FILE *fp;

  if (IS_NPC(ch))
    return;
  if (ch->loader != GET_IDNUM(ch))
  { /** they are not in the automeld state - mord*/
    if (!ch->desc)
    {

      log("Saving %s eq when they are linkless", GET_NAME(ch));
      return;
    }
    if (!IS_PLAYING(ch->desc))
    {
      log("Saving %s eq when they aren't state playing", GET_NAME(ch));
      //don't save because they might be naked in the menu
      return;
    }
  }
  if (IS_SAVING(ch))
  {
    log("Attempt made to crashsave %s's equipment when it is currently being saved!", GET_NAME(ch));
    return;
  }



  if (!get_filename(GET_NAME(ch), filename, (save_new_style) ? ASCII_OBJ_FILES : NEW_OBJ_FILES))
    return;

  IS_SAVING(ch) = TRUE;
  snprintf(tempname, sizeof(tempname), "%s%s", filename, ".tmp");
  if (!(fp = fopen(tempname, "wb")))
  {
    log("ERR: Can't save %s's file crash save.", GET_NAME(ch));
    IS_SAVING(ch) = FALSE;
    return;
  }
  if (!save_new_style)
  {
    rent.rentcode = RENT_CRASH;
    rent.time = time(0);
    rent.net_cost_per_diem = 0;
    rent.gold = 0;
    rent.account = 0;
    rent.nitems = 0;

    fprintf(fp,"%d %d %d %d %d %d\n",rent.rentcode,rent.time,
            rent.net_cost_per_diem,rent.gold,rent.account,rent.nitems);
  }

  for (j = 0; j < NUM_WEARS; j++)
    if (GET_EQ(ch, j))
    {
      if (!Crash_save(GET_EQ(ch, j), fp, j + 1))
      {
        fclose(fp);
        if (remove(tempname) == -1)
        {
          new_mudlog(NRM, LVL_GOD, TRUE, "unable to remove temp file: %s", tempname);
          log("unable to remove temp file: %s", tempname);
        }
        IS_SAVING(ch) = FALSE;
        return;
      }
      Crash_restore_weight(GET_EQ(ch, j));
      // Crash_extract_objs(GET_EQ(ch, j));
    }

  if (!Crash_save(ch->carrying, fp, 0))
  {
    fclose(fp);
    if (remove(tempname) == -1)
    {
      new_mudlog(NRM, LVL_GOD, TRUE, "unable to remove temp file: %s", tempname);
      log("unable to remove temp file: %s", tempname);
    }

    IS_SAVING(ch) = FALSE;
    return;
  }
  Crash_restore_weight(ch->carrying);

  fclose(fp);
  REMOVE_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);
#if 0 && defined(unix)
  snprintf(buf1, sizeof(buf1),"cp %s %s.cbak", buf, buf);
  system(buf1);
  snprintf(buf1, sizeof(buf1),"gzip -fq %s.cbak", buf);
  system(buf1);
#endif
  //Crash_extract_objs(ch->carrying);
  if (rename(tempname, filename) == -1)
  {
    new_mudlog(NRM, LVL_GOD, TRUE, "Major error (no disk space) can't save file: %s", tempname);
    core_dump();
  }
  IS_SAVING(ch) = FALSE;
}


void Crash_rentsave(Character *ch, int cost)
{
  char filename[MAX_INPUT_LENGTH];
  char tempname[MAX_INPUT_LENGTH + 4];
  struct rent_info rent;
  int j;
  FILE *fp;

  if (IS_NPC(ch))
    return;
  if (IS_SAVING(ch))
  {
    log("Attempt made to rentsave %s's equipment when it is currently being saved!", GET_NAME(ch));
    return;
  }
  if (ch->loader != GET_IDNUM(ch))
  { /** they are not in the automeld state - mord*/
    if (!ch->desc)
    {
  
      log("Saving %s eq when they are linkless", GET_NAME(ch));
      return;
    }
    if (!IS_PLAYING(ch->desc))
    {
      log("Saving %s eq when they aren't state playing", GET_NAME(ch));
      //don't save because they might be naked in the menu
      return;
    }
  }

  if (!get_filename(GET_NAME(ch), filename, (save_new_style) ? ASCII_OBJ_FILES : NEW_OBJ_FILES))
    return;
  /* this should not be made true again since it will be reset when they reenter the game */
  IS_SAVING(ch) = TRUE;
  snprintf(tempname, sizeof(tempname), "%s%s", filename, ".tmp");
  if (!(fp = fopen(tempname, "wb")))
  {
    log("ERR: Can't open %s's file for rent save.", GET_NAME(ch));
    return;
  }

  Crash_extract_norent_eq(ch);
  Crash_extract_norents(ch->carrying, ch);
  if (!save_new_style)
  {
    rent.net_cost_per_diem = 0;
    rent.rentcode = RENT_RENTED;
    rent.time = time(0);
    rent.gold = GET_GOLD(ch);
    rent.account = GET_BANK_GOLD(ch);


    fprintf(fp, "%d %d %d %d %d %d\n", rent.rentcode, rent.time,
            rent.net_cost_per_diem, rent.gold, rent.account,
            0);

  }
  for (j = 0; j < NUM_WEARS; j++)
    if (GET_EQ(ch, j))
    {
      if (!Crash_save(GET_EQ(ch, j), fp, j + 1))
      {
        fclose(fp);
        if (remove(tempname) == -1)
        {
          new_mudlog(NRM, LVL_GOD, TRUE, "unable to remove temp file: %s", tempname);
          log("unable to remove temp file: %s", tempname);
        }
        IS_SAVING(ch) = FALSE;
        return;
      }
      Crash_restore_weight(GET_EQ(ch, j));
      Crash_extract_objs(GET_EQ(ch, j), ch);
    }

  if (!Crash_save(ch->carrying, fp, 0))
  {
    fclose(fp);
    if (remove(tempname) == -1)
    {
      new_mudlog(NRM, LVL_GOD, TRUE, "unable to remove temp file: %s", tempname);
      log("unable to remove temp file: %s", tempname);
    }
    IS_SAVING(ch) = FALSE;
    return;
  }
  fclose(fp);

  Crash_extract_objs(ch->carrying, ch);
  if (rename(tempname, filename) == -1)
  {
    new_mudlog(NRM, LVL_GOD, TRUE, "Major error (no disk space) can't save file: %s", tempname);
    core_dump();
  }
}


/* ************************************************************************
* Routines used for the receptionist                          *
************************************************************************* */

void Crash_rent_deadline(Character *ch, Character *recep,
                         long cost)
{
  return;
}

int Crash_report_unrentables(Character *ch, Character *recep,
                             struct obj_data *obj)
{

  return (0);
}



void Crash_report_rent(Character *ch, Character *recep,
                       struct obj_data *obj, gold_int *cost,
                       long *nitems, int display, int factor)
{
  return;
}



int Crash_offer_rent(Character *ch, Character *receptionist,
                     int display, int factor)
{
  return 0;
}



int gen_receptionist(Character *ch, Character *recep,
                     int cmd, char *arg, int mode)
{
  return 0;
}


SPECIAL(receptionist)
{
  return (gen_receptionist
          (ch, (Character *) me, cmd, argument, RENT_FACTOR));
}


SPECIAL(cryogenicist)
{
  return (gen_receptionist
          (ch, (Character *) me, cmd, argument, CRYO_FACTOR));
}


void Crash_save_all(void)
{
  Descriptor *d;
  for (d = descriptor_list; d; d = d->next)
  {
    lock_desc(d);
    if (IS_PLAYING(d) && d->character && !IS_NPC(d->character))
    {
      if (PLR_FLAGGED(d->character, PLR_CRASH))
      {
        Crash_crashsave(d->character);
        d->character->save();
        write_aliases(d->character);
        REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_CRASH);
      }
    }
    unlock_desc(d);
  }
}

int relocate_obj(room_rnum rnum, Character *ch, OBJ_DATA *temp, int locate, OBJ_DATA **cont_row)
{

  int j;
  OBJ_DATA *obj1;
  /*
     what to do with a new loaded item:

     if there's a list with <locate> less than 1 below this:
     (equipped items are assumed to have <locate>==0 here) then its
     container has disappeared from the file   *gasp*
     -> put all the list back to ch's inventory
     if there's a list of contents with <locate> 1 below this:
     check if it's a container
     - if so: get it from ch, fill it, and give it back to ch (this way the
     container has its correct weight before modifying ch)
     - if not: the container is missing -> put all the list to ch's inventory

     for items with negative <locate>:
     if there's already a list of contents with the same <locate> put obj to it
     if not, start a new list

     Confused? Well maybe you can think of some better text to be put here ...

     since <locate> for contents is < 0 the list indices are switched to
     non-negative
   */

  if (locate > 0)
  {  /* item equipped */
    for (j = MAX_BAG_ROW - 1; j > 0; j--)
      if (cont_row[j])
      {   /* no container -> back to ch's inventory */
        for (; cont_row[j]; cont_row[j] = obj1)
        {
          obj1 = cont_row[j]->next_content;
          if (ch!=NULL)
            obj_to_char(cont_row[j], ch);
          else
            obj_to_room(cont_row[j], rnum);
        }
        cont_row[j] = NULL;
      }
    if (cont_row[0])
    {     /* content list existing */
      if (GET_OBJ_TYPE(temp) == ITEM_CONTAINER)
      {
        /* rem item ; fill ; equip again */
        if (ch != NULL)
          temp = unequip_char(ch, locate - 1);
        temp->contains = NULL;     /* should be empty - but who knows */
        for (; cont_row[0]; cont_row[0] = obj1)
        {
          obj1 = cont_row[0]->next_content;
          obj_to_obj(cont_row[0], temp);
        }
        if (ch != NULL)
          equip_char(ch, temp, locate - 1);
        else
          obj_to_room(temp, rnum);
      }
      else
      {   /* object isn't container -> empty content list */
        for (; cont_row[0]; cont_row[0] = obj1)
        {
          obj1 = cont_row[0]->next_content;
          if (ch != NULL)
            obj_to_char(cont_row[0], ch);
          else
            obj_to_room(cont_row[0], rnum);
        }
        cont_row[0] = NULL;
      }
    }
  }
  else
  {       /* locate <= 0 */
    for (j = MAX_BAG_ROW - 1; j > -locate; j--)
      if (cont_row[j])
      {   /* no container -> back to ch's inventory */
        for (; cont_row[j]; cont_row[j] = obj1)
        {
          obj1 = cont_row[j]->next_content;
          if (ch != NULL)
            obj_to_char(cont_row[j], ch);
          else
            obj_to_room(cont_row[j], rnum);
        }
        cont_row[j] = NULL;
      }

    if (j == -locate && cont_row[j])
    {     /* content list existing */
      if (GET_OBJ_TYPE(temp) == ITEM_CONTAINER)
      {
        /* take item ; fill ; give to char again */
        if (ch != NULL)
          obj_from_char(temp);
        else
          obj_from_room(temp);
        temp->contains = NULL;
        for (; cont_row[j]; cont_row[j] = obj1)
        {
          obj1 = cont_row[j]->next_content;
          obj_to_obj(cont_row[j], temp);
        }
        if (ch != NULL)
          obj_to_char(temp, ch);   /* add to inv first ... */
        else
          obj_to_room(temp, rnum);
      }
      else
      {   /* object isn't container -> empty content list */
        for (; cont_row[j]; cont_row[j] = obj1)
        {
          obj1 = cont_row[j]->next_content;
          if (ch != NULL)
            obj_to_char(cont_row[j], ch);
          else
            obj_to_room(cont_row[j], rnum);
        }
        cont_row[j] = NULL;
      }
    }

    if (locate < 0 && locate >= -MAX_BAG_ROW)
    {
      /* let obj be part of content list
         but put it at the list's end thus having the items
         in the same order as before renting */
      if (ch != NULL)
        obj_from_char(temp);
      else
        obj_from_room(temp);
      if ((obj1 = cont_row[-locate - 1]))
      {
        while (obj1->next_content)
          obj1 = obj1->next_content;
        obj1->next_content = temp;
      }
      else
        cont_row[-locate - 1] = temp;
    }
  }            /* locate less than zero */
  return 1;
}


int save_one_item( OBJ_DATA *obj,FILE *fl, int locate)
{
  obj_vnum nr;
  //obj_rnum rn;
  char buf[MAX_STRING_LENGTH] = "";
  int i;

  //rn = GET_OBJ_RNUM(obj);

  fprintf(fl, "#%d OBJ\n", (nr = GET_OBJ_VNUM(obj)));
  if (nr == NOTHING || IS_UNIQUE(obj))
  {
    if (obj->name)
      fprintf(fl, "Name: %s\n", obj->name);
    if (obj->description)
      fprintf(fl, "Description:\n%s~\n", remove_cr(obj->description, buf, sizeof(buf)));
    if (obj->short_description )
      fprintf(fl, "Short:\n%s~\n", remove_cr(obj->short_description, buf, sizeof(buf)));
    if (obj->action_description)
      fprintf(fl, "ActionD:\n%s~\n", remove_cr(obj->action_description, buf, sizeof(buf)));
    if (obj->smell)
      fprintf(fl, "Smell:\n%s~\n",remove_cr(obj->smell, buf, sizeof(buf)));
    if (obj->taste)
      fprintf(fl, "Taste:\n%s~\n", remove_cr(obj->taste, buf, sizeof(buf)));
    if (obj->feel)
      fprintf(fl, "Feel:\n%s~\n", remove_cr(obj->feel, buf, sizeof(buf)));

    if (obj->ex_description)
    {
      fprintf(fl, "ExDescr:\n");
      if (write_extra_descs(fl, obj) < 0)
        return -1;
    }

    fprintf(fl, "Type: %d\n", GET_OBJ_TYPE(obj));
    fprintf(fl, "Wear: %d %d %d %d\n", GET_OBJ_WEAR(obj)[0], GET_OBJ_WEAR(obj)[1],  GET_OBJ_WEAR(obj)[2], GET_OBJ_WEAR(obj)[3]);
    fprintf(fl, "Weight: %d\n", GET_OBJ_WEIGHT(obj));
    fprintf(fl, "Cost: %lld\n", GET_OBJ_COST(obj));
    fprintf(fl, "Rent: %d\n", GET_OBJ_RENT(obj));
    fprintf(fl, "Innate: %d\n", GET_OBJ_INNATE(obj));
    fprintf(fl, "Level: %d\n", GET_OBJ_LEVEL(obj));
    fprintf(fl, "Perm: %d %d %d %d\n", GET_OBJ_PERM(obj)[0], GET_OBJ_PERM(obj)[1], GET_OBJ_PERM(obj)[2], GET_OBJ_PERM(obj)[3]);
    fprintf(fl, "Affects:\n");
    if (write_object_affects(fl, obj) < 0)
      return -1;
  }
  if (obj->idents)
  {
    struct ident_list *iden;
    fprintf(fl, "Idents:\n");

    for (iden = obj->idents;iden;iden=iden->next)
      fprintf(fl, "%ld\n", iden->id);

    fprintf(fl, "%d\n", -1);
  }
  fprintf(fl, "Location: %d\n", locate);
  fprintf(fl, "Values:\n");
  for ( i = 0; i < 8; ++i )
    if ( GET_OBJ_VAL ( obj, i ) != 0 )
      fprintf ( fl, "%d %d\n", i, GET_OBJ_VAL ( obj, i ) );
  if ( GET_OBJ_QUALITY ( obj ) > LOWEST_QUALITY )
      fprintf ( fl, "8 %lf\n", GET_OBJ_QUALITY ( obj ) );
  for ( i = 9; i < 14; ++i )
    if ( GET_OBJ_VAL ( obj, i - 1 ) != 0 )
      fprintf ( fl, "%d %d\n", i, GET_OBJ_VAL ( obj, i - 1 ) );
  if ( GET_OBJ_MAX_QUALITY ( obj ) > LOWEST_QUALITY )
    fprintf ( fl, "14 %lf\n", GET_OBJ_MAX_QUALITY ( obj ) );

  fprintf(fl, "$\n");
  fprintf(fl, "Extra: %d %d %d %d\n", GET_OBJ_EXTRA(obj)[0], GET_OBJ_EXTRA(obj)[1],  GET_OBJ_EXTRA(obj)[2], GET_OBJ_EXTRA(obj)[3]);
  fprintf(fl, "Vroom: %d\n", GET_OBJ_VROOM(obj));
  fprintf(fl, "Owner: %ld\n", obj->owner);
  fprintf(fl, "Timer: %d\n", GET_OBJ_TIMER(obj));
  fprintf(fl, "Expir: %ld\n", GET_OBJ_EXPIRE(obj));
  fprintf(fl, "Exrem: %ld\n", GET_OBJ_SAVED_REMAINING_EXPIRE(obj));
  for ( i = 0; i < MAX_OBJ_AFFECT; ++i )
    if ( obj->orig_affected[ i ].location > 0 )
      fprintf(fl, "OrigAff: %d %d\n", obj->orig_affected[ i ].location, obj->orig_affected[ i ].modifier );
  return fprintf(fl, "@END\n\n");
}

int write_object_affects(FILE *fl, OBJ_DATA *obj)
{
  int i;
  for (i = 0; i < MAX_OBJ_AFFECT; i++)
  {
    if (obj->affected[i].modifier)
      fprintf(fl, "A\n"
              "%d %d\n", obj->affected[i].location, obj->affected[i].modifier);
  }
  return fprintf(fl, "$\n");
}

int write_extra_descs(FILE *fl, OBJ_DATA *obj)
{
  struct extra_descr_data *ex_desc;

  for (ex_desc = obj->ex_description; ex_desc; ex_desc = ex_desc->next)
  {
    /*
     * Sanity check to prevent nasty protection faults.
     */
    if (!ex_desc->keyword || !ex_desc->description || !*ex_desc->keyword || !*ex_desc->description)
    {
      new_mudlog(BRF, LVL_IMMORT, TRUE, "SYSERR: save_one_item: Corrupt ex_desc!");
      continue;
    }
    write_extra_desc(fl, ex_desc);
  }
  return fprintf(fl, "$\n");
}

int write_extra_desc(FILE *fl, struct extra_descr_data *ex_desc)
{
  char buf[MAX_STRING_LENGTH];
  return fprintf(fl, "E\n"
                 "%s~\n"
                 "%s~\n", ex_desc->keyword, remove_cr(ex_desc->description, buf, sizeof(buf)));
}

char *remove_cr(char *str, char *buf, size_t len)
{
  strncpy(buf, str, len - 1);
  strip_cr(buf);
  return buf;
}
void read_extra_descs(FILE *fl, OBJ_DATA *temp)
{
  struct extra_descr_data *new_descr;
  char line[READ_SIZE], buf2[MAX_INPUT_LENGTH];
  int zwei;
  FILE *tf = fl;

  temp->ex_description = NULL;

  get_line(fl, line);
  for (zwei = 0; !zwei && !feof(fl);)
  {
    switch (*line)
    {
    case 'E':
      CREATE(new_descr, struct extra_descr_data, 1);
      if ((new_descr->keyword = fread_string(fl, buf2)) == NULL)
        new_descr->keyword = strdup("Undefined");
      if ((new_descr->description = fread_string(fl, buf2)) == NULL)
        new_descr->description = strdup("Undefined");
      new_descr->next = temp->ex_description;
      temp->ex_description = new_descr;
      tf = fl;
      get_line(fl, line);
      break;
    case '$':
    case '#':
    case '@':
      zwei = 1;
      break;
    default:
      zwei = 1;
      break;
    }
  }
  fl = tf;
}
void read_object_affects(FILE *fl, OBJ_DATA *temp)
{
  FILE *tf = fl;
  char line[READ_SIZE];
  int  j, zwei;
  int t[2];


  get_line(fl, line);
  for (j = zwei = 0; !zwei && !feof(fl);)
  {
    switch (*line)
    {
    case 'A':
      if (j >= MAX_OBJ_AFFECT)
      {
        log("Too many object affectations in loading rent file");
        break;
      }
      get_line(fl, line);
      sscanf(line, "%d %d", t, t + 1);

      temp->affected[j].location = t[0];
      temp->affected[j].modifier = t[1];
      j++;
      tf = fl;
      get_line(fl, line);
      break;

    case '$':
    case '#':
    case '@':
      zwei = 1;
      break;
    default:
      zwei = 1;
      break;
    }
  }
  fl = tf;
}

int load_objects_to_room(room_rnum rnum, FILE *fl)
{
  int num_objs = 0;
  int j;
  int locate = 0;
  struct obj_data *temp;
  struct obj_data *cont_row[MAX_BAG_ROW];


  for (j = 0; j < MAX_BAG_ROW; j++)
    cont_row[j] = NULL;  /* empty the containers */

  while (!feof(fl))
  {
    temp = NULL;
    temp = read_one_item(fl, temp, &locate);
    if (temp != NULL)
    {
      num_objs++;
      generate_weapon(temp);
      obj_to_room(temp, rnum);
      relocate_obj(rnum, NULL, temp, locate, cont_row);
      if ( IS_OBJ_STAT (temp, ITEM_ARTIFACT))
         pause_timer(temp);
      else
	 check_timer(temp);
    }
  }

  log("Has %d saved objects.", num_objs);

  return 1;
}

void new_load_corpses(void)
{
  FILE *fl;
  OBJ_DATA *obj, *next_obj, *temp;
  int num_objs = 0, locate;
  room_rnum rnum = real_room(1202);

  if (!(fl = fopen(NEW_CORPSE_FILE, "r+b")))
  {
    if (errno != ENOENT)
    {
      log("SYSERR: READING CORPSE FILE %s in new_load_corpses",  NEW_CORPSE_FILE);
    }
    return;
  }

  while (!feof(fl))
  {
    temp = NULL;
    temp = read_one_item(fl, temp, &locate);
    if (temp != NULL)
    {
      num_objs++;
      /* Check if our object is a corpse */
      if (IS_OBJ_STAT(temp, ITEM_PC_CORPSE))
      {   /* scan our temp room for objects */
        add_corpse_to_list(temp);
        for (obj = rnum->contents; obj; obj = next_obj)
        {
          next_obj = obj->next_content;
          if (obj && !IS_CORPSE(obj))
          {
            obj_from_room(obj);    /* get those objs from that room */
            obj_to_obj(obj, temp); /* and put them in the corpse */
          }
        }      /* exit the room scanning loop */
        if (temp)
        { /* put the corpse in the right room */
          log("CORPSE: Corpse '%s' to room %d.",temp->short_description, GET_OBJ_VROOM(temp));
          obj_to_room(temp, IN_ROOM(temp) ? IN_ROOM(temp) : real_room(3001));
        }
      }
      else
      {
        /* just a plain obj..send it to a temp room until we load a corpse */
        if (temp)
          obj_to_room(temp, rnum);
      }
    }

  }
  fclose(fl);

}

/*doesnt handle containers so don't use them - mord */
OBJ_DATA * load_objects_to_list(FILE *fl)
{
  int locate;
  OBJ_DATA *list = NULL, *temp;
  if (feof(fl))
    return NULL;

  while (!feof(fl))
  {
    temp = NULL;
    temp = read_one_item(fl, temp, &locate);

    if (temp)
    {
      temp->next_content = list;
      list = temp;
    }
  }

  return list;

}

void scan_char_objects_qic(char *name, long id)
{
  FILE *fl;
  int objnum, nr, j;
  char fname[MAX_STRING_LENGTH], line[READ_SIZE];

  if (!get_filename(name, fname, ASCII_OBJ_FILES))
    return;

  if (!(fl = fopen(fname, "r+b")))
  {
    if (errno != ENOENT)      /* if it fails, NOT because of no file */
      log( "SYSERR: OPENING OBJECT FILE %s (4)", fname);

    return;
  }

  while (!feof(fl))
  {
    do
    {
      get_line(fl, line);
    }
    while (!feof(fl) && *line != '#');
    if (feof(fl))
    {
      fclose(fl);
      return;
    }
    if ((sscanf(line, "#%d OBJ", &nr)) == 1)
    {

      if (nr != NOTHING)
      {
        for (j = 0; j < qic_items; j++)
        {
          if (nr == qic_vnums[j])
          {
            if ((objnum = real_object(nr)) >= 0)
            {
              obj_index[objnum].qic->items++;
              log("Vnum: %5d, Owner: %10s, Count: %d", nr, name, obj_index[objnum].qic->items);
              add_owner(objnum, id);
            }
          }
        }


      }
    }
  }

  fclose(fl);
  return;
}

int load_char_objects_to_char(Character *ch, FILE * fl)
{
  //FILE *fl;
  int num_objs = 0;
  int j;
  int locate = 0;
  struct obj_data *temp;
  struct obj_data *cont_row[MAX_BAG_ROW];


  if (feof(fl))
    return 1;

  for (j = 0; j < MAX_BAG_ROW; j++)
    cont_row[j] = NULL;  /* empty all cont lists (you never know ...) */

  while (!feof(fl))
  {
    temp = NULL;
    temp = read_one_item(fl, temp, &locate);
    if (temp != NULL)
    {
      num_objs++;
      generate_weapon(temp);
      auto_equip(ch, temp, locate);
      relocate_obj(0, ch, temp, locate, cont_row);
    }
  }

  /* Little hoarding check. -gg 3/1/98 */
  if (ch->loader == NOBODY)
  {
    new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "  -- with %d items on character", num_objs);
  }


  return 0;
}

void tag_read(char *tag, char *buf)
{

  char *tmp = buf, *ttag = tag, *wrt = buf;

  while ((*tmp != ':' && *tmp != ' ' && *tmp != '\0'))
    *(ttag++) = *(tmp++);
  *ttag = '\0';

  while (*tmp == ':' || *tmp == ' ')
    tmp++;

  while (*tmp)
    *(wrt++) = *(tmp++);
  *wrt = '\0';
}
#define fread_new_string(old, fl) {  \
  if ((temp->old != NULL) && (temp->old != obj_proto[nrr].old)) \
  free(temp->old); \
  if ((temp->old = fread_string(fl, buf)) == NULL) \
    temp->(old) = strdup("Undefined"); }

#define ADD_NEW(str, line) {if (temp->str != NULL && temp->str != obj_proto[nrr].str) \
      free(temp->str); \
      str = strdup(line); \
      }

struct obj_data * read_one_item(FILE *fl, OBJ_DATA *temp, int *locate)
{
  int rv, aff_i = 0;
  obj_vnum nr, nrr = 0; // onr
  char buf[MAX_INPUT_LENGTH];
  char line[READ_SIZE] = "";
  char tag[READ_SIZE] = "";
  //int num;
  int t[4];
  double tf[1];
  int orig_timer = -1;
  //int orig_expir = -1;
  //int dup_strings = FALSE;
  //struct ident_list *tmp_idents = NULL;
  //int tmp_wep_bal;
  bool weight_read = FALSE;

  if (feof(fl))
    return NULL;

  do
  {
    get_line(fl, line);
  }
  while (!feof(fl) && *line != '#');

  if (feof(fl))
    return NULL;
  if ((rv = sscanf(line, "#%d OBJ", &nr)) != 1)
    return NULL;

  //onr = nr;
  /* we have the number, check it, load obj. */
  if (nr == NOTHING)
  {  /* then it is unique */
    temp = create_obj(NOTHING);
  }
  else if (nr < NOTHING)
  {
    log("read_one_item : Loading item that has negitive vnum: %d", nr);
    return NULL;
  }
  else
  {
    temp = read_object(nr, VIRTUAL);

    if (nr >= 999999 || !temp)
    {
      return NULL;
    }
    nrr = real_object(nr);
    if (nrr != NOTHING && obj_index[nrr].qic != NULL)
    {
      /*already added from scan rent*/
      obj_index[nrr].qic->items--;
    }

    //dup_strings = TRUE;
  }
  get_line(fl, line);
  while (!feof(fl) && strcmp(line, "@END"))
  {
    tag_read(tag, line);
    //num = atoi(line);
    switch (LOWER(*tag))
    {
    case 'a':
      if (!strcmp(tag, "ActionD"))
      {snprintf(buf, sizeof(buf), "%s on object %d, in read one item (%s)",tag,  nr, line);
        if ((temp->action_description = fread_string(fl, buf)) == NULL)
          temp->action_description = strdup("Undefined");
      }
      else if (!strcmp(tag, "Affects"))
        read_object_affects(fl, temp);
      break;
    case 'c':
      if (!strcmp(tag, "Cost"))
        GET_OBJ_COST(temp) = atoi(line);
      break;
    case 'd':
      if (!strcmp(tag, "Description"))
      {snprintf(buf, sizeof(buf), "%s on object %d, in read one item (%s)",tag,  nr, line);
        if ((temp->description = fread_string(fl, buf)) == NULL)
          temp->description = strdup("Undefined");
      }
      break;
    case 'e':
      if (!strcmp(tag, "Extra"))
        sscanf(line, "%d %d %d %d", &GET_OBJ_EXTRA(temp)[0], &GET_OBJ_EXTRA(temp)[1],  &GET_OBJ_EXTRA(temp)[2], &GET_OBJ_EXTRA(temp)[3]);
      else if (!strcmp(tag, "ExDescr"))
        read_extra_descs(fl, temp);
      else if (!strcmp(tag, "Expir")) {
        GET_OBJ_EXPIRE(temp) = atol(line);
        //orig_expir = GET_OBJ_EXPIRE(temp);
      }
      else if (!strcmp(tag, "Exrem")) {
	int remaining = atol(line);
	if (remaining != 0) {
	  GET_OBJ_EXPIRE(temp) = time(0) + remaining;
	  //orig_expir = GET_OBJ_EXPIRE(temp);
	}
      }
      break;
    case 'f':
      if (!strcmp(tag, "Feel"))
      {snprintf(buf, sizeof(buf), "%s on object %d, in read one item (%s)",tag,  nr, line);
        if ((temp->feel = fread_string(fl, buf)) == NULL)
          temp->feel = strdup("Undefined");
      }
      break;
    case 'i':
      if (!strcmp(tag, "Innate"))
        GET_OBJ_INNATE(temp) = atoi(line);
      if (!strcmp(tag, "Idents"))
      {
        do
        {
          get_line(fl, line);
          if (sscanf(line, "%d", t) != 1)
            break;
          if (t[0] == -1)
            break;
          add_identifier(temp, t[0]);
        }
        while (*line != '-');
        //if (temp->idents)
        //    tmp_idents = temp->idents;
      }
      break;
    case 'l':
      if (!strcmp(tag, "Level"))
        GET_OBJ_LEVEL(temp) = atoi(line);
      else if (!strcmp(tag, "Location"))
        *locate = atoi(line);
      break;
    case 'n':
      if (!strcmp(tag, "Name"))
      {
        temp->name = strdup(line);
      }
      break;
    case 'o':
      if ( !strcmp ( tag, "OrigAff" ) )
      {
	sscanf ( line, "%d %d", &t[0], &t[1] );
        temp->orig_affected[ aff_i ].location = t[0];
        temp->orig_affected[ aff_i ].modifier = t[1];
        aff_i++;
      }
      else if (!strcmp(tag, "Owner"))
        temp->owner = atol(line);
      break;
    case 'p':
      if (!strcmp(tag, "Perm"))
        sscanf(line, "%d %d %d %d", &GET_OBJ_PERM(temp)[0], &GET_OBJ_PERM(temp)[1],  &GET_OBJ_PERM(temp)[2], &GET_OBJ_PERM(temp)[3]);
      break;
    case 'r':
      if (!strcmp(tag, "Rent"))
        GET_OBJ_RENT(temp) = atoi(line);
      break;
    case 's':
      if (!strcmp(tag, "Short"))
      {
      snprintf(buf, sizeof(buf), "%s on object %d, in read one item (%s)",tag,  nr, line);
        if ((temp->short_description = fread_string(fl, buf)) == NULL)
          temp->short_description = strdup("Undefined");
      }
      else if (!strcmp(tag, "Smell"))
      {
      snprintf(buf, sizeof(buf), "%s on object %d, in read one item (%s)",tag,  nr, line);
        if ((temp->smell = fread_string(fl, buf)) == NULL)
          temp->smell = strdup("Undefined");
      }
      break;
    case 't':
      if (!strcmp(tag, "Timer")) {
        orig_timer = atoi(line);
        GET_OBJ_TIMER(temp) = orig_timer;
      }
      else if (!strcmp(tag, "Type"))
        GET_OBJ_TYPE(temp) = atoi(line);
      else if (!strcmp(tag, "Taste"))
      {
      snprintf(buf, sizeof(buf), "%s on object %d, in read one item (%s)",tag,  nr, line);
        if ((temp->taste = fread_string(fl, buf)) == NULL)
          temp->taste = strdup("Undefined");
      }
      break;
    case 'v':
      if (!strcmp(tag, "Vroom"))
        GET_OBJ_VROOM(temp) = atoi(line);
      else if (!strcmp(tag, "Values"))
      {
        for (t[0] = 0; t[0] < NUM_OBJ_VAL_POSITIONS; t[0]++)
          GET_OBJ_VAL(temp, t[0]) = 0;
        for (t[0] = 0; t[0] < NUM_OBJ_FLOATING_VAL_POSITIONS; t[0]++)
          GET_OBJ_FLOATING_VAL(temp, t[0]) = 0;
        while (TRUE)
        {
          get_line(fl, line);
          if (*line == '$')
            break;
          if (sscanf(line, "%d", t) != 1)
            continue;
          if (t[0] < 8)
          {
            if (sscanf(line, "%d %d", t, t+1) != 2)
              continue;
            GET_OBJ_VAL(temp, t[0]) = t[1];
          }
          else if (t[0] == 8)
          {
            if (sscanf(line, "%d %lf", t, tf) != 2)
              continue;
            GET_OBJ_QUALITY(temp) = tf[0];
          }
          else if (t[0] < 14)
          {
            if (sscanf(line, "%d %d", t, t+1) != 2)
              continue;
            GET_OBJ_VAL(temp, t[0] - 1) = t[1];
          }
          else if (t[0] == 14)
          {
            if (sscanf(line, "%d %lf", t, tf) != 2)
              continue;
            GET_OBJ_MAX_QUALITY(temp) = tf[0];
          }
        }
      }
      break;
    case 'w':
      if (!strcmp(tag, "Wear"))
        sscanf(line, "%d %d %d %d", &GET_OBJ_WEAR(temp)[0], &GET_OBJ_WEAR(temp)[1],  &GET_OBJ_WEAR(temp)[2], &GET_OBJ_WEAR(temp)[3]);
      else if (!strcmp(tag, "Weight"))
      {
        GET_OBJ_WEIGHT(temp) = atoi(line);
        weight_read = TRUE;
      }
      break;

    default:
      log("read_one_item: unknown tag (%s) line (%s).", tag, line);

      break;
    }


    get_line(fl, line);
  }

  if ( GET_OBJ_TYPE ( temp ) == ITEM_DRINKCON && !weight_read )
	GET_OBJ_WEIGHT ( temp ) = MAX ( 0, GET_OBJ_WEIGHT ( &obj_proto[nrr] ) - GET_OBJ_VAL ( &obj_proto[nrr], 1 ) + GET_OBJ_VAL ( temp, 1 ) );

  /* Horus - all eq will be updated automatically */
/*  if (nr > NOTHING && ((!IS_SET_AR(GET_OBJ_EXTRA(temp), ITEM_UNIQUE_SAVE) && !IS_SET_AR(GET_OBJ_EXTRA(temp), ITEM_TINKERED)) || isname_full("perz", temp->name) ))  {
      ubyte dt_save = 0;
      if (IS_SET_AR(GET_OBJ_EXTRA(temp), ITEM_ANTI_DT))
          dt_save = 1;

      if (tmp_idents) 
          temp->idents = NULL;
      tmp_wep_bal = GET_OBJ_VAL(temp, 5); 
      free_obj(temp, FALSE);
      temp = NULL;
      temp = read_object(onr, VIRTUAL);
      if (tmp_idents)
          temp->idents = tmp_idents;
      if (orig_timer != -1)
          GET_OBJ_TIMER(temp) = orig_timer;
      if (orig_expir != -1)
          GET_OBJ_EXPIRE(temp) = orig_expir;
      if (GET_OBJ_TYPE(temp) == ITEM_WEAPON)
          GET_OBJ_VAL(temp, 5) = tmp_wep_bal;

      if (dt_save)
	  SET_BIT_AR(GET_OBJ_EXTRA(temp), ITEM_ANTI_DT);

  }
*/
  return temp;
}

int Crash_load_xapobjs(Character *ch)
{
  char fname1[MAX_STRING_LENGTH], fname2[MAX_STRING_LENGTH];
  FILE *fl;
  int retval = 1;

  get_filename(GET_NAME(ch), fname1, ASCII_OBJ_FILES);
  get_filename(GET_NAME(ch), fname2, NEW_OBJ_FILES);

  if (!(fl = fopen(fname1, "r+b")))
  {
    if (errno != ENOENT)
    {     /* if it fails, NOT because of no file */
      new_mudlog( NRM, GET_LEVEL(ch), TRUE, "Error reading obj file: %s", fname1);
      log("SYSERR: READING OBJECT FILE %s (5)", fname1);
      ch->Send("\r\n********************* NOTICE *********************\r\n"
                       "There was a problem loading your objects from disk.\r\n"
                       "Contact a God for assistance.\r\n");
    }
    if (!(fl = fopen(fname2, "r+b")))
    {
      if (errno != ENOENT)
      {   /* if it fails, NOT because of no file */

        new_mudlog( NRM, GET_LEVEL(ch), TRUE, "Error reading obj file: %s", fname1);
        log("SYSERR: READING OBJECT FILE %s (5)", fname1);
        ch->Send("\r\n********************* NOTICE *********************\r\n"
                         "There was a problem loading your objects from disk.\r\n"
                         "Contact a God for assistance.\r\n");
      }

    }
    else
    {

      new_mudlog( NRM, GET_LEVEL(ch), TRUE, "%s loading eq from OLD type file, probably bad. Let Mord know.", GET_NAME(ch));
      retval = load_char_objects_to_char_old(ch, fl);
      fclose(fl);
    }
  }
  else
  {
    retval = load_char_objects_to_char(ch, fl);
    fclose(fl);
  }


  return retval;
}

int load_char_objects_to_char_old(Character *ch, FILE * fl)
{

  //FILE *fl;
  int t[20], zwei = 0;     //,num_of_days, cost;
  int orig_rent_code;
  char line[1024];
  OBJ_DATA *temp;
  int locate = 0, j, nr, num_objs = 0;
  struct obj_data *cont_row[MAX_BAG_ROW];
  struct extra_descr_data *new_descr;
  int rentcode, timed, netcost, gold, account, nitems;
  char buf2[MAX_INPUT_LENGTH];
  int retval, rv;





  if (!feof(fl))
    get_line(fl, line);

  sscanf(line, "%d %d %d %d %d %d", &rentcode, &timed,
         &netcost, &gold, &account, &nitems);

  switch (orig_rent_code = rentcode)
  {
  case RENT_RENTED:
    new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s un-renting and entering game.", GET_NAME(ch));
    break;
  case RENT_CRASH:
    new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s retrieving crash-saved items and entering game.",
               GET_NAME(ch));
    break;
  case RENT_CRYO:
    new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s un-cryo'ing and entering game.", GET_NAME(ch));
    break;
  case RENT_FORCED:
  case RENT_TIMEDOUT:
    new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s retrieving force-saved items and entering game.",
               GET_NAME(ch));
    break;
  default:
    new_mudlog(BRF, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "WARNING: %s entering game with undefined rent code.",
               GET_NAME(ch));
    break;
  }

  for (j = 0; j < MAX_BAG_ROW; j++)
    cont_row[j] = NULL;  /* empty all cont lists (you never know ...) */

  if (!feof(fl))
    get_line(fl, line);
  while (!feof(fl))
  {
    temp = NULL;
    /* first, we get the number. Not too hard. */
    if (*line == '#')
    {
      if ((rv = sscanf(line, "#%d", &nr)) != 1)
      {
        continue;
      }
      /* we have the number, check it, load obj. */
      if (nr == NOTHING)
      {   /* then it is unique */
        temp = create_obj(NOTHING);
      }
      else if (nr < NOTHING)
      {
        continue;
      }
      else
      {
        temp = read_object(nr, VIRTUAL);
        if (nr >= 999999 || !temp)
        {
          continue;
        }
      }

      get_line(fl, line);
      retval = sscanf(line, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                      t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6, t + 7,
                      t + 8, t + 9, t + 10, t + 11, t + 12, t + 13, t + 14, t + 15, t + 16);
      locate = t[0];
      if (retval == 10)
      {
        GET_OBJ_VAL(temp, 0) = t[1];
        GET_OBJ_VAL(temp, 1) = t[2];
        GET_OBJ_VAL(temp, 2) = t[3];
        GET_OBJ_VAL(temp, 3) = t[4];
        GET_OBJ_EXTRA(temp)[0] = t[5];
        GET_OBJ_EXTRA(temp)[1] = t[6];
        GET_OBJ_EXTRA(temp)[2] = t[7];
        GET_OBJ_EXTRA(temp)[3] = t[8];
        GET_OBJ_TIMER(temp) = t[9];
        /*---unknown----*/
        GET_OBJ_VAL(temp, 4) = 0;
        GET_OBJ_VAL(temp, 5) = 0;
        GET_OBJ_VAL(temp, 6) = 0;
        GET_OBJ_VAL(temp, 7) = 0;
        GET_OBJ_VAL(temp, 8) = 0;
        GET_OBJ_VAL(temp, 9) = 0;
      }
      else
      {
        GET_OBJ_VAL(temp, 0) = t[1];
        GET_OBJ_VAL(temp, 1) = t[2];
        GET_OBJ_VAL(temp, 2) = t[3];
        GET_OBJ_VAL(temp, 3) = t[4];
        GET_OBJ_VAL(temp, 4) = t[5];
        GET_OBJ_VAL(temp, 5) = t[6];
        GET_OBJ_VAL(temp, 6) = t[7];
        GET_OBJ_VAL(temp, 7) = t[8];
        GET_OBJ_VAL(temp, 8) = t[9];
        GET_OBJ_VAL(temp, 9) = t[10];
        GET_OBJ_EXTRA(temp)[0] = t[11];
        GET_OBJ_EXTRA(temp)[1] = t[12];
        GET_OBJ_EXTRA(temp)[2] = t[13];
        GET_OBJ_EXTRA(temp)[3] = t[14];
        GET_OBJ_TIMER(temp) = t[15];
        GET_OBJ_INNATE(temp) = t[16];
      }


      get_line(fl, line);
      /* read line check for xap. */
      if (!strcasecmp("XAP", line))
      {   /* then this is a Xap Obj, requires
                                                                                                     special care */
        if ((temp->name = fread_string(fl, buf2)) == NULL)
        {
          temp->name = (char *)"undefined";
        }

        if ((temp->short_description =
               fread_string(fl, buf2)) == NULL)
        {
          temp->short_description = (char *)"undefined";
        }

        if ((temp->description = fread_string(fl, buf2)) == NULL)
        {
          temp->description = (char *)"undefined";
        }

        if ((temp->action_description =
               fread_string(fl, buf2)) == NULL)
        {
          temp->action_description = 0;
        }
        /*
          if ((temp->smell = fread_string(fl, buf2)) == NULL)
            temp->smell = 0;
         
          if ((temp->taste = fread_string(fl, buf2)) == NULL)
            temp->taste = 0;
         
          if ((temp->feel = fread_string(fl, buf2)) == NULL)
            temp->feel = 0;
        */
        if (!get_line(fl, line) ||
            (sscanf(line, "%d %d %d %d %d %d %d %d",
                    t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6,
                    t + 7) != 8))
        {
          log("Format error in first numeric line of %s.new (expecting 8 args)", GET_NAME(ch));

          return 0;
        }
        temp->obj_flags.type_flag = t[0];
        temp->obj_flags.wear_flags[0] = t[1];
        temp->obj_flags.wear_flags[1] = t[2];
        temp->obj_flags.wear_flags[2] = t[3];
        temp->obj_flags.wear_flags[3] = t[4];
        temp->obj_flags.weight = t[5];
        temp->obj_flags.cost = t[6];
        temp->obj_flags.cost_per_day = t[7];



        /* we're clearing these for good luck */

        for (j = 0; j < MAX_OBJ_AFFECT; j++)
        {
          temp->affected[j].location = APPLY_NONE;
          temp->affected[j].modifier = 0;
        }

        /* You have to null out the extradesc when you are parsing a xap_obj.
           This is done right before the extradesc is read. */


        temp->ex_description = NULL;

        get_line(fl, line);
        for (j = zwei = 0; !zwei && !feof(fl);)
        {
          switch (*line)
          {
          case 'E':
            CREATE(new_descr, struct extra_descr_data, 1);
            if ((new_descr->keyword = fread_string(fl, buf2)) == NULL)
              new_descr->keyword = strdup("Undefined");
            if ((new_descr->description = fread_string(fl, buf2)) == NULL)
              new_descr->description = strdup("Undefined");
            new_descr->next = temp->ex_description;
            temp->ex_description = new_descr;
            get_line(fl, line);
            break;
          case 'A':
            if (j >= MAX_OBJ_AFFECT)
            {
              log("Too many object affectations in loading rent file");
              //danger = 1;
            }
            get_line(fl, line);
            sscanf(line, "%d %d", t, t + 1);

            temp->affected[j].location = t[0];
            temp->affected[j].modifier = t[1];
            j++;
            get_line(fl, line);
            break;

          case '$':
          case '#':
            zwei = 1;
            break;
          default:
            zwei = 1;
            break;
          }
        }      /* exit our for loop */
      }             /* exit our xap loop */

      if (temp != NULL)
      {
        num_objs++;
        generate_weapon(temp);
        auto_equip(ch, temp, locate);
      }
      else
      {
        continue;
      }
      relocate_obj(0, ch, temp, locate, cont_row);

    }
  }

  /* Little hoarding check. -gg 3/1/98 */
  new_mudlog(NRM, MAX(LVL_GOD, GET_INVIS_LEV(ch)), TRUE, "%s (level %d) has %d objects.",
             GET_NAME(ch), GET_LEVEL(ch), num_objs);



  if ((orig_rent_code == RENT_RENTED) || (orig_rent_code == RENT_CRYO))
    return 0;
  else
    return 0;//1;
}



