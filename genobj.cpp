/************************************************************************
 * Generic OLC Library - Objects / genobj.c			v1.0	*
 * Original author: Levork						*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "db.h"
#include "boards.h"
#include "shop.h"
#include "genolc.h"
#include "genobj.h"
#include "genzon.h"
#include "dg_olc.h"
#include "constants.h"
//#include "htree.h"

static int copy_object_main(struct obj_data *to, struct obj_data *from, int free_object);
void purge_qic(int rnum);

extern struct board_info_type board_info[];

obj_rnum add_object(struct obj_data *newobj, obj_vnum ovnum)
{
  int found = NOTHING;
  zone_rnum rznum = real_zone_by_thing(ovnum);

  /*
   * Write object to internal tables.
   */
  if ((newobj->item_number = real_object(ovnum)) != NOTHING) {
    copy_object(&obj_proto[newobj->item_number], newobj);
    update_objects(&obj_proto[newobj->item_number]);
    add_to_save_list(zone_table[rznum].number, SL_OBJ);
    return newobj->item_number;
  }

  found = insert_object(newobj, ovnum);
  adjust_objects(found);
  add_to_save_list(zone_table[rznum].number, SL_OBJ);
  return found;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */

/*
 * Fix all existing objects to have these values.
 * We need to run through each and every object currently in the
 * game to see which ones are pointing to this prototype.
 * if object is pointing to this prototype, then we need to replace it
 * with the new one.
 */
int update_objects(struct obj_data *refobj)
{
  struct obj_data *obj, swap;
  int count = 0;

  for (olt_it ob = object_list.begin(); ob != object_list.end(); ob++) {
	  obj = (ob->second);
    if (obj->item_number != refobj->item_number)
      continue;

    count++;

    /* Update the existing object but save a copy for private information. */
    swap = *obj;
    *obj = *refobj;

    /* Copy game-time dependent variables over. */
    GET_ID(obj) = GET_ID(&swap);
    IN_ROOM(obj) = swap.in_room;
    obj->carried_by = swap.carried_by;
    obj->in_locker = swap.in_locker;
    obj->worn_by = swap.worn_by;
    obj->worn_on = swap.worn_on;
    obj->in_obj = swap.in_obj;
    obj->contains = swap.contains;
    obj->next_content = swap.next_content;
    obj->sitting_here = swap.sitting_here;
  }

  return count;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */

/*
 * Adjust the internal values of other objects as if something was inserted at the given array index.
 * Might also be useful to make 'holes' in the array for some reason.
 */
obj_rnum adjust_objects(obj_rnum refpt)
{
  int shop, i, zone, cmd_no;

#if CIRCLE_UNSIGNED_INDEX
  if (refpt == NOTHING || refpt > top_of_objt)
#else
  if (refpt < 0 || refpt > top_of_objt)
#endif
    return NOTHING;

  /*
   * Renumber live objects.
   */
  for (olt_it ob = object_list.begin(); ob != object_list.end(); ob++)
	  GET_OBJ_RNUM((ob->second)) += (GET_OBJ_RNUM((ob->second)) >= refpt);

  /*
   * Renumber zone table.
   */
  for (zone = 0; zone <= top_of_zone_table; zone++) {
    for (cmd_no = 0; ZCMD(zone, cmd_no).command != 'S'; cmd_no++) {
      switch (ZCMD(zone, cmd_no).command) {
      case 'P':
        ZCMD(zone, cmd_no).arg3 += (ZCMD(zone, cmd_no).arg3 >= refpt);
         /*
          * No break here - drop into next case.
          */
      case 'O':
      case 'G':
      case 'E':
        ZCMD(zone, cmd_no).arg1 += (ZCMD(zone, cmd_no).arg1 >= refpt);
        break;
      case 'R':
        ZCMD(zone, cmd_no).arg2 += (ZCMD(zone, cmd_no).arg2 >= refpt);
        break;
      }
    }
  }

  /*
   * Renumber notice boards.
   */
  for (i = 0; i < NUM_OF_BOARDS; i++)
    BOARD_RNUM(i) += (BOARD_RNUM(i) >= refpt);

  /*
   * Renumber shop produce.
   */
  for (shop = 0; shop <= top_shop - top_shop_offset; shop++)
    for (i = 0; SHOP_PRODUCT(shop, i) != NOTHING; i++)
      SHOP_PRODUCT(shop, i) += (SHOP_PRODUCT(shop, i) >= refpt);

  return refpt;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */

/*
 * Function handle the insertion of an object within the prototype framework.  Note that this does not adjust internal values
 * of other objects, use add_object() for that.
 */
obj_rnum insert_object(struct obj_data *obj, obj_vnum ovnum)
{
  obj_rnum i;

  top_of_objt++;
  RECREATE(obj_index, struct index_data, top_of_objt + 1);
  RECREATE(obj_proto, struct obj_data, top_of_objt + 1);

  /*
   * Start counting through both tables.
   */
  for (i = top_of_objt; i > 0; i--) {
    /*
     * Check if current virtual is bigger than our virtual number.
     */
    if (ovnum > obj_index[i - 1].vnum)
      return index_object(obj, ovnum, i);

    /* Copy over the object that should be here. */
    obj_index[i] = obj_index[i - 1];
    obj_proto[i] = obj_proto[i - 1];
    obj_proto[i].item_number = i;

//    htree_add(obj_htree, obj_index[i].vnum, i);
    obj_vTor[obj_index[i].vnum]=i;
  }

  /* Not found, place at 0. */
  return index_object(obj, ovnum, 0);
}

/* ------------------------------------------------------------------------------------------------------------------------------ */

obj_rnum index_object(struct obj_data *obj, obj_vnum ovnum, obj_rnum ornum)
{
#if CIRCLE_UNSIGNED_INDEX
  if (obj == NULL || ornum == NOTHING || ornum > top_of_objt)
#else
  if (obj == NULL || ovnum < 0 || ornum < 0 || ornum > top_of_objt)
#endif
    return NOWHERE;

  obj->item_number = ornum;
  obj_index[ornum].vnum = ovnum;
  obj_index[ornum].number = 0;
  obj_index[ornum].func = NULL;

  copy_object_preserve(&obj_proto[ornum], obj);
  obj_proto[ornum].in_room = NULL;
//  htree_add(obj_htree, obj_index[ornum].vnum, ornum);
  obj_vTor[ovnum]=ornum;

  return ornum;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */

int save_objects(zone_rnum zone_num)
{
  char fname[128], buf[MAX_STRING_LENGTH];//, bit1[64], bit2[64];
  char smell[MAX_STRING_LENGTH], taste[MAX_STRING_LENGTH], feel[MAX_STRING_LENGTH];
  int counter, counter2, realcounter;
  FILE *fp;
  struct obj_data *obj;
  struct extra_descr_data *ex_desc;
  struct vehicle_attachment_data *att;

#if CIRCLE_UNSIGNED_INDEX
  if (zone_num == NOWHERE || zone_num > top_of_zone_table) {
#else
  if (zone_num < 0 || zone_num > top_of_zone_table) {
#endif
    log("SYSERR: GenOLC: save_objects: Invalid real zone number %d. (0-%d)", zone_num, top_of_zone_table);
    return FALSE;
  }

  snprintf(fname, sizeof(fname), "%s/%d/%d.new", LIB_WORLD, zone_table[zone_num].number, zone_table[zone_num].number);
  if (!(fp = fopen(fname, "w+"))) {
    new_mudlog(BRF, LVL_IMMORT, TRUE, "SYSERR: OLC: Cannot open objects file %s!", fname);
    return FALSE;
  }
  /*
   * Start running through all objects in this zone.
   */
  for (counter = zone_table[zone_num].Bot(); counter <= zone_table[zone_num].top; counter++) {
    if ((realcounter = real_object(counter)) != NOTHING) {
      if ((obj = &obj_proto[realcounter])->action_description) {
	strncpy(buf, obj->action_description, sizeof(buf) - 1);
	strip_cr(buf);
      } else
	*buf = '\0';
	if (obj && (obj)->smell) {
		strncpy(smell, obj->smell, sizeof(smell) - 1);
		strip_cr(smell);
	    } else
		*smell = '\0';
	    if (obj && (obj)->taste) {
		strncpy(taste, obj->taste, sizeof(taste) - 1);
		strip_cr(taste);
	    } else
		*taste = '\0';
	    if (obj && (obj)->feel) {
		strncpy(feel, obj->feel, sizeof(feel) - 1);
		strip_cr(feel);
	    } else
		*feel = '\0';

      fprintf(fp,
	      "#%d\n"
	      "%s~\n"
	      "%s~\n"
	      "%s~\n"
	      "%s~\n"
	      "%s~\n"
	      "%s~\n"
	      "%s~\n",

	      GET_OBJ_VNUM(obj),
	      (obj->name && *obj->name) ? obj->name : "undefined",
	      (obj->short_description && *obj->short_description) ? obj->short_description : "undefined",
	      (obj->description && *obj->description) ?	obj->description : "undefined",
	      buf, 
	      smell, 
	      taste, 
	      feel);

      //sprintascii(buf, GET_OBJ_EXTRA(obj));
      //sprintascii(bit1, GET_OBJ_WEAR(obj));
      //sprintascii(bit2, GET_OBJ_PERM(obj));

      fprintf(fp,
	      "%d %d %d %d %d %d %d %d %d\n"
	      "%d %d %d %d %d %d %d %d %lf %d %d %d %d %d %lf\n"
	      "%d %lld %d %d %d %d %d %d %d %d\n",

	      GET_OBJ_TYPE(obj), GET_OBJ_EXTRA(obj)[0], GET_OBJ_EXTRA(obj)[1],  GET_OBJ_EXTRA(obj)[2], GET_OBJ_EXTRA(obj)[3],
	      GET_OBJ_WEAR(obj)[0], GET_OBJ_WEAR(obj)[1],  GET_OBJ_WEAR(obj)[2], GET_OBJ_WEAR(obj)[3], /* row 1 */

	      GET_OBJ_VAL(obj, 0), GET_OBJ_VAL(obj, 1), GET_OBJ_VAL(obj, 2), GET_OBJ_VAL(obj, 3),
	      GET_OBJ_VAL(obj, 4), GET_OBJ_VAL(obj, 5), GET_OBJ_VAL(obj, 6), GET_OBJ_VAL(obj, 7),
	      GET_OBJ_FLOATING_VAL(obj, 0), GET_OBJ_VAL(obj, 8), GET_OBJ_VAL(obj, 9), GET_OBJ_VAL(obj, 10),
	      GET_OBJ_VAL(obj, 11), GET_OBJ_VAL(obj, 12), GET_OBJ_FLOATING_VAL(obj, 1), /* row 2 */

	      GET_OBJ_WEIGHT(obj), GET_OBJ_COST(obj), GET_OBJ_RENT(obj), GET_OBJ_INNATE(obj), GET_OBJ_TIMER(obj), GET_OBJ_LEVEL(obj),
	      GET_OBJ_PERM(obj)[0], GET_OBJ_PERM(obj)[1], GET_OBJ_PERM(obj)[2], GET_OBJ_PERM(obj)[3] /* row 3 */
	      
      );

      /*
       * Do we have script(s) attached ? 
       */
      script_save_to_disk(fp, obj, OBJ_TRIGGER);
      
      /*
       * Do we have extra descriptions? 
       */
      if (obj->ex_description) {	/* Yes, save them too. */
	for (ex_desc = obj->ex_description; ex_desc; ex_desc = ex_desc->next) {
	  /*
	   * Sanity check to prevent nasty protection faults.
	   */
	  if (!ex_desc->keyword || !ex_desc->description || !*ex_desc->keyword || !*ex_desc->description) {
	    new_mudlog(BRF, LVL_IMMORT, TRUE, "SYSERR: OLC: oedit_save_to_disk: Corrupt ex_desc!");
	    continue;
	  }
	  strncpy(buf, ex_desc->description, sizeof(buf) - 1);
	  strip_cr(buf);
	  fprintf(fp, "E\n"
		  "%s~\n"
		  "%s~\n", ex_desc->keyword, buf);
	}
      }
      /*
       * Do we have affects? 
       */
      for (counter2 = 0; counter2 < MAX_OBJ_AFFECT; counter2++)
	if (obj->affected[counter2].modifier)
	  fprintf(fp, "A\n"
		  "%d %d\n", obj->affected[counter2].location,
		  obj->affected[counter2].modifier);

      /* New vehicle attachments */
      for (att = obj->attachment; att; att = att->next) 
          fprintf(fp, "V\n"
                      "%d %d %d\n", att->type, att->value, att->max_value);
    }
  }

  /*
   * Write the final line, close the file.
   */
  fprintf(fp, "$~\n");
  fclose(fp);
  snprintf(buf, sizeof(buf), "%s/%d/%d.obj", LIB_WORLD, zone_table[zone_num].number, zone_table[zone_num].number);
  remove(buf);
  rename(fname, buf);

  if (in_save_list(zone_table[zone_num].number, SL_OBJ))
    remove_from_save_list(zone_table[zone_num].number, SL_OBJ);
  return TRUE;
}

/*
 * Free all, unconditionally.
 */
void free_object_strings(struct obj_data *obj)
{
#if 0 /* Debugging, do not enable. */
  struct obj_data *t;
  int i = 0;

  for (t = object_list; t; t = t->next) {
    if (t == obj) {
      i++;
      continue;
    }
    assert(obj->name != t->name);
    assert(obj->description != t->description);
    assert(obj->short_description != t->short_description);
    assert(obj->action_description != t->action_description);
    assert(obj->ex_description != t->ex_description);
  }
  assert(i <= 1);
#endif

  free_string(&obj->name);
  free_string(&obj->description);
  free_string(&obj->short_description);
  free_string(&obj->action_description);
  free_string(&obj->smell);
  free_string(&obj->taste);
  free_string(&obj->feel);
  if (obj->ex_description)
    free_ex_descriptions(obj->ex_description);
}

/*
 * For object instances that are not the prototype.
 */
void free_object_strings_proto(struct obj_data *obj)
{
  int robj_num = GET_OBJ_RNUM(obj);
  if ( obj->name && obj->name != obj_proto[robj_num].name)
  {
    free(obj->name);
    obj->name = NULL;
  }
  if ( obj->description && obj->description != obj_proto[robj_num].description)
  {
    free(obj->description);
    obj->description = NULL;
  }
  if (obj->short_description && obj->short_description != obj_proto[robj_num].short_description)
  {
    free(obj->short_description);
    obj->short_description = NULL;
  }
  if ( obj->action_description  && obj->action_description != obj_proto[robj_num].action_description)
  {
    free(obj->action_description);
    obj->action_description = NULL;
  }
  if (obj->smell  && obj->smell != obj_proto[robj_num].smell)
  {
    free(obj->smell);
    obj->smell = NULL;
  }
  if (obj->taste && obj->taste != obj_proto[robj_num].taste)
  {
    free(obj->taste);
    obj->taste = NULL;
  }
  if (obj->feel && obj->feel != obj_proto[robj_num].feel)
  {
    free(obj->feel);
    obj->feel = NULL;
  }
  if (obj->ex_description) {
    struct extra_descr_data *thised, *plist, *next_one; /* O(horrible) */
    int ok_key, ok_desc, ok_item;
    for (thised = obj->ex_description; thised; thised = next_one) {
      next_one = thised->next;
      for (ok_item = ok_key = ok_desc = 1, plist = obj_proto[robj_num].ex_description; plist; plist = plist->next) {
        if (plist->keyword == thised->keyword)
          ok_key = 0;
        if (plist->description == thised->description)
          ok_desc = 0;
        if (plist == thised)
          ok_item = 0;
      }
      if (thised->keyword && ok_key)
      {
        free(thised->keyword);
        thised->keyword = NULL;
      }
      if (thised->description && ok_desc)
      {
        free(thised->description);
        thised->description = NULL;
      }
      if (ok_item)
        free(thised);
    }
  
  }
}

void copy_object_strings(struct obj_data *to, struct obj_data *from)
{
  to->name = from->name ? strdup(from->name) : NULL;
  to->description = from->description ? strdup(from->description) : NULL;
  to->short_description = from->short_description ? strdup(from->short_description) : NULL;
  to->action_description = from->action_description ? strdup(from->action_description) : NULL;
  to->smell = from->smell ? strdup(from->smell) : NULL;
  to->feel = from->feel ? strdup(from->feel) : NULL;
  to->taste = from->taste ? strdup(from->taste) : NULL;

  if (from->ex_description)
    copy_ex_descriptions(&to->ex_description, from->ex_description);
  else
    to->ex_description = NULL;
}

int copy_object(struct obj_data *to, struct obj_data *from)
{
  free_object_strings(to);
  return copy_object_main(to, from, TRUE);
}

int copy_object_preserve(struct obj_data *to, struct obj_data *from)
{
  return copy_object_main(to, from, FALSE);
}

static int copy_object_main(struct obj_data *to, struct obj_data *from, int free_object)
{
  *to = *from;
  copy_object_strings(to, from);
  return TRUE;
}

const char *material_name(int type) {
	if (type < 0 || type > NUM_MATERIAL_TYPES)
		return "Etherial";

	return material_names[type];
}


