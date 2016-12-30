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

static int copy_object_main(struct obj_data *to, struct obj_data *from, int free_object);
void purge_qic(int rnum);

extern struct board_info_type board_info[];

obj_rnum add_object(struct obj_data *newobj, obj_vnum ovnum)
{
    zone_rnum rznum = real_zone_by_thing ( ovnum );
    obj_data *old_proto, *old_proto_descs;
    obj_rnum rn = real_object ( ovnum );

    /*
     * Write object to internal tables.
     */
    newobj->item_number = rn;
    if ( rn != NOTHING )
    {
        // old_proto has the same desc pointers as the old prototype
        CREATE ( old_proto, obj_data, 1 );
        *old_proto = obj_proto[rn];

        // old_proto_descs has the same descs as the old prototype
        CREATE ( old_proto_descs, obj_data, 1 );
        copy_object ( old_proto_descs, &obj_proto[rn] );

        // Update the old prototype to the new prototype
        if ( obj_proto[rn].proto_script )
            free_proto_script ( &obj_proto[rn], OBJ_TRIGGER );
        copy_object ( &obj_proto[rn], newobj );

        // Update existing objects
        update_objects ( old_proto, old_proto_descs, &obj_proto[rn] );

        free ( old_proto );
        free_object_strings ( old_proto_descs );
        free ( old_proto_descs );
        add_to_save_list ( zone_table[rznum].number, SL_OBJ );
        return rn;
    }

    int found = insert_object ( newobj, ovnum );
    adjust_objects ( found );
    add_to_save_list ( zone_table[rznum].number, SL_OBJ );
    return found;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */

/*
 * Only update the variables of existing objects to the
 * new prototype if they were equal to the old prototype
 */
void update_objects ( obj_data *old_proto, obj_data *old_proto_descs, obj_data *new_proto )
{
    struct obj_data *obj;
    int weight;

    for ( auto &ol : object_list )
    {
        obj = ol.second;
        if ( GET_OBJ_RNUM ( obj ) != GET_OBJ_RNUM ( new_proto ) )
            continue;

        /* type */
        if ( GET_OBJ_TYPE ( obj ) == GET_OBJ_TYPE ( old_proto ) )
            GET_OBJ_TYPE ( obj ) = GET_OBJ_TYPE ( new_proto );

        /* level */
        if ( GET_OBJ_LEVEL ( obj ) == GET_OBJ_LEVEL ( old_proto ) )
            GET_OBJ_LEVEL ( obj ) = GET_OBJ_LEVEL ( new_proto );

        /* wear flags */
        for ( int i = 0; i < TW_ARRAY_MAX; ++i )
            if ( GET_OBJ_WEAR ( obj )[i] == GET_OBJ_WEAR ( old_proto )[i] )
                GET_OBJ_WEAR ( obj )[i] = GET_OBJ_WEAR ( new_proto )[i];

        /* extra flags */
        for ( int i = 0; i < EF_ARRAY_MAX; ++i )
            if ( GET_OBJ_EXTRA ( obj )[i] == GET_OBJ_EXTRA ( old_proto )[i] )
                GET_OBJ_EXTRA ( obj )[i] = GET_OBJ_EXTRA ( new_proto )[i];

        /* weight */
        if ( GET_OBJ_TYPE ( obj ) == ITEM_CONTAINER )
        {
            weight = GET_OBJ_WEIGHT ( obj );
            for ( obj_data *o = obj->contains; o; o = o->next_content )
                weight -= GET_OBJ_WEIGHT ( o );

            if ( weight == GET_OBJ_WEIGHT ( old_proto ) )
                GET_OBJ_WEIGHT ( obj ) += GET_OBJ_WEIGHT ( new_proto ) - weight;
        }
        else if ( GET_OBJ_TYPE ( obj ) == ITEM_DRINKCON || GET_OBJ_TYPE ( obj ) == ITEM_FOUNTAIN )
        {
            // weight_container = weight - weight_contents
            if ( GET_OBJ_WEIGHT ( obj ) - GET_OBJ_VAL ( obj, 1 ) == GET_OBJ_WEIGHT ( old_proto ) - GET_OBJ_VAL ( old_proto, 1 ) )
                GET_OBJ_WEIGHT ( obj ) = GET_OBJ_WEIGHT ( new_proto ) - GET_OBJ_VAL ( new_proto, 1) + GET_OBJ_VAL ( obj, 1);
        }
        else if ( GET_OBJ_WEIGHT ( obj ) == GET_OBJ_WEIGHT ( old_proto ) )
            GET_OBJ_WEIGHT ( obj ) = GET_OBJ_WEIGHT ( new_proto );

        /* obj values, keep the crafting ones */
        for ( int i = 0; i <= 6; ++i )
            if ( GET_OBJ_VAL ( obj, i ) == GET_OBJ_VAL ( old_proto, i ) )
                GET_OBJ_VAL ( obj, i ) = GET_OBJ_VAL ( new_proto, i );

        /* cost */
        if ( GET_OBJ_COST ( obj ) == GET_OBJ_COST ( old_proto ) )
            GET_OBJ_COST ( obj ) = GET_OBJ_COST ( new_proto );

        /* cost per day */
        if ( GET_OBJ_RENT ( obj ) == GET_OBJ_RENT ( old_proto ) )
            GET_OBJ_RENT ( obj ) = GET_OBJ_RENT ( new_proto );

        /* timer */
        if ( GET_OBJ_TIMER ( obj ) == GET_OBJ_TIMER ( old_proto ) )
            GET_OBJ_TIMER ( obj ) = GET_OBJ_TIMER ( new_proto );

        /* perm */
        for ( int i = 0; i < AF_ARRAY_MAX; ++i )
            if ( GET_OBJ_PERM ( obj )[i] == GET_OBJ_PERM ( old_proto )[i] )
                GET_OBJ_PERM ( obj )[i] = GET_OBJ_PERM ( new_proto )[i];

        /* innate */
        if ( GET_OBJ_INNATE ( obj ) == GET_OBJ_INNATE ( old_proto ) )
            GET_OBJ_INNATE ( obj ) = GET_OBJ_INNATE ( new_proto );

        /* affects
         * don't update if the obj was crafted
         * only update if the affects were equal to the old prototype
         */
        if ( GET_OBJ_MAX_QUALITY ( obj ) < 0.001 )
        {
            bool equal_to_old = TRUE;
            for ( int i = 0; i < MAX_OBJ_AFFECT; ++i )
                if ( obj->affected[ i ].modifier != old_proto->affected[ i ].modifier
                    || obj->affected[ i ].location != old_proto->affected[ i ].location )
                    {
                        equal_to_old = FALSE;
                        break;
                    }

            if ( equal_to_old )
                for ( int i = 0; i < MAX_OBJ_AFFECT; ++i )
                {
                    obj->affected[ i ].location = new_proto->affected[ i ].location;
                    obj->affected[ i ].modifier = new_proto->affected[ i ].modifier;
                }
        }

        /* descs */
        if ( obj->name == old_proto->name )
            obj->name = new_proto->name;

        if ( obj->description == old_proto->description )
            obj->description = new_proto->description;

        if ( obj->smell == old_proto->smell )
            obj->smell = new_proto->smell;

        if ( obj->taste == old_proto->taste )
            obj->taste = new_proto->taste;

        if ( obj->feel == old_proto->feel )
            obj->feel = new_proto->feel;

        if ( obj->short_description == old_proto->short_description )
            obj->short_description = new_proto->short_description;

        if ( obj->action_description == old_proto->action_description )
            obj->action_description = new_proto->action_description;

        if ( obj->ex_description == old_proto->ex_description )
            obj->ex_description = new_proto->ex_description;
    }
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
                          "%d %d\n", obj->affected[counter2].location, obj->affected[counter2].modifier);

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
  if (obj->ex_description && obj->ex_description != obj_proto[robj_num].ex_description)
    free_ex_descriptions ( obj->ex_description );
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
        return "Ethereal";

    return material_names[type];
}


