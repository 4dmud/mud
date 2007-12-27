/************************************************************************
 * Generic OLC Library - Shops / genshp.c			v1.0	*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "db.h"
#include "shop.h"
#include "genolc.h"
#include "genshp.h"
#include "genzon.h"

extern struct shop_data *shop_index;
extern int top_shop;
void copy_list2(Room **tlist[], Room *flist[]);

/*
 * NOTE (gg): Didn't modify sedit much. Don't consider it as 'recent'
 * 	as the other editors with regard to updates or style.
 */

/*-------------------------------------------------------------------*/

void copy_shop(struct shop_data *tshop, struct shop_data *fshop, int free_old_strings)
{
  /*
   * Copy basic information over.
   */
  S_NUM(tshop) = S_NUM(fshop);
  S_KEEPER(tshop) = S_KEEPER(fshop);
  S_OPEN1(tshop) = S_OPEN1(fshop);
  S_CLOSE1(tshop) = S_CLOSE1(fshop);
  S_OPEN2(tshop) = S_OPEN2(fshop);
  S_CLOSE2(tshop) = S_CLOSE2(fshop);
  S_BANK(tshop) = S_BANK(fshop);
  S_BROKE_TEMPER(tshop) = S_BROKE_TEMPER(fshop);
  S_BITVECTOR(tshop) = S_BITVECTOR(fshop);
  S_NOTRADE(tshop) = S_NOTRADE(fshop);
  S_SORT(tshop) = S_SORT(fshop);
  S_BUYPROFIT(tshop) = S_BUYPROFIT(fshop);
  S_SELLPROFIT(tshop) = S_SELLPROFIT(fshop);
  S_FUNC(tshop) = S_FUNC(fshop);

  /*
   * Copy lists over.
   */
  copy_list2(&(S_ROOMS(tshop)), S_ROOMS(fshop));
  copy_list(&(S_PRODUCTS(tshop)), S_PRODUCTS(fshop));
  copy_type_list(&(tshop->type), fshop->type);

  /*
   * Copy notification strings over.
   */
   if (free_old_strings)
  free_shop_strings(tshop);
  S_NOITEM1(tshop) = str_udup(S_NOITEM1(fshop));
  S_NOITEM2(tshop) = str_udup(S_NOITEM2(fshop));
  S_NOCASH1(tshop) = str_udup(S_NOCASH1(fshop));
  S_NOCASH2(tshop) = str_udup(S_NOCASH2(fshop));
  S_NOBUY(tshop) = str_udup(S_NOBUY(fshop));
  S_BUY(tshop) = str_udup(S_BUY(fshop));
  S_SELL(tshop) = str_udup(S_SELL(fshop));
}

/*-------------------------------------------------------------------*/

/*
 * Copy a 'NOTHING' terminated integer array list.
 */
void copy_list(int **tlist, int *flist)
{
  int num_items, i;

  if (*tlist)
    free(*tlist);

  /*
   * Count number of entries.
   */
  for (i = 0; flist[i] != NOTHING; i++);
  num_items = i + 1;

  /*
   * Make space for entries.
   */
  CREATE(*tlist, int, num_items);

  /*
   * Copy entries over.
   */
  for (i = 0; i < num_items; i++)
    (*tlist)[i] = flist[i];
}


void copy_list2(Room **tlist[], Room *flist[])
{
  int num_items, i;

  if (*tlist)
    free(*tlist);

  /*
   * Count number of entries.
   */
  for (i = 0; flist[i] != NULL; i++);
  num_items = i + 1;

  /*
   * Make space for entries.
   */
  CREATE(*tlist, room_rnum, num_items);

  /*
   * Copy entries over.
   */
  for (i = 0; i < num_items; i++)
    (*tlist)[i] = flist[i];
}


void add_to_int_list(int **list, int newi)
{
  int i, num_items, *nlist;

  /*
   * Count number of entries.
   */
  for (i = 0; (*list)[i] != -1; i++);
  num_items = i;

  /*
   * Make a new list and slot in the new entry.
   */
  CREATE(nlist, int, num_items + 2);

  for (i = 0; i < num_items; i++)
    nlist[i] = (*list)[i];
  nlist[num_items] = newi;
  nlist[num_items + 1] = -1;

  /*
   * Out with the old, in with the new.
   */
  free(*list);
  *list = nlist;
}

/*-------------------------------------------------------------------*/

/*
 * Copy a -1 terminated (in the type field) shop_buy_data 
 * array list.
 */
void copy_type_list(struct shop_buy_data **tlist, struct shop_buy_data *flist)
{
  int num_items, i;

  if (*tlist)
    free_type_list(tlist);

  /*
   * Count number of entries.
   */
  for (i = 0; BUY_TYPE(flist[i]) != NOTHING; i++);
  num_items = i + 1;

  /*
   * Make space for entries.
   */
  CREATE(*tlist, struct shop_buy_data, num_items);

  /*
   * Copy entries over.
   */
  for (i = 0; i < num_items; i++) {
    (*tlist)[i].type = flist[i].type;
    if (BUY_WORD(flist[i]))
      BUY_WORD((*tlist)[i]) = strdup(BUY_WORD(flist[i]));
  }
}

/*-------------------------------------------------------------------*/

void remove_from_type_list(struct shop_buy_data **list, int num)
{
  int i, num_items;
  struct shop_buy_data *nlist;

  /*
   * Count number of entries.
   */
  for (i = 0; (*list)[i].type != NOTHING; i++);

  if (num < 0 || num >= i)
    return;
  num_items = i;

  CREATE(nlist, struct shop_buy_data, num_items);

  for (i = 0; i < num_items; i++)
    nlist[i] = (i < num) ? (*list)[i] : (*list)[i + 1];

  free(BUY_WORD((*list)[num]));
  free(*list);
  *list = nlist;
}

/*-------------------------------------------------------------------*/

void add_to_type_list(struct shop_buy_data **list, struct shop_buy_data *newl)
{
  int i, num_items;
  struct shop_buy_data *nlist;

  /*
   * Count number of entries.
   */
  for (i = 0; (*list)[i].type != NOTHING; i++);
  num_items = i;

  /*
   * Make a new list and slot in the new entry.
   */
  CREATE(nlist, struct shop_buy_data, num_items + 2);

  for (i = 0; i < num_items; i++)
    nlist[i] = (*list)[i];
  nlist[num_items] = *newl;
  nlist[num_items + 1].type = NOTHING;

  /*
   * Out with the old, in with the new.
   */
  free(*list);
  *list = nlist;
}

/*-------------------------------------------------------------------*/

void add_to_int_list2(Room **list[], room_rnum newi)
{
  int i, num_items;
  Room **nlist;

  /*
   * Count number of entries.
   */
  for (i = 0; (*list)[i] != NULL; i++);
  num_items = i;

  /*
   * Make a new list and slot in the new entry.
   */
  CREATE(nlist, room_rnum, num_items + 2);

  for (i = 0; i < num_items; i++)
    nlist[i] = (*list)[i];
  nlist[num_items] = newi;
  nlist[num_items + 1] = NULL;

  /*
   * Out with the old, in with the new.
   */
  free(*list);
  *list = nlist;
}

void remove_from_int_list2(Room **list[], room_rnum num)
{
  sh_int i, num_items;
  Room **nlist;

  /*
   * Count number of entries.
   */
  for (i = 0; (*list)[i] != NULL; i++);

  if (num->number >= i || num->number < 0)
    return;
  num_items = i;

  CREATE(nlist, room_rnum, num_items);

  for (i = 0; i < num_items; i++)
    nlist[i] = (i < num->number) ? (*list)[i] : (*list)[i + 1];

  free(*list);
  *list = nlist;
}
/*-------------------------------------------------------------------*/

void remove_from_int_list(int **list, int num)
{
  int i, num_items, *nlist;

  /*
   * Count number of entries.
   */
  for (i = 0; (*list)[i] != NOTHING; i++);

#if CIRCLE_UNSIGNED_INDEX
  if (num >= i)
#else
  if (num < 0 || num >= i)
#endif
    return;
  num_items = i;

  CREATE(nlist, int, num_items);

  for (i = 0; i < num_items; i++)
    nlist[i] = (i < num) ? (*list)[i] : (*list)[i + 1];

  free(*list);
  *list = nlist;
}

/*-------------------------------------------------------------------*/

/*
 * Free all the notice character strings in a shop structure.
 */
void free_shop_strings(struct shop_data *shop)
{
  if (S_NOITEM1(shop)) {
    free(S_NOITEM1(shop));
    S_NOITEM1(shop) = NULL;
  }
  if (S_NOITEM2(shop)) {
    free(S_NOITEM2(shop));
    S_NOITEM2(shop) = NULL;
  }
  if (S_NOCASH1(shop)) {
    free(S_NOCASH1(shop));
    S_NOCASH1(shop) = NULL;
  }
  if (S_NOCASH2(shop)) {
    free(S_NOCASH2(shop));
    S_NOCASH2(shop) = NULL;
  }
  if (S_NOBUY(shop)) {
    free(S_NOBUY(shop));
    S_NOBUY(shop) = NULL;
  }
  if (S_BUY(shop)) {
    free(S_BUY(shop));
    S_BUY(shop) = NULL;
  }
  if (S_SELL(shop)) {
    free(S_SELL(shop));
    S_SELL(shop) = NULL;
  }
}

/*-------------------------------------------------------------------*/

/*
 * Free a type list and all the strings it contains.
 */
void free_type_list(struct shop_buy_data **list)
{
  int i;

  for (i = 0; (*list)[i].type != NOTHING; i++)
    if (BUY_WORD((*list)[i]))
      free(BUY_WORD((*list)[i]));

  free(*list);
  *list = NULL;
}

/*-------------------------------------------------------------------*/

/*
 * Free up the whole shop structure and it's content.
 */
void free_shop(struct shop_data *shop)
{
  free_shop_strings(shop);
  free_type_list(&(S_NAMELISTS(shop)));
  free(S_ROOMS(shop));
  free(S_PRODUCTS(shop));
  free(shop);
}

/*-------------------------------------------------------------------*/

/* returns the real number of the shop with given virtual number 
 *
 * We take so good care to keep it sorted - let's use it :) - Welcor
 */
shop_rnum real_shop(shop_vnum vnum)
{

#if 0
  shop_rnum bot, top, mid;

  bot = 0;
  top = top_shop - top_shop_offset;

  /* perform binary search on shop_table */
  for (;;) {
    mid = (bot + top) / 2;

    if (SHOP_NUM(mid) == vnum)
      return (mid);
    if (bot >= top)
      return (NOWHERE);
    if (SHOP_NUM(mid) > vnum)
      top = mid;
    else
      bot = mid + 1;
  }
  #else
  int i;
  for (i = 0;i < top_shop - top_shop_offset;i++) {
  if (SHOP_NUM(i) == vnum)
  return (shop_rnum)i;
  }
  return NOWHERE;
  #endif
}

/*-------------------------------------------------------------------*/

/*
 * Generic string modifier for shop keeper messages.
 */
void modify_string(char **str, char *new_s)
{

  char buf[MAX_STRING_LENGTH];
  char *pointer;

  /*
   * Check the '%s' is present, if not, add it.
   */
  if (*new_s != '%') {
    snprintf(buf, sizeof(buf), "%%s %s", new_s);
    pointer = buf;
  } else
    pointer = new_s;

  if (*str)
    free(*str);
  *str = strdup(pointer);
}

/*-------------------------------------------------------------------*/

int add_shop(struct shop_data *nshp)
{
  shop_rnum rshop;
  int found = 0;
  zone_rnum rznum = real_zone_by_thing(S_NUM(nshp));

  /*
   * The shop already exists, just update it.
   */
  if ((rshop = real_shop(S_NUM(nshp))) != NOWHERE) {
   /* free old strings. They're not used in any other place -- Welcor */
   copy_shop(&shop_index[rshop], nshp, TRUE);
    if (rznum != NOWHERE)
      add_to_save_list(zone_table[rznum].number, SL_SHP);
    else
      new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: GenOLC: Cannot determine shop zone.");
    return rshop;
  }

  top_shop++;
  RECREATE(shop_index, struct shop_data, top_shop - top_shop_offset + 1);

  for (rshop = top_shop - top_shop_offset; rshop > 0; rshop--) {
    if (nshp->vnum > SHOP_NUM(rshop - 1)) {
      found = rshop;

      /* Make a "nofree" variant and remove these later. */
      shop_index[rshop].in_room = NULL;
      shop_index[rshop].producing = NULL;
      shop_index[rshop].type = NULL;

      /* don't free old strings - they're still in use -- Welcor */
      copy_shop(&shop_index[rshop], nshp, FALSE);
      break;
    }
    shop_index[rshop] = shop_index[rshop - 1];
  }

  if (!found) {
    /* Make a "nofree" variant and remove these later. */
    shop_index[rshop].in_room = NULL;
    shop_index[rshop].producing = NULL;
    shop_index[rshop].type = NULL;

    /* don't free old strings - they're still in use -- Welcor */
    copy_shop(&shop_index[0], nshp, FALSE);
  }

  if (rznum != NOWHERE)
    add_to_save_list(zone_table[rznum].number, SL_SHP);
  else
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: GenOLC: Cannot determine shop zone.");

  return rshop;
}

/*-------------------------------------------------------------------*/

int save_shops(zone_rnum zone_num)
{
  int i, j, rshop;
  FILE *shop_file;
  char fname[128], oldname[128];
  struct shop_data *shop;

#if CIRCLE_UNSIGNED_INDEX
  if (zone_num == NOWHERE || zone_num > top_of_zone_table) {
#else
  if (zone_num < 0 || zone_num > top_of_zone_table) {
#endif
    log("SYSERR: GenOLC: save_shops: Invalid real zone number %d. (0-%d)", zone_num, top_of_zone_table);
    return FALSE;
  }

  snprintf(fname, sizeof(fname), "%s/%d/%d.new", LIB_WORLD, zone_table[zone_num].number, zone_table[zone_num].number);
  if (!(shop_file = fopen(fname, "w"))) {
    new_mudlog(BRF, LVL_GOD, TRUE, "SYSERR: OLC: Cannot open shop file!");
    return FALSE;
  } else if (fprintf(shop_file, "CircleMUD v3.0 Shop File~\n") < 0) {
    new_mudlog(BRF, LVL_GOD, TRUE, "SYSERR: OLC: Cannot write to shop file!");
    fclose(shop_file);
    return FALSE;
  }
  /*
   * Search database for shops in this zone.
   */
  for (i = zone_table[zone_num].Bot(); i <= zone_table[zone_num].top; i++) {
    if ((rshop = real_shop(i)) != NOWHERE) {
      fprintf(shop_file, "#%d~\n", i);
      shop = shop_index + rshop;

      /*
       * Save the products.
       */
      for (j = 0; S_PRODUCT(shop, j) != NOTHING; j++)
	fprintf(shop_file, "%d\n", obj_index[S_PRODUCT(shop, j)].vnum);
      fprintf(shop_file, "-1\n");

      /*
       * Save the rates.
       */
      fprintf(shop_file, "%1.2f\n"
                         "%1.2f\n",
                         S_BUYPROFIT(shop),
                         S_SELLPROFIT(shop));

      /*
       * Save the buy types and namelists.
       */
      for (j = 0;S_BUYTYPE(shop, j) != NOTHING; j++) 
        fprintf(shop_file, "%d%s\n", 
                S_BUYTYPE(shop, j),
		S_BUYWORD(shop, j) ? S_BUYWORD(shop, j) : "");
      fprintf(shop_file, "-1\n");

      
/* Not allowed to use Ascii in shopfile anymore (bpl21)
      sprintascii(buf1, S_BITVECTOR(shop));
      sprintascii(buf2, S_NOTRADE(shop));
*/

      /*
       * Save messages'n'stuff.
       * Added some small'n'silly defaults as sanity checks.
       */
      fprintf(shop_file,
	      "%s~\n"
	      "%s~\n"
	      "%s~\n"
	      "%s~\n"
	      "%s~\n"
	      "%s~\n"
	      "%s~\n"
	      "%d\n"
	      "%ld\n"
	      "%d\n"
	      "%d\n",
	      S_NOITEM1(shop) ? S_NOITEM1(shop) : "%s Ke?!",
	      S_NOITEM2(shop) ? S_NOITEM2(shop) : "%s Ke?!",
	      S_NOBUY(shop) ? S_NOBUY(shop) : "%s Ke?!",
	      S_NOCASH1(shop) ? S_NOCASH1(shop) : "%s Ke?!",
	      S_NOCASH2(shop) ? S_NOCASH2(shop) : "%s Ke?!",
	      S_BUY(shop) ? S_BUY(shop) : "%s Ke?! %d?",
	      S_SELL(shop) ? S_SELL(shop) : "%s Ke?! %d?",
	      S_BROKE_TEMPER(shop),
	      S_BITVECTOR(shop),
	      S_KEEPER(shop) == NOBODY ? -1 : mob_index[S_KEEPER(shop)].vnum,
	      S_NOTRADE(shop)
	      );

      /*
       * Save the rooms.
       */
      for (j = 0;S_ROOM(shop, j) != NULL; j++) 
        fprintf(shop_file, "%d\n", S_ROOM(shop, j)->number);
      fprintf(shop_file, "-1\n");

      /*
       * Save open/closing times 
       */
      fprintf(shop_file, "%d\n%d\n%d\n%d\n", S_OPEN1(shop), S_CLOSE1(shop),
		S_OPEN2(shop), S_CLOSE2(shop));
    }
  }
  fprintf(shop_file, "$~\n");
  fclose(shop_file);
  
  snprintf(oldname, sizeof(oldname), "%s/%d/%d.shp", LIB_WORLD, zone_table[zone_num].number, zone_table[zone_num].number);
  remove(oldname);
  rename(fname, oldname);

  if (in_save_list(zone_table[zone_num].number, SL_SHP))
    remove_from_save_list(zone_table[zone_num].number, SL_SHP);
  return TRUE;
}
