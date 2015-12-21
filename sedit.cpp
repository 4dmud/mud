/************************************************************************
 * OasisOLC - Shops / sedit.c					v2.0	*
 * Copyright 1996 Harvey Gilpin						*
 * Copyright 1997-2001 George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "db.h"
#include "shop.h"
#include "genolc.h"
#include "genshp.h"
#include "genzon.h"
#include "oasis.h"
#include "descriptor.h"

/*-------------------------------------------------------------------*/

/*
 * External variable declarations.
 */
extern struct shop_data *shop_index;
extern const char *trade_letters[];
extern const char *shop_bits[];
extern const char *item_types[];

/*-------------------------------------------------------------------*/

/*
 * External functions.
 */
SPECIAL(shop_keeper);

/*
 * Should check more things.
 */
void sedit_save_internally(Descriptor *d)
{
  OLC_SHOP(d)->vnum = OLC_NUM(d);
  add_shop(OLC_SHOP(d));
}

void sedit_save_to_disk(int num)
{
  save_shops(num);
}

/*-------------------------------------------------------------------*\
  utility functions 
\*-------------------------------------------------------------------*/

ACMD(do_oasis_sedit)
{
  int num = NOWHERE, save = 0;
  shop_rnum real_num;
  Descriptor *d;
  //char *buf3;
  char buf1[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];
  
  /****************************************************************************/
  /** Parse any arguments.                                                   **/
  /****************************************************************************/
  //buf3 = 
  two_arguments(argument, buf1, buf2);
  
  if (!*buf1) {
    ch->Send( "Specify a shop VNUM to edit.\r\n");
    return;
  } else if (!isdigit(*buf1)) {
    if (str_cmp("save", buf1) != 0) {
      ch->Send( "Yikes!  Stop that, someone will get hurt!\r\n");
      return;
    }
    
    save = TRUE;
    
    if (is_number(buf2))
      num = atoi(buf2);
    else if (GET_OLC_ZONE(ch) > 0) {
      zone_rnum zlok;
      
      if ((zlok = real_zone(GET_OLC_ZONE(ch))) == NOWHERE)
        num = NOWHERE;
      else
        num = zone_table[zlok].Bot();
    }
    
    if (num == NOWHERE) {
      ch->Send( "Save which zone?\r\n");
      return;
    }
  }
  
  /****************************************************************************/
  /** If a numeric argument was given, get it.                               **/
  /****************************************************************************/
  if (num == NOWHERE)
    num = atoi(buf1);
  
  /****************************************************************************/
  /** Check that the shop isn't already being edited.                        **/
  /****************************************************************************/
  for (d = descriptor_list; d; d = d->next) {
    if (STATE(d) == CON_SEDIT) {
      if (d->olc && OLC_NUM(d) == num) {
        ch->Send( "That shop is currently being edited by %s.\r\n",
          PERS(d->character, ch));
        return;
      }
    }
  }
  
  /****************************************************************************/
  /** Point d to the builder's descriptor.                                   **/
  /****************************************************************************/
  d = ch->desc;
  
  /****************************************************************************/
  /** Give the descriptor an OLC structure.                                  **/
  /****************************************************************************/
  if (d->olc) {
    new_mudlog(BRF, LVL_IMMORT, TRUE,
      "SYSERR: do_oasis_sedit: Player already had olc structure.");
    free(d->olc);
  }
  
  CREATE(d->olc, struct oasis_olc_data, 1);
  
  /****************************************************************************/
  /** Find the zone.                                                         **/
  /****************************************************************************/
  OLC_ZNUM(d) = save ? real_zone(num) : real_zone_by_thing(num);
  if (OLC_ZNUM(d) == NOWHERE) {
    ch->Send( "Sorry, there is no zone for that number!\r\n");
    free(d->olc);
    d->olc = NULL;
    return;
  }
  
  /****************************************************************************/
  /** Everyone but IMPLs can only edit zones they have been assigned.        **/
  /****************************************************************************/
  if (!can_edit_zone(ch, OLC_ZNUM(d))) {
    ch->Send( "You do not have permission to edit this zone.\r\n");
    
    /**************************************************************************/
    /** Free the OLC structure.                                              **/
    /**************************************************************************/
    free(d->olc);
    d->olc = NULL;
    return;
  }
  
  if (save) {
    ch->Send( "Saving all shops in zone %d.\r\n",
      zone_table[OLC_ZNUM(d)].number);
    new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(ch)), TRUE,
      "OLC: %s saves shop info for zone %d.",
      GET_NAME(ch), zone_table[OLC_ZNUM(d)].number);
    
    /**************************************************************************/
    /** Save the shops to the shop file.                                     **/
    /**************************************************************************/
    save_shops(OLC_ZNUM(d));
    
    /**************************************************************************/
    /** Free the OLC structure.                                              **/
    /**************************************************************************/
    free(d->olc);
    d->olc = NULL;
    return;
  }
  
  OLC_NUM(d) = num;
  
  if ((real_num = real_shop(num)) != NOTHING)
    sedit_setup_existing(d, real_num);
  else
    sedit_setup_new(d);
  
  STATE(d) = CON_SEDIT;
  
  act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_WRITING);
  
  new_mudlog(BRF, LVL_IMMORT, TRUE, "OLC: %s starts editing zone %d allowed zone %d",
    GET_NAME(ch), zone_table[OLC_ZNUM(d)].number, GET_OLC_ZONE(ch));
}

void sedit_setup_new(Descriptor *d)
{
  struct shop_data *shop;

  /*
   * Allocate a scratch shop structure.
   */
  CREATE(shop, struct shop_data, 1);

  /*
   * Fill in some default values.
   */
  S_KEEPER(shop) = NOBODY;
  S_CLOSE1(shop) = 28;
  S_BUYPROFIT(shop) = 1.0;
  S_SELLPROFIT(shop) = 1.0;
  /*
   * Add a spice of default strings.
   */
  S_NOITEM1(shop) = strdup("%s Sorry, I don't stock that item.");
  S_NOITEM2(shop) = strdup("%s You don't seem to have that.");
  S_NOCASH1(shop) = strdup("%s I can't afford that!");
  S_NOCASH2(shop) = strdup("%s You are too poor!");
  S_NOBUY(shop) = strdup("%s I don't trade in such items.");
  S_BUY(shop) = strdup("%s That'll be %d coins, thanks.");
  S_SELL(shop) = strdup("%s I'll give you %d coins for that.");
  /*
   * Stir the lists lightly.
   */
  CREATE(S_PRODUCTS(shop), obj_vnum, 1);

  S_PRODUCT(shop, 0) = NOTHING;
  CREATE(S_ROOMS(shop), room_rnum, 1);

  S_ROOM(shop, 0) = NULL;
  CREATE(S_NAMELISTS(shop), struct shop_buy_data, 1);

  S_BUYTYPE(shop, 0) = NOTHING;

  /*
   * Presto! A shop.
   */
  OLC_SHOP(d) = shop;
  sedit_disp_menu(d);
}

/*-------------------------------------------------------------------*/

void sedit_setup_existing(Descriptor *d, int rshop_num)
{
  /*
   * Create a scratch shop structure.
   */
  CREATE(OLC_SHOP(d), struct shop_data, 1);

  /* don't waste time trying to free NULL strings -- Welcor */
  copy_shop(OLC_SHOP(d), shop_index + rshop_num, FALSE);
  sedit_disp_menu(d);
}

/**************************************************************************
 Menu functions 
 **************************************************************************/

void sedit_products_menu(Descriptor *d)
{
  struct shop_data *shop;
  int i;

  shop = OLC_SHOP(d);
  get_char_colours(d->character);

  clear_screen(d);
  d->Output( "##     VNUM     Product\r\n");
  for (i = 0; S_PRODUCT(shop, i) != NOTHING; i++) {
    d->Output( "%2d - [%s%5d%s] - %s%s%s\r\n", i,
	    cyn, obj_index[S_PRODUCT(shop, i)].vnum, nrm,
	    yel, obj_proto[S_PRODUCT(shop, i)].short_description, nrm);
  }
  d->Output( "\r\n"
	  "%sA%s) Add a new product.\r\n"
	  "%sD%s) Delete a product.\r\n"
	  "%sQ%s) Quit\r\n"
	  "Enter choice : ", grn, nrm, grn, nrm, grn, nrm);

  OLC_MODE(d) = SEDIT_PRODUCTS_MENU;
}

/*-------------------------------------------------------------------*/

void sedit_compact_rooms_menu(Descriptor *d)
{
  struct shop_data *shop;
  int i, count = 0;

  shop = OLC_SHOP(d);
  get_char_colours(d->character);

  clear_screen(d);
  for (i = 0; S_ROOM(shop, i) != NULL; i++) {
    d->Output( "%2d - [%s%5d%s]  | %s", i, cyn, S_ROOM(shop, i)->number, nrm,
			!(++count % 5) ? "\r\n" : "");
  }
  d->Output( "\r\n"
	  "%sA%s) Add a new room.\r\n"
	  "%sD%s) Delete a room.\r\n"
	  "%sL%s) Long display.\r\n"
	  "%sQ%s) Quit\r\n"
	  "Enter choice : ", grn, nrm, grn, nrm, grn, nrm, grn, nrm);

  OLC_MODE(d) = SEDIT_ROOMS_MENU;
}

/*-------------------------------------------------------------------*/

void sedit_rooms_menu(Descriptor *d)
{
  struct shop_data *shop;
  int i;

  shop = OLC_SHOP(d);
  get_char_colours(d->character);

  clear_screen(d);
  d->Output( "##     VNUM     Room\r\n\r\n");
  for (i = 0; S_ROOM(shop, i) != NULL; i++) {
    d->Output( "%2d - [%s%5d%s] - %s%s%s\r\n", i, cyn, S_ROOM(shop, i)->number, nrm,
	    yel, S_ROOM(shop, i)->name, nrm);
  }
  d->Output( "\r\n"
	  "%sA%s) Add a new room.\r\n"
	  "%sD%s) Delete a room.\r\n"
	  "%sC%s) Compact Display.\r\n"
	  "%sQ%s) Quit\r\n"
	  "Enter choice : ", grn, nrm, grn, nrm, grn, nrm, grn, nrm);

  OLC_MODE(d) = SEDIT_ROOMS_MENU;
}

/*-------------------------------------------------------------------*/

void sedit_namelist_menu(Descriptor *d)
{
  struct shop_data *shop;
  int i;

  shop = OLC_SHOP(d);
  get_char_colours(d->character);

  clear_screen(d);
  d->Output( "##              Type   Namelist\r\n\r\n");
  for (i = 0; S_BUYTYPE(shop, i) != NOTHING; i++) {
    d->Output( "%2d - %s%15s%s - %s%s%s\r\n", i, cyn,
		item_types[S_BUYTYPE(shop, i)], nrm, yel,
		S_BUYWORD(shop, i) ? S_BUYWORD(shop, i) : "<None>", nrm);
  }
  d->Output( "\r\n"
	  "%sA%s) Add a new entry.\r\n"
	  "%sD%s) Delete an entry.\r\n"
	  "%sQ%s) Quit\r\n"
	  "Enter choice : ", grn, nrm, grn, nrm, grn, nrm);

  OLC_MODE(d) = SEDIT_NAMELIST_MENU;
}

/*-------------------------------------------------------------------*/

void sedit_shop_flags_menu(Descriptor *d)
{
  char bits[MAX_STRING_LENGTH];
  int i, count = 0;

  get_char_colours(d->character);
  clear_screen(d);
  for (i = 0; i < NUM_SHOP_FLAGS; i++) {
    d->Output( "%s%2d%s) %-20.20s   %s", grn, i + 1, nrm, shop_bits[i],
		!(++count % 2) ? "\r\n" : "");
  }
  new_sprintbit(S_BITVECTOR(OLC_SHOP(d)), shop_bits, bits, sizeof(bits));
  d->Output( "\r\nCurrent Shop Flags : %s%s%s\r\nEnter choice : ",
		cyn, bits, nrm);
  OLC_MODE(d) = SEDIT_SHOP_FLAGS;
}

/*-------------------------------------------------------------------*/

void sedit_no_trade_menu(Descriptor *d)
{
  char bits[MAX_STRING_LENGTH];
  int i, count = 0;

  get_char_colours(d->character);
  clear_screen(d);
  for (i = 0; i < NUM_TRADERS; i++) {
    d->Output( "%s%2d%s) %-20.20s   %s", grn, i + 1, nrm, trade_letters[i],
		!(++count % 2) ? "\r\n" : "");
  }
  new_sprintbit(S_NOTRADE(OLC_SHOP(d)), trade_letters, bits, sizeof(bits));
  d->Output( "\r\nCurrently won't trade with: %s%s%s\r\n"
	  "Enter choice : ", cyn, bits, nrm);
  OLC_MODE(d) = SEDIT_NOTRADE;
}

/*-------------------------------------------------------------------*/

void sedit_types_menu(Descriptor *d)
{
  //struct shop_data *shop;
  int i, count = 0;

  //shop = OLC_SHOP(d);
  get_char_colours(d->character);

  clear_screen(d);
  for (i = 0; i < NUM_ITEM_TYPES; i++) {
    d->Output( "%s%2d%s) %s%-20s%s  %s", grn, i, nrm, cyn, item_types[i],
		nrm, !(++count % 3) ? "\r\n" : "");
  }
  d->Output( "%sEnter choice : ", nrm);
  OLC_MODE(d) = SEDIT_TYPE_MENU;
}

/*-------------------------------------------------------------------*/

/*
 * Display main menu.
 */
void sedit_disp_menu(Descriptor *d)
{
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];
  struct shop_data *shop;

  shop = OLC_SHOP(d);
  get_char_colours(d->character);

  clear_screen(d);
  new_sprintbit(S_NOTRADE(shop), trade_letters, buf1, sizeof(buf1));
  new_sprintbit(S_BITVECTOR(shop), shop_bits, buf2, sizeof(buf2));
  d->Output(
	  "-- Shop Number : [%s%d%s]\r\n"
	  "%s0%s) Keeper      : [%s%d%s] %s%s\r\n"
          "%s1%s) Open 1      : %s%4d%s          %s2%s) Close 1     : %s%4d\r\n"
          "%s3%s) Open 2      : %s%4d%s          %s4%s) Close 2     : %s%4d\r\n"
	  "%s5%s) Sell rate   : %s%1.2f%s          %s6%s) Buy rate    : %s%1.2f\r\n"
	  "%s7%s) Keeper no item : %s%s\r\n"
	  "%s8%s) Player no item : %s%s\r\n"
	  "%s9%s) Keeper no cash : %s%s\r\n"
	  "%sA%s) Player no cash : %s%s\r\n"
	  "%sB%s) Keeper no buy  : %s%s\r\n"
	  "%sC%s) Buy sucess     : %s%s\r\n"
	  "%sD%s) Sell sucess    : %s%s\r\n"
	  "%sE%s) No Trade With  : %s%s\r\n"
	  "%sF%s) Shop flags     : %s%s\r\n"
	  "%sR%s) Rooms Menu\r\n"
	  "%sP%s) Products Menu\r\n"
	  "%sT%s) Accept Types Menu\r\n"
	  "%sQ%s) Quit\r\n"
	  "Enter Choice : ",

	  cyn, OLC_NUM(d), nrm,
	  grn, nrm, cyn, S_KEEPER(shop) == NOBODY ? -1 : S_KEEPER(shop),
	  nrm, yel, S_KEEPER(shop) == NOBODY ? "None" : GetMobProto(S_KEEPER(shop))->player.short_descr,
	  grn, nrm, cyn, S_OPEN1(shop), nrm,
	  grn, nrm, cyn, S_CLOSE1(shop),
	  grn, nrm, cyn, S_OPEN2(shop), nrm,
	  grn, nrm, cyn, S_CLOSE2(shop),
	  grn, nrm, cyn, S_BUYPROFIT(shop), nrm,
	  grn, nrm, cyn, S_SELLPROFIT(shop),
	  grn, nrm, yel, S_NOITEM1(shop),
	  grn, nrm, yel, S_NOITEM2(shop),
	  grn, nrm, yel, S_NOCASH1(shop),
	  grn, nrm, yel, S_NOCASH2(shop),
	  grn, nrm, yel, S_NOBUY(shop),
	  grn, nrm, yel, S_BUY(shop),
	  grn, nrm, yel, S_SELL(shop),
	  grn, nrm, cyn, buf1,
	  grn, nrm, cyn, buf2,
	  grn, nrm, grn, nrm, grn, nrm, grn, nrm
  );

  OLC_MODE(d) = SEDIT_MAIN_MENU;
}

/**************************************************************************
  The GARGANTUAN event handler
 **************************************************************************/

void sedit_parse(Descriptor *d, char *arg)
{
  int i = 0;

  if (OLC_MODE(d) > SEDIT_NUMERICAL_RESPONSE) {
    if (!isdigit(arg[0]) && ((*arg == '-') && (!isdigit(arg[1])))) {
      d->Output( "Field must be numerical, try again : ");
      return;
    }
  }
  switch (OLC_MODE(d)) {
/*-------------------------------------------------------------------*/
  case SEDIT_CONFIRM_SAVESTRING:
    switch (*arg) {
    case 'y':
    case 'Y':
      sedit_save_internally(d);
      new_mudlog(CMP, MAX(LVL_BUILDER, GET_INVIS_LEV(d->character)), TRUE,
        "OLC: %s edits shop %d", GET_NAME(d->character), OLC_NUM(d));
      if (CONFIG_OLC_SAVE) {
	sedit_save_to_disk(real_zone_by_thing(OLC_NUM(d)));
	d->Output( "Shop saved to disk.\r\n");
      } else
        d->Output( "Shop saved to memory.\r\n");
      cleanup_olc(d, CLEANUP_STRUCTS);
      return;
    case 'n':
    case 'N':
      cleanup_olc(d, CLEANUP_ALL);
      return;
    default:
      d->Output( "Invalid choice!\r\nDo you wish to save the shop? : ");
      return;
    }
    break;

/*-------------------------------------------------------------------*/
  case SEDIT_MAIN_MENU:
    i = 0;
    switch (*arg) {
    case 'q':
    case 'Q':
      if (OLC_VAL(d)) {		/* Anything been changed? */
	d->Output( "Do you wish to save the changes to the shop? (y/n) : ");
	OLC_MODE(d) = SEDIT_CONFIRM_SAVESTRING;
      } else
	cleanup_olc(d, CLEANUP_ALL);
      return;
    case '0':
      OLC_MODE(d) = SEDIT_KEEPER;
      d->Output( "Enter vnum number of shop keeper : ");
      return;
    case '1':
      OLC_MODE(d) = SEDIT_OPEN1;
      d->Output( "When does this shop open (a day has 28 hours) ? ");
      i++;
      break;
    case '2':
      OLC_MODE(d) = SEDIT_CLOSE1;
      d->Output( "When does this shop close (a day has 28 hours) ? ");
      i++;
      break;
    case '3':
      OLC_MODE(d) = SEDIT_OPEN2;
      d->Output( "When does this shop open (a day has 28 hours) ? ");
      i++;
      break;
    case '4':
      OLC_MODE(d) = SEDIT_CLOSE2;
      d->Output( "When does this shop close (a day has 28 hours) ? ");
      i++;
      break;
    case '5':
      OLC_MODE(d) = SEDIT_BUY_PROFIT;
      i++;
      break;
    case '6':
      OLC_MODE(d) = SEDIT_SELL_PROFIT;
      i++;
      break;
    case '7':
      OLC_MODE(d) = SEDIT_NOITEM1;
      i--;
      break;
    case '8':
      OLC_MODE(d) = SEDIT_NOITEM2;
      i--;
      break;
    case '9':
      OLC_MODE(d) = SEDIT_NOCASH1;
      i--;
      break;
    case 'a':
    case 'A':
      OLC_MODE(d) = SEDIT_NOCASH2;
      i--;
      break;
    case 'b':
    case 'B':
      OLC_MODE(d) = SEDIT_NOBUY;
      i--;
      break;
    case 'c':
    case 'C':
      OLC_MODE(d) = SEDIT_BUY;
      i--;
      break;
    case 'd':
    case 'D':
      OLC_MODE(d) = SEDIT_SELL;
      i--;
      break;
    case 'e':
    case 'E':
      sedit_no_trade_menu(d);
      return;
    case 'f':
    case 'F':
      sedit_shop_flags_menu(d);
      return;
    case 'r':
    case 'R':
      sedit_rooms_menu(d);
      return;
    case 'p':
    case 'P':
      sedit_products_menu(d);
      return;
    case 't':
    case 'T':
      sedit_namelist_menu(d);
      return;
    default:
      sedit_disp_menu(d);
      return;
    }

    if (i == 0)
      break;
    else if (i == 1)
      d->Output( "\r\nEnter new value : ");
    else if (i == -1)
      d->Output( "\r\nEnter new text :\r\n] ");
    else
      d->Output( "Oops...\r\n");
    return;
/*-------------------------------------------------------------------*/
  case SEDIT_NAMELIST_MENU:
    switch (*arg) {
    case 'a':
    case 'A':
      sedit_types_menu(d);
      return;
    case 'd':
    case 'D':
      d->Output( "\r\nDelete which entry? : ");
      OLC_MODE(d) = SEDIT_DELETE_TYPE;
      return;
    case 'q':
    case 'Q':
      break;
    }
    break;
/*-------------------------------------------------------------------*/
  case SEDIT_PRODUCTS_MENU:
    switch (*arg) {
    case 'a':
    case 'A':
      d->Output( "\r\nEnter new product vnum number : ");
      OLC_MODE(d) = SEDIT_NEW_PRODUCT;
      return;
    case 'd':
    case 'D':
      d->Output( "\r\nDelete which product? : ");
      OLC_MODE(d) = SEDIT_DELETE_PRODUCT;
      return;
    case 'q':
    case 'Q':
      break;
    }
    break;
/*-------------------------------------------------------------------*/
  case SEDIT_ROOMS_MENU:
    switch (*arg) {
    case 'a':
    case 'A':
      d->Output( "\r\nEnter new room vnum number : ");
      OLC_MODE(d) = SEDIT_NEW_ROOM;
      return;
    case 'c':
    case 'C':
      sedit_compact_rooms_menu(d);
      return;
    case 'l':
    case 'L':
      sedit_rooms_menu(d);
      return;
    case 'd':
    case 'D':
      d->Output( "\r\nDelete which room? : ");
      OLC_MODE(d) = SEDIT_DELETE_ROOM;
      return;
    case 'q':
    case 'Q':
      break;
    }
    break;
/*-------------------------------------------------------------------*/
    /*
     * String edits.
     */
  case SEDIT_NOITEM1:
    if (genolc_checkstring(d, arg))
      modify_string(&S_NOITEM1(OLC_SHOP(d)), arg);
    break;
  case SEDIT_NOITEM2:
    if (genolc_checkstring(d, arg))
      modify_string(&S_NOITEM2(OLC_SHOP(d)), arg);
    break;
  case SEDIT_NOCASH1:
    if (genolc_checkstring(d, arg))
      modify_string(&S_NOCASH1(OLC_SHOP(d)), arg);
    break;
  case SEDIT_NOCASH2:
    if (genolc_checkstring(d, arg))
      modify_string(&S_NOCASH2(OLC_SHOP(d)), arg);
    break;
  case SEDIT_NOBUY:
    if (genolc_checkstring(d, arg))
      modify_string(&S_NOBUY(OLC_SHOP(d)), arg);
    break;
  case SEDIT_BUY:
    if (genolc_checkstring(d, arg))
      modify_string(&S_BUY(OLC_SHOP(d)), arg);
    break;
  case SEDIT_SELL:
    if (genolc_checkstring(d, arg))
      modify_string(&S_SELL(OLC_SHOP(d)), arg);
    break;
  case SEDIT_NAMELIST:
    if (genolc_checkstring(d, arg)) {
      struct shop_buy_data new_entry;

      BUY_TYPE(new_entry) = OLC_VAL(d);
      BUY_WORD(new_entry) = strdup(arg);
      add_to_type_list(&(S_NAMELISTS(OLC_SHOP(d))), &new_entry);
    }
    sedit_namelist_menu(d);
    return;

/*-------------------------------------------------------------------*/
    /*
     * Numerical responses.
     */
  case SEDIT_KEEPER:
    i = atoi(arg);
    if (i != -1)
      if (!MobProtoExists(i)) {
	d->Output( "That mobile does not exist, try again : ");
	return;
      }
    S_KEEPER(OLC_SHOP(d)) = i;
    if (i == -1)
      break;
    /*
     * Fiddle with special procs.
     */
    S_FUNC(OLC_SHOP(d)) =
      GetMobIndex(i)->func != shop_keeper ? GetMobIndex(i)->func : NULL;
    GetMobIndex(i)->func = shop_keeper;
    break;
  case SEDIT_OPEN1:
    S_OPEN1(OLC_SHOP(d)) = LIMIT(atoi(arg), 0, 28);
    break;
  case SEDIT_OPEN2:
    S_OPEN2(OLC_SHOP(d)) = LIMIT(atoi(arg), 0, 28);
    break;
  case SEDIT_CLOSE1:
    S_CLOSE1(OLC_SHOP(d)) = LIMIT(atoi(arg), 0, 28);
    break;
  case SEDIT_CLOSE2:
    S_CLOSE2(OLC_SHOP(d)) = LIMIT(atoi(arg), 0, 28);
    break;
  case SEDIT_BUY_PROFIT:
    sscanf(arg, "%f", &S_BUYPROFIT(OLC_SHOP(d)));
    break;
  case SEDIT_SELL_PROFIT:
    sscanf(arg, "%f", &S_SELLPROFIT(OLC_SHOP(d)));
    break;
  case SEDIT_TYPE_MENU:
    OLC_VAL(d) = LIMIT(atoi(arg), 0, NUM_ITEM_TYPES - 1);
    d->Output( "Enter namelist (return for none) :-\r\n] ");
    OLC_MODE(d) = SEDIT_NAMELIST;
    return;
  case SEDIT_DELETE_TYPE:
    remove_from_type_list(&(S_NAMELISTS(OLC_SHOP(d))), atoi(arg));
    sedit_namelist_menu(d);
    return;
  case SEDIT_NEW_PRODUCT:
    i = atoi(arg);
    if (i != -1)
      if ((i = real_object(i)) == NOTHING) {
	d->Output( "That object does not exist, try again : ");
	return;
      }
    if (i > 0)
      add_to_int_list(&(S_PRODUCTS(OLC_SHOP(d))), i);
    sedit_products_menu(d);
    return;
  case SEDIT_DELETE_PRODUCT:
    remove_from_int_list(&(S_PRODUCTS(OLC_SHOP(d))), atoi(arg));
    sedit_products_menu(d);
    return;
  case SEDIT_NEW_ROOM:
    if ( !is_number ( arg ) || !real_room ( atoi ( arg ) ) )
	{
		d->Output( "That room does not exist, try again : ");
		return;
    }
    if (i >= 0)
      add_to_int_list2(&(S_ROOMS(OLC_SHOP(d))), world_vnum[atoi(arg)]);
    sedit_rooms_menu(d);
    return;
  case SEDIT_DELETE_ROOM:
    remove_from_int_list2(&(S_ROOMS(OLC_SHOP(d))), world_vnum[atoi(arg)]);
    sedit_rooms_menu(d);
    return;
  case SEDIT_SHOP_FLAGS:
    if ((i = LIMIT(atoi(arg), 0, NUM_SHOP_FLAGS)) > 0) {
      TOGGLE_BIT(S_BITVECTOR(OLC_SHOP(d)), 1 << (i - 1));
      sedit_shop_flags_menu(d);
      return;
    }
    break;
  case SEDIT_NOTRADE:
    if ((i = LIMIT(atoi(arg), 0, NUM_TRADERS)) > 0) {
      TOGGLE_BIT(S_NOTRADE(OLC_SHOP(d)), 1 << (i - 1));
      sedit_no_trade_menu(d);
      return;
    }
    break;

/*-------------------------------------------------------------------*/
  default:
    /*
     * We should never get here.
     */
    cleanup_olc(d, CLEANUP_ALL);
    new_mudlog(BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: sedit_parse(): Reached default case!");
    d->Output( "Oops...\r\n");
    break;
  }

/*-------------------------------------------------------------------*/

/*
 * END OF CASE 
 * If we get here, we have probably changed something, and now want to
 * return to main menu.  Use OLC_VAL as a 'has changed' flag.
 */
  OLC_VAL(d) = 1;
  sedit_disp_menu(d);
}

