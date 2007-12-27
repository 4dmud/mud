/* ************************************************************************
*   File: act.item.c                                    Part of CircleMUD *
*  Usage: object handling routines -- get/drop and container handling     *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

/*
 * $Log: act.item.c,v $
 * Revision 1.14  2005/05/03 10:21:25  w4dimenscor
 * changed the free_string function to take a pointer to a pointer so it can nullk the string off properly now. Also, fixed a door loading error, that assumed that all door rooms existed when loading, and now it checks for existstance. Also, fixed the multi arg for 'get' command
 *
 * Revision 1.13  2005/05/01 12:31:07  w4dimenscor
 * added multi arg names to wear and remove
 *
 * Revision 1.12  2005/03/19 15:02:55  w4dimenscor
 * gave centaurs the innate skill mount and riding at 100 % also adjusted
 * damage and speed a little.
 *
 * Revision 1.11  2005/03/17 09:09:17  w4dimenscor
 * fixed a crash bug in the energize command
 *
 * Revision 1.10  2005/03/15 08:35:08  w4dimenscor
 * xml page update, and a few other bits
 *
 * Revision 1.9  2005/02/25 05:02:45  w4dimenscor
 * added new commands and a few little changes - i forget what eek
 *
 * Revision 1.8  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.7  2004/12/17 07:13:20  w4dimenscor
 * A few little updates.
 *
 * Revision 1.6  2004/12/07 09:31:26  w4dimenscor
 * Trees modularized, fix added to message event
 *
 * Revision 1.5  2004/12/05 09:46:51  w4dimenscor
 * fixed mtransform, fixed format in clan tell, and added limit on magic items carried, and lowered weight of magic items, and increased cost
 *
 * Revision 1.4  2004/11/27 20:16:46  w4dimenscor
 * fixed bug in 'get all all.corpse' that caused infinite loop, fixed up bug in combat where event wern't being canceled properly
 *
 * Revision 1.3  2004/11/20 20:16:51  w4dimenscor
 * removing olc.c and olc.h
 *
 * Revision 1.2  2004/11/20 02:33:25  w4dimenscor
 * updated and cleaned up the script system
 *
 * Revision 1.1.1.1  2004/11/12 02:16:46  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.56  2004/09/24 11:34:52  molly
 * fixed automeld
 *
 * Revision 1.55  2004/09/22 09:40:41  molly
 * automeld added so that corpses arent so easily lost, and also made pk corpses lootable
 *
 * Revision 1.54  2004/09/18 04:42:45  molly
 * cleared up some memory leaks again, possibly fixed the QIC miscounts
 *
 * Revision 1.50  2004/09/04 03:46:50  molly
 * made it so only one cost for recovering corpses, and skillist is sorted
 *
 * Revision 1.46  2004/08/22 01:31:16  molly
 * error in do_get where item not found returns a null pointer, but is not checked
 *
 * Revision 1.45  2004/08/18 14:02:25  malfestus
 * removed static int itemcounter. it's not used anymore
 *
 * Revision 1.44  2004/08/18 13:57:55  malfestus
 * Fixed ACMD(do_get) and ACMD(do_put)
 *  - some of the returned messages where wrong
 *  - reformated / changed the code
 *  - removed some not needed objects
 *
 */

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "constants.h"
#include "dg_scripts.h"
#include "screen.h"
#include "fight.h"

/* extern variables */
extern bool LS_REMOVE;

extern int top_of_p_table;

/* External functions */
void remove_corpse_from_list(OBJ_DATA *corpse);
void save_corpses(void);
void House_crashsave(room_vnum vnum);
int house_item_count(room_vnum vnum);
void improve_skill(struct char_data *ch, int skill);
int has_weapon(struct char_data *ch);
int get_weapon_speed(OBJ_DATA *wep);
int wep_hands(OBJ_DATA *wep);
char *find_exdesc(char *word, struct extra_descr_data *list);

int can_wear_on_pos(struct obj_data *obj, int pos);
void perform_meld(CHAR_DATA *ch, OBJ_DATA *corpse);
int count_magic(struct obj_data *obj, CHAR_DATA *ch);
int is_magic(OBJ_DATA *obj);

/* Local Variables */
int slipping = FALSE;
int curbid = 0;			/* current bid on item being auctioned */
int aucstat = AUC_NULL_STATE;	/* state of auction.. first_bid etc.. */
struct obj_data *obj_selling = NULL;	/* current object for sale */
struct char_data *ch_selling = NULL;	/* current character selling obj */
struct char_data *ch_buying = NULL;	/* current character buying the object */
static int wearall = 0;
/*gold tally*/
static long long shop_in = 0;
static long long shop_out = 0;
static long long dropped = 0;
static long long taken = 0;
static long long auction_in = 0;
static long long auction_out = 0;
static long long gold_given = 0;
static long long gold_received = 0;
static long long dg_in = 0;
static long long dg_out = 0;
/*speed functions*/
int speed_update(struct char_data *ch);
int class_speed(struct char_data *ch);
int race_speed(struct char_data *ch);
int alter_gold(struct char_data *ch, gold_int amount);
int where_to_worn(int where);


char *auctioneer[AUC_BID + 1] = {

                                  "The auctioneer auctions, '$n puts $p up for sale at %d coins.'",
                                  "The auctioneer auctions, '$p at %d coins going once!.'",
                                  "The auctioneer auctions, '$p at %d coins going twice!.'",
                                  "The auctioneer auctions, 'Last call: $p going to $N for %d coins.'",
                                  "The auctioneer auctions, 'Unfortunately $p is unsold, returning it to $n.'",
                                  "The auctioneer auctions, 'SOLD! $p to $n for %d coins!.'",
                                  "The auctioneer auctions, 'Sorry, $n has cancelled the auction.'",
                                  "The auctioneer auctions, 'Sorry, $n has left us, the auction can't go on.'",
                                  "The auctioneer auctions, 'Sorry, $p has been confiscated, shame on you $n.'",
                                  "The auctioneer tells you, '$n is selling $p for %d gold.'",
                                  "The auctioneer auctions, '$n bids %d coins on $p.'"
                                };

/* local functions */
bool can_take_obj(struct char_data *ch, struct obj_data *obj);
bool perform_get_from_room(struct char_data *ch, struct obj_data *obj);
int perform_drop(struct char_data *ch, struct obj_data *obj, byte mode,
                 const char *sname, struct room_data * RDR);
int find_eq_pos(struct char_data *ch, struct obj_data *obj, char *arg);
struct char_data *give_find_vict(struct char_data *ch, char *arg);
void get_from_room(struct char_data *ch, char *arg, int amount);
void perform_give_gold(struct char_data *ch, struct char_data *vict,
                       gold_int amount);
void perform_give(struct char_data *ch, struct char_data *vict,
                  struct obj_data *obj);
void perform_drop_gold(struct char_data *ch, gold_int amount, byte mode,
                       struct room_data *  RDR);
void get_check_money(struct char_data *ch, struct obj_data *obj);
void weight_change_object(struct obj_data *obj, int weight);
bool perform_put(struct char_data *ch, struct obj_data *obj,
                 struct obj_data *cont);
void name_from_drinkcon(struct obj_data *obj);
void get_from_container(struct char_data *ch, struct obj_data *cont,
                        int obj_dotmode, char *obj_desc, int mode, int amount);
void name_to_drinkcon(struct obj_data *obj, int type);
void wear_message(struct char_data *ch, struct obj_data *obj, int where);
void perform_wear(struct char_data *ch, struct obj_data *obj, int where);
bool perform_get_from_container(struct char_data *ch, struct obj_data *obj,
                                struct obj_data *cont, int mode);
void perform_remove(struct char_data *ch, int pos);
void auc_send_to_all(char *messg, bool buyer);
void stop_auction(int type, struct char_data *ch);
void auc_stat(struct char_data *ch, struct obj_data *obj);
long long gold_data(int type, long long amount);
ACMD(do_remove);
ACMD(do_put);
ACMD(do_get);
ACMD(do_drop);
ACMD(do_give);
ACMD(do_drink);
ACMD(do_eat);
ACMD(do_pour);
ACMD(do_wear);
ACMD(do_wield);
ACMD(do_grab);

gold_int max_gold(struct char_data *ch);
int spill_gold_amount(struct char_data *ch);
void spill_gold(struct char_data *ch);

long long gold_data(int type, long long amount)
{
  switch (type)
  {
  case SHOP_IN:
    return shop_in += amount;
    break;
  case SHOP_OUT:
    return shop_out += amount;
    break;
  case DROPPED:
    return dropped += amount;
    break;
  case TAKEN:
    return taken += amount;
    break;
  case AUCTION_IN:
    return auction_in += amount;
    break;
  case AUCTION_OUT:
    return auction_out += amount;
    break;
  case GOLD_GIVEN:
    return gold_given += amount;
    break;
  case GOLD_RECEIVED:
    return gold_received += amount;
    break;
  case DG_GOLD_IN:
    return dg_in += amount;
    break;
  case DG_GOLD_OUT:
    return dg_out += amount;
    break;
  }
  return 0;
}

gold_int max_gold(struct char_data * ch)
{
  return (GET_LEVEL(ch) * 5000000);
}

int spill_gold_amount(struct char_data *ch)
{
  gold_int val;
  if (char_gold(ch, 0, GOLD_HAND) > max_gold(ch))
  {
    val = (char_gold(ch, 0, GOLD_HAND) - max_gold(ch));
    return val;
  }
  return (-1);
}

void spill_gold(struct char_data *ch)
{
  if (spill_gold_amount(ch) <= 0)
    return;

  //break this for now
  return;

  perform_drop_gold(ch, spill_gold_amount(ch), SCMD_SPILL, 0);
}





bool perform_put(struct char_data *ch, struct obj_data *obj, struct obj_data *cont)
{
  int value;

  if (drop_otrigger(obj, ch) <= 0)
    return FALSE;

  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_HOUSE) &&
      (value = house_item_count(GET_ROOM_VNUM(IN_ROOM(ch)))) >= MAX_HOUSE_ITEMS)
  {
    new_send_to_char(ch, "No matter how you try you cant fit anything else in.[%d]\r\n", value);
    return FALSE;
  }

  if (IS_OBJ_STAT(cont, ITEM_PC_CORPSE))
  {
    new_send_to_char(ch, "You can't put things in player corpses.\r\n");
    return FALSE;
  }
  if (IS_OBJ_STAT(obj, ITEM_PC_CORPSE))
  {
    new_send_to_char(ch, "You can't put player corpses in things.\r\n");
    return FALSE;
  }

  if ((GET_OBJ_TYPE(cont) == ITEM_CONTAINER)
      && (GET_OBJ_TYPE(obj) == ITEM_CONTAINER))
  {
    send_to_char("You cant put containers inside containers.\r\n", ch);
    return FALSE;
  }
  if (GET_OBJ_WEIGHT(cont) + GET_OBJ_WEIGHT(obj) > GET_OBJ_VAL(cont, 0))
  {
    act("$p won't fit in $P.", FALSE, ch, obj, cont, TO_CHAR);
    return FALSE;
  }

  obj_from_char(obj);
  obj_to_obj(obj, cont);
  if (!wearall && !slipping)
    act("$n puts $p in $P.", TRUE, ch, obj, cont, TO_ROOM);

  /* Yes, I realize this is strange until we have auto-equip on rent. -gg */
  if (IS_OBJ_STAT(obj, ITEM_NODROP) && !IS_OBJ_STAT(cont, ITEM_NODROP))
  {
    SET_BIT_AR(GET_OBJ_EXTRA(cont), ITEM_NODROP);
    act("You get a strange feeling as you put $p in $P.", FALSE, ch, obj, cont, TO_CHAR);
  }
  else if (!wearall || (PRF_FLAGGED(ch, PRF_BATTLESPAM)))
  {
    act("You put $p in $P.", FALSE, ch, obj, cont, TO_CHAR);
  }
  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_HOUSE))
    SET_BIT_AR(ROOM_FLAGS(IN_ROOM(ch)), ROOM_HOUSE_CRASH);

  return TRUE;
}

bool is_put_ok(struct char_data *ch, char *obj_desc, char *cont_desc, int obj_dotmode, int cont_dotmode, struct obj_data **obj_data, struct obj_data **cont_data)
{
  struct char_data *tmp_char_data;
  // no obj name defined
  if (!*obj_desc)
  {
    send_to_char("Put what in what?\r\n", ch);
    return FALSE;
  }

  // no container name defined
  if (!*cont_desc)
  {
    new_send_to_char(ch, "What do you want to put %s in?\r\n",
                     ((obj_dotmode == FIND_INDIV) ? "it" : "them"));
    return FALSE;
  }

  // multiple (all.xxxx) container name used
  if (cont_dotmode != FIND_INDIV)
  {
    send_to_char("You can only put things into one container at a time.\r\n", ch);
    return FALSE;
  }

  generic_find(cont_desc, FIND_OBJ_INV | FIND_OBJ_ROOM, ch, &tmp_char_data, cont_data);

  // container not found
  if (!cont_data || !*cont_data)
  {
    new_send_to_char(ch, "You don't see %s %s here.\r\n", AN(cont_desc), cont_desc);
    return FALSE;
  }

  // container object not a container
  if (GET_OBJ_TYPE(*cont_data) != ITEM_CONTAINER)
  {
    act("$p is not a container.", FALSE, ch, *cont_data, 0, TO_CHAR);
    return FALSE;
  }

  // container is closed
  if (OBJVAL_FLAGGED(*cont_data, CONT_CLOSED))
  {
    send_to_char("You'd better open it first!\r\n", ch);
    return FALSE;
  }

  if (obj_dotmode == FIND_INDIV)
  {
    // no obj matching the desc found
    if (!(*obj_data = get_obj_in_list_vis(ch, obj_desc, NULL, ch->carrying)))
    {
      new_send_to_char(ch, "You aren't carrying %s %s.\r\n", AN(obj_desc), obj_desc);
      return FALSE;
    }

    if (obj_data == cont_data)
    {
      send_to_char("You attempt to fold it into itself, but fail.\r\n", ch);
      return FALSE;
    }
  }

  return TRUE;
}

/* The following put modes are supported by the code below:
 
	1) put <object> <container>
	2) put all.<object> <container>
	3) put all <container>
 
	<container> must be in inventory or on ground.
	all objects to be put into container must be in inventory.
*/
ACMD(do_put)
{
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];
  char arg3[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];
  struct obj_data *next_obj_data, *obj_data, *cont_data;
  int obj_dotmode, cont_dotmode, howmany = 1, processed_put_counter = 0, failed_put_counter = 0;
  char *obj_desc, *cont_desc;

  one_argument(two_arguments(argument, arg1, arg2), arg3);

  if (*arg3 && is_number(arg1))
  {
    howmany = atoi(arg1);
    obj_desc = arg2;
    cont_desc = arg3;
  }
  else
  {
    obj_desc = arg1;
    cont_desc = arg2;
  }
  obj_dotmode = find_all_dots(obj_desc);
  cont_dotmode = find_all_dots(cont_desc);

  if ( !is_put_ok(ch, obj_desc, cont_desc, obj_dotmode, cont_dotmode, &obj_data, &cont_data) )
    return;

  if (!IS_NPC(ch)) SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);

  // put <obj> <container>
  if (obj_dotmode == FIND_INDIV)
  {
    if (howmany > 1)
      wearall = 1;
    while (obj_data && howmany )
    {
      next_obj_data = obj_data->next_content;
      perform_put(ch, obj_data, cont_data) ? processed_put_counter++ : failed_put_counter++;
      obj_data = get_obj_in_list_vis(ch, obj_desc, NULL, next_obj_data);
      howmany--;
    }
    wearall = 0;

    if ( processed_put_counter > 1 ||
         (processed_put_counter == 1 && failed_put_counter > 0 ) )
    {
      snprintf(buf, sizeof(buf), "You put %d %s%s into $p.", processed_put_counter, arg2, (processed_put_counter > 1) ? "s" : "");
      act(buf, FALSE, ch, cont_data, 0, TO_CHAR);
      if (!slipping)
      {
        snprintf(buf, sizeof(buf), "$n puts some %s into $p.", arg2);
        act(buf, FALSE, ch, cont_data, 0, TO_ROOM);
      }
    }
    // put all.<obj> <container>
    // put all <container>
  }
  else
  {
    wearall = 1;
    for (obj_data = ch->carrying; obj_data; obj_data = next_obj_data)
    {
      next_obj_data = obj_data->next_content;
      if (obj_data != cont_data &&
          CAN_SEE_OBJ(ch, obj_data) &&
          GET_OBJ_TYPE(obj_data) != ITEM_CONTAINER &&
          (obj_dotmode == FIND_ALL || isname_full(obj_desc, obj_data->name)))
      {
        perform_put(ch, obj_data, cont_data) ? processed_put_counter++ : failed_put_counter++;
      }
    }
    wearall = 0;

    if ( !processed_put_counter && !failed_put_counter )
    {
      if (obj_dotmode == FIND_ALL)
      {
        send_to_char("You don't seem to have anything to put in it.\r\n", ch);
      }
      else
      {
        new_send_to_char(ch, "You don't seem to have any %ss.\r\n", obj_desc);
      }
    }
    else if ( processed_put_counter )
    {
      if (obj_dotmode == FIND_ALL)
      {
        snprintf(buf, sizeof(buf), "You put everything you can into $p.");
        act(buf, FALSE, ch, cont_data, 0, TO_CHAR);
        if (!slipping)
        {
          snprintf(buf, sizeof(buf), "$n puts everything $e can into $p.");
          act(buf, FALSE, ch, cont_data, 0, TO_ROOM);
        }
      }
      else
      {
        snprintf(buf, sizeof(buf), "You put %d %s%s into $p.", processed_put_counter, obj_desc, (processed_put_counter > 1) ? "s" : "");
        act(buf, FALSE, ch, cont_data, 0, TO_CHAR);
        if (!slipping)
        {
          snprintf(buf, sizeof(buf), "$n puts all the %ss $e can into $p.", arg2);
          act(buf, FALSE, ch, cont_data, 0, TO_ROOM);
        }
      }
    }
  }
}

int is_magic(OBJ_DATA *obj)
{
  switch (GET_OBJ_TYPE(obj))
  {
  case ITEM_SCROLL:
  case ITEM_POTION:
  case ITEM_WAND:
  case ITEM_STAFF:
    return TRUE;
    break;
  }
  return count_magic(obj->contains, NULL);
}
int count_magic(struct obj_data *obj, CHAR_DATA *ch)
{
  if (obj)
  {
    int i = 0;
    if (obj->contains)
      i += count_magic(obj->contains, ch);
    if (obj->next_content)
      i += count_magic(obj->next_content, ch);

    if (is_magic(obj))
      return i + 1;
  }
  return 0;
}

int count_magic_items(CHAR_DATA *ch)
{
  int total = 0;
  int k;
  for (k = 0;k < NUM_WEARS; k++)
  {
    if (GET_EQ(ch, k))
    {
      total += count_magic(GET_EQ(ch, k), ch);
    }
  }
  if (ch->carrying)
  {
    total += count_magic(ch->carrying, ch);
  }
  return total;
}

bool can_take_obj(struct char_data *ch, struct obj_data *obj)
{
  int i;
  if (IS_CARRYING_N(ch) >= CAN_CARRY_N(ch))
  {
    act("$p: you can't carry that many items.", FALSE, ch, obj, 0, TO_CHAR);
    return FALSE;
  }
  else if ((IS_CARRYING_W(ch) + GET_OBJ_WEIGHT(obj)) > CAN_CARRY_W(ch))
  {
    act("$p: you can't carry that much weight.", FALSE, ch, obj, 0, TO_CHAR);
    return FALSE;
  }
  else if (!(CAN_WEAR(obj, ITEM_WEAR_TAKE)))
  {
    act("$p: you can't take that!", FALSE, ch, obj, 0, TO_CHAR);
    return FALSE;
  }
  else if (is_magic(obj) && (i=count_magic_items(ch)) >= MAX_MAGIC_ITEMS)
  {
    if (i == MAX_MAGIC_ITEMS)
      act("$p: you have 45 magic items already.", FALSE, ch, obj, 0, TO_CHAR);
    else
      act("$p: you have more then 45 magic items already.", FALSE, ch, obj, 0, TO_CHAR);
    return FALSE;
  }
  return TRUE;
}


void get_check_money(struct char_data *ch, struct obj_data *obj)
{
  gold_int value = GET_OBJ_VAL(obj, 0);

  if (GET_OBJ_TYPE(obj) != ITEM_MONEY || value <= 0)
    return;

  obj_from_char(obj);
  extract_obj(obj);

  char_gold(ch, value, GOLD_HAND);
  gold_data(TAKEN, value);

  if (value == 1)
    send_to_char("There was 1 coin.\r\n", ch);
  else
  {
    new_send_to_char(ch, "There were %lld coins.\r\n", value);
  }
}

ACMD(do_meld)
{
  char arg[MAX_INPUT_LENGTH];
  struct obj_data *corpse;

  if (IS_NPC(ch))
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("Meld what?\r\n", ch);
    return;
  }

  if (!(corpse = get_obj_in_list_vis(ch, arg, NULL,IN_ROOM(ch)->contents)))
  {
    new_send_to_char(ch, "You don't see %s %s here.\r\n", AN(arg), arg);
    return;
  }
  if (!IS_OBJ_STAT(corpse, ITEM_PC_CORPSE))
  {
    new_send_to_char(ch, "You can't meld with that, it's not a player corpse!\r\n");
    return ;
  }

  if (!isname(GET_NAME(ch), corpse->short_description))
  {
    new_send_to_char(ch, "That isn't your corpse! Hands off!\r\n");
    return;
  }

  perform_meld(ch, corpse);
}

void perform_meld(CHAR_DATA *ch, OBJ_DATA *corpse)
{
  OBJ_DATA *item;
  new_send_to_char(ch, "You meld with your corpse in a sudden flash of light.\r\nYou are dazed.\r\n");
  act("$n melds with $s corpse in a sudden flash of light!", FALSE, ch, 0, 0, TO_ROOM);
  while ((item = corpse->contains))
  {
    obj_from_obj(item);
    obj_to_char(item, ch);
    get_check_money(ch, item);
  }
  extract_obj(corpse);
  if (REMORTS(ch))
  {
    if (REMORTS(ch) < 5)
      GET_WAIT_STATE(ch) = 5 RL_SEC;
    else
      GET_WAIT_STATE(ch) = REMORTS(ch) RL_SEC;
  }
  save_corpses();
  Crash_crashsave(ch);
}

int automeld(struct obj_data *obj)
{
  struct char_data *ch = NULL, *j;
  struct descriptor_data *d;
  int i;
  if (!obj)
    return 0;
  if (!IS_OBJ_STAT(obj, ITEM_PC_CORPSE))
    return 0;

  if ((ch = find_char(GET_OBJ_VAL(obj, 0))) != NULL)
  {
    perform_meld(ch, obj);
    new_mudlog( CMP, GET_LEVEL(ch), TRUE, "AUTOMELD: %s (online - fast link)", GET_NAME(ch));
    return 1;
  }
  for (d = descriptor_list; d; d = d->next)
  {
    if (d->character && GET_ID(d->character) == GET_OBJ_VAL(obj, 0))
    {

      perform_meld(d->character, obj);
      new_mudlog( CMP, GET_LEVEL(d->character), TRUE, "AUTOMELD: %s (online - link)", GET_NAME(d->character));
      return 1;
    }
  }

  for (j = character_list; j; j = j->next)
  {
    if (j && GET_ID(j) == GET_OBJ_VAL(obj, 0))
    {
      perform_meld(j, obj);
      new_mudlog( CMP, GET_LEVEL(j), TRUE, "AUTOMELD: %s (online - linkless)", GET_NAME(j));
      return 1;
    }
  }
  for (i = 0; i <= top_of_p_table; i++)
  {
    if (*player_table[i].name)
    {
      if (player_table[i].id == GET_OBJ_VAL(obj, 0))
      {
        CREATE(ch, struct char_data, 1);
        clear_char(ch);
        CREATE(ch->player_specials, struct player_special_data, 1);
        if (GET_IDNUM(ch))
          ch->loader = GET_IDNUM(ch);
        if (load_char(player_table[i].name, ch) > -1)
        {
          if (!ch)
          {
            new_mudlog( CMP, 51, TRUE, "AUTOMELD: %s (error)", player_table[i].name);
            free_char(ch);
            return 0;
          }
          new_mudlog( CMP, GET_LEVEL(ch), TRUE, "AUTOMELD: %s (offline)", GET_NAME(ch));
          ch->next = character_list;
          character_list = ch;
          ch->desc = NULL;
          char_to_room(ch, world_vnum[1200]);
          Crash_load(ch);
          perform_meld(ch, obj);
          ch->loader = NOBODY;
          extract_char(ch);
          return 1;
        }
        else
        {
          free_char(ch);
          return 1;
        }
      }
    }
  }
  return 0;
}

bool perform_get_from_container(struct char_data *ch, struct obj_data *obj,
                                struct obj_data *cont, int mode)
{

  if (IS_OBJ_STAT(cont, ITEM_PC_CORPSE))
  {

    if (IS_NPC(ch) || GET_OBJ_VAL(cont, 6) == 0 || !IS_PK(ch))
    {
      new_send_to_char(ch, "You can't take things from that players corpse.\r\n");
      return FALSE;
    }
  }


  if (mode != FIND_OBJ_INV && !can_take_obj(ch, obj))
  {
    return FALSE;
  }

  if (IS_CARRYING_N(ch) >= CAN_CARRY_N(ch))
  {
    act("$p: you can't hold any more items.", FALSE, ch, obj, 0, TO_CHAR);
    return FALSE;
  }

  if (get_otrigger(obj, ch) <= 0)
  {
    return FALSE;
  }

  obj_from_obj(obj);
  obj_to_char(obj, ch);
  if (!wearall || (PRF_FLAGGED(ch, PRF_BATTLESPAM)))
    act("You get $p from $P.", FALSE, ch, obj, cont, TO_CHAR);
  if (!wearall && !slipping)
    act("$n gets $p from $P.", TRUE, ch, obj, cont, TO_ROOM);

  if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_HOUSE))
    SET_BIT_AR(ROOM_FLAGS(IN_ROOM(ch)), ROOM_HOUSE_CRASH);
  get_check_money(ch, obj);
  if (IS_OBJ_STAT(cont, ITEM_PC_CORPSE) && GET_OBJ_VAL(cont, 6) != 0)
    save_corpses();
  return TRUE;
}


void get_from_container(struct char_data *ch, struct obj_data *cont,
                        int obj_dotmode, char *obj_desc, int mode, int howmany)
{
  struct obj_data *obj, *next_obj;
  int processed_get_counter = 0, failed_get_counter = 0;
  char buf[MAX_INPUT_LENGTH];

  if (OBJVAL_FLAGGED(cont, CONT_CLOSED))
  {
    act("$p is closed.", FALSE, ch, cont, 0, TO_CHAR);
    return;
  }
  if (IS_OBJ_STAT(cont, ITEM_PC_CORPSE))
    if ((long)GET_OBJ_VAL(cont, 0) == GET_IDNUM(ch))
    {
      perform_meld(ch, cont);
      return;
    }

  if (obj_dotmode == FIND_INDIV)
  {
    if (!(obj = get_obj_in_list_vis(ch, obj_desc, NULL, cont->contains)))
    {
      snprintf(buf, sizeof(buf), "There doesn't seem to be %s %s in $p.", AN(obj_desc), obj_desc);
      act(buf, FALSE, ch, cont, 0, TO_CHAR);
      return;
    }
    if (howmany > 1)
      wearall = 1;
    while (obj && howmany)
    {
      next_obj = obj->next_content;
      perform_get_from_container(ch, obj, cont, mode) ? processed_get_counter++ : failed_get_counter++;
      obj = get_obj_in_list_vis(ch, obj_desc, NULL, next_obj);
      howmany--;
    }
    wearall = 0;

    if (processed_get_counter > 1 ||
        (processed_get_counter == 1 && failed_get_counter > 0))
    {
      snprintf(buf, sizeof(buf), "You get %d %s%s from $p.", processed_get_counter, obj_desc, (processed_get_counter > 1) ? "s" : "");
      act(buf, FALSE, ch, cont, 0, TO_CHAR);
      if (!slipping)
      {
        snprintf(buf, sizeof(buf), "$n gets some %s from $p.", obj_desc);
        act(buf, FALSE, ch, cont, 0, TO_ROOM);
      }
    }
  }
  else
  {
    if (obj_dotmode == FIND_ALLDOT && !*obj_desc)
    {
      send_to_char("Get all of what?\r\n", ch);
      return;
    }
    wearall = 1;
    for (obj = cont->contains; obj; obj = next_obj)
    {
      next_obj = obj->next_content;
      if (CAN_SEE_OBJ(ch, obj) &&
          (obj_dotmode == FIND_ALL || isname_full(obj_desc, obj->name)))
      {
        perform_get_from_container(ch, obj, cont, mode) ? processed_get_counter++ : failed_get_counter++;
      }
    }
    wearall = 0;

    if ( !processed_get_counter && !failed_get_counter )
    {
      if (obj_dotmode == FIND_ALL)
        act("$p seems to be empty.", FALSE, ch, cont, 0, TO_CHAR);
      else
      {
        snprintf(buf, sizeof(buf), "You can't seem to find any %ss in $p.", obj_desc);
        act(buf, FALSE, ch, cont, 0, TO_CHAR);
      }
    }
    else if ( processed_get_counter )
    {
      if (obj_dotmode == FIND_ALL)
      {
        act("You get everything you can out of $p.", FALSE, ch, cont, 0, TO_CHAR);
        if (!slipping)
        {
          act("$n gets everything $e can out of $p.", FALSE, ch, cont, 0, TO_ROOM);
        }
      }
      else
      {
        snprintf(buf, sizeof(buf), "You get %d %s%s out of $p.", processed_get_counter, obj_desc, (processed_get_counter > 1) ? "s" : "");
        act(buf, FALSE, ch, cont, 0, TO_CHAR);
        if (!slipping)
        {
          snprintf(buf, sizeof(buf), "$n gets all the %ss $e can out of $p.", obj_desc);
          act(buf, FALSE, ch, cont, 0, TO_ROOM);
        }
      }
    }
  }
}

void perform_get(struct char_data *ch, char *num, char *arg2, char *arg3)
{
  int cont_dotmode, obj_dotmode, found = 0, mode = 0;
  struct obj_data *cont;
  struct char_data *tmp_char;
  int amount = 1;
  char *obj_desc, *cont_desc;


    amount = atoi(num);
    obj_desc = arg2;
    cont_desc = arg3;
  

  cont_dotmode = find_all_dots(cont_desc);
  obj_dotmode = find_all_dots(obj_desc);

  if (cont_dotmode == FIND_INDIV)
  {
    mode = generic_find(cont_desc, FIND_OBJ_INV | FIND_OBJ_ROOM, ch, &tmp_char, &cont);
    if (!cont)
    {
      new_send_to_char(ch, "You don't have %s %s.\r\n", AN(cont_desc), cont_desc);
      return;
    }
    if (GET_OBJ_TYPE(cont) != ITEM_CONTAINER)
    {
      act("$p is not a container.", FALSE, ch, cont, 0, TO_CHAR);
      return;
    }
    if (IS_OBJ_STAT(cont, ITEM_PC_CORPSE))
    {
      new_send_to_char(ch, "You can't take things from a player corpse.\r\n"
                       "Use: meld corpse\r\n");
      return;
    }
  }
  else if (cont_dotmode == FIND_ALLDOT && !*cont_desc)
  {
    send_to_char("Get from all of what?\r\n", ch);
    return;
  }

  if (cont_dotmode == FIND_INDIV)
  {
    get_from_container(ch, cont, obj_dotmode, obj_desc, mode, amount);
  }
  else
  {
    wearall = 1;
    for (cont = ch->carrying; cont; cont = cont->next_content)
    {
      if (CAN_SEE_OBJ(ch, cont) &&
          (cont_dotmode == FIND_ALL
           || isname(cont_desc, cont->name)))
      {
        if (GET_OBJ_TYPE(cont) == ITEM_CONTAINER)
        {
          found = 1;
          get_from_container(ch, cont, obj_dotmode, obj_desc, FIND_OBJ_INV, amount);
        }
        else if (cont_dotmode == FIND_ALLDOT)
        {
          found = 1;
          act("$p is not a container.", FALSE, ch, cont, 0, TO_CHAR);
        }
      }
    }
    for (cont = IN_ROOM(ch)->contents; cont; cont = cont->next_content)
    {
      if (CAN_SEE_OBJ(ch, cont)
          && (cont_dotmode == FIND_ALL
              || isname(cont_desc, cont->name)))
      {
        if (GET_OBJ_TYPE(cont) == ITEM_CONTAINER)
        {
          found = 1;
          get_from_container(ch, cont, obj_dotmode, obj_desc, FIND_OBJ_ROOM, amount);
        }
        else if (cont_dotmode == FIND_ALLDOT)
        {
          found = 1;
          act("$p is not a container.", FALSE, ch, cont, 0, TO_CHAR);
        }
      }
    }
    wearall = 0;
    if (!found)
    {
      if (cont_dotmode == FIND_ALL)
        send_to_char("You can't seem to find any containers.\r\n", ch);
      else
      {
        new_send_to_char(ch, "You can't seem to find any %ss here.\r\n", cont_desc);
      }
    }
  }
}


bool perform_get_from_room(struct char_data *ch, struct obj_data *obj)
{
  if (can_take_obj(ch, obj) && get_otrigger(obj, ch) > 0)
  {
    obj_from_room(obj);
    obj_to_char(obj, ch);
    if (!wearall || (PRF_FLAGGED(ch, PRF_BATTLESPAM)))
      act("You pick up $p.", FALSE, ch, obj, 0, TO_CHAR);
    if (!wearall && !slipping)
    {
      act("$n picks up $p.", TRUE, ch, obj, 0, TO_ROOM);
    }

    get_check_money(ch, obj);
    if (IS_OBJ_STAT(obj, ITEM_PC_CORPSE))
    {
      remove_corpse_from_list(obj);
      save_corpses();
    }
    if (ROOM_FLAGGED(IN_ROOM(ch), ROOM_HOUSE))
      SET_BIT_AR(ROOM_FLAGS(IN_ROOM(ch)), ROOM_HOUSE_CRASH);
    return TRUE;
  }
  return FALSE;
}


void get_from_room(struct char_data *ch, char *obj_desc, int howmany)
{
  struct obj_data *obj, *next_obj;
  int dotmode, processed_get_counter = 0, failed_get_counter = 0;
  char buf[MAX_INPUT_LENGTH];

  dotmode = find_all_dots(obj_desc);

  if (dotmode == FIND_INDIV)
  {
    if (!(obj = get_obj_in_list_vis(ch, obj_desc, NULL, IN_ROOM(ch)->contents)))
    {
      new_send_to_char(ch, "You don't see %s %s here.\r\n", AN(obj_desc), obj_desc);
    }
    else
    {
      if (howmany > 1)
        wearall = 1;
      while (obj && howmany)
      {
        next_obj = obj->next_content;
        perform_get_from_room(ch, obj) ? processed_get_counter++ : failed_get_counter++;
        obj = get_obj_in_list_vis(ch, obj_desc, NULL, next_obj);
        howmany--;
      }
      wearall = 0;
      if (processed_get_counter > 1 ||
          (processed_get_counter == 1 && failed_get_counter > 0))
      {
        snprintf(buf, sizeof(buf),
                 "You pick up %d thing%s named %s%s off the ground.",
                 processed_get_counter, (processed_get_counter > 1) ? "s" : "", obj_desc, (processed_get_counter > 1) ? "s" : "");
        act(buf, FALSE, ch, 0, 0, TO_CHAR);
        if (!slipping)
        {
          snprintf(buf, sizeof(buf),
                   "$n picks up %s thing%s named %s%s from the room.",
                   (processed_get_counter > 1) ? "some" : "a",
                   (processed_get_counter > 1) ? "s" : "", obj_desc,
                   (processed_get_counter > 1) ? "s" : "");
          act(buf, FALSE, ch, 0, 0, TO_ROOM);
        }
      }
    }
  }
  else
  {
    if (dotmode == FIND_ALLDOT && !*obj_desc)
    {
      send_to_char("Get all of what?\r\n", ch);
      return;
    }

    wearall = 1;
    for (obj = IN_ROOM(ch)->contents; obj; obj = next_obj)
    {
      next_obj = obj->next_content;
      if (CAN_SEE_OBJ(ch, obj)
          && !IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_HIDDEN)
          && (dotmode == FIND_ALL || isname(obj_desc, obj->name)))
      {
        perform_get_from_room(ch, obj) ? processed_get_counter++ : failed_get_counter++;
      }
    }
    wearall = 0;

    if ( !processed_get_counter && !failed_get_counter )
    {
      /* Are they trying to take something in a room extra description? */
      if (find_exdesc(obj_desc, IN_ROOM(ch)->ex_description) != NULL)
      {
        new_send_to_char(ch, "You can't take %s %s.\r\n", AN(obj_desc), obj_desc);
        return;
      }
      if (dotmode == FIND_ALL)
        send_to_char("There doesn't seem to be anything here.\r\n", ch);
      else
      {
        new_send_to_char(ch, "You don't see any %ss here.\r\n", obj_desc);
      }
    }
    else if ( processed_get_counter )
    {
      if (dotmode == FIND_ALL)
      {
        snprintf(buf, sizeof(buf), "You get everything you can off the ground.");
        act(buf, FALSE, ch, 0, 0, TO_CHAR);
        if (!slipping)
        {
          snprintf(buf, sizeof(buf), "$n gets everything $e can off the ground.");
          act(buf, TRUE, ch, 0, 0, TO_ROOM);
        }
      }
      else
      {
        snprintf(buf, sizeof(buf), "You get %d %s%s off the ground.",
                 processed_get_counter, obj_desc, (processed_get_counter > 1) ? "s" : "");
        act(buf, FALSE, ch, 0, 0, TO_CHAR);
        if (!slipping)
        {
          snprintf(buf, sizeof(buf), "$n gets all the %ss $e can off the ground.", obj_desc);
          act(buf, TRUE, ch, 0, 0, TO_ROOM);
        }
      }
    }
  }
}



ACMD(do_get)
{
  char arg1[MAX_INPUT_LENGTH];
  char arg3[MAX_INPUT_LENGTH] = "";
  char num[MAX_INPUT_LENGTH] = "1";
  char *from;

  skip_spaces(&argument);
  if (begins_with_number(argument))
  argument = any_one_arg(argument, num);
  from = str_until(argument, "from", arg1, sizeof(arg1));
  skip_spaces(&from);
  if (*from)
  strlcpy(arg3, from, sizeof(arg3));
  //one_argument(two_arguments(argument, arg1, arg2), arg3);
  

  if (IS_CARRYING_N(ch) >= CAN_CARRY_N(ch))
  {
    send_to_char("Your arms are already full!\r\n", ch);
    return;
  }

  // get
  if (!*arg1)
  {
    send_to_char("Get what?\r\n", ch);
    return;
  }

  if (!IS_NPC(ch)) SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);

  // get <item>
  if (!*from)
  {
    get_from_room(ch, arg1, 1);
    return;
  }

  // get <number> <item>
  if (*num && !*from)
  {
    get_from_room(ch, arg3, atoi(num));
    return;
  }

  // get <item> <container>
  // get <number> <item> from <container>
  perform_get(ch, num, arg1, arg3);

  // using SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH) instead of immediate saveing
  // Crash_crashsave(ch);
}


void perform_drop_gold(struct char_data *ch, gold_int amount,
                       byte mode, struct room_data *  RDR)
{
  struct obj_data *obj;
  char buf[MAX_INPUT_LENGTH];

  if (amount <= 0)
    send_to_char("Heh heh heh.. we are jolly funny today, eh?\r\n",
                 ch);
  else if (char_gold(ch, 0, GOLD_HAND) < amount)
    send_to_char("You don't have that many coins!\r\n", ch);
  else
  {
    /*for the gold tally code */
    gold_data(DROPPED, amount);

    if (mode != SCMD_JUNK)
    {
      WAIT_STATE(ch, PULSE_VIOLENCE);	/* to prevent coin-bombing */
      obj = create_money(amount);
      if (mode == SCMD_DONATE)
      {
        send_to_char
        ("You throw some gold into the air where it disappears in a puff of smoke!\r\n",
         ch);
        act("$n throws some gold into the air where it disappears in a puff of smoke!", FALSE, ch, 0, 0, TO_ROOM);
        obj_to_room(obj, RDR);
        act("$p suddenly appears in a puff of orange smoke!", 0, 0,
            obj, 0, TO_ROOM);
      }
      else if (mode == SCMD_SPILL)
      {
        int i, j = 0;
        new_send_to_char(ch,"Your gold spills all over the ground!\r\n");
        snprintf(buf, sizeof(buf), "$n's gold spills all over the ground!");
        act(buf, TRUE, ch, 0, 0, TO_ROOM);
        char_gold(ch,-amount, GOLD_HAND);
        for (i = number(1, amount); amount > 0;
             i = number(1, amount))
        {
          if (j < 10)
          {
            obj = create_money(i);
            amount -= i;
          }
          else
          {
            obj = create_money(amount);
            amount = 0;
          }
          obj_to_room(obj, IN_ROOM(ch));
        }

      }
      else
      {
        switch (drop_wtrigger(obj, ch))
        {	/* obj may be purged */
        case  0: extract_obj(obj); return;
        case -1: return;
        }
        send_to_char("You drop some gold.\r\n", ch);
        if (!slipping)
        {
          snprintf(buf, sizeof(buf), "$n drops %s.", money_desc(amount));
          act(buf, TRUE, ch, 0, 0, TO_ROOM);
        }
        obj_to_room(obj, IN_ROOM(ch));
      }
    }
    else
    {
      if (!slipping)
      {
        snprintf(buf, sizeof(buf),
                 "$n drops %s which disappears in a puff of smoke!",
                 money_desc(amount));
        act(buf, FALSE, ch, 0, 0, TO_ROOM);
      }
      send_to_char
      ("You drop some gold which disappears in a puff of smoke!\r\n",
       ch);
    }
    char_gold(ch,-amount, GOLD_HAND);
  }
}


#define VANISH(mode) ((mode == SCMD_DONATE || mode == SCMD_JUNK) ? \
		      "  It vanishes in a puff of smoke!" : "")

int perform_drop(struct char_data *ch, struct obj_data *obj,
                 byte mode, const char *sname, struct room_data *  RDR)
{
  int value;
  char buf[MAX_INPUT_LENGTH];

  if (drop_otrigger(obj, ch) <= 0)
    return (0);

  if ((mode == SCMD_DROP) && !drop_wtrigger(obj, ch))
    return (0);

  if ((mode == SCMD_DROP) && (ROOM_FLAGGED(IN_ROOM(ch), ROOM_HOUSE)))
  {
    if ((value =
           house_item_count(GET_ROOM_VNUM(IN_ROOM(ch)))) >=
        MAX_HOUSE_ITEMS)
    {
      new_send_to_char(ch,
                       "No matter how you try you cant fit anything else in.[%d]\r\n",
                       value);
      return (0);
    } /*else if (GET_OBJ_TYPE(obj) == ITEM_CONTAINER) {
                                                                                	    send_to_char("Sorry, but you drop containers here.\r\n", ch);
                                                                                	    return (0);
                                                                                	}*/
  }


  if (IS_OBJ_STAT(obj, ITEM_NODROP))
  {
    snprintf(buf, sizeof(buf), "You can't %s $p, it must be CURSED!", sname);
    act(buf, FALSE, ch, obj, 0, TO_CHAR);
    return (0);
  }
  if (!wearall || (PRF_FLAGGED(ch, PRF_BATTLESPAM)))
  {
    snprintf(buf, sizeof(buf), "You %s $p.%s", sname, VANISH(mode));
    act(buf, FALSE, ch, obj, 0, TO_CHAR);
  }
  if (!wearall)
  {
    if (!slipping)
    {
      snprintf(buf, sizeof(buf), "$n %ss $p.%s", sname, VANISH(mode));
      act(buf, TRUE, ch, obj, 0, TO_ROOM);
    }
  }
  obj_from_char(obj);

  if ((mode == SCMD_DONATE) && IS_OBJ_STAT(obj, ITEM_NODONATE))
    mode = SCMD_JUNK;

  switch (mode)
  {
  case SCMD_DROP:
    obj_to_room(obj, IN_ROOM(ch));
    if (IS_OBJ_STAT(obj, ITEM_MELT_DROP))
    {
      if (!slipping)
        act("$p dissolves into smoke.", FALSE, 0, obj, 0, TO_ROOM);
      act("$p dissolves into smoke.", FALSE, 0, obj, 0, TO_CHAR);
      extract_obj(obj);
    }
    return (0);
  case SCMD_DONATE:
    obj_to_room(obj, RDR);
    act("$p suddenly appears in a puff a smoke!", FALSE, 0, obj, 0, TO_ROOM);
    return (0);
  case SCMD_JUNK:
    value = MAX(1, MIN(200, GET_OBJ_COST(obj) / 16));
    extract_obj(obj);
    char_gold(ch, value, GOLD_BANK);
    return (value);
  default:
    log("SYSERR: Incorrect argument %d passed to perform_drop.", mode);
    break;
  }

  return (0);
}



ACMD(do_drop)
{
  struct obj_data *obj, *next_obj;
  struct room_data *  RDR = NULL;
  byte mode = SCMD_DROP;
  int dotmode, amount = 0, multi, num_don_rooms;
  const char *sname;
  char buf[MAX_INPUT_LENGTH];
  char arg[MAX_INPUT_LENGTH];

  switch (subcmd)
  {
  case SCMD_JUNK:
    sname = "junk";
    mode = SCMD_JUNK;
    break;
  case SCMD_DONATE:
    sname = "donate";
    mode = SCMD_DONATE;
    /* fail + double chance for room 1   */
    num_don_rooms = (CONFIG_DON_ROOM_1 != NULL) * 2 +
                    (CONFIG_DON_ROOM_2 != NULL)     +
                    (CONFIG_DON_ROOM_3 != NULL)     + 1 ;
    switch (number(0, num_don_rooms))
    {
    case 0:
      mode = SCMD_JUNK;
      break;
    case 1:
    case 2:
      RDR = CONFIG_DON_ROOM_1;
      break;
    case 3: RDR = CONFIG_DON_ROOM_2; break;
    case 4: RDR = CONFIG_DON_ROOM_3; break;
    }
    if (RDR == NULL)
    {
      send_to_char("Sorry, you can't donate anything right now.\r\n",
                   ch);
      return;
    }
    break;
  default:
    sname = "drop";
    break;
  }

  argument = one_argument(argument, arg);

  if (!*arg)
  {
    new_send_to_char(ch,  "What do you want to %s?\r\n", sname);
    return;
  }
  else if (is_number(arg))
  {
    multi = atoi(arg);
    one_argument(argument, arg);
    if (!str_cmp("coins", arg) || !str_cmp("coin", arg))
      perform_drop_gold(ch, multi, mode, RDR);
    else if (multi <= 0)
      send_to_char("Yeah, that makes sense.\r\n", ch);
    else if (!*arg)
    {
      new_send_to_char(ch, "What do you want to %s %d of?\r\n", sname,
                       multi);
    }
    else
      if (!(obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
      {
        new_send_to_char(ch, "You don't seem to have any %ss.\r\n", arg);
      }
      else
      {
        do
        {
          next_obj =
            get_obj_in_list_vis(ch, arg, NULL, obj->next_content);
          amount += perform_drop(ch, obj, mode, sname, RDR);
          obj = next_obj;
        }
        while (obj && --multi);
      }
  }
  else
  {
    dotmode = find_all_dots(arg);

    /* Can't junk or donate all */
    if ((dotmode == FIND_ALL)
        && (subcmd == SCMD_JUNK || subcmd == SCMD_DONATE))
    {
      if (subcmd == SCMD_JUNK)
        send_to_char
        ("Go to the dump if you want to junk EVERYTHING!\r\n",
         ch);
      else
        send_to_char
        ("Go do the donation room if you want to donate EVERYTHING!\r\n",
         ch);
      return;
    }
    if (dotmode == FIND_ALL)
    {
      if (!ch->carrying)
        send_to_char("You don't seem to be carrying anything.\r\n",
                     ch);
      else
        for (obj = ch->carrying; obj; obj = next_obj)
        {
          next_obj = obj->next_content;
          amount += perform_drop(ch, obj, mode, sname, RDR);
        }
    }
    else if (dotmode == FIND_ALLDOT)
    {
      if (!*arg)
      {
        new_send_to_char(ch, "What do you want to %s all of?\r\n", sname);
        return;
      }
      if (!(obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
      {
        new_send_to_char(ch, "You don't seem to have any %ss.\r\n",
                         arg);
      }
      wearall = 1;
      if (obj)
      {
        new_send_to_char(ch, "You %s all of %s.\r\n", sname, arg);
        if (!slipping)
        {
          snprintf(buf, sizeof(buf), "$n %ss all of %s.", sname, arg);
          act(buf, TRUE, ch, 0, 0, TO_ROOM);
        }
      }

      while (obj)
      {
        next_obj =
          get_obj_in_list_vis(ch, arg, NULL, obj->next_content);
        amount += perform_drop(ch, obj, mode, sname, RDR);
        obj = next_obj;
      }
      wearall = 0;
    }
    else
    {
      if (!(obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
      {
        new_send_to_char(ch, "You don't seem to have %s %s.\r\n", AN(arg),
                         arg);
      }
      else
        amount += perform_drop(ch, obj, mode, sname, RDR);
    }
  }

  if (amount && (subcmd == SCMD_JUNK))
  {
    send_to_char("You have been rewarded by the gods!\r\n", ch);
    if (!slipping)
    {
      act("$n has been rewarded by the gods!", TRUE, ch, 0, 0, TO_ROOM);
    }
    char_gold(ch, amount, GOLD_HAND);
    gold_data(TAKEN, amount);
  }
}


void perform_give(struct char_data *ch, struct char_data *vict,
                  struct obj_data *obj)
{

  if (give_otrigger(obj, ch, vict) <= 0)
    return;
  if (receive_mtrigger(vict, ch, obj) <= 0)
    return;
  if (IS_OBJ_STAT(obj, ITEM_NODROP))
  {
    act("You can't let go of $p!!  Yeech!", FALSE, ch, obj, 0,
        TO_CHAR);
    return;
  }
  if (IS_CARRYING_N(vict) >= CAN_CARRY_N(vict))
  {
    act("$N seems to have $S hands full.", FALSE, ch, 0, vict,
        TO_CHAR);
    if (!slipping)
      act("$n just tried to give you $p but your hands were full.",
          FALSE, ch, obj, vict, TO_VICT);
    return;
  }
  if (GET_OBJ_WEIGHT(obj) + IS_CARRYING_W(vict) > CAN_CARRY_W(vict))
  {
    act("$E can't carry that much weight.", FALSE, ch, 0, vict,
        TO_CHAR);
    if (!slipping)
      act("$n just tried to give you $p but you can't carry that much weight.", FALSE, ch, obj, vict, TO_VICT);
    return;
  }

  obj_from_char(obj);
  obj_to_char(obj, vict);
  if (!slipping)
  {
    act("You give $p to $N.", FALSE, ch, obj, vict, TO_CHAR);
    act("$n gives you $p.", FALSE, ch, obj, vict, TO_VICT);
    act("$n gives $p to $N.", TRUE, ch, obj, vict, TO_NOTVICT);
  }
  else
    act("You slip $p to $N.", FALSE, ch, obj, vict, TO_CHAR);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);
  SET_BIT_AR(PLR_FLAGS(vict), PLR_CRASH);
}

/* utility function for give */
struct char_data *give_find_vict(struct char_data *ch, char *arg)
{
  struct char_data *vict;

  if (!*arg)
  {
    send_to_char("To who?\r\n", ch);
    return (NULL);
  }
  else if (!(vict = get_char_vis(ch, arg, NULL, FIND_CHAR_ROOM)))
  {
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    return (NULL);
  }
  else if (vict == ch)
  {
    send_to_char("What's the point of that?\r\n", ch);
    return (NULL);
  }
  else
    return (vict);
}


void perform_give_gold(struct char_data *ch, struct char_data *vict,
                       gold_int amount)
{
  char buf[MAX_INPUT_LENGTH];

  if (amount <= 0)
  {
    send_to_char("Heh heh heh ... we are jolly funny today, eh?\r\n",
                 ch);
    return;
  }

  if ((char_gold(ch,0, GOLD_HAND) < amount)
      && (IS_NPC(ch) || (GET_LEVEL(ch) < LVL_GOD)))
  {
    send_to_char("You don't have that many coins!\r\n", ch);
    return;
  }
  new_send_to_char(ch, "%s", CONFIG_OK);
  if (!slipping)
  {
    snprintf(buf, sizeof(buf), "$n gives you %lld gold coin%s.", amount,
             amount == 1 ? "" : "s");
    act(buf, FALSE, ch, 0, vict, TO_VICT);

    snprintf(buf, sizeof(buf), "$n gives %s to $N.", money_desc(amount));
    act(buf, TRUE, ch, 0, vict, TO_NOTVICT);
  }
  if (IS_NPC(ch) || (GET_LEVEL(ch) < LVL_GOD))
  {
    char_gold(ch, -amount, GOLD_HAND);
    gold_data(GOLD_GIVEN, amount);
  }
  gold_data(GOLD_RECEIVED, amount);
  char_gold(vict, amount, GOLD_HAND);

  bribe_mtrigger(vict, ch, amount);
}


ACMD(do_give)
{
  gold_int amount;
  char arg[MAX_INPUT_LENGTH];
  int dotmode;
  struct char_data *vict = NULL;
  struct obj_data *obj, *next_obj;

  argument = one_argument(argument, arg);

  if (!*arg)
    send_to_char("Give what to who?\r\n", ch);
  else if (is_number(arg))
  {
    amount = atol(arg);
    argument = one_argument(argument, arg);
    if (!str_cmp("coins", arg) || !str_cmp("coin", arg))
    {
      one_argument(argument, arg);
      if ((vict = give_find_vict(ch, arg)) != NULL)
        perform_give_gold(ch, vict, amount);
      return;
    }
    else if (!*arg)
    {	/* Give multiple code. */
      new_send_to_char(ch, "What do you want to give %lld of?\r\n",
                       amount);
    }
    else if (!(vict = give_find_vict(ch, argument)))
    {
      return;
    }
    else
      if (!(obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
      {
        new_send_to_char(ch, "You don't seem to have any %ss.\r\n", arg);
      }
      else
      {
        while (obj && amount--)
        {
          next_obj =
            get_obj_in_list_vis(ch, arg, NULL, obj->next_content);
          perform_give(ch, vict, obj);
          obj = next_obj;
        }
      }
  }
  else
  {
    char buf1[MAX_INPUT_LENGTH];
    one_argument(argument, buf1);
    if (!(vict = give_find_vict(ch, buf1)))
      return;
    dotmode = find_all_dots(arg);
    if (dotmode == FIND_INDIV)
    {
      if (!(obj = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
      {
        new_send_to_char(ch, "You don't seem to have %s %s.\r\n", AN(arg),
                         arg);
      }
      else
        perform_give(ch, vict, obj);
    }
    else
    {
      if (dotmode == FIND_ALLDOT && !*arg)
      {
        send_to_char("All of what?\r\n", ch);
        return;
      }
      if (!ch->carrying)
        send_to_char("You don't seem to be holding anything.\r\n",
                     ch);
      else
        for (obj = ch->carrying; obj; obj = next_obj)
        {
          next_obj = obj->next_content;
          if (CAN_SEE_OBJ(ch, obj) &&
              ((dotmode == FIND_ALL || isname(arg, obj->name))))
            perform_give(ch, vict, obj);
        }
    }
  }
  if (vict)
    SET_BIT_AR(PLR_FLAGS(vict), PLR_CRASH);
  SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);
}



void weight_change_object(struct obj_data *obj, int weight)
{
  struct obj_data *tmp_obj;
  struct char_data *tmp_ch;

  if (obj->in_room != NULL)
  {
    GET_OBJ_WEIGHT(obj) += weight;
  }
  else if ((tmp_ch = obj->carried_by))
  {
    obj_from_char(obj);
    GET_OBJ_WEIGHT(obj) += weight;
    obj_to_char(obj, tmp_ch);
  }
  else if ((tmp_obj = obj->in_obj))
  {
    obj_from_obj(obj);
    GET_OBJ_WEIGHT(obj) += weight;
    obj_to_obj(obj, tmp_obj);
  }
  else if ((tmp_ch = obj->worn_by))
  {
    int pos = obj->worn_on;
    obj = unequip_char(tmp_ch, pos);
    GET_OBJ_WEIGHT(obj) += weight;
    equip_char(tmp_ch, obj, pos);
  }
  else
  {
    log("SYSERR: Unknown attempt to subtract weight from an object.");
  }
}

void weight_to_object(struct obj_data *obj, int weight)
{
  struct obj_data *tmp_obj;
  struct char_data *tmp_ch;

  if (obj->in_room != NULL)
  {
    GET_OBJ_WEIGHT(obj) = weight;
  }
  else if ((tmp_ch = obj->carried_by))
  {
    obj_from_char(obj);
    GET_OBJ_WEIGHT(obj) = weight;
    obj_to_char(obj, tmp_ch);
  }
  else if ((tmp_obj = obj->in_obj))
  {
    obj_from_obj(obj);
    GET_OBJ_WEIGHT(obj) = weight;
    obj_to_obj(obj, tmp_obj);
  }
  else if ((tmp_ch = obj->worn_by))
  {
    int pos = obj->worn_on;
    obj = unequip_char(tmp_ch, pos);
    GET_OBJ_WEIGHT(obj) = weight;
    equip_char(tmp_ch, obj, pos);

  }
  else
  {
    log("SYSERR: Unknown attempt to subtract weight from an object.");
  }
}

void name_from_drinkcon(struct obj_data *obj)
{
  char *new_name, *cur_name, *next;
  const char *liqname;
  int liqlen, cpylen;
  return;
  if (!obj
      || (GET_OBJ_TYPE(obj) != ITEM_DRINKCON
          && GET_OBJ_TYPE(obj) != ITEM_FOUNTAIN))
    return;

  liqname = drinknames[GET_OBJ_VAL(obj, 2)];
  if (!isname(liqname, obj->name))
  {
    log("SYSERR: Can't remove liquid '%s' from '%s' (%d) item.",
        liqname, obj->name, obj->item_number);
    return;
  }

  liqlen = strlen(liqname);
  CREATE(new_name, char, strlen(obj->name) - strlen(liqname));	/* +1 for NUL, -1 for space */

  for (cur_name = obj->name; cur_name; cur_name = next)
  {
    if (*cur_name == ' ')
      cur_name++;

    if ((next = strchr(cur_name, ' ')))
      cpylen = next - cur_name;
    else
      cpylen = strlen(cur_name);

    if (!strn_cmp(cur_name, liqname, liqlen))
      continue;

    if (*new_name)
      strcat(new_name, " ");	/* strcat: OK (size precalculated) */
    strncat(new_name, cur_name, cpylen);	/* strncat: OK (size precalculated) */
  }

  if (GET_OBJ_RNUM(obj) == NOTHING
      || obj->name != obj_proto[GET_OBJ_RNUM(obj)].name)
    free(obj->name);
  obj->name = new_name;
}



void name_to_drinkcon(struct obj_data *obj, int type)
{
  char *new_name;
  return;
  if (!obj
      || (GET_OBJ_TYPE(obj) != ITEM_DRINKCON
          && GET_OBJ_TYPE(obj) != ITEM_FOUNTAIN))
    return;

  CREATE(new_name, char,
         strlen(obj->name) + strlen(drinknames[type]) + 2);
  sprintf(new_name,  "%s %s", obj->name, drinknames[type]);	/* sprintf: OK */

  if (GET_OBJ_RNUM(obj) == NOTHING
      || obj->name != obj_proto[GET_OBJ_RNUM(obj)].name)
    free(obj->name);

  obj->name = new_name;
}



ACMD(do_drink)
{
  struct obj_data *temp;
  struct affected_type af;
  int amount = 0, weight;
  int on_ground = 0;
  char arg[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (IS_NPC(ch))		/* Cannot use GET_COND() on mobs. */
    return;

  if (!*arg)
  {
    send_to_char("Drink from what?\r\n", ch);
    return;
  }
  if (!(temp = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
  {
    if (!
        (temp =
           get_obj_in_list_vis(ch, arg, NULL,
                               IN_ROOM(ch)->contents)))
    {
      send_to_char("You can't find it!\r\n", ch);
      return;
    }
    else
      on_ground = 1;
  }
  if ((GET_OBJ_TYPE(temp) != ITEM_DRINKCON) &&
      (GET_OBJ_TYPE(temp) != ITEM_FOUNTAIN) &&
      (GET_OBJ_TYPE(temp) != ITEM_VIAL))
  {
    send_to_char("You can't drink from that!\r\n", ch);
    return;
  }
  if (on_ground && (GET_OBJ_TYPE(temp) != ITEM_FOUNTAIN))
  {
    send_to_char("You have to be holding that to drink from it.\r\n", ch);
    return;
  }

  if ((GET_OBJ_TYPE(temp) == ITEM_VIAL))
  {

    switch (GET_OBJ_VAL(temp, 0))
    {
    case VIAL_NONE:
      new_send_to_char(ch, "It has nothing in it.\r\n");
      return;
      break;
    case VIAL_HITP:
      if (GET_HIT(ch) == GET_MAX_HIT(ch))
      {
        new_send_to_char(ch, "Your health is already full.\r\n");
        return;
      }
      break;
    case VIAL_MANA:
      if (GET_MANA(ch) == GET_MAX_MANA(ch))
      {
        new_send_to_char(ch, "Your mana energy is already full.\r\n");
        return;
      }
      break;
    case VIAL_MOVE:
      if (GET_MOVE(ch) == GET_MAX_MOVE(ch))
      {
        new_send_to_char(ch, "Your move energy is already full.\r\n");
        return;
      }
      break;
    case VIAL_STAM:
      if (GET_STAMINA(ch) == GET_MAX_STAMINA(ch))
      {
        new_send_to_char(ch, "Your stamina is already full.\r\n");
        return;
      }
      break;
    }

  }
  else
  {
    if ((GET_COND(ch, DRUNK) > (5+(GET_SKILL(ch, SKILL_DRUNK) / 4))) && (GET_COND(ch, THIRST) > 0))
    {
      /* The pig is drunk */
      send_to_char
      ("You can't seem to get close enough to your mouth.\r\n", ch);
      act("$n tries to drink but misses $s mouth!", TRUE, ch, 0, 0,
          TO_ROOM);
      return;
    }
    if ((GET_COND(ch, FULL) > 23) && (GET_COND(ch, THIRST) > 0))
    {
      send_to_char("Your stomach can't contain anymore!\r\n", ch);
      return;
    }
  }
  if (!GET_OBJ_VAL(temp, 1))
  {
    send_to_char("It's empty.\r\n", ch);
    return;
  }


  if (consume_otrigger(temp, ch, OCMD_DRINK) <= 0)  /* check trigger */
    return;

  if (subcmd == SCMD_DRINK)
  {
    message_type = REST_MOVE;
    if (GET_OBJ_TYPE(temp) != ITEM_VIAL)
    {
      snprintf(buf, sizeof(buf), "$n drinks %s from $p.", drinks[GET_OBJ_VAL(temp, 2)]);
      act(buf, TRUE, ch, temp, 0, TO_ROOM);
      message_type = NOTHING;
      new_send_to_char(ch, "You drink the %s.\r\n", drinks[GET_OBJ_VAL(temp, 2)]);

      if (drink_aff[GET_OBJ_VAL(temp, 2)][DRUNK] > 0)
      {
        amount = (25 - GET_COND(ch, THIRST)) / drink_aff[GET_OBJ_VAL(temp,2)][DRUNK];
      }
      else
        amount = number(3, 10);

    }
    else
    {
      const char *to_char = NULL;
      const char *to_room = NULL;
      switch (GET_OBJ_VAL(temp, 0))
      {
      case VIAL_NONE:
        new_send_to_char(ch, "It has nothing in it.");
        return;
        break;
      case VIAL_HITP:
        amount = ((GET_HIT(ch) - GET_MAX_HIT(ch)) < GET_OBJ_VAL(temp, 1) ? (GET_HIT(ch) - GET_MAX_HIT(ch)) : GET_OBJ_VAL(temp, 1));
        to_char = "A million bright green sparks encircle your body as you drink.";
        to_room = "A million bright green sparks encircle $n's body as $e drinks.";
        break;
      case VIAL_MANA:
        amount = ((GET_MANA(ch) - GET_MAX_MANA(ch)) < GET_OBJ_VAL(temp, 1) ? (GET_MANA(ch) - GET_MAX_MANA(ch)) : GET_OBJ_VAL(temp, 1));
        to_char = "Dark purple flames flow into your body as you drink.";
        to_room = "Dark purple flames flow into $n's body as $e drinks.";
        break;
      case VIAL_MOVE:
        amount = ((GET_MOVE(ch) - GET_MAX_MOVE(ch)) < GET_OBJ_VAL(temp, 1) ? (GET_MOVE(ch) - GET_MAX_MOVE(ch)) : GET_OBJ_VAL(temp, 1));
        to_char = "Blue crystals form over your body as you drink.";
        to_room = "Blue crystals forms over $n's body as $e drinks.";
        break;
      case VIAL_STAM:
        amount = ((GET_STAMINA(ch) - GET_MAX_STAMINA(ch)) < GET_OBJ_VAL(temp, 1) ? (GET_STAMINA(ch) - GET_MAX_STAMINA(ch)) : GET_OBJ_VAL(temp, 1));
        to_char = "Thousands of black shadows slide into your body as you drink.";
        to_room = "Thousands of black shadows slide into $n's body as $e drinks.";
        break;
      }
      if (to_char)
        act(to_char, FALSE, ch, 0, 0, TO_CHAR);
      if (to_room)
        act(to_room, FALSE, ch, 0, 0, TO_ROOM);
    }

  }
  else
  {
    act("$n sips from $p.", TRUE, ch, temp, 0, TO_ROOM);
    new_send_to_char(ch, "It tastes like %s.\r\n",
                     (GET_OBJ_TYPE(temp) == ITEM_VIAL) ? vial_types[GET_OBJ_VAL(temp, 0)] : drinks[GET_OBJ_VAL(temp, 2)]);
    amount = (int)((float)GET_OBJ_VAL(temp, 1)/10.0);
  }

  amount = MIN(amount, GET_OBJ_VAL(temp, 1));

  /* You can't subtract more than the object weighs */
  if (GET_OBJ_TYPE(temp) == ITEM_VIAL)
  {
    weight = GET_OBJ_WEIGHT(temp) - (((GET_OBJ_WEIGHT(temp) * 100) - amount)/100);
  }
  else
  {
    weight = MIN(amount, GET_OBJ_WEIGHT(temp));
  }

  weight_change_object(temp, -weight);	/* Subtract amount */

  if (GET_OBJ_TYPE(temp) == ITEM_VIAL)
  {
    switch (GET_OBJ_VAL(temp, 0))
    {
    case VIAL_NONE:
      new_send_to_char(ch, "It has nothing in it.\r\n");
      return;
      break;
    case VIAL_HITP:
      alter_hit(ch, -amount);
      break;
    case VIAL_MANA:
      alter_mana(ch, -amount);
      break;
    case VIAL_MOVE:
      alter_move(ch, -amount);
      break;
    case VIAL_STAM:
      alter_stamina(ch, -amount);
      break;
    }
    update_pos(ch);
  }
  else
  {

    if ( drink_aff[GET_OBJ_VAL(temp, 2)][DRUNK] && number(1, 100) < GET_SKILL(ch, SKILL_DRUNK))
    {
      improve_skill(ch, SKILL_DRUNK);
      gain_condition(ch, DRUNK,  drink_aff[GET_OBJ_VAL(temp, 2)][DRUNK]  * amount / 4);
    }
    gain_condition(ch, FULL,   drink_aff[GET_OBJ_VAL(temp, 2)][FULL]   * amount / 4);
    gain_condition(ch, THIRST, drink_aff[GET_OBJ_VAL(temp, 2)][THIRST] * amount / 4);

    if (GET_COND(ch, DRUNK) > 10)
      send_to_char("You feel drunk.\r\n", ch);

    if (GET_COND(ch, THIRST) > 23)
      send_to_char("You don't feel thirsty any more.\r\n", ch);

    if (GET_COND(ch, FULL) > 23)
      send_to_char("You are full.\r\n", ch);

    if (GET_OBJ_VAL(temp, 3))
    {	/* The shit was poisoned ! */
      send_to_char("Oops, it tasted rather strange!\r\n", ch);
      act("$n chokes and utters some strange sounds.", TRUE, ch, 0, 0,
          TO_ROOM);

      af.type = SPELL_POISON;
      af.expire = HOURS_TO_EXPIRE((amount * 3));
      af.modifier = 0;
      af.location = APPLY_NONE;
      af.bitvector = AFF_POISON_1;
      affect_join(ch, &af, FALSE, FALSE, FALSE, FALSE);
    }

  }
  /* empty the container, and no longer poison. */
  GET_OBJ_VAL(temp, 1) -= amount;
  if (!GET_OBJ_VAL(temp, 1))
  {	/* The last bit */
    if (GET_OBJ_TYPE(temp) == ITEM_VIAL)
    {
      GET_OBJ_VAL(temp, 0) = -1;
    }
    else
    {
      GET_OBJ_VAL(temp, 2) = 0;
      GET_OBJ_VAL(temp, 3) = 0;
      name_from_drinkcon(temp);
    }
  }
  return;
}



ACMD(do_eat)
{
  struct obj_data *food;
  struct affected_type af;
  int amount;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (IS_NPC(ch))		/* Cannot use GET_COND() on mobs. */
    return;

  if (!*arg)
  {
    send_to_char("Eat what?\r\n", ch);
    return;
  }
  if (!(food = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)))
  {
    new_send_to_char(ch, "You don't seem to have %s %s.\r\n", AN(arg), arg);
    return;
  }
  if (subcmd == SCMD_TASTE && ((GET_OBJ_TYPE(food) == ITEM_DRINKCON) ||
                               (GET_OBJ_TYPE(food) == ITEM_FOUNTAIN)))
  {
    do_drink(ch, argument, 0, SCMD_SIP);
    return;
  }
  if ((GET_OBJ_TYPE(food) != ITEM_FOOD) && (GET_LEVEL(ch) < LVL_GOD))
  {
    send_to_char("You can't eat THAT!\r\n", ch);
    return;
  }
  if (GET_COND(ch, FULL) > 20)
  {	/* Stomach full */
    send_to_char("You are too full to eat more!\r\n", ch);
    return;
  }

  if (consume_otrigger(food, ch, OCMD_EAT) <= 0)  /* check trigger */
    return;

  if (subcmd == SCMD_EAT)
  {
    act("You eat $p.", FALSE, ch, food, 0, TO_CHAR);
    act("$n eats $p.", TRUE, ch, food, 0, TO_ROOM);
  }
  else
  {
    act("You nibble a little bit of $p.", FALSE, ch, food, 0, TO_CHAR);
    act("$n tastes a little bit of $p.", TRUE, ch, food, 0, TO_ROOM);
  }

  amount = (subcmd == SCMD_EAT ? GET_OBJ_VAL(food, 0) : 1);

  gain_condition(ch, FULL, amount);

  if (GET_COND(ch, FULL) > 20)
    send_to_char("You are full.\r\n", ch);

  if (GET_OBJ_VAL(food, 3) && (GET_LEVEL(ch) < LVL_GOD))
  {
    /* The shit was poisoned ! */
    send_to_char("Oops, that tasted rather strange!\r\n", ch);
    act("$n coughs and utters some strange sounds.", FALSE, ch, 0, 0,
        TO_ROOM);

    af.type = SPELL_POISON;
    af.expire = HOURS_TO_EXPIRE((amount * 2));
    af.modifier = 0;
    af.location = APPLY_NONE;
    af.bitvector = AFF_POISON_1;
    affect_join(ch, &af, FALSE, FALSE, FALSE, FALSE);
  }
  if (subcmd == SCMD_EAT)
    extract_obj(food);
  else
  {
    if (!(--GET_OBJ_VAL(food, 0)))
    {
      send_to_char("There's nothing left now.\r\n", ch);
      extract_obj(food);
    }
  }
}


ACMD(do_pour)
{
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];
  struct obj_data *from_obj = NULL, *to_obj = NULL;
  int amount = 0, type = VIAL_NONE;

  two_arguments(argument, arg1, arg2);

  if (subcmd == SCMD_POUR)
  {
    if (!*arg1)
    {		/* No arguments */
      send_to_char("From what do you want to pour?\r\n", ch);
      return;
    }
    if (!
        (from_obj =
           get_obj_in_list_vis(ch, arg1, NULL, ch->carrying)))
    {
      send_to_char("You can't find it!\r\n", ch);
      return;
    }
    if (GET_OBJ_TYPE(from_obj) != ITEM_DRINKCON && GET_OBJ_TYPE(from_obj) != ITEM_VIAL && GET_OBJ_TYPE(from_obj) != ITEM_POTION)
    {
      send_to_char("You can't pour from that!\r\n", ch);
      return;
    }
  }
  else if (subcmd == SCMD_FILL)
  {
    if (!*arg1)
    {		/* no arguments */
      send_to_char
      ("What do you want to fill?  And what are you filling it from?\r\n",
       ch);
      return;
    }
    if (!(to_obj = get_obj_in_list_vis(ch, arg1, NULL, ch->carrying)))
    {
      send_to_char("You can't find it!", ch);
      return;
    }
    if (GET_OBJ_TYPE(to_obj) != ITEM_DRINKCON)
    {
      act("You can't fill $p!", FALSE, ch, to_obj, 0, TO_CHAR);
      return;
    }
    if (!*arg2)
    {		/* no 2nd argument */
      act("What do you want to fill $p from?", FALSE, ch, to_obj, 0,
          TO_CHAR);
      return;
    }
    if (!
        (from_obj =
           get_obj_in_list_vis(ch, arg2, NULL,
                               IN_ROOM(ch)->contents)))
    {
      new_send_to_char(ch, "There doesn't seem to be %s %s here.\r\n",
                       AN(arg2), arg2);
      return;
    }
    if (GET_OBJ_TYPE(from_obj) != ITEM_FOUNTAIN)
    {
      act("You can't fill something from $p.", FALSE, ch, from_obj,
          0, TO_CHAR);
      return;
    }
  }
  if (GET_OBJ_VAL(from_obj, 1) == 0 && GET_OBJ_TYPE(from_obj) != ITEM_POTION)
  {
    act("The $p is empty.", FALSE, ch, from_obj, 0, TO_CHAR);
    return;
  }
  if (subcmd == SCMD_POUR)
  {	/* pour */
    if (!*arg2)
    {
      send_to_char("Where do you want it?  Out or in what?\r\n", ch);
      return;
    }
    if (!str_cmp(arg2, "out"))
    {
      act("$n empties $p.", TRUE, ch, from_obj, 0, TO_ROOM);
      act("You empty $p.", FALSE, ch, from_obj, 0, TO_CHAR);
      if (GET_OBJ_TYPE(from_obj) == ITEM_VIAL)
        weight_change_object(from_obj, GET_OBJ_WEIGHT(from_obj));
      else
        weight_change_object(from_obj, -GET_OBJ_VAL(from_obj, 1));	/* Empty */
      if (GET_OBJ_TYPE(from_obj) == ITEM_VIAL)
        GET_OBJ_VAL(from_obj, 0) = -1; /* vial type */
      else
      {
        GET_OBJ_VAL(from_obj, 2) = 0;
        GET_OBJ_VAL(from_obj, 3) = 0;
        name_from_drinkcon(from_obj);
      }
      /* amount*/
      GET_OBJ_VAL(from_obj, 1) = 0;

      return;
    }
    if (!(to_obj = get_obj_in_list_vis(ch, arg2, NULL, ch->carrying)))
    {
      send_to_char("You can't find it!\r\n", ch);
      return;
    }
    if ((GET_OBJ_TYPE(to_obj) != ITEM_DRINKCON) &&
        (GET_OBJ_TYPE(to_obj) != ITEM_FOUNTAIN) &&
        (GET_OBJ_TYPE(to_obj) != ITEM_VIAL))
    {
      send_to_char("You can't pour anything into that.\r\n", ch);
      return;
    }
  }
  if (to_obj == from_obj)
  {
    send_to_char("A most unproductive effort.\r\n", ch);
    return;
  }
  if ((GET_OBJ_TYPE(to_obj) != ITEM_VIAL))
  {
    if ((GET_OBJ_VAL(to_obj, 1) != 0) &&
        (GET_OBJ_VAL(to_obj, 2) != GET_OBJ_VAL(from_obj, 2)))
    {
      send_to_char("There is already another liquid in it!\r\n", ch);
      return;
    }

    if (!(GET_OBJ_VAL(to_obj, 1) < GET_OBJ_VAL(to_obj, 0)))
    {
      send_to_char("There is no room for more.\r\n", ch);
      return;
    }
  }
  else
  {

    if (GET_OBJ_VAL(to_obj, 2) == GET_OBJ_VAL(to_obj, 1))
    {
      act("No more can be fit into $p for now.", FALSE, ch, to_obj, 0, TO_CHAR);
      return;
    }
    if (GET_OBJ_TYPE(from_obj) == ITEM_VIAL && GET_OBJ_VAL(to_obj, 0) != VIAL_NONE &&
        (GET_OBJ_VAL(from_obj, 0) != GET_OBJ_VAL(to_obj, 0)))
    {
      act("Those two vials are of different energys and can't be mixed.", FALSE, ch, 0, 0, TO_CHAR);
      return;
    }
    if (GET_OBJ_TYPE(from_obj) == ITEM_POTION)
    {
      int i;
      for (i=1;i<4;i++)
      {
        switch (GET_OBJ_VAL(from_obj, i))
        {
        case SPELL_HEAL:
          if (!type || type == VIAL_HITP)
          {
            amount += 300;
            type = VIAL_HITP;
          }
          break;
        case SPELL_CURE_CRITIC:
          if (!type || type == VIAL_HITP)
          {
            amount += 150;
            type = VIAL_HITP;
          }
          break;
        case SPELL_CURE_LIGHT:
          if (!type || type == VIAL_HITP)
          {
            amount += 75;
            type = VIAL_HITP;
          }
          break;
        case SPELL_VITALIZE:
          if (number(0, 1))
          {
            if (!type || type == VIAL_MOVE)
            {
              amount += 100;
              type = VIAL_MOVE;
            }
          }
          else
          {
            if (!type || type == VIAL_STAM)
            {
              amount += 20;
              type = VIAL_STAM;
            }
          }

          break;
        }
      }
      if (amount == 0 && type == VIAL_NONE)
      {
        act("That potion does not have the properties needed to fill $p.", FALSE, ch, to_obj, 0, TO_CHAR);
        return;
      }
    }
    /* vials*/
  }
  if (subcmd == SCMD_POUR)
  {
    new_send_to_char(ch, "You pour the %s into the %s.",
                     (GET_OBJ_TYPE(to_obj) == ITEM_DRINKCON) ? drinks[GET_OBJ_VAL(from_obj, 2)] : from_obj->short_description, to_obj->short_description);
  }
  else if (subcmd == SCMD_FILL)
  {
    act("You gently fill $p from $P.", FALSE, ch, to_obj, from_obj,TO_CHAR);
    act("$n gently fills $p from $P.", TRUE, ch, to_obj, from_obj,TO_ROOM);
  }
  /* New alias */
  if (GET_OBJ_TYPE(to_obj) == ITEM_DRINKCON)
  {
    if (  GET_OBJ_VAL(to_obj, 1) == 0)
      name_to_drinkcon(to_obj, GET_OBJ_VAL(from_obj, 2));

    /* First same type liq. */
    GET_OBJ_VAL(to_obj, 2) = GET_OBJ_VAL(from_obj, 2);


    /* Then how much to pour */
    GET_OBJ_VAL(from_obj, 1) -= (amount =
                                   (GET_OBJ_VAL(to_obj, 0) -
                                    GET_OBJ_VAL(to_obj, 1 )));

    GET_OBJ_VAL(to_obj, 1) = GET_OBJ_VAL(to_obj, 0);

    if (GET_OBJ_VAL(from_obj, 1) < 0)
    {	/* There was too little */
      GET_OBJ_VAL(to_obj, 1) += GET_OBJ_VAL(from_obj, 1);
      amount += GET_OBJ_VAL(from_obj, 1);
      GET_OBJ_VAL(from_obj, 1) = 0;
      GET_OBJ_VAL(from_obj, 2) = 0;
      GET_OBJ_VAL(from_obj, 3) = 0;
      name_from_drinkcon(from_obj);
    }
    /* Then the poison boogie */
    GET_OBJ_VAL(to_obj, 3) =
      (GET_OBJ_VAL(to_obj, 3) || GET_OBJ_VAL(from_obj, 3));

    /* And the weight boogie */
    weight_change_object(from_obj, -amount);
    weight_change_object(to_obj, amount);	/* Add weight */
  }
  else
  {
    int weight;
    if (!amount)
    {
      GET_OBJ_VAL(from_obj, 1) -= (amount = (MAX(GET_OBJ_VAL(to_obj, 2) - GET_OBJ_VAL(to_obj, 1) , GET_OBJ_VAL(from_obj, 1) )));
      GET_OBJ_VAL(to_obj, 1) += amount;
      GET_OBJ_VAL(to_obj, 0) = GET_OBJ_VAL(from_obj, 0);
    }
    else
    {
      extract_obj(from_obj);
      from_obj = NULL;
      GET_OBJ_VAL(to_obj, 1) += amount;
      GET_OBJ_VAL(to_obj, 1) = MAX(GET_OBJ_VAL(to_obj, 2), GET_OBJ_VAL(to_obj, 1));
      GET_OBJ_VAL(to_obj, 0) = type;
    }
    /* And the weight boogie */
    weight = GET_OBJ_WEIGHT(to_obj);
    weight *= 100;
    weight = (GET_OBJ_VAL(to_obj, 1) - weight)/100.0;
    weight_change_object(to_obj, weight);	/* Add weight */
    weight = GET_OBJ_WEIGHT(from_obj);
    weight *= 100;
    weight = (weight - GET_OBJ_VAL(from_obj, 1))/100.0;
    weight_change_object(from_obj, -weight);	/* subtract weight */
  }

}
ACMD(do_energize)
{
  const char *to_char = NULL;
  const char *to_room = NULL;
  int  amount = 0, type = VIAL_NONE;
  char arg[MAX_INPUT_LENGTH], arg1[MAX_INPUT_LENGTH];
  OBJ_DATA *temp;
  two_arguments(argument, arg, arg1);

  if (!arg || !*arg)
  {
    new_send_to_char(ch, "USAGE: energize <item> [HIT|MANA|MOVE|STAMINA]\r\n");
    return;
  }
  if ((temp = get_obj_in_list_vis(ch, arg, NULL, ch->carrying)) == NULL)
  {
    new_send_to_char(ch, "Nothing about by that name.\r\n");
    return;
  }
  if (GET_OBJ_TYPE(temp) != ITEM_VIAL)
  {
    new_send_to_char(ch, "You can't energize that item!\r\n");
    return;
  }
  if ((!arg1 || !*arg1))
  {
    if ( GET_OBJ_VAL(temp, 0) == VIAL_NONE)
    {
      new_send_to_char(ch, "That is an empty vial - you must specify a new type for it.");
      return;
    }
    else
    {
      if (is_abbrev(arg1, "hitpoints") || is_abbrev(arg1, "health"))
        type = VIAL_HITP;
      else if (is_abbrev(arg1, "mana"))
        type = VIAL_MANA;
      else if (is_abbrev(arg1, "move"))
        type = VIAL_MOVE;
      else if (is_abbrev(arg1, "stamina"))
        type = VIAL_STAM;

      if (type == VIAL_NONE)
      {
        new_send_to_char(ch, "Sorry, I don't recognise that type.\r\n");
        return;
      }

    }
  }


  type = type ? type : GET_OBJ_VAL(temp, 0);
  switch (type)
  {
  case VIAL_NONE:
    new_send_to_char(ch, "It has nothing in it.");
    return;
    break;
  case VIAL_HITP:
    amount = (int)((float)GET_HIT(ch)/4.0);
    to_char = "A million bright green sparks leave your body.";
    to_room = "A million bright green sparks leave $n's body.";
    break;
  case VIAL_MANA:
    amount = (int)((float)GET_MANA(ch)/4.0);
    to_char = "Dark purple flames flow out of your body.";
    to_room = "Dark purple flames flow out of $n's body.";
    break;
  case VIAL_MOVE:
    amount = (int)((float)GET_MANA(ch)/4.0);
    to_char = "Blue crystal sluice from your body.";
    to_room = "Blue crystals sluices from $n's body.";
    break;
  case VIAL_STAM:
    amount = (int)((float)GET_STAMINA(ch)/4.0);
    to_char = "Thousands of black shadows slide out of your body.";
    to_room = "Thousands of black shadows slide out of $n's body.";
    break;
  }
  if (amount <= 0)
  {
    act("You are too weak to do that.", FALSE, ch, 0, 0, TO_CHAR);
    return;
  }
  if (to_char)
    act(to_char, FALSE, ch, 0, 0, TO_CHAR);
  if (to_room)
    act(to_room, FALSE, ch, 0, 0, TO_ROOM);

  act("$p glows bright with a flash!", FALSE, ch, temp, 0, TO_CHAR);
  act("$p glows bright with a flash!", FALSE, ch, temp, 0, TO_ROOM);

  amount = MAX(GET_OBJ_VAL(temp, 2) - GET_OBJ_VAL(temp, 1), amount);

  GET_OBJ_VAL(temp, 0) = type;
  GET_OBJ_VAL(temp, 1) += amount;
  amount *= 4;

  switch (type)
  {
  case VIAL_NONE:
    return;
    break;
  case VIAL_HITP:
    alter_hit(ch, -amount);
    break;
  case VIAL_MANA:
    alter_mana(ch, -amount);
    break;
  case VIAL_MOVE:
    alter_move(ch, -amount);
    break;
  case VIAL_STAM:
    alter_stamina(ch, -amount);
    break;
  }


}


OBJ_DATA * create_vial(void)
{

  struct obj_data *vial;
  int type = VIAL_NONE;
  int size = 0;
  register int i;

  for (i = 0; i < 2000;i++)
  {
    if (!(number(0, 10)))
      size++;
  }

  type = number(VIAL_HITP, VIAL_STAM);
  vial = create_obj();

  vial->item_number = NOTHING;
  vial->in_room = NULL;

  GET_OBJ_TYPE(vial) = ITEM_VIAL;
  SET_BIT_AR(GET_OBJ_EXTRA(vial), ITEM_UNIQUE_SAVE);
  GET_OBJ_VAL(vial, 0) = type;
  GET_OBJ_VAL(vial, 1) = number(0, size);
  GET_OBJ_VAL(vial, 2) = size;
  GET_OBJ_VAL(vial, 3) = 0;
  GET_OBJ_COST(vial) = 1000 * size;
  GET_OBJ_WEIGHT(vial) = (float)GET_OBJ_VAL(vial, 1)/100.0;
  GET_OBJ_RENT(vial) = 0;
  GET_OBJ_TIMER(vial) = -1;

  return (vial);

}



void wear_message(struct char_data *ch, struct obj_data *obj, int where)
{
  const char *wear_messages[][2] =
    {
      {"$n lights $p and holds it.",
       "You light $p and hold it."
      },

      {"$n slides $p on to $s right ring finger.",
       "You slide $p on to your right ring finger."},

      {"$n slides $p on to $s left ring finger.",
       "You slide $p on to your left ring finger."},

      {"$n wears $p around $s neck.",
       "You wear $p around your neck."},

      {"$n wears $p around $s neck.",
       "You wear $p around your neck."},

      {"$n wears $p on $s body.",
       "You wear $p on your body."},

      {"$n wears $p on $s head.",
       "You wear $p on your head."},

      {"$n puts $p on $s legs.",
       "You put $p on your legs."},

      {"$n wears $p on $s feet.",
       "You wear $p on your feet."},

      {"$n puts $p on $s hands.",
       "You put $p on your hands."},

      {"$n wears $p on $s arms.",
       "You wear $p on your arms."},

      {"$n straps $p around $s arm as a shield.",
       "You start to use $p as a shield."},

      {"$n wears $p about $s body.",
       "You wear $p around your body."},

      {"$n wears $p around $s waist.",
       "You wear $p around your waist."},

      {"$n puts $p on around $s right wrist.",
       "You put $p on around your right wrist."},

      {"$n puts $p on around $s left wrist.",
       "You put $p on around your left wrist."},

      {"$n wields $p.",
       "You wield $p."},

      {"$n grabs $p.",
       "You grab $p."},

      {"$n wears $p on $s face.",
       "You wear $p on your face."},

      {"$n wears $p over $s eyes.",
       "You wear $p over your eyes."},

      {"$n wears $p on $s hips.",
       "You wear $p on your hips."},

      {"$n sticks $p in $s ear.",
       "You stick $p in your ear."},

      {"$n sticks $p in $s ear.",
       "You stick $p in your ear."},

      {"$n wears $p on $s right ankle.",
       "You wear $p on your right ankle."},

      {"$n wears $p on $s left ankle.",
       "You wear $p on your left ankle."},

      {"$n wears $p on $s horns.",
       "You wear $p on your horns."},

      {"$n wears $p on $s antenna.",
       "You wear $p on your antenna."},

      {"$n slips $p on $s tail.",
       "You slip $p on your tail."},

      {"$n wields $p as a secondary weapon.",
       "You wield $p as a secondary weapon."},

      {"$n wears $p on $s hind legs.",
       "You wear $p on your hind legs."},

      {"$n wears $p on $s hind feet.",
       "You wear $p on your hind feet."},

      {"$n starts focusing on $p.",
       "You start focusing on $p."},

      {"$n wears $p on $s left thumb.",
       "You wear $p on your left thumb."},

      {"$n wears $p on $s right thumb.",
       "You wear $p on your right thumb."},

      {"$n wears $p over $s back.",
       "You wear $p over your back."},

      {"$n wears $p through $s ear tip.",
       "You wear $p through your ear tip."},

      {"$n wears $p on $s left shoulder.",
       "You wear $p on your left shoulder."},

      {"$n wears $p on $s right shoulder.",
       "You wear $p on your right shoulder."},

      {"$n wears $p as a crest.",
       "You wear $p as a crest."},

      {"$n straps $p on $s left thigh.",
       "You strap $p on your left thigh."},

      {"$n straps $p on $s right thigh.",
       "You strap $p on your right thigh."},

      {"$n binds $p on to $s left knee.",
       "You bind $p on to your left knee."},

      {"$n binds $p on to $s right knee.",
       "You bind $p on to your right knee."},

      {"$n lets $p go and it floats beside $m.",
       "You let $p go and it floats beside you."}
    };

  if (GET_EQ(ch, WEAR_FOCUS) && where == WEAR_SHIELD)
  {
    act("$n concentrates and $p floats up infront of $m.", TRUE, ch, obj, 0, TO_ROOM);
    act("You concentrate and $p floats up infront of you.", FALSE, ch, obj, 0, TO_CHAR);
  }
  else
  {
    act(wear_messages[where][0], TRUE, ch, obj, 0, TO_ROOM);
    act(wear_messages[where][1], FALSE, ch, obj, 0, TO_CHAR);
  }
}



void perform_wear(struct char_data *ch, struct obj_data *obj, int where)
{


  /*
   * ITEM_WEAR_TAKE is used for objects that do not require special bits
   * to be put into that position (e.g. you can hold any object, not just
   * an object with a HOLD bit.)
   */



  static const char *already_wearing[] =
    {
      "You're already using a light.\r\n",
      "YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT.\r\n",
      "You're already wearing something on both of your ring fingers.\r\n",
      "You can't wear anything else around your neck.\r\n",
      "You can't wear anything else around your neck.\r\n",
      "You're already wearing something on your body.\r\n",
      "You're already wearing something on your head.\r\n",
      "You're already wearing something on your legs.\r\n",
      "You're already wearing something on your feet.\r\n",
      "You're already wearing something on your hands.\r\n",
      "You're already wearing something on your arms.\r\n",
      "You're already using a shield.\r\n",
      "You're already wearing something about your body.\r\n",
      "You already have something around your waist.\r\n",
      "YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT.\r\n",
      "You're already wearing something around both of your wrists.\r\n",
      "You're already wielding a weapon.\r\n",
      "You're already holding something.\r\n",
      "You're already wearing something on your face.\r\n",
      "You're already wearing something on your eyes.\r\n",
      "You're already wearing something on your hips.\r\n",
      "YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT.\r\n",
      "You're already wearing something in both your ears.\r\n",
      "YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT.\r\n",
      "You're already wearing something on both your ankles.\r\n",
      "You're already wearing something on your horns.\r\n",
      "You're already wearing something on your antenna.\r\n",
      "You're already wearing something on your tail.\r\n",
      "You're already wielding a second weapon.\r\n",
      "You're already wearing something on your hind legs.\r\n",
      "You're already wearing something on your hind feet.\r\n",
      "You're already focusing on something.\r\n",
      "---------BAD MESSAGE----------\r\n",
      "You're already wearing something on both your thumbs.\r\n",
      "You're already saddled.\r\n",
      "You're already wearing something through the tip of your ear.\r\n",
      "---------BAD MESSAGE-----------\r\n",
      "You're already wearing something on both your shoulders.\r\n",
      "You're already wearing something as your crest.\r\n",
      "---------BAD MESSAGE-----------\r\n",
      "You're already wearing something on both your thighs.\r\n",
      "---------BAD_MESSAGE-----------\r\n",
      "You're already wearing something on both your knees.\r\n",
      "You're already have something floating around you.\r\n"
    };

  if (!HAS_BODY(ch, where))
  {
    send_to_char("You do not have that body position.\r\n", ch);
    return;
  }

  /* first, make sure that the wear position is valid. */
  if (!can_wear_on_pos(obj, where))
  {
    act("You can't wear $p there.", FALSE, ch, obj, 0, TO_CHAR);
    return;
  }

  /*
  if ((IS_CARRYING_W(ch) + GET_OBJ_WEIGHT(obj)) > CAN_CARRY_W(ch)) {
      act("You aren't strong enough to wear $p at the moment.", FALSE, ch, obj, 0, TO_CHAR);
      return;
  }
  */

  if (obj->owner != 0 && !IS_NPC(ch) && GET_IDNUM(ch) != obj->owner)
  {
    new_send_to_char(ch, "You can't wear that! It is owned by %s\r\n", get_name_by_id(obj->owner));
    return;
  }

  /* for neck, finger, and wrist, try pos 2 if pos 1 is already full */
  if (   (where == WEAR_FINGER_R) || (where == WEAR_NECK_1)
         || (where ==  WEAR_WRIST_R) || (where == WEAR_EAR_R)
         || (where ==  WEAR_ANKLE_R) || (where == WEAR_THUMB_R)
         || (where == WEAR_SHOULDER_L) || (where == WEAR_THIGH_L)
         || (where == WEAR_KNEE_L))
    if (GET_EQ(ch, where) && HAS_BODY(ch, (where + 1)))
      where++;

  if (where == WEAR_FINGER_L)
  {
    if (GET_EQ(ch, where) && HAS_BODY(ch, WEAR_THUMB_R))
      where = WEAR_THUMB_R;
    if (GET_EQ(ch, where) && HAS_BODY(ch, WEAR_THUMB_L))
      where = WEAR_THUMB_L;
  }

  if (where == WEAR_LEGS)
    if (GET_EQ(ch, where) && HAS_BODY(ch, WEAR_LEGS_2))
      where = WEAR_LEGS_2;

  if (where == WEAR_FEET)
    if (GET_EQ(ch, where) && HAS_BODY(ch, WEAR_FEET_2))
      where = WEAR_FEET_2;


  if (where == WEAR_FOCUS)
  {
    if ((GET_EQ(ch, WEAR_WIELD)) || (GET_EQ(ch, WEAR_WIELD_2)))
    {
      new_send_to_char(ch,
                       "You can't focus on anything while wielding a weapon.\r\n");
      return;
    }
  }

  if (where == WEAR_WIELD)
  {
    if (GET_EQ(ch, WEAR_FOCUS))
    {
      new_send_to_char(ch,
                       "You can't wield anything while focusing on something.\r\n");
      return;
    }
    if ((wep_hands(obj) == 2 || (GET_EQ(ch, WEAR_WIELD) && wep_hands(GET_EQ(ch, WEAR_WIELD)) == 2)) &&
        ((GET_EQ(ch, WEAR_SHIELD) || (!GET_SKILL(ch, SKILL_LONGARM) && (GET_EQ(ch, WEAR_WIELD_2) || GET_EQ(ch, WEAR_WIELD))))))
    {
      new_send_to_char(ch, "You cant wield a two handed weapon without two hands free, or the longarm skill.\r\n");
      return;
    }
    if (GET_EQ(ch, where) && GET_SKILL(ch, SKILL_DUAL)
        && !(GET_EQ(ch, WEAR_SHIELD)))
      where = WEAR_WIELD_2;
    else if (GET_EQ(ch, where) && GET_SKILL(ch, SKILL_DUAL)
             && (GET_EQ(ch, WEAR_SHIELD) ))
    {
      new_send_to_char
      (ch, "You can't wield a second weapon while wearing a shield.\r\n");
      return;
    }
    else if ((GET_EQ(ch, WEAR_WIELD))&& wep_hands(GET_EQ(ch, WEAR_WIELD)))
    {
      new_send_to_char
      (ch, "You can't wield a second weapon while wielding a two handed weapon.\r\n");
      return;
    }
  }

  if ((where == WEAR_SHIELD) && ((GET_EQ(ch, WEAR_WIELD_2))))
  {
    new_send_to_char
    (ch, "You can't wear a shield while wielding two weapons.\r\n");
    return;
  }

  if (GET_EQ(ch, where) )
  {
    if (!wearall && !OBJ_FLAGGED(GET_EQ(ch, where), ITEM_NODROP))
    {
      if (can_take_obj(ch, GET_EQ(ch, where)))
      {
        perform_remove(ch, where);
      }
      else return;
    }
    else
    {
      new_send_to_char(ch, "%s", already_wearing[where]);	//mord
      return;
    }
  }

  if (wear_otrigger(obj, ch, where) <= 0)
    return;

  wear_message(ch, obj, where);

  /* so that the first time newbie equip works */
  if (obj->carried_by)
    obj_from_char(obj);
  equip_char(ch, obj, where);
}



int find_eq_pos(struct char_data *ch, struct obj_data *obj, char *arg)
{
  int where = -1;
  extern const char *body[];

  if (!arg || !*arg)
  {
    if (CAN_WEAR(obj, ITEM_WEAR_FINGER))
      where = WEAR_FINGER_R;
    if (CAN_WEAR(obj, ITEM_WEAR_NECK))
      where = WEAR_NECK_1;
    if (CAN_WEAR(obj, ITEM_WEAR_BODY))
      where = WEAR_BODY;
    if (CAN_WEAR(obj, ITEM_WEAR_HEAD))
      where = WEAR_HEAD;
    if (CAN_WEAR(obj, ITEM_WEAR_LEGS))
      where = WEAR_LEGS;
    if (CAN_WEAR(obj, ITEM_WEAR_FEET))
      where = WEAR_FEET;
    if (CAN_WEAR(obj, ITEM_WEAR_HANDS))
      where = WEAR_HANDS;
    if (CAN_WEAR(obj, ITEM_WEAR_ARMS))
      where = WEAR_ARMS;
    if (CAN_WEAR(obj, ITEM_WEAR_SHIELD))
      where = WEAR_SHIELD;
    if (CAN_WEAR(obj, ITEM_WEAR_ABOUT))
      where = WEAR_ABOUT;
    if (CAN_WEAR(obj, ITEM_WEAR_WAIST))
      where = WEAR_WAIST;
    if (CAN_WEAR(obj, ITEM_WEAR_WRIST))
      where = WEAR_WRIST_R;
    if (CAN_WEAR(obj, ITEM_WEAR_FACE))
      where = WEAR_FACE;
    if (CAN_WEAR(obj, ITEM_WEAR_HIPS))
      where = WEAR_HIPS;
    if (CAN_WEAR(obj, ITEM_WEAR_EYES))
      where = WEAR_EYES;
    if (CAN_WEAR(obj, ITEM_WEAR_EAR))
      where = WEAR_EAR_R;
    if (CAN_WEAR(obj, ITEM_WEAR_ANKLE))
      where = WEAR_ANKLE_R;
    if (CAN_WEAR(obj, ITEM_WEAR_HORNS))
      where = WEAR_HORNS;
    if (CAN_WEAR(obj, ITEM_WEAR_ANTENNA))
      where = WEAR_ANTENNA;
    if (CAN_WEAR(obj, ITEM_WEAR_TAIL))
      where = WEAR_TAIL;
    if (CAN_WEAR(obj, ITEM_WEAR_FOCUS))
      where = WEAR_FOCUS;
    if (CAN_WEAR(obj, ITEM_WEAR_SHOULDER))
      where = WEAR_SHOULDER_L;
    if (CAN_WEAR(obj, ITEM_WEAR_CREST))
      where = WEAR_CREST;
    if (CAN_WEAR(obj, ITEM_WEAR_THIGH))
      where = WEAR_THIGH_L;
    if (CAN_WEAR(obj, ITEM_WEAR_KNEE))
      where = WEAR_KNEE_L;
    if (CAN_WEAR(obj, ITEM_WEAR_FLOATING))
      where = WEAR_FLOATING;

  }
  else
  {
  char arg1[MAX_INPUT_LENGTH];
  one_argument(arg, arg1);
    where = search_block(arg1, body, FALSE);
    if (!CAN_WEAR(obj, where_to_worn(where)))
      return -1;
    if (((where) < 0) || (*arg == '!'))
    {
      // sprintf(buf, "'%s'?  What part of your body is THAT?\r\n", arg);
      // send_to_char(buf, ch);
      return -1;
    }
  }

  return (where);
}


ACMD(do_wear)
{
  char arg1[MAX_INPUT_LENGTH]; /* item name */
  char *arg2; /* location name */
  struct obj_data *obj, *next_obj;
  int where, dotmode, items_worn = 0;

  skip_spaces(&argument);
  arg2 = str_until(argument, "on", arg1, sizeof(arg1));
  

  if (!*arg1)
  {
    send_to_char("Wear what?\r\n", ch);
    return;
  }
  dotmode = find_all_dots(arg1);

  if (*arg2 && (dotmode != FIND_INDIV))
  {
    send_to_char
    ("You can't specify the same body location for more than one item!\r\n",
     ch);
    return;
  }
  if (dotmode == FIND_ALL)
  {
    wearall = 1;
    for (obj = ch->carrying; obj; obj = next_obj)
    {
      next_obj = obj->next_content;
      if ((GET_LEVEL(ch) >= GET_OBJ_LEVEL(obj))&&CAN_SEE_OBJ(ch, obj)
          && (where = find_eq_pos(ch, obj, 0)) >= 0)
      {
        items_worn++;
        perform_wear(ch, obj, where);
      }
    }
    wearall = 0;
    if (!items_worn)
      send_to_char("You don't seem to have anything wearable.\r\n",
                   ch);
  }
  else if (dotmode == FIND_ALLDOT)
  {
    if (!*arg1)
    {
      send_to_char("Wear all of what?\r\n", ch);
      return;
    }
    if (!(obj = get_obj_in_list_vis(ch, arg1, NULL, ch->carrying)))
      new_send_to_char(ch, "You don't seem to have any %ss.\r\n", arg1);
    else if (GET_LEVEL(ch) < GET_OBJ_LEVEL(obj))
      new_send_to_char(ch, "You are not experienced enough to use that.\r\n");
    else
      wearall = 1;
    while (obj)
    {
      next_obj =
        get_obj_in_list_vis(ch, arg1, NULL, obj->next_content);
      if ((where = find_eq_pos(ch, obj, 0)) >= 0)
      {
        perform_wear(ch, obj, where);
      }
      else
        act("You can't wear $p.", FALSE, ch, obj, 0, TO_CHAR);
      obj = next_obj;
    }
    wearall = 0;
  }
  else
  {
    if (!(obj = get_obj_in_list_vis(ch, arg1, NULL, ch->carrying)))
    {
      new_send_to_char(ch, "You don't seem to have %s %s.\r\n", AN(arg1),
                       arg1);
    }
    else
    {
      if ((where = find_eq_pos(ch, obj, arg2)) >= 0)
      {
        perform_wear(ch, obj, where);
      }
      else if (!*arg2)
        act("You can't wear $p.", FALSE, ch, obj, 0, TO_CHAR);
      else
       new_send_to_char(ch, "You can't wear that there!\r\n");
    }
  }
}



ACMD(do_wield)
{
  struct obj_data *obj;
skip_spaces(&argument);

  if (!*argument)
    send_to_char("Wield what?\r\n", ch);
  else if (!(obj = get_obj_in_list_vis(ch, argument, NULL, ch->carrying)))
  {
    new_send_to_char(ch, "You don't seem to have %s %s.\r\n", AN(argument), argument);
  }
  else
  {
    if (!CAN_WEAR(obj, ITEM_WEAR_WIELD))
      send_to_char("You can't wield that.\r\n", ch);
    else if (GET_OBJ_WEIGHT(obj) >
             str_app[STRENGTH_APPLY_INDEX(ch)].wield_w)
      send_to_char("It's too heavy for you to use.\r\n", ch);
    else
      perform_wear(ch, obj, WEAR_WIELD);
  }
}

int where_to_worn(int where)
{
  int worn = -1;

  switch (where)
  {
  case WEAR_NECK_1:
  case WEAR_NECK_2:
    worn = ITEM_WEAR_NECK;
    break;
  case WEAR_BODY:
    worn = ITEM_WEAR_BODY;
    break;
  case WEAR_HEAD:
    worn = ITEM_WEAR_HEAD;
    break;
  case WEAR_LEGS:
    worn = ITEM_WEAR_LEGS;
    break;
  case WEAR_FEET:
    worn = ITEM_WEAR_FEET;
    break;
  case WEAR_HANDS:
    worn = ITEM_WEAR_HANDS;
    break;
  case WEAR_ARMS:
    worn = ITEM_WEAR_ARMS;
    break;
  case WEAR_SHIELD:
    worn = ITEM_WEAR_SHIELD;
    break;
  case WEAR_ABOUT:
    worn = ITEM_WEAR_ABOUT;
    break;
  case WEAR_WAIST:
    worn = ITEM_WEAR_WAIST;
    break;
  case WEAR_WRIST_R:
  case WEAR_WRIST_L:
    worn = ITEM_WEAR_WRIST;
    break;
  case WEAR_FACE:
    worn = ITEM_WEAR_FACE;
    break;
  case WEAR_HIPS:
    worn = ITEM_WEAR_HIPS;
    break;
  case WEAR_EYES:
    worn = ITEM_WEAR_EYES;
    break;
  case WEAR_EAR_R:
  case WEAR_EAR_L:
    worn = ITEM_WEAR_EAR;
    break;
  case WEAR_ANKLE_R:
  case WEAR_ANKLE_L:
    worn = ITEM_WEAR_ANKLE;
    break;
  case WEAR_HORNS:
    worn = ITEM_WEAR_HORNS;
    break;
  case WEAR_ANTENNA:
    worn = ITEM_WEAR_ANTENNA;
    break;
  case WEAR_TAIL:
    worn = ITEM_WEAR_TAIL;
    break;
  case WEAR_FOCUS:
    worn = ITEM_WEAR_FOCUS;
    break;
  case WEAR_THUMB_L:
  case WEAR_THUMB_R:
  case WEAR_FINGER_R:
  case WEAR_FINGER_L:
    worn = ITEM_WEAR_FINGER;
    break;
  case WEAR_SADDLE:
    worn = ITEM_WEAR_ABOUT;
    break;
  case WEAR_EAR_TIP:
    worn = ITEM_WEAR_EAR;
    break;
  case WEAR_SHOULDER_L:
  case WEAR_SHOULDER_R:
    worn = ITEM_WEAR_SHOULDER;
    break;
  case WEAR_CREST:
    worn = ITEM_WEAR_CREST;
    break;
  case WEAR_THIGH_L:
  case WEAR_THIGH_R:
    worn = ITEM_WEAR_THIGH;
    break;
  case WEAR_KNEE_L:
  case WEAR_KNEE_R:
    worn = ITEM_WEAR_KNEE;
    break;
  case WEAR_FLOATING:
    worn = ITEM_WEAR_FLOATING;
    break;
  }
  return worn;
}

ACMD(do_grab)
{
  struct obj_data *obj;
  skip_spaces(&argument);

  if (!*argument)
    send_to_char("Hold what?\r\n", ch);
  else if (!(obj = get_obj_in_list_vis(ch, argument, NULL, ch->carrying)))
  {
    new_send_to_char(ch, "You don't seem to have %s %s.\r\n", AN(argument), argument);
  }
  else
  {
    if (GET_OBJ_TYPE(obj) == ITEM_LIGHT)
      perform_wear(ch, obj, WEAR_LIGHT);
    else
    {
      if (!CAN_WEAR(obj, ITEM_WEAR_HOLD)
          && GET_OBJ_TYPE(obj) != ITEM_WAND
          && GET_OBJ_TYPE(obj) != ITEM_STAFF
          && GET_OBJ_TYPE(obj) != ITEM_SCROLL
          && GET_OBJ_TYPE(obj) != ITEM_POTION
          && GET_OBJ_TYPE(obj) != ITEM_ANTIDOTE_1
          && GET_OBJ_TYPE(obj) != ITEM_ANTIDOTE_2
          && GET_OBJ_TYPE(obj) != ITEM_ANTIDOTE_3)
        send_to_char("You can't hold that.\r\n", ch);
      else
        perform_wear(ch, obj, WEAR_HOLD);
    }
  }
}



void perform_remove(struct char_data *ch, int pos)
{
  struct obj_data *obj;


  if (!(obj = GET_EQ(ch, pos)))
    log("SYSERR: perform_remove: bad pos %d passed.", pos);
  else if (IS_OBJ_STAT(obj, ITEM_NODROP))
    act("You can't remove $p, it must be CURSED!", FALSE, ch, obj, 0,
        TO_CHAR);
  else if (IS_CARRYING_N(ch) >= CAN_CARRY_N(ch))
    act("$p: you can't carry that many items!", FALSE, ch, obj, 0,
        TO_CHAR);
  else
  {
    if ((pos == WEAR_WIELD) && GET_EQ(ch, WEAR_WIELD_2))
    {
      obj = GET_EQ(ch, WEAR_WIELD_2);
      pos = WEAR_WIELD_2;
    }
    if (remove_otrigger(obj, ch)<=0)
      return;
    LS_REMOVE = 1;
    obj_to_char((obj = unequip_char(ch, pos)), ch);
    LS_REMOVE = 0;
    if (obj != NULL)
    {
      act("You stop using $p.", FALSE, ch, obj, 0, TO_CHAR);
      act("$n stops using $p.", TRUE, ch, obj, 0, TO_ROOM);
    }
    else
    {
      new_send_to_char(ch, "Somethings wrong.\r\n");
    }
  }



}



ACMD(do_remove)
{
  int i, dotmode, found;
  char arg[MAX_INPUT_LENGTH];

  skip_spaces(&argument);

  if (!*argument)
  {
    send_to_char("Remove what?\r\n", ch);
    return;
  }
  dotmode = find_all_dots(argument);

  if (dotmode == FIND_ALL)
  {
    found = 0;
    for (i = 0; i < NUM_WEARS; i++)
      if (GET_EQ(ch, i))
      {
        perform_remove(ch, i);
        found = 1;
      }
    if (!found)
      send_to_char("You're not using anything.\r\n", ch);
  }
  else if (dotmode == FIND_ALLDOT)
  {
    if (!*arg)
      send_to_char("Remove all of what?\r\n", ch);
    else
    {
      found = 0;
      for (i = 0; i < NUM_WEARS; i++)
        if (GET_EQ(ch, i) && CAN_SEE_OBJ(ch, GET_EQ(ch, i)) &&
            isname_full(argument, GET_EQ(ch, i)->name))
        {
          perform_remove(ch, i);
          found = 1;
        }
      if (!found)
      {
        new_send_to_char(ch, "You don't seem to be using any %ss.\r\n",
                         arg);
      }
    }
  }
  else
  {
    if ((i =
           get_obj_pos_in_equip_vis(ch, argument, NULL, ch->equipment)) < 0)
      new_send_to_char(ch, "You don't seem to be using %s %s.\r\n",
                       AN(argument), argument);
    else
      perform_remove(ch, i);
  }
}

ACMD(do_pull)
{
  char arg[MAX_INPUT_LENGTH];
  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("Pull what?\r\n", ch);
    return;
  }
  send_to_char("You can't pull that.\r\n", ch);
  return;
  /* for now only pull pin will work and must be wielding a grenade */
  if (!str_cmp(arg, "pin"))
  {

    if (GET_EQ(ch, WEAR_WIELD))
    {
      if (GET_OBJ_TYPE(GET_EQ(ch, WEAR_WIELD)) == ITEM_GRENADE)
      {
        send_to_char
        ("You pull the pin, the grenade is activated!\r\n",
         ch);
        SET_BIT_AR(GET_OBJ_EXTRA(GET_EQ(ch, WEAR_WIELD)),
                   ITEM_LIVE_GRENADE);
      }
      else
        send_to_char("That's NOT a grenade!\r\n", ch);
    }
    else
      send_to_char("You aren't wielding anything!\r\n", ch);
  }
  else
    return;

  /* put other 'pull' options here later */
  return;
}

ACMD(do_compare)
{
  char arg1[MAX_STRING_LENGTH];
  char arg2[MAX_STRING_LENGTH];
  struct obj_data *obj1;
  struct obj_data *obj2;
  int value1;
  int value2;
  char *msg;

  two_arguments(argument, arg1, arg2);

  if (arg1[0] == '\0')
  {
    send_to_char("Compare what to what?\n\r", ch);
    return;
  }

  if (!(obj1 = get_obj_in_list_vis(ch, arg1, NULL, ch->carrying)))
  {
    send_to_char("You do not have that item.\n\r", ch);
    return;
  }

  if (arg2[0] == '\0')
  {
    for (obj2 = ch->carrying; obj2; obj2 = obj2->next_content)
    {
      if (!((obj2->obj_flags.type_flag == ITEM_WEAPON) ||
            (obj2->obj_flags.type_flag == ITEM_FIREWEAPON) ||
            (obj2->obj_flags.type_flag == ITEM_ARMOR) ||
            (obj2->obj_flags.type_flag == ITEM_WORN))
          && CAN_SEE_OBJ(ch, obj2)
          && obj1->obj_flags.type_flag == obj2->obj_flags.type_flag
          && CAN_GET_OBJ(ch, obj2))
        break;
    }

    if (!obj2)
    {
      send_to_char("You aren't wearing anything comparable.\n\r",
                   ch);
      return;
    }
  }
  else
  {
    if (!(obj2 = get_obj_in_list_vis(ch, arg2, NULL, ch->carrying)))
    {
      send_to_char("You do not have that item.\n\r", ch);
      return;
    }
  }

  msg = NULL;
  value1 = 0;
  value2 = 0;

  if (obj1 == obj2)
  {
    msg = "You compare $p to itself.  It looks about the same.";
  }
  else if (obj1->obj_flags.type_flag != obj2->obj_flags.type_flag)
  {
    msg = "You can't compare $p and $P.";
  }
  else
  {
    switch (obj1->obj_flags.type_flag)
    {
    default:
      msg = "You can't compare $p and $P.";
      break;

    case ITEM_ARMOR:
      value1 = obj1->obj_flags.value[0];
      value2 = obj2->obj_flags.value[0];
      break;

    case ITEM_WEAPON:
      value1 = obj1->obj_flags.value[1] + obj1->obj_flags.value[2];
      value2 = obj2->obj_flags.value[1] + obj2->obj_flags.value[2];
      break;
    }
  }

  if (!msg)
  {
    if (value1 == value2)
      msg = "$p and $P look about the same.";
    else if (value1 > value2)
      msg = "$p looks better than $P.";
    else
      msg = "$p looks worse than $P.";
  }

  act(msg, FALSE, ch, obj1, obj2, TO_CHAR);
  return;
}

void start_auction(struct char_data *ch, struct obj_data *obj, int bid)
{
  /* Take object from character and set variables */
  char buf[MAX_INPUT_LENGTH];
  obj_from_char(obj);
  obj_selling = obj;
  ch_selling = ch;
  ch_buying = NULL;
  curbid = bid;

  /* Tell th character where his item went */
  new_send_to_char(ch,
                   "%s magicly flies away from your hands to be auctioned!\r\n",
                   obj_selling->short_description);


  /* Anounce the item is being sold */
  snprintf(buf, sizeof(buf), auctioneer[AUC_NULL_STATE], curbid);
  auc_send_to_all(buf, FALSE);

  aucstat = AUC_OFFERING;
}

void check_auction(void)
{
  char buf[MAX_INPUT_LENGTH];
  switch (aucstat)
  {
  case AUC_NULL_STATE:
    return;
  case AUC_OFFERING:
    snprintf(buf, sizeof(buf), auctioneer[AUC_OFFERING], curbid);
    CAP(buf);
    auc_send_to_all(buf, FALSE);
    aucstat = AUC_GOING_ONCE;
    return;
  case AUC_GOING_ONCE:
    snprintf(buf, sizeof(buf), auctioneer[AUC_GOING_ONCE], curbid);
    CAP(buf);
    auc_send_to_all(buf, FALSE);
    aucstat = AUC_GOING_TWICE;
    return;
  case AUC_GOING_TWICE:
    snprintf(buf, sizeof(buf), auctioneer[AUC_GOING_TWICE], curbid);
    CAP(buf);
    auc_send_to_all(buf, FALSE);
    aucstat = AUC_LAST_CALL;
    return;
  case AUC_LAST_CALL:
    if (ch_buying == NULL)
    {
      snprintf(buf, sizeof(buf), auctioneer[AUC_LAST_CALL]);

      CAP(buf);
      auc_send_to_all(buf, FALSE);

      new_send_to_char(ch_selling, "%s flies out the sky and into your hands.\r\n",
                       obj_selling->short_description);

      obj_to_char(obj_selling, ch_selling);

      /* Reset auctioning values */
      obj_selling = NULL;
      ch_selling = NULL;
      ch_buying = NULL;
      curbid = 0;
      aucstat = AUC_NULL_STATE;
      return;
    }
    else
    {
      snprintf(buf, sizeof(buf), auctioneer[AUC_SOLD], curbid);
      auc_send_to_all(buf, TRUE);

      /* Give the object to the buyer */
      obj_to_char(obj_selling, ch_buying);
      new_send_to_char(ch_buying,
                       "%s flies out the sky and into your hands, what a steel!\r\n",
                       obj_selling->short_description);


      new_send_to_char(ch_selling, "Congrats! You have sold %s for %d coins!\r\n",
                       obj_selling->short_description, curbid);

      /* Give selling char the money for his stuff */
      char_gold(ch_selling, curbid, GOLD_ALL);
      gold_data(AUCTION_OUT, curbid);

      /* Reset auctioning values */
      obj_selling = NULL;
      ch_selling = NULL;
      ch_buying = NULL;
      curbid = 0;
      aucstat = AUC_NULL_STATE;
      return;
    }
  }
}


bool can_auction_item(struct char_data * ch, struct obj_data * obj)
{
  if (GET_OBJ_TYPE(obj) == ITEM_WORN || GET_OBJ_TYPE(obj) == ITEM_OTHER
      || GET_OBJ_TYPE(obj) == ITEM_TRASH
      || GET_OBJ_TYPE(obj) == ITEM_TRAP || GET_OBJ_TYPE(obj) == ITEM_NOTE
      || GET_OBJ_TYPE(obj) == ITEM_KEY || GET_OBJ_TYPE(obj) == ITEM_FOOD
      || GET_OBJ_TYPE(obj) == ITEM_MONEY || GET_OBJ_TYPE(obj) == ITEM_PEN
      || GET_OBJ_TYPE(obj) == ITEM_BOAT
      || GET_OBJ_TYPE(obj) == ITEM_FOUNTAIN
      || GET_OBJ_TYPE(obj) == ITEM_WEAPON
      || GET_OBJ_TYPE(obj) == ITEM_ARMOR)
    return (TRUE);
  else
    return (FALSE);
}

ACMD(do_auction)
{
  char arg1[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];
  struct obj_data *obj;
  int bid = 0, fee = 0;

  new_send_to_char(ch, "Sorry, but auto-auction has been disabled. \r\nTry your skills at a manual auction using AUCTALK.\r\n");
  return;


  two_arguments(argument, arg1, arg2);

  if (!*arg1)
  {
    send_to_char("Auction what?\r\n", ch);
    return;
  }
  else if (is_abbrev(arg1, "cancel") || is_abbrev(arg1, "stop"))
  {
    if ((ch != ch_selling && GET_LEVEL(ch) < LVL_GRGOD)
        || aucstat == AUC_NULL_STATE)
    {
      send_to_char("You're not even selling anything!\r\n", ch);
      return;
    }
    else if (ch == ch_selling)
    {
      stop_auction(AUC_NORMAL_CANCEL, NULL);
      return;
    }
    else
    {
      stop_auction(AUC_WIZ_CANCEL, ch);
    }
  }
  else if (is_abbrev(arg1, "stats") || is_abbrev(arg1, "identify"))
  {
    auc_stat(ch, obj_selling);
    return;
  }
  else if (!(obj = get_obj_in_list_vis(ch, arg1, NULL, ch->carrying)))
  {
    new_send_to_char(ch, "You don't seem to have %s %s.\r\n", AN(arg1), arg1);
    return;
  }
  else if (!*arg2 && (bid = obj->obj_flags.cost) <= 0)
  {
    new_send_to_char(ch, "What should be the minimum bid?\r\n");
    return;
  }
  else if (*arg2 && (bid = atoi(arg2)) <= 0)
  {
    send_to_char("Come on? One coin at least?\r\n", ch);
    return;
  }
  else if (aucstat != AUC_NULL_STATE)
  {
    new_send_to_char(ch,
                     "Sorry, but %s is already auctioning %s at %d coins!\r\n",
                     GET_NAME(ch_selling), obj_selling->short_description, bid);
    return;
  }
  else if (OBJ_FLAGGED(obj, ITEM_NOSELL) || !can_auction_item(ch, obj))
  {
    send_to_char("Sorry but you can't sell that!\r\n", ch);
    return;
  }
  else
  {
    new_send_to_char(ch, "%s", CONFIG_OK);
    fee = bid / 10;
    if (char_gold(ch, 0, GOLD_ALL) < fee)
    {
      send_to_char("Sorry but you can't pay the auction fees.\r\n",
                   ch);
      return;
    }
    else
      char_gold(ch, fee, GOLD_ALL);
    start_auction(ch, obj, bid);
    return;
  }
}


ACMD(do_bid)
{
  char arg[MAX_INPUT_LENGTH];
  int bid;
  char buf[MAX_INPUT_LENGTH];

  if (IS_NPC(ch))
    return;

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("Bid yes, good idea, but HOW MUCH??\r\n", ch);
    return;
  }
  else if (aucstat == AUC_NULL_STATE)
  {
    send_to_char
    ("Thats very enthusiastic of you, but nothing is being SOLD!\r\n",
     ch);
    return;
  }
  else if (ch == ch_selling)
  {
    send_to_char
    ("Why bid on something your selling?  You can 'cancel' the auction!\r\n",
     ch);
    return;
  }
  else if ((bid = atoi(arg)) < ((int) curbid * 1.1 - 1)
           && ch_buying != NULL)
  {
    new_send_to_char(ch,
                     "You must bid at least 10 percent more than the current bid. (%d)\r\n",
                     (int) (curbid * 1.1));
    return;
  }
  else if (ch_buying == NULL && bid < curbid)
  {
    new_send_to_char(ch, "You must at least bid the minimum!\r\n");
    return;
  }
  else if (bid > (char_gold(ch, 0, GOLD_ALL)))
  {
    send_to_char("You don't have that much gold!\r\n", ch);
    return;
  }
  else
  {
    if (ch == ch_buying)
    {
      char_gold(ch, -(bid - curbid), GOLD_ALL);
      gold_data(AUCTION_IN, (bid - curbid));
    }
    else
    {
      char_gold(ch, -bid, GOLD_ALL);
      auction_in += bid;

      if (!(ch_buying == NULL))
      {
        char_gold(ch_buying, curbid, GOLD_ALL);
        gold_data(AUCTION_IN, (curbid));
      }
    }

    curbid = bid;
    ch_buying = ch;


    snprintf(buf, sizeof(buf), auctioneer[AUC_BID], bid);
    auc_send_to_all(buf, TRUE);

    aucstat = AUC_OFFERING;
    return;
  }
}

void stop_auction(int type, struct char_data *ch)
{
  char buf[MAX_INPUT_LENGTH];
  switch (type)
  {
  case AUC_NORMAL_CANCEL:
    snprintf(buf, sizeof(buf), auctioneer[AUC_NORMAL_CANCEL]);
    auc_send_to_all(buf, FALSE);
    break;
  case AUC_QUIT_CANCEL:
    snprintf(buf, sizeof(buf), auctioneer[AUC_QUIT_CANCEL]);
    auc_send_to_all(buf, FALSE);
    break;
  case AUC_WIZ_CANCEL:
    snprintf(buf, sizeof(buf), auctioneer[AUC_WIZ_CANCEL]);
    auc_send_to_all(buf, FALSE);
    break;
  default:
    if (ch)
      send_to_char
      ("Sorry, that is an unrecognised cancel command, please report.",
       ch);
    return;
  }


  if (type != AUC_WIZ_CANCEL)
  {
    new_send_to_char(ch, "%s flies out the sky and into your hands.\r\n",
                     obj_selling->short_description);
    obj_to_char(obj_selling, ch_selling);
  }
  else
  {
    new_send_to_char(ch, "%s flies out the sky and into your hands.\r\n",
                     obj_selling->short_description);
    obj_to_char(obj_selling, ch);
  }

  if (!(ch_buying == NULL))
  {
    char_gold(ch_buying, curbid, GOLD_ALL);
    gold_data(AUCTION_OUT, curbid);
  }

  obj_selling = NULL;
  ch_selling = NULL;
  ch_buying = NULL;
  curbid = 0;

  aucstat = AUC_NULL_STATE;

}

void auc_stat(struct char_data *ch, struct obj_data *obj)
{

  char buf[MAX_INPUT_LENGTH];
  if (aucstat == AUC_NULL_STATE)
  {
    send_to_char("Nothing is being auctioned!\r\n", ch);
    return;
  }
  else if (ch == ch_selling)
  {
    send_to_char
    ("You should have found that out BEFORE auctioning it!\r\n",
     ch);
    return;
  }
  else if (char_gold(ch, 0, GOLD_ALL) < 500)
  {
    send_to_char
    ("You can't afford to find the stats on that, it costs 500 coins!\r\n",
     ch);
    return;
  }
  else
  {
    /* auctioneer tells the character the auction details */
    send_to_char(CCRED(ch, C_SPR), ch);
    snprintf(buf, sizeof(buf), auctioneer[AUC_STAT], curbid);

    act(buf, TRUE, ch_selling, obj, ch, TO_VICT | TO_SLEEP);
    send_to_char(CCNRM(ch, C_SPR), ch);
    char_gold(ch, -500, GOLD_ALL);
    auction_in += 500;

    call_magic(ch, NULL, obj_selling, 0, SPELL_IDENTIFY, 30,
               CAST_SPELL);
  }
}

void auc_send_to_all(char *messg, bool buyer)
{
  struct descriptor_data *i;

  if (messg == NULL)
    return;

  for (i = descriptor_list; i; i = i->next)
  {
    if (STATE(i) == CON_PLAYING && i->character)
    {
      if (PRF_FLAGGED(i->character, PRF_NOAUCT))
        continue;

      send_to_char(CCMAG(i->character, C_SPR), i->character);

      if (buyer)
        act(messg, TRUE, ch_buying, obj_selling, i->character,
            TO_VICT | TO_SLEEP);
      else
        act(messg, TRUE, ch_selling, obj_selling, i->character,
            TO_VICT | TO_SLEEP);

      send_to_char(CCNRM(i->character, C_SPR), i->character);
    }
  }
}



/*speed functions*/
int speed_update(struct char_data *ch)
{

  float speed = 0.0;
  float var = 0.0;
  int weps = has_weapon(ch);



  if (IS_NPC(ch)) //npc speed
    speed += ((GET_LEVEL(ch)-(30 - (2 * MOB_TIER(ch)))) * (5 + (2 *  MOB_TIER(ch))));
  else
  { // start of player speed


    speed += class_speed(ch);
    speed += race_speed(ch);
    speed += GET_ADD(ch);
    speed += ((GET_MOVE(ch)*300)/GET_MAX_MOVE(ch))- 200;


    speed += (((GET_DEX(ch) * 200) / 22)-130);
    speed += AFF_SPEED(ch);
    if (( GET_SUB(ch, SUB_LOYALSPEED) )> 0)
      speed += 50;
    if (CAN_CARRY_W(ch) > 0 && IS_CARRYING_W(ch) >= 0)
    {
      if (IS_CARRYING_W(ch) == 0)
        var = 200.0f;
      else if (IS_CARRYING_W(ch) < CAN_CARRY_W(ch))
        var = 300  - (((IS_CARRYING_W(ch) * 400) / CAN_CARRY_W(ch)));
      else
        var = -200;

    }
    speed += var;


    if (RIDING(ch) && HERE(RIDING(ch), ch) && GET_SKILL(ch, SKILL_MOUNTED_COMBAT))
      speed += 50 * 1.0 + (total_chance(ch, SKILL_MOUNTED_COMBAT) * 0.0025);
    else if (GET_RACE(ch) == RACE_CENTAUR && GET_SKILL(ch, SKILL_MOUNTED_COMBAT))
      speed += 50 * 1.0 + (total_chance(ch, SKILL_MOUNTED_COMBAT) * 0.0020);
    // end of player speed
  }
  if (weps)
  {
    speed += get_weapon_speed(GET_EQ(ch, WEAR_WIELD));
    speed += get_weapon_speed(GET_EQ(ch, WEAR_WIELD_2));
  }


  if (RIDDEN_BY(ch) && HERE(RIDDEN_BY(ch), ch))
    speed *= 0.75;

  if (!IS_NPC(ch))
    speed += (GET_HITROLL(ch) * 2);
  else
    speed += (GET_HITROLL(ch) * (MOB_TIER(ch) * 0.5));

  /*add in spell modifyers below*/

  if (AFF_FLAGGED(ch, AFF_FROZEN))
    speed -= abs((int)speed)/ 3.0f;
  if (AFF_FLAGGED(ch, AFF_FREEZING))
    speed *= 0.5;

  speed = IRANGE(-1000.0, speed, 1000.0);

  if (!IS_NPC(ch))
    GET_SPEED(ch) = (int)speed;

  return (int)speed;

}



int class_speed(struct char_data *ch)
{
  int c, speed = 0;
  float armorcost = 0.0;
  int compute_armor_class(struct char_data *ch);
  c = GET_CLASS(ch);
  switch (c)
  {
  case CLASS_MAGE:
  case CLASS_PRIEST:
  case CLASS_ESPER:
    speed = 10;
    armorcost = 2.5;
    break;
  case CLASS_RANGER:
  case CLASS_GYPSY:
  case CLASS_THIEF:
    speed = 70;
    armorcost = 1;
    break;
  case CLASS_WARRIOR:
  case CLASS_HUNTER:
    speed = 10;
    armorcost = 0.65;
    break;
  default:
    speed = 10;
    break;
  }

  speed *= (1.0  + (current_class_is_tier_num(ch) * 0.5));

  speed += (( 100 + (compute_armor_class(ch)))-200) * armorcost;

  return (speed);
}

int race_speed(struct char_data *ch)
{
  /*
  "Select a race:\r\n"
  "  [F]aun\r\n"
  "  [C]entaur\r\n"
  "  [E]lf\r\n"
  "  [D]warf\r\n"
  "  [I]ndian\r\n"
  "  [G]ringo\r\n"
  "  [M]artian\r\n"
  "  [S]pace-wolf\r\n";
  */

  switch (GET_RACE(ch))
  {
  case RACE_FAUN:
    return 100;
    break;
  case RACE_CENTAUR:
    return 150;
    break;
  case RACE_ELF:
    return 150;
    break;
  case RACE_DWARF:
    return 100;
    break;
  case RACE_INDIAN:
    return 100;
    break;
  case RACE_GRINGO:
    return 100;
    break;
  case RACE_MARTIAN:
    return 180;
    break;
  case RACE_SPACE_WOLF:
    return 120;
    break;
  default:
    return 50;
    break;
  }

}
