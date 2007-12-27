/************************************************************************
* auction.c   An automated auctioning system based on CircleMUD 3.0  *
*        bpl12 with mobile auctioneer and queue of items.  *
*                                           *
* Copyright (C) 1996,1997 by George Greer                  *
************************************************************************/

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "auction.h"

/*
 * Local global variables.
 */
struct auction_data *auction;


/*
 * Function prototypes.
 */
void free_auction(struct auction_data *auc);
void auction_forfeit(struct char_data *mob);
void auction_reset(void);
int is_on_block(struct obj_data *obj);
struct obj_data *check_obj(struct char_data *ch, struct obj_data *obj);
struct char_data *check_ch(struct char_data *ch);
void show_auction_status(struct char_data *ch);
ACMD(do_gen_comm);
struct char_data *get_char_auc(char *name);

bool can_auction_item(struct char_data * ch, struct obj_data * obj);

/*
 * External global variables.
 */
extern struct char_data *character_list;
extern struct room_data *world;

void identify_object(CHAR_DATA *ch, OBJ_DATA *obj);
/*
 * User tweakables.
 */
#define AUC_MOB         "auctioneer"
#define AUC_LIMIT  10

/* -------------------------------------------------------------------------- */

int valid_auction(struct auction_data *ac)
{
  if (ac->ticks == AUC_NONE)
    return FALSE;
  if (check_ch(ac->seller) == NULL)
    return FALSE;
  if (check_obj(ac->seller, ac->obj) == NULL)
    return FALSE;
  return TRUE;     /* Auction is ok to go. */
}

void auction_update(void)
{
  char buf2[MAX_STRING_LENGTH];
  struct char_data *mob = NULL;

  if (auction && auction->ticks == AUC_NONE)
    while (auction && !valid_auction(auction)) {
      struct auction_data *free_auc = auction;
      auction = auction->next;
      free_auction(free_auc);
    }

  if (auction == NULL)
    return;

  /*
   * Can't be static, the mob may have died. :)
   */
  mob = get_char_auc(AUC_MOB);

  /* Seller with us? */
  if (!check_ch(auction->seller)) {
    if (mob)
      do_gen_comm(mob, "The seller is no longer with us.", 0, SCMD_AUCTION);
    auction_reset();
    return;
  /* Seller's object? */
  } else if (!check_obj(auction->seller, auction->obj)) {
    auction_forfeit(mob);
    return;
  /* Make sure bidder exists */
  } else if (auction->bidder && !check_ch(auction->bidder)) {
    if (mob)
      do_gen_comm(mob, "The bidder has left, restarting.", 0, SCMD_AUCTION);
    auction->ticks = AUC_BID;
    auction->bidder = NULL;
    return;
  }

  /* Seller and bidder exist and seller has object */
  switch (auction->ticks) {
  case AUC_NEW:
  case AUC_BID:
  case AUC_ONCE:
  case AUC_TWICE:
    if (auction->bidder)
      snprintf(buf2, sizeof(buf2), "%s for %ld coin%sto %s.%s%s",
          auction->obj->short_description, auction->bid,
          auction->bid != 1 ? "s " : " ",
          GET_NAME(auction->bidder),
          auction->ticks == AUC_ONCE ? " ONCE!" : "",
          auction->ticks == AUC_TWICE ? " TWICE!!" : "");
    else if (auction->ticks == AUC_TWICE) {
      snprintf(buf2, sizeof(buf2), "%s withdrawn, no interest.",
          auction->obj->short_description);
      auction_reset();
      auction->ticks--;
    } else
      snprintf(buf2, sizeof(buf2), "%s%s, %ld coin%sminimum.",
      auction->ticks == AUC_NEW ? "New item: " : "",
          auction->obj->short_description, auction->bid, auction->bid != 1 ?
          "s " : " ");
    if (mob)
      do_gen_comm(mob, buf2, 0, SCMD_AUCTION);
    auction->ticks++;
    return;

  case AUC_SOLD:
    snprintf(buf2, sizeof(buf2), "%s SOLD to %s for %ld coin%s",
        auction->obj->short_description,
        GET_NAME(auction->bidder),
        auction->bid, auction->bid != 1 ? "s." : ".");
    if (mob)
      do_gen_comm(mob, buf2, 0, SCMD_AUCTION);

    /* Make sure object exists */
    
    if ((auction->obj = check_obj(auction->seller, auction->obj))) {
      if (char_gold(auction->bidder, 0, GOLD_HAND) < auction->bid) {
        act("You cannot afford to buy the $p from $N. Auction Canceled.", FALSE, auction->seller, auction->obj, auction->bidder, TO_CHAR);
        act("$n cannot afford to buy the $p from you. Auction Canceled.", FALSE, auction->seller, auction->obj, auction->bidder, TO_VICT);
      } else {
      /* Swap gold */
        char_gold(auction->bidder, -auction->bid, GOLD_HAND);
        char_gold(auction->seller,  auction->bid, GOLD_HAND);
      obj_from_char(auction->obj);
      obj_to_char(auction->obj, auction->bidder);

      act("A small daemon pops in, takes $p, hands you some gold, and leaves.",
         FALSE, auction->seller, auction->obj, 0, TO_CHAR);
      act("A small daemon pops in, gives you $p, takes some gold, and leaves.",
         FALSE, auction->bidder, auction->obj, 0, TO_CHAR);
      if (auction->bidder->in_room == auction->seller->in_room)
        act("A small daemon pops in, takes $p from $n, takes some gold "
        "from $N, gives $p to $N, and gives some gold to $n.",
         FALSE, auction->seller, auction->obj, auction->bidder,
         TO_NOTVICT);
      else {
        act("A small daemon pops in, takes $p and gives some gold from $n, and leaves.",
         FALSE, auction->seller, auction->obj, auction->bidder,
         TO_ROOM);
        act("A small daemon pops in, takes some gold and gives $p to $n, and leaves.",
         FALSE, auction->bidder, auction->obj, auction->bidder,
         TO_ROOM);
      }
      }
    } else
      auction_forfeit(mob);
    auction_reset();
    return;
  }
}

ACMD(do_bid)
{
  long bid;
  struct char_data *mob = get_char_auc((char *)AUC_MOB);
  int num = 0, i = 0;
  char buf[MAX_INPUT_LENGTH], buf1[MAX_INPUT_LENGTH];
  struct auction_data *auc = auction;

  if (!auc) {
    new_send_to_char(ch, "Nothing has been auctioned!\r\n");
    return;
  }
  
  two_arguments(argument, buf, buf1);
  bid = atoi(buf);
  if (*buf1) {
    if (*buf1 == '#') {
      if ((num = atoi(buf1 + 1)) != 0)
        for (auc = auction, i = 0; auc && i != num; auc = auc->next, i++)
          if (!auc->next)
            break;
    } else
      for (auc = auction, i = 0; auc && i < AUC_LIMIT; auc = auc->next, i++) {
    if (!auc->obj) {
      log("do_bid: ack, no object");
          break;
    } else if (isname(buf1, auc->obj->name)) {
          num = i;
          break;
        }
      }
  }

  if (i != num) {
    new_send_to_char(ch, "That number does not exist.\r\n");
    return;
  } else if (i == 0)
    *buf1 = '\0';

  if (auc->ticks == AUC_NONE) {
    new_send_to_char(ch, "Nothing is up for sale.\r\n");
    return;
  }
  if (!*buf) {
  new_send_to_char(ch, "Current bid: %ld coin%s\r\n", auc->bid,
        auc->bid != 1 ? "s." : ".");
   } else if (isname(buf, "stat")) {
     gold_int bid_stat = 0;
     if (auc->bid < 1000)
       bid_stat = 1000;
     else
       bid_stat = auc->bid * 0.1;
     if (char_gold(ch, 0, GOLD_HAND) < bid_stat) {
       new_send_to_char(ch, "You need at least %lld gold to stat that item.\r\n", bid_stat);
       return;
     }
     char_gold(ch, -bid_stat, GOLD_HAND);
     identify_object(ch, auc->obj);
  }
  else if (ch == auc->bidder)
    new_send_to_char(ch, "You're trying to outbid yourself.\r\n");
  else if (ch == auc->seller)
    new_send_to_char(ch, "You can't bid on your own item.\r\n");
  else if (!isname(buf1, auc->obj->name) && *buf1 && num == 0)
    new_send_to_char(ch, "That object is not for sale currently.\r\n");
  else if (bid < auc->bid && !auc->bidder) {
    new_send_to_char(ch, "You have to bid over the minimum of %ld coin%s\r\n",
    auc->bid, auc->bid != 1 ? "s." : ".");
  } else if ((bid < (auc->bid * 1.05) && auc->bidder) || bid == 0) {
    new_send_to_char(ch, "Try bidding at least 5%% over the current bid of %ld. (%.0f coins).\r\n",
        auc->bid, auc->bid * 1.05 + 1);
  } else if (char_gold(ch, 0, GOLD_HAND) < bid) {
    new_send_to_char(ch, "You have only %lld coins on hand.\r\n", char_gold(ch, 0, GOLD_HAND));
  } else if (PLR_FLAGGED(ch, PLR_NOSHOUT))
    new_send_to_char(ch, "You can't auction.\r\n");
  else if (mob == NULL)
    new_send_to_char(ch, "The auctioneer seems to be dead.\r\n");
  else {
    if (auc->bidder) {
      new_send_to_char(auc->bidder, "%s has placed a %ld bid over your %ld bid for %s.\r\n",
    GET_NAME(ch), bid, auc->bid, auc->obj->short_description);
    }
    auc->bid = bid;
    auc->bidder = ch;
    auc->ticks = AUC_BID;
    if (i == 0) {
      char buf2[MAX_INPUT_LENGTH];
      snprintf(buf2, sizeof(buf2), "%ld coin", bid);
      if (auc->bid != 1) strcat(buf2, "s."); else strcat(buf2, ".");
      do_gen_comm(ch, buf2, 0, SCMD_AUCTION);
    } else {
      new_send_to_char(ch, "A %ld coin bid placed on %s.\r\n", bid,
         auc->obj->short_description);
    }
  }
}

ACMD(do_auction)
{
  struct obj_data *obj;
  struct char_data *mob = get_char_auc(AUC_MOB);
  struct auction_data *auc_add;
  char buf1[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];
  int i = 0;

  two_arguments(argument, buf1, buf2);

  if (!*buf1) {
    new_send_to_char(ch, "Format: auction item [min]\r\n"
          "        auction list/status\r\n");
    if (GET_LEVEL(ch) >= LVL_GOD)
      new_send_to_char(ch, "        auction stop\r\n");
    new_send_to_char(ch, "Auction what for what minimum?\r\n");
  } else if (isname(buf1, "list status"))
    show_auction_status(ch);

  else if (GET_LEVEL(ch) >= LVL_GOD && is_abbrev(buf1, "stop"))
    auction_reset();
  else if ((obj = get_obj_in_list_vis(ch, buf1, NULL, ch->carrying)) == NULL) {
    new_send_to_char(ch, "You don't seem to have that to sell.\r\n");
  } else if (is_on_block(obj))
    new_send_to_char(ch, "You are already auctioning that!\r\n");
  else if (OBJ_FLAGGED(obj, ITEM_NOSELL) || !can_auction_item(ch, obj))
    new_send_to_char(ch, "You can't auction that item.\r\n");
  else if (mob == NULL)
    new_send_to_char(ch, "The auctioneer seems to be dead.\r\n");
  else if((atoi(buf2) != 0 ? atoi(buf2) : 1)<0){
    new_send_to_char(ch, "Try a positive amount instead.\r\n");
  }
  else {
    struct auction_data *n_auc;
    for (n_auc = auction; n_auc && n_auc->next; n_auc = n_auc->next, i++);
    if (i >= AUC_LIMIT) {
      new_send_to_char(ch, "Sorry, only %d items allowed on the block at a time.\r\n", AUC_LIMIT);
      return;
    }
    CREATE(auc_add, struct auction_data, 1);
    auc_add->ticks = AUC_NEW;
    auc_add->seller = ch;
    auc_add->bid = (atoi(buf2) != 0 ? atoi(buf2) : 1);
    auc_add->obj = obj;
    auc_add->bidder = NULL;
    auc_add->next = NULL;
    if (n_auc)
      n_auc->next = auc_add;
    else
      auction = auc_add;     /* Making the list. */
    if (i)
    new_send_to_char(ch, "Auction noted, currently %d item%sahead of yours.\r\n",
    i, i == 1 ? " " : "s ");
    else
      new_send_to_char(ch, "Auction noted.\r\n");
  }
}

void auction_reset(void)
{
  auction->bidder = NULL;
  auction->seller = NULL;
  auction->obj = NULL;
  auction->ticks = AUC_NONE;
  auction->bid = 0;
}

struct obj_data *check_obj(struct char_data *ch, struct obj_data *obj)
{
  struct obj_data *ch_obj;

  for (ch_obj = ch->carrying; ch_obj; ch_obj = ch_obj->next_content)
    if (ch_obj->item_number == obj->item_number)
      return ch_obj;

  return NULL;
}



void auction_forfeit(struct char_data *mob)
{
  /* Item not found */
  if (mob) {
    char buf2[MAX_STRING_LENGTH];
    snprintf(buf2, sizeof(buf2), "%s no longer holds object, auction is forfeit.", GET_NAME(auction->seller));
    do_gen_comm(mob, buf2, 0, SCMD_AUCTION);
  }
  new_send_to_char(auction->seller, "A small daemon pops in, takes some gold, and taunts you.\r\n" );
  act("A small daemon pops in, takes some gold from $n, and sticks its tongue out at $m.",
    FALSE, auction->seller, auction->obj, 0, TO_ROOM);
  GET_GOLD(auction->seller) -= auction->bid;
  auction_reset();
}

void show_auction_status(struct char_data *ch)
{
  struct auction_data *auc;
  int i = 1;

  new_send_to_char(ch, "Items up for auction:\r\n");
  for(auc = auction, i = 0; auc; auc = auc->next, i++)
    if (auc->seller)
      new_send_to_char(ch, "%d. %s auctioning %s for %ld coin%sto %s.\r\n",
    i, PERS(auc->seller, ch), auc->obj->short_description,
    auc->bid, auc->bid == 1 ? " " : "s ",
    auc->bidder ? GET_NAME(auc->bidder) : "no one");
  else
  new_send_to_char(ch, "  Nothing.\r\n");
}

int is_on_block(struct obj_data *obj)
{
  struct auction_data *auc;

  for(auc = auction; auc; auc = auc->next)
    if (auc->obj == obj)
      return 1;

  return 0;
}

void free_auction(struct auction_data *auc)
{
  free(auc);
}

struct char_data *get_char_auc(char *name)
{
  struct char_data *i;
  int j = 0, gcnumber;
  char tmpname[MAX_INPUT_LENGTH];
  char *tmp = tmpname;

  strcpy(tmp, name);
  if (!(gcnumber = get_number(&tmp)))
    return NULL;

  for (i = character_list; i && (j <= gcnumber); i = i->next)
    if (isname(tmp, i->player.name))
      if (++j == gcnumber)
        return i;

  return NULL;
}

bool can_auction_item(struct char_data * ch, struct obj_data * obj)
{
  if (GET_OBJ_TYPE(obj) == ITEM_WORN || GET_OBJ_TYPE(obj) == ITEM_OTHER
      || GET_OBJ_TYPE(obj) == ITEM_TRASH
      || GET_OBJ_TYPE(obj) == ITEM_TRAP || GET_OBJ_TYPE(obj) == ITEM_NOTE
      || GET_OBJ_TYPE(obj) == ITEM_KEY || GET_OBJ_TYPE(obj) == ITEM_FOOD
      || GET_OBJ_TYPE(obj) == ITEM_MONEY || GET_OBJ_TYPE(obj) == ITEM_PEN
      || GET_OBJ_TYPE(obj) == ITEM_BOAT || GET_OBJ_TYPE(obj) == ITEM_TREASURE
      || GET_OBJ_TYPE(obj) == ITEM_FOUNTAIN
      || GET_OBJ_TYPE(obj) == ITEM_WEAPON
      || GET_OBJ_TYPE(obj) == ITEM_ARMOR || GET_OBJ_TYPE(obj) == ITEM_FOCUS_MAJOR
      || GET_OBJ_TYPE(obj) == ITEM_FOCUS_MINOR)
    return (TRUE);
  else
    return (FALSE);
}
