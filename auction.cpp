/************************************************************************
* auction.c   An automated auctioning system based on CircleMUD 3.0  *
*        bpl12 with mobile auctioneer and queue of items.  *
*                                           *
* Copyright (C) 1996,1997 by George Greer                  *
************************************************************************/

#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "auction.h"
#include "descriptor.h"

/*
 * Local global variables.
 */
struct auction_data *auction;


/*
 * Function prototypes.
 */
void free_auction(struct auction_data *auc);
void auction_forfeit(Character *mob);
void auction_reset(void);
int is_on_block(struct obj_data *obj);
struct obj_data *check_obj(Character *ch, struct obj_data *obj);
Character *check_ch(Character *ch);
void show_auction_status(Character *ch);
ACMD(do_gen_comm);
Character *get_char_auc(char *name);

bool can_auction_item(Character * ch, struct obj_data * obj);

/*
 * External global variables.
 */
extern Character *character_list;

void identify_object(Character *ch, OBJ_DATA *obj);
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
  char buf2[MAX_STRING_LENGTH], price[50];
  Character *mob = NULL;

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
  mob = get_char_auc((char *)AUC_MOB);

  /* Seller with us? */
  if (!check_ch(auction->seller)) {
    if (mob)
      do_gen_comm(mob, (char *)"The seller is no longer with us.", 0, SCMD_AUCTION);
    auction_reset();
    return;
  /* Seller's object? */
  } else if (!check_obj(auction->seller, auction->obj)) {
    auction_forfeit(mob);
    return;
  /* Make sure bidder exists */
  } else if (auction->bidder && !check_ch(auction->bidder)) {
    if (mob)
      do_gen_comm(mob, (char *)"The bidder has left, restarting.", 0, SCMD_AUCTION);
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
    commafmt ( price, sizeof ( price ), auction->bid );
    if (auction->bidder)
      snprintf(buf2, sizeof(buf2), "%s for %s coin%sto %s.%s%s",
          auction->obj->short_description, price,
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
      snprintf(buf2, sizeof(buf2), "%s%s, %s coin%sminimum.",
      auction->ticks == AUC_NEW ? "New item: " : "",
          auction->obj->short_description, price, auction->bid != 1 ?
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
      if (auction->bidder->Gold(0, GOLD_HAND) < auction->bid) {
        act("You cannot afford to buy the $p from $N. Auction Canceled.", FALSE, auction->seller, auction->obj, auction->bidder, TO_CHAR);
        act("$n cannot afford to buy the $p from you. Auction Canceled.", FALSE, auction->seller, auction->obj, auction->bidder, TO_VICT);
      } else {
      /* Swap gold */
        auction->bidder->Gold( -auction->bid, GOLD_HAND);
        auction->seller->Gold(  auction->bid, GOLD_HAND);
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
  Character *mob = get_char_auc((char *)AUC_MOB);
  int num = 0, i = 0;
  char buf[MAX_INPUT_LENGTH], buf1[MAX_INPUT_LENGTH];
  struct auction_data *auc = auction;

  if (!auc) {
    ch->Send( "Nothing has been auctioned!\r\n");
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
    ch->Send( "That number does not exist.\r\n");
    return;
  } else if (i == 0)
    *buf1 = '\0';

  if (auc->ticks == AUC_NONE) {
    ch->Send( "Nothing is up for sale.\r\n");
    return;
  }
  if (!*buf) {
  ch->Send( "Current bid: %ld coin%s\r\n", auc->bid,
        auc->bid != 1 ? "s." : ".");
   } else if (isname(buf, "stat")) {
     gold_int bid_stat = 0;
     if (auc->bid < 1000)
       bid_stat = 1000;
     else
       bid_stat = (gold_int)(auc->bid * 0.1);
     if (ch->Gold( 0, GOLD_HAND) < bid_stat) {
       ch->Send( "You need at least %lld gold to stat that item.\r\n", bid_stat);
       return;
     }
     ch->Gold( -bid_stat, GOLD_HAND);
     identify_object(ch, auc->obj);
  }
  else if (ch == auc->bidder)
    ch->Send( "You're trying to outbid yourself.\r\n");
  else if (ch == auc->seller)
    ch->Send( "You can't bid on your own item.\r\n");
  else if (!isname(buf1, auc->obj->name) && *buf1 && num == 0)
    ch->Send( "That object is not for sale currently.\r\n");
  else if (bid < auc->bid && !auc->bidder) {
    ch->Send( "You have to bid over the minimum of %ld coin%s\r\n",
    auc->bid, auc->bid != 1 ? "s." : ".");
  } else if ((bid < (auc->bid * 1.05) && auc->bidder) || bid == 0) {
    ch->Send( "Try bidding at least 5%% over the current bid of %ld. (%.0f coins).\r\n",
        auc->bid, auc->bid * 1.05 + 1);
  } else if (ch->Gold( 0, GOLD_HAND) < bid) {
    ch->Send( "You have only %lld coins on hand.\r\n", ch->Gold( 0, GOLD_HAND));
  } else if (PLR_FLAGGED(ch, PLR_NOSHOUT))
    ch->Send( "You can't auction.\r\n");
  else if (mob == NULL)
    ch->Send( "The auctioneer seems to be dead.\r\n");
  else {
    if (auc->bidder) {
    auc->bidder->Send("%s has placed a %ld bid over your %ld bid for %s.\r\n",
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
      ch->Send( "A %ld coin bid placed on %s.\r\n", bid,
         auc->obj->short_description);
    }
  }
}

ACMD(do_auction)
{
  struct obj_data *obj;
  Character *mob = get_char_auc((char *)AUC_MOB);
  struct auction_data *auc_add;
  char buf1[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];
  int i = 0;

  two_arguments(argument, buf1, buf2);
  if(GET_RACE(ch) == RACE_GLADIATOR) {
	  ch->Send( "You have no time to auction, you must fight!\r\n" );
	  return;
  }

  if (!*buf1) {
    ch->Send( "Format: auction item [min]\r\n"
          "        auction list/status\r\n");
    if (GET_LEVEL(ch) >= LVL_GOD)
      ch->Send( "        auction stop\r\n");
    ch->Send( "Auction what for what minimum?\r\n");
  } else if (isname(buf1, "list status"))
    show_auction_status(ch);

  else if (GET_LEVEL(ch) >= LVL_GOD && is_abbrev(buf1, "stop"))
    auction_reset();
  else if ((obj = get_obj_in_list_vis(ch, buf1, NULL, ch->carrying)) == NULL) {
    ch->Send( "You don't seem to have that to sell.\r\n");
  } else if (is_on_block(obj))
    ch->Send( "You are already auctioning that!\r\n");
  else if (OBJ_FLAGGED(obj, ITEM_NOSELL) || !can_auction_item(ch, obj))
    ch->Send( "You can't auction that item.\r\n");
  else if (mob == NULL)
    ch->Send( "The auctioneer seems to be dead.\r\n");
  else if((atoi(buf2) != 0 ? atoi(buf2) : 1)<0){
    ch->Send( "Try a positive amount instead.\r\n");
  }
  else {
    struct auction_data *n_auc;
    for (n_auc = auction; n_auc && n_auc->next; n_auc = n_auc->next, i++);
    if (i >= AUC_LIMIT) {
      ch->Send( "Sorry, only %d items allowed on the block at a time.\r\n", AUC_LIMIT);
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
    ch->Send( "Auction noted, currently %d item%sahead of yours.\r\n",
    i, i == 1 ? " " : "s ");
    else
      ch->Send( "Auction noted.\r\n");
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

struct obj_data *check_obj(Character *ch, struct obj_data *obj)
{
  struct obj_data *ch_obj;

  for (ch_obj = ch->carrying; ch_obj; ch_obj = ch_obj->next_content)
    if (ch_obj->item_number == obj->item_number)
      return ch_obj;

  return NULL;
}



void auction_forfeit(Character *mob)
{
  /* Item not found */
  if (mob) {
    char buf2[MAX_STRING_LENGTH];
    snprintf(buf2, sizeof(buf2), "%s no longer holds object, auction is forfeit.", GET_NAME(auction->seller));
    do_gen_comm(mob, buf2, 0, SCMD_AUCTION);
  }
  auction->seller->Send("A small daemon pops in, takes some gold, and taunts you.\r\n" );
  act("A small daemon pops in, takes some gold from $n, and sticks its tongue out at $m.",
    FALSE, auction->seller, auction->obj, 0, TO_ROOM);
    // New code from Fizban changed by Prometheus
    if (GET_GOLD(auction->seller) >= auction->bid)
	GET_GOLD(auction->seller) -= auction->bid;
    else
        GET_GOLD(auction->seller) = 0;  
	auction_reset();
}

void show_auction_status(Character *ch)
{
  struct auction_data *auc;
  int i = 1;

  ch->Send( "Items up for auction:\r\n");
  for(auc = auction, i = 0; auc; auc = auc->next, i++)
    if (auc->seller)
      ch->Send( "%d. %s auctioning %s for %ld coin%sto %s.\r\n",
    i, PERS(auc->seller, ch), auc->obj->short_description,
    auc->bid, auc->bid == 1 ? " " : "s ",
    auc->bidder ? GET_NAME(auc->bidder) : "no one");
  else
  ch->Send( "  Nothing.\r\n");
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

Character *get_char_auc(char *name)
{
  Character *i;
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

bool can_auction_item(Character * ch, struct obj_data * obj)
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
