struct auction_data {
  Character *seller;
  Character *bidder;
  struct obj_data *obj;
  long obj_uid;
  long bid;
  int ticks;
  struct auction_data *next;
};

#define AUC_NONE   -1
#define AUC_NEW         0
#define AUC_BID         1
#define AUC_ONCE   2
#define AUC_TWICE  3
#define AUC_SOLD   4

/*
 * User tweakables.
 */
#define AUC_MOB         "auctioneer"
#define AUC_LIMIT  10

#define PULSE_AUCTION   (15 RL_SEC)
void auction_update(void);
Character *get_char_auc(char *name);

