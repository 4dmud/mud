struct auction_data {
  struct char_data *seller;
  struct char_data *bidder;
  struct obj_data *obj;
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

#define PULSE_AUCTION   (15 RL_SEC)
void auction_update(void);
