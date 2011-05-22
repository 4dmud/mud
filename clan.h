#define MAX_CLANS	 20
#define LVL_CLAN_GOD	 LVL_GOD
#define DEFAULT_APP_LVL	 8
#define CLAN_PLAN_LENGTH 800

#define GET_CLAN(ch)		((ch)->player_specials->saved.clan)
#define GET_CLAN_RANK(ch)	((ch)->player_specials->saved.clan_rank)

#define CP_SET_PLAN   0
#define CP_ENROLL     1
#define CP_EXPEL      2
#define CP_PROMOTE    3
#define CP_DEMOTE     4
#define CP_SET_FEES   5
#define CP_WITHDRAW   6
#define CP_SET_APPLEV 7
#define CP_SET_WAR    8
#define NUM_CP        9		/* Number of clan privileges */

#define CM_DUES   1
#define CM_APPFEE 2

#define CB_DEPOSIT  1
#define CB_WITHDRAW 2 
#define MAX_CLAN_SPELLS 5
#define NUM_CLAN_PRIVILEGE 20  /* Max clan privileges */
#define NUM_AT_CLAN_WAR 4
#define NUM_CLAN_EQ 3

void save_clans(void);
void init_clans(void);
sh_int find_clan_by_id(int clan_id);
sh_int find_clan(char *name);
char *clan_name(int idnum);

extern int num_of_clans;

struct clan_gold_type {
    gold_int coins;
    int gold;
    int silver;
    int bronze;
};

struct clan_rec {
    int id;
    char name[32];
    int ranks;
    char rank_name[20][20];
    gold_int treasure;
    struct clan_gold_type treasury;
    int members;
    int power;
    gold_int app_fee;
    gold_int dues;
    int spells[MAX_CLAN_SPELLS];
    int app_level;
    int privilege[NUM_CLAN_PRIVILEGE];
    int at_war[NUM_AT_CLAN_WAR];
    char description[CLAN_PLAN_LENGTH];
    room_vnum recall;
    int clan_eq[NUM_CLAN_EQ];
    obj_vnum board;
};

extern vector<clan_rec> clan;

// HORUS NEW CLAN CODE
#define MAX_CLAN_RANKS     20
#define MAX_CLAN_COMMANDS  20
#define MAX_CLAN_EQ        5

struct clan_data_type {
    int id;
    char *name;
    char *description;
    int ranks;
    char *rank_name[MAX_CLAN_RANKS];
    struct clan_gold_type treasure;
    int members;
    int power;
    int app_level;
    gold_int app_fee;
    gold_int app_dues;
    int commands[MAX_CLAN_COMMANDS];
    room_vnum recall;
    obj_vnum board;
    int clan_eq[MAX_CLAN_EQ];
    int spells[MAX_CLAN_SPELLS];
    int at_war[NUM_AT_CLAN_WAR];
};

