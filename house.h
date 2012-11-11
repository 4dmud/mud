#define MAX_HOUSES	100
// this is going to get changed soon from 10 to x by Once or Prom
//Prom
#define MAX_GUESTS	10

#define HOUSE_PRIVATE	0

void add_follower(Character *ch, Character *leader);
int house_capacity(room_vnum house);

struct house_control_rec {
    room_vnum vnum;		/* vnum of this house           */
    room_vnum atrium;		/* vnum of atrium               */
    sh_int exit_num;		/* direction of house's exit    */
    time_t built_on;		/* date this house was built    */
    int mode;			/* mode of ownership            */
    long owner;			/* idnum of house's owner       */
    int num_of_guests;		/* how many guests for house    */
    long guests[MAX_GUESTS];	/* idnums of house's guests     */
    time_t last_payment;	/* date of last house payment   */
    long stable;
    long size;
    long expantions;
    long spare3;
    long spare4;
    long spare5;
    long spare6;
    long spare7;
};

void House_listrent(Character *ch, room_vnum vnum);
void House_boot(void);
void House_save_all(void);
int House_can_enter(Character *ch, room_vnum house);
void House_crashsave(room_vnum vnum);
void House_list_guests(Character *ch, int i, int quiet);
void house_expand_house(Character *ch, int i);
void hcontrol_expand_house(Character *ch, char *argument);
void House_info ( Character *ch, int i, int quiet );
