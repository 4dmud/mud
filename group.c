/* for all grouping functions */
#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "db.h"
#include "comm.h"
#include "handler.h"
#include "spells.h"
#include "mail.h"
#include "interpreter.h"
#include "house.h"
#include "constants.h"
#include "dg_scripts.h"
#include "clan.h"


//#define MASTER 0

void display_group(Character *ch);
void check_group(Character *ch);

void clear_group(Character *ch);
Character find_master(Character *ch);
Character find_follower(Character *ch, int number);
int join_group(Character *master, Character *follower);
int leave_group(Character *master);



void display_group(Character *ch)
{
    Character *tch;
    int i;

    tch = find_master(ch);

    if (tch == NULL) {
	log("SYSERR: Display Group, master is null. Please Dont Crash");
	return;
    }
    sort_group(tch->group);
    for (i = 0; (i < MAX_GROUP) && (GET_GROUP(tch, i) != NOBODY); i++) {
	ch->Send( "%2d: %12s %3.2f %s", i,
			 get_name_by_id(GET_GROUP(tch, i)),
			 GET_PERC(GET_GROUP(tch, i)),
			 i % 2 ? "\r\n" : "    ");
    }
}



Character find_master(Character *ch)
{
    if (GET_GROUP(ch, MASTER) == NOBODY)
	clear_group(ch);

    return GET_GROUP(ch, MASTER);
}

int sort_group(long *group[])
{

    long temp[MAX_GROUP];



}
