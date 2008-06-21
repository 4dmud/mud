/* ************************************************************************
*   File: spec_assign.c                                 Part of CircleMUD *
*  Usage: Functions to assign function pointers to objs/mobs/rooms        *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "db.h"
#include "interpreter.h"
#include "utils.h"


extern int mini_mud;

SPECIAL(bank);
SPECIAL(bottle);
SPECIAL(cityguard);
SPECIAL(cleric);
SPECIAL(cow_mating);
SPECIAL(craps);
SPECIAL(cryogenicist);
SPECIAL(door_down);
SPECIAL(door_down_7377);
SPECIAL(dragon_acid);
SPECIAL(dragon_fire);
SPECIAL(dragon_frost);
SPECIAL(dragon_gas);
SPECIAL(dragon_lightning);
SPECIAL(dump);
SPECIAL(fido);
SPECIAL(fire);
SPECIAL(gen_board);
SPECIAL(guard_black);
SPECIAL(guard_white);
SPECIAL(guild);
SPECIAL(guild_guard);
SPECIAL(high_dice);
SPECIAL(janitor);
SPECIAL(magic_user);
SPECIAL(mayor);
SPECIAL(pet_shops);
SPECIAL(postmaster);
SPECIAL(puff);
SPECIAL(radar);
SPECIAL(receptionist);
SPECIAL(scorpion);
SPECIAL(seven);
SPECIAL(slots);
SPECIAL(snake);
SPECIAL(spider);
SPECIAL(slots);
SPECIAL(thief);
SPECIAL(triples);
SPECIAL(token_machine);

/* local functions */
void assign_mobiles(void);
void assign_objects(void);
void assign_rooms(void);
void ASSIGNROOM(room_vnum room, SPECIAL(fname));
void ASSIGNMOB(mob_vnum mob, SPECIAL(fname));
void ASSIGNOBJ(obj_vnum obj, SPECIAL(fname));

/* functions to perform assignments */

void ASSIGNMOB(mob_vnum mob, SPECIAL(fname))
{
    if (MobIndexExists(mob))
	mob_index[mob]->func = fname;
    else if (!mini_mud)
	log("SYSERR: Attempt to assign spec to non-existant mob #%d", mob);
}

void ASSIGNOBJ(obj_vnum obj, SPECIAL(fname))
{
    obj_rnum rnum;

    if ((rnum = real_object(obj)) >= 0)
	obj_index[rnum].func = fname;
    else if (!mini_mud)
	log("SYSERR: Attempt to assign spec to non-existant obj #%d", obj);
}

void ASSIGNROOM(room_vnum room, SPECIAL(fname))
{
    if (world_vnum[room] != NULL)
	world_vnum[room]->func = fname;
    else if (!mini_mud)
	log("SYSERR: Attempt to assign spec to non-existant room #%d", room);
}


/* ********************************************************************
*  Assignments                                                        *
******************************************************************** */

/* assign special procedures to mobiles */
void assign_mobiles(void)
{

    //ASSIGNMOB(1, puff);

    ASSIGNMOB(1201, postmaster);
    ASSIGNMOB(3015, postmaster);
    ASSIGNMOB(3180, postmaster);
    ASSIGNMOB(32090, postmaster);

    ASSIGNMOB(1202, janitor);
    ASSIGNMOB(1296, magic_user);

    ASSIGNMOB(3024, guild_guard);	// mage
    ASSIGNMOB(3026, guild_guard);	// thief
    ASSIGNMOB(3027, guild_guard);	// cleric
    ASSIGNMOB(3151, guild_guard);	// gypsy
    ASSIGNMOB(10428, guild_guard);	// esper

    ASSIGNMOB(1393, cleric);	// sicili clan
    ASSIGNMOB(2103, cleric);	// crete
    ASSIGNMOB(2154, cleric);	// crete
    ASSIGNMOB(2906, cleric);	// viking hq
    ASSIGNMOB(2938, cleric);	// new clan
    ASSIGNMOB(3047, cleric);	// olde yorke
    ASSIGNMOB(3051, cleric);	// cardinal
    ASSIGNMOB(3644, cleric);	// tournament place 
    ASSIGNMOB(6132, cleric);	// sherwood
    ASSIGNMOB(6736, cleric);	// prairie 
    ASSIGNMOB(6867, cleric);	// tombstone
    ASSIGNMOB(7723, cleric);
    ASSIGNMOB(8136, cleric);	// seshet priest
    ASSIGNMOB(9202, cleric);	// robber baron II 
    ASSIGNMOB(9410, cleric);	// chaos hq
    ASSIGNMOB(10002, cleric);
    ASSIGNMOB(10400, cleric);
    ASSIGNMOB(12407, cleric);
    ASSIGNMOB(12905, cleric);
    ASSIGNMOB(13030, cleric);
    ASSIGNMOB(15014, cleric);	// camelot priest
    ASSIGNMOB(20542, cleric);	// convent
    ASSIGNMOB(27800, cleric);
    ASSIGNMOB(30960, cleric);
    ASSIGNMOB(10203, cleric);

    ASSIGNMOB(1200, receptionist);	// immortal zone
    ASSIGNMOB(1809, receptionist);
    ASSIGNMOB(1826, receptionist);
    ASSIGNMOB(1840, receptionist);
    ASSIGNMOB(1863, receptionist);
    ASSIGNMOB(2176, receptionist);	// Crete 
    ASSIGNMOB(2320, receptionist);	// Green Fen
    ASSIGNMOB(2321, receptionist);	// Green Fen
    ASSIGNMOB(2330, receptionist);	// Green Fen
    ASSIGNMOB(3018, receptionist);	// central area
    ASSIGNMOB(3040, receptionist);	// olde yorke
    ASSIGNMOB(4802, receptionist);	// Legend
    ASSIGNMOB(5601, receptionist);	// Osten Ard
    ASSIGNMOB(6620, receptionist);	// Old Carthage
    ASSIGNMOB(6737, receptionist);	// Prairie
    ASSIGNMOB(6868, receptionist);	// tombstone
    ASSIGNMOB(10403, receptionist);	// palp's zone
    ASSIGNMOB(10491, receptionist);	// dark jedi
    ASSIGNMOB(13021, receptionist);
    ASSIGNMOB(15043, receptionist);	// Durmandle
    ASSIGNMOB(15044, receptionist);	// West Trade Road

    ASSIGNMOB(533, guard_white);
    ASSIGNMOB(1848, guard_white);
    ASSIGNMOB(1849, guard_white);
    ASSIGNMOB(1866, guard_white);
    ASSIGNMOB(2100, guard_white);
    ASSIGNMOB(2204, guard_white);
    ASSIGNMOB(3127, guard_white);
    ASSIGNMOB(8702, guard_white);
    ASSIGNMOB(13000, guard_white);
    ASSIGNMOB(13062, guard_white);
    ASSIGNMOB(15047, guard_white);
    ASSIGNMOB(15076, guard_white);
    ASSIGNMOB(15077, guard_white);
    ASSIGNMOB(15098, guard_white);

    ASSIGNMOB(1818, guard_black);
    ASSIGNMOB(1847, guard_black);
    ASSIGNMOB(1859, guard_black);
    ASSIGNMOB(3128, guard_black);
    ASSIGNMOB(13063, guard_black);
    ASSIGNMOB(15048, guard_black);

    ASSIGNMOB(810, magic_user);
    ASSIGNMOB(834, magic_user);
    ASSIGNMOB(848, magic_user);
    ASSIGNMOB(1824, magic_user);
    ASSIGNMOB(1838, magic_user);
    ASSIGNMOB(2154, magic_user);
    ASSIGNMOB(5020, magic_user);
    ASSIGNMOB(5003, magic_user);
    ASSIGNMOB(5004, magic_user);
    ASSIGNMOB(5008, magic_user);
    ASSIGNMOB(5009, magic_user);
    ASSIGNMOB(6736, magic_user);
    ASSIGNMOB(6746, magic_user);
    ASSIGNMOB(8117, magic_user);
    ASSIGNMOB(8118, magic_user);
    ASSIGNMOB(8119, magic_user);
    ASSIGNMOB(8120, magic_user);
    ASSIGNMOB(8121, magic_user);
    ASSIGNMOB(10806, magic_user);
    ASSIGNMOB(10807, magic_user);
    ASSIGNMOB(10810, magic_user);
    ASSIGNMOB(15070, magic_user);
    ASSIGNMOB(15072, magic_user);
    ASSIGNMOB(15083, magic_user);
    ASSIGNMOB(15084, magic_user);
    ASSIGNMOB(15092, magic_user);
    ASSIGNMOB(19117, magic_user);
    ASSIGNMOB(20501, magic_user);
    ASSIGNMOB(20515, magic_user);
    ASSIGNMOB(20509, magic_user);
    ASSIGNMOB(20511, magic_user);
    ASSIGNMOB(20516, magic_user);
    ASSIGNMOB(20517, magic_user);
    ASSIGNMOB(20525, magic_user);
    ASSIGNMOB(20529, magic_user);
    ASSIGNMOB(20530, magic_user);
    ASSIGNMOB(21801, magic_user);
    ASSIGNMOB(21820, magic_user);
    ASSIGNMOB(22315, magic_user);
    ASSIGNMOB(26073, magic_user);
    //ASSIGNMOB(26060, magic_user);
    ASSIGNMOB(28462, magic_user);

    ASSIGNMOB(3061, janitor);
    ASSIGNMOB(15027, janitor);
    ASSIGNMOB(19803, janitor);
    ASSIGNMOB(26059, janitor);

    ASSIGNMOB(3060, cityguard);
    ASSIGNMOB(3067, cityguard);
    ASSIGNMOB(3068, cityguard);
    ASSIGNMOB(3069, cityguard);
    ASSIGNMOB(3119, cityguard);
    ASSIGNMOB(3141, cityguard);
    ASSIGNMOB(15002, cityguard);
    ASSIGNMOB(15003, cityguard);

    ASSIGNMOB(7706, triples);
    ASSIGNMOB(32036, triples);

    ASSIGNMOB(7342, seven);
    ASSIGNMOB(32072, seven);

    ASSIGNMOB(3036, high_dice);
    ASSIGNMOB(6816, high_dice);
    ASSIGNMOB(7355, high_dice);
    ASSIGNMOB(20541, high_dice);

    ASSIGNMOB(19118, dragon_fire);
    ASSIGNMOB(19116, dragon_fire);

    ASSIGNMOB(21829, dragon_frost);

    ASSIGNMOB(21829, dragon_gas);

    ASSIGNMOB(19119, dragon_lightning);

    ASSIGNMOB(21829, dragon_acid);


    ASSIGNMOB(7515, thief);
    ASSIGNMOB(7516, thief);
    ASSIGNMOB(7553, thief);
    ASSIGNMOB(8101, thief);
    ASSIGNMOB(8102, thief);
    ASSIGNMOB(8103, thief);
    ASSIGNMOB(8104, thief);
    ASSIGNMOB(8105, thief);
    ASSIGNMOB(8106, thief);
    ASSIGNMOB(8110, thief);
    ASSIGNMOB(8152, thief);

    // mobs with level 1 poison
    ASSIGNMOB(8151, spider);

    // mobs with level 2 poison
    ASSIGNMOB(1135, scorpion);
    ASSIGNMOB(1163, scorpion);
    ASSIGNMOB(3544, scorpion);
    ASSIGNMOB(6626, scorpion);
    ASSIGNMOB(7557, scorpion);

    // mobs with level 3 poison
    ASSIGNMOB(8148, snake);

}



/* assign special procedures to objects */
void assign_objects(void)
{

#if !defined(WIN32)
    ASSIGNOBJ(590, gen_board);	/* board in mud school */
    ASSIGNOBJ(1315, gen_board);	/* board in sicilian hq */
  ASSIGNOBJ(1360, gen_board);  /* board in sicilian hq 2*/
    ASSIGNOBJ(2917, gen_board);	/* board in viking head quarters */
    ASSIGNOBJ(2999, gen_board);	/* table round */
    ASSIGNOBJ(3079, gen_board);	/* board for rp */
    ASSIGNOBJ(3080, gen_board);	/* board at healer */
    ASSIGNOBJ(3089, gen_board);	/* mortal idea board */
    ASSIGNOBJ(3098, gen_board);	/* board in 1204 */
    ASSIGNOBJ(3399, gen_board);	/* board in recall */
    ASSIGNOBJ(3201, gen_board);	/* Immortal announcement board */
    ASSIGNOBJ(8400, gen_board);	/* rp */
    ASSIGNOBJ(8401, gen_board);	/* rp */
    ASSIGNOBJ(8402, gen_board);	/* rp */
    ASSIGNOBJ(8403, gen_board);	/* rp */
    ASSIGNOBJ(8449, gen_board);	/* rp */
    ASSIGNOBJ(8470, gen_board); /* bitches*/
    ASSIGNOBJ(8477, gen_board); /* roleplay leaders */
    ASSIGNOBJ(9421, gen_board);	/* board in chaos hq */
    ASSIGNOBJ(9435, gen_board);	/* board 2 in chaos hq */
    ASSIGNOBJ(10026, gen_board);	/* dragon hq */
    ASSIGNOBJ(10402, gen_board);	/* board in dj hq */
    ASSIGNOBJ(12000, gen_board);	/* board for Zaade */
    ASSIGNOBJ(12422, gen_board);	/* board for Saints */
    ASSIGNOBJ(12902, gen_board);	/* board for Seeker */
    ASSIGNOBJ(17101, gen_board);	/* board for Nicoli */
    ASSIGNOBJ(17807, gen_board);
    ASSIGNOBJ(28736, gen_board);	/* board for vampire clan */
    ASSIGNOBJ(10031, gen_board);
    ASSIGNOBJ(10233, gen_board);
    ASSIGNOBJ(26708, gen_board);	/* board for the gladiator school */
    
#endif

    ASSIGNOBJ(216, bottle);
    ASSIGNOBJ(1144, bottle);
    ASSIGNOBJ(7563, bottle);
    ASSIGNOBJ(11244, bottle);
    ASSIGNOBJ(11245, bottle);

    ASSIGNOBJ(1317, bank);	/* sicilian */
    ASSIGNOBJ(3090, bank);	/* atm */
    ASSIGNOBJ(7582, bank);
    ASSIGNOBJ(10008, bank);
    ASSIGNOBJ(10400, bank);
    ASSIGNOBJ(13056, bank);
    ASSIGNOBJ(13057, bank);
    ASSIGNOBJ(17416, bank);	/* bank in Palpatines area */
    ASSIGNOBJ(21615, bank);
  ASSIGNOBJ(9436, bank); //chaos

    ASSIGNOBJ(284, radar);
    ASSIGNOBJ(8282, radar);


    ASSIGNOBJ(260, slots);

    ASSIGNOBJ(219, token_machine);
}



/* assign special procedures to rooms */
void assign_rooms(void)
{
    int i;

    ASSIGNROOM(532, pet_shops);	// mudschool
    ASSIGNROOM(3031, pet_shops);	// olde yorke
    ASSIGNROOM(3075, pet_shops);	// olde yorke
    ASSIGNROOM(3498, pet_shops);
    ASSIGNROOM(3612, pet_shops);	// tournament place
    ASSIGNROOM(6827, pet_shops);	// tombstone
    ASSIGNROOM(7763, pet_shops);
    ASSIGNROOM(8283, pet_shops);
    ASSIGNROOM(10453, pet_shops);
    ASSIGNROOM(14398, pet_shops);
    ASSIGNROOM(30943, pet_shops);

    ASSIGNROOM(201, door_down);
    ASSIGNROOM(7377, door_down_7377);

    ASSIGNROOM(280, fire);
    ASSIGNROOM(2499, fire);
    ASSIGNROOM(8200, fire);
    ASSIGNROOM(8201, fire);
    ASSIGNROOM(8202, fire);
    ASSIGNROOM(8203, fire);
    ASSIGNROOM(8204, fire);
    ASSIGNROOM(8205, fire);
    ASSIGNROOM(8206, fire);
    ASSIGNROOM(8207, fire);
    ASSIGNROOM(8208, fire);
    ASSIGNROOM(8209, fire);
    ASSIGNROOM(8210, fire);
    ASSIGNROOM(8211, fire);
    ASSIGNROOM(8212, fire);
    ASSIGNROOM(8213, fire);
    ASSIGNROOM(8214, fire);
    ASSIGNROOM(8215, fire);
    ASSIGNROOM(8216, fire);
    ASSIGNROOM(8217, fire);
    ASSIGNROOM(8218, fire);
    ASSIGNROOM(8219, fire);

    ASSIGNROOM(60743, dump);
    ASSIGNROOM(61193, dump);
    ASSIGNROOM(61319, dump);
    ASSIGNROOM(61376, dump);
    ASSIGNROOM(62214, dump);
    ASSIGNROOM(62667, dump);

    if (CONFIG_DTS_ARE_DUMPS)
	for (i = 0; i <= top_of_world; i++)
	    if (world_vnum[i] && ROOM_FLAGGED(world_vnum[i], ROOM_DEATH))
		world_vnum[i]->func = dump;
}
