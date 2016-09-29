//
// C++ Implementation: room_affect
//
// Description:
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "interpreter.h"
#include "spells.h"
#include "handler.h"
#include "comm.h"
#include "db.h"
#include "dg_scripts.h"
#include "constants.h"
#include "fight.h"
#include "action.h"
#include "descriptor.h"
#include "damage.h"

/**
A room affect is an event that happens to all creatures within a room.
The affect timer is kept on the room, and when it goes off, the action is applied.
These room affects will stack, so you can have multiple different affects, and multiple of the same affect.

The action will need to know:
Who the caster of the spell is, if there is one.
The frequency of the affect in miliseconds.
The number of times this affect will occur.
The room it is in.

Mordecai - 16 Nov 08
**/

/**
The first spell will be CALL_LIGHTNING.
Which will be changed so that each time the event is reached, a random enemy in the room is struck by lightning.

**/


ACTION ( thing_call_lightning )
{
    const char *to_char = NULL;
    const char *to_room = NULL;
    //const char *to_room_miss = NULL;
    Character *tch = NULL, *rndm = NULL;
    int count = 0;
    float multi = 0;

    if ( !*num || !room )
    {
        if ( ch )
            ch->Send ( "Broken for some reason!??\r\n" );
        return 0;
    }


    /* No random char found */
    //to_room_miss = "A bolt of lightning scorches the earth nearby. BOOM!";

    /* Choose a random victim */
    if ( ch && IN_ROOM ( ch ) == room )
    {
        for ( tch = room->people; tch; tch = tch->next_in_room )
            if ( !SELF ( tch,ch ) && can_fight ( ch, tch, true ) && valid_dg_target ( tch, true ) )
            {
                if ( !number ( 0, count ) )
                    rndm = tch;
                count++;
            }
    }
    else
    {
        for ( tch = room->people; tch; tch = tch->next_in_room )
            if ( valid_dg_target ( tch, true ) && !SELF ( tch,ch ) )
            {
                if ( !number ( 0, count ) )
                    rndm = tch;
                count++;
            }
    }

    if ( ch == rndm )
        rndm = NULL;
    else if ( ch && rndm && HERE(rndm, ch))
    {
        to_char = "You are struck by a controlled bolt of lightning!";
        to_room = "$n is struck by a controlled bolt of lightning!";

    }
    else if ( rndm )
    {
        to_char = "A wild bolt of lightning slams into your body!";
        to_room = "A wild bolt of lightning slams into $n's body!";
    }

    if ( to_room!=NULL )
        act ( to_room, FALSE, rndm, obj, vict, TO_ROOM );

    if ( to_char!=NULL )
        act ( to_char, FALSE, rndm, obj, vict, TO_CHAR );

    if ( ch && rndm && HERE(rndm, ch))
        multi = skill_type_multi ( ch, rndm, SPELL_CALL_LIGHTNING );
    else
        multi = 1;

    if ( rndm )
    {
        room_affect_damage ( ch, rndm, int ( 150 * multi )  );
        if ( HERE ( ch, rndm ) )
        {
            if ( PRF_FLAGGED ( ch, PRF_BATTLESPAM ) )
            {
                ch->Send ( "{cc" );
                diag_char_to_char ( rndm, ch );
                ch->Send ( "{c0" );
            }

            start_fighting_delay ( rndm, ch );
        }
        else if ( rndm->canHuntChar ( ch ) )
        {
            HUNTING ( rndm ) = ch;
            remove_hunter ( rndm );
            add_hunter ( rndm );
            hunt_victim ( rndm );
        }
    }
/* the more stormy the weather the more frequent the attacks will be since the charge builds up faster*/
 return 4 - zone_table[GET_ROOM_ZONE ( room ) ].sky;

}


