/* ************************************************************************
*   File: mobact.c                                      Part of CircleMUD *
*  Usage: Functions for generating intelligent (?) behavior in mobiles    *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "db.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "spells.h"
#include "constants.h"
#include "fight.h"
#include "descriptor.h"
#include "graph.h"
#include "dg_scripts.h"

/* external structs */
extern int no_specials;

ACMD ( do_get );
void hunt_victim ( Character *ch );
void parse_mob_commands ( Character *ch );
Character * parse_aggressive ( Character *ch );
int valid_perc ( Character *ch );
bool can_take_obj(Character *ch, struct obj_data *obj);

/* local functions */
void mobile_activity ( void );
void clearMemory ( Character *ch );
bool AggroTo ( Character *ch, Character *vict );

int total_actions = 0;
int max_actions = 0;
int min_actions = -1;


void mobile_activity ( void )
{
    Character *ch, *next_ch, *vict;
    struct obj_data *obj, *best_obj;
    int door, found, max;
    memory_rec *names;
    vector<long> tobjs;

    for ( olt_it ob = object_list.begin(); ob != object_list.end(); ob++ )
    {
        obj = ( ob->second );
        if ( TRAVEL_LIST ( obj ) != NULL )
            tobjs.push_back(GET_ID ( obj ));
    }
    while (!tobjs.empty()) {
        hunt_location ( tobjs.back(), STRUCT_IS_OBJ );
        tobjs.pop_back();
    }

    for ( ch = character_list; ch; ch = next_ch )
    {
        next_ch = ch->next;
        /*
           if ((!IS_NPC(ch)) && (IS_POISONED(ch)))
           ch->Send( "{cRYou feel the poison curdling your blood.\r\n");
         */

        if ( !IS_MOB ( ch ) || DEAD ( ch ) )
            continue;
        if ( !IN_ROOM ( ch ) )
            continue;

        /*this is in familiars.c */
        parse_mob_commands ( ch );

        /* Examine call for special procedure */
        if ( MOB_FLAGGED ( ch, MOB_SPEC ) && !no_specials )
        {
            if ( GetMobIndex ( GET_MOB_VNUM ( ch ) )->func == NULL )
            {
                log ( "SYSERR: %s (#%d): Attempting to call non-existing mob function.", GET_NAME ( ch ), GET_MOB_VNUM ( ch ) );
                REMOVE_BIT_AR ( MOB_FLAGS ( ch ), MOB_SPEC );
            }
            else
            {
                /* TODO: Need to see if they can handle NULL instead of "". */
                if ( ( GetMobIndex ( GET_MOB_VNUM ( ch ) )->func ) ( ch, ch, 0, ( char * ) "", (char *)"" ) )
                    continue;	/* go to next char */
            }
        }

        /* If the mob has no specproc, do the default actions */
        if ( FIGHTING ( ch ) || !AWAKE ( ch ) )
            continue;

        /* Scavenger (picking up objects) */
        if ( MOB_FLAGGED ( ch, MOB_SCAVENGER ) )
            if ( IN_ROOM ( ch )->contents && !number ( 0, 10 ) )
            {
                max = 1;
                best_obj = NULL;
                for ( obj = IN_ROOM ( ch )->contents; obj;
                        obj = obj->next_content )
                    if ( CAN_GET_OBJ ( ch, obj ) && can_take_obj ( ch, obj ) && GET_OBJ_COST ( obj ) > max && ! OBJ_FLAGGED ( obj, ITEM_PC_CORPSE ) )
                    {
                        best_obj = obj;
                        max = GET_OBJ_COST ( obj );
                    }
                if ( best_obj != NULL && get_otrigger ( best_obj, ch ) > 0 )
                {
                    obj_from_room ( best_obj );
                    obj_to_char ( best_obj, ch );
                    act ( "$n picks up $p.", FALSE, ch, best_obj, 0, TO_ROOM );
                    total_actions++;
                }
            }

        /* Mob Movement */
        if ( ( GET_POS ( ch ) == POS_STANDING ) )   /* Thanks to suggestion by Geoff Hollis */
        {
            if ( hunt_location ( GET_ID(ch), STRUCT_IS_MOB ) ) {}
            else if ( HUNTING ( ch ) )
            {
                hunt_victim ( ch );
            }
            else if ( !MOB_FLAGGED ( ch, MOB_SENTINEL )
                      && ( ( door = number ( 0, 18 ) ) < NUM_OF_DIRS )
                      && CAN_GO ( ch, door )
                      && !ROOM_FLAGGED ( EXIT ( ch, door )->to_room, ROOM_NOMOB )
                      && !ROOM_FLAGGED ( EXIT ( ch, door )->to_room, ROOM_DEATH )
                      && ( !MOB_FLAGGED ( ch, MOB_STAY_ZONE )
                           || ( EXIT ( ch, door )->to_room->zone ==
                                IN_ROOM ( ch )->zone ) ) )
            {
                if ( IS_HERD ( ch ) )
                {
                    /* Herds should move slower */
                    if ( !number ( 0, 2 ) )
                        perform_move ( ch, door, 1 );
                }
                else perform_move ( ch, door, 1 );
            }
        }

        /* Aggressive Mobs */
        if (MOB_FLAGGED(ch, MOB_AGGRESSIVE) ||
            MOB_FLAGGED(ch, MOB_AGGR_EVIL) ||
            MOB_FLAGGED(ch, MOB_AGGR_GOOD) ||
            MOB_FLAGGED(ch, MOB_AGGR_NEUTRAL) ||
            MOB_FLAGGED(ch, MOB_AGGR_FEMALE) ||
            MOB_FLAGGED(ch, MOB_AGGR_MALE) ||
            MOB_FLAGGED(ch, MOB_AGGR_SEX_NEUTRAL))
        {
            found = FALSE;
            Character *vnext;
            for ( vict = IN_ROOM ( ch )->people; vict && !found; vict = vnext )
            {
                vnext = vict->next_in_room;
                if ( vict == ch )
                    continue;
                if ( AggroTo ( ch, vict ) )
                {
                    start_fighting ( ch, vict );
                    found = TRUE;
                    total_actions++;
                }
            }
        }

        /* Mob Memory */
        if ( /*MOB_FLAGGED(ch, MOB_MEMORY) && */MEMORY ( ch ) && !ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_PEACEFUL ) )
        {

            found = FALSE;
            for ( vict = IN_ROOM ( ch )->people; vict && !found;
                    vict = vict->next_in_room )
            {
                if ( IS_NPC ( vict ) || !CAN_SEE ( ch, vict )
                        || PRF_FLAGGED ( vict, PRF_NOHASSLE ) )
                    continue;
                for ( names = MEMORY ( ch ); names && !found;
                        names = names->next )
                    if ( names->id == GET_IDNUM ( vict ) )
                    {
                        found = TRUE;
                        act ( "'Hey!  You're the fiend that attacked me!!!', exclaims $n.", FALSE, ch, 0, 0, TO_ROOM );
                        start_fighting ( ch, vict );
                        total_actions++;
                    }
            }
        }

        /* Helper Mobs */
        if ( MOB_FLAGGED ( ch, MOB_HELPER )
                && ( !AFF_FLAGGED ( ch, AFF_BLIND | AFF_CHARM ) || RIDDEN_BY ( ch ) ) )
        {
            found = FALSE;
            for ( vict = IN_ROOM ( ch )->people; vict && !found;
                    vict = vict->next_in_room )
            {
                if ( ch == vict || !FIGHTING ( vict ) )
                    continue;
                if ( ch == FIGHTING ( vict ) )
                    continue;
                if ( IS_GOOD ( ch ) && !IS_GOOD ( vict ) )
                    continue;
                if ( IS_EVIL ( ch ) && !IS_EVIL ( vict ) )
                    continue;
                if ( IS_EVIL ( ch ) && IS_EVIL ( FIGHTING ( vict ) ) )
                    continue;
                if ( IS_GOOD ( ch ) && IS_GOOD ( FIGHTING ( vict ) ) )
                    continue;

                act ( "$n jumps to the aid of $N!", FALSE, ch, 0, vict, TO_ROOM );
                start_fighting ( ch, FIGHTING ( vict ) );
                total_actions++;
                found = TRUE;
            }
        }
        /* Add new mobile actions here */

    }				/* end for() */

    if ( total_actions > max_actions )
    {
        max_actions = total_actions;
        //log("A total of %d mob actions parsed (new maximum).", total_actions);
    }
    else if ( total_actions < min_actions || min_actions == ( -1 ) )
    {
        min_actions = total_actions;
        //log("A total of %d mob actions parsed (new minimum).", total_actions);
    }

    total_actions = 0;
}



/* Mob Memory Routines */

/* make ch remember victim */
void remember ( Character *ch, Character *victim )
{
    memory_rec *tmp;
    bool present = FALSE;

    if ( !IS_NPC ( ch ) || IS_NPC ( victim ) || PRF_FLAGGED ( victim, PRF_NOHASSLE ) )
        return;

    for ( tmp = MEMORY ( ch ); tmp && !present; tmp = tmp->next )
        if ( tmp->id == GET_IDNUM ( victim ) )
            present = TRUE;

    if ( !present )
    {
        CREATE ( tmp, memory_rec, 1 );
        tmp->next = MEMORY ( ch );
        tmp->id = GET_IDNUM ( victim );
        MEMORY ( ch ) = tmp;
    }
}


/* make ch forget victim */
void forget ( Character *ch, Character *victim )
{
    memory_rec *curr, *prev = NULL;

    if ( ! ( curr = MEMORY ( ch ) ) )
        return;

    while ( curr && curr->id != GET_IDNUM ( victim ) )
    {
        prev = curr;
        curr = curr->next;
    }

    if ( !curr )
        return;			/* person wasn't there at all. */

    if ( curr == MEMORY ( ch ) )
        MEMORY ( ch ) = curr->next;
    else
        prev->next = curr->next;

    free ( curr );
}


/* erase ch's memory */
void clearMemory ( Character *ch )
{
    memory_rec *curr, *next;

    curr = MEMORY ( ch );

    while ( curr )
    {
        next = curr->next;
        free ( curr );
        curr = next;
    }

    MEMORY ( ch ) = NULL;
}

bool AggroTo ( Character *ch, Character *vict )
{
    if ( SELF ( vict, ch ) )
        return false;
    if ( IS_NPC ( vict ) && !vict->master )
        return false;
    if ( !CAN_SEE ( ch, vict ) )
        return false;
    if ( PRF_FLAGGED ( vict, PRF_NOHASSLE ) )
        return false;
    if ( AFF_FLAGGED ( vict, AFF_SNEAK ) && !AFF_FLAGGED ( ch, AFF_SENSE_LIFE ) )
        return false;
    if ( MOB_FLAGGED ( ch, MOB_WIMPY ) && AWAKE ( vict ) )
        return false;
    if ( valid_perc ( vict ) == 0 )
        return false;
    if ( MOB_FLAGGED ( ch, MOB_AGGRESSIVE ) )
        return true;
    if (( MOB_FLAGGED ( ch, MOB_AGGR_EVIL ) && IS_EVIL ( vict ) ) ||
        ( MOB_FLAGGED ( ch, MOB_AGGR_NEUTRAL ) && IS_NEUTRAL ( vict ) )
        || ( MOB_FLAGGED ( ch, MOB_AGGR_GOOD ) && IS_GOOD ( vict ) ) )
        return true;
    if (( MOB_FLAGGED ( ch, MOB_AGGR_MALE) && IS_MALE ( vict ) ) ||
        ( MOB_FLAGGED ( ch, MOB_AGGR_FEMALE) && IS_FEMALE ( vict ) ) ||
        ( MOB_FLAGGED ( ch, MOB_AGGR_SEX_NEUTRAL) && IS_SEX_NEUTRAL ( vict ) ) )
        return true;

    return false;
}
Character * parse_aggressive ( Character *ch )
{
    Character *vict = NULL;
    /* Aggressive Mobs */
    if ( !FIGHTING ( ch ) && ( MOB_FLAGGED ( ch, MOB_AGGRESSIVE ) ||
                    MOB_FLAGGED(ch, MOB_AGGR_EVIL) ||
                    MOB_FLAGGED(ch, MOB_AGGR_GOOD) ||
                    MOB_FLAGGED(ch, MOB_AGGR_NEUTRAL) ||
                    MOB_FLAGGED(ch, MOB_AGGR_FEMALE) ||
                    MOB_FLAGGED(ch, MOB_AGGR_MALE) ||
                    MOB_FLAGGED(ch, MOB_AGGR_SEX_NEUTRAL)))
    {
        for ( vict = IN_ROOM ( ch )->people; vict;
                vict = vict->next_in_room )
        {
            if ( AggroTo ( ch, vict ) )
                return vict;

        }
    }
    return NULL;
}

int hunt_location ( long id, int type )
{
    int dir;
    struct travel_point_data *tlist = NULL, *ttop = NULL;
    room_rnum rnum, curr_room = NULL;
    room_vnum dest = NOWHERE;
    struct obj_data *obj = NULL;
    Character *mob = NULL;
    int find_first_step ( room_rnum src, room_rnum target,bool honour_notrack=false );


    switch ( type )
    {
        case STRUCT_IS_MOB:
            mob = find_char(id);
            if (mob == NULL)
                return 0;
            if ( ( curr_room = IN_ROOM ( mob ) ) == NULL )
                return 0;
            if ( ( ttop = TRAVEL_LIST ( mob ) ) == NULL )
                return 0;
//			log ( "DEBUG: Mob vnum %d is hunting a location from room %d.", GET_MOB_VNUM ( mob ), IN_ROOM ( mob )->number );
            break;
        case STRUCT_IS_OBJ:
            obj = find_obj(id);
            if (!obj)
                return 0;
            if ( ( curr_room = IN_ROOM ( obj ) ) == NULL )
                return 0;
            if ( ( ttop = TRAVEL_LIST ( obj ) ) == NULL )
                return 0;
//			log ( "DEBUG: Obj vnum %d is hunting a location from room %d.", GET_OBJ_VNUM ( obj ), IN_ROOM ( obj )->number );
            break;
        default:
            return 0;
    }



    for ( tlist = ttop; tlist; tlist = tlist->next )
    {
        if ( tlist && tlist->last_stop == TRUE )
        {
            dest = tlist->dest;
            break;
        }
    }

    /* make sure it has a stopper */
    if ( dest == NOWHERE )
    {
        ttop->last_stop = TRUE;
        dest = ttop->dest;
    }

    if ( ( rnum = real_room ( dest ) ) == NULL )
        return 0;

    //std::binary_search as opposed to find*/
    if ( ( dir = graph.find_first_step ( curr_room, rnum ) ) < 0 )
    {
        if ( dir != BFS_ALREADY_THERE )
        {
            return 0;
        }
        else
        {
            for ( tlist = ttop; tlist; tlist = tlist->next )
            {
                if ( tlist->last_stop == TRUE )
                {
                    tlist->last_stop = FALSE;
                    if ( tlist->next )
                        dest = tlist->next->last_stop = TRUE;
                    else
                        dest = ttop->last_stop = TRUE;
                    break;
                }
            }
        }
        return 0;
    }
    else
    {
        switch ( type )
        {
            case STRUCT_IS_MOB:
                perform_move ( mob, dir, 1 );
                return 1;
            case STRUCT_IS_OBJ:
                perform_move_obj ( obj, dir, NULL );
                return 1;
        }
    }
    return 0;
}

void add_travel_point_by_pointer ( struct travel_point_data **tlist, room_vnum dest )
{
    struct travel_point_data *temp, *t;

    CREATE ( temp, struct travel_point_data, 1 );
    temp->next = NULL;
    temp->last_stop = FALSE;
    temp->dest = dest;

    for ( t = *tlist; t; t = t->next )
        if ( !t->next )
            break;

    if ( t )
        t->next = temp;
    else
        *tlist = temp;

}

void add_travel_point_by_thing ( void *thing, int type, room_vnum dest )
{
    struct travel_point_data *tlist, *temp, *ttop;
    Character *mob = NULL;
    struct obj_data  *obj = NULL;
    if ( !thing )
        return;
    switch ( type )
    {
        case STRUCT_IS_MOB:
            mob = * ( Character ** ) thing;
            ttop = TRAVEL_LIST ( mob );
            break;
        case STRUCT_IS_OBJ:
            obj = * ( struct  obj_data ** ) thing;
            ttop = TRAVEL_LIST ( obj );
            break;
        default:
            return;
    }

    CREATE ( tlist, struct travel_point_data, 1 );
    tlist->next = NULL;
    tlist->last_stop = FALSE;
    tlist->dest = dest;

    /* add new point at end */
    for ( temp = ttop; temp; temp = temp->next )
    {
        if ( temp->next == NULL )
            break;
    }

    if ( temp )
        temp->next = tlist;
    else
    {
        switch ( type )
        {
            case STRUCT_IS_MOB:
                TRAVEL_LIST ( mob ) = tlist;
                break;
            case STRUCT_IS_OBJ:
                TRAVEL_LIST ( obj ) = tlist;
                break;
        }

    }

    return;
}
/*For zedit*/
void remove_travel_point_by_num ( struct travel_point_data **tlist, int num )
{
    int i = 0;
    struct travel_point_data *t, *t_n, *temp;

    for ( t = *tlist; t; t=t_n )
    {
        t_n = t->next;
        if ( i++ == num )
        {
            REMOVE_FROM_LIST ( t, *tlist, next );
            free ( t );
            return;
        }
    }

}
/*for dg_scripts*/
void remove_travel_point_by_dest ( struct travel_point_data **tlist, room_vnum dest )
{
    struct travel_point_data *t, *t_n, *temp = NULL;

    for ( t = *tlist; t; t=t_n )
    {
        t_n = t->next;
        if ( t->dest == dest )
        {
            REMOVE_FROM_LIST ( t, *tlist, next );
            free ( t );
            return;
        }
    }

}
/*for code*/
void remove_travel_point_by_pointer ( struct travel_point_data **tlist, struct travel_point_data *dead )
{
    struct travel_point_data *temp;
    if ( !*tlist || !dead )
        return;

    REMOVE_FROM_LIST ( dead, *tlist, next );

    free ( dead );

}

void free_travel_points ( struct travel_point_data *t )
{
    if ( !t )
        return;
    if ( t->next )
        free_travel_points ( t->next );

    free ( t );
}

