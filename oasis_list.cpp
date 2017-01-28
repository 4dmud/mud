/******************************************************************************/
/** OasisOLC - InGame OLC Listings                                     v2.0  **/
/** Original author: Levork                                                  **/
/** Copyright 1996 Harvey Gilpin                                             **/
/** Copyright 1997-2001 George Greer (greerga@circlemud.org)                 **/
/** Copyright 2002 Kip Potter [Mythran] (kip_potter@hotmail.com)             **/
/******************************************************************************/
#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "genolc.h"
#include "oasis.h"
#include "improved-edit.h"
#include "shop.h"
#include "genshp.h"
#include "screen.h"
#include "constants.h"
#include "dg_scripts.h"

/******************************************************************************/
/** Ingame Commands                                                          **/
/******************************************************************************/
ACMD ( do_oasis_list )
{
    zone_rnum rzone = NOWHERE;
    room_vnum vmin = NOWHERE;
    room_vnum vmax = NOWHERE;
    char smin[MAX_INPUT_LENGTH];
    char smax[MAX_INPUT_LENGTH];

    two_arguments ( argument, smin, smax );

    if ( subcmd == SCMD_OASIS_VLIST )
    {
        list_vehicles ( ch );
        return;
    }

    if ( !*smin || *smin == '.' )
    {
        rzone = IN_ROOM ( ch )->zone;
    }
    else if ( !*smax )
    {
        rzone = real_zone ( atoi ( smin ) );

        if ( rzone == NOWHERE )
        {
            ch->Send ( "Sorry, there's no zone with that number\r\n" );
            return;
        }
    }
    else
    {
        /** Listing by min vnum / max vnum.  Retrieve the numeric values. **/
        vmin = atoi ( smin );
        vmax = atoi ( smax );

        if ( vmin > vmax )
        {
            ch->Send ( "List from %d to %d - Aren't we funny today!\r\n",
                       vmin, vmax );
            return;
        }
    }

    switch ( subcmd )
    {
        case SCMD_OASIS_MLIST: list_mobiles ( ch, rzone, vmin, vmax ); break;
        case SCMD_OASIS_OLIST: list_objects ( ch, rzone, vmin, vmax ); break;
        case SCMD_OASIS_RLIST: list_rooms ( ch, rzone, vmin, vmax ); break;
        case SCMD_OASIS_TLIST: list_triggers ( ch, rzone, vmin, vmax ); break;
        case SCMD_OASIS_SLIST: list_shops ( ch, rzone, vmin, vmax ); break;
        case SCMD_OASIS_ZLIST: list_zones ( ch, rzone, vmin, vmax ); break;
        default:
            ch->Send ( "You can't list that!\r\n" );
            new_mudlog ( BRF, LVL_IMMORT, TRUE,
                         "SYSERR: do_oasis_list: Unknown list option: %d", subcmd );
    }
}

ACMD ( do_oasis_links )
{
    zone_rnum zrnum;
    zone_vnum zvnum;
    room_rnum  to_room;
    room_vnum first, last;
    int j, nr;
    char arg[MAX_INPUT_LENGTH];

    skip_spaces ( &argument );
    one_argument ( argument, arg );

    if (!*arg )
    {
        ch->Send (
            "Syntax: links <zone_vnum> ('.' for zone you are standing in)\r\n" );
        return;
    }

    if ( !strcmp ( arg, "." ) )
    {
        zrnum = IN_ROOM ( ch )->zone;
        zvnum = zone_table[zrnum].number;
    }
    else
    {
        zvnum = atoi ( arg );
        zrnum = real_zone ( zvnum );
    }

    if ( zrnum == NOWHERE || zvnum == NOWHERE )
    {
        ch->Send ( "No zone was found with that number.\r\n" );
        return;
    }

    last  = zone_table[zrnum].top;
    first = zone_table[zrnum].bot;

    ch->Send ( "Zone %d is linked to the following zones:\r\n", zvnum );
    for ( nr = 0; nr <= top_of_world && ( GET_ROOM_VNUM ( world_vnum[nr] ) <= last ); nr++ )
    {
        if ( GET_ROOM_VNUM ( world_vnum[nr] ) >= first )
        {
            for ( j = 0; j < NUM_OF_DIRS; j++ )
            {
                if ( world_vnum[nr]->dir_option[j] )
                {
                    to_room = world_vnum[nr]->dir_option[j]->to_room;
                    if ( to_room != NULL && ( zrnum != to_room->zone ) )
                        ch->Send ( "%3d %-30s at %5d (%-5s) ---> %5d\r\n",
                                   zone_table[to_room->zone].number,
                                   zone_table[to_room->zone].name,
                                   nr, dirs[j], to_room->number );
                }
            }
        }
    }
}

/******************************************************************************/
/** Helper Functions                                                         **/
/******************************************************************************/


/*
 * List all rooms in a zone.
 */
void list_rooms ( Character *ch, zone_rnum rnum, zone_vnum vmin, zone_vnum vmax )
{
    room_vnum i;
    room_vnum bottom, top;
    int j, counter = 0;

    /*
     * Expect a minimum / maximum number if the rnum for the zone is NOWHERE.
     */
    if ( rnum != NOWHERE )
    {
        bottom = zone_table[rnum].bot;
        top    = zone_table[rnum].top;
    }
    else
    {
        bottom = vmin;
        top    = vmax;
    }

    ch->Send (
        "Index VNum    Room Name                                Exits\r\n"
        "----- ------- ---------------------------------------- -----\r\n" );

    DYN_DEFINE;
    DYN_CREATE;
    *dynbuf = '\0';
    char buf[MAX_INPUT_LENGTH];
    for ( i = 0; i <= top_of_world; i++ )
    {
        if ( !world_vnum[i] )
            continue;

        /** Check to see if this room is one of the ones needed to be listed.    **/
        if ( ( world_vnum[i]->number >= bottom ) && ( world_vnum[i]->number <= top ) )
        {
            counter++;

            snprintf ( buf, sizeof buf, "%4d) [%s%-5d%s] %s%-40.40s%s %s",
                       counter, QGRN, world_vnum[i]->number, QNRM,
                       QCYN, world_vnum[i]->name, QNRM,
                       world_vnum[i]->proto_script ? "[TRIG] " : ""
                     );
            DYN_RESIZE ( buf );

            for ( j = 0; j < NUM_OF_DIRS; j++ )
            {
                if ( W_EXIT ( world_vnum[i], j ) == NULL )
                    continue;
                if ( W_EXIT ( world_vnum[i], j )->to_room == NULL )
                    continue;

                if ( W_EXIT ( world_vnum[i], j )->to_room->zone != world_vnum[i]->zone )
                {
                    snprintf ( buf, sizeof buf, "(%s%d%s)", QYEL, W_EXIT ( world_vnum[i], j )->to_room->number, QNRM );
                    DYN_RESIZE ( buf );
                }

            }

            snprintf ( buf, sizeof buf, "\r\n" );
            DYN_RESIZE ( buf );
        }
    }

    if ( counter == 0 )
    {
        snprintf ( buf, sizeof buf, "No rooms found for zone #%d\r\n", zone_table[rnum].number );
        DYN_RESIZE ( buf );
    }
    page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

/*
 * List all mobiles in a zone.
 */
void list_mobiles ( Character *ch, zone_rnum rnum, zone_vnum vmin, zone_vnum vmax )
{
    mob_rnum i;
    mob_vnum bottom, top;
    int counter = 0;
    Character *pmob;

    if ( rnum != NOWHERE )
    {
        bottom = zone_table[rnum].bot;
        top    = zone_table[rnum].top;
    }
    else
    {
        bottom = vmin;
        top    = vmax;
    }

    ch->Send (
        "Index VNum    Mobile Name                                   Level\r\n"
        "----- ------- --------------------------------------------- -----\r\n" );

    DYN_DEFINE;
    DYN_CREATE;
    *dynbuf = '\0';
    char buf[MAX_INPUT_LENGTH];
    for ( i = bottom; i <= top; i++ )
    {
        if ( !MobProtoExists ( i ) )
            continue;
        counter++;
        pmob = GetMobProto ( i );

        snprintf ( buf, sizeof buf, "%s%4d%s) [%s%-5d%s] %s%-44.44s %s[%4d]%s%s\r\n",
                   QGRN, counter, QNRM, QGRN, i, QNRM,
                   QCYN, pmob->player.short_descr,
                   QYEL, pmob->player.level, QNRM,
                   pmob->proto_script ? " [TRIG]" : ""
                 );
        DYN_RESIZE ( buf );
    }

    if ( counter == 0 )
    {
        ch->Send ( "None found.\r\n" );
        DYN_RESIZE ( buf );
    }
    page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

/*
 * List all objects in a zone.
 */
void list_objects ( Character *ch, zone_rnum rnum, room_vnum vmin, room_vnum vmax )
{
    shop_rnum i;
    shop_vnum bottom, top;
    int counter = 0;

    if ( rnum != NOWHERE )
    {
        bottom = zone_table[rnum].bot;
        top    = zone_table[rnum].top;
    }
    else
    {
        bottom = vmin;
        top    = vmax;
    }

    ch->Send (
        "Index VNum    Object Name                                  Object Type\r\n"
        "----- ------- -------------------------------------------- ----------------\r\n" );

    DYN_DEFINE;
    DYN_CREATE;
    *dynbuf = '\0';
    char buf[MAX_INPUT_LENGTH];
    for ( i = 0; i <= top_of_objt; i++ )
    {
        if ( obj_index[i].vnum >= bottom && obj_index[i].vnum <= top )
        {
            counter++;

            snprintf ( buf, sizeof buf, "%s%4d%s) [%s%-5d%s] %s%-35.35s %s[%s]%s%s(%s%s%s)\r\n",
                       QGRN, counter, QNRM, QGRN, obj_index[i].vnum, QNRM,
                       QCYN, obj_proto[i].short_description, QYEL,
                       item_types[ ( int ) obj_proto[i].obj_flags.type_flag], QNRM,
                       obj_proto[i].proto_script ? " [TRIG]" : "",
                       QYEL, material_name ( GET_OBJ_MATERIAL ( obj_proto + i ) ), QNRM
                     );
            DYN_RESIZE ( buf );
        }
    }

    if ( counter == 0 )
    {
        snprintf ( buf, sizeof buf, "None found.\r\n" );
        DYN_RESIZE ( buf );
    }
    page_string ( ch->desc, dynbuf, DYN_BUFFER );
}


/*
 * List all shops in a zone.
 */
void list_shops ( Character *ch, zone_rnum rnum, shop_vnum vmin, shop_vnum vmax )
{
    shop_rnum i;
    shop_vnum bottom, top;
    int j, counter = 0;
    struct shop_data *shop;

    if ( rnum != NOWHERE )
    {
        bottom = zone_table[rnum].bot;
        top    = zone_table[rnum].top;
    }
    else
    {
        bottom = vmin;
        top    = vmax;
    }

    ch->Send (
        "Index VNum    RNum    Keeper  Shop Room(s)\r\n"
        "----- ------- ------- ------- -----------------------------------------\r\n" );

    DYN_DEFINE;
    DYN_CREATE;
    *dynbuf = '\0';
    char buf[MAX_INPUT_LENGTH];
    for ( i = 0; i <= top_shop; i++ )
    {
        if ( SHOP_NUM ( i ) >= bottom && SHOP_NUM ( i ) <= top )
        {
            counter++;

            shop = shop_index + i;
            /* the +1 is strange but fits the rest of the shop code */
            snprintf ( buf, sizeof buf, "%s%4d%s) [%s%-5d%s] [%s%-5d%s] [%s%-5d%s]",
                       QGRN, counter, QNRM, QGRN, SHOP_NUM ( i ), QNRM, QGRN, i + 1, QNRM, QGRN, S_KEEPER ( shop ) == NOBODY ? -1 : S_KEEPER ( shop ), QNRM );
            DYN_RESIZE ( buf );

            /* Thanks to Ken Ray (kenr86@hotmail.com) for this display fix -- Welcor*/
            for ( j = 0; SHOP_ROOM ( i, j ) != NULL; j++ )
            {
                snprintf ( buf, sizeof buf, "%s%s[%s%-5d%s]%s",
                           ( ( j > 0 ) && ( j % 6 == 0 ) ) ? "\r\n                      " : " ",
                           QCYN, QYEL, SHOP_ROOM ( i, j )->number, QCYN, QNRM );
                DYN_RESIZE ( buf );
            }

            if ( j == 0 )
            {
                snprintf ( buf, sizeof buf, "%sNone.%s", QCYN, QNRM );
                DYN_RESIZE ( buf );
            }

            snprintf ( buf, sizeof buf, "\r\n" );
            DYN_RESIZE ( buf );
        }
    }

    if ( counter == 0 )
    {
        snprintf ( buf, sizeof buf, "None found.\r\n" );
        DYN_RESIZE ( buf );
    }
    page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

/*
 * List all zones in the world (sort of like 'show zones').
 */
void list_zones ( Character *ch, zone_rnum rnum, zone_vnum vmin, zone_vnum vmax )
{
    int counter = 0;
    zone_rnum i;
    zone_vnum bottom, top;

    if ( rnum != NOWHERE )
    {
        /* Only one parameter was supplied - just list that zone */
        print_zone ( ch, rnum );
        return;
    }
    else
    {
        bottom = vmin;
        top    = vmax;
    }

    ch->Send (
        "VNum  Zone Name                      Builder(s)\r\n"
        "----- ------------------------------ --------------------------------------\r\n" );

    DYN_DEFINE;
    DYN_CREATE;
    *dynbuf = '\0';
    char buf[MAX_INPUT_LENGTH];
    for ( i = 0; i <= top_of_zone_table; i++ )
    {
        if ( zone_table[i].number >= bottom && zone_table[i].number <= top )
        {
            snprintf ( buf, sizeof buf, "[%s%3d%s] %s%-30.30s %s%-1s%s\r\n",
                       QGRN, zone_table[i].number, QNRM, QCYN, zone_table[i].name,
                       QYEL, zone_table[i].builders ? zone_table[i].builders : "None.", QNRM );
            DYN_RESIZE ( buf );
            counter++;
        }
    }

    if ( !counter )
    {
        snprintf ( buf, sizeof buf, "  None found within those parameters.\r\n" );
        DYN_RESIZE ( buf );
    }
    page_string ( ch->desc, dynbuf, DYN_BUFFER );
}



/*
 * Prints all of the zone information for the selected zone.
 */
void print_zone ( Character *ch, zone_rnum rnum )
{
    int size_rooms, size_objects, size_mobiles, i;
    room_vnum top, bottom;
    int largest_table;

#if CIRCLE_UNSIGNED_INDEX
    if ( rnum == NOWHERE || rnum > top_of_zone_table )
    {
#else
    if ( rnum < 0 || rnum > top_of_zone_table )
    {
#endif
        log ( "SYSERR: oasis_list: print_zone: Invalid zone number %d passed! (0-%d)", rnum, top_of_zone_table );
        ch->Send ( "Sorry but that zone is bigger than any we have.\r\n" );
        return;
    }

    /****************************************************************************/
    /** Locate the largest of the three, top_of_world, top_of_mobt, or         **/
    /** top_of_objt.                                                           **/
    /****************************************************************************/
    if ( top_of_world >= top_of_objt )
        largest_table = top_of_world;
    else
        largest_table = top_of_objt;

    /****************************************************************************/
    /** Initialize some of the variables.                                      **/
    /****************************************************************************/
    size_rooms   = 0;
    size_objects = 0;
    size_mobiles = 0;
    top          = zone_table[rnum].top;
    bottom       = zone_table[rnum].bot;

    for ( i = 0; i <= largest_table; i++ )
    {
        if ( i <= top_of_world && world_vnum[i] )
            if ( world_vnum[i]->zone == rnum )
                size_rooms++;

        if ( i <= top_of_objt )
            if ( obj_index[i].vnum >= bottom && obj_index[i].vnum <= top )
                size_objects++;
    }
    for ( mi_iter it = mob_index.begin();it != mob_index.end();it++ )
        if ( ( it->first ) >= bottom && ( it->first ) <= top )
            size_mobiles++;

    /****************************************************************************/
    /** Display all of the zone information at once.                           **/
    /****************************************************************************/
    ch->Send (
        "%sVirtual Number = %s%d\r\n"
        "%sName of zone   = %s%s\r\n"
        "%sBuilders       = %s%s\r\n"
        "%sLifespan       = %s%d\r\n"
        "%sAge            = %s%d\r\n"
        "%sBottom of Zone = %s%d\r\n"
        "%sTop of Zone    = %s%d\r\n"
        "%sReset Mode     = %s%s\r\n"
        "%sSize\r\n"
        "%s   Rooms       = %s%d\r\n"
        "%s   Objects     = %s%d\r\n"
        "%s   Mobiles     = %s%d%s\r\n",
        QGRN, QCYN, zone_table[rnum].number,
        QGRN, QCYN, zone_table[rnum].name,
        QGRN, QCYN, zone_table[rnum].builders,
        QGRN, QCYN, zone_table[rnum].lifespan,
        QGRN, QCYN, zone_table[rnum].age,
        QGRN, QCYN, zone_table[rnum].bot,
        QGRN, QCYN, zone_table[rnum].top,
        QGRN, QCYN, zone_table[rnum].reset_mode ? ( ( zone_table[rnum].reset_mode == 1 ) ?
                "Reset when no players are in zone." : "Normal reset." ) : "Never reset",
                QGRN,
                QGRN, QCYN, size_rooms,
                QGRN, QCYN, size_objects,
                QGRN, QCYN, size_mobiles, QNRM );
}

/* List code by Ronald Evers - dlanor@xs4all.nl */
void list_triggers ( Character *ch, zone_rnum rnum, trig_vnum vmin, trig_vnum vmax )
{
    int bottom, top, counter = 0;
    unsigned int i;
    char trgtypes[256];

    /** Expect a minimum / maximum number if the rnum for the zone is NOWHERE. **/
    if ( rnum != NOWHERE )
    {
        bottom = zone_table[rnum].bot;
        top    = zone_table[rnum].top;
    }
    else
    {
        bottom = vmin;
        top    = vmax;
    }


    /** Store the header for the room listing. **/
    ch->Send (
        "Index VNum    Trigger Name                        Type\r\n"
        "----- ------- -------------------------------------------------------\r\n" );

    DYN_DEFINE;
    DYN_CREATE;
    *dynbuf = '\0';
    char buf[MAX_INPUT_LENGTH];
    /** Loop through the world and find each room. **/
    for ( i = 0; i < top_of_trigt; i++ )
    {
        /** Check to see if this room is one of the ones needed to be listed.    **/
        if ( ( trig_index[i]->vnum >= bottom ) && ( trig_index[i]->vnum <= top ) )
        {
            counter++;

            snprintf ( buf, sizeof buf, "%4d) [%s%5d%s] %s%-45.45s ",
                       counter, QGRN, trig_index[i]->vnum, QNRM, QCYN, trig_index[i]->proto->name );
            DYN_RESIZE ( buf );

            if ( trig_index[i]->proto->attach_type == OBJ_TRIGGER )
            {
                new_sprintbit ( GET_TRIG_TYPE ( trig_index[i]->proto ), otrig_types, trgtypes, sizeof ( trgtypes ) );
                snprintf ( buf, sizeof buf, "obj %s%s%s\r\n", QYEL, trgtypes, QNRM );
            }
            else if ( trig_index[i]->proto->attach_type == WLD_TRIGGER )
            {
                new_sprintbit ( GET_TRIG_TYPE ( trig_index[i]->proto ), wtrig_types, trgtypes, sizeof ( trgtypes ) );
                snprintf ( buf, sizeof buf, "wld %s%s%s\r\n", QYEL, trgtypes, QNRM );
            }
            else
            {
                new_sprintbit ( GET_TRIG_TYPE ( trig_index[i]->proto ), trig_types, trgtypes, sizeof ( trgtypes ) );
                snprintf ( buf, sizeof buf, "mob %s%s%s\r\n", QYEL, trgtypes, QNRM );
            }
            DYN_RESIZE ( buf );
        }
    }

    if ( counter == 0 )
    {
        snprintf ( buf, sizeof buf, "No triggers found for zone #%d\r\n", zone_table[rnum].number );
        DYN_RESIZE ( buf );
    }
    page_string ( ch->desc, dynbuf, DYN_BUFFER );
}
