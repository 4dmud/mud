/**************************************************************************
*  File: objcmd.c                                                         *
*  Usage: contains the command_interpreter for objects,                   *
*         object commands.                                                *
*                                                                         *
*                                                                         *
*  $Author: w4dimenscor $
*  $Date: 2007/11/18 06:50:38 $
*  $Revision: 1.20 $
**************************************************************************/

#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "screen.h"
#include "dg_scripts.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "constants.h"

int can_fight ( Character *ch, Character *vict, int silent );
void start_fighting_delay ( Character *ch, Character *vict );
void die ( Character *ch, Character *killer );
int real_zone ( int number );
zone_rnum real_zone_by_thing ( room_vnum vznum );
#define OCMD(name)  \
   void (name)(obj_data *obj, char *argument, int cmd, int subcmd)

int followers_to_master ( Character *ch, room_rnum was_in );
void obj_log ( obj_data *obj, const char *format, ... );
room_rnum find_obj_target_room ( obj_data *obj, char *rawroomstr );
OCMD ( do_oecho );
OCMD ( do_oforce );
OCMD ( do_ozoneecho );
OCMD ( do_osend );
OCMD ( do_orecho );
OCMD ( do_otimer );
OCMD ( do_otransform );
OCMD ( do_opurge );
OCMD ( do_oteleport );
OCMD ( do_dgoload );
OCMD ( do_odamage );
OCMD ( do_oasound );
OCMD ( do_odoor );
OCMD ( do_osetval );
OCMD ( do_oat );
void update_timer ( struct obj_data *obj );
void obj_command_interpreter ( obj_data *obj, char *argument );
void update_affects ( struct obj_data *obj );

struct obj_command_info
{
    const char *command;
    void ( *command_pointer ) ( obj_data * obj, char *argument, int cmd, int subcmd );
    int subcmd;
};


/* do_osend */
#define SCMD_OSEND         0
#define SCMD_OECHOAROUND   1




/* attaches object name and vnum to msg and sends it to script_log */
void obj_log ( obj_data *obj, const char *format, ... )
{
    va_list args;
    char buf[MAX_STRING_LENGTH];

    snprintf ( buf, sizeof ( buf ), "Obj (%s, VNum %d):: %s", obj->short_description, GET_OBJ_VNUM ( obj ), format );

    va_start ( args, format );
    script_vlog ( buf, args );
    va_end ( args );
}

/* returns the room that the object or object's carrier is in */
room_rnum obj_room ( obj_data * obj )
{
    if ( IN_ROOM ( obj ) != NULL )
        return IN_ROOM ( obj );
    else if ( obj->carried_by )
        return IN_ROOM ( obj->carried_by );
    else if ( obj->worn_by )
        return IN_ROOM ( obj->worn_by );
    else if ( obj->in_obj )
        return obj_room ( obj->in_obj );
    else
        return NULL;
}


/* returns the real room number, or NOWHERE if not found or invalid */
room_rnum find_obj_target_room ( obj_data * obj, char *rawroomstr )
{
    int tmp;
    room_rnum location;
    Character *target_mob;
    obj_data *target_obj;
    char roomstr[MAX_INPUT_LENGTH];

    one_argument ( rawroomstr, roomstr );

    if ( !*roomstr )
        return NULL;

    if ( isdigit ( *roomstr ) && !strchr ( roomstr, '.' ) )
    {
        tmp = atoi ( roomstr );
        if ( ( location = real_room ( tmp ) ) == NULL )
            return NULL;
    }

    else if ( ( target_mob = get_char_by_obj ( obj, roomstr ) ) )
        location = IN_ROOM ( target_mob );
    else if ( ( target_obj = get_obj_by_obj ( obj, roomstr ) ) )
    {
        if ( IN_ROOM ( target_obj ) != NULL )
            location = IN_ROOM ( target_obj );
        else
            return NULL;
    }
    else
        return NULL;

    /* a room has been found.  Check for permission */
    if ( ROOM_FLAGGED ( location, ROOM_GODROOM ) ||
#ifdef ROOM_IMPROOM
            ROOM_FLAGGED ( location, ROOM_IMPROOM ) ||
#endif
            ROOM_FLAGGED ( location, ROOM_HOUSE ) )
        return NULL;

    if ( ROOM_FLAGGED ( location, ROOM_PRIVATE ) &&
            location->people && location->people->next_in_room )
        return NULL;

    return location;
}



/* Object commands */

OCMD ( do_oecho )
{
    room_rnum room;

    skip_spaces ( &argument );

    if ( !*argument )
        obj_log ( obj, "oecho called with no args" );
    else if ( ( room = obj_room ( obj ) ) != NULL )
    {
        if ( room->people )
            sub_write ( argument, room->people, TRUE,TO_ROOM | TO_CHAR );
    }
    else
        obj_log ( obj, "oecho called by object in NOWHERE" );
}


OCMD ( do_oforce )
{
    Character *ch, *next_ch;
    room_rnum room;
    char arg1[MAX_INPUT_LENGTH], *line;

    line = one_argument ( argument, arg1 );

    if ( !*arg1 || !*line )
    {
        obj_log ( obj, "oforce called with too few args" );
        return;
    }

    if ( !str_cmp ( arg1, "all" ) )
    {
        if ( ( room = obj_room ( obj ) ) == NULL )
            obj_log ( obj, "oforce called by object in NOWHERE" );
        else
        {
            for ( ch = room->people; ch; ch = next_ch )
            {
                next_ch = ch->next_in_room;

                if ( valid_dg_target ( ch, FALSE ) )
                    command_interpreter ( ch, line );
            }
        }
    }

    else
    {
        if ( ( ch = get_char_by_obj ( obj, arg1 ) ) )
        {
            if ( IS_NPC(ch) || GET_LEVEL ( ch ) < LVL_IMMORT )
                command_interpreter ( ch, line );
        }
        else
            obj_log ( obj, "oforce: no target found" );
    }
}

OCMD ( do_olag )
{
    Character *victim;
    int w = 0;
    //int room;
    char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

    two_arguments ( argument, arg1, arg2 );

    if ( !*arg1 || !*arg2 )
    {
        obj_log ( obj, "olag called with too few args" );
        return;
    }



    if ( *arg1 == UID_CHAR )
    {
        if ( ! ( victim = get_char ( arg1 ) ) )
        {
            obj_log ( obj, "olag: victim (%s) not found", arg1 );
            return;
        }
    }
    else if ( ! ( victim = get_char_by_obj ( obj, arg1 ) ) )
    {
        obj_log ( obj, "olag: victim (%s) not found", arg1 );
        return;
    }

    if ( !IS_NPC ( victim ) && PRF_FLAGGED ( victim, PRF_NOHASSLE ) )
    {
        obj_log ( obj, "olag: target has nohassle on" );
        return;
    }

    if ( ( w = atoi ( arg2 ) ) < 1 )
        return;

    if ( w > 300 )
    {
        obj_log ( obj, "olag: duration longer than 30 seconds, outside range" );
        return;
    }

    w = FTOI ( ( w RL_SEC ) *0.1 );

    WAIT_STATE ( victim, w );
    return;
}

OCMD ( do_ozoneecho )
{
    int zone;
    char room_number[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;

    msg = any_one_arg ( argument, room_number );
    skip_spaces ( &msg );

    if ( !*room_number || !*msg )
        obj_log ( obj, "ozoneecho called with too few args" );
    else if ( ( zone = real_zone ( atoi ( room_number ) ) ) == NOWHERE )
        obj_log ( obj, "ozoneecho called for nonexistant zone" );
    else
    {
        sprintf ( buf, "%s\r\n", msg );
        send_to_zone ( buf, zone );
    }
}



OCMD ( do_osend )
{
    char buf[MAX_INPUT_LENGTH], *msg;
    Character *ch;

    msg = any_one_arg ( argument, buf );

    if ( !*buf )
    {
        obj_log ( obj, "osend called with no args" );
        return;
    }

    skip_spaces ( &msg );

    if ( !*msg )
    {
        obj_log ( obj, "osend called without a message" );
        return;
    }

    if ( ( ch = get_char_by_obj ( obj, buf ) ) )
    {
        if ( subcmd == SCMD_OSEND )
            sub_write ( msg, ch, TRUE, TO_CHAR );
        else if ( subcmd == SCMD_OECHOAROUND )
        {
            if ( IN_ROOM ( ch ) != NULL )
            {
                sub_write ( msg, ch, TRUE, TO_ROOM );
            }
            else
                obj_log ( obj, "calling oechoaround when %s is in nowhere", GET_NAME ( ch ) );
        }
    }

    else
        obj_log ( obj, "no target found for osend" );
}

/* increases the target's exp */
OCMD ( do_oexp )
{
    Character *ch;
    char name[MAX_INPUT_LENGTH], amount[MAX_INPUT_LENGTH];

    two_arguments ( argument, name, amount );

    if ( !*name || !*amount )
    {
        obj_log ( obj, "oexp: too few arguments" );
        return;
    }

    if ( ( ch = get_char_by_obj ( obj, name ) ) )
        gain_exp ( ch, ( gold_int ) atoll ( amount ) );
    else
    {
        obj_log ( obj, "oexp: target not found" );
        return;
    }
}


/* set the object's timer value */
OCMD ( do_otimer )
{
    char arg[MAX_INPUT_LENGTH];

    one_argument ( argument, arg );

    if ( !*arg )
        obj_log ( obj, "otimer: missing argument" );
    else if ( !isdigit ( *arg ) )
        obj_log ( obj, "otimer: bad argument" );
    else
    {
        GET_OBJ_TIMER ( obj ) = atoi ( arg );
        update_timer ( obj );
    }

}


/* transform into a different object */
/* note: this shouldn't be used with containers unless both objects */
/* are containers! */
OCMD ( do_otransform )
{
    char arg[MAX_INPUT_LENGTH];
    obj_data *o, tmpobj;
    Character *wearer = nullptr, *carrier = nullptr;
    int pos = 0;
    long objid = 0;

    one_argument ( argument, arg );

    if ( !*arg )
        obj_log ( obj, "otransform: missing argument" );
    else if ( !isdigit ( *arg ) )
        obj_log ( obj, "otransform: bad argument" );
    else
    {
        o = read_object ( atoi ( arg ), VIRTUAL );
        if ( o == NULL )
        {
            obj_log ( obj, "otransform: bad object vnum" );
            return;
        }

        if ( GET_OBJ_TYPE ( obj ) == ITEM_CONTAINER && GET_OBJ_TYPE ( o ) != ITEM_CONTAINER )
        {
            obj_log ( obj, "otransform: trying to transform a container to non-container [%d]", GET_OBJ_VNUM ( o ) );
            extract_obj ( o );
            return;
        }

        objid = GET_ID ( o );

        if ( obj->worn_by )
        {
            pos = obj->worn_on;
            wearer = obj->worn_by;
            unequip_char ( obj->worn_by, pos );
        }
        else if ( obj->carried_by )
        {
            carrier = obj->carried_by;
            obj_from_char ( obj );
        }

        /* move new obj info over to old object and delete new obj */
        memcpy ( &tmpobj, o, sizeof ( *o ) );
        tmpobj.in_room = IN_ROOM ( obj );
        tmpobj.carried_by = obj->carried_by;
        tmpobj.in_locker = obj->in_locker;
        tmpobj.worn_by = obj->worn_by;
        tmpobj.worn_on = obj->worn_on;
        tmpobj.in_obj = obj->in_obj;
        tmpobj.contains = obj->contains;
        tmpobj.id = obj->id;
        tmpobj.proto_script = obj->proto_script;
        tmpobj.script = obj->script;
        tmpobj.next_content = obj->next_content;
        memcpy ( obj, &tmpobj, sizeof ( *obj ) );

        if ( wearer )
        {
            if ( can_wear_on_pos ( obj, pos ) )
                equip_char ( wearer, obj, pos );
            else
                obj_to_char ( obj, wearer );
        }
        else if ( carrier )
            obj_to_char ( obj, carrier );

        GET_ID ( o ) = objid;
        extract_obj ( o );
    }
}


/* purge all objects and npcs in room, or specified object or mob */
/* don't purge objects in a house, crashproof objects in a room, or player corpses */
OCMD ( do_opurge )
{
    char arg[MAX_INPUT_LENGTH];
    Character *ch, *next_ch;
    obj_data *o, *next_obj;
    room_rnum rm;

    one_argument ( argument, arg );
//***** handle no-argument cases
    if ( !*arg )
    {
        /* purge all */
        if ( ( rm = obj_room ( obj ) ) != NULL )
        {
            for ( ch = rm->people; ch; ch = next_ch )
            {
                next_ch = ch->next_in_room;
                if ( IS_NPC ( ch ) && !DEAD ( ch ) )
                    extract_char ( ch );
            }

            if ( ROOM_FLAGGED ( rm, ROOM_HOUSE ) )
                return;

            for ( o = rm->contents; o; o = next_obj )
            {
                next_obj = o->next_content;
                if ( o != obj && !OBJ_FLAGGED ( o, ITEM_CRASHPROOF ) && !OBJ_FLAGGED ( o, ITEM_PC_CORPSE ) )
                    extract_obj ( o );
            }
        }

        return;
    } /* no arg */
//***** get_char_by_obj will parse an arg starting
//***** with a UID char as a proper UID

    if ( !strcmp ( arg, "target_not_found" ) )
        return;

    ch = get_char_by_obj ( obj, arg );
    if ( !ch )
    {
//***** so will get_obj_by_obj!
        o = get_obj_by_obj ( obj, arg );
        if ( o )
        {
            if ( o == obj )
            {
                dg_owner_purged = 1;
                extract_obj ( o );
            }
            else if ( !( IN_ROOM ( o ) && ( OBJ_FLAGGED ( o, ITEM_CRASHPROOF ) || ROOM_FLAGGED ( IN_ROOM ( o ), ROOM_HOUSE ) ) ) && !OBJ_FLAGGED ( o, ITEM_PC_CORPSE ) )
                extract_obj ( o );
        }
        else
            obj_log ( obj, "opurge: bad argument" );

        return;
    }

    if ( !IS_NPC ( ch ) )
    {
        obj_log ( obj, "opurge: trying to purge a PC" );
        return;
    }

    if ( !DEAD ( ch ) )
        extract_char ( ch );
}

OCMD ( do_oteleport )
{
    Character *ch = nullptr, *next_ch, *target_char = nullptr;
    room_rnum target_room = nullptr;
    char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
    obj_data *object = nullptr, *target_container = nullptr;

    argument = two_arguments ( argument, arg1, arg2 );
    skip_spaces ( &argument );

    if ( !*arg1 || !*arg2 )
    {
        obj_log ( obj, "oteleport called with too few args (%s, %s)", arg1, arg2 );
        return;
    }

    target_room = get_room ( arg2 );
    if ( target_room == nullptr )
    {
        target_char = get_char_by_obj ( obj, arg2 );
        if ( target_char == nullptr )
            target_container = get_obj_by_obj ( obj, arg2 );
    }

    if ( !str_cmp ( arg1, "all" ) )
    {
        if ( target_room == nullptr )
        {
            obj_log ( obj, "oteleport target is an invalid room (%s, %s)", arg1, arg2 );
            return;
        }

        room_rnum rm = obj_room ( obj );
        if ( rm == nullptr )
            return;

        for ( ch = rm->people; ch; ch = next_ch )
        {
            next_ch = ch ? ch->next_in_room : nullptr;
            if ( !valid_dg_target ( ch, TRUE ) )
                continue;
            if ( ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_NORECALL ) && str_cmp(argument, "override") )
            {
                ch->Send ( "The magic fizzles out leaving you stranded.\r\n" );
                break;
            }
            char_from_room ( ch );
            char_to_room ( ch, target_room );
            enter_wtrigger ( IN_ROOM ( ch ), ch, -1 );
        }
    }
    else if ( ( ch = get_char_by_obj ( obj, arg1 ) ) )
    {
        if ( valid_dg_target ( ch, TRUE ) )
        {
            if ( ROOM_FLAGGED ( ch->in_room, ROOM_NORECALL ) && str_cmp(argument, "override") )
            {
                ch->Send ( "The magic fizzles out leaving you stranded.\r\n" );
                return;
            }
            room_rnum was_in = IN_ROOM ( ch );
            char_from_room ( ch );
            char_to_room ( ch, target_room );
            enter_wtrigger ( IN_ROOM ( ch ), ch, -1 );
            if ( isname ( argument, "followers" ) )
                followers_to_master ( ch, was_in );
        }
        return;
    }
    else if ( ( object = get_obj_by_obj ( obj, arg1 ) ) )
    {
        if ( target_room || target_char || target_container )
        {
            if ( IN_ROOM ( object ) )
                obj_from_room ( object );
            else if ( object->in_obj )
                obj_from_obj ( object );
            else if ( object->carried_by )
                obj_from_char ( object );
            else if ( object->worn_by )
                unequip_char ( object->worn_by, object->worn_on );
        }

        if ( target_room )
            obj_to_room ( object, target_room );
        else if ( target_char )
            obj_to_char ( object, target_char );
        else if ( target_container )
            obj_to_obj ( object, target_container );
        else
            obj_log ( obj, "oteleport: no target found (%s, %s)", arg1, arg2 );
    }
    else
        obj_log ( obj, "oteleport: no target found (%s, %s)", arg1, arg2 );
}


OCMD ( do_dgoload )
{
    char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
    int num = 0;
    room_rnum room;
    Character *mob;
    obj_data *object;
    char *target;
    Character *tch;
    obj_data *cnt;
    int pos;

    target = two_arguments ( argument, arg1, arg2 );

    skip_spaces ( &target );

    if ( !*arg1 || !*arg2 || !is_number ( arg2 )
            || ( ( num = atoi ( arg2 ) ) < 0 ) )
    {
        obj_log ( obj, "oload: bad syntax" );
        return;
    }

    if ( ( room = obj_room ( obj ) ) == NULL )
    {
        obj_log ( obj, "oload: object in NOWHERE trying to load" );
        return;
    }

    /* load mob to target room - Jamie Nelson, April 13 2004 */
    if ( is_abbrev ( arg1, "mob" ) )
    {
        room_rnum rnum;
        if ( !target || !*target )
            rnum = room;
        else
        {
            if ( !isdigit ( *target ) || ( rnum = real_room ( atoi ( target ) ) ) == NULL )
            {
                obj_log ( obj, "oload: room target vnum doesn't exist (loading mob vnum %d to room %s)", num, target );
                return;
            }
        }
        if ( ( mob = read_mobile ( num ) ) == NULL )
        {
            obj_log ( obj, "oload: bad mob vnum" );
            return;
        }
        char_to_room ( mob, rnum );
        load_mtrigger ( mob );
        if ( SCRIPT ( obj ) )   // it _should_ have, but it might be detached.
        {
            char buf[MAX_INPUT_LENGTH];
            sprintf ( buf, "%c%ld", UID_CHAR, GET_ID ( mob ) );
            add_var ( & ( SCRIPT ( obj )->global_vars ), "loaded", buf, 0 );
        }
    }

    else if ( is_abbrev ( arg1, "obj" ) )
    {
        if ( ( object = read_object ( num, VIRTUAL ) ) == NULL )
        {
            obj_log ( obj, "oload: bad object vnum %d", num );
            return;
        }
        if ( SCRIPT ( obj ) )   // it _should_ have, but it might be detached.
        {
            char buf[MAX_INPUT_LENGTH];
            sprintf ( buf, "%c%ld", UID_CHAR, GET_ID ( object ) );
            add_var ( & ( SCRIPT ( obj )->global_vars ), "loaded", buf, 0 );
        }
        /* special handling to make objects able to load on a person/in a container/worn etc. */
        if ( !target || !*target )
        {
            if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
                obj_log ( obj, "[TOKEN] loads %s to room %d", object->short_description, room->number );
            obj_to_room ( object, room );
            load_otrigger ( object );
            return;
        }

        /* load to char */
        two_arguments ( target, arg1, arg2 ); /* recycling ... */
        tch = get_char_near_obj ( obj, arg1 );
        if ( tch )
        {
            if ( *arg2 && ( pos = find_eq_pos_script ( arg2 ) ) >= 0 && !GET_EQ ( tch, pos ) && can_wear_on_pos ( object, pos ) )
            {
                equip_char ( tch, object, pos );
                load_otrigger ( object );
                return;
            }
            if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
                obj_log ( obj, "[TOKEN] loads %s to %s in %d", object->short_description, GET_NAME ( tch ), IN_ROOM ( tch ) ? IN_ROOM ( tch )->number : -1 );
            obj_to_char ( object, tch );
            load_otrigger ( object );
            return;
        }

        /* load to container */
        cnt = ( *arg1 == UID_CHAR ) ? get_obj ( arg1 ) : get_obj_in_list ( arg1, room->contents );
        if ( cnt && GET_OBJ_TYPE ( cnt ) == ITEM_CONTAINER )
        {
            if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
                obj_log ( obj, "[TOKEN] loads %s to %s", object->short_description, cnt->short_description );
            obj_to_obj ( object, cnt );
            load_otrigger ( object );
            return;
        }

        /* load to room */
        Room *r = get_room ( arg1 );
        if ( r )
        {
            if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
                obj_log ( obj, "[TOKEN] loads %s to room %d", object->short_description, r->number );
            obj_to_room ( object, r );
            load_otrigger ( object );
        }
        else
        {
            if ( GET_OBJ_VNUM ( object ) >= 3300 && GET_OBJ_VNUM ( object ) <= 3312 )
                obj_log ( obj, "[TOKEN] loads %s, but target %s couldn't be found, purging.", object->short_description, arg1 );
            else
                obj_log ( obj, "loads %s, but target %s couldn't be found, purging.", object->short_description, arg1 );
            extract_obj ( object );
        }
    }

    else
        obj_log ( obj, "oload: bad type %s", argument );
}

OCMD ( do_oasound )
{
    room_rnum room;
    int door;

    skip_spaces ( &argument );

    if ( !*argument )
    {
        obj_log ( obj, "oasound called with no args" );
        return;
    }

    if ( ( room = obj_room ( obj ) ) == NULL )
    {
        obj_log ( obj, "oecho called by object in NOWHERE" );
        return;
    }

    for ( door = 0; door < NUM_OF_DIRS; door++ )
    {
        if ( room->dir_option[door] != NULL &&
                room->dir_option[door]->to_room != NULL &&
                room->dir_option[door]->to_room != room &&
                room->dir_option[door]->to_room->people )
            sub_write ( argument, room->dir_option[door]->to_room->people, TRUE, TO_ROOM | TO_CHAR );
    }
}



OCMD ( do_odamage )
{
    char name[MAX_INPUT_LENGTH], amount[MAX_INPUT_LENGTH];
    int dam = 0;
    Character *ch = NULL;
    Character *vict = NULL;

    two_arguments ( argument, name, amount );

    if ( !*name )
    {
        obj_log ( obj, "odamage: bad syntax - no name" );
        return;
    }

    if ( !*amount )
    {
        obj_log ( obj, "odamage: bad syntax - no damage or non numeric" );
        return;
    }

    if ( obj->worn_by )
        ch = obj->worn_by;
    else if ( obj->carried_by )
        ch = obj->carried_by;

    dam = atoi ( amount );
    if ( !str_cmp ( "all", name ) )
    {
        Character *tvict;
        room_rnum rm;
        if ( ( rm = obj_room ( obj ) ) == NULL )
            return;
        /**TODO: I hate this loop, because it is possable that on the extraction
                 of a mob or player after damage, it could wipe the next char.
          **/
        for ( vict = rm->people;vict;vict = tvict )
        {
            if ( !DEAD ( vict ) )
                tvict = vict->next_in_room;
            else
                tvict = NULL;

            if ( !IS_NPC ( vict ) )
            {
                if ( ch && ch != vict && !IS_NPC ( ch ) && can_fight ( ch, vict, TRUE ) )
                {
                    start_fighting_delay ( ch, vict );
                    FIGHTING ( vict ) = ch;
                }
                script_damage ( vict, dam );
            }
        }
        return;
    }
    vict = get_char_by_obj ( obj, name );

    if ( !vict )
    {
        obj_log ( obj, "odamage: target not found" );
        return;
    }

    if ( ch && ch != vict && !IS_NPC ( ch ) && can_fight ( ch, vict, TRUE ) )
    {
        start_fighting_delay ( ch, vict );
        FIGHTING ( vict ) = ch;
    }
    script_damage ( vict, dam );
}


OCMD ( do_odoor )
{
    char target[MAX_INPUT_LENGTH], direction[MAX_INPUT_LENGTH];
    char field[MAX_INPUT_LENGTH], *value;
    Room *rm;
    struct room_direction_data *newexit;
    int dir, fd;
    room_rnum to_room;

    const char *door_field[] =
    {
        "purge",
        "description",
        "flags",
        "key",
        "name",
        "room",
        "\n"
    };


    argument = two_arguments ( argument, target, direction );
    value = one_argument ( argument, field );
    skip_spaces ( &value );

    if ( !*target || !*direction || !*field )
    {
        obj_log ( obj, "odoor called with too few args" );
        return;
    }

    if ( ( rm = get_room ( target ) ) == NULL )
    {
        obj_log ( obj, "odoor: invalid target" );
        return;
    }

    if ( ( dir = search_block ( direction, dirs, FALSE ) ) == -1 )
    {
        obj_log ( obj, "odoor: invalid direction" );
        return;
    }

    if ( ( fd = search_block ( field, door_field, FALSE ) ) == -1 )
    {
        obj_log ( obj, "odoor: invalid field" );
        return;
    }

    newexit = rm->dir_option[dir];

    /* purge exit */
    if ( fd == 0 )
    {
        if ( newexit )
        {
            if ( newexit->general_description )
                free ( newexit->general_description );
            if ( newexit->keyword )
                free ( newexit->keyword );
            free ( newexit );
            rm->dir_option[dir] = NULL;
        }
    }

    else
    {
        if ( !newexit )
        {
            CREATE ( newexit, struct room_direction_data, 1 );
            rm->dir_option[dir] = newexit;
        }

        switch ( fd )
        {
            case 1:		/* description */
                if ( newexit->general_description )
                    free ( newexit->general_description );
                CREATE ( newexit->general_description, char, strlen ( value ) + 3 );
                strcpy ( newexit->general_description, value );
                strcat ( newexit->general_description, "\r\n" );
                break;
            case 2:		/* flags       */
                newexit->exit_info = ( sh_int ) asciiflag_conv ( value );
                break;
            case 3:		/* key         */
                newexit->key = atoi ( value );
                break;
            case 4:		/* name        */
                if ( newexit->keyword )
                    free ( newexit->keyword );
                CREATE ( newexit->keyword, char, strlen ( value ) + 1 );
                strcpy ( newexit->keyword, value );
                break;
            case 5:		/* room        */
                if ( ( to_room = real_room ( atoi ( value ) ) ) != NULL )
                    newexit->to_room = to_room;
                else
                    obj_log ( obj, "odoor: invalid door target" );
                break;
        }
    }
}


OCMD ( do_osetval )
{
    char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];
    int position, new_value;

    two_arguments ( argument, arg1, arg2 );
    if ( !*arg1 || !*arg2 || !is_number ( arg1 ) || !is_number ( arg2 ) )
    {
        obj_log ( obj, "osetval: bad syntax" );
        return;
    }

    position = atoi ( arg1 );
    new_value = atoi ( arg2 );
    if ( position >= 0 && position < NUM_OBJ_VAL_POSITIONS + NUM_OBJ_FLOATING_VAL_POSITIONS )
    {
        if ( position < 8 )
            GET_OBJ_VAL ( obj, position ) = new_value;
        else if ( position == 8 )
        {
            GET_OBJ_QUALITY ( obj ) = IRANGE ( 0, new_value, GET_OBJ_MAX_QUALITY ( obj ) );
            update_affects ( obj );
        }
        else if ( position < 14 )
            GET_OBJ_VAL ( obj, position - 1 ) = new_value;
        else if ( position == 14 )
        {
            GET_OBJ_MAX_QUALITY ( obj ) = IRANGE ( 0, new_value, 100 );
            if ( GET_OBJ_QUALITY ( obj ) > GET_OBJ_MAX_QUALITY ( obj ) )
                GET_OBJ_QUALITY ( obj ) = GET_OBJ_MAX_QUALITY ( obj );
            update_affects ( obj );
        }
    }
    else
        obj_log ( obj, "osetval: position out of bounds" );
}

OCMD ( do_oat )
{
    room_rnum loc = NULL;
    Character *ch;
    obj_data *object;
    char arg[MAX_INPUT_LENGTH], *command;

    command = any_one_arg ( argument, arg );

    if ( !*arg )
    {
        obj_log ( obj, "oat called with no args" );
        return;
    }

    skip_spaces ( &command );

    if ( !*command )
    {
        obj_log ( obj, "oat called without a command" );
        return;
    }

    if ( isdigit ( *arg ) ) loc = real_room ( atoi ( arg ) );
    else if ( ( ch = get_char_by_obj ( obj, arg ) ) ) loc = IN_ROOM ( ch );

    if ( loc == NULL )
    {
        obj_log ( obj, "oat: location not found (%s)", arg );
        return;
    }

    if ( ! ( object = read_object ( GET_OBJ_VNUM ( obj ), VIRTUAL ) ) )
        return;

    obj_to_room ( object, loc );
    obj_command_interpreter ( object, command );

    if ( object->in_room == loc )
        extract_obj ( object, FALSE );
}

OCMD ( do_ocontains )
{
    char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH],
    arg3[MAX_INPUT_LENGTH];
    int count = 1;
    struct obj_data *object = NULL, *new_obj = NULL, *next_object = NULL;

    half_chop ( argument, arg1, argument );
    two_arguments ( argument, arg2, arg3 );

    if ( !*arg1 || !*arg2 || !is_number ( arg2 ) || atoi ( arg2 ) < 0 ||
            !*arg3 || !is_number ( arg3 ) )
    {
        obj_log ( obj, "ocontains: bad syntax" );
        return;
    }

    for ( object = obj->contains; object; object = next_object )
    {
        next_object = object->next_content;
        if ( isname ( arg1, object->name ) )
            count++;
    }

    if ( count >= atoi ( arg2 ) )
    {
        for ( object = obj->contains; object; object = next_object )
        {
            next_object = object->next_content;
            obj_from_obj ( object );
            extract_obj ( object );
        }
        new_obj = read_object ( atoi ( arg3 ), VIRTUAL );
        if ( new_obj )
            obj_to_obj ( new_obj, obj );
    }

    return;
}

OCMD ( do_ozecho )
{
    int zone;
    char zone_name[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;

    msg = any_one_arg ( argument, zone_name );
    skip_spaces ( &msg );

    if ( !*zone_name || !*msg )
        obj_log ( obj, "ozoneecho called with too few args" );
    else if ( ( zone = real_zone ( atoi ( zone_name ) ) ) < 0 )
        obj_log ( obj, "ozoneecho called for nonexistant zone" );
    else
    {
        sprintf ( buf, "%s\r\n", msg );
        send_to_zone ( buf, zone );
    }
}

/* prints the message to everyone in the range of numbers */
OCMD ( do_orecho )
{
    char start[MAX_INPUT_LENGTH], finish[MAX_INPUT_LENGTH], *msg;

    msg = two_arguments ( argument, start, finish );

    skip_spaces ( &msg );

    if ( !*msg || !*start || !*finish )
         obj_log ( obj, "mrecho called with too few args" );
    else
        send_to_range ( atoi ( start ), atoi ( finish ), "%s\r\n", msg );

}

OCMD ( do_ozrecho )
{
    int zone, lower_vnum, upper_vnum;
    char zone_name[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], *msg;
    char strlower[MAX_INPUT_LENGTH], strupper[MAX_INPUT_LENGTH];

    msg = any_one_arg ( argument, zone_name );
    msg = two_arguments ( msg, strlower, strupper );

    skip_spaces ( &msg );

    if ( !*zone_name || !*msg || !*strlower || !*strupper )
        obj_log ( obj, "ozrecho called with too few args" );
    else if ( ( zone = real_zone ( atoi ( zone_name ) ) ) < 0 )
        obj_log ( obj, "ozrecho called for nonexistant zone" );
    else
    {
        lower_vnum = atoi ( strlower );
        upper_vnum = atoi ( strupper );

        sprintf ( buf, "%s\r\n", msg );
        send_to_zone_range ( buf, zone, lower_vnum, upper_vnum );
    }
}


const struct obj_command_info obj_cmd_info[] =
{
    {"RESERVED", 0, 0},		/* this must be first -- for specprocs */
    {"oasound "    , do_oasound  , 0 },
    {"oat "        , do_oat      , 0 },
    {"odoor "      , do_odoor    , 0 },
    {"odamage "    , do_odamage,   0 },
    {"oecho ", do_oecho, 0},
    {"oechoaround ", do_osend, SCMD_OECHOAROUND},
    {"oforce ", do_oforce, 0},
    {"olag ", do_olag, 0},
    {"oload ", do_dgoload, 0},
    {"opurge ", do_opurge, 0},
    {"osend ", do_osend, SCMD_OSEND},
    {"osetval ", do_osetval, 0},
    {"oteleport ", do_oteleport, 0},
    {"otimer ", do_otimer, 0},
    {"otransform ", do_otransform, 0},
    {"ocontains", do_ocontains, 0},
    {"ozecho", do_ozecho, 0},
    {"ozrecho", do_ozrecho, 0},

    {"\n", 0, 0}		/* this must be last */
};




/*
 *  This is the command interpreter used by objects, called by script_driver.
 */
void obj_command_interpreter ( obj_data *obj, char *argument )
{
    int cmd, length;
    char *line, arg[MAX_INPUT_LENGTH];

    skip_spaces ( &argument );

    /* just drop to next line for hitting CR */
    if ( !*argument )
        return;

    line = any_one_arg ( argument, arg );


    /* find the command */
    for ( length = strlen ( arg ),cmd = 0;
            *obj_cmd_info[cmd].command != '\n'; cmd++ )
        if ( !strncmp ( obj_cmd_info[cmd].command, arg, length ) )
            break;

    if ( *obj_cmd_info[cmd].command == '\n' )
        obj_log ( obj, "Unknown object cmd: '%s'", argument );
    else
        ( ( *obj_cmd_info[cmd].command_pointer )
                ( obj, line, cmd, obj_cmd_info[cmd].subcmd ) );
}
