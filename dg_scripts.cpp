/**************************************************************************
*  File: scripts.c                                                        *
*  Usage: contains general functions for using scripts.                   *
*                                                                         *
*                                                                         *
*  $Author: w4dimenscor $
*  $Date: 2007/11/18 06:50:38 $
*  $Revision: 1.42 $
**************************************************************************/
/*
 * $Log: dg_scripts.c,v $
 * Revision 1.42  2007/11/18 06:50:38  w4dimenscor
 * Fixed the bug where you could dig up any buried object in the mud. Removed all threadding from the code to stop the freezes.
 *
 * Revision 1.41  2007/11/14 03:30:12  w4dimenscor
 * Changed the world and the index to classes. Fixed the room save bug where it would wipe descriptions
 *
 * Revision 1.40  2007/08/19 01:06:10  w4dimenscor
 * - Changed the playerindex to be a c++ object with search functions.
 * - changed the room descriptions to be searched from a MAP index, and
 * added Get and Set methods for room descriptions.
 * - changed the zone reset so that it doesnt search the entire object list
 * to find the object to PUT things into.
 * - rewrote other parts of the zone reset function, to make it give correct errors.
 * - rewrote the parts of the code to do with loading and searching for directorys and files.
 * - added a new dlib library.
 *
 * Revision 1.39  2007/07/01 14:34:03  w4dimenscor
 * fixed a whitepace issue in globals
 *
 * Revision 1.38  2007/06/14 23:55:39  w4dimenscor
 * Timers now work offline, keys can't be put in houses along with non-rent items. and the timers on items are run from the event system instead of 'ticks'
 *
 * Revision 1.37  2007/06/10 08:18:13  w4dimenscor
 * added new body parts CHEST and BACK
 *
 * Revision 1.36  2007/04/12 13:08:18  w4dimenscor
 * Forgot to remove a little pie comment.
 * --Thotter
 *
 * Revision 1.35  2007/04/12 12:50:51  w4dimenscor
 * fixed a crashbug in script log. The argument list was used twice.
 * -- Thotter
 *
 * Revision 1.34  2006/09/15 08:01:12  w4dimenscor
 * Changed a large amount of send_to_char's to ch->Send and d->Output. fixed namechange command
 *
 * Revision 1.33  2006/08/25 10:22:43  w4dimenscor
 * added command to fix peoples skills back to the practiced amount they were at
 *
 * Revision 1.32  2006/08/23 09:01:26  w4dimenscor
 * Changed some of the std::vectors to std::map, killlist, and the lookup tables for id nums
 *
 * Revision 1.31  2006/08/20 12:12:32  w4dimenscor
 * Changed the lookup table buckets to use sorted vectors. exciting. Also changed ignore list to use vectors, and fixed the valgrind error with the sort algorithm. Also sped up top gold command
 *
 * Revision 1.30  2006/08/18 11:09:58  w4dimenscor
 * updated some clan functions to use vectors instead of malloccing memory, and also sorted clan lists and updated their layout
 *
 * Revision 1.29  2006/08/17 12:28:31  w4dimenscor
 * Fixed a little bug concerning brackets
 *
 * Revision 1.28  2006/08/13 06:26:51  w4dimenscor
 * New branch created, most arrays in game converted to vectors, and the way new zones are created, many conversions of structs to classes
 *
 * Revision 1.27  2006/07/03 14:33:15  w4dimenscor
 * Added |= and %text.abbrev(blah)%. now we can test wether a word starts
 * with  something. Great for command triggers.
 *
 * Revision 1.26  2006/06/21 05:22:35  w4dimenscor
 * found an error where is find_room was passed too large a number, it would segfault. Fixed
 *
 * Revision 1.25  2006/05/30 09:14:19  w4dimenscor
 * rewrote the color code, process_output, and vwrite_to_output so that they use strings and have better buffer checks
 *
 * Revision 1.24  2006/05/22 10:50:48  w4dimenscor
 * Created 3 new files, mxp.cpp, mxp.h and descriptor.cpp
 * struct descriptor_data has been converted to class Descriptor
 *
 * Revision 1.23  2006/05/21 11:02:26  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.22  2006/05/13 01:31:29  w4dimenscor
 * Added the changes to the fread_string function thotter. Also put the hash lookup table back in which speeds things up, but i have found that in the past it adds a little instability. I added code optimisations in to speed functions up, such as isname
 *
 * Revision 1.21  2006/05/01 11:29:26  w4dimenscor
 * I wrote a typo checker that automaticly corrects typos in the comm channels. I have also been fixing shadowed variables. There may be residual issues with it.
 *
 * Revision 1.20  2006/04/06 14:00:57  w4dimenscor
 * fixed a memory bug in dg scripts
 *
 * Revision 1.19  2006/02/17 22:19:54  w4dimenscor
 * Fixed error for ubuntu that doesnt like empty array declarations, moved ice shield to a better place and fixed its messages, added auto auction fixes, allowed mounts to gain exp properly
 *
 * Revision 1.18  2005/11/30 19:08:47  w4dimenscor
 * fixes an issue in function triggers
 *
 * Revision 1.17  2005/08/19 09:34:04  w4dimenscor
 * fixed issue with FUNCTIONS not passing back vars, i mean, it does now.
 *
 * Revision 1.16  2005/08/19 09:32:57  w4dimenscor
 * fixed issue with FUNCTIONS not passing back vars, didnt compile, this one does
 *
 * Revision 1.15  2005/08/19 09:31:43  w4dimenscor
 * fixed issue with FUNCTIONS not passing back vars, maybe.
 *
 * Revision 1.14  2005/08/19 08:51:14  w4dimenscor
 * fixed the variables not working
 *
 * Revision 1.13  2005/08/14 02:27:13  w4dimenscor
 * added shiftable objects flag for the pull command, added to dg_variables ability to SET object values from variables, hopefully fixed issue where triggers would be removed from rooms after editing.
 *
 * Revision 1.12  2005/08/07 04:12:39  w4dimenscor
 * Manu changes and command have been made, sorry for the lack of description. Main changes include command landscape, fixes to helpfile stuff, subskill fixes
 *
 * Revision 1.11  2005/04/23 12:18:12  w4dimenscor
 * Fixed some buffer read errors in the fread_string function, also fixed (temp) an index search issue for real_trigger()
 *
 * Revision 1.10  2005/02/26 01:21:34  w4dimenscor
 * Changed more of the code to be more buffer safe using strlcpy and strlcat
 *
 * Revision 1.9  2005/02/20 01:18:11  w4dimenscor
 * added extra check to add to lookup table, and cleaned up freeing of characters and objects
 *
 * Revision 1.8  2005/02/16 07:36:25  w4dimenscor
 * Added math.c and updated code
 *
 * Revision 1.7  2005/02/09 09:23:44  w4dimenscor
 * added new code for using olc to create new mine shafts, and cleaned up the tsearch command, fixed a bug where there is no description in the log if the game crashes because a zone file is wanting to remove  a item from a room using zedit, but the room doesnt exist, and fixed an exp bug in flee
 *
 * Revision 1.6  2005/02/05 05:26:17  w4dimenscor
 * Added tsearch command to full text search triggers
 *
 * Revision 1.5  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.4  2004/11/26 23:02:45  w4dimenscor
 * Added more notes into fight code, and more debugging checks, made combat reuse the event
 *
 * Revision 1.3  2004/11/23 06:12:20  w4dimenscor
 * fixed some mem leaks in lightsabers, discovered function is_num is fucked, reverted it
 *
 * Revision 1.2  2004/11/20 02:33:25  w4dimenscor
 * updated and cleaned up the script system
 *
 * Revision 1.1.1.1  2004/11/12 02:15:49  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.42  2004/09/18 04:42:46  molly
 * cleared up some memory leaks again, possibly fixed the QIC miscounts
 *
 * Revision 1.41  2004/08/15 01:12:26  molly
 * aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
 *
 */
#include "config.h"
#include "sysdep.h"


#include "structs.h"
#include "dg_scripts.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "dg_event.h"
#include "db.h"
#include "screen.h"
#include "constants.h"
#include "spells.h"
#include "oasis.h"
#include "descriptor.h"
#include "name.map.h"

#define PULSES_PER_MUD_HOUR     (SECS_PER_MUD_HOUR*PASSES_PER_SEC)
#define NINE_MONTHS    6000	/* 6000 realtime minutes TO GO */

/* external vars */
extern unsigned long pulse;

/* other external vars */
extern struct time_info_data time_info;

/* external functions */
int edit_extra_desc ( obj_data *obj, const char *keyword, const char *description );
script_data* create_script();
void set_script_types ( script_data *sc );
void free_cmdlist ( trig_data *trig );
void copy_ex_descriptions ( struct extra_descr_data **to, struct extra_descr_data *from );
char *str_udup(const char *txt);
void view_room_by_rnum ( Character *ch, room_rnum is_in );
void free_varlist ( struct trig_var_data *vd );
int eval_lhs_op_rhs ( char *expr, char *result, size_t r_len,void *go,
                      struct script_data *sc, trig_data * trig, int type );
//int find_eq_pos_script(Character *ch, char *arg);
int find_skill_num ( char *name );
int find_sub_num ( char *name );
/*hm...mord*/
const char *simple_class_name ( Character *ch );
const char *race_name ( Character *ch );
int can_edit_zone ( Character *ch, zone_rnum zone );
zone_rnum real_zone_by_thing ( room_vnum vznum );
int genpreg ( void );
bool is_abbrev3 ( const string &s1, const string &s2 );
int zone_is_empty ( zone_rnum zone_nr );
room_rnum find_target_room ( Character *ch, char *rawroomstr );
/* function protos from this file */
void process_eval ( void *go, struct script_data *sc, trig_data *trig,
                    int type, char *cmd );



/* Local functions not used elsewhere */
void do_stat_trigger ( Character *ch, trig_data *trig );
void script_stat ( Character *ch, struct script_data *sc );
int remove_trigger ( void *thing, trig_data *trig, int type );
int is_num ( char *arg );
void eval_op ( char *op, char *lhs, char *rhs, char *result, size_t r_len, void *go,
               struct script_data *sc, trig_data *trig );
char *matching_paren ( char *p );
void eval_expr ( char *line, char *result, size_t r_len, void *go, struct script_data *sc,
                 trig_data *trig, int type );
int eval_lhs_op_rhs ( char *expr, char *result, size_t r_len, void *go, struct script_data *sc,
                      trig_data *trig, int type );
int process_if ( char *cond, void *go, struct script_data *sc,
                 trig_data *trig, int type );
struct cmdlist_element *find_end ( trig_data *trig, struct cmdlist_element *cl );
struct cmdlist_element *find_else_end ( trig_data *trig,
                                                    struct cmdlist_element *cl, void *go,
                                                    struct script_data *sc, int type );
void process_wait ( void *go, trig_data *trig, int type, char *cmd,
                    struct cmdlist_element *cl );
void process_set ( struct script_data *sc, trig_data *trig, char *cmd );
void process_attach ( void *go, struct script_data *sc, trig_data *trig,
                      int type, char *cmd );
void process_detach ( void *go, struct script_data *sc, trig_data *trig,
                      int type, char *cmd );
void makeuid_var ( void *go, struct script_data *sc, trig_data *trig,
                   int type, char *cmd );
int process_return ( trig_data *trig, char *cmd );
void process_unset ( struct script_data *sc, trig_data *trig, char *cmd );
void process_remote ( struct script_data *sc, trig_data *trig, char *cmd );
void process_rdelete ( struct script_data *sc, trig_data *trig, char *cmd );
void process_restring ( script_data *sc, trig_data *trig, char *cmd );
void process_global ( struct script_data *sc, trig_data *trig, char *cmd, long id );
void process_context ( struct script_data *sc, trig_data *trig, char *cmd );

void dg_letter_value ( struct script_data *sc, trig_data *trig, char *cmd );
struct cmdlist_element *
            find_case ( struct trig_data *trig, struct cmdlist_element *cl,
                        void *go, struct script_data *sc, int type, char *cond );
void extract_value ( struct script_data *sc, trig_data * trig, char *cmd );
struct cmdlist_element *find_done ( struct cmdlist_element *cl );
int fgetline ( FILE *file, char *p );
Character *find_char_by_uid_in_lookup_table ( long uid );
struct obj_data *find_obj_by_uid_in_lookup_table ( long uid );
EVENTFUNC ( trig_wait_event );
ACMD ( do_attach ) ;
ACMD ( do_detach );
ACMD ( do_vdelete );
ACMD ( do_tstat );
const char errorbuf[MAX_STRING_LENGTH] = "\0";
char *zero = ( char * ) "0";
map<trig_data*, TrigDebug> script_debug;

/* Return pointer to first occurrence of string ct in */
/* cs, or NULL if not present.  Case insensitive */
char *str_str ( char *cs, char *ct )
{
    char *s, *t;

    if ( !cs || !ct || !*ct )
        return NULL;

    while ( *cs )
    {
        t = ct;

        while ( *cs && ( LOWER ( *cs ) != LOWER ( *t ) ) )
            cs++;

        s = cs;

        while ( *t && *cs && ( LOWER ( *cs ) == LOWER ( *t ) ) )
        {
            t++;
            cs++;
        }

        if ( !*t )
            return s;
    }

    return NULL;
}
char *str_str ( char *cs, const char *ct )
{
    return str_str ( cs, ( char * ) ct );
}

int trgvar_in_room ( room_vnum vnum )
{
    room_rnum rnum = real_room ( vnum );
    int i = 0;
    Character *ch;

    if ( rnum == NULL )
    {
        script_log ( "people.vnum: world[rnum] does not exist" );
        return ( -1 );
    }

    for ( ch = rnum->people; ch != NULL; ch = ch->next_in_room )
        i++;

    return i;
}

obj_data *get_obj_in_list ( char *name, obj_data * list )
{
    obj_data *i;
    long id;

    if ( *name == UID_CHAR )
    {
        id = atoi ( name + 1 );

        for ( i = list; i; i = i->next_content )
            if ( id == GET_ID ( i ) )
                return i;
    }
    else
    {
        for ( i = list; i; i = i->next_content )
            if ( isname ( name, i->name ) )
                return i;
    }

    return NULL;
}
obj_data *get_obj_in_list ( const char *name, obj_data * list )
{
    obj_data *i;
    long id;

    if ( *name == UID_CHAR )
    {
        id = atoi ( name + 1 );

        for ( i = list; i; i = i->next_content )
            if ( id == GET_ID ( i ) )
                return i;
    }
    else
    {
        for ( i = list; i; i = i->next_content )
            if ( isname ( name, i->name ) )
                return i;
    }

    return NULL;
}

/* Handles 'held', 'light' and 'wield' positions - Welcor
   After idea from Byron Ellacott - bje@apnic.net */
int find_eq_pos_script ( char *arg )
{
    int i;
    if ( is_number ( arg ) && ( i = atoi ( arg ) ) >= 0 && i < NUM_WEARS )
        return i;

    for ( i = 0; eq_pos[i].where != -1; i++ )
    {
        if ( !strcasecmp ( eq_pos[i].pos, arg ) )
            return eq_pos[i].where;
    }
    return ( -1 );
}

int can_wear_on_pos ( struct obj_data *obj, int pos )
{
    switch ( pos )
    {
        case WEAR_HOLD:
        case WEAR_LIGHT:
            return CAN_WEAR ( obj, ITEM_WEAR_HOLD );
        case WEAR_WIELD_2:
        case WEAR_WIELD:
            return CAN_WEAR ( obj, ITEM_WEAR_WIELD );
        case WEAR_THUMB_R:
        case WEAR_THUMB_L:
        case WEAR_FINGER_R:
        case WEAR_FINGER_L:
            return CAN_WEAR ( obj, ITEM_WEAR_FINGER );
        case WEAR_NECK_1:
        case WEAR_NECK_2:
            return CAN_WEAR ( obj, ITEM_WEAR_NECK );
        case WEAR_BODY:
            return CAN_WEAR ( obj, ITEM_WEAR_BODY );
        case WEAR_HEAD:
            return CAN_WEAR ( obj, ITEM_WEAR_HEAD );
        case WEAR_LEGS_2:
        case WEAR_LEGS:
            return CAN_WEAR ( obj, ITEM_WEAR_LEGS );
        case WEAR_FEET_2:
        case WEAR_FEET:
            return CAN_WEAR ( obj, ITEM_WEAR_FEET );
        case WEAR_HANDS:
            return CAN_WEAR ( obj, ITEM_WEAR_HANDS );
        case WEAR_ARMS:
            return CAN_WEAR ( obj, ITEM_WEAR_ARMS );
        case WEAR_SHIELD:
            return CAN_WEAR ( obj, ITEM_WEAR_SHIELD );
        case WEAR_SADDLE:
        case WEAR_ABOUT:
            return CAN_WEAR ( obj, ITEM_WEAR_ABOUT );
        case WEAR_WAIST:
            return CAN_WEAR ( obj, ITEM_WEAR_WAIST );
        case WEAR_WRIST_R:
        case WEAR_WRIST_L:
            return CAN_WEAR ( obj, ITEM_WEAR_WRIST );
        case WEAR_FACE:
            return CAN_WEAR ( obj, ITEM_WEAR_FACE );
        case WEAR_EYES:
            return CAN_WEAR ( obj, ITEM_WEAR_EYES );
        case WEAR_HIPS:
            return CAN_WEAR ( obj, ITEM_WEAR_HIPS );
        case WEAR_EAR_TIP:
        case WEAR_EAR_R:
        case WEAR_EAR_L:
            return CAN_WEAR ( obj, ITEM_WEAR_EAR );
        case WEAR_ANKLE_R:
        case WEAR_ANKLE_L:
            return CAN_WEAR ( obj, ITEM_WEAR_ANKLE );
        case WEAR_HORNS:
            return CAN_WEAR ( obj, ITEM_WEAR_HORNS );
        case WEAR_ANTENNA:
            return CAN_WEAR ( obj, ITEM_WEAR_ANTENNA );
        case WEAR_TAIL:
            return CAN_WEAR ( obj, ITEM_WEAR_TAIL );
        case WEAR_FOCUS:
            return CAN_WEAR ( obj, ITEM_WEAR_FOCUS );
        case WEAR_SHOULDER_L:
        case WEAR_SHOULDER_R:
            return CAN_WEAR ( obj, ITEM_WEAR_SHOULDER );
        case WEAR_CREST:
            return CAN_WEAR ( obj, ITEM_WEAR_CREST );
        case WEAR_THIGH_L:
        case WEAR_THIGH_R:
            return CAN_WEAR ( obj, ITEM_WEAR_THIGH );
        case WEAR_KNEE_L :
        case WEAR_KNEE_R:
            return CAN_WEAR ( obj, ITEM_WEAR_KNEE );
        case WEAR_FLOATING:
            return CAN_WEAR ( obj, ITEM_WEAR_FLOATING );
        case WEAR_BACK:
            return CAN_WEAR ( obj, ITEM_WEAR_BACK );
        case WEAR_CHEST:
            return CAN_WEAR ( obj, ITEM_WEAR_CHEST );

        default:
            return FALSE;
    }
}
/*
#define WEAR_THUMB_R    32
#define WEAR_THUMB_L    33
#define WEAR_SADDLE     34
#define WEAR_EAR_TIP    35
#define WEAR_SHOULDER_L 36
#define WEAR_SHOULDER_R 37
#define WEAR_CREST      38
#define WEAR_THIGH_L    39
#define WEAR_THIGH_R    40
#define WEAR_KNEE_L     41
#define WEAR_KNEE_R     42
#define WEAR_FLOATING   43
*/
obj_data *get_object_in_equip ( Character * ch,const char *name )
{
    int j;
    obj_data *obj;
//	char tmpname[MAX_INPUT_LENGTH];
//	char *tmp = tmpname;
    long id;
    /*int n = 0, num;*/

    if ( *name == UID_CHAR )
    {
        id = atoi ( name + 1 );

        for ( j = 0; j < NUM_WEARS; j++ )
            if ( ( obj = GET_EQ ( ch, j ) ) )
                if ( id == GET_ID ( obj ) )
                    return ( obj );
    }
    else if ( is_number ( name ) )
    {
        obj_vnum ovnum = atoi ( name );
        for ( j = 0; j < NUM_WEARS; j++ )
            if ( ( obj = GET_EQ ( ch, j ) ) )
                if ( GET_OBJ_VNUM ( obj ) == ovnum )
                    return ( obj );
    }
    else
    {
        /*snprintf ( tmpname, sizeof ( tmpname ), "%s", name );
        if ( ! ( num = get_number ( &tmpname ) ) )
            return NULL;*/

        for ( j = 0; ( j < NUM_WEARS ) /* && ( n <= num )*/; j++ )
            if ( ( obj = GET_EQ ( ch, j ) ) )
                if ( isname ( /*tmp*/ name, obj->name ) )
                    /*if ( ++n == num )*/
                    return ( obj );
    }

    return NULL;
}

/************************************************************
 * search by number routines
 ************************************************************/

/* return char with UID n */
/*TODO: this function uses alot of time, fix it */
Character *find_char ( long n )
{


    if ( n>=ROOM_ID_BASE ) /* See note in dg_scripts.h */
        return NULL;

    return find_char_by_uid_in_lookup_table ( n );
}


/* return object with UID n */
struct obj_data *find_obj ( long n )
{
    if ( n < OBJ_ID_BASE ) /* see note in dg_scripts.h */
        return NULL;

    return find_obj_by_uid_in_lookup_table ( n );
}

/* return room with UID n */
Room *find_room ( long n )
{
    n -= ROOM_ID_BASE;
    return real_room ( n );
}



/************************************************************
 * generic searches based only on name
 ************************************************************/

/* search the entire world for a char, and return a pointer */
Character *get_char ( const char *name )
{
    Character *i = NULL;

    if ( *name == UID_CHAR )
    {
        i = find_char ( atoi ( name + 1 ) );

        if ( i && valid_dg_target ( i, TRUE ) )
            return i;
    }
    else
    {
#if 1
        long j = mobNames.nameLookup ( name );
        if ( j != -1 )
            i = find_char ( j );
        if ( i && valid_dg_target ( i, TRUE ) )
            return i;
        for ( Descriptor *d = descriptor_list; d; d = d->next )
        {
            i = d->character;
            if ( IS_PLAYING ( d ) && i && IN_ROOM ( i ) && isname_hard ( name, i->player.name ) && valid_dg_target ( i, TRUE ) )
                return i;
        }
#else
        for ( i = character_list; i; i = i->next )
            if ( isname_hard ( name, i->player.name ) && valid_dg_target ( i, TRUE ) )
                return i;
#endif
    }

    return NULL;
}


/*
 * Finds a char in the same room as the object with the name 'name'
 */
Character *get_char_near_obj ( obj_data * obj, char *name )
{
    Character *ch;

    if ( *name == UID_CHAR )
    {
        ch = find_char ( atoi ( name + 1 ) );

        if ( ch && valid_dg_target ( ch, TRUE ) )
            return ch;
    }
    else
    {
        room_rnum num;
        if ( ( num = obj_room ( obj ) ) != NULL )
            for ( ch = num->people; ch; ch = ch->next_in_room )
                if ( isname ( name, ch->player.name ) &&
                        valid_dg_target ( ch, TRUE ) )
                    return ch;
    }

    return NULL;
}


/*
 * returns a pointer to the first character in world by name name,
 * or NULL if none found.  Starts searching in room room first
 */
Character *get_char_in_room ( Room * room, char *name )
{
    Character *ch;

    if ( *name == UID_CHAR )
    {
        ch = find_char ( atoi ( name + 1 ) );

        if ( ch && valid_dg_target ( ch, TRUE ) )
            return ch;
    }
    else
    {
        for ( ch = room->people; ch; ch = ch->next_in_room )
            if ( isname ( name, ch->player.name ) && valid_dg_target ( ch, TRUE ) )
                return ch;
    }

    return NULL;
}

/* searches the room with the object for an object with name 'name'*/

obj_data *get_obj_near_obj ( obj_data * obj, char *name )
{
    struct obj_data *i = NULL;
    Character *ch;
    room_rnum rm;
    long id;

    if ( !strcasecmp ( name, "self" ) || !strcasecmp ( name, "me" ) )
        return obj;

    /* is it inside ? */
    if ( obj->contains && ( i = get_obj_in_list ( name, obj->contains ) ) )
        return i;

    /* or outside ? */
    if ( obj->in_obj )
    {
        if ( *name == UID_CHAR )
        {
            id = atoi ( name + 1 );

            if ( id == GET_ID ( obj->in_obj ) )
                return obj->in_obj;
        }
        else if ( isname ( name, obj->in_obj->name ) )
            return obj->in_obj;
    }
    /* or worn ? */
    else if ( obj->worn_by && ( i = get_object_in_equip ( obj->worn_by, name ) ) )
        return i;
    /* or carried ? */
    else if ( obj->carried_by &&
              ( i = get_obj_in_list ( name, obj->carried_by->carrying ) ) )
        return i;
    else if ( ( rm = obj_room ( obj ) ) != NULL )
    {
        /* check the floor */
        if ( ( i = get_obj_in_list ( name, rm->contents ) ) )
            return i;

        /* check peoples' inventory */
        for ( ch = rm->people; ch; ch = ch->next_in_room )
            if ( ( i = get_object_in_equip ( ch, name ) ) )
                return i;
    }
    return NULL;
}



/* returns the object in the world with name name, or NULL if not found */
struct obj_data *get_obj ( const char *name )
{
    if ( !name || !*name )
        return NULL;
    else if ( *name == UID_CHAR )
        return find_obj ( atoi ( name + 1 ) );
    else
    {
#if 1
        long i = objNames.nameLookup ( name );
        if ( i != -1 )
            return find_obj ( i );
#else
        for ( olt_it ob = object_list.begin(); ob != object_list.end(); ob++ )
            if ( ob->second && isname_hard ( ( ob->second )->name, name ) )
                return ob->second;
#endif
    }

    return NULL;
}
/**
switch (obj->Names.size()) {
    case 0:
                break;
    case 1:
                if (is_abbrev(name, obj->Names[0].c_str()))
                    return obj;
                break;
    default:
                for (vector<string>::iterator i = obj->Names.begin();
                        i != obj->Names.end();
                        i++)
                        if (is_abbrev((*i).c_str(), name)
                           )
                            return obj;
                //if (isname(name, obj->name))
                // return obj;
}**/

/* finds room by id or vnum.  returns NULL if not found */
room_rnum get_room ( const char *name )
{
    if ( *name == UID_CHAR )
        return find_room ( atoi ( name + 1 ) );
    else if ( !is_number ( name ) )
        return NULL;
    else
        return real_room ( atoi ( name ) );
}


/*
 * returns a pointer to the first character in world by name name,
 * or NULL if none found.  Starts searching with the person owning the object
 */
Character *get_char_by_obj ( obj_data * obj,const char *name )
{
    Character *ch;
    room_rnum r = NULL;

    if ( *name == UID_CHAR )
    {
        ch = find_char ( atoi ( name + 1 ) );

        if ( ch && valid_dg_target ( ch, TRUE ) )
            return ch;
    }
    else
    {
        if ( obj->carried_by &&
                isname ( name, obj->carried_by->player.name ) &&
                valid_dg_target ( obj->carried_by, TRUE ) )
            return obj->carried_by;

        if ( obj->worn_by &&
                isname ( name, obj->worn_by->player.name ) &&
                valid_dg_target ( obj->worn_by, TRUE ) )
            return obj->worn_by;

        if ( ( r = obj_room ( obj ) ) != NULL )
            for ( ch = r->people; ch; ch = ch->next_in_room )
                if ( isname ( name, ch->player.name ) && valid_dg_target ( ch, TRUE ) )
                    return ch;

        for ( ch = character_list; ch; ch = ch->next )
            if ( isname ( name, ch->player.name ) && valid_dg_target ( ch, TRUE ) )
                return ch;
    }

    return NULL;
}

Character *get_char_by_obj ( obj_data * obj,char *name )
{
    Character *ch;
    room_rnum r = NULL;

    if ( *name == UID_CHAR )
    {
        ch = find_char ( atoi ( name + 1 ) );

        if ( ch && valid_dg_target ( ch, TRUE ) )
            return ch;
    }
    else
    {
        if ( obj->carried_by &&
                isname ( name, obj->carried_by->player.name ) &&
                valid_dg_target ( obj->carried_by, TRUE ) )
            return obj->carried_by;

        if ( obj->worn_by &&
                isname ( name, obj->worn_by->player.name ) &&
                valid_dg_target ( obj->worn_by, TRUE ) )
            return obj->worn_by;

        if ( ( r = obj_room ( obj ) ) != NULL )
            for ( ch = r->people; ch; ch = ch->next_in_room )
                if ( isname ( name, ch->player.name ) && valid_dg_target ( ch, TRUE ) )
                    return ch;

        for ( ch = character_list; ch; ch = ch->next )
            if ( isname ( name, ch->player.name ) && valid_dg_target ( ch, TRUE ) )
                return ch;
    }

    return NULL;
}


/*
 * returns a pointer to the first character in world by name name,
 * or NULL if none found.  Starts searching in room room first
 */
Character *get_char_by_room ( Room * room,const char *name )
{
    Character *ch;

    if ( *name == UID_CHAR )
    {
        ch = find_char ( atoi ( name + 1 ) );

        if ( ch && valid_dg_target ( ch, TRUE ) )
            return ch;
    }
    else
    {
        for ( ch = room->people; ch; ch = ch->next_in_room )
            if ( isname ( name, ch->player.name ) && valid_dg_target ( ch, TRUE ) )
                return ch;

        for ( ch = character_list; ch; ch = ch->next )
            if ( isname ( name, ch->player.name ) && valid_dg_target ( ch, TRUE ) )
                return ch;
    }

    return NULL;
}


/*
 * returns the object in the world with name name, or NULL if not found
 * search based on obj
 */
obj_data *get_obj_by_obj ( obj_data * obj,const char *name )
{
    obj_data *i = NULL;
    room_rnum rm;


    if ( *name == UID_CHAR )
        return find_obj ( atoi ( name + 1 ) );

    if ( !strcasecmp ( name, "self" ) || !strcasecmp ( name, "me" ) )
        return obj;

    if ( obj->contains && ( i = get_obj_in_list ( name, obj->contains ) ) )
        return i;

    if ( obj->in_obj && isname ( name, obj->in_obj->name ) )
        return obj->in_obj;

    if ( obj->worn_by && ( i = get_object_in_equip ( obj->worn_by, name ) ) )
        return i;

    if ( obj->carried_by &&
            ( i = get_obj_in_list ( name, obj->carried_by->carrying ) ) )
        return i;

    if ( ( ( rm = obj_room ( obj ) ) != NULL ) &&
            ( i = get_obj_in_list ( name, rm->contents ) ) )
        return i;

    return NULL;
}


/* only searches the room */
obj_data *get_obj_in_room ( Room * room, char *name )
{
    struct obj_data *obj;

    if ( room )
    {
        for ( obj = room->contents; obj; obj = obj->next_content )
            if ( isname ( name, obj->name ) )
                return obj;
    }

    return NULL;
}

/* returns obj with name - searches room, then world */
obj_data *get_obj_by_room ( Room *room, const char *name )
{
    obj_data *obj = NULL;
    if ( *name == UID_CHAR )
        return find_obj ( atoi ( name + 1 ) );
    if ( room )
    {
        for ( obj = room->contents; obj; obj = obj->next_content )
            if ( isname ( name, obj->name ) )
                return obj;
    }

    return get_obj ( name );
}

void check_time_triggers ( void )
{
    Character *ch;
    obj_data *obj;
    Room *room=NULL;
    int nr;
    struct script_data *sc;

    for ( ch = character_list; ch; ch = ch->next )
    {
        if ( SCRIPT ( ch ) )
        {
            sc = SCRIPT ( ch );

            if ( IS_SET ( SCRIPT_TYPES ( sc ), MTRIG_TIME ) )
                time_mtrigger ( ch );
        }
    }

    for ( olt_it i = object_list.begin(); i != object_list.end(); i++ )
    {
        obj = ( i->second );
        if ( !obj->extracted && SCRIPT ( obj ) )
        {
            sc = SCRIPT ( obj );

            if ( IS_SET ( SCRIPT_TYPES ( sc ), OTRIG_TIME ) )
                time_otrigger ( obj );
        }
    }

    for ( nr = 0; nr <= top_of_world; nr++ )
    {
        if ( world_vnum[nr] && SCRIPT ( world_vnum[nr] ) )
        {
            room = world_vnum[nr];
            sc = SCRIPT ( room );

            if ( IS_SET ( SCRIPT_TYPES ( sc ), WTRIG_TIME ) )
                time_wtrigger ( room );
        }
    }
}

/* checks every PULSE_DG_SCRIPT for random triggers */
void script_trigger_check ( void )
{
    Character *ch;
    obj_data *obj;
    Room *room=NULL;
    int nr;
    struct script_data *sc;

    for ( ch = character_list; ch; ch = ch->next )
    {
        if ( SCRIPT ( ch ) )
        {
            sc = SCRIPT ( ch );

            if ( IS_SET ( SCRIPT_TYPES ( sc ), MTRIG_RANDOM ) )
                random_mtrigger ( ch );
        }
    }

    for ( olt_it i = object_list.begin(); i != object_list.end(); i++ )
    {
        obj = ( i->second );
        if ( !obj->extracted && SCRIPT ( obj ) )
        {
            sc = SCRIPT ( obj );
            if ( IS_SET ( SCRIPT_TYPES ( sc ), OTRIG_RANDOM ) )
                random_otrigger ( obj );
        }
    }

    for ( nr = 0; nr <= top_of_world; nr++ )
    {
        if ( !world_vnum[nr] )
            continue;
        if ( SCRIPT ( world_vnum[nr] ) )
        {
            room = world_vnum[nr];
            sc = SCRIPT ( room );

            if ( IS_SET ( SCRIPT_TYPES ( sc ), WTRIG_RANDOM ) )
                random_wtrigger ( room );
        }
    }
}

EVENTFUNC ( trig_wait_event )
{
    struct wait_event_data *wait_event_obj = ( struct wait_event_data * ) event_obj;
    trig_data *trig;
    void *go;
    int type;

    trig = wait_event_obj->trigger;
    go = wait_event_obj->go;
    type = wait_event_obj->type;

    delete wait_event_obj;
    GET_TRIG_WAIT ( trig ) = NULL;

#if 0  /* debugging */

    {
        int found = FALSE;
        if ( type == MOB_TRIGGER )
        {
            Character *tch;
            for ( tch = character_list;tch && !found;tch = tch->next )
                if ( tch == ( Character * ) go )
                    found = TRUE;
        }
        else if ( type == OBJ_TRIGGER )
        {
            struct obj_data *obj;
            for ( obj = object_list;obj && !found;obj = obj->next )
                if ( obj == ( struct obj_data * ) go )
                    found = TRUE;
        }
        else
        {
            room_vnum i;
            for ( i = 0;i<top_of_world && !found;i++ )
                if ( world_vnum[i] == ( Room * ) go )
                    found = TRUE;
        }
        if ( !found )
        {
            if ( 0 )
            {
                log ( "Trigger restarted on unknown entity. Vnum: %d", GET_TRIG_VNUM ( trig ) );
                log ( "Type: %s trigger", type==MOB_TRIGGER ? "Mob" : type == OBJ_TRIGGER ? "Obj" : "Room" );
                log ( "attached %d places", trig_index[trig->nr]->number );
            }
            script_log ( "Trigger restart attempt on unknown entity. " );
            script_log ( "Possibly caused by the mob or obj being purged that had the script on it" );
            return 0;
        }
    }
#endif

    if ( go )
        script_driver ( &go, trig, type, TRIG_RESTART );

    /* Do not reenqueue */
    return 0;
}



void do_stat_trigger ( Character *ch, trig_data *trig )
{
    struct cmdlist_element *cmd_list;
    char sb[MAX_STRING_LENGTH], buf[MAX_STRING_LENGTH];
    int len = 0;

    if ( !trig )
    {
        log ( "SYSERR: NULL trigger passed to do_stat_trigger." );
        return;
    }

    len += snprintf ( sb, sizeof ( sb ), "Name: '%s%s%s',  VNum: [%s%5d%s], RNum: [%5d]\r\n",
                      CCYEL ( ch, C_NRM ), GET_TRIG_NAME ( trig ), CCNRM ( ch, C_NRM ),
                      CCGRN ( ch, C_NRM ), GET_TRIG_VNUM ( trig ), CCNRM ( ch, C_NRM ),
                      GET_TRIG_RNUM ( trig ) );

    if ( trig->attach_type==OBJ_TRIGGER )
    {
        len += snprintf ( sb + len, sizeof ( sb )-len, "Trigger Intended Assignment: Objects\r\n" );
        new_sprintbit ( GET_TRIG_TYPE ( trig ), otrig_types, buf, sizeof ( buf ) );
    }
    else if ( trig->attach_type==WLD_TRIGGER )
    {
        len += snprintf ( sb + len, sizeof ( sb )-len, "Trigger Intended Assignment: Rooms\r\n" );
        new_sprintbit ( GET_TRIG_TYPE ( trig ), wtrig_types, buf, sizeof ( buf ) );
    }
    else
    {
        len += snprintf ( sb + len, sizeof ( sb )-len, "Trigger Intended Assignment: Mobiles\r\n" );
        new_sprintbit ( GET_TRIG_TYPE ( trig ), mtrig_types, buf, sizeof ( buf ) );
    }

    len += snprintf ( sb + len, sizeof ( sb )-len, "Trigger Type: %s, Numeric Arg: %d, Arg list: %s\r\n",
                      buf, GET_TRIG_NARG ( trig ),
                      ( ( GET_TRIG_ARG ( trig ) && *GET_TRIG_ARG ( trig ) )
                        ? GET_TRIG_ARG ( trig ) : "None" ) );

    len += snprintf ( sb + len, sizeof ( sb )-len, "Commands:\r\n" );

    cmd_list = trig->cmdlist;
    while ( cmd_list )
    {
        if ( cmd_list->cmd )
            len += snprintf ( sb + len, sizeof ( sb )-len, "%s\r\n", cmd_list->cmd );

        if ( len>MAX_STRING_LENGTH-80 )
        {
            snprintf ( sb + MAX_STRING_LENGTH - 50, 50, "*** Overflow - script too long! ***\r\n" );
            break;
        }
        cmd_list = cmd_list->next;
    }

    page_string ( ch->desc, sb, 1 );
}

/* find the name of what the uid points to */
void find_uid_name ( char *uid, char *name, size_t nlen )
{
    Character *ch;
    obj_data *obj;
    Room *room;

    if ( ( ch = get_char ( uid ) ) )
        snprintf ( name, nlen, "%s (uid of %s)", uid, GET_NAME ( ch ) );
    else if ( ( obj = get_obj ( uid ) ) )
        snprintf ( name, nlen, "%s (uid of %s)", uid, obj->short_description );
    else if ( ( room = get_room ( uid ) ) )
        snprintf ( name, nlen, "%s (uid of %s)", uid, room->name );
    else
        snprintf ( name, nlen, "%s", uid );
}
/* find the name of what the uid points to */
void find_uid_name ( string &uid, char *name, size_t nlen )
{
    Character *ch;
    obj_data *obj;
    Room *room;

    if ( ( ch = get_char ( uid.c_str() ) ) )
        snprintf ( name, nlen, "%s (uid of %s)", uid.c_str(), GET_NAME ( ch ) );
    else if ( ( obj = get_obj ( uid.c_str() ) ) )
        snprintf ( name, nlen, "%s (uid of %s)", uid.c_str(), obj->short_description );
    else if ( ( room = get_room ( uid.c_str() ) ) )
        snprintf ( name, nlen, "%s (uid of %s)", uid.c_str(), room->name );
    else
        snprintf ( name, nlen, "%s", uid.c_str() );
}



/* general function to display stats on script sc */
void script_stat ( Character *ch, struct script_data *sc )
{
    struct trig_var_data *tv;
    trig_data *t;
    char name[MAX_INPUT_LENGTH];
    char namebuf[512];
    char buf1[MAX_STRING_LENGTH];

    ch->Send ( "Global Variables: %s\r\n", sc->global_vars ? "" : "None" );
    ch->Send ( "Global context: %ld\r\n", sc->context );

    for ( tv = sc->global_vars; tv; tv = tv->next )
    {
        snprintf ( namebuf, sizeof ( namebuf ), "%s:%ld", tv->name.c_str(), tv->context );
        if ( tv->value[0] == UID_CHAR )
        {
            find_uid_name ( tv->value, name, sizeof ( name ) );
            ch->Send ( "    %15s:  %s\r\n", tv->context?namebuf:tv->name.c_str(), name );
        }
        else
            ch->Send ( "    %15s:  %s\r\n", tv->context?namebuf:tv->name.c_str(), tv->value.c_str() );
    }

    for ( t = TRIGGERS ( sc ); t; t = t->next )
    {
        ch->Send ( "\r\n  Trigger: %s%s%s, VNum: [%s%5d%s], RNum: [%5d]\r\n",
                   CCYEL ( ch, C_NRM ), GET_TRIG_NAME ( t ), CCNRM ( ch, C_NRM ),
                   CCGRN ( ch, C_NRM ), GET_TRIG_VNUM ( t ), CCNRM ( ch, C_NRM ),
                   GET_TRIG_RNUM ( t ) );

        if ( t->attach_type==OBJ_TRIGGER )
        {
            ch->Send ( "  Trigger Intended Assignment: Objects\r\n" );
            new_sprintbit ( GET_TRIG_TYPE ( t ), otrig_types, buf1, sizeof ( buf1 ) );
        }
        else if ( t->attach_type==WLD_TRIGGER )
        {
            ch->Send ( "  Trigger Intended Assignment: Rooms\r\n" );
            new_sprintbit ( GET_TRIG_TYPE ( t ), wtrig_types, buf1, sizeof ( buf1 ) );
        }
        else
        {
            ch->Send ( "  Trigger Intended Assignment: Mobiles\r\n" );
            new_sprintbit ( GET_TRIG_TYPE ( t ), mtrig_types, buf1, sizeof ( buf1 ) );
        }

        ch->Send ( "  Trigger Type: %s, Numeric Arg: %d, Arg list: %s\r\n",
                   buf1, GET_TRIG_NARG ( t ),
                   ( ( GET_TRIG_ARG ( t ) && *GET_TRIG_ARG ( t ) ) ? GET_TRIG_ARG ( t ) :
                     "None" ) );

        if ( GET_TRIG_WAIT ( t ) )
        {
            ch->Send ( "    Wait: %ld, Current line: %s\r\n",
                       event_time ( GET_TRIG_WAIT ( t ) ),
                       t->curr_state ? t->curr_state->cmd : "End of Script" );
            ch->Send ( "  Variables: %s\r\n", GET_TRIG_VARS ( t ) ? "" : "None" );

            for ( tv = GET_TRIG_VARS ( t ); tv; tv = tv->next )
            {
                if ( tv->value[0] == UID_CHAR )
                {
                    find_uid_name ( tv->value, name, sizeof ( name ) );
                    ch->Send ( "    %15s:  %s\r\n", tv->name.c_str(), name );
                }
                else
                    ch->Send ( "    %15s:  %s\r\n", tv->name.c_str(), tv->value.c_str() );
            }
        }
    }
}


void do_sstat_room ( Character *ch )
{
    Room *rm = IN_ROOM ( ch );

    ch->Send ( "Script information:\r\n" );
    if ( !SCRIPT ( rm ) )
    {
        ch->Send ( "  None.\r\n" );
        return;
    }

    script_stat ( ch, SCRIPT ( rm ) );
}


void do_sstat_object ( Character * ch, obj_data * j )
{
    ch->Send ( "Script information:\r\n" );
    if ( !SCRIPT ( j ) )
    {
        ch->Send ( "  None.\r\n" );
        return;
    }

    script_stat ( ch, SCRIPT ( j ) );
}


void do_sstat_character ( Character *ch, Character *k )
{
    ch->Send ( "Script information:\r\n" );
    if ( !SCRIPT ( k ) )
    {
        ch->Send ( "  None.\r\n" );
        return;
    }

    script_stat ( ch, SCRIPT ( k ) );
}


/*
 * adds the trigger t to script sc in in location loc.  loc = -1 means
 * add to the end, loc = 0 means add before all other triggers.
 */
void add_trigger ( struct script_data *sc, trig_data *t, int loc )
{
    trig_data *i;
    int n;
    if ( !sc )
    {
        log ( "add_trigger passed null script" );

        return;
    }
    for ( n = loc, i = TRIGGERS ( sc ); i && i->next && ( n != 0 ); n--, i = i->next )
        ;

    if ( !loc )
    {
        t->next = TRIGGERS ( sc );
        TRIGGERS ( sc ) = t;
    }
    else if ( !i )
        TRIGGERS ( sc ) = t;
    else
    {
        t->next = i->next;
        i->next = t;
    }

    set_script_types ( sc );

    t->next_in_world = trigger_list;
    trigger_list = t;
}

trig_data *find_trigger ( script_data *sc, trig_vnum vnum )
{
    if ( sc == nullptr )
        return nullptr;

    for ( auto trig = TRIGGERS ( sc ); trig; trig = trig->next )
        if ( GET_TRIG_VNUM ( trig ) == vnum )
            return trig;

    return nullptr;
}

ACMD ( do_attach )
{
    Character *victim;
    obj_data *object;
    Room *room;
    trig_data *trig_to_add;
    char targ_name[MAX_INPUT_LENGTH], trig_name[MAX_INPUT_LENGTH];
    char loc_name[MAX_INPUT_LENGTH], arg[MAX_INPUT_LENGTH];
    int loc, tn, rn, num_arg;
    room_rnum rnum;
    trig_data *has_trig = nullptr;

    argument = two_arguments ( argument, arg, trig_name );
    two_arguments ( argument, targ_name, loc_name );

    if ( !*arg || !*targ_name || !*trig_name )
    {
        ch->Send ( "Usage: attach {{ mob | obj | room } {{ trigger } {{ name } [ location ]\r\n" );
        return;
    }

    num_arg = atoi ( targ_name );
    tn = atoi ( trig_name );
    loc = ( *loc_name ) ? atoi ( loc_name ) : -1;

    if ( is_abbrev ( arg, "mobile" ) || is_abbrev ( arg, "mtr" ) )
    {
        victim = get_char_vis ( ch, targ_name, NULL, FIND_CHAR_WORLD );
        if ( !victim ) /* search room for one with this vnum */
        {
            for ( victim = IN_ROOM ( ch )->people; victim; victim = victim->next_in_room )
                if ( GET_MOB_VNUM ( victim ) == num_arg )
                    break;

            if ( !victim )
            {
                ch->Send ( "That mob does not exist.\r\n" );
                return;
            }
        }
        if ( !IS_NPC ( victim ) )
        {
            ch->Send ( "Players can't have scripts.\r\n" );
            return;
        }

        if ( !can_edit_zone ( ch, IN_ROOM ( ch )->zone ) )
        {
            ch->Send ( "You can only attach triggers in your own zone\r\n" );
            return;
        }

        /* have a valid mob, now get trigger */
        rn = real_trigger ( tn );
        if ( ( rn == NOTHING ) || ! ( trig_to_add = read_trigger ( rn ) ) )
        {
            ch->Send ( "That trigger does not exist.\r\n" );
            return;
        }

        if ( !SCRIPT ( victim ) )
            SCRIPT ( victim ) = create_script();

        has_trig = find_trigger ( SCRIPT ( victim ), tn );
        if ( !has_trig )
        {
            add_trigger ( SCRIPT ( victim ), trig_to_add, loc );

            ch->Send ( "Trigger %d (%s) attached to %s [%d].\r\n",
                tn, GET_TRIG_NAME ( trig_to_add ), GET_SHORT ( victim ), GET_MOB_VNUM ( victim ) );
        }
    }
    else if ( is_abbrev ( arg, "object" ) || is_abbrev ( arg, "otr" ) )
    {
        object = get_obj_vis ( ch, targ_name, NULL );
        if ( !object ) /* search room for one with this vnum */
        {
            for ( object = IN_ROOM ( ch )->contents; object; object = object->next_content )
                if ( GET_OBJ_VNUM ( object ) == num_arg )
                    break;

            if ( !object ) /* search inventory for one with this vnum */
            {
                for ( object = ch->carrying; object; object = object->next_content )
                    if ( GET_OBJ_VNUM ( object ) == num_arg )
                        break;

                if ( !object )
                {
                    ch->Send ( "That object does not exist.\r\n" );
                    return;
                }
            }
        }

#ifndef STOCK_CIRCLE
        if ( !can_edit_zone ( ch, real_zone_by_thing ( num_arg ) ) )
        {
            ch->Send ( "You can only attach triggers in your own zone\r\n" );
            return;
        }
#endif
        /* have a valid obj, now get trigger */
        rn = real_trigger ( tn );
        if ( ( rn == NOTHING ) || ! ( trig_to_add = read_trigger ( rn ) ) )
        {
            ch->Send ( "That trigger does not exist.\r\n" );
            return;
        }

        if ( !SCRIPT ( object ) )
            SCRIPT ( object ) = create_script();

        has_trig = find_trigger ( SCRIPT ( object ), tn );
        if ( !has_trig )
        {
            add_trigger ( SCRIPT ( object ), trig_to_add, loc );

            ch->Send ( "Trigger %d (%s) attached to %s [%d].\r\n",
                tn, GET_TRIG_NAME ( trig_to_add ),
                object->short_description ? object->short_description : object->name,
                GET_OBJ_VNUM ( object ) );
        }
    }
    else if ( is_abbrev ( arg, "room" ) || is_abbrev ( arg, "wtr" ) )
    {
        if ( strchr ( targ_name, '.' ) )
            rnum = IN_ROOM ( ch );
        else if ( isdigit ( *targ_name ) )
            rnum = find_target_room ( ch, targ_name );
        else
            rnum = NULL;

        if ( rnum == NULL )
        {
            ch->Send ( "You need to supply a room number or . for current room.\r\n" );
            return;
        }

#ifndef STOCK_CIRCLE
        if ( !can_edit_zone ( ch, rnum->zone ) )
        {
            ch->Send ( "You can only attach triggers in your own zone\r\n" );
            return;
        }
#endif
        /* have a valid room, now get trigger */
        rn = real_trigger ( tn );
        if ( ( rn == NOTHING ) || ! ( trig_to_add = read_trigger ( rn ) ) )
        {
            ch->Send ( "That trigger does not exist.\r\n" );
            return;
        }

        room = rnum;

        if ( !SCRIPT ( room ) )
            SCRIPT ( room ) = create_script();

        has_trig = find_trigger ( SCRIPT ( room ), tn );
        if ( !has_trig )
        {
            add_trigger ( SCRIPT ( room ), trig_to_add, loc );

            ch->Send ( "Trigger %d (%s) attached to room %d.\r\n",
                tn, GET_TRIG_NAME ( trig_to_add ), rnum->number );
        }
    }
    else
        ch->Send ( "Please specify 'mob', 'obj', or 'room'.\r\n" );

    if ( has_trig )
    {
        ch->Send ( "It already has trigger %d.\r\n", tn );
        free_trigger ( trig_to_add );
    }
}

/*
 *  finds a trigger specified by name, which can either be a number, or
 *  a 'silly' name including things like 2.beggar-death.
 */
trig_data *get_trigger ( script_data *sc, char *name )
{
    trig_data *t = nullptr;
    char *cname;
    int num = 0, n;
    bool string = FALSE;

    if ( ( cname = strstr ( name, "." ) ) || ( !isdigit ( *name ) ) )
    {
        string = TRUE;
        if ( cname )
        {
            *cname = '\0';
            num = atoi ( name );
            name = ++cname;
        }
    }
    else
        num = atoi ( name );

    for ( n = 0, t = TRIGGERS ( sc ); t; t = t->next )
    {
        if ( string )
        {
            if ( isname ( name, GET_TRIG_NAME ( t ) ) )
                if ( ++n >= num )
                    break;
        }

        /* this isn't clean... */
        /* a numeric value will match if it's position OR vnum */
        /* is found. originally the number was position-only */
        else if ( ++n >= num )
            break;
        else if ( trig_index[t->nr]->vnum == num )
            break;
    }
    return t;
}

/*
 *  Removes the trigger, plus the script if it removes the last trigger.
 *  Returns 0 if the trigger doesn't exist,
 *          1 if the trigger was removed,
 *      and 2 if the script was extracted as well.
 */
int remove_trigger ( void *thing, trig_data *trig, int type )
{
    script_data *sc = nullptr;
    switch ( type )
    {
        case MOB_TRIGGER:
            sc = SCRIPT ( ( Character * ) thing );
            break;
        case OBJ_TRIGGER:
            sc = SCRIPT ( ( obj_data * ) thing );
            break;
        case WLD_TRIGGER:
            sc = SCRIPT ( ( Room * ) thing );
            break;
    }

    if ( !sc || !trig )
        return 0;

    if ( trig != TRIGGERS ( sc ) )
    {
        trig_data *j = TRIGGERS ( sc );
        for ( ; j && j->next != trig; j = j->next )
            ;
        j->next = trig->next;
        extract_trigger ( trig );
    }
    else /* this was the first trigger */
    {
        TRIGGERS ( sc ) = trig->next;
        extract_trigger ( trig );
    }

    /* update the script type bitvector */
    set_script_types ( sc );

    if ( !TRIGGERS ( sc ) )
    {
        extract_script ( thing, type );
        return 2;
    }

    return 1;
}

ACMD ( do_detach )
{
    Character *victim = NULL;
    obj_data *object = NULL;
    Room *room;
    char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH], arg3[MAX_INPUT_LENGTH];
    char *trigger = 0;
    int num_arg;

    argument = two_arguments ( argument, arg1, arg2 );
    one_argument ( argument, arg3 );

    if ( !*arg1 || !*arg2 )
    {
        ch->Send ( "Usage: detach [ mob | object | room ] {{ target } {{ trigger | 'all' }\r\n" );
        return;
    }

    /* vnum of mob/obj, if given */
    num_arg = atoi ( arg2 );

    if ( !strcasecmp ( arg1, "room" ) || !strcasecmp ( arg1, "wtr" ) )
    {
        room = IN_ROOM ( ch );
#ifndef STOCK_CIRCLE

        if ( !can_edit_zone ( ch, room->zone ) )
        {
            ch->Send ( "You can only detach triggers in your own zone\r\n" );
            return;
        }
#endif
        if ( !SCRIPT ( room ) )
            ch->Send ( "This room does not have any triggers.\r\n" );
        else if ( !strcasecmp ( arg2, "all" ) )
        {
            bool running = FALSE;
            for ( trig_data *t_next, *t = TRIGGERS ( SCRIPT ( room ) ); t; t = t_next )
            {
                t_next = t->next;
                if ( t->curr_state )
                {
                    t->remove_me = TRUE;
                    running = TRUE;
                }
                else
                    remove_trigger ( room, t, WLD_TRIGGER );
            }

            if ( running )
                ch->Send ( "Some triggers on the room are running, they will be removed when they're done.\r\n" );
            else
                ch->Send ( "All triggers removed from room.\r\n" );
        }
        else
        {
            trig_data *t = get_trigger ( SCRIPT ( room ), arg2 );
            if ( t && t->curr_state )
            {
                t->remove_me = TRUE;
                ch->Send ( "That trigger is running, it will be removed once it's finished.\r\n" );
            }
            else if ( remove_trigger ( room, t, WLD_TRIGGER ) )
                ch->Send ( "Trigger removed.\r\n" );
            else
                ch->Send ( "That trigger was not found.\r\n" );
        }
    }
    else
    {
        if ( is_abbrev ( arg1, "mobile" ) || !strcasecmp ( arg1, "mtr" ) )
        {
            victim = get_char_vis ( ch, arg2, NULL, FIND_CHAR_WORLD );
            if ( !victim ) /* search room for one with this vnum */
            {
                for ( victim = IN_ROOM ( ch )->people;victim;victim=victim->next_in_room )
                    if ( GET_MOB_VNUM ( victim ) == num_arg )
                        break;

                if ( !victim )
                {
                    ch->Send ( "No such mobile around.\r\n" );
                    return;
                }
            }

            if ( !*arg3 )
                ch->Send ( "You must specify a trigger to remove.\r\n" );
            else
                trigger = arg3;
        }
        else if ( is_abbrev ( arg1, "object" ) || !strcasecmp ( arg1, "otr" ) )
        {
            object = get_obj_vis ( ch, arg2, NULL );
            if ( !object ) /* search room for one with this vnum */
            {
                for ( object = IN_ROOM ( ch )->contents;object;object=object->next_content )
                    if ( GET_OBJ_VNUM ( object ) == num_arg )
                        break;

                if ( !object ) /* search inventory for one with this vnum */
                {
                    for ( object = ch->carrying;object;object=object->next_content )
                        if ( GET_OBJ_VNUM ( object ) == num_arg )
                            break;

                    if ( !object ) /* give up */
                    {
                        ch->Send ( "No such object around.\r\n" );
                        return;
                    }
                }
            }

            if ( !*arg3 )
                ch->Send ( "You must specify a trigger to remove.\r\n" );
            else
                trigger = arg3;
        }
        else
        {
            /* Thanks to Carlos Myers for fixing the line below */
            if ( ( object = get_obj_in_equip_vis ( ch, arg1, NULL, ch->equipment ) ) )
                ;
            else if ( ( object = get_obj_in_list_vis ( ch, arg1, NULL, ch->carrying ) ) )
                ;
            else if ( ( victim = get_char_room_vis ( ch, arg1, NULL ) ) )
                ;
            else if ( ( object = get_obj_in_list_vis ( ch, arg1, NULL, IN_ROOM ( ch )->contents ) ) )
                ;
            else if ( ( victim = get_char_vis ( ch, arg1, NULL, FIND_CHAR_WORLD ) ) )
                ;
            else if ( ( object = get_obj_vis ( ch, arg1, NULL ) ) )
                ;
            else
                ch->Send ( "Nothing around by that name.\r\n" );

            trigger = arg2;
        }

        if ( victim )
        {
            if ( !IS_NPC ( victim ) )
                ch->Send ( "Players don't have triggers.\r\n" );

            else if ( !SCRIPT ( victim ) )
                ch->Send ( "That mob doesn't have any triggers.\r\n" );
#ifndef STOCK_CIRCLE

            else if ( !can_edit_zone ( ch, real_zone_by_thing ( GET_MOB_VNUM ( victim ) ) ) )
            {
                ch->Send ( "You can only detach triggers in your own zone\r\n" );
                return;
            }
#endif
            else if ( trigger && !strcasecmp ( trigger, "all" ) )
            {
                bool running = FALSE;
                for ( trig_data *t_next, *t = TRIGGERS ( SCRIPT ( victim ) ); t; t = t_next )
                {
                    t_next = t->next;
                    if ( t->curr_state )
                    {
                        t->remove_me = TRUE;
                        running = TRUE;
                    }
                    else
                        remove_trigger ( victim, t, MOB_TRIGGER );
                }

                if ( running )
                    ch->Send ( "Some triggers on the mob are running, they will be removed when they're done.\r\n" );
                else
                    ch->Send ( "All triggers removed from %s.\r\n", GET_SHORT ( victim ) );
            }
            else
            {
                trig_data *t = get_trigger ( SCRIPT ( victim ), trigger );
                if ( t && t->curr_state )
                {
                    t->remove_me = TRUE;
                    ch->Send ( "That trigger is running, it will be removed once it's finished.\r\n" );
                }
                else if ( remove_trigger ( victim, t, MOB_TRIGGER ) )
                    ch->Send ( "Trigger removed.\r\n" );
                else
                    ch->Send ( "That trigger was not found.\r\n" );
            }
        }
        else if ( object )
        {
            if ( !SCRIPT ( object ) )
                ch->Send ( "That object doesn't have any triggers.\r\n" );

#ifndef STOCK_CIRCLE

            else if ( !can_edit_zone ( ch, real_zone_by_thing ( GET_OBJ_VNUM ( object ) ) ) )
            {
                ch->Send ( "You can only detach triggers in your own zone\r\n" );
                return;
            }
#endif
            else if ( trigger && !strcasecmp ( trigger, "all" ) )
            {
                bool running = FALSE;
                for ( trig_data *t_next, *t = TRIGGERS ( SCRIPT ( object ) ); t; t = t_next )
                {
                    t_next = t->next;
                    if ( t->curr_state )
                    {
                        t->remove_me = TRUE;
                        running = TRUE;
                    }
                    else
                        remove_trigger ( object, t, OBJ_TRIGGER );
                }

                if ( running )
                    ch->Send ( "Some triggers on the object are running, they will be removed when they're done.\r\n" );
                else
                    ch->Send ( "All triggers removed from %s.\r\n",
                        object->short_description ? object->short_description : object->name );
            }
            else
            {
                trig_data *t = get_trigger ( SCRIPT ( object ), trigger );
                if ( t && t->curr_state )
                {
                    t->remove_me = TRUE;
                    ch->Send ( "That trigger is running, it will be removed once it's finished.\r\n" );
                }
                else if ( remove_trigger ( object, t, OBJ_TRIGGER ) )
                    ch->Send ( "Trigger removed.\r\n" );
                else
                    ch->Send ( "That trigger was not found.\r\n" );
            }
        }
    }
}


/*
*  Logs any errors caused by scripts to the system log.
 *  Will eventually allow on-line view of script errors.
 */
void script_vlog ( const char *format, va_list args )
{
    char buf[MAX_STRING_LENGTH];
    va_list argsbackup;
    va_copy ( argsbackup,args );
    Descriptor *i;
    bool syslog_complete;

    snprintf ( buf, sizeof ( buf ), "SCRIPT ERR: %s", format );

    basic_mud_vlog ( buf, args );
    va_copy ( args,argsbackup );

    /* the rest is mostly a rip from basic_mud_log() */
    strcpy ( buf, "[ " );         /* strcpy: OK */
    vsnprintf ( buf + 2, sizeof ( buf ) - 6, format, args );
    strcat ( buf, " ]\r\n" );     /* strcat: OK */

    for ( i = descriptor_list; i; i = i->next )
    {
        if ( STATE ( i ) != CON_PLAYING || IS_NPC ( i->character ) ) /* switch */
            continue;
        if ( GET_ORIG_LEV ( i->character ) == 0 && GET_LEVEL ( i->character ) < LVL_BUILDER )
            continue;
        if ( GET_ORIG_LEV ( i->character ) != 0 && GET_ORIG_LEV ( i->character ) < LVL_BUILDER )
            continue;
        if ( PLR_FLAGGED ( i->character, PLR_WRITING ) )
            continue;
        if ( ! ( CMD_FLAGS ( i->character ) & WIZ_TRIG_GRP ) )
            continue;
        syslog_complete = PRF_FLAGGED ( i->character, PRF_LOG1 ) && PRF_FLAGGED ( i->character, PRF_LOG2 );
        if ( ( CMD_FLAGS ( i->character ) & WIZ_IMM3_GRP ) && !syslog_complete )
            continue;

        i->Output ( "%s%s%s", CCGRN ( i->character, C_NRM ), buf, CCNRM ( i->character, C_NRM ) );
    }
}


void script_log ( const char *format, ... )
{
    va_list args;

    va_start ( args, format );
    script_vlog ( format, args );
    va_end ( args );
}

int count_dots ( char *str )
{
    int dot = 0;
    char *pt = str;

    while ( *pt++ )
        if ( *pt == '.' )
            dot++;

    return dot;

}

/* returns 1 if string is all digits, else 0 */
#if 0
int is_num ( char *arg )
{
    char *p = arg;
    if ( *arg == '\0' )
        return FALSE;

    if ( *arg == '+' || *arg == '-' )
        p++;

    for ( ; *p != '\0'; p++ )
    {
        if ( !isdigit ( *p ) )
            return FALSE;
    }

    return TRUE;
}
#else
int is_num ( char *num )
{
    if ( !*num )
        return 0;

    if ( *num == '-' )
        num++;

    while ( *num && isdigit ( *num ) )
        num++;

    if ( !*num || isspace ( *num ) )
        return 1;
    return 0;
}
#endif


#define E_FALSE strlcpy(result, "0", r_len)
#define E_TRUE strlcpy(result, "1", r_len)
/* evaluates 'lhs op rhs', and copies to result */
void eval_op ( char *op, char *lhs, char *rhs, char *result, size_t r_len, void *go,
               struct script_data *sc, trig_data *trig )
{
    char *p = NULL;
    int n =0;

    /* strip off extra spaces at begin and end */
    while ( *lhs && isspace ( *lhs ) )
        lhs++;
    while ( *rhs && isspace ( *rhs ) )
        rhs++;
    if ( *lhs != '\0' )
    {
        for ( p = lhs; *p; p++ )
            ;
        for ( --p; isspace ( *p ) && ( ( char * ) p > lhs ); *p-- = '\0' )
            ;
    }
    if ( *rhs != '\0' )
    {
        for ( p = rhs; *p; p++ )
            ;
        for ( --p; isspace ( *p ) && ( ( char * ) p > rhs ); *p-- = '\0' )
            ;
    }

    /* find the op, and figure out the value */
    if ( !strcmp ( "||", op ) )
    {
        if ( ( !*lhs || ( *lhs == '0' ) ) && ( !*rhs || ( *rhs == '0' ) ) )
            E_FALSE;
        else
            E_TRUE;
    }
    else if ( !strcmp ( "&&", op ) )
    {
        if ( !*lhs || ( *lhs == '0' ) || !*rhs || ( *rhs == '0' ) )
            E_FALSE;
        else
            E_TRUE;
    }
    else if ( !strcmp ( "==", op ) )
    {
        if ( is_num ( lhs ) && is_num ( rhs ) )
            snprintf ( result,r_len, "%d", atoi ( lhs ) == atoi ( rhs ) );
        else if ( ( !*lhs || ( *lhs == '0' ) ) && ( !*rhs || ( *rhs == '0' ) ) )
            E_TRUE;
        else
            snprintf ( result,r_len, "%d", !strcasecmp ( lhs, rhs ) );
    }
    else if ( !strcmp ( "!=", op ) )
    {
        if ( is_num ( lhs ) && is_num ( rhs ) )
            snprintf ( result,r_len, "%d", atoi ( lhs ) != atoi ( rhs ) );
        else if ( ( !*lhs || ( *lhs == '0' ) ) && ( !*rhs || ( *rhs == '0' ) ) )
            E_FALSE;
        else
            snprintf ( result,r_len, "%d", strcasecmp ( lhs, rhs ) );
    }
    else if ( !strcmp ( "<=", op ) )
    {
        if ( is_num ( lhs ) && is_num ( rhs ) )
            snprintf ( result,r_len, "%d", atoi ( lhs ) <= atoi ( rhs ) );
        else
            snprintf ( result,r_len, "%d", strcmp ( lhs, rhs ) <= 0 );
    }
    else if ( !strcmp ( ">=", op ) )
    {
        if ( is_num ( lhs ) && is_num ( rhs ) )
            snprintf ( result,r_len, "%d", atoi ( lhs ) >= atoi ( rhs ) );
        else
            snprintf ( result,r_len, "%d", strcmp ( lhs, rhs ) >= 0 );
    }
    else if ( !strcmp ( "<", op ) )
    {
        if ( is_num ( lhs ) && is_num ( rhs ) )
            snprintf ( result,r_len, "%d", atoi ( lhs ) < atoi ( rhs ) );
        else
            snprintf ( result,r_len, "%d", strcmp ( lhs, rhs ) < 0 );
    }
    else if ( !strcmp ( ">", op ) )
    {
        if ( is_num ( lhs ) && is_num ( rhs ) )
            snprintf ( result,r_len, "%d", atoi ( lhs ) > atoi ( rhs ) );
        else
            snprintf ( result,r_len, "%d", strcmp ( lhs, rhs ) > 0 );
    }
    else if ( !strcmp ( "/=", op ) )
        snprintf ( result,r_len, "%c", str_str ( lhs, rhs ) ? '1' : '0' );

    else if ( !strcmp ( "*", op ) )
        snprintf ( result,r_len, "%d", atoi ( lhs ) * atoi ( rhs ) );

    else if ( !strcmp ( "/", op ) )
        snprintf ( result,r_len, "%d", ( n = atoi ( rhs ) ) ? ( atoi ( lhs ) / n ) : 0 );

    else if ( !strcmp ( "+", op ) )
        snprintf ( result,r_len, "%d", atoi ( lhs ) + atoi ( rhs ) );

    else if ( !strcmp ( "-", op ) )
        snprintf ( result,r_len, "%d", atoi ( lhs ) - atoi ( rhs ) );

    else if ( !strcmp ( "!", op ) )
    {
        if ( is_num ( rhs ) )
            snprintf ( result,r_len, "%d", !atoi ( rhs ) );
        else
            snprintf ( result,r_len, "%d", !*rhs );
    }
    else if ( !strcmp ( "|=", op ) )
        snprintf ( result,r_len, "%c", is_abbrev ( lhs,rhs ) ? '1' : '0' );
}



/*
 * p points to the first quote, returns the matching
 * end quote, or the last non-null char in p.
*/
char *matching_quote ( char *p )
{
    for ( p++; *p && ( *p != '"' ); p++ )
    {
        if ( *p == '\\' )
            p++;
    }

    if ( !*p )
        p--;

    return p;
}

/*
 * p points to the first paren.  returns a pointer to the
 * matching closing paren, or the last non-null char in p.
 */
char *matching_paren ( char *p )
{
    int i;

    for ( p++, i = 1; *p && i; p++ )
    {
        if ( *p == '(' )
            i++;
        else if ( *p == ')' )
            i--;
        else if ( *p == '"' )
            p = matching_quote ( p );
    }

    return --p;
}



/* evaluates line, and returns answer in result */
void eval_expr ( char *line, char *result, size_t r_len, void *go, struct script_data *sc,
                 trig_data *trig, int type )
{
    char expr[MAX_INPUT_LENGTH] = "", *p = NULL;
    while ( *line && isspace ( *line ) )
        line++;


    /* added to get better valgrind output */
    if ( line )
        ; // line 2770
    if ( result )
        ;
    if ( go )
        ;
    if ( sc )
        ;
    if ( trig )
        ;
    if ( type )
        ;


    if ( eval_lhs_op_rhs ( line, result, r_len, go, sc, trig, type ) )
        ;

    else if ( *line == '(' )
    {
        p = strcpy ( expr, line );
        p = matching_paren ( expr );
        *p = '\0';
        eval_expr ( expr + 1, result, r_len, go, sc, trig, type );
    }
    else
        var_subst ( go, sc, trig, type, line, result, r_len );
}


/*
 * evaluates expr if it is in the form lhs op rhs, and copies
 * answer in result.  returns 1 if expr is evaluated, else 0
 */
int eval_lhs_op_rhs ( char *expr, char *result, size_t r_len, void *go, struct script_data *sc,
                      trig_data *trig, int type )
{
    char *p, *tokens[MAX_INPUT_LENGTH];
    char line[MAX_INPUT_LENGTH] = "", lhr[MAX_INPUT_LENGTH] = "", rhr[MAX_INPUT_LENGTH] = "";
    int i = 0, j = 0;

    /*
     * valid operands, in order of priority
     * each must also be defined in eval_op()
     */
    static const char *ops[] =
    {
        "||",
        "&&",
        "==",
        "!=",
        "<=",
        ">=",
        "<",
        ">",
        "/=",
        "+",
        "-",
        "/",
        "*",
        "!",
        "|=",
        "\n"
    };

    p = strcpy ( line, expr );

    /*
     * initialize tokens, an array of pointers to locations
     * in line where the ops could possibly occur.
     */
    for ( j = 0; *p; j++ )
    {
        tokens[j] = p;
        if ( *p == '(' )
            p = matching_paren ( p ) + 1;
        else if ( *p == '"' )
            p = matching_quote ( p ) + 1;
        else if ( isalnum ( *p ) )
            for ( p++; *p && ( isalnum ( *p ) || isspace ( *p ) ); p++ )
                ;
        else
            p++;
    }
    tokens[j] = NULL;
    for ( i = 0; *ops[i] != '\n'; i++ )
        for ( j = 0; tokens[j]; j++ )
            if ( !strn_cmp ( ops[i], tokens[j], strlen ( ops[i] ) ) )
            {
                *tokens[j] = '\0';
                p = tokens[j] + strlen ( ops[i] );

                eval_expr ( line, lhr, sizeof ( lhr ), go, sc, trig, type );

                // short-circuit &&
                if ( !strcmp ( ops[i], "&&" ) && *lhr == '0' )
                {
                    E_FALSE;
                    return 1;
                }

                // short-circuit ||
                if ( !strcmp ( ops[i], "||" ) && *lhr == '1' )
                {
                    E_TRUE;
                    return 1;
                }

                eval_expr ( p, rhr, sizeof ( rhr ), go, sc, trig, type );
                eval_op ( ( char * ) ops[i], lhr, rhr, result, r_len, go, sc, trig );
                return 1;
            }

    return 0;
}



/* returns 1 if cond is true, else 0 */
int process_if ( char *cond, void *go, struct script_data *sc,
                 trig_data *trig, int type )
{
    char result[MAX_INPUT_LENGTH] = "", *p;
    eval_expr ( cond, result, sizeof ( result ), go, sc, trig, type );

    p = result;
    skip_spaces ( &p );

    if ( !*p || *p == '0' )
        return 0;
    else
        return 1;
}


/*
 * scans for end of if-block.
 * returns the line containg 'end', or the last
 * line of the trigger if not found.
 */
struct cmdlist_element *find_end ( trig_data *trig, struct cmdlist_element *cl )
{
    struct cmdlist_element *c;
    char *p;

    if ( ! ( cl->next ) )
    {
        script_log ( "Trigger VNum %d has 'if' without 'end'. Line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cl->cmd );
        return cl;
    }

    for ( c = cl->next; c && c->next; c = c?c->next:NULL )
    {
        for ( p = c->cmd; *p && isspace ( *p ); p++ )
            ;

        if ( !strn_cmp ( "if ", p, 3 ) )
            c = find_end ( trig, c ); /* may return null */
        else if ( !strn_cmp ( "end", p, 3 ) )
            return c;
        if ( !c->next )   //rryan: this is the last line, we didn't find an end
        {
            script_log ( "Trigger VNum %d has 'if' without 'end'. Line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cl->cmd );
            return c;
        }
    }
    //rryan: if we got here, it's the last line, if its not an end,script_log it
    for ( p = c->cmd; *p && isspace ( *p ); p++ )
        ; /* skip spaces */
    if ( strn_cmp ( "end", p, 3 ) )
        script_log ( "Trigger VNum %d has 'if' without 'end'. Line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cl->cmd );
    return c;
}


/*
 * searches for valid elseif, else, or end to continue execution at.
 * returns line of elseif, else, or end if found, or last line of trigger.
 */
struct cmdlist_element *find_else_end ( trig_data *trig,
                                                    struct cmdlist_element *cl, void *go,
                                                    struct script_data *sc, int type )
{
    struct cmdlist_element *c;
    char *p;

    if ( ! ( cl->next ) )
        return cl;

    for ( c = cl->next; c && c->next; c = c ? c->next : NULL )
    {
        for ( p = c->cmd; *p && isspace ( *p ); p++ )
            ; /* skip spaces */

        if ( !strn_cmp ( "if ", p, 3 ) )
            c = find_end ( trig, c );

        else if ( !strn_cmp ( "elseif ", p, 7 ) || !strn_cmp ( "else if ", p, 8 ) )
        {
            if ( process_if ( p + 7, go, sc, trig, type ) )
            {
                GET_TRIG_DEPTH ( trig ) ++;
                return c;
            }
        }
        else if ( !strn_cmp ( "else", p, 4 ) )
        {
            GET_TRIG_DEPTH ( trig ) ++;
            return c;
        }
        else if ( !strn_cmp ( "end", p, 3 ) )
            return c;

        if ( !c->next )   //rryan: this is the last line, return
        {
            script_log ( "Trigger VNum %d has 'if' without 'end'. Line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cl->cmd );
            return c;
        }
    }
    //rryan: if we got here, it's the last line, if its not an end,script_log it
    for ( p = c->cmd; *p && isspace ( *p ); p++ )
        ; /* skip spaces */
    if ( strn_cmp ( "end", p, 3 ) )
        script_log ( "Trigger VNum %d has 'if' without 'end'. Line %d: %s",GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cl->cmd );
    return c;
}


/* processes any 'wait' commands in a trigger */
void process_wait ( void *go, trig_data *trig, int type, char *cmd, struct cmdlist_element *cl )
{
    char buf[MAX_INPUT_LENGTH] = "", *arg;
    struct wait_event_data *wait_event_obj;
    long when, hr, min, ntime;
    char c;

    arg = any_one_arg ( cmd, buf );
    skip_spaces ( &arg );

    if ( !*arg )
    {
        script_log ( "Trigger: %s, VNum %d. wait w/o an arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cl->cmd );
        return;
    }

    if ( !strn_cmp ( arg, "until ", 6 ) )
    {

        /* valid forms of time are 14:30 and 1430 */
        if ( sscanf ( arg, "until %ld:%ld", &hr, &min ) == 2 )
            min += ( hr * 60 );
        else
            min = ( hr % 100 ) + ( ( hr / 100 ) * 60 );

        /* calculate the pulse of the day of "until" time */
        ntime = ( min * SECS_PER_MUD_HOUR * PASSES_PER_SEC ) / 60;

        /* calculate pulse of day of current time */
        when = ( pulse % ( SECS_PER_MUD_HOUR * PASSES_PER_SEC ) ) +
               ( time_info.hours * SECS_PER_MUD_HOUR * PASSES_PER_SEC );

        if ( when >= ntime ) /* adjust for next day */
            when = ( SECS_PER_MUD_DAY * PASSES_PER_SEC ) - when + ntime;
        else
            when = ntime - when;
    }
    else
    {
        if ( sscanf ( arg, "%ld %c", &when, &c ) == 2 )
        {
            if ( c == 't' || c == 'T' )
                when *= PULSES_PER_MUD_HOUR;
            else if ( c == 's' || c == 'S' )
                when *= PASSES_PER_SEC;
        }
    }

    wait_event_obj = new wait_event_data ( trig, go, type );
    GET_TRIG_WAIT ( trig ) = event_create ( trig_wait_event, wait_event_obj, when, EVENT_TYPE_TRIG );
    trig->curr_state = cl->next;
}



/* processes a script set command */
void process_set ( struct script_data *sc, trig_data *trig, char *cmd )
{
    char arg[MAX_INPUT_LENGTH], name[MAX_INPUT_LENGTH] = "", *value;

    value = two_arguments ( cmd, arg, name );

    skip_spaces ( &value );

    if ( !*name )
    {
        script_log ( "Trigger: %s, VNum %d. set w/o an arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    add_var ( &GET_TRIG_VARS ( trig ), name, value, sc ? sc->context : 0 );

}


/* processes a script eval command */
void process_eval ( void *go, struct script_data *sc, trig_data *trig,
                    int type, char *cmd )
{
    char arg[MAX_INPUT_LENGTH], name[MAX_INPUT_LENGTH] = "";
    char result[MAX_INPUT_LENGTH], *expr;
    *result = '\0';

    expr = one_argument ( cmd, arg ); /* cut off 'eval' */
    expr = one_argument ( expr, name ); /* cut off name */

    skip_spaces ( &expr );

    if ( !*name )
    {
        script_log ( "Trigger: %s, VNum %d. eval w/o an arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    eval_expr ( expr, result, sizeof ( result ), go, sc, trig, type );
    add_var ( &GET_TRIG_VARS ( trig ), name, result, sc ? sc->context : 0 );
}

/* show a room to a char */
void process_peek ( void *go, struct script_data *sc, trig_data *trig, int type, char *cmd )
{
    char arg[MAX_INPUT_LENGTH], *room_v ;
    char result[MAX_INPUT_LENGTH];
    Character *c=NULL;
    Room *r=NULL;

    room_v = two_arguments ( cmd, result, arg );
    skip_spaces ( &room_v );

    if ( !*arg )
    {
        script_log ( "Trigger: %s, VNum %d. dg_peek w/o a char. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }
    else if ( !*room_v )
    {
        script_log ( "Trigger: %s, VNum %d. dg_peek w/o a room to peek into. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    if ( ( r = real_room ( atoi ( room_v ) ) ) == NULL )
    {
        script_log ( "Trigger: %s, VNum %d. dg_peek given an invalid room. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    if ( ( c = get_char ( arg ) ) == NULL )
    {
        script_log ( "Trigger: %s, VNum %d. dg_peek can't find the char to show the room to. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    view_room_by_rnum ( c, r );

}


/* script attaching a trigger to something */
void process_attach ( void *go, struct script_data *sc, trig_data *trig, int type, char *cmd )
{
    char arg[MAX_INPUT_LENGTH], trignum_s[MAX_INPUT_LENGTH];
    char *id_p;
    trig_data *trig_to_add;
    Character *c = nullptr;
    obj_data *o = nullptr;
    Room *r = nullptr;
    long trignum, id = 0;

    id_p = two_arguments ( cmd, arg, trignum_s );
    skip_spaces ( &id_p );

    if ( !*trignum_s )
    {
        script_log ( "Trigger: %s, VNum %d. attach w/o an arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    if ( id_p && *id_p )
    {
        if ( *id_p == UID_CHAR )
            id = atol ( id_p+1 );
        else
            id = atol ( id_p );
    }

    if ( id == 0 )
    {
        script_log ( "Trigger: %s, VNum %d. attach invalid id arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    c = find_char ( id );
    if ( !c )
    {
        o = find_obj ( id );
        if ( !o )
        {
            r = find_room ( id );
            if ( !r )
            {
                script_log ( "Trigger: %s, VNum %d. attach invalid id arg. Line %d: %s",
                             GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
                return;
            }
        }
    }

    /* locate and load the trigger specified */
    int trig_v = atoi ( trignum_s );
    trignum = real_trigger ( trig_v );
    if ( trignum == NOTHING || ! ( trig_to_add = read_trigger ( trignum ) ) )
    {
        script_log ( "Trigger: %s, VNum %d. attach invalid trigger: '%s'. Line %d: %s",
            GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), trignum_s, GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    trig_data *has_trig = nullptr;
    if ( c )
    {
        if ( !IS_NPC ( c ) )
        {
            script_log ( "Trigger: %s, VNum %d. attach invalid target: '%s'. Line %d: %s",
                GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_NAME ( c ), GET_TRIG_LINE_NR ( trig ), cmd );
            free_trigger ( trig_to_add );
            return;
        }

        if ( SCRIPT ( c ) )
            has_trig = find_trigger ( SCRIPT ( c ), trig_v );
        else
            SCRIPT ( c ) = create_script();

        if ( !has_trig )
            add_trigger ( SCRIPT ( c ), trig_to_add, -1 );
    }

    else if ( o )
    {
        if ( SCRIPT ( o ) )
            has_trig = find_trigger ( SCRIPT ( o ), trig_v );
        else
            SCRIPT ( o ) = create_script();

        if ( !has_trig )
            add_trigger ( SCRIPT ( o ), trig_to_add, -1 );
    }

    else if ( r )
    {
        if ( SCRIPT ( r ) )
            has_trig = find_trigger ( SCRIPT ( r ), trig_v );
        else
            SCRIPT ( r ) = create_script();

        if ( !has_trig )
            add_trigger ( SCRIPT ( r ), trig_to_add, -1 );
    }

    if ( has_trig )
        free_trigger ( trig_to_add );
}



/* script detaching a trigger from something */
void process_detach ( void *go, struct script_data *sc, trig_data *trig,
                      int type, char *cmd )
{
    char arg[MAX_INPUT_LENGTH], trignum_s[MAX_INPUT_LENGTH];
    char result[MAX_INPUT_LENGTH], *id_p;
    Character *c = nullptr;
    obj_data *o = nullptr;
    Room *r = nullptr;
    long id;
    trig_data *t, *t_next;

    id_p = two_arguments ( cmd, arg, trignum_s );
    skip_spaces ( &id_p );

    if ( !*trignum_s )
    {
        script_log ( "Trigger: %s, VNum %d. detach w/o an arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    if ( !id_p || !*id_p || atoi ( id_p ) ==0 )
    {
        script_log ( "Trigger: %s, VNum %d. detach invalid id arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    /* parse and locate the id specified */
    eval_expr ( id_p, result, sizeof ( result ), go, sc, trig, type );
    if ( ! ( id = atoi ( result ) ) )
    {
        script_log ( "Trigger: %s, VNum %d. detach invalid id arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }
    c = find_char ( id );
    if ( !c )
    {
        o = find_obj ( id );
        if ( !o )
        {
            r = find_room ( id );
            if ( !r )
            {
                script_log ( "Trigger: %s, VNum %d. detach invalid id arg. Line %d: %s",
                             GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
                return;
            }
        }
    }


    if ( c && SCRIPT ( c ) )
    {
        if ( !strcmp ( trignum_s, "all" ) )
        {
            for ( t = TRIGGERS ( SCRIPT ( c ) ); t; t = t_next )
            {
                t_next = t->next;
                if ( t->curr_state )
                    t->remove_me = TRUE;
                else
                    remove_trigger ( c, t, MOB_TRIGGER );
            }
            return;
        }

        auto t = find_trigger ( SCRIPT ( c ), atoi ( trignum_s ) );
        if ( t )
        {
            if ( t->curr_state )
                t->remove_me = TRUE;
            else
                remove_trigger ( c, t, MOB_TRIGGER );
        }
        return;
    }

    if ( o && SCRIPT ( o ) )
    {
        if ( !strcmp ( trignum_s, "all" ) )
        {
            for ( t = TRIGGERS ( SCRIPT ( o ) ); t; t = t_next )
            {
                t_next = t->next;
                if ( t->curr_state )
                    t->remove_me = TRUE;
                else
                    remove_trigger ( o, t, OBJ_TRIGGER );
            }
            return;
        }

        auto t = find_trigger ( SCRIPT ( o ), atoi ( trignum_s ) );
        if ( t )
        {
            if ( t->curr_state )
                t->remove_me = TRUE;
            else
                remove_trigger ( o, t, OBJ_TRIGGER );
        }
        return;
    }

    if ( r && SCRIPT ( r ) )
    {
        if ( !strcmp ( trignum_s, "all" ) )
        {
            for ( t = TRIGGERS ( SCRIPT ( r ) ); t; t = t_next )
            {
                t_next = t->next;
                if ( t->curr_state )
                    t->remove_me = TRUE;
                else
                    remove_trigger ( r, t, WLD_TRIGGER );
            }
            return;
        }

        auto t = find_trigger ( SCRIPT ( r ), atoi ( trignum_s ) );
        if ( t )
        {
            if ( t->curr_state )
                t->remove_me = TRUE;
            else
                remove_trigger ( r, t, WLD_TRIGGER );
        }
        return;
    }
}

room_rnum dg_room_of_obj ( struct obj_data *obj )
{
    if ( IN_ROOM ( obj ) != NULL )
        return IN_ROOM ( obj );
    if ( obj->carried_by )
        return IN_ROOM ( obj->carried_by );
    if ( obj->worn_by )
        return IN_ROOM ( obj->worn_by );
    if ( obj->in_obj )
        return ( dg_room_of_obj ( obj->in_obj ) );
    return NULL;
}


/* create a UID variable from the id number */
void makeuid_var ( void *go, struct script_data *sc, trig_data *trig,
                   int type, char *cmd )
{
    char junk[MAX_INPUT_LENGTH], varname[MAX_INPUT_LENGTH];
    char arg[MAX_INPUT_LENGTH], name[MAX_INPUT_LENGTH];
    char uid[MAX_INPUT_LENGTH];

    *uid = '\0';
    half_chop ( cmd, junk, cmd ); /* makeuid */
    half_chop ( cmd, varname, cmd ); /* variable name */
    half_chop ( cmd, arg, cmd );  /* numerical id or 'obj' 'mob' or 'room' */
    half_chop ( cmd, name, cmd ); /* if the above was obj, mob or room, this is the name */

    if ( !*varname )
    {
        script_log ( "Trigger: %s, VNum %d. makeuid w/o an arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );

        return;
    }

    if ( !*arg )
    {
        script_log ( "Trigger: %s, VNum %d. makeuid invalid id arg. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    if ( atoi ( arg ) !=0 )   /* easy, if you pass an id number */
    {
        char result[MAX_INPUT_LENGTH];

        eval_expr ( arg, result, sizeof ( result ), go, sc, trig, type );
        snprintf ( uid, sizeof ( uid ), "%c%s", UID_CHAR, result );
    }
    else   /* a lot more work without it */
    {
        if ( !*name )
        {
            script_log ( "Trigger: %s, VNum %d. makeuid needs name. Line %d: %s",
                         GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
            return;
        }

        if ( is_abbrev ( arg, "mob" ) )
        {
            Character *c = NULL;
            switch ( type )
            {
                case WLD_TRIGGER:
                    c = get_char_in_room ( ( Room * ) go, name );
                    break;
                case OBJ_TRIGGER:
                    c = get_char_near_obj ( ( struct obj_data * ) go, name );
                    break;
                case MOB_TRIGGER:
                    c = get_char_room_vis ( ( Character * ) go, name, NULL );
                    break;
            }
            if ( c )
                snprintf ( uid, sizeof ( uid ), "%c%ld", UID_CHAR, GET_ID ( c ) );
        }
        else if ( is_abbrev ( arg, "obj" ) )
        {
            struct obj_data *o = NULL;
            switch ( type )
            {
                case WLD_TRIGGER:
                    o = get_obj_in_room ( ( Room * ) go, name );
                    break;
                case OBJ_TRIGGER:
                    o = get_obj_near_obj ( ( struct obj_data * ) go, name );
                    break;
                case MOB_TRIGGER:
                    if ( ( o = get_obj_in_list_vis ( ( Character * ) go, name, NULL,
                                                     ( ( Character * ) go )->carrying ) ) == NULL )
                        o = get_obj_in_list_vis ( ( Character * ) go, name, NULL,
                                                  IN_ROOM ( ( Character * ) go )->contents );
                    break;
            }
            if ( o )
                snprintf ( uid, sizeof ( uid ), "%c%ld", UID_CHAR, GET_ID ( o ) );
        }
        else if ( is_abbrev ( arg, "room" ) )
        {
            room_rnum r = NULL;
            switch ( type )
            {
                case WLD_TRIGGER:
                    r = ( Room * ) go;
                    break;
                case OBJ_TRIGGER:
                    r = obj_room ( ( struct obj_data * ) go );
                    break;
                case MOB_TRIGGER:
                    r = IN_ROOM ( ( Character * ) go );
                    break;
            }
            if ( r != NULL )
                snprintf ( uid, sizeof ( uid ), "%c%ld", UID_CHAR, ( long ) r->number+ROOM_ID_BASE );
        }
        else
        {
            script_log ( "Trigger: %s, VNum %d. makeuid syntax error. Line %d: %s",
                         GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );

            return;
        }
    }
    if ( *uid )
        add_var ( &GET_TRIG_VARS ( trig ), varname, uid, sc ? sc->context : 0 );
}

// When a trigger stops running, reset its variables so it can be called again
// Reset the parents as well
void reset_trigger ( trig_data *trig )
{
    for ( trig_data *t = trig; t; t = t->parent )
    {
        if ( GET_TRIG_VARS ( t ) )
        {
            free_varlist ( GET_TRIG_VARS ( t ) );
            GET_TRIG_VARS ( t ) = nullptr;
        }

        if ( GET_TRIG_WAIT ( t ) )
        {
            event_cancel ( GET_TRIG_WAIT ( t ) );
            GET_TRIG_WAIT ( t ) = nullptr;
        }

        GET_TRIG_DEPTH ( t ) = 0;
        GET_TRIG_LINE_NR ( t ) = 0;
        GET_TRIG_LOOPS ( t ) = 0;
        t->curr_state = nullptr;
    }
}

void function_script ( void *go, struct script_data *sc, trig_data *parent, int type, char *cmd )
{
    trig_data *t;
    char buf[MAX_INPUT_LENGTH];
    trig_vnum vnum;
    struct trig_var_data *vd = NULL;
    cmd = any_one_arg ( cmd, buf ); /* remove 'function ' */
    cmd = any_one_arg ( cmd, buf ); /* vnum in buf, cmd is rest. */
    skip_spaces ( &cmd );

    if ( !*buf || !is_number ( buf ) )
    {
        script_log ( "Trigger: %s, VNum %d. calling function without a valid vnum! Line %d", GET_TRIG_NAME ( parent ), GET_TRIG_VNUM ( parent ), GET_TRIG_LINE_NR ( parent ) );
        reset_trigger ( parent ); // the parent is not resumed
        return;
    }

    vnum = atoi ( buf );
    t = read_trigger ( real_trigger ( vnum ) );

    if ( !t )
    {
        script_log ( "Trigger: %s, VNum %d. calling function %d when function doesn't exist! Line %d", GET_TRIG_NAME ( parent ), GET_TRIG_VNUM ( parent ), vnum, GET_TRIG_LINE_NR ( parent ) );
        reset_trigger ( parent ); // the parent is not resumed
        return;
    }

    if ( GET_TRIG_VNUM ( parent ) == vnum )
    {
        script_log ( "Trigger: %s, VNum %d. a function trigger is trying to call itself in line %d", GET_TRIG_NAME ( parent ), GET_TRIG_VNUM ( parent ), GET_TRIG_LINE_NR ( parent ) );
        reset_trigger ( parent ); // the parent is not resumed
        return;
    }

    t->attach_type = type;
    add_trigger ( sc, t, -1 ); // it's removed when it finishes in script_driver, or when the owner is purged

    // Only need to check for MTRIG_FUNCTION, obj and room functions have the same value.
    // Which is good, otherwise functions can't be called when attach types don't match (because of set_script_types).
    if ( !TRIGGER_CHECK ( t, MTRIG_FUNCTION ) )
    {
        script_log ( "Trigger: %s, VNum %d. calling non function trigger %d! Line %d", GET_TRIG_NAME ( parent ), GET_TRIG_VNUM ( parent ), vnum, GET_TRIG_LINE_NR ( parent ) );
        reset_trigger ( parent ); // the parent is not resumed
        remove_trigger ( go, t, type );
        return;
    }

    add_var ( &GET_TRIG_VARS ( parent ), "args", cmd, 0 );

    for ( vd = GET_TRIG_VARS ( parent ); vd; vd = vd->next )
        add_var ( &GET_TRIG_VARS ( t ), vd->name.c_str(), vd->value.c_str(), vd->context );

    t->parent = parent;
    script_driver ( &go, t, type, TRIG_NEW );
}
/** end of dg_scripts scriptable functions **/



/*
 * removes a variable from the global vars of sc,
 * or the local vars of trig if not found in global list.
 */
void process_unset ( struct script_data *sc, trig_data *trig, char *cmd )
{
    char arg[MAX_INPUT_LENGTH], *var;

    var = any_one_arg ( cmd, arg );

    skip_spaces ( &var );

    if ( !*var )
    {
        script_log ( "Trigger: %s, VNum %d. unset w/o an arg. Line %d: %s", GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    if ( !remove_var ( & ( sc->global_vars ), var ) )
        remove_var ( &GET_TRIG_VARS ( trig ), var );
}


/*
 * copy a locally owned variable to the globals of another script
 *     'remote <variable_name> <uid>'
 */
void process_remote ( struct script_data *sc, trig_data *trig, char *cmd )
{
    struct trig_var_data *vd;
    struct script_data *sc_remote=NULL;
    char *line, *var, *uid_p;
    char arg[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];
    long uid, context;
    Room *room;
    Character *mob;
    obj_data *obj;

    line = any_one_arg ( cmd, arg );
    two_arguments ( line, buf, buf2 );
    var = buf;
    uid_p = buf2;
    skip_spaces ( &var );
    skip_spaces ( &uid_p );


    if ( !*buf || !*buf2 )
    {
        script_log ( "Trigger: %s, VNum %d. remote: invalid arguments in line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    /* find the locally owned variable */
    for ( vd = GET_TRIG_VARS ( trig ); vd; vd = vd->next )
        if ( !strcasecmp ( vd->name.c_str(), buf ) )
            break;

    if ( !vd )
        for ( vd = sc->global_vars; vd; vd = vd->next )
            if ( !strcasecmp ( vd->name.c_str(), var ) &&
                    ( vd->context==0 || vd->context==sc->context ) )
                break;

    if ( !vd )
    {
        script_log ( "Trigger: %s, VNum %d. local var '%s' not found in remote call. Line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), buf, GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    /* find the target script from the uid number */
    uid = atoi ( buf2 );
    if ( uid<=0 )
    {
        script_log ( "Trigger: %s, VNum %d. remote: illegal uid '%s' in line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), buf2, GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    /* for all but PC's, context comes from the existing context. */
    /* for PC's, context is 0 (global) */
    context = vd->context;

    if ( ( room = find_room ( uid ) ) )
    {
        sc_remote = SCRIPT ( room );
    }
    else if ( ( mob = find_char ( uid ) ) )
    {
        sc_remote = SCRIPT ( mob );
        if ( !IS_NPC ( mob ) )
            context = 0;
    }
    else if ( ( obj = find_obj ( uid ) ) )
    {
        sc_remote = SCRIPT ( obj );
    }
    else
    {
        script_log ( "Trigger: %s, VNum %d. remote: uid '%ld' invalid in line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), uid, GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    if ( sc_remote==NULL )
        return; /* no script to assign */

    add_var ( & ( sc_remote->global_vars ), vd->name, vd->value, context );
    if (mob && !IS_NPC(mob))
      //this is a player. lets save the character right now
      save_char_vars(mob);
}



/*
 * command-line interface to rdelete
 * named vdelete so people didn't think it was to delete rooms
 */

ACMD ( do_vdelete )
{
    struct trig_var_data *vd, *vd_prev = NULL;
    struct script_data *sc_remote = NULL;
    char *var, *uid_p;
    char buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];
    long uid;
    //context;
    Room *room = NULL;
    Character *mob = NULL;
    obj_data *obj = NULL;

    argument = two_arguments ( argument, buf, buf2 );
    var = buf;
    uid_p = buf2;
    skip_spaces ( &var );
    skip_spaces ( &uid_p );


    if ( !*buf || !*buf2 )
    {
        ch->Send ( "Usage: vdelete < variable_name | * | all > < id | player_name >\r\n" );
        return;
    }


    /* find the target script from the uid number */
    if ( is_number ( buf2 ) )
    {
        uid = atoi ( buf2 );
        if ( uid<=0 )
        {
            ch->Send ( "vdelete: illegal id specified.\r\n" );
            return;
        }

        if ( ( room = find_room ( uid ) ) )
        {
            sc_remote = SCRIPT ( room );
        }
        else if ( ( mob = find_char ( uid ) ) )
        {
            sc_remote = SCRIPT ( mob );
            //if ( !IS_NPC ( mob ) )
            //	context = 0;
        }
        else if ( ( obj = find_obj ( uid ) ) )
        {
            sc_remote = SCRIPT ( obj );
        }
        else
        {
            ch->Send ( "vdelete: cannot resolve specified id.\r\n" );
            return;
        }
    }
    /* find the player name */
    else
    {
        if ( ( uid = pi.IdByName ( buf2 ) ) == -1 )
        {
            ch->Send ( "Player %s doesn't exist.\r\n", buf2 );
            return;
        }

        if ( ! ( mob = find_char ( uid ) ) )
        {
            ch->Send ( "Player %s is offline.\r\n", buf2 );
            return;
        }
        sc_remote = SCRIPT ( mob );
    }

    if ( !sc_remote )
    {
        ch->Send ( "No script found.\r\n" );
        return;
    }

    if ( !sc_remote->global_vars )
    {
        ch->Send ( "The script doesn't have any global variables.\r\n" );
        return;
    }

    if (*var == '*' || is_abbrev(var, "all")) {
        struct trig_var_data *vd_next;
        for (vd = sc_remote->global_vars; vd; vd = vd_next) {
            vd_next = vd->next;
            delete vd;
        }
        sc_remote->global_vars = NULL;
        ch->Send ("All variables deleted from that id.\r\n");
        return;
    }

    /* find the global */
    for ( vd = sc_remote->global_vars; vd; vd_prev = vd, vd = vd->next )
        if ( !strcasecmp ( vd->name.c_str(), var ) )
            break;

    if ( !vd )
    {
        ch->Send ( "That variable cannot be located.\r\n" );
        return;
    }

    /* ok, delete the variable */
    if ( vd_prev )
        vd_prev->next = vd->next;
    else
        sc_remote->global_vars = vd->next;

    /* and free up the space */
    delete vd;

    if (mob && !IS_NPC(mob))
      //this is a player. lets save the character right now
      save_char_vars(mob);

    ch->Send ( "Deleted.\r\n" );
}


/*
 * delete a variable from the globals of another script
 *     'rdelete <variable_name> <uid>'
 */
void process_rdelete ( struct script_data *sc, trig_data *trig, char *cmd )
{
    struct trig_var_data *vd, *vd_prev=NULL;
    struct script_data *sc_remote=NULL;
    char *line, *var, *uid_p;
    char arg[MAX_INPUT_LENGTH], buf[MAX_STRING_LENGTH], buf2[MAX_STRING_LENGTH];
    long uid;
    //, context;
    Room *room;
    Character *mob;
    obj_data *obj;

    line = any_one_arg ( cmd, arg );
    two_arguments ( line, buf, buf2 );
    var = buf;
    uid_p = buf2;
    skip_spaces ( &var );
    skip_spaces ( &uid_p );


    if ( !*buf || !*buf2 )
    {
        script_log ( "Trigger: %s, VNum %d. rdelete: invalid arguments in line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }


    /* find the target script from the uid number */
    uid = atoi ( buf2 );
    if ( uid<=0 )
    {
        script_log ( "Trigger: %s, VNum %d. rdelete: illegal uid '%s' in line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), buf2, GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }


    if ( ( room = find_room ( uid ) ) )
    {
        sc_remote = SCRIPT ( room );
    }
    else if ( ( mob = find_char ( uid ) ) )
    {
        sc_remote = SCRIPT ( mob );
        /*if ( !IS_NPC ( mob ) )
            context = 0;*/
    }
    else if ( ( obj = find_obj ( uid ) ) )
    {
        sc_remote = SCRIPT ( obj );
    }
    else
    {
        script_log ( "Trigger: %s, VNum %d. remote: uid '%ld' invalid in line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), uid, GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    if ( sc_remote==NULL )
        return; /* no script to delete a trigger from */
    if ( sc_remote->global_vars==NULL )
        return; /* no script globals */

    /* find the global */
    for ( vd = sc_remote->global_vars; vd; vd_prev = vd, vd = vd->next )
        if ( !strcasecmp ( vd->name.c_str(), var ) &&
                ( vd->context==0 || vd->context==sc->context ) )
            break;

    if ( !vd )
        return; /* the variable doesn't exist, or is the wrong context */

    /* ok, delete the variable */
    if ( vd_prev )
        vd_prev->next = vd->next;
    else
        sc_remote->global_vars = vd->next;

    /* and free up the space */
    delete vd;

    if (mob && !IS_NPC(mob))
      //this is a player. lets save the character right now
      save_char_vars(mob);

}

void process_restring ( script_data *sc, trig_data *trig, char *cmd )
{
    // Usage: restring <id> <field> <args>
    istringstream iss ( cmd );
    string arg;
    vector<string> args;
    int id = 0;
    obj_data *obj;

    iss >> arg;
    while ( iss >> arg )
        args.push_back ( arg );

    if ( args.size() < 3 )
    {
        script_log ( "Trigger [%d] %s, restring: missing argument in line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    iss.clear();
    if ( args[0][0] == UID_CHAR )
        iss.str ( args[0].substr ( 1 ) );
    else
        iss.str ( args[0] );

    iss >> id;
    obj = find_obj ( id );
    if ( obj == NULL )
    {
        script_log ( "Trigger [%d] %s, restring: invalid id in line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    obj_rnum rn = GET_OBJ_RNUM ( obj );
    if ( args[1] == "name" )
    {
        if ( obj->name && ( rn == NOTHING || obj->name != obj_proto[rn].name ) )
            free ( obj->name );
        arg = args[2];
        for ( int i = 3; i < args.size(); ++i )
            arg += " " + args[i];
        obj->name = str_udup ( arg.c_str() );
    }
    else if ( args[1] == "short" )
    {
        SET_BIT_AR ( GET_OBJ_EXTRA ( obj ), ITEM_UNIQUE_SHORTDESC );
        if ( obj->short_description && ( rn == NOTHING || obj->short_description != obj_proto[rn].short_description ) )
            free ( obj->short_description );
        arg = args[2];
        for ( int i = 3; i < args.size(); ++i )
            arg += " " + args[i];
        obj->short_description = str_udup ( arg.c_str() );
    }
    else if ( args[1] == "long" )
    {
        if ( obj->description && ( rn == NOTHING || obj->description != obj_proto[rn].description ) )
            free ( obj->description );
        arg = args[2];
        arg[0] = UPPER ( arg[0] );
        for ( int i = 3; i < args.size(); ++i )
            arg += " " + args[i];
        obj->description = str_udup ( arg.c_str() );
    }
    else if ( args[1] == "extra" )
    {
        // Usage: restring <id> extra '<keywords>' <description>
        if ( args.size() == 3 )
        {
            script_log ( "Trigger [%d] %s, restring: missing argument in line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
            return;
        }

        int symbol_count = 0, i = 3;
        string keywords = args[2];
        if ( keywords[0] == '\'' )
        {
            symbol_count++;
            keywords = keywords.substr ( 1 );
        }
        if ( keywords.back() == '\'' )
        {
            symbol_count++;
            keywords = keywords.substr ( 0, keywords.length()-1 );
        }
        for ( ; i < args.size() && symbol_count < 2; ++i )
        {
            keywords += " " + args[i];
            if ( keywords.back() == '\'' )
                symbol_count++;
        }
        if ( symbol_count != 2 )
        {
            script_log ( "Trigger [%d] %s, restring extra: missing symbols in line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
            return;
        }

        string desc;
        if ( i < args.size() )
            desc = args[i];
        while ( ++i < args.size() )
            desc += " " + args[i];
        desc[0] = UPPER ( desc[0] );

        int result = edit_extra_desc ( obj, keywords.c_str(), desc.c_str() );
        if ( result == 1 )
            script_log ( "Trigger [%d] %s, restring: couldn't change ex_desc because one of the keywords already exists in line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
    }
    else
        script_log ( "Trigger [%d] %s, restring: unknown field %s in line %d: %s", GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), args[1].c_str(), GET_TRIG_LINE_NR ( trig ), cmd );
}

void process_playeruid ( script_data *sc, trig_data *trig, char *cmd )
{
    char varname[MAX_INPUT_LENGTH], junk[MAX_INPUT_LENGTH];
    char *playername;

    playername = two_arguments ( cmd, junk, varname );
    skip_spaces ( &playername );

    for ( Descriptor *d = descriptor_list; d; d = d->next )
    {
        if ( !IS_PLAYING ( d ) )
            continue;
        if ( strcasecmp ( playername, d->character->player.name ) )
            continue;
        string value = string ( 1, UID_CHAR ) + to_string ( GET_ID ( d->character ) );
        add_var ( &GET_TRIG_VARS ( trig ), varname, value.c_str(), sc ? sc->context : 0 );
        return;
    }
    add_var ( &GET_TRIG_VARS ( trig ), varname, "", sc ? sc->context : 0 );
}

void delete_trailing_spaces(char* string) {
    if (!string || !*string) //null or empty string passed
    return;
    int end = strlen(string);

    while (--end >= 0 && string[end] == ' ');
    string[end+1]='\0';
}



/*
 * makes a local variable into a global variable
 */
void process_global ( struct script_data *sc, trig_data * trig, char *cmd, long id )
{
    struct trig_var_data *vd;
    char arg[MAX_INPUT_LENGTH], *var;
    //char arg1[MAX_INPUT_LENGTH];
    var = any_one_arg ( cmd, arg );

    //any_one_arg ( var, arg1 );
    skip_spaces(&var);
    delete_trailing_spaces(var);

    if ( !*var )
    {
        script_log ( "Trigger: %s, VNum %d. global w/o an arg in line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    for ( vd = GET_TRIG_VARS ( trig ); vd; vd = vd->next )
        if ( !strcasecmp ( vd->name.c_str(), var ) )
            break;

    if ( !vd )
    {
        script_log ( "Trigger: %s, VNum %d. local var '%s' not found in global call in line %d: %s",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), var, GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    add_var ( & ( sc->global_vars ), vd->name.c_str(), vd->value.c_str(), id );
    remove_var ( &GET_TRIG_VARS ( trig ), vd->name.c_str() );
}


/* set the current context for a script */
void process_context ( struct script_data *sc, trig_data * trig, char *cmd )
{
    char arg[MAX_INPUT_LENGTH], *var;

    var = any_one_arg ( cmd, arg );

    skip_spaces ( &var );

    if ( !*var )
    {
        script_log ( "Trigger: %s, VNum %d. context w/o an arg in line %d: '%s'",
                     GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    sc->context = atol ( var );
}

void purge_mob ( Character *mob )
{
    if ( !mob || !IS_NPC ( mob ) )
        return;

    // if this is the head of a segmented mob, purge the segments first
    combine_data *segment, *next;
    for ( segment = mob->mob_specials.join_list; segment; segment = next )
    {
        next = segment->next;
        purge_mob ( segment->joined );
    }

    // purge inv
    obj_data *obj, *next_obj;
    for ( obj = mob->carrying; obj; obj = next_obj )
    {
        next_obj = obj->next_content;
        extract_obj ( obj );
    }

    // purge eq
    for ( int i = 0; i < NUM_WEARS; ++i )
        if ( GET_EQ ( mob, i ) )
            extract_obj ( GET_EQ ( mob, i ) );

    // purge mob
    extract_char ( mob );
}

/* purge a mob including inventory and equipment */
void process_purgemob ( void *thing, int type, trig_data *trig, char *cmd )
{
    char command[MAX_INPUT_LENGTH], *arg;
    arg = one_argument ( cmd, command );
    skip_spaces ( &arg );

    if ( !*arg )
    {
        script_log ( "Trigger [%d] %s, purgemob has no argument in line %d",
            GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ) );
        return;
    }

    if ( !strcmp ( cmd, "target_not_found" ) )
        return;

    Character *victim = nullptr;
    Room *room = nullptr;

    if ( *arg == UID_CHAR )
    {
        victim = get_char ( arg );
        if ( !victim )
            return;
    }
    else if ( type == MOB_TRIGGER )
    {
        victim = get_char_room_vis ( (Character *) thing, arg, nullptr );
        if ( !victim )
            return;
    }
    else if ( type == OBJ_TRIGGER )
        room = obj_room ( (obj_data *) thing );
    else if ( type == WLD_TRIGGER )
        room = (Room *) thing;
    else
    {
        script_log ( "Trigger [%d] %s, purgemob: unknown type %d",
            GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), type );
        return;
    }

    if ( !victim )
        victim = get_room_vis ( room, arg, nullptr, nullptr );

    if ( victim )
    {
        if ( !IS_NPC ( victim ) )
            script_log ( "Trigger [%d] %s, purgemob tried to purge a PC. Line %d: %s",
                GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        else
            purge_mob ( victim );
    }
}

/* syntax: target <uid of actor> <var> inv|eq|room|char <args>
 * sets var to the uid of the obj or ch args refers to
 */
void process_target ( trig_data *trig, char *cmd )
{
    char actor[MAX_INPUT_LENGTH];
    char var[MAX_INPUT_LENGTH];
    char loc[MAX_INPUT_LENGTH];
    char *args;

    args = any_one_arg ( cmd, actor ); // skip "target"
    args = any_one_arg ( args, actor );
    args = any_one_arg ( args, var );
    args = any_one_arg ( args, loc );
    skip_spaces ( &args );

    if ( !*args )
    {
        script_log ( "Trigger [%d] %s, target didn't get enough arguments. Line %d: %s",
            GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    Character *ch = get_char ( actor );
    if ( !ch )
    {
        script_log ( "Trigger [%d] %s, target didn't get an actor. Line %d: %s",
            GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    bitvector_t location;
    if ( !strcmp ( loc, "inv" ) )
        location = FIND_OBJ_INV;
    else if ( !strcmp ( loc, "eq" ) )
        location = FIND_OBJ_EQUIP;
    else if ( !strcmp ( loc, "room" ) )
        location = FIND_OBJ_ROOM;
    else if ( !strcmp ( loc, "char" ) )
        location = FIND_CHAR_ROOM;
    else
    {
        script_log ( "Trigger [%d] %s, target got an invalid search location. Line %d: %s",
            GET_TRIG_VNUM ( trig ), GET_TRIG_NAME ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    Character *tch;
    obj_data *tobj;
    generic_find ( args, location, ch, &tch, &tobj );
    if ( location == FIND_CHAR_ROOM )
    {
        if ( tch )
        {
            char value[MAX_INPUT_LENGTH];
            sprintf ( value, "%c%ld", UID_CHAR, GET_ID ( tch ) );
            add_var ( &GET_TRIG_VARS ( trig ), var, value, 0 );
        }
        else
            add_var ( &GET_TRIG_VARS ( trig ), var, "", 0 );
    }
    else
    {
        if ( tobj )
        {
            char value[MAX_INPUT_LENGTH];
            sprintf ( value, "%c%ld", UID_CHAR, GET_ID ( tobj ) );
            add_var ( &GET_TRIG_VARS ( trig ), var, value, 0 );
        }
        else
            add_var ( &GET_TRIG_VARS ( trig ), var, "", 0 );
    }
}

void process_log ( trig_data *trig, char *cmd )
{
    new_mudlog ( CMP, LVL_IMMORT, TRUE, "Trigger [%d] %s: %s", GET_TRIG_VNUM ( trig ),
        GET_TRIG_NAME ( trig ), cmd );
}

void extract_value ( struct script_data *sc, trig_data * trig, char *cmd )
{
    char buf[MAX_INPUT_LENGTH];
    char buf2[MAX_INPUT_LENGTH];
    char *buf3 = NULL;
    char to[128];
    int num = 0;

    buf3 = any_one_arg ( cmd, buf );
    half_chop ( buf3, buf2, buf );
    strcpy ( to, buf2 );

    num = atoi ( buf );
    if ( num < 1 )
    {
        script_log ( "Trigger: %s, VNum %d. extract number < 1! Line %d: %s",
            GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    half_chop ( buf, buf3, buf2 );

    while ( num > 0 )
    {
        half_chop ( buf2, buf, buf2 );
        num--;
    }

    add_var ( &GET_TRIG_VARS ( trig ), to, buf, sc ? sc->context : 0 );
}


/*
  Thanks to Jamie Nelson for 4 dimensions for this addition

  Syntax :
    dg_letter <new varname> <letter position> <string to get from>

    ie:
    set string L337-String
    dg_letter var1 4 %string%
    dg_letter var2 11 %string%

    now %var1% == 7 and %var2% == g

    Note that the index starts at 1.

*/

void dg_letter_value ( struct script_data *sc, trig_data *trig, char *cmd )
{
    //set the letter/number at position 'num' as the variable.
    char junk[MAX_INPUT_LENGTH];
    char varname[MAX_INPUT_LENGTH];
    char num_s[MAX_INPUT_LENGTH];
    char string[MAX_INPUT_LENGTH];
    size_t num;

    half_chop ( cmd, junk, cmd );   /* "dg_letter" */
    half_chop ( cmd, varname, cmd );
    half_chop ( cmd, num_s, string );

    num = atoi ( num_s );

    if ( num < 1 )
    {
        script_log ( "Trigger: %s, VNum %d. dg_letter number < 1! Line %d: %s",
            GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    if ( num > strlen ( string ) )
    {
        script_log ( "Trigger: %s, VNum %d. dg_letter number > strlen! Line %d: %s",
            GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    *junk = string[num-1];
    * ( junk+1 ) = '\0';
    add_var ( &GET_TRIG_VARS ( trig ), varname, junk, sc->context );
}

void insert_word ( script_data *sc, trig_data *trig, char *cmd, bool replace )
{
    /* cmd = insert_word <word> <position> <var> */
    string input = string ( cmd );
    stringstream ss ( input );
    vector<string> args;
    string arg;
    ss >> arg;
    while ( ss >> arg )
        args.push_back ( arg );

    if ( args.size() != 3 || ( args.size() > 1 && !is_number ( args[1].c_str() ) ) )
    {
        script_log ( "Trigger: %s, VNum %d. insert_word: wrong syntax in line %d: %s",
            GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    int pos = atoi ( args[1].c_str() );

    trig_var_data *vd = nullptr;
    for ( vd = GET_TRIG_VARS ( trig ); vd; vd = vd->next )
        if ( vd->name == args[2] )
            break;

    if ( !vd )
    {
        // the variable doesn't exist: add it
        add_var ( &GET_TRIG_VARS ( trig ), args[2].c_str(), args[0].c_str(), sc->context );
        return;
    }

    ss.clear();
    ss.str ( vd->value );
    string result;

    for ( int c = 1;; c++ )
    {
        if ( c >= pos || ss.eof() )
        {
            if ( pos <= 1 )
                result = args[0];
            else
                result += " " + args[0];
            if ( replace )
                ss >> arg;
            while ( ss >> arg )
                result += " " + arg;
            break;
        }
        ss >> arg;
        if ( c == 1 )
            result = arg;
        else
            result += " " + arg;
    }

    vd->value = result;
}

void replace_letter ( trig_data *trig, char *cmd )
{
    // cmd = replace_letter <letter> <position> <variable>
    string input = string ( cmd );
    stringstream ss ( input );
    vector<string> args;
    string arg;
    ss >> arg;
    while ( ss >> arg )
        args.push_back ( arg );

    if ( args.size() != 3 || ( args.size() > 1 && !is_number ( args[1].c_str() ) ) )
    {
        script_log ( "Trigger: %s, VNum %d. insert_word: wrong syntax in line %d: %s",
            GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    trig_var_data *vd = nullptr;
    for ( vd = GET_TRIG_VARS ( trig ); vd; vd = vd->next )
        if ( vd->name == args[2] )
            break;

    if ( !vd )
    {
        script_log ( "Trigger: %s, VNum %d. insert_word: the variable doesn't exist. Line %d: %s",
            GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    int pos = atoi ( args[1].c_str() );

    if ( pos < 1 || pos > vd->value.size() )
    {
        script_log ( "Trigger: %s, VNum %d. insert_word: the position is out of bounds. Line %d: %s",
            GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), cmd );
        return;
    }

    vd->value[ pos-1 ] = args[0][0];
}

/*  This is the core driver for scripts. */
/*  Arguments:
    void *go_adress
      A pointer to a pointer to the entity running the script.
      The reason for this approcah is that we want to be able to see
      from the calling function, if the entity has been free'd.

    trig_data *trig
      A pointer to the current running trigger.

    int type
      MOB_TRIGGER, OBJ_TRIGGER or WLD_TRIGGER, respectively.

    int mode
      TRIG_NEW     just started from dg_triggers.c
      TRIG_RESTART restarted after a 'wait'
*/
#if 0
int script_driver ( union script_driver_data_u *sdd, trig_data *trig, int type, int mode )
#else
int script_driver ( void *go_adress, trig_data *trig, int type, int mode )
#endif
{
    static int depth = 0;
    int ret_val = 1;
    struct cmdlist_element *cl;
    char cmd[MAX_INPUT_LENGTH], *p;
    struct script_data *sc = 0;
    struct cmdlist_element *temp;
    unsigned long loops = 0;
    void *go = NULL;

    void obj_command_interpreter ( obj_data * obj, char *argument );
    void wld_command_interpreter ( Room *room, char *argument );

    if ( depth > MAX_SCRIPT_DEPTH )
    {
        script_log ( "Trigger %d recursed beyond maximum allowed depth.", GET_TRIG_VNUM ( trig ) );
        if ( go )
        {
            switch ( type )
            {
                case MOB_TRIGGER:
                    script_log ( "It was attached to %s [%d]",
                                 GET_NAME ( ( Character * ) go ), GET_MOB_VNUM ( ( Character * ) go ) );
                    break;
                case OBJ_TRIGGER:
                    script_log ( "It was attached to %s [%d]",
                                 ( ( obj_data * ) go )->short_description, GET_OBJ_VNUM ( ( obj_data * ) go ) );
                    break;
                case WLD_TRIGGER:
                    script_log ( "It was attached to %s [%d]",
                                 ( ( Room * ) go )->name, ( ( Room * ) go )->number );
                    break;
            }


            extract_script ( go, type );
        }

        /*
           extract_script() works on rooms, but on mobiles and objects,
           it will be called again if the
           caller is load_mtrigger or load_otrigger
           if it is one of these, we must make sure the script
           is not just reloaded on the next mob

           We make the calling code decide how to handle it, so it doesn't
           get totally removed unless it's a load_xtrigger().
         */

        return SCRIPT_ERROR_CODE;
    }

    depth++;


    switch ( type )
    {
        case MOB_TRIGGER:
#if DRIVER_USES_UNION

            go = sdd->c;
#else

            go = * ( Character ** ) go_adress;
#endif

            sc = SCRIPT ( ( Character * ) go );
            break;
        case OBJ_TRIGGER:
#if DRIVER_USES_UNION

            go = sdd->o;
#else

            go = * ( obj_data ** ) go_adress;
#endif

            sc = SCRIPT ( ( obj_data * ) go );
            break;
        case WLD_TRIGGER:
#if DRIVER_USES_UNION

            go = sdd->r;
#else

            go = * ( Room ** ) go_adress;
#endif

            sc = SCRIPT ( ( Room * ) go );
            break;
    }

    if ( mode == TRIG_NEW )
    {
        GET_TRIG_DEPTH ( trig ) = 1;
        GET_TRIG_LOOPS ( trig ) = 0;
        trig->curr_state = trig->cmdlist;
        sc->context = 0;
    }

    dg_owner_purged = 0;

    // get breakpoints
    vector<Breakpoint> *bps = nullptr;
    auto debug = script_debug.find ( trig );
    if ( debug != script_debug.end() )
        bps = &debug->second.breakpoints;

    bool first_line_to_execute = TRUE;

    for ( cl = trig->curr_state; cl && GET_TRIG_DEPTH ( trig ); cl = cl ? cl->next : NULL )
    {
        trig->curr_state = cl;
        GET_TRIG_LINE_NR ( trig ) = cl->line_nr;

        if ( !cl || !cl->cmd || !strcmp ( cl->cmd, "" ) )
        {
            script_log ( "Trigger: %s, VNum %d. has no command", GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ) );
            continue;
        }

        if ( bps )
        {
            for ( auto it = bps->begin(); it != bps->end(); ++it )
                if ( it->line_nr == GET_TRIG_LINE_NR ( trig ) )
                {
                    // do we need to break here?
                    if ( !it->stop )
                    {
                        it->stop = TRUE;
                        break;
                    }

                    // tell the debuggers
                    for ( const auto &ch : script_debug[ trig ].chs )
                        ch->Send ( "{cy[DBG]{c0 Trigger %d breaks at line %d: %s\r\n", GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), trig->curr_state->cmd );

                    // put the trigger in a wait state for a long time
                    long when = 1e6 * PASSES_PER_SEC;
                    wait_event_data *wait_event_obj = new wait_event_data ( trig, go, type );
                    GET_TRIG_WAIT ( trig ) = event_create ( trig_wait_event, wait_event_obj, when, EVENT_TYPE_TRIG );
                    depth--;
                    return ret_val;
                }
        }

        // only execute one line if we're stepping
        if ( first_line_to_execute )
            first_line_to_execute = FALSE;
        else if ( debug != script_debug.end() && debug->second.stepping )
        {
            for ( const auto &ch : script_debug[ trig ].chs )
                ch->Send ( "{cy[DBG]{c0 Trigger %d breaks at line %d: %s\r\n", GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ), trig->curr_state->cmd );

            long when = 1e6 * PASSES_PER_SEC;
            wait_event_data *wait_event_obj = new wait_event_data ( trig, go, type );
            GET_TRIG_WAIT ( trig ) = event_create ( trig_wait_event, wait_event_obj, when, EVENT_TYPE_TRIG );
            depth--;
            return ret_val;
        }

        for ( p = cl->cmd; *p && isspace ( *p ); p++ )
            ;

        if ( !*p || *p == '*' )		/* comment */
            continue;

        if ( !strn_cmp ( p, "if ", 3 ) )
        {
            if ( process_if ( p + 3, go, sc, trig, type ) )
                GET_TRIG_DEPTH ( trig ) ++;
            else
                cl = find_else_end ( trig, cl, go, sc, type );
        }
        else if ( !strn_cmp ( "elseif ", p, 7 ) || !strn_cmp ( "else", p, 4 ) || !strn_cmp ( "else if ", p, 8 ) )
        {
            /*
             * if not in an if-block, ignore the extra 'else[if]' and warn about it
             */
            if ( GET_TRIG_DEPTH ( trig ) == 1 )
            {
                script_log ( "Trigger VNum %d has 'else' without 'if' in line %d.", GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ) );
                continue;
            }
            cl = find_end ( trig, cl );
            GET_TRIG_DEPTH ( trig )--;
        }
        else if ( !strn_cmp ( "while ", p, 6 ) )
        {
            temp = find_done ( cl );
            if ( !temp )
            {
                script_log ( "Trigger VNum %d has 'while' without 'done' in line %d.", GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ) );
                reset_trigger ( trig );
                return ret_val;
            }
            if ( process_if ( p + 6, go, sc, trig, type ) )
            {
                temp->original = cl;
            }
            else
            {
                cl = temp;
                loops = 0;
            }
        }
        else if ( !strn_cmp ( "switch ", p, 7 ) )
        {
            cl = find_case ( trig, cl, go, sc, type, p + 7 );
        }
        else if ( !strn_cmp ( "end", p, 3 ) )
        {
            /*
             * if not in an if-block, ignore the extra 'end' and warn about it.
             */
            if ( GET_TRIG_DEPTH ( trig ) == 1 )
            {
                script_log ( "Trigger VNum %d has 'end' without 'if' in line %d.", GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ) );
                reset_trigger ( trig );
                continue;
            }
            GET_TRIG_DEPTH ( trig )--;
        }
        else if ( !strn_cmp ( "done", p, 4 ) )
        {
            /* in a while loop, cl->original is non-null */
            if ( cl->original )
            {
                char *orig_cmd = cl->original->cmd;
                while ( *orig_cmd && isspace ( *orig_cmd ) )
                    orig_cmd++;
                if ( cl->original
                        && process_if ( orig_cmd + 6, go, sc, trig, type ) )
                {
                    cl = cl->original;
                    loops++;
                    GET_TRIG_LOOPS ( trig ) ++;
                    if ( ! ( ( loops+1 ) %30 ) )
                    {
                        process_wait ( go, trig, type, ( char * ) "wait 1", cl );
                        depth--;
                        return ret_val;
                    }
                    if ( GET_TRIG_LOOPS ( trig ) == 100 )
                    {
                        /*script_log("Trigger VNum %d has looped 100 times!!!",
                         GET_TRIG_VNUM(trig)); */

                    }
                    if ( GET_TRIG_LOOPS ( trig ) >= 1000 )
                    {
                        /*script_log("Trigger VNum %d has looped 1000 times!!! Halting Trigger",
                         GET_TRIG_VNUM(trig));*/
                        break;
                    }
                }
                else
                {
                    /* if we are falling through a switch statement, this ends it*/
                }
            }
        }
        else if ( !strn_cmp ( "break", p, 5 ) )
        {
            cl = find_done ( cl );
        }
        else if ( !strn_cmp ( "case", p, 4 ) )
        {
            /* Do nothing, this allows multiple cases to a single instance */
        }
        else
        {

            var_subst(go, sc, trig, type, p, cmd, sizeof(cmd));

            /** dg_script functions **/
            if ( !strn_cmp ( cmd, "function ", 9 ) )
            {
                depth--;
                trig->curr_state = cl->next;
                function_script ( go, sc, trig, type, cmd );
                return ret_val;
            }
            /** end of dg_script functions **/

            if ( !strn_cmp ( cmd, "eval ", 5 ) )
                process_eval ( go, sc, trig, type, cmd );

            else if ( !strn_cmp ( cmd, "nop ", 4 ) )
                ;	/* nop: do nothing */

            else if ( !strn_cmp ( cmd, "extract ", 8 ) )
                extract_value ( sc, trig, cmd );

            else if ( !strn_cmp ( cmd, "dg_letter ", 10 ) )
                dg_letter_value ( sc, trig, cmd );

            else if ( !strn_cmp ( cmd, "insert_word ", 12 ) )
                insert_word ( sc, trig, cmd, FALSE );

            else if ( !strn_cmp ( cmd, "replace_word ", 13 ) )
                insert_word ( sc, trig, cmd, TRUE );

            else if ( !strn_cmp ( cmd, "replace_letter ", 15 ) )
                replace_letter ( trig, cmd );

            else if ( !strn_cmp ( cmd, "makeuid ", 8 ) )
                makeuid_var ( go, sc, trig, type, cmd );

            else if ( !strn_cmp ( cmd, "halt", 4 ) )
                break;

            else if ( !strn_cmp ( cmd, "dg_cast ", 8 ) )
                do_dg_cast ( go, sc, trig, type, cmd );

            else if ( !strn_cmp ( cmd, "dg_affect ", 10 ) )
                do_dg_affect ( go, sc, trig, type, cmd );

            else if ( !strn_cmp ( cmd, "dg_dest ", 8 ) )
                do_dg_destination ( go, sc, trig, type, cmd );

            else if ( !strn_cmp ( cmd, "dg_peek ", 8 ) )
                process_peek ( go, sc, trig, type, cmd );

            else if ( !strn_cmp ( cmd, "global ", 7 ) )
                process_global ( sc, trig, cmd, sc->context );

            else if ( !strn_cmp ( cmd, "context ", 8 ) )
                process_context ( sc, trig, cmd );

            else if ( !strn_cmp ( cmd, "remote ", 7 ) )
                process_remote ( sc, trig, cmd );

            else if ( !strn_cmp ( cmd, "rdelete ", 8 ) )
                process_rdelete ( sc, trig, cmd );

            else if ( !strn_cmp ( cmd, "restring ", 9 ) )
                process_restring ( sc, trig, cmd );

            else if ( !strn_cmp ( cmd, "return ", 7 ) )
                ret_val = process_return ( trig, cmd );

            else if ( !strn_cmp ( cmd, "set ", 4 ) )
                process_set ( sc, trig, cmd );

            else if ( !strn_cmp ( cmd, "unset ", 6 ) )
                process_unset ( sc, trig, cmd );

            else if ( !strn_cmp ( cmd, "wait ", 5 ) )
            {

                process_wait ( go, trig, type, cmd, cl );
                depth--;
                return ret_val;
            }
            else if ( !strn_cmp ( cmd, "attach ", 7 ) )
                process_attach ( go, sc, trig, type, cmd );

            else if ( !strn_cmp ( cmd, "detach ", 7 ) )
                process_detach ( go, sc, trig, type, cmd );

            else if ( !strn_cmp ( cmd, "version", 7 ) )
                mudlog ( DG_SCRIPT_VERSION, NRM, LVL_GOD, TRUE );

            else if ( !strn_cmp ( cmd, "playeruid", 9 ) )
                process_playeruid ( sc, trig, cmd );

            else if ( !strn_cmp ( cmd, "purgemob", 8 ) )
                process_purgemob ( go, type, trig, cmd );

            else if ( !strn_cmp ( cmd, "target", 6 ) )
                process_target ( trig, cmd );

            else if  (!strn_cmp ( cmd, "log", 3 ) )
                process_log ( trig, cmd );

            else
            {
                switch ( type )
                {
                    case MOB_TRIGGER:
                        command_interpreter ( ( Character * ) go, cmd, trig );
                        break;
                    case OBJ_TRIGGER:
                        obj_command_interpreter ( ( obj_data * ) go, cmd );
                        break;
                    case WLD_TRIGGER:
                        wld_command_interpreter ( ( Room * ) go, cmd );
                        break;
                }
                if ( dg_owner_purged ||
#if DRIVER_USES_UNION
                     (type == OBJ_TRIGGER && sdd->o->extracted))
#else
                     (type == OBJ_TRIGGER && (*(obj_data**)go_adress)->extracted))
#endif
                {
                    depth--;

                    if ( type == OBJ_TRIGGER )
#if DRIVER_USES_UNION

                        sdd->o = NULL;
#else

                        * ( obj_data ** ) go_adress = NULL;
#endif

                    return ret_val;

                }
            }

        }
    }

    // remove stepping so the trigger will run normally the next time
    if  (debug != script_debug.end() )
        debug->second.stepping = FALSE;

    depth--;
    if ( sc )
    {
        trig_data *parent = trig->parent;
        if ( parent )
        {
            for ( trig_var_data *vd = GET_TRIG_VARS ( trig ); vd; vd = vd->next )
                add_var ( &GET_TRIG_VARS ( parent ), vd->name, vd->value, vd->context );

            remove_trigger ( go, trig, type ); // remove the function trigger
            script_driver ( &go, parent, type, TRIG_RESTART ); // continue with the parent
        }
        else
        {
            reset_trigger ( trig );

            if ( trig->remove_me )
            {
                // the trigger was removed in OLC while it was running, or detached: remove it now
                remove_trigger ( go, trig, type );
            }
            else if ( trig->update_me )
            {
                // the trigger was updated in trigedit while it was running: update it now
                trig_rnum rnum = trig->nr;
                free_cmdlist ( trig );

                if ( remove_trigger ( go, trig, type ) == 2 )
                {
                    // the script was removed as well, add a new one
                    switch ( type )
                    {
                        case MOB_TRIGGER:
                            SCRIPT ( ( Character * ) go ) = create_script();
                            sc = SCRIPT ( ( Character * ) go );
                            break;
                        case OBJ_TRIGGER:
                            SCRIPT ( ( obj_data * ) go ) = create_script();
                            sc = SCRIPT ( ( obj_data * ) go );
                            break;
                        case WLD_TRIGGER:
                            SCRIPT ( ( Room * ) go ) = create_script();
                            sc = SCRIPT ( ( Room * ) go );
                            break;
                    }
                }
                add_trigger ( sc, read_trigger ( rnum ), -1 );
            }
        }
    }

    return ret_val;
}

ACMD ( do_tlist )
{

    int bottom, top;
    unsigned int i;
    int counter = 0;
    char trgtypes[256];
    char buf[MAX_INPUT_LENGTH];

    char buf2[MAX_INPUT_LENGTH];
    DYN_DEFINE;
    *buf = 0;

    two_arguments ( argument, buf, buf2 );

    if ( !*buf )
    {
        ch->Send ( "Usage: tlist <begining number or zone> [<ending number>]\r\n" );
        return;
    }

    bottom = atoi ( buf );
    if ( *buf2 )
        top = atoi ( buf2 );
    else
    {
        bottom *= 100;
        top = bottom + 99;
    }

    if ( ( bottom < 0 ) || ( bottom > 999999 ) || ( top < 0 ) || ( top > 999999 ) )
    {
        ch->Send ( "Values must be between 0 and 999999.\n\r" );
        return;
    }

    if ( bottom >= top )
    {
        send_to_char ( "Second value must be greater than first.\n\r", ch );
        return;
    }
    DYN_CREATE;
    *dynbuf = 0;
    /** Store the header for the room listing. **/
    ch->Send (
        "Index VNum    Trigger Name                        Type\r\n"
        "----- ------- -------------------------------------------------------\r\n" );
    /** Loop through the world and find each room. **/
    for ( i = 0; i < top_of_trigt; i++ )
    {
        /** Check to see if this room is one of the ones needed to be listed.    **/
        if ( ( trig_index[i]->vnum >= bottom ) && ( trig_index[i]->vnum <= top ) )
        {
            counter++;

            sprintf ( buf, "%4d) [%s%5d%s] %s%-35.35s ",
                      counter, QGRN, trig_index[i]->vnum, QNRM, QCYN, trig_index[i]->proto->name );
            DYN_RESIZE ( buf );
            if ( trig_index[i]->proto->attach_type == OBJ_TRIGGER )
            {
                new_sprintbit ( GET_TRIG_TYPE ( trig_index[i]->proto ), otrig_types, trgtypes, sizeof ( trgtypes ) );
                sprintf ( buf, "obj %s%s%s\r\n", QYEL, trgtypes, QNRM );
            }
            else if ( trig_index[i]->proto->attach_type==WLD_TRIGGER )
            {
                new_sprintbit ( GET_TRIG_TYPE ( trig_index[i]->proto ), wtrig_types, trgtypes, sizeof ( trgtypes ) );
                sprintf ( buf, "wld %s%s%s\r\n", QYEL, trgtypes, QNRM );
            }
            else
            {
                new_sprintbit ( GET_TRIG_TYPE ( trig_index[i]->proto ), mtrig_types, trgtypes, sizeof ( trgtypes ) );
                snprintf ( buf, sizeof ( buf ), "mob %s%s%s\r\n", QYEL, trgtypes, QNRM );
            }
            DYN_RESIZE ( buf );

        }
    }

    if ( !counter )
        send_to_char ( "No triggers were found in those parameters.\n\r",
                       ch );
    else
        page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

/* returns the real number of the trigger with given virtual number */
trig_rnum real_trigger ( int vnum )
{
    if ( vnum == NOTHING )
        return NOTHING;

    int bot = 0, mid;
    int top = top_of_trigt - 1;

    for (;;)
    {
        mid = (bot + top) / 2;
        // Thanks to Derek Fisk for fixing this loop
        if ( trig_index[ mid ]->vnum > vnum )
        {
            top = mid - 1;
            if ( top <= 0 )
                return NOTHING;
        }
        else if ( trig_index[ mid ]->vnum == vnum )
            return mid;
        else
        {
            bot = mid + 1;
            if ( bot > top )
                return NOTHING;
        }
    }
}

ACMD ( do_tstat )
{
    int  rnum;
    char str[MAX_INPUT_LENGTH];

    half_chop ( argument, str, argument );
    if ( *str )
    {
        rnum = real_trigger ( atoi ( str ) );
        if ( rnum < 0 )
        {
            send_to_char ( "That vnum does not exist.\r\n", ch );
            return;
        }
        new_mudlog ( NRM, ( GET_LEVEL ( ch ) == LVL_IMPL ? GET_LEVEL ( ch ) : GET_LEVEL ( ch ) +1 ), TRUE, "(GC) %s statted trigger %d.", GET_NAME ( ch ),  atoi ( str ) );
        do_stat_trigger ( ch, trig_index[rnum]->proto );
    }
    else
        send_to_char ( "Usage: tstat <vnum>\r\n", ch );
}

// Show the breakpoints of a trigger debugged by ch to ch
void dbg_list_breakpoints ( Character *ch, const pair<trig_data *, TrigDebug> &debug )
{
    if ( debug.second.breakpoints.size() == 0 )
    {
        ch->Send ( "  Breakpoints: none\r\n" );
        return;
    }

    char buf[MAX_INPUT_LENGTH];
    DYN_DEFINE;
    DYN_CREATE;
    int bp_i = 0;
    cmdlist_element *cl = debug.first->cmdlist;
    for ( ; bp_i < debug.second.breakpoints.size() && cl; cl = cl->next )
        if ( cl->line_nr == debug.second.breakpoints[ bp_i ].line_nr )
        {
            snprintf ( buf, sizeof ( buf ), "    Line %d: %s\r\n", cl->line_nr, cl->cmd );
            DYN_RESIZE ( buf );
            bp_i++;
        }

    ch->Send ( "  Breakpoints:\r\n" );
    page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

// Show the variables of a trigger debugged by ch to ch
void dbg_list_variables ( Character *ch, const pair<trig_data *, TrigDebug> &debug )
{
    DYN_DEFINE;
    DYN_CREATE;
    char buf[MAX_INPUT_LENGTH];

    trig_var_data *vd = GET_TRIG_VARS ( debug.first );
    if ( !vd )
        ch->Send ( "  Local variables: none\r\n" );
    else
    {
        ch->Send ( "  Local variables:\r\n" );
        for ( ; vd; vd = vd->next )
        {
            if ( vd->context == 0 )
                snprintf ( buf, sizeof ( buf ), "    %s = %s\r\n", vd->name.c_str(), vd->value.c_str() );
            else
                snprintf ( buf, sizeof ( buf ), "    %s = %s, context: %ld\r\n",
                    vd->name.c_str(), vd->value.c_str(), vd->context );
            DYN_RESIZE ( buf );
        }
    }

    switch ( debug.second.type )
    {
        case MOB_TRIGGER:
            vd = SCRIPT ( (Character*) debug.second.thing )->global_vars;
            break;
        case OBJ_TRIGGER:
            vd = SCRIPT ( (obj_data*) debug.second.thing )->global_vars;
            break;
        case WLD_TRIGGER:
            vd = SCRIPT ( (Room*) debug.second.thing )->global_vars;
            break;
        default:
        {
            log ( "SYSERR: script_debug entry has unknown type %d", debug.second.type );
            return;
        }
    }

    if ( !vd )
    {
        snprintf ( buf, sizeof ( buf ), "%s\r\n", "  Global variables: none" );
        DYN_RESIZE ( buf );
    }
    else
    {
        snprintf ( buf, sizeof ( buf ), "%s\r\n", "  Global variables:" );
        DYN_RESIZE ( buf );
        for ( ; vd; vd = vd->next )
        {
            if ( vd->context == 0 )
                snprintf ( buf, sizeof ( buf ), "    %s = %s\r\n", vd->name.c_str(), vd->value.c_str() );
            else
                snprintf ( buf, sizeof ( buf ), "    %s = %s, context: %ld\r\n", vd->name.c_str(), vd->value.c_str(), vd->context );
            DYN_RESIZE ( buf );
        }
    }
    page_string ( ch->desc, dynbuf, DYN_BUFFER );
}

// Return the number of triggers ch is debugging
int dbg_trigger_count ( Character *ch )
{
    int i = 0;
    for ( const auto &debug : script_debug )
        if ( find ( debug.second.chs.begin(), debug.second.chs.end(), ch ) != debug.second.chs.end() )
            i++;
    return i;
}

// Return the line at line_nr
char *dbg_line ( trig_data *trig, int line_nr )
{
    for ( auto cl = trig->cmdlist; cl; cl = cl->next )
        if ( cl->line_nr == line_nr )
            return cl->cmd;
    return (char*) "";
}

void resume_trigger ( trig_data *trig, void *thing, int type )
{
    // delete the old wait and add a new wait with duration zero
    event_cancel ( GET_TRIG_WAIT ( trig ) );
    auto wait_event_obj = new wait_event_data ( trig, thing, type );
    GET_TRIG_WAIT ( trig ) = event_create ( trig_wait_event, wait_event_obj, 0, EVENT_TYPE_TRIG );
}

ACMD ( do_dbg )
{
    Character *mob = nullptr;
    obj_data *obj = nullptr;
    Room *room = nullptr;
    trig_data *trig = nullptr;
    char buf[MAX_INPUT_LENGTH];
    DYN_DEFINE;
    string s = string ( argument );
    stringstream ss ( s );
    string arg;
    vector<string> args;

    while ( ss >> arg )
        args.push_back ( arg );

    if ( args.size() == 0 )
    {
        ch->Send ( "Usage: see help debug.\r\n" );
        return;
    }
    else if ( is_abbrev3 ( args[0], "mob" ) )
    {
        if ( args.size() < 3 || !is_number ( args[2].c_str() ) )
        {
            ch->Send ( "Usage: dbg mob <name> <trig_vnum>\r\n" );
            return;
        }

        generic_find ( (char*) args[1].c_str(), FIND_CHAR_ROOM, ch, &mob, &obj );
        if ( !mob )
        {
            ch->Send ( "There's no mob named %s here.\r\n", args[1].c_str() );
            return;
        }

        trig_vnum vnum = stoi ( args[2] );
        if ( real_trigger ( vnum ) < 0 )
        {
            ch->Send ( "Trigger %d doesn't exist.\r\n", vnum );
            return;
        }

        if ( SCRIPT ( mob ) )
            for ( trig = TRIGGERS ( SCRIPT ( mob ) ); trig; trig = trig->next )
                if ( GET_TRIG_VNUM ( trig ) == vnum )
                    break;

        if ( trig == nullptr )
        {
            ch->Send ( "That mob doesn't have trigger %d.\r\n", vnum );
            return;
        }

        auto debug = script_debug.find ( trig );
        if ( debug != script_debug.end() )
        {
            if ( find ( debug->second.chs.begin(), debug->second.chs.end(), ch ) != debug->second.chs.end() )
            {
                ch->Send ( "You are already debugging this trigger.\r\n" );
                return;
            }

            for ( const auto &Ch : debug->second.chs )
                Ch->Send ( "{cy[DBG]{c0 %s starts debugging trigger %d.\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ) );
            debug->second.chs.push_back ( ch );
        }
        else
            script_debug[ trig ] = TrigDebug ( mob, MOB_TRIGGER, vector<Character*> { ch }, vector<Breakpoint> {} );

        ch->Send ( "{cy[DBG]{c0 You start debugging trigger %d on %s.\r\n", vnum, GET_NAME ( mob ) );
    }
    else if ( is_abbrev3 ( args[0], "obj" ) )
    {
        if ( args.size() < 3 || !is_number ( args[2].c_str() ) )
        {
            ch->Send ( "Usage: dbg obj <name> <trig_vnum>\r\n" );
            return;
        }

        generic_find ( (char*) args[1].c_str(), FIND_OBJ_INV | FIND_OBJ_ROOM | FIND_OBJ_EQUIP, ch, &mob, &obj );
        if ( !obj )
        {
            ch->Send ( "There's no object named %s here.\r\n", args[1].c_str() );
            return;
        }

        trig_vnum vnum = stoi ( args[2] );
        if ( real_trigger ( vnum ) < 0 )
        {
            ch->Send ( "Trigger %d doesn't exist.\r\n", vnum );
            return;
        }

        if ( SCRIPT ( obj ) )
            for ( trig = TRIGGERS ( SCRIPT ( obj ) ); trig; trig = trig->next )
                if ( GET_TRIG_VNUM ( trig ) == vnum )
                    break;

        if ( trig == nullptr )
        {
            ch->Send ( "That object doesn't have trigger %d.\r\n", vnum );
            return;
        }

        auto debug = script_debug.find ( trig );
        if ( debug != script_debug.end() )
        {
            if ( find ( debug->second.chs.begin(), debug->second.chs.end(), ch ) != debug->second.chs.end() )
            {
                ch->Send ( "You are already debugging this trigger.\r\n" );
                return;
            }

            for ( const auto &Ch : debug->second.chs )
                Ch->Send ( "{cy[DBG]{c0 %s starts debugging trigger %d.\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ) );
            debug->second.chs.push_back ( ch );
        }
        else
            script_debug[ trig ] = TrigDebug ( obj, OBJ_TRIGGER, vector<Character*> { ch }, vector<Breakpoint> {} );

        ch->Send ( "{cy[DBG]{c0 You start debugging trigger %d on %s.\r\n", vnum, obj->short_description );
    }
    else if ( is_abbrev3 ( args[0], "room" ) )
    {
        if ( args.size() < 2 || !is_number ( args[1].c_str() ) )
        {
            ch->Send ( "Usage: dbg room <trig_vnum>\r\n" );
            return;
        }


        trig_vnum vnum = stoi ( args[1] );
        if ( real_trigger ( vnum ) < 0 )
        {
            ch->Send ( "Trigger %d doesn't exist.\r\n", vnum );
            return;
        }

        room = IN_ROOM ( ch );
        if ( SCRIPT ( room ) )
            for ( trig = TRIGGERS ( SCRIPT ( room ) ); trig; trig = trig->next )
                if ( GET_TRIG_VNUM ( trig ) == vnum )
                    break;

        if ( trig == nullptr )
        {
            ch->Send ( "The room doesn't have trigger %d.\r\n", vnum );
            return;
        }

        auto debug = script_debug.find ( trig );
        if ( debug != script_debug.end() )
        {
            if ( find ( debug->second.chs.begin(), debug->second.chs.end(), ch ) != debug->second.chs.end() )
            {
                ch->Send ( "You are already debugging this trigger.\r\n" );
                return;
            }

            for ( const auto &Ch : debug->second.chs )
                Ch->Send ( "{cy[DBG]{c0 %s starts debugging trigger %d.\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ) );
            debug->second.chs.push_back ( ch );
        }
        else
            script_debug[ trig ] = TrigDebug ( room, WLD_TRIGGER, vector<Character*> { ch }, vector<Breakpoint> {} );

        ch->Send ( "{cy[DBG]{c0 You start debugging trigger %d on %s.\r\n", vnum, room->name );
    }
    else if ( is_abbrev3 ( args[0], "info" ) )
    {
        /* dbg info: show general info about which triggers are being debugged and on what
           dbg info <num>: show more detailed info on a specific trigger
           If only debugging one trigger then 'dbg info' does 'dbg info 1'.
        */

        int trigger_count = dbg_trigger_count ( ch ), num = 0, k = 1, i = 0;
        if ( trigger_count == 0 )
        {
            ch->Send ( "You're currently not debugging any triggers.\r\n" );
            return;
        }

        bool show_breakpoints = TRUE, show_variables = TRUE;
        if ( args.size() > 1 )
        {
            if ( is_number ( args[1].c_str() ) )
            {
                num = stoi ( args[1] );
                k = 0;
            }
            else if ( trigger_count > 1 )
            {
                ch->Send ( "Usage: dbg info [<num>] [breakpoints|variables]\r\n" );
                return;
            }

            if ( args.size() > 2-k )
            {
                if ( is_abbrev3 ( args[2-k], "breakpoints" ) )
                    show_variables = FALSE;
                else if ( is_abbrev3 ( args[2-k], "variables" ) )
                    show_breakpoints = FALSE;
                else
                {
                    ch->Send ( "Usage: dbg info [<num>] [breakpoints|variables]\r\n" );
                    return;
                }
            }
        }

        if ( args.size() == 1 && trigger_count > 1 )
            DYN_CREATE;

        for ( const auto &debug : script_debug )
        {
            if ( find ( debug.second.chs.begin(), debug.second.chs.end(), ch ) == debug.second.chs.end() )
                continue;

            i++;
            if ( num > 0 )
            {
                if ( num > i )
                    continue;
                if ( num < i )
                    break;
            }

            switch ( debug.second.type )
            {
                case MOB_TRIGGER:
                    mob = (Character*) debug.second.thing;
                    if ( args.size() > 1 || trigger_count == 1 )
                    {
                        ch->Send ( "{cy[DBG]{c0 Info about %d. Trigger [%d] on mob [%ld] %s\r\n",
                            i, GET_TRIG_VNUM ( debug.first ), GET_ID ( mob ), GET_NAME ( mob ) );

                        if ( GET_TRIG_WAIT ( debug.first ) )
                            ch->Send ( "  Wait %ld, line %d: %s\r\n", event_time ( GET_TRIG_WAIT ( debug.first ) ), GET_TRIG_LINE_NR ( debug.first ), debug.first->curr_state->cmd );
                        else
                            ch->Send ( "  Not running.\r\n" );

                        if ( show_breakpoints )
                            dbg_list_breakpoints ( ch, debug );

                        if ( show_variables )
                            dbg_list_variables ( ch, debug );
                    }
                    else
                    {
                        snprintf ( buf, sizeof ( buf ), "%d. [%d] on mob [%ld] %s\r\n",
                            i, GET_TRIG_VNUM ( debug.first ), GET_ID ( mob ), GET_NAME ( mob ) );
                        DYN_RESIZE ( buf );
                    }
                    break;
                case OBJ_TRIGGER:
                    obj = (obj_data*) debug.second.thing;
                    if ( args.size() > 1 || trigger_count == 1 )
                    {
                        ch->Send ( "{cy[DBG]{c0 Info about %d. Trigger [%d] on obj [%ld] %s\r\n",
                            i, GET_TRIG_VNUM ( debug.first ), GET_ID ( obj ), obj->short_description );

                        if ( GET_TRIG_WAIT ( debug.first ) )
                            ch->Send ( "  Wait %ld, line %d: %s\r\n", event_time ( GET_TRIG_WAIT ( debug.first ) ), GET_TRIG_LINE_NR ( debug.first ), debug.first->curr_state->cmd );
                        else
                            ch->Send ( "  Not running.\r\n" );

                        if ( show_breakpoints )
                            dbg_list_breakpoints ( ch, debug );

                        if ( show_variables )
                            dbg_list_variables ( ch, debug );
                    }
                    else
                    {
                        snprintf ( buf, sizeof ( buf ), "%d. [%d] on obj [%ld] %s\r\n",
                            i, GET_TRIG_VNUM ( debug.first ), GET_ID ( obj ), obj->short_description );
                        DYN_RESIZE ( buf );
                    }
                    break;
                case WLD_TRIGGER:
                    room = (Room*) debug.second.thing;
                    if ( args.size() > 1 || trigger_count == 1 )
                    {
                        ch->Send ( "{cy[DBG]{c0 Info about %d. Trigger [%d] on room [%d] %s\r\n",
                            i, GET_TRIG_VNUM ( debug.first ), GET_ROOM_VNUM ( room ), room->name );

                        if ( GET_TRIG_WAIT ( debug.first ) )
                            ch->Send ( "  Wait %ld, line %d: %s\r\n", event_time ( GET_TRIG_WAIT ( debug.first ) ), GET_TRIG_LINE_NR ( debug.first ), debug.first->curr_state->cmd );
                        else
                            ch->Send ( "  Not running.\r\n" );

                        if ( show_breakpoints )
                            dbg_list_breakpoints ( ch, debug );

                        if ( show_variables )
                            dbg_list_variables ( ch, debug );
                    }
                    else
                    {
                        snprintf ( buf, sizeof ( buf ), "%d. [%d] on room [%d] %s\r\n",
                            i, GET_TRIG_VNUM ( debug.first ), GET_ROOM_VNUM ( room ), room->name );
                        DYN_RESIZE ( buf );
                    }
                    break;
                default:
                {
                    log ( "SYSERR: script_debug entry has unknown type %d", debug.second.type );
                    return;
                }
            }
        }

        if ( num > i )
            ch->Send ( "You're only debugging %d trigger%s.\r\n", trigger_count, trigger_count > 1 ? "s" : "" );
        else if ( args.size() == 1 && trigger_count > 1 )
        {
            ch->Send ( "{cy[DBG]{c0 You are debugging the following triggers:\r\n" );
            page_string ( ch->desc, dynbuf, DYN_BUFFER );
        }
    }
    else if ( is_abbrev3 ( args[0], "continue" ) )
    {
        int trigger_count = dbg_trigger_count ( ch ), num = 1, i = 0;
        if ( trigger_count == 0 )
        {
            ch->Send ( "You're not debugging any triggers.\r\n" );
            return;
        }
        else if ( trigger_count > 1 )
        {
            if ( args.size() == 1 || !is_number ( args[1].c_str() ) )
            {
                ch->Send ( "Usage: dbg continue [<num>]\r\n" );
                return;
            }
            num = stoi ( args[1] );
        }

        for ( auto &debug : script_debug )
        {
            if ( find ( debug.second.chs.begin(), debug.second.chs.end(), ch ) == debug.second.chs.end() )
                continue;

            i++;
            if ( i < num )
                continue;

            trig = debug.first;
            if ( !trig->curr_state )
            {
                ch->Send ( "Trigger %d isn't running.\r\n", GET_TRIG_VNUM ( trig ) );
                return;
            }

            // if there's a breakpoint here, turn it off so it won't break
            debug.second.stepping = FALSE;
            vector<Breakpoint> &bps = debug.second.breakpoints;
            for ( auto it = bps.begin(); it != bps.end(); ++it )
                if ( it->line_nr == GET_TRIG_LINE_NR ( trig ) )
                {
                    it->stop = FALSE;
                    break;
                }

            resume_trigger ( trig, debug.second.thing, debug.second.type );

            for ( const auto &Ch : debug.second.chs )
                if ( Ch == ch )
                    Ch->Send ( "{cy[DBG]{c0 Continuing trigger %d.\r\n", GET_TRIG_VNUM ( trig ) );
                else
                    Ch->Send ( "{cy[DBG]{c0 [%s] Trigger %d: continue.\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ) );
            return;
        }

        ch->Send ( "You're only debugging %d trigger%s.\r\n", trigger_count, trigger_count > 1 ? "s" : "" );
    }
    else if ( is_abbrev3 ( args[0], "next" ) )
    {
        int trigger_count = dbg_trigger_count ( ch ), num = 1, i = 0;
        if ( trigger_count == 0 )
        {
            ch->Send ( "You're not debugging any triggers.\r\n" );
            return;
        }
        else if ( trigger_count > 1 )
        {
            if ( args.size() == 1 || !is_number ( args[1].c_str() ) )
            {
                ch->Send ( "Usage: dbg next [<num>]\r\n" );
                return;
            }
            num = stoi ( args[1] );
        }

        for ( auto &debug : script_debug )
        {
            if ( find ( debug.second.chs.begin(), debug.second.chs.end(), ch ) == debug.second.chs.end() )
                continue;

            i++;
            if ( i < num )
                continue;

            trig = debug.first;
            if ( !trig->curr_state )
            {
                ch->Send ( "Trigger %d isn't running.\r\n", GET_TRIG_VNUM ( trig ) );
                return;
            }

            // if there's a breakpoint here, turn it off so it won't break
            debug.second.stepping = TRUE;
            vector<Breakpoint> &bps = debug.second.breakpoints;
            for ( auto it = bps.begin(); it != bps.end(); ++it )
                if ( it->line_nr == GET_TRIG_LINE_NR ( trig ) )
                {
                    it->stop = FALSE;
                    break;
                }

            resume_trigger ( trig, debug.second.thing, debug.second.type );

            for ( const auto &Ch : debug.second.chs )
                if ( Ch != ch )
                    Ch->Send ( "{cy[DBG]{c0 [%s] Trigger %d: next.\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ) );
            return;
        }

        ch->Send ( "You're only debugging %d trigger%s.\r\n", trigger_count, trigger_count > 1 ? "s" : "" );
    }
    else if ( is_abbrev3 ( args[0], "print" ) )
    {
        if ( args.size() == 1 )
        {
            ch->Send ( "Usage: dbg print [<num>] <expr>\r\n" );
            return;
        }

        int trigger_count = dbg_trigger_count ( ch ), num = 1, i = 0;
        if ( trigger_count == 0 )
        {
            ch->Send ( "You're not debugging any triggers.\r\n" );
            return;
        }
        else if ( trigger_count > 1 )
        {
            if ( !is_number ( args[1].c_str() ) )
            {
                ch->Send ( "Usage: dbg print <num> <expr>\r\n" );
                return;
            }
        }

        if ( is_number ( args[1].c_str() ) )
            num = stoi ( args[1] );

        string expr = args.back();

        for ( auto &debug : script_debug )
        {
            if ( find ( debug.second.chs.begin(), debug.second.chs.end(), ch ) == debug.second.chs.end() )
                continue;

            i++;
            if ( i < num )
                continue;

            trig = debug.first;
            void *thing = debug.second.thing;
            script_data *sc = nullptr;
            switch ( debug.second.type )
            {
                case MOB_TRIGGER:
                    sc = SCRIPT ( (Character*) thing );
                    break;
                case OBJ_TRIGGER:
                    sc = SCRIPT ( (obj_data*) thing );
                    break;
                case WLD_TRIGGER:
                    sc = SCRIPT ( (Room*) thing );
                    break;
                default:
                {
                    log ( "SYSERR: script_debug entry has unknown type %d", debug.second.type );
                    return;
                }
            }
            char result[MAX_INPUT_LENGTH];
            var_subst ( thing, sc, trig, debug.second.type, (char*) expr.c_str(), result, sizeof result );
            ch->Send ( "{cy[DBG]{c0 %s = %s\r\n", expr.c_str(), result );
            return;
        }
    }
    else if ( is_abbrev3 ( args[0], "set" ) )
    {
        /* dbg set [<num>] breakpoint <line_nr>: set a breakpoint at line line_nr
           dbg set [<num>] variable <name> <value>: set variable name = value
           If only debugging one trigger then 'dbg set' does 'dbg set 1'.
        */

        if ( args.size() < 3 )
        {
            ch->Send ( "Usage: dbg set [<num>] breakpoint <line_nr>\r\n"
                       "       dbg set [<num>] variable <name> <value>\r\n" );
            return;
        }

        int trigger_count = dbg_trigger_count ( ch ), num = 1, k = 1, i = 0;
        if ( trigger_count == 0 )
        {
            ch->Send ( "You're not debugging any triggers.\r\n" );
            return;
        }
        else if ( trigger_count > 1 && !is_number ( args[1].c_str() ) )
        {
            ch->Send ( "Usage: dbg set [<num>] breakpoint <line_nr>\r\n"
                       "       dbg set [<num>] variable <name> <value>\r\n" );
            return;
        }

        if ( is_number ( args[1].c_str() ) )
        {
            num = stoi ( args[1] );
            k = 0;
        }

        for ( auto &debug : script_debug )
        {
            if ( find ( debug.second.chs.begin(), debug.second.chs.end(), ch ) == debug.second.chs.end() )
                continue;

            i++;
            if ( i < num )
                continue;

            if ( is_abbrev3 ( args[2-k], "breakpoint" ) )
            {
                if ( args.size() < 4-k || !is_number ( args[3-k].c_str() ) )
                {
                    ch->Send ( "Usage: dbg set [<num>] breakpoint <line_nr>\r\n" );
                    return;
                }

                int line_nr = stoi ( args[3-k] );
                vector<Breakpoint> &bps = debug.second.breakpoints;
                trig = debug.first;
                for ( auto it = bps.begin(); it != bps.end(); ++it )
                    if ( it->line_nr == line_nr )
                    {
                        // breakpoint already exists, remove it
                        bps.erase ( it );
                        char *line = dbg_line ( trig, line_nr );
                        for ( const auto &Ch : debug.second.chs )
                            if ( Ch == ch )
                                Ch->Send ( "{cy[DBG]{c0 Trigger %d: removed breakpoint at line %d: %s\r\n", GET_TRIG_VNUM ( trig ), line_nr, line );
                            else
                                Ch->Send ( "{cy[DBG]{c0 [%s] Trigger %d: removed breakpoint at line %d: %s\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ), line_nr, line );
                        return;
                    }

                // add breakpoint at line_nr
                bps.push_back ( Breakpoint ( line_nr ) );
                sort ( bps.begin(), bps.end(), []( Breakpoint &a, Breakpoint &b ) {
                        return a.line_nr < b.line_nr; });
                char *line = dbg_line ( trig, line_nr );
                for ( const auto &Ch : debug.second.chs )
                    if ( Ch == ch )
                        Ch->Send ( "{cy[DBG]{c0 Trigger %d: breakpoint set at line %d: %s\r\n", GET_TRIG_VNUM ( trig ), line_nr, line );
                    else
                        Ch->Send ( "{cy[DBG]{c0 [%s] Trigger %d: breakpoint set at line %d: %s\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ), line_nr, line );
            }
            else if ( is_abbrev3 ( args[2-k], "variable" ) )
            {
                if ( args.size() < 4-k )
                {
                    ch->Send ( "Usage: dbg set [<num>] variable <name> <value>\r\n" );
                    return;
                }

                string var = args[3-k];
                trig = debug.first;
                trig_var_data *vd = GET_TRIG_VARS ( trig );

                // check local variables
                for ( ; vd; vd = vd->next )
                    if ( var == vd->name )
                    {
                        string value;
                        if ( args.size() > 4-k )
                        {
                            value = args[4-k];
                            for ( int i = 5-k; i < args.size(); ++i )
                                value += " " + args[i];
                        }
                        vd->value = value;
                        for ( const auto &Ch : debug.second.chs )
                            if ( Ch == ch )
                                Ch->Send ( "{cy[DBG]{c0 Trigger %d: set var %s = %s\r\n", GET_TRIG_VNUM ( trig ), var.c_str(), value.c_str() );
                            else
                                Ch->Send ( "{cy[DBG]{c0 [%s] Trigger %d: set var %s = %s\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ), var.c_str(), value.c_str() );
                        return;
                    }

                // check global variables
                switch ( debug.second.type )
                {
                    case MOB_TRIGGER:
                        vd = SCRIPT ( (Character*) debug.second.thing )->global_vars;
                        break;
                    case OBJ_TRIGGER:
                        vd = SCRIPT ( (obj_data*) debug.second.thing )->global_vars;
                        break;
                    case WLD_TRIGGER:
                        vd = SCRIPT ( (Room*) debug.second.thing )->global_vars;
                        break;
                    default:
                    {
                        log ( "SYSERR: script_debug entry has unknown type %d", debug.second.type );
                        return;
                    }
                }

                for ( ; vd; vd = vd->next )
                    if ( var == vd->name )
                    {
                        string value;
                        if ( args.size() > 4-k )
                        {
                            value = args[4-k];
                            for ( int i = 5-k; i < args.size(); ++i )
                                value += " " + args[i];
                        }
                        vd->value = value;
                        for ( const auto &Ch : debug.second.chs )
                            if ( Ch == ch )
                                Ch->Send ( "{cy[DBG]{c0 Trigger %d: set var %s = %s\r\n", GET_TRIG_VNUM ( trig ), var.c_str(), value.c_str() );
                            else
                                Ch->Send ( "{cy[DBG]{c0 [%s] Trigger %d: set var %s = %s\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ), var.c_str(), value.c_str() );
                        return;
                    }

                ch->Send ( "There's no variable called %s.\r\n", var.c_str() );
            }
            else
                ch->Send ( "Usage: dbg set [<num>] breakpoint <line_nr>\r\n"
                           "       dbg set [<num>] variable <name> <value>\r\n" );
            return;
        }

        ch->Send ( "You're only debugging %d trigger%s.\r\n", trigger_count, trigger_count > 1 ? "s" : "" );
    }
    else if ( is_abbrev3 ( args[0], "list" ) )
    {
        int trigger_count = dbg_trigger_count ( ch ), num = 1, i = 0;
        if ( trigger_count == 0 )
        {
            ch->Send ( "You're not debugging any triggers.\r\n" );
            return;
        }
        else if ( trigger_count > 1 )
        {
            if ( args.size() == 1 || !is_number ( args[1].c_str() ) )
            {
                ch->Send ( "Usage: dbg list [<num>]\r\n" );
                return;
            }
            num = stoi ( args[1] );
        }

        for ( const auto &debug : script_debug )
        {
            if ( find ( debug.second.chs.begin(), debug.second.chs.end(), ch ) != debug.second.chs.end() )
            {
                i++;
                if ( i < num )
                    continue;

                ch->Send ( "{cy[DBG]{c0 Trigger %d:\r\n", GET_TRIG_VNUM ( debug.first ) );
                DYN_CREATE;
                for ( auto cl = debug.first->cmdlist; cl; cl = cl->next )
                {
                    snprintf ( buf, sizeof ( buf ), "  %d. %s\r\n", cl->line_nr, cl->cmd );
                    DYN_RESIZE ( buf );
                }
                page_string ( ch->desc, dynbuf, DYN_BUFFER );
                return;
            }
        }

        ch->Send ( "You're only debugging %d trigger%s.\r\n", trigger_count, trigger_count > 1 ? "s" : "" );
    }
    else if ( is_abbrev3 ( args[0], "delete" ) )
    {
        int trigger_count = dbg_trigger_count ( ch ), num = 1, i = 0;
        if ( trigger_count == 0 )
        {
            ch->Send ( "You're not debugging any triggers.\r\n" );
            return;
        }
        else if ( trigger_count > 1 )
        {
            if ( args.size() == 1 || !is_number ( args[1].c_str() ) )
            {
                ch->Send ( "Usage: dbg delete [<num>]\r\n" );
                return;
            }
            num = stoi ( args[1] );
        }

        for ( auto it = script_debug.begin(); it != script_debug.end(); it++ )
        {
            auto it_debugger = find ( it->second.chs.begin(), it->second.chs.end(), ch );
            if ( it_debugger == it->second.chs.end() )
                continue;

            i++;
            if ( i < num )
                continue;

            auto trig = it->first;
            for ( const auto &Ch : it->second.chs )
                if ( Ch == ch )
                    Ch->Send ( "{cy[DBG]{c0 You stop debugging %d. Trigger %d.\r\n", num, GET_TRIG_VNUM ( trig ) );
                else
                    Ch->Send ( "{cy[DBG]{c0 %s stops debugging trigger %d.\r\n", GET_NAME ( ch ), GET_TRIG_VNUM ( trig ) );

            // resume the trigger if it's at a breakpoint and there's only one debugger
            if ( GET_TRIG_WAIT ( trig ) && it->second.chs.size() == 1 )
            {
                if ( it->second.stepping )
                    resume_trigger ( trig, it->second.thing, it->second.type );
                else
                {
                    auto &bps = it->second.breakpoints;
                    for ( auto it_bp = bps.begin(); it_bp != bps.end(); it_bp++ )
                        if ( it_bp->line_nr == GET_TRIG_LINE_NR ( trig ) )
                        {
                            resume_trigger ( trig, it->second.thing, it->second.type );
                            break;
                        }
                }
            }

            if ( it->second.chs.size() > 1 )
                // remove the debugger
                it->second.chs.erase ( it_debugger );
            else
                // stop debugging the trigger
                script_debug.erase ( it );
            return;
        }

        if ( num != i )
            ch->Send ( "You're only debugging %d trigger%s.\r\n", trigger_count, trigger_count > 1 ? "s" : "" );
    }
    else
        ch->Send ( "Usage: see help debug.\r\n" );
}

/*
* scans for a case/default instance
* returns the line containg the correct case instance, or the last
* line of the trigger if not found.
* Malformed scripts may cause NULL to be returned.
*/
struct cmdlist_element *find_case ( struct trig_data *trig,
                                                struct cmdlist_element *cl, void *go,
                                                struct script_data *sc, int type,
                                                char *cond )
{
    char result[MAX_INPUT_LENGTH];
    struct cmdlist_element *c;
    char *p, *buf;

    eval_expr ( cond, result, sizeof ( result ), go, sc, trig, type );

    if ( ! ( cl->next ) )
        return cl;

    for ( c = cl->next; c->next; c = c->next )
    {
        for ( p = c->cmd; *p && isspace ( *p ); p++ )
            ;

        if ( !strn_cmp ( "while ", p, 6 ) || !strn_cmp ( "switch", p, 6 ) )
            c = find_done ( c );
        else if ( !strn_cmp ( "case ", p, 5 ) )
        {
            buf = ( char * ) malloc ( MAX_STRING_LENGTH );
#if 0				/* the original implementation */

            sprintf ( buf, "(%s) == (%s)", cond, p + 5 );
            if ( process_if ( buf, go, sc, trig, type ) )
            {
#else				/* new! improved! bug fixed! */
            eval_op ( ( char * ) "==", result, p + 5, buf, MAX_STRING_LENGTH - 1, go, sc, trig );
            if ( *buf && *buf != '0' )
            {
#endif
                free ( buf );
                return c;
            }
            free ( buf );
        }
        else if ( !strn_cmp ( "default", p, 7 ) )
            return c;
        else if ( !strn_cmp ( "done", p, 3 ) )
            return c;
    }
    return c;
}

/*
* scans for end of while/switch-blocks.
* returns the line containg 'end', or the last
* line of the trigger if not found.
*/
struct cmdlist_element *find_done ( struct cmdlist_element *cl )
{
    struct cmdlist_element *c;
    char *p;

    if ( !cl || ! ( cl->next ) )
        return cl;

    for ( c = cl->next; c && c->next; c = c->next )
    {
        for ( p = c->cmd; *p && isspace ( *p ); p++ )
            ;

        if ( !strn_cmp ( "while ", p, 6 ) || !strn_cmp ( "switch ", p, 7 ) )
        {
            c = find_done ( c );
            if ( !c )
                return c;
        }
        else if ( !strn_cmp ( "done", p, 3 ) )
            return c;
    }

    return c;
}


/* read a line in from a file, return the number of chars read */
int fgetline ( FILE * file, char *p )
{
    int count = 0;

    do
    {
        *p = fgetc ( file );
        if ( *p != '\n' && !feof ( file ) )
        {
            p++;
            count++;
        }
    }
    while ( *p != '\n' && !feof ( file ) );

    if ( *p == '\n' )
        *p = '\0';

    return count;
}


/* load in a character's saved variables */
void read_saved_vars ( Character *ch )
{
    FILE *file;
    long context;
    char fn[127];
    char input_line[1024], *temp, *p;
    char varname[32];
    char context_str[16];

    /* create the space for the script structure which holds the vars */
    /* We need to do this first, because later calls to 'remote' will need */
    /* a script already assigned. */
    SCRIPT ( ch ) = create_script();

    /* find the file that holds the saved variables and open it*/
    get_filename ( GET_NAME ( ch ), fn, SCRIPT_VARS_FILE );
    file = fopen ( fn,"r" );

    /* if we failed to open the file, return */
    if ( !file )
    {
        log ( "%s had no variable file", GET_NAME ( ch ) );
        return;
    }

    /* walk through each line in the file parsing variables */
    do
    {
        if ( get_line ( file, input_line ) >0 )
        {
            p = temp = strdup ( input_line );
            temp = any_one_arg ( temp, varname );
            temp = any_one_arg ( temp, context_str );
            skip_spaces ( &temp ); /* temp now points to the rest of the line */

            context = atol ( context_str );
            add_var ( & ( SCRIPT ( ch )->global_vars ), varname, temp, context );
            free ( p ); /* plug memory hole */
        }
    } while ( !feof ( file ) );

    /* close the file and return */
    fclose ( file );
}




/* save a characters variables out to disk */
void save_char_vars ( Character *ch )
{
    FILE *file;
    char fn[127];
    struct trig_var_data *vars;

    /* immediate return if no script (and therefore no variables) structure */
    /* has been created. this will happen when the player is logging in */
    if ( SCRIPT ( ch ) == NULL )
        return;

    /* we should never be called for an NPC, but just in case... */
    if ( IS_NPC ( ch ) )
        return;

    get_filename ( GET_NAME ( ch ), fn, SCRIPT_VARS_FILE );
    unlink ( fn );

    /* make sure this char has global variables to save */
    if ( ch->script->global_vars == NULL )
        return;
    vars = ch->script->global_vars;

    file = fopen ( fn,"wt" );
    if ( !file )
    {
        new_mudlog ( NRM, LVL_GOD, TRUE,
                     "SYSERR: Could not open player variable file %s for writing.:%s",
                     fn, strerror ( errno ) );
        return;
    }
    /* note that currently, context will always be zero. this may change */
    /* in the future */
    while ( vars )
    {
        if ( vars->name[0] != '-' ) /* don't save if it begins with - */
            fprintf ( file, "%s %ld %s\n", vars->name.c_str(), vars->context, vars->value.c_str() );
        vars = vars->next;
    }

    fclose ( file );
}

/* find_char() helpers */
typedef  map<long, Character *> ch_map;
typedef  map<long, obj_data *> obj_map;
ch_map ch_lookup_table;
obj_map obj_lookup_table;

#if 0
// Must be power of 2
#define BUCKET_COUNT 64
// to recognize an empty bucket
#define UID_OUT_OF_RANGE 1000000000

struct lookup_table_t
{
    long uid;
    void * c;
    //struct lookup_table_t *next;
    lookup_table_t() : uid ( -1 ), c ( NULL ) {}
    lookup_table_t ( long u, void * v ) : uid ( u ), c ( v ) {}
    bool operator== ( const long i )
    {
        return ( uid == i );
    }

}
;
vector<lookup_table_t> lookup_table[BUCKET_COUNT];
bool operator< ( const lookup_table_t &a, const lookup_table_t &b )
{
    return a.uid < b.uid;
}

void init_lookup_table ( void )
{
#if 0
    int i;
    for ( i = 0; i < BUCKET_COUNT; i++ )
    {
        lookup_table[i].uid  = UID_OUT_OF_RANGE;
        lookup_table[i].c    = NULL;
        lookup_table[i].next = NULL;
    }
#endif
}
#endif
Character *find_char_by_uid_in_lookup_table ( long uid )
{
#if 0
    Character *tch;
    for ( tch = character_list; tch; tch = tch->next )
        if ( GET_ID ( tch ) == uid )
            return tch;

    return NULL;

#elseif (0)

    int bucket = ( int ) ( uid & ( BUCKET_COUNT - 1 ) );
    vector<lookup_table_t>::iterator lt;

    lt = find ( lookup_table[bucket].begin(), lookup_table[bucket].end(), uid );
    if ( lt != lookup_table[bucket].end() )
    {
        Character *ch = ( Character * ) ( ( *lt ).c );

        if ( !ch )
            return NULL;
        if ( DEAD ( ch ) )
        {
            log ( "find_char_by_uid_in_lookup_table : character is flagged to be extracted" );
            //return NULL;
        }

        return ch;
    }

    log ( "find_char_by_uid_in_lookup_table : No entity with number %ld in lookup table", uid );

    return NULL;
#else

    ch_map::iterator ch = ch_lookup_table.find ( uid );
    if ( ch != ch_lookup_table.end() )
    {
        if ( ch->second == NULL )
            return NULL;
        //if ( DEAD ( ( ch->second ) ) )
        //	log ( "find_char_by_uid_in_lookup_table : character %s in room %d is flagged to be extracted", ch->second->player.name, ch->second->in_room->number );

        return ( ch->second );
    }

    //log ( "find_char_by_uid_in_lookup_table : No entity with number %ld in lookup table", uid );

    return NULL;
#endif
}

struct obj_data *find_obj_by_uid_in_lookup_table ( long uid )
{
#if 0
    struct obj_data *o;
    for ( o = object_list;o;o = o->next )
        if ( GET_ID ( o ) == uid )
            return o;

    return NULL;
#elseif (0)

    int bucket = ( int ) ( uid & ( BUCKET_COUNT - 1 ) );
    vector<lookup_table_t>::iterator lt;

    lt = find ( lookup_table[bucket].begin(), lookup_table[bucket].end(), uid );
    if ( lt != lookup_table[bucket].end() )
        return ( struct obj_data * ) ( ( *lt ).c );

    log ( "find_obj_by_uid_in_lookup_table : No entity with number %ld in lookup table", uid );
    return NULL;
#else

    obj_map::iterator o = obj_lookup_table.find ( uid );
    if ( o == obj_lookup_table.end() )
    {
        //log ( "find_obj_by_uid_in_lookup_table : No entity with number %ld in lookup table", uid );
        return NULL;
    }
    else
        return ( o->second );
#endif
}

void addChToLookupTable ( long uid, Character * c )
{
    ch_map::iterator ch = ch_lookup_table.find ( uid );
    if ( ch == ch_lookup_table.end() )
        ch_lookup_table[uid] = c;
    else
    {
        if ( ( ch->second ) != c )
        {
            log ( "Adding %s to lookup table when %s already exists there.", GET_NAME ( c ), GET_NAME ( ( ch->second ) ) );
        }
    }
}

void addObjToLookupTable ( long uid, obj_data * o )
{
    obj_map::iterator obj = obj_lookup_table.find ( uid );
    if ( obj == obj_lookup_table.end() )
        obj_lookup_table[uid] = o;
    else
    {
        if ( ( obj->second ) != o )
        {
            log ( "Adding %d to lookup table when %d already exists there.", o->item_number, obj->second->item_number );
        }
    }
}
void add_to_lookup_tablex ( long uid, void *c )
{
#if 0
    int bucket = ( int ) ( uid & ( BUCKET_COUNT - 1 ) );
    //struct lookup_table_t *lt = &lookup_table[bucket];
    vector<lookup_table_t>::iterator lt;

    if ( uid < 0 )
    {
        log ( "Add_to_lookup failed. ID -1" );
        return;
    }


    for ( lt = lookup_table[bucket].begin();lt != lookup_table[bucket].end(); lt++ )
    {
        if ( lt->uid == uid )
        {
            log ( "Add_to_lookup failed. ID - Already there. (uid = %ld)", uid );
            return;
            if ( lt->c == c )
            {
                log ( "Add_to_lookup correcting. ID and CHAR Already there. (uid = %ld)", uid );
                lookup_table[bucket].erase ( lt );
                break;
            }
        }
    }
    lookup_table_t ltt = lookup_table_t ( uid, c );
    lookup_table[bucket].push_back ( ltt );
    sort ( lookup_table[bucket].begin(), lookup_table[bucket].end() );
#endif
}

void removeFromChLookupTable ( long uid )
{
    if ( ch_lookup_table.find ( uid ) != ch_lookup_table.end() )
        ch_lookup_table.erase ( uid );
}
void removeFromObjLookupTable ( long uid )
{
    if ( obj_lookup_table.find ( uid ) != obj_lookup_table.end() )
        obj_lookup_table.erase ( uid );
}

void remove_from_lookup_tablex ( long uid )
{

#if 0

    int bucket = ( int ) ( uid & ( BUCKET_COUNT - 1 ) );
    //struct lookup_table_t *lt = &lookup_table[bucket], *flt = NULL;
    vector<lookup_table_t>::iterator lt;

    /*
    * This is not supposed to happen. UID 0 is not used.
    * However, while I'm debugging the issue, let's just return right away.
    *
    * Welcor 02/04
    */
    if ( uid == 0 )
    {
        log ( "Removing id 0 from lookup table" );
        return;
    }
    lt = find ( lookup_table[bucket].begin(), lookup_table[bucket].end(), uid );
    if ( lt != lookup_table[bucket].end() )
    {
        lookup_table[bucket].erase ( lt );
        return;
    }

#if DEBUG
    log ( "remove_from_lookup. UID %ld not found.", uid );
#endif

#endif
}

bool valid_id_num ( long id )
{
    Descriptor *d;
    for ( d = descriptor_list; d; d = d->next )
        if ( d->character && GET_ID ( d->character ) == id )
            return FALSE;
    if ( ch_lookup_table.find ( id ) != ch_lookup_table.end() )
        return FALSE;

    return TRUE;
}

/*
 * processes a script return command.
 * returns the new value for the script to return.
 */
int process_return ( trig_data *trig, char *cmd )
{
    char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

    two_arguments ( cmd, arg1, arg2 );

    if ( !*arg2 )
    {
        script_log ( "Trigger: %s, VNum %d. return w/o an arg in line %d", GET_TRIG_NAME ( trig ), GET_TRIG_VNUM ( trig ), GET_TRIG_LINE_NR ( trig ) );
        return 1;
    }
    return ( int ) atoi ( arg2 );
}
