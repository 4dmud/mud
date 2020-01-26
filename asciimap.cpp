/********************************************************************\
*   History: Developed by: mlkesl@stthomas.edu                       *
*                     and: mlk                                       *
*   MapArea: when given a room, ch, x, and y                         *
*            this function will fill in values of map as it should   *
*   ShowMap: will simply spit out the contents of map array          *
*          Would look much nicer if you built your own areas         *
*          without all of the overlapping stock Rom has              *
*   do_map: core function, takes map size as argument                *
*   update: map is now a 2 dimensional array of integers             *
*       uses NUM_ROOM_SECTORS for null                               *
*       uses NUM_ROOM_SECTORS+1 for mazes or one ways to SECT_ENTER  *
*       use the SECTOR numbers to represent sector types :)          *
*                                                                    *
\********************************************************************/

/**********************************************************\
* WELCOME TO THE WORLD OF CIRCLEMUD ASCII MAP!!!           *
*  This was originall for Rom(who would use that?) and is  *
*  now available for CircleMUD. This was done on bpl17 but *
*  the only major conversions were room structures so it   *
*  should work on any Circle with fairly up to date rooms. *
*                                                          *
*  If you use this drop me a line maybe, and if your       *
*  generous put me in a line in your credits. Doesn't      *
*  matter though, ENJOY! Feel free to fix any bugs here    *
*  make sure you mail the CircleMUD discussion list to let *
*  everyone know!                                          *
*                                                          *
*  Edward Felch, efelch@hotmail.com  4/25/2001             *
\**********************************************************/

/************************************************************************************\
  Notes:
  - You will need more cool sector types!
  - In act.informative.c put in checks for if a room is flagged
    wilderness, if it is: do_map(ch, " "); instead of the normal
    room name and description sending.
  - #define NUM_ROOM_SECTORS 22 should be the max number of sector types + 1
    when I tried to include oasis.h for this it gave me errors so I
    just used the number 22.
  - In utils.h I added this in:
    #define URANGE(a, b, c)          ((b) < (a) ? (a) : ((b) > (c) ? (c) : (b)))
  - Edit XXX.zon so its top room is real high or something and use
     that as a masssively large world map type file
  - We have a ROOM_NOVIEW flag as well as a ROOM_NOENTER flag (used in act.movement.c)
\************************************************************************************/

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "house.h"
#include "constants.h"
#include "dg_scripts.h"
#include "oasis.h"
#include "mapper.h"

#define MAX_MAP 72
#define MAX_MAP_DIR 4

struct obj_data *find_vehicle_by_vnum ( int vnum );
struct obj_data *get_obj_in_list_type ( int type, struct obj_data *list );

void show_map( Character *ch, int mxp, int subcmd = SCMD_MAP );
void display_map ( Character *ch );
void parse_room_name ( room_rnum in_room, char *bufptr, size_t len );

int mapgrid[MAX_MAP][MAX_MAP]; // mapgrid[x][y]: x = row, y = column
int offsets[4][2] = { {-1, 0}, {0, 1}, {1, 0}, {0, -1} };
string local_map;

/* visible_room returns the displayable room where you end up when you go into the
   direction door from room r */

room_rnum visible_room ( room_rnum r, int door, bool* two_way_connection )
{
    struct room_direction_data *pexit;
    room_rnum room_to;

    *two_way_connection = FALSE;
    pexit = r->dir_option[ door ];

    if ( pexit == NULL || IS_SET ( pexit->exit_info, EX_CLOSED ) || IS_SET ( pexit->exit_info, EX_HIDDEN ) )
        return NULL;

    room_to = pexit->to_room;

    if ( room_to == NULL || room_to == r ||  ROOM_FLAGGED ( room_to, ROOM_NOVIEW ) )
        return NULL;

    pexit = room_to->dir_option[ rev_dir [ door ]];

    if ( pexit != NULL && !IS_SET ( pexit->exit_info, EX_CLOSED ) && !IS_SET ( pexit->exit_info, EX_HIDDEN ) && pexit->to_room == r )
        *two_way_connection = TRUE;

    return room_to;
}

/* Heavily modified - Edward */
void MapArea ( room_rnum room, Character *ch, int x, int y, int min,
               int max, bool show_vehicles, bool two_way_connection )
{
    room_rnum prospect_room;
    struct obj_data *obj;
    int door;
    bool two_way;

    if ( ( x < min ) || ( y < min ) || ( x > max ) || ( y > max ) )
        return;

    /* marks the room as visited */
    mapgrid[x][y] = room->sector_type;

    /* mark vehicles on the map */
    if ( show_vehicles )
    {
        for ( obj = room->contents; obj; obj = obj->next_content )
            if ( GET_OBJ_TYPE ( obj ) == ITEM_VEHICLE || GET_OBJ_TYPE ( obj ) == ITEM_VEHICLE2 )
                mapgrid[x][y] = SECT_VEHICLE;
    }

    /* Don't go beyond one-way connections */
    if ( two_way_connection == FALSE )
        return;

    for ( door = 0; door < MAX_MAP_DIR; door++ )
    {
        prospect_room = visible_room ( room, door, &two_way );

        if ( prospect_room == NULL )
            continue;

        if ( mapgrid[x + offsets[door][0]][y + offsets[door][1]] == NUM_ROOM_SECTORS )
        {
            MapArea ( prospect_room, ch, x + offsets[door][0],
                      y + offsets[door][1], min, max, show_vehicles, two_way );
        }
    }
}

/* mlk :: shows a map, specified by size */
void ShowMap ( Character *ch, int min, int max, int subcmd )
{
    int x, y;
    int sect = 0;
    char mapdisp[MAX_STRING_LENGTH];
    size_t len = 0;

    /* every row */
    for ( x = min; x <= max; ++x )
    {
        if ( subcmd == SCMD_MAP )
            len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"  %s%1s{cx%c%-10s   %s%1s{cx%c%-10s   |   ",
            MDIS ( 0 ) ? map_bit[sect].colour : "", MDIS ( 0 ) ? map_bit[sect].bit : "",
            MDIS ( 0 ) ? ' ' : ' ', MDIS ( 0 ) ? map_bit[sect].name : "",
            MDIS ( 1 ) ? map_bit[sect+1].colour : "", MDIS ( 1 ) ? map_bit[sect+1].bit : "",
            MDIS ( 1 ) ? ' ' : ' ', MDIS ( 1 ) ? map_bit[sect+1].name : "" );
        sect += 2;

        /* every column */
        for ( y = min; y <= max; ++y )
        {
            switch ( mapgrid[x][y] )
            {
                case NUM_ROOM_SECTORS:
                case SECT_INSIDE:
                case SECT_CITY:
                case SECT_FIELD:
                case SECT_FOREST:
                case SECT_HILLS:
                case SECT_MOUNTAIN:
                case SECT_WATER_SWIM:
                case SECT_WATER_NOSWIM:
                case SECT_UNDERWATER:
                case SECT_FLYING:
                case SECT_DESERT:
                case SECT_SPACE:
                case SECT_ROAD:
                case SECT_ENTRANCE:
                case SECT_ATMOSPHERE:
                case SECT_SUN:
                case SECT_BLACKHOLE:
                case SECT_VEHICLE:
                case SECT_SWAMP:
                case SECT_REEF:
                case SECT_DEATHTRAP:
                case SECT_SNOW:
                case SECT_ICE:
                case SECT_PRAIRIE:
                case SECT_BADLANDS:
                case SECT_RAIL:
                    len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len, "%s%s", map_bit[mapgrid[x][y]].colour, map_bit[mapgrid[x][y]].bit );
                    break;
                default:
                    len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len, "{cR*" );
                    break;
            }
            len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len, " " );
        }
        len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len, "{cx\r\n" );
    }

    len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len, "{cx\r\n" );
    mapdisp[len] = 0;

    if ( subcmd == SCMD_MAP )
        local_map += string ( mapdisp );
    else
        local_map = string ( mapdisp );
}


/* will put a small map with current room desc and title */
/* this is the main function to show the map, its do_map with " " */

/* Heavily modified - Edward */
void ShowRoom ( Character *ch, int min, int max, int subcmd )
{
    char dispbuf[MAX_INPUT_LENGTH];

    ShowMap ( ch, min, max, subcmd );
    if ( subcmd == SCMD_AUTOMAP )
        return;

    parse_room_name ( IN_ROOM ( ch ), dispbuf, sizeof ( dispbuf ) );
    ch->Send ( "{cx   {cc%s{cx", dispbuf );

    if ( GET_LEVEL ( ch ) >= LVL_GOD )
        ch->Send ( " {cc[Room %d]{cx", IN_ROOM ( ch )->number );

    ch->Send ( "\r\n" );
}

void draw_wilderness_map ( Character *ch )
{
    int size = URANGE ( 10, 0, MAX_MAP );
    int center = MAX_MAP / 2;
    int min = MAX_MAP / 2 - size / 2;
    int max = MAX_MAP / 2 + size / 2;
    int x,y;
    bool two_way;

    for ( x = 0; x < MAX_MAP; ++x )
        for ( y = 0; y < MAX_MAP; ++y )
            mapgrid[x][y] = NUM_ROOM_SECTORS;

    /* starts the mapping with the center room */
    MapArea ( IN_ROOM ( ch ), ch, center, center, min, max, TRUE, TRUE );

    /* make sure the rooms in the cardinal directions are correct */
    y = center;
    x = center + 1;
    Room *r = visible_room ( IN_ROOM ( ch ), EAST, &two_way );
    while ( r != NULL && x < MAX_MAP )
    {
        mapgrid[y][x++] = r->sector_type;
        r = visible_room ( r, EAST, &two_way );
    }

    x = center - 1;
    r = visible_room ( IN_ROOM ( ch ), WEST, &two_way );
    while ( r != NULL && x >= 0 )
    {
        mapgrid[y][x--] = r->sector_type;
        r = visible_room ( r, WEST, &two_way );
    }

    y = center - 1;
    x = center;
    r = visible_room ( IN_ROOM ( ch ), NORTH, &two_way );
    while ( r != NULL && y >= 0 )
    {
        mapgrid[y--][x] = r->sector_type;
        r = visible_room ( r, NORTH, &two_way );
    }

    y = center + 1;
    r = visible_room ( IN_ROOM ( ch ), SOUTH, &two_way );
    while ( r != NULL && y < MAX_MAP )
    {
        mapgrid[y++][x] = r->sector_type;
        r = visible_room ( r, SOUTH, &two_way );
    }
}

void crop_wilderness_map()
{
    // remove empty rows starting from the top
    size_t pos1 = 0, pos2;
    while (true) {
		pos2 = local_map.find ( "{cx\r\n", pos1 );
		if ( pos2 == string::npos )
			break;
		if ( count ( local_map.begin() + pos1, local_map.begin() + pos2, '{' ) > count ( local_map.begin() + pos1, local_map.begin() + pos2, '?' ) )
            break;
        local_map.erase ( pos1, pos2 + 5 - pos1 );
	}

    // remove empty rows starting from the bottom
    while (true) {
        pos2 = local_map.rfind ( "{cx\r\n" );
        pos1 = local_map.rfind ( "{cx\r\n", pos2-1 );
        if ( count ( local_map.begin() + pos1+1, local_map.begin() + pos2, '{' ) > count ( local_map.begin() + pos1+1, local_map.begin() + pos2, '?' ) )
            break;
        local_map.erase ( pos1 + 5, pos2 - pos1 );
    }

    // find the number of columns to remove from the left
    int num_col = 1e6;
    pos1 = 0;
    while (true) {
        int c = 0;
        pos2 = local_map.find ( "{cx\r\n", pos1 );
        if ( pos2 == string::npos )
            break;
        while ( pos1 < pos2 && c < num_col )
        {
            if ( local_map.substr ( pos1, 4 ) != "{cl?" )
                break;
            c++;
            pos1 += 5;
        }
        num_col = min ( num_col, c );
        pos1 = pos2 + 5;
    }

    // remove empty columns from the left
    if ( num_col > 0 )
    {
        pos1 = 0;
        while (true)
        {
            for ( int i = 0; i < num_col; ++i )
                local_map.erase ( pos1, 5 );
            pos2 = local_map.find ( "{cx\r\n", pos1 );
            if ( pos2 == string::npos )
                break;
            pos1 = pos2 + 5;
        }
    }

    // find the number of columns to remove from the right
    num_col = 1e6;
    pos1 = local_map.length() - 10; // position of the last colour
    while ( pos1 < local_map.length() )
    {
        int c = 0;
        pos2 = local_map.rfind ( "{cx\r\n", pos1 );
        if ( pos2 == string::npos )
            pos2 = 0;
        while ( pos1 > pos2 && c < num_col )
        {
            if ( local_map.substr ( pos1, 4 ) != "{cl?" )
                break;
            c++;
            pos1 -= 5;
        }
        num_col = min ( num_col, c );
        pos1 = pos2 - 5;
    }
    if ( num_col == 0 )
        return;

    // remove empty columns from the right
    pos1 = local_map.length() - 10; // position of the last colour
    while ( pos1 < local_map.length() )
    {
        for ( int i = 0; i < num_col; ++i )
        {
            local_map.erase ( pos1, 5 );
            pos1 -= 5;
        }
        pos2 = local_map.rfind ( "{cx\r\n", pos1 );
        if ( pos2 == string::npos )
            break;
        pos1 = pos2 - 5;
    }
}

/* This is the main map function, do_map(ch, " ") is good to use */
/* do_map(ch "number") is for immortals to see the world map     */

/* Edward: If you play with some of the values here you can make the normal
 ** map people see larger or smaller. size = URANGE(9, size, MAX_MAP), the 9
 ** is the map size shown by default. Also look for: ShowMap (ch, min, max+1);
 ** and change the size of min and max and see what you like.
 */
/* If subcmd == SCMD_AUTOMAP (the player typed "look" or moved with automap
   not being "off"):
   - Don't show the legend
   - Remove empty rows and columns in the wilderness map
   - If automap is "left" or "right", combine the map with the room desc.
*/
void do_map ( Character *ch, char *argument, int cmd, int subcmd )
{
    local_map = "";
    if ( IS_DARK ( IN_ROOM ( ch ) ) && !CAN_SEE_IN_DARK ( ch ) )
    {
        ch->Send ( "{cbThe wilderness is pitch black at night... {cx\r\n" );
        return;
    }

    Room *was_in = IN_ROOM ( ch );

    struct obj_data *viewport, *vehicle;
    viewport = get_obj_in_list_type ( ITEM_V_WINDOW, was_in->contents );

    if ( viewport )
    {
        vehicle = find_vehicle_by_vnum ( GET_OBJ_VAL ( viewport, 0 ) );
        if ( !vehicle )
        {
            ch->Send ( "Something is wrong with this vehicle.\r\n" );
            return;
        }
        IN_ROOM ( ch ) = IN_ROOM ( vehicle );
    }
    else if ( IN_ROOM ( ch )->vehicle )
        IN_ROOM ( ch ) = IN_ROOM ( IN_ROOM ( ch )->vehicle );

    if ( !ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_WILDERNESS ) && !PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
    {
        draw_map ( ch );
        show_map ( ch, FALSE, subcmd );
        IN_ROOM ( ch ) = was_in;
        if ( subcmd == SCMD_MAP )
            ch->Send ( "%s", local_map.c_str() );
        return;
    }

    draw_wilderness_map ( ch );

    int size = URANGE ( 10, 0, MAX_MAP );
    int center = MAX_MAP / 2;
    int min = MAX_MAP / 2 - size / 2;
    int max = MAX_MAP / 2 + size / 2;

    /* show text-based map */
    if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
    {
        char buf[MAX_INPUT_LENGTH];
        bool two_way;
        int i, center = MAX_MAP / 2;
        int size = atoi ( argument );

        if ( size < 1 || size > 5 )
            size = 4;
        Room *r = was_in;
        parse_room_name ( r, buf, sizeof ( buf ) );
        local_map += "{cw" + string ( buf ) + ": " + string ( map_bit[mapgrid[center][center]].name ) + "{cx\r\n";

        if ( mapgrid[center - 1][center] < NUM_ROOM_SECTORS )
        {
            local_map += "{cwNorth: " + string ( map_bit[mapgrid[center - 1][center]].name );
            for ( i = 2; i <= size && mapgrid[center - i][center] < NUM_ROOM_SECTORS; i++ )
                local_map += ", " + string ( map_bit[mapgrid[center - i][center]].name );
            local_map += "{cx\r\n";
        }
        if ( mapgrid[center][center + 1] < NUM_ROOM_SECTORS )
        {
            local_map += "{cwEast: " + string ( map_bit[mapgrid[center][center + 1]].name );
            for ( i = 2; i <= size && mapgrid[center][center + i] < NUM_ROOM_SECTORS; i++ )
                local_map += ", " + string ( map_bit[mapgrid[center][center + i]].name );
            local_map += "{cx\r\n";
        }
        if ( mapgrid[center + 1][center] < NUM_ROOM_SECTORS )
        {
            local_map += "{cwSouth: " + string ( map_bit[mapgrid[center + 1][center]].name );
            for ( i = 2; i <= size && mapgrid[center + i][center] < NUM_ROOM_SECTORS; i++ )
                local_map += ", " + string ( map_bit[mapgrid[center + i][center]].name );
            local_map += "{cx\r\n";
        }
        if ( mapgrid[center][center - 1] < NUM_ROOM_SECTORS )
        {
            local_map += "{cwWest: " + string ( map_bit[mapgrid[center][center - 1]].name );
            for ( i = 2; i <= size && mapgrid[center][center - i] < NUM_ROOM_SECTORS; i++ )
                local_map += ", " + string ( map_bit[mapgrid[center][center - i]].name );
            local_map += "{cx\r\n";
        }

        r = visible_room ( IN_ROOM ( ch ), UP, &two_way );
        if ( r != NULL )
            local_map += "{cwUp: " + string ( map_bit[ r->sector_type ].name ) + "{cx\r\n";

        r = visible_room ( IN_ROOM ( ch ), DOWN, &two_way );
        if ( r != NULL )
            local_map += "{cwDown: " + string ( map_bit[ r->sector_type ].name ) + "{cx\r\n";

        if ( mapgrid[center - 1][center - 1] < NUM_ROOM_SECTORS )
            local_map += "{cwNorthwest: " + string ( map_bit[mapgrid[center - 1][center - 1]].name ) + "{cx\r\n";

        if ( mapgrid[center - 1][center + 1] < NUM_ROOM_SECTORS )
            local_map += "{cwNortheast: " + string ( map_bit[mapgrid[center - 1][center + 1]].name ) + "{cx\r\n";

        if ( mapgrid[center + 1][center + 1] < NUM_ROOM_SECTORS )
            local_map += "{cwSoutheast: " + string ( map_bit[mapgrid[center + 1][center + 1]].name ) + "{cx\r\n";

        if ( mapgrid[center + 1][center - 1] < NUM_ROOM_SECTORS )
            local_map += "{cwSouthwest: " + string ( map_bit[mapgrid[center + 1][center - 1]].name ) + "{cx\r\n";

        IN_ROOM ( ch ) = was_in;
        if ( subcmd == SCMD_MAP )
            ch->Send ( "%s", local_map.c_str() );
        return;
    }

    /* marks the center, where ch is */
    mapgrid[center][center] = NUM_ROOM_SECTORS + 2;	/* can be any number above NUM_ROOM_SECTORS+1 */

    ShowRoom ( ch, min, max, subcmd );
    IN_ROOM ( ch ) = was_in;
    if ( subcmd == SCMD_MAP )
        ch->Send ( "%s", local_map.c_str() );
    else if ( subcmd == SCMD_AUTOMAP && ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_WILDERNESS ) )
        crop_wilderness_map();
}
