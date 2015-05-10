/********************************************************************\
*   History: Developed by: mlkesl@stthomas.edu			     *
*                     and: mlk                                       *
*   MapArea: when given a room, ch, x, and y                         *
*            this function will fill in values of map as it should   *
*   ShowMap: will simply spit out the contents of map array          *
*	    Would look much nicer if you built your own areas        *
*	    without all of the overlapping stock Rom has             *
*   do_map: core function, takes map size as argument                *
*   update: map is now a 2 dimensional array of integers             *
*	    uses NUM_ROOM_SECTORS for null                                   *
*	    uses NUM_ROOM_SECTORS+1 for mazes or one ways to SECT_ENTER	     *
*	    use the SECTOR numbers to represent sector types :)	     *
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

void display_map ( Character *ch );
void parse_room_name ( room_rnum in_room, char *bufptr, size_t len );

int mapgrid[MAX_MAP][MAX_MAP];
int offsets[4][2] = { {-1, 0}, {0, 1}, {1, 0}, {0, -1} };


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
void ShowMap ( Character *ch, int min, int max )
{
	int x, y;
	int sect;
	char mapdisp[MAX_STRING_LENGTH];
	size_t len = 0;


	sect = 0;

	/* every row */
	for ( x = min; x < ( min + 13 ); ++x )
	{
		len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"  %s%1s{cx%c%-10s   %s%1s{cx%c%-10s   |   ",
		                  MDIS ( 0 ) ? map_bit[sect].colour : "", MDIS ( 0 ) ? map_bit[sect].bit : "", MDIS ( 0 ) ? ' ' : ' ', MDIS ( 0 ) ? map_bit[sect].name : "",
		                  MDIS ( 1 ) ? map_bit[sect+1].colour : "", MDIS ( 1 ) ? map_bit[sect+1].bit : "", MDIS ( 1 ) ? ' ' : ' ', MDIS ( 1 ) ? map_bit[sect+1].name : "" );
		sect += 2;

		if ( x < max )
		{
			/* every column */
			for ( y = min; y < max; ++y )
			{
				if ( ( y == min ) || ( mapgrid[x][y - 1] != mapgrid[x][y] ) )
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
							len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"%s%s", map_bit[mapgrid[x][y]].colour, map_bit[mapgrid[x][y]].bit );
							break;
						case ( NUM_ROOM_SECTORS + 1 ) :
							len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"{cl?{cn" );
							break;
						default:
							len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"{cR*" );
							break;
					}
					len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len," " );
				}
				else
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
							len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"%s%s", map_bit[mapgrid[x][y]].colour, map_bit[mapgrid[x][y]].bit );
							break;
						case ( NUM_ROOM_SECTORS + 1 ) :
							len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"{cl?{cn" );
							break;
						default:
							len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"{cR*" );
							break;
					}
					len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len," " ); //add and extra space between the emblimbs to square it out
				}

			}
		}

		len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"{cx\r\n" );
	}

	len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"{cx\r\n" );
	mapdisp[len] = 0;
	ch->Send ( "%s", mapdisp );
	return;
}


/* will put a small map with current room desc and title */
/* this is the main function to show the map, its do_map with " " */

/* Heavily modified - Edward */
void ShowRoom ( Character *ch, int min, int max )
{
	char dispbuf[MAX_INPUT_LENGTH];

#if 0
	/* mlk :: rounds edges */
	mapgrid[min][min] = NUM_ROOM_SECTORS;
	mapgrid[max - 1][max - 1] = NUM_ROOM_SECTORS;
	mapgrid[min][max - 1] = NUM_ROOM_SECTORS;
	mapgrid[max - 1][min] = NUM_ROOM_SECTORS;
#endif



	parse_room_name ( IN_ROOM ( ch ), dispbuf, sizeof ( dispbuf ) );
	ch->Send ( "{cx   {cc%s{cx", dispbuf );

	if ( GET_LEVEL ( ch ) >= LVL_GOD )
		ch->Send ( " {cc[Room %d]{cx", IN_ROOM ( ch )->number );

	ch->Send ( "\r\n" );

	ShowMap ( ch, min, max );
//draw_map(ch);

	return;
}

/* This is the main map function, do_map(ch, " ") is good to use */
/* do_map(ch "number") is for immortals to see the world map     */

/* Edward: If you play with some of the values here you can make the normal
 ** map people see larger or smaller. size = URANGE(9, size, MAX_MAP), the 9
 ** is the map size shown by default. Also look for: ShowMap (ch, min, max+1);
 ** and change the size of min and max and see what you like.
 */
ACMD ( do_map )
{
	int size = 0, center, x, y, min, max, i;
	room_rnum was_in, r;
	struct obj_data *viewport, *vehicle;
	char buf[MAX_INPUT_LENGTH];
	bool two_way;

	if ( IS_DARK ( IN_ROOM ( ch ) ) && !CAN_SEE_IN_DARK ( ch ) )
	{
		ch->Send ( "{cbThe wilderness is pitch black at night... {cx\r\n" );
		return;
	}

	was_in = IN_ROOM ( ch );

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
#if defined(MINI_MAP)
		mini_map ( ch );
#else
		//display_map(ch);
		draw_map ( ch );
#endif
		IN_ROOM ( ch ) = was_in;
		return;
	}

	size = URANGE ( 10, size, MAX_MAP );

	center = MAX_MAP / 2;

	min = MAX_MAP / 2 - size / 2;
	max = MAX_MAP / 2 + size / 2;

	for ( x = 0; x < MAX_MAP; ++x )
		for ( y = 0; y < MAX_MAP; ++y )
			mapgrid[x][y] = NUM_ROOM_SECTORS;

	/* starts the mapping with the center room */
	MapArea ( IN_ROOM ( ch ), ch, center, center, min, max, TRUE, TRUE );

	/* make sure the rooms e,s,w are correct */
	r = visible_room ( IN_ROOM ( ch ), EAST, &two_way );
	if ( r != NULL )
		mapgrid[x][y+1] = r->sector_type;

	r = visible_room ( IN_ROOM ( ch ), SOUTH, &two_way );
	if ( r != NULL )
		mapgrid[x+1][y] = r->sector_type;

	r = visible_room ( IN_ROOM ( ch ), WEST, &two_way );
	if ( r != NULL )
		mapgrid[x][y-1] = r->sector_type;

	/* show text-based map */
	if ( PRF_FLAGGED ( ch, PRF_NOGRAPHICS ) )
	{
		size = atoi ( argument );
		if ( size < 1 || size > 5 )
			size = 4;
		r = was_in;
		parse_room_name ( r, buf, sizeof ( buf ) );
		ch->Send ( "%s: %s\r\n", buf, map_bit[mapgrid[center][center]].name );

		if ( mapgrid[center - 1][center] < NUM_ROOM_SECTORS )
		{
			ch->Send ( "North: %s", map_bit[mapgrid[center - 1][center]].name );
			for ( i = 2; i <= size && mapgrid[center - i][center] < NUM_ROOM_SECTORS; i++ )
				ch->Send ( ", %s", map_bit[mapgrid[center - i][center]].name );
			ch->Send ( "\r\n" );
		}
		if ( mapgrid[center][center + 1] < NUM_ROOM_SECTORS )
		{
			ch->Send ( "East: %s", map_bit[mapgrid[center][center + 1]].name );
			for ( i = 2; i <= size && mapgrid[center][center + i] < NUM_ROOM_SECTORS; i++ )
				ch->Send ( ", %s", map_bit[mapgrid[center][center + i]].name );
			ch->Send ( "\r\n" );
		}
		if ( mapgrid[center + 1][center] < NUM_ROOM_SECTORS )
		{
			ch->Send ( "South: %s", map_bit[mapgrid[center + 1][center]].name );
			for ( i = 2; i <= size && mapgrid[center + i][center] < NUM_ROOM_SECTORS; i++ )
				ch->Send ( ", %s", map_bit[mapgrid[center + i][center]].name );
			ch->Send ( "\r\n" );
		}
		if ( mapgrid[center][center - 1] < NUM_ROOM_SECTORS )
		{
			ch->Send ( "West: %s", map_bit[mapgrid[center][center - 1]].name );
			for ( i = 2; i <= size && mapgrid[center][center - i] < NUM_ROOM_SECTORS; i++ )
				ch->Send ( ", %s", map_bit[mapgrid[center][center - i]].name );
			ch->Send ( "\r\n" );
		}

		r = visible_room ( IN_ROOM ( ch ), UP, &two_way );
		if ( r != NULL )
			ch->Send ( "Up: %s\r\n", map_bit[ r->sector_type ].name );

		r = visible_room ( IN_ROOM ( ch ), DOWN, &two_way );
		if ( r != NULL )
			ch->Send ( "Down: %s\r\n", map_bit[ r->sector_type ].name );

		if ( mapgrid[center - 1][center - 1] < NUM_ROOM_SECTORS )
			ch->Send ( "Northwest: %s\r\n", map_bit[mapgrid[center - 1][center - 1]].name );

		if ( mapgrid[center - 1][center + 1] < NUM_ROOM_SECTORS )
			ch->Send ( "Northeast: %s\r\n", map_bit[mapgrid[center - 1][center + 1]].name );

		if ( mapgrid[center + 1][center + 1] < NUM_ROOM_SECTORS )
			ch->Send ( "Southeast: %s\r\n", map_bit[mapgrid[center + 1][center + 1]].name );

		if ( mapgrid[center + 1][center - 1] < NUM_ROOM_SECTORS )
			ch->Send ( "Southwest: %s\r\n", map_bit[mapgrid[center + 1][center - 1]].name );

		IN_ROOM ( ch ) = was_in;
		return;
	}

	/* marks the center, where ch is */
	mapgrid[center][center] = NUM_ROOM_SECTORS + 2;	/* can be any number above NUM_ROOM_SECTORS+1 */

	ShowRoom ( ch, min, max + 1 );
	IN_ROOM ( ch ) = was_in;
	return;

//    }
	/* mortals not in city, enter or inside will always get a ShowRoom */
	/* unnecesary to have a seperate command for gods. besides, it crashes the mud.
	    if (GET_LEVEL(ch) >= LVL_GOD) {
		if (arg1[0] == '\0') {
		   // ShowRoom(ch, min, max + 1);
		   // IN_ROOM(ch) = was_in;

		} else {
		    ShowMap(ch, min, max + 1);
		    IN_ROOM(ch) = was_in;
		}
		return;
	    }
	    */
}
