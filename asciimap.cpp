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
struct obj_data *get_obj_in_list_type ( int type,
			                                        struct obj_data *list );

void display_map ( Character *ch );
void parse_room_name ( room_rnum in_room, char *bufptr, size_t len );

int mapgrid[MAX_MAP][MAX_MAP];
int offsets[4][2] = { {-1, 0}, {0, 1}, {1, 0}, {0, -1} };

/* Heavily modified - Edward */
void MapArea ( room_rnum room, Character *ch, int x, int y, int min,
               int max, bool show_vehicles )
{
	room_rnum prospect_room;
	struct room_direction_data *pexit;
	struct obj_data *obj;
	int door;

	/* marks the room as visited */
	mapgrid[x][y] = room->sector_type;

	/* mark vehicles on the map */
	if ( show_vehicles )
	{
		for ( obj = room->contents; obj; obj = obj->next_content )
			if ( GET_OBJ_TYPE ( obj ) == ITEM_VEHICLE )
				mapgrid[x][y] = SECT_VEHICLE;
	}

	/* Otherwise we get a nasty crash */
	if ( !IS_SET_AR ( IN_ROOM ( ch )->room_flags, ROOM_WILDERNESS ) )
		return;

	for ( door = 0; door < MAX_MAP_DIR; door++ )
	{
		if ( ( pexit = room->dir_option[door] ) != NULL &&
		        ( pexit->to_room > 0 ) &&
		        ( !IS_SET ( pexit->exit_info, EX_CLOSED ) ) )
		{
			if ( ( x < min ) || ( y < min ) || ( x > max ) || ( y > max ) )
				return;
			prospect_room = pexit->to_room;

			/* one way into area OR maze */
			/* if not two way */
			if ( prospect_room->dir_option[rev_dir[door]] &&
			        prospect_room->dir_option[rev_dir[door]]->to_room !=room )
			{
				mapgrid[x][y] = NUM_ROOM_SECTORS + 1;
				return;
			}
			/* end two way */
			/* players can't see past these */
			if ( ( prospect_room->sector_type == SECT_HILLS )
			        || ( prospect_room->sector_type == SECT_CITY )
			        || ( prospect_room->sector_type == SECT_INSIDE )
			        || ( prospect_room->sector_type == SECT_FOREST )
			        || IS_SET_AR ( prospect_room->room_flags, ROOM_NOVIEW ) )
			{
				mapgrid[x + offsets[door][0]][y + offsets[door][1]] =
				    prospect_room->sector_type;
				/* ^--two way into area */
			}

			if ( mapgrid[x + offsets[door][0]][y + offsets[door][1]] ==
			        NUM_ROOM_SECTORS )
			{
				MapArea ( pexit->to_room, ch, x + offsets[door][0],
				          y + offsets[door][1], min, max, show_vehicles );
			}
		}			/* end if exit there */
	}
	return;
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
	for ( x = min; x < ( min + 12 ); ++x )
	{
		len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"  %s%1s{cx%c%-10s   %s%1s{cx%c%-10s   |   ",
		                  MDIS ( 0 ) ? map_bit[sect].colour : "", MDIS ( 0 ) ? map_bit[sect].bit : "", MDIS ( 0 ) ? '-' : ' ', MDIS ( 0 ) ? map_bit[sect].name : "",
		                  MDIS ( 1 ) ? map_bit[sect+1].colour : "", MDIS ( 1 ) ? map_bit[sect+1].bit : "", MDIS ( 1 ) ? '-' : ' ', MDIS ( 1 ) ? map_bit[sect+1].name : "" );
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
						case SECT_TUNDRA:
						case SECT_SNOW:
						case SECT_ICE:
						case SECT_PRAIRIE:
						case SECT_BADLANDS:
						case SECT_RAIL:
							len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"%s%s", map_bit[mapgrid[x][y]].colour, map_bit[mapgrid[x][y]].bit );
							break;
						case ( NUM_ROOM_SECTORS + 1 ) :
										len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"{cM?" );
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
						case SECT_TUNDRA:
						case SECT_SNOW:
						case SECT_ICE:
						case SECT_PRAIRIE:
						case SECT_BADLANDS:
						case SECT_RAIL:
							len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"%s%s", map_bit[mapgrid[x][y]].colour, map_bit[mapgrid[x][y]].bit );
							break;
						case ( NUM_ROOM_SECTORS + 1 ) :
										len += snprintf ( mapdisp + len, sizeof ( mapdisp ) - len,"{cM?" );
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
	int size = 0, center, x, y, min, max;
	room_rnum was_in;
	char arg1[10];
	struct obj_data *viewport, *vehicle;
	arg1[0] = 0;
	//one_argument(argument, arg1);
	//size = atoi(arg1);
	size = URANGE ( 10, size, MAX_MAP );

	center = MAX_MAP / 2;

	min = MAX_MAP / 2 - size / 2;
	max = MAX_MAP / 2 + size / 2;

	was_in = IN_ROOM ( ch );

	viewport =
	    get_obj_in_list_type ( ITEM_V_WINDOW, IN_ROOM ( ch )->contents );
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
        else if (IN_ROOM(ch)->vehicle)
            IN_ROOM(ch) = IN_ROOM(IN_ROOM(ch)->vehicle);

	for ( x = 0; x < MAX_MAP; ++x )
		for ( y = 0; y < MAX_MAP; ++y )
			mapgrid[x][y] = NUM_ROOM_SECTORS;

	/* starts the mapping with the center room */
	MapArea ( IN_ROOM ( ch ), ch, center, center, min - 1, max - 1, true );

	/* marks the center, where ch is */
	mapgrid[center][center] = NUM_ROOM_SECTORS + 2;	/* can be any number above NUM_ROOM_SECTORS+1 */

	/* switch default will print out the */
//    if ((GET_LEVEL(ch) < LVL_GOD) || (IS_NPC(ch))) {
	if ( !IS_SET_AR ( IN_ROOM ( ch )->room_flags, ROOM_WILDERNESS ) )
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
	if ( IS_DARK ( IN_ROOM ( ch ) ) && !CAN_SEE_IN_DARK ( ch ) )
	{
		send_to_char
		( "{cbThe wilderness is pitch black at night... {cx\r\n",
		  ch );
		IN_ROOM ( ch ) = was_in;
		return;
	}
	else
	{
		ShowRoom ( ch, min, max + 1 );
		IN_ROOM ( ch ) = was_in;
		return;
	}
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
	ch->Send ( "What??\r\n" );
	IN_ROOM ( ch ) = was_in;
	return;
}
