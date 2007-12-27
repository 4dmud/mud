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

#include "conf.h"
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

#define MAX_MAP 72
#define MAX_MAP_DIR 4

struct obj_data *find_vehicle_by_vnum(int vnum);
struct obj_data *get_obj_in_list_type(int type,
					     struct obj_data *list);

extern struct room_data *world_vnum[];
void display_map(struct char_data *ch);
void parse_room_name(room_rnum in_room, char *bufptr, size_t len);

int map[MAX_MAP][MAX_MAP];
int offsets[4][2] = { {-1, 0}, {0, 1}, {1, 0}, {0, -1} };

/* Heavily modified - Edward */
void MapArea(room_rnum room, struct char_data *ch, int x, int y, int min,
	     int max)
{
    room_rnum prospect_room;
    struct room_direction_data *pexit;
    struct obj_data *obj;
    int door;

    /* marks the room as visited */
    map[x][y] = room->sector_type;

    /* mark vehicles on the map */
    for (obj = room->contents; obj; obj = obj->next_content)
	if (GET_OBJ_TYPE(obj) == ITEM_VEHICLE)
	    map[x][y] = SECT_VEHICLE;

    /* Otherwise we get a nasty crash */
    if (!IS_SET_AR(IN_ROOM(ch)->room_flags, ROOM_WILDERNESS))
	return;

    for (door = 0; door < MAX_MAP_DIR; door++) {
	if ((pexit = room->dir_option[door]) != NULL &&
	    (pexit->to_room > 0) &&
	    (!IS_SET(pexit->exit_info, EX_CLOSED))) {
	    if ((x < min) || (y < min) || (x > max) || (y > max))
		return;
	    prospect_room = pexit->to_room;

	    /* one way into area OR maze */
	    /* if not two way */
	    if (prospect_room->dir_option[rev_dir[door]] &&
		prospect_room->dir_option[rev_dir[door]]->to_room !=room) {
		map[x][y] = NUM_ROOM_SECTORS + 1;
		return;
	    }
	    /* end two way */
	    /* players cant see past these */
	    if ((prospect_room->sector_type == SECT_HILLS)
		|| (prospect_room->sector_type == SECT_CITY)
		|| (prospect_room->sector_type == SECT_INSIDE)
		|| (prospect_room->sector_type == SECT_FOREST)
		|| IS_SET_AR(prospect_room->room_flags, ROOM_NOVIEW)) {
		map[x + offsets[door][0]][y + offsets[door][1]] =
		    prospect_room->sector_type;
		/* ^--two way into area */
	    }

	    if (map[x + offsets[door][0]][y + offsets[door][1]] ==
		NUM_ROOM_SECTORS) {
		MapArea(pexit->to_room, ch, x + offsets[door][0],
			y + offsets[door][1], min, max);
	    }
	}			/* end if exit there */
    }
    return;
}

/* mlk :: shows a map, specified by size */
void ShowMap(struct char_data *ch, int min, int max)
{
    int x, y;
    int sect;
    char mapdisp[MAX_STRING_LENGTH];
    size_t len = 0;
    
static struct map_bit_data {
    char bit[2];
    char color[5];
    char name[15];
    } map_bit[] = {
{"%", "{cW", "Inside"    },//SECT_INSIDE         
{"#", "{cW", "City"      },//SECT_CITY           
{"\"","{cG", "Field"     },//SECT_FIELD          
{"@", "{cg", "Forest"    },//SECT_FOREST         
{"^", "{cG", "Hills"     },//SECT_HILLS          
{"^", "{cy", "Mountain"  },//SECT_MOUNTAIN       
{":", "{cC", "Water-Swim"},//SECT_WATER_SWIM     
{":", "{cB", "Water-Boat"},//SECT_WATER_NOSWIM   
{"~", "{cb", "Underwater"},//SECT_UNDERWATER	    
{"%", "{cC", "Flying"    },//SECT_FLYING         
{"~", "{cY", "Desert"    },//SECT_DESERT         
{".", "{cW", "Space"     },//SECT_SPACE	
{"-", "{cY", "Road"      },//SECT_ROAD	
{"@", "{cY", "Entrance"  },//SECT_ENTRANCE	
{"@", "{cC", "Atmosphere"},//SECT_ATMOSPHERE 
{"*", "{cY", "Sun"       },//SECT_SUN	
{"O", "{cL", "Blackhole" },//SECT_BLACKHOLE	
{"<", "{cM", "Vehicle"   },//SECT_VEHICLE	
{"\"", "{cg", "Swamp"     },//SECT_SWAMP
{";", "{cM", "Reef"      },//SECT_REEF  
{"\"", "{cW", "Tundra"    },//SECT_TUNDRA
{"o", "{cW", "Snow"      },//SECT_SNOW
{":", "{cC", "Ice"       },//SECT_ICE
{"\"", "{cy", "Praire"     },//SECT_PRAIRIE
{"'", "{cr", "Badlands"       },//SECT_BADLANDS
{"+", "{cy", "Rail"       },//SECT_RAIL

{"X", "{cx", "Unknown"   } //NUM_ROOM_SECTORS
    };
sect = 0;
#define MDIS(num)  ((sect + (num)) <= NUM_ROOM_SECTORS)
    /* every row */
    for (x = min; x < (min + 12); ++x) {
    len += snprintf(mapdisp + len, sizeof(mapdisp) - len,"  %s%1s{cx%c%-10s   %s%1s{cx%c%-10s   |   ", 
    MDIS(0) ? map_bit[sect].color : "", MDIS(0) ? map_bit[sect].bit : "", MDIS(0) ? '-' : ' ', MDIS(0) ? map_bit[sect].name : "",
    MDIS(1) ? map_bit[sect+1].color : "", MDIS(1) ? map_bit[sect+1].bit : "", MDIS(1) ? '-' : ' ', MDIS(1) ? map_bit[sect+1].name : "");
    sect += 2;
    
    if (x < max) {
	/* every column */
	for (y = min; y < max; ++y) {
	    if ((y == min) || (map[x][y - 1] != map[x][y])) {
		switch (map[x][y]) {
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
		    len += snprintf(mapdisp + len, sizeof(mapdisp) - len,"%s%s", map_bit[map[x][y]].color, map_bit[map[x][y]].bit);
		    break;
		case (NUM_ROOM_SECTORS + 1):
		    len += snprintf(mapdisp + len, sizeof(mapdisp) - len,"{cM?");
		    break;
		default:
		    len += snprintf(mapdisp + len, sizeof(mapdisp) - len,"{cR*");
		    break;
		}
		len += snprintf(mapdisp + len, sizeof(mapdisp) - len," ");
	    } else {
		switch (map[x][y]) {
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
		    len += snprintf(mapdisp + len, sizeof(mapdisp) - len,"%s%s", map_bit[map[x][y]].color, map_bit[map[x][y]].bit);
		    break;
		case (NUM_ROOM_SECTORS + 1):
		    len += snprintf(mapdisp + len, sizeof(mapdisp) - len,"{cM?");
		    break;
		default:
		    len += snprintf(mapdisp + len, sizeof(mapdisp) - len,"{cR*");
		    break;
		}
		len += snprintf(mapdisp + len, sizeof(mapdisp) - len," "); //add and extra space between the emblimbs to square it out
	    }
	
       }
       }
       
	len += snprintf(mapdisp + len, sizeof(mapdisp) - len,"{cx\r\n");
    }
    
    len += snprintf(mapdisp + len, sizeof(mapdisp) - len,"{cx\r\n");
    mapdisp[len] = 0;
    new_send_to_char(ch, "%s", mapdisp);
    return;
}


/* will put a small map with current room desc and title */
/* this is the main function to show the map, its do_map with " " */

/* Heavily modified - Edward */
void ShowRoom(struct char_data *ch, int min, int max)
{
    char dispbuf[MAX_INPUT_LENGTH];

    #if 0
    /* mlk :: rounds edges */
    map[min][min] = NUM_ROOM_SECTORS;
    map[max - 1][max - 1] = NUM_ROOM_SECTORS;
    map[min][max - 1] = NUM_ROOM_SECTORS;
    map[max - 1][min] = NUM_ROOM_SECTORS;
    #endif

   

		 parse_room_name(IN_ROOM(ch), dispbuf, sizeof(dispbuf));
		 new_send_to_char(ch, "{cx   {cc%s{cx", dispbuf);

	    if (GET_LEVEL(ch) >= LVL_GOD) 
		new_send_to_char(ch, " {cc[Room %d]{cx", IN_ROOM(ch)->number);
		
	    new_send_to_char(ch, "\r\n");
	    
 ShowMap(ch, min, max);
	    
    return;
}

/* This is the main map function, do_map(ch, " ") is good to use */
/* do_map(ch "number") is for immortals to see the world map     */

/* Edward: If you play with some of the values here you can make the normal
 ** map people see larger or smaller. size = URANGE(9, size, MAX_MAP), the 9
 ** is the map size shown by default. Also look for: ShowMap (ch, min, max+1);
 ** and change the size of min and max and see what you like.
 */
ACMD(do_map)
{
    int size = 0, center, x, y, min, max;
    room_rnum was_in;
    char arg1[10];
    struct obj_data *viewport, *vehicle;
    arg1[0] = 0;
    //one_argument(argument, arg1);
    //size = atoi(arg1);
    size = URANGE(10, size, MAX_MAP);

    center = MAX_MAP / 2;

    min = MAX_MAP / 2 - size / 2;
    max = MAX_MAP / 2 + size / 2;

    was_in = IN_ROOM(ch);

    viewport =
	get_obj_in_list_type(ITEM_V_WINDOW, IN_ROOM(ch)->contents);
    if (viewport) {
	vehicle = find_vehicle_by_vnum(GET_OBJ_VAL(viewport, 0));
	if (!vehicle) {
	    send_to_char("Something is wrong with this vehicle.\r\n", ch);
	    return;
	}
	IN_ROOM(ch) = IN_ROOM(vehicle);
    }

    for (x = 0; x < MAX_MAP; ++x)
	for (y = 0; y < MAX_MAP; ++y)
	    map[x][y] = NUM_ROOM_SECTORS;

    /* starts the mapping with the center room */
    MapArea(IN_ROOM(ch), ch, center, center, min - 1, max - 1);

    /* marks the center, where ch is */
    map[center][center] = NUM_ROOM_SECTORS + 2;	/* can be any number above NUM_ROOM_SECTORS+1 */

    /* switch default will print out the */
    if ((GET_LEVEL(ch) < LVL_GOD) || (IS_NPC(ch))) {
	if (!IS_SET_AR(IN_ROOM(ch)->room_flags, ROOM_WILDERNESS)) {
#if defined(MINI_MAP)
         mini_map(ch);
#else
	 display_map(ch);
#endif
	    IN_ROOM(ch) = was_in;
	    return;
	}
	if (IS_DARK(IN_ROOM(ch)) && !CAN_SEE_IN_DARK(ch)) {
	    send_to_char
		("{cbThe wilderness is pitch black at night... {cx\r\n",
		 ch);
	    IN_ROOM(ch) = was_in;
	    return;
	} else {
	    ShowRoom(ch, min, max + 1);
	    IN_ROOM(ch) = was_in;
	    return;
	}
    }

    /* mortals not in city, enter or inside will always get a ShowRoom */
    if (GET_LEVEL(ch) >= LVL_GOD) {
	if (arg1[0] == '\0') {
	    ShowRoom(ch, min, max + 1);
	    IN_ROOM(ch) = was_in;
	} else {
	    ShowMap(ch, min, max + 1);
	    IN_ROOM(ch) = was_in;
	}
	return;
    }

    send_to_char("What??\r\n", ch);
    IN_ROOM(ch) = was_in;
    return;
}
